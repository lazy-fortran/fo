module fo_scan
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_process, only: process_scan_sources
    use fo_scan_types, only: scan_unit_t, MAX_NAME, MAX_PATH, MAX_UNITS, MAX_DEPS
    use fo_scan_cache, only: scan_cache_load, scan_cache_load_trusted, &
        scan_cache_save
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_file, &
        query_program_units, query_program_unit, query_use_statements, &
        program_unit_query_t, use_statement_query_t, INPUT_MODE_STANDARD
    implicit none
    private

    public :: scan_unit_t, scan_file, scan_file_regex, scan_dir, scan_dir_regex, &
        scan_dir_cached
    public :: is_slow_test, source_defines_module
    public :: MAX_NAME, MAX_PATH, MAX_UNITS

    character(len=32), dimension(10), parameter :: INTRINSIC_MODULES = [ &
        'iso_fortran_env  ', &
        'iso_c_binding    ', &
        'ieee_arithmetic  ', &
        'ieee_exceptions  ', &
        'ieee_features    ', &
        'omp_lib          ', &
        'openacc          ', &
        'mpi              ', &
        'mpi_f08          ', &
        'coarray_intrinsic' &
        ]

contains

    subroutine scan_dir_cached(dirname, units, n_units, ierr, &
            allow_regex_fallback)
        character(len=*), intent(in) :: dirname
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        integer, intent(out) :: n_units, ierr
        logical, intent(in), optional :: allow_regex_fallback
        logical :: hit

        call scan_cache_load_trusted(dirname, units, hit)
        if (hit) then
            n_units = size(units)
            ierr = 0
            return
        end if
        call scan_dir(dirname, units, n_units, ierr, &
            allow_regex_fallback=allow_regex_fallback)
    end subroutine scan_dir_cached

    logical function source_defines_module(filename) result(defines_module)
        character(len=*), intent(in) :: filename

        character(len=512) :: line
        character(len=MAX_NAME) :: name
        integer :: funit, iostat

        defines_module = .false.
        open (newunit=funit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return
        do
            read (funit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            call extract_module_def(adjustl(line), name)
            if (len_trim(name) > 0) then
                defines_module = .true.
                exit
            end if
        end do
        close (funit)
    end function source_defines_module

    subroutine scan_file(filename, unit_info, ierr, allow_regex_fallback, &
            diagnostic)
        character(len=*), intent(in) :: filename
        type(scan_unit_t), intent(out) :: unit_info
        integer, intent(out) :: ierr
        logical, intent(in), optional :: allow_regex_fallback
        character(len=*), intent(out), optional :: diagnostic

        character(len=1024) :: ast_diagnostic
        logical :: use_fallback

        ast_diagnostic = ''
        if (present(diagnostic)) diagnostic = ''
        ! The environment policy is also the emergency backend selector for
        ! sources that can crash the optional AST frontend.  An explicit
        ! allow_regex_fallback argument retains the AST-first unit-test/API
        ! behavior; FO_SCAN_FALLBACK=regex skips the AST process entirely.
        use_fallback = regex_fallback_policy(allow_regex_fallback)
        if (use_fallback .and. .not. present(allow_regex_fallback)) then
            call scan_file_regex(filename, unit_info, ierr)
            return
        end if
        call scan_file_ast(filename, unit_info, ierr, ast_diagnostic)
        if (ierr == 0) then
            if (present(diagnostic)) diagnostic = ast_diagnostic
            return
        end if

        if (present(diagnostic)) diagnostic = ast_diagnostic
        if (.not. use_fallback) return

        call scan_file_regex(filename, unit_info, ierr)
        if (ierr /= 0 .and. present(diagnostic)) then
            diagnostic = trim(ast_diagnostic)//'; regex fallback failed'
        end if
    end subroutine scan_file

    subroutine scan_file_ast(filename, unit_info, ierr, diagnostic)
        character(len=*), intent(in) :: filename
        type(scan_unit_t), intent(out) :: unit_info
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: diagnostic

        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(program_unit_query_t), allocatable :: program_units(:)
        type(program_unit_query_t) :: selected_unit, candidate_unit
        type(use_statement_query_t), allocatable :: use_statements(:)
        integer :: i, j, primary
        character(len=MAX_NAME) :: unit_kind, name, parent_identifier
        character(len=MAX_NAME) :: ancestor_name, parent_name

        call reset_scan_unit(filename, unit_info)
        ierr = 1
        diagnostic = ''
        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        call compile_frontend_from_file(filename, result, options)
        if (.not. result%parse_ok) then
            call compiler_diagnostic(result, diagnostic)
            return
        end if

        program_units = query_program_units(result%arena, result%root_index)
        primary = 0
        do i = 1, size(program_units)
            if (.not. program_units(i)%found) cycle
            unit_kind = ''
            if (allocated(program_units(i)%unit_kind)) then
                unit_kind = program_units(i)%unit_kind
            end if
            call to_lower(unit_kind)
            select case (trim(unit_kind))
            case ('module', 'submodule', 'program', 'function', &
                    'subroutine', 'block_data')
                primary = i
                exit
            end select
        end do
        if (primary == 0) then
            diagnostic = 'FortFront parsed no dependency-bearing program unit'
            return
        end if

        selected_unit = program_units(primary)
        unit_kind = ''
        if (allocated(selected_unit%unit_kind)) then
            unit_kind = selected_unit%unit_kind
        end if
        call to_lower(unit_kind)
        name = ''
        if (allocated(selected_unit%name)) then
            name = selected_unit%name
        end if
        call to_lower(name)
        if (trim(unit_kind) == 'program' .and. trim(name) == 'main') then
            do j = 1, size(selected_unit%body_indices)
                candidate_unit = query_program_unit(result%arena, &
                    selected_unit%body_indices(j))
                if (.not. candidate_unit%found) cycle
                unit_kind = ''
                if (allocated(candidate_unit%unit_kind)) then
                    unit_kind = candidate_unit%unit_kind
                end if
                call to_lower(unit_kind)
                if (trim(unit_kind) /= 'function' .and. &
                    trim(unit_kind) /= 'subroutine') cycle
                selected_unit = candidate_unit
                exit
            end do
        end if
        unit_kind = ''
        if (allocated(selected_unit%unit_kind)) then
            unit_kind = selected_unit%unit_kind
        end if
        call to_lower(unit_kind)
        if (allocated(selected_unit%name)) then
            name = selected_unit%name
        else
            name = ''
        end if
        call to_lower(name)
        unit_info%source_line = selected_unit%line
        unit_info%source_column = selected_unit%column
        select case (trim(unit_kind))
        case ('module', 'submodule', 'block_data')
            unit_info%module_name = name
        case ('program')
            unit_info%program_name = name
            unit_info%is_program = .true.
        case ('function', 'subroutine')
            unit_info%module_name = name
        end select

        if (trim(unit_kind) == 'submodule' .and. &
            allocated(selected_unit%parent_identifier)) then
            parent_identifier = selected_unit%parent_identifier
            call to_lower(parent_identifier)
            call split_parent_identifier(parent_identifier, ancestor_name, &
                parent_name)
            if (len_trim(ancestor_name) > 0) then
                call add_dep(unit_info, ancestor_name, &
                    selected_unit%line, selected_unit%column)
            end if
            if (len_trim(parent_name) > 0) then
                call add_dep(unit_info, parent_name, &
                    selected_unit%line, selected_unit%column)
            end if
        end if

        use_statements = query_use_statements(result%arena)
        do i = 1, size(use_statements)
            if (.not. use_statements(i)%found) cycle
            if (use_statements(i)%is_intrinsic) cycle
            name = ''
            if (allocated(use_statements(i)%module_name)) then
                name = use_statements(i)%module_name
            end if
            call to_lower(name)
            if (len_trim(name) == 0) cycle
            call add_dep(unit_info, name, use_statements(i)%line, &
                use_statements(i)%column)
        end do
        ierr = 0
    end subroutine scan_file_ast

    subroutine scan_file_regex(filename, unit_info, ierr)
        character(len=*), intent(in) :: filename
        type(scan_unit_t), intent(out) :: unit_info
        integer, intent(out) :: ierr

        integer :: funit, iostat
        character(len=512) :: line
        integer :: line_number

        call reset_scan_unit(filename, unit_info)
        ierr = 0
        line_number = 0

        open (newunit=funit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) then
            write (error_unit, '(a)') 'fo: cannot open '//trim(filename)
            ierr = 1
            return
        end if

        do
            read (funit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            line_number = line_number + 1
            call parse_line(line, unit_info, line_number)
        end do

        close (funit)
    end subroutine scan_file_regex

    subroutine scan_dir(dirname, units, n_units, ierr, allow_regex_fallback)
        character(len=*), intent(in) :: dirname
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        integer, intent(out) :: n_units, ierr
        logical, intent(in), optional :: allow_regex_fallback

        call scan_dir_impl(dirname, units, n_units, ierr, allow_regex_fallback, &
            .false.)
    end subroutine scan_dir

    subroutine scan_dir_regex(dirname, units, n_units, ierr)
        !! Scan a dependency source directory without invoking FortFront.
        !!
        !! Acquired dependencies have already passed their compiler's build
        !! step. The repair path only needs module names and USE edges, and
        !! must remain safe for valid legacy Fortran that the optional AST
        !! frontend cannot parse or report without crashing.
        character(len=*), intent(in) :: dirname
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        integer, intent(out) :: n_units, ierr

        call scan_dir_impl(dirname, units, n_units, ierr, .false., .true.)
    end subroutine scan_dir_regex

    subroutine scan_dir_impl(dirname, units, n_units, ierr, allow_regex_fallback, &
            regex_only)
        character(len=*), intent(in) :: dirname
        type(scan_unit_t), allocatable, intent(out) :: units(:)
        integer, intent(out) :: n_units, ierr
        logical, intent(in), optional :: allow_regex_fallback
        logical, intent(in) :: regex_only

        character(len=512), allocatable :: paths(:)
        character(len=512) :: tmpfile, line, diagnostic
        integer :: funit, iostat, sub_ierr, n_files, i
        logical :: cache_hit

        ierr = 0
        n_units = 0
        n_files = 0
        call make_tmpfile('fo_scan_files', tmpfile)

        call process_scan_sources(dirname, tmpfile, sub_ierr)
        if (sub_ierr /= 0) then
            ierr = 1
            allocate (units(0))
            call delete_tmpfile(tmpfile)
            return
        end if

        open (newunit=funit, file=tmpfile, status='old', iostat=iostat)
        if (iostat /= 0) then
            ierr = 1
            allocate (units(0))
            call delete_tmpfile(tmpfile)
            return
        end if

        do
            read (funit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (len_trim(line) == 0) cycle
            n_files = n_files + 1
            if (n_files > MAX_UNITS) then
                write (error_unit, '(a,i0)') &
                    'fo: too many source files, max ', MAX_UNITS
                n_files = MAX_UNITS
                exit
            end if
        end do

        allocate (units(n_files), paths(n_files))
        rewind (funit)
        do i = 1, n_files
            read (funit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            paths(i) = trim(line)
        end do
        close (funit)
        call delete_tmpfile(tmpfile)

        if (.not. regex_only) then
            call scan_cache_load(dirname, paths, units, cache_hit)
            if (cache_hit) then
                n_units = n_files
                return
            end if
        end if

        do i = 1, n_files
            n_units = n_units + 1
            if (regex_only) then
                call scan_file_regex(trim(paths(i)), units(n_units), sub_ierr)
                cycle
            end if
            call scan_file(trim(paths(i)), units(n_units), sub_ierr, &
                allow_regex_fallback=allow_regex_fallback, diagnostic=diagnostic)
            if (sub_ierr /= 0) then
                if (len_trim(diagnostic) > 0) then
                    write (error_unit, '(a)') 'fo: '//trim(diagnostic)
                end if
                ! A front-end parse failure must never silently remove a
                ! compilation unit: the build would then link against a stale
                ! module or fail with a missing .mod far from the real cause.
                ! Recover the unit with the line scanner and keep building.
                call scan_file_regex(trim(paths(i)), units(n_units), sub_ierr)
                if (sub_ierr == 0) then
                    write (error_unit, '(a)') 'fo: recovered '// &
                        trim(paths(i))//' with the line scanner'
                else
                    n_units = n_units - 1
                end if
            end if
        end do

        if (.not. regex_only .and. n_units == n_files) then
            call scan_cache_save(dirname, paths, units)
        end if
    end subroutine scan_dir_impl

    subroutine reset_scan_unit(filename, unit_info)
        character(len=*), intent(in) :: filename
        type(scan_unit_t), intent(out) :: unit_info

        unit_info%filename = ''
        unit_info%module_name = ''
        unit_info%program_name = ''
        unit_info%is_program = .false.
        unit_info%is_test = .false.
        unit_info%source_line = 0
        unit_info%source_column = 0
        unit_info%n_deps = 0
        unit_info%deps = ''
        unit_info%dependency_lines = 0
        unit_info%dependency_columns = 0
        unit_info%filename = filename
        unit_info%is_test = is_test_path(filename)
    end subroutine reset_scan_unit

    subroutine compiler_diagnostic(result, diagnostic)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=*), intent(out) :: diagnostic

        diagnostic = ''
        if (allocated(result%diagnostic_text)) then
            if (len_trim(result%diagnostic_text) > 0) then
                diagnostic = result%diagnostic_text
                return
            end if
        end if
        if (allocated(result%error_msg)) then
            if (len_trim(result%error_msg) > 0) then
                diagnostic = result%error_msg
                return
            end if
        end if
        diagnostic = 'FortFront could not parse '//trim(result%source_path)
    end subroutine compiler_diagnostic

    logical function regex_fallback_policy(requested) result(enabled)
        logical, intent(in), optional :: requested
        character(len=32) :: policy

        if (present(requested)) then
            enabled = requested
            return
        end if

        ! The environment switch is deliberately opt-in. Callers that own a
        ! bootstrap phase can pass allow_regex_fallback=.true. explicitly.
        policy = ''
        call get_environment_variable('FO_SCAN_FALLBACK', policy)
        call to_lower(policy)
        enabled = trim(policy) == 'regex' .or. trim(policy) == 'true' .or. &
            trim(policy) == '1'
    end function regex_fallback_policy

    subroutine split_parent_identifier(value, ancestor, parent)
        character(len=*), intent(in) :: value
        character(len=*), intent(out) :: ancestor, parent
        integer :: colon

        ancestor = ''
        parent = ''
        colon = index(value, ':')
        if (colon == 0) then
            ancestor = trim(adjustl(value))
        else
            if (colon > 1) ancestor = trim(adjustl(value(:colon - 1)))
            if (colon < len_trim(value)) then
                parent = trim(adjustl(value(colon + 1:)))
            end if
        end if
    end subroutine split_parent_identifier

    logical function is_test_path(path)
        !! A unit is a test iff it lives under a test directory, matching fpm's
        !! directory-based classification. A test_ filename prefix alone does NOT
        !! make a library source a test: src/utilities/test_shell_commands.f90 is
        !! a library module, and classifying it as a test would drop it from the
        !! library build and break the link of anything that uses it.
        character(len=*), intent(in) :: path

        character(len=512) :: clean

        clean = trim(path)
        is_test_path = index(clean, '/test/') > 0 .or. &
            index(clean, '/tests/') > 0
    end function is_test_path

    subroutine parse_line(line, unit_info, line_number)
        character(len=*), intent(in) :: line
        type(scan_unit_t), intent(inout) :: unit_info
        integer, intent(in), optional :: line_number

        character(len=512) :: trimmed
        character(len=MAX_NAME) :: name, ancestor_name, parent_name
        integer :: current_line

        current_line = 0
        if (present(line_number)) current_line = line_number

        trimmed = adjustl(line)
        if (len_trim(trimmed) == 0) return
        if (trimmed(1:1) == '!') return

        call extract_use(trimmed, name)
        if (len_trim(name) > 0) then
            if (.not. is_intrinsic(name)) then
                call add_dep(unit_info, name, current_line, leading_column(line))
            end if
            return
        end if

        call extract_module_def(trimmed, name)
        if (len_trim(name) > 0) then
            unit_info%module_name = name
            unit_info%source_line = current_line
            unit_info%source_column = leading_column(line)
            return
        end if

        call extract_submodule_def(trimmed, name, ancestor_name, parent_name)
        if (len_trim(name) > 0) then
            unit_info%module_name = name
            unit_info%source_line = current_line
            unit_info%source_column = leading_column(line)
            if (len_trim(ancestor_name) > 0) then
                call add_dep(unit_info, ancestor_name, current_line, &
                    leading_column(line))
            end if
            if (len_trim(parent_name) > 0) then
                call add_dep(unit_info, parent_name, current_line, &
                    leading_column(line))
            end if
            return
        end if

        if (len_trim(unit_info%module_name) == 0 .and. &
            .not. unit_info%is_program) then
            call extract_external_procedure_def(trimmed, name)
            if (len_trim(name) > 0) then
                unit_info%module_name = name
                unit_info%source_line = current_line
                unit_info%source_column = leading_column(line)
                return
            end if
        end if

        call extract_program_def(trimmed, name)
        if (len_trim(name) > 0) then
            unit_info%program_name = name
            unit_info%is_program = .true.
            unit_info%source_line = current_line
            unit_info%source_column = leading_column(line)
        end if
    end subroutine parse_line

    subroutine extract_use(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        integer :: start, fin, comma_pos, only_pos

        name = ''
        if (len_trim(line) < 5) return

        if (line(1:4) /= 'use ' .and. line(1:4) /= 'USE ') return

        start = 5

        ! skip 'intrinsic ::' or ', intrinsic ::'
        if (index(line, '::') > 0) then
            start = index(line, '::') + 2
        end if

        ! skip whitespace
        do while (start <= len_trim(line))
            if (line(start:start) /= ' ') exit
            start = start + 1
        end do

        comma_pos = index(line(start:), ',')
        only_pos = index(line(start:), ' ')

        if (comma_pos > 0 .and. (only_pos == 0 .or. comma_pos < only_pos)) then
            fin = start + comma_pos - 2
        else if (only_pos > 0) then
            fin = start + only_pos - 2
        else
            fin = len_trim(line)
        end if

        do while (fin >= start)
            if (line(fin:fin) /= ' ') exit
            fin = fin - 1
        end do

        if (fin >= start) then
            name = line(start:fin)
            call to_lower(name)
        end if
    end subroutine extract_use

    subroutine extract_module_def(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        character(len=512) :: lower_line
        character(len=MAX_NAME) :: first_name
        integer :: start, fin

        name = ''
        lower_line = line
        call to_lower(lower_line)

        if (len_trim(lower_line) < 8) return
        if (lower_line(1:7) /= 'module ') return
        start = 8
        do while (start <= len_trim(line))
            if (line(start:start) /= ' ') exit
            start = start + 1
        end do

        fin = start
        do while (fin <= len_trim(line))
            if (line(fin:fin) == ' ' .or. line(fin:fin) == '!') exit
            fin = fin + 1
        end do
        first_name = adjustl(line(start:fin - 1))
        call to_lower(first_name)

        if (trim(first_name) == 'procedure') return
        if (trim(first_name) == 'subroutine') return
        if (trim(first_name) == 'function') return

        name = first_name
        call to_lower(name)
    end subroutine extract_module_def

    subroutine extract_submodule_def(line, name, ancestor, parent)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name
        character(len=*), intent(out) :: ancestor, parent

        character(len=512) :: lower_line, parent_text, ancestor_text
        integer :: open_pos, close_pos, start, fin

        name = ''
        ancestor = ''
        parent = ''
        lower_line = line
        call to_lower(lower_line)

        if (len_trim(lower_line) < 11) return
        if (index(lower_line, 'submodule') /= 1) return

        open_pos = index(lower_line, '(')
        close_pos = index(lower_line, ')')
        if (open_pos == 0 .or. close_pos <= open_pos + 1) return
        if (open_pos > 10) then
            if (len_trim(lower_line(10:open_pos - 1)) > 0) return
        end if

        parent_text = adjustl(lower_line(open_pos + 1:close_pos - 1))
        call split_parent_identifier(parent_text, ancestor_text, lower_line)
        ancestor = trim(ancestor_text)
        parent = trim(lower_line)

        start = close_pos + 1
        do while (start <= len_trim(line))
            if (line(start:start) /= ' ') exit
            start = start + 1
        end do
        fin = start
        do while (fin <= len_trim(line))
            if (line(fin:fin) == ' ' .or. line(fin:fin) == '!') exit
            fin = fin + 1
        end do

        if (fin > start) then
            name = adjustl(line(start:fin - 1))
            call to_lower(name)
        end if
    end subroutine extract_submodule_def

    subroutine extract_external_procedure_def(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        character(len=512) :: lower_line
        integer :: start

        name = ''
        lower_line = line
        call to_lower(lower_line)

        if (index(lower_line, 'subroutine ') == 1) then
            start = 12
        else if (index(lower_line, 'function ') == 1) then
            start = 10
        else
            return
        end if

        do while (start <= len_trim(line))
            if (line(start:start) /= ' ') exit
            start = start + 1
        end do

        name = adjustl(line(start:))
        if (index(name, '(') > 0) name = name(1:index(name, '(') - 1)
        if (index(name, ' ') > 0) name = name(1:index(name, ' ') - 1)
        if (index(name, '!') > 0) name = name(1:index(name, '!') - 1)
        call to_lower(name)
    end subroutine extract_external_procedure_def

    subroutine extract_program_def(line, name)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: name

        character(len=512) :: lower_line
        integer :: start

        name = ''
        lower_line = line
        call to_lower(lower_line)

        if (len_trim(lower_line) < 9) return
        if (lower_line(1:8) /= 'program ') return

        start = 9
        do while (start <= len_trim(line))
            if (line(start:start) /= ' ') exit
            start = start + 1
        end do

        name = adjustl(line(start:))
        if (index(name, ' ') > 0) name = name(1:index(name, ' ') - 1)
        if (index(name, '!') > 0) name = name(1:index(name, '!') - 1)
        call to_lower(name)
    end subroutine extract_program_def

    logical function is_intrinsic(name)
        character(len=*), intent(in) :: name
        integer :: i

        is_intrinsic = .false.
        do i = 1, size(INTRINSIC_MODULES)
            if (trim(name) == trim(INTRINSIC_MODULES(i))) then
                is_intrinsic = .true.
                return
            end if
        end do
    end function is_intrinsic

    subroutine add_dep(unit_info, name, line, column)
        type(scan_unit_t), intent(inout) :: unit_info
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column

        integer :: i

        ! skip duplicates
        do i = 1, unit_info%n_deps
            if (trim(unit_info%deps(i)) == trim(name)) return
        end do

        if (unit_info%n_deps < MAX_DEPS) then
            unit_info%n_deps = unit_info%n_deps + 1
            unit_info%deps(unit_info%n_deps) = name
            if (present(line)) unit_info%dependency_lines(unit_info%n_deps) = line
            if (present(column)) then
                unit_info%dependency_columns(unit_info%n_deps) = column
            end if
        end if
    end subroutine add_dep

    integer function leading_column(line) result(column)
        character(len=*), intent(in) :: line
        integer :: i

        column = 1
        do i = 1, len(line)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then
                column = i
                return
            end if
        end do
    end function leading_column

    subroutine to_lower(str)
        character(len=*), intent(inout) :: str
        integer :: i, ic

        do i = 1, len_trim(str)
            ic = iachar(str(i:i))
            if (ic >= iachar('A') .and. ic <= iachar('Z')) then
                str(i:i) = achar(ic + 32)
            end if
        end do
    end subroutine to_lower

    logical function is_slow_test(name)
        character(len=*), intent(in) :: name

        character(len=MAX_NAME) :: lower_name
        integer :: n

        is_slow_test = .false.
        lower_name = name
        call to_lower(lower_name)
        n = len_trim(lower_name)
        if (n == 0) return

        ! matches *_slow or *_slow_*
        if (n >= 5) then
            if (lower_name(n - 4:n) == '_slow') then
                is_slow_test = .true.
                return
            end if
        end if
        if (index(trim(lower_name), '_slow_') > 0) then
            is_slow_test = .true.
        end if
    end function is_slow_test

end module fo_scan
