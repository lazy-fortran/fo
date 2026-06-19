module fo_fpm_config
    implicit none
    private
    public :: fpm_dep_t, fpm_config_t
    public :: fpm_config_init, fpm_config_parse

    integer, parameter :: MAX_DEPS = 64
    integer, parameter :: MAX_DEV_DEPS = 32
    integer, parameter :: MAX_LINK_LIBS = 32
    integer, parameter :: MAX_FLAGS = 16

    type :: fpm_dep_t
        character(len=256) :: name = ''
        character(len=512) :: git = ''
        character(len=128) :: branch = ''
        character(len=128) :: tag = ''
        character(len=512) :: path = ''
        character(len=32)  :: version = '*'
    end type fpm_dep_t

    type :: fpm_config_t
        character(len=128) :: name = ''
        character(len=32)  :: version = ''
        character(len=256) :: source_dir = 'src'
        character(len=256) :: app_dir = 'app'
        character(len=256) :: test_dir = 'test'
        character(len=256) :: project_dir = '.'
        logical :: auto_executables = .true.
        logical :: auto_tests = .true.
        integer :: n_deps = 0
        type(fpm_dep_t) :: deps(MAX_DEPS)
        integer :: n_dev_deps = 0
        type(fpm_dep_t) :: dev_deps(MAX_DEV_DEPS)
        integer :: n_link_libs = 0
        character(len=128) :: link_libs(MAX_LINK_LIBS)
        integer :: n_flags = 0
        character(len=128) :: flags(MAX_FLAGS)
        ! fpm "openmp" metapackage (openmp = "*" under [dependencies]). When set,
        ! the backend compiles and links with -fopenmp so the project's `!$omp`
        ! regions run in parallel. Without it gfortran ignores the directives.
        logical :: openmp = .false.
    end type fpm_config_t

contains

    subroutine fpm_config_init(c)
        type(fpm_config_t), intent(out) :: c

        c%name = ''
        c%version = ''
        c%source_dir = 'src'
        c%app_dir = 'app'
        c%test_dir = 'test'
        c%project_dir = '.'
        c%auto_executables = .true.
        c%auto_tests = .true.
        c%openmp = .false.
        c%n_deps = 0
        c%n_dev_deps = 0
        c%n_link_libs = 0
        c%n_flags = 0
    end subroutine fpm_config_init

    subroutine fpm_config_parse(project_dir, config, ierr)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(out) :: config
        integer, intent(out) :: ierr

        character(len=1024) :: line, key, val, section
        ! accumulated value for multi-line arrays (flags = [\n  "...",\n])
        character(len=4096) :: accum
        logical :: in_array
        character(len=1024) :: pending_key
        integer :: u, ios

        call fpm_config_init(config)
        config%project_dir = trim(project_dir)
        ierr = 0
        section = ''
        in_array = .false.
        accum = ''
        pending_key = ''

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', &
            status='old', iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if

        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call strip_comment(line)
            line = adjustl(line)
            if (len_trim(line) == 0) cycle

            ! while accumulating a multi-line array, append each line until ']'
            if (in_array) then
                accum = trim(accum)//trim(line)
                if (index(line, ']') > 0) then
                    in_array = .false.
                    val = trim(accum)
                    select case (trim(section))
                    case ('build')
                        call parse_build(pending_key, val, config)
                    end select
                end if
                cycle
            end if

            if (line(1:1) == '[') then
                call get_section(line, section)
                cycle
            end if

            call split_kv(line, key, val)
            if (len_trim(key) == 0) cycle

            ! detect a value that opens '[' without closing ']': multi-line array
            if (index(val, '[') > 0 .and. index(val, ']') == 0) then
                in_array = .true.
                accum = trim(val)
                pending_key = trim(key)
                cycle
            end if

            select case (trim(section))
            case ('')
                call parse_top_level(key, val, config)
            case ('build')
                call parse_build(key, val, config)
            case ('dependencies')
                if (trim(key) == 'openmp') then
                    ! fpm metapackage, not a real dependency: maps to -fopenmp.
                    config%openmp = .true.
                else if (config%n_deps < MAX_DEPS) then
                    config%n_deps = config%n_deps + 1
                    call parse_dep(key, val, config%deps(config%n_deps))
                end if
            case ('dev-dependencies')
                if (config%n_dev_deps < MAX_DEV_DEPS) then
                    config%n_dev_deps = config%n_dev_deps + 1
                    call parse_dep(key, val, config%dev_deps(config%n_dev_deps))
                end if
            end select
        end do

        close (u)
    end subroutine fpm_config_parse

    subroutine parse_top_level(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        character(len=512) :: str_val

        select case (trim(key))
        case ('name')
            call extract_string(val, str_val)
            config%name = trim(str_val)
        case ('version')
            call extract_string(val, str_val)
            config%version = trim(str_val)
        end select
    end subroutine parse_top_level

    subroutine parse_build(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        character(len=256) :: str_val

        select case (trim(key))
        case ('source-dir')
            call extract_string(val, str_val)
            if (len_trim(str_val) > 0) config%source_dir = trim(str_val)
        case ('app-dir')
            call extract_string(val, str_val)
            if (len_trim(str_val) > 0) config%app_dir = trim(str_val)
        case ('test-dir')
            call extract_string(val, str_val)
            if (len_trim(str_val) > 0) config%test_dir = trim(str_val)
        case ('auto-executables')
            config%auto_executables = (index(val, 'true') > 0)
        case ('auto-tests')
            config%auto_tests = (index(val, 'true') > 0)
        case ('link')
            call parse_link_libs(val, config)
        case ('flags')
            call parse_flags(val, config)
        end select
    end subroutine parse_build

    subroutine parse_dep(name_key, val, dep)
        character(len=*), intent(in) :: name_key, val
        type(fpm_dep_t), intent(out) :: dep

        character(len=64)  :: ikeys(8)
        character(len=512) :: ivals(8)
        integer :: n_fields, i
        character(len=512) :: str_val

        dep%name = trim(name_key)
        dep%git = ''
        dep%branch = ''
        dep%tag = ''
        dep%path = ''
        dep%version = '*'

        if (len_trim(val) == 0) return

        if (val(1:1) == '{') then
            call parse_inline_table(val, ikeys, ivals, n_fields)
            do i = 1, n_fields
                call extract_string(ivals(i), str_val)
                select case (trim(ikeys(i)))
                case ('git')
                    dep%git = trim(str_val)
                case ('branch')
                    dep%branch = trim(str_val)
                case ('tag')
                    dep%tag = trim(str_val)
                case ('path')
                    dep%path = trim(str_val)
                end select
            end do
        else
            call extract_string(val, str_val)
            dep%version = trim(str_val)
        end if
    end subroutine parse_dep

    subroutine parse_link_libs(val, config)
        character(len=*), intent(in) :: val
        type(fpm_config_t), intent(inout) :: config

        integer :: pos, start, n
        character(len=128) :: lib
        logical :: in_str

        pos = 1
        n = len_trim(val)
        in_str = .false.

        do while (pos <= n)
            if (val(pos:pos) == '"') then
                if (.not. in_str) then
                    in_str = .true.
                    start = pos + 1
                else
                    in_str = .false.
                    lib = val(start:pos - 1)
                    if (len_trim(lib) > 0 .and. &
                        config%n_link_libs < MAX_LINK_LIBS) then
                    config%n_link_libs = config%n_link_libs + 1
                    config%link_libs(config%n_link_libs) = trim(lib)
                end if
            end if
        end if
        pos = pos + 1
    end do
end subroutine parse_link_libs

subroutine parse_flags(val, config)
    character(len=*), intent(in) :: val
    type(fpm_config_t), intent(inout) :: config

    integer :: pos, start, n
    character(len=128) :: flag
    logical :: in_str

    pos = 1
    n = len_trim(val)
    in_str = .false.

    do while (pos <= n)
        if (val(pos:pos) == '"') then
            if (.not. in_str) then
                in_str = .true.
                start = pos + 1
            else
                in_str = .false.
                flag = val(start:pos - 1)
                if (len_trim(flag) > 0 .and. &
                    config%n_flags < MAX_FLAGS) then
                config%n_flags = config%n_flags + 1
                config%flags(config%n_flags) = trim(flag)
            end if
        end if
    end if
    pos = pos + 1
end do
end subroutine parse_flags

subroutine strip_comment(line)
    character(len=*), intent(inout) :: line

    integer :: i
    logical :: in_str

    in_str = .false.
    do i = 1, len_trim(line)
        if (line(i:i) == '"') then
            in_str = .not. in_str
        else if (line(i:i) == '#' .and. .not. in_str) then
            line(i:) = ' '
            return
        end if
    end do
end subroutine strip_comment

subroutine get_section(line, section)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: section

    integer :: i1, i2, n

    section = ''
    n = len_trim(line)
    if (n < 2) return

    ! strip [[ ]] for array-of-tables (treat same as regular section)
    if (n >= 4 .and. line(1:2) == '[[') then
        i1 = 3
        i2 = index(line, ']]') - 1
    else
        i1 = 2
        i2 = index(line, ']') - 1
    end if
    if (i2 < i1) return
    section = adjustl(line(i1:i2))
end subroutine get_section

subroutine split_kv(line, key, val)
    character(len=*), intent(in) :: line
    character(len=*), intent(out) :: key, val

    integer :: eq_pos

    key = ''
    val = ''
    eq_pos = index(line, '=')
    if (eq_pos < 2) return

    key = adjustl(line(1:eq_pos - 1))
    ! trim trailing whitespace from key
    key = trim(key)
    val = adjustl(line(eq_pos + 1:))
end subroutine split_kv

subroutine extract_string(raw_val, str_val)
    character(len=*), intent(in) :: raw_val
    character(len=*), intent(out) :: str_val

    integer :: q1, q2, n

    str_val = ''
    n = len_trim(raw_val)
    if (n < 2) then
        ! bare value (e.g. "*")
        str_val = trim(raw_val)
        return
    end if

    q1 = index(raw_val, '"')
    if (q1 == 0) then
        str_val = trim(raw_val)
        return
    end if
    q2 = index(raw_val(q1 + 1:), '"')
    if (q2 == 0) return
    q2 = q1 + q2
    str_val = raw_val(q1 + 1:q2 - 1)
end subroutine extract_string

subroutine parse_inline_table(val, keys, vals, n_fields)
    character(len=*), intent(in) :: val
    character(len=*), intent(out) :: keys(:), vals(:)
    integer, intent(out) :: n_fields

    character(len=1024) :: inner
    integer :: i1, i2, max_fields, pos, eq_pos, comma_pos, n

    n_fields = 0
    max_fields = min(size(keys), size(vals))

    ! find content between { }
    i1 = index(val, '{')
    i2 = index(val, '}')
    if (i1 == 0 .or. i2 <= i1) return
    inner = adjustl(val(i1 + 1:i2 - 1))

    pos = 1
    n = len_trim(inner)
    do while (pos <= n .and. n_fields < max_fields)
        ! skip whitespace and commas
        do while (pos <= n .and. &
                (inner(pos:pos) == ' ' .or. inner(pos:pos) == ','))
            pos = pos + 1
        end do
        if (pos > n) exit

        ! find '=' for this key
        eq_pos = index(inner(pos:), '=')
        if (eq_pos == 0) exit
        eq_pos = pos + eq_pos - 1

        n_fields = n_fields + 1
        keys(n_fields) = trim(adjustl(inner(pos:eq_pos - 1)))

        ! find the value (quoted string or bare)
        pos = eq_pos + 1
        do while (pos <= n .and. inner(pos:pos) == ' ')
            pos = pos + 1
        end do
        if (pos > n) exit

        if (inner(pos:pos) == '"') then
            ! quoted string: find closing quote
            i1 = index(inner(pos + 1:), '"')
            if (i1 == 0) then
                vals(n_fields) = ''
                exit
            end if
            vals(n_fields) = inner(pos:pos + i1)
            pos = pos + i1 + 1
        else
            ! bare value: ends at comma or end of string
            comma_pos = index(inner(pos:), ',')
            if (comma_pos == 0) then
                vals(n_fields) = trim(inner(pos:n))
                pos = n + 1
            else
                vals(n_fields) = trim(inner(pos:pos + comma_pos - 2))
                pos = pos + comma_pos
            end if
        end if
    end do
end subroutine parse_inline_table

end module fo_fpm_config
