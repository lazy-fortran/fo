module fo_fpm_config
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: fpm_dep_t, fpm_exe_t, fpm_config_t
    public :: fpm_config_init, fpm_config_parse
    public :: dep_kind, DEP_PATH, DEP_GIT, DEP_REGISTRY
    public :: manifest_exe_name, manifest_test_name, manifest_example_name
    public :: manifest_executable_selected
    public :: manifest_test_args
    public :: MAX_LINK_LIBS, add_link_lib
    public :: MAX_EXTERNAL_MODULES

    ! How a dependency is acquired, derived from which fields the manifest set.
    ! path = local dir (mutable, may be edited); git = cloned at a ref (pinned,
    ! immutable); registry = a bare version spec like `stdlib = "*"` resolved
    ! through the package registry (pinned, immutable).
    integer, parameter :: DEP_PATH = 1
    integer, parameter :: DEP_GIT = 2
    integer, parameter :: DEP_REGISTRY = 3

    integer, parameter :: MAX_DEPS = 64
    integer, parameter :: MAX_DEV_DEPS = 32
    integer, parameter :: MAX_LINK_LIBS = 64
    integer, parameter :: MAX_EXTERNAL_MODULES = 64
    integer, parameter :: MAX_FLAGS = 64
    integer, parameter :: MAX_EXES = 64
    integer, parameter :: MAX_TEST_ARG_SETS = 128

    type :: fpm_dep_t
        character(len=256) :: name = ''
        character(len=512) :: git = ''
        character(len=128) :: branch = ''
        character(len=128) :: tag = ''
        character(len=:), allocatable :: rev
        character(len=512) :: path = ''
        character(len=32)  :: version = '*'
    end type fpm_dep_t

    ! An explicit [[executable]] entry. fpm names the built binary after `name`,
    ! not after the source stem, so `main = "main.f90"` under `name = "demo"`
    ! links to `demo`, not the package name.
    type :: fpm_exe_t
        character(len=128) :: name = ''
        character(len=256) :: main = 'main.f90'
        character(len=256) :: source_dir = 'app'
    end type fpm_exe_t

    type :: fpm_test_args_t
        character(len=128) :: name = ''
        character(len=:), allocatable :: args
    end type fpm_test_args_t

    type :: fpm_config_t
        character(len=128) :: name = ''
        character(len=32)  :: version = ''
        character(len=256) :: source_dir = 'src'
        character(len=256) :: app_dir = 'app'
        character(len=256) :: test_dir = 'test'
        character(len=256) :: example_dir = 'example'
        character(len=256) :: project_dir = '.'
        logical :: auto_executables = .true.
        logical :: auto_tests = .true.
        logical :: auto_examples = .true.
        integer :: n_deps = 0
        type(fpm_dep_t) :: deps(MAX_DEPS)
        integer :: n_dev_deps = 0
        type(fpm_dep_t) :: dev_deps(MAX_DEV_DEPS)
        integer :: n_link_libs = 0
        character(len=128) :: link_libs(MAX_LINK_LIBS)
        ! [build] external-modules: modules the project uses but does not
        ! define, such as hdf5 or netcdf from a system package.  fo does not
        ! resolve them in the DAG and instead searches the system module path
        ! for each one, so that gfortran gets a -I to wherever it is installed.
        integer :: n_external_modules = 0
        character(len=128) :: external_modules(MAX_EXTERNAL_MODULES)
        integer :: n_flags = 0
        character(len=128) :: flags(MAX_FLAGS)
        integer :: n_exes = 0
        type(fpm_exe_t) :: exes(MAX_EXES)
        integer :: n_tests = 0
        type(fpm_exe_t) :: tests(MAX_EXES)
        integer :: n_examples = 0
        type(fpm_exe_t) :: examples(MAX_EXES)
        integer :: n_test_arg_sets = 0
        type(fpm_test_args_t) :: test_arg_sets(MAX_TEST_ARG_SETS)
        ! fpm "openmp" metapackage (openmp = "*" under [dependencies]). When set,
        ! fo compiles and links with -fopenmp so the project's `!$omp` regions
        ! run in parallel. Without it gfortran ignores the directives.
        logical :: openmp = .false.
        ! fpm "blas" metapackage. Resolution is performed through the same
        ! pkg-config candidate order as fpm, so a consumer inherits the
        ! provider's actual link and compile flags rather than naming OpenBLAS
        ! (or netlib BLAS) itself.
        logical :: blas = .false.
        ! The remaining fpm metapackages.  They are resolved below into the
        ! same flags, external modules, and source dependencies that fpm adds.
        logical :: mpi = .false.
        logical :: hdf5 = .false.
        logical :: netcdf = .false.
        logical :: stdlib = .false.
        logical :: minpack = .false.
        ! [fortran] implicit-typing / implicit-external.  fpm defaults both to
        ! false, meaning the strict flags apply; a manifest that sets one to
        ! true is asking for the corresponding flag to be left off, which is
        ! what legacy fixed-form sources such as libneo's polylag_3.f90 need.
        logical :: implicit_typing = .false.
        logical :: implicit_external = .false.
    end type fpm_config_t

contains

    pure function dep_kind(dep) result(kind)
        !! Classify a parsed dependency by which source field the manifest set.
        !! path wins over git wins over a bare version (registry), matching how
        !! fpm treats a dependency table.
        type(fpm_dep_t), intent(in) :: dep
        integer :: kind

        if (len_trim(dep%path) > 0) then
            kind = DEP_PATH
        else if (len_trim(dep%git) > 0) then
            kind = DEP_GIT
        else
            kind = DEP_REGISTRY
        end if
    end function dep_kind

    subroutine fpm_config_init(c)
        type(fpm_config_t), intent(out) :: c

        c%name = ''
        c%version = ''
        c%source_dir = 'src'
        c%app_dir = 'app'
        c%test_dir = 'test'
        c%example_dir = 'example'
        c%project_dir = '.'
        c%auto_executables = .true.
        c%auto_tests = .true.
        c%auto_examples = .true.
        c%openmp = .false.
        c%blas = .false.
        c%mpi = .false.
        c%hdf5 = .false.
        c%netcdf = .false.
        c%stdlib = .false.
        c%minpack = .false.
        c%implicit_typing = .false.
        c%implicit_external = .false.
        c%n_deps = 0
        c%n_dev_deps = 0
        c%n_link_libs = 0
        c%n_external_modules = 0
        c%n_flags = 0
        c%n_exes = 0
        c%n_tests = 0
        c%n_examples = 0
        c%n_test_arg_sets = 0
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
                    case ('preprocess', 'preprocess.cpp')
                        call parse_preprocess(pending_key, val, config)
                    case ('extra.fo.test-args')
                        call parse_test_args(pending_key, val, config)
                    end select
                end if
                cycle
            end if

            if (line(1:1) == '[') then
                call get_section(line, section)
                if (trim(section) == 'executable' .and. &
                    config%n_exes < MAX_EXES) then
                    config%n_exes = config%n_exes + 1
                    config%exes(config%n_exes) = fpm_exe_t()
                else if (trim(section) == 'test' .and. &
                        config%n_tests < MAX_EXES) then
                    config%n_tests = config%n_tests + 1
                    config%tests(config%n_tests) = fpm_exe_t(source_dir='test')
                else if (trim(section) == 'example' .and. &
                        config%n_examples < MAX_EXES) then
                    config%n_examples = config%n_examples + 1
                    config%examples(config%n_examples) = &
                        fpm_exe_t(source_dir='example')
                end if
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
                call parse_dependency_entry(key, val, config)
            case ('dev-dependencies')
                call parse_dep_entry(key, val, config%dev_deps, config%n_dev_deps)
            case ('executable')
                if (config%n_exes > 0) &
                    call parse_exe(key, val, config%exes(config%n_exes))
            case ('test')
                if (config%n_tests > 0) &
                    call parse_exe(key, val, config%tests(config%n_tests))
            case ('example')
                if (config%n_examples > 0) &
                    call parse_exe(key, val, config%examples(config%n_examples))
            case ('preprocess', 'preprocess.cpp')
                call parse_preprocess(key, val, config)
            case ('extra.fo.test-args')
                call parse_test_args(key, val, config)
            case ('library')
                call parse_library(key, val, config)
            case ('fortran')
                call parse_fortran(key, val, config)
            end select
        end do

        close (u)
        if (ierr == 0) call resolve_metapackages(config, ierr)
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
        case default
            if (index(trim(key), 'dependencies.') == 1) then
                call parse_top_level_dependency(trim(key(14:)), val, config)
            end if
        end select
    end subroutine parse_top_level

    subroutine parse_top_level_dependency(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        call parse_dependency_entry(key, val, config)
    end subroutine parse_top_level_dependency

    subroutine parse_dependency_entry(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        select case (trim(key))
        case ('openmp')
            config%openmp = .true.
        case ('blas')
            config%blas = .true.
        case ('mpi')
            config%mpi = .true.
        case ('hdf5')
            config%hdf5 = .true.
        case ('netcdf')
            config%netcdf = .true.
        case ('stdlib')
            config%stdlib = .true.
        case ('minpack')
            config%minpack = .true.
        case default
            call parse_dep_entry(key, val, config%deps, config%n_deps)
        end select
    end subroutine parse_dependency_entry

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
        case ('auto-examples')
            config%auto_examples = (index(val, 'true') > 0)
        case ('link')
            call parse_link_libs(val, config)
        case ('external-modules')
            call parse_external_modules(val, config)
        case ('flags')
            call parse_flags(val, config)
        end select
    end subroutine parse_build

    subroutine parse_library(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config
        character(len=256) :: str_val

        if (trim(key) /= 'source-dir') return
        call extract_string(val, str_val)
        if (len_trim(str_val) > 0) config%source_dir = trim(str_val)
    end subroutine parse_library

    subroutine parse_fortran(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config
        character(len=256) :: str_val

        select case (trim(key))
        case ('implicit-typing')
            config%implicit_typing = (index(val, 'true') > 0)
            if (index(val, 'false') > 0) call append_config_flag( &
                '-fimplicit-none', config)
        case ('implicit-external')
            config%implicit_external = (index(val, 'true') > 0)
            if (index(val, 'false') > 0) call append_config_flag( &
                '-Werror=implicit-interface', config)
        case ('source-form')
            call extract_string(val, str_val)
            if (trim(str_val) == 'free') call append_config_flag( &
                '-ffree-form', config)
            if (trim(str_val) == 'fixed') call append_config_flag( &
                '-ffixed-form', config)
        end select
    end subroutine parse_fortran

    subroutine append_config_flag(flag, config)
        character(len=*), intent(in) :: flag
        type(fpm_config_t), intent(inout) :: config
        integer :: i

        do i = 1, config%n_flags
            if (trim(config%flags(i)) == trim(flag)) return
        end do
        if (config%n_flags >= MAX_FLAGS) return
        config%n_flags = config%n_flags + 1
        config%flags(config%n_flags) = trim(flag)
    end subroutine append_config_flag

    subroutine parse_exe(key, val, exe)
        character(len=*), intent(in) :: key, val
        type(fpm_exe_t), intent(inout) :: exe

        character(len=256) :: str_val

        call extract_string(val, str_val)
        if (len_trim(str_val) == 0) return
        select case (trim(key))
        case ('name')
            exe%name = trim(str_val)
        case ('main')
            exe%main = trim(str_val)
        case ('source-dir')
            exe%source_dir = trim(str_val)
        end select
    end subroutine parse_exe

    function manifest_exe_name(config, app_dir, stem) result(name)
        !! Binary name for an app program from an explicit [[executable]] entry
        !! whose main-source stem and source-dir match, or '' when none applies.
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: app_dir, stem
        character(len=128) :: name
        character(len=256) :: main_stem
        integer :: i, dot

        name = ''
        do i = 1, config%n_exes
            if (trim(config%exes(i)%source_dir) /= trim(app_dir)) cycle
            main_stem = trim(config%exes(i)%main)
            dot = index(trim(main_stem), '.', back=.true.)
            if (dot > 1) main_stem = main_stem(1:dot - 1)
            if (trim(main_stem) == trim(stem) .and. &
                len_trim(config%exes(i)%name) > 0) then
                name = trim(config%exes(i)%name)
                return
            end if
        end do
    end function manifest_exe_name

    logical function manifest_executable_selected(config, app_dir, stem) result(selected)
        !! fpm only auto-discovers app programs when auto-executables is true.
        !! With it disabled, an app program is buildable only when an explicit
        !! executable entry names its source stem.
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: app_dir, stem

        selected = config%auto_executables
        if (.not. selected) selected = len_trim(manifest_exe_name( &
            config, app_dir, stem)) > 0
    end function manifest_executable_selected

    function manifest_test_name(config, test_dir, stem) result(name)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: test_dir, stem
        character(len=128) :: name
        character(len=256) :: main_stem
        integer :: i, dot, slash

        name = ''
        do i = 1, config%n_tests
            if (trim(config%tests(i)%source_dir) /= trim(test_dir)) cycle
            main_stem = trim(config%tests(i)%main)
            slash = index(trim(main_stem), '/', back=.true.)
            if (slash > 0) main_stem = main_stem(slash + 1:)
            dot = index(trim(main_stem), '.', back=.true.)
            if (dot > 1) main_stem = main_stem(1:dot - 1)
            if (trim(main_stem) == trim(stem) .and. &
                len_trim(config%tests(i)%name) > 0) then
                name = trim(config%tests(i)%name)
                return
            end if
        end do
    end function manifest_test_name

    function manifest_test_args(config, name) result(args)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: args
        integer :: i

        args = ''
        do i = 1, config%n_test_arg_sets
            if (trim(config%test_arg_sets(i)%name) /= trim(name)) cycle
            if (allocated(config%test_arg_sets(i)%args)) then
                args = config%test_arg_sets(i)%args
            end if
            return
        end do
    end function manifest_test_args

    function manifest_example_name(config, example_dir, stem) result(name)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: example_dir, stem
        character(len=128) :: name
        character(len=256) :: main_stem
        integer :: i, dot, slash

        name = ''
        do i = 1, config%n_examples
            if (trim(config%examples(i)%source_dir) /= trim(example_dir)) cycle
            main_stem = trim(config%examples(i)%main)
            slash = index(trim(main_stem), '/', back=.true.)
            if (slash > 0) main_stem = main_stem(slash + 1:)
            dot = index(trim(main_stem), '.', back=.true.)
            if (dot > 1) main_stem = main_stem(1:dot - 1)
            if (trim(main_stem) == trim(stem) .and. &
                len_trim(config%examples(i)%name) > 0) then
                name = trim(config%examples(i)%name)
                return
            end if
        end do
    end function manifest_example_name

    subroutine parse_dep_entry(name_key, val, deps, n_deps)
        character(len=*), intent(in) :: name_key, val
        type(fpm_dep_t), intent(inout) :: deps(:)
        integer, intent(inout) :: n_deps

        character(len=256) :: name, field
        character(len=512) :: str_val
        integer :: dot, i, found

        dot = index(trim(name_key), '.')
        if (dot <= 1) then
            if (n_deps >= size(deps)) return
            n_deps = n_deps + 1
            call parse_dep(name_key, val, deps(n_deps))
            return
        end if

        name = trim(name_key(:dot - 1))
        field = trim(name_key(dot + 1:))
        found = 0
        do i = 1, n_deps
            if (trim(deps(i)%name) == trim(name)) found = i
        end do
        if (found == 0) then
            if (n_deps >= size(deps)) return
            n_deps = n_deps + 1
            found = n_deps
            call parse_dep(name, '', deps(found))
        end if

        call extract_string(val, str_val)
        select case (trim(field))
        case ('git')
            deps(found)%git = trim(str_val)
        case ('branch')
            deps(found)%branch = trim(str_val)
        case ('tag')
            deps(found)%tag = trim(str_val)
        case ('rev')
            deps(found)%rev = trim(str_val)
        case ('path')
            deps(found)%path = trim(str_val)
        case ('version')
            deps(found)%version = trim(str_val)
        end select
    end subroutine parse_dep_entry

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
        dep%rev = ''
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
                case ('rev')
                    dep%rev = trim(str_val)
                case ('path')
                    dep%path = trim(str_val)
                end select
            end do
        else
            call extract_string(val, str_val)
            dep%version = trim(str_val)
        end if
    end subroutine parse_dep

    subroutine add_link_lib(config, lib)
        !! Append a link library unless it is already listed. Order is kept, so
        !! a library the root package named stays where the root put it and a
        !! library inherited from a dependency lands after it.
        type(fpm_config_t), intent(inout) :: config
        character(len=*), intent(in) :: lib

        integer :: i

        if (len_trim(lib) == 0) return
        do i = 1, config%n_link_libs
            if (trim(config%link_libs(i)) == trim(lib)) return
        end do
        if (config%n_link_libs >= MAX_LINK_LIBS) return
        config%n_link_libs = config%n_link_libs + 1
        config%link_libs(config%n_link_libs) = trim(lib)
    end subroutine add_link_lib

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

    subroutine parse_external_modules(val, config)
        character(len=*), intent(in) :: val
        type(fpm_config_t), intent(inout) :: config

        integer :: pos, start, n
        character(len=128) :: name
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
                    name = val(start:pos - 1)
                    if (len_trim(name) > 0 .and. &
                        config%n_external_modules < MAX_EXTERNAL_MODULES) then
                        config%n_external_modules = config%n_external_modules + 1
                        config%external_modules(config%n_external_modules) = &
                            trim(name)
                    end if
                end if
            end if
            pos = pos + 1
        end do
    end subroutine parse_external_modules

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

    subroutine parse_test_args(name, val, config)
        character(len=*), intent(in) :: name, val
        type(fpm_config_t), intent(inout) :: config

        integer :: pos, start, n, slot
        character(len=512) :: arg
        logical :: in_str

        if (config%n_test_arg_sets >= MAX_TEST_ARG_SETS) return
        config%n_test_arg_sets = config%n_test_arg_sets + 1
        slot = config%n_test_arg_sets
        config%test_arg_sets(slot)%name = trim(name)
        config%test_arg_sets(slot)%args = ''
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
                    arg = val(start:pos - 1)
                    if (len_trim(config%test_arg_sets(slot)%args) > 0) then
                        config%test_arg_sets(slot)%args = &
                            trim(config%test_arg_sets(slot)%args)//new_line('a')
                    end if
                    config%test_arg_sets(slot)%args = &
                        trim(config%test_arg_sets(slot)%args)//trim(arg)
                end if
            end if
            pos = pos + 1
        end do
    end subroutine parse_test_args

    subroutine parse_preprocess(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        character(len=1024) :: macros

        if (trim(key) /= 'macros' .and. trim(key) /= 'cpp.macros') return
        macros = val
        call parse_macro_flags(macros, config)
    end subroutine parse_preprocess

    subroutine parse_macro_flags(val, config)
        character(len=*), intent(in) :: val
        type(fpm_config_t), intent(inout) :: config

        integer :: pos, start, n
        character(len=128) :: macro
        logical :: in_str

        pos = 1
        n = len_trim(val)
        in_str = .false.
        if (index(val, '"') > 0 .and. config%n_flags < MAX_FLAGS) then
            config%n_flags = config%n_flags + 1
            config%flags(config%n_flags) = '-cpp'
        end if
        do while (pos <= n)
            if (val(pos:pos) == '"') then
                if (.not. in_str) then
                    in_str = .true.
                    start = pos + 1
                else
                    in_str = .false.
                    macro = val(start:pos - 1)
                    if (len_trim(macro) > 0 .and. config%n_flags < MAX_FLAGS) then
                        config%n_flags = config%n_flags + 1
                        config%flags(config%n_flags) = '-D'//trim(macro)
                    end if
                end if
            end if
            pos = pos + 1
        end do
    end subroutine parse_macro_flags

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

    subroutine resolve_metapackages(config, ierr)
        !! Resolve every metapackage currently understood by fpm.  The parser
        !! deliberately records requests first and performs discovery only
        !! after the complete manifest is known, because stdlib's configuration
        !! depends on whether the BLAS metapackage was requested as well.
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr

        ierr = 0
        if (config%blas) call resolve_blas_metapackage(config, ierr)
        if (ierr /= 0) return
        if (config%hdf5) call resolve_hdf5_metapackage(config, ierr)
        if (ierr /= 0) return
        if (config%netcdf) call resolve_netcdf_metapackage(config, ierr)
        if (ierr /= 0) return
        if (config%mpi) call resolve_mpi_metapackage(config, ierr)
        if (ierr /= 0) return
        if (config%stdlib) call resolve_stdlib_metapackage(config)
        if (config%minpack) call resolve_minpack_metapackage(config)
    end subroutine resolve_metapackages

    subroutine resolve_blas_metapackage(config, ierr)
        !! Mirror fpm's BLAS provider order and import both pkg-config link
        !! and compiler flags.  The order is intentionally fpm's order.
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr

        character(len=*), parameter :: candidates(4) = [ character(len=32) :: &
            'mkl-dynamic-lp64-tbb', 'openblas', 'blas', 'flexiblas' ]
        integer :: i
        logical :: found

        ierr = 0
        if (.not. config%blas) return
        if (.not. pkg_config_available()) then
            write (error_unit, '(a)') &
                'fo: blas metapackage requires pkg-config'
            ierr = 1
            return
        end if

        found = .false.
        do i = 1, size(candidates)
            if (.not. pkg_config_has_package(trim(candidates(i)))) cycle
            call import_pkg_config_package(config, trim(candidates(i)), ierr)
            if (ierr /= 0) return
            write (error_unit, '(a,a)') &
                'fo: found blas package: ', trim(candidates(i))
            found = .true.
            exit
        end do
        if (.not. found) then
            write (error_unit, '(a)') &
                'fo: pkg-config could not find a suitable blas package'
            ierr = 1
        end if
    end subroutine resolve_blas_metapackage

    subroutine resolve_hdf5_metapackage(config, ierr)
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr

        character(len=*), parameter :: candidates(7) = [ character(len=32) :: &
            'hdf5_hl_fortran', 'hdf5-hl-fortran', 'hdf5_fortran', &
            'hdf5-fortran', 'hdf5_hl', 'hdf5', 'hdf5-serial' ]
        character(len=128) :: package
        integer :: i

        ierr = 0
        if (.not. pkg_config_available()) then
            write (error_unit, '(a)') 'fo: hdf5 metapackage requires pkg-config'
            ierr = 1
            return
        end if

        package = ''
        do i = 1, size(candidates)
            if (pkg_config_has_package(trim(candidates(i)))) then
                package = trim(candidates(i))
                exit
            end if
        end do
        if (len_trim(package) == 0) then
            call pkg_config_find_package('hdf5', package)
        end if
        if (len_trim(package) == 0) then
            write (error_unit, '(a)') &
                'fo: pkg-config could not find a suitable hdf5 package'
            ierr = 1
            return
        end if

        call import_pkg_config_package(config, trim(package), ierr)
        if (ierr /= 0) return
        call add_hdf5_external_modules(config)
        write (error_unit, '(a,a)') 'fo: found hdf5 package: ', trim(package)
    end subroutine resolve_hdf5_metapackage

    subroutine resolve_netcdf_metapackage(config, ierr)
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr

        ierr = 0
        if (.not. pkg_config_available()) then
            write (error_unit, '(a)') 'fo: netcdf metapackage requires pkg-config'
            ierr = 1
            return
        end if
        if (.not. pkg_config_has_package('netcdf')) then
            write (error_unit, '(a)') &
                'fo: pkg-config could not find a suitable netcdf package'
            ierr = 1
            return
        end if
        if (.not. pkg_config_has_package('netcdf-fortran')) then
            write (error_unit, '(a)') &
                'fo: pkg-config could not find a suitable netcdf-fortran package'
            ierr = 1
            return
        end if

        call import_pkg_config_package(config, 'netcdf', ierr)
        if (ierr /= 0) return
        call import_pkg_config_package(config, 'netcdf-fortran', ierr)
        if (ierr /= 0) return
        call add_netcdf_external_modules(config)
        write (error_unit, '(a)') 'fo: found netcdf packages: netcdf netcdf-fortran'
    end subroutine resolve_netcdf_metapackage

    subroutine resolve_mpi_metapackage(config, ierr)
        !! Import the flags emitted by the local MPI wrapper.  OpenMPI uses
        !! --showme, MPICH uses -compile-info/-link-info, and older wrappers
        !! expose the complete command through -show; these are the same query
        !! families fpm probes in its MPI metapackage.
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr

        character(len=*), parameter :: wrappers(6) = [ character(len=32) :: &
            'mpifort', 'mpif90', 'mpiifx', 'mpifort.openmpi', 'mpif90.openmpi', 'ftn' ]
        character(len=128) :: wrapper
        character(len=4096) :: output
        logical :: found
        integer :: i

        ierr = 0
        wrapper = ''
        do i = 1, size(wrappers)
            call command_run(trim(wrappers(i)), '--version', output, ierr)
            if (ierr == 0) then
                wrapper = trim(wrappers(i))
                exit
            end if
        end do
        found = len_trim(wrapper) > 0 .and. ierr == 0
        if (.not. found) then
            write (error_unit, '(a)') 'fo: cannot find an MPI wrapper compiler'
            ierr = 1
            return
        end if

        call import_mpi_wrapper_flags(config, trim(wrapper), 'compile', found)
        if (.not. found) then
            write (error_unit, '(a,a)') &
                'fo: MPI wrapper cannot report compile flags: ', trim(wrapper)
            ierr = 1
            return
        end if
        call import_mpi_wrapper_flags(config, trim(wrapper), 'link', found)
        if (.not. found) then
            write (error_unit, '(a,a)') &
                'fo: MPI wrapper cannot report link flags: ', trim(wrapper)
            ierr = 1
            return
        end if
        call add_external_module(config, 'mpi')
        call add_external_module(config, 'mpi_f08')
        write (error_unit, '(a,a)') 'fo: found MPI wrapper: ', trim(wrapper)
    end subroutine resolve_mpi_metapackage

    subroutine resolve_stdlib_metapackage(config)
        type(fpm_config_t), intent(inout) :: config

        call add_git_dependency(config%deps, config%n_deps, 'stdlib', &
            'https://github.com/fortran-lang/stdlib', branch='stdlib-fpm')
        call add_git_dependency(config%dev_deps, config%n_dev_deps, 'test-drive', &
            'https://github.com/fortran-lang/test-drive', branch='v0.4.0')
        if (config%blas) then
            call append_config_flag('-DSTDLIB_EXTERNAL_BLAS', config)
            call append_config_flag('-DSTDLIB_EXTERNAL_LAPACK', config)
        end if
    end subroutine resolve_stdlib_metapackage

    subroutine resolve_minpack_metapackage(config)
        type(fpm_config_t), intent(inout) :: config

        call add_git_dependency(config%deps, config%n_deps, 'minpack', &
            'https://github.com/fortran-lang/minpack', tag='v2.0.0-rc.1')
    end subroutine resolve_minpack_metapackage

    subroutine add_git_dependency(deps, n_deps, name, url, branch, tag)
        type(fpm_dep_t), intent(inout) :: deps(:)
        integer, intent(inout) :: n_deps
        character(len=*), intent(in) :: name, url
        character(len=*), intent(in), optional :: branch, tag
        integer :: i

        do i = 1, n_deps
            if (trim(deps(i)%name) == trim(name)) return
        end do
        if (n_deps >= size(deps)) return
        n_deps = n_deps + 1
        call parse_dep(name, '', deps(n_deps))
        deps(n_deps)%git = trim(url)
        if (present(branch)) deps(n_deps)%branch = trim(branch)
        if (present(tag)) deps(n_deps)%tag = trim(tag)
    end subroutine add_git_dependency

    subroutine import_pkg_config_package(config, package, ierr)
        type(fpm_config_t), intent(inout) :: config
        character(len=*), intent(in) :: package
        integer, intent(out) :: ierr
        character(len=4096) :: output
        integer :: exitcode

        ierr = 0
        call pkg_config_run(trim(package), '--libs', output, exitcode)
        if (exitcode /= 0) then
            write (error_unit, '(a,a)') &
                'fo: pkg-config could not read package ', trim(package)
            ierr = 1
            return
        end if
        call append_pkg_config_flags(output, config)

        call pkg_config_run(trim(package), '--cflags', output, exitcode)
        if (exitcode /= 0) then
            write (error_unit, '(a,a)') &
                'fo: pkg-config could not read compiler flags for ', trim(package)
            ierr = 1
            return
        end if
        call append_pkg_config_flags(output, config)
    end subroutine import_pkg_config_package

    logical function pkg_config_available()
        character(len=64) :: output
        integer :: exitcode

        call pkg_config_run('', '--version', output, exitcode)
        pkg_config_available = exitcode == 0
    end function pkg_config_available

    logical function pkg_config_has_package(package)
        character(len=*), intent(in) :: package
        character(len=64) :: output
        integer :: exitcode

        call pkg_config_run(trim(package), '--exists', output, exitcode)
        pkg_config_has_package = exitcode == 0
    end function pkg_config_has_package

    subroutine pkg_config_find_package(prefix, package)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: package
        character(len=4096) :: output
        character(len=256) :: line, word
        integer :: exitcode, i, start, finish

        package = ''
        call pkg_config_run('', '--list-all', output, exitcode)
        if (exitcode /= 0) return
        start = 1
        do i = 1, len_trim(output) + 1
            if (i <= len_trim(output) .and. output(i:i) /= new_line('a')) cycle
            finish = i - 1
            if (finish >= start) then
                line = output(start:finish)
                call first_word(line, word)
                if (index(trim(word), trim(prefix)) == 1) then
                    package = trim(word)
                    return
                end if
            end if
            start = i + 1
        end do
    end subroutine pkg_config_find_package

    subroutine first_word(line, word)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: word
        integer :: i, n

        word = ''
        n = len_trim(line)
        i = 1
        do while (i <= n .and. is_space(line(i:i)))
            i = i + 1
        end do
        if (i > n) return
        word = line(i:min(n, i + len(word) - 1))
    end subroutine first_word

    subroutine add_hdf5_external_modules(config)
        type(fpm_config_t), intent(inout) :: config
        character(len=*), parameter :: names(22) = [ character(len=16) :: &
            'h5a', 'h5d', 'h5es', 'h5e', 'h5f', 'h5g', 'h5i', 'h5l', &
            'h5o', 'h5p', 'h5r', 'h5s', 'h5t', 'h5vl', 'h5z', 'h5lt', &
            'h5lib', 'h5global', 'h5_gen', 'h5fortkit', 'hdf5', 'h5']
        integer :: i

        do i = 1, size(names)
            call add_external_module(config, trim(names(i)))
        end do
    end subroutine add_hdf5_external_modules

    subroutine add_netcdf_external_modules(config)
        type(fpm_config_t), intent(inout) :: config
        character(len=*), parameter :: names(10) = [ character(len=32) :: &
            'netcdf', 'netcdf4_f03', 'netcdf4_nc_interfaces', &
            'netcdf4_nf_interfaces', 'netcdf_f03', &
            'netcdf_fortv2_c_interfaces', 'netcdf_nc_data', &
            'netcdf_nc_interfaces', 'netcdf_nf_data', 'netcdf_nf_interfaces']
        integer :: i

        do i = 1, size(names)
            call add_external_module(config, trim(names(i)))
        end do
    end subroutine add_netcdf_external_modules

    subroutine add_external_module(config, name)
        type(fpm_config_t), intent(inout) :: config
        character(len=*), intent(in) :: name
        integer :: i

        do i = 1, config%n_external_modules
            if (trim(config%external_modules(i)) == trim(name)) return
        end do
        if (config%n_external_modules >= MAX_EXTERNAL_MODULES) return
        config%n_external_modules = config%n_external_modules + 1
        config%external_modules(config%n_external_modules) = trim(name)
    end subroutine add_external_module

    subroutine import_mpi_wrapper_flags(config, wrapper, kind, found)
        type(fpm_config_t), intent(inout) :: config
        character(len=*), intent(in) :: wrapper, kind
        logical, intent(out) :: found
        character(len=*), parameter :: compile_options(3) = [ character(len=32) :: &
            '--showme:compile', '-compile-info', '-show' ]
        character(len=*), parameter :: link_options(3) = [ character(len=32) :: &
            '--showme:link', '-link-info', '-show' ]
        character(len=32) :: option
        character(len=4096) :: output
        integer :: i, exitcode

        found = .false.
        do i = 1, 3
            if (trim(kind) == 'compile') then
                option = compile_options(i)
            else
                option = link_options(i)
            end if
            call command_run(trim(wrapper), trim(option), output, exitcode)
            if (exitcode /= 0) cycle
            call append_command_flags(output, config, &
                skip_command=(index(trim(option), 'showme') == 0))
            found = .true.
            return
        end do
    end subroutine import_mpi_wrapper_flags

    subroutine command_run(command, option, output, exitcode)
        character(len=*), intent(in) :: command, option
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode
        character(len=512) :: tmpfile
        character(len=:), allocatable :: packed
        integer :: n_args

        output = ''
        call make_tmpfile('fo-command', tmpfile)
        n_args = 0
        call argv_push(packed, n_args, trim(command))
        if (len_trim(option) > 0) call argv_push(packed, n_args, trim(option))
        call process_run_argv_logged('', packed, n_args, trim(tmpfile), .false., &
            30, exitcode)
        if (exitcode == 0) call read_text_file(trim(tmpfile), output)
        call delete_tmpfile(trim(tmpfile))
    end subroutine command_run

    subroutine pkg_config_run(package, option, output, exitcode)
        character(len=*), intent(in) :: package, option
        character(len=*), intent(out) :: output
        integer, intent(out) :: exitcode

        character(len=512) :: tmpfile
        character(len=:), allocatable :: packed
        integer :: n_args

        output = ''
        call make_tmpfile('fo-pkg-config', tmpfile)
        n_args = 0
        call argv_push(packed, n_args, 'pkg-config')
        if (len_trim(option) > 0) call argv_push(packed, n_args, trim(option))
        if (len_trim(package) > 0) call argv_push(packed, n_args, trim(package))
        call process_run_argv_logged('', packed, n_args, trim(tmpfile), .false., &
            30, exitcode)
        if (exitcode == 0) call read_text_file(trim(tmpfile), output)
        call delete_tmpfile(trim(tmpfile))
    end subroutine pkg_config_run

    subroutine append_pkg_config_flags(output, config)
        character(len=*), intent(in) :: output
        type(fpm_config_t), intent(inout) :: config

        integer :: i, n, start, finish
        character(len=256) :: token

        n = len_trim(output)
        i = 1
        do while (i <= n)
            do while (i <= n .and. is_space(output(i:i)))
                i = i + 1
            end do
            if (i > n) exit
            start = i
            do while (i <= n .and. .not. is_space(output(i:i)))
                i = i + 1
            end do
            finish = min(i - 1, start + len(token) - 1)
            token = ''
            token(:finish - start + 1) = output(start:finish)
            call append_provider_token(trim(token), config)
        end do
    end subroutine append_pkg_config_flags

    subroutine append_command_flags(output, config, skip_command)
        character(len=*), intent(in) :: output
        type(fpm_config_t), intent(inout) :: config
        logical, intent(in) :: skip_command

        integer :: i, n, start, finish
        logical :: skipped
        character(len=256) :: token

        n = len_trim(output)
        i = 1
        skipped = .not. skip_command
        do while (i <= n)
            do while (i <= n .and. is_space(output(i:i)))
                i = i + 1
            end do
            if (i > n) exit
            start = i
            do while (i <= n .and. .not. is_space(output(i:i)))
                i = i + 1
            end do
            finish = min(i - 1, start + len(token) - 1)
            token = ''
            token(:finish - start + 1) = output(start:finish)
            if (.not. skipped) then
                skipped = .true.
            else
                call append_provider_token(trim(token), config)
            end if
        end do
    end subroutine append_command_flags

    subroutine append_provider_token(token, config)
        character(len=*), intent(in) :: token
        type(fpm_config_t), intent(inout) :: config

        if (len_trim(token) == 0) return
        if (len_trim(token) >= 3 .and. token(1:2) == '-l') then
            call add_link_lib(config, trim(token(3:)))
        else if (token(1:1) == '-') then
            ! fpm separates link flags and build flags; fo's compiler backend
            ! uses one translated stream for both phases.  These options are
            ! valid in both contexts and preserve provider-specific paths,
            ! rpaths, pthread/OpenMP settings, and wrapper flags.
            call append_config_flag(trim(token), config)
        end if
    end subroutine append_provider_token

    logical pure function is_space(ch)
        character(len=1), intent(in) :: ch

        is_space = ch == ' ' .or. ch == char(9) .or. ch == char(10) .or. &
            ch == char(13)
    end function is_space

end module fo_fpm_config
