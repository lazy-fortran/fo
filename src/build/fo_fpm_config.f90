module fo_fpm_config
    use fx_hash, only: sha256_string
    use fo_fs, only: fs_path_is_absolute, fs_native_path, fs_parent_path, &
        fs_realpath, fs_identity
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_process, only: process_run_argv_logged, argv_push
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: fpm_dep_t, fpm_exe_t, fpm_config_t
    public :: fpm_config_init, fpm_config_parse, fpm_config_allocate
    public :: dep_kind, DEP_PATH, DEP_GIT, DEP_REGISTRY
    public :: valid_registry_version
    public :: manifest_exe_name, manifest_test_name, manifest_example_name
    public :: manifest_executable_selected
    public :: manifest_test_args
    public :: MAX_LINK_LIBS, add_link_lib
    public :: MAX_EXTERNAL_MODULES
    public :: valid_manifest_module_name, absolute_dependency_destination

    ! How a dependency is acquired, derived from which fields the manifest set.
    ! path = local dir (mutable, may be edited); git = cloned at a ref (pinned,
    ! immutable); registry = namespace/name with an optional exact numeric v,
    ! resolved from configured local registry sources through the native DAG.
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
    integer, parameter :: MAX_FO_INPUTS = 256

    type :: fpm_dep_t
        character(len=256) :: name = ''
        character(len=256) :: namespace = ''
        character(len=128) :: unsupported_field = ''
        character(len=512) :: git = ''
        character(len=128) :: branch = ''
        character(len=128) :: tag = ''
        character(len=:), allocatable :: rev
        character(len=512) :: path = ''
        character(len=64) :: registry_v = ''
        logical :: namespace_seen = .false.
        logical :: registry_v_seen = .false.
        logical :: path_seen = .false.
        logical :: git_seen = .false.
        logical :: branch_seen = .false.
        logical :: tag_seen = .false.
        logical :: rev_seen = .false.
    end type fpm_dep_t

    ! An explicit [[executable]] entry. fpm names the built binary after `name`,
    ! not after the source stem, so `main = "main.f90"` under `name = "demo"`
    ! links to `demo`, not the package name.
    type :: fpm_exe_t
        character(len=1024) :: name = ''
        character(len=256) :: main = 'main.f90'
        character(len=256) :: source_dir = 'app'
    end type fpm_exe_t

    type :: fpm_test_args_t
        character(len=1024) :: name = ''
        character(len=:), allocatable :: args
    end type fpm_test_args_t

    type, public :: fpm_input_t
        character(len=512) :: path = ''
        character(len=64) :: role = ''
        character(len=256) :: root = ''
        character(len=16) :: kind = 'file'
        logical :: writable_at_execution = .false.
        logical :: path_seen = .false.
        logical :: role_seen = .false.
        logical :: root_seen = .false.
        logical :: kind_seen = .false.
        logical :: writable_seen = .false.
    end type fpm_input_t

    type :: fpm_config_t
        character(len=1024) :: name = ''
        character(len=32)  :: version = ''
        character(len=256) :: source_dir = 'src'
        integer :: n_include_dirs = 1
        character(len=256) :: include_dirs(64) = 'include'
        character(len=256) :: app_dir = 'app'
        character(len=256) :: test_dir = 'test'
        character(len=256) :: example_dir = 'example'
        ! Project roots follow the native scanner's path limit, not token size.
        character(len=512) :: project_dir = '.'
        logical :: auto_executables = .true.
        logical :: auto_tests = .true.
        logical :: auto_examples = .true.
        logical :: module_naming = .false.
        character(len=63) :: module_prefix = ''
        logical :: install_library = .false.
        logical :: install_test = .false.
        character(len=256) :: install_module_dir = ''
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
        integer :: n_fo_inputs = 0
        type(fpm_input_t) :: fo_inputs(MAX_FO_INPUTS)
        character(len=512) :: fo_input_parse_error = ''
        character(len=512) :: dependency_parse_error = ''
        character(len=512) :: manifest_parse_error = ''
        ! [extra.fo] test budgets in seconds; 0 leaves fo's default. The
        ! FO_TEST_TIMEOUT, FO_SLOW_TEST_TIMEOUT and FO_TEST_WALL_TIMEOUT
        ! environment variables override these per invocation.
        integer :: test_timeout = 0
        integer :: slow_test_timeout = 0
        integer :: test_wall_timeout = 0
        !> [extra.fo] debug-info = "g0" | "line-tables" | "full": how much
        !> debug info the default profile emits. A project declares its own
        !> budget here so the diet is visible in the manifest, not only in fo's
        !> defaults. Empty leaves fo's default (`g0`).
        character(len=32) :: debug_info = ''
        logical :: pic = .false.
        logical :: link_shared = .false.
        !! Name of the consolidated test executable, if the project uses one
        !! (`[extra.fo] dispatcher = "ffc_suite"`). Tests whose source carries
        !! the `! fo: dispatcher` marker run inside that binary with their name
        !! as the argument instead of linking their own copy of the library:
        !! one link instead of one per test. Empty means no dispatcher and the
        !! behaviour is exactly as before.
        character(len=64) :: dispatcher = ''
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
        character(len=7) :: source_form = 'free'
    end type fpm_config_t

contains

    pure function dep_kind(dep) result(kind)
        !! Classify a parsed dependency by which source field the manifest set.
        !! The parser rejects conflicting sources before classification.
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

    subroutine fpm_config_allocate(config)
        !! Isolate compiler default-initialization scratch from caller frames.
        !! Traversal retains heap configs across recursive calls; allocating
        !! them inline also retains a large compiler temporary on the stack.
        type(fpm_config_t), allocatable, intent(out) :: config

        allocate (config)
    end subroutine fpm_config_allocate

    subroutine fpm_config_init(c)
        type(fpm_config_t), intent(inout) :: c
        integer :: i

        c%name = ''
        c%version = ''
        c%source_dir = 'src'
        c%n_include_dirs = 1
        c%include_dirs = ''
        c%include_dirs(1) = 'include'
        c%app_dir = 'app'
        c%test_dir = 'test'
        c%example_dir = 'example'
        c%project_dir = '.'
        c%auto_executables = .true.
        c%auto_tests = .true.
        c%auto_examples = .true.
        c%module_naming = .false.
        c%module_prefix = ''
        c%install_library = .false.
        c%install_test = .false.
        c%install_module_dir = ''
        c%openmp = .false.
        c%blas = .false.
        c%mpi = .false.
        c%hdf5 = .false.
        c%netcdf = .false.
        c%stdlib = .false.
        c%minpack = .false.
        c%implicit_typing = .false.
        c%implicit_external = .false.
        c%source_form = 'free'
        c%n_deps = 0
        c%n_dev_deps = 0
        do i = 1, MAX_DEPS
            c%deps(i)%name = ''
            c%deps(i)%git = ''
            c%deps(i)%branch = ''
            c%deps(i)%tag = ''
            if (allocated(c%deps(i)%rev)) deallocate (c%deps(i)%rev)
            c%deps(i)%path = ''
            c%deps(i)%namespace = ''
            c%deps(i)%unsupported_field = ''
            c%deps(i)%registry_v = ''
            c%deps(i)%namespace_seen = .false.
            c%deps(i)%registry_v_seen = .false.
            c%deps(i)%path_seen = .false.
            c%deps(i)%git_seen = .false.
            c%deps(i)%branch_seen = .false.
            c%deps(i)%tag_seen = .false.
            c%deps(i)%rev_seen = .false.
        end do
        do i = 1, MAX_DEV_DEPS
            c%dev_deps(i)%name = ''
            c%dev_deps(i)%git = ''
            c%dev_deps(i)%branch = ''
            c%dev_deps(i)%tag = ''
            if (allocated(c%dev_deps(i)%rev)) deallocate (c%dev_deps(i)%rev)
            c%dev_deps(i)%path = ''
            c%dev_deps(i)%namespace = ''
            c%dev_deps(i)%unsupported_field = ''
            c%dev_deps(i)%registry_v = ''
            c%dev_deps(i)%namespace_seen = .false.
            c%dev_deps(i)%registry_v_seen = .false.
            c%dev_deps(i)%path_seen = .false.
            c%dev_deps(i)%git_seen = .false.
            c%dev_deps(i)%branch_seen = .false.
            c%dev_deps(i)%tag_seen = .false.
            c%dev_deps(i)%rev_seen = .false.
        end do
        c%n_link_libs = 0
        c%link_libs = ''
        c%n_external_modules = 0
        c%external_modules = ''
        c%n_flags = 0
        c%flags = ''
        c%n_exes = 0
        do i = 1, MAX_EXES
            c%exes(i)%name = ''
            c%exes(i)%main = 'main.f90'
            c%exes(i)%source_dir = 'app'
            c%tests(i)%name = ''
            c%tests(i)%main = 'main.f90'
            c%tests(i)%source_dir = 'app'
            c%examples(i)%name = ''
            c%examples(i)%main = 'main.f90'
            c%examples(i)%source_dir = 'app'
        end do
        c%n_tests = 0
        c%n_examples = 0
        c%n_test_arg_sets = 0
        do i = 1, MAX_TEST_ARG_SETS
            c%test_arg_sets(i)%name = ''
            if (allocated(c%test_arg_sets(i)%args)) &
                deallocate (c%test_arg_sets(i)%args)
        end do
        c%n_fo_inputs = 0
        do i = 1, MAX_FO_INPUTS
            c%fo_inputs(i)%path = ''
            c%fo_inputs(i)%role = ''
            c%fo_inputs(i)%root = ''
            c%fo_inputs(i)%kind = 'file'
            c%fo_inputs(i)%writable_at_execution = .false.
            c%fo_inputs(i)%path_seen = .false.
            c%fo_inputs(i)%role_seen = .false.
            c%fo_inputs(i)%root_seen = .false.
            c%fo_inputs(i)%kind_seen = .false.
            c%fo_inputs(i)%writable_seen = .false.
        end do
        c%fo_input_parse_error = ''
        c%dependency_parse_error = ''
        c%manifest_parse_error = ''
        c%test_timeout = 0
        c%slow_test_timeout = 0
        c%test_wall_timeout = 0
        c%debug_info = ''
        c%pic = .false.
        c%link_shared = .false.
        c%dispatcher = ''
    end subroutine fpm_config_init

    subroutine fpm_config_parse(project_dir, config, ierr)
        character(len=*), intent(in) :: project_dir
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr

        character(len=1025) :: line
        character(len=1024) :: key, val, section
        ! accumulated value for multi-line arrays (flags = [\n  "...",\n])
        character(len=4096) :: accum
        logical :: in_array
        character(len=1024) :: pending_key
        integer :: u, ios, i

        call fpm_config_init(config)
        ierr = 0
        if (len_trim(project_dir) > len(config%project_dir)) then
            write (error_unit, '(a)') 'fo: project directory exceeds supported path length'
            ierr = 1
            return
        end if
        config%project_dir = trim(project_dir)
        section = ''
        in_array = .false.
        accum = ''
        pending_key = ''

        open (newunit=u, file=trim(project_dir)//'/fpm.toml', &
            status='old', iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(a)') 'fo: cannot open dependency/project manifest: '// &
                trim(project_dir)//'/fpm.toml'
            ierr = 1
            return
        end if

        do
            if (len_trim(config%manifest_parse_error) > 0) then
                write (error_unit, '(a)') 'fo: fpm.toml: '// &
                    trim(config%manifest_parse_error)
                ierr = 1
                close (u)
                return
            end if
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == len(line)) then
                write (error_unit, '(a)') &
                    'fo: fpm.toml line exceeds 1024 characters'
                ierr = 1
                close (u)
                return
            end if
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
                    case ('library')
                        call parse_library(pending_key, val, config)
                    case ('executable')
                        if (config%n_exes > 0) call parse_exe(pending_key, val, &
                            config%exes(config%n_exes), config%manifest_parse_error)
                    case ('test')
                        if (config%n_tests > 0) call parse_exe(pending_key, val, &
                            config%tests(config%n_tests), config%manifest_parse_error)
                    case ('example')
                        if (config%n_examples > 0) call parse_exe(pending_key, val, &
                            config%examples(config%n_examples), &
                            config%manifest_parse_error)
                    case ('extra.fo.test-args')
                        call parse_test_args(pending_key, val, config)
                    case ('extra.fo.inputs')
                        config%fo_input_parse_error = &
                            'fixture input fields must be scalar values'
                    end select
                end if
                cycle
            end if

            if (line(1:1) == '[') then
                call get_section(line, section)
                select case (trim(section))
                case ('executable')
                    if (config%n_exes >= MAX_EXES) write ( &
                        config%manifest_parse_error, '(a,i0,a)') &
                        '[[executable]] exceeds supported limit ', MAX_EXES, &
                        ' declarations'
                case ('test')
                    if (config%n_tests >= MAX_EXES) write ( &
                        config%manifest_parse_error, '(a,i0,a)') &
                        '[[test]] exceeds supported limit ', MAX_EXES, ' declarations'
                case ('example')
                    if (config%n_examples >= MAX_EXES) write ( &
                        config%manifest_parse_error, '(a,i0,a)') &
                        '[[example]] exceeds supported limit ', MAX_EXES, &
                        ' declarations'
                end select
                if (len_trim(config%manifest_parse_error) > 0) cycle
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
                else if (trim(section) == 'extra.fo.inputs') then
                    if (config%n_fo_inputs >= MAX_FO_INPUTS) then
                        write (config%manifest_parse_error, '(a,i0,a)') &
                            '[[extra.fo.inputs]] exceeds supported limit ', &
                            MAX_FO_INPUTS, ' declarations'
                        cycle
                    end if
                    config%n_fo_inputs = config%n_fo_inputs + 1
                    config%fo_inputs(config%n_fo_inputs) = fpm_input_t()
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
            case ('install')
                call parse_install(key, val, config)
            case ('dependencies')
                call parse_dependency_entry(key, val, config)
            case ('dev-dependencies')
                call parse_dep_entry(key, val, config%dev_deps, config%n_dev_deps, &
                    config%manifest_parse_error)
            case ('test.dependencies')
                if (config%n_tests == 0) then
                    config%manifest_parse_error = &
                        '[test.dependencies] requires a preceding [[test]]'
                else
                    call parse_dep_entry(key, val, config%dev_deps, config%n_dev_deps, &
                        config%manifest_parse_error)
                end if
            case ('executable.dependencies', 'example.dependencies')
                config%manifest_parse_error = 'unsupported ['//trim(section)// &
                    '] dependency declaration; move dependencies to [dependencies]'
            case ('executable')
                if (config%n_exes > 0) &
                    call parse_exe(key, val, config%exes(config%n_exes), &
                        config%manifest_parse_error)
            case ('test')
                if (config%n_tests > 0) &
                    call parse_exe(key, val, config%tests(config%n_tests), &
                        config%manifest_parse_error)
            case ('example')
                if (config%n_examples > 0) &
                    call parse_exe(key, val, config%examples(config%n_examples), &
                        config%manifest_parse_error)
            case ('preprocess', 'preprocess.cpp')
                call parse_preprocess(key, val, config)
            case ('extra.fo.test-args')
                call parse_test_args(key, val, config)
            case ('extra.fo')
                call parse_extra_fo(key, val, config)
            case ('extra.fo.inputs')
                call parse_fo_input(key, val, config)
            case ('extra')
                if (index(key, 'fo.') == 1) &
                    call parse_extra_fo(key(4:), val, config)
            case ('library')
                call parse_library(key, val, config)
            case ('fortran')
                call parse_fortran(key, val, config)
            end select
        end do

        close (u)
        if (len_trim(config%fo_input_parse_error) > 0) then
            ierr = 1
            return
        end if
        do i = 1, config%n_deps
            call validate_dependency(config%deps(i), 'dependencies', &
                config%dependency_parse_error)
            if (len_trim(config%dependency_parse_error) > 0) exit
        end do
        if (len_trim(config%dependency_parse_error) == 0) then
            do i = 1, config%n_dev_deps
                call validate_dependency(config%dev_deps(i), 'dev-dependencies', &
                    config%dependency_parse_error)
                if (len_trim(config%dependency_parse_error) > 0) exit
            end do
        end if
        if (len_trim(config%dependency_parse_error) > 0) then
            write (error_unit, '(a)') 'fo: fpm.toml: '// &
                trim(config%dependency_parse_error)
            ierr = 1
            return
        end if
        call relocate_frozen_absolute_dependencies(config, ierr)
        if (ierr /= 0) then
            write (error_unit, '(a)') 'fo: fpm.toml: '// &
                trim(config%manifest_parse_error)
            return
        end if
        if (ierr == 0) call resolve_metapackages(config, ierr)
        if (len_trim(config%manifest_parse_error) > 0) then
            write (error_unit, '(a)') 'fo: fpm.toml: '// &
                trim(config%manifest_parse_error)
            ierr = 1
        end if
    end subroutine fpm_config_parse

    function absolute_dependency_destination(path) result(destination)
        character(len=*), intent(in) :: path
        character(len=96) :: destination

        destination = 'build/dependencies/.fo-path/'// &
            sha256_string(fs_native_path(trim(path)))
    end function absolute_dependency_destination

    subroutine relocate_frozen_absolute_dependencies(config, ierr)
        type(fpm_config_t), intent(inout) :: config
        integer, intent(out) :: ierr
        character(len=512) :: frozen, destination
        integer :: i, status, length
        logical :: has_absolute

        ierr = 0
        has_absolute = .false.
        do i = 1, config%n_deps
            if (dep_kind(config%deps(i)) /= DEP_PATH) cycle
            if (fs_path_is_absolute(config%deps(i)%path)) has_absolute = .true.
        end do
        do i = 1, config%n_dev_deps
            if (dep_kind(config%dev_deps(i)) /= DEP_PATH) cycle
            if (fs_path_is_absolute(config%dev_deps(i)%path)) has_absolute = .true.
        end do
        if (.not. has_absolute) return
        call get_environment_variable('FO_GREMLIN_FROZEN_ROOT', frozen, length, status)
        if (status == 1 .or. length == 0) return
        if (status /= 0 .or. .not. fs_path_is_absolute(trim(frozen))) then
            config%manifest_parse_error = 'invalid or too long frozen dependency root'
            ierr = 1
            return
        end if
        ! Only captured packages share this private bundle. Tests may create
        ! unrelated projects in their own TMPDIR while inheriting this marker.
        if (.not. package_in_frozen_bundle(config%project_dir, frozen)) return
        do i = 1, config%n_deps + config%n_dev_deps
            if (i <= config%n_deps) then
                if (.not. fs_path_is_absolute(config%deps(i)%path)) cycle
                destination = absolute_dependency_destination(config%deps(i)%path)
            else
                if (.not. fs_path_is_absolute( &
                    config%dev_deps(i - config%n_deps)%path)) cycle
                destination = absolute_dependency_destination( &
                    config%dev_deps(i - config%n_deps)%path)
            end if
            if (len_trim(frozen) + len_trim(destination) + 1 > len(destination)) then
                config%manifest_parse_error = &
                    'frozen absolute dependency path exceeds supported length'
                ierr = 1
                return
            end if
            destination = trim(frozen)//'/'//trim(destination)
            if (i <= config%n_deps) then
                config%deps(i)%path = trim(destination)
            else
                config%dev_deps(i - config%n_deps)%path = trim(destination)
            end if
        end do
    end subroutine relocate_frozen_absolute_dependencies

    logical function package_in_frozen_bundle(project, frozen) result(inside)
        character(len=*), intent(in) :: project, frozen
        character(len=512) :: current, parent, bundle, temporary
        integer(c_long_long) :: device, inode, root_device, root_inode
        integer(c_long_long) :: temporary_device, temporary_inode
        integer :: depth
        logical :: ok, temporary_exists

        inside = .false.
        call fs_parent_path(frozen, bundle)
        call fs_identity(trim(bundle), root_device, root_inode, ok)
        if (.not. ok) return
        temporary = trim(bundle)//'/.fo-tmp'
        call fs_identity(trim(temporary), temporary_device, temporary_inode, &
            temporary_exists)
        call fs_realpath(project, current, ok)
        if (.not. ok) return
        do depth = 1, 64
            call fs_identity(trim(current), device, inode, ok)
            if (.not. ok) return
            if (temporary_exists) then
                if (device == temporary_device .and. inode == temporary_inode) return
            end if
            if (device == root_device .and. inode == root_inode) then
                inside = .true.
                return
            end if
            call fs_parent_path(trim(current), parent)
            if (len_trim(parent) == 0 .or. parent == current) return
            current = parent
        end do
    end function package_in_frozen_bundle

    subroutine validate_dependency(dep, section, error)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: section
        character(len=*), intent(out) :: error
        integer :: selectors

        error = ''
        selectors = 0
        if (dep%branch_seen) selectors = selectors + 1
        if (dep%tag_seen) selectors = selectors + 1
        if (dep%rev_seen) selectors = selectors + 1
        if (len_trim(dep%unsupported_field) > 0) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" has unsupported dependency field '//trim(dep%unsupported_field)
        else if (dep%namespace_seen .and. (dep%path_seen .or. dep%git_seen)) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" cannot have namespace with path or git'
        else if (dep%path_seen .and. dep%git_seen) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" cannot have both path and git'
        else if (selectors > 1) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" can have only one of branch, tag, or rev'
        else if (selectors > 0 .and. .not. dep%git_seen) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" has a Git selector without git'
        else if (dep%registry_v_seen .and. &
                (dep%path_seen .or. dep%git_seen)) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" cannot have v with path or git'
        else if (.not. dep%path_seen .and. .not. dep%git_seen .and. &
                len_trim(dep%namespace) == 0) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" requires namespace for registry resolution'
        else if (dep%registry_v_seen .and. &
                .not. valid_registry_version(trim(dep%registry_v))) then
            error = '['//section//'] dependency "'//trim(dep%name)// &
                '" has invalid registry v: "'//trim(dep%registry_v)//'"'
        end if
    end subroutine validate_dependency

    pure logical function quoted_dependency_value(value)
        character(len=*), intent(in) :: value
        character(len=len(value)) :: stripped
        integer :: n
        stripped = adjustl(value)
        n = len_trim(stripped)
        quoted_dependency_value = .false.
        if (n < 2) return
        if (stripped(1:1) /= '"' .and. stripped(1:1) /= "'") return
        quoted_dependency_value = stripped(n:n) == stripped(1:1)
    end function quoted_dependency_value

    pure logical function valid_registry_version(value)
        character(len=*), intent(in) :: value
        integer :: i, start, parts, parsed, ios

        valid_registry_version = .false.
        if (len_trim(value) == 0) return
        parts = 0
        start = 1
        do i = 1, len_trim(value) + 1
            if (i <= len_trim(value)) then
                if (value(i:i) /= '.') cycle
            end if
            if (i == start .or. parts >= 3) return
            if (verify(value(start:i - 1), '0123456789') /= 0) return
            read (value(start:i - 1), *, iostat=ios) parsed
            if (ios /= 0) return
            parts = parts + 1
            start = i + 1
        end do
        valid_registry_version = parts > 0
    end function valid_registry_version

    subroutine parse_top_level(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        character(len=1024) :: str_val

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

        if (trim(val) /= '"*"' .and. trim(val) /= "'*'") then
            call parse_dep_entry(key, val, config%deps, config%n_deps, &
                config%manifest_parse_error)
            return
        end if

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
            call parse_dep_entry(key, val, config%deps, config%n_deps, &
                config%manifest_parse_error)
        end select
    end subroutine parse_dependency_entry

    subroutine parse_build(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        character(len=1024) :: str_val

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
            call parse_boolean(val, config%auto_executables, key, &
                config%manifest_parse_error)
        case ('auto-tests')
            call parse_boolean(val, config%auto_tests, key, config%manifest_parse_error)
        case ('auto-examples')
            call parse_boolean(val, config%auto_examples, key, &
                config%manifest_parse_error)
        case ('module-naming')
            config%module_prefix = ''
            select case (trim(val))
            case ('true')
                config%module_naming = .true.
            case ('false')
                config%module_naming = .false.
            case default
                str_val = ''
                if (len_trim(val) >= 2) then
                    if (val(1:1) == '"' .or. val(1:1) == "'") then
                        if (val(len_trim(val):len_trim(val)) == val(1:1)) &
                            str_val = val(2:len_trim(val) - 1)
                    end if
                end if
                if (.not. valid_module_prefix(trim(str_val))) then
                    config%manifest_parse_error = &
                        '[build] module-naming requires a boolean or '// &
                        'an alphanumeric '// &
                        'prefix beginning with a letter (1-63 characters)'
                    return
                end if
                config%module_naming = .true.
                config%module_prefix = trim(str_val)
            end select
        case ('link')
            call parse_link_libs(val, config)
        case ('external-modules')
            call parse_external_modules(val, config)
        case ('flags')
            call parse_flags(val, config)
        case default
            config%manifest_parse_error = 'unsupported [build] field: '//trim(key)
        end select
    end subroutine parse_build

    subroutine parse_boolean(value, boolean, field, error)
        character(len=*), intent(in) :: value, field
        logical, intent(out) :: boolean
        character(len=*), intent(out) :: error
        error = ''
        boolean = .false.
        select case (trim(value))
        case ('true')
            boolean = .true.
        case ('false')
        case default
            error = trim(field)//' requires a boolean true or false'
        end select
    end subroutine parse_boolean

    pure logical function valid_module_prefix(prefix) result(valid)
        character(len=*), intent(in) :: prefix
        character(len=*), parameter :: alpha = &
            'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        valid = .false.
        if (len(prefix) < 1 .or. len(prefix) > 63) return
        if (verify(prefix(:1), alpha) /= 0) return
        valid = verify(prefix, alpha//'0123456789') == 0
    end function valid_module_prefix

    pure logical function valid_manifest_module_name(config, name) result(valid)
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: package, module_name, prefix
        integer :: i, code

        valid = .true.
        if (.not. config%module_naming) return
        package = trim(config%name)
        module_name = trim(name)
        prefix = trim(config%module_prefix)
        do i = 1, len(package)
            if (package(i:i) == '-') package(i:i) = '_'
        end do
        do i = 1, len(package)
            code = iachar(package(i:i))
            if (code >= 65 .and. code <= 90) package(i:i) = achar(code + 32)
        end do
        do i = 1, len(module_name)
            code = iachar(module_name(i:i))
            if (code >= 65 .and. code <= 90) module_name(i:i) = achar(code + 32)
        end do
        do i = 1, len(prefix)
            code = iachar(prefix(i:i))
            if (code >= 65 .and. code <= 90) prefix(i:i) = achar(code + 32)
        end do
        valid = naming_prefix_matches(module_name, package, '__')
        if (.not. valid .and. len(prefix) > 0) &
            valid = naming_prefix_matches(module_name, prefix, '_')
    end function valid_manifest_module_name

    pure logical function naming_prefix_matches(name, prefix, separator) result(valid)
        character(len=*), intent(in) :: name, prefix, separator
        integer :: n, last
        valid = .false.
        n = len(prefix)
        if (n == 0) return
        if (prefix(n:n) == '_') return
        if (name == prefix) then
            valid = .true.
            return
        end if
        last = n + len(separator)
        if (len(name) <= last) return
        valid = name(:last) == prefix//separator
    end function naming_prefix_matches

    subroutine parse_install(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config
        character(len=1024) :: str_val

        select case (trim(key))
        case ('library')
            call parse_boolean(val, config%install_library, key, &
                config%manifest_parse_error)
        case ('test')
            call parse_boolean(val, config%install_test, key, &
                config%manifest_parse_error)
        case ('module-dir')
            call extract_string(val, str_val)
            config%install_module_dir = trim(str_val)
        end select
    end subroutine parse_install

    subroutine parse_library(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config
        character(len=1024) :: str_val

        select case (trim(key))
        case ('source-dir')
            call extract_string(val, str_val)
            if (len_trim(str_val) > 0) config%source_dir = trim(str_val)
        case ('type')
            call extract_string(val, str_val)
            if (trim(str_val) /= 'static') config%manifest_parse_error = &
                'unsupported [library] type: '//trim(str_val)// &
                '; native library type currently supports "static"'
        case ('include-dir')
            call parse_include_directories(val, config)
        case default
            config%manifest_parse_error = 'unsupported [library] field: '//trim(key)
        end select
    end subroutine parse_library

    subroutine parse_include_directories(val, config)
        character(len=*), intent(in) :: val
        type(fpm_config_t), intent(inout) :: config
        character(:), allocatable :: input
        character(len=1) :: quote
        integer :: position, start, last
        logical :: array

        input = trim(adjustl(val))
        config%n_include_dirs = 0
        config%include_dirs = ''
        last = len(input)
        if (last == 0) goto 900
        array = input(1:1) == '['
        position = 1
        if (array) then
            if (input(last:last) /= ']') goto 900
            position = 2
            last = last - 1
        end if
        do while (position <= last)
            if (input(position:position) == ' ') then
                position = position + 1
                cycle
            end if
            quote = input(position:position)
            if (quote /= '"' .and. quote /= "'") goto 900
            start = position + 1
            position = start
            do while (position <= last)
                if (input(position:position) == quote) exit
                position = position + 1
            end do
            if (position > last .or. position == start) goto 900
            if (position - start > len(config%include_dirs(1))) goto 900
            if (config%n_include_dirs == size(config%include_dirs)) goto 900
            config%n_include_dirs = config%n_include_dirs + 1
            config%include_dirs(config%n_include_dirs) = input(start:position - 1)
            position = position + 1
            do while (position <= last)
                if (input(position:position) /= ' ') exit
                position = position + 1
            end do
            if (position > last) exit
            if (.not. array) goto 900
            if (input(position:position) /= ',') goto 900
            position = position + 1
        end do
        return
900     config%manifest_parse_error = &
            '[library] include-dir requires a quoted path or list of quoted paths'
    end subroutine parse_include_directories

    subroutine parse_fortran(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config
        character(len=1024) :: str_val

        select case (trim(key))
        case ('implicit-typing')
            call parse_boolean(val, config%implicit_typing, key, &
                config%manifest_parse_error)
        case ('implicit-external')
            call parse_boolean(val, config%implicit_external, key, &
                config%manifest_parse_error)
        case ('source-form')
            call extract_string(val, str_val)
            if (trim(str_val) /= 'free' .and. trim(str_val) /= 'fixed' .and. &
                trim(str_val) /= 'default') then
                config%manifest_parse_error = &
                    '[fortran] source-form must be "free", "fixed" or "default"'
                return
            end if
            config%source_form = trim(str_val)
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

    subroutine parse_exe(key, val, exe, error)
        character(len=*), intent(in) :: key, val
        type(fpm_exe_t), intent(inout) :: exe
        character(len=*), intent(out) :: error

        character(len=1024) :: str_val

        error = ''
        call extract_string(val, str_val)
        if (len_trim(str_val) == 0) return
        select case (trim(key))
        case ('name')
            exe%name = trim(str_val)
        case ('main')
            exe%main = trim(str_val)
        case ('source-dir')
            exe%source_dir = trim(str_val)
        case ('link')
            error = 'target-local link is unsupported; '// &
                'declare libraries in [build] link'
        case default
            error = 'unsupported executable/test/example field: '//trim(key)
        end select
    end subroutine parse_exe

    function manifest_exe_name(config, app_dir, stem) result(name)
        !! Binary name for an app program from an explicit [[executable]] entry
        !! whose main-source stem and source-dir match, or '' when none applies.
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: app_dir, stem
        character(len=1024) :: name
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
        character(len=1024) :: name
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
        character(len=1024) :: name
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

    subroutine parse_dep_entry(name_key, val, deps, n_deps, error)
        character(len=*), intent(in) :: name_key, val
        type(fpm_dep_t), intent(inout) :: deps(:)
        integer, intent(inout) :: n_deps

        character(len=*), intent(inout) :: error
        character(len=256) :: name, field
        character(len=1024) :: str_val
        integer :: dot, i, found

        dot = index(trim(name_key), '.')
        if (dot <= 1) then
            if (n_deps >= size(deps)) then
                write (error, '(a,i0)') 'dependency "'//trim(name_key)// &
                    '" exceeds supported dependency limit ', size(deps)
                return
            end if
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
            if (n_deps >= size(deps)) then
                write (error, '(a,i0)') 'dependency "'//trim(name_key)// &
                    '" exceeds supported dependency limit ', size(deps)
                return
            end if
            n_deps = n_deps + 1
            found = n_deps
            call parse_dep(name, '', deps(found))
        end if

        call extract_string(val, str_val)
        select case (trim(field))
        case ('git')
            deps(found)%git_seen = .true.
            deps(found)%git = trim(str_val)
        case ('branch')
            deps(found)%branch_seen = .true.
            deps(found)%branch = trim(str_val)
        case ('tag')
            deps(found)%tag_seen = .true.
            deps(found)%tag = trim(str_val)
        case ('rev')
            deps(found)%rev_seen = .true.
            deps(found)%rev = trim(str_val)
        case ('path')
            deps(found)%path_seen = .true.
            deps(found)%path = trim(str_val)
        case ('namespace')
            deps(found)%namespace_seen = .true.
            deps(found)%namespace = trim(str_val)
            if (.not. quoted_dependency_value(val)) &
                deps(found)%unsupported_field = 'unquoted registry identity'
            if (len_trim(str_val) > len(deps(found)%namespace)) &
                deps(found)%unsupported_field = 'oversized registry identity'
        case ('v')
            deps(found)%registry_v_seen = .true.
            deps(found)%registry_v = trim(str_val)
            if (.not. quoted_dependency_value(val)) &
                deps(found)%unsupported_field = 'unquoted registry identity'
            if (len_trim(str_val) > len(deps(found)%registry_v)) &
                deps(found)%unsupported_field = 'oversized registry identity'
        case default
            deps(found)%unsupported_field = trim(field)
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

        if (len_trim(val) == 0) return

        if (val(1:1) == '{') then
            call parse_inline_table(val, ikeys, ivals, n_fields)
            do i = 1, n_fields
                call extract_string(ivals(i), str_val)
                select case (trim(ikeys(i)))
                case ('git')
                    dep%git_seen = .true.
                    dep%git = trim(str_val)
                case ('branch')
                    dep%branch_seen = .true.
                    dep%branch = trim(str_val)
                case ('tag')
                    dep%tag_seen = .true.
                    dep%tag = trim(str_val)
                case ('rev')
                    dep%rev_seen = .true.
                    dep%rev = trim(str_val)
                case ('path')
                    dep%path_seen = .true.
                    dep%path = trim(str_val)
                case ('namespace')
                    dep%namespace_seen = .true.
                    dep%namespace = trim(str_val)
                    if (.not. quoted_dependency_value(ivals(i))) &
                        dep%unsupported_field = 'unquoted registry identity'
                    if (len_trim(str_val) > len(dep%namespace)) &
                        dep%unsupported_field = 'oversized registry identity'
                case ('v')
                    dep%registry_v_seen = .true.
                    dep%registry_v = trim(str_val)
                    if (.not. quoted_dependency_value(ivals(i))) &
                        dep%unsupported_field = 'unquoted registry identity'
                    if (len_trim(str_val) > len(dep%registry_v)) &
                        dep%unsupported_field = 'oversized registry identity'
                case default
                    dep%unsupported_field = trim(ikeys(i))
                end select
            end do
        else
            call extract_string(val, str_val)
            dep%unsupported_field = 'bare registry version; use namespace and v'
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
        character(len=1024) :: name
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

    subroutine parse_extra_fo(key, val, config)
        !! Scalar fo settings under [extra.fo]; unknown keys are left to other
        !! tools sharing the namespace.
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config

        character(len=64) :: str_val

        select case (trim(key))
        case ('test-timeout')
            config%test_timeout = positive_seconds(val, 'test-timeout')
        case ('slow-test-timeout')
            config%slow_test_timeout = positive_seconds(val, 'slow-test-timeout')
        case ('test-wall-timeout')
            config%test_wall_timeout = positive_seconds(val, 'test-wall-timeout')
        case ('dispatcher')
            call extract_string(val, str_val)
            config%dispatcher = trim(str_val)
        case ('debug-info')
            call extract_string(val, str_val)
            select case (trim(str_val))
            case ('g0', '-g0', 'none')
                config%debug_info = 'g0'
            case ('line-tables', '-gline-tables-only', 'line-tables-only')
                config%debug_info = 'line-tables'
            case ('full', 'g', '-g')
                config%debug_info = 'full'
            case default
                write (error_unit, '(a)') 'fo: warning: ignoring [extra.fo] '// &
                    'debug-info = '//trim(val)// &
                ' (expected g0, line-tables, or full)'
            end select
        case ('pic')
            ! Position-independent code is not a style preference here: it is
            ! the precondition for a shared libffc, without which every test
            ! executable relinks the whole static archive.
            call extract_string(val, str_val)
            select case (trim(str_val))
            case ('true', 'True', 'TRUE', 'yes', 'on', '1')
                config%pic = .true.
            case ('false', 'False', 'FALSE', 'no', 'off', '0')
                config%pic = .false.
            case default
                write (error_unit, '(a)') 'fo: warning: ignoring [extra.fo] '// &
                    'pic = '//trim(val)//' (expected true or false)'
            end select
        case ('link')
            ! `shared` folds the library once into a .so and lets every test
            ! executable link against it, instead of relinking the whole static
            ! archive ~500 times. Needs `pic = "true"`; the build says so
            ! rather than letting the linker emit a relocation wall.
            call extract_string(val, str_val)
            select case (trim(str_val))
            case ('shared', 'so', 'dynamic')
                config%link_shared = .true.
            case ('static', 'archive', 'a')
                config%link_shared = .false.
            case default
                write (error_unit, '(a)') 'fo: warning: ignoring [extra.fo] '// &
                    'link = '//trim(val)//' (expected shared or static)'
            end select
        case default
        end select
    end subroutine parse_extra_fo

    subroutine parse_fo_input(key, val, config)
        character(len=*), intent(in) :: key, val
        type(fpm_config_t), intent(inout) :: config
        character(len=1024) :: str_val
        integer :: slot

        if (config%n_fo_inputs <= 0) then
            config%fo_input_parse_error = 'fixture input fields require [[extra.fo.inputs]]'
            return
        end if
        if (len_trim(val) == 0) then
            config%fo_input_parse_error = 'fixture input fields cannot be empty'
            return
        end if
        if (val(1:1) == '[' .or. val(1:1) == '{') then
            config%fo_input_parse_error = 'fixture input fields must be scalar values'
            return
        end if
        slot = config%n_fo_inputs
        select case (trim(key))
        case ('path')
            if (config%fo_inputs(slot)%path_seen) then
                config%fo_input_parse_error = 'duplicate fixture input path field'
                return
            end if
            call extract_string(val, str_val)
            config%fo_inputs(slot)%path = trim(str_val)
            config%fo_inputs(slot)%path_seen = .true.
        case ('role')
            if (config%fo_inputs(slot)%role_seen) then
                config%fo_input_parse_error = 'duplicate fixture input role field'
                return
            end if
            call extract_string(val, str_val)
            config%fo_inputs(slot)%role = trim(str_val)
            config%fo_inputs(slot)%role_seen = .true.
        case ('root')
            if (config%fo_inputs(slot)%root_seen) then
                config%fo_input_parse_error = 'duplicate fixture input root field'
                return
            end if
            call extract_string(val, str_val)
            if (len_trim(str_val) == 0) then
                config%fo_input_parse_error = 'fixture input root cannot be empty'
                return
            end if
            config%fo_inputs(slot)%root = trim(str_val)
            config%fo_inputs(slot)%root_seen = .true.
        case ('kind')
            if (config%fo_inputs(slot)%kind_seen) then
                config%fo_input_parse_error = 'duplicate fixture input kind field'
                return
            end if
            call extract_string(val, str_val)
            select case (trim(str_val))
            case ('file', 'directory')
                config%fo_inputs(slot)%kind = trim(str_val)
                config%fo_inputs(slot)%kind_seen = .true.
            case default
                config%fo_input_parse_error = &
                    'fixture input kind must be file or directory'
            end select
        case ('writable-at-execution')
            if (config%fo_inputs(slot)%writable_seen) then
                config%fo_input_parse_error = &
                    'duplicate fixture writable-at-execution field'
                return
            end if
            config%fo_inputs(slot)%writable_seen = .true.
            if (trim(val) == 'true') then
                config%fo_inputs(slot)%writable_at_execution = .true.
            else if (trim(val) == 'false') then
                config%fo_inputs(slot)%writable_at_execution = .false.
            else
                config%fo_input_parse_error = &
                    'fixture writable-at-execution must be true or false'
            end if
        case default
            config%fo_input_parse_error = 'unknown [[extra.fo.inputs]] field: '// &
                trim(key)
        end select
    end subroutine parse_fo_input

    integer function positive_seconds(val, key) result(seconds)
        character(len=*), intent(in) :: val, key
        integer :: ios

        read (val, *, iostat=ios) seconds
        if (ios == 0) then
            if (seconds > 0) return
        end if
        write (error_unit, '(a)') 'fo: warning: ignoring [extra.fo] '//trim(key)// &
            ' = '//trim(val)//' (expected a positive whole number of seconds)'
        seconds = 0
    end function positive_seconds

    subroutine parse_test_args(name, val, config)
        character(len=*), intent(in) :: name, val
        type(fpm_config_t), intent(inout) :: config

        integer :: pos, start, n, slot
        character(len=512) :: arg
        logical :: in_str

        if (config%n_test_arg_sets >= MAX_TEST_ARG_SETS) then
            write (config%manifest_parse_error, '(a,i0)') &
                'test arguments "'//trim(name)//'" exceed supported limit ', &
                MAX_TEST_ARG_SETS
            return
        end if
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

        integer :: pos, start, n, equals
        character(len=:), allocatable :: macro
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
                    equals = index(macro, '=')
                    if (equals > 0) then
                        if (trim(adjustl(macro(equals + 1:))) == '{version}') then
                            if (len_trim(config%version) > 0) then
                                macro = macro(:equals)//trim(config%version)
                            else
                                ! Native fpm defaults a missing package version to 0.
                                macro = macro(:equals)//'0'
                            end if
                        end if
                    end if
                    if (len_trim(macro) + 2 > len(config%flags)) then
                        config%manifest_parse_error = &
                            'preprocessor macro exceeds supported flag length'
                        return
                    end if
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
        character :: quote

        quote = achar(0)
        i = 1
        do while (i <= len_trim(line))
            if (quote == achar(0)) then
                select case (line(i:i))
                case ('"', "'")
                    quote = line(i:i)
                case ('#')
                    line(i:) = ' '
                    return
                end select
            else if (quote == '"' .and. line(i:i) == achar(92)) then
                ! Basic-string escapes do not end the surrounding string.
                i = i + 1
            else if (line(i:i) == quote) then
                quote = achar(0)
            end if
            i = i + 1
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

        integer :: q2, n
        character(len=len(raw_val)) :: value
        character :: quote

        str_val = ''
        value = adjustl(raw_val)
        n = len_trim(raw_val)
        if (n < 2) then
            ! bare value (e.g. "*")
            str_val = trim(raw_val)
            return
        end if

        quote = value(1:1)
        if (quote /= '"' .and. quote /= "'") then
            str_val = trim(value)
            return
        end if
        q2 = index(value(2:), quote)
        if (q2 == 0) return
        q2 = 1 + q2
        if (q2 > 2) str_val = value(2:q2 - 1)
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
            'https://github.com/fortran-lang/stdlib', config%manifest_parse_error, &
            branch='stdlib-fpm')
        call add_git_dependency(config%dev_deps, config%n_dev_deps, 'test-drive', &
            'https://github.com/fortran-lang/test-drive', config%manifest_parse_error, &
            branch='v0.4.0')
        if (config%blas) then
            call append_config_flag('-DSTDLIB_EXTERNAL_BLAS', config)
            call append_config_flag('-DSTDLIB_EXTERNAL_LAPACK', config)
        end if
    end subroutine resolve_stdlib_metapackage

    subroutine resolve_minpack_metapackage(config)
        type(fpm_config_t), intent(inout) :: config

        call add_git_dependency(config%deps, config%n_deps, 'minpack', &
            'https://github.com/fortran-lang/minpack', config%manifest_parse_error, &
            tag='v2.0.0-rc.1')
    end subroutine resolve_minpack_metapackage

    subroutine add_git_dependency(deps, n_deps, name, url, error, branch, tag)
        type(fpm_dep_t), intent(inout) :: deps(:)
        integer, intent(inout) :: n_deps
        character(len=*), intent(inout) :: error
        character(len=*), intent(in) :: name, url
        character(len=*), intent(in), optional :: branch, tag
        integer :: i

        do i = 1, n_deps
            if (trim(deps(i)%name) == trim(name)) return
        end do
        if (n_deps >= size(deps)) then
            write (error, '(a,i0)') 'dependency "'//trim(name)// &
                '" exceeds supported dependency limit ', size(deps)
            return
        end if
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
