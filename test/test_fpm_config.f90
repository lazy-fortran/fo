program test_fpm_config
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, fpm_config_init, &
        manifest_test_args
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_parse_fo_own_toml()
    call test_init_defaults()
    call test_parse_missing_file()
    call test_dotted_dependency_keys()
    call test_blas_metapackage()
    call test_source_metapackages()
    call test_system_metapackages()
    call test_flags_with_equals_inline()
    call test_flags_multiline_array()
    call test_preprocess_macros()
    call test_per_test_arguments()
    call test_external_modules_array()
    call test_implicit_typing_allowed()

    write (output_unit, '(a,i0,a,i0,a)') 'fpm_config: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (output_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_init_defaults()
        type(fpm_config_t) :: c

        call fpm_config_init(c)
        call assert(trim(c%source_dir) == 'src', 'default source_dir = src')
        call assert(trim(c%app_dir) == 'app', 'default app_dir = app')
        call assert(trim(c%test_dir) == 'test', 'default test_dir = test')
        call assert(c%auto_executables, 'default auto_executables = true')
        call assert(c%auto_tests, 'default auto_tests = true')
        call assert(c%auto_examples, 'default auto_examples = true')
        call assert(c%n_deps == 0, 'default n_deps = 0')
        call assert(c%n_dev_deps == 0, 'default n_dev_deps = 0')
    end subroutine test_init_defaults

    subroutine test_parse_missing_file()
        type(fpm_config_t) :: c
        integer :: ierr

        call fpm_config_parse('/nonexistent/path', c, ierr)
        call assert(ierr /= 0, 'missing file returns error')
    end subroutine test_parse_missing_file

    subroutine test_parse_fo_own_toml()
        type(fpm_config_t) :: c
        integer :: ierr
        character(len=256) :: fo_dir

        ! fo runs every test binary with the project root as its working
        ! directory, so fo's own fpm.toml is always at './fpm.toml' here
        ! regardless of where fo itself was invoked from.
        fo_dir = '.'
        call fpm_config_parse(fo_dir, c, ierr)

        call assert(ierr == 0, 'parse fo fpm.toml succeeds')
        if (ierr /= 0) return

        call assert(trim(c%name) == 'fo', 'name = fo')
        call assert(len_trim(c%version) > 0, 'version not empty')
        call assert(trim(c%source_dir) == 'src', 'source_dir = src')
        call assert(trim(c%app_dir) == 'app', 'app_dir = app')
        call assert(trim(c%test_dir) == 'test', 'test_dir = test')
        call assert(c%n_deps >= 1, 'at least 1 dep (fx)')

        block
            integer :: i
            logical :: found_fx

            found_fx = .false.
            do i = 1, c%n_deps
                if (trim(c%deps(i)%name) == 'fx') found_fx = .true.
            end do
            call assert(found_fx, 'dep fx present')
        end block
    end subroutine test_parse_fo_own_toml

    subroutine test_dotted_dependency_keys()
        type(fpm_config_t) :: c
        integer :: ierr, u, ios
        character(len=*), parameter :: dir = '/tmp/fo_test_dotted_deps'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'dotted_dependency_keys: cannot write fpm.toml')
            return
        end if
        write (u, '(a)') 'name = "dotted-dependencies"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a)') 'toml-f.git = "https://example.invalid/toml-f"'
        write (u, '(a)') 'toml-f.rev = "0123456789abcdef"'
        write (u, '(a)') 'fortran-shlex.git = "https://example.invalid/shlex"'
        write (u, '(a)') 'fortran-shlex.tag = "2.0.1"'
        write (u, '(a)') 'inline = { git = "https://example.invalid/inline", '// &
            'rev = "fedcba9876543210" }'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'dotted_dependency_keys: parse succeeds')
        call assert(c%n_deps == 3, 'dotted_dependency_keys: three dependencies')
        if (c%n_deps >= 3) then
            call assert(trim(c%deps(1)%name) == 'toml-f', &
                'dotted_dependency_keys: first name')
            call assert(len_trim(c%deps(1)%git) > 0, &
                'dotted_dependency_keys: first git source')
            call assert(allocated(c%deps(1)%rev), &
                'dotted_dependency_keys: revision allocated')
            call assert(trim(c%deps(1)%rev) == '0123456789abcdef', &
                'dotted_dependency_keys: exact revision')
            call assert(trim(c%deps(2)%name) == 'fortran-shlex', &
                'dotted_dependency_keys: second name')
            call assert(trim(c%deps(2)%tag) == '2.0.1', &
                'dotted_dependency_keys: second tag')
            call assert(allocated(c%deps(3)%rev), &
                'dotted_dependency_keys: inline revision allocated')
            call assert(trim(c%deps(3)%rev) == 'fedcba9876543210', &
                'dotted_dependency_keys: inline revision')
        end if
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_dotted_dependency_keys

    subroutine test_blas_metapackage()
        !! The oracle is the provider contract: when pkg-config has a BLAS
        !! provider, fo must import at least one provider flag; when none is
        !! installed, the metapackage must fail instead of silently guessing.
        type(fpm_config_t) :: c
        integer :: ierr, u, ios, exitstat, i
        logical :: provider_available
        character(len=*), parameter :: candidates(4) = [ character(len=32) :: &
            'mkl-dynamic-lp64-tbb', 'openblas', 'blas', 'flexiblas' ]
        character(len=*), parameter :: dir = '/tmp/fo_test_blas_meta'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'blas_metapackage: cannot write fpm.toml')
            return
        end if
        write (u, '(a)') 'name = "blas-metapackage"'
        write (u, '(a)') 'dependencies.blas = "*"'
        close (u)

        provider_available = .false.
        do i = 1, size(candidates)
            call execute_command_line('pkg-config --exists '//trim(candidates(i))// &
                ' >/dev/null 2>&1', wait=.true., exitstat=exitstat)
            if (exitstat == 0) provider_available = .true.
        end do

        call fpm_config_parse(dir, c, ierr)
        call assert(c%blas, 'blas_metapackage: request is recorded')
        call assert((ierr == 0) .eqv. provider_available, &
            'blas_metapackage: provider availability controls resolution')
        if (ierr == 0) then
            call assert(c%n_link_libs > 0 .or. c%n_flags > 0, &
                'blas_metapackage: provider flags are imported')
        end if
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_blas_metapackage

    subroutine test_source_metapackages()
        !! stdlib and minpack are fpm metapackages backed by pinned git
        !! dependencies.  The oracle is the dependency contract, not a copy of
        !! fo's parser: both source names and the stdlib test dependency must be
        !! present after resolution.
        type(fpm_config_t) :: c
        integer :: ierr, u, ios
        character(len=*), parameter :: dir = '/tmp/fo_test_source_meta'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'source_metapackages: cannot write fpm.toml')
            return
        end if
        write (u, '(a)') 'name = "source-metapackages"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a)') 'stdlib = "*"'
        write (u, '(a)') 'minpack = "*"'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'source_metapackages: resolution succeeds')
        call assert(c%stdlib .and. c%minpack, &
            'source_metapackages: requests are recorded')
        call assert(has_dependency(c, 'stdlib'), &
            'source_metapackages: stdlib source dependency is added')
        call assert(has_dependency(c, 'minpack'), &
            'source_metapackages: minpack source dependency is added')
        call assert(has_dev_dependency(c, 'test-drive'), &
            'source_metapackages: test-drive dev dependency is added')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_source_metapackages

    subroutine test_system_metapackages()
        !! hdf5, netcdf, and mpi are resolved from the local system.  The
        !! independent oracle asks the provider tools whether each package is
        !! available, then checks that fo agrees and imports usable metadata.
        type(fpm_config_t) :: c
        integer :: ierr, u, ios, exitstat, cmdstat
        logical :: available
        character(len=*), parameter :: dir = '/tmp/fo_test_system_meta'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'system_metapackages: cannot write fpm.toml')
            return
        end if
        write (u, '(a)') 'name = "system-metapackages"'
        write (u, '(a)') '[dependencies]'
        write (u, '(a)') 'hdf5 = "*"'
        write (u, '(a)') 'netcdf = "*"'
        write (u, '(a)') 'mpi = "*"'
        close (u)

        available = .true.
        call execute_command_line('pkg-config --exists hdf5 >/dev/null 2>&1', &
            wait=.true., exitstat=exitstat)
        available = available .and. exitstat == 0
        call execute_command_line('pkg-config --exists netcdf >/dev/null 2>&1', &
            wait=.true., exitstat=exitstat)
        available = available .and. exitstat == 0
        call execute_command_line('pkg-config --exists netcdf-fortran >/dev/null 2>&1', &
            wait=.true., exitstat=exitstat)
        available = available .and. exitstat == 0
        call execute_command_line('mpifort --version >/dev/null 2>&1', &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        available = available .and. cmdstat == 0 .and. exitstat == 0

        call fpm_config_parse(dir, c, ierr)
        call assert((ierr == 0) .eqv. available, &
            'system_metapackages: provider availability controls resolution')
        if (ierr == 0) then
            call assert(c%hdf5 .and. c%netcdf .and. c%mpi, &
                'system_metapackages: requests are recorded')
            call assert(c%n_link_libs > 0 .and. c%n_flags > 0, &
                'system_metapackages: provider flags are imported')
            call assert(c%n_external_modules > 0, &
                'system_metapackages: external modules are recorded')
        end if
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_system_metapackages

    logical function has_dependency(c, name)
        type(fpm_config_t), intent(in) :: c
        character(len=*), intent(in) :: name
        integer :: i

        has_dependency = .false.
        do i = 1, c%n_deps
            if (trim(c%deps(i)%name) == trim(name)) then
                has_dependency = .true.
                return
            end if
        end do
    end function has_dependency

    logical function has_dev_dependency(c, name)
        type(fpm_config_t), intent(in) :: c
        character(len=*), intent(in) :: name
        integer :: i

        has_dev_dependency = .false.
        do i = 1, c%n_dev_deps
            if (trim(c%dev_deps(i)%name) == trim(name)) then
                has_dev_dependency = .true.
                return
            end if
        end do
    end function has_dev_dependency

    subroutine test_flags_with_equals_inline()
        !! Flags with '=' (e.g. -fsanitize=address) must be preserved verbatim
        !! when written as a single-line TOML array in [build] flags.
        type(fpm_config_t) :: c
        integer :: ierr, u, ios
        character(len=256) :: dir
        character(len=512) :: toml_path

        dir = '/tmp/fo_test_flags_eq'
        call execute_command_line('mkdir -p '//trim(dir), wait=.true.)
        toml_path = trim(dir)//'/fpm.toml'
        open (newunit=u, file=trim(toml_path), status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'flags_with_equals_inline: cannot write fpm.toml')
            return
        end if
        write (u, '(a)') 'name = "test"'
        write (u, '(a)') 'version = "0.1.0"'
        write (u, '(a)') ''
        write (u, '(a)') '[build]'
        write (u, '(a)') 'flags = ["-O0", "-fsanitize=address"]'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'flags_with_equals_inline: parse succeeds')
        call assert(c%n_flags == 2, 'flags_with_equals_inline: 2 flags')
        if (c%n_flags >= 1) &
            call assert(trim(c%flags(1)) == '-O0', 'flags_with_equals_inline: first flag = -O0')
        if (c%n_flags >= 2) &
            call assert(trim(c%flags(2)) == '-fsanitize=address', &
            'flags_with_equals_inline: second flag = -fsanitize=address')
        call execute_command_line('rm -rf '//trim(dir), wait=.true.)
    end subroutine test_flags_with_equals_inline

    subroutine test_flags_multiline_array()
        !! Flags in a multi-line TOML array must all be captured, including
        !! flags containing '='.
        type(fpm_config_t) :: c
        integer :: ierr, u, ios
        character(len=256) :: dir
        character(len=512) :: toml_path

        dir = '/tmp/fo_test_flags_ml'
        call execute_command_line('mkdir -p '//trim(dir), wait=.true.)
        toml_path = trim(dir)//'/fpm.toml'
        open (newunit=u, file=trim(toml_path), status='replace', iostat=ios)
        if (ios /= 0) then
            call assert(.false., 'flags_multiline_array: cannot write fpm.toml')
            return
        end if
        write (u, '(a)') 'name = "test2"'
        write (u, '(a)') 'version = "0.1.0"'
        write (u, '(a)') ''
        write (u, '(a)') '[build]'
        write (u, '(a)') 'flags = ['
        write (u, '(a)') '  "-g",'
        write (u, '(a)') '  "-O0",'
        write (u, '(a)') '  "-fsanitize=address"'
        write (u, '(a)') ']'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'flags_multiline_array: parse succeeds')
        call assert(c%n_flags == 3, 'flags_multiline_array: 3 flags')
        if (c%n_flags >= 1) &
            call assert(trim(c%flags(1)) == '-g', 'flags_multiline_array: first flag = -g')
        if (c%n_flags >= 2) &
            call assert(trim(c%flags(2)) == '-O0', 'flags_multiline_array: second flag = -O0')
        if (c%n_flags >= 3) &
            call assert(trim(c%flags(3)) == '-fsanitize=address', &
            'flags_multiline_array: third flag = -fsanitize=address')
        call execute_command_line('rm -rf '//trim(dir), wait=.true.)
    end subroutine test_flags_multiline_array

    subroutine test_preprocess_macros()
        type(fpm_config_t) :: c
        integer :: ierr, u
        character(len=*), parameter :: dir = '/tmp/fo_test_preprocess_macros'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "preprocess-macros"'
        write (u, '(a)') '[preprocess]'
        write (u, '(a)') 'cpp.macros = ["ENABLE_TUI=1", "WITH_FEATURE"]'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'preprocess_macros: parse succeeds')
        call assert(c%n_flags == 3, 'preprocess_macros: preprocessor plus definitions')
        if (c%n_flags >= 3) then
            call assert(trim(c%flags(1)) == '-cpp', &
                'preprocess_macros: preprocessing enabled for lowercase sources')
            call assert(trim(c%flags(2)) == '-DENABLE_TUI=1', &
                'preprocess_macros: value definition preserved')
            call assert(trim(c%flags(3)) == '-DWITH_FEATURE', &
                'preprocess_macros: flag definition preserved')
        end if
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_preprocess_macros

    subroutine test_per_test_arguments()
        type(fpm_config_t) :: c
        integer :: ierr, u
        character(len=4096) :: args
        character(len=*), parameter :: dir = '/tmp/fo_test_per_test_arguments'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "per-test-arguments"'
        write (u, '(a)') '[extra.fo.test-args]'
        write (u, '(a)') 'test_oracle = ["data/reference.csv", "two words"]'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        args = manifest_test_args(c, 'test_oracle')
        call assert(ierr == 0, 'per_test_arguments: parse succeeds')
        call assert(c%n_test_arg_sets == 1, 'per_test_arguments: one test mapping')
        call assert(trim(args) == 'data/reference.csv'//new_line('a')//'two words', &
            'per_test_arguments: argument boundaries are retained')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_per_test_arguments

    subroutine test_external_modules_array()
        !! libneo declares its system modules as a multi-line array; the parser
        !! has to keep every entry, not just the one on the opening line.
        type(fpm_config_t) :: c
        integer :: ierr, u
        character(len=*), parameter :: dir = '/tmp/fo_test_external_modules'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "external-modules"'
        write (u, '(a)') '[build]'
        write (u, '(a)') 'external-modules = ['
        write (u, '(a)') '    "netcdf", "hdf5",'
        write (u, '(a)') '    "omp_lib"'
        write (u, '(a)') ']'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'external_modules: parse succeeds')
        call assert(c%n_external_modules == 3, &
            'external_modules: all three entries survive the line breaks')
        if (c%n_external_modules == 3) then
            call assert(trim(c%external_modules(1)) == 'netcdf', &
                'external_modules: first entry')
            call assert(trim(c%external_modules(3)) == 'omp_lib', &
                'external_modules: last entry')
        end if
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_external_modules_array

    subroutine test_implicit_typing_allowed()
        !! fpm defaults implicit-typing to false, so the strict flag applies.
        !! A manifest that sets it true is asking for the flag to be left off;
        !! recording only the false case silently ignored that request and made
        !! legacy fixed-form sources uncompilable.
        type(fpm_config_t) :: c
        integer :: ierr, u
        character(len=*), parameter :: dir = '/tmp/fo_test_implicit_typing'

        call execute_command_line('mkdir -p '//dir, wait=.true.)
        open (newunit=u, file=dir//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "implicit-typing"'
        write (u, '(a)') '[fortran]'
        write (u, '(a)') 'implicit-typing = true'
        write (u, '(a)') 'implicit-external = true'
        close (u)

        call fpm_config_parse(dir, c, ierr)
        call assert(ierr == 0, 'implicit_typing: parse succeeds')
        call assert(c%implicit_typing, 'implicit_typing: true is recorded')
        call assert(c%implicit_external, 'implicit_external: true is recorded')
        open (newunit=u, file=dir//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "implicit-typing-off"'
        write (u, '(a)') '[fortran]'
        write (u, '(a)') 'implicit-typing = false'
        close (u)
        call fpm_config_parse(dir, c, ierr)
        call assert(.not. c%implicit_typing, &
            'implicit_typing: false leaves the strict default in place')
        call execute_command_line('rm -rf '//dir, wait=.true.)
    end subroutine test_implicit_typing_allowed

end program test_fpm_config
