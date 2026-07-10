program test_fpm_config
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse, fpm_config_init
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_parse_fo_own_toml()
    call test_init_defaults()
    call test_parse_missing_file()
    call test_dotted_dependency_keys()
    call test_flags_with_equals_inline()
    call test_flags_multiline_array()

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

        ! fo's own fpm.toml is in the parent of the test executable's cwd
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

end program test_fpm_config
