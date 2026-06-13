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

end program test_fpm_config
