program test_scaffold
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scaffold, only: scaffold_project
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_new_exe_project()
    call test_new_lib_project()
    call test_scaffold_builds()

    write (output_unit, '(a,i0,a,i0,a)') 'scaffold: ', n_pass, ' pass, ', &
        n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine test_new_exe_project()
        character(len=512) :: tmpdir, name
        integer :: ierr
        logical :: exists

        call make_tmp_path('fo_scaffold_exe', tmpdir)
        name = 'demo_exe'

        call scaffold_project(trim(tmpdir), trim(name), .false., ierr)
        call assert(ierr == 0, 'scaffold_project succeeds for exe')

        inquire (file=trim(tmpdir)//'/fpm.toml', exist=exists)
        call assert(exists, 'fpm.toml exists')

        inquire (file=trim(tmpdir)//'/app/main.f90', exist=exists)
        call assert(exists, 'app/main.f90 exists')

        inquire (file=trim(tmpdir)//'/src/lib.f90', exist=exists)
        call assert(exists, 'src/lib.f90 exists')

        inquire (file=trim(tmpdir)//'/test/test_demo_exe.f90', exist=exists)
        call assert(exists, 'test file exists')

        inquire (file=trim(tmpdir)//'/.gitignore', exist=exists)
        call assert(exists, '.gitignore exists')

        inquire (file=trim(tmpdir)//'/README.md', exist=exists)
        call assert(exists, 'README.md exists')

        call execute_command_line('rm -rf '//trim(tmpdir), wait=.true.)
    end subroutine test_new_exe_project

    subroutine test_new_lib_project()
        character(len=512) :: tmpdir, name
        integer :: ierr
        logical :: exists

        call make_tmp_path('fo_scaffold_lib', tmpdir)
        name = 'demo_lib'

        call scaffold_project(trim(tmpdir), trim(name), .true., ierr)
        call assert(ierr == 0, 'scaffold_project succeeds for lib')

        inquire (file=trim(tmpdir)//'/fpm.toml', exist=exists)
        call assert(exists, 'fpm.toml exists for lib')

        inquire (file=trim(tmpdir)//'/app/main.f90', exist=exists)
        call assert(.not. exists, 'app/main.f90 does not exist for lib')

        inquire (file=trim(tmpdir)//'/src/lib.f90', exist=exists)
        call assert(exists, 'src/lib.f90 exists for lib')

        inquire (file=trim(tmpdir)//'/test/test_demo_lib.f90', exist=exists)
        call assert(exists, 'test file exists for lib')

        call execute_command_line('rm -rf '//trim(tmpdir), wait=.true.)
    end subroutine test_new_lib_project

    subroutine test_scaffold_builds()
        character(len=512) :: tmpdir, name
        integer :: ierr, exitcode

        call make_tmp_path('fo_scaffold_build', tmpdir)
        name = 'demo_build'

        call scaffold_project(trim(tmpdir), trim(name), .false., ierr)
        call assert(ierr == 0, 'scaffold succeeds for build test')

        ! Test that the scaffolded project can build with fo check
        call execute_command_line('cd '//trim(tmpdir)//' && fo check', &
            wait=.true., exitstat=exitcode)
        call assert(exitcode == 0, 'scaffolded project builds with fo check')

        call execute_command_line('rm -rf '//trim(tmpdir), wait=.true.)
    end subroutine test_scaffold_builds

    subroutine make_tmp_path(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count, pid
        character(len=32) :: pid_str

        call system_clock(count)
        call execute_command_line('echo $$', wait=.true.)
        write (pid_str, '(i0)') count
        write (path, '(a,a,a,a)') '/tmp/', trim(prefix), '-', trim(pid_str)
        call execute_command_line('mkdir -p '//trim(path), wait=.true.)
    end subroutine make_tmp_path

end program test_scaffold
