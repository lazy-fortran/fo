program test_scaffold
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scaffold, only: scaffold_project
    use fo_fs, only: fs_remove_tree
    use fo_test_cli, only: resolve_driver
    use fo_test_harness, only: run_list, argv
    use fo_fs, only: fs_make_dir
    implicit none
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_new_exe_project()
    call test_new_lib_project()
    call test_new_path_with_shell_metacharacters()
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

        call fs_remove_tree(trim(tmpdir))
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

        call fs_remove_tree(trim(tmpdir))
    end subroutine test_new_lib_project

    subroutine test_new_path_with_shell_metacharacters()
        character(len=512) :: base, tmpdir
        integer :: ierr
        logical :: exists

        call make_tmp_path('fo scaffold space', base)
        tmpdir = trim(base)//'/a b;touch fo_scaffold_pwn $(echo x)'

        call scaffold_project(trim(tmpdir), 'demo_meta', .false., ierr)
        call assert(ierr == 0, 'scaffold_project accepts spaces and shell metacharacters')
        inquire (file=trim(tmpdir)//'/app/main.f90', exist=exists)
        call assert(exists, 'app/main.f90 created inside the literal path')
        inquire (file=trim(tmpdir)//'/test/test_demo_meta.f90', exist=exists)
        call assert(exists, 'test file created inside the literal path')
        inquire (file='fo_scaffold_pwn', exist=exists)
        call assert(.not. exists, 'path text is never interpreted by a shell')

        call fs_remove_tree(trim(base))
    end subroutine test_new_path_with_shell_metacharacters

    subroutine test_scaffold_builds()
        character(len=512) :: tmpdir, name
        character(:), allocatable :: driver
        integer :: ierr, exitcode

        call make_tmp_path('fo_scaffold_build', tmpdir)
        name = 'demo_build'

        call scaffold_project(trim(tmpdir), trim(name), .false., ierr)
        call assert(ierr == 0, 'scaffold succeeds for build test')

        ! Test that the scaffolded project can build with fo check
        call resolve_driver(driver)
        call run_list(argv(driver, 'check'), exitcode, cwd=trim(tmpdir))
        call assert(exitcode == 0, 'scaffolded project builds with fo check')

        call fs_remove_tree(trim(tmpdir))
    end subroutine test_scaffold_builds

    subroutine make_tmp_path(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count, pid
        character(len=32) :: pid_str

        call system_clock(count)
        write (pid_str, '(i0)') count
        write (path, '(a,a,a,a)') '/tmp/', trim(prefix), '-', trim(pid_str)
        call fs_make_dir(trim(path))
    end subroutine make_tmp_path

end program test_scaffold
