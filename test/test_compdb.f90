program test_compdb
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_gfortran_build, only: gfortran_build
    use fo_process, only: process_getpid
    implicit none

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: ierr
        end function setenv

        function unsetenv(name) bind(C, name='unsetenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: ierr
        end function unsetenv
    end interface

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_gfortran_build_writes_compile_commands()
    call report()

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

    subroutine test_gfortran_build_writes_compile_commands()
        character(len=512) :: project_dir, log_file, compdb, cache_dir
        integer :: exitcode, n_first, n_second, ierr
        logical :: exists

        call make_tmp_path('fo_compdb_project', project_dir)
        call make_tmp_path('fo_compdb_log', log_file)
        call make_tmp_path('fo_compdb_cache', cache_dir)
        call execute_command_line('mkdir -p '//trim(cache_dir))
        ierr = setenv('FO_CACHE_DIR'//c_null_char, trim(cache_dir)//c_null_char, 1_c_int)
        call make_compdb_project(project_dir)
        compdb = trim(project_dir)//'/build/compile_commands.json'

        call gfortran_build(project_dir, log_file, exitcode, n_compiled=n_first)
        call assert(exitcode == 0, 'cold build succeeds')
        call assert(n_first == 2, 'cold build compiles module and app')
        inquire (file=trim(compdb), exist=exists)
        call assert(exists, 'cold build writes compile_commands.json')
        call assert(valid_compdb(project_dir, compdb), 'cold compdb is valid')

        call execute_command_line('rm -f '//trim(compdb))
        call gfortran_build(project_dir, log_file, exitcode, n_compiled=n_second)
        call assert(exitcode == 0, 'warm build succeeds')
        call assert(n_second == 0, 'warm build restores sources from cache')
        inquire (file=trim(compdb), exist=exists)
        call assert(exists, 'warm build rewrites compile_commands.json')
        call assert(valid_compdb(project_dir, compdb), 'warm compdb is complete')

        call execute_command_line('rm -rf '//trim(project_dir))
        call execute_command_line('rm -f '//trim(log_file))
        ierr = unsetenv('FO_CACHE_DIR'//c_null_char)
        call execute_command_line('rm -rf '//trim(cache_dir))
    end subroutine test_gfortran_build_writes_compile_commands

    logical function valid_compdb(project_dir, compdb)
        character(len=*), intent(in) :: project_dir, compdb
        character(len=512) :: script, cmd
        integer :: u, exitcode

        valid_compdb = .false.
        script = trim(project_dir)//'/check_compdb.py'
        open (newunit=u, file=trim(script), status='replace', action='write')
        write (u, '(a)') 'import json, sys'
        write (u, '(a)') 'path = sys.argv[1]'
        write (u, '(a)') 'data = json.load(open(path))'
        write (u, '(a)') 'assert isinstance(data, list), data'
        write (u, '(a)') 'assert len(data) == 2, data'
        write (u, '(a)') 'for entry in data:'
        write (u, '(a)') '    assert set(["directory", "file", "arguments"]) <= set(entry), entry'
        write (u, '(a)') '    assert isinstance(entry["arguments"], list), entry'
        write (u, '(a)') '    assert len(entry["arguments"]) > 0, entry'
        write (u, '(a)') '    assert entry["arguments"][0].endswith("gfortran"), entry'
        write (u, '(a)') '    assert "-c" in entry["arguments"], entry'
        write (u, '(a)') '    assert any(arg.startswith("-J") or arg == "-J" for arg in entry["arguments"]), entry'
        write (u, '(a)') '    assert entry["file"] in entry["arguments"], entry'
        write (u, '(a)') 'files = {entry["file"] for entry in data}'
        write (u, '(a)') 'assert any(file.endswith("src/lib.f90") for file in files), files'
        write (u, '(a)') 'assert any(file.endswith("app/main.f90") for file in files), files'
        close (u)

        cmd = 'python3 '//trim(script)//' '//trim(compdb)
        call execute_command_line(trim(cmd), exitstat=exitcode)
        valid_compdb = exitcode == 0
    end function valid_compdb

    subroutine make_compdb_project(project_dir)
        character(len=*), intent(in) :: project_dir
        integer :: u

        call execute_command_line('mkdir -p '//trim(project_dir)//'/src '// &
            trim(project_dir)//'/app')
        open (newunit=u, file=trim(project_dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "compdb_project"'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/src/lib.f90', status='replace')
        write (u, '(a)') 'module lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'contains'
        write (u, '(a)') 'integer function value()'
        write (u, '(a)') 'value = 1'
        write (u, '(a)') 'end function value'
        write (u, '(a)') 'end module lib'
        close (u)

        open (newunit=u, file=trim(project_dir)//'/app/main.f90', status='replace')
        write (u, '(a)') 'program main'
        write (u, '(a)') 'use lib, only: value'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'if (value() /= 1) stop 1'
        write (u, '(a)') 'end program main'
        close (u)
    end subroutine make_compdb_project

    subroutine make_tmp_path(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path
        integer :: pid

        pid = process_getpid()
        write (path, '("/tmp/",a,"_",i0)') trim(prefix), pid
        call execute_command_line('rm -rf '//trim(path))
    end subroutine make_tmp_path

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'compdb: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_compdb
