program test_compdb
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_json, only: json_value_t, json_array, json_object, json_string, &
        json_parse, json_member, json_element, json_size, json_string_value
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
        character(len=512) :: project_dir, log_file, compdb, cache_dir, invalid_compdb
        integer :: exitcode, n_first, n_second, ierr, u
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

        invalid_compdb = trim(project_dir)//'/invalid_compile_commands.json'
        open (newunit=u, file=trim(invalid_compdb), status='replace', action='write')
        write (u, '(a)') '[{"directory":"x","file":"x",'// &
            '"arguments":["gfortran",],"extra":true}]'
        close (u)
        call assert(.not. valid_compdb(project_dir, invalid_compdb), &
            'compdb oracle rejects malformed JSON')

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
        type(json_value_t) :: document, entry, field, arguments, argument
        character(len=512) :: filename
        character(:), allocatable :: source, error_message, value
        integer :: u, file_size, ios, i, j
        logical :: valid, has_compile, has_module_dir, has_source_arg
        logical :: has_library, has_main

        valid_compdb = .false.
        inquire (file=trim(compdb), size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size <= 0) return
        allocate (character(len=file_size) :: source)
        open (newunit=u, file=trim(compdb), status='old', action='read', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) source
        close (u)
        if (ios /= 0) return

        call json_parse(source, document, valid, error_message)
        if (.not. valid) return
        if (document%kind /= json_array .or. json_size(document) /= 2) return

        has_library = .false.
        has_main = .false.
        do i = 1, json_size(document)
            entry = json_element(document, i)
            if (entry%kind /= json_object) return
            field = json_member(entry, 'directory')
            if (field%kind /= json_string) return
            if (json_string_value(field) /= trim(project_dir)) return
            field = json_member(entry, 'file')
            if (field%kind /= json_string) return
            filename = json_string_value(field)
            if (index(trim(filename), '/src/lib.f90') > 0) has_library = .true.
            if (index(trim(filename), '/app/main.f90') > 0) has_main = .true.

            arguments = json_member(entry, 'arguments')
            if (arguments%kind /= json_array .or. json_size(arguments) == 0) return
            argument = json_element(arguments, 1)
            if (argument%kind /= json_string) return
            value = json_string_value(argument)
            if (len(value) < 8) return
            if (value(len(value) - 7:) /= 'gfortran') return

            has_compile = .false.
            has_module_dir = .false.
            has_source_arg = .false.
            do j = 1, json_size(arguments)
                argument = json_element(arguments, j)
                if (argument%kind /= json_string) return
                value = json_string_value(argument)
                if (value == '-c') has_compile = .true.
                if (value == trim(filename)) has_source_arg = .true.
                if (len(value) >= 2) then
                    if (value(1:2) == '-J') has_module_dir = .true.
                end if
            end do
            if (.not. has_compile .or. .not. has_module_dir .or. &
                .not. has_source_arg) return
        end do
        valid_compdb = has_library .and. has_main
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
