program test_compiler_artifacts
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_find_executable, fs_make_dir, fs_remove_tree
    use fo_gfortran_build, only: gfortran_build
    use fo_util, only: make_tmpfile
    use fo_test_cli, only: run_external
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    implicit none

    character(len=512) :: project, compiler, wrapper, marker, log, object, old_fc
    integer :: unit, status, old_status, result, mode
    logical :: found

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(code)
            import c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: code
        end function setenv
        function unsetenv(name) bind(C, name='unsetenv') result(code)
            import c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: code
        end function unsetenv
    end interface

    call fs_find_executable('gfortran', compiler, found)
    if (.not. found) error stop 'artifact oracle needs the real gfortran'
    call make_tmpfile('fo_compiler_artifacts', project)
    call fs_make_dir(trim(project)//'/src')
    log = trim(project)//'/build.log'
    wrapper = trim(project)//'/fixture-gfortran'
    marker = trim(project)//'/silent'
    object = trim(project)//'/build/fo/obj/src_answer.f90.o'
    open (newunit=unit, file=trim(project)//'/fpm.toml', status='replace')
    write (unit, '(a)') 'name="compiler-artifacts-oracle"'
    close (unit)
    open (newunit=unit, file=trim(project)//'/src/answer.f90', status='replace')
    write (unit, '(a)') 'module answer'
    write (unit, '(a)') 'implicit none'
    write (unit, '(a)') 'integer :: value = 42'
    write (unit, '(a)') 'end module answer'
    close (unit)
    call write_wrapper()
    call check_recursion_guard()
    call get_environment_variable('FO_FC', old_fc, status=old_status)
    status = setenv('FO_FC'//c_null_char, trim(wrapper)//c_null_char, 1_c_int)
    if (status /= 0) error stop 'cannot select compiler wrapper'

    call gfortran_build(project, log, result, use_cache=.false., build_apps=.false.)
    if (result /= 0) error stop 'real compiler must first build the source'
    inquire (file=trim(object), exist=found)
    if (.not. found) error stop 'real compiler produced no object'

    ! A silent compiler must neither preserve a stale object nor pass with an
    ! absent or empty output. The same executable identity avoids tree reset.
    do mode = 1, 4
        open (newunit=unit, file=trim(marker), status='replace')
        if (mode == 3) write (unit, '(a)') 'empty'
        if (mode == 4) write (unit, '(a)') 'retry'
        close (unit)
        call gfortran_build(project, log, result, use_cache=.false., &
            build_apps=.false.)
        if (result == 0) then
            if (mode == 4) error stop 'compiler retry reused an earlier failed output'
            error stop 'compiler success without object was accepted'
        end if
        inquire (file=trim(object), exist=found)
        if (found) error stop 'invalid compiler output survived a failed build'
        call check_diagnostic()
        if (mode == 4) then
            inquire (file=trim(marker)//'.retried', exist=found)
            if (.not. found) error stop 'compiler retry was not exercised'
        end if
    end do

    if (old_status == 0) then
        status = setenv('FO_FC'//c_null_char, trim(old_fc)//c_null_char, 1_c_int)
    else
        status = unsetenv('FO_FC'//c_null_char)
    end if
    if (status /= 0) error stop 'cannot restore compiler selection'
    call fs_remove_tree(trim(project))
    print '(a)', 'PASS: compiler requires fresh objects, including after a failed retry'

contains

    subroutine write_wrapper()
        integer :: unit
        type(string_list_t) :: arguments
        type(process_result_t) :: child

        ! A separate program cannot interpret compiler arguments as a request to
        ! run this test. execv replaces one process; it never creates children.
        open (newunit=unit, file=trim(wrapper)//'.f90', status='replace')
        write (unit, '(a)') &
            'program fixture_compiler', &
            'use iso_c_binding, only: c_char, c_int, c_ptr, c_loc, &', &
            '    c_null_char, c_null_ptr', &
            'use iso_fortran_env, only: error_unit', &
            'implicit none', &
            'character(len=4096) :: argument, real_compiler, marker, output', &
            'character(len=32) :: mode', &
            'character(kind=c_char), allocatable, target :: bytes(:,:)', &
            'type(c_ptr), allocatable :: argv(:)', &
            'integer :: count, first, i, j, n, unit, ios, status', &
            'logical :: exists', &
            'interface', &
            '    function execv(path, args) bind(C, name="execv") result(code)', &
            '        import c_char, c_int, c_ptr', &
            '        character(kind=c_char), intent(in) :: path(*)', &
            '        type(c_ptr), intent(in) :: args(*)', &
            '        integer(c_int) :: code', &
            '    end function', &
            '    function setenv(name, value, overwrite) &', &
            '            bind(C, name="setenv") result(code)', &
            '        import c_char, c_int', &
            '        character(kind=c_char), intent(in) :: name(*), value(*)', &
            '        integer(c_int), value :: overwrite', &
            '        integer(c_int) :: code', &
            '    end function', &
            '    subroutine c_exit(code) bind(C, name="exit")', &
            '        import c_int', &
            '        integer(c_int), value :: code', &
            '    end subroutine', &
            'end interface', &
            'call get_environment_variable("FO_TEST_ARTIFACT_DEPTH", &', &
            '    length=n, status=status)', &
            'if (status /= 1) then', &
            '    write(error_unit, "(a)") "compiler fixture recursion rejected"', &
            '    flush(error_unit)', &
            '    call c_exit(86_c_int)', &
            'end if'
        write (unit, '(a)') 'real_compiler = '//fortran_literal(trim(compiler))
        write (unit, '(a)') 'marker = '//fortran_literal(trim(marker))
        write (unit, '(a)') &
            'count = command_argument_count()', &
            'first = 1', &
            'call get_command_argument(1, argument)', &
            'if (trim(argument) == "--fixture-recurse") then', &
            '    if (count < 2) call c_exit(87_c_int)', &
            '    call get_command_argument(2, real_compiler)', &
            '    first = 3', &
            'end if', &
            'inquire(file=trim(marker), exist=exists)', &
            'if (exists) then', &
            '    mode = ""', &
            '    output = ""', &
            '    do i = first, count - 1', &
            '        call get_command_argument(i, argument)', &
            '        if (trim(argument) /= "-o") cycle', &
            '        call get_command_argument(i + 1, output)', &
            '        exit', &
            '    end do', &
            '    open(newunit=unit, file=trim(marker), status="old")', &
            '    read(unit, "(a)", iostat=ios) mode', &
            '    close(unit)', &
            '    if (trim(mode) == "retry") then', &
            '        inquire(file=trim(marker)//".failed", exist=exists)', &
            '        if (exists) then', &
            '            call touch(trim(marker)//".retried", "")', &
            '            stop', &
            '        end if', &
            '        call touch(trim(marker)//".failed", "")', &
            '        call touch(trim(output), "failed-output")', &
            '        write(error_unit, "(a)") &', &
            '            "Fatal Error: Cannot read module file"', &
            '        flush(error_unit)', &
            '        call c_exit(1_c_int)', &
            '    end if', &
            '    if (trim(mode) == "empty") call touch(trim(output), "")', &
            '    stop', &
            'end if', &
            'n = count - first + 2', &
            'allocate(bytes(4097,n), argv(n + 1))', &
            'bytes = c_null_char', &
            'do i = 1, n', &
            '    argument = real_compiler', &
            '    if (i > 1) then', &
            '        call get_command_argument(first + i - 2, argument, status=ios)', &
            '        if (ios /= 0) call c_exit(91_c_int)', &
            '    end if', &
            '    do j = 1, len_trim(argument)', &
            '        bytes(j,i) = argument(j:j)', &
            '    end do', &
            '    argv(i) = c_loc(bytes(1,i))', &
            'end do', &
            'argv(n + 1) = c_null_ptr', &
            'status = setenv("FO_TEST_ARTIFACT_DEPTH"//c_null_char, &', &
            '    "1"//c_null_char, 1_c_int)', &
            'if (status /= 0) call c_exit(88_c_int)', &
            'status = execv(trim(real_compiler)//c_null_char, argv)', &
            'call c_exit(89_c_int)', &
            'contains', &
            'subroutine touch(path, contents)', &
            '    character(len=*), intent(in) :: path, contents', &
            '    integer :: unit', &
            '    if (len(path) == 0) call c_exit(90_c_int)', &
            '    open(newunit=unit, file=path, status="replace")', &
            '    if (len(contents) > 0) write(unit, "(a)", advance="no") contents', &
            '    close(unit)', &
            'end subroutine', &
            'end program'
        close (unit)

        call list_add(arguments, trim(wrapper)//'.f90')
        call list_add(arguments, '-o')
        call list_add(arguments, trim(wrapper))
        call run_external(trim(compiler), arguments, trim(project), child, &
            timeout_ms=30000)
        if (child%runner_failed) error stop 'cannot launch native fixture compiler'
        if (child%exit_code /= 0) then
            if (allocated(child%stderr)) print '(a)', child%stderr
            error stop 'cannot build native compiler fixture'
        end if
    end subroutine write_wrapper

    function fortran_literal(value) result(literal)
        character(len=*), intent(in) :: value
        character(:), allocatable :: literal
        integer :: i

        literal = "'"
        do i = 1, len(value)
            literal = literal//value(i:i)
            if (value(i:i) == "'") literal = literal//"'"
        end do
        literal = literal//"'"
    end function fortran_literal

    subroutine check_recursion_guard()
        type(string_list_t) :: arguments
        type(process_result_t) :: child
        logical :: exists

        ! Force forwarding back into the fixture. One exec replaces the same
        ! process, and the inherited guard must reject it before any work.
        call list_add(arguments, '--fixture-recurse')
        call list_add(arguments, trim(wrapper))
        call list_add(arguments, '-c')
        call list_add(arguments, trim(project)//'/src/answer.f90')
        call list_add(arguments, '-o')
        call list_add(arguments, trim(project)//'/recursion.o')
        call run_external(trim(wrapper), arguments, trim(project), child, &
            timeout_ms=2000)
        if (child%runner_failed) error stop 'recursion guard process failed'
        if (child%timed_out) error stop 'recursion guard did not terminate boundedly'
        if (child%exit_code /= 86) error stop 'recursive compiler was not rejected'
        if (.not. allocated(child%stderr)) error stop 'recursion diagnostic missing'
        if (index(child%stderr, 'compiler fixture recursion rejected') == 0) &
            error stop 'recursion diagnostic missing'
        inquire (file=trim(project)//'/recursion.o', exist=exists)
        if (exists) error stop 'recursive compiler unexpectedly produced an object'
        print '(a)', 'PASS: compiler fixture recursion is bounded without forks'
    end subroutine check_recursion_guard

    subroutine check_diagnostic()
        character(len=1024) :: line
        integer :: unit, ios
        logical :: found

        found = .false.
        open (newunit=unit, file=trim(log), status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'compiler produced no nonempty object') > 0) &
                found = .true.
        end do
        close (unit)
        if (.not. found) error stop 'compiler artifact diagnostic was not reported'
    end subroutine check_diagnostic

end program test_compiler_artifacts
