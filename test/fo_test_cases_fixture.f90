module fo_test_cases_fixture
    !! The multi-case fixture of the test-case oracles (#217): an fpm package
    !! or a CMake project with the library modules tc_one and tc_two, the
    !! executable test_multi with the cases one (uses tc_one) and two (uses
    !! tc_two) that implements the fo listing protocol, and test_sched with a
    !! steady, a flaky (fails on its first run, then passes) and a slow case.
    !! The protocol variable names are filled in at run time so that the
    !! executables of the oracles themselves do not look like protocol
    !! executables to fo.
    use fo_test_harness, only: join_path, write_lines, append_text
    implicit none
    private

    public :: cases_fixture_write

contains

    function protocol(text) result(out)
        !! Fixture source with the protocol variable names filled in at run
        !! time: the names are not literals of this test's own executable,
        !! which would then look like a protocol executable to fo.
        character(len=*), intent(in) :: text
        character(len=len(text) + 16) :: out
        character(len=4) :: list_word, case_word
        character(len=12) :: list_name, case_name
        integer :: at

        list_word = 'LIST'
        case_word = 'CASE'
        write (list_name, '(a,a)') 'FO_TEST_', list_word
        write (case_name, '(a,a)') 'FO_TEST_', case_word
        out = text
        at = index(out, '@LIST@')
        if (at > 0) out = out(1:at - 1)//list_name//trim(out(at + 6:))
        at = index(out, '@CASE@')
        if (at > 0) out = out(1:at - 1)//case_name//trim(out(at + 6:))
    end function protocol

    subroutine write_source(path, lines)
        character(len=*), intent(in) :: path
        character(len=96), intent(in) :: lines(:)
        character(len=96) :: filled(size(lines))
        integer :: i

        do i = 1, size(lines)
            filled(i) = protocol(lines(i))
        end do
        call write_lines(path, filled)
    end subroutine write_source

    subroutine cases_fixture_write(dir, backend, scheduling)
        !! backend is 'fpm' or 'cmake'; scheduling (default true) adds test_sched.
        character(len=*), intent(in) :: dir, backend
        logical, intent(in), optional :: scheduling
        logical :: with_sched

        with_sched = .true.
        if (present(scheduling)) with_sched = scheduling

        call write_lines(join_path(dir, 'src/tc_one.f90'), [character(len=48) :: &
            'module tc_one', 'implicit none', 'contains', &
            'integer function one_value()', 'one_value = 1', 'end function', &
            'end module'])
        call write_lines(join_path(dir, 'src/tc_two.f90'), [character(len=48) :: &
            'module tc_two', 'implicit none', 'contains', &
            'integer function two_value()', 'two_value = 2', 'end function', &
            'end module'])
        call write_source(join_path(dir, 'test/test_multi.f90'), [character(len=96) :: &
            'program test_multi', 'use tc_one, only: one_value', &
            'use tc_two, only: two_value', 'implicit none', &
            'character(len=512) :: value', 'integer :: status', &
            "call get_environment_variable('@LIST@', value, status=status)", &
            'if (status == 0) then', &
            "write (*, '(a)') 'fo-case one'//achar(9)//'src/tc_one.f90'", &
            "write (*, '(a)') 'fo-case two'//achar(9)//'src/tc_two.f90'", &
            'stop 0', 'end if', &
            "call get_environment_variable('@CASE@', value, status=status)", &
            "if (status /= 0) value = ''", &
            "if (len_trim(value) == 0 .or. trim(value) == 'one') &", &
            "call mark('one', one_value() == 1)", &
            "if (len_trim(value) == 0 .or. trim(value) == 'two') &", &
            "call mark('two', two_value() == 2)", 'contains', &
            'subroutine mark(name, ok)', 'character(len=*), intent(in) :: name', &
            'logical, intent(in) :: ok', 'integer :: unit', &
            "call get_environment_variable('TC_MARK_DIR', value, status=status)", &
            'if (.not. ok) error stop 1', 'if (status /= 0) return', &
            "open (newunit=unit, file=trim(value)//'/ran_'//name, status='replace')", &
            'close (unit)', 'end subroutine', 'end program'])
        if (with_sched) call write_source(join_path(dir, 'test/test_sched.f90'), [character(len=96) :: &
            'program test_sched', 'implicit none', 'character(len=512) :: value', &
            'integer :: status, unit', 'logical :: seen', &
            "call get_environment_variable('@LIST@', value, status=status)", &
            'if (status == 0) then', "write (*, '(a)') 'fo-case steady'", &
            "write (*, '(a)') 'fo-case flaky'", "write (*, '(a)') 'fo-case slow'", &
            'stop 0', 'end if', &
            "call get_environment_variable('@CASE@', value, status=status)", &
            "if (status /= 0) value = ''", 'select case (trim(value))', &
            "case ('flaky')", &
            "call get_environment_variable('FLAKY_STATE', value, status=status)", &
            'if (status /= 0) error stop 3', &
            'inquire (file=trim(value), exist=seen)', 'if (.not. seen) then', &
            "open (newunit=unit, file=trim(value), status='new')", 'close (unit)', &
            'error stop 7', 'end if', "case ('slow')", &
            "call execute_command_line('sleep 120')", 'end select', &
            'end program'])
        if (backend == 'fpm') then
            call write_lines(join_path(dir, 'fpm.toml'), [character(len=80) :: &
                'name = "tcases"', '[extra.fo]', &
                'slow-timeout = { period = 1, terminate-after = 2 }'])
        else
            call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=112) :: &
                'cmake_minimum_required(VERSION 3.20)', 'project(tcases Fortran)', &
                'enable_testing()', 'add_library(tc_one STATIC src/tc_one.f90)', &
                'add_library(tc_two STATIC src/tc_two.f90)', &
                'add_executable(test_multi test/test_multi.f90)', &
                'target_link_libraries(test_multi tc_one tc_two)', &
                'add_test(NAME test_multi COMMAND test_multi)'])
            if (with_sched) call append_text(join_path(dir, 'CMakeLists.txt'), &
                'add_executable(test_sched test/test_sched.f90)'//new_line('a')// &
                'add_test(NAME test_sched COMMAND test_sched)'//new_line('a')// &
                'set_tests_properties(test_sched PROPERTIES FO_RETRIES 1 '// &
                'FO_SLOW_PERIOD 1 FO_SLOW_TERMINATE_AFTER 2)'//new_line('a'))
        end if
    end subroutine cases_fixture_write

end module fo_test_cases_fixture
