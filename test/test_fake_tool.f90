program test_fake_tool
    !! The shared fake command must behave like the small shell scripts it replaces.
    use fo_fake_tool, only: fake_tool_dispatch, install_fake_tool
    use fo_test_harness, only: make_scratch, remove_tree, join_path, run_list, &
        read_text, write_text, finish_assertions, assert_equal_string, &
        assert_equal_integer, assert_true, string_list_t, list_add, &
        resolve_test_executable
    implicit none

    character(:), allocatable :: scratch, tool, calls_log, output
    type(string_list_t) :: environment
    character(len=1), parameter :: lf = achar(10)
    integer :: status

    call fake_tool_dispatch()
    call make_scratch('fo-fake-tool', scratch)
    tool = join_path(scratch, 'tool')
    calls_log = join_path(scratch, 'calls.log')

    ! Streams, exit status, argument logging and conditions.
    call install_fake_tool(tool, &
        'append '//calls_log//' "$ARGS|$ARG2|$OUT"\n'//lf// &
        'if $ARG1 = --version print "tool 1.2"'//lf// &
        'if $ARG1 = --version exit 0'//lf// &
        'if $ARG1 = fail eprint "broken"'//lf// &
        'if $ARG1 = fail exit 9'//lf// &
        'collect "*/src/*.f90|*.c" '//calls_log//lf// &
        'if ${FAKE_MODE:-plain} = loud print "loud $FAKE_MODE"'//lf// &
        'exit 3')
    call run_tool([character(len=64) :: '--version'], status, output)
    call assert_equal_integer(status, 0, 'version probe exits zero')
    call assert_equal_string(output, 'tool 1.2'//lf, 'version probe prints its line')
    call run_tool([character(len=64) :: 'fail', 'x'], status, output)
    call assert_equal_integer(status, 9, 'failing mode exits with its code')
    call assert_equal_string(output, 'broken'//lf, 'failing mode reports on stderr')
    call run_tool([character(len=64) :: 'a/src/m.f90', 'b.txt', 'z.c', '-o', 'out.o'], &
        status, output)
    call assert_equal_integer(status, 3, 'fall-through exit status')
    call assert_equal_string(output, '', 'plain mode prints nothing')
    call list_add(environment, 'FAKE_MODE=loud')
    call run_tool([character(len=64) :: 'q'], status, output, environment)
    call assert_equal_string(output, 'loud loud'//lf, 'environment expansion')
    call assert_equal_string(read_text(calls_log), &
        '--version||'//lf//'fail x|x|'//lf// &
        'a/src/m.f90 b.txt z.c -o out.o|b.txt|out.o'//lf//'a/src/m.f90'//lf//'z.c'//lf// &
        'q||'//lf, 'argument log and glob collection')

    ! Forwarding to a real tool and status propagation.
    call install_fake_tool(tool, &
        'run /bin/echo forwarded $@'//lf//'append '//calls_log//' "status=$?"\n'//lf// &
        'exec /usr/bin/true')
    call run_tool([character(len=64) :: 'one', 'two'], status, output)
    call assert_equal_integer(status, 0, 'exec propagates the real tool status')
    call assert_equal_string(output, 'forwarded one two'//lf, 'run forwards output')
    call assert_true(index(read_text(calls_log), 'status=0') > 0, 'run records its status')

    ! A barrier file: the tool waits for it up to its deadline.
    call install_fake_tool(tool, 'wait-file '//join_path(scratch, 'go')//' 50'//lf// &
        'ifexists '//join_path(scratch, 'go')//' exit 0'//lf//'exit 4')
    call run_tool([character(len=64) :: 'x'], status, output)
    call assert_equal_integer(status, 4, 'barrier gives up at its deadline')
    call write_text(join_path(scratch, 'go'), '')
    call run_tool([character(len=64) :: 'x'], status, output)
    call assert_equal_integer(status, 0, 'released barrier lets the tool succeed')

    call remove_tree(scratch)
    call finish_assertions()
    print '(a)', 'fake tool oracle PASS'

contains

    subroutine run_tool(arguments, exit_status, text, extra_environment)
        !! Run the installed tool through its interpreter entry, the same code the
        !! kernel starts for the tool's first line.
        character(len=*), intent(in) :: arguments(:)
        integer, intent(out) :: exit_status
        character(:), allocatable, intent(out) :: text
        type(string_list_t), optional, intent(in) :: extra_environment
        character(:), allocatable :: self
        type(string_list_t) :: command
        integer :: i
        logical :: ok

        call resolve_test_executable(self, ok)
        call assert_true(ok, 'resolve the test executable')
        call list_add(command, self)
        call list_add(command, '--fo-fake-tool')
        call list_add(command, tool)
        do i = 1, size(arguments)
            call list_add(command, trim(arguments(i)))
        end do
        call run_list(command, exit_status, text, environment=extra_environment)
    end subroutine run_tool
end program test_fake_tool
