program test_gremlin_public_readiness
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    implicit none

    character(len=4096) :: root, fixture, prefix, cli_file, rpc_file, rpc_input
    character(len=65536) :: cli_json, rpc_json, output
    character(len=32) :: conditions(5)
    integer :: unit, ios, status, failures, i
    integer(int64) :: clock

    failures = 0
    call get_environment_variable('PWD', root, status=ios)
    call check(ios == 0, 'project working directory is available')
    if (ios /= 0) stop 1
    call system_clock(clock)
    write (fixture, '(a,i0)') '/var/tmp/fo-public-readiness-', clock
    prefix = 'cd '//quote(trim(root))//' && FO_DISABLE_SELF_REFRESH=1 '// &
        'fo exec --no-build fo '
    cli_file = trim(fixture)//'-cli.json'
    rpc_file = trim(fixture)//'-rpc.json'
    rpc_input = trim(fixture)//'-rpc-input.jsonl'
    call command('mkdir -p '//quote(trim(fixture)//'/test'), status)
    if (status /= 0) stop 1
    open (newunit=unit, file=trim(fixture)//'/fpm.toml', status='replace')
    write (unit, '(a)') 'name = "public_readiness_fixture"'
    write (unit, '(a)') '[extra.fo]'
    write (unit, '(a)') 'test-timeout = 10'
    write (unit, '(a)') 'slow-test-timeout = 20'
    close (unit)
    do i = 1, 2
        open (newunit=unit, file=trim(fixture)//'/test/test_case_'// &
            achar(iachar('a') + i - 1)//'.f90', status='replace')
        write (unit, '(a)') 'program fixture_case_'//achar(iachar('a') + i - 1)
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'end program fixture_case_'//achar(iachar('a') + i - 1)
        close (unit)
    end do
    open (newunit=unit, file=trim(fixture)//'/test/test_case_slow.f90', status='replace')
    write (unit, '(a)') 'program fixture_slow'
    write (unit, '(a)') 'use, intrinsic :: iso_c_binding, only: c_int'
    write (unit, '(a)') 'implicit none'
    write (unit, '(a)') 'interface'
    write (unit, '(a)') 'function sleep_seconds(seconds) bind(C, name="sleep") result(left)'
    write (unit, '(a)') 'import :: c_int'
    write (unit, '(a)') 'integer(c_int), value :: seconds'
    write (unit, '(a)') 'integer(c_int) :: left'
    write (unit, '(a)') 'end function sleep_seconds'
    write (unit, '(a)') 'end interface'
    write (unit, '(a)') 'if (sleep_seconds(6_c_int) /= 0) error stop 1'
    write (unit, '(a)') 'end program fixture_slow'
    close (unit)

    call cli('start --seed 17 --random 2', cli_json, status)
    call check(status == 0, 'real CLI starts an owned fixture campaign')
    if (status == 0) then
        call cli('wait --until fully-verified --wait-ms 30000', cli_json, status)
        call check(status == 0, 'real CLI reads full verification')
        call check(field(cli_json, 'wait_satisfied') == 'true', &
            'slow case longer than five seconds passes with its normal budget')
        call cli('wait --until quiescent --wait-ms 30000', cli_json, status)
        call check(status == 0, 'fully covered fixture enters quiescence')
        call check(field(cli_json, 'wait_satisfied') == 'true', &
            'quiescent wait observes the completed fixture')
        call cli('status', cli_json, status)
        call mcp('status', '', rpc_json, status)
        call check(trim(cli_json) == trim(rpc_json), &
            'independent CLI and MCP processes report identical exact readiness facts')
        call check(field(cli_json, 'local_gate_green') == 'true', &
            'public gate is green for the verified current generation')
        call check(field(cli_json, 'fully_verified') == 'true', &
            'public full verification includes the slow inventory')
        call check(field(cli_json, 'gate_token_version') == '1', &
            'public gate token is explicitly versioned')
        conditions = [character(len=32) :: 'local-gate-green', 'ordinary-verified', &
            'fully-verified', 'quiescent', 'failure']
        do i = 1, size(conditions)
            call cli('wait --until '//trim(conditions(i))//' --wait-ms 0', &
                cli_json, status)
            call check(status == 0, 'CLI typed wait returns a factual result')
            call mcp('wait', trim(conditions(i)), rpc_json, status)
            call check(status == 0, 'MCP typed wait returns a factual result')
            call check(trim(cli_json) == trim(rpc_json), &
                trim(conditions(i))//' returns identical state and outcome across processes')
            if (i < 5) then
                call check(field(cli_json, 'wait_satisfied') == 'true', &
                    trim(conditions(i))//' is independently expected true')
            else
                call check(field(cli_json, 'wait_satisfied') == 'false', &
                    'failure wait cannot be satisfied by an all-pass fixture')
                call check(field(cli_json, 'wait_timed_out') == 'true', &
                    'false zero-duration failure wait reports its timeout')
            end if
        end do
        open (newunit=unit, file=trim(fixture)//'/test/test_case_flaky.f90', &
            status='replace')
        write (unit, '(a)') 'program fixture_flaky'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'logical :: seen'
        write (unit, '(a)') 'integer :: unit'
        write (unit, '(a)') "inquire(file='"//trim(fixture)//"-once', exist=seen)"
        write (unit, '(a)') 'if (.not. seen) then'
        write (unit, '(a)') "open(newunit=unit, file='"//trim(fixture)// &
            "-once', status='replace')"
        write (unit, '(a)') 'close(unit)'
        write (unit, '(a)') 'error stop 7'
        write (unit, '(a)') 'end if'
        write (unit, '(a)') 'end program fixture_flaky'
        close (unit)
        call cli('wait --until failure --wait-ms 30000', cli_json, status)
        call check(status == 0 .and. field(cli_json, 'wait_satisfied') == 'true', &
            'normal runner flaky verdict is a current-generation failure fact')
        call check(index(cli_json, '"status":"FLAKY"') > 0, &
            'Gremlin retains FLAKY rather than converting runner exit zero to PASS')
        call check(field(cli_json, 'local_gate_green') == 'false' .and. &
            field(cli_json, 'fully_verified') == 'false', &
            'a flaky current-generation case prevents both gate and full green')
    end if
    call cli('stop', cli_json, status)
    call check(status == 0, 'owned fixture campaign stops through the public CLI')
    if (status == 0) then
        call command('rm -rf '//quote(trim(fixture))//' '//quote(trim(cli_file))// &
            ' '//quote(trim(rpc_file))//' '//quote(trim(rpc_input))// &
            ' '//quote(trim(fixture)//'-once'), status)
    end if
    if (failures > 0) stop 1
    print '(a)', 'Public readiness: CLI/MCP facts, five typed waits and normal slow budget PASS'

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        if (condition) return
        failures = failures + 1
        write (error_unit, '(a)') 'FAIL: '//label
    end subroutine check

    subroutine command(text, exitcode)
        character(len=*), intent(in) :: text
        integer, intent(out) :: exitcode
        integer :: launch_error
        call execute_command_line(text, exitstat=exitcode, cmdstat=launch_error)
        if (launch_error /= 0) exitcode = launch_error
    end subroutine command

    subroutine cli(arguments, json, exitcode)
        character(len=*), intent(in) :: arguments
        character(len=*), intent(out) :: json
        integer, intent(out) :: exitcode
        call command(trim(prefix)//' gremlin '//arguments//' --dir '// &
            quote(trim(fixture))//' --lane public-readiness --json > '// &
            quote(trim(cli_file)), exitcode)
        call read_output(cli_file, json)
    end subroutine cli

    subroutine mcp(action, condition, json, exitcode)
        character(len=*), intent(in) :: action, condition
        character(len=*), intent(out) :: json
        integer, intent(out) :: exitcode
        integer :: rpc_unit
        character(len=:), allocatable :: parameters
        parameters = '"action":"gremlin_'//action//'","dir":"'//trim(fixture)// &
            '","lane_id":"public-readiness"'
        if (len_trim(condition) > 0) parameters = parameters// &
            ',"wait_until":"'//condition//'","wait_ms":0'
        open (newunit=rpc_unit, file=trim(rpc_input), status='replace')
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","id":1,"method":"initialize",'// &
            '"params":{"protocolVersion":"2024-11-05","capabilities":{},'// &
            '"clientInfo":{"name":"independent-fortran-oracle","version":"1"}}}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","method":"notifications/initialized"}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","id":2,"method":"tools/call",'// &
            '"params":{"name":"fo","arguments":{'//parameters//'}}}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","id":3,"method":"shutdown"}'
        write (rpc_unit, '(a)') '{"jsonrpc":"2.0","method":"exit"}'
        close (rpc_unit)
        call command(trim(prefix)//' mcp-server < '//quote(trim(rpc_input))// &
            ' > '//quote(trim(rpc_file)), exitcode)
        call read_output(rpc_file, output)
        json = field(output, 'text')
    end subroutine mcp

    subroutine read_output(filename, text)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: text
        integer :: file_unit, read_status, used
        character(len=65536) :: line
        text = ''
        used = 0
        open (newunit=file_unit, file=trim(filename), status='old', iostat=read_status)
        if (read_status /= 0) return
        do
            read (file_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (used + len_trim(line) > len(text)) exit
            text(used + 1:used + len_trim(line)) = trim(line)
            used = used + len_trim(line)
        end do
        close (file_unit)
    end subroutine read_output

    function field(json, key) result(value)
        character(len=*), intent(in) :: json, key
        character(len=65536) :: value
        integer :: at, cursor, used, n
        logical :: quoted
        value = ''
        at = index(json, '"'//key//'":')
        if (at == 0) return
        cursor = at + len(key) + 3
        n = len_trim(json)
        do while (cursor <= n)
            if (json(cursor:cursor) /= ' ') exit
            cursor = cursor + 1
        end do
        if (cursor > n) return
        quoted = json(cursor:cursor) == '"'
        if (quoted) cursor = cursor + 1
        used = 0
        do while (cursor <= n)
            if (quoted) then
                if (json(cursor:cursor) == '"') exit
                if (json(cursor:cursor) == achar(92)) then
                    cursor = cursor + 1
                    if (cursor > n) return
                end if
            else
                if (index(',}'//achar(10), json(cursor:cursor)) > 0) exit
            end if
            used = used + 1
            value(used:used) = json(cursor:cursor)
            cursor = cursor + 1
        end do
    end function field

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted
        integer :: i
        quoted = "'"
        do i = 1, len(text)
            if (text(i:i) == "'") then
                quoted = quoted//"'"//'"'//"'"//'"'//"'"
            else
                quoted = quoted//text(i:i)
            end if
        end do
        quoted = quoted//"'"
    end function quote
end program test_gremlin_public_readiness
