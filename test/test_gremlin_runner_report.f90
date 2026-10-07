program test_gremlin_runner_report
    use fo_gremlin_supervisor, only: runner_case_outcome
    use fo_test_harness, only: make_scratch, write_text, assert_true, finish_assertions
    implicit none

    character(:), allocatable :: scratch, log_path
    character(:), allocatable :: long_target
    character(len=*), parameter :: target = 'test_relative_pass'
    character(len=*), parameter :: pass_record = &
        '{"name":"test_relative_pass","status":"pass","seconds":0.01,"output":""}'
    character(len=*), parameter :: pass_report = '{"tests":['//pass_record// &
        '],"summary":{"passed":1,"failed":0,"skipped":0,"total_seconds":0.01},'// &
        '"exit_code":0}'

    call make_scratch('fo-runner-report', scratch)
    log_path = scratch//'/runner.log'
    ! This is the real successful nested runner shape that was classified INFRA_ERROR.
    call check('fo build tests [####################] 1/1 100%'//new_line('a')// &
        pass_report, 'PASS', 'successful nested report following build progress')
    call check('{"tests":[{"name":"test_relative_pass","status":"fail",'// &
        '"seconds":0.01,"output":"{\"status\":\"pass\"}"}],'// &
        '"summary":{"passed":0,"failed":1},"exit_code":1}', 'FAIL', &
        'real failure remains FAIL despite PASS-shaped user output')
    call check('{"tests":[{"name":"test_relative_pass","status":"timeout",'// &
        '"seconds":1.00,"output":"","timeout_kind":"wall"}],'// &
        '"summary":{"passed":0,"failed":1},"exit_code":1}', 'TIMEOUT', &
        'nested timeout remains distinct from failure and infrastructure')
    call check('  {"summary":{},"tests":['//pass_record//']}', 'PASS', &
        'valid structured report accepts whitespace and field order')
    call check('{"tests":[{"name":"another_case","status":"pass"},'// &
        pass_record//']}', 'PASS', 'finds the requested case among report entries')
    call check('{"tests":[{"name":"another_case","status":"pass"}]}', &
        'INFRA_ERROR', 'unrelated successful case cannot credit requested case')
    call invalid('{"tests":['//pass_record//','//pass_record//']}', &
        'duplicate requested case')
    call invalid('{"tests":['//pass_record//'],"tests":[]}', 'duplicate tests key')
    call invalid('{"tests":[{"name":"wrong","name":"test_relative_pass",'// &
        '"status":"pass"}]}', 'duplicate name key')
    call invalid('{"tests":[{"name":"test_relative_pass","status":"fail",'// &
        '"status":"pass"}]}', 'duplicate status key')
    call invalid('{"tests":[{"name":17,"unrelated":"test_relative_pass",'// &
        '"status":"pass"}]}', 'wrong name type cannot capture later string')
    call invalid('{"tests":[{"name":"test_relative_pass","status":false,'// &
        '"unrelated":"pass"}]}', 'wrong status type cannot capture later string')
    call invalid('{"tests":[{"name":"test_relative_pass","status":{},'// &
        '"unrelated":"pass"}]}', 'container status cannot capture later string')
    call invalid('{"tests":[{"name":"test_relative_pass"}]}', 'missing status')
    call invalid('{"tests":[{"name":"test_relative_pass","status":"unknown"}]}', &
        'unsupported verdict')
    call invalid('{"tests":[{} ,'//pass_record//']}', &
        'missing fields in a report entry')
    call invalid('{"tests":["noise",'//pass_record//']}', 'non-object report entry')
    call invalid('{"tests":['//pass_record, 'truncated JSON')
    call invalid(pass_report//' trailing', 'trailing garbage')
    call invalid('{"tests":[{"name":"'//target//repeat('x', 150)// &
        '","status":"pass"}]}', 'overlong name cannot match by truncation')
    long_target = target//repeat('x', 1500)
    call check_named('{"tests":[{"name":"'//long_target// &
        '","status":"pass"}]}', long_target, 'PASS', &
        'long result name is matched without truncation')
    call invalid('{"tests":[{"name":"test_relative_pass","status":"pass'// &
        repeat(' ', 40)//'extra"}]}', 'overlong status cannot match by truncation')
    call invalid('{"tests":[{"name":"test_relative_pass","status":"fail",'// &
        '"sta\u0074us":"pass"}]}', 'duplicate decoded status key')
    call finish_assertions()

contains

    subroutine check(report, expected, description)
        character(len=*), intent(in) :: report, expected, description
        character(len=16) :: observed

        call write_text(log_path, report//new_line('a'))
        observed = runner_case_outcome(log_path, target)
        call assert_true(trim(observed) == expected, description// &
            ': expected '//expected//', observed '//trim(observed))
    end subroutine check

    subroutine invalid(report, description)
        character(len=*), intent(in) :: report, description

        call check(report, 'INFRA_ERROR', description)
    end subroutine invalid

    subroutine check_named(report, requested_name, expected, description)
        character(len=*), intent(in) :: report, requested_name, expected, description
        character(len=16) :: observed

        call write_text(log_path, report//new_line('a'))
        observed = runner_case_outcome(log_path, requested_name)
        call assert_true(trim(observed) == expected, description// &
            ': expected '//expected//', observed '//trim(observed))
    end subroutine check_named

end program test_gremlin_runner_report
