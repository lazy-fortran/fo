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
    character(len=*), parameter :: skip_record = &
        '{"name":"test_relative_pass","status":"skip"}'
    character(len=*), parameter :: skip_report = '{"tests":['//skip_record// &
        '],"summary":{"passed":0,"failed":0,"skipped":1},"exit_code":0}'

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
    call check(skip_report, 'SKIP', &
        'native skip remains a distinct completed verdict')
    call check('{"tests":[{"name":"test_relative_pass","status":"skip",'// &
        '"reason":"CTest test is disabled"}],'// &
        '"summary":{"passed":0,"failed":0,"skipped":1,"total_seconds":0.0},'// &
        '"exit_code":0}', 'SKIP', 'disabled native case has valid skip metadata')
    call invalid_skip_fields('', 'skip requires the producer summary and exit code')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":1}', &
        'skip requires the producer exit code')
    call invalid_skip_fields(',"exit_code":0', 'skip requires the producer summary')
    call invalid_skip_fields(',"summary":{"passed":0,"skipped":1},"exit_code":0', &
        'skip requires the producer failed count')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0},"exit_code":0', &
        'skip requires the producer skipped count')
    call invalid_skip_fields(',"summary":{"passed":1,"failed":0,"skipped":1},'// &
        '"exit_code":0', 'passed report count contradicts the skipped case')
    call invalid_skip_fields(',"summary":{"failed":0,"skipped":1},"exit_code":0', &
        'skip requires the producer passed count')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":1},'// &
        '"exit_code":1', 'nonzero report exit contradicts the skipped case')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":1,"skipped":1},'// &
        '"exit_code":0', 'failed report count contradicts the skipped case')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":0},'// &
        '"exit_code":0', 'zero skipped count contradicts the skipped case')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":-1},'// &
        '"exit_code":0', 'negative skipped count cannot qualify a skip')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":1},'// &
        '"exit_code":0.0', 'skip report exit code must be an integer')
    call invalid_skip_fields(',"summary":[],"exit_code":0', &
        'skip summary must be an object')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":"0","skipped":1},'// &
        '"exit_code":0', 'skip failed count must be an integer')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":true},'// &
        '"exit_code":0', 'skip skipped count must be an integer')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":1},'// &
        '"exit_code":0,"exit_code":0', 'duplicate skip report exit code is invalid')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":1},'// &
        '"summary":{"passed":0,"failed":0,"skipped":1},"exit_code":0', &
        'duplicate skip summary is invalid')
    call invalid_skip_fields(',"summary":{'// &
        '"passed":0,"failed":0,"failed":0,"skipped":1},'// &
        '"exit_code":0', 'duplicate skip failed count is invalid')
    call invalid_skip_fields(',"summary":{'// &
        '"passed":0,"failed":0,"skipped":1,"skipped":1},'// &
        '"exit_code":0', 'duplicate skip skipped count is invalid')
    call invalid_skip_fields(',"summary":{"passed":0,"failed":0,"skipped":1},'// &
        '"exit_code":0,"ex\u0069t_code":0', &
        'decoded duplicate skip report exit code is invalid')
    call check('{"tests":[{"name":"test_relative_pass","status":"fail",'// &
        '"output":"{\"status\":\"skip\"}"}]}', 'FAIL', &
        'skip-shaped user output cannot hide a native failure')
    call invalid('{"tests":[{"name":"test_relative_pass","status":"untested"}]}', &
        'an untested case does not become a native skip')
    call invalid('{"tests":[{"name":"test_relative_pass","status":"skip",'// &
        '"status":"pass"}]}', 'duplicate skip verdict remains malformed')
    call write_text(log_path, skip_report//new_line('a'))
    call assert_true(runner_case_outcome(log_path, target, 1) == 'INFRA_ERROR', &
        'a nonzero runner exit contradicts a claimed native skip')
    call assert_true(runner_case_outcome(log_path, target, 0) == 'SKIP', &
        'a zero runner exit agrees with the complete native skip report')
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

    subroutine invalid_skip_fields(fields, description)
        character(len=*), intent(in) :: fields, description

        call invalid('{"tests":['//skip_record//']'//fields//'}', description)
    end subroutine invalid_skip_fields

    subroutine check_named(report, requested_name, expected, description)
        character(len=*), intent(in) :: report, requested_name, expected, description
        character(len=16) :: observed

        call write_text(log_path, report//new_line('a'))
        observed = runner_case_outcome(log_path, requested_name)
        call assert_true(trim(observed) == expected, description// &
            ': expected '//expected//', observed '//trim(observed))
    end subroutine check_named

end program test_gremlin_runner_report
