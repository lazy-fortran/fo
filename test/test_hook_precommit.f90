program test_hook_precommit
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, make_directory, join_path, write_text, read_text, remove_tree, &
        assert_contains, assert_equal_string, assert_true, assert_process_ok, &
        finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_json, gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: driver, scratch, project, original, tree, session
    type(process_result_t) :: result
    type(json_value_t) :: reply
    type(string_list_t) :: args

    call resolve_driver(driver)
    call make_scratch('fo-hook', scratch)
    project = join_path(scratch, 'project')
    call write_text(project//'/fpm.toml', 'name = "hook_probe"'//nl)
    original = 'module answer_mod'//nl//'    implicit none'//nl// &
        '    integer, parameter :: answer = 42'//nl//'end module answer_mod'//nl
    call write_text(project//'/src/answer.f90', original)
    call write_text(project//'/test/test_answer.f90', 'program test_answer'//nl// &
        '    use answer_mod, only: answer'//nl//'    implicit none'//nl// &
        '    if (answer /= 42) stop 1'//nl//'end program test_answer'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/answer.f90', 'test/test_answer.f90'])
    call hook()
    call assert_process_ok(result, 'cold staged build and fast test')
    call assert_contains(result%stdout, '"fast_passed":1', 'cold fast test ran')
    call assert_contains(result%stdout, '"cache_hit":false', 'first check misses')
    call assert_contains(result%stdout, '"build":"PASS', 'mandatory build passed')

    call write_text(project//'/src/answer.f90', 'unstaged invalid Fortran'//nl)
    call hook()
    call assert_process_ok(result, 'ignore unstaged broken source')
    call assert_contains(result%stdout, '"cache_hit":true', 'exact staged cache hit')
    call assert_equal_string(read_text(project//'/src/answer.f90'), &
        'unstaged invalid Fortran'//nl, 'working file preserved')

    ! A new staged edit gets its only evidence from the real resident lane.
    call write_text(project//'/src/answer.f90', '! Gremlin-verified edit'//nl//original)
    call git([character(len=64) :: 'add', 'src/answer.f90'])
    call make_directory(scratch//'/state/tmp')
    call lane_args('start')
    call list_add(args, '--target')
    call list_add(args, 'test_answer')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, scratch//'/cache', scratch//'/state', &
        args, reply, result, 30000)
    call assert_process_ok(result, 'start resident hook proof lane')
    session = gremlin_field(reply, 'session_id')
    call lane_args('wait')
    call list_add(args, '--until')
    call list_add(args, 'quiescent')
    call list_add(args, '--wait-ms')
    call list_add(args, '20000')
    call gremlin_json(driver, project, scratch//'/cache', scratch//'/state', &
        args, reply, result, 30000)
    call assert_process_ok(result, 'resident generation reaches quiescence')
    call hook()
    call assert_process_ok(result, 'reuse Gremlin-verified staged content')
    call assert_contains(result%stdout, '"cache_hit":true', 'real Gremlin receipt hit')
    call gremlin_stop_lane(driver, project, scratch//'/cache', scratch//'/state', &
        'hook-proof', session)

    call write_text(project//'/src/answer.f90', 'module answer_mod'//nl// &
        '    implicit none'//nl//'    integer, parameter :: answer = missing_symbol'//nl// &
        'end module answer_mod'//nl)
    call git([character(len=64) :: 'add', 'src/answer.f90'])
    call write_text(project//'/src/answer.f90', original)
    call hook()
    call assert_true(result%exit_code /= 0, 'partially staged build failure blocks')
    call assert_contains(result%stdout, 'staged build failed', 'compiler rejection reported')
    call assert_equal_string(read_text(project//'/src/answer.f90'), original, &
        'partially staged worktree preserved')
    call git([character(len=64) :: 'add', 'src/answer.f90'])
    call write_text(project//'/src/answer.f90', 'module answer_mod'//nl// &
        'implicit none'//nl//'integer, parameter :: answer = 42'//nl// &
        'end module answer_mod'//nl)
    call git([character(len=64) :: 'add', 'src/answer.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'staged formatting blocks')
    call assert_contains(result%stdout, 'needs formatting', 'format check-only diagnostic')
    call write_text(project//'/src/answer.f90', original)
    call git([character(len=64) :: 'add', 'src/answer.f90'])

    call write_text(project//'/test/test_answer.f90', 'program test_answer'//nl// &
        '    use answer_mod, only: answer'//nl//'    implicit none'//nl// &
        '    if (answer /= 1) stop 1'//nl//'end program test_answer'//nl)
    call git([character(len=64) :: 'add', 'test/test_answer.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'failing completed fast test blocks')
    call assert_contains(result%stdout, 'fast test failed: test_answer', 'test failure identified')
    call write_text(project//'/test/test_answer.f90', 'program test_answer'//nl// &
        '    use answer_mod, only: answer'//nl//'    implicit none'//nl// &
        '    if (answer /= 42) stop 1'//nl//'end program test_answer'//nl)
    call git([character(len=64) :: 'add', 'test/test_answer.f90'])

    call write_text(project//'/fpm.toml', 'name = "hook_probe"'//nl// &
        '[extra.fo.test-groups]'//nl//'slow = { tests = ["test_answer"] }'//nl)
    call git([character(len=64) :: 'add', 'fpm.toml'])
    call hook()
    call assert_process_ok(result, 'explicit slow label is deferred')
    call assert_contains(result%stdout, '"deferred":["test_answer"]', 'slow name reported')
    call assert_contains(result%stdout, '"fast_passed":0', 'slow test never passed implicitly')

    ! CTest's external command gives an independent deterministic budget oracle.
    project = join_path(scratch, 'cmake')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_cmake LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_library(answer answer.f90)'//nl// &
        'add_test(NAME wait COMMAND "${CMAKE_COMMAND}" -E sleep 3)'//nl// &
        'add_test(NAME slow COMMAND "${CMAKE_COMMAND}" -E false)'//nl// &
        'set_tests_properties(slow PROPERTIES LABELS slow)'//nl)
    call write_text(project//'/answer.f90', original)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt', 'answer.f90'])
    call git([character(len=64) :: 'config', 'fo.hookBudget', '2'])
    call hook()
    call assert_process_ok(result, 'budget exhaustion preserves mandatory build')
    call assert_contains(result%stdout, '"build":"PASS', 'over-budget build passes')
    call assert_contains(result%stdout, '"deferred_count":2', 'unrun tests deferred')
    call assert_contains(result%stdout, '"fast_passed":0', 'timed out test is not PASS')
    call git([character(len=64) :: 'config', 'fo.hookBudget', '10'])
    call hook()
    call assert_process_ok(result, 'slow fast-tier test only warns')
    call assert_contains(result%stdout, '"slow_fast_tests":["wait"]', 'slow warning names test')
    call assert_contains(result%stdout, '"fast_passed":1', 'completed fast tier recorded')

    call write_text(project//'/CMakeLists.txt', read_text(project//'/CMakeLists.txt')// &
        'add_custom_target(wait_build ALL COMMAND "${CMAKE_COMMAND}" -E sleep 3)'//nl)
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    call git([character(len=64) :: 'config', 'fo.hookBuildCap', '1'])
    call hook()
    call assert_true(result%exit_code /= 0, 'mandatory build hard cap blocks')
    call assert_contains(result%stdout, 'staged build exceeded hard cap', 'hard cap identified')
    call finish_assertions(retain_failed_scratch=.true.)
    call remove_tree(scratch)
contains
    subroutine lane_args(action)
        character(len=*), intent(in) :: action
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, action)
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, 'hook-proof')
    end subroutine lane_args
    subroutine git(words)
        character(len=*), intent(in) :: words(:)
        integer :: i
        args = string_list_t()
        do i = 1, size(words)
            call list_add(args, trim(words(i)))
        end do
        call run_external('git', args, project, result)
        call assert_process_ok(result, 'git fixture command')
    end subroutine git
    subroutine hook()
        args = string_list_t()
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, timeout_ms=90000)
    end subroutine hook
end program test_hook_precommit
