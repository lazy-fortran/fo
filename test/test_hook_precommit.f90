program test_hook_precommit
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, make_directory, join_path, write_text, read_text, remove_tree, &
        assert_contains, assert_equal_string, assert_true, assert_process_ok, &
        finish_assertions, copy_mtime
    use fo_test_gremlin_oracle, only: gremlin_json, gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: driver, scratch, project, original, tree, session
    type(process_result_t) :: result
    type(json_value_t) :: reply
    type(string_list_t) :: args, environment, cmake_args
    integer(int64) :: started, finished, rate
    character(len=:), allocatable :: tag
    logical :: downloaded

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
    call bounded_hook()
    call assert_process_ok(result, 'bounded hook reuses exact staged evidence')
    call assert_contains(result%stdout, '"cache_hit":true', 'bounded staged cache hit')

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
    call bounded_hook()
    call assert_true(result%exit_code /= 0, 'bounded hook rejects invalid staged content')
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
    call bounded_hook()
    call assert_process_ok(result, 'bounded CMake reuses prepared evidence')
    call assert_contains(result%stdout, '"cache_hit":true', 'bounded CMake receipt hit')
    call git([character(len=64) :: 'write-tree'])
    tree = trim(result%stdout)
    call git([character(len=64) :: 'symbolic-ref', 'HEAD', 'refs/heads/alternate'])
    call bounded_hook()
    call assert_process_ok(result, 'verify changed unborn Git ref')
    call assert_contains(result%stdout, '"cache_hit":false', 'unborn ref invalidates proof')
    call git([character(len=64) :: 'commit', '-qm', 'Create synthetic CMake provenance'])
    call bounded_hook()
    call assert_process_ok(result, 'verify newly committed Git provenance')
    call assert_contains(result%stdout, '"cache_hit":false', 'new HEAD invalidates proof')
    call git([character(len=64) :: 'commit', '--allow-empty', '-qm', 'Advance synthetic HEAD'])
    call git([character(len=64) :: 'write-tree'])
    call assert_equal_string(trim(result%stdout), tree, 'HEAD edit preserves staged tree')
    call bounded_hook()
    call assert_process_ok(result, 'verify changed HEAD at same staged tree')
    call assert_contains(result%stdout, '"cache_hit":false', 'changed HEAD cannot reuse proof')
    call git([character(len=64) :: 'tag', 'prepared-context'])
    call bounded_hook()
    call assert_process_ok(result, 'verify changed tag at same staged tree')
    call assert_contains(result%stdout, '"cache_hit":false', 'tag change invalidates proof')
    environment = string_list_t()
    call list_add(environment, 'FO_CMAKE_ARGS=-DHOOK_CONTEXT_FLAG=1')
    call bounded_hook(environment)
    call assert_process_ok(result, 'verify changed effective CMake arguments')
    call assert_contains(result%stdout, '"cache_hit":false', 'CMake arguments invalidate proof')
    environment = string_list_t()
    call list_add(environment, 'FFLAGS=-O1')
    call bounded_hook(environment)
    call assert_process_ok(result, 'verify changed compiler environment')
    call assert_contains(result%stdout, '"cache_hit":false', 'compiler environment invalidates proof')


    call write_text(project//'/CMakeLists.txt', read_text(project//'/CMakeLists.txt')// &
        'add_custom_target(wait_build ALL COMMAND "${CMAKE_COMMAND}" -E sleep 3)'//nl)
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    call git([character(len=64) :: 'config', 'fo.hookBuildCap', '1'])
    call hook()
    call assert_true(result%exit_code /= 0, 'mandatory build hard cap blocks')
    call assert_contains(result%stdout, 'staged build exceeded hard cap', 'hard cap identified')
    call git([character(len=64) :: 'config', 'fo.hookBuildCap', '60'])
    call write_text(project//'/CMakeLists.txt', read_text(project//'/CMakeLists.txt')// &
        'add_custom_target(long_build ALL COMMAND "${CMAKE_COMMAND}" -E sleep 30)'//nl)
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    call system_clock(count=started, count_rate=rate)
    call bounded_hook()
    call system_clock(count=finished)
    call assert_true(result%exit_code /= 0, 'bounded hook cancels a slow build')
    call assert_contains(result%stdout, 'prepare with: fo hook pre-commit', &
        'bounded timeout directs explicit preparation')
    call assert_true(finished - started < 12_int64*rate, &
        'bounded configure/build/cancellation returns within commit safety budget')
    ! An online Gremlin PASS is not an offline configure PASS: the project
    ! deliberately changes observable test behavior with this CMake flag.
    project = join_path(scratch, 'context-branch')
    call write_text(project//'/README.md', 'Synthetic CMake context fixture.'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'README.md'])
    call git([character(len=64) :: 'commit', '-qm', 'Create synthetic context fixture'])
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_context LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_library(answer answer.f90)'//nl// &
        'if(FETCHCONTENT_UPDATES_DISCONNECTED)'//nl// &
        '  add_test(NAME context_probe COMMAND "${CMAKE_COMMAND}" -E false)'//nl// &
        'else()'//nl// &
        '  add_test(NAME context_probe COMMAND "${CMAKE_COMMAND}" -E true)'//nl//'endif()'//nl)
    call write_text(project//'/answer.f90', original)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt', 'answer.f90'])
    call lane_args('start')
    call list_add(args, '--target')
    call list_add(args, 'context_probe')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_json(driver, project, scratch//'/cache', scratch//'/state', &
        args, reply, result, 30000)
    call assert_process_ok(result, 'start online CMake context proof')
    session = gremlin_field(reply, 'session_id')
    call lane_args('wait')
    call list_add(args, '--until')
    call list_add(args, 'quiescent')
    call list_add(args, '--wait-ms')
    call list_add(args, '20000')
    call gremlin_json(driver, project, scratch//'/cache', scratch//'/state', &
        args, reply, result, 30000)
    call assert_process_ok(result, 'online context reaches quiescence')
    call assert_contains(result%stdout, '"local_gate_green":true', 'online context has verified PASS')
    call gremlin_stop_lane(driver, project, scratch//'/cache', scratch//'/state', &
        'hook-proof', session)
    call bounded_hook()
    call assert_true(result%exit_code /= 0, 'offline configure cannot reuse online PASS')
    call assert_contains(result%stdout, '"cache_hit":false', 'different effective context misses')
    call assert_contains(result%stdout, 'fast test failed: context_probe', &
        'offline branch actually executed')

    ! The missing local source cannot be fetched inside a commit. A synthetic
    ! download command is an independent tripwire, even without network access.
    project = join_path(scratch, 'missing-dependency')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_missing LANGUAGES Fortran)'//nl//'include(FetchContent)'//nl// &
        'FetchContent_Declare(absent SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/absent" '// &
        'DOWNLOAD_COMMAND "${CMAKE_COMMAND}" -E touch "'//scratch//'/downloaded")'//nl// &
        'FetchContent_MakeAvailable(absent)'//nl// &
        'if(NOT EXISTS "${absent_SOURCE_DIR}/CMakeLists.txt")'//nl// &
        '  message(FATAL_ERROR "dependency must be prepared")'//nl//'endif()'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    call bounded_hook()
    call assert_true(result%exit_code /= 0, 'missing bounded dependency blocks')
    inquire(file=scratch//'/downloaded', exist=downloaded)
    call assert_true(.not. downloaded, 'bounded FetchContent never runs acquisition command')
    args = string_list_t()
    call list_add(args, 'build')
    call run_fo(driver, args, project, scratch//'/cache', result, timeout_ms=30000)
    call assert_true(result%exit_code /= 0, 'missing dependency remains invalid')
    inquire(file=scratch//'/downloaded', exist=downloaded)
    call assert_true(downloaded, 'normal build can run declared acquisition outside hooks')
    project = join_path(scratch, 'external_consumer')
    call write_text(project//'/fpm.toml', 'name = "hook_external"'//nl// &
        '[dependencies]'//nl//'helper = { path = "../dependency" }'//nl)
    call write_text(scratch//'/dependency/fpm.toml', 'name = "helper"'//nl)
    call write_text(scratch//'/dependency/src/dependency.f90', original)
    call write_text(project//'/test/test_answer.f90', 'program test_answer'//nl// &
        '    use answer_mod, only: answer'//nl//'    implicit none'//nl// &
        '    if (answer /= 42) stop 1'//nl//'end program test_answer'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'test/test_answer.f90'])
    call hook()
    call assert_process_ok(result, 'prepare external dependency evidence')
    call bounded_hook()
    call assert_process_ok(result, 'reuse unchanged external dependency')
    call assert_contains(result%stdout, '"cache_hit":true', 'external dependency cache hit')
    call write_text(scratch//'/saved.f90', original)
    call copy_mtime(scratch//'/dependency/src/dependency.f90', scratch//'/saved.f90')
    call write_text(scratch//'/dependency/src/dependency.f90', &
        'module answer_mod'//nl//'    implicit none'//nl// &
        '    integer, parameter :: answer = 41'//nl//'end module answer_mod'//nl)
    call copy_mtime(scratch//'/saved.f90', scratch//'/dependency/src/dependency.f90')
    call bounded_hook()
    call assert_true(result%exit_code /= 0, 'restored-mtime external byte edit rejects old proof')
    call assert_contains(result%stdout, '"cache_hit":false', 'restored-mtime bytes invalidate proof')
    call write_text(scratch//'/dependency/src/dependency.f90', original)
    call write_text(scratch//'/dependency/src/added.f90', &
        'module added_mod'//nl//'    implicit none'//nl//'end module added_mod'//nl)
    call bounded_hook()
    call assert_contains(result%stdout, '"cache_hit":false', 'external source addition invalidates proof')
    call remove_tree(scratch//'/dependency/src/added.f90')
    call remove_tree(scratch//'/dependency/src/dependency.f90')
    call bounded_hook()
    call assert_true(result%exit_code /= 0, 'external source deletion rejects old proof')
    call assert_contains(result%stdout, '"cache_hit":false', 'external source deletion invalidates proof')

    ! An fpm manifest beside a CMake project that is already configured does not
    ! become its build authority. The fpm manifest names a dependency a commit
    ! cannot acquire, so the fpm route fails while the configured CMake tree builds.
    project = join_path(scratch, 'cmake-authority')
    call write_text(project//'/fpm.toml', 'name = "hook_authority"'//nl// &
        '[dependencies]'//nl//'hookdep = { git = "https://example.invalid/hookdep.git" }'//nl)
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_authority LANGUAGES Fortran)'//nl// &
        'set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE '// &
        '"${CMAKE_COMMAND} -E touch compile-launched &&")'//nl// &
        'option(HOOK_TESTING "Enable tests" ON)'//nl// &
        'add_executable(answer.x src/answer.f90)'//nl// &
        'if(HOOK_TESTING)'//nl//'  enable_testing()'//nl// &
        '  add_test(NAME answer COMMAND answer.x)'//nl//'endif()'//nl)
    call write_text(project//'/src/answer.f90', 'program answer'//nl// &
        '    implicit none'//nl//'    if (42 /= 42) stop 1'//nl//'end program answer'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'CMakeLists.txt', 'src/answer.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic dual-manifest project'])
    call configure_cmake(project)
    call write_text(project//'/src/answer.f90', 'program answer'//nl// &
        '    implicit none'//nl//'    logical :: launched'//nl// &
        '    inquire (file="compile-launched", exist=launched)'//nl// &
        '    if (.not. launched) stop 1'//nl//'    if (42 /= 42) stop 1'//nl// &
        '    if (43 /= 43) stop 1'//nl//'end program answer'//nl)
    call git([character(len=64) :: 'add', 'src/answer.f90'])
    call hook()
    call assert_process_ok(result, 'configured CMake project keeps its native authority')
    call assert_contains(result%stdout, '"backend":"cmake"', 'CMake chosen over fpm.toml')
    call assert_contains(result%stdout, '"build":"PASS', 'mandatory build ran on the CMake tree')
    call assert_contains(result%stdout, '"fast_passed":1', &
        'CTest case observes the native CMake compile launcher')

    ! A staged copy of a project with fetched dependencies reuses the dependency
    ! roots the project already populated; a commit never acquires a dependency.
    project = join_path(scratch, 'fetched-dependency')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hookdep LANGUAGES Fortran)'//nl//'add_library(dep dep.f90)'//nl// &
        'set_target_properties(dep PROPERTIES '// &
        'Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/mod")'//nl// &
        'target_include_directories(dep PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/mod")'//nl)
    call write_text(project//'/dep.f90', 'module hook_dep_mod'//nl//'    implicit none'//nl// &
        '    integer, parameter :: dep_answer = 7'//nl//'end module hook_dep_mod'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt', 'dep.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic fetched dependency'])
    call git([character(len=64) :: 'rev-parse', 'HEAD'])
    tag = trim(result%stdout)
    project = join_path(scratch, 'fetch-content')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_fetch LANGUAGES Fortran)'//nl//'include(FetchContent)'//nl// &
        'FetchContent_Declare(hookdep GIT_REPOSITORY "'// &
        join_path(scratch, 'fetched-dependency')//'" GIT_TAG '//trim(tag)//')'//nl// &
        'FetchContent_MakeAvailable(hookdep)'//nl//'enable_testing()'//nl// &
        'add_executable(app.x app.f90)'//nl// &
        'target_link_libraries(app.x PRIVATE dep)'//nl// &
        'add_test(NAME app COMMAND app.x)'//nl)
    call write_text(project//'/app.f90', 'program app'//nl// &
        '    use hook_dep_mod, only: dep_answer'//nl//'    implicit none'//nl// &
        '    if (dep_answer /= 7) stop 1'//nl//'end program app'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt', 'app.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic FetchContent project'])
    call configure_cmake(project)
    call write_text(project//'/app.f90', 'program app'//nl// &
        '    use hook_dep_mod, only: dep_answer'//nl//'    implicit none'//nl// &
        '    if (dep_answer /= 7) stop 1'//nl//'    if (dep_answer < 7) stop 1'//nl// &
        'end program app'//nl)
    call git([character(len=64) :: 'add', 'app.f90'])
    call hook()
    call assert_process_ok(result, 'staged copy reuses populated FetchContent roots')
    call assert_contains(result%stdout, '"backend":"cmake"', 'FetchContent project configures')
    call assert_contains(result%stdout, '"fast_passed":1', 'fetched dependency test ran in the hook')

    call write_text(scratch//'/header.inc', 'integer, parameter :: header_answer = 9'//nl)
    call write_text(project//'/CMakeLists.txt', read_text(project//'/CMakeLists.txt')// &
        'FetchContent_Declare(headerdep URL "file://'//scratch//'/header.inc" '// &
        'DOWNLOAD_NO_EXTRACT TRUE)'//nl//'FetchContent_MakeAvailable(headerdep)'//nl// &
        'target_include_directories(app.x PRIVATE "${headerdep_SOURCE_DIR}")'//nl)
    call write_text(project//'/app.f90', 'program app'//nl// &
        '    use hook_dep_mod, only: dep_answer'//nl//'    implicit none'//nl// &
        '    include "header.inc"'//nl// &
        '    if (dep_answer /= 7 .or. header_answer /= 9) stop 1'//nl// &
        'end program app'//nl)
    call configure_cmake(project)
    call git([character(len=64) :: 'add', 'CMakeLists.txt', 'app.f90'])
    call hook()
    call assert_process_ok(result, 'reuse header-only populated roots discovered by File API')
    call assert_contains(result%stdout, '"fast_passed":1', 'fetched header test ran')

    ! An explicit absolute build directory belongs to the real checkout, while
    ! the staged build must stay private and use that tree only for source roots.
    call configure_cmake(project, 'alternate')
    call remove_tree(project//'/build')
    environment = string_list_t()
    call list_add(environment, 'FO_CMAKE_BUILD_DIR='//project//'/alternate')
    call hook(environment)
    call assert_process_ok(result, 'absolute populated build root stays usable in the hook')
    call assert_contains(result%stdout, '"fast_passed":1', 'absolute root test ran')

    ! The project's own CTest labels keep reference-comparison cases out of a
    ! commit; without that declaration the hook runs them and is blocked.
    project = join_path(scratch, 'label-tier')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_tier LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_test(NAME fast_ok COMMAND "${CMAKE_COMMAND}" -E true)'//nl// &
        'add_test(NAME reference COMMAND "${CMAKE_COMMAND}" -E false)'//nl// &
        'set_tests_properties(reference PROPERTIES LABELS "regression")'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    call hook()
    call assert_true(result%exit_code /= 0, 'unlabelled tier runs the reference case')
    call assert_contains(result%stdout, 'fast test failed: reference', 'reference case actually ran')
    call git([character(len=64) :: 'config', 'fo.hookExcludeLabels', 'regression'])
    call hook()
    call assert_process_ok(result, 'declared label exclusion keeps reference out of the commit')
    call assert_contains(result%stdout, '"fast_passed":1', 'fast case still verified')
    call assert_contains(result%stdout, '"exclude_labels":["regression"]', 'exclusion is reported')
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
    subroutine configure_cmake(directory, build_dir)
        character(len=*), intent(in) :: directory
        character(len=*), intent(in), optional :: build_dir
        character(len=:), allocatable :: selected
        selected = 'build'
        if (present(build_dir)) selected = build_dir
        call write_text(directory//'/'//selected//'/.cmake/api/v1/query/codemodel-v2', '')
        cmake_args = string_list_t()
        call list_add(cmake_args, '-S')
        call list_add(cmake_args, '.')
        call list_add(cmake_args, '-B')
        call list_add(cmake_args, selected)
        call run_external('cmake', cmake_args, directory, result, timeout_ms=180000)
        call assert_process_ok(result, 'configure the CMake fixture')
    end subroutine configure_cmake
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
    subroutine hook(extra)
        type(string_list_t), intent(in), optional :: extra
        args = string_list_t()
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, &
            extra_environment=extra, timeout_ms=90000)
    end subroutine hook
    subroutine bounded_hook(extra_environment)
        type(string_list_t), optional, intent(in) :: extra_environment
        args = string_list_t()
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(args, '--bounded')
        call list_add(args, '--json')
        block
            integer(int64) :: begin, end, ticks
            call system_clock(count=begin, count_rate=ticks)
            call run_fo(driver, args, project, scratch//'/cache', result, &
                extra_environment=extra_environment, timeout_ms=15000)
            call system_clock(count=end)
            write (*, '(a,f8.3,a,i0)') 'bounded hook seconds=', &
                real(end-begin)/real(ticks), ' exit=', result%exit_code
        end block
    end subroutine bounded_hook
end program test_hook_precommit
