program test_hook_precommit
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, make_directory, join_path, write_text, read_text, remove_tree, &
        assert_contains, assert_equal_string, assert_true, assert_process_ok, &
        finish_assertions, copy_mtime, set_mode, make_symlink
    use fo_fs, only: fs_find_executable, fs_sleep_ms
    use fo_test_gremlin_oracle, only: gremlin_json, gremlin_field, gremlin_stop_lane
    use fo_test_json, only: json_value_t
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: driver, scratch, project, original, tree, session, config_before
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
    ! Preparation shares this budget; only the bounded hook measures commit latency.
    call git([character(len=64) :: 'config', 'fo.hookBudget', '30'])
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
    call write_text(project//'/answer.f90', original//'! Staged hook input'//nl)
    call git([character(len=64) :: 'add', 'answer.f90'])
    call git([character(len=64) :: 'write-tree'])
    tree = trim(result%stdout)
    environment = string_list_t()
    call list_add(environment, 'GIT_INDEX_FILE='//project//'/.git/index')
    call hook(environment)
    call assert_process_ok(result, 'Git commit index environment validates staged CMake')
    call assert_contains(result%stdout, '"build":"PASS', 'inherited-index build passes')
    call git([character(len=64) :: 'write-tree'])
    call assert_equal_string(trim(result%stdout), tree, 'hook preserves original staged index')
    call git([character(len=128) :: 'remote', 'add', 'origin', &
        'https://example.invalid/hook-fixture'])
    config_before = read_text(project//'/.git/config')
    call write_text(scratch//'/hooks/pre-commit', '#!/bin/sh'//nl// &
        'exec "$FO_TEST_HOOK_DRIVER" hook pre-commit --json'//nl)
    call set_mode(scratch//'/hooks/pre-commit', int(o'755'))
    args = string_list_t()
    call list_add(args, '-c')
    call list_add(args, 'core.hooksPath='//scratch//'/hooks')
    call list_add(args, '--git-dir='//project//'/.git')
    call list_add(args, '--work-tree='//project)
    call list_add(args, 'hook')
    call list_add(args, 'run')
    call list_add(args, 'pre-commit')
    environment = string_list_t()
    call list_add(environment, 'FO_TEST_HOOK_DRIVER='//driver)
    call list_add(environment, 'FO_CACHE_DIR='//scratch//'/cache')
    call run_external('git', args, project, result, environment, timeout_ms=90000)
    call assert_process_ok(result, 'actual Git hook validates staged CMake')
    call assert_contains(result%stdout//result%stderr, '"build":"PASS', &
        'actual Git hook build passes')
    call assert_equal_string(read_text(project//'/.git/config'), config_before, &
        'actual Git hook preserves original repository configuration')
    call write_text(project//'/.git/config', config_before)
    call git([character(len=64) :: 'write-tree'])
    call assert_equal_string(trim(result%stdout), tree, 'actual Git hook preserves index')
    call git([character(len=128) :: 'read-tree', tree(:index(tree, nl) - 1)])
    call git([character(len=64) :: 'remote', 'remove', 'origin'])
    call write_text(project//'/answer.f90', original)
    call git([character(len=64) :: 'add', 'answer.f90'])

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

    ! A fresh checkout whose Git dependency was never populated cannot build in
    ! the offline hook; the diagnostic names the step that populates it.
    project = join_path(scratch, 'unpopulated-dependency')
    call write_text(project//'/fpm.toml', 'name = "hook_unpopulated"'//nl// &
        '[dependencies]'//nl//'hookdep = { git = "https://example.invalid/hookdep.git" }'//nl)
    call write_text(project//'/src/answer.f90', original)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/answer.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'unpopulated dependency blocks the commit')
    call assert_contains(result%stdout, 'dependencies are not populated in this checkout', &
        'hook explains the missing dependency checkout')
    call assert_contains(result%stdout, 'run `fo build` here once', &
        'hook names the command that populates dependencies')

    ! The staged configure keeps the compiler the project was configured with
    ! instead of repeating CMake's search, which may find another compiler.
    project = join_path(scratch, 'configured-compiler')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_compiler LANGUAGES Fortran)'//nl// &
        'get_filename_component(selected "${CMAKE_Fortran_COMPILER}" NAME)'//nl// &
        'if(NOT selected STREQUAL "hook-fortran")'//nl// &
        '  message(FATAL_ERROR "unexpected compiler ${CMAKE_Fortran_COMPILER}")'//nl// &
        'endif()'//nl//'enable_testing()'//nl// &
        'add_test(NAME compiler_ok COMMAND "${CMAKE_COMMAND}" -E true)'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    block
        character(len=4096) :: compiler
        logical :: located
        call fs_find_executable('gfortran', compiler, located)
        call assert_true(located, 'locate the real Fortran compiler')
        call make_directory(project//'/toolchain')
        call make_symlink(trim(compiler), project//'/toolchain/hook-fortran')
    end block
    call write_text(project//'/build/.cmake/api/v1/query/codemodel-v2', '')
    cmake_args = string_list_t()
    call list_add(cmake_args, '-S')
    call list_add(cmake_args, '.')
    call list_add(cmake_args, '-B')
    call list_add(cmake_args, 'build')
    call list_add(cmake_args, '-DCMAKE_Fortran_COMPILER='//project//'/toolchain/hook-fortran')
    environment = string_list_t()
    call list_add(environment, 'FC=')
    call run_external('cmake', cmake_args, project, result, environment, timeout_ms=180000)
    call assert_process_ok(result, 'configure with an explicitly chosen compiler')
    call hook(environment)
    call assert_process_ok(result, 'staged configure keeps the configured compiler')
    call assert_contains(result%stdout, '"fast_passed":1', 'configured-compiler case ran')

    ! A compiler recorded at configuration but removed since (a toolchain
    ! upgrade) is not forced on the staged configure.
    project = join_path(scratch, 'removed-compiler')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_removed_compiler LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_test(NAME removed_compiler_ok COMMAND "${CMAKE_COMMAND}" -E true)'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    block
        character(len=4096) :: compiler
        logical :: located
        call fs_find_executable('gfortran', compiler, located)
        call assert_true(located, 'locate the real Fortran compiler')
        call make_directory(project//'/toolchain')
        call make_symlink(trim(compiler), project//'/toolchain/old-fortran')
    end block
    call write_text(project//'/build/.cmake/api/v1/query/codemodel-v2', '')
    cmake_args = string_list_t()
    call list_add(cmake_args, '-S')
    call list_add(cmake_args, '.')
    call list_add(cmake_args, '-B')
    call list_add(cmake_args, 'build')
    call list_add(cmake_args, '-DCMAKE_Fortran_COMPILER='//project//'/toolchain/old-fortran')
    environment = string_list_t()
    call list_add(environment, 'FC=')
    call run_external('cmake', cmake_args, project, result, environment, timeout_ms=180000)
    call assert_process_ok(result, 'configure with a compiler that is later removed')
    call remove_tree(project//'/toolchain')
    call hook(environment)
    call assert_process_ok(result, 'staged configure ignores a removed compiler')
    call assert_contains(result%stdout, '"fast_passed":1', 'removed-compiler case ran')

    ! A CMake project that enables no Fortran keeps .f90 files as data, here
    ! a compiler test input. Its configure needs a generated, ignored file, so
    ! the index alone cannot be built; the configured build decides instead.
    project = join_path(scratch, 'cmake-data')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/version")'//nl// &
        '  message(FATAL_ERROR "generated version file is missing")'//nl// &
        'endif()'//nl//'project(hook_data NONE)'//nl)
    call write_text(project//'/.gitignore', 'version'//nl//'build/'//nl)
    call write_text(project//'/version', '1.0.0'//nl)
    call write_text(project//'/tests/invalid.f90', 'program invalid'//nl// &
        '    x = '//nl//'end program invalid'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt', '.gitignore', 'tests/invalid.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic non-Fortran CMake project'])
    call configure_cmake(project)
    call write_text(project//'/tests/second.f90', 'program second'//nl// &
        '    y = '//nl//'end program second'//nl)
    call git([character(len=64) :: 'add', 'tests/second.f90'])
    call hook()
    call assert_process_ok(result, 'Fortran data in a non-Fortran CMake project')
    call assert_contains(result%stdout, 'enables no Fortran', 'non-Fortran build named')
    call git([character(len=64) :: 'worktree', 'add', '-q', '../cmake-data-worktree'])
    project = join_path(scratch, 'cmake-data-worktree')
    call write_text(project//'/tests/third.f90', 'program third'//nl// &
        '    z = '//nl//'end program third'//nl)
    call git([character(len=64) :: 'add', 'tests/third.f90'])
    call hook()
    call assert_process_ok(result, 'linked worktree uses its main checkout configuration')
    call assert_contains(result%stdout, 'enables no Fortran', 'worktree names main build')

    ! Without configuration the hard cap admits a cold build longer than a
    ! minute, as in a fresh worktree; it bounds runaway builds only.
    project = join_path(scratch, 'default-cap')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_default_cap NONE)'//nl//'enable_testing()'//nl// &
        'add_custom_target(cold_build ALL COMMAND "${CMAKE_COMMAND}" -E sleep 65)'//nl// &
        'add_test(NAME default_cap_ok COMMAND "${CMAKE_COMMAND}" -E true)'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt'])
    call hook()
    call assert_process_ok(result, 'default hard cap admits a cold build over one minute')
    call assert_contains(result%stdout, '"build":"PASS', 'default-cap build completed')

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

    ! Legacy formatting on lines a commit does not touch never blocks it; the
    ! changed lines are still checked.
    project = join_path(scratch, 'legacy-format')
    call write_text(project//'/fpm.toml', 'name = "hook_legacy"'//nl)
    call write_text(project//'/src/legacy.f90', 'module legacy_mod'//nl// &
        'implicit none'//nl//'    integer, parameter :: answer = 42'//nl// &
        'end module legacy_mod'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/legacy.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic legacy source'])
    call write_text(project//'/src/legacy.f90', 'module legacy_mod'//nl// &
        'implicit none'//nl//'    integer, parameter :: answer = 43'//nl// &
        'end module legacy_mod'//nl)
    call git([character(len=64) :: 'add', 'src/legacy.f90'])
    call hook()
    call assert_process_ok(result, 'clean change beside legacy formatting passes')
    call write_text(project//'/src/legacy.f90', 'module legacy_mod'//nl// &
        'implicit none'//nl//'      integer,parameter::answer=43'//nl// &
        'end module legacy_mod'//nl)
    call git([character(len=64) :: 'add', 'src/legacy.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'badly formatted changed line blocks')
    call assert_contains(result%stdout, 'legacy.f90:3: needs formatting', &
        'changed line is reported')

    ! A changed line follows its file's own indentation width.
    project = join_path(scratch, 'legacy-indent')
    call write_text(project//'/src/indent.f90', two_space_module('1', '    '))
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'src/indent.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic two-space source'])
    call write_text(project//'/fpm.toml', 'name = "hook_indent"'//nl)
    call write_text(project//'/src/indent.f90', two_space_module('2', '    '))
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/indent.f90'])
    call hook()
    call assert_process_ok(result, 'change in a two-space file passes')
    call write_text(project//'/src/indent.f90', two_space_module('2', '  '))
    call git([character(len=64) :: 'add', 'src/indent.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'misindented changed line blocks')
    call assert_contains(result%stdout, 'indent.f90:6: needs formatting', &
        'misindented line is reported')

    ! A file whose procedures sit at column 0 after `contains` keeps that style,
    ! and a stray three-space line does not set its indentation width.
    project = join_path(scratch, 'flat-contains')
    call write_text(project//'/src/flat.f90', flat_module('1', '        '))
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'src/flat.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic column-0 procedures'])
    call write_text(project//'/fpm.toml', 'name = "hook_flat"'//nl)
    call write_text(project//'/src/flat.f90', flat_module('2', '        '))
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/flat.f90'])
    call hook()
    call assert_process_ok(result, 'change beside column-0 procedures passes')
    call write_text(project//'/src/flat.f90', flat_module('2', '      '))
    call git([character(len=64) :: 'add', 'src/flat.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'misindented line beside column-0 procedures blocks')
    call assert_contains(result%stdout, 'flat.f90:17: needs formatting', &
        'misindented column-0 neighbour line is reported')

    ! A bounded hook past its deadline must stop the nested fo test too; an
    ! orphan would keep running in the removed view and recreate it.
    project = join_path(scratch, 'cancel/cmake')
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_cancel LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_library(answer answer.f90)'//nl// &
        'add_test(NAME wait COMMAND "${CMAKE_COMMAND}" -E sleep 12)'//nl)
    call write_text(project//'/answer.f90', original)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'CMakeLists.txt', 'answer.f90'])
    call git([character(len=64) :: 'config', 'fo.hookBudget', '30'])
    call bounded_hook()
    call assert_contains(result%stdout, 'staged verification incomplete', &
        'bounded hook stops at its deadline')
    call fs_sleep_ms(14000)
    args = string_list_t()
    call list_add(args, join_path(scratch, 'cancel'))
    call list_add(args, '-maxdepth')
    call list_add(args, '1')
    call list_add(args, '-name')
    call list_add(args, '.fo-hook-*')
    call run_external('find', args, scratch, result)
    call assert_process_ok(result, 'list staged views after cancellation')
    call assert_equal_string(result%stdout, '', 'cancelled bounded hook leaves no staged view')
    call finish_assertions(retain_failed_scratch=.true.)
    call remove_tree(scratch)
contains
    function flat_module(step, indent) result(text)
        ! Column-0 procedures, four-space steps and one stray three-space line.
        character(len=*), intent(in) :: step, indent
        character(len=:), allocatable :: text
        integer :: i
        text = 'module flat_mod'//nl//'    implicit none'//nl
        do i = 1, 9
            text = text//'    integer :: a'//achar(iachar('0') + i)//nl
        end do
        text = text//'   integer :: odd'//nl//'contains'//nl// &
            'subroutine bump(x)'//nl//'    integer, intent(inout) :: x'//nl// &
            '    if (x > 0) then'//nl//indent//'x = x + '//step//nl// &
            '    end if'//nl//'end subroutine bump'//nl//'end module flat_mod'//nl
    end function flat_module
    function two_space_module(step, indent) result(text)
        character(len=*), intent(in) :: step, indent
        character(len=:), allocatable :: text
        text = 'module indent_mod'//nl//'  implicit none'//nl//'contains'//nl// &
            '  subroutine bump(x)'//nl//'    integer, intent(inout) :: x'//nl// &
            indent//'x = x + '//step//nl//'  end subroutine bump'//nl// &
            'end module indent_mod'//nl
    end function two_space_module
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
