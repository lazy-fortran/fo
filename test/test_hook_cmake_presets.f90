program test_hook_cmake_presets
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, read_text, remove_tree, assert_process_ok, &
        assert_true, assert_contains, assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fx_string, only: replace_str
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, binary, presets, before
    character(:), allocatable :: staged, index_before, cache_before
    type(string_list_t) :: args, env
    type(process_result_t) :: result
    integer :: branch

    call resolve_driver(driver)
    call make_scratch('fo-hook-cmake-presets', scratch)
    staged = 'program answer'//nl//'    implicit none'//nl// &
        '    print *, 42'//nl//'end program answer'//nl
    do branch = 1, 3
        project = scratch//'/nested'
        binary = '${sourceDir}/build/native'
        if (branch == 2) then
            project = scratch//'/absolute'
            binary = scratch//'/external-build'
        else if (branch == 3) then
            project = scratch//'/included'
        end if
        call write_text(project//'/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.22)'//nl// &
            'project(hook_preset LANGUAGES Fortran)'//nl// &
            'if(NOT "$ENV{HOOK_PRESET_ENV}" STREQUAL "inherited")'//nl// &
            '  message(FATAL_ERROR "configure preset environment was lost")'//nl// &
            'endif()'//nl//'enable_testing()'//nl// &
            'add_executable(answer answer.f90)'//nl// &
            'add_custom_target(unselected ALL COMMAND "${CMAKE_COMMAND}" -E false)'//nl// &
            'add_test(NAME answer COMMAND answer)'//nl// &
            'add_test(NAME excluded COMMAND "${CMAKE_COMMAND}" -E false)'//nl// &
            'set_tests_properties(excluded PROPERTIES LABELS excluded)'//nl)
        call write_text(project//'/answer.f90', staged)
        presets = '{"name":"base","hidden":true,"generator":"Ninja",'// &
            '"binaryDir":"'//binary//'",'// &
            '"environment":{"HOOK_PRESET_ENV":"inherited"}}'
        if (branch == 3) then
            call write_text(project//'/cmake/base.json', &
                '{"version":4,"configurePresets":['//presets//']}')
            presets = '"include":["cmake/base.json"],"configurePresets":['
        else
            presets = '"configurePresets":['//presets//','
        end if
        presets = '{"version":4,'//presets// &
            '{"name":"native","inherits":"base"}],'// &
            '"buildPresets":[{"name":"native","configurePreset":"native",'// &
            '"targets":["answer"]}],'// &
            '"testPresets":[{"name":"native","configurePreset":"native",'// &
            '"filter":{"exclude":{"label":"excluded"}}}]}'
        call write_text(project//'/CMakePresets.json', presets)
        call git('init', '-q')
        call git('config', 'fo.hookBudget', '600')
        call git('add', '.')
        env = string_list_t()
        call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=native')
        call list_add(env, 'FO_CMAKE_BUILD_PRESET=native')
        call list_add(env, 'FO_CMAKE_TEST_PRESET=native')
        args = string_list_t()
        call list_add(args, '--preset')
        call list_add(args, 'native')
        call run_external('cmake', args, project, result, env, timeout_ms=90000)
        call assert_process_ok(result, 'populate original preset build')
        if (branch /= 2) binary = project//'/build/native'
        cache_before = read_text(binary//'/CMakeCache.txt')
        before = read_text(project//'/CMakePresets.json')
        call git('write-tree')
        index_before = trim(result%stdout)
        call write_text(project//'/answer.f90', 'unstaged invalid Fortran'//nl)
        call hook()
        call assert_process_ok(result, 'private hook preserves native preset settings')
        call assert_contains(result%stdout, '"fast_passed":1', 'registered preset test ran')
        call assert_contains(result%stdout, '"deferred":["excluded"]', &
            'test preset filter remains authoritative')
        call assert_equal_string(read_text(project//'/CMakePresets.json'), before, &
            'authored presets remain unchanged')
        call assert_equal_string(read_text(binary//'/CMakeCache.txt'), cache_before, &
            'authored build cache remains unchanged')
        call assert_equal_string(read_text(project//'/answer.f90'), &
            'unstaged invalid Fortran'//nl, 'unstaged source remains unchanged')
        call git('write-tree')
        call assert_equal_string(trim(result%stdout), index_before, 'hook preserves staged index')
        call write_text(project//'/answer.f90', 'program answer'//nl// &
            '    implicit none'//nl//'    stop 19'//nl//'end program answer'//nl)
        call git('add', 'answer.f90')
        call write_text(project//'/answer.f90', staged)
        call hook()
        call assert_true(result%exit_code /= 0, 'staged preset test failure blocks hook')
        call assert_contains(result%stdout, 'fast test failed: answer', &
            'preset test failure is identified')
    end do
    before = read_text(project//'/cmake/base.json')
    call write_text(scratch//'/external-config.json', before)
    presets = replace_str(read_text(project//'/CMakePresets.json'), &
        'cmake/base.json', scratch//'/external-config.json')
    call write_text(project//'/CMakePresets.json', presets)
    call git('add', 'CMakePresets.json', 'answer.f90')
    call list_add(env, 'FO_CMAKE_BUILD_DIR='//project//'/build/native')
    call hook()
    call assert_true(result%exit_code /= 0, 'external preset include is rejected')
    call assert_contains(result%stdout, 'outside the private view', &
        'external include fails before private preset adaptation')
    call assert_equal_string(read_text(scratch//'/external-config.json'), before, &
        'external preset include remains unchanged')
    call remove_tree(scratch)
    call finish_assertions()
contains
    subroutine git(first, second, third)
        character(len=*), intent(in) :: first
        character(len=*), intent(in), optional :: second, third
        type(string_list_t) :: git_args, git_env
        call list_add(git_args, first)
        if (present(second)) call list_add(git_args, second)
        if (present(third)) call list_add(git_args, third)
        call list_add(git_env, 'GIT_CONFIG_GLOBAL=')
        call run_external('git', git_args, project, result, git_env, timeout_ms=90000)
        call assert_process_ok(result, 'synthetic preset fixture git operation')
    end subroutine git
    subroutine hook()
        args = string_list_t()
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, env, &
            timeout_ms=180000)
    end subroutine hook
end program test_hook_cmake_presets
