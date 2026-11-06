program test_hook_reuse
    !! A commit that stages only documentation builds nothing, and CMake commits
    !! of every worktree recompile only what changed in one shared staged build.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, read_text, remove_tree, assert_process_ok, &
        assert_true, assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, log
    type(process_result_t) :: result
    character(:), allocatable :: a_before, b_before, first, second

    call resolve_driver(driver)
    call make_scratch('fo-hook-reuse', scratch)

    project = scratch//'/docs'
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_docs LANGUAGES NONE)'//nl//'# installs guide.md'//nl// &
        'message(FATAL_ERROR "configure ran")'//nl)
    call write_text(project//'/answer.f90', 'program answer'//nl//'end program answer'//nl)
    call write_text(project//'/README.md', 'Read me.'//nl)
    call write_text(project//'/guide.md', 'Guide.'//nl)
    call git('init', '-q')
    call git('add', '.')
    call git('commit', '-qm', 'base')
    call write_text(project//'/README.md', 'Read me again.'//nl)
    call git('add', 'README.md')
    call hook()
    call assert_process_ok(result, 'documentation-only commit passes without configuring')
    call assert_contains(result%stderr, 'only documentation staged', 'skip is reported')
    call git('reset', '-q', '--hard')
    call write_text(project//'/guide.md', 'Changed guide.'//nl)
    call git('add', 'guide.md')
    call hook()
    call assert_true(result%exit_code /= 0, 'a page the build script names is checked')

    project = scratch//'/repo'
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(hook_reuse LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_library(parts a.f90 b.f90)'//nl//'add_executable(answer main.f90)'//nl// &
        'target_link_libraries(answer parts)'//nl//'add_test(NAME answer COMMAND answer)'//nl)
    call write_text(project//'/a.f90', module_text('a', 1))
    call write_text(project//'/b.f90', module_text('b', 2))
    call write_text(project//'/main.f90', 'program main'//nl//'    use a, only: a_value'//nl// &
        '    use b, only: b_value'//nl//'    implicit none'//nl// &
        '    if (a_value() + b_value() /= 3) error stop 1'//nl//'end program main'//nl)
    call git('init', '-q')
    call git('add', '.')
    call git('commit', '-qm', 'base')
    call write_text(project//'/a.f90', module_text('a', 1)//'! first change'//nl)
    call git('add', 'a.f90')
    call hook()
    call assert_process_ok(result, 'first staged build passes')
    call assert_contains(result%stderr, 'fo hook: toolchain Fortran', 'toolchain is reported')
    call assert_contains(result%stderr, '(FC)', 'FC from the environment selects Fortran')
    log = read_text(scratch//'/.fo-staged-repo/build/.ninja_log')
    a_before = record(log, 'a.f90.o')
    b_before = record(log, 'b.f90.o')
    call assert_true(len(a_before) > 0 .and. len(b_before) > 0, &
        'shared staged build compiled both units')

    ! A new worktree of the same repository builds on the same staged build.
    call git('worktree', 'add', '-q', '--detach', scratch//'/lane')
    project = scratch//'/lane'
    call write_text(project//'/a.f90', module_text('a', 1)//'! lane change'//nl)
    call git('add', 'a.f90')
    call hook()
    call assert_process_ok(result, 'worktree commit passes')
    log = read_text(scratch//'/.fo-staged-repo/build/.ninja_log')
    ! Ninja keeps one record per output, holding the output's build time.
    call assert_true(record(log, 'a.f90.o') /= a_before, 'changed unit recompiled')
    call assert_true(record(log, 'b.f90.o') == b_before, 'unchanged unit reused')

    ! A commit that moves a FetchContent pin builds the newly pinned revision.
    project = scratch//'/dep'
    call write_text(project//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(dep LANGUAGES Fortran)'//nl//'add_library(dep dep.f90)'//nl// &
        'target_include_directories(dep PUBLIC ${CMAKE_CURRENT_BINARY_DIR})'//nl)
    call write_text(project//'/dep.f90', module_text('dep', 1))
    call git('init', '-q')
    call git('add', '.')
    call git('commit', '-qm', 'one')
    call git('rev-parse', 'HEAD')
    first = trim(result%stdout)
    call write_text(project//'/dep.f90', module_text('dep', 2))
    call git('commit', '-qam', 'two')
    call git('rev-parse', 'HEAD')
    second = trim(result%stdout)
    project = scratch//'/pinned'
    call write_text(project//'/CMakeLists.txt', pinned_cmake(first))
    call write_text(project//'/main.f90', pinned_main(1))
    call git('init', '-q')
    call git('config', 'fo.hookBudget', '120')
    call git('add', '.')
    call git('commit', '-qm', 'base')
    call write_text(project//'/main.f90', pinned_main(1)//'! first pin'//nl)
    call git('add', 'main.f90')
    call hook()
    call assert_process_ok(result, 'first pinned revision builds and passes')
    call git('commit', '-qm', 'touch')
    call write_text(project//'/CMakeLists.txt', pinned_cmake(second))
    call write_text(project//'/main.f90', pinned_main(2))
    call git('add', 'CMakeLists.txt', 'main.f90')
    call hook()
    call assert_process_ok(result, 'moved pin builds the new revision')
    call assert_contains(result%stdout, 'fast PASS 1', 'pinned revision test ran')
    call remove_tree(scratch)
    call finish_assertions()
contains
    function module_text(name, value) result(text)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=1) :: digit

        write(digit, '(i1)') value
        text = 'module '//name//nl//'    implicit none'//nl//'contains'//nl// &
            '    integer function '//name//'_value()'//nl// &
            '        '//name//'_value = '//digit//nl// &
            '    end function '//name//'_value'//nl//'end module '//name//nl
    end function module_text

    function pinned_cmake(revision) result(text)
        character(len=*), intent(in) :: revision
        character(:), allocatable :: text

        text = 'cmake_minimum_required(VERSION 3.20)'//nl// &
            'project(pinned LANGUAGES Fortran)'//nl//'include(FetchContent)'//nl// &
            'FetchContent_Declare(dep GIT_REPOSITORY "file://'//scratch//'/dep" '// &
            'GIT_TAG '//revision//')'//nl//'FetchContent_MakeAvailable(dep)'//nl// &
            'enable_testing()'//nl//'add_executable(answer main.f90)'//nl// &
            'target_link_libraries(answer dep)'//nl//'add_test(NAME answer COMMAND answer)'//nl
    end function pinned_cmake

    function pinned_main(expected) result(text)
        integer, intent(in) :: expected
        character(:), allocatable :: text
        character(len=1) :: digit

        write(digit, '(i1)') expected
        text = 'program main'//nl//'    use dep, only: dep_value'//nl// &
            '    implicit none'//nl//'    if (dep_value() /= '//digit//') error stop 1'//nl// &
            'end program main'//nl
    end function pinned_main

    function record(text, object) result(line)
        character(len=*), intent(in) :: text, object
        character(:), allocatable :: line
        character(:), allocatable :: key
        integer :: at, finish

        line = ''
        key = '/parts.dir/'//object//achar(9)
        at = index(text, key, back=.true.)
        if (at == 0) return
        finish = at + index(text(at:), new_line('a')) - 2
        at = index(text(:at), new_line('a'), back=.true.) + 1
        line = text(at:finish)
    end function record

    subroutine git(first, second, third, fourth, fifth)
        character(len=*), intent(in) :: first
        character(len=*), intent(in), optional :: second, third, fourth, fifth
        type(string_list_t) :: git_args, git_env
        call list_add(git_args, first)
        if (present(second)) call list_add(git_args, second)
        if (present(third)) call list_add(git_args, third)
        if (present(fourth)) call list_add(git_args, fourth)
        if (present(fifth)) call list_add(git_args, fifth)
        call list_add(git_env, 'GIT_CONFIG_GLOBAL=')
        call list_add(git_env, 'GIT_AUTHOR_NAME=fo')
        call list_add(git_env, 'GIT_AUTHOR_EMAIL=fo@example.invalid')
        call list_add(git_env, 'GIT_COMMITTER_NAME=fo')
        call list_add(git_env, 'GIT_COMMITTER_EMAIL=fo@example.invalid')
        call run_external('git', git_args, project, result, git_env, timeout_ms=60000)
        call assert_process_ok(result, 'fixture git '//first)
    end subroutine git

    subroutine hook()
        type(string_list_t) :: args, env
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(env, 'FC=gfortran')
        call run_fo(driver, args, project, scratch//'/cache', result, env, &
            timeout_ms=120000)
    end subroutine hook
end program test_hook_reuse
