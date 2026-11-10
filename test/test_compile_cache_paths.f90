program test_compile_cache_paths
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_equal_integer, assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, cache, includes
    type(string_list_t) :: args, env
    type(process_result_t) :: result
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-compile-cache-paths', scratch)
    cache = join_path(scratch, 'cache')
    call list_add(args, 'exec')
    call list_add(args, 'pathprobe')
    call list_add(env, 'FO_JOBS=2')
    do i = 1, 2
        if (i == 1) then
            project = join_path(scratch, 'first')
        else
            project = join_path(scratch, 'second')
        end if
        call write_text(join_path(project, 'fpm.toml'), 'name = "pathprobe"'//nl)
        call write_text(join_path(project, 'app/main.F90'), &
            'program main'//nl//'print *, __FILE__'//nl//'end program'//nl)
        call run_fo(driver, args, project, cache, result, env, 120000)
        call assert_equal_integer(result%exit_code, 0, &
            'path-sensitive project builds: '//result%stdout//result%stderr)
        call assert_contains(result%stdout, join_path(project, 'app/main.F90'), &
            'restored source must preserve its compiler-observed filename')
    end do
    project = join_path(scratch, 'include-project')
    call write_text(join_path(project, 'app/main.F90'), &
        'program main'//nl//'#include "probe.inc"'//nl//'end program'//nl)
    do i = 1, 2
        if (i == 1) then
            includes = join_path(scratch, 'include-first')
        else
            includes = join_path(scratch, 'include-second')
        end if
        call write_text(join_path(includes, 'probe.inc'), 'print *, __FILE__'//nl)
        call write_text(join_path(project, 'fpm.toml'), &
            'name = "pathprobe"'//nl//'[library]'//nl// &
            'include-dir = "'//includes//'"'//nl)
        call remove_tree(join_path(project, 'build'))
        call run_fo(driver, args, project, cache, result, env, 120000)
        call assert_equal_integer(result%exit_code, 0, &
            'include-sensitive project builds: '//result%stdout//result%stderr)
        call assert_contains(result%stdout, join_path(includes, 'probe.inc'), &
            'restored source must preserve its compiler-observed include path')
    end do
    call remove_tree(scratch)
    call finish_assertions()
end program test_compile_cache_paths
