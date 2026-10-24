program test_transitive_openmp_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(:), allocatable :: driver, scratch, consumer, leaf
    character, parameter :: nl = new_line('a')
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment

    call resolve_driver(driver)
    call make_scratch('fo-transitive-openmp', scratch)
    consumer = join_path(scratch, 'consumer')
    leaf = join_path(scratch, 'leaf')
    call write_text(join_path(leaf, 'fpm.toml'), &
        'name = "omp_leaf"'//nl//'[dependencies]'//nl//'openmp = "*"'//nl)
    call write_text(join_path(leaf, 'src/omp_leaf.f90'), &
        'module omp_leaf'//nl//'contains'//nl// &
        'integer function thread_count() result(n)'//nl//'n = 0'//nl// &
        '!$omp parallel reduction(+:n)'//nl//'n = n + 1'//nl// &
        '!$omp end parallel'//nl//'end function'//nl//'end module'//nl)
    call write_text(join_path(scratch, 'middle/fpm.toml'), &
        'name = "middle"'//nl//'[dependencies]'//nl// &
        'omp_leaf = { path = "../leaf" }'//nl)
    call write_text(join_path(scratch, 'middle/src/middle.f90'), &
        'module middle'//nl//'use omp_leaf, only: thread_count'//nl// &
        'end module'//nl)
    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe'//nl//'use middle, only: thread_count'//nl// &
        'if (thread_count() /= 2) stop 1'//nl// &
        'print "(a)", "two OpenMP threads"'//nl//'end program'//nl)
    call write_text(join_path(consumer, 'test/test_probe.f90'), &
        'program test_probe'//nl//'use middle, only: thread_count'//nl// &
        'if (thread_count() /= 2) stop 1'//nl//'end program'//nl)
    call list_add(environment, 'FO_JOBS=1')
    call list_add(environment, 'OMP_NUM_THREADS=2')
    call list_add(environment, 'OMP_DYNAMIC=FALSE')

    call write_consumer('dependencies')
    call list_add(arguments, 'exec')
    call list_add(arguments, 'probe')
    call run_fo(driver, arguments, consumer, join_path(scratch, 'cache'), &
                result, environment)
    call assert_process_ok(result, 'app inherits transitive OpenMP requirement')
    call assert_equal_string(result%stdout, 'two OpenMP threads'//nl, &
                             'dependency OpenMP region runs on two threads')
    call check_test('regular dependency test links OpenMP')
    call write_consumer('dev-dependencies')
    call check_test('dev-dependency test links OpenMP')
    call remove_tree(scratch)
    call finish_assertions()
contains
    subroutine write_consumer(section)
        character(*), intent(in) :: section
        call write_text(join_path(consumer, 'fpm.toml'), &
            'name = "omp_consumer"'//nl//'['//section//']'//nl// &
            'middle = { path = "../middle" }'//nl)
    end subroutine write_consumer

    subroutine check_test(label)
        character(*), intent(in) :: label
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, 'test_probe')
        call list_add(arguments, '--retries')
        call list_add(arguments, '0')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'cache'), &
                    result, environment)
        call assert_process_ok(result, label)
    end subroutine check_test
end program test_transitive_openmp_cli
