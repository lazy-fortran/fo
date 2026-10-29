program test_transitive_blas_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text
    use fo_test_harness, only: assert_process_ok, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, consumer, leaf, external
    character, parameter :: nl = new_line('a')
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    integer :: warm

    call resolve_driver(driver)
    call make_scratch('fo-transitive-blas', scratch)
    consumer = join_path(scratch, 'consumer')
    leaf = join_path(scratch, 'leaf')
    external = join_path(scratch, 'external')
    call write_text(join_path(external, 'probe.c'), &
        'int fo_blas_value(void) { return 42; }'//nl)
    call list_add(arguments, '-c')
    call list_add(arguments, 'probe.c')
    call list_add(arguments, '-o')
    call list_add(arguments, 'probe.o')
    call run_external('cc', arguments, external, result)
    call assert_process_ok(result, 'compile independent provider')
    arguments = string_list_t()
    call list_add(arguments, 'rcs')
    call list_add(arguments, 'libfo_blas_probe.a')
    call list_add(arguments, 'probe.o')
    call run_external('ar', arguments, external, result)
    call assert_process_ok(result, 'archive independent provider')
    call write_text(join_path(external, 'openblas.pc'), &
        'Name: Fo BLAS link fixture'//nl//'Description: synthetic provider'//nl// &
        'Version: 1.0'//nl//'Libs: -L'//external//' -lfo_blas_probe'//nl// &
        'Cflags: -DFO_PROVIDER_COMPILE_ONLY'//nl)
    call write_text(join_path(leaf, 'fpm.toml'), &
        'name = "blas_leaf"'//nl//'[dependencies]'//nl//'blas = "*"'//nl)
    call write_text(join_path(leaf, 'src/blas_leaf.f90'), &
        '#ifndef FO_PROVIDER_COMPILE_ONLY'//nl// &
        '#error provider compile flags are missing'//nl//'#endif'//nl// &
        'module blas_leaf'//nl//'use iso_c_binding, only: c_int'//nl// &
        'implicit none'//nl//'interface'//nl// &
        'integer(c_int) function fo_blas_value() bind(c)'//nl// &
        'import c_int'//nl//'end function'//nl//'end interface'//nl// &
        'end module'//nl)
    call write_text(join_path(scratch, 'middle/fpm.toml'), &
        'name = "middle"'//nl//'[dependencies]'//nl// &
        'blas_leaf = { path = "../leaf" }'//nl)
    call write_text(join_path(scratch, 'middle/src/middle.f90'), &
        'module middle'//nl//'use blas_leaf, only: fo_blas_value'//nl// &
        'end module'//nl)
    call write_text(join_path(consumer, 'app/probe.f90'), &
        '#ifdef FO_PROVIDER_COMPILE_ONLY'//nl// &
        '#error provider compile flags leaked to the consumer'//nl//'#endif'//nl// &
        'program probe'//nl//'use middle, only: fo_blas_value'//nl// &
        'if (fo_blas_value() /= 42) stop 1'//nl// &
        'print "(i0)", fo_blas_value()'//nl//'end program'//nl)
    call write_text(join_path(consumer, 'test/test_probe.f90'), &
        '#ifdef FO_PROVIDER_COMPILE_ONLY'//nl// &
        '#error provider compile flags leaked to the consumer'//nl//'#endif'//nl// &
        'program test_probe'//nl//'use middle, only: fo_blas_value'//nl// &
        'if (fo_blas_value() /= 42) stop 1'//nl//'end program'//nl)
    call list_add(environment, 'PKG_CONFIG_LIBDIR='//external)
    call list_add(environment, 'PKG_CONFIG_PATH='//external)
    call list_add(environment, 'LIBRARY_PATH=')
    call list_add(environment, 'FO_LIBRARY_PATH=')
    call list_add(environment, 'FO_JOBS=1')

    call write_consumer('dependencies')
    do warm = 1, 2
        arguments = string_list_t()
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'cache'), &
            result, environment)
        call assert_process_ok(result, 'app links transitive BLAS provider')
        call assert_equal_string(result%stdout, '42'//nl, 'provider value')
        call check_test('regular dependency test links BLAS')
    end do
    call write_consumer('dev-dependencies')
    call check_test('dev-dependency test links BLAS')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine write_consumer(section)
        character(*), intent(in) :: section

        call write_text(join_path(consumer, 'fpm.toml'), &
            'name = "blas_consumer"'//nl//'['//section//']'//nl// &
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
end program test_transitive_blas_cli
