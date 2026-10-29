program test_gremlin_case_timeout
    !! Gremlin's backstop for a CTest case follows the project's limits, as
    !! plain ctest does: the TIMEOUT property, fixture cases that run with
    !! it, else ctest's default of 1500 s. Only fpm tests keep fo's budget.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: assert_true, assert_equal_integer, finish_assertions, &
        make_scratch, write_text, join_path, remove_tree
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_campaign, only: case_wall_timeout
    use fo_project_model, only: model_init, model_add_case, model_finalize, &
        model_attr_t
    implicit none

    interface
        function c_setenv(name, value, overwrite) bind(C, name='setenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: ierr
        end function c_setenv
        function c_unsetenv(name) bind(C, name='unsetenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: ierr
        end function c_unsetenv
    end interface

    type(generation_t) :: cmake, fpm, bare
    character(len=:), allocatable :: scratch, detail
    integer :: ierr

    call unset('FO_TEST_WALL_TIMEOUT')
    call unset('FO_TEST_TIMEOUT')
    call unset('FO_SLOW_TEST_TIMEOUT')

    allocate (cmake%model)
    call model_init(cmake%model, 'cmake', 'fixture')
    call model_add_case(cmake%model, 'own', attrs=[attr('TIMEOUT', '600')])
    call model_add_case(cmake%model, 'plain')
    call model_add_case(cmake%model, 'setup', &
        attrs=[attr('TIMEOUT', '120'), attr('FIXTURES_SETUP', 'data')])
    call model_add_case(cmake%model, 'needs', &
        attrs=[attr('TIMEOUT', '600'), attr('FIXTURES_REQUIRED', 'data')])
    call model_finalize(cmake%model, ierr, detail)
    call assert_true(ierr == 0, 'CMake fixture model is valid')

    call assert_equal_integer(case_wall_timeout(cmake, 'own', 0), 605, &
        'a TIMEOUT property sets the Gremlin limit')
    call assert_equal_integer(case_wall_timeout(cmake, 'plain', 0), 1505, &
        'a case without TIMEOUT gets ctest''s default, not the fpm budget')
    call assert_equal_integer(case_wall_timeout(cmake, 'needs', 0), 725, &
        'the fixture setup that runs with a case adds its own limit')
    call assert_equal_integer(case_wall_timeout(cmake, 'own', 30), 30, &
        'an explicit Gremlin timeout wins')

    call set('FO_TEST_WALL_TIMEOUT', '7')
    call assert_equal_integer(case_wall_timeout(cmake, 'plain', 0), 12, &
        'FO_TEST_WALL_TIMEOUT replaces the default of a case without TIMEOUT')
    call assert_equal_integer(case_wall_timeout(cmake, 'own', 0), 605, &
        'FO_TEST_WALL_TIMEOUT leaves a TIMEOUT property in force')
    call unset('FO_TEST_WALL_TIMEOUT')

    call make_scratch('fo-gremlin-case-timeout', scratch)
    allocate (fpm%model)
    call model_init(fpm%model, 'fpm', 'fixture')
    call model_add_case(fpm%model, 'probe')
    call model_finalize(fpm%model, ierr, detail)
    call assert_true(ierr == 0, 'fpm fixture model is valid')
    fpm%project_root = scratch
    call assert_equal_integer(case_wall_timeout(fpm, 'probe', 0), 105, &
        'an fpm test keeps the wall cap of its CPU budget')

    call write_text(join_path(scratch, 'CMakeLists.txt'), &
        'cmake_minimum_required(VERSION 3.20)'//new_line('a'))
    bare%project_root = scratch
    call assert_equal_integer(case_wall_timeout(bare, 'plain', 0), 1505, &
        'a CMake generation without a loaded model falls back to ctest''s default')

    call remove_tree(scratch)
    call finish_assertions()
contains
    function attr(key, value) result(a)
        character(len=*), intent(in) :: key, value
        type(model_attr_t) :: a

        a%key = key
        a%value = value
    end function attr

    subroutine set(name, value)
        character(len=*), intent(in) :: name, value

        call assert_true(c_setenv(name//c_null_char, value//c_null_char, 1_c_int) &
            == 0, 'sets '//name)
    end subroutine set

    subroutine unset(name)
        character(len=*), intent(in) :: name

        call assert_true(c_unsetenv(name//c_null_char) == 0, 'clears '//name)
    end subroutine unset
end program test_gremlin_case_timeout
