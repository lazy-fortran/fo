program test_scan_dependency_capacity_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, make_directory, join_path, write_text, run_process, &
        assert_true, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_number_value
    implicit none
    integer, parameter :: dependencies = 128
    character(:), allocatable :: driver, scratch, umbrella, suffix
    character(len=16) :: number
    type(string_list_t) :: args, env, native
    type(process_result_t) :: observed
    type(json_value_t) :: report
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-many-source-dependencies', scratch)
    call make_directory(scratch//'/src')
    call make_directory(scratch//'/test')
    call write_text(scratch//'/fpm.toml', 'name="many-source-dependencies"')
    umbrella = 'module umbrella'//new_line('a')
    do i = 1, dependencies
        write(number, '(i0)') i
        suffix = trim(number)
        call write_leaf(i, i)
        umbrella = umbrella//'use leaf_'//suffix//', only: value_'//suffix// &
            new_line('a')
    end do
    umbrella = umbrella//'implicit none'//new_line('a')// &
        'integer, parameter :: answer = 0'
    do i = 1, dependencies
        write(number, '(i0)') i
        umbrella = umbrella//' + &'//new_line('a')//'value_'//trim(number)
    end do
    umbrella = umbrella//new_line('a')//'end module umbrella'
    call write_text(scratch//'/src/00_umbrella.f90', umbrella)
    call write_text(scratch//'/test/test_payload.f90', &
        'program test_payload'//new_line('a')//'use umbrella, only: answer'// &
        new_line('a')//'implicit none'//new_line('a')// &
        'if (answer /= 8256) error stop "dependency payload changed"'// &
        new_line('a')//'print *, "payload verified"'//new_line('a')// &
        'end program test_payload')
    call list_add(native, 'fpm')
    call list_add(native, 'test')
    call list_add(native, '--target')
    call list_add(native, 'test_payload')
    call list_add(native, '--profile')
    call list_add(native, 'release')
    call run_process(native, scratch, observed)
    call assert_process_ok(observed, 'native FPM builds and checks all 128 dependencies')
    call assert_true(index(observed%stdout, 'payload verified') > 0, &
        'native compiler establishes the independent payload oracle')
    call list_add(env, 'FO_BACKEND=fpm')
    call list_add(env, 'FO_JOBS=4')
    call list_add(args, 'test')
    call list_add(args, 'test_payload')
    call list_add(args, '--json')
    call check(.true., 'cold Fo dependency graph')
    call check(.true., 'warm serialized dependency graph')
    call write_leaf(dependencies, dependencies + 1000)
    call check(.false., 'late dependency edit changes executable payload')
    call write_leaf(dependencies, dependencies)
    call check(.true., 'restored dependency restores exact payload')
    call finish_assertions()
contains
    subroutine write_leaf(index, value)
        integer, intent(in) :: index, value
        character(len=16) :: label, text
        write(label, '(i0)') index
        write(text, '(i0)') value
        call write_text(scratch//'/src/leaf_'//trim(label)//'.f90', &
            'module leaf_'//trim(label)//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, parameter :: value_'//trim(label)//' = '//trim(text)// &
            new_line('a')//'end module leaf_'//trim(label))
    end subroutine write_leaf
    subroutine check(success, label)
        logical, intent(in) :: success
        character(len=*), intent(in) :: label
        call run_fo(driver, args, scratch, join_path(scratch, 'cache'), observed, env)
        call assert_true((observed%exit_code == 0) .eqv. success, label)
        call parse_json_report(observed, report, label//' produces structured evidence')
        call assert_true((json_number_value(json_member(report, 'exit_code')) == 0) &
            .eqv. success, label//' JSON agrees with actual process')
    end subroutine check
end program test_scan_dependency_capacity_cli
