program test_package_macros_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, run_process, assert_process_ok, &
        assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, &
        json_string_value
    implicit none
    character(:), allocatable :: driver, scratch, project, manifest, reference
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: result
    type(json_value_t) :: report, entry
    integer :: phase, expected
    character(len=16) :: number

    call resolve_driver(driver)
    call make_scratch('fo-package-macros', scratch)
    project = scratch//'/project'
    call package(project, 'macro_root', 11, 'ROOT_ONLY', &
        '[dependencies]'//nl//'dep_a={path="dep_a"}'//nl// &
        '[dev-dependencies]'//nl//'dep_dev={path="dep_dev"}'//nl)
    call package(project//'/dep_a', 'dep_a', 22, 'A_ONLY', &
        '[dependencies]'//nl//'dep_b={path="../dep_b"}'//nl)
    call package(project//'/dep_b', 'dep_b', 33, 'B_ONLY', '')
    call package(project//'/dep_dev', 'dep_dev', 44, 'DEV_ONLY', &
        '[dependencies]'//nl//'dep_dev_child={path="../dep_dev_child"}'//nl)
    call package(project//'/dep_dev_child', 'dep_dev_child', 55, 'CHILD_ONLY', '')
    call emit_module(project, 'root', 'A_ONLY')
    call emit_module(project//'/dep_a', 'dep_a', 'ROOT_ONLY')
    call emit_module(project//'/dep_b', 'dep_b', 'A_ONLY')
    call emit_module(project//'/dep_dev', 'dep_dev', 'ROOT_ONLY')
    call emit_module(project//'/dep_dev_child', 'dep_dev_child', 'DEV_ONLY')
    call write_text(project//'/test/test_macro_ownership.F90', &
        'program test_macro_ownership'//nl// &
        'use root,only:r=>value,rl=>leak'//nl// &
        'use dep_a,only:a=>value,al=>leak'//nl// &
        'use dep_b,only:b=>value,bl=>leak'//nl// &
        'use dep_dev,only:d=>value,dl=>leak'//nl// &
        'use dep_dev_child,only:c=>value,cl=>leak'//nl// &
        'implicit none'//nl//'integer::expected'//nl//'character(16)::text'//nl// &
        'call get_environment_variable("EXPECTED_B",text)'//nl// &
        'read(text,*)expected'//nl// &
        'if(r/=11.or.a/=22.or.b/=expected.or.d/=44.or.c/'// &
        '=55)error stop "owner values"'//nl// &
        'if(rl.or.al.or.bl.or.dl.or.cl)error stop "cross-package macro leak"'//nl// &
        '#ifndef OWNER'//nl//'error stop "root test macro absent"'//nl// &
        '#else'//nl//'if(OWNER/=11)error stop "root test macro owner"'//nl// &
        '#endif'//nl//'print *,"PACKAGE_MACRO_ORACLE_PASS"'//nl//'end program'//nl)
    manifest = project//'/dep_b/fpm.toml'
    reference = scratch//'/manifest-reference'
    call list_add(args, 'cp')
    call list_add(args, '-p')
    call list_add(args, manifest)
    call list_add(args, reference)
    call run_process(args, scratch, result)
    call assert_process_ok(result, 'save manifest metadata independently')
    do phase = 1, 4
        expected = 33
        if (phase == 3) expected = 34
        if (phase >= 3) then
            call package(project//'/dep_b', 'dep_b', expected, 'B_ONLY', '')
            args = string_list_t()
            call list_add(args, 'touch')
            call list_add(args, '-r')
            call list_add(args, reference)
            call list_add(args, manifest)
            call run_process(args, scratch, result)
            call assert_process_ok(result, 'restore dependency manifest timestamp')
        end if
        write(number, '(i0)') expected
        environment = string_list_t()
        call list_add(environment, 'FO_BACKEND=fpm')
        call list_add(environment, 'FO_JOBS=2')
        call list_add(environment, 'EXPECTED_B='//trim(number))
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'test_macro_ownership')
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, environment)
        call assert_process_ok(result, 'compiled package macro ownership and edits')
        call parse_json_report(result, report, 'package macro runtime report')
        entry = json_element(json_member(report, 'tests'), 1)
        call assert_equal_string(json_string_value(json_member(entry, 'status')), &
            'pass', 'independent package ownership oracle passes')
    end do
    call finish_assertions()
contains
    subroutine package(root, name, value, marker, dependencies)
        character(len=*), intent(in) :: root, name, marker, dependencies
        integer, intent(in) :: value
        character(len=16) :: text
        write(text, '(i0)') value
        call write_text(root//'/fpm.toml', 'name="'//name//'"'//nl// &
            '[preprocess.cpp]'//nl//'macros=["OWNER='//trim(text)//'","'//marker// &
            '"]'//nl//dependencies)
    end subroutine package

    subroutine emit_module(root, name, forbidden)
        character(len=*), intent(in) :: root, name, forbidden
        call write_text(root//'/src/'//name//'.F90', 'module '//name//nl// &
            'implicit none'//nl//'#ifdef OWNER'//nl// &
            'integer,parameter::value=OWNER'//nl//'#else'//nl// &
            'integer,parameter::value=-1'//nl//'#endif'//nl// &
            '#if defined('//forbidden//')'//nl//'logical,parameter::leak=.true.'//nl// &
            '#else'//nl//'logical,parameter::leak=.false.'//nl//'#endif'//nl// &
            'end module'//nl)
    end subroutine emit_module
end program test_package_macros_cli
