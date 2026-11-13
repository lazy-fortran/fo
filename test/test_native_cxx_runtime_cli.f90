program test_native_cxx_runtime_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, assert_process_ok, assert_equal_string, &
        finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, manifest, source, owner
    character(len=6) :: mode
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result
    integer :: phase, shared, dependency

    call resolve_driver(driver)
    call make_scratch('fo-native-cxx-runtime', scratch)
    call list_add(environment, 'FO_JOBS=2')
    call list_add(environment, 'FO_BACKEND=fpm')
    do dependency = 0, 1
        owner = 'root'
        if (dependency == 1) owner = 'dependency'
        do shared = 0, 1
            mode = 'static'
            if (shared == 1) mode = 'shared'
            project = scratch//'/'//owner//'-'//trim(mode)
            manifest = 'name="cxx_runtime"'//nl//'[dependencies]'//nl// &
                'cpp_dep={path="dependency"}'//nl//'[extra.fo]'//nl// &
                'link="'//trim(mode)//'"'//nl//'pic="true"'//nl
            call write_text(project//'/fpm.toml', manifest)
            call write_text(project//'/dependency/fpm.toml', 'name="cpp_dep"'//nl)
            source = '#include <string>'//nl// &
                'extern "C" int cpp_value() { return std::stoi(std::string("42")); }'//nl
            if (dependency == 1) then
                call write_text(project//'/dependency/src/value.cpp', source)
            else
                call write_text(project//'/src/value.cpp', source)
                call write_text(project//'/dependency/src/unused.c', &
                    'int unused(void) { return 0; }'//nl)
            end if
            source = 'program probe'//nl//'use iso_c_binding,only:c_int'//nl// &
                'implicit none'//nl//'interface'//nl// &
                'integer(c_int) function cpp_value() bind(C)'//nl// &
                'import c_int'//nl//'end function'//nl// &
                'end interface'//nl// &
                'if(cpp_value()/=42)error stop 1'//nl// &
                'print "(i0)",cpp_value()'//nl//'end program'//nl
            call write_text(project//'/app/probe.f90', source)
            call write_text(project//'/test/test_probe.f90', source)
            do phase = 1, 2
                arguments = string_list_t()
                call list_add(arguments, 'exec')
                call list_add(arguments, 'probe')
                call run_fo(driver, arguments, project, scratch//'/cache', result, environment)
                call assert_process_ok(result, owner//' '//trim(mode)//' C++ app links')
                call assert_equal_string(result%stdout, '42'//nl, &
                    owner//' '//trim(mode)//' C++ runtime value')
                arguments = string_list_t()
                call list_add(arguments, 'test')
                call list_add(arguments, 'test_probe')
                call run_fo(driver, arguments, project, scratch//'/cache', result, environment)
                call assert_process_ok(result, owner//' '//trim(mode)//' C++ test runs')
            end do
        end do
    end do
    call finish_assertions(retain_failed_scratch=.true.)
end program test_native_cxx_runtime_cli
