program test_debug_info_diet
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, run_process, assert_process_ok, assert_true, &
        assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_fs, only: fs_find_executable, fs_is_windows
    implicit none

    character(:), allocatable :: driver, scratch, project, object, inspector, option
    character(len=512) :: command_path
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: environment
    logical :: found, linux

    call resolve_driver(driver)
    call make_scratch('fo-debug-info-diet', scratch)
    project = scratch//'/project'
    object = project//'/build/fo/obj/app_main.f90.o'
    call fs_find_executable('gfortran', command_path, found)
    call assert_true(found, 'GNU compiler is available for its debug-info oracle')
    call list_add(environment, 'FO_FC='//trim(command_path))
    call list_add(environment, 'FO_BACKEND=fpm')
    call list_add(environment, 'FO_JOBS=2')
    inquire(file='/proc/self/status', exist=linux)
    if (linux) then
        inspector = 'readelf'
        option = '--debug-dump=info'
    else if (fs_is_windows()) then
        inspector = 'objdump'
        option = '--dwarf=info'
    else
        inspector = 'dwarfdump'
        option = '--debug-info'
    end if
    call fs_find_executable(inspector, command_path, found)
    call assert_true(found, 'toolchain provides an independent DWARF inspector')
    inspector = trim(command_path)
    call write_text(project//'/app/main.f90', &
        'program manifest_debug_probe'//nl//'implicit none'//nl// &
        'integer,volatile::debug_local'//nl//'debug_local=31'//nl// &
        'if(debug_local/=31)error stop "runtime31"'//nl// &
        'print *,debug_local'//nl//'end program'//nl)

    call manifest('full')
    call observe('', .true., .true., 'manifest full enables compiler DWARF')
    call manifest('g0')
    call observe('', .false., .false., 'warm manifest edit removes compiler DWARF')
    call observe('--debug', .true., .true., 'explicit debug overrides manifest g0')
    call observe('--asan', .true., .true., 'explicit sanitizer retains compiler DWARF')
    call manifest('line-tables')
    call observe('', .true., .false., 'GNU line tables omit full local-variable DWARF')
    call manifest('full')
    call observe('', .true., .true., 'warm restoration recovers full compiler DWARF')
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine manifest(budget)
        character(len=*), intent(in) :: budget
        call write_text(project//'/fpm.toml', 'name="debug_probe"'//nl// &
            '[extra.fo]'//nl//'debug-info="'//budget//'"'//nl)
    end subroutine manifest

    subroutine observe(request, has_debug, has_local, description)
        character(len=*), intent(in) :: request, description
        logical, intent(in) :: has_debug, has_local
        type(string_list_t) :: arguments
        type(process_result_t) :: result

        call list_add(arguments, 'build')
        if (len(request) > 0) call list_add(arguments, request)
        call run_fo(driver, arguments, project, scratch//'/cache', result, &
            environment, 60000)
        call assert_process_ok(result, description)
        arguments = string_list_t()
        call list_add(arguments, inspector)
        call list_add(arguments, option)
        call list_add(arguments, object)
        call run_process(arguments, project, result)
        call assert_process_ok(result, 'independent inspector reads compiled object')
        if (has_debug) then
            call assert_contains(result%stdout, 'DW_TAG_compile_unit', description)
        else
            call assert_not_contains(result%stdout, 'DW_TAG_compile_unit', description)
        end if
        if (has_local) then
            call assert_contains(result%stdout, 'debug_local', description)
        else
            call assert_not_contains(result%stdout, 'debug_local', description)
        end if
    end subroutine observe

end program test_debug_info_diet
