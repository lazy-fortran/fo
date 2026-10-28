program test_self_refresh_profile
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, read_text, current_directory, assert_true, &
        assert_equal_integer, assert_equal_string, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_find_executable, fs_is_windows
    implicit none

    character(:), allocatable :: driver, scratch, project, wrapper, count_log
    character(len=512) :: compiler, c_compiler
    character(:), allocatable :: cwd
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: result
    logical :: found

    call resolve_driver(driver)
    call make_scratch('fo-self-refresh-profile', scratch)
    call fs_find_executable('gfortran', compiler, found)
    call assert_true(found, 'locate real compiler for profile oracle')
    if (.not. found) call finish_assertions()
    project = scratch//'/project'
    wrapper = scratch//'/gfortran-profile-wrapper'
    count_log = scratch//'/compile.log'
    call current_directory(cwd)
    call fs_find_executable('cc', c_compiler, found)
    if (fs_is_windows()) then
        wrapper = wrapper//'.exe'
        call fs_find_executable('gcc', c_compiler, found)
    end if
    call assert_true(found, 'locate native C compiler for source observer')
    call list_add(args, cwd//'/test-fixtures/c/compiler_probe.c')
    if (fs_is_windows()) call list_add(args, '-municode')
    call list_add(args, '-o')
    call list_add(args, wrapper)
    call run_external(trim(c_compiler), args, cwd, result)
    call assert_process_ok(result, 'build existing native compiler observer')
    call list_add(environment, 'FO_TEST_REAL_COMPILER='//trim(compiler))
    call list_add(environment, 'FO_TEST_COMPILER_MARKER='//count_log)
    call list_add(environment, 'FO_TEST_COMPILER_SOURCES_ONLY=1')
    call list_add(environment, 'FO_FC='//wrapper)
    call list_add(environment, 'FO_JOBS=2')
    call write_text(project//'/fpm.toml', 'name = "profile_probe"'//nl)
    call library_source('PROFILE')
    call write_text(project//'/app/profile.F90', &
        'program profile'//nl//'use payload, only: value'//nl// &
        'implicit none'//nl//'print ''(i0,1x,i0)'', value(), PROFILE'//nl// &
        'end program profile'//nl)
    call invoke('-cpp -DPROFILE=17', '17 17', 1, 1)
    call invoke('-cpp -DPROFILE=29', '29 29', 2, 2)
    call invoke('-cpp -DPROFILE=17', '17 17', 2, 2)
    call library_source('PROFILE + 1')
    call invoke('-cpp -DPROFILE=17', '18 17', 3, 2)
    call finish_assertions(retain_failed_scratch=.true.)
contains
    subroutine library_source(expression)
        character(len=*), intent(in) :: expression
        call write_text(project//'/src/payload.F90', &
            'module payload'//nl//'implicit none'//nl//'contains'//nl// &
            'integer function value()'//nl//'value = '//expression//nl// &
            'end function value'//nl//'end module payload'//nl)
    end subroutine library_source

    subroutine invoke(flags, expected, library_count, app_count)
        character(len=*), intent(in) :: flags, expected
        integer, intent(in) :: library_count, app_count
        character(:), allocatable :: transcript

        args = string_list_t()
        call list_add(args, 'exec')
        call list_add(args, '--flag')
        call list_add(args, flags)
        call list_add(args, 'profile')
        call run_fo(driver, args, project, scratch//'/cache', result, environment)
        call assert_process_ok(result, 'real library and app profile execution')
        call assert_equal_string(result%stdout, expected//nl, &
            'selected profile controls both implementation and application')
        transcript = read_text(count_log)
        call assert_equal_integer(occurrences(transcript, '/payload.F90'), &
            library_count, 'library cold, flag-change, hit and source-change counts')
        call assert_equal_integer(occurrences(transcript, '/profile.F90'), &
            app_count, 'app cold, flag-change and profile-hit counts')
    end subroutine invoke

    integer function occurrences(text, needle) result(count)
        character(len=*), intent(in) :: text, needle
        integer :: cursor, found_at
        cursor = 1
        count = 0
        do
            found_at = index(text(cursor:), needle)
            if (found_at == 0) exit
            count = count + 1
            cursor = cursor + found_at + len(needle) - 1
        end do
    end function occurrences
end program test_self_refresh_profile
