module fo_check_output
    use fo_check, only: check_result_t
    use fo_json_object, only: json_value_raw
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, json_key, &
        json_value_string, json_key_string, json_key_int, json_key_bool, &
        json_to_string
    implicit none
    private
    public :: check_result_compact_json, check_result_full_json

contains

    function agent_summary(res) result(summary)
        type(check_result_t), intent(in) :: res
        character(len=512) :: summary

        if (len_trim(res%summary) > 0) then
            summary = res%summary
        else if (res%build_ok .and. res%tests_ok) then
            if (res%n_tests_selected == 0) then
                summary = 'build passed; no affected tests ran'
            else
                summary = 'build and tests passed'
            end if
        else if (len_trim(res%error_msg) > 0) then
            summary = res%error_msg
        else
            summary = 'fo check did not complete'
        end if
    end function agent_summary

    subroutine compact_members(jb, res)
        !! The members of the compact object, inside an open object.
        type(json_builder_t), intent(inout) :: jb
        type(check_result_t), intent(in) :: res
        character(len=32) :: elapsed
        integer :: i, n_listed

        write (elapsed, '(f12.3)') res%elapsed
        ! The verdict comes first so a reader can stop after one line.
        call json_key_string(jb, 'summary', trim(agent_summary(res)))
        call json_key_bool(jb, 'ok', res%build_ok .and. res%tests_ok)
        call json_key_string(jb, 'stage', trim(res%stage))
        call json_key_string(jb, 'target', trim(res%target))
        call json_key_string(jb, 'hint', trim(res%hint))
        call json_key_string(jb, 'rerun', trim(res%rerun))
        call json_key_string(jb, 'log_path', trim(res%log_path))
        call json_key(jb, 'elapsed_s')
        call json_value_raw(jb, trim(adjustl(elapsed)))
        call json_key_int(jb, 'modules', res%n_modules)
        call json_key_int(jb, 'cached', res%n_cached)
        call json_key_int(jb, 'changed', res%n_changed)
        if (res%n_tests_selected >= 0) &
            call json_key_int(jb, 'tests_selected', res%n_tests_selected)
        ! Every test that did not pass, and every failing test by name, so an
        ! agent sees the whole problem in one call; passing suites are counted.
        n_listed = 0
        do i = 1, res%n_test_results
            if (trim(res%test_results(i)%status) /= 'pass') n_listed = n_listed + 1
        end do
        if (res%n_test_results > 0) then
            call json_key_int(jb, 'tests_total', res%n_test_results)
            call json_key_int(jb, 'tests_passed', res%n_test_results - n_listed)
        end if
        if (n_listed > 0) then
            call json_key(jb, 'tests')
            call json_array_start(jb)
            do i = 1, res%n_test_results
                if (trim(res%test_results(i)%status) == 'pass') cycle
                call json_object_start(jb)
                call json_key_string(jb, 'name', trim(res%test_results(i)%name))
                call json_key_string(jb, 'status', trim(res%test_results(i)%status))
                call json_key_int(jb, 'pass', res%test_results(i)%n_pass)
                call json_key_int(jb, 'fail', res%test_results(i)%n_fail)
                call json_object_end(jb)
            end do
            call json_array_end(jb)
        end if
        if (res%n_failed_tests > 0) then
            call json_key(jb, 'failed_tests')
            call json_array_start(jb)
            do i = 1, res%n_failed_tests
                call json_value_string(jb, trim(res%failed_tests(i)))
            end do
            call json_array_end(jb)
        end if
    end subroutine compact_members

    function check_result_compact_json(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=:), allocatable :: line
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call compact_members(jb, res)
        call json_object_end(jb)
        line = json_to_string(jb)
    end function check_result_compact_json

    function check_result_full_json(res, cap_json_str) result(line)
        type(check_result_t), intent(in) :: res
        character(len=*), intent(in) :: cap_json_str
        character(len=:), allocatable :: line
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call compact_members(jb, res)
        call json_key(jb, 'diagnostics')
        call json_array_start(jb)
        if (.not. (res%build_ok .and. res%tests_ok)) then
            call json_object_start(jb)
            call json_key_string(jb, 'kind', trim(res%stage))
            call json_key_string(jb, 'file', trim(res%diag_file))
            call json_key_int(jb, 'line', res%diag_line)
            call json_key_int(jb, 'column', res%diag_column)
            call json_key_string(jb, 'target', trim(res%target))
            call json_key_string(jb, 'message', trim(agent_summary(res)))
            call json_key_string(jb, 'hint', trim(res%hint))
            call json_key_string(jb, 'rerun', trim(res%rerun))
            call json_object_end(jb)
        end if
        call json_array_end(jb)
        if (len_trim(cap_json_str) > 0) then
            call json_key(jb, 'capabilities')
            call json_value_raw(jb, trim(cap_json_str))
        end if
        call json_object_end(jb)
        line = json_to_string(jb)
    end function check_result_full_json

end module fo_check_output
