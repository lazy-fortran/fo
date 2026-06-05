module fo_check_output
    use fo_util, only: json_bool, json_int
    use fx_json_build, only: json_escape_string
    use fo_check, only: check_result_t, fo_check_run
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
                               capabilities_json
    implicit none
    private
    public :: check_result_text, check_result_json
    public :: check_result_compact_json, check_result_full_json
    public :: fo_check_write

contains

    function agent_summary(res) result(summary)
        type(check_result_t), intent(in) :: res
        character(len=512) :: summary

        if (len_trim(res%summary) > 0) then
            summary = res%summary
        else if (res%build_ok .and. res%tests_ok) then
            summary = 'build and tests passed'
        else if (len_trim(res%error_msg) > 0) then
            summary = res%error_msg
        else
            summary = 'fo check did not complete'
        end if
    end function agent_summary

    function check_result_text(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=2048) :: line

        character(len=32) :: modules, cached, changed, affected, elapsed

        write (modules, '(i0)') res%n_modules
        write (cached, '(i0)') res%n_cached
        write (changed, '(i0)') res%n_changed
        write (affected, '(i0)') res%n_affected
        write (elapsed, '(f12.3)') res%elapsed

        if (res%build_ok .and. res%tests_ok) then
            line = 'OK modules='//trim(modules)//' cached='//trim(cached)
            line = trim(line)//' changed='//trim(changed)
            line = trim(line)//' affected='//trim(affected)
            if (res%n_in_cycle > 0) then
                block
                    character(len=32) :: cyc
                    write (cyc, '(i0)') res%n_in_cycle
                    line = trim(line)//' cycle_warning='//trim(cyc)
                end block
            end if
            line = trim(line)//' elapsed_s='//trim(adjustl(elapsed))
        else if (.not. res%build_ok) then
            line = 'Build: FAIL '//trim(res%error_msg)
        else
            line = 'Tests: FAIL '//trim(res%error_msg)
        end if
    end function check_result_text

    function check_result_json(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=2048) :: line

        character(len=32) :: modules, cached, changed, affected, elapsed

        write (modules, '(i0)') res%n_modules
        write (cached, '(i0)') res%n_cached
        write (changed, '(i0)') res%n_changed
        write (affected, '(i0)') res%n_affected
        write (elapsed, '(f12.3)') res%elapsed

        line = '{'
        line = trim(line)//'"build_ok":'//trim(json_bool(res%build_ok))
        line = trim(line)//',"tests_ok":'//trim(json_bool(res%tests_ok))
        line = trim(line)//',"modules":'//trim(modules)
        line = trim(line)//',"cached":'//trim(cached)
        line = trim(line)//',"changed":'//trim(changed)
        line = trim(line)//',"affected":'//trim(affected)
        if (res%n_in_cycle > 0) &
            line = trim(line)//',"in_cycle":'//trim(json_int(res%n_in_cycle))
        line = trim(line)//',"elapsed_s":'//trim(adjustl(elapsed))
        line = trim(line)//',"error":"'
        line = trim(line)//trim(json_escape_string(res%error_msg))//'"}'
    end function check_result_json

    function test_results_json(res) result(s)
        type(check_result_t), intent(in) :: res
        character(len=4096) :: s

        integer :: i

        if (res%n_test_results == 0) then
            s = ''
            return
        end if
        s = ',"tests":['
        do i = 1, res%n_test_results
            if (i > 1) s = trim(s)//','
            s = trim(s)//'{"name":"'// &
                trim(json_escape_string(res%test_results(i)%name))//'"'
            s = trim(s)//',"pass":'//trim(json_int(res%test_results(i)%n_pass))
            s = trim(s)//',"fail":'//trim(json_int(res%test_results(i)%n_fail))
            s = trim(s)//'}'
        end do
        s = trim(s)//']'
    end function test_results_json

    function check_result_compact_json(res) result(line)
        type(check_result_t), intent(in) :: res
        character(len=8192) :: line

        character(len=2048) :: base

        base = make_agent_json(res, .false.)
        line = base(1:len_trim(base) - 1)
        line = trim(line)//trim(test_results_json(res))//'}'
    end function check_result_compact_json

    function check_result_full_json(res, cap_json_str) result(line)
        type(check_result_t), intent(in) :: res
        character(len=*), intent(in) :: cap_json_str
        character(len=16384) :: line

        character(len=2048) :: base

        base = check_result_json(res)
        line = base(1:len_trim(base) - 1)
        line = trim(line)//',"stage":"'//trim(json_escape_string(res%stage))//'"'
        line = trim(line)//',"target":"'//trim(json_escape_string(res%target))//'"'
        line = trim(line)//',"summary":"'//trim(json_escape_string(agent_summary(res)))//'"'
        line = trim(line)//',"hint":"'//trim(json_escape_string(res%hint))//'"'
        line = trim(line)//',"rerun":"'//trim(json_escape_string(res%rerun))//'"'
        line = trim(line)//',"log_path":"'//trim(json_escape_string(res%log_path))//'"'
        line = trim(line)//trim(test_results_json(res))
        if (res%build_ok .and. res%tests_ok) then
            line = trim(line)//',"diagnostics":[]'
        else
            line = trim(line)//',"diagnostics":[{"kind":"'// &
                   trim(json_escape_string(res%stage))//'"'
            if (len_trim(res%diag_file) > 0) then
                line = trim(line)//',"file":"'// &
                       trim(json_escape_string(res%diag_file))//'"'
            else
                line = trim(line)//',"file":""'
            end if
            line = trim(line)//',"line":'//trim(json_int(res%diag_line))
            line = trim(line)//',"column":'//trim(json_int(res%diag_column))
            line = trim(line)//',"target":"'//trim(json_escape_string(res%target))//'"'
            line = trim(line)//',"message":"'// &
                   trim(json_escape_string(agent_summary(res)))//'"'
            line = trim(line)//',"hint":"'//trim(json_escape_string(res%hint))//'"'
            line = trim(line)//',"rerun":"'//trim(json_escape_string(res%rerun))//'"}]'
        end if
        if (len_trim(cap_json_str) > 0) then
            line = trim(line)//',"capabilities":'//trim(cap_json_str)//'}'
        else
            line = trim(line)//'}'
        end if
    end function check_result_full_json

    function make_agent_json(res, include_legacy) result(line)
        type(check_result_t), intent(in) :: res
        logical, intent(in) :: include_legacy
        character(len=2048) :: line

        character(len=32) :: elapsed
        logical :: ok

        ok = res%build_ok .and. res%tests_ok
        write (elapsed, '(f12.3)') res%elapsed

        line = '{'
        line = trim(line)//'"ok":'//trim(json_bool(ok))
        line = trim(line)//',"stage":"'//trim(json_escape_string(res%stage))//'"'
        line = trim(line)//',"target":"'//trim(json_escape_string(res%target))//'"'
       line = trim(line)//',"summary":"'//trim(json_escape_string(agent_summary(res)))//'"'
        line = trim(line)//',"hint":"'//trim(json_escape_string(res%hint))//'"'
        line = trim(line)//',"rerun":"'//trim(json_escape_string(res%rerun))//'"'
        line = trim(line)//',"log_path":"'//trim(json_escape_string(res%log_path))//'"'
        line = trim(line)//',"elapsed_s":'//trim(adjustl(elapsed))
        line = trim(line)//',"modules":'//trim(json_int(res%n_modules))
        line = trim(line)//',"cached":'//trim(json_int(res%n_cached))
        line = trim(line)//',"changed":'//trim(json_int(res%n_changed))
        if (include_legacy) then
            line = trim(line)//',"legacy":'//trim(check_result_json(res))
        end if
        line = trim(line)//'}'
    end function make_agent_json

    subroutine fo_check_write(dir, mode, output_path, ierr)
        character(len=*), intent(in) :: dir, mode, output_path
        integer, intent(out) :: ierr

        type(check_result_t) :: res
        type(capabilities_t) :: cap
        character(len=2048) :: cap_json
        character(len=8192) :: line
        integer :: u, io
        logical :: need_caps

        ierr = 0
        if (len_trim(output_path) == 0) then
            ierr = 3
            return
        end if
        select case (trim(mode))
        case ('', 'text', 'json', 'json=compact', 'compact', &
              'json=full', 'full', 'agent')
        case default
            ierr = 2
            return
        end select

        need_caps = (trim(mode) == 'json=full' .or. trim(mode) == 'full')
        cap_json = ''
        if (need_caps) then
            call detect_capabilities(cap)
            call capabilities_json(cap, cap_json)
        end if

        call fo_check_run(dir, res)
        select case (trim(mode))
        case ('', 'text')
            line = check_result_text(res)
        case ('json')
            line = check_result_json(res)
        case ('json=compact', 'compact')
            line = check_result_compact_json(res)
        case ('json=full', 'full')
            line = check_result_full_json(res, cap_json)
        case ('agent')
            line = check_result_compact_json(res)
        case default
            ierr = 2
            return
        end select

        open (newunit=u, file=trim(output_path), status='replace', &
              action='write', iostat=io)
        if (io /= 0) then
            ierr = 3
            return
        end if
        write (u, '(a)') trim(line)
        close (u)

        if (.not. (res%build_ok .and. res%tests_ok)) ierr = 1
    end subroutine fo_check_write

end module fo_check_output
