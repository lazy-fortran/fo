program test_gremlin_help_cli
    !! fo gremlin ACTION --help lists the options that action accepts, with
    !! their choices and defaults, without starting or contacting a lane.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_process_ok
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(:), allocatable :: driver, scratch, text

    call resolve_driver(driver)
    call make_scratch('fo-gremlin-help', scratch)
    call make_directory(join_path(scratch, 'cache'))

    text = help_text([character(len=16) :: 'gremlin', 'wait', '--help'])
    call assert_contains(text, 'usage: fo gremlin wait', 'wait help names the action')
    call assert_contains(text, '--until', 'wait help lists --until')
    call assert_contains(text, &
        'local-gate-green,ordinary-verified,fully-verified,quiescent,failure', &
        'wait help lists the --until choices')
    call assert_contains(text, '--cursor N', 'wait help lists --cursor')
    call assert_contains(text, '--lifecycle-cursor N', 'wait help lists --lifecycle-cursor')
    call assert_contains(text, '--since', 'wait help lists --since')
    call assert_contains(text, '--wait-ms N', 'wait help lists --wait-ms')
    call assert_contains(text, 'default: 5000', 'wait help states the --wait-ms default')
    call assert_not_contains(text, '--random', 'wait help omits start-only options')

    text = help_text([character(len=16) :: 'gremlin', 'start', '-h'])
    call assert_contains(text, '--owner|--lifetime CHOICE', 'start help lists --owner')
    call assert_contains(text, 'persistent,task', 'start help lists owner choices')
    call assert_contains(text, '--random N', 'start help lists --random')
    call assert_not_contains(text, '--until', 'start help omits wait-only options')

    text = help_text([character(len=16) :: 'gremlin', '--help'])
    call assert_contains(text, 'usage: fo gremlin reproduce', 'general help covers reproduce')
    call assert_contains(text, 'usage: fo gremlin wait', 'general help covers wait')

    text = help_text([character(len=16) :: 'help', 'gremlin'])
    call assert_contains(text, 'usage: fo gremlin stop', 'fo help gremlin covers stop')

    call finish_assertions()

contains

    function help_text(words) result(output)
        character(len=*), intent(in) :: words(:)
        character(:), allocatable :: output
        type(string_list_t) :: arguments
        type(process_result_t) :: result
        integer :: i

        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), result)
        call assert_process_ok(result, 'fo '//trim(words(1))//' '//trim(words(2))// &
            ' help exits 0')
        output = result%stdout
    end function help_text

end program test_gremlin_help_cli
