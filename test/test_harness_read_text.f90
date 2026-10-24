! Regression for lazy-fortran/fo#254: read_text must survive a file that is
! truncated and rewritten between its size inspection and its read, as
! fo watch --fmt does to the file a fixture polls.
module test_harness_read_text_probes
    use fo_test_harness, only: write_text
    implicit none
    private
    public :: probe_calls, shrink_once, grow_once

    integer, save :: probe_calls = 0

contains

    subroutine shrink_once(probe_path)
        character(len=*), intent(in) :: probe_path
        probe_calls = probe_calls + 1
        if (probe_calls == 1) call write_text(probe_path, 'program p' // new_line('a'))
    end subroutine shrink_once

    subroutine grow_once(probe_path)
        character(len=*), intent(in) :: probe_path
        probe_calls = probe_calls + 1
        if (probe_calls == 1) call write_text(probe_path, 'a longer replacement' // new_line('a'))
    end subroutine grow_once

end module test_harness_read_text_probes

program test_harness_read_text
    use fo_test_harness, only: make_scratch, write_text, read_text, assert_equal_string, &
        set_read_text_probe, finish_assertions
    use test_harness_read_text_probes, only: probe_calls, shrink_once, grow_once
    implicit none

    character(:), allocatable :: scratch, path

    call make_scratch('fo-harness-read-text', scratch)
    path = scratch // '/sample.f90'

    ! Shrink: the read would meet end of file before the inspected size.
    call write_text(path, 'program old_sample' // new_line('a') // &
        '  integer :: unformatted_value' // new_line('a') // 'end program' // new_line('a'))
    probe_calls = 0
    call set_read_text_probe(shrink_once)
    call assert_equal_string(read_text(path), 'program p' // new_line('a'), &
        'read_text returns the rewritten content after a concurrent truncation')
    call assert_equal_string(itoa(probe_calls), '2', &
        'read_text retried the read that the truncation interrupted')

    ! Grow: the inspected size covers only a prefix of the new content.
    call write_text(path, 'short' // new_line('a'))
    probe_calls = 0
    call set_read_text_probe(grow_once)
    call assert_equal_string(read_text(path), 'a longer replacement' // new_line('a'), &
        'read_text does not return a prefix of content that grew under the read')
    call assert_equal_string(itoa(probe_calls), '2', &
        'read_text retried the read whose file grew')

    call set_read_text_probe()
    call finish_assertions()

contains

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=16) :: buffer
        write(buffer, '(i0)') value
        text = trim(buffer)
    end function itoa

end program test_harness_read_text
