program test_process_replaced_image
    !! Reinstalling fo during a long build unlinks the running image. Child
    !! commands such as the archiver must still start afterwards.
    use fo_fs, only: fs_copy_exec
    use fo_process, only: argv_push, process_run_argv_logged, process_self_executable
    use fo_test_harness, only: make_scratch, join_path, remove_path, remove_tree, &
        write_text, read_text, file_exists, assert_true, assert_equal_string, &
        finish_assertions
    implicit none

    character(len=4096) :: mode, scratch_arg, self
    character(:), allocatable :: scratch, copy, verdict, packed
    integer :: exitcode, n_args
    logical :: found

    call get_command_argument(1, mode)
    if (trim(mode) == 'replaced-child') then
        call get_command_argument(2, scratch_arg)
        call remove_path(join_path(trim(scratch_arg), 'image'))
        n_args = 0
        call argv_push(packed, n_args, 'true')
        call process_run_argv_logged(trim(scratch_arg), packed, n_args, &
            join_path(trim(scratch_arg), 'child.log'), .false., 60, exitcode)
        call write_text(join_path(trim(scratch_arg), 'verdict'), itoa(exitcode))
        stop
    end if

    call process_self_executable(self, found)
    call assert_true(found, 'test image path resolves')
    call make_scratch('fo-replaced-image', scratch)
    copy = join_path(scratch, 'image')
    call assert_true(fs_copy_exec(trim(self), copy) == 0, 'test image copied')
    n_args = 0
    call argv_push(packed, n_args, copy)
    call argv_push(packed, n_args, 'replaced-child')
    call argv_push(packed, n_args, scratch)
    call process_run_argv_logged(scratch, packed, n_args, &
        join_path(scratch, 'parent.log'), .false., 60, exitcode)
    call assert_true(exitcode == 0, 'replaced image exits normally')
    verdict = join_path(scratch, 'verdict')
    call assert_true(file_exists(verdict), 'replaced image reports its child status')
    if (file_exists(verdict)) call assert_equal_string(read_text(verdict), '0', &
        'child command starts after the running image was unlinked')
    call remove_tree(scratch)
    call finish_assertions()

contains

    function itoa(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=16) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function itoa

end program test_process_replaced_image
