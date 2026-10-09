program test_harness_failure_evidence
    use fo_test_harness, only: make_scratch, exercise_failure_cleanup_probe
    use fo_test_harness, only: finish_assertions
    use fo_test_harness, only: harness_probe_entry
    use fo_test_harness, only: make_directory, write_text, file_exists, remove_tree
    use fo_test_harness, only: string_list_t, process_result_t, list_add, run_process
    use fo_test_harness, only: resolve_test_executable, assert_true
    use fo_test_harness, only: assert_equal_integer
    use fo_test_harness, only: assert_file_equals, assert_contains
    use fo_test_harness, only: move_path, read_text, assert_not_contains
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_cli, only: exercise_harness
    implicit none

    character(:), allocatable :: scratch

    interface
        integer(c_int) function scratch_create(view, parent, output, capacity) &
                bind(C, name='fo_c_execution_scratch_create')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: view(*), parent(*)
            character(kind=c_char), intent(out) :: output(*)
            integer(c_int), value :: capacity
        end function scratch_create

        integer(c_int) function scratch_release(view) &
                bind(C, name='fo_c_execution_scratch_release')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: view(*)
        end function scratch_release
    end interface

    call harness_probe_entry()
    call make_scratch('fo-harness-evidence-oracle', scratch)
    call exercise_failure_cleanup_probe(scratch//'/default.marker')
    call exercise_failure_cleanup_probe(scratch//'/retained.marker', &
        retain_failed_scratch=.true.)
    call exercise_nested_execution_cleanup(scratch)
    call exercise_harness()
    call finish_assertions()

contains

    subroutine exercise_nested_execution_cleanup(root)
        character(len=*), intent(in) :: root
        character(:), allocatable :: outer_tmp, inner_tmp, executable, retained
        character(:), allocatable :: outer_view, inner_view
        character(kind=c_char) :: output(4096)
        character(len=*), parameter :: prefix = 'Retained failed fixture scratch: '
        type(string_list_t) :: command, environment
        type(process_result_t) :: result
        integer :: rc, first, last
        logical :: ok

        outer_view = root//'/outer-view'
        call make_directory(outer_view)
        rc = scratch_create(outer_view//c_null_char, root//c_null_char, output, &
            int(size(output), c_int))
        call assert_equal_integer(rc, 0, 'create actual owned outer execution TMP')
        if (rc /= 0) return
        outer_tmp = c_text(output)
        inner_view = outer_tmp//'/inner-view'
        call make_directory(inner_view)
        rc = scratch_create(inner_view//c_null_char, outer_tmp//c_null_char, &
            output, int(size(output), c_int))
        call assert_equal_integer(rc, 0, 'create nested owned execution TMP')
        if (rc /= 0) then
            rc = scratch_release(outer_view//c_null_char)
            return
        end if
        inner_tmp = c_text(output)
        call write_text(outer_tmp//'/ordinary.tmp', 'discard outer temporary bytes')
        call write_text(inner_tmp//'/ordinary.tmp', 'discard inner temporary bytes')
        call resolve_test_executable(executable, ok)
        call assert_true(ok, 'resolve failed evidence subprocess')
        if (ok) then
            call list_add(command, executable)
            call list_add(command, '--fo-test-cleanup-probe')
            call list_add(command, root//'/nested.marker')
            call list_add(command, '1')
            call list_add(environment, 'TMPDIR='//inner_tmp)
            call move_path(inner_tmp//'/.fo-view-owner', root//'/saved-owner')
            call move_path(outer_tmp//'/.fo-view-owner', inner_tmp//'/.fo-view-owner')
            call run_process(command, '.', result, environment, timeout_ms=20000)
            call assert_contains(result%stderr, &
                'FAIL: preserve failed fixture scratch:', &
                'mismatched scratch ownership is an explicit retention failure')
            call assert_not_contains(result%stderr, prefix, &
                'invalid ownership never reports an evidence promotion')
            retained = read_text(root//'/nested.marker')
            call assert_file_equals(retained//'/journal.jsonl', &
                'unique failure receipt', &
                'invalid ownership leaves the original fixture bytes untouched')
            deallocate(retained)
            call move_path(inner_tmp//'/.fo-view-owner', outer_tmp//'/.fo-view-owner')
            call move_path(root//'/saved-owner', inner_tmp//'/.fo-view-owner')
            call run_process(command, '.', result, environment, timeout_ms=20000)
            call assert_true(.not. result%runner_failed .and. result%reaped, &
                'nested failed fixture owner is reaped before TMP cleanup')
            call assert_equal_integer(result%exit_code, 1, &
                'nested fixture failure remains a failing subprocess')
            call assert_contains(result%stderr, 'Retained failed fixture bytes: 22', &
                'retention reports exact unique journal byte count')
            first = index(result%stderr, prefix)
            call assert_true(first > 0, 'nested retention reports the stable path')
            if (first > 0) then
                first = first + len(prefix)
                last = index(result%stderr(first:), new_line('a'))
                if (last > 0) retained = result%stderr(first:first + last - 2)
            end if
        end if
        rc = scratch_release(inner_view//c_null_char)
        call assert_equal_integer(rc, 0, 'release nested execution TMP normally')
        rc = scratch_release(outer_view//c_null_char)
        call assert_equal_integer(rc, 0, 'release outer execution TMP normally')
        call assert_true(.not. file_exists(inner_tmp), 'ordinary nested TMP is removed')
        call assert_true(.not. file_exists(outer_tmp), 'ordinary outer TMP is removed')
        call assert_true(allocated(retained), 'retention returns an evidence location')
        if (.not. allocated(retained)) return
        call assert_true(index(retained, outer_tmp//'/') == 0, &
            'failed evidence escapes every enclosing owned execution TMP')
        call assert_file_equals(retained//'/journal.jsonl', 'unique failure receipt', &
            'exact failed journal survives actual nested and outer TMP releases')
        last = index(retained, '/fixture', back=.true.)
        if (last > 0) call remove_tree(retained(:last - 1))
    end subroutine exercise_nested_execution_cleanup

    function c_text(bytes) result(text)
        character(kind=c_char), intent(in) :: bytes(:)
        character(:), allocatable :: text
        integer :: i, length

        length = 0
        do i = 1, size(bytes)
            if (bytes(i) == c_null_char) exit
            length = i
        end do
        text = repeat(' ', length)
        do i = 1, length
            text(i:i) = bytes(i)
        end do
    end function c_text
end program test_harness_failure_evidence
