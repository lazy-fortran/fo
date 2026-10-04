program test_gremlin_execution_view
    use fo_cache, only: HASH_LEN, cache_file_digest
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_write_text
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    use fo_input_inventory, only: input_declaration_t, input_inventory_t, &
        input_inventory_discover, INPUT_FILE
    use fo_process, only: argv_push, process_getcwd, process_run_argv_logged
    use fo_util, only: delete_tmpfile, make_tmpfile, read_text_file
    implicit none

    type(input_declaration_t) :: declarations(1)
    type(input_inventory_t) :: inventory, escaping
    type(execution_view_t) :: first, second, rejected
    character(len=512) :: root, views, source_file, text, message
    character(len=512) :: first_fixture, second_output, source_digest
    character(len=512) :: after_digest, escape_path
    character(len=512) :: argument, executable, current_dir, probe_log
    character(len=:), allocatable :: packed
    integer :: n_args, probe_exit, cwd_status
    integer :: ierr, release_status, i
    logical :: exists

    call get_command_argument(1, argument)
    if (trim(argument) == '--execution-view-probe') then
        call runtime_probe()
        stop
    end if

    call make_tmpfile('fo-view-oracle', root)
    call fs_make_dir(trim(root)//'/fixtures')
    call fs_make_dir(trim(root)//'/views')
    call fs_write_text(trim(root)//'/fpm.toml', &
        'name = "execution_view_fixture"'//new_line('a')// &
        'version = "0.1.0"')
    source_file = trim(root)//'/fixtures/input.txt'
    call fs_write_text(trim(source_file), 'frozen fixture')
    call cache_file_digest(trim(source_file), source_digest)

    declarations(1)%root_alias = 'project'
    declarations(1)%relative_path = 'fixtures/input.txt'
    declarations(1)%role = 'test-fixture'
    declarations(1)%expected_kind = INPUT_FILE
    declarations(1)%writable_at_execution = .true.
    call input_inventory_discover(trim(root), declarations, inventory, ierr, message)
    call check(ierr == 0 .and. inventory%complete, &
        'discover the fixture through the canonical inventory')

    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-case-1', 'test_case', inventory, .true., .true., first, ierr, message)
    call check(ierr == 0 .and. first%active .and. first%complete, &
        'create a complete private fixture view')
    first_fixture = trim(first%cwd)//'/fixtures/input.txt'
    call read_text_file(trim(first_fixture), text)
    call check(index(text, 'frozen fixture') > 0, 'materialize the declared fixture')
    call fs_write_text(trim(first_fixture), 'private mutation')
    call read_text_file(trim(first_fixture), text)
    call check(index(text, 'private mutation') > 0, &
        'writable intent permits a private edit')
    call cache_file_digest(trim(source_file), after_digest)
    call check(source_digest == after_digest, &
        'fixture writes leave source bytes unchanged')

    call get_command_argument(0, executable)
    if (len_trim(executable) > 0 .and. executable(1:1) /= '/') then
        call process_getcwd(current_dir, cwd_status)
        call check(cwd_status == 0, 'resolve the native test executable path')
        executable = trim(current_dir)//'/'//trim(executable)
    end if
    call make_tmpfile('fo-view-probe', probe_log)
    packed = ''
    n_args = 0
    call argv_push(packed, n_args, trim(executable))
    call argv_push(packed, n_args, '--execution-view-probe')
    call process_run_argv_logged(trim(first%cwd), packed, n_args, trim(probe_log), &
        .false., 10, probe_exit)
    call check(probe_exit == 0, &
        'native test reads the declared fixture and writes relative output in its view')
    call delete_tmpfile(trim(probe_log))
    call read_text_file(trim(first%cwd)//'/relative-output.txt', text)
    call check(index(text, 'written relative') > 0, &
        'relative output is created inside the private working directory')

    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-case-2', 'test_case', inventory, .true., .true., second, ierr, message)
    call check(ierr == 0 .and. second%cwd /= first%cwd, &
        'concurrent cases receive distinct working directories')
    second_output = trim(second%cwd)//'/relative-output.txt'
    call fs_write_text(trim(second_output), 'case two')
    inquire(file=trim(first%cwd)//'/relative-output.txt', exist=exists)
    call check(.not. exists, 'relative outputs stay isolated between cases')

    call execution_view_release(first, .true., release_status, message)
    inquire(file=trim(first_fixture), exist=exists)
    call check(exists, 'retained failure view keeps its fixture evidence')
    call execution_view_release(second, .false., release_status, message)
    inquire(file=trim(second_output), exist=exists)
    call check(.not. exists, 'owned inactive view cleanup removes its scratch')

    escaping = inventory
    do i = 1, escaping%entry_count
        if (trim(escaping%entries(i)%role) /= 'test-fixture') cycle
        escaping%entries(i)%relative_path = '../escape.txt'
        exit
    end do
    escape_path = trim(root)//'/escape.txt'
    call execution_view_create(trim(root)//'/views', repeat('a', HASH_LEN), &
        'session-escape', 'test_case', escaping, .true., .true., rejected, ierr, &
        message)
    call check(ierr /= 0 .and. index(message, 'escapes') > 0, &
        'escaping declared paths receive an explicit rejection')
    inquire(file=trim(escape_path), exist=exists)
    call check(.not. exists, 'path rejection does not write outside the view')
    call fs_remove_tree(trim(root))
    print '(a)', 'Gremlin writable execution view behavioral oracle: PASS'

contains

    subroutine runtime_probe()
        character(len=256) :: line
        integer :: unit, status

        open (newunit=unit, file='fixtures/input.txt', status='old', &
            action='read', iostat=status)
        if (status /= 0) error stop 2
        read (unit, '(a)', iostat=status) line
        close (unit)
        if (status /= 0) error stop 2
        if (trim(line) /= 'private mutation') error stop 2
        open (newunit=unit, file='relative-output.txt', status='new', &
            action='write', iostat=status)
        if (status /= 0) error stop 2
        write (unit, '(a)', iostat=status) 'written relative'
        close (unit)
        if (status /= 0) error stop 2
    end subroutine runtime_probe

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) return
        print '(a)', 'FAIL: '//description
        error stop 1
    end subroutine check
end program test_gremlin_execution_view
