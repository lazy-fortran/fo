program test_gremlin_state_portability
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_process, only: process_getpid
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_test_harness, only: run_list, argv, write_text, string_list_t, list_add, add_words
    implicit none

    integer :: checks, failures, command_status, exit_status, io_status, unit
    character(len=2048) :: work_dir, os_file, actual_log, old_log, fixed_log, tmp_root
    character(len=32) :: platform
    character(:), allocatable :: uname_text
    logical :: is_darwin

    checks = 0
    failures = 0
    call get_environment_variable('TMPDIR', tmp_root, status=io_status)
    if (io_status /= 0 .or. len_trim(tmp_root) == 0) tmp_root = '/var/tmp'
    write (work_dir, '(a,a,i0)') trim(tmp_root), &
        '/fo_gremlin_state_portability_', process_getpid()
    os_file = trim(work_dir)//'/os.txt'
    actual_log = trim(work_dir)//'/provider.log'
    old_log = trim(work_dir)//'/old-macros.log'
    fixed_log = trim(work_dir)//'/fixed-macros.log'

    call fs_make_dir(trim(work_dir), exit_status)
    command_status = 0
    call assert(command_status == 0 .and. exit_status == 0, &
        'temporary oracle directory is created')
    if (command_status /= 0 .or. exit_status /= 0) stop 1

    call run_list(argv('uname', '-s'), exit_status, uname_text)
    command_status = 0
    call assert(exit_status == 0, 'operating system is identified')
    platform = ''
    if (exit_status == 0) platform = uname_text(:max(0, index(uname_text, new_line('a')) - 1))
    is_darwin = trim(platform) == 'Darwin'

    call compile_c(' -Wall -Wextra -fsyntax-only', &
        'src/proc/fo_gremlin_state.c', actual_log, command_status, exit_status)
    call assert(command_status == 0, 'C compiler runs on the provider source')
    if (command_status == 0) then
        call assert(exit_status == 0, &
            'current provider compiles with source-selected declarations')
        if (.not. is_darwin) then
            call assert(file_is_empty(actual_log), &
                'Linux provider syntax check emits no C warnings')
        end if
    end if

    call compile_c(' -std=c11 -Wall -Wextra -Werror -fsyntax-only -x c', &
        'test/fixtures/gremlin_state_feature_macros.c.inc', fixed_log, &
        command_status, exit_status)
    call assert(command_status == 0, 'C compiler runs on the fixed oracle')
    if (command_status == 0) then
        call assert(exit_status == 0, &
            'Darwin and POSIX declarations compile with selected macros')
    end if

    call compile_c(' -std=c11 -Wall -Wextra -Werror -fsyntax-only -x c '// &
        '-DFO_TEST_OLD_FEATURE_MACROS', &
        'test/fixtures/gremlin_state_feature_macros.c.inc', old_log, &
        command_status, exit_status)
    call assert(command_status == 0, 'C compiler runs on the old-macro oracle')
    if (command_status == 0) then
        if (is_darwin) then
            call assert(exit_status /= 0, &
                'old POSIX/XOPEN macros fail on the Darwin declarations')
            if (exit_status /= 0) then
                call assert(contains_darwin_declaration_error(old_log), &
                    'old-macro failure names a hidden Darwin declaration')
            end if
        else
            call assert(exit_status == 0, &
                'old POSIX/XOPEN macros remain valid on Linux')
        end if
    end if

    write (output_unit, '(a,a,a,i0,a,i0,a)') &
        'gremlin state portability (', trim(platform), '): ', &
        checks - failures, ' checks, ', failures, ' failed'
    if (failures == 0) then
        call fs_remove_tree(trim(work_dir))
    end if
    if (failures > 0) stop 1

contains

    subroutine assert(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        checks = checks + 1
        if (.not. condition) then
            failures = failures + 1
            write (output_unit, '(a,a)') 'FAIL: ', description
        end if
    end subroutine assert

    subroutine compile_c(flags, source, log_file, command_result, exit_result)
        character(len=*), intent(in) :: flags, source, log_file
        integer, intent(out) :: command_result, exit_result

        type(string_list_t) :: compile_command
        character(:), allocatable :: compiler_output

        call list_add(compile_command, 'gcc')
        call add_words(compile_command, flags)
        call list_add(compile_command, trim(source))
        command_result = 0
        call run_list(compile_command, exit_result, compiler_output)
        call write_text(trim(log_file), compiler_output)
    end subroutine compile_c

    logical function contains_darwin_declaration_error(log_file)
        character(len=*), intent(in) :: log_file

        integer :: read_status, log_unit
        character(len=2048) :: line

        contains_darwin_declaration_error = .false.
        open (newunit=log_unit, file=trim(log_file), status='old', &
            action='read', iostat=read_status)
        if (read_status /= 0) return
        do
            read (log_unit, '(a)', iostat=read_status) line
            if (read_status /= 0) exit
            if (index(line, 'u_int') > 0 .or. index(line, 'u_short') > 0 .or. &
                index(line, 'u_long') > 0 .or. index(line, 'rusage_info_t') > 0 .or. &
                index(line, 'O_NOFOLLOW') > 0 .or. index(line, 'flock') > 0 .or. &
                index(line, 'LOCK_EX') > 0) then
                contains_darwin_declaration_error = .true.
                exit
            end if
        end do
        close (log_unit)
    end function contains_darwin_declaration_error

    logical function file_is_empty(file_name)
        character(len=*), intent(in) :: file_name

        integer :: file_size, file_status

        file_is_empty = .false.
        inquire (file=trim(file_name), size=file_size, iostat=file_status)
        if (file_status == 0) file_is_empty = file_size == 0
    end function file_is_empty

end program test_gremlin_state_portability
