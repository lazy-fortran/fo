module fo_process
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private
    public :: process_detect_nproc
    public :: process_fpm_build, process_fpm_test_list, process_fpm_test_all
    public :: process_fpm_test_names, process_cmake_build, process_ctest
    public :: process_scan_sources
    public :: process_start_fo_check, process_poll_pid, process_cancel_pid
    public :: process_run_logged
    public :: process_stderr_is_tty, process_write_stderr
    public :: process_getpid
    public :: process_run_argv_logged, argv_push, argv_push_split
    public :: argv_push_split_nl
    integer, parameter :: C_PATH_LEN = 4096
    integer, parameter :: C_ARG_LEN = 4096

    interface
        subroutine fo_c_detect_nproc(nproc) bind(C, name='fo_c_detect_nproc')
            import :: c_int
            integer(c_int), intent(out) :: nproc
        end subroutine fo_c_detect_nproc

        function fo_c_isatty(fd) bind(C, name='fo_c_isatty') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: r
        end function fo_c_isatty

        subroutine fo_c_getpid(pid_out) bind(C, name='fo_c_getpid')
            import :: c_int
            integer(c_int), intent(out) :: pid_out
        end subroutine fo_c_getpid

        subroutine fo_c_write_stderr(buf, n) bind(C, name='fo_c_write_stderr')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: buf(*)
            integer(c_int), value :: n
        end subroutine fo_c_write_stderr

        subroutine fo_c_scan_sources(root, output_file, exitcode) &
                bind(C, name='fo_c_scan_sources')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), output_file(*)
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_scan_sources

        subroutine fo_c_fpm_build(project_dir, flags, jobs, log_file, &
                exitcode) bind(C, name='fo_c_fpm_build')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), flags(*)
            character(kind=c_char), intent(in) :: log_file(*)
            integer(c_int), value :: jobs
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_fpm_build

        subroutine fo_c_fpm_test_list(project_dir, log_file, exitcode) &
                bind(C, name='fo_c_fpm_test_list')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), log_file(*)
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_fpm_test_list

        subroutine fo_c_fpm_test_all(project_dir, jobs, log_file, exitcode) &
                bind(C, name='fo_c_fpm_test_all')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), log_file(*)
            integer(c_int), value :: jobs
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_fpm_test_all

        subroutine fo_c_fpm_test_names(project_dir, names, jobs, log_file, &
                exitcode) bind(C, name='fo_c_fpm_test_names')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), names(*)
            character(kind=c_char), intent(in) :: log_file(*)
            integer(c_int), value :: jobs
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_fpm_test_names

        subroutine fo_c_cmake_build(project_dir, flags, jobs, log_file, &
                exitcode) bind(C, name='fo_c_cmake_build')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), flags(*)
            character(kind=c_char), intent(in) :: log_file(*)
            integer(c_int), value :: jobs
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_cmake_build

        subroutine fo_c_ctest(project_dir, jobs, regex, include_slow, &
                log_file, exitcode) bind(C, name='fo_c_ctest')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), regex(*)
            character(kind=c_char), intent(in) :: log_file(*)
            integer(c_int), value :: jobs, include_slow
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_ctest

        subroutine fo_c_start_fo_check(project_dir, mode, output_file, pid, &
                exitcode) bind(C, name='fo_c_start_fo_check')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: project_dir(*), mode(*)
            character(kind=c_char), intent(in) :: output_file(*)
            integer(c_int), intent(out) :: pid, exitcode
        end subroutine fo_c_start_fo_check

        subroutine fo_c_run_logged(cwd, exe_path, log_file, append, timeout_s, &
                env_extra, exitcode) bind(C, name='fo_c_run_logged')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: cwd(*), exe_path(*), log_file(*)
            character(kind=c_char), intent(in) :: env_extra(*)
            integer(c_int), value :: append, timeout_s
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_run_logged

        subroutine fo_c_run_argv_logged(cwd, args, args_len, n_args, log_file, &
                append, timeout_s, env_extra, exitcode) &
                bind(C, name='fo_c_run_argv_logged')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: cwd(*), args(*), log_file(*)
            character(kind=c_char), intent(in) :: env_extra(*)
            integer(c_int), value :: args_len, n_args, append, timeout_s
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_run_argv_logged

        subroutine fo_c_poll_pid(pid, done, exitcode) &
                bind(C, name='fo_c_poll_pid')
            import :: c_int
            integer(c_int), value :: pid
            integer(c_int), intent(out) :: done, exitcode
        end subroutine fo_c_poll_pid

        subroutine fo_c_cancel_pid(pid, exitcode) bind(C, name='fo_c_cancel_pid')
            import :: c_int
            integer(c_int), value :: pid
            integer(c_int), intent(out) :: exitcode
        end subroutine fo_c_cancel_pid

    end interface

contains

    integer function process_getpid()
        !! Current process id. Used to make per-process unique paths so parallel
        !! test processes do not collide on shared /tmp names.
        integer(c_int) :: pid
        call fo_c_getpid(pid)
        process_getpid = int(pid)
    end function process_getpid

    logical function process_stderr_is_tty()
        !! True when stderr (fd 2) is a terminal, so progress can use an
        !! animated carriage-return bar instead of plain lines.
        process_stderr_is_tty = fo_c_isatty(2_c_int) /= 0
    end function process_stderr_is_tty

    subroutine process_write_stderr(s)
        !! Raw unbuffered write of s to stderr (no newline added, no fork).
        character(len=*), intent(in) :: s
        if (len(s) > 0) call fo_c_write_stderr(s, int(len(s), c_int))
    end subroutine process_write_stderr

    subroutine process_run_argv_logged(cwd, packed, n_args, log_file, append, &
            timeout_s, exitcode)
        !! Run a command given as an argv vector with no shell. packed holds
        !! n_args NUL-terminated tokens back-to-back (built by argv_begin/
        !! argv_push). This is the quote-proof, async-signal-safe path for
        !! compile/link inside the OpenMP build loop: fork+execve, no /bin/sh.
        character(len=*), intent(in) :: cwd, packed, log_file
        integer, intent(in) :: n_args
        logical, intent(in) :: append
        integer, intent(in) :: timeout_s
        integer, intent(out) :: exitcode
        integer(c_int) :: ec, ap

        ap = 0
        if (append) ap = 1
        call fo_c_run_argv_logged(trim(cwd)//c_null_char, packed, &
                                  int(len(packed), c_int), int(n_args, c_int), &
                                  trim(log_file)//c_null_char, ap, &
                                  int(timeout_s, c_int), c_null_char, ec)
        exitcode = int(ec)
    end subroutine process_run_argv_logged

    subroutine argv_push(packed, n_args, token)
        !! Append one argv token (a whole word) to the packed NUL-separated
        !! buffer and bump the count. Empty tokens are skipped.
        character(len=:), allocatable, intent(inout) :: packed
        integer, intent(inout) :: n_args
        character(len=*), intent(in) :: token

        if (len_trim(token) == 0) return
        if (.not. allocated(packed)) packed = ''
        packed = packed//trim(token)//c_null_char
        n_args = n_args + 1
    end subroutine argv_push

    subroutine argv_push_split(packed, n_args, words)
        !! Split words on spaces and append each non-empty field as its own
        !! argv token. Use for flag strings that hold several flags at once.
        character(len=:), allocatable, intent(inout) :: packed
        integer, intent(inout) :: n_args
        character(len=*), intent(in) :: words
        integer :: i, start, n

        n = len(words)
        start = 0
        do i = 1, n
            if (words(i:i) == ' ') then
                if (start > 0) then
                    call argv_push(packed, n_args, words(start:i - 1))
                    start = 0
                end if
            else if (start == 0) then
                start = i
            end if
        end do
        if (start > 0) call argv_push(packed, n_args, words(start:n))
    end subroutine argv_push_split

    subroutine argv_push_split_nl(packed, n_args, words)
        !! Split words on newlines and append each non-empty line as one argv
        !! token, internal spaces preserved. Use for paths that may contain
        !! spaces (compiler -I/-J dirs, resolved library paths).
        character(len=:), allocatable, intent(inout) :: packed
        integer, intent(inout) :: n_args
        character(len=*), intent(in) :: words
        integer :: i, start, n

        n = len(words)
        start = 0
        do i = 1, n
            if (words(i:i) == char(10)) then
                if (start > 0) then
                    call argv_push(packed, n_args, words(start:i - 1))
                    start = 0
                end if
            else if (start == 0) then
                start = i
            end if
        end do
        if (start > 0) call argv_push(packed, n_args, words(start:n))
    end subroutine argv_push_split_nl

    function process_detect_nproc() result(nproc)
        integer :: nproc

        integer(c_int) :: c_nproc

        call fo_c_detect_nproc(c_nproc)
        nproc = int(c_nproc)
        if (nproc < 1) nproc = 1
    end function process_detect_nproc

    subroutine process_scan_sources(root, output_file, exitcode)
        character(len=*), intent(in) :: root, output_file
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_root(C_PATH_LEN), c_output(C_PATH_LEN)
        integer(c_int) :: c_exit

        call to_c_string(root, c_root)
        call to_c_string(output_file, c_output)
        call fo_c_scan_sources(c_root, c_output, c_exit)
        exitcode = int(c_exit)
    end subroutine process_scan_sources

    subroutine process_fpm_build(project_dir, flags, jobs, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, flags, log_file
        integer, intent(in) :: jobs
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_flags(C_ARG_LEN)
        character(kind=c_char) :: c_log(C_PATH_LEN)
        integer(c_int) :: c_exit

        call to_c_string(project_dir, c_project)
        call to_c_string(flags, c_flags)
        call to_c_string(log_file, c_log)
        call fo_c_fpm_build(c_project, c_flags, int(jobs, c_int), c_log, c_exit)
        exitcode = int(c_exit)
    end subroutine process_fpm_build

    subroutine process_fpm_test_list(project_dir, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_log(C_PATH_LEN)
        integer(c_int) :: c_exit

        call to_c_string(project_dir, c_project)
        call to_c_string(log_file, c_log)
        call fo_c_fpm_test_list(c_project, c_log, c_exit)
        exitcode = int(c_exit)
    end subroutine process_fpm_test_list

    subroutine process_fpm_test_all(project_dir, jobs, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, log_file
        integer, intent(in) :: jobs
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_log(C_PATH_LEN)
        integer(c_int) :: c_exit

        call to_c_string(project_dir, c_project)
        call to_c_string(log_file, c_log)
        call fo_c_fpm_test_all(c_project, int(jobs, c_int), c_log, c_exit)
        exitcode = int(c_exit)
    end subroutine process_fpm_test_all

    subroutine process_fpm_test_names(project_dir, names, n_names, jobs, &
            log_file, exitcode)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names, jobs
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_names(C_ARG_LEN)
        character(kind=c_char) :: c_log(C_PATH_LEN)
        integer(c_int) :: c_exit

        call to_c_string(project_dir, c_project)
        call names_to_c_string(names, n_names, c_names)
        call to_c_string(log_file, c_log)
        call fo_c_fpm_test_names(c_project, c_names, int(jobs, c_int), c_log, &
            c_exit)
        exitcode = int(c_exit)
    end subroutine process_fpm_test_names

    subroutine process_cmake_build(project_dir, flags, jobs, log_file, exitcode)
        character(len=*), intent(in) :: project_dir, flags, log_file
        integer, intent(in) :: jobs
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_flags(C_ARG_LEN)
        character(kind=c_char) :: c_log(C_PATH_LEN)
        integer(c_int) :: c_exit

        call to_c_string(project_dir, c_project)
        call to_c_string(flags, c_flags)
        call to_c_string(log_file, c_log)
        call fo_c_cmake_build(c_project, c_flags, int(jobs, c_int), c_log, c_exit)
        exitcode = int(c_exit)
    end subroutine process_cmake_build

    subroutine process_ctest(project_dir, jobs, regex, include_slow, log_file, &
            exitcode)
        character(len=*), intent(in) :: project_dir, regex, log_file
        integer, intent(in) :: jobs
        logical, intent(in) :: include_slow
        integer, intent(out) :: exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_regex(C_ARG_LEN)
        character(kind=c_char) :: c_log(C_PATH_LEN)
        integer(c_int) :: c_exit, c_slow

        call to_c_string(project_dir, c_project)
        call to_c_string(regex, c_regex)
        call to_c_string(log_file, c_log)
        c_slow = 0
        if (include_slow) c_slow = 1
        call fo_c_ctest(c_project, int(jobs, c_int), c_regex, c_slow, c_log, &
            c_exit)
        exitcode = int(c_exit)
    end subroutine process_ctest

    subroutine process_start_fo_check(project_dir, mode, output_file, pid, &
            exitcode)
        character(len=*), intent(in) :: project_dir, mode, output_file
        integer, intent(out) :: pid, exitcode

        character(kind=c_char) :: c_project(C_PATH_LEN), c_mode(C_ARG_LEN)
        character(kind=c_char) :: c_output(C_PATH_LEN)
        integer(c_int) :: c_pid, c_exit

        call to_c_string(project_dir, c_project)
        call to_c_string(mode, c_mode)
        call to_c_string(output_file, c_output)
        call fo_c_start_fo_check(c_project, c_mode, c_output, c_pid, c_exit)
        pid = int(c_pid)
        exitcode = int(c_exit)
    end subroutine process_start_fo_check

    subroutine process_run_logged(cwd, exe_path, log_file, append, timeout_s, &
            exitcode, cache_dir)
        character(len=*), intent(in) :: cwd, exe_path, log_file
        logical, intent(in) :: append
        integer, intent(in) :: timeout_s
        integer, intent(out) :: exitcode
        ! Optional: run the child with FO_CACHE_DIR set to this directory, so a
        ! test built in parallel uses its own cache and cannot corrupt a shared
        ! one. The KEY=VALUE env is applied via execve (async-signal-safe).
        character(len=*), intent(in), optional :: cache_dir

        character(kind=c_char) :: c_cwd(C_PATH_LEN), c_exe(C_PATH_LEN)
        character(kind=c_char) :: c_log(C_PATH_LEN)
        character(kind=c_char) :: c_env(C_PATH_LEN)
        integer(c_int) :: c_exit, c_append

        call to_c_string(cwd, c_cwd)
        call to_c_string(exe_path, c_exe)
        call to_c_string(log_file, c_log)
        if (present(cache_dir) .and. len_trim(cache_dir) > 0) then
            call to_c_string('FO_CACHE_DIR='//trim(cache_dir), c_env)
        else
            c_env(1) = c_null_char
        end if
        c_append = 0
        if (append) c_append = 1
        call fo_c_run_logged(c_cwd, c_exe, c_log, c_append, &
            int(timeout_s, c_int), c_env, c_exit)
        exitcode = int(c_exit)
    end subroutine process_run_logged

    subroutine process_poll_pid(pid, done, exitcode)
        integer, intent(in) :: pid
        logical, intent(out) :: done
        integer, intent(out) :: exitcode

        integer(c_int) :: c_done, c_exit

        call fo_c_poll_pid(int(pid, c_int), c_done, c_exit)
        done = c_done /= 0
        exitcode = int(c_exit)
    end subroutine process_poll_pid

    subroutine process_cancel_pid(pid, exitcode)
        integer, intent(in) :: pid
        integer, intent(out) :: exitcode

        integer(c_int) :: c_exit

        call fo_c_cancel_pid(int(pid, c_int), c_exit)
        exitcode = int(c_exit)
    end subroutine process_cancel_pid

    subroutine to_c_string(text, c_text)
        character(len=*), intent(in) :: text
        character(kind=c_char), intent(out) :: c_text(:)

        integer :: i, n

        c_text = c_null_char
        n = min(len_trim(text), size(c_text) - 1)
        do i = 1, n
            c_text(i) = char(iachar(text(i:i)), kind=c_char)
        end do
        c_text(n + 1) = c_null_char
    end subroutine to_c_string

    subroutine names_to_c_string(names, n_names, c_text)
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        character(kind=c_char), intent(out) :: c_text(:)

        integer :: i, j, n, name_len

        c_text = c_null_char
        n = 0
        do i = 1, n_names
            name_len = len_trim(names(i))
            do j = 1, name_len
                if (n + 2 > size(c_text)) exit
                n = n + 1
                c_text(n) = char(iachar(names(i) (j:j)), kind=c_char)
            end do
            if (n + 2 > size(c_text)) exit
            n = n + 1
            c_text(n) = char(10, kind=c_char)
        end do
        if (n + 1 <= size(c_text)) c_text(n + 1) = c_null_char
    end subroutine names_to_c_string

end module fo_process
