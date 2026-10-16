module fo_fake_tool
    !! One compiled fake command for every test that needs a stand-in tool.
    !!
    !! A fake tool is a text file whose first line makes the kernel run the test
    !! executable itself as its interpreter (no shell):
    !!
    !!     #!/path/to/test --fo-fake-tool
    !!
    !! The remaining lines are rules, executed top to bottom on every call; a
    !! test publishes one with install_fake_tool and calls fake_tool_dispatch as
    !! the first statement of its main program. Editing the rules edits the tool
    !! file, so tool-identity tests keep seeing a changed executable.
    !!
    !! A rule is optional conditions followed by one verb. Words are separated
    !! by blanks, double quotes group a word, a literal backslash-n is a newline,
    !! backslash-quote is a quote and backslash-dollar a dollar sign that is kept
    !! for the next stage (see install). Words expand $NAME, ${NAME:-default},
    !! $ARGS (all arguments, space-joined), $ARG1..$ARG9, $LAST, $OUT (the word after
    !! -o), $PID, $PGID, $PPID, $? (status of the last run) and variables made with set;
    !! a lone $@ is every argument.
    !!
    !! Conditions: `if A = B`, `if A != B`, `if A ~ B` (contains), `if A !~ B`,
    !! `if A glob P1|P2` (shell-style patterns), `if A !glob P`, `ifexists FILE`,
    !! `ifabsent FILE`. Verbs: print, eprint, append FILE TEXT, write FILE TEXT,
    !! touch FILE, remove FILE, copy SRC DST, collect GLOBS FILE [PREFIX] (append
    !! each matching argument), sleep MS, wait-file FILE MAX_MS, exit N, exec PATH
    !! ARGS (replace this process), run PATH ARGS (run it, keep going, set $?),
    !! set NAME VALUE, install FILE RULES (publish another fake tool), spawn
    !! PIDFILE PATH ARGS (background child in this process group), wait,
    !! ignore-term, forever, spin, heartbeat FILE MS (append a line every MS).
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_loc, c_null_ptr, &
        c_null_char, c_intptr_t
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_fs, only: fs_remove_file, fs_copy_exec, fs_sleep_ms, fs_make_dir
    use fo_process, only: process_getpid
    use fo_test_harness, only: string_list_t, list_add, run_list, resolve_test_executable
    implicit none
    private

    public :: fake_tool_dispatch, install_fake_tool

    character(len=*), parameter :: FAKE_TOOL_FLAG = '--fo-fake-tool'
    character(len=1), parameter :: LF = achar(10)

    type :: word_t
        character(:), allocatable :: text
    end type word_t

    type :: state_t
        type(word_t), allocatable :: args(:)
        type(word_t) :: names(16), values(16)
        integer :: n_vars = 0
        integer :: last_status = 0
    end type state_t

    interface
        function c_fork() bind(C, name='fork') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_fork

        function c_execvp(file, argv) bind(C, name='execvp') result(rc)
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: file(*)
            type(c_ptr), intent(in) :: argv(*)
            integer(c_int) :: rc
        end function c_execvp

        function c_waitpid(pid, status, options) bind(C, name='waitpid') result(rc)
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
            integer(c_int) :: rc
        end function c_waitpid

        function c_getpgrp() bind(C, name='getpgrp') result(group)
            import :: c_int
            integer(c_int) :: group
        end function c_getpgrp

        function c_getppid() bind(C, name='getppid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getppid

        function c_chmod(path, mode) bind(C, name='chmod') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
            integer(c_int) :: rc
        end function c_chmod

        function c_fnmatch(pattern, string, flags) bind(C, name='fnmatch') result(rc)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: pattern(*), string(*)
            integer(c_int), value :: flags
            integer(c_int) :: rc
        end function c_fnmatch

        subroutine c_exit_now(code) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: code
        end subroutine c_exit_now

        function c_signal(signum, handler) bind(C, name='signal') result(previous)
            import :: c_int, c_ptr
            integer(c_int), value :: signum
            type(c_ptr), value :: handler
            type(c_ptr) :: previous
        end function c_signal
    end interface

contains

    subroutine install_fake_tool(raw_path, rules)
        !! Publish an executable fake tool at path (replacing any file there).
        character(len=*), intent(in) :: raw_path, rules
        character(:), allocatable :: self, text, path
        integer :: unit, ios, slash
        logical :: ok

        path = trim(raw_path)
        call resolve_test_executable(self, ok)
        if (.not. ok) error stop 'fake tool cannot resolve its test executable'
        text = '#!'//self//' '//FAKE_TOOL_FLAG//LF//rules
        if (rules(len(rules):) /= LF) text = '#!'//self//' '//FAKE_TOOL_FLAG//LF//rules//LF
        slash = index(path, '/', back=.true.)
        if (slash > 1) call fs_make_dir(path(:slash - 1))
        call fs_remove_file(path)
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write', iostat=ios)
        if (ios /= 0) error stop 'fake tool cannot write '//path
        write (unit) text
        close (unit)
        if (c_chmod(path//c_null_char, int(o'755', c_int)) /= 0) &
            error stop 'fake tool cannot chmod '//path
    end subroutine install_fake_tool

    subroutine fake_tool_dispatch()
        !! First statement of a test main: run as a fake tool when invoked as one.
        character(len=4096) :: argument
        type(state_t) :: state
        integer :: count, i, length

        count = command_argument_count()
        if (count < 2) return
        call get_command_argument(1, argument)
        if (trim(argument) /= FAKE_TOOL_FLAG) return
        allocate (state%args(count - 2))
        do i = 3, count
            call get_command_argument(i, length=length)
            allocate (character(len=length) :: state%args(i - 2)%text)
            call get_command_argument(i, state%args(i - 2)%text)
        end do
        call get_command_argument(2, argument)
        call run_rules(read_whole(trim(argument)), state)
        stop
    end subroutine fake_tool_dispatch

    subroutine run_rules(text, state)
        character(len=*), intent(in) :: text
        type(state_t), intent(inout) :: state
        integer :: start, finish

        start = index(text, LF) + 1
        do while (start <= len(text))
            finish = index(text(start:), LF)
            if (finish == 0) then
                finish = len(text) + 1
            else
                finish = start + finish - 1
            end if
            call run_rule(text(start:finish - 1), state)
            start = finish + 1
        end do
        call finish_process(0)
    end subroutine run_rules

    subroutine run_rule(line, state)
        character(len=*), intent(in) :: line
        type(state_t), intent(inout) :: state
        type(word_t), allocatable :: words(:), call_words(:)
        integer :: n, i

        call split_words(line, words, n)
        if (n == 0) return
        if (len(words(1)%text) > 0) then
            if (words(1)%text(1:1) == '#') return
        end if
        i = 1
        do while (i <= n)
            select case (words(i)%text)
            case ('if')
                if (i + 3 > n) error stop 'fake tool: incomplete condition'
                if (.not. holds(expand(words(i + 1)%text, state), words(i + 2)%text, &
                    expand(words(i + 3)%text, state))) return
                i = i + 4
            case ('ifexists')
                if (.not. file_present(expand(words(i + 1)%text, state))) return
                i = i + 2
            case ('ifabsent')
                if (file_present(expand(words(i + 1)%text, state))) return
                i = i + 2
            case default
                exit
            end select
        end do
        if (i > n) return
        call expand_words(words(i + 1:n), state, call_words)
        call run_verb(words(i)%text, call_words, state)
    end subroutine run_rule

    subroutine run_verb(verb, w, state)
        character(len=*), intent(in) :: verb
        type(word_t), intent(in) :: w(:)
        type(state_t), intent(inout) :: state
        type(string_list_t) :: command
        character(:), allocatable :: standard_output, standard_error
        integer :: i
        integer(c_int) :: rc, waited

        select case (verb)
        case ('print')
            write (output_unit, '(a)') arg(w, 1)
            flush (output_unit)
        case ('eprint')
            write (error_unit, '(a)') arg(w, 1)
            flush (error_unit)
        case ('append')
            call put_file(arg(w, 1), arg(w, 2), .true.)
        case ('write')
            call put_file(arg(w, 1), arg(w, 2), .false.)
        case ('touch')
            call put_file(arg(w, 1), '', .false.)
        case ('remove')
            call fs_remove_file(arg(w, 1))
        case ('copy')
            if (fs_copy_exec(arg(w, 1), arg(w, 2)) /= 0) call finish_process(70)
        case ('collect')
            do i = 1, size(state%args)
                if (matches_any(state%args(i)%text, arg(w, 1))) &
                    call put_file(arg(w, 2), arg(w, 3)//state%args(i)%text//LF, .true.)
            end do
        case ('sleep')
            call fs_sleep_ms(number(arg(w, 1)))
        case ('wait-file')
            do i = 1, max(1, number(arg(w, 2)) / 10)
                if (file_present(arg(w, 1))) exit
                call fs_sleep_ms(10)
            end do
        case ('exit')
            call finish_process(number(arg(w, 1)))
        case ('exec')
            call exec_process(w)
        case ('run')
            do i = 1, size(w)
                call list_add(command, w(i)%text)
            end do
            call run_list(command, state%last_status, standard_output, standard_error, &
                timeout_ms=-1)
            if (len(standard_output) > 0) write (output_unit, '(a)', advance='no') &
                standard_output
            flush (output_unit)
            if (len(standard_error) > 0) write (error_unit, '(a)', advance='no') &
                standard_error
            flush (error_unit)
            if (state%last_status < 0) state%last_status = 127
        case ('set')
            call set_variable(state, arg(w, 1), arg(w, 2))
        case ('install')
            call install_fake_tool(arg(w, 1), arg(w, 2))
        case ('spawn')
            call spawn_child(arg(w, 1), w(2:))
        case ('wait')
            do
                waited = c_waitpid(-1_c_int, rc, 0_c_int)
                if (waited < 0) exit
            end do
        case ('ignore-term')
            block
                type(c_ptr) :: ignore, previous

                ! SIG_IGN is the pointer value 1.
                ignore = transfer(1_c_intptr_t, ignore)
                previous = c_signal(15_c_int, ignore)
            end block
        case ('forever')
            do
                call fs_sleep_ms(100)
            end do
        case ('spin')
            do
            end do
        case ('heartbeat')
            do
                call put_file(arg(w, 1), 'tick'//LF, .true.)
                call fs_sleep_ms(number(arg(w, 2)))
            end do
        case default
            write (error_unit, '(a)') 'fake tool: unknown verb '//verb
            call finish_process(64)
        end select
    end subroutine run_verb

    subroutine set_variable(state, name, value)
        type(state_t), intent(inout) :: state
        character(len=*), intent(in) :: name, value
        integer :: i

        do i = 1, state%n_vars
            if (state%names(i)%text == name) then
                state%values(i)%text = value
                return
            end if
        end do
        if (state%n_vars == size(state%names)) call finish_process(65)
        state%n_vars = state%n_vars + 1
        state%names(state%n_vars)%text = name
        state%values(state%n_vars)%text = value
    end subroutine set_variable

    subroutine exec_process(w)
        type(word_t), intent(in) :: w(:)
        type(c_ptr), allocatable :: pointers(:)
        character(kind=c_char), allocatable, target :: storage(:, :)
        integer(c_int) :: rc

        call make_argv(w, pointers, storage)
        rc = c_execvp(storage(:, 1), pointers)
        write (error_unit, '(a)') 'fake tool: cannot execute '//w(1)%text
        call finish_process(127)
    end subroutine exec_process

    subroutine spawn_child(pid_file, w)
        character(len=*), intent(in) :: pid_file
        type(word_t), intent(in) :: w(:)
        type(c_ptr), allocatable :: pointers(:)
        character(kind=c_char), allocatable, target :: storage(:, :)
        integer(c_int) :: child, rc
        character(len=32) :: digits

        call make_argv(w, pointers, storage)
        child = c_fork()
        if (child == 0) then
            rc = c_execvp(storage(:, 1), pointers)
            call c_exit_now(127_c_int)
        end if
        write (digits, '(i0)') child
        call put_file(pid_file, trim(digits)//LF, .false.)
    end subroutine spawn_child

    subroutine make_argv(w, pointers, storage)
        type(word_t), intent(in) :: w(:)
        type(c_ptr), allocatable, intent(out) :: pointers(:)
        character(kind=c_char), allocatable, target, intent(out) :: storage(:, :)
        integer :: i, j, width

        width = 1
        do i = 1, size(w)
            width = max(width, len(w(i)%text) + 1)
        end do
        allocate (storage(width, size(w)), pointers(size(w) + 1))
        storage = c_null_char
        do i = 1, size(w)
            do j = 1, len(w(i)%text)
                storage(j, i) = w(i)%text(j:j)
            end do
            pointers(i) = c_loc(storage(1, i))
        end do
        pointers(size(w) + 1) = c_null_ptr
    end subroutine make_argv

    subroutine finish_process(code)
        integer, intent(in) :: code

        flush (output_unit)
        flush (error_unit)
        call c_exit_now(int(code, c_int))
    end subroutine finish_process

    logical function holds(left, op, right) result(ok)
        character(len=*), intent(in) :: left, op, right

        select case (op)
        case ('=')
            ok = left == right
        case ('!=')
            ok = left /= right
        case ('~')
            ok = index(left, right) > 0
        case ('!~')
            ok = index(left, right) == 0
        case ('glob')
            ok = matches_any(left, right)
        case ('!glob')
            ok = .not. matches_any(left, right)
        case default
            error stop 'fake tool: unknown condition operator '//op
        end select
    end function holds

    logical function matches_any(text, patterns) result(ok)
        character(len=*), intent(in) :: text, patterns
        integer :: start, bar

        ok = .false.
        start = 1
        do
            bar = index(patterns(start:), '|')
            if (bar == 0) bar = len(patterns) - start + 2
            if (c_fnmatch(patterns(start:start + bar - 2)//c_null_char, &
                text//c_null_char, 0_c_int) == 0) then
                ok = .true.
                return
            end if
            start = start + bar
            if (start > len(patterns)) return
        end do
    end function matches_any

    logical function file_present(path) result(ok)
        character(len=*), intent(in) :: path

        inquire (file=path, exist=ok)
    end function file_present

    function arg(w, index) result(text)
        type(word_t), intent(in) :: w(:)
        integer, intent(in) :: index
        character(:), allocatable :: text

        if (index <= size(w)) then
            text = w(index)%text
        else
            text = ''
        end if
    end function arg

    integer function number(text) result(value)
        character(len=*), intent(in) :: text
        integer :: ios

        read (text, *, iostat=ios) value
        if (ios /= 0) value = 0
    end function number

    subroutine put_file(path, text, append)
        character(len=*), intent(in) :: path, text
        logical, intent(in) :: append
        integer :: unit, ios

        if (append) then
            open (newunit=unit, file=path, access='stream', form='unformatted', &
                status='unknown', position='append', action='write', iostat=ios)
        else
            open (newunit=unit, file=path, access='stream', form='unformatted', &
                status='replace', action='write', iostat=ios)
        end if
        if (ios /= 0) return
        if (len(text) > 0) write (unit) text
        close (unit)
    end subroutine put_file

    function read_whole(path) result(text)
        character(len=*), intent(in) :: path
        character(:), allocatable :: text
        integer :: unit, ios, bytes

        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) then
            text = ''
            return
        end if
        inquire (unit=unit, size=bytes)
        allocate (character(len=max(0, bytes)) :: text)
        if (bytes > 0) read (unit, iostat=ios) text
        close (unit)
    end function read_whole

    subroutine split_words(line, words, n)
        character(len=*), intent(in) :: line
        type(word_t), allocatable, intent(out) :: words(:)
        integer, intent(out) :: n
        character(:), allocatable :: current
        logical :: quoted, active
        integer :: i

        allocate (words(len(line) / 2 + 1))
        n = 0
        current = ''
        quoted = .false.
        active = .false.
        i = 1
        do while (i <= len(line))
            if (quoted) then
                if (line(i:i + min(1, len(line) - i)) == '\"') then
                    current = current//'"'
                    i = i + 1
                else if (line(i:i) == '"') then
                    quoted = .false.
                else if (line(i:i + min(1, len(line) - i)) == '\n') then
                    current = current//LF
                    i = i + 1
                else
                    current = current//line(i:i)
                end if
            else if (line(i:i) == '"') then
                quoted = .true.
                active = .true.
            else if (line(i:i) == ' ' .or. line(i:i) == achar(9)) then
                if (active) call push_word()
            else if (line(i:i + min(1, len(line) - i)) == '\n') then
                current = current//LF
                active = .true.
                i = i + 1
            else
                current = current//line(i:i)
                active = .true.
            end if
            i = i + 1
        end do
        if (active) call push_word()

    contains

        subroutine push_word()
            n = n + 1
            words(n)%text = current
            current = ''
            active = .false.
        end subroutine push_word

    end subroutine split_words

    subroutine expand_words(raw, state, expanded)
        type(word_t), intent(in) :: raw(:)
        type(state_t), intent(in) :: state
        type(word_t), allocatable, intent(out) :: expanded(:)
        type(word_t), allocatable :: work(:)
        integer :: i, n, k

        allocate (work(size(raw) + size(state%args)))
        n = 0
        do i = 1, size(raw)
            if (raw(i)%text == '$@') then
                do k = 1, size(state%args)
                    n = n + 1
                    work(n)%text = state%args(k)%text
                end do
            else
                n = n + 1
                work(n)%text = expand(raw(i)%text, state)
            end if
        end do
        allocate (expanded(n))
        do i = 1, n
            expanded(i)%text = work(i)%text
        end do
    end subroutine expand_words

    function expand(text, state) result(out)
        character(len=*), intent(in) :: text
        type(state_t), intent(in) :: state
        character(:), allocatable :: out
        integer :: i, j, close_brace, dash

        out = ''
        i = 1
        do while (i <= len(text))
            if (text(i:i + min(1, len(text) - i)) == '\$') then
                out = out//'$'
                i = i + 2
            else if (text(i:i) /= '$' .or. i == len(text)) then
                out = out//text(i:i)
                i = i + 1
            else if (text(i + 1:i + 1) == '{') then
                close_brace = index(text(i:), '}')
                if (close_brace == 0) error stop 'fake tool: unterminated ${'
                close_brace = i + close_brace - 1
                dash = index(text(i + 2:close_brace - 1), ':-')
                if (dash == 0) then
                    out = out//lookup(text(i + 2:close_brace - 1), state, '')
                else
                    dash = i + 1 + dash
                    out = out//lookup(text(i + 2:dash - 1), state, &
                        text(dash + 2:close_brace - 1))
                end if
                i = close_brace + 1
            else if (text(i + 1:i + 1) == '?') then
                out = out//integer_text(state%last_status)
                i = i + 2
            else
                j = i + 1
                do while (j <= len(text))
                    if (.not. (is_name_char(text(j:j)))) exit
                    j = j + 1
                end do
                if (j == i + 1) then
                    out = out//'$'
                    i = i + 1
                else
                    out = out//lookup(text(i + 1:j - 1), state, '')
                    i = j
                end if
            end if
        end do
    end function expand

    logical function is_name_char(c) result(ok)
        character(len=1), intent(in) :: c

        ok = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z') .or. &
            (c >= '0' .and. c <= '9') .or. c == '_'
    end function is_name_char

    function lookup(name, state, default) result(value)
        character(len=*), intent(in) :: name, default
        type(state_t), intent(in) :: state
        character(:), allocatable :: value
        integer :: i, length, status

        do i = 1, state%n_vars
            if (state%names(i)%text == name) then
                value = state%values(i)%text
                return
            end if
        end do
        select case (name)
        case ('ARGS')
            value = ''
            do i = 1, size(state%args)
                if (i > 1) value = value//' '
                value = value//state%args(i)%text
            end do
            return
        case ('OUT')
            value = ''
            do i = 1, size(state%args) - 1
                if (state%args(i)%text == '-o') value = state%args(i + 1)%text
            end do
            return
        case ('PID')
            value = integer_text(process_getpid())
            return
        case ('LAST')
            value = ''
            if (size(state%args) > 0) value = state%args(size(state%args))%text
            return
        case ('PGID')
            value = integer_text(int(c_getpgrp()))
            return
        case ('PPID')
            value = integer_text(int(c_getppid()))
            return
        case ('ARG1', 'ARG2', 'ARG3', 'ARG4', 'ARG5', 'ARG6', 'ARG7', 'ARG8', 'ARG9')
            i = iachar(name(4:4)) - iachar('0')
            value = ''
            if (i <= size(state%args)) value = state%args(i)%text
            return
        end select
        call get_environment_variable(name, length=length, status=status)
        if (status /= 0 .or. length == 0) then
            value = default
            return
        end if
        allocate (character(len=length) :: value)
        call get_environment_variable(name, value, status=status)
        if (status /= 0) value = default
    end function lookup

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: digits

        write (digits, '(i0)') value
        text = trim(digits)
    end function integer_text

end module fo_fake_tool
