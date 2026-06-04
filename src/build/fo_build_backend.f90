module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: backend_t, detect_backend, detect_nproc
    public :: BACKEND_FPM, BACKEND_CMAKE, BACKEND_NONE

    integer, parameter :: BACKEND_NONE = 0
    integer, parameter :: BACKEND_FPM = 1
    integer, parameter :: BACKEND_CMAKE = 2
    integer, parameter :: MAX_TEST_TARGETS = 512

    type :: backend_t
        integer :: kind = BACKEND_NONE
        character(len=512) :: project_dir = '.'
    contains
        procedure :: build => backend_build
        procedure :: test => backend_test
        procedure :: test_names => backend_test_names
    end type backend_t

contains

    function detect_backend(dir) result(b)
        character(len=*), intent(in) :: dir
        type(backend_t) :: b
        logical :: has_fpm, has_cmake
        character(len=512) :: current, parent
        integer :: depth

        current = absolute_dir(dir)

        do depth = 1, 64
            b%project_dir = current

            inquire (file=trim(current)//'/fpm.toml', exist=has_fpm)
            inquire (file=trim(current)//'/CMakeLists.txt', exist=has_cmake)

            if (has_fpm) then
                b%kind = BACKEND_FPM
                return
            else if (has_cmake) then
                b%kind = BACKEND_CMAKE
                return
            end if

            call parent_dir(current, parent)
            if (trim(parent) == trim(current)) exit
            current = parent
        end do

        b%kind = BACKEND_NONE
    end function detect_backend

    function absolute_dir(dir) result(absdir)
        character(len=*), intent(in) :: dir
        character(len=512) :: absdir

        character(len=512) :: pwd

        if (len_trim(dir) == 0) then
            absdir = '.'
        else if (dir(1:1) == '/') then
            absdir = trim(dir)
        else
            call get_environment_variable('PWD', pwd)
            if (len_trim(pwd) > 0) then
                if (trim(dir) == '.') then
                    absdir = trim(pwd)
                else
                    absdir = trim(pwd)//'/'//trim(dir)
                end if
            else
                absdir = trim(dir)
            end if
        end if
    end function absolute_dir

    subroutine parent_dir(path, parent)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: parent

        character(len=512) :: clean
        integer :: n, last

        clean = trim(path)
        n = len_trim(clean)
        do while (n > 1 .and. clean(n:n) == '/')
            clean(n:n) = ' '
            n = n - 1
        end do

        if (trim(clean) == '/') then
            parent = '/'
            return
        end if

        last = index(trim(clean), '/', back=.true.)
        if (last <= 1) then
            parent = '/'
        else
            parent = clean(1:last - 1)
        end if
    end subroutine parent_dir

    function detect_nproc() result(np)
        integer :: np

        character(len=32) :: buf
        character(len=128) :: tmpfile, cmd
        integer :: u, iostat

        np = 1
        call make_tmpfile('fo_nproc', tmpfile)
        cmd = 'nproc > '//trim(tmpfile)//' 2>/dev/null'
        call execute_command_line(cmd, wait=.true.)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            read (u, '(a)', iostat=iostat) buf
            if (iostat == 0) read (buf, *, iostat=iostat) np
            close (u)
        end if
        call execute_command_line('rm -f '//trim(tmpfile), wait=.true.)
        if (np < 1) np = 1
    end function detect_nproc

    subroutine backend_build(self, exitcode, flags, log_file)
        class(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: log_file

        integer :: cmdstat, np
        character(len=2048) :: cmd
        character(len=8) :: np_str

        np = detect_nproc()
        write (np_str, '(i0)') np

        select case (self%kind)
        case (BACKEND_FPM)
            if (present(flags) .and. len_trim(flags) > 0) then
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && fpm build --flag "'//trim(flags)//'"'
            else
                cmd = 'cd '//trim(self%project_dir)//' && fpm build'
            end if
        case (BACKEND_CMAKE)
            if (present(flags) .and. len_trim(flags) > 0) then
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && cmake -S . -B build -G Ninja'// &
                      ' -DCMAKE_Fortran_FLAGS="'//trim(flags)//'"'// &
                      ' && cmake --build build -j '//trim(np_str)
            else
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && cmake -S . -B build -G Ninja'// &
                      ' && cmake --build build -j '//trim(np_str)
            end if
        case default
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end select

        call redirect_command(cmd, log_file)
        call execute_command_line(cmd, exitstat=exitcode, cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) exitcode = 1
    end subroutine backend_build

    subroutine backend_test(self, exitcode, include_slow, log_file)
        class(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file

        integer :: cmdstat, list_ierr, n_names
        character(len=2048) :: cmd
        character(len=128) :: names(MAX_TEST_TARGETS)
        logical :: has_tests, slow

        slow = .false.
        if (present(include_slow)) slow = include_slow

        select case (self%kind)
        case (BACKEND_FPM)
            if (slow) then
                cmd = 'cd '//trim(self%project_dir)//' && fpm test'
            else
                call fpm_list_tests(self%project_dir, names, n_names, &
                                    list_ierr, log_file)
                if (list_ierr /= 0) then
                    exitcode = 1
                    return
                end if
                call filter_slow_tests(names, n_names)
                if (n_names == 0) then
                    exitcode = 0
                    return
                end if
                call fpm_run_tests(self%project_dir, names, n_names, &
                                   exitcode, log_file)
                return
            end if
        case (BACKEND_CMAKE)
            inquire (file=trim(self%project_dir)//'/build/CTestTestfile.cmake', &
                     exist=has_tests)
            if (.not. has_tests) then
                exitcode = 0
                return
            end if
            if (slow) then
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && cd build && ctest --output-on-failure'
            else
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && cd build && ctest --output-on-failure -LE slow'
            end if
        case default
            write (error_unit, '(a)') 'fo: no build backend detected'
            exitcode = 1
            return
        end select

        call redirect_command(cmd, log_file)
        call execute_command_line(cmd, exitstat=exitcode, cmdstat=cmdstat, wait=.true.)
        if (cmdstat /= 0) exitcode = 1
    end subroutine backend_test

    subroutine backend_test_names(self, names, n_names, exitcode, include_slow, &
                                  log_file)
        use fo_scan, only: is_slow_test
        class(backend_t), intent(in) :: self
        character(len=128), intent(in) :: names(:)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file

        integer :: cmdstat, i, sub_exit
        character(len=2048) :: cmd
        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        logical :: slow
        integer :: n_fast

        slow = .false.
        if (present(include_slow)) slow = include_slow
        exitcode = 0

        if (self%kind == BACKEND_FPM) then
            n_fast = 0
            do i = 1, n_names
                if (.not. slow .and. is_slow_test(names(i))) cycle
                if (n_fast < MAX_TEST_TARGETS) then
                    n_fast = n_fast + 1
                    fast_names(n_fast) = names(i)
                end if
            end do
            if (n_fast == 0) return
            call fpm_run_tests(self%project_dir, fast_names, n_fast, &
                               exitcode, log_file)
            return
        end if

        do i = 1, n_names
            if (.not. slow .and. is_slow_test(names(i))) cycle

            select case (self%kind)
            case (BACKEND_FPM)
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && fpm test '//trim(names(i))//' 2>&1'
            case (BACKEND_CMAKE)
                cmd = 'cd '//trim(self%project_dir)// &
                      ' && cd build && ctest --output-on-failure -R '// &
                      trim(names(i))//' 2>&1'
            case default
                exitcode = 1
                return
            end select

            call execute_command_line(cmd, exitstat=sub_exit, &
                                      cmdstat=cmdstat, wait=.true.)
            if (cmdstat /= 0) sub_exit = 1
            if (sub_exit /= 0) exitcode = sub_exit
        end do
    end subroutine backend_test_names

    subroutine redirect_command(cmd, log_file)
        character(len=*), intent(inout) :: cmd
        character(len=*), intent(in), optional :: log_file

        if (present(log_file) .and. len_trim(log_file) > 0) then
            cmd = trim(cmd)//' > '//trim(log_file)//' 2>&1'
        else
            cmd = trim(cmd)//' 2>&1'
        end if
    end subroutine redirect_command

    subroutine fpm_list_tests(project_dir, names, n_names, exitcode, log_file)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(out) :: names(MAX_TEST_TARGETS)
        integer, intent(out) :: n_names, exitcode
        character(len=*), intent(in), optional :: log_file

        character(len=2048) :: cmd
        character(len=512) :: list_file, parse_file
        integer :: cmdstat

        names = ''
        n_names = 0
        exitcode = 0

        if (present(log_file) .and. len_trim(log_file) > 0) then
            list_file = log_file
            parse_file = log_file
        else
            call make_tmpfile('fo-fpm-tests', list_file)
            parse_file = list_file
        end if

        cmd = 'cd '//trim(project_dir)//' && fpm test --list'
        call redirect_command(cmd, list_file)
        call execute_command_line(cmd, exitstat=exitcode, cmdstat=cmdstat, &
                                  wait=.true.)
        if (cmdstat /= 0) exitcode = 1
        if (exitcode == 0) call parse_fpm_test_list(parse_file, names, n_names)
        if (.not. present(log_file)) call delete_file(list_file)
    end subroutine fpm_list_tests

    subroutine parse_fpm_test_list(path, names, n_names)
        character(len=*), intent(in) :: path
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        character(len=512) :: line
        integer :: u, iostat, colon
        logical :: in_names

        in_names = .false.
        open (newunit=u, file=path, status='old', iostat=iostat)
        if (iostat /= 0) return

        do
            read (u, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            colon = index(line, 'Matched names:')
            if (colon > 0) then
                in_names = .true.
                line = line(colon + len('Matched names:'):)
            else if (.not. in_names) then
                cycle
            end if
            call parse_words(line, names, n_names)
        end do
        close (u)
    end subroutine parse_fpm_test_list

    subroutine parse_words(line, names, n_names)
        character(len=*), intent(in) :: line
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        integer :: pos, start, finish, n

        n = len_trim(line)
        pos = 1
        do while (pos <= n)
            do while (pos <= n .and. line(pos:pos) == ' ')
                pos = pos + 1
            end do
            if (pos > n) exit

            start = pos
            do while (pos <= n .and. line(pos:pos) /= ' ')
                pos = pos + 1
            end do
            finish = pos - 1

            if (n_names < MAX_TEST_TARGETS) then
                n_names = n_names + 1
                names(n_names) = line(start:finish)
            end if
        end do
    end subroutine parse_words

    subroutine filter_slow_tests(names, n_names)
        use fo_scan, only: is_slow_test
        character(len=128), intent(inout) :: names(MAX_TEST_TARGETS)
        integer, intent(inout) :: n_names

        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        integer :: i, n_fast

        fast_names = ''
        n_fast = 0
        do i = 1, n_names
            if (is_slow_test(names(i))) cycle
            n_fast = n_fast + 1
            fast_names(n_fast) = names(i)
        end do

        names = fast_names
        n_names = n_fast
    end subroutine filter_slow_tests

    subroutine fpm_run_tests(project_dir, names, n_names, exitcode, log_file)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(in) :: names(MAX_TEST_TARGETS)
        integer, intent(in) :: n_names
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: log_file

        integer :: i, cmdstat
        character(len=2048) :: cmd

        if (n_names == 0) then
            exitcode = 0
            return
        end if

        cmd = 'cd '//trim(project_dir)//' && fpm test'
        do i = 1, n_names
            cmd = trim(cmd)//' '//trim(names(i))
        end do

        call redirect_command(cmd, log_file)
        call execute_command_line(cmd, exitstat=exitcode, cmdstat=cmdstat, &
                                  wait=.true.)
        if (cmdstat /= 0) exitcode = 1
    end subroutine fpm_run_tests

    subroutine make_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, '.tmp'
    end subroutine make_tmpfile

    subroutine delete_file(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -f '//trim(path), wait=.true.)
    end subroutine delete_file

end module fo_build_backend
