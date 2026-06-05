module fo_build_backend
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_json, only: make_tmpfile, delete_tmpfile
    use fo_process, only: process_detect_nproc, process_fpm_build, &
                          process_fpm_test_list, process_fpm_test_all, &
                          process_fpm_test_names, process_cmake_build, &
                          process_ctest
    implicit none
    private
    public :: backend_t, detect_backend, detect_nproc, detect_jobs
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

            if (has_cmake) then
                b%kind = BACKEND_CMAKE
                return
            else if (has_fpm) then
                b%kind = BACKEND_FPM
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

        np = process_detect_nproc()
        if (np < 1) np = 1
    end function detect_nproc

    function detect_jobs() result(jobs)
        integer :: jobs

        character(len=32) :: buf
        integer :: status, iostat

        jobs = detect_nproc()
        call get_environment_variable('FO_JOBS', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return

        read (buf, *, iostat=iostat) jobs
        if (iostat /= 0 .or. jobs < 1) jobs = detect_nproc()
    end function detect_jobs

    subroutine backend_build(self, exitcode, flags, log_file)
        class(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        character(len=*), intent(in), optional :: flags
        character(len=*), intent(in), optional :: log_file

        integer :: np
        character(len=512) :: log_path, flag_text

        np = detect_jobs()
        log_path = ''
        if (present(log_file)) log_path = log_file
        flag_text = ''
        if (present(flags)) flag_text = flags

        select case (self%kind)
        case (BACKEND_FPM)
            call process_fpm_build(self%project_dir, flag_text, np, log_path, &
                                   exitcode)
        case (BACKEND_CMAKE)
            call process_cmake_build(self%project_dir, flag_text, np, log_path, &
                                     exitcode)
        case default
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end select
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: build timed out (FO_BUILD_TIMEOUT exceeded);' // &
                ' set FO_BUILD_TIMEOUT env var or investigate slow build'
        end if
    end subroutine backend_build

    subroutine backend_test(self, exitcode, include_slow, log_file)
        class(backend_t), intent(in) :: self
        integer, intent(out) :: exitcode
        logical, intent(in), optional :: include_slow
        character(len=*), intent(in), optional :: log_file

        integer :: list_ierr, n_names, jobs
        character(len=128) :: names(MAX_TEST_TARGETS)
        character(len=512) :: log_path
        logical :: has_tests, slow

        slow = .false.
        if (present(include_slow)) slow = include_slow
        jobs = detect_jobs()
        log_path = ''
        if (present(log_file)) log_path = log_file

        select case (self%kind)
        case (BACKEND_FPM)
            if (slow) then
                call process_fpm_test_all(self%project_dir, jobs, log_path, exitcode)
            else
                call fpm_list_tests(self%project_dir, names, n_names, &
                                    list_ierr, log_path)
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
                                   exitcode, log_path)
            end if
        case (BACKEND_CMAKE)
            inquire (file=trim(self%project_dir)//'/build/CTestTestfile.cmake', &
                     exist=has_tests)
            if (.not. has_tests) then
                exitcode = 0
                return
            end if
            call process_ctest(self%project_dir, jobs, '', slow, log_path, exitcode)
        case default
            write (error_unit, '(a)') 'fo: no build backend detected'
            exitcode = 1
            return
        end select
        if (exitcode == 124) then
            write (error_unit, '(a)') &
                'fo: WARNING: tests timed out (FO_TEST_TIMEOUT exceeded);' // &
                ' set FO_TEST_TIMEOUT env var or mark slow tests with _slow suffix'
        end if
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

        integer :: i
        character(len=128) :: fast_names(MAX_TEST_TARGETS)
        logical :: slow
        integer :: n_fast, jobs
        character(len=1024) :: regex
        character(len=512) :: log_path

        slow = .false.
        if (present(include_slow)) slow = include_slow
        exitcode = 0
        jobs = detect_jobs()
        log_path = ''
        if (present(log_file)) log_path = log_file

        n_fast = 0
        do i = 1, n_names
            if (.not. slow .and. is_slow_test(names(i))) cycle
            if (n_fast < MAX_TEST_TARGETS) then
                n_fast = n_fast + 1
                fast_names(n_fast) = names(i)
            end if
        end do
        if (n_fast == 0) return

        if (self%kind == BACKEND_FPM) then
            call fpm_run_tests(self%project_dir, fast_names, n_fast, &
                               exitcode, log_path)
            return
        end if

        if (self%kind == BACKEND_CMAKE) then
            call names_to_ctest_regex(fast_names, n_fast, regex)
            call process_ctest(self%project_dir, jobs, regex, slow, log_path, &
                               exitcode)
            return
        end if

        exitcode = 1
    end subroutine backend_test_names

    subroutine names_to_ctest_regex(names, n_names, regex)
        character(len=128), intent(in) :: names(MAX_TEST_TARGETS)
        integer, intent(in) :: n_names
        character(len=*), intent(out) :: regex

        integer :: i

        regex = '^('
        do i = 1, n_names
            if (i > 1) regex = trim(regex)//'|'
            call append_ctest_regex_name(regex, names(i))
        end do
        regex = trim(regex)//')$'
    end subroutine names_to_ctest_regex

    subroutine append_ctest_regex_name(regex, name)
        character(len=*), intent(inout) :: regex
        character(len=*), intent(in) :: name

        integer :: i
        character(len=1) :: ch

        do i = 1, len_trim(name)
            ch = name(i:i)
            select case (ch)
            case ('.', '^', '$', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|')
                regex = trim(regex)//achar(92)//ch
            case (achar(92))
                regex = trim(regex)//achar(92)//achar(92)
            case default
                regex = trim(regex)//ch
            end select
        end do
    end subroutine append_ctest_regex_name

    subroutine fpm_list_tests(project_dir, names, n_names, exitcode, log_file)
        character(len=*), intent(in) :: project_dir
        character(len=128), intent(out) :: names(MAX_TEST_TARGETS)
        integer, intent(out) :: n_names, exitcode
        character(len=*), intent(in), optional :: log_file

        character(len=512) :: list_file, parse_file

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

        call process_fpm_test_list(project_dir, list_file, exitcode)
        if (exitcode == 0) call parse_fpm_test_list(parse_file, names, n_names)
        if (.not. present(log_file) .or. len_trim(log_file) == 0) then
            call delete_tmpfile(list_file)
        end if
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

        integer :: jobs

        if (n_names == 0) then
            exitcode = 0
            return
        end if

        jobs = detect_jobs()

        if (present(log_file)) then
            call process_fpm_test_names(project_dir, names, n_names, jobs, log_file, &
                                        exitcode)
        else
            call process_fpm_test_names(project_dir, names, n_names, jobs, '', &
                                        exitcode)
        end if
    end subroutine fpm_run_tests

end module fo_build_backend
