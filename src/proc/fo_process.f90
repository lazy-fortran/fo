module fo_process
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private
    public :: process_detect_nproc
    public :: process_fpm_build, process_fpm_test_list, process_fpm_test_all
    public :: process_fpm_test_names, process_cmake_build, process_ctest
    public :: process_scan_sources

    integer, parameter :: C_PATH_LEN = 4096
    integer, parameter :: C_ARG_LEN = 4096

    interface
        subroutine fo_c_detect_nproc(nproc) bind(C, name='fo_c_detect_nproc')
            import :: c_int
            integer(c_int), intent(out) :: nproc
        end subroutine fo_c_detect_nproc

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
    end interface

contains

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
