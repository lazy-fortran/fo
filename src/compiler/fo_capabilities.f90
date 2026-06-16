module fo_capabilities
    use, intrinsic :: iso_c_binding, only: c_int
    use fo_util, only: make_tmpfile, delete_tmpfile, json_bool
    use fo_fs, only: fs_make_dir, fs_remove_tree
    use fo_process, only: process_run_argv_logged, argv_push, argv_push_split
    implicit none
    private
    public :: capabilities_t, detect_capabilities
    public :: capabilities_text, capabilities_json

    integer, parameter :: CAP_LEN = 128

    type :: capabilities_t
        character(len=CAP_LEN) :: compiler_id = 'unknown'
        character(len=CAP_LEN) :: compiler_version = ''
        character(len=512) :: compiler_path = ''
        logical :: has_openmp = .false.
        logical :: has_module_output_dir = .false.
        logical :: has_depfile = .false.
        character(len=32) :: parallel_compile_limit = 'module DAG'
    end type capabilities_t

    interface
        subroutine fo_c_detect_nproc(nproc) bind(C, name='fo_c_detect_nproc')
            import :: c_int
            integer(c_int), intent(out) :: nproc
        end subroutine fo_c_detect_nproc
    end interface

contains

    subroutine detect_capabilities(cap)
        type(capabilities_t), intent(out) :: cap

        call detect_compiler_id(cap)
        call detect_compiler_path(cap)
        call probe_openmp(cap)
        call probe_module_output_dir(cap)
        call probe_depfile(cap)
    end subroutine detect_capabilities

    subroutine detect_compiler_id(cap)
        type(capabilities_t), intent(inout) :: cap

        character(len=512) :: line, tmpfile
        character(len=:), allocatable :: packed
        integer :: u, iostat, n_args, exitcode

        cap%compiler_id = 'unknown'
        cap%compiler_version = ''
        call make_tmpfile('fo_cap_detect', tmpfile)

        n_args = 0
        call argv_push(packed, n_args, 'gfortran')
        call argv_push(packed, n_args, '--version')
        call process_run_argv_logged('', packed, n_args, trim(tmpfile), &
                                     .false., 120, exitcode)

        open (newunit=u, file=tmpfile, status='old', iostat=iostat)
        if (iostat == 0) then
            read (u, '(a)', iostat=iostat) line
            close (u)
            if (iostat == 0 .and. len_trim(line) > 0) then
                if (index(line, 'GNU Fortran') > 0) then
                    cap%compiler_id = 'gfortran'
                    call extract_version(line, cap%compiler_version)
                else if (index(line, 'ifx') > 0 .or. &
                        index(line, 'ifort') > 0) then
                    cap%compiler_id = 'intel'
                    call extract_version(line, cap%compiler_version)
                else if (index(line, 'flang') > 0) then
                    cap%compiler_id = 'flang'
                    call extract_version(line, cap%compiler_version)
                else if (index(line, 'lfortran') > 0 .or. &
                        index(line, 'LFortran') > 0) then
                    cap%compiler_id = 'lfortran'
                    call extract_version(line, cap%compiler_version)
                end if
            end if
        end if
        call delete_tmpfile(tmpfile)

        if (trim(cap%compiler_id) == 'unknown') then
            n_args = 0
            call argv_push(packed, n_args, 'ifx')
            call argv_push(packed, n_args, '--version')
            call process_run_argv_logged('', packed, n_args, trim(tmpfile), &
                                         .false., 120, exitcode)
            open (newunit=u, file=tmpfile, status='old', iostat=iostat)
            if (iostat == 0) then
                read (u, '(a)', iostat=iostat) line
                close (u)
                if (iostat == 0 .and. len_trim(line) > 0) then
                    cap%compiler_id = 'intel'
                    call extract_version(line, cap%compiler_version)
                end if
            end if
            call delete_tmpfile(tmpfile)
        end if
    end subroutine detect_compiler_id

    subroutine detect_compiler_path(cap)
        type(capabilities_t), intent(inout) :: cap

        character(len=512) :: found

        cap%compiler_path = ''

        select case (trim(cap%compiler_id))
        case ('gfortran')
            call which_in_path('gfortran', found)
        case ('intel')
            call which_in_path('ifx', found)
            if (len_trim(found) == 0) call which_in_path('ifort', found)
        case ('flang')
            call which_in_path('flang', found)
        case ('lfortran')
            call which_in_path('lfortran', found)
        case default
            return
        end select

        if (len_trim(found) > 0) cap%compiler_path = trim(found)
    end subroutine detect_compiler_path

    subroutine which_in_path(name, path)
        !! Locate an executable on $PATH without a shell. Mirrors `which name`:
        !! returns the first PATH entry holding an existing file by that name.
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: path

        character(len=:), allocatable :: env
        character(len=1024) :: candidate
        integer :: env_len, i, start, stat
        logical :: present

        path = ''
        call get_environment_variable('PATH', length=env_len, status=stat)
        if (stat /= 0 .or. env_len <= 0) return
        allocate (character(len=env_len) :: env)
        call get_environment_variable('PATH', value=env, status=stat)
        if (stat /= 0) return

        start = 1
        do i = 1, env_len + 1
            if (i > env_len .or. env(i:i) == ':') then
                if (i > start) then
                    candidate = env(start:i - 1)//'/'//trim(name)
                    inquire (file=trim(candidate), exist=present)
                    if (present) then
                        path = trim(candidate)
                        return
                    end if
                end if
                start = i + 1
            end if
        end do
    end subroutine which_in_path

    subroutine probe_openmp(cap)
        type(capabilities_t), intent(inout) :: cap

        character(len=512) :: tmpdir, srcfile, tmpfile
        character(len=:), allocatable :: packed
        integer :: u, exitcode, n_args

        cap%has_openmp = .false.
        call make_tmpfile('fo_cap_omp', tmpdir)
        call fs_make_dir(trim(tmpdir))

        srcfile = trim(tmpdir)//'/test_omp.f90'
        open (newunit=u, file=srcfile, status='replace')
        write (u, '(a)') 'program test_omp'
        write (u, '(a)') 'use omp_lib'
        write (u, '(a)') 'implicit none'
        write (u, '(a)') 'integer :: n'
        write (u, '(a)') 'n = omp_get_max_threads()'
        write (u, '(a)') 'end program test_omp'
        close (u)

        call make_tmpfile('fo_cap_omp_log', tmpfile)
        select case (trim(cap%compiler_id))
        case ('gfortran')
            n_args = 0
            call argv_push(packed, n_args, 'gfortran')
            call argv_push_split(packed, n_args, '-fopenmp -o /dev/null')
            call argv_push(packed, n_args, trim(srcfile))
            call process_run_argv_logged('', packed, n_args, trim(tmpfile), &
                                         .false., 120, exitcode)
        case ('intel')
            n_args = 0
            call argv_push(packed, n_args, 'ifx')
            call argv_push_split(packed, n_args, '-qopenmp -o /dev/null')
            call argv_push(packed, n_args, trim(srcfile))
            call process_run_argv_logged('', packed, n_args, trim(tmpfile), &
                                         .false., 120, exitcode)
        case default
            exitcode = 1
        end select

        cap%has_openmp = (exitcode == 0)
        call fs_remove_tree(trim(tmpdir))
        call delete_tmpfile(tmpfile)
    end subroutine probe_openmp

    subroutine probe_module_output_dir(cap)
        type(capabilities_t), intent(inout) :: cap

        cap%has_module_output_dir = .false.
        select case (trim(cap%compiler_id))
        case ('gfortran')
            cap%has_module_output_dir = .true.
        case ('intel')
            cap%has_module_output_dir = .true.
        case ('flang')
            cap%has_module_output_dir = .true.
        end select
    end subroutine probe_module_output_dir

    subroutine probe_depfile(cap)
        type(capabilities_t), intent(inout) :: cap

        cap%has_depfile = .false.
        select case (trim(cap%compiler_id))
        case ('gfortran')
            cap%has_depfile = .true.
        case ('intel')
            cap%has_depfile = .true.
        end select
    end subroutine probe_depfile

    subroutine extract_version(line, version)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: version

        integer :: i, start, fin, n

        version = ''
        n = len_trim(line)
        start = 0
        do i = 1, n
            if (line(i:i) >= '0' .and. line(i:i) <= '9') then
                if (start == 0) start = i
            else if (start > 0 .and. line(i:i) /= '.') then
                exit
            end if
        end do

        if (start > 0) then
            fin = start
            do while (fin <= n)
                if ((line(fin:fin) >= '0' .and. line(fin:fin) <= '9') .or. &
                    line(fin:fin) == '.') then
                fin = fin + 1
            else
                exit
            end if
        end do
        fin = fin - 1
        if (fin > start .and. line(fin:fin) == '.') fin = fin - 1
        version = line(start:fin)
    end if
end subroutine extract_version

subroutine capabilities_text(cap, text)
    type(capabilities_t), intent(in) :: cap
    character(len=*), intent(out) :: text

    character(len=5) :: yesno

    text = 'compiler: '//trim(cap%compiler_id)
    if (len_trim(cap%compiler_version) > 0) then
        text = trim(text)//' '//trim(cap%compiler_version)
    end if
    text = trim(text)//char(10)

    yesno = 'no'
    if (cap%has_openmp) yesno = 'yes'
    text = trim(text)//'openmp: '//trim(yesno)//char(10)

    yesno = 'no'
    if (cap%has_module_output_dir) yesno = 'yes'
    text = trim(text)//'module-output-dir: '//trim(yesno)//char(10)

    yesno = 'no'
    if (cap%has_depfile) yesno = 'yes'
    text = trim(text)//'depfile: '//trim(yesno)//char(10)

    text = trim(text)//'parallel-compile-limit: '// &
        trim(cap%parallel_compile_limit)//char(10)
    text = trim(text)//'fo-can-optimize: '// &
        'discovery, scheduling, cache, test runner, diagnostics'//char(10)
    text = trim(text)//'compiler-limited: '// &
        'front-end parse, module generation, codegen'
end subroutine capabilities_text

subroutine capabilities_json(cap, json)
    type(capabilities_t), intent(in) :: cap
    character(len=*), intent(out) :: json

    json = '{"compiler":"'//trim(cap%compiler_id)//'"'
    json = trim(json)//',"compiler_version":"'// &
        trim(cap%compiler_version)//'"'
    json = trim(json)//',"compiler_path":"'// &
        trim(cap%compiler_path)//'"'
    json = trim(json)//',"openmp":'//trim(json_bool(cap%has_openmp))
    json = trim(json)//',"module_output_dir":'// &
        trim(json_bool(cap%has_module_output_dir))
    json = trim(json)//',"depfile":'//trim(json_bool(cap%has_depfile))
    json = trim(json)//',"parallel_compile_limit":"'// &
        trim(cap%parallel_compile_limit)//'"'
    json = trim(json)//',"fo_can_optimize":'// &
        '["discovery","scheduling","cache","test_runner","diagnostics"]'
    json = trim(json)//',"compiler_limited":'// &
        '["front_end_parse","module_generation","codegen"]}'
end subroutine capabilities_json

end module fo_capabilities
