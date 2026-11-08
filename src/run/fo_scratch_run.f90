module fo_scratch_run
    !! `fo run FILE.f90` in a CMake project: build a scratch program that uses
    !! the project's modules against the project's own build tree and run it.
    !! The program takes the compile settings and link line of the smallest
    !! executable target that links every library providing a module it uses
    !! (fo_cmake_build), compiles and links in a private directory, and never
    !! writes into the project or its build tree beyond building that target.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_build_backend, only: backend_t, BACKEND_CMAKE, backend_build, cmake_configure
    use fo_cmake_native_config, only: native_cmake_selected
    use fo_cmake_model, only: cmake_plan_t, cmake_plan_load
    use fo_cmake_build, only: cmake_scratch_donor, cmake_scratch_build
    use fo_fs, only: fs_canonical_path, fs_remove_tree, fs_mkdir_excl
    use fo_util, only: temporary_root, make_tmpfile, delete_tmpfile, int_text, &
        read_text_file_alloc
    use fo_process, only: process_getpid, process_run_argv_logged
    implicit none
    private

    public :: scratch_run

contains

    subroutine scratch_run(b, source, flags, skip_build, cwd, program_args, &
            n_program_args, exitcode)
        !! program_args: n_program_args packed argv tokens for the program.
        !! exitcode is the program's, or 1 when it cannot be built.
        type(backend_t), intent(inout) :: b
        character(len=*), intent(in) :: source, flags, cwd, program_args
        logical, intent(in) :: skip_build
        integer, intent(in) :: n_program_args
        integer, intent(out) :: exitcode
        type(cmake_plan_t) :: plan
        character(len=1024) :: log_file
        character(len=:), allocatable :: path, message, out_dir, executable, stem
        character(len=:), allocatable :: packed, text
        integer :: donor, status, n_args, attempt, slash, dot

        exitcode = 1
        if (b%kind /= BACKEND_CMAKE .or. native_cmake_selected(b%project_dir)) then
            write (error_unit, '(a)') 'fo run: a scratch source needs a CMake '// &
                'project; in an fpm project put the program under app/ or example/'
            return
        end if
        path = fs_canonical_path(source)
        call make_tmpfile('fo-run-scratch', log_file)
        call cmake_configure(b%cmake, flags, trim(log_file), status)
        if (status /= 0) then
            call fail('CMake configure failed')
            return
        end if
        call cmake_plan_load(b%cmake, plan, message)
        if (len(message) > 0) then
            call fail('CMake build tree unreadable: '//message)
            return
        end if
        call cmake_scratch_donor(plan, path, donor, message)
        if (donor == 0) then
            call fail(message)
            return
        end if
        write (error_unit, '(a)') 'fo run: '//trim(source)// &
            ' uses the compile settings and link line of '//plan%targets(donor)%name
        if (.not. skip_build) then
            if (allocated(b%cmake%build_targets)) deallocate (b%cmake%build_targets)
            allocate (character(len=len(plan%targets(donor)%name)) :: &
                b%cmake%build_targets(1))
            b%cmake%build_targets(1) = plan%targets(donor)%name
            call backend_build(b, status, flags=flags, log_file=trim(log_file), &
                configured=.true.)
            if (status /= 0) then
                call fail('build of '//plan%targets(donor)%name//' failed')
                return
            end if
        end if
        out_dir = ''
        do attempt = 1, 100
            out_dir = temporary_root()//'/fo-run-'//int_text(process_getpid())// &
                '-'//int_text(attempt)
            if (fs_mkdir_excl(out_dir) == 0) exit
            out_dir = ''
        end do
        if (len(out_dir) == 0) then
            call fail('cannot create a private build directory')
            return
        end if
        slash = index(path, '/', back=.true.)
        stem = path(slash + 1:)
        dot = index(stem, '.', back=.true.)
        if (dot > 1) stem = stem(:dot - 1)
        executable = out_dir//'/'//stem
        call cmake_scratch_build(plan, donor, path, out_dir, executable, &
            trim(log_file), status)
        if (status /= 0) then
            call fs_remove_tree(out_dir, status)
            call fail('cannot build '//trim(source))
            return
        end if
        call delete_tmpfile(trim(log_file))
        packed = executable//achar(0)//program_args
        n_args = 1 + n_program_args
        call process_run_argv_logged(trim(cwd), packed, n_args, '', .false., 0, &
            exitcode)
        call fs_remove_tree(out_dir, status)

    contains

        subroutine fail(reason)
            character(len=*), intent(in) :: reason

            write (error_unit, '(a)') 'fo run: '//reason
            call read_text_file_alloc(trim(log_file), text, status)
            if (status == 0) then
                if (len(text) > 4000) text = text(len(text) - 3999:)
                if (len(text) > 0) write (error_unit, '(a)') text
            end if
            call delete_tmpfile(trim(log_file))
            exitcode = 1
        end subroutine fail

    end subroutine scratch_run

end module fo_scratch_run
