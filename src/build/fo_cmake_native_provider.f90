module fo_cmake_native_provider
    !! Native find_package providers for the opt-in CMake interpreter. Only the
    !! packages listed here are answered; anything else is a precise diagnostic.
    !! netCDF is probed through pkg-config include directories. BLAS and LAPACK
    !! are probed by linking an empty Fortran program against pkg-config flags,
    !! the plain -l<name> library or, on macOS, the Accelerate framework.
    use fo_cmake_native_model, only: cm_plan_t, cm_word_t, cm_set, cm_get, cm_words, &
                                     cm_add, cm_lower
    use fo_compiler_dialect, only: selected_compiler_command
    use fo_fs, only: fs_write_text
    use fo_util, only: read_text_file_alloc, make_tmpfile, delete_tmpfile
    use fo_process, only: process_run_argv_logged, argv_push, argv_push_split
    implicit none
    private
    public :: native_find_package
contains
    subroutine native_find_package(plan, words)
        type(cm_plan_t), intent(inout) :: plan
        type(cm_word_t), intent(in) :: words(:)
        character(:), allocatable :: package, module, libraries
        logical :: required, found
        integer :: i

        if (size(words) < 1) then
            plan%error = 'native CMake: package name is missing'
            return
        end if
        package = words(1)%text
        module = cm_lower(package)
        if (module /= 'blas' .and. module /= 'lapack' .and. module /= 'netcdf') then
            plan%error = 'native CMake: unsupported package provider '//package
            return
        end if
        required = .false.
        do i = 2, size(words)
            select case (words(i)%text)
            case ('REQUIRED')
                required = .true.
            case ('QUIET')
            case default
                plan%error = 'native CMake: package options/components unsupported'
                return
            end select
        end do
        libraries = ''
        if (module == 'netcdf') then
            call probe_pkg_config_includes(plan, module, found)
        else
            call probe_linkable(module, libraries, found)
        end if
        if (found) then
            call cm_set(plan, package//'_FOUND', 'TRUE')
            if (len(libraries) > 0) call cm_set(plan, package//'_LIBRARIES', libraries)
        else if (required) then
            plan%error = 'native CMake: unavailable provider '//package
        else
            call cm_set(plan, package//'_FOUND', 'FALSE')
        end if
    end subroutine

    subroutine probe_pkg_config_includes(plan, module, found)
        type(cm_plan_t), intent(inout) :: plan
        character(len=*), intent(in) :: module
        logical, intent(out) :: found
        character(:), allocatable :: value
        type(cm_word_t), allocatable :: flags(:)
        integer :: i
        call run_capture('pkg-config --cflags-only-I '//module, value, found)
        if (.not. found) return
        flags = cm_words(value)
        do i = 1, size(flags)
            if (index(flags(i)%text, '-I') /= 1) cycle
            call cm_add(plan%includes, flags(i)%text(3:))
        end do
    end subroutine

    subroutine probe_linkable(module, libraries, found)
        character(len=*), intent(in) :: module
        character(:), allocatable, intent(out) :: libraries
        logical, intent(out) :: found
        character(:), allocatable :: candidate, value
        logical :: ok
        integer :: i
        found = .false.
        libraries = ''
        do i = 1, 3
            select case (i)
            case (1)
                call run_capture('pkg-config --libs '//module, value, ok)
                if (.not. ok) cycle
                candidate = trim(adjustl(value))
            case (2)
                candidate = '-l'//module
            case default
                if (module == 'netcdf') cycle
                call run_capture('uname -s', value, ok)
                if (.not. ok) cycle
                if (index(value, 'Darwin') /= 1) cycle
                candidate = '-framework Accelerate'
            end select
            call links(candidate, ok)
            if (ok) then
                libraries = candidate
                found = .true.
                return
            end if
        end do
    end subroutine

    subroutine links(flags, ok)
        character(len=*), intent(in) :: flags
        logical, intent(out) :: ok
        character(len=512) :: source, log
        character(:), allocatable :: packed
        integer :: count, exitcode
        call make_tmpfile('fo-native-cmake-link', source)
        call make_tmpfile('fo-native-cmake-link-log', log)
        call fs_write_text(source, 'program fo_probe'//achar(10)//'end program'//achar(10))
        count = 0
        call argv_push(packed, count, trim(selected_compiler_command()))
        call argv_push(packed, count, '-x')
        call argv_push(packed, count, 'f95')
        call argv_push(packed, count, trim(source))
        call argv_push(packed, count, '-x')
        call argv_push(packed, count, 'none')
        call argv_push(packed, count, '-o')
        call argv_push(packed, count, trim(source)//'.out')
        call argv_push_split(packed, count, flags)
        call process_run_argv_logged('', packed, count, log, .false., 60, exitcode)
        ok = exitcode == 0
        call delete_tmpfile(source)
        call delete_tmpfile(trim(source)//'.out')
        call delete_tmpfile(log)
    end subroutine

    subroutine run_capture(command, value, ok)
        character(len=*), intent(in) :: command
        character(:), allocatable, intent(out) :: value
        logical, intent(out) :: ok
        character(len=512) :: log
        character(:), allocatable :: packed
        integer :: count, exitcode
        call make_tmpfile('fo-native-cmake-provider', log)
        count = 0
        call argv_push_split(packed, count, command)
        call process_run_argv_logged('', packed, count, log, .false., 10, exitcode)
        value = ''
        ok = exitcode == 0
        if (ok) then
            call read_text_file_alloc(log, value, exitcode)
            ok = exitcode == 0
        end if
        if (ok) then
            if (index(value, achar(10)) > 0) value = value(:index(value, achar(10)) - 1)
        end if
        call delete_tmpfile(log)
    end subroutine
end module fo_cmake_native_provider
