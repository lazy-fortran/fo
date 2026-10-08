module fo_cmake_native
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use fo_cmake_native_model, only: cm_plan_t, cm_target_t, cm_get, cm_lower
    use fo_cmake_native_config, only: native_cmake_configure, target_index
    use fo_cmake_context, only: cmake_context_t
    use fo_gfortran_build, only: native_compile_units, native_link_target, &
           native_archive_target, make_obj_path, native_run_test, write_test_result_line
    use fo_scan, only: scan_unit_t, scan_file, MAX_UNITS
    use fo_fs, only: fs_make_dir, fs_write_text
    implicit none
    private
    public :: native_cmake_build, native_cmake_test, native_cmake_resolve
contains
    subroutine native_cmake_build(context, log_file, exitcode)
        type(cmake_context_t), intent(in) :: context
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        type(cm_plan_t) :: plan
        type(scan_unit_t), allocatable :: units(:)
        character(len=4096), allocatable :: flags(:)
        character(len=512), allocatable :: includes(:), objects(:)
        character(len=128), allocatable :: libraries(:)
        character(:), allocatable :: module_dir, obj_dir, source, compile_flags
        integer, allocatable :: selected(:)
        integer :: i, j, k, n, compiled, at, io_status
        logical :: duplicate

        call native_cmake_configure(context, plan)
        exitcode = 1
        call fs_write_text(log_file, '')
        if (failed(plan, log_file)) return
        allocate (selected(size(plan%targets)))
        selected = 0
        if (size(plan%build_targets) == 0) then
            do i = 1, size(selected)
                call select_target(plan, i, selected)
            end do
        else
            do i = 1, size(plan%build_targets)
                at = target_index(plan, trim(plan%build_targets(i)))
                if (at == 0) then
                    plan%error = 'native CMake: unknown build target '// &
                        trim(plan%build_targets(i))
                    exit
                end if
                call select_target(plan, at, selected)
            end do
        end if
        if (failed(plan, log_file)) return
        module_dir = plan%build
        do i = 1, size(selected)
            if (selected(i) == 0) cycle
            if (len(plan%targets(i)%module_dir) == 0) cycle
            if (module_dir /= plan%build .and. &
                module_dir /= plan%targets(i)%module_dir) then
             plan%error = 'native CMake: multiple module output directories unsupported'
                exit
            end if
            module_dir = plan%targets(i)%module_dir
        end do
        if (failed(plan, log_file)) return
        obj_dir = plan%build//'/.fo-native/obj'
        call fs_make_dir(plan%build)
        call fs_make_dir(module_dir)
        call fs_make_dir(obj_dir)
        if (size(plan%includes) > 64) then
            plan%error = 'native CMake: include capacity exceeded'
            if (failed(plan, log_file)) return
        end if
        allocate (includes(size(plan%includes)))
        do i = 1, size(includes)
            includes(i) = plan%includes(i)%text
        end do
        allocate (units(0), flags(0))
        compile_flags = compiler_flags(plan)
        do i = 1, size(selected)
            if (selected(i) == 0) cycle
            do j = 1, size(plan%targets(i)%sources)
                source = plan%targets(i)%sources(j)%text
                at = index(source, '.', back=.true.)
                if (at == 0) then
                    plan%error = 'native CMake: unsupported source language '//source
                    exit
                end if
                select case (cm_lower(source(at:)))
                case ('.f90', '.f95', '.f03', '.f08', '.f', '.for')
                case default
                    plan%error = 'native CMake: unsupported source language '//source
                    exit
                end select
                duplicate = .false.
                do k = 1, size(units)
                    if (trim(units(k)%filename) == source) duplicate = .true.
                end do
                if (duplicate) cycle
                if (size(units) >= MAX_UNITS) then
                    plan%error = 'native CMake: declared source capacity exceeded'
                    exit
                end if
                if (len(source) > 512 .or. len(compile_flags) > 4096) then
                    plan%error = 'native CMake: source or flag capacity exceeded'
                    exit
                end if
                units = [units, scan_unit_t()]
                call scan_file(source, units(size(units)), io_status)
                if (io_status /= 0) then
               plan%error = 'native CMake: cannot scan declared Fortran source '//source
                    exit
                end if
                if (plan%targets(i)%kind == 'EXECUTABLE' .and. &
                    len_trim(units(size(units))%module_name) > 0) then
                    plan%error = 'native CMake: executable-defined modules unsupported'
                    exit
                end if
                units(size(units))%is_test = .false.
                flags = [flags, repeat(' ', 4096)]
                flags(size(flags)) = compile_flags
            end do
        end do
        if (failed(plan, log_file)) return
        call native_compile_units(plan%root, units, flags, includes, module_dir, &
                                  obj_dir, log_file, exitcode, compiled, &
                                  plan%build//'/compile_commands.json')
        if (exitcode /= 0) return
        do i = 1, size(selected)
            if (selected(i) == 0 .or. plan%targets(i)%kind /= 'STATIC') cycle
            call target_objects(plan%targets(i), plan%root, obj_dir, objects)
            call fs_make_dir(plan%targets(i)%binary_dir)
            call native_archive_target(plan%root, objects, &
                                       target_output(plan, i), log_file, exitcode)
            if (exitcode /= 0) return
        end do
        do i = 1, size(selected)
            if (selected(i) == 0 .or. plan%targets(i)%kind /= 'EXECUTABLE') cycle
            call target_objects(plan%targets(i), plan%root, obj_dir, objects)
            call executable_main_first(plan, plan%targets(i), units, objects)
            allocate (libraries(0))
            call linked_outputs(plan, i, objects, libraries, 0)
            if (failed(plan, log_file)) return
            call fs_make_dir(plan%targets(i)%binary_dir)
            call native_link_target(plan%root, objects, libraries, &
                           link_flags(plan), target_output(plan, i), log_file, exitcode)
            deallocate (libraries)
            if (exitcode /= 0) return
        end do
        exitcode = 0
    end subroutine

    subroutine native_cmake_test(context, names, log_file, exitcode)
        type(cmake_context_t), intent(in) :: context
        character(len=*), intent(in) :: names(:), log_file
        integer, intent(out) :: exitcode
        type(cm_plan_t) :: plan
        logical, allocatable :: selected(:)
        character(:), allocatable :: args, status
        character(len=32) :: exit_text
        integer :: i, j, target, code, count
        integer(int64) :: start, finish, rate
        real :: seconds

        call native_cmake_configure(context, plan)
        exitcode = 1
        if (failed(plan, log_file)) return
        allocate (selected(size(plan%tests)))
        selected = size(names) == 0
        do i = 1, size(names)
            count = 0
            do j = 1, size(plan%tests)
                if (plan%tests(j)%name /= trim(names(i))) cycle
                selected(j) = .true.
                count = count + 1
            end do
            if (count /= 1) then
      plan%error = 'native CMake: unknown or ambiguous registered test '//trim(names(i))
                if (failed(plan, log_file)) return
            end if
        end do
        if (.not. any(selected)) then
            plan%error = 'native CMake: selected scope contains no registered tests'
            if (failed(plan, log_file)) return
        end if
        exitcode = 0
        do i = 1, size(plan%tests)
            if (.not. selected(i)) cycle
            target = target_index(plan, plan%tests(i)%target)
            if (target == 0) then
                plan%error = 'native CMake: test command is not an executable target'
                if (failed(plan, log_file)) then
                    exitcode = 1
                    return
                end if
            end if
            if (plan%targets(target)%kind /= 'EXECUTABLE') then
                plan%error = 'native CMake: test command is not an executable target'
                if (failed(plan, log_file)) then
                    exitcode = 1
                    return
                end if
            end if
            args = ''
            do j = 1, size(plan%tests(i)%args)
                args = args//plan%tests(i)%args(j)%text//achar(10)
            end do
            call system_clock(start, rate)
            call native_run_test(plan%tests(i)%cwd, target_output(plan, target), &
                                 args, log_file, code)
            call system_clock(finish)
            seconds = real(finish - start)/real(max(1_int64, rate))
            status = 'PASS'
            if (code /= 0) status = 'FAIL'
            if (code == 124) status = 'TIMEOUT'
            write (exit_text, '(i0)') code
            call write_test_result_line(log_file, plan%tests(i)%name, &
                                        status, trim(exit_text), seconds)
            if (code /= 0) exitcode = 1
        end do
    end subroutine

    subroutine native_cmake_resolve(context, name, output, found)
        type(cmake_context_t), intent(in) :: context
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: output
        logical, intent(out) :: found
        type(cm_plan_t) :: plan
        integer :: at
        call native_cmake_configure(context, plan)
        output = ''
        found = .false.
        if (len(plan%error) > 0) return
        at = target_index(plan, name)
        if (at == 0) return
        if (plan%targets(at)%kind /= 'EXECUTABLE') return
        output = target_output(plan, at)
        inquire (file=trim(output), exist=found)
    end subroutine

    recursive subroutine select_target(plan, at, selected)
        type(cm_plan_t), intent(inout) :: plan
        integer, intent(in) :: at
        integer, intent(inout) :: selected(:)
        integer :: i, dependency
        if (selected(at) == 2) return
        if (selected(at) == 1) then
            plan%error = 'native CMake: cyclic target dependency unsupported'
            return
        end if
        selected(at) = 1
        do i = 1, size(plan%targets(at)%links)
            dependency = target_index(plan, plan%targets(at)%links(i)%text)
            if (dependency > 0) call select_target(plan, dependency, selected)
        end do
        selected(at) = 2
    end subroutine

    subroutine target_objects(target, root, obj_dir, objects)
        type(cm_target_t), intent(in) :: target
        character(len=*), intent(in) :: root, obj_dir
        character(len=512), allocatable, intent(out) :: objects(:)
        integer :: i
        allocate (objects(size(target%sources)))
        do i = 1, size(objects)
            call make_obj_path(target%sources(i)%text, root, obj_dir, objects(i))
        end do
    end subroutine

    subroutine executable_main_first(plan, target, units, objects)
        type(cm_plan_t), intent(inout) :: plan
        type(cm_target_t), intent(in) :: target
        type(scan_unit_t), intent(in) :: units(:)
        character(len=512), intent(inout) :: objects(:)
        character(len=512) :: swap
        integer :: i, j, count, main
        count = 0
        main = 0
        do i = 1, size(target%sources)
            do j = 1, size(units)
                if (trim(units(j)%filename) /= target%sources(i)%text) cycle
                if (.not. units(j)%is_program) cycle
                count = count + 1
                main = i
            end do
        end do
        if (count /= 1) then
            plan%error = 'native CMake: executable requires one Fortran program unit'
            return
        end if
        swap = objects(1)
        objects(1) = objects(main)
        objects(main) = swap
    end subroutine

    recursive subroutine linked_outputs(plan, at, objects, libraries, depth)
        type(cm_plan_t), intent(inout) :: plan
        integer, intent(in) :: at, depth
        character(len=512), allocatable, intent(inout) :: objects(:)
        character(len=128), allocatable, intent(inout) :: libraries(:)
        integer :: i, dependency
        character(len=512) :: output
        character(len=128) :: library
        if (depth > size(plan%targets)) then
            plan%error = 'native CMake: cyclic link closure unsupported'
            return
        end if
        do i = 1, size(plan%targets(at)%links)
            dependency = target_index(plan, plan%targets(at)%links(i)%text)
            if (dependency == 0) then
                if (len(plan%targets(at)%links(i)%text) > len(library)) then
                    plan%error = 'native CMake: link library name capacity exceeded'
                    return
                end if
                library = plan%targets(at)%links(i)%text
                libraries = [libraries, library]
            else
                if (plan%targets(dependency)%kind /= 'STATIC') then
                    plan%error = 'native CMake: only static target links supported'
                    return
                end if
                if (len(plan%targets(dependency)%module_dir) > 0 .and. &
                    .not. plan%targets(dependency)%module_exported) then
                    plan%error = 'native CMake: linked module directory is not exported'
                    return
                end if
                output = target_output(plan, dependency)
                objects = [objects, output]
                call linked_outputs(plan, dependency, objects, libraries, depth + 1)
            end if
        end do
    end subroutine

    function target_output(plan, at) result(output)
        type(cm_plan_t), intent(in) :: plan
        integer, intent(in) :: at
        character(:), allocatable :: output
        output = plan%targets(at)%binary_dir//'/'//plan%targets(at)%name
        if (plan%targets(at)%kind == 'STATIC') then
            output = plan%targets(at)%binary_dir//'/lib'//plan%targets(at)%name//'.a'
        else if (cm_get(plan, 'WIN32') == 'TRUE') then
            output = output//'.exe'
        end if
    end function

    function configuration_flags(plan) result(flags)
        type(cm_plan_t), intent(in) :: plan
        character(:), allocatable :: flags, configuration
        integer :: i, code
        configuration = plan%configuration
        do i = 1, len(configuration)
            code = iachar(configuration(i:i))
            if (code >= 97 .and. code <= 122) &
                configuration(i:i) = achar(code - 32)
        end do
        flags = cm_get(plan, 'CMAKE_Fortran_FLAGS')//' '// &
                cm_get(plan, 'CMAKE_Fortran_FLAGS_'//configuration)
    end function

    function compiler_flags(plan) result(flags)
        type(cm_plan_t), intent(in) :: plan
        character(:), allocatable :: flags
        integer :: i
        flags = configuration_flags(plan)
        do i = 1, size(plan%compile_flags)
            flags = flags//' '//plan%compile_flags(i)%text
        end do
    end function

    function link_flags(plan) result(flags)
        type(cm_plan_t), intent(in) :: plan
        character(:), allocatable :: flags
        integer :: i
        flags = configuration_flags(plan)
        do i = 1, size(plan%link_flags)
            flags = flags//' '//plan%link_flags(i)%text
        end do
    end function

    logical function failed(plan, log_file) result(error)
        type(cm_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: log_file
        integer :: unit, ierr
        error = len(plan%error) > 0
        if (.not. error) return
        write (error_unit, '(a)') plan%error
    open (newunit=unit, file=log_file, status='unknown', position='append', iostat=ierr)
        if (ierr /= 0) return
        write (unit, '(a)') plan%error
        close (unit)
    end function
end module fo_cmake_native
