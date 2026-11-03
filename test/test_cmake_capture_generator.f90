program test_cmake_capture_generator
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t
    use fo_fs, only: fs_find_executable
    use fo_test_cli, only: run_external
    use fo_test_harness, only: string_list_t, process_result_t, make_scratch, &
        write_text, read_text, list_add, assert_true, assert_contains, &
        assert_process_ok, remove_tree, finish_assertions
    implicit none
    character(:), allocatable :: scratch, project, metadata
    character(len=4096) :: ninja, message
    character, parameter :: nl = new_line('a')
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    type(string_list_t) :: args
    type(process_result_t) :: process
    logical :: available
    integer :: i, ierr, rc, forced
    interface
        integer(c_int) function set_env(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_env
    end interface

    call fs_find_executable('ninja', ninja, available)
    if (.not. available) stop 'SKIP: Ninja is unavailable'
    call make_scratch('fo-capture-generator', scratch)
    project = scratch//'/project'
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(capture_generator LANGUAGES Fortran)'//nl// &
        'add_library(value value.f90)'//nl)
    call write_text(project//'/value.f90', &
        'module value'//nl//'end module value'//nl)
    call list_add(args, '-S')
    call list_add(args, project)
    call list_add(args, '-B')
    call list_add(args, project//'/build')
    call list_add(args, '-G')
    call list_add(args, 'Unix Makefiles')
    call run_external('cmake', args, project, process, timeout_ms=30000)
    call assert_process_ok(process, 'configures the caller with Makefiles')
    do forced = 0, 1
        rc = set_env('FO_CMAKE_GENERATOR'//c_null_char, c_null_char, 1_c_int)
        if (forced == 1) rc = set_env('FO_CMAKE_GENERATOR'//c_null_char, &
            'Unix Makefiles'//c_null_char, 1_c_int)
        call assert_true(rc == 0, 'selects the generator policy')
        backend = detect_backend(project)
        call cmake_capture_inventory(backend, inventory, ierr, message)
        call assert_true(ierr == 0, 'captures configuration: '//trim(message))
        metadata = ''
        do i = 1, inventory%root_count
            if (inventory%roots(i)%canonical_alias == 'cmake-context') &
                metadata = trim(inventory%roots(i)%physical_path)
        end do
        call assert_true(len(metadata) > 0, 'capture declares configuration metadata')
        if (len(metadata) > 0) then
            if (forced == 0) then
                call assert_contains(read_text(metadata//'/generator.txt'), &
                    'Ninja', 'unforced captured trees prefer Ninja')
                call assert_true(index(read_text(metadata//'/cache.cmake'), &
                    'set(CMAKE_MAKE_PROGRAM ') == 0, &
                    'generator switch drops the incompatible build program')
            else
                call assert_contains(read_text(metadata//'/generator.txt'), &
                    'Unix Makefiles', 'an explicitly forced generator is retained')
            end if
        end if
        call cmake_capture_cleanup(inventory, ierr)
    end do
    call remove_tree(scratch)
    call finish_assertions()
end program test_cmake_capture_generator
