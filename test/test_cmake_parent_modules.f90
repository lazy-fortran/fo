program test_cmake_parent_modules
    use fo_test_harness, only: make_directory, write_text, read_text, &
        assert_true, assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_test_cli, only: run_fo
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t, input_inventory_revalidate
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_cache, only: cache_file_digest
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, modules
    character(len=*), parameter :: nl = new_line('a')
    character(len=4096) :: message
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    type(generation_context_t) :: context
    type(generation_t) :: generation
    type(execution_view_t) :: view
    type(string_list_t) :: args
    type(process_result_t) :: process
    integer :: ierr, cleanup_status, i
    logical :: module_found

    call gremlin_setup(driver, scratch, project, cache, state)
    modules = scratch//'/cmake'
    call make_directory(modules//'/nested')
    call write_text(modules//'/Util.cmake', &
        'include("${CMAKE_CURRENT_LIST_DIR}/nested/Value.cmake")'//nl)
    call write_text(modules//'/nested/Value.cmake', 'set(VALUE 19)'//nl)
    call make_directory(scratch//'/shared')
    call write_text(scratch//'/shared/value.f90', &
        'module shared_value'//nl//'integer, parameter :: value = 23'//nl// &
        'end module'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(parent_modules LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/../cmake")'//nl// &
        'include(Util)'//nl// &
        'set(SHARED_SOURCE_DIR "${PROJECT_SOURCE_DIR}/../shared"'// &
        ' CACHE PATH "source")'//nl// &
        'configure_file(main.f90.in main.f90 @ONLY)'//nl// &
        'configure_file(template.in copied.in COPYONLY)'//nl// &
        'add_executable(probe "${CMAKE_CURRENT_BINARY_DIR}/main.f90"'//nl// &
        ' "${PROJECT_SOURCE_DIR}/../shared/value.f90")'//nl// &
        'add_test(NAME module_value COMMAND probe'// &
        ' "${CMAKE_CURRENT_BINARY_DIR}/copied.in")'//nl)
    call write_text(project//'/template.in', 'seed'//nl)
    call write_text(project//'/main.f90.in', &
        'program main'//nl//'use shared_value, only: value'//nl// &
        'integer :: unit, ios'//nl//'character(4096) :: path'//nl// &
        'call get_command_argument(1, path)'//nl// &
        'open(newunit=unit, file=trim(path), status="old",'// &
        ' action="write", iostat=ios)'//nl// &
        'if (ios /= 0) error stop "copied fixture permissions"'//nl// &
        'write(unit, "(a)") "private edit"'//nl//'close(unit)'//nl// &
        'if (value /= 23) error stop "shared value"'//nl// &
        'if (@VALUE@ /= 19) error stop "module value"'//nl// &
        'end program'//nl)
    backend = detect_backend(project)
    call cmake_capture_inventory(backend, inventory, ierr, message)
    call assert_true(ierr == 0, 'capture parent CMake modules: '//trim(message))
    if (ierr == 0) then
        module_found = .false.
        do i = 1, inventory%entry_count
            if (inventory%entries(i)%relative_path /= 'nested/Value.cmake') cycle
            module_found = len_trim(inventory%entries(i)%content_digest) == 64
        end do
        call assert_true(module_found, 'nested parent module is fingerprinted')
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        context%input_inventory = inventory
        call generation_capture(project, state, context, generation, ierr, message)
        call assert_true(ierr == 0, 'freeze parent modules: '//trim(message))
        if (ierr == 0) then
            call execution_view_create(scratch//'/views', generation%identity, &
                'parent-module-oracle', 'module_value', generation%input_inventory, &
                .true., .true., view, ierr, message, &
                candidate_bundle_root=trim(generation%root)//'/bundle')
            call assert_true(ierr == 0, 'private module view: '//trim(message))
            if (ierr == 0) then
                call assert_contains(read_text(trim(view%cwd)// &
                    '/../cmake/nested/Value.cmake'), 'set(VALUE 19)', &
                    'frozen modules retain the authored parent-relative layout')
                call list_add(args, 'test')
                call list_add(args, 'module_value')
                call run_fo(driver, args, trim(view%cwd), cache, process, &
                    timeout_ms=30000)
                call assert_process_ok(process, 'captured parent module drives CTest')
                call assert_contains(read_text(trim(generation%project_root)// &
                    '/template.in'), 'seed', 'private fixture edits preserve frozen inputs')
                call write_text(modules//'/nested/Value.cmake', 'set(VALUE 31)'//nl)
                call input_inventory_revalidate(project, inventory%declarations, &
                    inventory, ierr, message)
                call assert_true(ierr /= 0, 'parent module edit invalidates capture')
                call assert_contains(trim(message), 'nested/Value.cmake', &
                    'changed parent module is identified')
                call run_fo(driver, args, trim(view%cwd), cache, process, &
                    timeout_ms=30000)
                call assert_true(process%exit_code /= 0, &
                    'cached PASS cannot hide a changed parent module')
                call assert_contains(process%stderr//process%stdout, &
                    'captured CMake modules changed', 'module mismatch is explicit')
                call execution_view_release(view, .false., cleanup_status, message)
                call assert_true(cleanup_status == 0, 'private module view cleaned')
            end if
        end if
    end if
    call cmake_capture_cleanup(inventory, cleanup_status)
    call assert_true(cleanup_status == 0, 'owned module metadata cleaned')
    call finish_assertions()
end program test_cmake_parent_modules
