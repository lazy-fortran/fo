program test_cmake_static_source
    use fo_test_harness, only: make_directory, write_text, read_text, remove_tree, &
        assert_true, assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add, run_process
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_cache, only: cache_file_digest
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, dependency
    character(:), allocatable :: configured_cache, frozen_cache, build
    character(len=*), parameter :: nl = new_line('a')
    character(len=4096) :: message
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    type(generation_context_t) :: context
    type(generation_t) :: generation
    type(execution_view_t) :: view
    type(string_list_t) :: command
    type(process_result_t) :: process
    integer :: ierr, cleanup_status

    call gremlin_setup(driver, scratch, project, cache, state)
    dependency = scratch//'/external/source'
    call make_directory(dependency)
    call write_text(dependency//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(MixedCase LANGUAGES Fortran)'//nl// &
        'add_library(value value.f90)'//nl)
    call write_text(dependency//'/value.f90', &
        'integer function dependency_value()'//nl// &
        'dependency_value = 23'//nl//'end function'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(parent LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'set(MixedCase_SOURCE_DIR "'//dependency//'" CACHE PATH "source")'//nl// &
        'add_subdirectory("${MixedCase_SOURCE_DIR}" dependency)'//nl// &
        'add_executable(probe main.f90)'//nl// &
        'target_link_libraries(probe PRIVATE value)'//nl// &
        'add_test(NAME dependency_value COMMAND probe)'//nl)
    call write_text(project//'/main.f90', &
        'program main'//nl//'integer, external :: dependency_value'//nl// &
        'if (dependency_value() /= 23) stop 1'//nl//'end program'//nl)
    backend = detect_backend(project)
    call cmake_capture_inventory(backend, inventory, ierr, message)
    call assert_true(ierr == 0, 'capture project source cache: '//trim(message))
    if (ierr == 0) then
        configured_cache = read_text(project//'/build/CMakeCache.txt')
        call assert_contains(configured_cache, 'MixedCase_SOURCE_DIR:STATIC=', &
            'native child project converts the parent PATH cache entry to STATIC')
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        context%input_inventory = inventory
        call generation_capture(project, state, context, generation, ierr, message)
        call assert_true(ierr == 0, 'freeze source cache: '//trim(message))
        if (ierr == 0) then
            call execution_view_create(scratch//'/relocated', generation%identity, &
                'static-source-oracle', 'dependency_value', &
                generation%input_inventory, .true., .true., view, ierr, message, &
                candidate_bundle_root=trim(generation%root)//'/bundle')
            call assert_true(ierr == 0, 'relocate source cache: '//trim(message))
            if (ierr == 0) then
                frozen_cache = read_text(trim(view%cwd)//'/.fo-cmake/cache.cmake')
                call assert_true(index(frozen_cache, dependency) == 0, &
                    'frozen cache does not retain the original dependency path')
                call assert_true(index(frozen_cache, 'set(parent_SOURCE_DIR ') == 0, &
                    'parent project STATIC metadata is not replayed')
                call remove_tree(dependency)
                call remove_tree(project)
                build = scratch//'/native-replay'
                call list_add(command, 'cmake')
                call list_add(command, '-G')
                call list_add(command, 'Ninja')
                call list_add(command, '-C')
                call list_add(command, trim(view%cwd)//'/.fo-cmake/cache.cmake')
                call list_add(command, '-S')
                call list_add(command, trim(view%cwd))
                call list_add(command, '-B')
                call list_add(command, build)
                call run_process(command, scratch, process, timeout_ms=30000)
                call assert_process_ok(process, &
                    'native configure replays the captured dependency source variable')
                if (process%exit_code == 0) then
                    command = string_list_t()
                    call list_add(command, 'cmake')
                    call list_add(command, '--build')
                    call list_add(command, build)
                    call list_add(command, '--parallel')
                    call list_add(command, '2')
                    call run_process(command, scratch, process, timeout_ms=30000)
                    call assert_process_ok(process, 'native relocated dependency builds')
                    command = string_list_t()
                    call list_add(command, 'ctest')
                    call list_add(command, '--test-dir')
                    call list_add(command, build)
                    call list_add(command, '--output-on-failure')
                    call list_add(command, '--no-tests=error')
                    call run_process(command, scratch, process, timeout_ms=30000)
                    call assert_process_ok(process, 'native relocated dependency test passes')
                    call assert_contains(process%stdout, '1/1 Test', &
                        'native replay executes one registered test')
                end if
                call execution_view_release(view, .false., cleanup_status, message)
                call assert_true(cleanup_status == 0, 'source view cleaned')
            end if
        end if
    end if
    call cmake_capture_cleanup(inventory, cleanup_status)
    call assert_true(cleanup_status == 0, 'source metadata cleaned')
    call finish_assertions()
end program test_cmake_static_source
