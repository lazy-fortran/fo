program test_cmake_package_prefix
    use fo_test_harness, only: make_directory, write_text, assert_true, &
        assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_test_cli, only: run_fo
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_cache, only: cache_file_digest
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, sdk, config
    character(len=*), parameter :: nl = new_line('a')
    character(len=4096) :: message
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    type(generation_context_t) :: context
    type(generation_t) :: generation
    type(execution_view_t) :: view
    type(string_list_t) :: args
    type(process_result_t) :: process
    integer :: ierr, cleanup_status

    call gremlin_setup(driver, scratch, project, cache, state)
    sdk = scratch//'/sdk'
    config = sdk//'/lib/cmake/Tiny/TinyConfig.cmake'
    call make_directory(sdk//'/include')
    call write_text(sdk//'/include/tiny.inc', 'integer, parameter :: value=19'//nl)
    call write_text(config, &
        'get_filename_component(prefix '// &
        '"${CMAKE_CURRENT_LIST_DIR}/../../.." ABSOLUTE)'//nl// &
        'add_library(Tiny::headers INTERFACE IMPORTED)'//nl// &
        'set_target_properties(Tiny::headers PROPERTIES'// &
        ' INTERFACE_INCLUDE_DIRECTORIES "${prefix}/include")'//nl)
    call write_text(sdk//'/lib/cmake/tiny_headers/tiny_headers-config.cmake', &
        'get_filename_component(prefix '// &
        '"${CMAKE_CURRENT_LIST_DIR}/../../.." ABSOLUTE)'//nl// &
        'add_library(tiny_headers::headers INTERFACE IMPORTED)'//nl// &
        'set_target_properties(tiny_headers::headers PROPERTIES'// &
        ' INTERFACE_INCLUDE_DIRECTORIES "${prefix}/include")'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(package_prefix LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'find_package(Tiny CONFIG REQUIRED HINTS '// &
        '"${PROJECT_SOURCE_DIR}/../sdk/lib/cmake/Tiny")'//nl// &
        'find_package(tiny_headers CONFIG REQUIRED HINTS '// &
        '"${PROJECT_SOURCE_DIR}/../sdk/lib/cmake/tiny_headers")'//nl// &
        'add_executable(probe main.f90)'//nl// &
        'target_link_libraries(probe PRIVATE Tiny::headers '// &
        'tiny_headers::headers)'//nl// &
        'add_test(NAME package_value COMMAND probe)'//nl)
    call write_text(project//'/main.f90', 'program main'//nl// &
        'include "tiny.inc"'//nl//'if (value/=19) error stop "SDK value"'//nl// &
        'end program'//nl)
    backend = detect_backend(project)
    call cmake_capture_inventory(backend, inventory, ierr, message)
    call assert_true(ierr == 0, 'capture installed package: '//trim(message))
    if (ierr == 0) then
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        context%input_inventory = inventory
        call generation_capture(project, state, context, generation, ierr, message)
        call assert_true(ierr == 0, 'freeze package inputs: '//trim(message))
        if (ierr == 0) then
            call execution_view_create(scratch//'/views', generation%identity, &
                'package-prefix-oracle', 'package_value', generation%input_inventory, &
                .true., .true., view, ierr, message, &
                candidate_bundle_root=trim(generation%root)//'/bundle')
            call assert_true(ierr == 0, 'private package view: '//trim(message))
            if (ierr == 0) then
                call list_add(args, 'test')
                call list_add(args, 'package_value')
                call run_fo(driver, args, trim(view%cwd), cache, process, &
                    timeout_ms=30000)
                call assert_process_ok(process, &
                    'self-relative package drives native CTest')
                call write_text(config, '# changed package metadata'//nl)
                call run_fo(driver, args, trim(view%cwd), cache, process, &
                    timeout_ms=30000)
                call assert_true(process%exit_code /= 0, &
                    'changed package cannot reuse PASS')
                call assert_contains(process%stderr//process%stdout, &
                    'captured CMake modules changed', 'package fingerprint is checked')
                call execution_view_release(view, .false., cleanup_status, message)
                call assert_true(cleanup_status == 0, 'private package view cleaned')
            end if
        end if
    end if
    call cmake_capture_cleanup(inventory, cleanup_status)
    call assert_true(cleanup_status == 0, 'owned package metadata cleaned')
    call finish_assertions()
end program test_cmake_package_prefix
