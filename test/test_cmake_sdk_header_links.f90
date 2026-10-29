program test_cmake_sdk_header_links
    use fo_test_harness, only: make_directory, write_text, make_symlink, &
        assert_true, assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_test_cli, only: run_fo
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t, cmake_inputs_revalidate
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    use fo_cache, only: cache_file_digest
    use fo_fs, only: fs_remove_file
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, sdk, installed
    character(len=*), parameter :: nl = new_line('a')
    character(len=4096) :: message
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory, rejected
    type(generation_context_t) :: context
    type(generation_t) :: generation
    type(execution_view_t) :: view
    type(string_list_t) :: args, cached_environment
    type(process_result_t) :: process
    integer :: ierr, cleanup

    call gremlin_setup(driver, scratch, project, cache, state)
    sdk = scratch//'/sdk/include'
    installed = scratch//'/installed/include'
    call make_directory(sdk)
    call make_directory(installed)
    call write_text(installed//'/version.h', &
        '#define SDK_VERSION "1.2.3"'//nl)
    call write_text(installed//'/value.h', '#define SDK_VALUE 19'//nl)
    call make_symlink('../../installed/include/version.h', sdk//'/version.h')
    call make_symlink('../../installed/include', sdk//'/package')
    call write_text(project//'/main.c', '#include "version.h"'//nl// &
        '#include "package/value.h"'//nl// &
        'int main(void) { return SDK_VALUE == 19 ? 0 : 1; }'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(sdk_links LANGUAGES C)'//nl// &
        'include(CTest)'//nl// &
        'set(SDK_INCLUDE "'//sdk//'" CACHE PATH "headers")'//nl// &
        'file(READ "${SDK_INCLUDE}/version.h" version)'//nl// &
        'if(NOT version MATCHES "1\\.2\\.3")'//nl// &
        '  message(FATAL_ERROR "incorrect SDK version")'//nl// &
        'endif()'//nl// &
        'add_executable(probe main.c)'//nl// &
        'target_include_directories(probe PRIVATE "${SDK_INCLUDE}")'//nl// &
        'add_test(NAME sdk_value COMMAND probe)'//nl)
    backend = detect_backend(project)
    call cmake_capture_inventory(backend, inventory, ierr, message)
    call assert_true(ierr == 0, 'capture SDK links: '//trim(message))
    if (ierr == 0) then
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        context%input_inventory = inventory
        call generation_capture(project, state, context, generation, ierr, message)
        call assert_true(ierr == 0, 'capture resolved SDK contents: '//trim(message))
        if (ierr == 0) then
            call execution_view_create(scratch//'/views', generation%identity, &
                'sdk-link-oracle', 'sdk_value', generation%input_inventory, &
                .true., .true., view, ierr, message, &
                candidate_bundle_root=trim(generation%root)//'/bundle')
            call assert_true(ierr == 0, 'frozen SDK view: '//trim(message))
            if (ierr == 0) then
                call list_add(args, 'test')
                call list_add(args, 'sdk_value')
                call run_fo(driver, args, trim(view%cwd), cache, process, &
                    timeout_ms=30000)
                call assert_process_ok(process, &
                    'frozen version read and linked SDK compile')
                call write_text(installed//'/version.h', &
                    '#define SDK_VERSION "9.9.9"'//nl)
                call cmake_inputs_revalidate(trim(view%cwd), ierr, message)
                call assert_true(ierr /= 0, 'file-link target edit invalidates SDK')
                call assert_contains(trim(message), 'headers changed', &
                    'file edit diagnostic')
                call write_text(installed//'/version.h', &
                    '#define SDK_VERSION "1.2.3"'//nl)
                call write_text(installed//'/value.h', '#define SDK_VALUE 31'//nl)
                call list_add(cached_environment, 'FO_TEST_RERUN=0')
                call run_fo(driver, args, trim(view%cwd), cache, process, &
                    extra_environment=cached_environment, timeout_ms=30000)
                call assert_true(process%exit_code /= 0, &
                    'directory-link edit rejects cached PASS')
                call assert_contains(process%stdout//process%stderr, &
                    'headers changed', 'directory edit diagnostic')
                call write_text(installed//'/value.h', '#define SDK_VALUE 19'//nl)
                call execution_view_release(view, .false., cleanup, message)
                call assert_true(cleanup == 0, 'release frozen SDK view')
            end if
        end if
        call cmake_capture_cleanup(inventory, cleanup)
        call assert_true(cleanup == 0, 'release SDK capture metadata')
    end if
    call make_symlink('.', sdk//'/cycle')
    call cmake_capture_inventory(backend, rejected, ierr, message)
    call assert_true(ierr /= 0, 'cyclic SDK directory link is rejected')
    call fs_remove_file(sdk//'/cycle')
    call make_directory(project//'/local')
    call make_symlink(installed//'/version.h', project//'/local/version.h')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(strict_headers LANGUAGES C)'//nl// &
        'add_executable(probe main.c local/version.h)'//nl// &
        'target_include_directories(probe PRIVATE "${CMAKE_SOURCE_DIR}/local" "'// &
        sdk//'")'//nl)
    call cmake_capture_inventory(backend, rejected, ierr, message)
    call assert_true(ierr /= 0, 'referenced project header still rejects escaping link')
    call cmake_capture_cleanup(rejected, cleanup)
    call make_directory(scratch//'/provider/custom')
    call write_text(scratch//'/provider/CMakeCache.txt', '')
    call make_symlink(installed//'/version.h', scratch//'/provider/custom/version.h')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(strict_provider LANGUAGES C)'//nl// &
        'set(TOY_BUILD "'//scratch//'/provider" CACHE PATH "provider")'//nl// &
        'add_executable(probe main.c "${TOY_BUILD}/custom/version.h")'//nl// &
        'target_include_directories(probe PRIVATE "${TOY_BUILD}/custom" "'// &
        sdk//'")'//nl)
    call cmake_capture_inventory(backend, rejected, ierr, message)
    call assert_true(ierr /= 0, &
        'referenced provider header still rejects escaping link')
    call cmake_capture_cleanup(rejected, cleanup)
    call write_text(installed//'/escaped.c', 'int escaped(void) { return 0; }'//nl)
    call make_symlink(installed//'/escaped.c', project//'/escaped.c')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(strict_source LANGUAGES C)'//nl// &
        'add_executable(probe main.c escaped.c)'//nl// &
        'target_include_directories(probe PRIVATE "'//sdk//'")'//nl)
    call cmake_capture_inventory(backend, rejected, ierr, message)
    call assert_true(ierr /= 0, 'referenced project source still rejects escaping link')
    call cmake_capture_cleanup(rejected, cleanup)
    call finish_assertions(retain_failed_scratch=.true.)
end program test_cmake_sdk_header_links
