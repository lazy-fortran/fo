program test_cmake_capture_inputs
    use fo_test_harness, only: make_directory, write_text, &
        assert_true, assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add, make_symlink
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
    character(:), allocatable :: driver, scratch, project, cache, state, sdk
    character(len=*), parameter :: nl = new_line('a')
    character(len=4096) :: message
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    type(generation_context_t) :: context
    type(generation_t) :: generation
    type(execution_view_t) :: view
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    integer :: ierr, cleanup_status, i
    logical :: header_found, generated_found

    call gremlin_setup(driver, scratch, project, cache, state)
    sdk = scratch//'/sdk/include'
    call make_directory(sdk//'/nested')
    call write_text(sdk//'/nested/value.h', '#define VALUE 19'//nl)
    call make_symlink(sdk, scratch//'/include-alias')
    call make_directory(project//'/.venv/include')
    call write_text(project//'/.venv/include/local.h', '#define LOCAL 3'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(capture_inputs LANGUAGES C)'//nl// &
        'include(CTest)'//nl// &
        'set(SDK_INCLUDE "'//scratch//'/include-alias" CACHE INTERNAL "headers")'//nl// &
        'set(LOCAL_INCLUDE "${CMAKE_SOURCE_DIR}/.venv/include" CACHE PATH "local")'//nl// &
        'configure_file(seed.in "${CMAKE_CURRENT_SOURCE_DIR}/generated.h" @ONLY)'//nl// &
        'add_executable(probe main.c)'//nl// &
        'target_include_directories(probe PRIVATE "${SDK_INCLUDE}" "${LOCAL_INCLUDE}")'//nl// &
        'add_test(NAME header_value COMMAND probe)'//nl)
    call write_text(project//'/seed.in', '#define GENERATED 7'//nl)
    call write_text(project//'/main.c', &
        '#include "nested/value.h"'//nl//'#include "generated.h"'//nl// &
        '#include "local.h"'//nl// &
        'int main(void) { return VALUE == 19 && GENERATED == 7 && LOCAL == 3 ? 0 : 1; }'//nl)
    backend = detect_backend(project)
    call cmake_capture_inventory(backend, inventory, ierr, message)
    call assert_true(ierr == 0, 'capture external headers: '//trim(message))
    if (ierr == 0) then
        header_found = .false.
        generated_found = .false.
        do i = 1, inventory%entry_count
            associate(entry => inventory%entries(i))
                if (entry%relative_path == 'nested/value.h') then
                    header_found = len_trim(entry%content_digest) == 64
                end if
                if (entry%root_alias == 'project' .and. &
                    entry%relative_path == 'generated.h') generated_found = .true.
            end associate
        end do
        call assert_true(header_found, 'referenced external header is fingerprinted')
        call assert_true(generated_found, 'configure output in source is captured')
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        context%input_inventory = inventory
        call generation_capture(project, state, context, generation, ierr, message)
        call assert_true(ierr == 0, 'stable configured inventory: '//trim(message))
        call write_text(sdk//'/nested/value.h', '#define VALUE 31'//nl)
        call input_inventory_revalidate(project, inventory%declarations, inventory, &
            ierr, message)
        call assert_true(ierr /= 0, 'changed external header is rejected')
        call assert_contains(trim(message), 'nested/value.h', 'changed header named')
        call execution_view_create(scratch//'/views', generation%identity, &
            'capture-input-oracle', 'header_value', generation%input_inventory, &
            .true., .true., view, ierr, message, &
            candidate_bundle_root=trim(generation%root)//'/bundle')
        call assert_true(ierr == 0, 'private frozen view: '//trim(message))
        call list_add(args, 'test')
        call list_add(args, 'header_value')
        call run_fo(driver, args, trim(view%cwd), cache, process, &
            timeout_ms=30000)
        call assert_true(process%exit_code /= 0, 'forced live SDK edit is rejected')
        call assert_contains(process%stderr//process%stdout, &
            'captured CMake headers changed', 'forced SDK mismatch is explicit')
        call write_text(sdk//'/nested/value.h', '#define VALUE 19'//nl)
        call run_fo(driver, args, trim(view%cwd), cache, process, &
            timeout_ms=30000)
        call assert_process_ok(process, 'matching recorded SDK builds and tests')
        call write_text(sdk//'/nested/value.h', '#define VALUE 31'//nl)
        call list_add(environment, 'FO_TEST_RERUN=0')
        call run_fo(driver, args, trim(view%cwd), cache, process, &
            extra_environment=environment, timeout_ms=30000)
        call assert_true(process%exit_code /= 0, 'cached PASS cannot hide SDK edit')
        call assert_contains(process%stderr//process%stdout, &
            'captured CMake headers changed', 'cached SDK mismatch is explicit')
        call write_text(sdk//'/nested/value.h', '#define VALUE 19'//nl)
        call execution_view_release(view, .false., cleanup_status, message)
        call assert_true(cleanup_status == 0, 'private view cleaned')
        call write_text(project//'/generated.h', '#define GENERATED 9'//nl)
        call input_inventory_revalidate(project, inventory%declarations, inventory, &
            ierr, message)
        call assert_true(ierr /= 0, 'later source output edit is rejected')
        call assert_contains(trim(message), 'project:generated.h', &
            'changed source output named')
    end if
    call cmake_capture_cleanup(inventory, cleanup_status)
    call assert_true(cleanup_status == 0, 'owned capture metadata cleaned')
    call finish_assertions()
end program test_cmake_capture_inputs
