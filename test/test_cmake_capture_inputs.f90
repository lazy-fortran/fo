program test_cmake_capture_inputs
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: make_directory, write_text, read_text, &
        assert_true, assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add, make_symlink, &
        current_directory, run_process, environment_value, assert_equal_string
    use fo_test_gremlin_oracle, only: gremlin_setup
    use fo_test_cli, only: run_fo
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t, input_inventory_revalidate, &
        alias_root
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_cache, only: cache_file_digest
    use fo_fs, only: fs_remove_tree, fs_remove_file, fs_find_executable
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, sdk, cwd
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
    logical :: header_found, generated_found, present, found
    character(:), allocatable :: compiler_args, original_args
    character(len=4096) :: compiler_paths(3)
    character(len=8), parameter :: compiler_tools(3) = &
        [character(len=8) :: 'gcc', 'g++', 'gfortran']
    character(len=7), parameter :: languages(3) = &
        [character(len=7) :: 'C', 'CXX', 'Fortran']
    interface
        integer(c_int) function setenv(name, value, overwrite) &
                bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function setenv
        integer(c_int) function chdir(path) bind(C, name='fo_test_chdir')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function chdir
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    sdk = scratch//'/sdk/include'
    call make_directory(sdk//'/nested')
    call write_text(sdk//'/nested/value.h', '#define VALUE 19'//nl)
    call make_symlink(sdk, scratch//'/include-alias')
    call make_directory(project//'/.venv/include')
    call make_directory(project//'/include')
    call make_directory(project//'/optional')
    call write_text(project//'/.venv/include/local.h', '#define LOCAL 3'//nl)
    call write_text(project//'/include/public.h', '#define PUBLIC 11'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(capture_inputs LANGUAGES C CXX Fortran)'//nl// &
        'set_property(CACHE CMAKE_C_COMPILER PROPERTY HELPSTRING '// &
        '"authored compiler help")'//nl// &
        'include(CTest)'//nl// &
        'include(GNUInstallDirs)'//nl// &
        'set(SDK_INCLUDE "'//scratch//'/include-alias" CACHE INTERNAL "headers")'//nl// &
        'set(LOCAL_INCLUDE "${CMAKE_SOURCE_DIR}/.venv/include" CACHE PATH "local")'//nl// &
        'add_library(headers INTERFACE)'//nl// &
        'target_include_directories(headers INTERFACE '// &
        '"$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")'//nl// &
        'install(TARGETS headers EXPORT headers)'//nl// &
        'install(EXPORT headers DESTINATION lib/cmake/headers)'//nl// &
        'install(FILES .venv/include/local.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})'//nl// &
        'configure_file(seed.in "${CMAKE_CURRENT_SOURCE_DIR}/generated.h" @ONLY)'//nl// &
        'add_executable(probe main.c)'//nl// &
        'target_include_directories(probe PRIVATE '// &
        '"${CMAKE_SOURCE_DIR}/optional/include" '// &
        '"${CMAKE_SOURCE_DIR}/include" '// &
        '"${SDK_INCLUDE}" "${LOCAL_INCLUDE}")'//nl// &
        'add_test(NAME header_value COMMAND probe)'//nl)
    call write_text(project//'/seed.in', '#define GENERATED 7'//nl)
    call write_text(project//'/main.c', &
        '#include "nested/value.h"'//nl//'#include "generated.h"'//nl// &
        '#include "local.h"'//nl//'#include "public.h"'//nl// &
        'int main(void) { return VALUE == 19 && GENERATED == 7 && '// &
        'LOCAL == 3 && PUBLIC == 11 ? 0 : 1; }'//nl)
    original_args = environment_value('FO_CMAKE_ARGS')
    compiler_args = ''
    do i = 1, size(compiler_tools)
        call fs_find_executable(trim(compiler_tools(i)), compiler_paths(i), found)
        call assert_true(found, 'native '//trim(compiler_tools(i))//' is available')
        if (.not. found) call finish_assertions()
        compiler_args = compiler_args//' -DCMAKE_'//trim(languages(i))// &
            '_COMPILER="'//trim(compiler_paths(i))//'"'
    end do
    cleanup_status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
        compiler_args//c_null_char, 1_c_int))
    call assert_true(cleanup_status == 0, 'untyped compiler arguments are selected')
    backend = detect_backend(project)
    call current_directory(cwd)
    ierr = int(chdir(project//c_null_char))
    call assert_true(ierr == 0, 'capture from the configured project directory')
    call cmake_capture_inventory(backend, inventory, ierr, message)
    cleanup_status = int(chdir(cwd//c_null_char))
    call assert_true(cleanup_status == 0, 'restore the fixture working directory')
    call assert_true(ierr == 0, 'capture external headers: '//trim(message))
    if (ierr == 0) then
        call install_headers(project, 'live')
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
        if (ierr == 0) call repeated_compiler_capture()
        call make_symlink(project//'/unavailable', project//'/optional/include')
        call input_inventory_revalidate(project, inventory%declarations, inventory, &
            ierr, message)
        call assert_true(ierr /= 0, 'materialized dangling include link is rejected')
        call assert_contains(trim(message), 'include-search path', &
            'retained absent include detects dangling link during rediscovery')
        call fs_remove_file(project//'/optional/include')
        call make_symlink(sdk, project//'/optional/include')
        call input_inventory_revalidate(project, inventory%declarations, inventory, &
            ierr, message)
        call assert_true(ierr /= 0, 'materialized directory include link is rejected')
        call assert_contains(trim(message), 'include-search path', &
            'retained absent include detects directory alias during rediscovery')
        call fs_remove_file(project//'/optional/include')
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
        inquire(file=trim(view%cwd)//'/optional/include', exist=present)
        call assert_true(.not. present, 'absent include stays absent in relocated view')
        call make_directory(project//'/optional/include')
        call write_text(project//'/optional/include/local.h', '#define LOCAL 99'//nl)
        call run_fo(driver, args, trim(view%cwd), cache, process, &
            timeout_ms=30000)
        call assert_process_ok(process, &
            'relocated build ignores later live include directory')
        call install_headers(trim(view%cwd), 'frozen')
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
        call fs_remove_tree(project//'/optional/include', cleanup_status)
        call assert_true(cleanup_status == 0, 'live optional include fixture cleaned')
        call write_text(project//'/generated.h', '#define GENERATED 9'//nl)
        call input_inventory_revalidate(project, inventory%declarations, inventory, &
            ierr, message)
        call assert_true(ierr /= 0, 'later source output edit is rejected')
        call assert_contains(trim(message), 'project:generated.h', &
            'changed source output named')
    end if
    call cmake_capture_cleanup(inventory, cleanup_status)
    call assert_true(cleanup_status == 0, 'owned capture metadata cleaned')
    cleanup_status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
        original_args//c_null_char, 1_c_int))
    call assert_true(cleanup_status == 0, 'original configure arguments are restored')
    call make_symlink(project//'/unavailable', project//'/dangling-include')
    call reject_include(project//'/dangling-include', 'dangling include link')
    call make_directory(project//'/ordinary')
    call make_symlink(project//'/ordinary', project//'/include-alias')
    call reject_include(project//'/include-alias/absent', &
        'absent include below a directory alias')
    call reject_include(scratch//'/undeclared-absent', 'absent external include')
    call reject_include(project//'/.fo-cmake/absent', 'absent capture-output include')
    call finish_assertions(retain_failed_scratch=.true.)
contains
    subroutine repeated_compiler_capture()
        type(input_inventory_t) :: repeated
        type(generation_t) :: captured
        character(:), allocatable :: raw_cache, frozen_cache
        integer :: j, metadata_root

        raw_cache = read_text(project//'/'//backend%cmake%build_root//'/CMakeCache.txt')
        do j = 1, size(languages)
            call assert_contains(raw_cache, 'CMAKE_'//trim(languages(j))// &
                '_COMPILER:STRING='//trim(compiler_paths(j)), &
                'first native configure types the explicit compiler')
        end do
        call cmake_capture_inventory(backend, repeated, ierr, message)
        cleanup_status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
            original_args//c_null_char, 1_c_int))
        call assert_true(cleanup_status == 0, 'frozen replay uses captured compilers')
        call assert_true(ierr == 0, 'repeat native configure/capture: '//trim(message))
        if (ierr /= 0) return
        raw_cache = read_text(project//'/'//backend%cmake%build_root//'/CMakeCache.txt')
        metadata_root = alias_root(repeated, 'cmake-context')
        call assert_true(metadata_root > 0, 'repeat capture has frozen metadata')
        if (metadata_root > 0) then
            frozen_cache = read_text(trim(repeated%roots(metadata_root)%physical_path)// &
                '/cache.cmake')
            do j = 1, size(languages)
                call assert_contains(raw_cache, 'CMAKE_'//trim(languages(j))// &
                    '_COMPILER:UNINITIALIZED='//trim(compiler_paths(j)), &
                    'repeat native configure leaves the same override untyped')
                call assert_contains(frozen_cache, 'set(CMAKE_'//trim(languages(j))// &
                    '_COMPILER "'//trim(compiler_paths(j))//'" CACHE STRING ', &
                    'repeat frozen cache retains each valid compiler value')
            end do
            call assert_contains(frozen_cache, 'set(CMAKE_C_COMPILER "'// &
                trim(compiler_paths(1))//'" CACHE STRING "authored compiler help"', &
                'custom compiler cache documentation is preserved')
        end if
        context%input_inventory = repeated
        call generation_capture(project, state, context, captured, ierr, message)
        call assert_true(ierr == 0, 'repeat configured generation: '//trim(message))
        if (ierr == 0) call assert_equal_string(captured%identity, generation%identity, &
            'untyped compiler overrides retain one frozen generation')
        call cmake_capture_cleanup(repeated, cleanup_status)
        call assert_true(cleanup_status == 0, 'repeat capture metadata cleaned')
    end subroutine repeated_compiler_capture

    subroutine install_headers(source, label)
        character(len=*), intent(in) :: source, label
        type(string_list_t) :: command
        type(process_result_t) :: result
        character(:), allocatable :: prefix

        prefix = scratch//'/'//label//'-install'
        call list_add(command, 'cmake')
        call list_add(command, '--install')
        call list_add(command, source//'/'//backend%cmake%build_root)
        call list_add(command, '--prefix')
        call list_add(command, prefix)
        call run_process(command, source, result, timeout_ms=30000)
        call assert_process_ok(result, label//' native install succeeds')
        call assert_contains(read_text(prefix//'/include/local.h'), &
            '#define LOCAL 3', label//' header uses the relative include destination')
        call assert_contains(read_text(prefix//'/lib/cmake/headers/headers.cmake'), &
            '${_IMPORT_PREFIX}/include', label//' export remains prefix-relative')
    end subroutine install_headers

    subroutine reject_include(path, label)
        character(len=*), intent(in) :: path, label
        character(len=:), allocatable :: authored
        type(input_inventory_t) :: rejected

        authored = read_text(project//'/CMakeLists.txt')
        call write_text(project//'/CMakeLists.txt', authored// &
            'target_include_directories(probe PRIVATE "'//path//'" )'//nl)
        backend = detect_backend(project)
        call cmake_capture_inventory(backend, rejected, ierr, message)
        call assert_true(ierr /= 0, label//' is rejected')
        call cmake_capture_cleanup(rejected, cleanup_status)
        call assert_true(cleanup_status == 0, label//' capture metadata cleaned')
        call write_text(project//'/CMakeLists.txt', authored)
    end subroutine reject_include
end program test_cmake_capture_inputs
