program test_cmake_capture_inputs
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: make_directory, write_text, read_text, &
        assert_true, assert_contains, assert_process_ok, finish_assertions, &
        string_list_t, process_result_t, list_add, make_symlink, &
        current_directory, run_process, environment_value, assert_equal_string, run_git
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
    logical :: implicit_found, declared_found, child_found
    character(:), allocatable :: compiler_args, original_args, original_toolchain
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
    call make_directory(scratch//'/compiler/include/package')
    call make_directory(scratch//'/declared/include')
    call write_text(scratch//'/compiler/include/implicit.h', '#define IMPLICIT 5'//nl)
    call write_text(scratch//'/compiler/include/package/child.h', '#define CHILD 6'//nl)
    call write_text(scratch//'/declared/include/declared.h', '#define DECLARED 8'//nl)
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
        'set(DECLARED_SOURCE_DIR "'//scratch//'/declared" CACHE PATH "source")'//nl// &
        'list(APPEND CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES "'// &
        scratch//'/compiler/include" "${DECLARED_SOURCE_DIR}/include")'//nl// &
        'set(CHILD_INCLUDE "'//scratch// &
        '/compiler/include/package" CACHE PATH "headers")'//nl// &
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
        'target_include_directories(probe SYSTEM PRIVATE "'// &
        scratch//'/compiler/include" "${DECLARED_SOURCE_DIR}/include" '// &
        '"${CHILD_INCLUDE}" "${SDK_INCLUDE}")'//nl// &
        'add_test(NAME header_value COMMAND probe)'//nl)
    call write_text(project//'/seed.in', '#define GENERATED 7'//nl)
    call write_text(project//'/main.c', &
        '#include "nested/value.h"'//nl//'#include "generated.h"'//nl// &
        '#include "local.h"'//nl//'#include "public.h"'//nl// &
        '#include "declared.h"'//nl//'#include "child.h"'//nl// &
        'int main(void) { return VALUE == 19 && GENERATED == 7 && '// &
        'LOCAL == 3 && PUBLIC == 11 && DECLARED == 8 && CHILD == 6 ? 0 : 1; }'//nl)
    call initialize_project_git()
    original_args = environment_value('FO_CMAKE_ARGS')
    original_toolchain = environment_value('CMAKE_TOOLCHAIN_FILE')
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
    ! This fixture selects its compilers explicitly; an inherited toolchain
    ! must not replace them before CMake initializes their cache entries.
    cleanup_status = int(setenv('CMAKE_TOOLCHAIN_FILE'//c_null_char, &
        c_null_char, 1_c_int))
    call assert_true(cleanup_status == 0, 'inherited compiler toolchain is isolated')
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
        implicit_found = .false.
        declared_found = .false.
        child_found = .false.
        do i = 1, inventory%entry_count
            associate(entry => inventory%entries(i))
                if (entry%relative_path == 'nested/value.h') then
                    header_found = len_trim(entry%content_digest) == 64
                end if
                if (entry%relative_path == 'implicit.h') implicit_found = .true.
                if (entry%relative_path == 'include/declared.h') &
                    declared_found = len_trim(entry%content_digest) == 64
                if (entry%relative_path == 'child.h') &
                    child_found = len_trim(entry%content_digest) == 64
                if (entry%root_alias == 'project' .and. &
                    entry%relative_path == 'generated.h') generated_found = .true.
            end associate
        end do
        call assert_true(.not. implicit_found, &
            'native compiler implicit include stays delegated')
        call assert_true(declared_found, &
            'declared SYSTEM source overrides compiler implicit delegation')
        call assert_true(child_found, &
            'nonimplicit child of compiler include stays captured')
        call assert_true(header_found, 'referenced external SYSTEM header is fingerprinted')
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
        call write_text(scratch//'/declared/include/declared.h', &
            '#define DECLARED 9'//nl)
        call input_inventory_revalidate(project, inventory%declarations, inventory, &
            ierr, message)
        call assert_true(ierr /= 0, 'changed declared implicit header is rejected')
        call assert_contains(trim(message), 'include/declared.h', &
            'changed declared implicit header is named')
        call write_text(scratch//'/declared/include/declared.h', &
            '#define DECLARED 8'//nl)
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
    call verify_native_library_alias()
    cleanup_status = int(setenv('CMAKE_TOOLCHAIN_FILE'//c_null_char, &
        original_toolchain//c_null_char, 1_c_int))
    call assert_true(cleanup_status == 0, 'inherited toolchain is restored')
    call assert_equal_string(environment_value('CMAKE_TOOLCHAIN_FILE'), &
        original_toolchain, 'fixture preserves the inherited toolchain selection')
    call finish_assertions(retain_failed_scratch=.true.)
contains
    subroutine verify_native_library_alias()
        type(backend_t) :: library_backend
        type(input_inventory_t) :: captured, nonimplicit, declared
        type(generation_context_t) :: library_context
        type(generation_t) :: library_generation
        type(execution_view_t) :: library_view
        type(string_list_t) :: command, selected
        type(process_result_t) :: result
        character(:), allocatable :: root, native_sdk, library, frozen, declared_bundle
        integer :: status, metadata, source_root, alias_index

        root = scratch//'/library-project'
        native_sdk = scratch//'/library-sdk'
        library = native_sdk//'/lib/libtoy.a'
        call make_directory(root)
        call make_directory(native_sdk//'/include')
        call make_directory(native_sdk//'/libsrc')
        call write_text(native_sdk//'/include/toy.h', 'int toy_value(void);'//nl)
        call write_text(native_sdk//'/libsrc/toy.c', &
            'int toy_value(void) { return 37; }'//nl)
        call write_text(native_sdk//'/libsrc/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.22)'//nl// &
            'project(toy_sdk LANGUAGES C)'//nl// &
            'add_library(toy STATIC toy.c)'//nl)
        call list_add(command, 'cmake')
        call list_add(command, '-S')
        call list_add(command, native_sdk//'/libsrc')
        call list_add(command, '-B')
        call list_add(command, scratch//'/library-build')
        call list_add(command, '-DCMAKE_C_COMPILER='//trim(compiler_paths(1)))
        call list_add(command, '-DCMAKE_ARCHIVE_OUTPUT_DIRECTORY='//native_sdk//'/lib')
        call run_process(command, root, result, timeout_ms=30000)
        call assert_process_ok(result, 'native toy SDK configures')
        command = string_list_t()
        call list_add(command, 'cmake')
        call list_add(command, '--build')
        call list_add(command, scratch//'/library-build')
        call list_add(command, '--parallel')
        call list_add(command, '1')
        call run_process(command, root, result, timeout_ms=30000)
        call assert_process_ok(result, 'native toy SDK archive builds')
        call make_symlink('../lib/libtoy.a', native_sdk//'/include/libtoy.a')
        call write_text(root//'/main.c', '#include "toy.h"'//nl// &
            'int main(void) { return toy_value() == 37 ? 0 : 1; }'//nl)
        call write_text(root//'/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.22)'//nl// &
            'project(native_library LANGUAGES C)'//nl// &
            'include(CTest)'//nl// &
            '# Explicit synthetic SDK role, not a discovered host system path.'//nl// &
            'option(TOY_NATIVE_SDK "Declare the synthetic SDK role" ON)'//nl// &
            'if(TOY_NATIVE_SDK)'//nl// &
            'list(APPEND CMAKE_C_IMPLICIT_LINK_DIRECTORIES "'// &
            native_sdk//'/lib")'//nl// &
            'endif()'//nl// &
            'set(TOY_LIBRARY "'//native_sdk// &
            '/include/libtoy.a" CACHE FILEPATH "toy")'//nl// &
            'add_executable(probe main.c)'//nl// &
            'target_include_directories(probe PRIVATE "'// &
            native_sdk//'/include")'//nl// &
            'target_link_libraries(probe PRIVATE "${TOY_LIBRARY}")'//nl// &
            'add_test(NAME native_library_value COMMAND probe)'//nl)
        status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
            compiler_args//c_null_char, 1_c_int))
        call assert_true(status == 0, 'native library fixture compilers selected')
        library_backend = detect_backend(root)
        call cmake_capture_inventory(library_backend, captured, status, message)
        call assert_true(status == 0, &
            'native library fixture captures: '//trim(message))
        if (status /= 0) return
        metadata = alias_root(captured, 'cmake-context')
        frozen = read_text(trim(captured%roots(metadata)%physical_path)//'/cache.cmake')
        call assert_contains(frozen, 'set(TOY_LIBRARY "'//library//'" CACHE FILEPATH', &
            'native SDK library alias keeps its canonical target')
        library_context%driver_path = driver
        call cache_file_digest(driver, library_context%driver_digest)
        inquire(file=driver, size=library_context%driver_size)
        library_context%input_inventory = captured
        call generation_capture(root, state, library_context, library_generation, &
            status, message)
        call assert_true(status == 0, 'native library generation: '//trim(message))
        if (status == 0) then
            call execution_view_create(scratch//'/library-views', &
                library_generation%identity, 'native-library-oracle', &
                'native_library_value', library_generation%input_inventory, &
                .true., .true., library_view, status, message, &
                candidate_bundle_root=trim(library_generation%root)//'/bundle')
            call assert_true(status == 0, 'native library frozen view exists')
            if (status == 0) then
                call list_add(selected, 'test')
                call list_add(selected, 'native_library_value')
                call run_fo(driver, selected, trim(library_view%cwd), cache, result, &
                    timeout_ms=30000)
                call assert_process_ok(result, &
                    'frozen native SDK library links and runs')
                call execution_view_release(library_view, .false., status, message)
                call assert_true(status == 0, 'native library view released')
            end if
        end if
        status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
            (compiler_args//' -DTOY_NATIVE_SDK=OFF')//c_null_char, 1_c_int))
        call assert_true(status == 0, 'synthetic native SDK role is disabled')
        library_backend = detect_backend(root)
        call cmake_capture_inventory(library_backend, nonimplicit, status, message)
        call assert_true(status == 0, &
            'nonimplicit library fixture captures: '//trim(message))
        if (status == 0) then
            metadata = alias_root(nonimplicit, 'cmake-context')
            frozen = read_text(trim(nonimplicit%roots(metadata)%physical_path)// &
                '/cache.cmake')
            call assert_contains(frozen, 'set(TOY_LIBRARY "'// &
                '${CMAKE_CURRENT_LIST_DIR}/../../project/.fo-cmake/'// &
                'dependencies/include-', &
                'nonimplicit library keeps ordinary frozen include relocation')
            call assert_contains(frozen, '/libtoy.a" CACHE FILEPATH', &
                'nonimplicit library retains its selected alias operand')
            call assert_true(index(frozen, &
                'set(TOY_LIBRARY "'//library//'" CACHE FILEPATH') == 0, &
                'nonimplicit library is not delegated as a native SDK')
        end if
        call cmake_capture_cleanup(nonimplicit, status)
        call assert_true(status == 0, 'nonimplicit library metadata cleaned')
        status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
            (compiler_args//' -DTOY_NATIVE_SDK=ON -DTOY_SOURCE_DIR="'// &
            native_sdk//'"')//c_null_char, 1_c_int))
        call assert_true(status == 0, 'explicit source ownership is selected')
        library_backend = detect_backend(root)
        call cmake_capture_inventory(library_backend, declared, status, message)
        call assert_true(status == 0, &
            'declared library source captures: '//trim(message))
        if (status == 0) then
            metadata = alias_root(declared, 'cmake-context')
            frozen = read_text(trim(declared%roots(metadata)%physical_path)// &
                '/cache.cmake')
            source_root = alias_root(declared, 'cmake-dependency:TOY')
            call assert_true(source_root > 0, 'declared library source root exists')
            declared_bundle = ''
            if (source_root > 0) then
                call assert_equal_string( &
                    trim(declared%roots(source_root)%physical_path), native_sdk, &
                    'declared library root retains its native source identity')
                do alias_index = 1, declared%roots(source_root)%alias_count
                    if (trim(declared%roots(source_root)%aliases(alias_index)) /= &
                        'cmake-dependency:TOY') cycle
                    declared_bundle = &
                        trim(declared%roots(source_root)%bundle_paths(alias_index))
                    exit
                end do
            end if
            call assert_true(len(declared_bundle) > 0, &
                'declared library source has a mapped bundle path')
            if (len(declared_bundle) > 0) then
                call assert_contains(frozen, 'set(TOY_LIBRARY "'// &
                    '${CMAKE_CURRENT_LIST_DIR}/../../'//declared_bundle// &
                    '/include/libtoy.a" CACHE FILEPATH', &
                    'explicit source ownership wins over native SDK delegation')
            end if
            call assert_true(index(frozen, &
                'set(TOY_LIBRARY "'//library//'" CACHE FILEPATH') == 0, &
                'declared library is not delegated as a native SDK')
            call write_text(native_sdk//'/libsrc/toy.c', &
                'int toy_value(void) { return 38; }'//nl)
            call input_inventory_revalidate(root, declared%declarations, declared, &
                status, message)
            call assert_true(status /= 0, &
                'declared library source edit invalidates capture')
        end if
        call cmake_capture_cleanup(declared, status)
        call assert_true(status == 0, 'declared library metadata cleaned')
        call cmake_capture_cleanup(captured, status)
        call assert_true(status == 0, 'native library metadata cleaned')
        status = int(setenv('FO_CMAKE_ARGS'//c_null_char, &
            original_args//c_null_char, 1_c_int))
        call assert_true(status == 0, 'library fixture restores configure arguments')
    end subroutine verify_native_library_alias

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
        call repack_project_git('9', '50')
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
            'compiler overrides and Git storage retain one frozen generation')
        call cmake_capture_cleanup(repeated, cleanup_status)
        call assert_true(cleanup_status == 0, 'repeat capture metadata cleaned')
    end subroutine repeated_compiler_capture

    subroutine initialize_project_git()
        ! Related, compressible objects distinguish native packing layouts
        ! without changing any source bytes, refs or decoded Git objects.
        call write_text(project//'/packing-a.txt', repeat('synthetic payload '//nl, 256))
        call write_text(project//'/packing-b.txt', repeat('synthetic payload '//nl, 255)// &
            'different suffix'//nl)
        call fixture_git([character(len=16) :: 'init'], 'fixture Git initializes')
        call fixture_git([character(len=16) :: 'add', '.'], 'fixture files are staged')
        call fixture_git([character(len=32) :: 'commit', '-qm', 'synthetic project'], &
            'fixture source commit exists')
        call repack_project_git('1', '0')
    end subroutine initialize_project_git

    subroutine repack_project_git(compression, window)
        character(len=*), intent(in) :: compression, window
        character(:), allocatable :: before, after
        integer :: status

        call run_git(project, [character(len=16) :: 'show-ref', '--head'], status, before)
        call assert_true(status == 0, 'native refs are available before repacking')
        call fixture_git([character(len=32) :: 'config', 'pack.compression', &
            compression], 'select native transfer compression')
        call fixture_git([character(len=32) :: '-c', 'pack.compression='//compression, &
            'repack', '-a', '-d', '-f', '-F', '--window='//window, '--threads=1'], &
            'repack the synthetic source without changing its objects')
        call run_git(project, [character(len=16) :: 'show-ref', '--head'], status, after)
        call assert_true(status == 0, 'native refs are available after repacking')
        call assert_equal_string(after, before, 'native storage changes preserve refs')
    end subroutine repack_project_git

    subroutine fixture_git(arguments, label)
        character(len=*), intent(in) :: arguments(:), label
        integer :: status

        call run_git(project, arguments, status)
        call assert_true(status == 0, label)
    end subroutine fixture_git

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
