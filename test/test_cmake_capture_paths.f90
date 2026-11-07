program test_cmake_capture_paths
    use fo_build_backend, only: backend_t
    use fo_cmake_context, only: cmake_context_init
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t, alias_root
    use fo_test_harness, only: make_scratch, make_directory, write_text, remove_tree, &
        read_text
    use fo_test_harness, only: assert_true, assert_contains, assert_equal_integer, &
                               finish_assertions
    implicit none
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    character(:), allocatable :: scratch, reply, path
    character(len=4096) :: message
    integer :: ierr, cleanup_status, i
    character, parameter :: nl = new_line('a')

    call make_scratch('fo-cmake-capture-paths', scratch)
    backend%project_dir = scratch
    call cmake_context_init(backend%cmake, scratch, build_root='build')
    reply = scratch//'/build/.cmake/api/v1/reply'
    call make_directory(reply)
    call write_text(scratch//'/CMakeLists.txt', &
                    'cmake_minimum_required(VERSION 3.20)'//nl)
    call write_text(scratch//'/build/CMakeCache.txt', &
                    'CMAKE_GENERATOR:INTERNAL=Ninja'//nl// &
                    'CMAKE_ROOT:INTERNAL=/opt/homebrew/share/cmake'//nl// &
                    'CMAKE_TOOLCHAIN_FILE:FILEPATH=/opt/toolchain/compiler.cmake'//nl)

    ! Exercise actual File API capture without requiring write access to /opt.
    ! Authored paths there, including runner workspaces, are not toolchain files.
    do i = 1, 3
        select case (i)
        case (1)
            path = '/opt/runner/_work/project/outside.f90'
        case (2)
            path = '/usr/local/project/outside.f90'
        case (3)
            path = '/opt/homebrew/share/cmake/Modules/CMakeFortranInformation.cmake'
        end select
        call write_text(reply//'/target-fixture.json', &
                        '{"sources":[{"path":"'//path//'"}]}')
        call cmake_capture_inventory(backend, inventory, ierr, message, &
                                     configured=.true.)
        call assert_true(ierr /= 0, 'external authored target source is rejected')
        call assert_contains(message, 'unsupported uncaptured external CMake input', &
                             'rejection happens during capture')
        call assert_contains(message, path, 'diagnostic identifies authored input')
    end do
    call write_text(reply//'/target-fixture.json', '{"sources":[]}')
    path = '/custom/cmake/Modules/CMakeFortranInformation.cmake'
    call write_text(reply//'/cmakeFiles-fixture.json', &
                    '{"inputs":[{"path":"'//path//'","isCMake":true}]}')
    call cmake_capture_inventory(backend, inventory, ierr, message, &
                                 configured=.true.)
    call assert_equal_integer(ierr, 0, 'CMake input stays delegated: '//trim(message))
    if (ierr == 0) call cmake_capture_cleanup(inventory, cleanup_status)
    call write_text(reply//'/cmakeFiles-fixture.json', &
                    '{"inputs":[{"path":"/opt/toolchain/compiler.cmake"}]}')
    call cmake_capture_inventory(backend, inventory, ierr, message, &
                                 configured=.true.)
    call assert_equal_integer(ierr, 0, 'Exact toolchain delegated: '//trim(message))
    if (ierr == 0) call cmake_capture_cleanup(inventory, cleanup_status)
    call write_text(reply//'/cmakeFiles-fixture.json', &
                    '{"inputs":[{"path":"/opt/homebrew/share/cmake/Templates/'// &
                    'CTestScript.cmake.in"}]}')
    call cmake_capture_inventory(backend, inventory, ierr, message, &
                                 configured=.true.)
    call assert_equal_integer(ierr, 0, 'CMake template delegated: '//trim(message))
    if (ierr == 0) call cmake_capture_cleanup(inventory, cleanup_status)
    call write_text(reply//'/cmakeFiles-fixture.json', &
                    '{"inputs":[{"path":"'//path//'","isCMake":true},'// &
                    '{"path":"/opt/toolchain/compiler.cmake"},'// &
                    '{"path":"/opt/toolchain/custom.cmake"}]}')
    call cmake_capture_inventory(backend, inventory, ierr, message, &
                                 configured=.true.)
    call assert_true(ierr /= 0, 'authored CMake input after a system input is rejected')
    call assert_contains(message, '/opt/toolchain/custom.cmake', &
                         'CMake ownership never leaks to sibling inputs')
    call check_sdk_framework()
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine check_sdk_framework()
        !! FindBLAS on macOS links Accelerate from the SDK as a framework.
        character(:), allocatable :: sdk, framework, loose, digest
        integer :: metadata

        call make_scratch('fo-cmake-capture-sdk', sdk)
        framework = sdk//'/MacOSX.sdk/System/Library/Frameworks/Accelerate.framework'
        call make_directory(framework//'/Headers')
        call write_text(sdk//'/MacOSX.sdk/SDKSettings.json', '{"Version":"26.5"}')
        call write_text(reply//'/cmakeFiles-fixture.json', '{"inputs":[]}')
        call write_text(reply//'/target-fixture.json', &
            '{"compileGroups":[{"frameworks":[{"path":"'// &
            framework//'"}]}]}')
        call cmake_capture_inventory(backend, inventory, ierr, message, &
            configured=.true.)
        call assert_equal_integer(ierr, 0, 'SDK framework delegated: '//trim(message))
        if (ierr /= 0) return
        metadata = alias_root(inventory, 'cmake-context')
        call assert_contains(read_text(trim(inventory%roots(metadata)%physical_path)// &
            '/sdk-frameworks.txt'), '26.5', 'SDK version is recorded')
        digest = inventory%digest
        call cmake_capture_cleanup(inventory, cleanup_status)
        call write_text(sdk//'/MacOSX.sdk/SDKSettings.json', '{"Version":"26.6"}')
        call cmake_capture_inventory(backend, inventory, ierr, message, &
            configured=.true.)
        call assert_equal_integer(ierr, 0, 'updated SDK framework: '//trim(message))
        if (ierr /= 0) return
        call assert_true(inventory%digest /= digest, 'SDK version changes the generation')
        call cmake_capture_cleanup(inventory, cleanup_status)

        ! A framework outside an SDK is an authored input, not a system library.
        loose = sdk//'/Frameworks/Local.framework'
        call make_directory(loose)
        call write_text(reply//'/target-fixture.json', &
            '{"compileGroups":[{"frameworks":[{"path":"'//loose//'"}]}]}')
        call cmake_capture_inventory(backend, inventory, ierr, message, &
            configured=.true.)
        call assert_true(ierr /= 0, 'framework outside an SDK is rejected')
        call assert_contains(message, 'Local.framework', 'rejected framework is named')
        call remove_tree(sdk)
    end subroutine check_sdk_framework
end program test_cmake_capture_paths
