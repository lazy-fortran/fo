program test_cmake_preset_capture_token
    !! A public NONE-language delegated configure, without build/test/resident work.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: make_scratch, write_text, read_text, remove_tree, &
        assert_true, assert_contains, assert_equal_string, finish_assertions, &
        environment_value, make_directory
    use fo_build_backend, only: backend_t, detect_backend
    use fo_cmake_context, only: cmake_context_read_reply, cmake_context_validate_hint
    use fo_cmake_generation, only: cmake_capture_inventory, cmake_capture_cleanup
    use fo_input_inventory, only: input_inventory_t
    use fo_fs, only: fs_collect_files
    implicit none
    character(:), allocatable :: scratch, root, observed, old_preset, old_hint
    character(:), allocatable :: old_backend, before_token, at_configure, reply
    character(:), allocatable :: old_native
    character(:), allocatable :: capture_token, capture_error, selected_index
    character(len=32) :: capture_status_text
    character(len=4096) :: message, paths(64)
    character(len=*), parameter :: nl = new_line('a')
    type(backend_t) :: backend
    type(input_inventory_t) :: inventory
    integer :: branch, status, cleanup_status, ignored, count, i
    logical :: replaced, hint_valid, capture_seen
    interface
        integer(c_int) function setenv(name, value, overwrite) &
                bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function setenv
    end interface

    call make_scratch('fo-preset-capture-token', scratch)
    observed = environment_value('FO_TOKEN_DIAGNOSTIC_OUT')
    old_preset = environment_value('FO_CMAKE_CONFIGURE_PRESET')
    old_hint = environment_value('FO_CMAKE_BUILD_DIR')
    old_backend = environment_value('FO_BACKEND')
    old_native = environment_value('FO_CMAKE_NATIVE')
    ignored = setenv('FO_CMAKE_NATIVE'//c_null_char, '0'//c_null_char, 1_c_int)
    ignored = setenv('FO_BACKEND'//c_null_char, 'cmake'//c_null_char, 1_c_int)
    ignored = setenv('FO_CMAKE_CONFIGURE_PRESET'//c_null_char, &
        'public-token'//c_null_char, 1_c_int)
    ignored = setenv('FO_CMAKE_BUILD_DIR'//c_null_char, 'bld'//c_null_char, 1_c_int)
    do branch = 1, 2
        replaced = branch == 2
        root = scratch//'/ordinary'
        if (replaced) root = scratch//'/controlled-replacement'
        call write_text(root//'/CMakePresets.json', &
            '{"version":3,"configurePresets":[{"name":"public-token",'// &
            '"generator":"Ninja","binaryDir":"${sourceDir}/bld"}]}'//nl)
        call write_text(root//'/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.22)'//nl// &
            'project(public_token NONE)'//nl// &
            'set(query "${CMAKE_BINARY_DIR}/.cmake/api/v1/query/client-fo/query.json")'//nl// &
            'file(READ "${query}" request)'//nl// &
            'file(WRITE "${CMAKE_BINARY_DIR}/query-at-configure.json" "${request}")'//nl// &
            'if(EXISTS "${CMAKE_BINARY_DIR}/configure-count.txt")'//nl// &
            ' file(READ "${CMAKE_BINARY_DIR}/configure-count.txt" count)'//nl// &
            'else()'//nl// &
            ' set(count 0)'//nl// &
            'endif()'//nl// &
            'math(EXPR count "${count}+1")'//nl// &
            'file(WRITE "${CMAKE_BINARY_DIR}/configure-count.txt" "${count}")'//nl// &
            'enable_testing()'//nl// &
            'add_test(NAME public_token_sentinel COMMAND "${CMAKE_COMMAND}" -E true)'//nl)
        backend = detect_backend(root)
        ! cmake_context_init creates the token, and capture uses that context.
        ! Record it here instead of assuming an uninitialized entry token.
        before_token = backend%cmake%request_token
        call cmake_capture_inventory(backend, inventory, status, message)
        capture_token = backend%cmake%request_token
        capture_seen = backend%cmake%request_seen
        capture_error = context_text(backend%cmake%error)
        write (capture_status_text, '(i0)') status
        at_configure = read_text(root//'/bld/query-at-configure.json')
        selected_index = context_text(backend%cmake%reply_index)
        reply = ''
        if (len(selected_index) > 0) reply = read_text(selected_index)
        call assert_true(len(selected_index) > 0, 'capture selects a reply index')
        call assert_equal_string(backend%cmake%request_token, before_token, &
            'delegated capture preserves its context request token')
        call assert_contains(at_configure, before_token, &
            'CMake observed the token written by the actual helper')
        call assert_equal_string(read_text(root//'/bld/configure-count.txt'), '1', &
            'each fresh explicit-hint branch configures exactly once')
        call assert_true(status == 0, 'ordinary named-preset capture succeeds: '//trim(message))
        if (replaced) then
            ! Mutate only test memory after an actual successful capture. CMake
            ! reads File API requests before executing the project script, so
            ! editing query.json there cannot inject the intended mismatch.
            backend%cmake%request_token = 'controlled-context-replacement'
            call cmake_context_read_reply(backend%cmake)
            call cmake_context_validate_hint(backend%cmake, hint_valid)
            call assert_true(.not. hint_valid, 'controlled context/query mismatch is rejected')
            call assert_true(.not. backend%cmake%request_seen, &
                'matching final disk query/reply does not override the context token')
            call assert_contains(reply, before_token, &
                'controlled reply retains the actual disk query token')
            call assert_contains(read_text(root//'/bld/.cmake/api/v1/query/'// &
                'client-fo/query.json'), before_token, 'disk query still matches that reply')
            call assert_contains(context_text(backend%cmake%error), 'request token', &
                'controlled rejection preserves the exact token gate')
        else
            call assert_true(backend%cmake%request_seen, 'actual selected reply matches the request')
            call assert_contains(reply, before_token, 'actual selected reply retains context token')
        end if
        if (len(observed) > 0) then
            call make_directory(observed)
            call write_text(observed//'/branch-'//achar(iachar('0') + branch)//'.txt', &
                'context-token-before='//before_token//nl// &
                'capture-status='//trim(capture_status_text)//nl// &
                'capture-post-token='//capture_token//nl// &
                'capture-post-request-seen='//merge('true ', 'false', capture_seen)//nl// &
                'capture-post-error='//capture_error//nl// &
                'configure-count='//read_text(root//'/bld/configure-count.txt')//nl// &
                'context-token-after='//backend%cmake%request_token//nl// &
                'request-seen-after='//merge('true ', 'false', backend%cmake%request_seen)//nl// &
                'selected-index='//context_text(backend%cmake%reply_index)//nl// &
                'reported-generator='//context_text(backend%cmake%reported_generator)//nl// &
                'context-error='//context_text(backend%cmake%error)//nl// &
                'capture-message='//trim(message)//nl)
            call fs_collect_files(root//'/bld', '', '.json', '', paths, count, &
                ierr=cleanup_status, reject_aliases=.true.)
            call assert_true(cleanup_status == 0, 'bounded synthetic observation collection')
            do i = 1, count
                call copy_observation(trim(paths(i)), observed//'/branch-'// &
                    achar(iachar('0') + branch)//'-'//trim(paths(i)(index(trim(paths(i)), &
                    '/', back=.true.) + 1:)), cleanup_status)
                call assert_true(cleanup_status == 0, 'retain exact synthetic selected bytes')
            end do
        end if
        if (len(observed) > 0) then
            call copy_observation(root//'/bld/CMakeCache.txt', &
                observed//'/branch-'//achar(iachar('0') + branch)//'-CMakeCache.txt', cleanup_status)
            call assert_true(cleanup_status == 0, 'retain exact synthetic cache bytes')
        end if
        call cmake_capture_cleanup(inventory, cleanup_status)
        call assert_true(cleanup_status == 0, 'private capture metadata cleaned')
    end do
    ignored = setenv('FO_CMAKE_CONFIGURE_PRESET'//c_null_char, old_preset//c_null_char, 1_c_int)
    ignored = setenv('FO_CMAKE_BUILD_DIR'//c_null_char, old_hint//c_null_char, 1_c_int)
    ignored = setenv('FO_BACKEND'//c_null_char, old_backend//c_null_char, 1_c_int)
    ignored = setenv('FO_CMAKE_NATIVE'//c_null_char, old_native//c_null_char, 1_c_int)
    call remove_tree(scratch)
    call finish_assertions()
contains
    function context_text(value) result(text)
        character(:), allocatable, intent(in) :: value
        character(:), allocatable :: text
        text = ''
        if (allocated(value)) text = value
    end function context_text

    subroutine copy_observation(source, destination, status)
        character(len=*), intent(in) :: source, destination
        integer, intent(out) :: status
        character(:), allocatable :: bytes
        integer :: input, output, length
        inquire(file=source, size=length, iostat=status)
        if (status /= 0) return
        if (length < 0 .or. length > 1048576) then
            status = 1
            return
        end if
        allocate(character(len=length) :: bytes)
        open(newunit=input, file=source, status='old', access='stream', &
            form='unformatted', action='read', iostat=status)
        if (status /= 0) return
        read(input, iostat=status) bytes
        close(input)
        if (status /= 0) return
        open(newunit=output, file=destination, status='new', access='stream', &
            form='unformatted', action='write', iostat=status)
        if (status /= 0) return
        write(output, iostat=status) bytes
        close(output)
    end subroutine copy_observation
end program test_cmake_preset_capture_token
