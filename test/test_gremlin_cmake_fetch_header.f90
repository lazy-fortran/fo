program test_gremlin_cmake_fetch_header
    !! A header-only FetchContent dependency is consumed only through an
    !! include directory under FETCHCONTENT_BASE_DIR. The frozen generation
    !! must carry the populated source, because its disconnected replay
    !! cannot download it again.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: write_text, assert_true, finish_assertions, &
        make_directory
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_field, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: header, generation
    character(*), parameter :: lane = 'cmake-fetch-header'
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    logical :: found

    call gremlin_setup(driver, scratch, project, cache, state)
    ! The download source is a single local header, as for a header-only
    ! library fetched without extraction.
    call make_directory(scratch//'/upstream')
    header = scratch//'/upstream/header_value.inc'
    call write_text(header, 'integer, parameter :: header_value = 13'//nl)
    call write_text(project//'/fpm.toml', 'name = "cmake_fetch_header_probe"'//nl)
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(fetch_header_probe LANGUAGES Fortran)'//nl// &
        'include(FetchContent)'//nl// &
        'FetchContent_Declare(header_dep URL "file://'//header//'" '// &
        'DOWNLOAD_NO_EXTRACT TRUE)'//nl// &
        'FetchContent_MakeAvailable(header_dep)'//nl// &
        'enable_testing()'//nl// &
        'add_executable(marker test/marker.f90)'//nl// &
        'target_include_directories(marker PRIVATE "${header_dep_SOURCE_DIR}")'//nl// &
        'add_test(NAME registered_marker COMMAND marker)'//nl)
    call write_text(project//'/test/marker.f90', &
        'program header_marker'//nl//'implicit none'//nl// &
        'include "header_value.inc"'//nl// &
        'if (header_value /= 13) error stop "wrong header"'//nl// &
        'print *, "registered marker passed"'//nl// &
        'end program header_marker'//nl)
    call list_add(args, 'gremlin')
    call list_add(args, 'start')
    call list_add(args, '--dir')
    call list_add(args, project)
    call list_add(args, '--lane')
    call list_add(args, lane)
    call gremlin_json(driver, project, cache, state, args, reply, process, 30000)
    session = gremlin_field(reply, 'session_id')
    call assert_true(process%exit_code == 0 .and. len(session) > 0, &
        'starts a CMake campaign with a header-only FetchContent dependency')
    if (len(session) == 0) call finish_assertions(retain_failed_scratch=.true.)

    call wait_verified(generation, found)
    call assert_true(found, &
        'frozen generation builds against the populated header-only dependency')
    call gremlin_stop_lane(driver, project, cache, state, lane, session, &
        allow_terminal_error=.true.)
    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine wait_verified(generation, found)
        character(:), allocatable, intent(out) :: generation
        logical, intent(out) :: found
        type(string_list_t) :: status_args
        type(process_result_t) :: status_process
        type(json_value_t) :: status, value
        character(:), allocatable :: state_name
        integer :: poll

        call list_add(status_args, 'gremlin')
        call list_add(status_args, 'status')
        call list_add(status_args, '--dir')
        call list_add(status_args, project)
        call list_add(status_args, '--lane')
        call list_add(status_args, lane)
        call list_add(status_args, '--session')
        call list_add(status_args, session)
        found = .false.
        generation = ''
        do poll = 1, 1200
            call gremlin_json(driver, project, cache, state, status_args, &
                status, status_process, 10000)
            if (status_process%exit_code /= 0) return
            state_name = gremlin_field(status, 'state')
            if (state_name == 'error' .or. state_name == 'build_failed') return
            generation = gremlin_field(status, 'active_generation')
            if (len(generation) == 64) then
                value = json_member(status, 'fully_verified')
                found = json_boolean_value(value)
                if (found) return
            end if
            call gremlin_wait_ms(50)
        end do
    end subroutine wait_verified

end program test_gremlin_cmake_fetch_header
