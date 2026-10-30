program test_cmake_configure_error
    !! A failed CMake configure reports CMake's own error lines, even when the
    !! configure keeps running and logs far more than the error afterwards.
    use fo_test_harness, only: make_scratch, write_text, assert_true, &
        assert_contains, finish_assertions
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_CMAKE
    use fo_project_load, only: project_load
    use fo_project_model, only: project_model_t
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: scratch, message
    type(backend_t) :: backend
    type(project_model_t) :: model
    integer :: ierr

    call make_scratch('fo-cmake-configure-error', scratch)
    ! SEND_ERROR, like a missing add_subdirectory source, lets CMake go on;
    ! the padding puts the error well before the last few kilobytes of the log.
    call write_text(scratch//'/CMakeLists.txt', 'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(configure_error LANGUAGES NONE)'//nl// &
        'message(SEND_ERROR "dependency libprobe has no source directory")'//nl// &
        'foreach(i RANGE 1 200)'//nl// &
        '    message(STATUS "padding line ${i} after the configure error")'//nl// &
        'endforeach()'//nl//'enable_testing()'//nl//'add_test(NAME t COMMAND true)'//nl)
    backend = detect_backend(scratch)
    call assert_true(backend%kind == BACKEND_CMAKE, 'fixture is a CMake project')
    call project_load(backend, '', model, ierr, message)
    call assert_true(ierr /= 0, 'configure error fails the model load')
    call assert_contains(message, 'CMake configure failed', 'failure is named')
    call assert_contains(message, 'CMake Error at CMakeLists.txt:3 (message):', &
        'CMake error location survives a long log')
    call assert_contains(message, 'dependency libprobe has no source directory', &
        'CMake error text survives a long log')
    call assert_true(len(message) < 4000, 'excerpt leaves out the status padding')
    call finish_assertions()
end program test_cmake_configure_error
