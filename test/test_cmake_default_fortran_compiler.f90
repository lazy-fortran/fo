program test_cmake_default_fortran_compiler
    !! A fresh CMake configure without FC uses Fo's default gfortran, not the
    !! first Fortran compiler CMake's own search finds on PATH.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: make_scratch, write_text, read_text, assert_true, &
        assert_contains, assert_not_contains, finish_assertions, environment_value
    use fo_fake_tool, only: fake_tool_dispatch, install_fake_tool
    use fo_fs, only: fs_find_executable
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_CMAKE
    use fo_project_load, only: project_load
    use fo_project_model, only: project_model_t
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: scratch, message, cache
    character(len=4096) :: gfortran
    type(backend_t) :: backend
    type(project_model_t) :: model
    integer :: ierr
    logical :: available
    interface
        integer(c_int) function set_env(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_env
    end interface

    call fake_tool_dispatch()
    call fs_find_executable('gfortran', gfortran, available)
    if (.not. available) stop 'SKIP: gfortran is unavailable'
    call make_scratch('fo-cmake-default-fc', scratch)
    ! ifx comes before gfortran in CMake's search list; this one cannot compile.
    call install_fake_tool(scratch//'/bin/ifx', 'exit 1')
    ierr = set_env('PATH'//c_null_char, scratch//'/bin:'// &
        environment_value('PATH')//c_null_char, 1_c_int)
    ierr = set_env('FC'//c_null_char, c_null_char, 1_c_int)
    ierr = set_env('FO_FC'//c_null_char, c_null_char, 1_c_int)
    ierr = set_env('CMAKE_Fortran_COMPILER'//c_null_char, c_null_char, 1_c_int)
    call write_text(scratch//'/project/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(default_fc Fortran)'//nl// &
        'enable_testing()'//nl//'add_test(NAME t COMMAND true)'//nl)
    backend = detect_backend(scratch//'/project')
    call assert_true(backend%kind == BACKEND_CMAKE, 'fixture is a CMake project')
    call project_load(backend, '', model, ierr, message)
    call assert_true(ierr == 0, 'fresh configure succeeds: '//message)
    cache = read_text(scratch//'/project/build/CMakeCache.txt')
    call assert_contains(cache, 'CMAKE_Fortran_COMPILER:FILEPATH='//trim(gfortran)//nl, &
        'configure records Fo''s gfortran')
    call assert_not_contains(cache, scratch//'/bin/ifx', 'CMake search result is unused')
    call finish_assertions()
end program test_cmake_default_fortran_compiler
