program test_test_since
    !! fo test --since REF selects the cases that the commits since REF
    !! affect, as a landing check needs; --only-changed on the same clean
    !! tree selects none.
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_contains, assert_not_contains, &
        finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-test-since', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "since_probe"'//nl)
    call module_source('a', 1)
    call module_source('b', 2)
    call test_source('a', 1)
    call test_source('b', 2)
    call git([character(len=32) :: 'init', '-q'])
    call git([character(len=32) :: 'add', '-A'])
    call git([character(len=32) :: 'commit', '-qm', 'Base'])
    call module_source('a', 3)
    call test_source('a', 3)
    call git([character(len=32) :: 'commit', '-qam', 'Change a'])

    call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
    call assert_process_ok(result, 'fo test --since lists')
    call assert_contains(result%stdout, 'test_a', 'the changed module''s test is selected')
    call assert_not_contains(result%stdout, 'test_b', 'the unchanged module''s test is not')
    call fo([character(len=32) :: 'test', '--since', 'HEAD~1'])
    call assert_process_ok(result, 'fo test --since runs the selected test')

    call fo([character(len=32) :: 'test', '--only-changed', '--list'])
    call assert_process_ok(result, 'fo test --only-changed lists')
    call assert_not_contains(result%stdout, 'test_a', 'a clean tree selects nothing')

    call fo([character(len=32) :: 'test', '--since', 'no-such-ref', '--list'])
    call assert_contains(result%stdout//result%stderr, 'no-such-ref', &
        'an unknown revision is named')

    call pin_bump()
    call cmake_pin_bump()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'test-since: --since selects the cases affected since REF'

contains

    subroutine pin_bump()
        !! A commit that only moves a Git dependency's pin selects the cases
        !! whose module closure reaches what the dependency changed between
        !! the pins, through the dependency's own uses; any other manifest
        !! change widens to all.
        character(:), allocatable :: dep, app, first, second
        dep = join_path(scratch, 'dep')
        app = join_path(scratch, 'app')
        project = dep
        call write_text(join_path(dep, 'fpm.toml'), 'name = "dep"'//nl)
        call write_text(join_path(dep, 'src/m1.f90'), 'module m1'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v1 = 1'//nl//'end module m1'//nl)
        call write_text(join_path(dep, 'src/m2.f90'), 'module m2'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v2 = 1'//nl//'end module m2'//nl)
        call write_text(join_path(dep, 'src/m3.f90'), 'module m3'//nl//'    use m1'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v3 = v1'//nl//'end module m3'//nl)
        call git([character(len=32) :: 'init', '-q'])
        call git([character(len=32) :: 'add', '-A'])
        call git([character(len=32) :: 'commit', '-qm', 'One'])
        first = head()
        call write_text(join_path(dep, 'src/m1.f90'), 'module m1'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v1 = 1 + 0'//nl// &
            'end module m1'//nl)
        call git([character(len=32) :: 'commit', '-qam', 'Two'])
        second = head()

        project = app
        call write_text(join_path(app, '.gitignore'), 'build/'//nl)
        call manifest(first, '')
        call write_text(join_path(app, 'test/test_1.f90'), 'program test_1'//nl// &
            '    use m1'//nl//'    if (v1 /= 1) error stop 1'//nl//'end program test_1'//nl)
        call write_text(join_path(app, 'test/test_2.f90'), 'program test_2'//nl// &
            '    use m2'//nl//'    if (v2 /= 1) error stop 1'//nl//'end program test_2'//nl)
        call write_text(join_path(app, 'test/test_3.f90'), 'program test_3'//nl// &
            '    use m3'//nl//'    if (v3 /= 1) error stop 1'//nl//'end program test_3'//nl)
        call git([character(len=32) :: 'init', '-q'])
        call git([character(len=32) :: 'add', '-A'])
        call git([character(len=32) :: 'commit', '-qm', 'Base'])
        call fo([character(len=32) :: 'build'])
        call assert_process_ok(result, 'the app builds at the first pin')
        call manifest(second, '')
        call git([character(len=32) :: 'commit', '-qam', 'Bump'])
        call fo([character(len=32) :: 'build'])
        call assert_process_ok(result, 'the app builds at the second pin')
        call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
        call assert_process_ok(result, 'fo test --since lists after a pin bump')
        call assert_contains(result%stdout, 'test_1', 'a user of the changed module is selected')
        call assert_contains(result%stdout, 'test_3', &
            'a user of a dependency module that uses it is selected')
        call assert_not_contains(result%stdout, 'test_2', &
            'a user of an unchanged dependency module is not')
        call assert_not_contains(result%stdout//result%stderr, 'affected selection widened', &
            'a pin bump does not widen')

        call manifest(second, '[build]'//nl//'auto-executables = true'//nl)
        call git([character(len=32) :: 'commit', '-qam', 'Edit'])
        call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
        call assert_contains(result%stdout//result%stderr, 'affected selection widened', &
            'another manifest change widens')
        call assert_contains(result%stdout, 'test_2', 'and selects every case')
    end subroutine pin_bump

    subroutine cmake_pin_bump()
        !! The same for a CMake FetchContent pin held in a variable, as MILCA's
        !! MILCA_*_REVISION: the moved hash selects the users of what the
        !! dependency changed, through its own uses.
        character(:), allocatable :: dep, app, first, second
        dep = join_path(scratch, 'cmake-dep')
        app = join_path(scratch, 'cmake-app')
        project = dep
        call write_text(join_path(dep, 'CMakeLists.txt'), &
            'cmake_minimum_required(VERSION 3.20)'//nl//'project(dep LANGUAGES Fortran)'//nl// &
            'add_library(dep src/m1.f90 src/m2.f90 src/m3.f90)'//nl// &
            'set_target_properties(dep PROPERTIES Fortran_MODULE_DIRECTORY '// &
            '${CMAKE_CURRENT_BINARY_DIR}/mod)'//nl// &
            'target_include_directories(dep PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/mod)'//nl)
        call write_text(join_path(dep, 'src/m1.f90'), 'module m1'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v1 = 1'//nl//'end module m1'//nl)
        call write_text(join_path(dep, 'src/m2.f90'), 'module m2'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v2 = 1'//nl//'end module m2'//nl)
        call write_text(join_path(dep, 'src/m3.f90'), 'module m3'//nl//'    use m1'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v3 = v1'//nl//'end module m3'//nl)
        call git([character(len=32) :: 'init', '-q'])
        call git([character(len=32) :: 'add', '-A'])
        call git([character(len=32) :: 'commit', '-qm', 'One'])
        first = head()
        call write_text(join_path(dep, 'src/m1.f90'), 'module m1'//nl// &
            '    implicit none'//nl//'    integer, parameter :: v1 = 1 + 0'//nl// &
            'end module m1'//nl)
        call git([character(len=32) :: 'commit', '-qam', 'Two'])
        second = head()

        project = app
        call write_text(join_path(app, '.gitignore'), 'build/'//nl)
        call cmake_lists(dep, first)
        call write_text(join_path(app, 'test/test_1.f90'), 'program test_1'//nl// &
            '    use m1'//nl//'    if (v1 /= 1) error stop 1'//nl//'end program test_1'//nl)
        call write_text(join_path(app, 'test/test_2.f90'), 'program test_2'//nl// &
            '    use m2'//nl//'    if (v2 /= 1) error stop 1'//nl//'end program test_2'//nl)
        call write_text(join_path(app, 'test/test_3.f90'), 'program test_3'//nl// &
            '    use m3'//nl//'    if (v3 /= 1) error stop 1'//nl//'end program test_3'//nl)
        call git([character(len=32) :: 'init', '-q'])
        call git([character(len=32) :: 'add', '-A'])
        call git([character(len=32) :: 'commit', '-qm', 'Base'])
        call fo([character(len=32) :: 'build'])
        call assert_process_ok(result, 'the CMake app builds at the first pin')
        call cmake_lists(dep, second)
        call git([character(len=32) :: 'commit', '-qam', 'Bump'])
        call fo([character(len=32) :: 'build'])
        call assert_process_ok(result, 'the CMake app builds at the second pin')
        call fo([character(len=32) :: 'test', '--since', 'HEAD~1', '--list'])
        call assert_process_ok(result, 'fo test --since lists after a CMake pin bump')
        call assert_contains(result%stdout, 'test_1', 'CMake: a user of the changed module')
        call assert_contains(result%stdout, 'test_3', 'CMake: a user through the dependency')
        call assert_not_contains(result%stdout, 'test_2', 'CMake: not a user of the unchanged one')
        call assert_not_contains(result%stdout//result%stderr, 'affected selection widened', &
            'a CMake pin bump does not widen')
    end subroutine cmake_pin_bump

    subroutine cmake_lists(dep, rev)
        character(len=*), intent(in) :: dep, rev
        call write_text(join_path(project, 'CMakeLists.txt'), &
            'cmake_minimum_required(VERSION 3.20)'//nl//'project(app LANGUAGES Fortran)'//nl// &
            'enable_testing()'//nl//'include(FetchContent)'//nl// &
            'set(DEP_REVISION "'//rev//'")'//nl// &
            'FetchContent_Declare(dep GIT_REPOSITORY '//dep// &
            ' GIT_TAG ${DEP_REVISION} GIT_SHALLOW FALSE)'//nl// &
            'FetchContent_MakeAvailable(dep)'//nl// &
            'foreach(t 1 2 3)'//nl// &
            '    add_executable(test_${t} test/test_${t}.f90)'//nl// &
            '    target_link_libraries(test_${t} PRIVATE dep)'//nl// &
            '    add_test(NAME test_${t} COMMAND test_${t})'//nl//'endforeach()'//nl)
    end subroutine cmake_lists

    subroutine manifest(rev, extra)
        character(len=*), intent(in) :: rev, extra
        call write_text(join_path(project, 'fpm.toml'), 'name = "app"'//nl//extra// &
            '[dependencies]'//nl//'dep = { git = "'//join_path(scratch, 'dep')// &
            '", rev = "'//rev//'" }'//nl)
    end subroutine manifest

    function head() result(sha)
        character(:), allocatable :: sha
        call git([character(len=32) :: 'rev-parse', 'HEAD'])
        sha = trim(result%stdout(:index(result%stdout, nl) - 1))
    end function head

    subroutine module_source(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=8) :: digits
        write (digits, '(i0)') value
        call write_text(join_path(project, 'src/'//name//'.f90'), &
            'module '//name//nl//'    implicit none'//nl// &
            '    integer, parameter :: value_'//name//' = '//trim(digits)//nl// &
            'end module '//name//nl)
    end subroutine module_source

    subroutine test_source(name, value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: value
        character(len=8) :: digits
        write (digits, '(i0)') value
        call write_text(join_path(project, 'test/test_'//name//'.f90'), &
            'program test_'//name//nl//'    use '//name//nl//'    implicit none'//nl// &
            '    if (value_'//name//' /= '//trim(digits)//') error stop 1'//nl// &
            'end program test_'//name//nl)
    end subroutine test_source

    subroutine git(words)
        character(len=*), intent(in) :: words(:)
        type(string_list_t) :: arguments
        integer :: i
        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_external('git', arguments, project, result)
        call assert_process_ok(result, 'git fixture command')
    end subroutine git

    subroutine fo(words)
        character(len=*), intent(in) :: words(:)
        type(string_list_t) :: arguments
        integer :: i
        do i = 1, size(words)
            call list_add(arguments, trim(words(i)))
        end do
        call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, &
            timeout_ms=300000)
    end subroutine fo
end program test_test_since
