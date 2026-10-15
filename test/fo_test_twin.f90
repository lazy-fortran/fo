module fo_test_twin
    !! Twin fixture generator: the same sources, labels, layers, test groups
    !! and default exclusion written once as an fpm package and once as a
    !! CMake project. Sources: src/tw_core.f90, src/tw_phys.f90 (uses core),
    !! tests test_core_a (core), test_phys_b (phys), test_io_c, test_slow_d.
    !! Labels: unit (core_a, phys_b), io (io_c), slow (slow_d; excluded by
    !! default). Layers tw_core and tw_phys (uses tw_core): CMake libraries,
    !! fpm source layers of the same names. Declared input data/io_ref.dat
    !! of test_io_c: an `[[extra.fo.inputs]]` row in the io group (fpm), the
    !! CTest REQUIRED_FILES property (CMake).
    use fo_test_harness, only: join_path, write_lines
    implicit none
    private

    public :: twin_write, twin_case_count, twin_case_name

    integer, parameter :: n_cases = 4
    character(len=11), parameter :: case_names(n_cases) = &
        [character(len=11) :: 'test_core_a', 'test_phys_b', 'test_io_c', 'test_slow_d']

contains

    integer function twin_case_count() result(n)
        n = n_cases
    end function twin_case_count

    function twin_case_name(i) result(name)
        integer, intent(in) :: i
        character(len=11) :: name
        name = case_names(i)
    end function twin_case_name

    subroutine twin_write(dir, backend)
        !! backend is 'fpm' or 'cmake'.
        character(len=*), intent(in) :: dir, backend
        character(len=64) :: used(n_cases)
        character(len=64) :: lines(4)
        integer :: i

        used = [character(len=64) :: 'use tw_core, only: core_value', &
            'use tw_phys, only: phys_value', '', '']
        call write_lines(join_path(dir, 'src/tw_core.f90'), [character(len=48) :: &
            'module tw_core', 'implicit none', 'contains', &
            'integer function core_value()', 'core_value = 1', 'end function', &
            'end module'])
        call write_lines(join_path(dir, 'src/tw_phys.f90'), [character(len=48) :: &
            'module tw_phys', 'use tw_core, only: core_value', 'implicit none', &
            'contains', 'integer function phys_value()', &
            'phys_value = core_value() + 1', 'end function', 'end module'])
        do i = 1, n_cases
            lines(1) = 'program '//trim(case_names(i))
            lines(2) = used(i)
            lines(3) = 'implicit none'
            lines(4) = 'end program'
            call write_lines(join_path(dir, 'test/'//trim(case_names(i))//'.f90'), &
                lines)
        end do
        lines(1) = '1'
        call write_lines(join_path(dir, 'data/io_ref.dat'), lines(1:1))
        if (backend == 'fpm') then
            call write_lines(join_path(dir, 'fpm.toml'), [character(len=80) :: &
                'name = "tw"', '[extra.fo.layers]', &
                'tw_core = { source = ["src/tw_core.f90"] }', &
                'tw_phys = { source = ["src/tw_phys.f90"], uses = ["tw_core"] }', &
                '[extra.fo.test-groups]', &
                'unit = { tests = ["test/test_core_a*", "test/test_phys_b*"] }', &
                'io = { tests = ["test/test_io_c*"], inputs = ["data/io_ref.dat"] }', &
                'slow = { tests = ["test/test_slow_d*"] }', &
                '[extra.fo.test-defaults]', 'exclude = ["slow"]', &
                '[[extra.fo.inputs]]', 'path = "data/io_ref.dat"', &
                'role = "test-fixture"'])
        else
            call write_lines(join_path(dir, 'CMakeLists.txt'), [character(len=96) :: &
                'cmake_minimum_required(VERSION 3.20)', 'project(tw Fortran)', &
                'enable_testing()', &
                'add_library(tw_core STATIC src/tw_core.f90)', &
                'add_library(tw_phys STATIC src/tw_phys.f90)', &
                'target_link_libraries(tw_phys tw_core)', &
                'foreach(t core_a phys_b io_c slow_d)', &
                '  add_executable(test_${t} test/test_${t}.f90)', &
                '  add_test(NAME test_${t} COMMAND test_${t})', 'endforeach()', &
                'target_link_libraries(test_core_a tw_core)', &
                'target_link_libraries(test_phys_b tw_phys)', &
                'set_tests_properties(test_core_a test_phys_b PROPERTIES LABELS "unit")', &
                'set_tests_properties(test_io_c PROPERTIES LABELS "io"', &
                '  REQUIRED_FILES "${CMAKE_CURRENT_SOURCE_DIR}/data/io_ref.dat")', &
                'set_tests_properties(test_slow_d PROPERTIES LABELS "slow")'])
        end if
    end subroutine twin_write

end module fo_test_twin
