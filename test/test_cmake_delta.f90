program test_cmake_delta
    !! A CMake listfile edit counts as a test registration only when it adds
    !! tests and targets without changing an existing one; the templates it
    !! returns cover every test and target the edit creates. The edits follow
    !! the shapes of real test registrations: appended foreach items (outer
    !! and nested loops), literal add_test blocks, new executables inside an
    !! if block and new nested loops.
    use fx_string, only: string_t
    use fo_cmake_delta, only: cmake_registration_delta, cmake_template_matches
    use fo_test_harness, only: assert_true, finish_assertions
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: base
    type(string_t), allocatable :: none(:)

    allocate(none(0))
    base = &
        'cmake_minimum_required(VERSION 3.22)'//nl// &
        'project(p LANGUAGES Fortran)'//nl// &
        'enable_testing()'//nl// &
        'set(PIN "8dbbf3a2")'//nl// &
        'add_library(core src/core.f90)'//nl// &
        'add_executable(test_island test/test_island.f90'//nl// &
        '    test/support/leaves.f90)'//nl// &
        'target_link_libraries(test_island PRIVATE core)'//nl// &
        'foreach(family equal mixed)'//nl// &
        '    foreach(gate boundary equilibrium width)'//nl// &
        '        add_test(NAME island_${gate}_${family}'//nl// &
        '            COMMAND test_island ${gate} ${family})'//nl// &
        '        set_tests_properties(island_${gate}_${family} PROPERTIES LABELS rm'//nl// &
        '            PROCESSORS 4 TIMEOUT 300'//nl// &
        '            ENVIRONMENT "OMP_NUM_THREADS=4;OPENBLAS_NUM_THREADS=1")'//nl// &
        '    endforeach()'//nl// &
        'endforeach()'//nl// &
        'add_test(NAME island_decimal COMMAND test_island probe mixed 4 4)'//nl// &
        'set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)'//nl// &
        'if(WITH_GEN)'//nl// &
        '    add_executable(gen_leaves gen/gen_leaves.f90)'//nl// &
        '    target_link_libraries(gen_leaves PRIVATE core)'//nl// &
        'endif()'//nl// &
        'if(TARGET helper)'//nl// &
        '    target_compile_definitions(core PRIVATE HAVE_HELPER)'//nl// &
        'endif()'//nl// &
        'foreach(t alpha beta)'//nl// &
        '    add_executable(unit_${t} test/unit_${t}.f90)'//nl// &
        'endforeach()'//nl// &
        '# Study driver; run by scripts, not CTest.'//nl// &
        'foreach(case tangent nested)'//nl// &
        '    add_test(NAME provider_${case}_p2 COMMAND test_island ${case} 2 4)'//nl// &
        '    set_tests_properties(provider_${case}_p2 PROPERTIES TIMEOUT 900)'//nl// &
        'endforeach()'//nl// &
        'foreach(item IN LISTS EXTRA_TESTS)'//nl// &
        '    add_test(NAME extra_${item} COMMAND test_island ${item})'//nl// &
        'endforeach()'//nl// &
        'function(label_test test)'//nl// &
        '    set_property(TEST ${test} APPEND PROPERTY LABELS fast)'//nl// &
        'endfunction()'//nl

    ! Registrations.
    call registers('nested foreach item appended', &
        edit('foreach(gate boundary equilibrium width)', &
            'foreach(gate boundary equilibrium width graded)'), &
        'island_graded_equal island_graded_mixed', &
        'island_width_equal island_decimal provider_tangent_p2', '')
    call registers('outer foreach item appended', &
        edit('foreach(case tangent nested)', 'foreach(case tangent nested vacuum)'), &
        'provider_vacuum_p2', 'provider_nested_p2 island_width_equal', '')
    call registers('outer loop of a nested pair appended', &
        edit('foreach(family equal mixed)', 'foreach(family equal mixed graded)'), &
        'island_width_graded island_boundary_graded', 'island_width_mixed', '')
    call registers('literal add_test block inserted', &
        edit('set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)', &
            'set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)'//nl// &
            'add_test(NAME island_graded_input COMMAND test_island'//nl// &
            '    width mixed 4 6 ${CMAKE_SOURCE_DIR}/test/fixtures/mesh.nml)'//nl// &
            'set_tests_properties(island_graded_input PROPERTIES LABELS rm'//nl// &
            '    PROCESSORS 4 TIMEOUT 300'//nl// &
            '    ENVIRONMENT "OMP_NUM_THREADS=4;OPENBLAS_NUM_THREADS=1")'), &
        'island_graded_input', 'island_decimal island_width_mixed', '')
    call registers('new executable and test inside an if block', &
        edit('    target_link_libraries(gen_leaves PRIVATE core)', &
            '    target_link_libraries(gen_leaves PRIVATE core)'//nl// &
            '    add_executable(gen_island gen/gen_island.f90)'//nl// &
            '    target_link_libraries(gen_island PRIVATE core)'//nl// &
            '    add_test(NAME island_regeneration COMMAND ${CMAKE_COMMAND}'//nl// &
            '        -DGENERATOR=$<TARGET_FILE:gen_island> -P check.cmake)'//nl// &
            '    set_tests_properties(island_regeneration PROPERTIES LABELS rm)'), &
        'island_regeneration', 'island_decimal', 'gen_island')
    call registers('new nested loops with a new executable', &
        edit('# Study driver; run by scripts, not CTest.', &
            'add_executable(test_slab test/test_slab.f90)'//nl// &
            'target_link_libraries(test_slab PRIVATE core)'//nl// &
            'foreach(family equal mixed)'//nl// &
            '    foreach(gate boundary width)'//nl// &
            '        add_test(NAME slab_${gate}_${family} COMMAND test_slab ${gate})'//nl// &
            '        set_tests_properties(slab_${gate}_${family} PROPERTIES TIMEOUT 60)'//nl// &
            '    endforeach()'//nl// &
            'endforeach()'//nl// &
            '# Study driver; run by scripts, not CTest.'), &
        'slab_boundary_equal slab_width_mixed', 'island_width_mixed', 'test_slab')
    call registers('test added to every iteration of an unchanged loop', &
        edit('    set_tests_properties(provider_${case}_p2 PROPERTIES TIMEOUT 900)', &
            '    set_tests_properties(provider_${case}_p2 PROPERTIES TIMEOUT 900)'//nl// &
            '    add_test(NAME provider_${case}_p4 COMMAND test_island ${case} 4 4)'), &
        'provider_tangent_p4 provider_nested_p4', 'island_decimal', '')
    call registers('comment and blank lines only', &
        edit('# Study driver; run by scripts, not CTest.', &
            '# Study drivers run by scripts.'//nl//nl//'# Not by CTest.'), &
        '', 'island_decimal provider_tangent_p2', '')
    call registers_with('declared project labelling command', &
        edit('set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)', &
            'set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)'//nl// &
            'add_test(NAME island_tail COMMAND test_island tail)'//nl// &
            'label_test(island_tail fast CLASSES mhd)'), &
        [string_t('label_test')], 'island_tail', 'island_decimal')

    ! Edits that must widen.
    call widens('dependency pin changed', &
        edit('set(PIN "8dbbf3a2")', 'set(PIN "57e3bb59")'))
    call widens('source appended to an existing library', &
        edit('add_library(core src/core.f90)', 'add_library(core src/core.f90 src/more.f90)'))
    call widens('existing target gains a link library', &
        edit('target_link_libraries(test_island PRIVATE core)', &
            'target_link_libraries(test_island PRIVATE core)'//nl// &
            'target_link_libraries(test_island PRIVATE extra)'))
    call widens('existing test gains a property', &
        edit('if(WITH_GEN)', &
            'set_tests_properties(island_decimal PROPERTIES TIMEOUT 5)'//nl//'if(WITH_GEN)'))
    call widens('existing test command changed', &
        edit('add_test(NAME island_decimal COMMAND test_island probe mixed 4 4)', &
            'add_test(NAME island_decimal COMMAND test_island probe mixed 4 8)'))
    call widens('foreach item removed', &
        edit('foreach(case tangent nested)', 'foreach(case tangent)'))
    call widens('foreach item replaced', &
        edit('foreach(case tangent nested)', 'foreach(case tangent vacuum)'))
    call widens('foreach over a variable list changed', &
        edit('foreach(item IN LISTS EXTRA_TESTS)', 'foreach(item IN LISTS MORE_TESTS)'))
    call widens('new test sets up a fixture', &
        edit('if(WITH_GEN)', &
            'add_test(NAME start_server COMMAND test_island serve)'//nl// &
            'set_tests_properties(start_server PROPERTIES FIXTURES_SETUP server)'//nl// &
            'if(WITH_GEN)'))
    call widens('test registered inside a function body', &
        edit('    set_property(TEST ${test} APPEND PROPERTY LABELS fast)', &
            '    set_property(TEST ${test} APPEND PROPERTY LABELS fast)'//nl// &
            '    add_test(NAME labelled COMMAND test_island labelled)'))
    call widens('existing loop targets gain a link library', &
        edit('    add_executable(unit_${t} test/unit_${t}.f90)', &
            '    add_executable(unit_${t} test/unit_${t}.f90)'//nl// &
            '    target_link_libraries(unit_${t} PRIVATE core)'))
    call widens('new library an unchanged if(TARGET) observes', &
        edit('if(TARGET helper)', &
            'add_library(helper src/helper.f90)'//nl//'if(TARGET helper)'))
    call widens('include of another listfile', &
        edit('enable_testing()', 'enable_testing()'//nl//'include(cmake/more.cmake)'))
    call widens('new conditional block', &
        edit('enable_testing()', 'enable_testing()'//nl//'if(FOO)'//nl// &
            'add_test(NAME foo COMMAND test_island foo)'//nl//'endif()'))
    call widens('unbalanced parenthesis', base//'add_test(NAME broken COMMAND x'//nl)
    call widens('undeclared project command', &
        edit('set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)', &
            'set_tests_properties(island_decimal PROPERTIES LABELS rm TIMEOUT 300)'//nl// &
            'add_test(NAME island_tail COMMAND test_island tail)'//nl// &
            'label_test(island_tail fast CLASSES mhd)'))
    call widens_in_context('another project listfile names the new target', &
        edit('    target_link_libraries(gen_leaves PRIVATE core)', &
            '    target_link_libraries(gen_leaves PRIVATE core)'//nl// &
            '    add_library(gen_support src/gen_support.f90)'), &
        'target_link_libraries(tool PRIVATE gen_support)'//nl)

    call assert_true(cmake_template_matches('a_*_*', 'a_x_y'), 'templates match')
    call assert_true(.not. cmake_template_matches('a_*_b', 'a_x_c'), &
        'templates are anchored')
    call finish_assertions()

contains

    function edit(old, new) result(text)
        character(len=*), intent(in) :: old, new
        character(len=:), allocatable :: text
        integer :: at

        at = index(base, old)
        call assert_true(at > 0, 'fixture contains: '//old)
        text = base(1:at - 1)//new//base(at + len(old):)
    end function edit

    subroutine registers(label, new_text, created, untouched, targets)
        character(len=*), intent(in) :: label, new_text, created, untouched, targets

        call registers_with(label, new_text, none, created, untouched, targets)
    end subroutine registers

    subroutine registers_with(label, new_text, commands, created, untouched, targets)
        character(len=*), intent(in) :: label, new_text, created, untouched
        type(string_t), intent(in) :: commands(:)
        character(len=*), intent(in), optional :: targets
        type(string_t), allocatable :: tests(:), made(:), words(:)
        character(len=:), allocatable :: reason
        logical :: ok
        integer :: k

        call cmake_registration_delta(base, new_text, commands, tests, made, ok, reason)
        call assert_true(ok, label//': is a registration ('//reason//')')
        if (.not. ok) return
        call split_words(created, words)
        do k = 1, size(words)
            call assert_true(matches(tests, words(k)%s), &
                label//': selects created test '//words(k)%s)
        end do
        call split_words(untouched, words)
        do k = 1, size(words)
            call assert_true(.not. matches(tests, words(k)%s), &
                label//': leaves existing test '//words(k)%s)
        end do
        if (present(targets)) then
            call split_words(targets, words)
            do k = 1, size(words)
                call assert_true(matches(made, words(k)%s), &
                    label//': names created target '//words(k)%s)
            end do
            call assert_true(.not. matches(made, 'core'), &
                label//': leaves existing target core')
        end if
    end subroutine registers_with

    subroutine widens(label, new_text)
        character(len=*), intent(in) :: label, new_text
        type(string_t), allocatable :: tests(:), made(:)
        character(len=:), allocatable :: reason
        logical :: ok

        call cmake_registration_delta(base, new_text, none, tests, made, ok, reason)
        call assert_true(.not. ok, label//': widens')
        call assert_true(len(reason) > 0, label//': says why')
    end subroutine widens

    subroutine widens_in_context(label, new_text, other)
        character(len=*), intent(in) :: label, new_text, other
        type(string_t), allocatable :: tests(:), made(:)
        character(len=:), allocatable :: reason
        logical :: ok

        call cmake_registration_delta(base, new_text, none, tests, made, ok, reason)
        call assert_true(ok, label//': registers without the other listfile')
        call cmake_registration_delta(base, new_text, none, tests, made, ok, reason, &
            [string_t(other)])
        call assert_true(.not. ok, label//': widens')
    end subroutine widens_in_context

    logical function matches(templates, name)
        type(string_t), intent(in) :: templates(:)
        character(len=*), intent(in) :: name
        integer :: k

        matches = .false.
        do k = 1, size(templates)
            if (cmake_template_matches(templates(k)%s, name)) matches = .true.
        end do
    end function matches

    subroutine split_words(text, words)
        character(len=*), intent(in) :: text
        type(string_t), allocatable, intent(out) :: words(:)
        integer :: i, start

        allocate(words(0))
        i = 1
        do while (i <= len(text))
            if (text(i:i) == ' ') then
                i = i + 1
                cycle
            end if
            start = i
            do while (i <= len(text))
                if (text(i:i) == ' ') exit
                i = i + 1
            end do
            words = [words, string_t(text(start:i - 1))]
        end do
    end subroutine split_words

end program test_cmake_delta
