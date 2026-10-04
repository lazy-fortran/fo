program test_gremlin_test_impact
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_collect_files, fs_make_dir, fs_remove_file, &
        fs_remove_tree, fs_rename, fs_write_text
    use fo_util, only: make_tmpfile
    use fo_input_inventory, only: input_inventory_t, input_declaration_t, &
        input_inventory_discover
    use fo_scan, only: scan_unit_t, scan_dir_regex, MAX_PATH, is_slow_test
    use fo_gfortran_build, only: gfortran_test_names
    use fo_dag_bridge, only: build_dag_from_units
    use fx_dag, only: dag_t, dag_find_node, MAX_NODES
    use fo_test_impact, only: test_impact_case_t, test_impact_result_t, &
        test_impact_select
    implicit none

    character(len=4096) :: fixture, project
    character(len=4096) :: source_a, include_file, harness_file, runtime_file
    character(len=4096) :: toolchain_file
    character(len=4096) :: source_b
    type(input_declaration_t) :: declarations(4)
    type(input_inventory_t) :: baseline, candidate
    type(test_impact_case_t), allocatable :: cases(:)
    type(test_impact_result_t) :: selected, repeated
    type(dag_t) :: dag
    character(len=MAX_PATH) :: filenames(MAX_NODES)
    integer :: ierr, filesystem_status
    character(len=1024) :: message
    character(len=4096) :: warm_cache, cold_cache, run_log
    character(len=4096) :: cache_files(128)
    character(len=128) :: run_names(2)
    logical :: dependency_model_complete
    integer :: run_exit, n_cache_files

    interface
        integer(c_int) function set_environment(name, value, overwrite) &
                bind(C, name='setenv') result(status)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_environment
    end interface

    call make_tmpfile('fo-test-impact', fixture)
    call fs_make_dir(trim(fixture))
    project = trim(fixture)//'/project'
    call fs_make_dir(trim(project)//'/src')
    call fs_make_dir(trim(project)//'/test')
    call fs_make_dir(trim(project)//'/include')
    call fs_write_text(trim(project)//'/fpm.toml', 'name = "impact_fixture"'// &
        new_line('a'))
    source_a = trim(project)//'/src/a_base.f90'
    source_b = trim(project)//'/src/b_base.f90'
    include_file = trim(project)//'/include/shared.inc'
    harness_file = trim(project)//'/test-support/harness.f90'
    runtime_file = trim(project)//'/runtime.dat'
    toolchain_file = trim(project)//'/toolchain.txt'
    call fs_make_dir(trim(project)//'/test-support')
    call fs_write_text(source_a, 'module a_base'//new_line('a')// &
        'contains'//new_line('a')// &
        'integer function a_value()'//new_line('a')// &
        'a_value = 1'//new_line('a')// &
        'end function a_value'//new_line('a')// &
        'end module a_base'//new_line('a'))
    call fs_write_text(trim(project)//'/src/a_middle.f90', &
        'module a_middle'//new_line('a')//'use a_base'//new_line('a')// &
        'contains'//new_line('a')//'integer function middle_value()'// &
        new_line('a')//'middle_value = a_value()'//new_line('a')// &
        'end function middle_value'//new_line('a')//'end module a_middle'// &
        new_line('a'))
    call fs_write_text(trim(project)//'/test/test_alpha.f90', &
        'program test_alpha'//new_line('a')//'use a_middle'//new_line('a')// &
        'if (middle_value() /= 1) error stop 1'//new_line('a')// &
        'end program test_alpha'//new_line('a'))
    call fs_write_text(source_b, &
        'module b_base'//new_line('a')//'end module b_base'//new_line('a'))
    call fs_write_text(trim(project)//'/test/test_beta_slow.f90', &
        'program test_beta_slow'//new_line('a')//'use b_base'//new_line('a')// &
        'end program test_beta_slow'//new_line('a'))
    call fs_write_text(include_file, 'integer, parameter :: shared = 1'//new_line('a'))
    call fs_write_text(harness_file, 'module harness'//new_line('a')// &
        'end module harness'//new_line('a'))
    call fs_write_text(runtime_file, 'runtime input'//new_line('a'))
    call fs_write_text(toolchain_file, 'compiler identity'//new_line('a'))
    declarations(1)%root_alias = 'project'
    declarations(1)%relative_path = 'test-support/harness.f90'
    declarations(1)%role = 'test-harness'
    declarations(2)%root_alias = 'project'
    declarations(2)%relative_path = 'runtime.dat'
    declarations(2)%role = 'runtime-data'
    declarations(3)%root_alias = 'project'
    declarations(3)%relative_path = 'runtime.dat'
    declarations(3)%role = 'environment-input'
    declarations(4)%root_alias = 'project'
    declarations(4)%relative_path = 'toolchain.txt'
    declarations(4)%role = 'toolchain-identity'

    warm_cache = trim(fixture)//'/warm-cache'
    cold_cache = trim(fixture)//'/cold-cache'
    run_log = trim(fixture)//'/fixture-tests.log'
    call fs_make_dir(trim(warm_cache))
    call fs_make_dir(trim(cold_cache))
    call set_cache_root(warm_cache)
    run_names = ['test_alpha    ', 'test_beta_slow']
    call gfortran_test_names(project, run_names, 2, run_log, run_exit, &
        include_slow=.true., use_cache=.true.)
    call fs_collect_files(warm_cache, '', '', '', cache_files, n_cache_files)
    call require(run_exit == 0 .and. n_cache_files > 0, &
        'baseline run populates its isolated warm action cache')
    call require(run_exit == 0 .and. file_contains(run_log, &
        'TEST_RESULT test_alpha PASS') .and. file_contains(run_log, &
        'TEST_RESULT test_beta_slow PASS'), &
        'baseline Fortran behavior passes both independent test families')

    call discover(baseline)
    call fs_write_text(source_a, 'module a_base'//new_line('a')// &
        'contains'//new_line('a')//'integer function a_value()'//new_line('a')// &
        'a_value = 2'//new_line('a')//'end function a_value'//new_line('a')// &
        'end module a_base'//new_line('a'))
    call discover(candidate)
    call scan_model()
    call select(baseline, candidate, selected)
    call require(has_case(selected, 'test_alpha'), &
        'changed transitive module selects its public test')
    call require(.not. has_case(selected, 'test_beta_slow'), &
        'disjoint unchanged module family stays eligible for background coverage')
    call require(selected%model_complete .and. .not. selected%widened, &
        'represented transitive change preserves a complete narrow model')
    call require(.not. case_is_slow(selected, 'test_alpha'), &
        'ordinary affected case keeps its budget class')
    call select(baseline, candidate, repeated)
    call require(same_selection(selected, repeated), &
        'selection and digest do not depend on action-cache warmth')
    call gfortran_test_names(project, run_names, 2, run_log, run_exit, &
        include_slow=.true., use_cache=.true.)
    call require(run_exit /= 0 .and. file_contains(run_log, &
        'TEST_RESULT test_alpha FAIL') .and. file_contains(run_log, &
        'TEST_RESULT test_beta_slow PASS'), &
        'changed compiled function breaks alpha while beta remains valid')
    call require(has_case(selected, 'test_alpha') .and. &
        .not. has_case(selected, 'test_beta_slow'), &
        'narrow selector includes the independent failing oracle only')
    call fs_remove_tree(trim(project)//'/build/fo')
    call set_cache_root(cold_cache)
    call fs_collect_files(cold_cache, '', '', '', cache_files, n_cache_files)
    call require(n_cache_files == 0, 'cold isolated action cache starts empty')
    call gfortran_test_names(project, run_names, 2, run_log, run_exit, &
        include_slow=.true., use_cache=.true.)
    call require(run_exit /= 0 .and. file_contains(run_log, &
        'TEST_RESULT test_alpha FAIL') .and. file_contains(run_log, &
        'TEST_RESULT test_beta_slow PASS'), &
        'empty isolated action cache preserves fixture behavior')
    call select(baseline, candidate, repeated)
    call require(same_selection(selected, repeated), &
        'warm and empty isolated caches yield identical selected identities')

    call fs_write_text(source_b, 'module b_base'//new_line('a')// &
        'integer, parameter :: b_value = 2'//new_line('a')// &
        'end module b_base'//new_line('a'))
    call discover(candidate)
    call scan_model()
    call select(baseline, candidate, selected)
    call require(has_case(selected, 'test_beta_slow') .and. &
        case_is_slow(selected, 'test_beta_slow'), &
        'affected slow case is selected with its slow classification')
    run_names(1) = 'test_beta_slow'
    call gfortran_test_names(project, run_names(:1), 1, run_log, run_exit, &
        include_slow=.true., use_cache=.true.)
    call require(run_exit == 0 .and. file_contains(run_log, &
        'TEST_RESULT test_beta_slow PASS'), &
        'affected slow test retains its own executable oracle and budget')
    baseline = candidate

    call fs_write_text(include_file, 'integer, parameter :: shared = 2'//new_line('a'))
    call widen_after_change('shared include change selects all cases')
    call fs_write_text(harness_file, 'module harness'//new_line('a')// &
        'integer, parameter :: version = 2'//new_line('a')// &
        'end module harness'//new_line('a'))
    call widen_after_change('shared harness change selects all cases')
    call fs_write_text(runtime_file, 'changed runtime input'//new_line('a'))
    call widen_after_change('runtime and environment change selects all cases')
    call fs_write_text(toolchain_file, 'different compiler identity'//new_line('a'))
    call widen_after_change('toolchain change selects all cases')
    call fs_write_text(trim(project)//'/fpm.toml', &
        'name = "impact_fixture_v2"'//new_line('a'))
    call widen_after_change('manifest change selects all cases')

    call fs_write_text(trim(project)//'/src/new_module.f90', &
        'module new_module'//new_line('a')//'end module new_module'//new_line('a'))
    call widen_after_change('new input selects all cases')
    call discover(baseline)
    filesystem_status = fs_rename(trim(project)//'/src/new_module.f90', &
        trim(project)//'/src/renamed_module.f90')
    call require(filesystem_status == 0, 'fixture module rename succeeds')
    call widen_after_change('renamed input selects all cases')
    call discover(baseline)
    call fs_remove_file(trim(project)//'/src/renamed_module.f90')
    call widen_after_change('deleted input selects all cases')

    call discover(candidate)
    call scan_model()
    call fs_write_text(trim(project)//'/test/test_beta_slow.f90', &
        'program test_beta_slow'//new_line('a')// &
        'use b_base'//new_line('a')//'use unsupported_package'//new_line('a')// &
        'end program test_beta_slow'//new_line('a'))
    call discover(candidate)
    call scan_model()
    call require(.not. dependency_model_complete, &
        'scanner reports an unsupported module edge')
    call select(candidate, candidate, selected)
    call require(selected%widened .and. .not. selected%model_complete .and. &
        all_cases_selected(selected), &
        'unsupported dependency edge selects every eligible case')
    call require(has_case(selected, 'test_beta_slow') .and. &
        any_slow_case(selected), 'widening includes affected slow cases')
    call fs_remove_tree(trim(fixture))

contains

    subroutine discover(inventory)
        type(input_inventory_t), intent(out) :: inventory
        call input_inventory_discover(trim(project), declarations, inventory, &
            ierr, message)
        call require(ierr == 0, 'canonical inventory discovery: '//trim(message))
    end subroutine discover

    subroutine set_cache_root(root)
        character(len=*), intent(in) :: root
        integer(c_int) :: status
        status = set_environment('FO_CACHE_DIR'//c_null_char, &
            trim(root)//c_null_char, 1_c_int)
        call require(status == 0, 'isolated action cache root is selected')
    end subroutine set_cache_root

    logical function file_contains(path, text)
        character(len=*), intent(in) :: path, text
        character(len=4096) :: line
        integer :: unit, ios
        file_contains = .false.
        open (newunit=unit, file=trim(path), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, trim(text)) == 0) cycle
            file_contains = .true.
            exit
        end do
        close (unit)
    end function file_contains

    subroutine scan_model()
        type(scan_unit_t), allocatable :: units(:)
        type(test_impact_case_t), allocatable :: found_cases(:)
        integer :: n_units, scan_error, i, count_cases, next_case, cut
        character(len=MAX_PATH) :: relative

        call scan_dir_regex(trim(project), units, n_units, scan_error)
        call require(scan_error == 0, 'native scanner reads fixture sources')
        call build_dag_from_units(units, n_units, dag, filenames)
        dependency_model_complete = .true.
        do i = 1, n_units
            do cut = 1, units(i)%n_deps
                if (dag_find_node(dag, trim(units(i)%deps(cut))) > 0) cycle
                dependency_model_complete = .false.
            end do
        end do
        count_cases = 0
        do i = 1, n_units
            if (.not. units(i)%is_program) cycle
            if (index(units(i)%filename, '/test/') == 0) cycle
            count_cases = count_cases + 1
        end do
        allocate(found_cases(count_cases))
        next_case = 0
        do i = 1, n_units
            if (.not. units(i)%is_program) cycle
            if (index(units(i)%filename, '/test/') == 0) cycle
            next_case = next_case + 1
            found_cases(next_case)%public_name = units(i)%program_name
            found_cases(next_case)%node_id = dag_find_node(dag, &
                trim(units(i)%program_name))
            cut = len_trim(project) + 2
            relative = units(i)%filename(cut:)
            found_cases(next_case)%identity = 'project|'//trim(relative)// &
                '|test-oracle'
            found_cases(next_case)%slow = is_slow_test(units(i)%program_name)
        end do
        call move_alloc(found_cases, cases)
    end subroutine scan_model

    subroutine select(before, after, result)
        type(input_inventory_t), intent(in) :: before, after
        type(test_impact_result_t), intent(out) :: result
        call test_impact_select(before, after, dag, filenames, cases, &
            dependency_model_complete, result, ierr, message)
        call require(ierr == 0, 'selector returns a classified result: '//trim(message))
        call require(len_trim(result%model_identity) > 0, &
            'selection carries a stable model identity')
    end subroutine select

    subroutine widen_after_change(expectation)
        character(len=*), intent(in) :: expectation
        call discover(candidate)
        call scan_model()
        call select(baseline, candidate, selected)
        call require(selected%widened .and. all_cases_selected(selected), expectation)
        baseline = candidate
    end subroutine widen_after_change

    logical function has_case(result, name)
        type(test_impact_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i
        has_case = .false.
        do i = 1, result%required_count
            if (trim(result%required(i)%public_name) == trim(name)) then
                has_case = .true.
                return
            end if
        end do
    end function has_case

    logical function all_cases_selected(result)
        type(test_impact_result_t), intent(in) :: result
        all_cases_selected = result%required_count == size(cases)
    end function all_cases_selected

    logical function any_slow_case(result)
        type(test_impact_result_t), intent(in) :: result
        integer :: i
        any_slow_case = .false.
        do i = 1, result%required_count
            if (result%required(i)%slow) then
                any_slow_case = .true.
                return
            end if
        end do
    end function any_slow_case

    logical function case_is_slow(result, name)
        type(test_impact_result_t), intent(in) :: result
        character(len=*), intent(in) :: name
        integer :: i
        case_is_slow = .false.
        do i = 1, result%required_count
            if (trim(result%required(i)%public_name) /= trim(name)) cycle
            case_is_slow = result%required(i)%slow
            return
        end do
    end function case_is_slow

    logical function same_selection(left, right)
        type(test_impact_result_t), intent(in) :: left, right
        integer :: i
        same_selection = .false.
        if (left%required_count /= right%required_count) return
        if (left%model_identity /= right%model_identity) return
        do i = 1, left%required_count
            if (left%required(i)%public_name /= right%required(i)%public_name) return
            if (left%required(i)%identity /= right%required(i)%identity) return
            if (left%required(i)%reason /= right%required(i)%reason) return
        end do
        same_selection = .true.
    end function same_selection

    subroutine require(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        if (condition) return
        write (*, '(a)') 'FAIL: '//trim(description)
        error stop 1
    end subroutine require

end program test_gremlin_test_impact
