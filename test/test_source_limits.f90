program test_source_limits
    !! Negative oracles: sources, units, flags and imports must neither be
    !! silently excluded nor silently truncated, and manifest tables fo does
    !! not read must be reported.
    use fo_scan, only: scan_unit_t, scan_dir_regex, MAX_UNITS
    use fo_dag_bridge, only: build_dag_from_units
    use fx_dag, only: dag_t, MAX_NODES
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_lint, only: lint_finding_t, lint_file, MAX_FINDINGS
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, finish_assertions
    implicit none

    character(len=1), parameter :: nl = new_line('a')
    character(:), allocatable :: scratch

    call make_scratch('fo-source-limits', scratch)
    call test_fpm_source_scan_exclusions()
    call test_too_many_sources_fail()
    call test_overlong_source_path_fails()
    call test_dag_node_limit_fails()
    call test_manifest_tables()
    call test_manifest_flag_and_library_limits()
    call test_use_only_symbol_limit()
    call remove_tree(scratch)
    call finish_assertions()

contains

    logical function listed(units, n, fragment)
        type(scan_unit_t), intent(in) :: units(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: fragment
        integer :: i

        listed = .false.
        do i = 1, n
            if (index(units(i)%filename, fragment) > 0) listed = .true.
        end do
    end function listed

    subroutine test_fpm_source_scan_exclusions()
        character(:), allocatable :: root
        type(scan_unit_t), allocatable :: units(:)
        integer :: n, ierr

        root = join_path(scratch, 'scan_policy')
        call write_text(join_path(root, 'fpm.toml'), 'name = "scan_policy"'//nl)
        call write_text(join_path(root, 'src/dependencies/dep.f90'), &
            'module in_dependencies'//nl//'end module in_dependencies'//nl)
        call write_text(join_path(root, 'src/venv/venv_mod.f90'), &
            'module in_venv'//nl//'end module in_venv'//nl)
        call write_text(join_path(root, 'src/node_modules/nm.f90'), &
            'module in_node_modules'//nl//'end module in_node_modules'//nl)
        call write_text(join_path(root, 'src/_deps/d.f90'), &
            'module in_underscore_deps'//nl//'end module in_underscore_deps'//nl)
        call write_text(join_path(root, 'build/skipped.f90'), &
            'module in_build'//nl//'end module in_build'//nl)
        call write_text(join_path(root, '.hidden/skipped.f90'), &
            'module in_hidden'//nl//'end module in_hidden'//nl)
        call scan_dir_regex(root, units, n, ierr)
        call assert_true(ierr == 0, 'policy scan succeeds')
        call assert_true(listed(units, n, 'src/dependencies/dep.f90'), &
            'FPM builds src/dependencies, so fo must scan it')
        call assert_true(listed(units, n, 'src/venv/venv_mod.f90'), &
            'src/venv is a source directory')
        call assert_true(listed(units, n, 'src/node_modules/nm.f90'), &
            'src/node_modules is a source directory')
        call assert_true(listed(units, n, 'src/_deps/d.f90'), &
            'src/_deps is a source directory')
        call assert_true(.not. listed(units, n, 'build/skipped.f90'), &
            'the project build directory is excluded')
        call assert_true(.not. listed(units, n, '.hidden/'), &
            'hidden directories are excluded')
    end subroutine test_fpm_source_scan_exclusions

    subroutine test_too_many_sources_fail()
        character(:), allocatable :: root
        character(len=32) :: name
        type(scan_unit_t), allocatable :: units(:)
        integer :: i, n, ierr

        root = join_path(scratch, 'many')
        call write_text(join_path(root, 'fpm.toml'), 'name = "many"'//nl)
        do i = 1, MAX_UNITS + 1
            write (name, '(a,i0,a)') 'src/s', i, '.f90'
            call write_text(join_path(root, trim(name)), 'end'//nl)
        end do
        call scan_dir_regex(root, units, n, ierr)
        call assert_true(ierr /= 0, 'more sources than MAX_UNITS is an error')
        call remove_tree(root)
    end subroutine test_too_many_sources_fail

    subroutine test_overlong_source_path_fails()
        character(:), allocatable :: root, path
        type(scan_unit_t), allocatable :: units(:)
        integer :: i, n, ierr

        root = join_path(scratch, 'long')
        call write_text(join_path(root, 'fpm.toml'), 'name = "long"'//nl)
        path = join_path(root, 'src')
        do i = 1, 6
            path = join_path(path, repeat(char(96 + i), 100))
        end do
        call write_text(join_path(path, 'deep.f90'), &
            'module deep'//nl//'end module deep'//nl)
        call scan_dir_regex(root, units, n, ierr)
        call assert_true(ierr /= 0, 'a source path beyond the unit path length is an error')
    end subroutine test_overlong_source_path_fails

    subroutine test_dag_node_limit_fails()
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        character(len=24) :: name
        integer :: i, n, ierr

        n = MAX_NODES + 1
        allocate (units(n))
        do i = 1, n
            write (name, '(a,i0)') 'mod_', i
            units(i)%filename = trim(name)//'.f90'
            units(i)%module_name = name
            allocate (units(i)%deps(0))
        end do
        call build_dag_from_units(units, n, dag, ierr=ierr)
        call assert_true(ierr /= 0, 'units beyond the graph capacity are an error')
    end subroutine test_dag_node_limit_fails

    subroutine parse_manifest(text, ierr)
        character(len=*), intent(in) :: text
        integer, intent(out) :: ierr
        type(fpm_config_t) :: config
        character(:), allocatable :: root

        root = join_path(scratch, 'manifest')
        call write_text(join_path(root, 'fpm.toml'), text)
        call fpm_config_parse(root, config, ierr)
    end subroutine parse_manifest

    subroutine test_manifest_tables()
        integer :: ierr

        call parse_manifest('name = "p"'//nl//'[extra.tool]'//nl//'x = 1'//nl// &
            '[install]'//nl//'library = true'//nl//'[profiles]'//nl, ierr)
        call assert_true(ierr == 0, 'tables accepted by fpm are accepted')
        call parse_manifest('name = "p"'//nl//'[librery]'//nl//'source-dir = "x"'//nl, &
            ierr)
        call assert_true(ierr /= 0, 'a misspelled table is an error')
        call parse_manifest('name = "p"'//nl//'[features]'//nl//'a = 1'//nl, ierr)
        call assert_true(ierr == 0, '[features] may be accepted by fpm: warn only')
        call parse_manifest('name = "p"'//nl//'[feature]'//nl//'a = 1'//nl, ierr)
        call assert_true(ierr /= 0, 'a misspelled feature table is an error')
        call test_dependency_subtables()
    end subroutine test_manifest_tables

    subroutine test_dependency_subtables()
        type(fpm_config_t) :: inline_form, table_form
        character(:), allocatable :: root
        integer :: ierr

        root = join_path(scratch, 'subtables')
        call write_text(join_path(root, 'fpm.toml'), 'name = "p"'//nl// &
            '[dependencies]'//nl// &
            'bar = { git = "https://example.invalid/bar.git", tag = "v1" }'//nl// &
            '[dev-dependencies]'//nl//'loc = { path = "../loc" }'//nl)
        call fpm_config_parse(root, inline_form, ierr)
        call assert_true(ierr == 0, 'inline dependencies parse')
        call write_text(join_path(root, 'fpm.toml'), 'name = "p"'//nl// &
            '[dependencies.bar]'//nl// &
            'git = "https://example.invalid/bar.git"'//nl//'tag = "v1"'//nl// &
            '[dev-dependencies.loc]'//nl//'path = "../loc"'//nl)
        call fpm_config_parse(root, table_form, ierr)
        call assert_true(ierr == 0, 'dependency tables parse')
        call assert_true(table_form%n_deps == 1 .and. inline_form%n_deps == 1, &
            'one dependency in each spelling')
        call assert_true(table_form%n_dev_deps == 1 .and. inline_form%n_dev_deps == 1, &
            'one dev dependency in each spelling')
        if (table_form%n_deps == 1 .and. inline_form%n_deps == 1) then
            call assert_true(trim(table_form%deps(1)%name) == trim(inline_form%deps(1)%name) &
                .and. trim(table_form%deps(1)%git) == trim(inline_form%deps(1)%git) &
                .and. trim(table_form%deps(1)%tag) == trim(inline_form%deps(1)%tag) &
                .and. trim(table_form%deps(1)%git) == 'https://example.invalid/bar.git', &
                'table and inline spellings give the same git dependency')
        end if
        if (table_form%n_dev_deps == 1 .and. inline_form%n_dev_deps == 1) then
            call assert_true(trim(table_form%dev_deps(1)%name) == 'loc' &
                .and. trim(table_form%dev_deps(1)%path) == &
                trim(inline_form%dev_deps(1)%path), 'same dev path dependency')
        end if
    end subroutine test_dependency_subtables

    subroutine test_manifest_flag_and_library_limits()
        character(:), allocatable :: flags, libs
        character(len=24) :: item
        integer :: i, ierr

        flags = ''
        libs = ''
        do i = 1, 70
            write (item, '(a,i0,a)') '"-DF', i, '", '
            flags = flags//trim(item)
            write (item, '(a,i0,a)') '"lib', i, '", '
            libs = libs//trim(item)
        end do
        call parse_manifest('name = "p"'//nl//'[fortran]'//nl//'[build]'//nl// &
            'link = ['//libs//'"last"]'//nl, ierr)
        call assert_true(ierr /= 0, 'too many link libraries is an error')
        call parse_manifest('name = "p"'//nl//'[preprocess.cpp]'//nl// &
            'macros = ['//flags//'"last"]'//nl, ierr)
        call assert_true(ierr /= 0, 'too many macro flags is an error')
        call parse_manifest('name = "p"'//nl//'[build]'//nl//'link = ['//nl// &
            repeat('    "libx",'//nl, 300)//']'//nl, ierr)
        call assert_true(ierr /= 0, 'an overlong multi-line array is an error')
    end subroutine test_manifest_flag_and_library_limits

    subroutine test_use_only_symbol_limit()
        character(:), allocatable :: path, text
        character(len=24) :: item
        type(lint_finding_t) :: findings(MAX_FINDINGS)
        integer :: i, n

        text = 'program m'//nl//'use other, only: '
        do i = 1, 100
            write (item, '(a,i0)') 'sym', i
            if (i > 1) text = text//', '
            text = text//trim(item)
        end do
        text = text//nl//'implicit none'//nl//'end program m'//nl
        path = join_path(scratch, 'many_symbols.f90')
        call write_text(path, text)
        n = 0
        call lint_file(path, findings, n)
        call assert_true(n == 100, 'every unused symbol of a long use list is reported, got '//char(48+n/100)//char(48+mod(n/10,10))//char(48+mod(n,10)))
    end subroutine test_use_only_symbol_limit

end program test_source_limits
