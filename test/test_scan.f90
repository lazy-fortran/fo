program test_scan
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_scan, only: scan_unit_t, scan_file, scan_dir, MAX_UNITS, is_slow_test
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_scan_use_statements()
    call test_scan_module_def()
    call test_scan_module_name_containing_procedure()
    call test_scan_submodule_def()
    call test_scan_external_subroutine_def()
    call test_scan_contained_subroutine_keeps_program()
    call test_scan_program_def()
    call test_scan_intrinsic_skip()
    call test_slow_test_detection()
    call test_scan_dir_empty()
    call test_scan_dir_path_with_spaces()
    call test_scan_dir_skips_nested_projects()
    call test_scan_dir_skips_build_outputs()
    call test_scan_dir_skips_agent_worktrees()
    call test_scan_classifies_test_by_directory()

    write (output_unit, '(a,i0,a,i0,a)') 'scan: ', n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine write_file(filename, lines, n_lines)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: n_lines
        integer :: u, i

        open (newunit=u, file=filename, status='replace')
        do i = 1, n_lines
            write (u, '(a)') trim(lines(i))
        end do
        close (u)
    end subroutine write_file

    subroutine test_scan_use_statements()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(5)

        lines(1) = 'module foo'
        lines(2) = '    use bar, only: x'
        lines(3) = '    use baz'
        lines(4) = '    implicit none'
        lines(5) = 'end module foo'
        call make_tmp_path('fo_test_use', path, '.f90')
        call write_file(path, lines, 5)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_use: no error')
        call assert(trim(info%module_name) == 'foo', 'scan_use: module name')
        call assert(info%n_deps == 2, 'scan_use: 2 deps')
        call assert(trim(info%deps(1)) == 'bar', 'scan_use: dep 1 is bar')
        call assert(trim(info%deps(2)) == 'baz', 'scan_use: dep 2 is baz')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_use_statements

    subroutine test_scan_module_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(3)

        lines(1) = 'module my_module'
        lines(2) = '    implicit none'
        lines(3) = 'end module my_module'
        call make_tmp_path('fo_test_mod', path, '.f90')
        call write_file(path, lines, 3)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_mod: no error')
        call assert(trim(info%module_name) == 'my_module', 'scan_mod: name')
        call assert(info%n_deps == 0, 'scan_mod: no deps')
        call assert(.not. info%is_program, 'scan_mod: not a program')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_module_def

    subroutine test_scan_module_name_containing_procedure()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(3)

        lines(1) = 'module ast_nodes_procedure'
        lines(2) = '    implicit none'
        lines(3) = 'end module ast_nodes_procedure'
        call make_tmp_path('fo_test_proc_name', path, '.f90')
        call write_file(path, lines, 3)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_proc_name: no error')
        call assert(trim(info%module_name) == 'ast_nodes_procedure', &
            'scan_proc_name: module name')
        call execute_command_line('rm -f '//trim(path))

        lines(1) = 'module procedure_classification'
        lines(2) = '    implicit none'
        lines(3) = 'end module procedure_classification'
        call make_tmp_path('fo_test_proc_prefix', path, '.f90')
        call write_file(path, lines, 3)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_proc_prefix: no error')
        call assert(trim(info%module_name) == 'procedure_classification', &
            'scan_proc_prefix: module name')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_module_name_containing_procedure

    subroutine test_scan_submodule_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(3)

        lines(1) = 'submodule(semantic_analyzer) semantic_analyzer_context_impl'
        lines(2) = '    implicit none'
        lines(3) = 'end submodule semantic_analyzer_context_impl'
        call make_tmp_path('fo_test_submodule', path, '.f90')
        call write_file(path, lines, 3)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_submodule: no error')
        call assert(trim(info%module_name) == 'semantic_analyzer_context_impl', &
            'scan_submodule: submodule name')
        call assert(info%n_deps == 1, 'scan_submodule: one parent dependency')
        call assert(trim(info%deps(1)) == 'semantic_analyzer', &
            'scan_submodule: parent dependency')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_submodule_def

    subroutine test_scan_external_subroutine_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(3)

        lines(1) = 'subroutine ensure_if_do_registration_bridge()'
        lines(2) = '    implicit none'
        lines(3) = 'end subroutine ensure_if_do_registration_bridge'
        call make_tmp_path('fo_test_external_subroutine', path, '.f90')
        call write_file(path, lines, 3)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_external_subroutine: no error')
        call assert(trim(info%module_name) == 'ensure_if_do_registration_bridge', &
            'scan_external_subroutine: procedure name')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_external_subroutine_def

    subroutine test_scan_contained_subroutine_keeps_program()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(6)

        lines(1) = 'program test_named'
        lines(2) = 'contains'
        lines(3) = 'subroutine helper()'
        lines(4) = 'end subroutine helper'
        lines(5) = 'end program test_named'
        lines(6) = ''
        call make_tmp_path('fo_test_contained_subroutine', path, '.f90')
        call write_file(path, lines, 5)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_contained_subroutine: no error')
        call assert(trim(info%program_name) == 'test_named', &
            'scan_contained_subroutine: program name')
        call assert(len_trim(info%module_name) == 0, &
            'scan_contained_subroutine: no external unit')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_contained_subroutine_keeps_program

    subroutine test_scan_program_def()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(4)

        lines(1) = 'program main'
        lines(2) = '    use utils, only: helper'
        lines(3) = '    implicit none'
        lines(4) = 'end program main'
        call make_tmp_path('fo_test_prog', path, '.f90')
        call write_file(path, lines, 4)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_prog: no error')
        call assert(trim(info%program_name) == 'main', 'scan_prog: name')
        call assert(info%is_program, 'scan_prog: is program')
        call assert(info%n_deps == 1, 'scan_prog: 1 dep')
        call assert(trim(info%deps(1)) == 'utils', 'scan_prog: dep is utils')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_program_def

    subroutine test_scan_intrinsic_skip()
        type(scan_unit_t) :: info
        integer :: ierr
        character(len=512) :: path
        character(len=80) :: lines(5)

        lines(1) = 'module calc'
        lines(2) = '    use, intrinsic :: iso_fortran_env, only: dp => real64'
        lines(3) = '    use my_lib'
        lines(4) = '    implicit none'
        lines(5) = 'end module calc'
        call make_tmp_path('fo_test_intrinsic', path, '.f90')
        call write_file(path, lines, 5)

        call scan_file(path, info, ierr)
        call assert(ierr == 0, 'scan_intrinsic: no error')
        call assert(info%n_deps == 1, 'scan_intrinsic: 1 dep (intrinsic skipped)')
        call assert(trim(info%deps(1)) == 'my_lib', 'scan_intrinsic: dep is my_lib')
        call execute_command_line('rm -f '//trim(path))
    end subroutine test_scan_intrinsic_skip

    subroutine test_slow_test_detection()
        ! *_slow suffix
        call assert(is_slow_test('test_integration_slow'), &
            'slow: test_integration_slow is slow')
        call assert(is_slow_test('perf_slow'), &
            'slow: perf_slow is slow')

        ! *_slow_* infix
        call assert(is_slow_test('test_slow_network'), &
            'slow: test_slow_network is slow')
        call assert(is_slow_test('my_slow_test'), &
            'slow: my_slow_test is slow')

        ! not slow
        call assert(.not. is_slow_test('test_fast'), &
            'slow: test_fast is not slow')
        call assert(.not. is_slow_test('test_slowly'), &
            'slow: test_slowly is not slow')
        call assert(.not. is_slow_test('slowtest'), &
            'slow: slowtest is not slow')
        call assert(.not. is_slow_test('test_cache'), &
            'slow: test_cache is not slow')
    end subroutine test_slow_test_detection

    subroutine test_scan_dir_empty()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr
        character(len=512) :: dir

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_empty_dir', dir, '')
        call execute_command_line('rm -rf '//trim(dir))
        call execute_command_line('mkdir -p '//trim(dir))

        call scan_dir(dir, units, n_units, ierr)
        call assert(ierr == 0, 'empty_dir: no error')
        call assert(n_units == 0, 'empty_dir: 0 files')

        call execute_command_line('rm -rf '//trim(dir))
    end subroutine test_scan_dir_empty

    subroutine test_scan_dir_path_with_spaces()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, n_tests
        character(len=512) :: base_dir, dir
        character(len=80) :: lib_lines(3), test_lines(3)

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_scan_spaces', base_dir, '')
        dir = trim(base_dir)//' path with spaces'
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src')
        call make_dir(trim(dir)//'/test')

        lib_lines(1) = 'module lib'
        lib_lines(2) = 'implicit none'
        lib_lines(3) = 'end module lib'
        call write_file(trim(dir)//'/src/lib.f90', lib_lines, 3)

        test_lines(1) = 'program test_fast'
        test_lines(2) = 'use lib'
        test_lines(3) = 'end program test_fast'
        call write_file(trim(dir)//'/test/test_fast.f90', test_lines, 3)

        call scan_dir(dir, units, n_units, ierr)
        n_tests = count(units(1:n_units)%is_test)
        call assert(ierr == 0, 'scan spaces: no error')
        call assert(n_units == 2, 'scan spaces: finds source files')
        call assert(n_tests == 1, 'scan spaces: marks test file')

        call remove_tree(dir)
    end subroutine test_scan_dir_path_with_spaces

    subroutine test_scan_dir_skips_nested_projects()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, u
        character(len=512) :: dir
        character(len=80) :: root_lines(3), nested_lines(3)

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_nested_project', dir, '')
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src')
        call make_dir(trim(dir)//'/bench/nested/src')

        open (newunit=u, file=trim(dir)//'/fpm.toml', status='replace')
        write (u, '(a)') 'name = "root"'
        close (u)
        open (newunit=u, file=trim(dir)//'/bench/nested/fpm.toml', &
            status='replace')
        write (u, '(a)') 'name = "nested"'
        close (u)

        root_lines(1) = 'module root_mod'
        root_lines(2) = 'implicit none'
        root_lines(3) = 'end module root_mod'
        call write_file(trim(dir)//'/src/root_mod.f90', root_lines, 3)

        nested_lines(1) = 'module nested_mod'
        nested_lines(2) = 'implicit none'
        nested_lines(3) = 'end module nested_mod'
        call write_file(trim(dir)//'/bench/nested/src/nested_mod.f90', &
            nested_lines, 3)

        call scan_dir(dir, units, n_units, ierr)
        call assert(ierr == 0, 'nested project: no scan error')
        call assert(n_units == 1, 'nested project: nested fpm tree skipped')
        call assert(trim(units(1)%module_name) == 'root_mod', &
            'nested project: root module remains')

        call remove_tree(dir)
    end subroutine test_scan_dir_skips_nested_projects

    subroutine test_scan_dir_skips_build_outputs()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr, u
        character(len=512) :: dir
        character(len=80) :: root_lines(3), generated_lines(3)
        logical :: ci_fs

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_build_outputs', dir, '')
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src')
        ! On a case-insensitive filesystem (macOS default) 'SRC' aliases 'src',
        ! so the uppercase-SRC fixture would merge into the real source dir and
        ! its vendored tree would be reached below the depth-0 skip. Detect it
        ! with a probe and exercise the SRC case only where it is distinct.
        open (newunit=u, file=trim(dir)//'/src/.fo_probe', status='replace')
        close (u)
        inquire (file=trim(dir)//'/SRC/.fo_probe', exist=ci_fs)
        call make_dir(trim(dir)//'/build/_deps/libneo-src/src')
        call make_dir(trim(dir)//'/build_axisheal/dependencies/libneo/src')
        call make_dir(trim(dir)//'/build-mgd/deps-src/hdf5/src')
        if (.not. ci_fs) call make_dir(trim(dir)//'/SRC/libneo/src')

        open (newunit=u, file=trim(dir)//'/CMakeLists.txt', status='replace')
        write (u, '(a)') 'project(root Fortran)'
        close (u)

        root_lines(1) = 'module root_mod'
        root_lines(2) = 'implicit none'
        root_lines(3) = 'end module root_mod'
        call write_file(trim(dir)//'/src/root_mod.f90', root_lines, 3)

        generated_lines(1) = 'module generated_mod'
        generated_lines(2) = 'implicit none'
        generated_lines(3) = 'end module generated_mod'
        call write_file(trim(dir)//'/build/_deps/libneo-src/src/generated_mod.f90', &
            generated_lines, 3)
        call write_file(trim(dir)//'/build_axisheal/dependencies/libneo/src/generated_mod.f90', &
            generated_lines, 3)
        call write_file(trim(dir)//'/build-mgd/deps-src/hdf5/src/generated_mod.f90', &
            generated_lines, 3)
        if (.not. ci_fs) call write_file(trim(dir)//'/SRC/libneo/src/generated_mod.f90', &
            generated_lines, 3)

        call scan_dir(dir, units, n_units, ierr)
        call assert(ierr == 0, 'build outputs: no scan error')
        call assert(n_units == 1, 'build outputs: generated trees skipped')
        call assert(trim(units(1)%module_name) == 'root_mod', &
            'build outputs: root module remains')

        call remove_tree(dir)
    end subroutine test_scan_dir_skips_build_outputs

    subroutine test_scan_dir_skips_agent_worktrees()
        type(scan_unit_t), allocatable :: units(:)
        integer :: n_units, ierr
        character(len=512) :: dir
        character(len=80) :: root_lines(3), nested_lines(3)

        allocate (units(MAX_UNITS))
        call make_tmp_path('fo_test_agent_worktree', dir, '')
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src')
        call make_dir(trim(dir)//'/.claude/worktrees/wf/src')

        root_lines(1) = 'module root_mod'
        root_lines(2) = 'implicit none'
        root_lines(3) = 'end module root_mod'
        call write_file(trim(dir)//'/src/root_mod.f90', root_lines, 3)

        nested_lines(1) = 'module ghost_mod'
        nested_lines(2) = 'implicit none'
        nested_lines(3) = 'end module ghost_mod'
        call write_file(trim(dir)//'/.claude/worktrees/wf/src/ghost_mod.f90', &
            nested_lines, 3)

        call scan_dir(dir, units, n_units, ierr)
        call assert(ierr == 0, 'agent worktree: no scan error')
        call assert(n_units == 1, 'agent worktree: hidden worktree skipped')
        call assert(trim(units(1)%module_name) == 'root_mod', &
            'agent worktree: root module remains')

        call remove_tree(dir)
    end subroutine test_scan_dir_skips_agent_worktrees

    subroutine test_scan_classifies_test_by_directory()
        ! Classification follows fpm: a unit is a test iff it lives under a test
        ! directory. A library module whose name starts with test_ (e.g.
        ! src/utilities/test_shell_commands.f90) is NOT a test, or it would be
        ! dropped from the library build and break the link.
        type(scan_unit_t) :: unit_info
        integer :: ierr
        character(len=512) :: dir, libpath, testpath
        character(len=80) :: lines(3)

        call make_tmp_path('fo_test_classify', dir, '')
        call remove_tree(dir)
        call make_dir(trim(dir)//'/src/utilities')
        call make_dir(trim(dir)//'/test')

        lines(1) = 'module test_shell_commands'
        lines(2) = 'implicit none'
        lines(3) = 'end module test_shell_commands'
        libpath = trim(dir)//'/src/utilities/test_shell_commands.f90'
        call write_file(trim(libpath), lines, 3)

        lines(1) = 'module test_helper'
        lines(2) = 'implicit none'
        lines(3) = 'end module test_helper'
        testpath = trim(dir)//'/test/test_helper.f90'
        call write_file(trim(testpath), lines, 3)

        call scan_file(trim(libpath), unit_info, ierr)
        call assert(ierr == 0, 'classify: library file scanned')
        call assert(.not. unit_info%is_test, &
            'classify: src test_ module is not a test')

        call scan_file(trim(testpath), unit_info, ierr)
        call assert(ierr == 0, 'classify: test file scanned')
        call assert(unit_info%is_test, &
            'classify: file under test/ is a test')

        call remove_tree(dir)
    end subroutine test_scan_classifies_test_by_directory

    subroutine make_tmp_path(prefix, path, suffix)
        character(len=*), intent(in) :: prefix, suffix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, trim(suffix)
    end subroutine make_tmp_path

    subroutine make_dir(path)
        character(len=*), intent(in) :: path

        call execute_command_line('mkdir -p "'//trim(path)//'"')
    end subroutine make_dir

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf "'//trim(path)//'"')
    end subroutine remove_tree

end program test_scan
