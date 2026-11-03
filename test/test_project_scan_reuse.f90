program test_project_scan_reuse
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_long_long
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fx_string, only: string_t
    use fo_scan, only: scan_file_parse_count
    use fo_project_model, only: project_model_t, model_unit_t, model_init
    use fo_project_load, only: project_model_units
    use fo_test_harness, only: make_scratch, join_path, make_directory, &
        write_text, remove_tree, set_mtime, finish_assertions
    use fo_fs, only: fs_stat
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) &
                bind(C, name='fo_test_setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
    end interface

    type(project_model_t) :: model
    type(model_unit_t), allocatable :: units(:)
    type(string_t) :: paths(2), broken(1)
    character(len=:), allocatable :: root, first, second, text_a, text_b
    integer :: status, before, cold, i, failures
    integer(c_long_long) :: mtime, bytes
    logical :: ok

    failures = 0
    call make_scratch('project-scan-reuse', root)
    first = join_path(root, 'first')
    second = join_path(root, 'second')
    call make_directory(first)
    call make_directory(second)
    status = c_setenv('FO_CACHE_DIR'//c_null_char, &
        join_path(root, 'cache')//c_null_char, 1_c_int)
    call check(status == 0, 'isolated scan cache')
    text_a = 'module alpha'//new_line('a')//'use olddep'//new_line('a')// &
        'end module alpha'//new_line('a')
    text_b = 'module beta'//new_line('a')//'end module beta'//new_line('a')
    call write_text(join_path(first, 'alpha.f90'), text_a)
    call write_text(join_path(first, 'beta.f90'), text_b)
    call write_text(join_path(second, 'alpha.f90'), text_a)
    call write_text(join_path(second, 'beta.f90'), text_b)
    paths(1)%s = join_path(first, 'alpha.f90')
    paths(2)%s = join_path(first, 'beta.f90')
    call model_init(model, 'test', 'scan-reuse')
    before = scan_file_parse_count()
    call project_model_units(model, paths, units, first, first)
    cold = scan_file_parse_count()
    call check(size(units) == 2 .and. cold - before == 2, 'cold sources parse')
    call project_model_units(model, paths, units, first, second)
    call check(scan_file_parse_count() == cold, 'relocated unchanged sources reuse scans')
    call check(size(units) == 2, 'relocated units remain complete')
    do i = 1, size(units)
        call check(index(units(i)%path, second//'/') == 1, 'unit paths relocate')
    end do
    call fs_stat(join_path(second, 'alpha.f90'), mtime, bytes, ok)
    call check(ok, 'stat source before same-size edit')
    call write_text(join_path(second, 'alpha.f90'), &
        'module alpha'//new_line('a')//'use newdep'//new_line('a')// &
        'end module alpha'//new_line('a'))
    call set_mtime(join_path(second, 'alpha.f90'), mtime)
    call project_model_units(model, paths, units, first, second)
    call check(scan_file_parse_count() == cold + 1, 'only changed source reparses')
    do i = 1, size(units)
        if (units(i)%path /= join_path(second, 'alpha.f90')) cycle
        call check(size(units(i)%uses) == 1, 'edited dependency remains complete')
        if (size(units(i)%uses) == 1) &
            call check(units(i)%uses(1)%s == 'newdep', 'same-size edit updates dependency')
    end do
    broken(1)%s = join_path(second, 'broken.f90')
    call write_text(broken(1)%s, 'module broken_scan'//new_line('a')// &
        'use broken_dep, only: value'//new_line('a')// &
        'integer :: value ='//new_line('a')//'end module broken_scan'//new_line('a'))
    call project_model_units(model, broken, units, second, second)
    before = scan_file_parse_count()
    call check(size(units) == 0, 'failed parse remains unscanned')
    call project_model_units(model, broken, units, second, second)
    call check(scan_file_parse_count() == before, 'unchanged failed parse is not repeated')
    call check(size(units) == 0, 'cached failed parse remains unscanned')
    call write_text(broken(1)%s, 'module repaired_scan'//new_line('a')// &
        'end module repaired_scan'//new_line('a'))
    call project_model_units(model, broken, units, second, second)
    call check(scan_file_parse_count() == before + 1 .and. size(units) == 1, &
        'repair invalidates the failed scan')
    call remove_tree(root)
    call finish_assertions()
    if (failures /= 0) stop 1
contains
    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        if (condition) return
        failures = failures + 1
        write (error_unit, '(a)') 'FAIL: '//label
    end subroutine check
end program test_project_scan_reuse
