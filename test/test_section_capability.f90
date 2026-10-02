program test_section_capability
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_capabilities, only: compiler_supports_section_splitting
    use fo_compiler_dialect, only: selected_compiler_command
    use fo_fs, only: fs_find_executable, fs_make_dir, fs_remove_tree
    use fo_gfortran_build, only: gfortran_test
    use fo_util, only: make_tmpfile
    implicit none

    character(len=512) :: compiler, old_compiler, flang_path
    integer :: environment_status, status
    logical :: found

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(code)
            import c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: code
        end function setenv
        function unsetenv(name) bind(C, name='unsetenv') result(code)
            import c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int) :: code
        end function unsetenv
    end interface

    compiler = selected_compiler_command()
    call get_environment_variable('FO_FC', old_compiler, status=environment_status)
    if (compiler_supports_section_splitting(trim(compiler), &
        '--fo-unsupported-section-probe-flag')) &
        error stop 'unknown compiler flag was accepted'
    call check_native_source(trim(compiler))
    call fs_find_executable('flang-new', flang_path, found)
    if (.not. found) call fs_find_executable('flang', flang_path, found)
    if (found) then
        status = setenv('FO_FC'//c_null_char, trim(flang_path)//c_null_char, 1_c_int)
        if (status /= 0) error stop 'cannot select Flang'
        call check_native_source(trim(flang_path))
    else
        write (output_unit, '(a)') 'SKIP: LLVM Flang is not installed'
    end if
    if (environment_status == 0) then
        status = setenv('FO_FC'//c_null_char, trim(old_compiler)//c_null_char, 1_c_int)
    else
        status = unsetenv('FO_FC'//c_null_char)
    end if
    if (status /= 0) error stop 'cannot restore compiler selection'

contains

    subroutine check_native_source(command)
        character(len=*), intent(in) :: command
        character(len=512) :: project, log
        integer :: unit, result

        call make_tmpfile('fo_section_source', project)
        call fs_make_dir(trim(project))
        call fs_make_dir(trim(project)//'/src')
        call fs_make_dir(trim(project)//'/test')
        log = trim(project)//'/build.log'
        open (newunit=unit, file=trim(project)//'/fpm.toml', status='replace')
        write (unit, '(a)') 'name="section-source-oracle"'
        close (unit)
        open (newunit=unit, file=trim(project)//'/src/answer.f90', status='replace')
        write (unit, '(a)') 'module answer'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'integer :: stored_value = 7'
        write (unit, '(a)') 'contains'
        write (unit, '(a)') 'integer function compute()'
        write (unit, '(a)') 'compute = 6 * stored_value'
        write (unit, '(a)') 'end function compute'
        write (unit, '(a)') 'end module answer'
        close (unit)
        open (newunit=unit, file=trim(project)//'/test/oracle.f90', status='replace')
        write (unit, '(a)') 'program oracle'
        write (unit, '(a)') 'use answer, only: compute'
        write (unit, '(a)') 'implicit none'
        write (unit, '(a)') 'if (compute() /= 42) error stop 1'
        write (unit, '(a)') 'end program oracle'
        close (unit)
        call gfortran_test(project, log, result, use_cache=.false.)
        if (result /= 0) then
            write (error_unit, '(a)') 'native source oracle failed for '//trim(command)
            write (error_unit, '(a)') 'log: '//trim(log)
            error stop 1
        end if
        call fs_remove_tree(trim(project))
    end subroutine check_native_source

end program test_section_capability
