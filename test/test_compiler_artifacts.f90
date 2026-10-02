program test_compiler_artifacts
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_fs, only: fs_find_executable, fs_make_dir, fs_remove_tree
    use fo_gfortran_build, only: gfortran_build
    use fo_util, only: make_tmpfile
    implicit none

    character(len=512) :: project, compiler, wrapper, marker, log, object, old_fc
    integer :: unit, status, old_status, result, mode
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

    call fs_find_executable('gfortran', compiler, found)
    if (.not. found) error stop 'artifact oracle needs the real gfortran'
    call make_tmpfile('fo_compiler_artifacts', project)
    call fs_make_dir(trim(project)//'/src')
    log = trim(project)//'/build.log'
    wrapper = trim(project)//'/fixture-gfortran'
    marker = trim(project)//'/silent'
    object = trim(project)//'/build/fo/obj/src_answer.f90.o'
    open (newunit=unit, file=trim(project)//'/fpm.toml', status='replace')
    write (unit, '(a)') 'name="compiler-artifacts-oracle"'
    close (unit)
    open (newunit=unit, file=trim(project)//'/src/answer.f90', status='replace')
    write (unit, '(a)') 'module answer'
    write (unit, '(a)') 'implicit none'
    write (unit, '(a)') 'integer :: value = 42'
    write (unit, '(a)') 'end module answer'
    close (unit)
    call write_wrapper()
    call execute_command_line('chmod +x "'//trim(wrapper)//'"', exitstat=status)
    if (status /= 0) error stop 'cannot make compiler wrapper executable'
    call get_environment_variable('FO_FC', old_fc, status=old_status)
    status = setenv('FO_FC'//c_null_char, trim(wrapper)//c_null_char, 1_c_int)
    if (status /= 0) error stop 'cannot select compiler wrapper'

    call gfortran_build(project, log, result, use_cache=.false., build_apps=.false.)
    if (result /= 0) error stop 'real compiler must first build the source'
    inquire (file=trim(object), exist=found)
    if (.not. found) error stop 'real compiler produced no object'

    ! A silent compiler must neither preserve a stale object nor pass with an
    ! absent or empty output. The same executable identity avoids tree reset.
    do mode = 1, 4
        open (newunit=unit, file=trim(marker), status='replace')
        if (mode == 3) write (unit, '(a)') 'empty'
        if (mode == 4) write (unit, '(a)') 'retry'
        close (unit)
        call gfortran_build(project, log, result, use_cache=.false., &
            build_apps=.false.)
        if (result == 0) then
            if (mode == 4) error stop 'compiler retry reused an earlier failed output'
            error stop 'compiler success without object was accepted'
        end if
        inquire (file=trim(object), exist=found)
        if (found) error stop 'invalid compiler output survived a failed build'
        call check_diagnostic()
        if (mode == 4) then
            inquire (file=trim(marker)//'.retried', exist=found)
            if (.not. found) error stop 'compiler retry was not exercised'
        end if
    end do

    if (old_status == 0) then
        status = setenv('FO_FC'//c_null_char, trim(old_fc)//c_null_char, 1_c_int)
    else
        status = unsetenv('FO_FC'//c_null_char)
    end if
    if (status /= 0) error stop 'cannot restore compiler selection'
    call fs_remove_tree(trim(project))
    print '(a)', 'PASS: compiler requires fresh objects, including after a failed retry'

contains

    subroutine write_wrapper()
        integer :: unit

        open (newunit=unit, file=trim(wrapper), status='replace')
        write (unit, '(a)') '#!/bin/sh'
        write (unit, '(a)') 'if [ -f "'//trim(marker)//'" ]; then'
        write (unit, '(a)') '  mode=$(cat "'//trim(marker)//'")'
        write (unit, '(a)') '  if [ "$mode" = retry ]; then'
        write (unit, '(a)') '    if [ -f "'//trim(marker)//'.failed" ]; then'
        write (unit, '(a)') '      : > "'//trim(marker)//'.retried"; exit 0'
        write (unit, '(a)') '    fi'
        write (unit, '(a)') '    : > "'//trim(marker)//'.failed"'
        write (unit, '(a)') '    while [ "$#" -gt 0 ]; do'
        write (unit, '(a)') '      if [ "$1" = -o ]; then'
        write (unit, '(a)') '        shift; printf failed-output > "$1"; break'
        write (unit, '(a)') '      fi'
        write (unit, '(a)') '      shift'
        write (unit, '(a)') '    done'
        write (unit, '(a)') '    echo "Fatal Error: Cannot read module file" >&2'
        write (unit, '(a)') '    exit 1'
        write (unit, '(a)') '  fi'
        write (unit, '(a)') '  if [ "$mode" = empty ]; then'
        write (unit, '(a)') '    while [ "$#" -gt 0 ]; do'
        write (unit, '(a)') '      if [ "$1" = -o ]; then shift; : > "$1"; break; fi'
        write (unit, '(a)') '      shift'
        write (unit, '(a)') '    done'
        write (unit, '(a)') '  fi'
        write (unit, '(a)') '  exit 0'
        write (unit, '(a)') 'fi'
        write (unit, '(a)') 'exec "'//trim(compiler)//'" "$@"'
        close (unit)
    end subroutine write_wrapper

    subroutine check_diagnostic()
        character(len=1024) :: line
        integer :: unit, ios
        logical :: found

        found = .false.
        open (newunit=unit, file=trim(log), status='old', action='read')
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'compiler produced no nonempty object') > 0) &
                found = .true.
        end do
        close (unit)
        if (.not. found) error stop 'compiler artifact diagnostic was not reported'
    end subroutine check_diagnostic

end program test_compiler_artifacts
