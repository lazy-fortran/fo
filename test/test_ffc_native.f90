program test_ffc_native
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_ffc_native, only: ffc_native_build, ffc_native_run
    use fo_process, only: process_getpid
    use fo_util, only: read_text_file
    implicit none

    interface
        function setenv(name, value, overwrite) bind(C, name='setenv') result(ierr)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
            integer(c_int) :: ierr
        end function setenv
    end interface

    integer :: n_fail, n_pass
    character(len=4096) :: original_path
    character(len=512) :: test_dir

    n_fail = 0
    n_pass = 0
    call get_environment_variable('PATH', original_path)
    write (test_dir, '(a,i0)') '/tmp/fo_ffc_native_', process_getpid()
    call execute_command_line('/bin/mkdir -p "'//trim(test_dir)//'"')
    call write_fake_ffc()

    call test_missing_ffc()
    call test_incompatible_ffc()
    call test_compiler_diagnostic()
    call test_multiple_source_build()
    call test_run_exit_status()

    call set_env('PATH', trim(original_path))
    call execute_command_line('/bin/rm -rf "'//trim(test_dir)//'"')
    write (output_unit, '(a,i0,a,i0)') 'ffc_native: ', n_pass, ' pass, ', n_fail
    if (n_fail > 0) stop 1

contains

    subroutine test_missing_ffc()
        character(len=512) :: sources(1), output
        character(len=512) :: message
        integer :: exitcode

        call set_env('PATH', trim(test_dir)//'/missing')
        sources(1) = trim(test_dir)//'/main.lf'
        output = trim(test_dir)//'/missing-output'
        call ffc_native_build(sources, 1, output, exitcode, message)
        call assert(exitcode == 127, 'missing ffc preserves discovery exit status')
        call assert(index(message, 'native mode requires ffc') > 0, &
            'missing ffc reports discovery error')
    end subroutine test_missing_ffc

    subroutine test_incompatible_ffc()
        character(len=512) :: sources(1), output
        character(len=512) :: message
        integer :: exitcode

        call use_fake_ffc()
        call set_env('FFC_FAKE_VERSION', 'not ffc')
        sources(1) = trim(test_dir)//'/main.lf'
        output = trim(test_dir)//'/incompatible-output'
        call ffc_native_build(sources, 1, output, exitcode, message)
        call assert(exitcode /= 0, 'incompatible ffc fails discovery')
        call assert(index(message, 'incompatible ffc') > 0, &
            'incompatible ffc reports version contract')
        call set_env('FFC_FAKE_VERSION', 'ffc 0.0.9')
        call ffc_native_build(sources, 1, output, exitcode, message)
        call assert(exitcode /= 0 .and. index(message, '0.1.0 or newer') > 0, &
            'old ffc reports minimum compatible version')
        call set_env('FFC_FAKE_VERSION', 'ffc 0.1.0')
    end subroutine test_incompatible_ffc

    subroutine test_compiler_diagnostic()
        character(len=512) :: sources(1), output, log_file
        character(len=1024) :: message, text
        integer :: exitcode

        call use_fake_ffc()
        call set_env('FFC_FAKE_MODE', 'bad')
        call set_env('FFC_FAKE_DIAGNOSTIC', &
            'bad source.lf:4:7: error: unsupported widget')
        sources(1) = trim(test_dir)//'/bad source.lf'
        output = trim(test_dir)//'/bad-output'
        log_file = trim(test_dir)//'/diagnostic.log'
        call ffc_native_build(sources, 1, output, exitcode, message, log_file)
        call read_text_file(log_file, text)
        call assert(exitcode == 9, 'compiler failure preserves ffc exit status')
        call assert(index(text, &
            'bad source.lf:4:7: error: unsupported widget') > 0, &
            'compiler diagnostic preserves file line and column')
        call set_env('FFC_FAKE_MODE', 'ok')
    end subroutine test_compiler_diagnostic

    subroutine test_multiple_source_build()
        character(len=512) :: sources(3), output, calls_file
        character(len=4096) :: text
        character(len=512) :: message
        integer :: exitcode
        logical :: exists

        call use_fake_ffc()
        call set_env('FFC_FAKE_VERSION_NOISE', '1')
        calls_file = trim(test_dir)//'/calls.log'
        call set_env('FFC_FAKE_LOG', calls_file)
        sources(1) = trim(test_dir)//'/module one.lf'
        sources(2) = trim(test_dir)//'/module two.lf'
        sources(3) = trim(test_dir)//'/main program.lf'
        output = trim(test_dir)//'/linked program'
        call ffc_native_build(sources, 3, output, exitcode, message)
        call read_text_file(calls_file, text)
        inquire (file=trim(output), exist=exists)
        call assert(exitcode == 0 .and. exists, &
            'multiple native sources compile and link')
        call assert(count_substring(text, 'CALL') == 3, &
            'multiple native sources use two compiles and one link')
        call assert(count_substring(text, 'ARG=-c') == 2, &
            'dependency sources compile separately')
        call assert(index(text, 'ARG='//trim(sources(1))) > 0, &
            'source path with spaces remains one argument')
        call assert(index(text, 'ARG='//trim(output)) > 0, &
            'output path with spaces remains one argument')
        call set_env('FFC_FAKE_VERSION_NOISE', '0')
    end subroutine test_multiple_source_build

    subroutine test_run_exit_status()
        character(len=512) :: args(2), message, run_log, source
        character(len=1024) :: text
        integer :: exitcode

        call use_fake_ffc()
        call set_env('FFC_FAKE_PROGRAM_EXIT', '23')
        source = trim(test_dir)//'/run source.lf'
        run_log = trim(test_dir)//'/run.log'
        call set_env('FFC_FAKE_RUN_LOG', run_log)
        args(1) = 'first argument'
        args(2) = '--second'
        call ffc_native_run(source, args, 2, exitcode, message)
        call read_text_file(run_log, text)
        call assert(exitcode == 23, 'native run preserves program exit status')
        call assert(index(text, 'RUN_ARG=first argument') > 0 .and. &
            index(text, 'RUN_ARG=--second') > 0, &
            'native run forwards program arguments without splitting')
        call set_env('FFC_FAKE_PROGRAM_EXIT', '0')
        call set_env('FFC_FAKE_VERSION_NOISE', '0')
    end subroutine test_run_exit_status

    subroutine use_fake_ffc()
        call set_env('PATH', trim(test_dir)//':'//trim(original_path))
        call set_env('FFC_FAKE_VERSION', 'ffc 0.1.0')
        call set_env('FFC_FAKE_VERSION_EXIT', '0')
        call set_env('FFC_FAKE_MODE', 'ok')
        call set_env('FFC_FAKE_PROGRAM_EXIT', '0')
        call set_env('FFC_FAKE_LOG', trim(test_dir)//'/default-calls.log')
    end subroutine use_fake_ffc

    subroutine write_fake_ffc()
        integer :: u
        character(len=512) :: path

        path = trim(test_dir)//'/ffc'
        open (newunit=u, file=trim(path), status='replace')
        write (u, '(a)') '#!/bin/sh'
        write (u, '(a)') 'if [ "$1" = "--version" ]; then'
        write (u, '(a)') '  if [ "$FFC_FAKE_VERSION_NOISE" = "1" ]; then'
        write (u, '(a)') '    printf "STOP 0\n" >&2'
        write (u, '(a)') '  fi'
        write (u, '(a)') '  printf "%s\n" "${FFC_FAKE_VERSION:-ffc 0.1.0}"'
        write (u, '(a)') '  exit "${FFC_FAKE_VERSION_EXIT:-0}"'
        write (u, '(a)') 'fi'
        write (u, '(a)') 'printf "CALL\n" >> "$FFC_FAKE_LOG"'
        write (u, '(a)') 'out='
        write (u, '(a)') 'compile=0'
        write (u, '(a)') 'previous='
        write (u, '(a)') 'for arg in "$@"; do'
        write (u, '(a)') '  printf "ARG=%s\n" "$arg" >> "$FFC_FAKE_LOG"'
        write (u, '(a)') '  if [ "$previous" = "-o" ]; then out=$arg; fi'
        write (u, '(a)') '  if [ "$arg" = "-c" ]; then compile=1; fi'
        write (u, '(a)') '  previous=$arg'
        write (u, '(a)') 'done'
        write (u, '(a)') 'if [ "${FFC_FAKE_MODE:-ok}" = "bad" ]; then'
        write (u, '(a)') '  printf "%s\n" "$FFC_FAKE_DIAGNOSTIC" >&2'
        write (u, '(a)') '  exit 9'
        write (u, '(a)') 'fi'
        write (u, '(a)') 'if [ "$compile" -eq 1 ]; then : > "$out"; exit 0; fi'
        write (u, '(a)') 'printf "#!/bin/sh\n" > "$out"'
        write (u, '(a)') 'printf ''%s\n'' ''printf "RUN_ARG=%s\n" '// &
            '"$@" > "$FFC_FAKE_RUN_LOG"'' >> "$out"'
        write (u, '(a)') 'printf "exit %s\n" '// &
            '"${FFC_FAKE_PROGRAM_EXIT:-0}" >> "$out"'
        write (u, '(a)') '/bin/chmod +x "$out"'
        close (u)
        call execute_command_line('/bin/chmod +x "'//trim(path)//'"')
    end subroutine write_fake_ffc

    subroutine set_env(name, value)
        character(len=*), intent(in) :: name, value
        integer(c_int) :: ierr

        ierr = setenv(trim(name)//c_null_char, trim(value)//c_null_char, 1_c_int)
        if (ierr /= 0) error stop 'setenv failed'
    end subroutine set_env

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//trim(message)
        end if
    end subroutine assert

    integer function count_substring(text, needle) result(count)
        character(len=*), intent(in) :: text, needle
        integer :: offset, found

        count = 0
        offset = 1
        do while (offset <= len_trim(text))
            found = index(text(offset:), needle)
            if (found == 0) exit
            count = count + 1
            offset = offset + found + len(needle) - 1
        end do
    end function count_substring

end program test_ffc_native
