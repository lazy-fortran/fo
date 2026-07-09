module fo_ffc_cli
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use fo_ffc_native, only: ffc_native_build, ffc_native_run
    implicit none
    private
    public :: ffc_cmd_build, ffc_cmd_run, ffc_native_requested

contains

    logical function ffc_native_requested()
        ffc_native_requested = arg_equals(2, '--native')
    end function ffc_native_requested

    subroutine ffc_cmd_run()
        character(len=4096) :: source, arg
        character(len=4096), allocatable :: args(:)
        character(len=512) :: error_msg
        integer :: exitcode, i, n_run_args

        if (command_argument_count() < 3) then
            call print_run_usage(error_unit)
            stop 2, quiet=.true.
        end if
        if (arg_equals(3, '--help') .or. arg_equals(3, '-h')) then
            call print_run_usage(output_unit)
            return
        end if
        call get_command_argument(3, source)
        n_run_args = command_argument_count() - 3
        allocate (args(max(1, n_run_args)))
        do i = 1, n_run_args
            call get_command_argument(i + 3, arg)
            args(i) = arg
        end do
        call ffc_native_run(source, args, n_run_args, exitcode, error_msg)
        if (len_trim(error_msg) > 0) write (error_unit, '(a)') trim(error_msg)
        if (exitcode /= 0) stop exitcode, quiet=.true.
    end subroutine ffc_cmd_run

    subroutine ffc_cmd_build()
        character(len=4096), allocatable :: sources(:)
        character(len=4096) :: arg, output
        character(len=512) :: error_msg
        integer :: exitcode, i, n_sources

        if (arg_equals(3, '--help') .or. arg_equals(3, '-h')) then
            call print_build_usage(output_unit)
            return
        end if
        allocate (sources(max(1, command_argument_count() - 2)))
        output = 'a.out'
        n_sources = 0
        i = 3
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            if (trim(arg) == '-o') then
                if (i == command_argument_count()) then
                    write (error_unit, '(a)') 'fo build --native: -o requires a path'
                    stop 2, quiet=.true.
                end if
                i = i + 1
                call get_command_argument(i, output)
            else if (arg(1:1) == '-') then
                write (error_unit, '(a)') &
                    'fo build --native: unknown option: '//trim(arg)
                stop 2, quiet=.true.
            else
                n_sources = n_sources + 1
                sources(n_sources) = arg
            end if
            i = i + 1
        end do
        if (n_sources == 0) then
            call print_build_usage(error_unit)
            stop 2, quiet=.true.
        end if
        call ffc_native_build(sources, n_sources, output, exitcode, error_msg)
        if (len_trim(error_msg) > 0) write (error_unit, '(a)') trim(error_msg)
        if (exitcode /= 0) stop exitcode, quiet=.true.
    end subroutine ffc_cmd_build

    subroutine print_run_usage(unit)
        integer, intent(in) :: unit

        write (unit, '(a)') 'usage: fo run --native <source> [program-args...]'
        write (unit, '(a)') ''
        write (unit, '(a)') 'Compile one source with ffc, then run it.'
    end subroutine print_run_usage

    subroutine print_build_usage(unit)
        integer, intent(in) :: unit

        write (unit, '(a)') &
            'usage: fo build --native [-o <executable>] <source> [source ...]'
        write (unit, '(a)') ''
        write (unit, '(a)') &
            'Compile sources in dependency order; the final source is the link unit.'
        write (unit, '(a)') 'The default output is a.out.'
    end subroutine print_build_usage

    logical function arg_equals(position, expected)
        integer, intent(in) :: position
        character(len=*), intent(in) :: expected
        character(len=4096) :: arg

        arg_equals = .false.
        if (command_argument_count() < position) return
        call get_command_argument(position, arg)
        arg_equals = trim(arg) == trim(expected)
    end function arg_equals

end module fo_ffc_cli
