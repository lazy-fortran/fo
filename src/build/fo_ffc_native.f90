module fo_ffc_native
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree
    use fo_process, only: argv_push, process_run_argv_logged
    use fo_util, only: delete_tmpfile, make_tmpfile, read_text_file
    implicit none
    private
    public :: ffc_native_build, ffc_native_run

contains

    subroutine ffc_native_build(sources, n_sources, output, exitcode, error_msg, &
            log_file)
        character(len=*), intent(in) :: sources(:), output
        integer, intent(in) :: n_sources
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: error_msg
        character(len=*), intent(in), optional :: log_file
        character(len=512) :: work_dir, log_path
        character(len=512), allocatable :: objects(:)
        integer :: i
        logical :: append

        error_msg = ''
        exitcode = 1
        if (n_sources < 1 .or. n_sources > size(sources)) then
            error_msg = 'fo: native build requires at least one source'
            return
        end if
        if (len_trim(output) == 0) then
            error_msg = 'fo: native build requires a nonempty output path'
            return
        end if
        call discover_ffc(exitcode, error_msg)
        if (exitcode /= 0) return

        log_path = ''
        if (present(log_file)) log_path = log_file
        call make_tmpfile('fo-ffc-native', work_dir)
        call fs_make_dir(trim(work_dir))
        allocate (objects(max(1, n_sources - 1)))
        append = .false.
        do i = 1, n_sources - 1
            write (objects(i), '(a,a,i0,a)') trim(work_dir), '/input-', i, '.o'
            call compile_ffc_object(sources(i), objects(i), work_dir, log_path, &
                append, exitcode)
            if (exitcode /= 0) then
                call fs_remove_tree(trim(work_dir))
                return
            end if
            if (len_trim(log_path) > 0) append = .true.
        end do
        call link_ffc_program(sources(n_sources), objects, n_sources - 1, &
            work_dir, output, log_path, append, exitcode)
        call fs_remove_tree(trim(work_dir))
    end subroutine ffc_native_build

    subroutine ffc_native_run(source, args, n_args, exitcode, error_msg, log_file)
        character(len=*), intent(in) :: source, args(:)
        integer, intent(in) :: n_args
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: error_msg
        character(len=*), intent(in), optional :: log_file
        character(len=512) :: executable, log_path, sources(1)
        character(len=:), allocatable :: packed
        integer :: i, packed_count

        call make_tmpfile('fo-ffc-run', executable)
        sources(1) = source
        log_path = ''
        if (present(log_file)) log_path = log_file
        call ffc_native_build(sources, 1, executable, exitcode, error_msg, log_path)
        if (exitcode /= 0) then
            call fs_remove_file(trim(executable))
            return
        end if

        packed_count = 0
        call argv_push(packed, packed_count, trim(executable))
        do i = 1, min(n_args, size(args))
            call argv_push(packed, packed_count, trim(args(i)))
        end do
        call process_run_argv_logged('', packed, packed_count, log_path, &
            len_trim(log_path) > 0, 0, exitcode)
        call fs_remove_file(trim(executable))
    end subroutine ffc_native_run

    subroutine discover_ffc(exitcode, error_msg)
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: error_msg
        character(len=1024) :: output
        character(len=512) :: version_log
        character(len=:), allocatable :: packed
        integer :: n_args, major, minor, patch

        error_msg = ''
        call make_tmpfile('fo-ffc-version', version_log)
        n_args = 0
        call argv_push(packed, n_args, 'ffc')
        call argv_push(packed, n_args, '--version')
        call process_run_argv_logged('', packed, n_args, version_log, .false., &
            30, exitcode)
        if (exitcode /= 0) then
            write (error_msg, '(a,i0,a)') &
                'fo: native mode requires ffc; ffc --version failed (exit ', &
                exitcode, ')'
            call delete_tmpfile(version_log)
            return
        end if
        call read_text_file(version_log, output)
        call delete_tmpfile(version_log)
        call parse_ffc_version(output, major, minor, patch, exitcode)
        if (exitcode /= 0) then
            error_msg = 'fo: incompatible ffc; expected "ffc <semver>" from '// &
                'ffc --version'
            return
        end if
        if (version_before(major, minor, patch, 0, 1, 0)) then
            error_msg = 'fo: incompatible ffc; native mode requires ffc 0.1.0 or newer'
            exitcode = 1
        end if
    end subroutine discover_ffc

    subroutine parse_ffc_version(text, major, minor, patch, exitcode)
        character(len=*), intent(in) :: text
        integer, intent(out) :: major, minor, patch, exitcode
        character(len=128) :: line, version
        integer :: first_dot, second_dot, iostat

        major = 0
        minor = 0
        patch = 0
        exitcode = 1
        call find_ffc_version_line(text, line)
        if (len_trim(line) < 7) return
        if (line(1:4) /= 'ffc ') return
        version = adjustl(line(5:))
        first_dot = index(trim(version), '.')
        if (first_dot <= 1) return
        second_dot = index(version(first_dot + 1:), '.')
        if (second_dot <= 1) return
        second_dot = second_dot + first_dot
        read (version(1:first_dot - 1), *, iostat=iostat) major
        if (iostat /= 0) return
        read (version(first_dot + 1:second_dot - 1), *, iostat=iostat) minor
        if (iostat /= 0) return
        read (version(second_dot + 1:len_trim(version)), *, iostat=iostat) patch
        if (iostat /= 0) return
        if (major < 0 .or. minor < 0 .or. patch < 0) return
        exitcode = 0
    end subroutine parse_ffc_version

    subroutine find_ffc_version_line(text, line)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: line
        character(len=len(line)) :: candidate
        integer :: first, last, newline

        line = ''
        first = 1
        do while (first <= len_trim(text))
            newline = index(text(first:), new_line('a'))
            if (newline == 0) then
                last = len_trim(text)
            else
                last = first + newline - 2
            end if
            candidate = ''
            if (last >= first) candidate = adjustl(text(first:last))
            if (len_trim(candidate) >= 4) then
                if (candidate(1:4) == 'ffc ') then
                    line = candidate
                    return
                end if
            end if
            if (newline == 0) exit
            first = last + 2
        end do
    end subroutine find_ffc_version_line

    logical function version_before(major, minor, patch, want_major, want_minor, &
            want_patch)
        integer, intent(in) :: major, minor, patch
        integer, intent(in) :: want_major, want_minor, want_patch

        version_before = major < want_major
        if (major /= want_major) return
        version_before = minor < want_minor
        if (minor /= want_minor) return
        version_before = patch < want_patch
    end function version_before

    subroutine compile_ffc_object(source, object, include_dir, log_file, append, &
            exitcode)
        character(len=*), intent(in) :: source, object, include_dir, log_file
        logical, intent(in) :: append
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        integer :: n_args

        n_args = 0
        call argv_push(packed, n_args, 'ffc')
        call argv_push(packed, n_args, trim(source))
        call argv_push(packed, n_args, '-c')
        call argv_push(packed, n_args, '-I')
        call argv_push(packed, n_args, trim(include_dir))
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, trim(object))
        call process_run_argv_logged('', packed, n_args, log_file, append, 0, &
            exitcode)
    end subroutine compile_ffc_object

    subroutine link_ffc_program(source, objects, n_objects, include_dir, output, &
            log_file, append, exitcode)
        character(len=*), intent(in) :: source, objects(:), include_dir, output
        integer, intent(in) :: n_objects
        character(len=*), intent(in) :: log_file
        logical, intent(in) :: append
        integer, intent(out) :: exitcode
        character(len=:), allocatable :: packed
        integer :: i, n_args

        n_args = 0
        call argv_push(packed, n_args, 'ffc')
        call argv_push(packed, n_args, trim(source))
        call argv_push(packed, n_args, '-I')
        call argv_push(packed, n_args, trim(include_dir))
        do i = 1, n_objects
            call argv_push(packed, n_args, trim(objects(i)))
        end do
        call argv_push(packed, n_args, '-o')
        call argv_push(packed, n_args, trim(output))
        call process_run_argv_logged('', packed, n_args, log_file, append, 0, &
            exitcode)
    end subroutine link_ffc_program

end module fo_ffc_native
