module fo_compdb
    use fo_process, only: argv_push, argv_push_split, argv_push_split_nl
    implicit none
    private

    public :: compdb_write

contains

    subroutine compdb_write(output_path, project_dir, sources, objects, n_sources, &
            compiler, base_flags, includes_flag, user_flags)
        character(len=*), intent(in) :: output_path, project_dir
        character(len=*), intent(in) :: sources(:), objects(:)
        integer, intent(in) :: n_sources
        character(len=*), intent(in) :: compiler, base_flags
        character(len=*), intent(in) :: includes_flag, user_flags

        integer :: u, ios, i, n_args
        character(len=:), allocatable :: packed

        open (newunit=u, file=trim(output_path), status='replace', action='write', &
            iostat=ios)
        if (ios /= 0) return

        write (u, '(a)') '['
        do i = 1, n_sources
            n_args = 0
            if (allocated(packed)) deallocate (packed)
            call argv_push_split(packed, n_args, compiler)
            call argv_push(packed, n_args, '-c')
            call argv_push_split_nl(packed, n_args, includes_flag)
            call argv_push_split(packed, n_args, base_flags)
            call argv_push_split(packed, n_args, user_flags)
            call argv_push(packed, n_args, '-o')
            call argv_push(packed, n_args, trim(objects(i)))
            call argv_push(packed, n_args, trim(sources(i)))

            write (u, '(a)', advance='no') '  {"directory": '
            call write_json_string(u, trim(project_dir))
            write (u, '(a)', advance='no') ', "file": '
            call write_json_string(u, trim(sources(i)))
            write (u, '(a)', advance='no') ', "arguments": '
            call write_json_argv(u, packed)
            if (i < n_sources) then
                write (u, '(a)') '},'
            else
                write (u, '(a)') '}'
            end if
        end do
        write (u, '(a)') ']'
        close (u)
    end subroutine compdb_write

    subroutine write_json_argv(u, packed)
        integer, intent(in) :: u
        character(len=*), intent(in) :: packed

        integer :: i, start, n_written

        write (u, '(a)', advance='no') '['
        start = 1
        n_written = 0
        do i = 1, len(packed)
            if (packed(i:i) /= achar(0)) cycle
            if (i > start) then
                if (n_written > 0) write (u, '(a)', advance='no') ', '
                call write_json_string(u, packed(start:i - 1))
                n_written = n_written + 1
            end if
            start = i + 1
        end do
        write (u, '(a)', advance='no') ']'
    end subroutine write_json_argv

    subroutine write_json_string(u, text)
        integer, intent(in) :: u
        character(len=*), intent(in) :: text

        integer :: i

        write (u, '(a)', advance='no') '"'
        do i = 1, len_trim(text)
            select case (text(i:i))
            case ('"')
                write (u, '(a)', advance='no') '\"'
            case ('\')
                write (u, '(a)', advance='no') '\\'
            case (char(9))
                write (u, '(a)', advance='no') '\t'
            case default
                if (iachar(text(i:i)) >= 32) write (u, '(a)', advance='no') text(i:i)
            end select
        end do
        write (u, '(a)', advance='no') '"'
    end subroutine write_json_string

end module fo_compdb
