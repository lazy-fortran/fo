module fo_link_graph
    !! The native build's link graph: which output links which library input.
    !!
    !! An archive is consumed only by the linker, so nothing on disk says it is
    !! still needed. The builder records `output<TAB>input` for every executable
    !! it links; `fo clean --stale` treats an input as live while any recorded
    !! output still exists. One small file under build/fo, rewritten only when a
    !! link changed, always under the project build lock the builder holds.
    use fo_fs, only: fs_rename, fs_remove_file
    use fo_process, only: process_getpid
    implicit none
    private

    public :: link_graph_record, link_graph_load, link_graph_path

    character(len=*), parameter :: TAB = achar(9)
    character(len=*), parameter :: LF = achar(10)

contains

    function link_graph_path(project_dir) result(path)
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable :: path

        path = trim(project_dir)//'/build/fo/link-graph'
    end function link_graph_path

    subroutine link_graph_load(project_dir, outputs, inputs, n)
        !! Recorded (output, input) pairs; n is 0 when nothing was recorded.
        character(len=*), intent(in) :: project_dir
        character(len=512), allocatable, intent(out) :: outputs(:), inputs(:)
        integer, intent(out) :: n

        character(len=:), allocatable :: text
        integer :: start, finish, cut

        n = 0
        allocate (outputs(0), inputs(0))
        call read_all(link_graph_path(project_dir), text)
        if (.not. allocated(text)) return
        deallocate (outputs, inputs)
        allocate (outputs(count_lines(text)), inputs(count_lines(text)))
        start = 1
        do while (start <= len(text))
            finish = index(text(start:), LF)
            if (finish == 0) then
                finish = len(text)
            else
                finish = start + finish - 2
            end if
            cut = index(text(start:finish), TAB)
            if (cut > 1 .and. start + cut <= finish) then
                n = n + 1
                outputs(n) = text(start:start + cut - 2)
                inputs(n) = text(start + cut:finish)
            end if
            start = finish + 2
        end do
    end subroutine link_graph_load

    subroutine link_graph_record(project_dir, outputs, n_outputs, inputs, &
            n_inputs)
        !! Replace the recorded inputs of these outputs. Only library inputs
        !! (archives and shared libraries) are kept; objects follow their sources.
        character(len=*), intent(in) :: project_dir
        character(len=512), intent(in) :: outputs(:), inputs(:)
        integer, intent(in) :: n_outputs, n_inputs

        character(len=512), allocatable :: old_out(:), old_in(:)
        character(len=:), allocatable :: text, previous
        character(len=1024) :: temporary
        integer :: n_old, i, j, unit, ios
        logical :: replaced

        if (n_outputs < 1 .or. n_inputs < 1) return
        call link_graph_load(project_dir, old_out, old_in, n_old)
        text = ''
        do i = 1, n_old
            replaced = .false.
            do j = 1, n_outputs
                if (trim(old_out(i)) == trim(outputs(j))) replaced = .true.
            end do
            if (.not. replaced) &
                call append_pair(text, old_out(i), old_in(i))
        end do
        do j = 1, n_outputs
            do i = 1, n_inputs
                if (.not. is_library(inputs(i))) cycle
                call append_pair(text, outputs(j), inputs(i))
            end do
        end do
        call read_all(link_graph_path(project_dir), previous)
        if (allocated(previous)) then
            if (previous == text) return
        end if
        write (temporary, '(a,".",i0)') link_graph_path(project_dir), &
            process_getpid()
        open (newunit=unit, file=trim(temporary), status='replace', action='write', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        write (unit, iostat=ios) text
        close (unit)
        if (ios /= 0) then
            call fs_remove_file(trim(temporary))
            return
        end if
        if (fs_rename(trim(temporary), link_graph_path(project_dir)) /= 0) &
            call fs_remove_file(trim(temporary))
    end subroutine link_graph_record

    subroutine append_pair(text, output, input)
        !! Keep the growing record on the heap. Flang otherwise retains a
        !! concatenation temporary on the stack for each enclosing iteration.
        character(len=:), allocatable, intent(inout) :: text
        character(len=*), intent(in) :: output, input
        character(len=:), allocatable :: grown
        integer :: n, output_length, input_length

        n = len(text)
        output_length = len_trim(output)
        input_length = len_trim(input)
        allocate (character(len=n + output_length + input_length + 2) :: grown)
        grown(:n) = text
        grown(n + 1:n + output_length) = trim(output)
        n = n + output_length
        grown(n + 1:n + 1) = TAB
        grown(n + 2:n + input_length + 1) = trim(input)
        grown(n + input_length + 2:) = LF
        call move_alloc(grown, text)
    end subroutine append_pair

    pure logical function is_library(path) result(yes)
        character(len=*), intent(in) :: path
        integer :: n

        n = len_trim(path)
        yes = .false.
        if (n > 2) yes = path(n - 1:n) == '.a'
        if (n > 3 .and. .not. yes) yes = path(n - 2:n) == '.so'
        if (n > 6 .and. .not. yes) yes = path(n - 5:n) == '.dylib'
    end function is_library

    pure integer function count_lines(text) result(n)
        character(len=*), intent(in) :: text
        integer :: i

        n = 1
        do i = 1, len(text)
            if (text(i:i) == LF) n = n + 1
        end do
    end function count_lines

    subroutine read_all(path, text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer :: unit, ios
        integer(8) :: size

        inquire (file=path, size=size)
        if (size < 0) return
        open (newunit=unit, file=path, status='old', action='read', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        allocate (character(len=size) :: text)
        if (size > 0) read (unit, iostat=ios) text
        close (unit)
        if (ios /= 0) deallocate (text)
    end subroutine read_all

end module fo_link_graph
