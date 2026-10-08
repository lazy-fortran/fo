module fo_pe_image
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: pe_image_valid
contains
    subroutine pe_image_valid(path, valid)
        character(len=*), intent(in) :: path
        logical, intent(out) :: valid
        character(len=64) :: dos
        character(len=24) :: coff
        character(len=40) :: section
        character(len=:), allocatable :: optional
        integer :: u, ios, count, optional_bytes, base_bytes, i
        integer(int64) :: file_size, pe_offset, section_offset, header_size
        integer(int64) :: image_size, raw_size, raw_offset, virtual_size, rva
        integer(int64) :: directories, certificate_offset, certificate_size
        integer(int64) :: file_alignment, section_alignment, magic, machine

        valid = .false.
        inquire (file=trim(path), size=file_size, iostat=ios)
        if (ios /= 0 .or. file_size < 64_int64) return
        open (newunit=u, file=trim(path), access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        image: block
            read (u, pos=1, iostat=ios) dos
            if (ios /= 0) exit image
            if (dos(:2) /= 'MZ') exit image
            pe_offset = little_uint(dos(61:64))
            if (pe_offset < 64 .or. pe_offset > file_size - 24) exit image
            read (u, pos=pe_offset + 1, iostat=ios) coff
            if (ios /= 0) exit image
            if (coff(:4) /= 'PE'//achar(0)//achar(0)) exit image
            machine = little_uint(coff(5:6))
            if (machine /= int(z'014c', int64) .and. &
                machine /= int(z'8664', int64) .and. &
                machine /= int(z'aa64', int64)) exit image
            count = int(little_uint(coff(7:8)))
            if (count < 1 .or. count > 96) exit image
            if (.not. btest(little_uint(coff(23:24)), 1)) exit image
            optional_bytes = int(little_uint(coff(21:22)))
            if (optional_bytes < 96) exit image
            section_offset = pe_offset + 24_int64 + optional_bytes
            if (section_offset + 40_int64*count > file_size) exit image
            allocate (character(len=optional_bytes) :: optional)
            read (u, pos=pe_offset + 25, iostat=ios) optional
            if (ios /= 0) exit image
            magic = little_uint(optional(:2))
            select case (magic)
            case (int(z'010b', int64))
                base_bytes = 96
            case (int(z'020b', int64))
                base_bytes = 112
            case default
                exit image
            end select
            if (optional_bytes < base_bytes) exit image
            directories = little_uint(optional(base_bytes - 3:base_bytes))
            if (directories > (optional_bytes - base_bytes)/8) exit image
            image_size = little_uint(optional(57:60))
            header_size = little_uint(optional(61:64))
            if (image_size == 0 .or. header_size > file_size) exit image
            if (header_size < section_offset + 40_int64*count) exit image
            section_alignment = little_uint(optional(33:36))
            file_alignment = little_uint(optional(37:40))
            if (file_alignment == 0 .or. section_alignment < file_alignment) exit image
            if (iand(file_alignment, file_alignment - 1) /= 0) exit image
            if (section_alignment < 4096) then
                if (file_alignment /= section_alignment) exit image
            else
                if (file_alignment < 512 .or. file_alignment > 65536) exit image
            end if
            if (directories > 4) then
                i = base_bytes + 33
                certificate_offset = little_uint(optional(i:i + 3))
                certificate_size = little_uint(optional(i + 4:i + 7))
                if (certificate_offset + certificate_size > file_size) exit image
            end if
            do i = 1, count
                read (u, pos=section_offset + 40_int64*(i - 1) + 1, &
                    iostat=ios) section
                if (ios /= 0) exit image
                virtual_size = little_uint(section(9:12))
                rva = little_uint(section(13:16))
                raw_size = little_uint(section(17:20))
                raw_offset = little_uint(section(21:24))
                if (rva + max(virtual_size, raw_size) > image_size) exit image
                if (mod(rva, section_alignment) /= 0) exit image
                if (raw_size == 0) cycle
                if (raw_offset < header_size) exit image
                if (raw_offset + raw_size > file_size) exit image
                if (mod(raw_offset, file_alignment) /= 0) exit image
                if (mod(raw_size, file_alignment) /= 0) exit image
            end do
            valid = .true.
        end block image
        close (u)
    end subroutine pe_image_valid

    pure integer(int64) function little_uint(bytes) result(value)
        character(len=*), intent(in) :: bytes
        integer :: i
        value = 0_int64
        do i = 1, len(bytes)
            value = ior(value, shiftl(int(iachar(bytes(i:i)), int64), 8*(i - 1)))
        end do
    end function little_uint
end module fo_pe_image
