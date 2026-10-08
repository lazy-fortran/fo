module fo_macho_image
    !! Bounds for linked Mach-O images, independent of the host OS. Layouts:
    !! Apple xnu EXTERNAL_HEADERS/mach-o/{loader,fat}.h. This checks complete
    !! containers; the linker and platform loader remain the ABI authorities.
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private
    public :: macho_image_valid

contains

    subroutine macho_image_valid(path, valid)
        character(len=*), intent(in) :: path
        logical, intent(out) :: valid
        character(len=32) :: header, arch
        integer(int64) :: bytes, count, stride, table_end, first, length, align
        integer(int64) :: at, i, cpu, subtype
        integer :: unit, ios
        logical :: little, wide, fat

        valid = .false.
        inquire (file=path, size=bytes, iostat=ios)
        if (ios /= 0) return
        if (bytes < 4) return
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, pos=1, iostat=ios) header(:4)
        if (ios == 0) then
            call image_kind(header(:4), little, wide, fat)
            if (.not. fat) then
                valid = thin_valid(unit, 0_int64, bytes)
            else if (bytes >= 8) then
                read (unit, pos=1, iostat=ios) header(:8)
                if (ios == 0) then
                    count = field(header, 4, 4, little)
                    stride = 20
                    if (wide) stride = 32
                    if (count > 0 .and. count <= (bytes - 8) / stride) then
                        table_end = 8 + count * stride
                        valid = .true.
                        do i = 0, count - 1
                            at = 8 + i * stride
                            read (unit, pos=at + 1, iostat=ios) arch(:stride)
                            if (ios /= 0) then
                                valid = .false.
                                exit
                            end if
                            cpu = field(arch, 0, 4, little)
                            subtype = field(arch, 4, 4, little)
                            first = field(arch, 8, 4, little)
                            length = field(arch, 12, 4, little)
                            align = field(arch, 16, 4, little)
                            if (wide) then
                                first = field(arch, 8, 8, little)
                                length = field(arch, 16, 8, little)
                                align = field(arch, 24, 4, little)
                            end if
                            valid = span_fits(first, length, bytes)
                            if (first < table_end .or. length < 28) valid = .false.
                            if (align < 0 .or. align > 62) valid = .false.
                            if (.not. valid) exit
                            if (mod(first, 2_int64**align) /= 0) then
                                valid = .false.
                                exit
                            end if
                            valid = thin_valid(unit, first, length, cpu, subtype)
                            if (.not. valid) exit
                        end do
                    end if
                end if
            end if
        end if
        close (unit)
    end subroutine macho_image_valid

    logical function thin_valid(unit, first, bytes, cpu, subtype) result(valid)
        integer, intent(in) :: unit
        integer(int64), intent(in) :: first, bytes
        integer(int64), intent(in), optional :: cpu, subtype
        character(len=32) :: header
        character(len=80) :: command, section
        integer(int64) :: header_size, count, command_bytes, limit, at, i
        integer(int64) :: kind, command_size, fileoff, filesize, nsects, j
        integer(int64) :: section_start, section_size, section_stride, flags
        integer(int64) :: section_table
        integer :: ios, alignment
        logical :: little, wide, fat, has_header_segment

        valid = .false.
        if (bytes < 28) return
        read (unit, pos=first + 1, iostat=ios) header(:28)
        if (ios /= 0) return
        call image_kind(header(:4), little, wide, fat)
        if (fat) return
        select case (field(header, 0, 4, little))
        case (int(z'FEEDFACE', int64), int(z'FEEDFACF', int64))
        case default
            return
        end select
        header_size = 28
        alignment = 4
        if (wide) then
            header_size = 32
            alignment = 8
        end if
        if (bytes < header_size) return
        if (present(cpu)) then
            if (field(header, 4, 4, little) /= cpu) return
        end if
        if (present(subtype)) then
            if (field(header, 8, 4, little) /= subtype) return
        end if
        kind = field(header, 12, 4, little)
        if (kind /= 2 .and. kind /= 6 .and. kind /= 8) return
        count = field(header, 16, 4, little)
        command_bytes = field(header, 20, 4, little)
        if (.not. span_fits(header_size, command_bytes, bytes)) return
        if (count < 1 .or. count > command_bytes / 8) return
        limit = header_size + command_bytes
        at = header_size
        has_header_segment = .false.
        do i = 1, count
            if (.not. span_fits(at, 8_int64, limit)) return
            read (unit, pos=first + at + 1, iostat=ios) command(:8)
            if (ios /= 0) return
            kind = field(command, 0, 4, little)
            command_size = field(command, 4, 4, little)
            if (command_size < 8) return
            if (mod(command_size, int(alignment, int64)) /= 0) return
            if (.not. span_fits(at, command_size, limit)) return
            if (kind == 1 .or. kind == 25) then
                section_table = 56
                section_stride = 68
                if (kind == 25) then
                    section_table = 72
                    section_stride = 80
                end if
                if (command_size < section_table) return
                read (unit, pos=first + at + 1, iostat=ios) &
                    command(:section_table)
                if (ios /= 0) return
                fileoff = field(command, 32, 4, little)
                filesize = field(command, 36, 4, little)
                nsects = field(command, 48, 4, little)
                if (kind == 25) then
                    fileoff = field(command, 40, 8, little)
                    filesize = field(command, 48, 8, little)
                    nsects = field(command, 64, 4, little)
                end if
                if (.not. span_fits(fileoff, filesize, bytes)) return
                if (fileoff == 0 .and. filesize >= limit) has_header_segment = .true.
                if (nsects > (command_size - section_table) / section_stride) return
                do j = 0, nsects - 1
                    read (unit, pos=first + at + section_table + &
                        j * section_stride + 1, iostat=ios) section(:section_stride)
                    if (ios /= 0) return
                    section_size = field(section, 36, 4, little)
                    section_start = field(section, 40, 4, little)
                    flags = field(section, 56, 4, little)
                    if (kind == 25) then
                        section_size = field(section, 40, 8, little)
                        section_start = field(section, 48, 4, little)
                        flags = field(section, 64, 4, little)
                    end if
                    if (section_size < 0) return
                    select case (iand(flags, 255_int64))
                    case (1, 12, 18) ! Zero-fill sections occupy no file bytes.
                    case default
                        if (section_start < fileoff) return
                        if (.not. span_fits(section_start, section_size, &
                                fileoff + filesize)) return
                    end select
                end do
            end if
            at = at + command_size
        end do
        valid = at == limit .and. has_header_segment
    end function thin_valid

    logical function span_fits(first, length, bytes) result(valid)
        integer(int64), intent(in) :: first, length, bytes

        valid = .false.
        if (first < 0 .or. length < 0) return
        if (first > bytes) return
        valid = length <= bytes - first
    end function span_fits

    integer(int64) function field(bytes, offset, width, little) result(value)
        character(len=*), intent(in) :: bytes
        integer, intent(in) :: offset, width
        logical, intent(in) :: little
        integer :: i, at
        integer(int64) :: byte

        value = 0
        do i = 1, width
            at = offset + i
            if (little) at = offset + width - i + 1
            byte = int(iachar(bytes(at:at)), int64)
            if (value > (huge(value) - byte) / 256_int64) then
                value = -1
                return
            end if
            value = value * 256_int64 + byte
        end do
    end function field

    subroutine image_kind(magic, little, wide, fat)
        character(len=4), intent(in) :: magic
        logical, intent(out) :: little, wide, fat

        little = .true.
        wide = .false.
        fat = .false.
        select case (magic)
        case (achar(254)//achar(237)//achar(250)//achar(206))
            little = .false.
        case (achar(207)//achar(250)//achar(237)//achar(254))
            wide = .true.
        case (achar(254)//achar(237)//achar(250)//achar(207))
            wide = .true.
            little = .false.
        case (achar(202)//achar(254)//achar(186)//achar(190))
            fat = .true.
            little = .false.
        case (achar(190)//achar(186)//achar(254)//achar(202))
            fat = .true.
        case (achar(202)//achar(254)//achar(186)//achar(191))
            fat = .true.
            wide = .true.
            little = .false.
        case (achar(191)//achar(186)//achar(254)//achar(202))
            fat = .true.
            wide = .true.
        end select
    end subroutine image_kind

end module fo_macho_image
