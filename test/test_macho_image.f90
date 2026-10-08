program test_macho_image
    !! Portable bounds oracles for Apple's loader.h/fat.h record layouts.
    !! Positive synthetic containers test structure only; LLVM's optional
    !! cross linker supplies independent real images, without a Darwin runtime.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_macho_image, only: macho_image_valid
    use fo_fs, only: fs_find_executable
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, remove_tree, write_text
    use fo_test_harness, only: assert_true, assert_process_ok, finish_assertions
    use fo_test_cli, only: run_external
    implicit none

    character(:), allocatable :: scratch, thin, fat, damaged
    integer :: endian, width
    logical :: little, wide

    call make_scratch('fo-macho-image', scratch)
    do endian = 0, 1
        little = endian == 0
        do width = 0, 1
            wide = width == 1
            thin = thin_image(little, wide)
            call check(thin, .true., 'complete thin image structure is accepted')
            call check(thin(:27), .false., 'truncated thin header is rejected')
            call check(thin(:len(thin) - 1), .false., &
                'truncated segment payload is rejected')
            damaged = thin
            call put(damaged, 20, 4, int(len(thin), int64), little)
            call check(damaged, .false., 'command table outside image is rejected')
            damaged = thin
            call put(damaged, 16, 4, 2_int64, little)
            call check(damaged, .false., 'missing declared load command is rejected')
            damaged = thin
            if (wide) then
                call put(damaged, 36, 4, 8_int64, little)
            else
                call put(damaged, 32, 4, 8_int64, little)
            end if
            call check(damaged, .false., 'undersized segment command is rejected')
            damaged = thin
            if (wide) then
                call put(damaged, 72, 8, huge(0_int64), little)
                call put(damaged, 80, 8, 16_int64, little)
            else
                call put(damaged, 60, 4, int(z'FFFFFFFF', int64), little)
            end if
            call check(damaged, .false., 'segment extent overflow is rejected')
            damaged = thin
            if (wide) then
                call put(damaged, 96, 4, 1_int64, little)
            else
                call put(damaged, 76, 4, 1_int64, little)
            end if
            call check(damaged, .false., 'missing section records are rejected')
            fat = fat_image(thin, little, wide)
            call check(fat, .true., 'complete fat slice structure is accepted')
            call check(fat(:len(fat) - 1), .false., &
                'fat slice truncated past architecture table is rejected')
            damaged = fat
            call put(damaged, 4, 4, 2_int64, little)
            call check(damaged, .false., 'missing fat architecture record is rejected')
            damaged = fat
            call put(damaged, 8, 4, 12_int64, little)
            call check(damaged, .false., 'fat slice CPU mismatch is rejected')
            damaged = fat
            if (wide) then
                call put(damaged, 16, 8, huge(0_int64), little)
            else
                call put(damaged, 16, 4, int(z'FFFFFFFF', int64), little)
            end if
            call check(damaged, .false., 'fat slice offset overflow is rejected')
        end do
    end do
    call check_llvm_reference()
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine check(bytes, expected, message)
        character(len=*), intent(in) :: bytes, message
        logical, intent(in) :: expected
        character(:), allocatable :: path
        integer :: unit
        logical :: valid

        path = join_path(scratch, 'probe.image')
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write')
        write (unit) bytes
        close (unit)
        call macho_image_valid(path, valid)
        call assert_true(valid .eqv. expected, message)
    end subroutine check

    function thin_image(little, wide) result(bytes)
        logical, intent(in) :: little, wide
        character(:), allocatable :: bytes
        integer :: header_size, segment_size

        header_size = 28
        segment_size = 56
        if (wide) then
            header_size = 32
            segment_size = 72
        end if
        bytes = repeat(achar(0), header_size + segment_size + 8)
        call put(bytes, 0, 4, int(z'FEEDFACE', int64), little)
        call put(bytes, 4, 4, 7_int64, little)
        if (wide) then
            call put(bytes, 0, 4, int(z'FEEDFACF', int64), little)
            call put(bytes, 4, 4, int(z'01000007', int64), little)
        end if
        call put(bytes, 8, 4, 3_int64, little)
        call put(bytes, 12, 4, 6_int64, little)
        call put(bytes, 16, 4, 1_int64, little)
        call put(bytes, 20, 4, int(segment_size, int64), little)
        call put(bytes, header_size, 4, 1_int64, little)
        if (wide) call put(bytes, header_size, 4, 25_int64, little)
        call put(bytes, header_size + 4, 4, int(segment_size, int64), little)
        bytes(header_size + 9:header_size + 14) = '__TEXT'
        if (wide) then
            call put(bytes, header_size + 48, 8, int(len(bytes), int64), little)
        else
            call put(bytes, header_size + 36, 4, int(len(bytes), int64), little)
        end if
    end function thin_image

    function fat_image(thin, little, wide) result(bytes)
        character(len=*), intent(in) :: thin
        logical, intent(in) :: little, wide
        character(:), allocatable :: bytes

        bytes = repeat(achar(0), 64) // thin
        call put(bytes, 0, 4, int(z'CAFEBABE', int64), little)
        call put(bytes, 8, 4, 7_int64, little)
        if (wide) then
            call put(bytes, 0, 4, int(z'CAFEBABF', int64), little)
            call put(bytes, 8, 4, int(z'01000007', int64), little)
        end if
        call put(bytes, 4, 4, 1_int64, little)
        call put(bytes, 12, 4, 3_int64, little)
        if (wide) then
            call put(bytes, 16, 8, 64_int64, little)
            call put(bytes, 24, 8, int(len(thin), int64), little)
            call put(bytes, 32, 4, 6_int64, little)
        else
            call put(bytes, 16, 4, 64_int64, little)
            call put(bytes, 20, 4, int(len(thin), int64), little)
            call put(bytes, 24, 4, 6_int64, little)
        end if
    end function fat_image

    subroutine put(bytes, offset, width, value, little)
        character(len=*), intent(inout) :: bytes
        integer, intent(in) :: offset, width
        integer(int64), intent(in) :: value
        logical, intent(in) :: little
        integer :: i, at

        do i = 0, width - 1
            at = offset + width - i
            if (little) at = offset + i + 1
            bytes(at:at) = achar(int(iand(shiftr(value, 8 * i), 255_int64)))
        end do
    end subroutine put

    subroutine check_llvm_reference()
        character(len=512) :: clang, linker, lipo
        character(len=8) :: architectures(2) = [character(len=8) :: &
            'x86_64', 'arm64']
        character(:), allocatable :: source, object, image, target, bytes
        type(string_list_t) :: args
        type(process_result_t) :: result
        integer :: i, at
        logical :: found

        call fs_find_executable('clang', clang, found)
        if (.not. found) then
            write (*, '(a)') 'SKIP: optional LLVM Mach-O reference needs clang'
            return
        end if
        call fs_find_executable('ld64.lld', linker, found)
        if (.not. found) then
            write (*, '(a)') 'SKIP: optional LLVM Mach-O reference needs ld64.lld'
            return
        end if
        source = join_path(scratch, 'answer.c')
        call write_text(source, 'int answer(void) { return 42; }' // new_line('a'))
        do i = 1, 2
            object = join_path(scratch, trim(architectures(i)) // '.o')
            image = join_path(scratch, trim(architectures(i)) // '.dylib')
            target = trim(architectures(i)) // '-apple-macos11'
            args = words([character(len=512) :: '-target', target, '-c', source, &
                '-o', object])
            call run_external(trim(clang), args, scratch, result)
            call assert_process_ok(result, 'LLVM cross compiler produces Mach-O object')
            if (result%exit_code /= 0) return
            args = words([character(len=512) :: '-dylib', '-arch', &
                trim(architectures(i)), '-platform_version', 'macos', '11.0', &
                '11.0', '-install_name', '@rpath/libanswer.dylib', object, '-o', image])
            call run_external(trim(linker), args, scratch, result)
            call assert_process_ok(result, 'LLVM cross linker produces real dylib')
            if (result%exit_code /= 0) return
            bytes = read_image(image)
            call check(bytes, .true., 'independent real LLVM dylib is accepted')
            call check(bytes(:len(bytes) - 1), .false., &
                'truncated real LLVM dylib is rejected')
            at = index(bytes, '__text' // achar(0))
            call assert_true(at > 0, 'real LLVM dylib contains its text section')
            if (at > 0) then
                call put(bytes, at - 1 + 48, 4, int(len(bytes), int64), .true.)
                call check(bytes, .false., 'real section payload past EOF is rejected')
            end if
        end do
        call fs_find_executable('llvm-lipo', lipo, found)
        if (.not. found) return
        image = join_path(scratch, 'universal.dylib')
        do i = 1, 2
            args = words([character(len=512) :: '-create', &
                join_path(scratch, 'x86_64.dylib'), join_path(scratch, 'arm64.dylib'), &
                '-output', image])
            if (i == 2) call list_add(args, '-fat64')
            call run_external(trim(lipo), args, scratch, result)
            call assert_process_ok(result, &
                'LLVM lipo supplies independent universal image')
            if (result%exit_code /= 0) return
            bytes = read_image(image)
            call check(bytes, .true., 'independent LLVM universal image is accepted')
            call check(bytes(:len(bytes) - 1), .false., &
                'truncated real universal slice is rejected')
        end do
    end subroutine check_llvm_reference

    function read_image(path) result(bytes)
        character(len=*), intent(in) :: path
        character(:), allocatable :: bytes
        integer :: unit, count

        inquire (file=path, size=count)
        allocate (character(len=count) :: bytes)
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read')
        read (unit) bytes
        close (unit)
    end function read_image

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(items)
            call list_add(list, trim(items(i)))
        end do
    end function words

end program test_macho_image
