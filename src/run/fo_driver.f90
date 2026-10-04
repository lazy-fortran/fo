module fo_driver
    !! Capture and retain the exact native fo image for one Gremlin session.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_hash, only: sha256_file
    use fo_fs, only: fs_stat
    implicit none
    private

    integer, parameter :: DRIVER_DIGEST_LEN = 64
    integer, parameter :: PATH_LEN = 4096

    type, public :: driver_pin_t
        character(len=PATH_LEN) :: path = ''
        character(len=DRIVER_DIGEST_LEN) :: digest = ''
        integer(int64) :: size = 0_int64
    end type driver_pin_t

    public :: driver_image_init, driver_pin_current, driver_pin_existing

    interface
        integer(c_int) function c_driver_image_init() &
                bind(C, name='fo_c_driver_image_init')
            import :: c_int
        end function c_driver_image_init

        integer(c_int) function c_driver_stage_copy(root, path, cap) &
                bind(C, name='fo_c_driver_stage_copy')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: cap
        end function c_driver_stage_copy

        integer(c_int) function c_driver_publish(root, stage, digest, path, cap) &
                bind(C, name='fo_c_driver_publish')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), stage(*), digest(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: cap
        end function c_driver_publish

        integer(c_int) function c_driver_remove_stage(root, stage) &
                bind(C, name='fo_c_driver_remove_stage')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), stage(*)
        end function c_driver_remove_stage

        integer(c_int) function c_driver_validate_root(root) &
                bind(C, name='fo_c_driver_validate_root')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
        end function c_driver_validate_root

        integer(c_int) function c_driver_validate_pin(path, size) &
                bind(C, name='fo_c_driver_validate_pin')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: size
        end function c_driver_validate_pin
    end interface

contains

    subroutine driver_image_init(ierr)
        !! Capture the native running-image handle. Called from C at process
        !! startup and again by clients that use the provider directly.
        integer, intent(out) :: ierr
        ierr = int(c_driver_image_init())
    end subroutine driver_image_init

    subroutine driver_pin_current(session_state_dir, pin, ierr, message)
        character(len=*), intent(in) :: session_state_dir
        type(driver_pin_t), intent(out) :: pin
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(kind=c_char) :: c_stage(PATH_LEN + 1), c_final(PATH_LEN + 1)
        character(len=PATH_LEN) :: root, stage_path, final_path
        character(len=DRIVER_DIGEST_LEN) :: digest
        integer(c_long_long) :: file_size, mtime_ns, pin_size
        integer(c_int) :: native_status, publish_status
        integer :: hash_status
        logical :: stat_ok

        pin = driver_pin_t()
        ierr = 1
        message = ''
        digest = ''
        file_size = 0_c_long_long
        pin_size = 0_c_long_long
        call driver_pin_root(session_state_dir, root, hash_status, message)
        if (hash_status /= 0) return

        native_status = c_driver_image_init()
        if (native_status /= 0) then
            message = 'cannot identify the native running fo image'
            return
        end if
        c_stage = c_null_char
        native_status = c_driver_stage_copy(trim(root)//c_null_char, c_stage, &
            int(size(c_stage), c_int))
        if (native_status /= 0) then
            message = 'cannot stage the native running fo image'// &
                ' in the session pin directory'
            return
        end if
        stage_path = c_text(c_stage)

        call sha256_file(trim(stage_path), digest, hash_status)
        if (hash_status /= 0) then
            native_status = c_driver_remove_stage(trim(root)//c_null_char, &
                trim(stage_path)//c_null_char)
            message = 'cannot hash the staged native fo image'
            return
        end if
        if (.not. valid_digest(digest)) then
            native_status = c_driver_remove_stage(trim(root)//c_null_char, &
                trim(stage_path)//c_null_char)
            message = 'staged native fo image returned an invalid SHA-256 digest'
            return
        end if
        call fs_stat(trim(stage_path), mtime_ns, file_size, stat_ok)
        if (.not. stat_ok .or. file_size <= 0_c_long_long) then
            native_status = c_driver_remove_stage(trim(root)//c_null_char, &
                trim(stage_path)//c_null_char)
            message = 'cannot stat the staged native fo image'
            return
        end if

        c_final = c_null_char
        publish_status = c_driver_publish(trim(root)//c_null_char, &
            trim(stage_path)//c_null_char, trim(digest)//c_null_char, c_final, &
            int(size(c_final), c_int))
        if (publish_status < 0) then
            native_status = c_driver_remove_stage(trim(root)//c_null_char, &
                trim(stage_path)//c_null_char)
            message = 'cannot publish the immutable session fo driver pin'
            return
        end if
        final_path = c_text(c_final)
        if (publish_status == 1) then
            native_status = c_driver_remove_stage(trim(root)//c_null_char, &
                trim(stage_path)//c_null_char)
            if (native_status /= 0) then
                message = 'cannot remove the duplicate staged fo driver pin'
                return
            end if
        end if
        if (len_trim(final_path) == 0) then
            message = 'native fo driver pin publication returned no path'
            return
        end if

        native_status = c_driver_validate_pin(trim(final_path)//c_null_char, pin_size)
        if (native_status /= 0) then
            message = 'published fo driver pin is not an owned read-only executable'
            return
        end if
        if (pin_size /= file_size) then
            message = 'published fo driver pin is not an owned read-only executable'
            return
        end if
        pin%digest = ''
        call sha256_file(trim(final_path), pin%digest, hash_status)
        if (hash_status /= 0) then
            message = 'published fo driver pin cannot be hashed'
            return
        end if
        if (pin%digest /= digest) then
            message = 'published fo driver pin failed SHA-256 validation'
            pin = driver_pin_t()
            return
        end if
        pin%path = trim(final_path)
        pin%size = int(pin_size, int64)
        ierr = 0
    end subroutine driver_pin_current

    subroutine driver_pin_existing(session_state_dir, digest, expected_size, &
            pin, ierr, message)
        character(len=*), intent(in) :: session_state_dir, digest
        integer(int64), intent(in) :: expected_size
        type(driver_pin_t), intent(out) :: pin
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: root, pin_path
        character(len=DRIVER_DIGEST_LEN) :: actual_digest
        integer(c_long_long) :: actual_size
        integer :: root_status, hash_status
        integer(c_int) :: native_status

        pin = driver_pin_t()
        ierr = 1
        message = ''
        actual_size = 0_c_long_long
        actual_digest = ''
        call driver_pin_root(session_state_dir, root, root_status, message)
        if (root_status /= 0) return
        if (.not. valid_digest(digest) .or. expected_size <= 0_int64) then
            message = 'stored fo driver identity is incomplete'
            return
        end if
        native_status = c_driver_validate_root(trim(root)//c_null_char)
        if (native_status /= 0) then
            message = 'stored fo driver pin directory is missing or not owned'
            return
        end if
        pin_path = trim(root)//'/'//trim(digest)
        native_status = c_driver_validate_pin(trim(pin_path)//c_null_char, actual_size)
        if (native_status /= 0) then
            message = 'stored fo driver pin is missing or has changed permissions'
            return
        end if
        if (int(actual_size, int64) /= expected_size) then
            message = 'stored fo driver pin is missing or has changed permissions'// &
                ' or size'
            return
        end if
        call sha256_file(trim(pin_path), actual_digest, hash_status)
        if (hash_status /= 0) then
            message = 'stored fo driver pin cannot be hashed'
            return
        end if
        if (actual_digest /= digest) then
            message = 'stored fo driver pin failed SHA-256 validation'
            return
        end if
        pin%path = trim(pin_path)
        pin%digest = actual_digest
        pin%size = int(actual_size, int64)
        ierr = 0
    end subroutine driver_pin_existing

    subroutine driver_pin_root(session_state_dir, root, ierr, message)
        character(len=*), intent(in) :: session_state_dir
        character(len=*), intent(out) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        root = ''
        ierr = 1
        message = ''
        if (len_trim(session_state_dir) == 0 .or. &
            index(session_state_dir, achar(0)) /= 0 .or. &
            len_trim(session_state_dir) + len('/driver-pins') >= len(root)) then
            message = 'Gremlin session path cannot hold its fo driver pin directory'
            return
        end if
        root = trim(session_state_dir)//'/driver-pins'
        ierr = 0
    end subroutine driver_pin_root

    function c_text(buffer) result(text)
        character(kind=c_char), intent(in) :: buffer(:)
        character(len=PATH_LEN) :: text
        integer :: i

        text = ''
        do i = 1, min(size(buffer), len(text))
            if (buffer(i) == c_null_char) exit
            text(i:i) = buffer(i)
        end do
    end function c_text

    logical function valid_digest(digest)
        character(len=*), intent(in) :: digest
        integer :: i, value

        valid_digest = .false.
        if (len_trim(digest) /= DRIVER_DIGEST_LEN) return
        do i = 1, DRIVER_DIGEST_LEN
            value = iachar(digest(i:i))
            if (.not. ((value >= iachar('0') .and. value <= iachar('9')) .or. &
                (value >= iachar('a') .and. value <= iachar('f')))) return
        end do
        valid_digest = .true.
    end function valid_digest

end module fo_driver
