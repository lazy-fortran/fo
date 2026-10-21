module fo_hook_evidence
    !! Small receipts in the existing shared store; never infer tests from a build.
    use fo_gremlin_generation, only: generation_t
    use fo_project_model, only: model_case_t
    use fo_cache, only: cache_store_root
    use fo_fs, only: fs_make_dir, fs_rename, fs_write_text, fs_remove_file
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile, read_text_file_alloc
    use fx_hash, only: sha256_string
    use fx_path, only: path_dirname
    implicit none
    private
    public :: hook_input_key, hook_receipt_read, hook_receipt_write, hook_receipt_forget, hook_slow_case
contains
    function hook_input_key(generation) result(key)
        type(generation_t), intent(in) :: generation
        character(len=64) :: key

        key = ''
        if (.not. generation%input_inventory_complete) return
        if (.not. generation%input_inventory%valid) return
        if (len_trim(generation%toolchain_key) /= 64) return
        if (len_trim(generation%driver_digest) /= 64) return
        key = sha256_string('fo-hook-v1|'//generation%input_inventory%digest// &
            generation%toolchain_key//generation%driver_digest)
    end function hook_input_key

    logical function hook_slow_case(item) result(slow)
        type(model_case_t), intent(in) :: item
        integer :: i

        slow = .false.
        if (.not. allocated(item%labels)) return
        do i = 1, size(item%labels)
            if (item%labels(i)%s == 'slow') slow = .true.
        end do
    end function hook_slow_case

    function receipt_path(key) result(path)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: path
        character(len=4096) :: root

        call cache_store_root(root)
        path = trim(root)//'/'//key(1:2)//'/'//key//'-hook'
    end function receipt_path

    logical function hook_receipt_read(key) result(found)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: text
        integer :: ierr

        found = .false.
        if (len_trim(key) /= 64) return
        call read_text_file_alloc(receipt_path(key), text, ierr)
        if (ierr == 0) found = text == key//achar(10)
    end function hook_receipt_read

    subroutine hook_receipt_forget(key)
        character(len=*), intent(in) :: key
        if (len_trim(key) /= 64) return
        call fs_remove_file(receipt_path(key))
    end subroutine hook_receipt_forget

    subroutine hook_receipt_write(key)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: path
        character(len=4096) :: temporary
        integer :: ierr

        if (len_trim(key) /= 64) return
        if (hook_receipt_read(key)) return
        path = receipt_path(key)
        call fs_make_dir(path_dirname(path))
        call make_sibling_tmpfile(path, temporary)
        call fs_write_text(trim(temporary), key)
        ierr = fs_rename(trim(temporary), path)
        if (ierr /= 0) call delete_tmpfile(trim(temporary))
    end subroutine hook_receipt_write
end module fo_hook_evidence
