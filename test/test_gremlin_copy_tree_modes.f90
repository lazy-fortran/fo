program test_gremlin_copy_tree_modes
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long, c_null_char
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_write_text
    use fo_process, only: process_getpid
    use fo_util, only: read_text_file
    implicit none

    interface
        integer(c_int) function copy_tree(root, destination, manifest) &
                bind(C, name='fo_c_generation_copy_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), destination(*), manifest(*)
        end function copy_tree
        integer(c_int) function copy_tree_ephemeral(root, destination, manifest) &
                bind(C, name='fo_c_generation_copy_tree_ephemeral')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), destination(*), manifest(*)
        end function copy_tree_ephemeral
        integer(c_long) function copy_sync_count(reset) &
                bind(C, name='fo_c_generation_copy_sync_count')
            import :: c_int, c_long
            integer(c_int), value :: reset
        end function copy_sync_count
        integer(c_int) function c_chmod(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
        integer(c_int) function c_symlink(target, path) bind(C, name='symlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: target(*), path(*)
        end function c_symlink
        integer(c_int) function c_access(path, mode) bind(C, name='access')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_access
    end interface

    character(len=512) :: root, durable, ephemeral, text, pid_text
    character(len=2048) :: durable_manifest, ephemeral_manifest
    integer(c_int) :: rc
    integer :: pid

    pid = process_getpid()
    write(pid_text, '(i0)') pid
    root = '/var/tmp/fo-copy-modes-'//trim(pid_text)
    call fs_remove_tree(trim(root))
    call fs_make_dir(trim(root))
    call fs_make_dir(trim(root)//'/source')
    call fs_make_dir(trim(root)//'/source/nested')
    call fs_write_text(trim(root)//'/source/executable', 'payload')
    call fs_write_text(trim(root)//'/source/nested/data', 'nested bytes')
    rc = c_chmod(trim(root)//'/source/executable'//c_null_char, 493_c_int)
    call check(rc == 0, 'prepare executable source mode')
    rc = c_symlink('executable'//c_null_char, &
        (trim(root)//'/source/allowed-link')//c_null_char)
    call check(rc == 0, 'prepare allowed relative symlink')

    durable = trim(root)//'/durable'
    rc = int(copy_sync_count(1_c_int), c_int)
    rc = copy_tree((trim(root)//'/source')//c_null_char, trim(durable)//c_null_char, &
        (trim(root)//'/durable.manifest')//c_null_char)
    call check(rc == 0, 'durable tree copy succeeds')
    call check(copy_sync_count(0_c_int) == 2_c_long, &
        'durable tree copy synchronizes every regular file')
    rc = int(copy_sync_count(1_c_int), c_int)
    call read_text_file(trim(durable)//'/nested/data', text)
    call check(index(text, 'nested bytes') > 0, 'durable copy preserves nested bytes')
    call check(c_access((trim(durable)//'/executable')//c_null_char, 1_c_int) == 0, &
        'durable copy preserves executable mode')
    call read_text_file(trim(durable)//'/allowed-link', text)
    call check(index(text, 'payload') > 0, 'durable copy preserves allowed symlink')

    ephemeral = trim(root)//'/ephemeral'
    rc = copy_tree_ephemeral((trim(root)//'/source')//c_null_char, &
        trim(ephemeral)//c_null_char, (trim(root)//'/ephemeral.manifest')//c_null_char)
    call check(rc == 0, 'ephemeral tree copy succeeds')
    call check(copy_sync_count(1_c_int) == 0_c_long, &
        'ephemeral tree copy performs zero per-file syncs')
    call read_text_file(trim(ephemeral)//'/nested/data', text)
    call check(index(text, 'nested bytes') > 0, 'ephemeral copy preserves nested bytes')
    call check(c_access((trim(ephemeral)//'/executable')//c_null_char, 1_c_int) == 0, &
        'ephemeral copy preserves executable mode')
    call read_text_file(trim(ephemeral)//'/allowed-link', text)
    call check(index(text, 'payload') > 0, 'ephemeral copy preserves allowed symlink')
    call read_text_file(trim(root)//'/durable.manifest', durable_manifest)
    call read_text_file(trim(root)//'/ephemeral.manifest', ephemeral_manifest)
    call check(durable_manifest == ephemeral_manifest, &
        'durable and ephemeral copies report the same tree manifest')
    call fs_remove_tree(trim(root))

contains

    subroutine check(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        if (.not. condition) then
            write(*, '(a)') 'FAIL: '//description
            error stop 1
        end if
    end subroutine check

end program test_gremlin_copy_tree_modes
