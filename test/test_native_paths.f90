program test_native_paths
    !! Native path spellings must address real files without a POSIX runtime.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fo_fs, only: fs_is_windows, fs_native_path, fs_path_is_absolute
    use fo_fs, only: fs_parent_path, fs_make_dir, fs_remove_tree
    use fo_fs, only: fs_realpath, fs_identity, fs_find_executable
    use fo_dep_resolve, only: normalize_path, join_path
    use fo_util, only: make_tmpfile
    use fo_registry, only: registry_config_path
    implicit none

    character(len=512) :: result, root, alias, file, compiler, config, expected
    character(len=64) :: contents
    integer(8) :: first_device, first_inode, second_device, second_inode
    integer :: failures, unit, status, i
    logical :: found, first_ok, second_ok

    failures = 0
    call normalize_path('/one/../two/./three', result)
    call check(result == '/two/three', 'lexical relative segments')
    call normalize_path('../../one', result)
    call check(result == '../../one', 'relative parents remain relative')
    if (fs_is_windows()) then
        call normalize_path('C:'//achar(92)//'one'//achar(92)//'..'// &
            achar(92)//'two', result)
        call check(result == 'C:/two', 'native drive normalization')
        call normalize_path('C:/../..', result)
        call check(result == 'C:/', 'drive root bounds lexical parents')
        call normalize_path('//server/share/one/../..', result)
        call check(result == '//server/share/', 'UNC share bounds parents')
        call normalize_path('C:one/../two', result)
        call check(result == 'C:two', 'drive-relative path stays drive relative')
        call join_path('C:/project', 'D:/dependency', result)
        call check(result == 'D:/dependency', 'absolute dependency changes drive')
        call join_path('C:/project', achar(92)//'dependency', result)
        call check(result == 'C:/dependency', 'root-relative path retains drive')
        call join_path('//server/share/project', '../dependency', result)
        call check(result == '//server/share/dependency', 'UNC relative dependency')
        call fs_parent_path('C:/one', result)
        call check(result == 'C:/', 'parent reaches drive root')
        call fs_parent_path('C:/', result)
        call check(result == 'C:/', 'parent remains at drive root')
        call fs_parent_path('//server/share/one', result)
        call check(result == '//server/share/', 'parent reaches UNC share')
        call check(fs_path_is_absolute('C:/one'), 'native drive is absolute')
        call check(fs_path_is_absolute('//server/share/one'), 'UNC is absolute')
        call check(.not. fs_path_is_absolute('C:one'), 'drive-relative is relative')
        call check(.not. fs_path_is_absolute('/c/one'), 'no fabricated MSYS mapping')
    else
        call normalize_path('one'//achar(92)//'two', result)
        call check(result == 'one'//achar(92)//'two', 'POSIX backslash is a byte')
        call normalize_path('C:/one', result)
        call check(result == 'C:/one', 'POSIX colon is a filename byte')
        call check(.not. fs_path_is_absolute('C:/one'), 'POSIX drive spelling relative')
    end if

    call fs_find_executable('gfortran', compiler, found)
    call check(found, 'native PATH discovers the installed compiler')
    if (fs_is_windows() .and. found) then
        call check(index(compiler, '.exe', back=.true.) > 0, 'native executable suffix')
    end if
    call make_tmpfile('fo-native-path-π', root)
    call fs_make_dir(root)
    file = trim(root)//'/unicode-λ.txt'
    open (newunit=unit, file=trim(file), status='replace', iostat=status)
    call check(status == 0, 'Fortran opens native Unicode temporary path')
    if (status == 0) then
        write (unit, '(a)') 'independent native filesystem marker'
        close (unit)
        alias = file
        if (fs_is_windows()) then
            do i = 1, len_trim(alias)
                if (alias(i:i) == '/') alias(i:i) = achar(92)
            end do
        end if
        open (newunit=unit, file=trim(alias), status='old', iostat=status)
        call check(status == 0, 'alternate native separators open the same file')
        if (status == 0) then
            read (unit, '(a)', iostat=status) contents
            close (unit)
            call check(status == 0 .and. &
                contents == 'independent native filesystem marker', 'actual file contents')
        end if
        call fs_identity(file, first_device, first_inode, first_ok)
        call fs_identity(alias, second_device, second_inode, second_ok)
        call check(first_ok .and. second_ok, 'native aliases have file identities')
        call check(first_device == second_device .and. first_inode == second_inode, &
            'alternate spellings identify the same file')
        call fs_realpath(alias, result, found)
        call check(found, 'native physical path resolves Unicode file')
        call check(index(result, achar(92)) == 0, 'physical path uses shared separators')
    end if
    call get_command_argument(1, expected)
    if (len_trim(expected) > 0) then
        call registry_config_path(config, status)
        call check(status == 0, 'native registry config path accepted')
        call check(config == fs_native_path(expected), 'registry path follows native contract')
    end if
    call fs_remove_tree(root, status)
    call check(status == 0, 'owned native scratch removed')
    if (failures > 0) stop 1
    print '(a)', 'native paths PASS'

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label

        if (condition) return
        failures = failures + 1
        write (error_unit, '(a)') 'FAIL: '//label
    end subroutine check

end program test_native_paths
