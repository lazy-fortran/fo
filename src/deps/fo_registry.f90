module fo_registry
    !! Native, offline FPM registry resolution. Remote/authenticated registries
    !! are rejected explicitly; local registry trees are the source authority.
    use fo_fpm_config, only: fpm_dep_t, valid_registry_version
    use fo_fs, only: fs_identity, fs_path_is_absolute, fs_native_path, &
        fs_is_windows, fs_parent_path
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: registry_resolve, registry_config_path

    interface
        integer(c_int) function registry_versions(path, versions, cap) &
            bind(C, name='fo_c_registry_versions')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: versions(*)
            integer(c_int), value :: cap
        end function registry_versions
    end interface
contains
    subroutine registry_config_path(path, ierr)
        character(len=*), intent(out) :: path
        integer, intent(out) :: ierr
        character(len=4096) :: home_dir
        integer :: status, length
        character(len=:), allocatable :: suffix

        ierr = 0
        call get_environment_variable('FO_FPM_CONFIG_FILE', path, length, status)
        if (status == -1 .or. length >= len(path)) then
            call registry_error('FO_FPM_CONFIG_FILE exceeds supported path length', &
                ierr)
            return
        end if
        if (status == 0 .and. length > 0) then
            path = fs_native_path(path)
            if (.not. fs_path_is_absolute(path)) then
                call registry_error('FO_FPM_CONFIG_FILE must be an absolute path', ierr)
            end if
            return
        end if
        suffix = '/.local/share/fpm/config.toml'
        if (fs_is_windows()) then
            call get_environment_variable('APPDATA', home_dir, length, status)
            suffix = '/local/fpm/config.toml'
        else
            call get_environment_variable('HOME', home_dir, length, status)
        end if
        if (status /= 0 .or. length == 0 .or. length + len(suffix) > len(path)) then
            call registry_error('cannot locate platform FPM registry config', ierr)
            return
        end if
        path = fs_native_path(home_dir)//suffix
    end subroutine registry_config_path

    subroutine registry_resolve(dep, root, directory, ierr)
        type(fpm_dep_t), intent(in) :: dep
        character(len=*), intent(in) :: root
        character(len=*), intent(out) :: directory
        integer, intent(out) :: ierr
        character(len=4096) :: registry, package, config, line, key, value
        character(len=64) :: latest, candidate
        character(kind=c_char) :: versions(65536)
        character(len=4096) :: frozen
        integer :: unit, ios, equals, i, j, rc, status
        logical :: exists, in_registry, have_path

        directory = ''
        ierr = 0
        if (.not. safe_component(trim(dep%namespace))) then
            call registry_error('dependency '//trim(dep%name)// &
                ' requires a nonempty safe namespace', ierr)
            return
        end if
        if (.not. safe_component(trim(dep%name))) then
            call registry_error('unsafe registry package name '//trim(dep%name), ierr)
            return
        end if
        call get_environment_variable('FO_GREMLIN_FROZEN_ROOT', frozen, status=status)
        if (status == 0) then
            if (same_directory(frozen, root)) then
                directory = trim(root)//'/build/dependencies/'//trim(dep%name)
                inquire (file=trim(directory)//'/fpm.toml', exist=exists)
                if (.not. exists) call registry_error( &
                    'missing frozen registry source '//trim(dep%name), ierr)
                return
            end if
        end if
        call registry_config_path(config, ierr)
        if (ierr /= 0) return
        open (newunit=unit, file=trim(config), status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call registry_error( &
                'native registry dependencies require [registry] path '// &
                'in '//trim(config)//'; remote registry access is unsupported', ierr)
            return
        end if
        registry = ''
        in_registry = .false.
        have_path = .false.
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == len(line)) then
                call registry_error('registry config line exceeds supported length', &
                    ierr)
                exit
            end if
            call remove_comment(line)
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '[') then
                if (trim(line) /= '[registry]') then
                    call registry_error('unsupported FPM registry config section '// &
                        trim(line), ierr)
                    exit
                end if
                if (in_registry) then
                    call registry_error('duplicate registry config section', ierr)
                    exit
                end if
                in_registry = .true.
                cycle
            end if
            equals = index(line, '=')
            if (equals <= 1 .or. .not. in_registry) then
                call registry_error('invalid or unsupported registry configuration', &
                    ierr)
                exit
            end if
            key = trim(line(:equals - 1))
            value = adjustl(line(equals + 1:))
            if (trim(key) /= 'path') then
                call registry_error( &
                    'unsupported registry config/auth key '//trim(key)// &
                    '; native resolution supports only local path', &
                    ierr)
                exit
            end if
            if (have_path .or. len_trim(value) < 3) then
                call registry_error('invalid or duplicate registry path', ierr)
                exit
            end if
            i = len_trim(value)
            if ((value(1:1) /= '"' .and. value(1:1) /= "'") .or. &
                value(i:i) /= value(1:1)) then
                call registry_error('registry path must be a quoted string', ierr)
                exit
            end if
            registry = value(2:i - 1)
            if (index(trim(registry), achar(92)) /= 0) then
                if (fs_is_windows() .and. value(1:1) == "'") then
                    registry = fs_native_path(registry)
                else
                    call registry_error('escaped registry paths are unsupported', ierr)
                    exit
                end if
            end if
            have_path = .true.
        end do
        close (unit)
        if (ierr /= 0) return
        if (ios > 0 .or. .not. have_path) then
            call registry_error('registry config requires a local path', ierr)
            return
        end if
        if (.not. fs_path_is_absolute(registry)) then
            call fs_parent_path(config, line)
            registry = trim(line)//'/'//trim(registry)
        end if
        package = trim(registry)//'/'//trim(dep%namespace)//'/'//trim(dep%name)
        if (dep%registry_v_seen) then
            latest = trim(dep%registry_v)
        else
            rc = registry_versions(trim(package)//c_null_char, versions, &
                int(size(versions), c_int))
            if (rc < 0) then
                call registry_error( &
                    'cannot list registry versions for '//trim(package), ierr)
                return
            end if
            latest = ''
            i = 1
            do while (i <= rc)
                j = i
                do while (j <= rc)
                    if (versions(j) == c_null_char) exit
                    j = j + 1
                end do
                candidate = ''
                if (j - i > len(candidate)) then
                    call registry_error('registry version exceeds supported length', &
                        ierr)
                    return
                end if
                do status = i, j - 1
                    candidate(status - i + 1:status - i + 1) = versions(status)
                end do
                if (.not. valid_registry_version(trim(candidate))) then
                    call registry_error( &
                        'invalid local registry version '//trim(candidate), ierr)
                    return
                end if
                if (len_trim(latest) == 0) then
                    latest = candidate
                else if (version_after(candidate, latest)) then
                    latest = candidate
                end if
                i = j + 1
            end do
            if (len_trim(latest) == 0) then
                call registry_error( &
                    'no registry versions found for '//trim(package), ierr)
                return
            end if
        end if
        call canonical_version(latest, candidate)
        latest = candidate
        if (len_trim(package) + len_trim(latest) + 1 >= len(directory)) then
            call registry_error('registry source path exceeds supported length', ierr)
            return
        end if
        directory = trim(package)//'/'//trim(latest)
        inquire (file=trim(directory)//'/fpm.toml', exist=exists)
        if ( &
            .not. exists) call registry_error( &
            'missing registry namespace/package/version '// &
            trim( &
            dep%namespace)//'/'//trim( &
            dep%name)//'/'//trim(latest), &
            ierr)
    end subroutine registry_resolve

    logical function same_directory(left, right) result(same)
        character(len=*), intent(in) :: left, right
        integer(c_long_long) :: left_device, left_inode, right_device, right_inode
        logical :: found

        same = .false.
        call fs_identity(left, left_device, left_inode, found)
        if (.not. found) return
        call fs_identity(right, right_device, right_inode, found)
        if (.not. found) return
        same = left_device == right_device .and. left_inode == right_inode
    end function same_directory

    pure subroutine canonical_version(value, canonical)
        character(len=*), intent(in) :: value
        character(len=*), intent(out) :: canonical
        character(len=32) :: number
        integer :: i, start, parsed, ios
        canonical = ''
        start = 1
        do i = 1, len_trim(value) + 1
            if (i <= len_trim(value)) then
                if (value(i:i) /= '.') cycle
            end if
            read (value(start:i - 1), *, iostat=ios) parsed
            write (number, '(i0)') parsed
            if (start > 1) canonical = trim(canonical)//'.'
            canonical = trim(canonical)//trim(number)
            start = i + 1
        end do
    end subroutine canonical_version

    pure logical function safe_component(value)
        character(len=*), intent(in) :: value
        safe_component = len_trim(value) > 0
        if (.not. safe_component) return
        safe_component = verify(value, &
            'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
            '0123456789_-') == 0
    end function safe_component

    pure logical function version_after(left, right)
        character(len=*), intent(in) :: left, right
        integer :: lhs(3), rhs(3), i, start, part, ios
        lhs = 0
        rhs = 0
        start = 1
        part = 1
        do i = 1, len_trim(left) + 1
            if (i <= len_trim(left)) then
                if (left(i:i) /= '.') cycle
            end if
            read (left(start:i - 1), *, iostat=ios) lhs(part)
            start = i + 1
            part = part + 1
        end do
        start = 1
        part = 1
        do i = 1, len_trim(right) + 1
            if (i <= len_trim(right)) then
                if (right(i:i) /= '.') cycle
            end if
            read (right(start:i - 1), *, iostat=ios) rhs(part)
            start = i + 1
            part = part + 1
        end do
        version_after = .false.
        do i = 1, 3
            if (lhs(i) == rhs(i)) cycle
            version_after = lhs(i) > rhs(i)
            return
        end do
        ! Numeric ties have a stable lexical selection independent of readdir.
        version_after = lgt(trim(left), trim(right))
    end function version_after

    subroutine remove_comment(line)
        character(len=*), intent(inout) :: line
        character :: quote
        integer :: i
        quote = ' '
        do i = 1, len_trim(line)
            if (quote /= ' ') then
                if (line(i:i) == quote) quote = ' '
            else if (line(i:i) == '"' .or. line(i:i) == "'") then
                quote = line(i:i)
            else if (line(i:i) == '#') then
                line(i:) = ''
                return
            end if
        end do
    end subroutine remove_comment

    subroutine registry_error(message, ierr)
        character(len=*), intent(in) :: message
        integer, intent(out) :: ierr
        write (error_unit, '(a)') 'fo: '//message
        ierr = 1
    end subroutine registry_error
end module fo_registry
