module fo_remote_cache
    !! Optional remote tier of the compile cache: an HTTP store of compile
    !! results by action key, as fortyard's shared build cache provides it
    !! (GET and PUT BASE/NAMESPACE/KEY). A local miss asks the namespace
    !! `shared`; a build that may write uploads what it compiles.
    !!
    !!   FO_REMOTE_CACHE        base URL, e.g. https://host/cache/v1; unset: off
    !!   FO_REMOTE_CACHE_WRITE  namespace to upload to (e.g. shared); unset: read only
    !!   FO_REMOTE_CACHE_LOGIN  file with token= and proxy= lines
    !!                          (default ~/.config/fortyard/credentials)
    !!
    !! A result travels as a tar archive of `object`, `mod/NAME.mod` and
    !! `smod/NAME.smod`. A download over HTTP is taken only when its SHA-256
    !! equals the server's X-Content-SHA256 header and it holds nothing but
    !! plain files of those names. Every failure is a miss: the build compiles.
    use fx_hash, only: sha256_file
    use fx_proc, only: proc_exec_silent
    use fo_fs, only: fs_make_dir, fs_remove_tree, fs_rename
    implicit none
    private

    public :: remote_cache_enabled, remote_cache_writes, remote_cache_fetch, &
        remote_cache_put, remote_cache_reset

    integer, parameter :: PATH_MAX = 4096
    logical, save :: loaded = .false.
    character(len=:), allocatable, save :: base_url, write_namespace, login_file

    ! Token and proxy of the login file $4, read without starting a process.
    character(len=*), parameter :: LOGIN_SCRIPT = &
        'tok=; proxy=; if [ -r "$4" ]; then while IFS= read -r l || [ -n "$l" ]; do '// &
        'case $l in token=*) tok=${l#token=};; proxy=*) proxy=${l#proxy=};; esac; '// &
        'done < "$4"; fi; '

    ! The token reaches curl on its standard input, never on a command line.
    character(len=*), parameter :: CURL_AUTH = &
        '[ -z "$tok" ] || printf ''header = "Authorization: Bearer %s"\n'' "$tok"; '// &
        '[ -z "$proxy" ] || printf ''proxy = "%s"\n'' "$proxy"; } | curl -K -'

    ! $1 url, $2 archive, $3 header file, $4 login file
    character(len=*), parameter :: FETCH_SCRIPT = LOGIN_SCRIPT// &
        '{ printf ''url = "%s"\nsilent\nfail\nmax-time = 60\noutput = "%s"\n'// &
        'dump-header = "%s"\n'' "$1" "$2" "$3"; '//CURL_AUTH

    ! $1 archive, $2 existing directory to extract into
    character(len=*), parameter :: EXTRACT_SCRIPT = &
        'if tar -tf "$1" | grep -q -v -E '// &
        '"^(object|mod/[a-z0-9_]+\.mod|smod/[a-z0-9_@]+\.smod)$"; then exit 5; fi; '// &
        'if tar -tvf "$1" | grep -q -v "^-"; then exit 5; fi; '// &
        'tar -xf "$1" -C "$2"'

    ! $1 work directory with s/mod, s/smod and the member list, $2 object,
    ! $3 module directory
    character(len=*), parameter :: PACK_SCRIPT = &
        'set -e; cp "$2" "$1/s/object"; while IFS= read -r f; do case $f in object) ;; '// &
        '*) cp "$3/${f#*/}" "$1/s/$f";; esac; done < "$1/list"; '// &
        'tar -cf "$1/a.tar" -C "$1/s" -T "$1/list"'

    ! $1 url, $2 archive, $3 its SHA-256, $4 login file
    character(len=*), parameter :: PUT_SCRIPT = LOGIN_SCRIPT// &
        '{ printf ''url = "%s"\nsilent\nfail\nmax-time = 120\nupload-file = "%s"\n'// &
        'header = "X-Content-SHA256: %s"\n'' "$1" "$2" "$3"; '//CURL_AUTH//' >/dev/null'

contains

    logical function remote_cache_enabled()
        call load()
        remote_cache_enabled = len(base_url) > 0
    end function remote_cache_enabled

    logical function remote_cache_writes()
        call load()
        remote_cache_writes = len(base_url) > 0 .and. len(write_namespace) > 0
    end function remote_cache_writes

    subroutine remote_cache_reset()
        !! Reads the settings again at the next use (for tests).
        !$omp critical(fo_remote_cache_load)
        loaded = .false.
        !$omp end critical(fo_remote_cache_load)
    end subroutine remote_cache_reset

    subroutine remote_cache_fetch(key, obj_path, mod_dir, modules, submodules, fetched)
        !! On success the object is at `obj_path` and every module and submodule
        !! file of the unit is in `mod_dir`.
        character(len=*), intent(in) :: key, obj_path, mod_dir
        !! Absent when unallocated at the caller: a unit without such outputs.
        character(len=*), intent(in), optional :: modules(:), submodules(:)
        logical, intent(out) :: fetched
        character(len=:), allocatable :: tmp, archive, headers, tree, want
        character(len=64) :: got
        integer :: status, i, n_mod, n_smod
        logical :: exists

        fetched = .false.
        n_mod = 0
        n_smod = 0
        if (present(modules)) n_mod = size(modules)
        if (present(submodules)) n_smod = size(submodules)
        if (.not. remote_cache_enabled()) return
        if (.not. key_ok(key)) return
        tmp = trim(obj_path)//'.remote'
        archive = tmp//'/a.tar'
        headers = tmp//'/headers'
        tree = tmp//'/x'
        call fs_remove_tree(tmp)
        call fs_make_dir(tree, status)
        if (status /= 0) return
        call run_sh(FETCH_SCRIPT, status, base_url//'/shared/'//trim(key), archive, headers, &
            login_file)
        if (status == 0) then
            want = header_value(headers, 'x-content-sha256')
            ! Without the header only a file:// store (a directory) is trusted.
            if (len(want) == 0 .and. index(base_url, 'file://') /= 1) status = 1
            if (status == 0 .and. len(want) > 0) then
                call sha256_file(archive, got, status)
                if (status == 0 .and. got /= want) status = 1
            end if
        end if
        if (status == 0) call run_sh(EXTRACT_SCRIPT, status, archive, tree)
        if (status == 0) then
            inquire (file=tree//'/object', exist=exists)
            if (.not. exists) status = 1
            do i = 1, n_mod
                inquire (file=tree//'/mod/'//lower(modules(i))//'.mod', exist=exists)
                if (.not. exists) status = 1
            end do
            do i = 1, n_smod
                inquire (file=tree//'/smod/'//lower(submodules(i))//'.smod', exist=exists)
                if (.not. exists) status = 1
            end do
        end if
        ! Only the outputs the unit declares leave the archive.
        if (status == 0) call fs_make_dir(trim(mod_dir), status)
        if (status == 0) status = fs_rename(tree//'/object', trim(obj_path))
        do i = 1, n_mod
            if (status /= 0) exit
            want = lower(modules(i))//'.mod'
            status = fs_rename(tree//'/mod/'//want, trim(mod_dir)//'/'//want)
        end do
        do i = 1, n_smod
            if (status /= 0) exit
            want = lower(submodules(i))//'.smod'
            status = fs_rename(tree//'/smod/'//want, trim(mod_dir)//'/'//want)
        end do
        fetched = status == 0
        call fs_remove_tree(tmp)
    end subroutine remote_cache_fetch

    subroutine remote_cache_put(key, obj_path, mod_dir, modules, submodules)
        !! Uploads a compiled unit; a failure only means it is not shared.
        character(len=*), intent(in) :: key, obj_path, mod_dir
        !! Absent when unallocated at the caller: a unit without such outputs.
        character(len=*), intent(in), optional :: modules(:), submodules(:)
        character(len=:), allocatable :: tmp, stage
        character(len=64) :: digest
        integer :: status, i, u, n_mod, n_smod

        n_mod = 0
        n_smod = 0
        if (present(modules)) n_mod = size(modules)
        if (present(submodules)) n_smod = size(submodules)
        if (.not. remote_cache_writes()) return
        if (.not. key_ok(key)) return
        tmp = trim(obj_path)//'.remote-put'
        stage = tmp//'/s'
        call fs_remove_tree(tmp)
        call fs_make_dir(stage//'/mod', status)
        if (status == 0) call fs_make_dir(stage//'/smod', status)
        if (status /= 0) return
        open (newunit=u, file=tmp//'/list', status='replace', action='write', iostat=status)
        if (status == 0) then
            write (u, '(a)') 'object'
            do i = 1, n_mod
                write (u, '(a)') 'mod/'//lower(modules(i))//'.mod'
            end do
            do i = 1, n_smod
                write (u, '(a)') 'smod/'//lower(submodules(i))//'.smod'
            end do
            close (u)
        end if
        if (status == 0) call run_sh(PACK_SCRIPT, status, tmp, trim(obj_path), trim(mod_dir))
        if (status == 0) call sha256_file(tmp//'/a.tar', digest, status)
        if (status == 0) then
            call run_sh(PUT_SCRIPT, status, base_url//'/'//write_namespace//'/'//trim(key), &
                tmp//'/a.tar', digest, login_file)
        end if
        call fs_remove_tree(tmp)
    end subroutine remote_cache_put

    subroutine load()
        character(len=:), allocatable :: home

        if (loaded) return
        !$omp critical(fo_remote_cache_load)
        if (.not. loaded) then
            base_url = env('FO_REMOTE_CACHE')
            do while (len(base_url) > 0)
                if (base_url(len(base_url):len(base_url)) /= '/') exit
                base_url = base_url(1:len(base_url) - 1)
            end do
            write_namespace = env('FO_REMOTE_CACHE_WRITE')
            if (verify(write_namespace, 'abcdefghijklmnopqrstuvwxyz0123456789') > 0) &
                write_namespace = ''
            login_file = env('FO_REMOTE_CACHE_LOGIN')
            if (len(login_file) == 0) then
                home = env('XDG_CONFIG_HOME')
                if (len(home) == 0) home = env('HOME')//'/.config'
                login_file = home//'/fortyard/credentials'
            end if
            loaded = .true.
        end if
        !$omp end critical(fo_remote_cache_load)
    end subroutine load

    function env(name) result(value)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: value
        integer :: n, status

        call get_environment_variable(name, length=n, status=status)
        if (status /= 0) n = 0
        allocate (character(len=n) :: value)
        if (n > 0) call get_environment_variable(name, value)
    end function env

    pure logical function key_ok(key)
        !! The store takes keys of 64 hexadecimal digits only.
        character(len=*), intent(in) :: key
        key_ok = len_trim(key) == 64 .and. verify(trim(key), '0123456789abcdef') == 0
    end function key_ok

    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i, c

        t = trim(s)
        do i = 1, len(t)
            c = iachar(t(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) t(i:i) = achar(c + 32)
        end do
    end function lower

    function header_value(path, name) result(value)
        !! The last value of the header `name` (lower case) in a curl header dump.
        character(len=*), intent(in) :: path, name
        character(len=:), allocatable :: value
        character(len=1024) :: line
        integer :: u, ios, colon

        value = ''
        open (newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            colon = index(line, ':')
            if (colon < 2) cycle
            if (lower(line(1:colon - 1)) /= name) cycle
            value = trim(adjustl(line(colon + 1:)))
            if (len(value) > 0) then
                if (value(len(value):len(value)) == achar(13)) value = value(1:len(value) - 1)
            end if
        end do
        close (u)
    end function header_value

    subroutine run_sh(script, status, a1, a2, a3, a4)
        !! Runs `script` with sh and up to four arguments. The arguments are
        !! copied one by one into fixed-length elements: gfortran overflows
        !! typed array constructors around deferred-length strings.
        character(len=*), intent(in) :: script
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: a1, a2, a3, a4
        character(len=PATH_MAX) :: argv(8)
        integer :: n

        argv(1) = 'sh'
        argv(2) = '-c'
        argv(3) = script
        argv(4) = 'fo-remote-cache'
        n = 4
        if (present(a1)) then
            n = 5
            argv(5) = a1
        end if
        if (present(a2)) then
            n = 6
            argv(6) = a2
        end if
        if (present(a3)) then
            n = 7
            argv(7) = a3
        end if
        if (present(a4)) then
            n = 8
            argv(8) = a4
        end if
        call proc_exec_silent(argv, n, status)
    end subroutine run_sh

end module fo_remote_cache
