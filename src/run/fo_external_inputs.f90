module fo_external_inputs
    !! Direct external inputs of a native FPM build: the library files that
    !! `link` entries and metapackages resolve to, the compiler-provided OpenMP
    !! spec, runtime and module files, and the pkg-config metadata behind
    !! provider flags. Each resolved file is recorded with its content digest,
    !! so a changed library changes the canonical input inventory. The closure
    !! stops at these direct inputs: transitive runtime dependencies, the C
    !! runtime and the rest of the toolchain remain covered by the compiler
    !! identity, as for any project without external inputs.
    use fo_fpm_config, only: fpm_config_t, command_run, pkg_config_run
    use fo_link_resolve, only: resolve_link_token, build_lib_search_dirs, is_macos
    use fo_compiler_dialect, only: compiler_dialect, compiler_dialect_t, &
        selected_compiler_command, COMPILER_GFORTRAN
    use fo_fs, only: fs_find_executable, fs_realpath, fs_path_is_absolute
    use fx_immutable_store, only: immutable_store_hash_file, IMMUTABLE_OK
    use fo_cache, only: HASH_LEN
    implicit none
    private

    integer, parameter :: NAME_LEN = 512
    integer, parameter :: MAX_REQUESTS = 128

    type, public :: external_requests_t
        logical :: openmp = .false.
        logical :: static_link = .false.
        integer :: n_links = 0, n_packages = 0, n_search = 0
        ! Allocated on first use: inventories are copied and held on stacks.
        character(len=NAME_LEN), allocatable :: links(:), packages(:), search(:)
        character(len=:), allocatable :: overflow
    end type external_requests_t

    public :: external_requests_add, external_inputs_resolve

contains

    subroutine external_requests_add(requests, config, root)
        !! Collect the external requests of one manifest. Only the root's
        !! library-search and static-link flags reach the native link line.
        type(external_requests_t), intent(inout) :: requests
        type(fpm_config_t), intent(in) :: config
        logical, intent(in) :: root
        integer :: i, start, finish
        character(len=:), allocatable :: flag

        requests%openmp = requests%openmp .or. config%openmp
        do i = 1, config%n_link_libs
            call add_unique(requests%links, requests%n_links, &
                trim(config%link_libs(i)), requests%overflow)
        end do
        start = 1
        do while (start <= len_trim(config%provider_packages))
            if (config%provider_packages(start:start) == ' ') then
                start = start + 1
                cycle
            end if
            finish = index(config%provider_packages(start:), ' ')
            if (finish == 0) then
                finish = len_trim(config%provider_packages)
            else
                finish = start + finish - 2
            end if
            call add_unique(requests%packages, requests%n_packages, &
                config%provider_packages(start:finish), requests%overflow)
            start = finish + 1
        end do
        if (.not. root) return
        do i = 1, config%n_flags
            flag = trim(config%flags(i))
            if (flag == '-static' .or. index(flag, 'Bstatic') > 0 .or. &
                flag == '-static-libgcc' .or. flag == '-static-libgfortran') &
                requests%static_link = .true.
            if (flag == '-L' .and. i < config%n_flags) then
                call add_unique(requests%search, requests%n_search, &
                    trim(config%flags(i + 1)), requests%overflow)
            else if (len(flag) > 2 .and. index(flag, '-L') == 1) then
                call add_unique(requests%search, requests%n_search, flag(3:), &
                    requests%overflow)
            end if
        end do
    end subroutine external_requests_add

    subroutine add_unique(list, n, item, overflow)
        character(len=NAME_LEN), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: item
        character(len=:), allocatable, intent(inout) :: overflow

        if (len_trim(item) == 0) return
        if (.not. allocated(list)) allocate(list(MAX_REQUESTS))
        if (any(list(:n) == item)) return
        if (n >= size(list) .or. len_trim(item) > len(list)) then
            overflow = 'external input request exceeds the supported size: '//item
            return
        end if
        n = n + 1
        list(n) = item
    end subroutine add_unique

    subroutine external_inputs_resolve(project_root, requests, text, reasons)
        !! text is the canonical record of every resolved external input, or
        !! empty when the build has none. reasons lists, newline separated,
        !! each requested input that could not be resolved to a file.
        character(len=*), intent(in) :: project_root
        type(external_requests_t), intent(in) :: requests
        character(len=:), allocatable, intent(out) :: text, reasons
        character(len=4096) :: compiler
        character(len=1024) :: token
        character(len=512) :: dirs(128)
        character(len=4096) :: output
        type(compiler_dialect_t) :: dialect
        character(len=:), allocatable :: name, package
        integer :: i, n_dirs, n_env, exitcode
        logical :: found

        text = ''
        reasons = ''
        if (.not. requests%openmp .and. requests%n_links == 0 .and. &
            requests%n_packages == 0) return
        if (allocated(requests%overflow)) call reason(reasons, requests%overflow)
        text = 'fo-external-inputs 1'//new_line('a')
        call fs_find_executable(trim(selected_compiler_command()), compiler, found)
        if (.not. found) then
            call reason(reasons, 'compiler for external inputs is not an executable: '// &
                trim(selected_compiler_command()))
            return
        end if
        text = text//'compiler '//trim(compiler)//new_line('a')
        dialect = compiler_dialect(trim(selected_compiler_command()))
        if (requests%static_link) call reason(reasons, &
            'static external linking is not modeled')
        if (requests%openmp) then
            if (dialect%kind /= COMPILER_GFORTRAN) then
                call reason(reasons, 'OpenMP inputs are modeled for gfortran only')
            else
                call record_compiler_file(trim(compiler), 'libgomp.spec', text, reasons)
                if (is_macos()) then
                    call record_compiler_file(trim(compiler), 'libgomp.dylib', text, &
                        reasons)
                else
                    call record_compiler_file(trim(compiler), 'libgomp.so', text, &
                        reasons)
                end if
                call record_compiler_file(trim(compiler), 'finclude/omp_lib.mod', &
                    text, reasons)
                call record_compiler_file(trim(compiler), &
                    'finclude/omp_lib_kinds.mod', text, reasons)
                call record_compiler_file(trim(compiler), 'finclude/omp_lib.h', &
                    text, reasons)
            end if
        end if
        call build_lib_search_dirs(dirs, n_dirs, n_env)
        do i = 1, requests%n_links
            name = trim(requests%links(i))
            if (index(name, '/') > 0) then
                if (fs_path_is_absolute(name)) then
                    call record_file('link-path', name, name, text, reasons)
                else
                    call record_file('link-path', name, &
                        trim(project_root)//'/'//name, text, reasons)
                end if
                cycle
            end if
            if (name(1:1) == '-') then
                call reason(reasons, 'link token is not modeled: '//name)
                cycle
            end if
            ! Fo's own link resolves the entry first; a declared-flags link
            ! passes -l<name> to the linker search. Record both files.
            call resolve_link_token(project_root, name, dirs, n_dirs, n_env, &
                token)
            found = index(token, '-l') /= 1
            if (found) call record_file('link-fo', name, trim(token), text, &
                reasons)
            call linker_search(trim(compiler), requests, name, token)
            if (len_trim(token) > 0) then
                call record_file('link-search', name, trim(token), text, reasons)
            else if (.not. found) then
                call reason(reasons, 'link library is not resolvable to a file: '// &
                    name)
            end if
        end do
        do i = 1, requests%n_packages
            package = trim(requests%packages(i))
            call pkg_config_run(package, '--variable=pcfiledir', output, exitcode)
            call first_line(output)
            if (exitcode /= 0 .or. len_trim(output) == 0) then
                call reason(reasons, 'pkg-config metadata is unavailable: '// &
                    package)
                cycle
            end if
            call record_file('pkg-config', package, &
                trim(output)//'/'//package//'.pc', text, reasons)
        end do
    end subroutine external_inputs_resolve

    subroutine record_compiler_file(compiler, name, text, reasons)
        character(len=*), intent(in) :: compiler, name
        character(len=:), allocatable, intent(inout) :: text, reasons
        character(len=4096) :: output
        integer :: exitcode

        call command_run(compiler, '-print-file-name='//name, output, exitcode)
        call first_line(output)
        if (exitcode /= 0 .or. .not. fs_path_is_absolute(trim(output))) then
            call reason(reasons, 'compiler does not provide OpenMP input '//name)
            return
        end if
        call record_file('openmp', name, trim(output), text, reasons)
    end subroutine record_compiler_file

    subroutine linker_search(compiler, requests, name, path)
        !! The file the linker selects for -l<name>: explicit -L directories,
        !! then the compiler's library search directories (which include
        !! LIBRARY_PATH), then on macOS the SDK and local defaults of ld64.
        !! Each directory prefers the shared form before the archive.
        character(len=*), intent(in) :: compiler, name
        type(external_requests_t), intent(in) :: requests
        character(len=*), intent(out) :: path
        character(len=16384) :: output
        character(len=4096) :: sysroot
        integer :: i, start, finish, exitcode, marker

        path = ''
        do i = 1, requests%n_search
            call search_directory(trim(requests%search(i)), name, path)
            if (len_trim(path) > 0) return
        end do
        call command_run(compiler, '-print-search-dirs', output, exitcode)
        if (exitcode == 0) then
            marker = index(output, 'libraries: =')
            if (marker > 0) then
                start = marker + len('libraries: =')
                finish = index(output(start:), new_line('a'))
                if (finish == 0) then
                    finish = len_trim(output)
                else
                    finish = start + finish - 2
                end if
                do while (start <= finish)
                    i = index(output(start:finish), ':')
                    if (i == 0) then
                        i = finish + 1
                    else
                        i = start + i - 1
                    end if
                    if (i > start) call search_directory(output(start:i - 1), name, &
                        path)
                    if (len_trim(path) > 0) return
                    start = i + 1
                end do
            end if
        end if
        if (.not. is_macos()) return
        call command_run(compiler, '-print-sysroot', sysroot, exitcode)
        call first_line(sysroot)
        if (exitcode == 0 .and. len_trim(sysroot) > 0) then
            call search_directory(trim(sysroot)//'/usr/lib', name, path)
            if (len_trim(path) > 0) return
            call search_directory(trim(sysroot)//'/usr/local/lib', name, path)
            if (len_trim(path) > 0) return
        end if
        call search_directory('/usr/local/lib', name, path)
    end subroutine linker_search

    subroutine search_directory(directory, name, path)
        character(len=*), intent(in) :: directory, name
        character(len=*), intent(inout) :: path
        character(len=8), parameter :: darwin(4) = &
            [character(len=8) :: '.tbd', '.dylib', '.so', '.a']
        character(len=8), parameter :: elf(2) = [character(len=8) :: '.so', '.a']
        character(len=:), allocatable :: base
        integer :: i
        logical :: exists

        base = trim(directory)
        if (len(base) > 1) then
            if (base(len(base):) == '/') base = base(:len(base) - 1)
        end if
        if (len(base) == 0) return
        if (is_macos()) then
            do i = 1, size(darwin)
                inquire(file=base//'/lib'//name//trim(darwin(i)), &
                    exist=exists)
                if (.not. exists) cycle
                path = base//'/lib'//name//trim(darwin(i))
                return
            end do
        else
            do i = 1, size(elf)
                inquire(file=base//'/lib'//name//trim(elf(i)), &
                    exist=exists)
                if (.not. exists) cycle
                path = base//'/lib'//name//trim(elf(i))
                return
            end do
        end if
    end subroutine search_directory

    subroutine record_file(kind, name, path, text, reasons)
        !! One record per input: kind, logical name, content digest, the path
        !! the build names and the file it resolves to.
        character(len=*), intent(in) :: kind, name, path
        character(len=:), allocatable, intent(inout) :: text, reasons
        character(len=4096) :: resolved
        character(len=HASH_LEN) :: digest
        integer :: status
        logical :: ok

        if (index(path, new_line('a')) > 0 .or. index(name, new_line('a')) > 0) then
            call reason(reasons, kind//' input path contains a newline: '//name)
            return
        end if
        call fs_realpath(path, resolved, ok)
        if (.not. ok) then
            call reason(reasons, kind//' input does not exist: '//name//' at '//path)
            return
        end if
        call immutable_store_hash_file(trim(resolved), digest, status)
        if (status /= IMMUTABLE_OK .or. len_trim(digest) == 0) then
            call reason(reasons, kind//' input cannot be hashed: '//trim(resolved))
            return
        end if
        text = text//kind//' '//name//' '//trim(digest)//' '//path//' -> '// &
            trim(resolved)//new_line('a')
    end subroutine record_file

    subroutine reason(reasons, message)
        character(len=:), allocatable, intent(inout) :: reasons
        character(len=*), intent(in) :: message

        reasons = reasons//trim(message)//new_line('a')
    end subroutine reason

    subroutine first_line(text)
        character(len=*), intent(inout) :: text
        integer :: newline_at

        newline_at = index(text, new_line('a'))
        if (newline_at > 0) text(newline_at:) = ''
        newline_at = index(text, achar(13))
        if (newline_at > 0) text(newline_at:) = ''
    end subroutine first_line

end module fo_external_inputs
