module fo_install
    use fo_build_backend, only: backend_t, BACKEND_NATIVE, backend_build, &
        profile_flags
    use fo_build_tree, only: native_output_dir
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_scan, only: scan_unit_t
    use fo_gfortran_build, only: gfortran_app_source_name, scan_project_app_units
    use fo_fs, only: fs_copy_exec, fs_make_dir, fs_mkdir_excl, fs_remove_file, &
        fs_collect_files, fs_remove_tree, fs_rename
    use fo_process, only: process_getpid
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_diagnostics, only: diagnostic_t, diagnostic_from_log
    implicit none
    private
    public :: install_native_executables

contains

    subroutine install_native_executables(backend, prefix, message, exitcode)
        type(backend_t), intent(inout) :: backend
        character(len=*), intent(in) :: prefix
        character(len=*), intent(out) :: message
        integer, intent(out) :: exitcode

        type(fpm_config_t) :: config
        type(scan_unit_t), allocatable :: units(:)
        character(len=128) :: names(256)
        character(len=512) :: stage, source, target
        character(len=512) :: log_path
        character(len=32) :: line_text
        type(diagnostic_t) :: diag
        character(len=32) :: pid_text
        character(len=128) :: name
        character(len=512) :: prefix_files(256)
        character(len=:), allocatable :: release_flags, bin_dir
        integer :: ierr, n_units, n_names, i, build_status, pid, attempt, stage_rc
        integer :: cleanup_rc
        logical :: exists, had_previous(256), published(256), rollback_ok

        message = ''
        exitcode = 1
        if (backend%kind /= BACKEND_NATIVE) then
            message = 'fo install: this backend does not support native installation'
            return
        end if
        call fpm_config_parse(backend%project_dir, config, ierr)
        if (ierr /= 0) then
            message = 'fo install: could not read fpm.toml'
            return
        end if
        if (config%install_library .or. config%install_test .or. &
                len_trim(config%install_module_dir) > 0) then
            message = 'fo install: unsupported FPM installation form: '// &
                unsupported_install_description(config)
            return
        end if

        call scan_project_app_units(backend%project_dir, config, units, n_units, ierr)
        if (ierr /= 0) then
            message = 'fo install: could not scan declared executable sources'
            return
        end if
        n_names = 0
        do i = 1, n_units
            if (.not. units(i)%is_program .or. units(i)%is_test) cycle
            name = gfortran_app_source_name(config, units(i)%filename)
            if (len_trim(name) == 0) cycle
            if (.not. safe_executable_name(trim(name))) then
                message = 'fo install: invalid executable name: '//trim(name)
                return
            end if
            if (name_in_list(names, n_names, trim(name))) cycle
            if (n_names == size(names)) then
                message = 'fo install: too many executable targets'
                return
            end if
            n_names = n_names + 1
            names(n_names) = name
        end do
        if (n_names == 0) then
            message = 'fo install: no supported executable targets found'
            return
        end if

        release_flags = profile_flags('release')
        call make_tmpfile('fo-install-build', log_path)
        call backend_build(backend, build_status, flags=release_flags, &
            log_file=log_path)
        if (build_status /= 0) then
            ! Report the first compiler diagnostic like fo build, and keep the
            ! log so the full output stays inspectable.
            call diagnostic_from_log('build', log_path, 'fo install', diag)
            message = 'fo install: release build failed: '//trim(diag%message)
            if (len_trim(diag%file) > 0) then
                write (line_text, '(i0)') diag%line
                message = trim(message)//new_line('a')//'fo: at: '// &
                    trim(diag%file)//':'//trim(line_text)
            end if
            message = trim(message)//new_line('a')//'fo: full log: '//trim(log_path)
            exitcode = build_status
            return
        end if
        call delete_tmpfile(log_path)

        pid = process_getpid()
        write (pid_text, '(i0)') pid
        call fs_make_dir(trim(prefix))
        do attempt = 1, 100
            stage = trim(prefix)//'/.fo-install-'//trim(pid_text)//'-'// &
                integer_text(attempt)
            stage_rc = fs_mkdir_excl(trim(stage))
            if (stage_rc == 0) exit
        end do
        if (stage_rc /= 0) then
            message = 'fo install: could not reserve a staging directory'
            return
        end if
        call fs_make_dir(trim(stage)//'/incoming')
        call fs_make_dir(trim(stage)//'/previous')
        had_previous = .false.
        published = .false.
        bin_dir = native_output_dir(backend%project_dir, release_flags, 'bin')
        do i = 1, n_names
            source = trim(bin_dir)//'/'//trim(names(i))
            target = trim(stage)//'/incoming/'//trim(names(i))
            inquire (file=trim(source), exist=exists)
            if (.not. exists .or. fs_copy_exec(trim(source), trim(target)) /= 0) then
                call fs_remove_tree(trim(stage), cleanup_rc)
                message = 'fo install: could not stage executable '//trim(names(i))
                if (cleanup_rc /= 0) message = trim(message)// &
                    '; staging cleanup failed: '//trim(stage)
                return
            end if
        end do
        do i = 1, n_names
            target = trim(prefix)//'/bin/'//trim(names(i))
            inquire (file=trim(target), exist=exists)
            if (exists) then
                if (.not. regular_prefix_file(trim(prefix)//'/bin', &
                        trim(names(i)), prefix_files)) then
                    call fs_remove_tree(trim(stage), cleanup_rc)
                    message = 'fo install: non-regular prefix entry: '//trim(target)
                    if (cleanup_rc /= 0) message = trim(message)// &
                        '; staging cleanup failed: '//trim(stage)
                    return
                end if
            end if
        end do
        call fs_make_dir(trim(prefix)//'/bin')
        do i = 1, n_names
            source = trim(stage)//'/incoming/'//trim(names(i))
            target = trim(prefix)//'/bin/'//trim(names(i))
            inquire (file=trim(target), exist=exists)
            if (exists) then
                if (fs_rename(trim(target), trim(stage)//'/previous/'// &
                        trim(names(i))) /= 0) then
                    call rollback_install(stage, prefix, names, had_previous, &
                        published, i - 1, rollback_ok)
                    message = 'fo install: could not preserve existing executable '// &
                        trim(names(i))
                    if (rollback_ok) then
                        call fs_remove_tree(trim(stage), cleanup_rc)
                        if (cleanup_rc /= 0) message = trim(message)// &
                            '; staging cleanup failed: '//trim(stage)
                    else
                        message = trim(message)// &
                            '; rollback failed; recovery files: '//trim(stage)
                    end if
                    return
                end if
                had_previous(i) = .true.
            end if
            if (fs_rename(trim(source), trim(target)) /= 0) then
                call rollback_install(stage, prefix, names, had_previous, &
                    published, i, rollback_ok)
                message = 'fo install: could not publish executable '//trim(names(i))
                if (rollback_ok) then
                    call fs_remove_tree(trim(stage), cleanup_rc)
                    if (cleanup_rc /= 0) message = trim(message)// &
                        '; staging cleanup failed: '//trim(stage)
                else
                    message = trim(message)//'; rollback failed; recovery files: '// &
                        trim(stage)
                end if
                return
            end if
            published(i) = .true.
        end do
        call fs_remove_tree(trim(stage), cleanup_rc)
        if (cleanup_rc /= 0) then
            message = 'fo install: published; staging cleanup failed: '//trim(stage)
            return
        end if
        message = 'installed: '//trim(prefix)//'/bin/'
        exitcode = 0
    end subroutine install_native_executables

    subroutine rollback_install(stage, prefix, names, had_previous, published, &
            through, ok)
        character(len=*), intent(in) :: stage, prefix, names(:)
        logical, intent(in) :: had_previous(:), published(:)
        integer, intent(in) :: through
        logical, intent(out) :: ok
        integer :: k, rc
        character(len=512) :: installed, backup
        logical :: exists

        ok = .true.
        do k = through, 1, -1
            installed = trim(prefix)//'/bin/'//trim(names(k))
            backup = trim(stage)//'/previous/'//trim(names(k))
            if (had_previous(k)) then
                rc = fs_rename(trim(backup), trim(installed))
                if (rc /= 0) ok = .false.
            else if (published(k)) then
                call fs_remove_file(trim(installed))
                inquire (file=trim(installed), exist=exists)
                if (exists) ok = .false.
            end if
        end do
    end subroutine rollback_install

    logical function name_in_list(names, count, name)
        character(len=*), intent(in) :: names(:), name
        integer, intent(in) :: count
        integer :: i
        name_in_list = .false.
        do i = 1, count
            if (trim(names(i)) == name) then
                name_in_list = .true.
                return
            end if
        end do
    end function name_in_list

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=8) :: text
        write (text, '(i0)') value
    end function integer_text

    function unsupported_install_description(config) result(description)
        type(fpm_config_t), intent(in) :: config
        character(len=256) :: description

        if (config%install_library) then
            description = '[install].library=true'
        else if (config%install_test) then
            description = '[install].test=true'
        else
            description = '[install].module-dir'
        end if
    end function unsupported_install_description

    logical function regular_prefix_file(bin_dir, name, files)
        character(len=*), intent(in) :: bin_dir, name
        character(len=*), intent(out) :: files(:)
        integer :: count, i
        call fs_collect_files(bin_dir, name, '', '', files, count, recursive=.false.)
        regular_prefix_file = .false.
        do i = 1, count
            if (trim(files(i)) == trim(bin_dir)//'/'//trim(name)) then
                regular_prefix_file = .true.
                return
            end if
        end do
    end function regular_prefix_file

    logical function safe_executable_name(name)
        character(len=*), intent(in) :: name
        integer :: i, code

        safe_executable_name = .false.
        if (len_trim(name) == 0 .or. trim(name) == '.' .or. trim(name) == '..') return
        do i = 1, len_trim(name)
            code = iachar(name(i:i))
            if (.not. ((code >= iachar('a') .and. code <= iachar('z')) .or. &
                    (code >= iachar('A') .and. code <= iachar('Z')) .or. &
                    (code >= iachar('0') .and. code <= iachar('9')) .or. &
                    index('._-', name(i:i)) > 0)) return
        end do
        safe_executable_name = .true.
    end function safe_executable_name

end module fo_install
