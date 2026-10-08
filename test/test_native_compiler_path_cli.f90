program test_native_compiler_path_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, make_symlink
    use fo_test_harness, only: assert_true, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_find_executable, fs_parent_path, fs_is_windows
    implicit none

    character(:), allocatable :: driver, scratch, project, alias, selected
    character(len=512) :: compiler, directory, prefix
    character(len=1), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: result
    logical :: found
    integer :: slash

    call resolve_driver(driver)
    call make_scratch('fo-native-compiler-path', scratch)
    call fs_find_executable('gfortran', compiler, found)
    call assert_true(found, 'locates the actual installed compiler')
    if (.not. found) call finish_assertions()
    call fs_parent_path(trim(compiler), directory)
    call fs_parent_path(trim(directory), prefix)
    slash = index(trim(directory), '/', back=.true.)
    alias = join_path(scratch, 'compiler prefix')
    if (fs_is_windows()) then
        call list_add(args, '/c')
        call list_add(args, 'mklink')
        call list_add(args, '/J')
        call list_add(args, cmd_path(alias))
        call list_add(args, cmd_path(trim(prefix)))
        call run_external('cmd.exe', args, scratch, result)
        call assert_process_ok(result, 'creates private compiler-prefix junction')
    else
        call make_symlink(trim(prefix), alias)
    end if
    selected = alias//'/'//trim(directory(slash + 1:))//'/'// &
        trim(compiler(len_trim(directory) + 2:))
    ! Alias the whole prefix so GCC can still locate its own libexec tree.
    ! The selected driver has spaces and is deliberately supplied unquoted.
    call list_add(environment, 'FO_FC='//selected)
    project = join_path(scratch, 'consumer')
    call write_text(join_path(project, 'fpm.toml'), &
        'name = "compiler_path_probe"'//nl//'[dependencies]'//nl// &
        'provider = { path = "../external provider" }'//nl)
    call write_text(join_path(scratch, 'external provider/fpm.toml'), &
        'name = "provider"'//nl)
    call write_text(join_path(scratch, 'external provider/src/provider.f90'), &
        'module provider'//nl//'implicit none'//nl// &
        'integer, parameter :: payload = 91'//nl//'end module provider'//nl)
    call write_text(join_path(project, 'app/probe.f90'), &
        'program probe'//nl//'use provider, only: payload'//nl// &
        'implicit none'//nl//"print '(i0)', payload"//nl//'end program probe'//nl)
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, 'probe')
    call run_fo(driver, args, project, join_path(scratch, 'cache'), &
        result, environment)
    call assert_process_ok(result, &
        'native compiler path compiles and links sibling input')
    call assert_true(result%stdout == '91'//nl, &
        'actual compiler behind spaced prefix executes external dependency payload')
    call run_fo(driver, args, project, join_path(scratch, 'cache'), &
        result, environment)
    call assert_process_ok(result, &
        'spaced selected compiler supports warm native run')
    call assert_true(result%stdout == '91'//nl, 'warm native run preserves exact payload')
    if (fs_is_windows()) then
        ! rmdir without /S removes only the junction, never the compiler tree.
        args = string_list_t()
        call list_add(args, '/c')
        call list_add(args, 'rmdir')
        call list_add(args, cmd_path(alias))
        call run_external('cmd.exe', args, scratch, result)
        call assert_process_ok(result, 'removes owned compiler junction before cleanup')
    end if
    call finish_assertions(retain_failed_scratch=.true.)
contains
    function cmd_path(path) result(native)
        character(len=*), intent(in) :: path
        character(:), allocatable :: native
        integer :: i

        native = path
        do i = 1, len(native)
            if (native(i:i) == '/') native(i:i) = achar(92)
        end do
    end function cmd_path
end program test_native_compiler_path_cli
