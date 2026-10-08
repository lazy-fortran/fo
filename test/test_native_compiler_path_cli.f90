program test_native_compiler_path_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text
    use fo_test_harness, only: assert_true, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_find_executable
    use fx_path, only: path_dirname, path_basename
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
    implicit none

    character(:), allocatable :: driver, scratch, project, alias, selected
    character(len=512) :: compiler
    character(:), allocatable :: directory, prefix
    character(len=1), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: result
    logical :: found, junction
    integer :: link_status
    interface
        integer(c_int) function link_directory(target, link_path) &
                bind(C, name='fo_test_symlink')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: target(*), link_path(*)
        end function link_directory
    end interface

    call resolve_driver(driver)
    call make_scratch('fo-native-compiler-path', scratch)
    call fs_find_executable('gfortran', compiler, found)
    call assert_true(found, 'locates the actual installed compiler')
    if (.not. found) call finish_assertions()
    directory = path_dirname(trim(compiler))
    prefix = path_dirname(directory)
    alias = join_path(scratch, 'compiler prefix')
    link_status = int(link_directory(prefix//c_null_char, alias//c_null_char))
    junction = link_status /= 0
    if (junction) then
        call list_add(args, '/c')
        call list_add(args, 'mklink')
        call list_add(args, '/J')
        call list_add(args, cmd_path(alias))
        call list_add(args, cmd_path(trim(prefix)))
        call run_external('cmd.exe', args, scratch, result)
        call assert_process_ok(result, 'creates private compiler-prefix junction')
    end if
    selected = alias//'/'//path_basename(directory)//'/'//path_basename(trim(compiler))
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
        'implicit none'//nl//"print '(i0,1x,i0)', payload, kind(payload)"//nl// &
        'end program probe'//nl)
    args = string_list_t()
    call list_add(args, 'exec')
    call list_add(args, 'probe')
    call run_fo(driver, args, project, join_path(scratch, 'cache'), &
        result, environment)
    call assert_process_ok(result, &
        'native compiler path compiles and links sibling input')
    call assert_true(result%stdout == '91 4'//nl, &
        'actual compiler behind spaced prefix executes external dependency payload')
    call run_fo(driver, args, project, join_path(scratch, 'cache'), &
        result, environment)
    call assert_process_ok(result, &
        'spaced selected compiler supports warm native run')
    call assert_true(result%stdout == '91 4'//nl, 'warm native run preserves exact payload')
    environment = string_list_t()
    call list_add(environment, 'FO_FC=')
    call list_add(environment, 'FC='//selected)
    call check_compiler_selection('fc-fallback', '91 4', &
        'FC fallback preserves the literal spaced compiler executable')
    environment = string_list_t()
    call list_add(environment, 'FO_FC='//trim(compiler)//' -fdefault-integer-8')
    call check_compiler_selection('command-flags', '91 8', &
        'true compiler command string retains its actual compilation flag')
    if (junction) then
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
    subroutine check_compiler_selection(cache_name, expected, assertion)
        character(len=*), intent(in) :: cache_name, expected, assertion
        integer :: iteration

        do iteration = 1, 2
            call run_fo(driver, args, project, join_path(scratch, cache_name), &
                result, environment)
            call assert_process_ok(result, assertion)
            call assert_true(result%stdout == expected//nl, &
                assertion//' produces exact dependency payload and compiler-selected kind')
        end do
    end subroutine check_compiler_selection

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
