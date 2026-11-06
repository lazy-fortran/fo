program test_pkg_config_kegs
    !! Keg-only Homebrew OpenBLAS is found without a hand-set PKG_CONFIG_PATH,
    !! and a path the user set keeps precedence.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, remove_tree, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_fs, only: fs_make_dir
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch, project, keg
    type(string_list_t) :: args, env
    type(process_result_t) :: result

    call resolve_driver(driver)
    call make_scratch('fo-pkg-config-kegs', scratch)
    keg = scratch//'/brew/opt/openblas/lib/pkgconfig'
    call fs_make_dir(keg)
    project = scratch//'/project'
    call write_text(project//'/fpm.toml', 'name = "kegs"'//nl)
    call write_text(project//'/test/check.f90', 'program check'//nl// &
        '    implicit none'//nl//'    character(len=4096) :: value'//nl// &
        '    call get_environment_variable("PKG_CONFIG_PATH", value)'//nl// &
        '    if (index(value, "/user/first:") /= 1) error stop 1'//nl// &
        '    if (index(value, "'//keg//'") == 0) error stop 2'//nl// &
        'end program check'//nl)
    call list_add(args, 'test')
    call list_add(env, 'HOMEBREW_PREFIX='//scratch//'/brew')
    call list_add(env, 'PKG_CONFIG_PATH=/user/first')
    call run_fo(driver, args, project, scratch//'/cache', result, env, timeout_ms=120000)
    call assert_process_ok(result, 'keg pkg-config directory follows the user path')
    call remove_tree(scratch)
    call finish_assertions()
end program test_pkg_config_kegs
