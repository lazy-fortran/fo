program test_build_as_init
    !! fo builds when it is process 1, as the entry point of a container: the
    !! monitor that starts each compiler accepts its owner even when that is
    !! process 1. Runs fo as the first process of a new PID namespace.
    use fo_test_harness, only: make_scratch, join_path, remove_tree, write_text, &
        environment_value, assert_true, finish_assertions
    implicit none

    character(:), allocatable :: fo, scratch
    integer :: stat

    fo = environment_value('FO_BIN')
    call execute_command_line('unshare -Urpf --mount-proc true >/dev/null 2>&1', exitstat=stat)
    if (stat /= 0) then
        print '(a)', 'SKIP: no unprivileged PID namespaces on this host'
        stop
    end if
    call make_scratch('fo-build-as-init', scratch)
    call write_text(join_path(scratch, 'fpm.toml'), 'name = "init"'//new_line('a'))
    call write_text(join_path(scratch, 'app/main.f90'), &
        'program main'//new_line('a')//'print *, 1'//new_line('a')//'end program main'// &
        new_line('a'))
    call execute_command_line('cd '''//scratch//''' && unshare -Urpf --mount-proc sh -c ''exec "$0" build'' '''// &
        fo//''' >/dev/null 2>&1', exitstat=stat)
    call assert_true(stat == 0, 'fo builds as process 1 of a PID namespace')
    call remove_tree(scratch)
    call finish_assertions()
end program test_build_as_init
