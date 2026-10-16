program test_git_dependency_pins_cli
    ! Tag (annotated and lightweight), branch and rev pins of a Git dependency
    ! served by a bare repository must resolve on a clean cache, and a changed
    ! pin must move the checkout on fo update.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory
    use fo_test_harness, only: write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, work, bare, consumer, cache
    character(:), allocatable :: commit2, commit3
    type(process_result_t) :: result
    type(string_list_t) :: args, environment

    call resolve_driver(driver)
    call make_scratch('fo-git-pins', scratch)
    call list_add(environment, 'GIT_CONFIG_NOSYSTEM=1')
    call list_add(environment, 'GIT_CONFIG_GLOBAL=/dev/null')
    call list_add(environment, 'GIT_TERMINAL_PROMPT=0')
    call list_add(environment, 'GIT_AUTHOR_NAME=Fo test')
    call list_add(environment, 'GIT_AUTHOR_EMAIL=fo-test@example.invalid')
    call list_add(environment, 'GIT_COMMITTER_NAME=Fo test')
    call list_add(environment, 'GIT_COMMITTER_EMAIL=fo-test@example.invalid')
    call list_add(environment, 'FO_JOBS=2')
    call make_directory(join_path(scratch, 'tmp'))
    call list_add(environment, 'TMPDIR=' // join_path(scratch, 'tmp'))
    work = join_path(scratch, 'work')
    bare = join_path(scratch, 'provider.git')
    consumer = join_path(scratch, 'consumer')
    cache = join_path(scratch, 'cache')

    call write_text(join_path(work, 'fpm.toml'), 'name = "pin_provider"' // new_line('a'))
    call git(work, 'init', '-q', '-b', 'main')
    call write_value(41)
    call git(work, 'tag', '-a', '-m', 'annotated', 'v1.0')
    call write_value(42)
    call git(work, 'tag', 'v1.1')
    commit2 = head()
    call write_value(43)
    commit3 = head()
    call git(scratch, 'clone', '-q', '--bare', work, bare)

    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe' // new_line('a') // 'use pin_provider, only: value' // &
        new_line('a') // "print '(i0)', value()" // new_line('a') // &
        'end program probe' // new_line('a'))

    ! Each pin starts from an empty dependency checkout and cache.
    call expect_pin('tag = "v1.0"', '41', 'annotated tag')
    call expect_pin('tag = "v1.1"', '42', 'lightweight tag')
    call expect_pin('branch = "main"', '43', 'branch')
    call expect_pin('rev = "' // commit2 // '"', '42', 'rev')

    ! A changed pin moves the existing checkout, on build and on update.
    call write_manifest('tag = "v1.0"')
    call fo('build')
    call write_manifest('tag = "v1.1"')
    call fo('update')
    call fo('build')
    call probe('42', 'changing tag then fo update moves the checkout')
    call write_manifest('tag = "v1.0"')
    call fo('update')
    call probe('41', 'fo update moves back to the annotated tag')
    call write_manifest('rev = "' // commit3 // '"')
    call fo('update')
    call probe('43', 'fo update resolves a rev pin')

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine expect_pin(selector, expected, label)
        character(len=*), intent(in) :: selector, expected, label

        call remove_tree(join_path(consumer, 'build'))
        call remove_tree(cache)
        call write_manifest(selector)
        call fo('build')
        call probe(expected, label // ' pin builds the pinned content on a clean cache')
    end subroutine expect_pin

    subroutine write_manifest(selector)
        character(len=*), intent(in) :: selector

        call write_text(join_path(consumer, 'fpm.toml'), &
            'name = "pin_probe"' // new_line('a') // '[dependencies]' // &
            new_line('a') // 'pin_provider = { git = "file://' // bare // '", ' // &
            selector // ' }' // new_line('a'))
    end subroutine write_manifest

    subroutine fo(command)
        character(len=*), intent(in) :: command

        args = string_list_t()
        call list_add(args, command)
        call run_fo(driver, args, consumer, cache, result, environment)
        call assert_process_ok(result, 'fo ' // command // ' with Git pin: ' // &
            result%stderr)
    end subroutine fo

    subroutine probe(expected, label)
        character(len=*), intent(in) :: expected, label

        args = string_list_t()
        call list_add(args, 'exec')
        call list_add(args, 'probe')
        call run_fo(driver, args, consumer, cache, result, environment)
        call assert_process_ok(result, label // ': ' // result%stderr)
        call assert_equal_string(result%stdout, expected // new_line('a'), label)
    end subroutine probe

    subroutine write_value(number)
        integer, intent(in) :: number
        character(len=16) :: digits

        write (digits, '(i0)') number
        call write_text(join_path(work, 'src/provider.f90'), &
            'module pin_provider' // new_line('a') // 'implicit none' // &
            new_line('a') // 'contains' // new_line('a') // &
            'integer function value()' // new_line('a') // &
            'value = ' // trim(digits) // new_line('a') // &
            'end function value' // new_line('a') // 'end module pin_provider' // &
            new_line('a'))
        call git(work, 'add', '-A')
        call git(work, 'commit', '-q', '-m', 'value ' // trim(digits))
    end subroutine write_value

    function head() result(value)
        character(:), allocatable :: value
        integer :: n

        args = string_list_t()
        call list_add(args, '-C')
        call list_add(args, work)
        call list_add(args, 'rev-parse')
        call list_add(args, 'HEAD')
        call run_external('git', args, scratch, result, environment)
        call assert_process_ok(result, 'read fixture HEAD')
        n = len_trim(result%stdout)
        value = result%stdout(:n - 1)
    end function head

    subroutine git(cwd, a, b, c, d, e)
        character(len=*), intent(in) :: cwd, a
        character(len=*), intent(in), optional :: b, c, d, e

        args = string_list_t()
        if (cwd /= scratch) then
            call list_add(args, '-C')
            call list_add(args, cwd)
        end if
        call list_add(args, a)
        if (present(b)) call list_add(args, b)
        if (present(c)) call list_add(args, c)
        if (present(d)) call list_add(args, d)
        if (present(e)) call list_add(args, e)
        call run_external('git', args, scratch, result, environment)
        call assert_process_ok(result, 'create Git fixture: ' // result%stderr)
    end subroutine git

end program test_git_dependency_pins_cli
