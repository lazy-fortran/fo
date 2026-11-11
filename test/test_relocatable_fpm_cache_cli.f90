program test_relocatable_fpm_cache_cli
    !! FO_CACHE_RELOCATABLE=1 for fpm projects, against one local cache shared
    !! by several directories and a compiler wrapper that records every
    !! compilation:
    !!  - a copy of a project at another path compiles nothing, its restored
    !!    objects link and run, and they equal objects compiled independently
    !!    at a third path; __FILE__ is the name relative to the project root;
    !!  - a changed flag, source body, header content or source name compiles
    !!    again, and so does a header of the same name that another include
    !!    directory provides, also when its content is equal;
    !!  - debug builds are shared too, their objects name no checkout, and a
    !!    build through a symbolic link gives the same objects;
    !!  - coverage builds, user include options and other compilers stay bound
    !!    to their checkout;
    !!  - a fetched dependency and a path dependency outside the project are
    !!    reused by a moved consumer, a moved dependency is compiled again.
    use fo_fs, only: fs_find_executable
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree, set_mode
    use fo_test_harness, only: make_symlink, move_path, read_text, file_exists, run_git
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(len=*), parameter :: objects = 'build/fo/obj/'
    character(:), allocatable :: driver, scratch, cache, log
    character(len=4096) :: real_fc
    type(string_list_t) :: plain, relocatable
    logical :: located

    call resolve_driver(driver)
    call make_scratch('fo-relocatable-fpm', scratch)
    cache = join_path(scratch, 'cache')
    log = join_path(scratch, 'compiles.log')
    call fs_find_executable('gfortran', real_fc, located)
    call assert_true(located, 'gfortran is installed')
    call write_recording_compiler(join_path(scratch, 'bin/gfortran'), trim(real_fc))
    call list_add(plain, 'FO_JOBS=2')
    call list_add(plain, 'FO_FC='//join_path(scratch, 'bin/gfortran'))
    relocatable = plain
    call list_add(relocatable, 'FO_CACHE_RELOCATABLE=1')

    call copy_reuses_results()
    call changed_inputs_compile()
    call include_directories_count()
    call debug_builds_name_no_checkout()
    call bound_builds_stay_bound()
    call dependencies_follow_their_location()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine write_recording_compiler(path, compiler)
        !! One line per compiled source; fo's own capability probes are not
        !! project compilations.
        character(len=*), intent(in) :: path, compiler

        call write_text(path, '#!/bin/sh'//nl//'for a in "$@"; do case "$a" in '// &
            '*fo_cap_*) ;; *.f90|*.F90) echo "$a" >> "'//log//'";; esac; done'//nl// &
            'exec "'//compiler//'" "$@"'//nl)
        call set_mode(path, int(o'755'))
    end subroutine write_recording_compiler

    subroutine write_project(root, answer, header)
        character(len=*), intent(in) :: root, answer, header

        call write_text(join_path(root, 'fpm.toml'), 'name = "reloc"'//nl)
        call write_text(join_path(root, 'src/m.F90'), 'module m'//nl// &
            '    implicit none'//nl//'contains'//nl// &
            '    integer function answer()'//nl//'        answer = '//answer//nl// &
            '    end function'//nl//'    subroutine names()'//nl// &
            '        print ''(a)'', __FILE__'//nl//'#include "probe.inc"'//nl// &
            '    end subroutine'//nl//'end module'//nl)
        call write_text(join_path(root, 'include/probe.inc'), header//nl)
        call write_text(join_path(root, 'app/main.f90'), 'program main'//nl// &
            '    use m, only: answer, names'//nl//'    print ''(i0)'', answer()'//nl// &
            '    call names()'//nl//'end program'//nl)
    end subroutine write_project

    subroutine fo(root, words, environment, result, cache_directory)
        !! Run fo with blank-separated words in root and count its compilations.
        character(len=*), intent(in) :: root, words
        type(string_list_t), intent(in) :: environment
        type(process_result_t), intent(out) :: result
        character(len=*), intent(in), optional :: cache_directory
        type(string_list_t) :: args
        integer :: start, blank

        start = 1
        do
            blank = index(words(start:), ' ')
            if (blank == 0) exit
            call list_add(args, words(start:start + blank - 2))
            start = start + blank
        end do
        call list_add(args, words(start:))
        call write_text(log, '')
        if (present(cache_directory)) then
            call run_fo(driver, args, root, cache_directory, result, environment, 120000)
        else
            call run_fo(driver, args, root, cache, result, environment, 120000)
        end if
        call assert_equal_integer(result%exit_code, 0, &
            'fo '//words//' in '//root//': '//result%stdout//result%stderr)
    end subroutine fo

    integer function compiles()
        character(:), allocatable :: text
        integer :: i

        text = read_text(log)
        compiles = 0
        do i = 1, len(text)
            if (text(i:i) == nl) compiles = compiles + 1
        end do
    end function compiles

    subroutine copy_reuses_results()
        character(:), allocatable :: first, second, third, bound
        type(process_result_t) :: result

        first = join_path(scratch, 'first')
        ! Paths of other lengths: the key must not depend on them.
        second = join_path(scratch, 'second-longer-copy')
        third = join_path(scratch, 'x')
        bound = join_path(scratch, 'bound-copy')
        call write_project(first, '42', 'print ''(a)'', __FILE__')
        call write_project(second, '42', 'print ''(a)'', __FILE__')
        call write_project(third, '42', 'print ''(a)'', __FILE__')
        call write_project(bound, '42', 'print ''(a)'', __FILE__')

        call fo(first, 'build', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'the first build compiles both units')
        call fo(second, 'exec reloc', relocatable, result)
        call assert_equal_integer(compiles(), 0, &
            'a copy at another path compiles nothing: '//read_text(log))
        call assert_contains(result%stdout, '42'//nl//'src/m.F90'//nl// &
            'include/probe.inc'//nl, &
            'the program linked from restored objects runs and names its '// &
            'files relative to the project root')
        call assert_not_contains(result%stdout, scratch, 'no checkout path is printed')

        ! Independent oracle: the same sources compiled at a third path with
        ! an empty cache.
        call fo(third, 'build', relocatable, result, join_path(scratch, 'cache-third'))
        call assert_equal_integer(compiles(), 2, 'an empty cache compiles both units')
        call assert_equal_string(read_text(join_path(second, objects//'src_m.F90.o')), &
            read_text(join_path(third, objects//'src_m.F90.o')), &
            'a restored object equals one compiled at another path')
        call assert_equal_string(read_text(join_path(second, objects//'app_main.f90.o')), &
            read_text(join_path(third, objects//'app_main.f90.o')), &
            'a restored program object equals one compiled at another path')
        call assert_not_contains(read_text(join_path(third, objects//'src_m.F90.o')), &
            scratch, 'the object names no checkout')

        call fo(bound, 'exec reloc', plain, result)
        call assert_equal_integer(compiles(), 2, &
            'without FO_CACHE_RELOCATABLE a copy compiles at its own location')
        call assert_contains(result%stdout, join_path(bound, 'src/m.F90'), &
            'and __FILE__ stays the absolute name')
    end subroutine copy_reuses_results

    subroutine changed_inputs_compile()
        character(:), allocatable :: second
        type(process_result_t) :: result

        second = join_path(scratch, 'second-longer-copy')
        call fo(second, 'build --flag -O1', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'another flag compiles both units')

        call write_project(second, '43', 'print ''(a)'', __FILE__')
        call fo(second, 'exec reloc', relocatable, result)
        call assert_equal_integer(compiles(), 1, 'a body edit compiles the edited unit')
        call assert_contains(read_text(log), 'src/m.F90', 'the edited unit is compiled')
        call assert_contains(result%stdout, '43'//nl, 'the edit reaches the program')

        call write_project(second, '43', 'print ''(a)'', ''edited header''')
        call fo(second, 'exec reloc', relocatable, result)
        call assert_equal_integer(compiles(), 1, 'a header edit compiles its includer')
        call assert_contains(result%stdout, 'edited header', 'the header reaches the program')

        call move_path(join_path(second, 'app/main.f90'), join_path(second, 'app/start.f90'))
        call fo(second, 'build', relocatable, result)
        call assert_equal_integer(compiles(), 1, 'a renamed source is another action')
        call assert_contains(read_text(log), 'app/start.f90', 'the renamed unit is compiled')
    end subroutine changed_inputs_compile

    subroutine write_include_project(root, order, one, two)
        !! app/main.F90 includes probe.inc. two/ always provides it, with the
        !! line `two`; one/ provides it with the line `one` unless that is empty.
        character(len=*), intent(in) :: root, order, one, two

        call write_text(join_path(root, 'fpm.toml'), 'name = "incs"'//nl// &
            '[library]'//nl//'include-dir = ['//order//']'//nl)
        call write_text(join_path(root, 'one/unrelated.inc'), '! not included'//nl)
        if (len(one) > 0) call write_text(join_path(root, 'one/probe.inc'), one//nl)
        call write_text(join_path(root, 'two/probe.inc'), two//nl)
        call write_text(join_path(root, 'app/main.F90'), 'program main'//nl// &
            '#include "probe.inc"'//nl//'end program'//nl)
    end subroutine write_include_project

    subroutine include_directories_count()
        character(len=*), parameter :: from_one = 'print ''(a)'', ''from one'''
        character(len=*), parameter :: from_two = 'print ''(a)'', ''from two'''
        character(len=*), parameter :: own_name = 'print ''(a)'', __FILE__'
        character(:), allocatable :: root
        type(process_result_t) :: result

        root = join_path(scratch, 'include-a')
        call write_include_project(root, '"one", "two"', '', from_two)
        call fo(root, 'exec incs', relocatable, result)
        call assert_equal_integer(compiles(), 1, 'the include project compiles once')
        call assert_contains(result%stdout, 'from two', 'the only header is used')

        ! The same options, but the first directory now provides the name.
        root = join_path(scratch, 'include-b-copy')
        call write_include_project(root, '"one", "two"', from_one, from_two)
        call fo(root, 'exec incs', relocatable, result)
        call assert_equal_integer(compiles(), 1, &
            'a header of the same name in an earlier directory compiles again')
        call assert_contains(result%stdout, 'from one', 'the earlier directory wins')

        ! The same files, searched in the other order.
        root = join_path(scratch, 'include-c')
        call write_include_project(root, '"two", "one"', from_one, from_two)
        call fo(root, 'exec incs', relocatable, result)
        call assert_equal_integer(compiles(), 1, 'another search order compiles again')
        call assert_contains(result%stdout, 'from two', 'the search order decides')

        root = join_path(scratch, 'include-d-same-as-b')
        call write_include_project(root, '"one", "two"', from_one, from_two)
        call fo(root, 'exec incs', relocatable, result)
        call assert_equal_integer(compiles(), 0, &
            'equal headers in equal order are reused: '//read_text(log))
        call assert_contains(result%stdout, 'from one', 'the reused result is the right one')

        ! Equal content from another directory is another file to the compiler.
        root = join_path(scratch, 'include-e')
        call write_include_project(root, '"one", "two"', own_name, own_name)
        call fo(root, 'exec incs', relocatable, result)
        call assert_contains(result%stdout, 'one/probe.inc', 'the header names itself')
        root = join_path(scratch, 'include-f')
        call write_include_project(root, '"one", "two"', '', own_name)
        call fo(root, 'exec incs', relocatable, result)
        call assert_equal_integer(compiles(), 1, &
            'an equal header that another directory provides compiles again')
        call assert_contains(result%stdout, 'two/probe.inc', 'and names that directory')
    end subroutine include_directories_count

    subroutine debug_builds_name_no_checkout()
        character(:), allocatable :: first, second, link, object
        type(string_list_t) :: through_link
        type(process_result_t) :: result

        first = join_path(scratch, 'debug-a')
        second = join_path(scratch, 'debug-b-longer')
        link = join_path(scratch, 'debug-link')
        call write_project(first, '42', 'print ''(a)'', __FILE__')
        call write_project(second, '42', 'print ''(a)'', __FILE__')

        call fo(first, 'build --debug', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'the first debug build compiles both units')
        object = read_text(join_path(first, objects//'src_m.F90.o'))
        call assert_contains(object, 'debug_info', 'the object carries debug information')
        call assert_contains(object, 'src/m.F90', 'which names the source relative to the root')
        call assert_not_contains(object, scratch, 'and no checkout')

        call fo(second, 'exec --debug reloc', relocatable, result)
        call assert_equal_integer(compiles(), 0, &
            'a copy of a debug build compiles nothing: '//read_text(log))
        call assert_contains(result%stdout, '42'//nl//'src/m.F90', 'the debug program runs')

        ! The compiler takes its directory from PWD, here the link's spelling.
        call remove_tree(join_path(first, 'build'))
        call make_symlink(first, link)
        through_link = relocatable
        call list_add(through_link, 'PWD='//link)
        call fo(link, 'build --debug', through_link, result, join_path(scratch, 'cache-link'))
        call assert_equal_integer(compiles(), 2, 'an empty cache compiles through the link')
        call assert_equal_string(read_text(join_path(first, objects//'src_m.F90.o')), object, &
            'a debug object compiled through a symbolic link is the same')
    end subroutine debug_builds_name_no_checkout

    subroutine bound_builds_stay_bound()
        character(:), allocatable :: first, second
        character(len=4096) :: flang
        type(string_list_t) :: other_compiler
        type(process_result_t) :: result

        first = join_path(scratch, 'bound-a')
        second = join_path(scratch, 'bound-b-longer')
        call write_project(first, '42', 'print ''(a)'', __FILE__')
        call write_project(second, '42', 'print ''(a)'', __FILE__')

        ! A coverage object writes its counters to an absolute path.
        call fo(first, 'build --flag --coverage', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'the first coverage build compiles')
        call fo(second, 'exec --flag --coverage reloc', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'a copy of a coverage build compiles again')
        call assert_contains(result%stdout, '42'//nl, 'the coverage program runs')
        call assert_true(file_exists(join_path(second, objects//'src_m.F90.gcda')), &
            'and counts into its own checkout')
        call assert_true(.not. file_exists(join_path(first, objects//'src_m.F90.gcda')), &
            'not into the one built first')

        ! Headers behind a user include option are not keyed by content.
        call fo(first, 'build --flag -Iinclude', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'the first build with -I compiles')
        call fo(second, 'build --flag -Iinclude', relocatable, result)
        call assert_equal_integer(compiles(), 2, 'a copy with a user -I compiles again')

        call fs_find_executable('flang', flang, located)
        if (.not. located) return
        ! flang embeds the absolute source path whatever it is passed.
        call write_recording_compiler(join_path(scratch, 'other/flang'), trim(flang))
        call list_add(other_compiler, 'FO_JOBS=2')
        call list_add(other_compiler, 'FO_FC='//join_path(scratch, 'other/flang'))
        call list_add(other_compiler, 'FO_CACHE_RELOCATABLE=1')
        call fo(first, 'build', other_compiler, result)
        call assert_equal_integer(compiles(), 2, 'the first flang build compiles')
        call fo(second, 'build', other_compiler, result)
        call assert_equal_integer(compiles(), 2, 'a copy of a flang build compiles again')
    end subroutine bound_builds_stay_bound

    subroutine write_library(root, name)
        character(len=*), intent(in) :: root, name

        call write_text(join_path(root, 'fpm.toml'), 'name = "'//name//'"'//nl)
        call write_text(join_path(root, 'src/'//name//'.f90'), 'module '//name//nl// &
            '    implicit none'//nl//'contains'//nl// &
            '    integer function '//name//'_value()'//nl// &
            '        '//name//'_value = 20'//nl//'    end function'//nl// &
            'end module'//nl)
    end subroutine write_library

    subroutine write_consumer(root, fetched, outside)
        character(len=*), intent(in) :: root, fetched, outside

        call write_text(join_path(root, 'fpm.toml'), 'name = "consumer"'//nl// &
            '[dependencies]'//nl//'fetched = { git = "file://'//fetched// &
            '", tag = "v1" }'//nl//'outside = { path = "'//outside//'" }'//nl)
        call write_text(join_path(root, 'app/main.f90'), 'program main'//nl// &
            '    use fetched, only: fetched_value'//nl// &
            '    use outside, only: outside_value'//nl// &
            '    print ''(i0)'', fetched_value() + outside_value()'//nl// &
            'end program'//nl)
    end subroutine write_consumer

    subroutine dependencies_follow_their_location()
        character(:), allocatable :: fetched, outside, moved, root
        type(string_list_t) :: environment
        type(process_result_t) :: result
        integer :: code

        fetched = join_path(scratch, 'fetched')
        outside = join_path(scratch, 'outside')
        moved = join_path(scratch, 'moved/outside')
        call write_library(fetched, 'fetched')
        call write_library(outside, 'outside')
        call write_library(moved, 'outside')
        call run_git(fetched, [character(len=32) :: 'init', '-q', '-b', 'main'], code)
        call run_git(fetched, [character(len=32) :: 'add', 'fpm.toml', &
            'src/fetched.f90'], code)
        call run_git(fetched, [character(len=32) :: 'commit', '-q', '-m', 'fetched'], code)
        call run_git(fetched, [character(len=32) :: 'tag', '--no-sign', '--no-annotate', &
            'v1'], code)
        call assert_equal_integer(code, 0, 'the dependency repository is tagged')
        environment = relocatable
        call list_add(environment, 'GIT_CONFIG_NOSYSTEM=1')
        call list_add(environment, 'GIT_CONFIG_GLOBAL=/dev/null')
        call list_add(environment, 'GIT_TERMINAL_PROMPT=0')

        root = join_path(scratch, 'consumer-a')
        call write_consumer(root, fetched, outside)
        call fo(root, 'build', environment, result)
        call assert_equal_integer(compiles(), 3, 'consumer and both dependencies compile')
        call assert_contains(read_text(log), 'build/dependencies/fetched/src/fetched.f90'//nl, &
            'the fetched dependency is compiled')
        call assert_not_contains(read_text(log), join_path(root, 'build'), &
            'by its name below the project root')
        call assert_contains(read_text(log), join_path(outside, 'src/outside.f90'), &
            'the dependency outside the project by its own location')

        root = join_path(scratch, 'consumer-b-longer')
        call write_consumer(root, fetched, outside)
        call fo(root, 'exec consumer', environment, result)
        call assert_equal_integer(compiles(), 0, &
            'a moved consumer reuses both dependencies: '//read_text(log))
        call assert_contains(result%stdout, '40', 'the consumer runs')

        root = join_path(scratch, 'consumer-c')
        call write_consumer(root, fetched, moved)
        call fo(root, 'build', environment, result)
        call assert_equal_integer(compiles(), 1, &
            'a moved dependency is compiled at its new location: '//read_text(log))
        call assert_contains(read_text(log), join_path(moved, 'src/outside.f90'), &
            'and only that dependency')
    end subroutine dependencies_follow_their_location

end program test_relocatable_fpm_cache_cli
