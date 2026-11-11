program test_compile_key_inputs_cli
    !! What an fpm compile reads besides its source and command must be in its
    !! key, or a second checkout that shares the cache gets the object of the
    !! first. Each case builds one project, then a copy that differs in one
    !! such input, with FO_CACHE_RELOCATABLE=1 and one cache; a compiler
    !! wrapper records every compilation. The copy must run what a compile
    !! of its own files gives:
    !!  - a module file lying next to a source, which gfortran reads before
    !!    the one fo built;
    !!  - a header found through CPATH, also when only the variable changes
    !!    in a built tree, and reused where header and place are equal;
    !!  - a Fortran include inside an included file, which gfortran looks up
    !!    next to the compiled source, not next to the including file, also
    !!    after an edit in place with the default key;
    !!  - options that write files beside the object or name directories in
    !!    their long spelling keep an action bound to its checkout;
    !!  - a compiler proper selected through COMPILER_PATH;
    !!  - a source that expands __TIME__ is compiled in every checkout.
    use fo_fs, only: fs_find_executable
    use fo_test_harness, only: process_result_t, string_list_t, list_add, argv
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree, set_mode
    use fo_test_harness, only: read_text, file_exists, run_process
    use fo_test_harness, only: assert_true, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, log
    character(len=4096) :: real_fc
    type(string_list_t) :: plain, relocatable
    logical :: located

    call resolve_driver(driver)
    call make_scratch('fo-compile-key-inputs', scratch)
    log = join_path(scratch, 'compiles.log')
    call fs_find_executable('gfortran', real_fc, located)
    call assert_true(located, 'gfortran is installed')
    call write_text(join_path(scratch, 'bin/gfortran'), '#!/bin/sh'//nl// &
        'for a in "$@"; do case "$a" in *fo_cap_*) ;; *.f90|*.F90) echo "$a" >> "'// &
        log//'";; esac; done'//nl//'exec "'//trim(real_fc)//'" "$@"'//nl)
    call set_mode(join_path(scratch, 'bin/gfortran'), int(o'755'))
    call list_add(plain, 'FO_JOBS=2')
    call list_add(plain, 'FO_FC='//join_path(scratch, 'bin/gfortran'))
    relocatable = plain
    call list_add(plain, 'FO_CACHE_RELOCATABLE=0')
    call list_add(relocatable, 'FO_CACHE_RELOCATABLE=1')

    call module_file_beside_a_source()
    call header_from_the_environment()
    call include_inside_an_include()
    call options_with_side_files_stay_bound()
    call long_include_option_stays_bound()
    call compiler_proper_from_the_environment()
    call time_macro_compiles_everywhere()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine fo(root, words, environment, result, cache)
        !! Run fo with blank-separated words in root and count its compilations.
        character(len=*), intent(in) :: root, words, cache
        type(string_list_t), intent(in) :: environment
        type(process_result_t), intent(out) :: result
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
        call run_fo(driver, args, root, join_path(scratch, cache), result, environment, &
            120000)
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

    subroutine write_program(root, name, source, body)
        !! app/<source> prints `value=` and what the line(s) body assign.
        character(len=*), intent(in) :: root, name, source, body

        call write_text(join_path(root, 'fpm.toml'), 'name = "'//name//'"'//nl)
        call write_text(join_path(root, 'app/'//source), 'program main'//nl// &
            '    implicit none'//nl//'    print ''(a,i0)'', ''value='', value()'//nl// &
            'contains'//nl//'    integer function value()'//nl//body//nl// &
            '    end function'//nl//'end program'//nl)
    end subroutine write_program

    subroutine module_file_beside_a_source()
        character(:), allocatable :: dirty, clean, stale
        type(process_result_t) :: result

        dirty = join_path(scratch, 'stray-dirty')
        clean = join_path(scratch, 'stray-clean-copy')
        call write_text(join_path(dirty, 'src/kmod.f90'), kmod('1'))
        call write_text(join_path(clean, 'src/kmod.f90'), kmod('1'))
        call write_consumer(dirty)
        call write_consumer(clean)
        ! An outdated kmod.mod as an in-source build or an editor leaves it.
        stale = join_path(scratch, 'stale/kmod.f90')
        call write_text(stale, kmod('2'))
        call run_process(argv(trim(real_fc), '-fsyntax-only', stale), &
            join_path(dirty, 'src'), result)
        call assert_true(file_exists(join_path(dirty, 'src/kmod.mod')), &
            'the outdated module file lies next to the sources: '//result%stderr)

        call fo(dirty, 'exec stray', relocatable, result, 'cache-stray')
        call assert_contains(result%stdout, 'value=2', &
            'gfortran reads the module file next to the source first')
        call fo(clean, 'exec stray', relocatable, result, 'cache-stray')
        call assert_contains(read_text(log), 'src/m.f90', &
            'a clean copy compiles the consumer of the module itself')
        call assert_contains(result%stdout, 'value=1', &
            'and runs what its own sources say')
    end subroutine module_file_beside_a_source

    function kmod(k) result(text)
        character(len=*), intent(in) :: k
        character(:), allocatable :: text

        text = 'module kmod'//nl//'    implicit none'//nl// &
            '    integer, parameter :: k = '//k//nl//'end module'//nl
    end function kmod

    subroutine write_consumer(root)
        character(len=*), intent(in) :: root

        call write_text(join_path(root, 'fpm.toml'), 'name = "stray"'//nl)
        call write_text(join_path(root, 'src/m.f90'), 'module m'//nl// &
            '    use kmod, only: k'//nl//'    implicit none'//nl//'contains'//nl// &
            '    integer function answer()'//nl//'        answer = k'//nl// &
            '    end function'//nl//'end module'//nl)
        call write_text(join_path(root, 'app/main.f90'), 'program main'//nl// &
            '    use m, only: answer'//nl// &
            '    print ''(a,i0)'', ''value='', answer()'//nl//'end program'//nl)
    end subroutine write_consumer

    subroutine header_from_the_environment()
        character(len=*), parameter :: body = '#include <site_config.h>'//nl// &
            '        value = SITE_K'
        character(:), allocatable :: first, second, third
        type(string_list_t) :: site_one, site_two
        type(process_result_t) :: result

        first = join_path(scratch, 'site-a')
        second = join_path(scratch, 'site-b-copy')
        third = join_path(scratch, 'site-c')
        call write_program(first, 'site', 'main.F90', body)
        call write_program(second, 'site', 'main.F90', body)
        call write_program(third, 'site', 'main.F90', body)
        call write_text(join_path(scratch, 'one/site_config.h'), '#define SITE_K 1'//nl)
        call write_text(join_path(scratch, 'two/site_config.h'), '#define SITE_K 2'//nl)
        site_one = relocatable
        call list_add(site_one, 'CPATH='//join_path(scratch, 'one'))
        site_two = relocatable
        call list_add(site_two, 'CPATH='//join_path(scratch, 'two'))

        call fo(first, 'exec site', site_one, result, 'cache-site')
        call assert_contains(result%stdout, 'value=1', 'the header on CPATH is used')
        call fo(second, 'exec site', site_two, result, 'cache-site')
        call assert_contains(result%stdout, 'value=2', &
            'a copy that finds another header through CPATH compiles with it')
        call fo(third, 'exec site', site_two, result, 'cache-site')
        call assert_equal_integer(compiles(), 0, &
            'the same header at the same place is reused: '//read_text(log))
        call assert_contains(result%stdout, 'value=2', 'with the right result')
        call fo(first, 'exec site', site_two, result, 'cache-site')
        call assert_contains(result%stdout, 'value=2', &
            'a built tree follows a changed CPATH')
    end subroutine header_from_the_environment

    subroutine write_nested(root, inner)
        !! main.f90 includes sub/a.inc, which includes b.inc: gfortran reads
        !! app/b.inc, although app/sub/b.inc lies next to the including file.
        character(len=*), intent(in) :: root, inner

        call write_program(root, 'nested', 'main.f90', '        include ''sub/a.inc''')
        call write_text(join_path(root, 'app/sub/a.inc'), 'include ''b.inc'''//nl)
        call write_text(join_path(root, 'app/sub/b.inc'), 'value = 7'//nl)
        call write_text(join_path(root, 'app/b.inc'), 'value = '//inner//nl)
    end subroutine write_nested

    subroutine include_inside_an_include()
        character(:), allocatable :: first, second, third
        type(process_result_t) :: result

        first = join_path(scratch, 'nested-a')
        second = join_path(scratch, 'nested-b-copy')
        third = join_path(scratch, 'nested-c')
        call write_nested(first, '1')
        call write_nested(second, '2')
        call write_nested(third, '2')
        call fo(first, 'exec nested', relocatable, result, 'cache-nested')
        call assert_contains(result%stdout, 'value=1', &
            'gfortran reads the nested include next to the compiled source')
        call fo(second, 'exec nested', relocatable, result, 'cache-nested')
        call assert_contains(result%stdout, 'value=2', &
            'a copy with another nested include compiles with it')

        ! The same input with the default key, edited in place.
        call fo(third, 'exec nested', plain, result, 'cache-nested')
        call assert_contains(result%stdout, 'value=2', 'the default key builds the same')
        call write_text(join_path(third, 'app/b.inc'), 'value = 3'//nl)
        call fo(third, 'exec nested', plain, result, 'cache-nested')
        call assert_contains(result%stdout, 'value=3', &
            'an edited nested include compiles its source again')
    end subroutine include_inside_an_include

    subroutine options_with_side_files_stay_bound()
        character(:), allocatable :: first, second, probe
        type(process_result_t) :: result

        first = join_path(scratch, 'side-a')
        second = join_path(scratch, 'side-b-copy')
        call write_program(first, 'side', 'main.f90', '        value = 1')
        call write_program(second, 'side', 'main.f90', '        value = 1')
        ! The stack usage file is written beside the object, not into the cache.
        call fo(first, 'build --flag -fstack-usage', relocatable, result, 'cache-side')
        call fo(second, 'build --flag -fstack-usage', relocatable, result, 'cache-side')
        call assert_equal_integer(compiles(), 1, &
            'a copy built with -fstack-usage compiles again')
        call assert_true(file_exists(join_path(second, 'build/fo/obj/app_main.f90.su')), &
            'and has its stack usage file')

        ! Condition coverage (GCC 14) writes the path of its counters into
        ! the object, like --coverage.
        probe = join_path(scratch, 'probe/probe.f90')
        call write_text(probe, 'end'//nl)
        call run_process(argv(trim(real_fc), '-fcondition-coverage', '-fsyntax-only', &
            probe), join_path(scratch, 'probe'), result)
        if (result%exit_code /= 0) return
        call fo(first, 'build --flag -fcondition-coverage', relocatable, result, 'cache-side')
        call fo(second, 'build --flag -fcondition-coverage', relocatable, result, &
            'cache-side')
        call assert_equal_integer(compiles(), 1, &
            'a copy built with -fcondition-coverage compiles again')
        call assert_not_contains(read_text(join_path(second, &
            'build/fo/obj/app_main.f90.o')), first, &
            'and its object does not count into the first checkout')
    end subroutine options_with_side_files_stay_bound

    subroutine long_include_option_stays_bound()
        character(len=*), parameter :: body = '#include "outside.inc"'
        character(:), allocatable :: first, second
        type(process_result_t) :: result

        first = join_path(scratch, 'long-a/project')
        second = join_path(scratch, 'long-b/project')
        call write_program(first, 'long', 'main.F90', body)
        call write_program(second, 'long', 'main.F90', body)
        call write_text(join_path(scratch, 'long-a/shared/outside.inc'), &
            '        value = 1'//nl)
        call write_text(join_path(scratch, 'long-b/shared/outside.inc'), &
            '        value = 2'//nl)
        call fo(first, 'exec --flag --include-directory=../shared long', relocatable, &
            result, 'cache-long')
        call assert_contains(result%stdout, 'value=1', 'the long option is an include path')
        call fo(second, 'exec --flag --include-directory=../shared long', relocatable, &
            result, 'cache-long')
        call assert_contains(result%stdout, 'value=2', &
            'a copy reads its own directory behind --include-directory')
    end subroutine long_include_option_stays_bound

    subroutine compiler_proper_from_the_environment()
        character(len=*), parameter :: body = '        value = kind(1.0)'
        character(:), allocatable :: first, second, proper, tools
        type(string_list_t) :: other_proper
        type(process_result_t) :: result

        call run_process(argv(trim(real_fc), '-print-prog-name=f951'), scratch, result)
        proper = result%stdout
        if (index(proper, nl) > 0) proper = proper(:index(proper, nl) - 1)
        if (len(proper) == 0) return
        if (proper(1:1) /= '/' .or. .not. file_exists(proper)) return
        ! The same driver, but a compiler proper with another default real.
        tools = join_path(scratch, 'tools')
        call write_text(join_path(tools, 'f951'), '#!/bin/sh'//nl//'exec "'//proper// &
            '" "$@" -fdefault-real-8'//nl)
        call set_mode(join_path(tools, 'f951'), int(o'755'))
        other_proper = relocatable
        call list_add(other_proper, 'COMPILER_PATH='//tools)

        first = join_path(scratch, 'proper-a')
        second = join_path(scratch, 'proper-b-copy')
        call write_program(first, 'proper', 'main.f90', body)
        call write_program(second, 'proper', 'main.f90', body)
        call fo(first, 'exec proper', relocatable, result, 'cache-proper')
        call assert_contains(result%stdout, 'value=4', 'the default real has kind 4')
        call fo(second, 'exec proper', other_proper, result, 'cache-proper')
        call assert_contains(result%stdout, 'value=8', &
            'a copy compiled by another compiler proper is its own result')
    end subroutine compiler_proper_from_the_environment

    subroutine time_macro_compiles_everywhere()
        character(len=*), parameter :: body = '        value = len(__TIME__)'
        character(:), allocatable :: first, second
        type(process_result_t) :: result

        first = join_path(scratch, 'time-a')
        second = join_path(scratch, 'time-b-copy')
        call write_program(first, 'time', 'main.F90', body)
        call write_program(second, 'time', 'main.F90', body)
        call fo(first, 'build', relocatable, result, 'cache-time')
        call fo(second, 'exec time', relocatable, result, 'cache-time')
        call assert_equal_integer(compiles(), 1, &
            'a source with __TIME__ is compiled in the copy, not replayed')
        call assert_contains(result%stdout, 'value=8', 'and runs')
    end subroutine time_macro_compiles_everywhere

end program test_compile_key_inputs_cli
