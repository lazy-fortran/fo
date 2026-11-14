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
    !!    in a built tree, and reused where header and place are equal; such
    !!    a header is not cached when gfortran does not tell its search list;
    !!  - a Fortran include inside an included file, which gfortran looks up
    !!    next to the compiled source, not next to the including file, also
    !!    after an edit in place with the default key;
    !!  - options that write files beside the object or name directories in
    !!    their long spelling keep an action bound to its checkout;
    !!  - a compiler proper selected through COMPILER_PATH;
    !!  - a source that expands __TIME__ is compiled in every checkout;
    !!  - a module file that appears next to a source between two builds of
    !!    one resident MCP server;
    !!  - a module no source provides that gfortran finds in the module
    !!    directory;
    !!  - headers reached through #include_next, #import and the has-include
    !!    operator of #if;
    !!  - include lines behind the OpenMP sentinel `!$` and fixed-form ones
    !!    with blanks inside the keyword;
    !!  - the header the compiler driver pre-includes into every compile.
    use fo_fs, only: fs_find_executable
    use fo_test_harness, only: process_result_t, string_list_t, list_add, argv
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree, set_mode
    use fo_test_harness, only: remove_path
    use fo_test_harness, only: read_text, file_exists, run_process
    use fo_test_harness, only: assert_true, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_mcp, only: mcp_quote
    use fo_test_mcp_session, only: mcp_session_t, mcp_session_start, &
        mcp_session_request, mcp_session_notify, mcp_session_call, mcp_session_shutdown
    use fo_test_json, only: json_value_t
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
    call header_without_a_search_list()
    call include_inside_an_include()
    call options_with_side_files_stay_bound()
    call long_include_option_stays_bound()
    call compiler_proper_from_the_environment()
    call time_macro_compiles_everywhere()
    call module_file_between_resident_builds()
    call module_file_in_the_module_directory()
    call header_through_include_next()
    call header_through_import_and_probe()
    call include_spellings_beyond_the_plain_line()
    call header_the_driver_preincludes()

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine fo(root, words, environment, result, cache, may_fail)
        !! Run fo with blank-separated words in root and count its compilations.
        character(len=*), intent(in) :: root, words, cache
        type(string_list_t), intent(in) :: environment
        type(process_result_t), intent(out) :: result
        logical, intent(in), optional :: may_fail
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
        if (present(may_fail)) then
            if (may_fail) return
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

    subroutine header_without_a_search_list()
        !! A compiler that does not list its search directories: the header
        !! found through CPATH cannot be keyed, so the action is not cached.
        character(len=*), parameter :: body = '#include <site_config.h>'//nl// &
            '        value = SITE_K'
        character(:), allocatable :: first, second, quiet
        type(string_list_t) :: site_one, site_two
        type(process_result_t) :: result

        quiet = join_path(scratch, 'quiet/gfortran')
        call write_text(quiet, '#!/bin/sh'//nl// &
            'for a in "$@"; do [ "$a" = -E ] && exit 1; done'//nl// &
            'exec "'//join_path(scratch, 'bin/gfortran')//'" "$@"'//nl)
        call set_mode(quiet, int(o'755'))
        first = join_path(scratch, 'quiet-a')
        second = join_path(scratch, 'quiet-b-copy')
        call write_program(first, 'site', 'main.F90', body)
        call write_program(second, 'site', 'main.F90', body)
        call list_add(site_one, 'FO_JOBS=2')
        call list_add(site_one, 'FO_FC='//quiet)
        call list_add(site_one, 'FO_CACHE_RELOCATABLE=1')
        site_two = site_one
        call list_add(site_one, 'CPATH='//join_path(scratch, 'one'))
        call list_add(site_two, 'CPATH='//join_path(scratch, 'two'))

        call fo(first, 'exec site', site_one, result, 'cache-quiet')
        call assert_contains(result%stdout, 'value=1', 'the header on CPATH is used')
        call fo(second, 'exec site', site_two, result, 'cache-quiet')
        call assert_contains(result%stdout, 'value=2', &
            'without the search list a copy compiles with its own header')
    end subroutine header_without_a_search_list

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
        ! A relocated toolchain finds its own f951 before COMPILER_PATH.
        call run_process(argv('/usr/bin/env', 'COMPILER_PATH='//tools, trim(real_fc), &
            '-print-prog-name=f951'), scratch, result)
        if (index(result%stdout, join_path(tools, 'f951')) /= 1) return
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

    subroutine module_file_between_resident_builds()
        !! The MCP server builds in one process. A module file that appears
        !! next to the sources after its first build must still reach the key
        !! of its second, or the object compiled against that file is stored
        !! under the key of a clean tree.
        character(:), allocatable :: root, cache, stale
        type(mcp_session_t) :: server
        type(json_value_t) :: response
        type(process_result_t) :: result
        type(string_list_t) :: environment
        integer :: exit_code

        root = join_path(scratch, 'resident')
        cache = join_path(scratch, 'cache-resident')
        call write_text(join_path(root, 'src/kmod.f90'), kmod('1'))
        call write_consumer(root)
        ! Only library sources, all in one directory, while the server builds.
        call remove_tree(join_path(root, 'app'))
        call mcp_session_start(server, driver, root, cache, &
            join_path(scratch, 'resident-state'), join_path(scratch, 'resident.stderr'))
        call mcp_session_request(server, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{},'// &
            '"clientInfo":{"name":"fo-key-inputs-test","version":"1"}}', response)
        call mcp_session_notify(server, 'notifications/initialized')
        call mcp_session_call(server, '{"action":"build","dir":'//mcp_quote(root)//'}', &
            response, 120000)

        stale = join_path(scratch, 'resident-stale/kmod.f90')
        call write_text(stale, kmod('2'))
        call run_process(argv(trim(real_fc), '-fsyntax-only', stale), &
            join_path(root, 'src'), result)
        call assert_true(file_exists(join_path(root, 'src/kmod.mod')), &
            'an outdated module file appears next to the sources: '//result%stderr)
        call write_text(join_path(root, 'src/m.f90'), read_text(join_path(root, &
            'src/m.f90'))//'! edited'//nl)
        call mcp_session_call(server, '{"action":"build","dir":'//mcp_quote(root)//'}', &
            response, 120000)
        call mcp_session_shutdown(server, exit_code)

        ! The same tree without that file, as a clean checkout has it.
        call remove_path(join_path(root, 'src/kmod.mod'))
        call remove_tree(join_path(root, 'build'))
        call write_consumer_program(root)
        call list_add(environment, 'FO_JOBS=1')
        call fo(root, 'exec stray', environment, result, 'cache-resident')
        call assert_contains(result%stdout, 'value=1', &
            'a build without the module file does not get the object compiled '// &
            'against it: '//result%stdout//result%stderr)
    end subroutine module_file_between_resident_builds

    subroutine write_consumer_program(root)
        character(len=*), intent(in) :: root
        character(:), allocatable :: text

        text = read_text(join_path(root, 'src/m.f90'))
        call write_consumer(root)
        call write_text(join_path(root, 'src/m.f90'), text)
    end subroutine write_consumer_program

    subroutine module_file_in_the_module_directory()
        !! A module whose source was removed or renamed, or that another
        !! branch built, stays in build/fo/mod, where gfortran finds it.
        character(len=*), parameter :: ext = 'module ext'//nl// &
            '    integer, parameter :: k = '
        character(:), allocatable :: first, second, clean
        type(process_result_t) :: result

        first = join_path(scratch, 'moddir-a')
        second = join_path(scratch, 'moddir-b-copy')
        clean = join_path(scratch, 'moddir-c-clean')
        call write_ext_consumer(first)
        call write_ext_consumer(second)
        call write_ext_consumer(clean)
        call write_text(join_path(scratch, 'moddir-ext/two.f90'), ext//'2'//nl// &
            'end module'//nl)
        call write_text(join_path(scratch, 'moddir-ext/one.f90'), ext//'1'//nl// &
            'end module'//nl)
        call write_text(join_path(first, 'build/fo/mod/.keep'), '')
        call write_text(join_path(second, 'build/fo/mod/.keep'), '')
        call run_process(argv(trim(real_fc), '-fsyntax-only', &
            join_path(scratch, 'moddir-ext/two.f90')), join_path(first, 'build/fo/mod'), &
            result)
        call run_process(argv(trim(real_fc), '-fsyntax-only', &
            join_path(scratch, 'moddir-ext/one.f90')), join_path(second, 'build/fo/mod'), &
            result)
        call assert_true(file_exists(join_path(second, 'build/fo/mod/ext.mod')), &
            'a module file of no source lies in the module directory: '//result%stderr)

        call fo(first, 'exec xm', relocatable, result, 'cache-moddir')
        call assert_contains(result%stdout, 'value=2', &
            'gfortran reads the module from the module directory')
        call fo(second, 'exec xm', relocatable, result, 'cache-moddir')
        call assert_contains(result%stdout, 'value=1', &
            'a copy with another such module file compiles with it')
        call fo(clean, 'exec xm', relocatable, result, 'cache-moddir', may_fail=.true.)
        call assert_true(result%exit_code /= 0, &
            'a clean copy without the module fails as its compile does: '// &
            result%stdout//result%stderr)
    end subroutine module_file_in_the_module_directory

    subroutine write_ext_consumer(root)
        character(len=*), intent(in) :: root

        call write_text(join_path(root, 'fpm.toml'), 'name = "xm"'//nl)
        call write_text(join_path(root, 'src/m.f90'), 'module m'//nl// &
            '    use ext, only: k'//nl//'    implicit none'//nl//'contains'//nl// &
            '    integer function answer()'//nl//'        answer = k'//nl// &
            '    end function'//nl//'end module'//nl)
        call write_text(join_path(root, 'app/main.f90'), 'program main'//nl// &
            '    use m, only: answer'//nl// &
            '    print ''(a,i0)'', ''value='', answer()'//nl//'end program'//nl)
    end subroutine write_ext_consumer

    subroutine write_wrapper_project(root)
        !! include/site_config.h of the library continues the search with
        !! #include_next, as wrappers of system headers do.
        character(len=*), intent(in) :: root

        call write_text(join_path(root, 'fpm.toml'), 'name = "wrap"'//nl// &
            '[library]'//nl//'include-dir = ["include"]'//nl)
        call write_text(join_path(root, 'include/site_config.h'), &
            '#include_next <site_config.h>'//nl//'#ifndef SITE_K'//nl// &
            '#define SITE_K 0'//nl//'#endif'//nl)
        call write_text(join_path(root, 'src/m.F90'), 'module m'//nl// &
            '    implicit none'//nl//'contains'//nl// &
            '    integer function answer()'//nl//'#include <site_config.h>'//nl// &
            '        answer = SITE_K'//nl//'    end function'//nl//'end module'//nl)
        call write_text(join_path(root, 'app/main.f90'), 'program main'//nl// &
            '    use m, only: answer'//nl// &
            '    print ''(a,i0)'', ''value='', answer()'//nl//'end program'//nl)
    end subroutine write_wrapper_project

    subroutine header_through_include_next()
        character(:), allocatable :: first, second
        type(string_list_t) :: site_one, site_two
        type(process_result_t) :: result

        first = join_path(scratch, 'next-a')
        second = join_path(scratch, 'next-b-copy')
        call write_wrapper_project(first)
        call write_wrapper_project(second)
        call write_text(join_path(scratch, 'one/site_config.h'), '#define SITE_K 1'//nl)
        call write_text(join_path(scratch, 'two/site_config.h'), '#define SITE_K 2'//nl)
        site_one = relocatable
        call list_add(site_one, 'CPATH='//join_path(scratch, 'one'))
        site_two = relocatable
        call list_add(site_two, 'CPATH='//join_path(scratch, 'two'))

        call fo(first, 'exec wrap', site_one, result, 'cache-next')
        call assert_contains(result%stdout, 'value=1', &
            '#include_next reaches the header on CPATH')
        call fo(second, 'exec wrap', site_two, result, 'cache-next')
        call assert_contains(result%stdout, 'value=2', &
            'a copy whose #include_next finds another header compiles with it')

        ! The default key in one tree.
        site_one = plain
        call list_add(site_one, 'CPATH='//join_path(scratch, 'one'))
        site_two = plain
        call list_add(site_two, 'CPATH='//join_path(scratch, 'two'))
        call fo(first, 'exec wrap', site_one, result, 'cache-next')
        call assert_contains(result%stdout, 'value=1', 'the default key builds the same')
        call fo(first, 'exec wrap', site_two, result, 'cache-next')
        call assert_contains(result%stdout, 'value=2', &
            'and follows a CPATH that changes what #include_next reads')
    end subroutine header_through_include_next

    subroutine header_through_import_and_probe()
        character(len=*), parameter :: import = '#import <site_config.h>'//nl// &
            '        value = SITE_K'
        character(len=*), parameter :: probe = '#if __has_'//'include(<site_extra.h>)'// &
            nl//'        value = 1'//nl//'#else'//nl//'        value = 2'//nl//'#endif'
        character(:), allocatable :: first, second
        type(string_list_t) :: site_one, site_two
        type(process_result_t) :: result

        first = join_path(scratch, 'import-a')
        second = join_path(scratch, 'import-b-copy')
        call write_program(first, 'import', 'main.F90', import)
        call write_program(second, 'import', 'main.F90', import)
        site_one = relocatable
        call list_add(site_one, 'CPATH='//join_path(scratch, 'one'))
        site_two = relocatable
        call list_add(site_two, 'CPATH='//join_path(scratch, 'two'))
        call fo(first, 'exec import', site_one, result, 'cache-import')
        call assert_contains(result%stdout, 'value=1', '#import reads the header on CPATH')
        call fo(second, 'exec import', site_two, result, 'cache-import')
        call assert_contains(result%stdout, 'value=2', &
            'a copy whose #import finds another header compiles with it')

        first = join_path(scratch, 'probe-a')
        second = join_path(scratch, 'probe-b-copy')
        call write_program(first, 'probe', 'main.F90', probe)
        call write_program(second, 'probe', 'main.F90', probe)
        call write_text(join_path(scratch, 'one/site_extra.h'), ''//nl)
        call fo(first, 'exec probe', site_one, result, 'cache-probe')
        call assert_contains(result%stdout, 'value=1', 'the probed header is found')
        ! gfortran 16 crashes on the probe of an absent header; a later one
        ! gives 2. Either way the copy compiles and gets no result of the first.
        call fo(second, 'exec probe', site_two, result, 'cache-probe', may_fail=.true.)
        call assert_contains(read_text(log), 'app/main.F90', &
            'a copy where the probed header is absent compiles itself')
        if (result%exit_code == 0) call assert_contains(result%stdout, 'value=2', &
            'and runs without the header')
    end subroutine header_through_import_and_probe

    subroutine include_spellings_beyond_the_plain_line()
        character(len=*), parameter :: sentinel = '        value = 0'//nl// &
            '!$ include "y.inc"'
        character(:), allocatable :: first, second
        type(process_result_t) :: result

        first = join_path(scratch, 'sentinel-a')
        second = join_path(scratch, 'sentinel-b-copy')
        call write_program(first, 'sentinel', 'main.f90', sentinel)
        call write_program(second, 'sentinel', 'main.f90', sentinel)
        call write_text(join_path(first, 'app/y.inc'), 'value = 1'//nl)
        call write_text(join_path(second, 'app/y.inc'), 'value = 2'//nl)
        call fo(first, 'exec --flag -fopenmp sentinel', relocatable, result, 'cache-spell')
        call assert_contains(result%stdout, 'value=1', 'gfortran reads !$ include')
        call fo(second, 'exec --flag -fopenmp sentinel', relocatable, result, 'cache-spell')
        call assert_contains(result%stdout, 'value=2', &
            'a copy with another file behind !$ include compiles with it')

        first = join_path(scratch, 'fixed-a')
        second = join_path(scratch, 'fixed-b-copy')
        call write_fixed(first, '1')
        call write_fixed(second, '2')
        call fo(first, 'exec fixed', relocatable, result, 'cache-spell')
        call assert_contains(result%stdout, 'value=1', &
            'fixed form allows blanks inside INCLUDE')
        call fo(second, 'exec fixed', relocatable, result, 'cache-spell')
        call assert_contains(result%stdout, 'value=2', &
            'a copy with another file behind such a line compiles with it')
    end subroutine include_spellings_beyond_the_plain_line

    subroutine write_fixed(root, k)
        character(len=*), intent(in) :: root, k

        call write_text(join_path(root, 'fpm.toml'), 'name = "fixed"'//nl// &
            '[fortran]'//nl//'source-form = "default"'//nl)
        call write_text(join_path(root, 'app/main.f'), &
            '      program main'//nl//'      integer value'//nl// &
            '      inc lude "x.inc"'//nl//'      print ''(a,i0)'', ''value='', value'//nl// &
            '      end'//nl)
        call write_text(join_path(root, 'app/x.inc'), '      value = '//k//nl)
    end subroutine write_fixed

    subroutine header_the_driver_preincludes()
        !! On glibc systems the driver passes the C library's
        !! math-vector-fortran.h to every compile; its SIMD declarations
        !! decide whether a loop calls libmvec. A driver that pre-includes a
        !! header of the test stands for hosts with different C libraries.
        character(len=*), parameter :: simd = &
            '!GCC$ builtin (cos) attributes simd (notinbranch) if(''x86_64'')'//nl
        character(len=*), parameter :: vector_cos = '_ZGVbN2v_cos'
        character(:), allocatable :: first, second, wrapper, header
        type(string_list_t) :: environment
        type(process_result_t) :: result

        header = join_path(scratch, 'preinclude/vector.h')
        call write_text(header, simd)
        wrapper = join_path(scratch, 'preinclude/bin/gfortran')
        call write_text(wrapper, '#!/bin/sh'//nl//'exec "'// &
            join_path(scratch, 'bin/gfortran')//'" -nostdinc -fpre-include='//header// &
            ' "$@"'//nl)
        call set_mode(wrapper, int(o'755'))
        call list_add(environment, 'FO_JOBS=2')
        call list_add(environment, 'FO_FC='//wrapper)
        call list_add(environment, 'FO_CACHE_RELOCATABLE=1')

        first = join_path(scratch, 'vector-a')
        second = join_path(scratch, 'vector-b-copy')
        call write_vector_loop(first)
        call write_vector_loop(second)
        call fo(first, 'build --flag -O3 --flag -ffast-math', environment, result, &
            'cache-vector')
        ! Only where the header vectorises the loop.
        if (index(read_text(join_path(first, 'build/fo/obj/src_v.f90.o')), &
            vector_cos) == 0) return
        call write_text(header, ''//nl)
        call fo(second, 'build --flag -O3 --flag -ffast-math', environment, result, &
            'cache-vector')
        call assert_equal_integer(compiles(), 1, &
            'a copy whose driver pre-includes another header compiles: '//read_text(log))
        call assert_not_contains(read_text(join_path(second, &
            'build/fo/obj/src_v.f90.o')), vector_cos, &
            'and calls no vector function the header does not declare')
    end subroutine header_the_driver_preincludes

    subroutine write_vector_loop(root)
        character(len=*), intent(in) :: root

        call write_text(join_path(root, 'fpm.toml'), 'name = "vector"'//nl)
        call write_text(join_path(root, 'src/v.f90'), 'subroutine s(x, y, n)'//nl// &
            '    integer, intent(in) :: n'//nl// &
            '    real(8), intent(in) :: x(n)'//nl//'    real(8), intent(out) :: y(n)'//nl// &
            '    integer :: i'//nl//'    do i = 1, n'//nl//'        y(i) = cos(x(i))'//nl// &
            '    end do'//nl//'end subroutine'//nl)
    end subroutine write_vector_loop

end program test_compile_key_inputs_cli
