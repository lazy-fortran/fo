program test_fpm_extra_fo_model
    !! [extra.fo.layers], [extra.fo.test-groups] and [extra.fo.test-defaults]:
    !! parsed values and the strict diagnostics for malformed manifests.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_test_harness, only: make_scratch, write_text, remove_tree
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    character(len=*), parameter :: head = 'name = "m"'//nl//'version = "0.1.0"'//nl
    character(len=*), parameter :: inputs = &
        '[[extra.fo.inputs]]'//nl//'path = "data/a.dat"'//nl
    character(len=:), allocatable :: directory
    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0
    call make_scratch('fo-extra-model', directory)

    call test_parse_model()
    call test_multiline_defaults()
    call test_absent_sections()
    call test_errors()
    call test_large_manifest()

    call remove_tree(directory)
    write (output_unit, '(a,i0,a,i0,a)') 'fpm_extra_fo_model: ', n_pass, &
        ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (output_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine parse(body, c, ierr)
        character(len=*), intent(in) :: body
        type(fpm_config_t), intent(out) :: c
        integer, intent(out) :: ierr

        call write_text(directory//'/fpm.toml', head//body)
        call fpm_config_parse(directory, c, ierr)
    end subroutine parse

    subroutine expect_error(label, body, fragment)
        character(len=*), intent(in) :: label, body, fragment
        type(fpm_config_t) :: c
        integer :: ierr

        call parse(body, c, ierr)
        call assert(ierr /= 0, label//': rejected')
        call assert(index(c%manifest_parse_error, fragment) > 0, &
            label//': diagnostic mentions "'//fragment//'", got "'// &
            trim(c%manifest_parse_error)//'"')
    end subroutine expect_error

    subroutine test_parse_model()
        type(fpm_config_t) :: c
        integer :: ierr

        call parse(inputs// &
            '[extra.fo.layers]'//nl// &
            'base = { source = ["src/base/**", "src/util.f90"] }'//nl// &
            "phys = { modules = ['phys_mod', 'phys_aux'], uses = ['base'] }"//nl// &
            'top = { source = ["src/top/**"], uses = ["base", "phys"] }'//nl// &
            '[extra.fo.test-groups]'//nl// &
            'slow = { tests = ["test/test_*_slow*"], inputs = ["data/a.dat"] }'//nl// &
            'unit = { tests = ["test/unit_*"] }'//nl// &
            '[extra.fo.test-defaults]'//nl// &
            'exclude = ["slow", "perf"]'//nl, c, ierr)
        call assert(ierr == 0, 'model: manifest parses')
        call assert(c%n_layers == 3, 'model: three layers')
        if (c%n_layers == 3) then
            call assert(c%layers(1)%name == 'base', 'model: layer order')
            call assert(size(c%layers(1)%source) == 2, 'model: base globs')
            call assert(c%layers(1)%source(2)%s == 'src/util.f90', &
                'model: second glob')
            call assert(size(c%layers(1)%uses) == 0, 'model: base uses nothing')
            call assert(size(c%layers(2)%modules) == 2, 'model: module layer')
            call assert(size(c%layers(2)%source) == 0, 'model: module layer no globs')
            call assert(c%layers(2)%modules(1)%s == 'phys_mod', &
                'model: single-quoted module')
            call assert(c%layers(2)%uses(1)%s == 'base', 'model: phys uses base')
            call assert(size(c%layers(3)%uses) == 2, 'model: top uses two layers')
        end if
        call assert(c%n_test_groups == 2, 'model: two groups')
        if (c%n_test_groups == 2) then
            call assert(c%test_groups(1)%name == 'slow', 'model: group name')
            call assert(c%test_groups(1)%tests(1)%s == 'test/test_*_slow*', &
                'model: group glob')
            call assert(c%test_groups(1)%inputs(1)%s == 'data/a.dat', &
                'model: group input')
            call assert(size(c%test_groups(2)%inputs) == 0, 'model: no inputs')
        end if
        call assert(c%default_exclude_seen .and. size(c%default_exclude) == 2, &
            'model: default excludes')
        if (size(c%default_exclude) == 2) call assert( &
            c%default_exclude(2)%s == 'perf', 'model: second exclude')
    end subroutine test_parse_model

    subroutine test_multiline_defaults()
        type(fpm_config_t) :: c
        integer :: ierr

        call parse('[extra.fo.test-defaults]'//nl//'exclude = ['//nl// &
            '    "slow",'//nl//'    "perf",'//nl//']'//nl, c, ierr)
        call assert(ierr == 0, 'multiline: parses')
        call assert(allocated(c%default_exclude), 'multiline: allocated')
        if (allocated(c%default_exclude)) call assert( &
            size(c%default_exclude) == 2, 'multiline: two entries')
        call parse('[extra.fo.test-defaults]'//nl//'exclude = []'//nl, c, ierr)
        call assert(ierr == 0 .and. c%default_exclude_seen, &
            'multiline: explicit empty exclude clears the default')
    end subroutine test_multiline_defaults

    subroutine test_absent_sections()
        type(fpm_config_t) :: c
        integer :: ierr

        call parse('[extra.fo]'//nl//'dispatcher = "x"'//nl, c, ierr)
        call assert(ierr == 0, 'absent: parses')
        call assert(c%n_layers == 0 .and. c%n_test_groups == 0 .and. &
            .not. c%default_exclude_seen, 'absent: model is empty')
    end subroutine test_absent_sections

    subroutine test_errors()
        character(len=*), parameter :: l = '[extra.fo.layers]'//nl
        character(len=*), parameter :: g = '[extra.fo.test-groups]'//nl

        call expect_error('unknown layer key', l// &
            'a = { source = ["x"], bogus = ["y"] }'//nl, 'unknown key bogus')
        call expect_error('layer without members', l// &
            'a = { uses = [] }'//nl, 'exactly one of source or modules')
        call expect_error('layer with both forms', l// &
            'a = { source = ["x"], modules = ["m"] }'//nl, 'exactly one of')
        call expect_error('dangling uses', l// &
            'a = { source = ["x"], uses = ["ghost"] }'//nl, 'unknown layer ghost')
        call expect_error('self cycle', l// &
            'a = { source = ["x"], uses = ["a"] }'//nl, 'layer cycle: a -> a')
        call expect_error('three-layer cycle', l// &
            'a = { source = ["x"], uses = ["b"] }'//nl// &
            'b = { source = ["y"], uses = ["c"] }'//nl// &
            'c = { source = ["z"], uses = ["a"] }'//nl, 'layer cycle: a -> b -> c -> a')
        call expect_error('duplicate layer', l// &
            'a = { source = ["x"] }'//nl//'a = { source = ["y"] }'//nl, &
            'duplicate layer a')
        call expect_error('not a table', l//'a = ["x"]'//nl, 'inline table')
        call expect_error('unquoted element', l//'a = { source = [x] }'//nl, &
            'quoted strings')
        call expect_error('empty element', l//'a = { source = [""] }'//nl, &
            'quoted strings')
        call expect_error('scalar where array expected', l// &
            'a = { source = "x" }'//nl, 'array of strings')
        call expect_error('unterminated table', l//'a = { source = ["x"]'//nl, &
            'inline table')
        call expect_error('bad layer name', l//'"a b" = { source = ["x"] }'//nl, &
            'invalid [extra.fo.layers] name')
        call expect_error('inline layers in [extra.fo]', &
            '[extra.fo]'//nl//'layers = { a = { source = ["x"] } }'//nl, &
            'own [extra.fo.layers] table')

        call expect_error('unknown group key', g// &
            'a = { tests = ["t"], extra = [] }'//nl, 'unknown key extra')
        call expect_error('group without tests', g//'a = { inputs = [] }'//nl, &
            'requires a tests array')
        call expect_error('dangling input', inputs//g// &
            'a = { tests = ["t"], inputs = ["data/missing.dat"] }'//nl, &
            'not declared in [[extra.fo.inputs]]: data/missing.dat')
        call expect_error('input without any inputs table', g// &
            'a = { tests = ["t"], inputs = ["data/a.dat"] }'//nl, 'not declared')
        call expect_error('duplicate group', g// &
            'a = { tests = ["t"] }'//nl//'a = { tests = ["u"] }'//nl, &
            'duplicate test group a')

        call expect_error('unknown defaults key', &
            '[extra.fo.test-defaults]'//nl//'include = ["a"]'//nl, &
            'unknown [extra.fo.test-defaults] key: include')
        call expect_error('defaults not an array', &
            '[extra.fo.test-defaults]'//nl//'exclude = "slow"'//nl, &
            'array of strings')
        call expect_error('duplicate defaults', &
            '[extra.fo.test-defaults]'//nl//'exclude = ["a"]'//nl// &
            'exclude = ["b"]'//nl, 'duplicate')
    end subroutine test_errors

    subroutine test_large_manifest()
        !! A big layer is one long line: no cap on entries or line length.
        type(fpm_config_t) :: c
        character(len=:), allocatable :: layer, group, mods, globs
        character(len=48) :: item
        integer :: i, ierr

        mods = ''
        do i = 1, 500
            write (item, '(a,i0,a)') '"module_with_a_long_name_number_', i, '"'
            if (i > 1) mods = mods//', '
            mods = mods//trim(item)
        end do
        globs = ''
        do i = 1, 300
            write (item, '(a,i0,a)') '"test/group_directory/case_', i, '_*.f90"'
            if (i > 1) globs = globs//', '
            globs = globs//trim(item)
        end do
        layer = 'big = { modules = ['//mods//'], uses = ["small"] }'//nl
        group = 'huge = { tests = ['//globs//'] }'//nl
        call parse('[extra.fo.layers]'//nl//'small = { source = ["s"] }'//nl// &
            layer//'[extra.fo.test-groups]'//nl//group, c, ierr)
        call assert(ierr == 0, 'large: manifest parses')
        call assert(len(layer) > 10000, 'large: layer line is long')
        if (c%n_layers == 2) then
            call assert(size(c%layers(2)%modules) == 500, 'large: 500 modules')
            if (size(c%layers(2)%modules) == 500) then
                call assert(c%layers(2)%modules(500)%s == &
                    'module_with_a_long_name_number_500', 'large: last module intact')
            end if
        else
            call assert(.false., 'large: two layers')
        end if
        if (c%n_test_groups == 1) then
            call assert(size(c%test_groups(1)%tests) == 300, 'large: 300 globs')
        else
            call assert(.false., 'large: one group')
        end if

        ! A long multi-line exclude array is not capped either.
        mods = '[extra.fo.test-defaults]'//nl//'exclude = ['//nl
        do i = 1, 400
            write (item, '(a,i0,a)') '    "label_', i, '",'
            mods = mods//trim(item)//nl
        end do
        call parse(mods//']'//nl, c, ierr)
        call assert(ierr == 0, 'large: long exclude array parses')
        if (allocated(c%default_exclude)) call assert( &
            size(c%default_exclude) == 400, 'large: 400 excludes')
    end subroutine test_large_manifest

end program test_fpm_extra_fo_model
