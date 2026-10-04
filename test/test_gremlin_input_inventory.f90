program test_gremlin_input_inventory
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_rename
    use fo_input_inventory, only: input_declaration_t, input_inventory_t, &
        input_inventory_discover, INPUT_FILE
    use fo_util, only: make_tmpfile
    implicit none

    character(len=4096) :: fixture, project, dependency, leaf
    character(len=1024) :: diagnostic
    type(input_inventory_t) :: initial, changed, ignored, runtime, previous
    type(input_declaration_t) :: declarations(2), bad_declaration(1)
    integer :: ierr

    call make_tmpfile('fo-input-inventory-fixture', fixture)
    call fs_make_dir(trim(fixture))
    project = trim(fixture)//'/project'
    dependency = trim(fixture)//'/dependency'
    leaf = trim(fixture)//'/leaf'
    call fs_make_dir(trim(project)//'/src')
    call fs_make_dir(trim(project)//'/src/build')
    call fs_make_dir(trim(project)//'/src/cache')
    call fs_make_dir(trim(project)//'/test')
    call fs_make_dir(trim(project)//'/include')
    call fs_make_dir(trim(project)//'/build')
    call fs_make_dir(trim(project)//'/cache')
    call fs_make_dir(trim(project)//'/.git/hooks')
    call fs_make_dir(trim(project)//'/.gremlin/cache')
    call fs_make_dir(trim(project)//'/.cache')
    call fs_make_dir(trim(dependency)//'/src')
    call fs_make_dir(trim(leaf)//'/src')
    call write(trim(project)//'/fpm.toml', &
        'name = "inventory-fixture"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'fixture-dep = { path = "../dependency" }'//new_line('a')// &
        'dependency-copy = { path = "../dependency" }')
    call write(trim(dependency)//'/fpm.toml', &
        'name = "fixture-dep"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'fixture-leaf = { path = "../leaf" }')
    call write(trim(leaf)//'/fpm.toml', 'name = "fixture-leaf"')
    call write(trim(project)//'/src/main.f90', 'program main')
    call write(trim(project)//'/test/oracle.f90', 'program oracle')
    call write(trim(project)//'/include/runtime.inc', 'include dependency')
    call write(trim(dependency)//'/src/dep.f90', 'module dep')
    call write(trim(leaf)//'/src/leaf.f90', 'module leaf')
    call write(trim(project)//'/build/generated.f90', 'program generated')
    call write(trim(project)//'/.git/hooks/ignored.py', '#!/usr/bin/env python3')
    call write(trim(project)//'/.gremlin/cache/ignored.f90', 'generated')
    call write(trim(project)//'/.cache/ignored.f90', 'generated')
    call write(trim(project)//'/cache/ignored.f90', 'generated')
    call write(trim(project)//'/cache/runtime.dat', 'runtime declaration')
    call write(trim(project)//'/src/build/retained.f90', 'module retained_build')
    call write(trim(project)//'/src/cache/retained.f90', 'module retained_cache')
    call write(trim(project)//'/PLAN.md', 'not a build input')

    call discover(initial, ierr, diagnostic)
    call require(ierr == 0, 'initial inventory: '//trim(diagnostic))
    call require(initial%complete, 'successful inventory is complete')
    call require(len_trim(initial%digest) > 0, 'inventory digest is present')
    call require(initial%root_count == 3, &
        'duplicate dependency paths share one physical root')
    call require(has_entry(initial, 'project', 'test/oracle.f90', 'test-oracle'), &
        'test oracle is a declared input')
    call require(has_entry(initial, 'project', 'include/runtime.inc', 'include'), &
        'include dependency is an input')
    call require(has_entry(initial, 'project', 'src/build/retained.f90', &
        'build-source'), 'nested src/build source is retained')
    call require(has_entry(initial, 'project', 'src/cache/retained.f90', &
        'build-source'), 'nested src/cache source is retained')
    call require(has_root_alias(initial, 'dependency:fixture-dep'), &
        'canonical dependency root retains the original alias')
    call require(has_entry(initial, 'dependency:dependency-copy', 'src/dep.f90', &
        'dependency-source'), 'path dependency source is an input')
    call require(has_entry(initial, 'dependency:fixture-dep/fixture-leaf', &
        'src/leaf.f90', 'dependency-source'), &
        'transitive path dependency source is an input')

    call write(trim(project)//'/PLAN.md', 'changed unrelated plan')
    call discover(ignored, ierr, diagnostic)
    call require(ierr == 0, 'inventory after ignored mutations: '//trim(diagnostic))
    call require(ignored%digest == initial%digest, &
        'plans and generated/cache/git outputs do not affect the digest')

    previous = initial
    call write(trim(project)//'/src/build/retained.f90', &
        'module retained_build_changed')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after nested build-source edit: '// &
        trim(diagnostic))
    call require(changed%digest /= previous%digest, &
        'nested src/build source content changes inventory identity')
    previous = changed
    call write(trim(project)//'/src/cache/retained.f90', &
        'module retained_cache_changed')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after nested cache-source edit: '// &
        trim(diagnostic))
    call require(changed%digest /= previous%digest, &
        'nested src/cache source content changes inventory identity')

    call write(trim(project)//'/src/main.f90', 'program changed')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after content edit: '//trim(diagnostic))
    call require(changed%digest /= initial%digest, 'content edit changes digest')
    previous = changed
    call write(trim(project)//'/src/added.f90', 'module added')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after file addition: '//trim(diagnostic))
    call require(changed%digest /= previous%digest, 'file addition changes digest')
    previous = changed
    call require(fs_rename(trim(project)//'/src/added.f90', &
        trim(project)//'/src/renamed.f90') == 0, 'fixture file can be renamed')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after rename: '//trim(diagnostic))
    call require(changed%digest /= previous%digest, 'rename changes digest')
    previous = changed
    call fs_remove_file(trim(project)//'/src/renamed.f90')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after removal: '//trim(diagnostic))
    call require(changed%digest /= previous%digest, 'file removal changes digest')
    previous = changed
    call write(trim(dependency)//'/src/dep.f90', 'module dep changed')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after dependency edit: '//trim(diagnostic))
    call require(changed%digest /= previous%digest, &
        'path dependency content edit changes digest')

    declarations(1)%root_alias = 'project'
    declarations(1)%relative_path = 'cache/runtime.dat'
    declarations(1)%role = 'runtime-data'
    declarations(1)%expected_kind = INPUT_FILE
    declarations(1)%writable_at_execution = .false.
    declarations(2) = declarations(1)
    declarations(2)%role = 'runtime-oracle'
    call input_inventory_discover(trim(project), declarations, runtime, ierr, &
        diagnostic)
    call require(ierr == 0, 'runtime declarations: '//trim(diagnostic))
    call require(has_entry(runtime, 'project', 'cache/runtime.dat', &
        'runtime-data'), &
        'runtime data role is retained')
    call require(has_entry(runtime, 'project', 'cache/runtime.dat', &
        'runtime-oracle'), &
        'duplicate physical path retains its second role')
    declarations(2)%writable_at_execution = .true.
    call input_inventory_discover(trim(project), declarations, runtime, ierr, &
        diagnostic)
    call require(ierr /= 0 .and. .not. runtime%valid, &
        'same-path roles with conflicting writable intent are rejected')
    declarations(2)%writable_at_execution = .false.

    bad_declaration(1) = declarations(1)
    bad_declaration(1)%relative_path = '../escape'
    call expect_rejected(bad_declaration, 'parent traversal is rejected')
    bad_declaration(1)%relative_path = '/absolute/path'
    call expect_rejected(bad_declaration, 'absolute declaration is rejected')
    bad_declaration(1)%relative_path = 'missing.dat'
    call expect_rejected(bad_declaration, 'missing declaration is diagnosed')
    bad_declaration(1)%relative_path = 'cache/runtime.dat'
    bad_declaration(1)%root_alias = 'undeclared-root'
    call expect_rejected(bad_declaration, 'unknown root alias is rejected')

    call write(trim(project)//'/fpm.toml', &
        'name = "inventory-fixture"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'fixture-dep = { path = "../dependency" }'//new_line('a')// &
        'external-dep = "*"')
    call discover(runtime, ierr, diagnostic)
    call require(ierr == 0, 'partial inventory is available: '//trim(diagnostic))
    call require(.not. runtime%complete .and. len_trim(runtime%diagnostic) > 0, &
        'unresolved registry dependency cannot certify complete closure')
    call fs_make_dir(trim(project)//'/build/dependencies/external-dep/src')
    call write(trim(project)//'/build/dependencies/external-dep/fpm.toml', &
        'name = "external-dep"')
    call write(trim(project)//'/build/dependencies/external-dep/src/external.f90', &
        'module external_dep')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory with acquired dependency: '//trim(diagnostic))
    call require(changed%complete, &
        'existing acquired registry source closes its declared dependency root')
    call require(has_entry(changed, 'dependency:external-dep', &
        'src/external.f90', 'dependency-source'), &
        'acquired registry source is included in canonical inventory')
    previous = changed
    call write(trim(project)//'/build/dependencies/external-dep/src/external.f90', &
        'module external_dep_changed')
    call discover(changed, ierr, diagnostic)
    call require(ierr == 0, 'inventory after acquired-source edit: '// &
        trim(diagnostic))
    call require(changed%digest /= previous%digest, &
        'acquired dependency source edit changes inventory digest')

    call check_whole_root_outputs(trim(fixture))
    call fs_remove_tree(trim(fixture))

contains

    subroutine check_whole_root_outputs(parent)
        character(len=*), intent(in) :: parent
        character(len=4096) :: whole_root
        character(len=1024) :: message
        type(input_inventory_t) :: before, after
        type(input_declaration_t) :: none(0)
        integer :: status

        whole_root = trim(parent)//'/whole-root-project'
        call fs_make_dir(trim(whole_root)//'/src')
        call fs_make_dir(trim(whole_root)//'/build')
        call fs_make_dir(trim(whole_root)//'/cache')
        call fs_make_dir(trim(whole_root)//'/.cache')
        call write(trim(whole_root)//'/fpm.toml', &
            'name = "whole-root-fixture"'//new_line('a')// &
            '[build]'//new_line('a')// &
            'source-dir = "."')
        call write(trim(whole_root)//'/src/main.f90', 'program main')
        call write(trim(whole_root)//'/build/generated.f90', &
            'program generated')
        call write(trim(whole_root)//'/cache/generated.dat', 'generated')
        call write(trim(whole_root)//'/.cache/generated.dat', 'generated')

        call input_inventory_discover(trim(whole_root), none, before, status, &
            message)
        call require(status == 0, 'whole-root inventory: '//trim(message))
        call require(has_entry(before, 'project', 'src/main.f90', &
            'build-source'), 'whole-root scan keeps configured source files')
        call write(trim(whole_root)//'/build/generated.f90', &
            'program generated_changed')
        call write(trim(whole_root)//'/cache/generated.dat', 'changed')
        call write(trim(whole_root)//'/.cache/generated.dat', 'changed')
        call input_inventory_discover(trim(whole_root), none, after, status, &
            message)
        call require(status == 0, &
            'whole-root inventory after output edits: '//trim(message))
        call require(before%digest == after%digest, &
            'whole-root generated directories do not affect inventory identity')
        call fs_remove_tree(trim(whole_root))
    end subroutine check_whole_root_outputs

    subroutine discover(inventory, status, message)
        type(input_inventory_t), intent(out) :: inventory
        integer, intent(out) :: status
        character(len=*), intent(out) :: message
        type(input_declaration_t) :: none(0)

        call input_inventory_discover(trim(project), none, inventory, status, &
            message)
    end subroutine discover

    subroutine expect_rejected(items, reason)
        type(input_declaration_t), intent(in) :: items(:)
        character(len=*), intent(in) :: reason
        type(input_inventory_t) :: inventory

        call input_inventory_discover(trim(project), items, inventory, ierr, &
            diagnostic)
        call require(ierr /= 0 .and. .not. inventory%complete, reason)
        call require(len_trim(diagnostic) > 0, reason//' has a diagnostic')
    end subroutine expect_rejected

    logical function has_entry(inventory, alias, path, role)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: alias, path, role
        integer :: i, j, root_index

        has_entry = .false.
        root_index = 0
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) /= alias) cycle
                root_index = i
                exit
            end do
            if (root_index > 0) exit
        end do
        if (root_index == 0) return
        do i = 1, inventory%entry_count
            if (trim(inventory%entries(i)%root_alias) /= &
                    trim(inventory%roots(root_index)%canonical_alias)) cycle
            if (trim(inventory%entries(i)%relative_path) /= path) cycle
            if (trim(inventory%entries(i)%role) /= role) cycle
            has_entry = .true.
            return
        end do
    end function has_entry

    logical function has_root_alias(inventory, alias)
        type(input_inventory_t), intent(in) :: inventory
        character(len=*), intent(in) :: alias
        integer :: i, j

        has_root_alias = .false.
        do i = 1, inventory%root_count
            do j = 1, inventory%roots(i)%alias_count
                if (trim(inventory%roots(i)%aliases(j)) /= alias) cycle
                has_root_alias = .true.
                return
            end do
        end do
    end function has_root_alias

    subroutine write(path, contents)
        character(len=*), intent(in) :: path, contents
        integer :: unit, status

        open (newunit=unit, file=path, status='replace', action='write', &
            iostat=status)
        call require(status == 0, 'create fixture file: '//path)
        write (unit, '(a)', iostat=status) contents
        call require(status == 0, 'write fixture file: '//path)
        close (unit, iostat=status)
        call require(status == 0, 'close fixture file: '//path)
    end subroutine write

    subroutine require(condition, reason)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: reason
        if (condition) return
        write (*, '(a)') 'FAIL: '//trim(reason)
        error stop 1
    end subroutine require

end program test_gremlin_input_inventory
