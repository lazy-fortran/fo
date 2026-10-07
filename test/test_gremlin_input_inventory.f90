program test_gremlin_input_inventory
    use fo_fs, only: fs_make_dir, fs_remove_file, fs_remove_tree, fs_rename
    use fo_input_inventory, only: input_declaration_t, input_inventory_t, &
        input_inventory_discover, input_inventory_declarations_from_config, &
        INPUT_FILE, INPUT_DIRECTORY
    use fo_util, only: make_tmpfile, read_text_file
    use fo_gremlin_execution_view, only: execution_view_t, execution_view_create, &
        execution_view_release
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_gremlin_generation, only: generation_context_t, generation_t, &
        generation_capture
    use fo_cache, only: cache_file_digest
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
    call check_flattened_git_dependencies(trim(fixture))
    call check_dependency_directory_inputs(trim(fixture))
    call fs_remove_tree(trim(fixture))

contains

    subroutine check_flattened_git_dependencies(parent)
        character(len=*), intent(in) :: parent
        character(len=4096) :: root, provider, child
        character(len=1024) :: message
        character(len=:), allocatable :: driver, manifest
        type(input_inventory_t) :: captured
        type(input_declaration_t) :: none(0)
        type(execution_view_t) :: view
        type(generation_context_t) :: context
        type(generation_t) :: generation
        type(string_list_t) :: arguments, environment
        type(process_result_t) :: result
        integer :: status, release_status, scenario, answer

        root = trim(parent)//'/flat-git-project'
        provider = trim(root)//'/build/dependencies/provider'
        child = trim(root)//'/build/dependencies/child'
        call fs_make_dir(trim(root)//'/app')
        call fs_make_dir(trim(provider)//'/src')
        call fs_make_dir(trim(child)//'/src')
        ! No Git metadata or reachable remotes: acquisition must not run.
        call write(trim(provider)//'/fpm.toml', &
            'name = "provider"'//new_line('a')//'[dependencies]'//new_line('a')// &
            'child = { git = "https://invalid.invalid/child", rev = "'// &
            repeat('1', 40)//'" }')
        call write(trim(child)//'/fpm.toml', 'name = "child"')
        call write(trim(provider)//'/src/provider.f90', &
            'module provider_mod'//new_line('a')// &
            'use child_mod, only: child_value'//new_line('a')// &
            'integer, parameter :: provider_value = child_value + 5'// &
            new_line('a')//'end module')
        call write(trim(root)//'/app/flat_probe.f90', &
            'program flat_probe'//new_line('a')// &
            'use provider_mod, only: provider_value'//new_line('a')// &
            'print "(i0)", provider_value'//new_line('a')//'end program')
        call resolve_driver(driver)
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        do scenario = 1, 3
            manifest = 'name = "flat-git-project"'//new_line('a')// &
                '[dependencies]'//new_line('a')
            if (scenario == 2) then
                manifest = manifest// &
                    'provider = { path = "build/dependencies/provider" }'
            else
                manifest = manifest// &
                    'provider = { git = "https://invalid.invalid/provider", rev = "'// &
                    repeat('2', 40)//'" }'
            end if
            if (scenario == 3) manifest = manifest//new_line('a')// &
                'child = { git = "https://invalid.invalid/child", rev = "'// &
                repeat('1', 40)//'" }'
            call write(trim(root)//'/fpm.toml', manifest)
            call write(trim(child)//'/src/child.f90', &
                'module child_mod'//new_line('a')// &
                'integer, parameter :: child_value = 37'//new_line('a')//'end module')
            call input_inventory_discover(trim(root), none, captured, status, message)
            call require(status == 0, 'discover flat Git fixture: '//trim(message))
            call require(captured%complete, 'acquired Git closure is complete')
            context%input_inventory = captured
            call generation_capture(trim(root), trim(root)//'/generations', context, &
                generation, status, message)
            call require(status == 0, 'capture flat Git generation: '//trim(message))
            call execution_view_create(trim(root)//'/views', generation%identity, &
                'flat-git-oracle', 'build', generation%input_inventory, &
                .true., generation%input_inventory_complete, view, status, message, &
                candidate_bundle_root=trim(generation%root)//'/bundle')
            call require(status == 0, 'create flat Git view: '//trim(message))
            ! The answer must come from captured bytes, not this changed live leaf.
            call write(trim(child)//'/src/child.f90', &
                'module child_mod'//new_line('a')// &
                'integer, parameter :: child_value = 100'//new_line('a')//'end module')
            arguments = string_list_t()
            environment = string_list_t()
            call list_add(arguments, 'exec')
            call list_add(arguments, 'flat_probe')
            call list_add(environment, 'FO_JOBS=1')
            call run_fo(driver, arguments, trim(view%cwd), trim(root)//'/fo-cache', &
                result, environment, 120000)
            if (allocated(result%stderr)) then
                if (result%exit_code /= 0) print '(a)', result%stderr
            end if
            call require(result%exit_code == 0, 'build/run captured flat Git chain')
            call require(allocated(result%stdout), 'capture flat Git answer')
            read(result%stdout, *, iostat=status) answer
            call require(status == 0, 'read flat Git answer')
            call require(answer == 42, 'frozen transitive answer is 42')
            call execution_view_release(view, .false., release_status, message)
            call require(release_status == 0, 'release flat Git view: '//trim(message))
        end do
    end subroutine

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

    subroutine check_dependency_directory_inputs(parent)
        use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
        character(len=*), intent(in) :: parent
        character(len=4096) :: root, dependency, dependency_target, message
        character(len=1024) :: diagnostic
        character(len=4096) :: text
        character(len=:), allocatable :: driver
        type(input_declaration_t), allocatable :: declarations(:)
        type(input_inventory_t) :: captured
        type(execution_view_t) :: view
        type(generation_context_t) :: context
        type(generation_t) :: generation
        integer :: status, release_status

        interface
            integer(c_int) function c_symlink(target, path) bind(C, name='symlink')
                import :: c_char, c_int
                character(kind=c_char), intent(in) :: target(*), path(*)
            end function c_symlink
        end interface

        root = trim(parent)//'/dependency-directory-project'
        dependency = trim(parent)//'/dependency-directory-fortfront'
        dependency_target = trim(parent)//'/dependency-directory-fortfront-target'
        call fs_make_dir(trim(root)//'/src')
        call fs_make_dir(trim(dependency_target)//'/src')
        call fs_make_dir(trim(dependency_target)//'/examples/f90')
        call fs_make_dir(trim(dependency_target)//'/examples/lf')
        call require(c_symlink(trim(dependency_target)//c_null_char, &
            trim(dependency)//c_null_char) == 0, &
            'declared path dependency root is a symlink')
        call write(trim(root)//'/fpm.toml', &
            'name = "dependency-directory-project"'//new_line('a')// &
            '[dependencies]'//new_line('a')// &
            'fortfront = { path = "../dependency-directory-fortfront" }'// &
            new_line('a')// &
            '[[extra.fo.inputs]]'//new_line('a')// &
            'root = "dependency:fortfront"'//new_line('a')// &
            'path = "examples/f90"'//new_line('a')// &
            'kind = "directory"'//new_line('a')// &
            'role = "test-fixture"'//new_line('a')// &
            '[[extra.fo.inputs]]'//new_line('a')// &
            'root = "dependency:fortfront"'//new_line('a')// &
            'path = "examples/lf"'//new_line('a')// &
            'kind = "directory"'//new_line('a')// &
            'role = "test-fixture"')
        call write(trim(root)//'/src/main.f90', 'program directory_probe')
        call write(trim(dependency_target)//'/fpm.toml', 'name = "fortfront"')
        call write(trim(dependency_target)//'/examples/f90/probe.f90', &
            'program f90_probe')
        call write(trim(dependency_target)//'/examples/lf/probe.lf', &
            'program lf_probe')

        call input_inventory_declarations_from_config(trim(root), declarations, &
            status, diagnostic)
        call require(status == 0, 'parse dependency directory declarations: '// &
            trim(diagnostic))
        call require(size(declarations) == 2, &
            'both dependency directory declarations are parsed')
        call require(declarations(1)%root_alias == 'dependency:fortfront' .and. &
            declarations(1)%expected_kind == INPUT_DIRECTORY, &
            'manifest selects a dependency root and directory kind')
        call input_inventory_discover(trim(root), declarations, captured, status, &
            diagnostic)
        call require(status == 0 .and. captured%complete, &
            'discover dependency directory inputs: '//trim(diagnostic))
        call require(has_entry(captured, 'dependency:fortfront', &
            'examples/f90/probe.f90', 'test-fixture'), &
            'F90 directory files enter the dependency input inventory')
        call require(has_entry(captured, 'dependency:fortfront', &
            'examples/lf/probe.lf', 'test-fixture'), &
            'LF directory files enter the dependency input inventory')

        call resolve_driver(driver)
        context%driver_path = driver
        call cache_file_digest(driver, context%driver_digest)
        inquire(file=driver, size=context%driver_size)
        context%input_inventory = captured
        call generation_capture(trim(root), trim(root)//'/generations', context, &
            generation, status, diagnostic)
        call require(status == 0, 'capture dependency directory generation: '// &
            trim(diagnostic))
        call execution_view_create(trim(root)//'/views', generation%identity, &
            'dependency-directory-oracle', 'test-fixtures', &
            generation%input_inventory, .true., &
            generation%input_inventory_complete, view, status, diagnostic)
        call require(status == 0, 'materialize dependency directory inputs: '// &
            trim(diagnostic))
        call read_text_file(trim(view%cwd)// &
            '/.fo-inputs/dependency:fortfront/examples/f90/probe.f90', text)
        call require(index(text, 'program f90_probe') > 0, &
            'private view contains frozen dependency F90 bytes')
        call write(trim(dependency_target)//'/examples/f90/probe.f90', &
            'program changed_after_capture')
        call read_text_file(trim(view%cwd)// &
            '/.fo-inputs/dependency:fortfront/examples/f90/probe.f90', text)
        call require(index(text, 'program f90_probe') > 0, &
            'private view does not read later dependency edits')
        call input_inventory_discover(trim(root), declarations, runtime, status, &
            diagnostic)
        call require(status == 0 .and. runtime%complete, &
            'rediscover symlink dependency after target edit: '//trim(diagnostic))
        call require(runtime%digest /= captured%digest, &
            'target edits through declared symlink change inventory identity')
        call require(has_entry(runtime, 'dependency:fortfront', &
            'examples/f90/probe.f90', 'test-fixture'), &
            'symlink dependency provenance remains the declared alias')
        call execution_view_release(view, .false., release_status, diagnostic)
        call require(release_status == 0, &
            'release dependency directory execution view: '//trim(diagnostic))
        call fs_remove_tree(trim(root))
        call fs_remove_tree(trim(dependency_target))
    end subroutine check_dependency_directory_inputs

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
