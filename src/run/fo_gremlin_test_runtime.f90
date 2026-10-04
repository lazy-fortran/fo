module fo_gremlin_test_runtime
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: HASH_LEN, cache_digest
    use fo_driver, only: driver_pin_t, driver_pin_built_file, driver_pin_existing
    use fo_fpm_config, only: fpm_config_t, fpm_config_parse
    use fo_build_tree, only: native_output_dir
    use fo_fs, only: fs_rename
    use fo_util, only: make_sibling_tmpfile, delete_tmpfile, read_text_file
    use fo_gremlin_request, only: gremlin_json_field
    use fx_json_build, only: json_escape_string
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096

    type, public :: test_runtime_t
        logical :: enabled = .false.
        character(len=128) :: target = ''
        type(driver_pin_t) :: pin
    end type test_runtime_t

    public :: test_runtime_build, test_runtime_load, test_runtime_action_key
    public :: test_runtime_environment

contains

    subroutine test_runtime_build(project_dir, state_dir, generation, runtime, &
            ierr, message)
        character(len=*), intent(in) :: project_dir, state_dir, generation
        type(test_runtime_t), intent(out) :: runtime
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(fpm_config_t) :: config
        character(len=:), allocatable :: executable
        integer :: status
        logical :: exists

        runtime = test_runtime_t()
        ierr = 0
        message = ''
        call fpm_config_parse(project_dir, config, status)
        if (status /= 0) then
            ierr = 1
            message = 'cannot read the declared Gremlin test driver target'
            if (len_trim(config%test_driver_target_error) > 0) &
                message = trim(config%test_driver_target_error)
            return
        end if
        if (len_trim(config%test_driver_target) == 0) return
        runtime%enabled = .true.
        runtime%target = config%test_driver_target
        executable = native_output_dir(project_dir, '', 'bin')//'/'// &
            trim(runtime%target)
        inquire(file=trim(executable), exist=exists)
        if (.not. exists) then
            ierr = 1
            message = 'declared Gremlin test driver was not built: '// &
                trim(runtime%target)
            return
        end if
        call driver_pin_built_file(state_dir, trim(executable), runtime%pin, &
            ierr, message)
        if (ierr /= 0) return
        if (index(trim(runtime%pin%path), ';') > 0) then
            ierr = 1
            message = 'test driver path contains an unsupported environment separator'
            return
        end if
        call publish_binding(state_dir, generation, runtime, ierr, message)
    end subroutine test_runtime_build

    subroutine test_runtime_load(project_dir, state_dir, generation, runtime, &
            ierr, message)
        character(len=*), intent(in) :: project_dir, state_dir, generation
        type(test_runtime_t), intent(out) :: runtime
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        type(fpm_config_t) :: config
        integer :: status

        runtime = test_runtime_t()
        ierr = 0
        message = ''
        call fpm_config_parse(project_dir, config, status)
        if (status /= 0) then
            ierr = 1
            message = 'cannot read the declared Gremlin test driver target'
            if (len_trim(config%test_driver_target_error) > 0) &
                message = trim(config%test_driver_target_error)
            return
        end if
        if (len_trim(config%test_driver_target) == 0) return
        runtime%enabled = .true.
        runtime%target = config%test_driver_target
        call load_binding(state_dir, generation, runtime, ierr, message)
    end subroutine test_runtime_load

    function test_runtime_action_key(generation, case_id, policy_key, runtime) &
            result(key)
        character(len=*), intent(in) :: generation, case_id, policy_key
        type(test_runtime_t), intent(in) :: runtime
        character(len=HASH_LEN) :: key

        character(len=32) :: size_text
        character(len=256) :: parts(7)

        write(size_text, '(i0)') runtime%pin%size
        parts(1) = 'fo-gremlin-test-action-v1'
        parts(2) = generation
        parts(3) = case_id
        parts(4) = policy_key
        parts(5) = runtime%target
        parts(6) = runtime%pin%digest
        parts(7) = trim(size_text)
        key = cache_digest(parts, size(parts))
    end function test_runtime_action_key

    subroutine test_runtime_environment(runtime, assignments, ierr, message)
        type(test_runtime_t), intent(in) :: runtime
        character(len=*), intent(out) :: assignments, message
        integer, intent(out) :: ierr

        assignments = ''
        message = ''
        ierr = 0
        if (.not. runtime%enabled) return
        if (index(trim(runtime%pin%path), ';') > 0) then
            ierr = 1
            message = 'test driver path contains an unsupported environment separator'
            return
        end if
        assignments = 'FO='//trim(runtime%pin%path)//';FO_BIN='// &
            trim(runtime%pin%path)
    end subroutine test_runtime_environment

    subroutine publish_binding(state_dir, generation, runtime, ierr, message)
        character(len=*), intent(in) :: state_dir, generation
        type(test_runtime_t), intent(in) :: runtime
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: path, temporary
        character(len=HASH_LEN) :: existing_digest
        character(len=64) :: size_text
        character(len=PATH_LEN) :: existing_path
        character(len=128) :: existing_target
        character(len=HASH_LEN) :: existing_generation
        integer(int64) :: existing_size
        integer :: unit, ios, rename_status
        logical :: exists

        ierr = 1
        message = ''
        call binding_file_path(state_dir, generation, path, ierr, message)
        if (ierr /= 0) return
        inquire(file=trim(path), exist=exists)
        if (exists) then
            call read_binding(path, existing_generation, existing_target, &
                existing_path, existing_digest, existing_size, ierr, message)
            if (ierr /= 0) return
            if (trim(existing_generation) /= trim(generation) .or. &
                    trim(existing_target) /= trim(runtime%target) .or. &
                    trim(existing_digest) /= trim(runtime%pin%digest) .or. &
                    existing_size /= runtime%pin%size .or. &
                    trim(existing_path) /= trim(runtime%pin%path)) then
                ierr = 1
                message = 'generation already has a different test driver binding'
                return
            end if
            ierr = 0
            return
        end if

        write(size_text, '(i0)') runtime%pin%size
        call make_sibling_tmpfile(trim(path), temporary)
        open(newunit=unit, file=trim(temporary), status='new', action='write', &
            iostat=ios)
        if (ios /= 0) then
            message = 'cannot stage the generation test driver binding'
            return
        end if
        write(unit, '(a)', iostat=ios) '{"schema":"fo-gremlin-test-runtime-v1",'// &
            '"generation":"'//trim(generation)//'","target":"'// &
            trim(json_escape_string(trim(runtime%target)))//'","runtime_path":"'// &
            trim(json_escape_string(trim(runtime%pin%path)))//'","runtime_digest":"'// &
            trim(runtime%pin%digest)//'","runtime_size":'//trim(size_text)//'}'
        close(unit, iostat=rename_status)
        if (ios == 0 .and. rename_status == 0) then
            rename_status = fs_rename(trim(temporary), trim(path))
        end if
        call delete_tmpfile(trim(temporary))
        if (ios /= 0 .or. rename_status /= 0) then
            message = 'cannot publish the generation test driver binding'
            return
        end if
        ierr = 0
    end subroutine publish_binding

    subroutine load_binding(state_dir, generation, runtime, ierr, message)
        character(len=*), intent(in) :: state_dir, generation
        type(test_runtime_t), intent(inout) :: runtime
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN) :: path, ignored_path
        character(len=128) :: target
        character(len=HASH_LEN) :: stored_generation, digest
        integer(int64) :: size

        ierr = 1
        message = ''
        call binding_file_path(state_dir, generation, path, ierr, message)
        if (ierr /= 0) return
        call read_binding(path, stored_generation, target, ignored_path, digest, &
            size, ierr, message)
        if (ierr /= 0) return
        if (trim(stored_generation) /= trim(generation) .or. &
                trim(target) /= trim(runtime%target)) then
            ierr = 1
            message = 'stored generation test driver binding does not match the request'
            return
        end if
        call driver_pin_existing(state_dir, trim(digest), size, runtime%pin, &
            ierr, message)
        if (ierr /= 0) return
        if (trim(runtime%pin%path) /= trim(ignored_path)) then
            ierr = 1
            message = 'stored generation test driver path does not match its digest pin'
            return
        end if
        ierr = 0
    end subroutine load_binding

    subroutine read_binding(path, generation, target, runtime_path, digest, size, &
            ierr, message)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: generation, target, runtime_path, digest
        integer(int64), intent(out) :: size
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        character(len=PATH_LEN*2+1024) :: text
        character(len=64) :: schema, size_text
        integer :: ios

        generation = ''
        target = ''
        runtime_path = ''
        digest = ''
        size = 0_int64
        ierr = 1
        message = 'stored generation test driver binding is missing or invalid'
        call read_text_file(path, text)
        call gremlin_json_field(trim(text), 'schema', schema)
        call gremlin_json_field(trim(text), 'generation', generation)
        call gremlin_json_field(trim(text), 'target', target)
        call gremlin_json_field(trim(text), 'runtime_path', runtime_path)
        call gremlin_json_field(trim(text), 'runtime_digest', digest)
        call gremlin_json_field(trim(text), 'runtime_size', size_text)
        if (trim(schema) /= 'fo-gremlin-test-runtime-v1' .or. &
                len_trim(generation) /= HASH_LEN .or. len_trim(target) == 0 .or. &
                len_trim(runtime_path) == 0 .or. len_trim(digest) /= HASH_LEN) return
        read(size_text, *, iostat=ios) size
        if (ios /= 0 .or. size <= 0_int64) return
        ierr = 0
        message = ''
    end subroutine read_binding

    subroutine binding_file_path(state_dir, generation, path, ierr, message)
        character(len=*), intent(in) :: state_dir, generation
        character(len=*), intent(out) :: path, message
        integer, intent(out) :: ierr

        ierr = 1
        message = 'generation test driver binding requires a valid generation ID'
        path = ''
        if (len_trim(generation) /= HASH_LEN) return
        if (len_trim(state_dir) + len('/test-runtime-') + HASH_LEN + &
                len('.json') >= len(path)) then
            message = 'generation test driver binding path is too long'
            return
        end if
        path = trim(state_dir)//'/test-runtime-'//trim(generation)//'.json'
        ierr = 0
        message = ''
    end subroutine binding_file_path

end module fo_gremlin_test_runtime
