module fo_cmake_native_snapshot
    use fo_cmake_native_model, only: cm_plan_t, cm_variable_t
    use fo_cmake_context, only: cmake_context_t
    use fo_compiler_dialect, only: selected_compiler_command
    use fo_util, only: read_text_file_alloc
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
                         json_object_end, json_key_string, json_key, json_array_start, &
                             json_array_end, json_value_string, json_to_string
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init_strict, &
               json_parser_next, JSON_OBJECT_START, JSON_OBJECT_END, JSON_ARRAY_START, &
                    JSON_ARRAY_END, JSON_KEY, JSON_STRING, JSON_ERROR, JSON_END_OF_INPUT
    implicit none
    private
    public :: native_snapshot_read, native_snapshot_write
contains
    subroutine native_snapshot_write(context, plan, text)
        type(cmake_context_t), intent(in) :: context
        type(cm_plan_t), intent(in) :: plan
        character(:), allocatable, intent(out) :: text
        type(json_builder_t) :: json
        integer :: i
        json = json_new()
        call json_object_start(json)
        call json_key_string(json, 'schema', 'fo-native-cmake-v1')
        call json_key_string(json, 'build_directory', context%build_root)
        call json_key_string(json, 'configuration', context%configuration)
        call json_key_string(json, 'generator', context%generator)
        call json_key_string(json, 'profile', context%profile)
        call json_key_string(json, 'compiler', trim(selected_compiler_command()))
        call json_key(json, 'configure_arguments')
        call json_array_start(json)
        do i = 1, size(context%extra_args)
            call json_value_string(json, trim(context%extra_args(i)))
        end do
        call json_array_end(json)
        call json_key(json, 'build_targets')
        call json_array_start(json)
        do i = 1, size(context%build_targets)
            call json_value_string(json, trim(context%build_targets(i)))
        end do
        call json_array_end(json)
        call json_key(json, 'environment')
        call json_object_start(json)
        do i = 1, size(plan%environment)
            call json_key_string(json, plan%environment(i)%name, &
                                 plan%environment(i)%value)
        end do
        call json_object_end(json)
        call json_object_end(json)
        text = json_to_string(json)
    end subroutine

    subroutine native_snapshot_read(context, plan)
        type(cmake_context_t), intent(inout) :: context
        type(cm_plan_t), intent(inout) :: plan
        type(json_parser_t) :: parser
        type(json_event_t) :: event
        character(:), allocatable :: text, key, section
        integer :: ierr, depth, n
        logical :: exists, schema
      inquire (file=context%source_root//'/.fo-cmake/native-context.json', exist=exists)
        if (.not. exists) return
        call read_text_file_alloc(context%source_root// &
                                  '/.fo-cmake/native-context.json', text, ierr)
        if (ierr /= 0) then
            plan%error = 'native CMake: frozen context is unreadable'
            return
        end if
        call json_parser_init_strict(parser, text)
        depth = 0
        section = ''
        key = ''
        schema = .false.
        deallocate (context%extra_args, context%build_targets)
       allocate (character(len=4096) :: context%extra_args(0), context%build_targets(0))
        do
            call json_parser_next(parser, event)
            select case (event%event_type)
            case (JSON_OBJECT_START, JSON_ARRAY_START)
                depth = depth + 1
                if (depth == 2) section = key
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
                if (depth == 1) section = ''
            case (JSON_KEY)
                key = event%string_val
            case (JSON_STRING)
                if (depth == 1) then
                    select case (key)
                    case ('schema')
                        schema = event%string_val == 'fo-native-cmake-v1'
                    case ('build_directory')
                        context%build_root = event%string_val
                    case ('configuration')
                        context%configuration = event%string_val
                    case ('generator')
                        context%generator = event%string_val
                    case ('profile')
                        context%profile = event%string_val
                    case ('compiler')
                        if (event%string_val /= trim(selected_compiler_command())) then
                            plan%error = 'native CMake: frozen compiler choice differs'
                            return
                        end if
                    case default
                        plan%error = 'native CMake: unsupported frozen context field'
                        return
                    end select
                else if (depth == 2) then
                    select case (section)
                    case ('environment')
                        plan%environment = [plan%environment, &
                                            cm_variable_t(key, event%string_val)]
                    case ('configure_arguments', 'build_targets')
                        if (len(event%string_val) > 4096) then
                            plan%error = &
                                'native CMake: frozen argument exceeds capacity'
                            return
                        end if
                        if (section == 'configure_arguments') then
                            context%extra_args = [context%extra_args, repeat(' ', 4096)]
                            n = size(context%extra_args)
                            context%extra_args(n) = event%string_val
                        else
                            context%build_targets = [context%build_targets, &
                                repeat(' ', 4096)]
                            n = size(context%build_targets)
                            context%build_targets(n) = event%string_val
                        end if
                    case default
                        plan%error = 'native CMake: unsupported frozen context section'
                        return
                    end select
                end if
            case (JSON_END_OF_INPUT)
                exit
            case default
                plan%error = 'native CMake: invalid frozen context JSON'
                return
            end select
        end do
        if (.not. schema) then
            plan%error = 'native CMake: unsupported frozen context schema'
            return
        end if
        if (len(context%build_root) == 0) then
            plan%error = 'native CMake: frozen build directory is empty'
            return
        end if
        context%configure_preset = ''
        context%build_preset = ''
        context%test_preset = ''
        context%multi_config = .false.
        context%valid = .true.
        if (allocated(context%error)) deallocate (context%error)
        plan%frozen = .true.
    end subroutine
end module fo_cmake_native_snapshot
