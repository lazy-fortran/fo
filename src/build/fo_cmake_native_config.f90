module fo_cmake_native_config
    use fo_cmake_native_model, only: cm_plan_t, cm_word_t, cm_target_t, cm_test_t, &
                   cm_variable_t, cm_set, cm_get, cm_add, cm_words, cm_lower, cm_path, &
                                     cm_expand, cm_next
    use fo_cmake_context, only: cmake_context_t, cmake_context_build_path
    use fo_compiler_dialect, only: compiler_dialect_t, compiler_dialect, &
                                   selected_compiler_command, COMPILER_GFORTRAN
    use fo_util, only: read_text_file_alloc, make_tmpfile, delete_tmpfile
    use fo_process, only: process_run_argv_logged, argv_push
    implicit none
    private
    public :: native_cmake_configure, native_cmake_selected
    public :: target_index
contains
    logical function native_cmake_selected()
        character(len=16) :: value
        integer :: status
        call get_environment_variable('FO_CMAKE_NATIVE', value, status=status)
        native_cmake_selected = status == 0 .and. trim(value) == '1'
    end function

    subroutine native_cmake_configure(context, plan)
        type(cmake_context_t), intent(in) :: context
        type(cm_plan_t), intent(out) :: plan
        type(compiler_dialect_t) :: dialect
        character(:), allocatable :: value, key
        integer :: i, at
        plan%root = context%source_root
        plan%build = cmake_context_build_path(context)
        plan%configuration = context%configuration
        plan%error = ''
        plan%project = ''
        allocate (plan%targets(0), plan%tests(0), plan%compile_flags(0), &
                  plan%link_flags(0), plan%includes(0), plan%inputs(0))
        if (.not. context%valid) then
            plan%error = context%error
            return
        end if
        if (len(context%configure_preset) + len(context%build_preset) + &
            len(context%test_preset) > 0) then
            plan%error = 'native CMake: presets are not supported by this initial slice'
            return
        end if
        if (context%multi_config) then
            plan%error = 'native CMake: multi-configuration generators unsupported'
            return
        end if
        select case (context%generator)
        case ('', 'Ninja', 'Unix Makefiles')
        case default
            plan%error = 'native CMake: unsupported generator '//context%generator
            return
        end select
        dialect = compiler_dialect(selected_compiler_command())
        if (dialect%kind /= COMPILER_GFORTRAN) then
            plan%error = 'native CMake: this profile requires GNU Fortran'
            return
        end if
        call cm_set(plan, 'CMAKE_SOURCE_DIR', plan%root)
        call cm_set(plan, 'CMAKE_BINARY_DIR', plan%build)
        call cm_set(plan, 'CMAKE_BUILD_TYPE', plan%configuration)
        call cm_set(plan, 'CMAKE_Fortran_FLAGS', cm_expand(plan, '$ENV{FFLAGS}'))
        call cm_set(plan, 'CMAKE_Fortran_FLAGS_DEBUG', '-g')
        call cm_set(plan, 'CMAKE_Fortran_FLAGS_RELEASE', '-O3')
        call cm_set(plan, 'CMAKE_Fortran_FLAGS_RELWITHDEBINFO', '-O2 -g')
        call cm_set(plan, 'CMAKE_Fortran_FLAGS_MINSIZEREL', '-Os')
        call host_variables(plan)
        do i = 1, size(context%extra_args)
            value = trim(context%extra_args(i))
            at = index(value, '=')
            if (index(value, '-D') /= 1 .or. at <= 3) then
                plan%error = 'native CMake: unsupported configure argument '//value
                return
            end if
            key = value(3:at - 1)
            if (index(key, ':') > 0) key = key(:index(key, ':') - 1)
            if (index(key, 'CMAKE_') == 1) then
                if (key /= 'CMAKE_BUILD_TYPE' .and. &
                    index(key, 'CMAKE_Fortran_FLAGS') /= 1) then
                   plan%error = 'native CMake: unsupported configuration variable '//key
                    return
                end if
            end if
            if (key == 'CMAKE_TOOLCHAIN_FILE' .or. &
                index(key, 'CMAKE_') == 1 .and. index(key, '_COMPILER') > 0) then
                plan%error = 'native CMake: explicit toolchain/compiler unsupported'
                return
            end if
            call cm_set(plan, key, value(at + 1:))
        end do
        call load_directory(plan, plan%root, plan%build, 0)
        plan%configuration = cm_get(plan, 'CMAKE_BUILD_TYPE')
    end subroutine

    logical function condition(plan, words) result(value)
        type(cm_plan_t), intent(inout) :: plan
        type(cm_word_t), intent(in) :: words(:)
        integer :: i
        logical :: next, invert
        character(:), allocatable :: operation, left, right
        logical :: has_and, has_or
        has_and = .false.
        has_or = .false.
        do i = 1, size(words)
            if (cm_lower(words(i)%text) == 'and') has_and = .true.
            if (cm_lower(words(i)%text) == 'or') has_or = .true.
        end do
        if (has_and .and. has_or) then
            plan%error = 'native CMake: mixed AND/OR conditions unsupported'
            value = .false.
            return
        end if
        value = .false.
        operation = 'or'
        i = 1
        do while (i <= size(words))
            invert = .false.
            if (cm_lower(words(i)%text) == 'not') then
                invert = .true.
                i = i + 1
            end if
            if (i > size(words)) then
                plan%error = 'native CMake: incomplete condition'
                return
            end if
            left = words(i)%text
            if (index(left, '(') > 0 .or. index(left, ')') > 0) then
                plan%error = 'native CMake: grouped conditions unsupported'
                return
            end if
            i = i + 1
            next = truth(plan, left)
            if (i <= size(words)) then
                if (cm_lower(words(i)%text) == 'strequal') then
                    i = i + 1
                    if (i > size(words)) then
                        plan%error = 'native CMake: missing STREQUAL operand'
                        return
                    end if
                    right = words(i)%text
                    if (len(cm_get(plan, left)) > 0) left = cm_get(plan, left)
                    if (len(cm_get(plan, right)) > 0) right = cm_get(plan, right)
                    next = left == right
                    i = i + 1
                end if
            end if
            if (invert) next = .not. next
            if (operation == 'or') value = value .or. next
            if (operation == 'and') value = value .and. next
            if (i > size(words)) exit
            operation = cm_lower(words(i)%text)
            if (operation /= 'or' .and. operation /= 'and') then
                plan%error = 'native CMake: unsupported condition '//operation
                return
            end if
            i = i + 1
        end do
    end function

    logical function truth(plan, text) result(value)
        type(cm_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: text
        character(:), allocatable :: lowered, variable
        integer :: number, status
        lowered = cm_lower(text)
        variable = cm_get(plan, text)
        if (len(variable) > 0) lowered = cm_lower(variable)
        value = lowered == 'true' .or. lowered == 'on' .or. &
                lowered == 'yes' .or. lowered == '1'
        read (lowered, *, iostat=status) number
        if (status == 0) value = number /= 0
    end function

    recursive subroutine execute_command(plan, command, words, directory, binary, depth)
        type(cm_plan_t), intent(inout) :: plan
        character(len=*), intent(in) :: command, directory, binary
        type(cm_word_t), intent(in) :: words(:)
        integer, intent(in) :: depth
        type(cm_target_t) :: target
        type(cm_test_t) :: test
        character(:), allocatable :: value, kind, child_binary
        integer :: i, at, first, count
        count = size(words)
        select case (command)
        case ('cmake_minimum_required', 'enable_testing')
            return
        case ('message')
            if (count > 0) then
                if (words(1)%text == 'FATAL_ERROR' .or. &
                    words(1)%text == 'SEND_ERROR') &
                    plan%error = 'native CMake: project configuration error'
            end if
        case ('project')
            if (count == 0) then
                plan%error = 'native CMake: project name is missing'
                return
            end if
            plan%project = words(1)%text
            i = 2
            do while (i <= count)
                select case (words(i)%text)
                case ('VERSION')
                    if (i == count) then
                        plan%error = 'native CMake: missing project version'
                        return
                    end if
                    call cm_set(plan, 'PROJECT_VERSION', words(i + 1)%text)
                    call cm_set(plan, plan%project//'_VERSION', words(i + 1)%text)
                    i = i + 2
                    if (i > count + 1) then
                        plan%error = 'native CMake: missing project version'
                        return
                    end if
                case ('LANGUAGES')
                    i = i + 1
                case ('Fortran', 'C', 'CXX', 'NONE')
                    i = i + 1
                case default
               plan%error = 'native CMake: unsupported project argument '//words(i)%text
                    return
                end select
            end do
            call cm_set(plan, 'PROJECT_NAME', plan%project)
            call cm_set(plan, 'PROJECT_SOURCE_DIR', directory)
            call cm_set(plan, 'PROJECT_BINARY_DIR', binary)
        case ('enable_language')
            do i = 1, count
                if (words(i)%text == 'Fortran') cycle
               plan%error = 'native CMake: unsupported enabled language '//words(i)%text
            end do
        case ('set')
            if (count < 2) then
                plan%error = 'native CMake: set requires a value'
                return
            end if
            if (index(words(1)%text, '_OUTPUT_DIRECTORY') > 0 .or. &
                words(1)%text == 'CMAKE_TOOLCHAIN_FILE') then
         plan%error = 'native CMake: unsupported configuration variable '//words(1)%text
                return
            end if
            if (index(words(1)%text, 'CMAKE_') == 1) then
                if (words(1)%text /= 'CMAKE_BUILD_TYPE' .and. &
                    index(words(1)%text, 'CMAKE_Fortran_FLAGS') /= 1) then
                    plan%error = 'native CMake: unsupported configuration variable '// &
                                 words(1)%text
                    return
                end if
            end if
            value = words(2)%text
            do i = 3, count
                if (words(i)%text == 'CACHE') then
                    if (len(cm_get(plan, words(1)%text)) > 0) return
                    exit
                end if
                if (words(i)%text == 'PARENT_SCOPE') then
                    plan%error = 'native CMake: PARENT_SCOPE is unsupported'
                    return
                end if
                value = value//';'//words(i)%text
            end do
            call cm_set(plan, words(1)%text, value)
        case ('option')
            if (count /= 3) then
                plan%error = 'native CMake: option requires name/help/default'
                return
            end if
            if (len(cm_get(plan, words(1)%text)) == 0) &
                call cm_set(plan, words(1)%text, words(3)%text)
        case ('include_directories', 'add_compile_options', 'add_link_options')
            if (depth /= 0) then
                plan%error = 'native CMake: scoped subdirectory flags unsupported'
                return
            end if
            do i = 1, count
                if (index(words(i)%text, '$<') > 0) then
                    plan%error = 'native CMake: generator expressions unsupported'
                    return
                end if
                select case (command)
                case ('include_directories')
                    call cm_add(plan%includes, cm_path(directory, words(i)%text))
                case ('add_compile_options')
                    call cm_add(plan%compile_flags, words(i)%text)
                case ('add_link_options')
                    call cm_add(plan%link_flags, words(i)%text)
                end select
            end do
        case ('find_package')
            call find_provider(plan, words)
        case ('add_subdirectory')
            if (count < 1 .or. count > 2) then
                plan%error = 'native CMake: unsupported add_subdirectory arguments'
                return
            end if
            child_binary = cm_path(binary, words(1)%text)
            if (count == 2) child_binary = cm_path(binary, words(2)%text)
            call load_directory(plan, cm_path(directory, words(1)%text), &
                                child_binary, depth + 1)
        case ('add_library', 'add_executable')
            if (count < 2) then
                plan%error = 'native CMake: target declaration has no sources'
                return
            end if
            target%name = words(1)%text
            if (target_index(plan, target%name) > 0) then
                plan%error = 'native CMake: duplicate target '//target%name
                return
            end if
            target%kind = 'EXECUTABLE'
            first = 2
            if (command == 'add_library') then
                target%kind = words(2)%text
                if (target%kind /= 'STATIC') then
                   plan%error = 'native CMake: only explicit STATIC libraries supported'
                    return
                end if
                first = 3
            end if
            if (count < first) then
                plan%error = 'native CMake: target declaration has no sources'
                return
            end if
            target%source_dir = directory
            target%binary_dir = binary
            target%module_dir = ''
            allocate (target%sources(0), target%links(0))
            do i = first, count
                if (index(words(i)%text, '$<') > 0) then
                    plan%error = 'native CMake: generator expression source unsupported'
                    return
                end if
                call cm_add(target%sources, cm_path(directory, words(i)%text))
            end do
            plan%targets = [plan%targets, target]
        case ('target_link_libraries')
            if (count < 2) then
                plan%error = 'native CMake: target link declaration incomplete'
                return
            end if
            at = target_index(plan, words(1)%text)
            if (at == 0) then
                plan%error = 'native CMake: link target does not exist'
                return
            end if
            do i = 2, count
                kind = words(i)%text
                if (kind == 'PRIVATE' .or. kind == 'PUBLIC') cycle
                if (kind == 'INTERFACE' .or. index(kind, '::') > 0) then
                    plan%error = 'native CMake: interface/imported links unsupported'
                    return
                end if
                call cm_add(plan%targets(at)%links, kind)
            end do
        case ('set_target_properties', 'target_include_directories')
            call target_properties(plan, command, words)
        case ('add_test')
            if (count < 4) then
                plan%error = 'native CMake: named test declaration incomplete'
                return
            end if
            if (words(1)%text /= 'NAME' .or. words(3)%text /= 'COMMAND') then
                plan%error = 'native CMake: only named target tests supported'
                return
            end if
            test%name = words(2)%text
            test%target = words(4)%text
            test%cwd = binary
            allocate (test%args(0))
            do i = 5, count
                call cm_add(test%args, words(i)%text)
            end do
            plan%tests = [plan%tests, test]
        case default
            plan%error = 'native CMake: unsupported command '//command
        end select
    end subroutine

    integer function target_index(plan, name) result(at)
        type(cm_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: name
        integer :: i
        at = 0
        do i = 1, size(plan%targets)
            if (plan%targets(i)%name == name) at = i
        end do
    end function

    subroutine target_properties(plan, command, words)
        type(cm_plan_t), intent(inout) :: plan
        character(len=*), intent(in) :: command
        type(cm_word_t), intent(in) :: words(:)
        integer :: at, i
        if (size(words) < 3) then
            plan%error = 'native CMake: incomplete target property'
            return
        end if
        at = target_index(plan, words(1)%text)
        if (at == 0) then
            plan%error = 'native CMake: property target does not exist'
            return
        end if
        if (command == 'set_target_properties') then
            if (size(words) /= 4) then
                plan%error = 'native CMake: unsupported target property arity'
                return
            end if
            if (words(2)%text /= 'PROPERTIES' .or. &
                words(3)%text /= 'Fortran_MODULE_DIRECTORY') then
                plan%error = 'native CMake: unsupported target property'
                return
            end if
            plan%targets(at)%module_dir = cm_path( &
                                          plan%targets(at)%binary_dir, words(4)%text)
        else
            if (words(2)%text /= 'PUBLIC' .and. words(2)%text /= 'PRIVATE') then
                plan%error = 'native CMake: unsupported include visibility'
                return
            end if
            do i = 3, size(words)
                if (words(i)%text == plan%targets(at)%module_dir) then
                    plan%targets(at)%module_exported = words(2)%text == 'PUBLIC'
                    cycle
                end if
                plan%error = 'native CMake: per-target authored includes unsupported'
            end do
        end if
    end subroutine

    subroutine find_provider(plan, words)
        type(cm_plan_t), intent(inout) :: plan
        type(cm_word_t), intent(in) :: words(:)
        character(:), allocatable :: package, module, packed, value
        character(len=512) :: log
        integer :: count, exitcode, i
        type(cm_word_t), allocatable :: flags(:)
        if (size(words) < 1) then
            plan%error = 'native CMake: package name is missing'
            return
        end if
        package = words(1)%text
        module = cm_lower(package)
        if (module /= 'blas' .and. module /= 'lapack' .and. module /= 'netcdf') then
            plan%error = 'native CMake: unsupported package provider '//package
            return
        end if
        if (size(words) > 2) then
            plan%error = 'native CMake: package options/components unsupported'
            return
        end if
        if (size(words) == 2) then
            if (words(2)%text /= 'REQUIRED') then
                plan%error = 'native CMake: package options/components unsupported'
                return
            end if
        end if
        call make_tmpfile('fo-native-cmake-provider', log)
        count = 0
        call argv_push(packed, count, 'pkg-config')
        call argv_push(packed, count, '--cflags-only-I')
        call argv_push(packed, count, module)
        call process_run_argv_logged('', packed, count, log, .false., 10, exitcode)
        if (exitcode /= 0) then
            plan%error = 'native CMake: unavailable pkg-config provider '//package
            call delete_tmpfile(log)
            return
        end if
        call read_text_file_alloc(log, value, exitcode)
        if (exitcode /= 0) then
            plan%error = 'native CMake: unreadable package provider receipt '//package
            call delete_tmpfile(log)
            return
        end if
        flags = cm_words(value)
        do i = 1, size(flags)
            if (index(flags(i)%text, '-I') /= 1) cycle
            call cm_add(plan%includes, flags(i)%text(3:))
        end do
        call cm_set(plan, package//'_FOUND', 'TRUE')
        call delete_tmpfile(log)
    end subroutine

    subroutine host_variables(plan)
        type(cm_plan_t), intent(inout) :: plan
        character(len=128) :: value
        integer :: status
        call get_environment_variable('OS', value, status=status)
        call cm_set(plan, 'WIN32', 'FALSE')
        call cm_set(plan, 'UNIX', 'TRUE')
        call cm_set(plan, 'APPLE', 'FALSE')
        if (status == 0 .and. trim(value) == 'Windows_NT') then
            call cm_set(plan, 'WIN32', 'TRUE')
            call cm_set(plan, 'UNIX', 'FALSE')
            call get_environment_variable('PROCESSOR_ARCHITECTURE', value)
            call cm_set(plan, 'CMAKE_HOST_SYSTEM_PROCESSOR', trim(value))
        else
            call host_uname(plan)
        end if
    end subroutine
    subroutine host_uname(plan)
        type(cm_plan_t), intent(inout) :: plan
        character(len=512) :: log
        character(:), allocatable :: packed, value
        integer :: count, exitcode
        call make_tmpfile('fo-native-cmake-host', log)
        count = 0
        call argv_push(packed, count, 'uname')
        call argv_push(packed, count, '-sm')
        call process_run_argv_logged('', packed, count, log, .false., 10, exitcode)
        value = ''
        if (exitcode == 0) call read_text_file_alloc(log, value, exitcode)
        if (index(value, 'Darwin ') == 1) call cm_set(plan, 'APPLE', 'TRUE')
        if (index(value, ' ') > 0) then
            value = trim(value(index(value, ' ') + 1:))
            if (index(value, achar(10)) > 0) value = value(:index(value, achar(10)) - 1)
        end if
        call cm_set(plan, 'CMAKE_HOST_SYSTEM_PROCESSOR', value)
        call delete_tmpfile(log)
    end subroutine

    recursive subroutine load_directory(plan, directory, binary, depth)
        type(cm_plan_t), intent(inout) :: plan
        character(len=*), intent(in) :: directory, binary
        integer, intent(in) :: depth
        character(:), allocatable :: text, command, error, old_source, old_binary
        type(cm_word_t), allocatable :: words(:)
        type(cm_variable_t), allocatable :: old_variables(:)
        logical :: active(64), taken(64), parents(64), branch
        integer :: cursor, level, i, ierr
        if (depth > 32) then
            plan%error = 'native CMake: subdirectory recursion limit exceeded'
            return
        end if
        old_source = cm_get(plan, 'CMAKE_CURRENT_SOURCE_DIR')
        old_binary = cm_get(plan, 'CMAKE_CURRENT_BINARY_DIR')
        if (depth > 0) old_variables = plan%variables
        call cm_set(plan, 'CMAKE_CURRENT_SOURCE_DIR', directory)
        call cm_set(plan, 'CMAKE_CURRENT_BINARY_DIR', binary)
        call cm_add(plan%inputs, directory//'/CMakeLists.txt')
        call read_text_file_alloc(directory//'/CMakeLists.txt', text, ierr)
        if (ierr /= 0) then
           plan%error = 'native CMake: source configuration is unavailable: '//directory
            return
        end if
        cursor = 1
        level = 1
        active = .false.
        active(1) = .true.
        taken = .false.
        parents = .false.
        do while (cursor <= len(text) .and. len(plan%error) == 0)
            call cm_next(text, cursor, command, words, error)
            if (len(command) == 0) exit
            if (len(error) > 0) then
                plan%error = error
                exit
            end if
            do i = 1, size(words)
                words(i)%text = cm_expand(plan, words(i)%text)
                if (index(words(i)%text, ';') > 0) then
                    plan%error = 'native CMake: list-variable expansion unsupported'
                    exit
                end if
            end do
            if (len(plan%error) > 0) exit
            select case (command)
            case ('if')
                if (level == size(active)) then
                    plan%error = 'native CMake: condition nesting limit exceeded'
                    exit
                end if
                branch = .false.
                if (active(level)) branch = condition(plan, words)
                parents(level + 1) = active(level)
                level = level + 1
                active(level) = parents(level) .and. branch
                taken(level) = branch
            case ('else', 'elseif')
                if (level == 1) then
                    plan%error = 'native CMake: unmatched else/elseif'
                    exit
                end if
                branch = .true.
                if (command == 'elseif') then
                    branch = .false.
                    if (parents(level) .and. .not. taken(level)) &
                        branch = condition(plan, words)
                end if
                active(level) = parents(level) .and. .not. taken(level) .and. branch
                taken(level) = taken(level) .or. branch
            case ('endif')
                if (level == 1) then
                    plan%error = 'native CMake: unmatched endif'
                    exit
                end if
                level = level - 1
            case default
                if (active(level)) call execute_command(plan, command, words, &
                                                        directory, binary, depth)
            end select
        end do
        if (level /= 1 .and. len(plan%error) == 0) &
            plan%error = 'native CMake: unterminated if'
        if (len(plan%error) > 0) plan%error = &
            directory//'/CMakeLists.txt: '//plan%error
        call cm_set(plan, 'CMAKE_CURRENT_SOURCE_DIR', old_source)
        call cm_set(plan, 'CMAKE_CURRENT_BINARY_DIR', old_binary)
        if (depth > 0) plan%variables = old_variables
    end subroutine
end module fo_cmake_native_config
