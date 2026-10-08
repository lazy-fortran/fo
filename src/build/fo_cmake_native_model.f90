module fo_cmake_native_model
    implicit none
    private
    public :: cm_word_t, cm_target_t, cm_test_t, cm_plan_t, cm_words, cm_add
    public :: cm_set, cm_get, cm_defined, cm_expand, cm_next, cm_lower, cm_path
    public :: cm_variable_t

    type :: cm_word_t
        character(:), allocatable :: text
    end type
    type :: cm_target_t
        character(:), allocatable :: name, kind, source_dir, binary_dir, module_dir
        type(cm_word_t), allocatable :: sources(:), links(:)
        logical :: module_exported = .false.
    end type
    type :: cm_test_t
        character(:), allocatable :: name, target, cwd
        type(cm_word_t), allocatable :: args(:)
    end type
    type :: cm_variable_t
        character(:), allocatable :: name, value
    end type
    type :: cm_plan_t
        character(:), allocatable :: root, build, configuration, error, project
        type(cm_target_t), allocatable :: targets(:)
        type(cm_test_t), allocatable :: tests(:)
        type(cm_variable_t), allocatable :: variables(:)
        type(cm_variable_t), allocatable :: environment(:)
        logical :: frozen = .false.
        logical :: fortran_enabled = .false.
        type(cm_word_t), allocatable :: compile_flags(:), link_flags(:), includes(:)
        type(cm_word_t), allocatable :: inputs(:)
        character(:), allocatable :: build_targets(:)
    end type
contains
    subroutine cm_add(words, text)
        type(cm_word_t), allocatable, intent(inout) :: words(:)
        character(len=*), intent(in) :: text
        if (.not. allocated(words)) allocate (words(0))
        words = [words, cm_word_t(text)]
    end subroutine

    function cm_words(text) result(words)
        character(len=*), intent(in) :: text
        type(cm_word_t), allocatable :: words(:)
        character(:), allocatable :: word
        integer :: i
        logical :: quoted, started
        allocate (words(0))
        word = ''
        quoted = .false.
        started = .false.
        i = 1
        do while (i <= len(text))
            if (text(i:i) == '"') then
                quoted = .not. quoted
                started = .true.
            else if (text(i:i) == achar(92)) then
                i = i + 1
                if (i <= len(text)) word = word//text(i:i)
            else if (.not. quoted .and. index(' '//achar(9)//achar(10)// &
                                              achar(13)//';', text(i:i)) > 0) then
                if (started) call cm_add(words, word)
                word = ''
                started = .false.
            else
                word = word//text(i:i)
                started = .true.
            end if
            i = i + 1
        end do
        if (started) call cm_add(words, word)
    end function

    logical function cm_defined(plan, name) result(defined)
        type(cm_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: name
        integer :: i
        defined = .false.
        if (.not. allocated(plan%variables)) return
        do i = 1, size(plan%variables)
            if (plan%variables(i)%name == name) defined = .true.
        end do
    end function cm_defined

    subroutine cm_set(plan, name, value)
        type(cm_plan_t), intent(inout) :: plan
        character(len=*), intent(in) :: name, value
        integer :: i
        if (.not. allocated(plan%variables)) allocate (plan%variables(0))
        do i = 1, size(plan%variables)
            if (plan%variables(i)%name /= name) cycle
            plan%variables(i)%value = value
            return
        end do
        plan%variables = [plan%variables, cm_variable_t(name, value)]
    end subroutine

    function cm_get(plan, name) result(value)
        type(cm_plan_t), intent(in) :: plan
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        integer :: i
        value = ''
        if (.not. allocated(plan%variables)) return
        do i = 1, size(plan%variables)
            if (plan%variables(i)%name == name) value = plan%variables(i)%value
        end do
    end function

    function cm_expand(plan, text) result(value)
        type(cm_plan_t), intent(inout) :: plan
        character(len=*), intent(in) :: text
        character(:), allocatable :: value, replacement, name
        integer :: i, j, close, start, status, length
        logical :: captured
        value = ''
        i = 1
        do while (i <= len(text))
            start = 0
            if (index(text(i:), '${') == 1) start = i + 2
            if (index(text(i:), '$ENV{') == 1) start = i + 5
            if (start == 0) then
                value = value//text(i:i)
                i = i + 1
                cycle
            end if
            close = index(text(start:), '}')
            if (close == 0) then
                value = value//text(i:)
                return
            end if
            close = start + close - 1
            name = text(start:close - 1)
            replacement = ''
            if (start == i + 2) then
                replacement = cm_get(plan, name)
            else
                captured = .false.
                if (.not. allocated(plan%environment)) allocate (plan%environment(0))
                do j = 1, size(plan%environment)
                    if (plan%environment(j)%name /= name) cycle
                    replacement = plan%environment(j)%value
                    captured = .true.
                    exit
                end do
                if (.not. captured) then
                    if (plan%frozen) then
                     plan%error = 'native CMake: uncaptured environment variable '//name
                        return
                    end if
                    call get_environment_variable(name, length=length, status=status)
                    if (status == 0) then
                        deallocate (replacement)
                        allocate (character(len=length) :: replacement)
                        call get_environment_variable(name, replacement)
                    end if
                 plan%environment = [plan%environment, cm_variable_t(name, replacement)]
                end if
            end if
            value = value//replacement
            i = close + 1
        end do
    end function

    subroutine cm_next(text, cursor, command, words, error)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: cursor
        character(:), allocatable, intent(out) :: command, error
        type(cm_word_t), allocatable, intent(out) :: words(:)
        character(:), allocatable :: body
        integer :: start, depth
        logical :: quoted
        command = ''
        error = ''
        allocate (words(0))
        do while (cursor <= len(text))
            if (text(cursor:cursor) == '#') then
                do while (cursor <= len(text))
                    if (text(cursor:cursor) == achar(10)) exit
                    cursor = cursor + 1
                end do
            else if (index(' '//achar(9)//achar(10)//achar(13), &
                           text(cursor:cursor)) == 0) then
                exit
            else
                cursor = cursor + 1
            end if
        end do
        if (cursor > len(text)) return
        start = cursor
        do while (cursor <= len(text))
            if (index(' ('//achar(9)//achar(10)//achar(13), &
                      text(cursor:cursor)) > 0) exit
            cursor = cursor + 1
        end do
        command = cm_lower(text(start:cursor - 1))
        do while (cursor <= len(text))
            if (text(cursor:cursor) == '(') exit
            cursor = cursor + 1
        end do
        if (cursor > len(text)) then
            error = 'missing command opening parenthesis'
            return
        end if
        body = ''
        quoted = .false.
        depth = 1
        cursor = cursor + 1
        do while (cursor <= len(text))
            if (text(cursor:cursor) == '"') quoted = .not. quoted
            if (.not. quoted) then
                if (text(cursor:cursor) == '(') depth = depth + 1
                if (text(cursor:cursor) == ')') depth = depth - 1
                if (depth == 0) exit
                if (text(cursor:cursor) == '#') then
                    do while (cursor <= len(text))
                        if (text(cursor:cursor) == achar(10)) exit
                        cursor = cursor + 1
                    end do
                    body = body//' '
                    cycle
                end if
            end if
            body = body//text(cursor:cursor)
            cursor = cursor + 1
        end do
        if (depth /= 0 .or. quoted) then
            error = 'unterminated command argument'
            return
        end if
        cursor = cursor + 1
        words = cm_words(body)
    end subroutine

    pure function cm_lower(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower
        integer :: i, c
        lower = text
        do i = 1, len(text)
            c = iachar(text(i:i))
            if (c >= 65 .and. c <= 90) lower(i:i) = achar(c + 32)
        end do
    end function

    function cm_path(root, path) result(absolute)
        character(len=*), intent(in) :: root, path
        character(:), allocatable :: absolute
        absolute = path
        if (len(path) == 0) then
            absolute = root
        else if (path(1:1) /= '/') then
            if (index(path, ':') /= 2) absolute = root//'/'//path
        end if
    end function
end module fo_cmake_native_model
