module fo_test_json
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    integer, parameter, public :: json_invalid = 0, json_object = 1, json_array = 2
    integer, parameter, public :: json_string = 3, json_number = 4
    integer, parameter, public :: json_boolean = 5, json_null = 6
    public :: json_value_t, json_parse, json_member, json_element, json_size
    public :: json_string_value, json_number_value, json_boolean_value

    type :: json_value_t
        integer :: kind = json_invalid
        logical :: boolean = .false.
        character(:), allocatable :: text
        character(:), allocatable :: name
        type(json_value_t), allocatable :: children(:)
    contains
        procedure, private :: assign_value
        generic, public :: assignment(=) => assign_value
    end type json_value_t

contains

    subroutine json_parse(source, value, valid, error_message)
        character(len=*), intent(in) :: source
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        integer :: position

        position = 1
        error_message = ''
        call parse_value(source, position, value, valid, error_message)
        if (.not. valid) return
        call skip_space(source, position)
        if (position <= len(source)) then
            valid = .false.
            error_message = 'trailing bytes after JSON value'
            return
        end if
        valid = .true.
    end subroutine json_parse

    recursive subroutine parse_value(source, position, value, valid, error_message)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        character(len=1) :: first

        call skip_space(source, position)
        valid = .false.
        error_message = 'expected a JSON value'
        if (position > len(source)) return
        first = source(position:position)
        select case (first)
        case ('{')
            call parse_object(source, position, value, valid, error_message)
        case ('[')
            call parse_array(source, position, value, valid, error_message)
        case ('"')
            value%kind = json_string
            call parse_string(source, position, value%text, valid, error_message)
        case ('t')
            call parse_literal(source, position, 'true', json_boolean, value, valid, error_message)
            value%boolean = .true.
        case ('f')
            call parse_literal(source, position, 'false', json_boolean, value, valid, error_message)
            value%boolean = .false.
        case ('n')
            call parse_literal(source, position, 'null', json_null, value, valid, error_message)
        case ('-', '0':'9')
            call parse_number(source, position, value, valid, error_message)
        case default
            error_message = 'unexpected byte at value position'
        end select
    end subroutine parse_value

    recursive subroutine parse_object(source, position, value, valid, error_message)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        type(json_value_t) :: child
        character(:), allocatable :: key
        character(len=1) :: delimiter

        value%kind = json_object
        allocate(value%children(0))
        position = position + 1
        call skip_space(source, position)
        if (position > len(source)) then
            valid = .false.
            error_message = 'unterminated JSON object'
            return
        end if
        if (source(position:position) == '}') then
            position = position + 1
            valid = .true.
            error_message = ''
            return
        end if
        do
            if (position > len(source)) then
                valid = .false.
                error_message = 'unterminated JSON object'
                return
            end if
            if (source(position:position) /= '"') then
                valid = .false.
                error_message = 'object key must be a string'
                return
            end if
            call parse_string(source, position, key, valid, error_message)
            if (.not. valid) return
            call skip_space(source, position)
            if (position > len(source)) then
                valid = .false.
                error_message = 'missing colon after object key'
                return
            end if
            if (source(position:position) /= ':') then
                valid = .false.
                error_message = 'missing colon after object key'
                return
            end if
            position = position + 1
            call parse_value(source, position, child, valid, error_message)
            if (.not. valid) return
            child%name = key
            call append_child(value, child)
            call skip_space(source, position)
            if (position > len(source)) then
                valid = .false.
                error_message = 'unterminated JSON object'
                return
            end if
            delimiter = source(position:position)
            position = position + 1
            if (delimiter == '}') exit
            if (delimiter /= ',') then
                valid = .false.
                error_message = 'expected comma or closing brace'
                return
            end if
            call skip_space(source, position)
        end do
        valid = .true.
        error_message = ''
    end subroutine parse_object

    recursive subroutine parse_array(source, position, value, valid, error_message)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        type(json_value_t) :: child
        character(len=1) :: delimiter

        value%kind = json_array
        allocate(value%children(0))
        position = position + 1
        call skip_space(source, position)
        if (position > len(source)) then
            valid = .false.
            error_message = 'unterminated JSON array'
            return
        end if
        if (source(position:position) == ']') then
            position = position + 1
            valid = .true.
            error_message = ''
            return
        end if
        do
            call parse_value(source, position, child, valid, error_message)
            if (.not. valid) return
            call append_child(value, child)
            call skip_space(source, position)
            if (position > len(source)) then
                valid = .false.
                error_message = 'unterminated JSON array'
                return
            end if
            delimiter = source(position:position)
            position = position + 1
            if (delimiter == ']') exit
            if (delimiter /= ',') then
                valid = .false.
                error_message = 'expected comma or closing bracket'
                return
            end if
            call skip_space(source, position)
        end do
        valid = .true.
        error_message = ''
    end subroutine parse_array

    subroutine append_child(parent, child)
        type(json_value_t), intent(inout) :: parent
        type(json_value_t), intent(inout) :: child
        type(json_value_t), allocatable :: grown(:)
        integer :: count, index

        count = size(parent%children)
        allocate(grown(count + 1))
        ! Each parsed node has one owner. Move its allocatable components rather
        ! than copying recursive arrays while growing the sibling collection.
        do index = 1, count
            call move_value(parent%children(index), grown(index))
        end do
        call move_value(child, grown(count + 1))
        call move_alloc(grown, parent%children)
    end subroutine append_child

    subroutine assign_value(target, source)
        class(json_value_t), intent(inout) :: target
        type(json_value_t), intent(in) :: source
        type(json_value_t) :: owned_copy

        call copy_value(source, owned_copy)
        call move_value(owned_copy, target)
    end subroutine assign_value

    recursive subroutine copy_value(source, target)
        type(json_value_t), intent(in) :: source
        type(json_value_t), intent(out) :: target
        integer :: index

        target%kind = source%kind
        target%boolean = source%boolean
        if (allocated(source%text)) target%text = source%text
        if (allocated(source%name)) target%name = source%name
        if (allocated(source%children)) then
            allocate(target%children(size(source%children)))
            do index = 1, size(source%children)
                call copy_value(source%children(index), target%children(index))
            end do
        end if
    end subroutine copy_value

    subroutine move_value(source, target)
        type(json_value_t), intent(inout) :: source
        type(json_value_t), intent(out) :: target

        target%kind = source%kind
        target%boolean = source%boolean
        call move_alloc(source%text, target%text)
        call move_alloc(source%name, target%name)
        call move_alloc(source%children, target%children)
        source%kind = json_invalid
        source%boolean = .false.
    end subroutine move_value

    subroutine parse_literal(source, position, expected, kind, value, valid, error_message)
        character(len=*), intent(in) :: source, expected
        integer, intent(inout) :: position
        integer, intent(in) :: kind
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        integer :: finish

        finish = position + len(expected) - 1
        if (finish > len(source)) then
            valid = .false.
            error_message = 'incomplete JSON literal'
            return
        end if
        if (source(position:finish) /= expected) then
            valid = .false.
            error_message = 'invalid JSON literal'
            return
        end if
        position = finish + 1
        value%kind = kind
        valid = .true.
        error_message = ''
    end subroutine parse_literal

    subroutine parse_number(source, position, value, valid, error_message)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        integer :: start, length

        start = position
        length = len(source)
        if (source(position:position) == '-') position = position + 1
        if (position > length) then
            call number_error(valid, error_message, 'incomplete JSON number')
            return
        end if
        if (source(position:position) == '0') then
            position = position + 1
            if (position <= length) then
                if (is_digit(source(position:position))) then
                    call number_error(valid, error_message, 'leading zero in JSON number')
                    return
                end if
            end if
        else if (source(position:position) >= '1' .and. source(position:position) <= '9') then
            do while (position <= length)
                if (.not. is_digit(source(position:position))) exit
                position = position + 1
            end do
        else
            call number_error(valid, error_message, 'integer part required in JSON number')
            return
        end if
        if (position <= length) then
            if (source(position:position) == '.') then
                position = position + 1
                if (position > length) then
                    call number_error(valid, error_message, 'fraction digit required')
                    return
                end if
                if (.not. is_digit(source(position:position))) then
                    call number_error(valid, error_message, 'fraction digit required')
                    return
                end if
                do while (position <= length)
                    if (.not. is_digit(source(position:position))) exit
                    position = position + 1
                end do
            end if
        end if
        if (position <= length) then
            if (source(position:position) == 'e' .or. source(position:position) == 'E') then
                position = position + 1
                if (position <= length) then
                    if (source(position:position) == '+' .or. source(position:position) == '-') &
                        position = position + 1
                end if
                if (position > length) then
                    call number_error(valid, error_message, 'exponent digit required')
                    return
                end if
                if (.not. is_digit(source(position:position))) then
                    call number_error(valid, error_message, 'exponent digit required')
                    return
                end if
                do while (position <= length)
                    if (.not. is_digit(source(position:position))) exit
                    position = position + 1
                end do
            end if
        end if
        value%kind = json_number
        value%text = source(start:position - 1)
        valid = .true.
        error_message = ''
    end subroutine parse_number

    subroutine number_error(valid, error_message, message)
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        character(len=*), intent(in) :: message

        valid = .false.
        error_message = message
    end subroutine number_error

    logical function is_digit(character)
        character(len=1), intent(in) :: character

        is_digit = character >= '0' .and. character <= '9'
    end function is_digit

    subroutine parse_string(source, position, value, valid, error_message)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        character(:), allocatable, intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: error_message
        character(len=1) :: current
        integer :: byte, sequence_length, codepoint, low_codepoint, low_position
        logical :: hex_valid, pair_valid

        value = ''
        valid = .false.
        error_message = 'expected a JSON string'
        if (position > len(source)) return
        if (source(position:position) /= '"') return
        position = position + 1
        do
            if (position > len(source)) then
                error_message = 'unterminated JSON string'
                return
            end if
            current = source(position:position)
            byte = iachar(current)
            if (current == '"') then
                position = position + 1
                valid = .true.
                error_message = ''
                return
            else if (current == '\') then
                position = position + 1
                if (position > len(source)) then
                    error_message = 'unfinished JSON escape'
                    return
                end if
                current = source(position:position)
                select case (current)
                case ('"', '\', '/')
                    value = value // current
                    position = position + 1
                case ('b')
                    value = value // achar(8)
                    position = position + 1
                case ('f')
                    value = value // achar(12)
                    position = position + 1
                case ('n')
                    value = value // achar(10)
                    position = position + 1
                case ('r')
                    value = value // achar(13)
                    position = position + 1
                case ('t')
                    value = value // achar(9)
                    position = position + 1
                case ('u')
                    position = position + 1
                    call parse_hex4(source, position, codepoint, hex_valid)
                    if (.not. hex_valid) then
                        error_message = 'invalid Unicode escape'
                        return
                    end if
                    pair_valid = .false.
                    if (codepoint >= int(z'D800')) then
                        if (codepoint <= int(z'DBFF')) then
                            if (position + 1 <= len(source)) then
                                if (source(position:position + 1) == '\u') then
                                    low_position = position + 2
                                    call parse_hex4(source, low_position, low_codepoint, hex_valid)
                                    if (hex_valid) then
                                        if (low_codepoint >= int(z'DC00')) then
                                            if (low_codepoint <= int(z'DFFF')) then
                                                codepoint = 65536 + (codepoint - 55296) * 1024 + &
                                                    low_codepoint - 56320
                                                position = low_position
                                                pair_valid = .true.
                                            end if
                                        end if
                                    end if
                                end if
                            end if
                            if (.not. pair_valid) codepoint = 65533
                        end if
                    end if
                    if (codepoint >= int(z'DC00')) then
                        if (codepoint <= int(z'DFFF')) codepoint = 65533
                    end if
                    call append_codepoint(value, codepoint)
                case default
                    error_message = 'invalid JSON escape'
                    return
                end select
            else
                if (byte < 32) then
                    error_message = 'unescaped control byte in JSON string'
                    return
                end if
                sequence_length = 1
                if (byte >= 128) then
                    sequence_length = utf8_sequence_length(source, position)
                    if (sequence_length == 0) then
                        error_message = 'invalid UTF-8 in JSON string'
                        return
                    end if
                end if
                value = value // source(position:position + sequence_length - 1)
                position = position + sequence_length
            end if
        end do
    end subroutine parse_string

    subroutine parse_hex4(source, position, codepoint, valid)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        integer, intent(out) :: codepoint
        logical, intent(out) :: valid
        integer :: i, digit

        codepoint = 0
        valid = .false.
        do i = 0, 3
            if (position + i > len(source)) return
            digit = hex_value(source(position + i:position + i))
            if (digit < 0) return
            codepoint = codepoint * 16 + digit
        end do
        position = position + 4
        valid = .true.
    end subroutine parse_hex4

    integer function hex_value(character)
        character(len=1), intent(in) :: character

        select case (character)
        case ('0':'9')
            hex_value = iachar(character) - iachar('0')
        case ('a':'f')
            hex_value = iachar(character) - iachar('a') + 10
        case ('A':'F')
            hex_value = iachar(character) - iachar('A') + 10
        case default
            hex_value = -1
        end select
    end function hex_value

    subroutine append_codepoint(value, codepoint)
        character(:), allocatable, intent(inout) :: value
        integer, intent(in) :: codepoint
        integer :: first, second, third, fourth

        if (codepoint <= 127) then
            value = value // achar(codepoint)
        else if (codepoint <= 2047) then
            first = 192 + codepoint / 64
            second = 128 + mod(codepoint, 64)
            value = value // achar(first) // achar(second)
        else if (codepoint <= 65535) then
            first = 224 + codepoint / 4096
            second = 128 + mod(codepoint / 64, 64)
            third = 128 + mod(codepoint, 64)
            value = value // achar(first) // achar(second) // achar(third)
        else
            first = 240 + codepoint / 262144
            second = 128 + mod(codepoint / 4096, 64)
            third = 128 + mod(codepoint / 64, 64)
            fourth = 128 + mod(codepoint, 64)
            value = value // achar(first) // achar(second) // achar(third) // achar(fourth)
        end if
    end subroutine append_codepoint

    integer function utf8_sequence_length(source, position) result(length)
        character(len=*), intent(in) :: source
        integer, intent(in) :: position
        integer :: first, second, third, fourth

        length = 0
        first = iachar(source(position:position))
        if (first >= 194 .and. first <= 223) then
            if (position + 1 > len(source)) return
            second = iachar(source(position + 1:position + 1))
            if (second < 128 .or. second > 191) return
            length = 2
            return
        end if
        if (first >= 224 .and. first <= 239) then
            if (position + 2 > len(source)) return
            second = iachar(source(position + 1:position + 1))
            third = iachar(source(position + 2:position + 2))
            if (second < 128 .or. second > 191) return
            if (third < 128 .or. third > 191) return
            if (first == 224 .and. second < 160) return
            if (first == 237 .and. second > 159) return
            length = 3
            return
        end if
        if (first >= 240 .and. first <= 244) then
            if (position + 3 > len(source)) return
            second = iachar(source(position + 1:position + 1))
            third = iachar(source(position + 2:position + 2))
            fourth = iachar(source(position + 3:position + 3))
            if (second < 128 .or. second > 191) return
            if (third < 128 .or. third > 191) return
            if (fourth < 128 .or. fourth > 191) return
            if (first == 240 .and. second < 144) return
            if (first == 244 .and. second > 143) return
            length = 4
        end if
    end function utf8_sequence_length

    subroutine skip_space(source, position)
        character(len=*), intent(in) :: source
        integer, intent(inout) :: position
        integer :: byte

        do
            if (position > len(source)) return
            byte = iachar(source(position:position))
            if (byte /= 32 .and. byte /= 9 .and. byte /= 10 .and. byte /= 13) return
            position = position + 1
        end do
    end subroutine skip_space

    function json_member(object, name) result(value)
        type(json_value_t), intent(in) :: object
        character(len=*), intent(in) :: name
        type(json_value_t) :: value
        integer :: i

        value%kind = json_invalid
        if (object%kind /= json_object) return
        if (.not. allocated(object%children)) return
        do i = 1, size(object%children)
            if (object%children(i)%name == name) then
                value = object%children(i)
                return
            end if
        end do
    end function json_member

    function json_element(array, index) result(value)
        type(json_value_t), intent(in) :: array
        integer, intent(in) :: index
        type(json_value_t) :: value

        value%kind = json_invalid
        if (array%kind /= json_array) return
        if (.not. allocated(array%children)) return
        if (index < 1 .or. index > size(array%children)) return
        value = array%children(index)
    end function json_element

    integer function json_size(value)
        type(json_value_t), intent(in) :: value

        json_size = 0
        if (value%kind /= json_array .and. value%kind /= json_object) return
        if (allocated(value%children)) json_size = size(value%children)
    end function json_size

    function json_string_value(value) result(text)
        type(json_value_t), intent(in) :: value
        character(:), allocatable :: text

        text = ''
        if (value%kind == json_string) text = value%text
    end function json_string_value

    real(real64) function json_number_value(value) result(number)
        type(json_value_t), intent(in) :: value
        integer :: io_status

        number = 0.0_real64
        if (value%kind /= json_number) return
        read(value%text, *, iostat=io_status) number
        if (io_status /= 0) number = 0.0_real64
    end function json_number_value

    logical function json_boolean_value(value)
        type(json_value_t), intent(in) :: value

        json_boolean_value = .false.
        if (value%kind == json_boolean) json_boolean_value = value%boolean
    end function json_boolean_value

end module fo_test_json
