program test_test_json_parser
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_number_value
    implicit none

    call exercise_ownership()
    print '(a)', 'JSON parser ownership: nested siblings, copies and rejected-input reuse pass'

contains

    subroutine exercise_ownership()
        type(json_value_t) :: document, rows, row, item, field, saved
        character(:), allocatable :: source, message
        character(len=16) :: digits
        logical :: valid
        integer :: iteration, index

        source = '{"rows":['
        do index = 1, 80
            if (index > 1) source = source // ','
            write(digits, '(i0)') index
            source = source // '[' // trim(digits) // &
                ',{"text":"owned","nested":[true,null,{"leaf":"value"}]}]'
        end do
        source = source // ']}'
        do iteration = 1, 12
            call json_parse(source, document, valid, message)
            if (.not. valid) error stop 'valid nested JSON rejected'
            rows = json_member(document, 'rows')
            if (json_size(rows) /= 80) error stop 'nested array lost rows'
            do index = 1, 80
                row = json_element(rows, index)
                item = json_element(row, 1)
                if (nint(json_number_value(item)) /= index) error stop 'row number changed'
                item = json_element(row, 2)
                field = json_member(item, 'text')
                if (json_string_value(field) /= 'owned') error stop 'sibling string changed'
                field = json_member(item, 'nested')
                item = json_element(field, 3)
                field = json_member(item, 'leaf')
                if (json_string_value(field) /= 'value') error stop 'nested leaf changed'
            end do
            saved = document
            call json_parse('{"replacement":0}', document, valid, message)
            if (.not. valid) error stop 'valid replacement JSON rejected'
            rows = json_member(saved, 'rows')
            row = json_element(rows, 80)
            item = json_element(row, 1)
            if (nint(json_number_value(item)) /= 80) error stop 'copy lost ownership'
            call json_parse('{"broken":[{"partial":[1,2]},', document, valid, message)
            if (valid) error stop 'incomplete nested JSON accepted'
            call json_parse('[{"again":"safe"}]', document, valid, message)
            if (.not. valid) error stop 'reuse after rejected JSON failed'
            item = json_element(document, 1)
            field = json_member(item, 'again')
            if (json_string_value(field) /= 'safe') error stop 'reuse retained stale values'
        end do
    end subroutine exercise_ownership
end program test_test_json_parser
