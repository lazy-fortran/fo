program test_lsp_unsaved_cli
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_size_t, c_null_char
    use fo_test_harness, only: make_scratch, make_directory, write_text, &
           assert_true, assert_equal_string, assert_equal_integer, assert_file_equals, &
                               assert_file_absent, finish_assertions
    use fo_test_cli, only: resolve_driver
    use fo_test_mcp, only: mcp_quote
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element, &
                            json_size, json_string_value, json_number_value
    implicit none

    interface
        integer(c_int) function start(driver, directory, cache, state, errors) &
            bind(C, name='fo_test_lsp_start')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: driver(*), directory(*), cache(*)
            character(kind=c_char), intent(in) :: state(*), errors(*)
        end function start
    integer(c_int) function send(handle, body, length) bind(C, name='fo_test_mcp_write')
            import :: c_char, c_int, c_size_t
            integer(c_int), intent(in), value :: handle
            character(kind=c_char), intent(in) :: body(*)
            integer(c_size_t), intent(in), value :: length
        end function send
        integer(c_int) function read_line(handle, body, capacity, timeout) &
            bind(C, name='fo_test_mcp_read_line')
            import :: c_char, c_int, c_size_t
            integer(c_int), intent(in), value :: handle, timeout
            character(kind=c_char), intent(out) :: body(*)
            integer(c_size_t), intent(in), value :: capacity
        end function read_line
        integer(c_int) function read_bytes(handle, body, length, timeout) &
            bind(C, name='fo_test_mcp_read_bytes')
            import :: c_char, c_int, c_size_t
            integer(c_int), intent(in), value :: handle, timeout
            character(kind=c_char), intent(out) :: body(*)
            integer(c_size_t), intent(in), value :: length
        end function read_bytes
        integer(c_int) function wait_child(handle, timeout, exit_code) &
            bind(C, name='fo_test_mcp_wait')
            import :: c_int
            integer(c_int), intent(in), value :: handle, timeout
            integer(c_int), intent(out) :: exit_code
        end function wait_child
        integer(c_int) function close_child(handle) bind(C, name='fo_test_mcp_close')
            import :: c_int
            integer(c_int), intent(in), value :: handle
        end function close_child
    end interface

   character(:), allocatable :: driver, scratch, uri, good, parser_error, semantic_error
    character(kind=c_char, len=32768) :: bytes
    type(json_value_t) :: response, params, diagnostics, diagnostic, span, position
    integer(c_int) :: handle, status, exit_code

    call resolve_driver(driver)
    call make_scratch('lsp-unsaved', scratch)
    call make_directory(scratch//'/src')
    call write_text(scratch//'/fpm.toml', 'name = "typing_sentinel"'//achar(10))
    call write_text(scratch//'/src/space #.f90', 'disk source must stay untouched')
    uri = 'file://'//scratch//'/src/space%20%23.f90'
    good = 'program good'//achar(10)//'implicit none'//achar(10)//'end program good'
    parser_error = 'program broken'//achar(10)//'implicit none'//achar(10)// &
                   'integer :: value'//achar(10)//'value = identity{integer'
    semantic_error = 'elemental subroutine invalid_intent(value)'//achar(10)// &
                     '  integer :: value'//achar(10)//'end subroutine invalid_intent'
    handle = start(driver//c_null_char, scratch//c_null_char, &
                   scratch//'/cache'//c_null_char, scratch//'/state'//c_null_char, &
                   scratch//'/stderr'//c_null_char)
    call assert_true(handle > 0, 'starts public LSP process')
    if (handle <= 0) call finish_assertions('lsp_unsaved_cli')

    call notify('{"jsonrpc":"2.0","id":"17","method":"initialize","params":{}}')
    call receive(response)
    call assert_equal_string(json_string_value(json_member(response, 'id')), '17', &
                             'initialize preserves a numeric-looking string request id')
    call notify(edit('didOpen', 1, parser_error))
    call notify(edit('didChange', 2, semantic_error))
    call notify(edit('didChange', 3, good))
    status = read_line(handle, bytes, int(len(bytes), c_size_t), 100_c_int)
    call assert_equal_integer(int(status), -2, &
                     'configured 300 ms debounce publishes nothing during rapid typing')
    call receive(response)
    call check_publication(response, 3, .false.)

    call notify(edit('didChange', 4, semantic_error))
    call receive(response)
    call check_publication(response, 4, .true.)
    params = json_member(response, 'params')
    diagnostics = json_member(params, 'diagnostics')
    diagnostic = json_element(diagnostics, 1)
    span = json_member(diagnostic, 'range')
    position = json_member(span, 'start')
    call assert_equal_integer(int(json_number_value(json_member(position, 'line'))), &
                       1, 'unsaved semantic diagnostic points to exact zero-based line')
 call assert_equal_integer(int(json_number_value(json_member(position, 'character'))), &
                     2, 'unsaved semantic diagnostic points to exact zero-based column')
    position = json_member(span, 'end')
    call assert_true(json_number_value(json_member(position, 'line')) >= 1, &
                     'diagnostic carries the frontend end span')
call assert_equal_integer(int(json_number_value(json_member(diagnostic, 'severity'))), &
                              1, 'frontend error is LSP severity error')
    call assert_true(json_number_value(json_member(diagnostic, 'code')) > 0, &
                     'frontend stable error code is preserved')

    call notify(edit('didChange', 5, &
        'elemental subroutine invalid_intent(a, b)'//achar(10)// &
        '  integer :: a'//achar(10)//'  integer :: b'//achar(10)// &
        'end subroutine invalid_intent'))
    call receive(response)
    call check_publication(response, 5, .true.)
    params = json_member(response, 'params')
    call assert_equal_integer(json_size(json_member(params, 'diagnostics')), &
        2, 'publishes every independent semantic error in one document')

    call notify(edit('didChange', 6, parser_error))
    call receive(response)
    call check_publication(response, 6, .true.)
    params = json_member(response, 'params')
    diagnostic = json_element(json_member(params, 'diagnostics'), 1)
    position = json_member(json_member(diagnostic, 'range'), 'start')
    call assert_equal_integer(int(json_number_value(json_member(position, 'line'))), &
                              3, 'unsaved parser diagnostic points to exact line')

    call notify(edit('didChange', 7, &
        'program broken'//achar(10)//'implicit none'//achar(10)// &
        'integer :: value'//achar(10)//'print *, "'//achar(206)//achar(177)// &
        achar(240)//achar(159)//achar(152)//achar(128)// &
        '"; value = identity{integer'))
    call receive(response)
    call check_publication(response, 7, .true.)
    params = json_member(response, 'params')
    diagnostic = json_element(json_member(params, 'diagnostics'), 1)
    span = json_member(diagnostic, 'range')
    position = json_member(span, 'start')
    call assert_equal_integer( &
        int(json_number_value(json_member(position, 'character'))), 32, &
        'Greek and astral literal before error uses UTF16 start column')
    position = json_member(span, 'end')
    call assert_equal_integer( &
        int(json_number_value(json_member(position, 'character'))), 33, &
        'Greek and astral literal before error uses UTF16 end column')

    call notify(edit('didChange', 8, good))
    call receive(response)
    call check_publication(response, 8, .false.)
    call notify(edit('didChange', 5, parser_error))
    status = read_line(handle, bytes, int(len(bytes), c_size_t), 400_c_int)
    call assert_equal_integer(int(status), -2, &
                              'older versions cannot replace newer results')
    call notify('{"jsonrpc":"2.0","method":"textDocument/didClose","params":'// &
                '{"textDocument":{"uri":'//mcp_quote(uri)//'}}}')
    call receive(response)
    params = json_member(response, 'params')
    call assert_equal_integer(json_size(json_member(params, 'diagnostics')), &
                              0, 'closing document clears diagnostics')
    call notify('{"jsonrpc":"2.0","id":9,"method":"shutdown"}')
    call receive(response)
    call notify('{"jsonrpc":"2.0","method":"exit"}')
    status = wait_child(handle, 5000_c_int, exit_code)
    call assert_equal_integer(int(status), 0, 'LSP shutdown terminates boundedly')
    call assert_equal_integer(int(exit_code), 0, 'LSP exits cleanly')
    status = close_child(handle)
    call assert_equal_integer(int(status), 0, 'owned transport closes after shutdown')
    call assert_file_equals(scratch//'/src/space #.f90', &
                            'disk source must stay untouched', &
                            'diagnostics analyze editor text without writing source')
    call assert_file_absent(scratch//'/build', &
        'typing never starts compiler/test builds')
    call assert_file_absent(scratch//'/cache', 'typing never captures/builds a project')
    call assert_file_absent(scratch//'/state', &
                            'typing never starts resident test campaigns')
    call finish_assertions('lsp_unsaved_cli', retain_failed_scratch=.true.)

contains

    function edit(method, version, text) result(body)
        character(len=*), intent(in) :: method, text
        integer, intent(in) :: version
        character(:), allocatable :: body
        character(len=32) :: number

        write (number, '(i0)') version
        body = '{"jsonrpc":"2.0","method":"textDocument/'//method// &
               '","params":{"textDocument":{"uri":'//mcp_quote(uri)// &
               ',"version":'//trim(number)
        if (method == 'didOpen') then
            body = body//',"languageId":"fortran","text":'//mcp_quote(text)//'}}'
        else
            body = body//'},"contentChanges":[{"text":'//mcp_quote(text)//'}]}}'
        end if
    end function edit

    subroutine notify(body)
        character(len=*), intent(in) :: body
        character(:), allocatable :: frame
        character(len=32) :: length

        write (length, '(i0)') len(body)
        frame = 'Content-Length: '//trim(length)//achar(13)//achar(10)// &
                achar(13)//achar(10)//body
        status = send(handle, frame, int(len(frame), c_size_t))
        call assert_equal_integer(int(status), 0, 'writes complete framed LSP message')
    end subroutine notify

    subroutine receive(value)
        type(json_value_t), intent(out) :: value
        integer :: length, ios
        logical :: valid

        status = read_line(handle, bytes, int(len(bytes), c_size_t), 10000_c_int)
        call assert_true(status > 16, 'receives Content-Length response header')
        if (status <= 16) return
        call assert_equal_string(bytes(:16), 'Content-Length: ', 'valid LSP framing')
        read (bytes(17:status), *, iostat=ios) length
        call assert_equal_integer(ios, 0, 'response header carries a byte count')
        if (ios /= 0) return
        if (length < 1 .or. length > len(bytes)) return
        status = read_line(handle, bytes, int(len(bytes), c_size_t), 10000_c_int)
       call assert_equal_integer(int(status), 0, 'response header ends with blank line')
        status = read_bytes(handle, bytes, int(length, c_size_t), 10000_c_int)
      call assert_equal_integer(int(status), length, 'response body matches byte count')
        if (status /= length) return
        call json_parse(bytes(:length), value, valid)
        call assert_true(valid, 'independent parser accepts LSP response JSON')
    end subroutine receive

    subroutine check_publication(value, version, has_errors)
        type(json_value_t), intent(in) :: value
        integer, intent(in) :: version
        logical, intent(in) :: has_errors
        type(json_value_t) :: fields

        call assert_equal_string(json_string_value(json_member(value, 'method')), &
                         'textDocument/publishDiagnostics', 'receives push diagnostics')
        fields = json_member(value, 'params')
        call assert_equal_string(json_string_value(json_member(fields, 'uri')), uri, &
                                 'diagnostics preserve encoded document URI')
     call assert_equal_integer(int(json_number_value(json_member(fields, 'version'))), &
                    version, 'diagnostics identify the latest unsaved document version')
        if (has_errors) then
            call assert_true(json_size(json_member(fields, 'diagnostics')) > 0, &
                             'unsaved error generates diagnostics')
        else
            call assert_equal_integer(json_size(json_member(fields, 'diagnostics')), &
                                      0, 'correcting unsaved text clears errors')
        end if
    end subroutine check_publication

end program test_lsp_unsaved_cli
