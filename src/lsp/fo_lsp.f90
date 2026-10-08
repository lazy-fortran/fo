module fo_lsp
    use fx_lsp, only: lsp_server_t, lsp_server_init, lsp_server_run
    use fx_diag, only: diag_t
    use fortfront_compiler, only: compiler_frontend_options_t, &
                                  compiler_frontend_result_t, &
                               compile_frontend_from_string, get_compiler_diagnostics, &
                                  INPUT_MODE_STANDARD, OPERATING_MODE_INFER
    implicit none
    private
    public :: lsp_serve

contains

    subroutine lsp_serve()
        type(lsp_server_t) :: server
        character(len=32) :: debounce
        integer :: status, value

        call lsp_server_init(server, 'fo')
        call get_environment_variable('FO_LSP_DEBOUNCE_MS', debounce, status=status)
        if (status == 0) then
            read (debounce, *, iostat=status) value
            if (status == 0) server%debounce_ms = max(0, min(60000, value))
        end if
        call lsp_server_run(server, diagnose_document)
    end subroutine lsp_serve

    subroutine diagnose_document(uri, text, diags)
        character(len=*), intent(in) :: uri, text
        type(diag_t), allocatable, intent(out) :: diags(:)
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        integer :: i

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%operating_mode = OPERATING_MODE_INFER
        options%run_semantics = .true.
        call compile_frontend_from_string(text, result, options)
        associate (frontend => get_compiler_diagnostics(result))
            allocate (diags(size(frontend)))
            do i = 1, size(frontend)
                diags(i)%file = uri
                diags(i)%line = frontend(i)%span%start%line
                diags(i)%col = utf16_column(text, diags(i)%line, &
                                            frontend(i)%span%start%column)
                diags(i)%end_line = frontend(i)%span%end%line
                diags(i)%end_col = utf16_column(text, diags(i)%end_line, &
                                                frontend(i)%span%end%column)
                diags(i)%severity = max(0, min(3, frontend(i)%severity - 1))
                diags(i)%code = frontend(i)%code
                if (allocated(frontend(i)%message)) then
                    diags(i)%message = frontend(i)%message
                end if
            end do
        end associate
    end subroutine diagnose_document

    integer function utf16_column(text, line, column) result(converted)
        character(len=*), intent(in) :: text
        integer, intent(in) :: line, column
        integer :: i, current_line, first, width, byte

        converted = max(1, column)
        if (line < 1 .or. column < 1) return
        first = 1
        current_line = 1
        do i = 1, len(text)
            if (current_line == line) exit
            if (text(i:i) /= achar(10)) cycle
            current_line = current_line + 1
            first = i + 1
        end do
        converted = 1
        i = first
        do while (i < first + column - 1)
            if (i > len(text)) exit
            byte = iachar(text(i:i))
            width = 1
            if (byte >= 192 .and. byte < 224) width = 2
            if (byte >= 224 .and. byte < 240) width = 3
            if (byte >= 240) width = 4
            converted = converted + 1
            if (width == 4) converted = converted + 1
            i = i + width
        end do
    end function utf16_column

end module fo_lsp
