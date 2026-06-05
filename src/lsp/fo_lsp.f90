module fo_lsp
    use fx_lsp, only: lsp_server_t, lsp_server_init, lsp_server_run, &
                      lsp_publish_diagnostics
    use fx_diag, only: diag_t, DIAG_ERROR
    use fo_check, only: check_result_t, fo_check_run
    implicit none
    private
    public :: lsp_serve

contains

    subroutine lsp_serve()
        type(lsp_server_t) :: s

        call lsp_server_init(s, 'fo')
        call lsp_server_run(s, on_save)
    end subroutine lsp_serve

    subroutine on_save(uri, text)
        character(len=*), intent(in) :: uri
        character(len=*), intent(in) :: text

        type(check_result_t) :: res
        type(diag_t) :: diags(1)
        integer :: n_diags

        call fo_check_run('.', res)

        n_diags = 0
        if (.not. (res%build_ok .and. res%tests_ok)) then
            n_diags = 1
            diags(1)%severity = DIAG_ERROR
            diags(1)%message = trim(res%error_msg)
            diags(1)%line = res%diag_line
            diags(1)%col = res%diag_column
        end if
        call lsp_publish_diagnostics(uri, diags, n_diags)
    end subroutine on_save

end module fo_lsp
