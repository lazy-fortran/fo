module fo_lsp
    use fx_lsp, only: lsp_server_t, lsp_server_init, lsp_server_run, &
        lsp_publish_diagnostics, lsp_uri_to_path, lsp_path_to_uri
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
        character(len=512) :: file_path, project_dir, diag_uri

        if (len_trim(text) < 0) return ! text required by interface; content not inspected
        file_path = lsp_uri_to_path(uri)
        call find_project_dir(file_path, project_dir)
        call fo_check_run(trim(project_dir), res)

        n_diags = 0
        diag_uri = uri
        if (.not. (res%build_ok .and. res%tests_ok)) then
            n_diags = 1
            diags(1)%severity = DIAG_ERROR
            diags(1)%message = trim(res%error_msg)
            diags(1)%line = res%diag_line
            diags(1)%col = res%diag_column
            if (len_trim(res%diag_file) > 0) then
                diag_uri = lsp_path_to_uri(trim(res%diag_file))
            end if
        end if
        call lsp_publish_diagnostics(diag_uri, diags, n_diags)
    end subroutine on_save

    subroutine find_project_dir(file_path, project_dir)
        character(len=*), intent(in) :: file_path
        character(len=*), intent(out) :: project_dir

        character(len=512) :: dir
        integer :: slash
        logical :: exists

        dir = file_path
        do
            slash = index(trim(dir), '/', back=.true.)
            if (slash < 2) exit
            dir = dir(1:slash - 1)
            inquire (file=trim(dir)//'/fpm.toml', exist=exists)
            if (exists) then
                project_dir = trim(dir)
                return
            end if
        end do
        project_dir = '.'
    end subroutine find_project_dir

end module fo_lsp
