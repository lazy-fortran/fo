program test_fortfront_diagnostics
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_diagnostics, only: diagnostic_t, map_fortfront_diagnostic, &
        frontend_diagnostics_from_file, FO_DIAG_SEVERITY_ERROR, &
        FO_DIAG_SEVERITY_WARNING
    use fortfront_compiler, only: compiler_diagnostic_t, &
        DIAGNOSTIC_PHASE_PARSER, DIAGNOSTIC_PHASE_SEMANTIC, &
        DIAGNOSTIC_CODE_PARSER, DIAGNOSTIC_WARNING
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_valid_source_has_no_frontend_errors()
    call test_mapping_preserves_warning_span_and_code()
    call test_parser_error_maps_to_exact_span()
    call test_semantic_error_maps_to_exact_span()
    call test_frontend_error_sets_had_error()

    write (output_unit, '(a,i0,a,i0,a)') 'fortfront_diagnostics: ', &
        n_pass, ' pass, ', n_fail, ' fail'
    if (n_fail > 0) stop 1

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a,a)') 'FAIL: ', msg
        end if
    end subroutine assert

    subroutine write_source(path, lines)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: lines(:)
        integer :: u, i

        open (newunit=u, file=path, status='replace')
        do i = 1, size(lines)
            write (u, '(a)') trim(lines(i))
        end do
        close (u)
    end subroutine write_source

    subroutine test_valid_source_has_no_frontend_errors()
        character(len=512) :: path
        character(len=80) :: lines(4)
        type(diagnostic_t) :: diags(16)
        integer :: n_diags
        logical :: had_error

        call make_tmp_path('fo_ff_valid', path, '.f90')
        lines(1) = 'program valid'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: value'
        lines(4) = 'end program valid'
        call write_source(path, lines)

        call frontend_diagnostics_from_file(trim(path), diags, n_diags, &
            had_error)
        call assert(n_diags == 0, 'valid source reports no frontend diagnostics')
        call assert(.not. had_error, 'valid source has no frontend error')
    end subroutine test_valid_source_has_no_frontend_errors

    subroutine test_mapping_preserves_warning_span_and_code()
        type(compiler_diagnostic_t) :: fd
        type(diagnostic_t) :: diag

        fd%phase = DIAGNOSTIC_PHASE_PARSER
        fd%code = DIAGNOSTIC_CODE_PARSER
        fd%severity = DIAGNOSTIC_WARNING
        fd%span%start%line = 3
        fd%span%start%column = 5
        fd%span%end%line = 3
        fd%span%end%column = 6
        fd%message = 'unused continuation marker'
        fd%category = 'parser'

        call map_fortfront_diagnostic(fd, 'src/example.f90', diag)
        call assert(trim(diag%file) == 'src/example.f90', &
            'mapping preserves source path')
        call assert(diag%line == 3 .and. diag%column == 5, &
            'mapping preserves span start')
        call assert(diag%severity == FO_DIAG_SEVERITY_WARNING, &
            'mapping preserves warning severity')
        call assert(diag%phase == DIAGNOSTIC_PHASE_PARSER, &
            'mapping preserves phase')
        call assert(diag%code == DIAGNOSTIC_CODE_PARSER, &
            'mapping preserves stable code')
        call assert(index(diag%message, 'parser') > 0 .and. &
            index(diag%message, 'warning') > 0 .and. &
            index(diag%message, 'unused continuation marker') > 0, &
            'formatted message carries phase, severity and text')
    end subroutine test_mapping_preserves_warning_span_and_code

    subroutine test_parser_error_maps_to_exact_span()
        character(len=512) :: path
        character(len=80) :: lines(4)
        type(diagnostic_t) :: diags(16)
        integer :: n_diags, i
        logical :: had_error
        logical :: found

        call make_tmp_path('fo_ff_parse_error', path, '.f90')
        lines(1) = 'program broken'
        lines(2) = '    implicit none'
        lines(3) = '    integer :: value'
        lines(4) = '    value = identity{integer'
        call write_source(path, lines)

        call frontend_diagnostics_from_file(trim(path), diags, n_diags, &
            had_error)
        call assert(had_error, 'parser error sets had_error')
        call assert(n_diags > 0, 'parser error produces a diagnostic')

        found = .false.
        do i = 1, n_diags
            if (diags(i)%severity == FO_DIAG_SEVERITY_ERROR) then
                found = .true.
                call assert(diags(i)%phase == DIAGNOSTIC_PHASE_PARSER, &
                    'parser diagnostic carries parser phase')
                call assert(diags(i)%line == 4, &
                    'parser diagnostic line is exact')
                call assert(trim(diags(i)%file) == trim(path), &
                    'parser diagnostic path is exact')
                call assert(index(diags(i)%message, 'parser') > 0, &
                    'parser diagnostic message is formatted')
            end if
        end do
        call assert(found, 'at least one parser error diagnostic mapped')
    end subroutine test_parser_error_maps_to_exact_span

    subroutine test_semantic_error_maps_to_exact_span()
        character(len=512) :: path
        character(len=80) :: lines(3)
        type(diagnostic_t) :: diags(16)
        integer :: n_diags, i
        logical :: had_error
        logical :: found

        call make_tmp_path('fo_ff_semantic_error', path, '.f90')
        lines(1) = 'elemental subroutine invalid_intent(value)'
        lines(2) = '  integer :: value'
        lines(3) = 'end subroutine invalid_intent'
        call write_source(path, lines)

        call frontend_diagnostics_from_file(trim(path), diags, n_diags, &
            had_error)
        call assert(had_error, 'semantic error sets had_error')
        call assert(n_diags > 0, 'semantic error produces a diagnostic')

        found = .false.
        do i = 1, n_diags
            if (diags(i)%severity == FO_DIAG_SEVERITY_ERROR) then
                found = .true.
                call assert(diags(i)%phase == DIAGNOSTIC_PHASE_SEMANTIC, &
                    'semantic diagnostic carries semantic phase')
                call assert(diags(i)%line == 2 .and. &
                    diags(i)%column == 3, 'semantic diagnostic span is exact')
                call assert(trim(diags(i)%file) == trim(path), &
                    'semantic diagnostic path is exact')
                call assert(index(diags(i)%message, 'semantic') > 0, &
                    'semantic diagnostic message is formatted')
            end if
        end do
        call assert(found, 'at least one semantic error diagnostic mapped')
    end subroutine test_semantic_error_maps_to_exact_span

    subroutine test_frontend_error_sets_had_error()
        character(len=512) :: path
        character(len=80) :: lines(3)
        type(diagnostic_t) :: diags(16)
        integer :: n_diags
        logical :: had_error

        call make_tmp_path('fo_ff_had_error', path, '.f90')
        lines(1) = 'program broken'
        lines(2) = '  value = identity{integer'
        lines(3) = 'end program broken'
        call write_source(path, lines)

        call frontend_diagnostics_from_file(trim(path), diags, n_diags, &
            had_error)
        call assert(had_error, 'malformed source reports a frontend error')
        call assert(n_diags > 0, 'malformed source produces diagnostics')
    end subroutine test_frontend_error_sets_had_error


    subroutine make_tmp_path(prefix, path, suffix)
        character(len=*), intent(in) :: prefix, suffix
        character(len=*), intent(out) :: path

        integer :: count
        integer, save :: serial = 0

        serial = serial + 1
        call system_clock(count)
        write (path, '(a,a,a,i0,a,i0,a)') '/tmp/', trim(prefix), '-', &
            count, '-', serial, trim(suffix)
    end subroutine make_tmp_path

end program test_fortfront_diagnostics
