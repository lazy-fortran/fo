module fo_lint_lex
    !! Shared lexical front end for fo's text-level lint rules. Every rule here
    !! matches on code with comments and string literals removed: Fortran
    !! sources (test sources above all) carry fixture text in literals, and that
    !! text routinely contains the very tokens a rule looks for. Matching raw
    !! file text therefore reports hazards that do not exist and, worse, credits
    !! a file with code it does not run. One masking implementation serves all
    !! rules so they cannot drift apart.
    implicit none
    private
    public :: lex_read_logical_line, mask_code, is_ident_char, lower_ch

    integer, parameter :: MAXLEN = 4096

contains

    subroutine lex_read_logical_line(unit, code, start_line, phys_no, ierr)
        !! Read physical lines from unit until one free-form logical line (the
        !! '&' continuations joined) is complete, and return it masked. code is
        !! lowercased with comments and string literals blanked; start_line is
        !! the physical line the logical line began on; phys_no counts physical
        !! lines read so far and is carried across calls. ierr /= 0 means end of
        !! input with nothing left to return.
        integer, intent(in) :: unit
        character(len=*), intent(out) :: code
        integer, intent(out) :: start_line
        integer, intent(inout) :: phys_no
        integer, intent(out) :: ierr

        character(len=MAXLEN) :: phys, masked
        integer :: iostat
        logical :: cont, continuing

        code = ''
        start_line = 0
        ierr = 0
        continuing = .false.
        do
            read (unit, '(a)', iostat=iostat) phys
            if (iostat /= 0) then
                ! A file that ends mid-continuation still has a logical line
                ! worth scanning; report end of input only once it is consumed.
                if (len_trim(code) == 0) ierr = iostat
                return
            end if
            phys_no = phys_no + 1
            call mask_code(phys, masked)
            call rstrip(masked)
            cont = ends_with_amp(masked)
            if (cont) call drop_trailing_amp(masked)
            if (continuing) then
                call drop_leading_amp(masked)
            else
                start_line = phys_no
            end if
            call append_joined(code, masked)
            if (.not. cont) return
            continuing = .true.
        end do
    end subroutine lex_read_logical_line

    subroutine mask_code(line, code)
        !! Copy line into code with comments and string literals blanked and the
        !! rest lowercased, so tokens inside strings never match. Doubled quotes
        !! inside a literal are the Fortran escape and stay inside the string.
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: code
        integer :: i, L
        character(len=1) :: c, q
        logical :: in_str

        code = ''
        L = len(line)
        in_str = .false.
        q = ' '
        i = 1
        do
            if (i > L) exit
            c = line(i:i)
            if (.not. in_str) then
                if (c == '!') exit
                if (c == '''' .or. c == '"') then
                    in_str = .true.
                    q = c
                    code(i:i) = ' '
                else
                    code(i:i) = lower_ch(c)
                end if
            else
                if (c == q) then
                    if (i < L) then
                        if (line(i + 1:i + 1) == q) then
                            code(i + 1:i + 1) = ' '
                            i = i + 1
                        else
                            in_str = .false.
                        end if
                    else
                        in_str = .false.
                    end if
                end if
                code(i:i) = ' '
            end if
            i = i + 1
        end do
    end subroutine mask_code

    subroutine append_joined(buf, code)
        character(len=*), intent(inout) :: buf
        character(len=*), intent(in) :: code

        if (len_trim(buf) == 0) then
            buf = adjustl(code)
        else if (len_trim(buf) + len_trim(code) + 1 <= len(buf)) then
            buf = trim(buf)//' '//trim(adjustl(code))
        end if
    end subroutine append_joined

    logical function ends_with_amp(code)
        character(len=*), intent(in) :: code
        integer :: e

        ends_with_amp = .false.
        e = len_trim(code)
        if (e >= 1) ends_with_amp = code(e:e) == '&'
    end function ends_with_amp

    subroutine drop_trailing_amp(code)
        character(len=*), intent(inout) :: code
        integer :: e

        e = len_trim(code)
        if (e >= 1) code(e:e) = ' '
    end subroutine drop_trailing_amp

    subroutine drop_leading_amp(code)
        character(len=*), intent(inout) :: code
        integer :: i

        i = 1
        do
            if (i > len_trim(code)) return
            if (code(i:i) /= ' ') exit
            i = i + 1
        end do
        if (code(i:i) == '&') code(i:i) = ' '
    end subroutine drop_leading_amp

    subroutine rstrip(s)
        character(len=*), intent(inout) :: s
        integer :: e

        e = len_trim(s)
        if (e < len(s)) s(e + 1:) = ''
    end subroutine rstrip

    pure logical function is_ident_char(c)
        character(len=1), intent(in) :: c
        integer :: ic

        ic = iachar(c)
        is_ident_char = (ic >= iachar('a') .and. ic <= iachar('z')) .or. &
            (ic >= iachar('A') .and. ic <= iachar('Z')) .or. &
            (ic >= iachar('0') .and. ic <= iachar('9')) .or. c == '_'
    end function is_ident_char

    pure character(len=1) function lower_ch(c)
        character(len=1), intent(in) :: c
        integer :: ic

        ic = iachar(c)
        if (ic >= iachar('A') .and. ic <= iachar('Z')) then
            lower_ch = achar(ic + 32)
        else
            lower_ch = c
        end if
    end function lower_ch

end module fo_lint_lex
