module bench_json
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    integer, parameter, public :: json_invalid = 0, json_object = 1, json_array = 2
    integer, parameter, public :: json_string = 3, json_number = 4
    integer, parameter, public :: json_boolean = 5, json_null = 6
    type, public :: json_value_t
        integer :: kind = json_invalid
        logical :: boolean = .false.
        character(:), allocatable :: text, name
        type(json_value_t), allocatable :: children(:)
    contains
        procedure, private :: assign_value
        generic, public :: assignment(=) => assign_value
    end type json_value_t
    public :: json_parse, json_member, json_number_value, json_string_value
contains
    subroutine json_parse(source, value, valid, message)
        character(len=*), intent(in) :: source
        type(json_value_t), intent(out) :: value
        logical, intent(out) :: valid
        character(:), allocatable, intent(out) :: message
        integer :: position
        position = 1
        call parse_value(source, position, value, valid, message)
        if (.not. valid) return
        call skip_space(source, position)
        if (position <= len(source)) then
            valid = .false.; message = 'trailing JSON bytes'
        end if
    end subroutine json_parse

    recursive subroutine parse_value(s, p, v, ok, message)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        type(json_value_t), intent(out) :: v
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        call skip_space(s,p)
        ok = .false.; message = 'expected JSON value'
        if (p > len(s)) return
        select case (s(p:p))
        case ('{'); call parse_object(s,p,v,ok,message)
        case ('['); call parse_array(s,p,v,ok,message)
        case ('"')
            v%kind=json_string; call parse_string(s,p,v%text,ok,message)
        case ('t'); call literal(s,p,'true',json_boolean,v,ok,message)
        case ('f'); call literal(s,p,'false',json_boolean,v,ok,message)
        case ('n'); call literal(s,p,'null',json_null,v,ok,message)
        case ('-', '0':'9'); call parse_number(s,p,v,ok,message)
        end select
    end subroutine parse_value

    recursive subroutine parse_object(s,p,v,ok,message)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        type(json_value_t), intent(out) :: v
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(json_value_t) :: child
        character(:), allocatable :: key
        character :: d
        v%kind=json_object; allocate(v%children(0)); p=p+1
        call skip_space(s,p)
        if (p <= len(s)) then
            if (s(p:p)=='}') then
                p=p+1; ok=.true.; message=''; return
            end if
        end if
        do
            if (p>len(s)) then
                ok=.false.; message='unterminated object'; return
            end if
            if (s(p:p)/='"') then
                ok=.false.; message='expected object key'; return
            end if
            call parse_string(s,p,key,ok,message); if (.not.ok) return
            call skip_space(s,p)
            if (p>len(s)) then
                ok=.false.; message='expected colon'; return
            end if
            if (s(p:p)/=':') then
                ok=.false.; message='expected colon'; return
            end if
            p=p+1; call parse_value(s,p,child,ok,message); if (.not.ok) return
            child%name=key; call append(v,child); call skip_space(s,p)
            if (p>len(s)) then
                ok=.false.; message='unterminated object'; return
            end if
            d=s(p:p); p=p+1
            if (d=='}') exit
            if (d/=',') then
                ok=.false.; message='expected comma or }'; return
            end if
            call skip_space(s,p)
        end do
        ok=.true.; message=''
    end subroutine parse_object

    recursive subroutine parse_array(s,p,v,ok,message)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        type(json_value_t), intent(out) :: v
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(json_value_t) :: child
        character :: d
        v%kind=json_array; allocate(v%children(0)); p=p+1; call skip_space(s,p)
        if (p<=len(s)) then
            if (s(p:p)==']') then
                p=p+1; ok=.true.; message=''; return
            end if
        end if
        do
            call parse_value(s,p,child,ok,message); if (.not.ok) return
            call append(v,child); call skip_space(s,p)
            if (p>len(s)) then
                ok=.false.; message='unterminated array'; return
            end if
            d=s(p:p); p=p+1
            if (d==']') exit
            if (d/=',') then
                ok=.false.; message='expected comma or ]'; return
            end if
        end do
        ok=.true.; message=''
    end subroutine parse_array

    subroutine parse_string(s,p,value,ok,message)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        character(:), allocatable, intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        character :: c
        value=''; p=p+1; ok=.false.; message='unterminated string'
        do while (p<=len(s))
            c=s(p:p); p=p+1
            if (c=='"') then
                ok=.true.; message=''; return
            else if (c==achar(92)) then
                if (p>len(s)) exit
                c=s(p:p); p=p+1
                select case(c)
                case ('"','/'); value=value//c
                case (achar(92)); value=value//achar(92)
                case ('n'); value=value//achar(10)
                case ('r'); value=value//achar(13)
                case ('t'); value=value//achar(9)
                case ('b'); value=value//achar(8)
                case ('f'); value=value//achar(12)
                case ('u')
                    block
                        integer :: codepoint,j,digit
                        codepoint=0
                        if(p+3>len(s)) then
                            message='incomplete unicode escape'; return
                        end if
                        do j=0,3
                            digit=hex_digit(s(p+j:p+j))
                            if(digit<0) then
                                message='invalid unicode escape'; return
                            end if
                            codepoint=16*codepoint+digit
                        end do
                        p=p+4
                        if(codepoint>=55296 .and. codepoint<=57343) then
                            message='surrogate unicode escape is unsupported'; return
                        else if(codepoint<128) then
                            value=value//achar(codepoint)
                        else if(codepoint<2048) then
                            value=value//achar(192+codepoint/64)//achar(128+mod(codepoint,64))
                        else
                            value=value//achar(224+codepoint/4096)// &
                                achar(128+mod(codepoint/64,64))//achar(128+mod(codepoint,64))
                        end if
                    end block
                case default; ok=.false.; message='unsupported JSON escape'; return
                end select
            else
                if (iachar(c)<32) then
                    ok=.false.; message='control byte in string'; return
                end if
                value=value//c
            end if
        end do
    end subroutine parse_string

    integer function hex_digit(c) result(value)
        character(len=1), intent(in) :: c
        select case(c)
        case('0':'9'); value=iachar(c)-iachar('0')
        case('a':'f'); value=10+iachar(c)-iachar('a')
        case('A':'F'); value=10+iachar(c)-iachar('A')
        case default; value=-1
        end select
    end function hex_digit

    subroutine parse_number(s,p,v,ok,message)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        type(json_value_t), intent(out) :: v
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: start
        start=p
        do while(p<=len(s))
            if(index('0123456789+-.eE',s(p:p))==0) exit
            p=p+1
        end do
        v%kind=json_number; v%text=s(start:p-1)
        block
            real(real64) :: x
            integer :: ios
            read(v%text,*,iostat=ios) x
            ok=ios==0 .and. len(v%text)>0
        end block
        message='invalid number'
    end subroutine parse_number

    subroutine literal(s,p,word,kind,v,ok,message)
        character(len=*), intent(in) :: s,word
        integer, intent(inout) :: p
        integer, intent(in) :: kind
        type(json_value_t), intent(out) :: v
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        ok=p+len(word)-1<=len(s)
        if(ok) ok=s(p:p+len(word)-1)==word
        if(ok) then
            p=p+len(word); v%kind=kind; v%boolean=word=='true'; message=''
        else
            message='invalid JSON literal'
        end if
    end subroutine literal

    subroutine skip_space(s,p)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: p
        do while(p<=len(s))
            if(index(' '//achar(9)//achar(10)//achar(13),s(p:p))==0) exit
            p=p+1
        end do
    end subroutine skip_space

    subroutine append(v,child)
        type(json_value_t), intent(inout) :: v
        type(json_value_t), intent(in) :: child
        type(json_value_t), allocatable :: grown(:)
        integer :: n
        n=size(v%children); allocate(grown(n+1)); if(n>0) grown(:n)=v%children
        grown(n+1)=child; call move_alloc(grown,v%children)
    end subroutine append

    function json_member(v,key) result(child)
        type(json_value_t), intent(in) :: v
        character(len=*), intent(in) :: key
        type(json_value_t) :: child
        integer :: i
        if(.not.allocated(v%children)) return
        do i=1,size(v%children)
            if(v%children(i)%name==key) then
                child=v%children(i); return
            end if
        end do
    end function json_member

    real(real64) function json_number_value(v) result(number)
        type(json_value_t), intent(in) :: v
        integer :: ios
        number=0.0_real64; if(v%kind/=json_number) return
        read(v%text,*,iostat=ios) number
        if(ios/=0) number=0.0_real64
    end function json_number_value

    function json_string_value(v) result(value)
        type(json_value_t), intent(in) :: v
        character(:), allocatable :: value
        value=''; if(v%kind==json_string) value=v%text
    end function json_string_value

    subroutine assign_value(lhs,rhs)
        class(json_value_t), intent(inout) :: lhs
        class(json_value_t), intent(in) :: rhs
        lhs%kind=rhs%kind; lhs%boolean=rhs%boolean
        if(allocated(rhs%text)) lhs%text=rhs%text
        if(allocated(rhs%name)) lhs%name=rhs%name
        if(allocated(rhs%children)) lhs%children=rhs%children
    end subroutine assign_value
end module bench_json
