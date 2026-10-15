module fo_gremlin_retention
    !! Retained Gremlin storage as status data: generations, bytes, pins and live
    !! owners, plus the per-invocation views. Computed by one bounded native scan
    !! (src/proc/fo_gremlin_retention.c) so publishing a status stays cheap.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    implicit none
    private

    public :: gremlin_retention_json

    interface
        integer(c_int) function c_retention(generation_root, views_dir, out) &
                bind(C, name='fo_c_gremlin_retention')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: generation_root(*), views_dir(*)
            integer(c_long_long), intent(out) :: out(7)
        end function c_retention
    end interface

contains

    function gremlin_retention_json(generation_root, views_dir) result(json)
        !! `{"generations":N,"bytes":B,"pinned":P,"owners":O,"views":V,
        !! "view_bytes":VB,"view_bytes_exact":true}`; zeros when the paths do
        !! not exist yet. generation_root is any published generation's root.
        character(len=*), intent(in) :: generation_root, views_dir
        character(len=:), allocatable :: json
        integer(c_long_long) :: v(7)
        integer(c_int) :: rc

        rc = c_retention(trim(generation_root)//c_null_char, &
            trim(views_dir)//c_null_char, v)
        if (rc /= 0) v = 0_c_long_long
        json = '{"generations":'//num(v(1))//',"bytes":'//num(v(2))// &
            ',"pinned":'//num(v(3))//',"owners":'//num(v(4))// &
            ',"views":'//num(v(5))//',"view_bytes":'//num(v(6))// &
            ',"view_bytes_exact":'
        if (v(7) /= 0_c_long_long) then
            json = json//'true}'
        else
            json = json//'false}'
        end if
    end function gremlin_retention_json

    function num(value) result(text)
        integer(c_long_long), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function num

end module fo_gremlin_retention
