module fo_test_os_link
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    private
    public :: test_os_link_probe

    interface
        integer(c_int) function c_test_os_link_probe() &
                bind(C, name='fo_test_link_probe')
            import :: c_int
        end function c_test_os_link_probe
    end interface

contains

    integer function test_os_link_probe() result(value)
        value = int(c_test_os_link_probe())
    end function test_os_link_probe

end module fo_test_os_link
