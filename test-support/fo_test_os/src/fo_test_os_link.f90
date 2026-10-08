module fo_test_os_link
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
    private
    public :: test_os_initialize

    interface
        integer(c_int) function c_test_os_initialize() &
                bind(C, name='fo_test_os_initialize')
            import :: c_int
        end function c_test_os_initialize
    end interface

contains

    integer function test_os_initialize() result(value)
        value = int(c_test_os_initialize())
    end function test_os_initialize

end module fo_test_os_link
