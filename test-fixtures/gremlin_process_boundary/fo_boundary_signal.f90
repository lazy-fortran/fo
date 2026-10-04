module fo_boundary_signal
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
contains
    subroutine ignore_term(signal_number) bind(C, name='fo_process_boundary_ignore_term')
        integer(c_int), value :: signal_number
        if (signal_number == 0_c_int) return
    end subroutine ignore_term
end module fo_boundary_signal
