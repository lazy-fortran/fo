module fo_scan_types
    implicit none
    private

    integer, parameter, public :: MAX_NAME = 128
    integer, parameter, public :: MAX_PATH = 512
    integer, parameter, public :: MAX_UNITS = 8192
    integer, parameter, public :: MAX_DEPS = 64

    type, public :: scan_unit_t
        character(len=MAX_PATH) :: filename = ''
        character(len=MAX_NAME) :: module_name = ''
        character(len=MAX_NAME) :: program_name = ''
        logical :: is_program = .false.
        logical :: is_test = .false.
        integer :: n_deps = 0
        character(len=MAX_NAME) :: deps(MAX_DEPS)
    end type scan_unit_t

end module fo_scan_types
