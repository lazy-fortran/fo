module fo_compiler_service
    implicit none
    private

    integer, parameter, public :: COMPILER_SERVICE_UNAVAILABLE = 69
    integer, parameter, public :: COMPILER_EMIT_OBJECT = 1

    type, public :: compiler_service_request_t
        character(len=512) :: source_path = ''
        character(len=512) :: output_path = ''
        character(len=512) :: module_dir = ''
        character(len=512) :: flags = ''
        integer :: emit_kind = COMPILER_EMIT_OBJECT
    end type compiler_service_request_t

    type, public :: compiler_service_result_t
        logical :: supported = .false.
        integer :: exitcode = COMPILER_SERVICE_UNAVAILABLE
        character(len=256) :: diagnostic = ''
    end type compiler_service_result_t

    type, abstract, public :: compiler_service_t
    contains
        procedure(service_name_interface), deferred :: name
        procedure(service_available_interface), deferred :: available
        procedure(service_compile_interface), deferred :: compile
    end type compiler_service_t

    type, extends(compiler_service_t), public :: lfortran_service_stub_t
        character(len=16) :: service_name = 'lfortran'
        logical :: wired = .false.
    contains
        procedure :: name => lfortran_name
        procedure :: available => lfortran_available
        procedure :: compile => lfortran_compile
    end type lfortran_service_stub_t

    type, extends(compiler_service_t), public :: fortfront_service_stub_t
        character(len=16) :: service_name = 'fortfront'
        logical :: wired = .false.
    contains
        procedure :: name => fortfront_name
        procedure :: available => fortfront_available
        procedure :: compile => fortfront_compile
    end type fortfront_service_stub_t

    abstract interface
        function service_name_interface(self) result(name)
            import :: compiler_service_t
            class(compiler_service_t), intent(in) :: self
            character(len=:), allocatable :: name
        end function service_name_interface

        logical function service_available_interface(self)
            import :: compiler_service_t
            class(compiler_service_t), intent(in) :: self
        end function service_available_interface

        subroutine service_compile_interface(self, request, result)
            import :: compiler_service_request_t, compiler_service_result_t
            import :: compiler_service_t
            class(compiler_service_t), intent(in) :: self
            type(compiler_service_request_t), intent(in) :: request
            type(compiler_service_result_t), intent(out) :: result
        end subroutine service_compile_interface
    end interface

contains

    function lfortran_name(self) result(name)
        class(lfortran_service_stub_t), intent(in) :: self
        character(len=:), allocatable :: name

        name = trim(self%service_name)
    end function lfortran_name

    logical function lfortran_available(self)
        class(lfortran_service_stub_t), intent(in) :: self

        lfortran_available = self%wired
    end function lfortran_available

    subroutine lfortran_compile(self, request, result)
        class(lfortran_service_stub_t), intent(in) :: self
        type(compiler_service_request_t), intent(in) :: request
        type(compiler_service_result_t), intent(out) :: result

        call unavailable_result(self%service_name, request, result)
    end subroutine lfortran_compile

    function fortfront_name(self) result(name)
        class(fortfront_service_stub_t), intent(in) :: self
        character(len=:), allocatable :: name

        name = trim(self%service_name)
    end function fortfront_name

    logical function fortfront_available(self)
        class(fortfront_service_stub_t), intent(in) :: self

        fortfront_available = self%wired
    end function fortfront_available

    subroutine fortfront_compile(self, request, result)
        class(fortfront_service_stub_t), intent(in) :: self
        type(compiler_service_request_t), intent(in) :: request
        type(compiler_service_result_t), intent(out) :: result

        call unavailable_result(self%service_name, request, result)
    end subroutine fortfront_compile

    subroutine unavailable_result(name, request, result)
        character(len=*), intent(in) :: name
        type(compiler_service_request_t), intent(in) :: request
        type(compiler_service_result_t), intent(out) :: result

        result%supported = .false.
        result%exitcode = COMPILER_SERVICE_UNAVAILABLE
        result%diagnostic = trim(name)//' compiler service is not wired'
        if (len_trim(request%source_path) > 0) &
            result%diagnostic = trim(result%diagnostic)//': '//trim(request%source_path)
    end subroutine unavailable_result

end module fo_compiler_service
