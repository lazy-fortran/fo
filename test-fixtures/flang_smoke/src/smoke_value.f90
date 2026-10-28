module smoke_value
    implicit none
    private
    public :: answer
contains
    pure integer function answer() result(value)
        value = 41
    end function answer
end module smoke_value
