#include "toastmacros.h"

#ifndef MACRO_ASSERTEQUALINTARRAY_NAME
#define MACRO_ASSERTEQUALINTARRAY_NAME(type) MACRO_CAT2(assertequalarray_,type)
#endif

!> We can apply a tolerance to real values
subroutine MACRO_ASSERTEQUALINTARRAY_NAME(MACRO_INT_TYPE)(this, a, b, message)
    class(TestCase), intent(inout)                    :: this
    integer(MACRO_INT_TYPE), dimension(:), intent(in) :: a
    integer(MACRO_INT_TYPE), dimension(:), intent(in) :: b
    character(*), intent(in), optional  :: message

    integer(ki4) :: i
    logical :: areequal

    areequal = .false.
    if((lbound(a, 1) == lbound(b, 1)) .and. (ubound(a, 1) == ubound(b, 1))) then
        do i = lbound(a, 1), ubound(a, 1)
            if(a(i) == b(i))then
                areequal = .true.
            else
                areequal = .false.
                exit
            end if
        end do
    else
        areequal = .false.
    end if

    if(.not. areequal)then
        this%fcount = this%fcount + 1_ki4
        if(present(message)) then
            call this%appendmessage(message)
        end if
    else
        this%pcount = this%pcount + 1_ki4
    end if

end subroutine MACRO_ASSERTEQUALINTARRAY_NAME(MACRO_INT_TYPE)

#undef MACRO_INT_TYPE
