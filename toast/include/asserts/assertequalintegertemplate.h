#include "toastmacros.h"

#ifndef MACRO_ASSERTEQUALINT_NAME
#define MACRO_ASSERTEQUALINT_NAME(type) MACRO_CAT2(assertequal_,type)
#endif

!> We can apply a tolerance to real values
subroutine MACRO_ASSERTEQUALINT_NAME(MACRO_INT_TYPE)(this, a, b, message)
    class(TestCase), intent(inout)      :: this
    integer(MACRO_INT_TYPE), intent(in) :: a
    integer(MACRO_INT_TYPE), intent(in) :: b
    character(*), intent(in), optional  :: message

    if(a == b) then
        this%pcount = this%pcount + 1_ki4
    else
        this%fcount = this%fcount + 1_ki4
        if(present(message)) then
            call this%appendmessage(message)
        end if
    end if

end subroutine MACRO_ASSERTEQUALINT_NAME(MACRO_INT_TYPE)

#undef MACRO_INT_TYPE
