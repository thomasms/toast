#include "toastmacros.h"

#ifndef MACRO_ASSERTEQUALREAL_NAME
#define MACRO_ASSERTEQUALREAL_NAME(type) MACRO_CAT2(assertequal_,type)
#endif

!> We can apply a tolerance to real values
subroutine MACRO_ASSERTEQUALREAL_NAME(MACRO_REAL_TYPE)(this, a, b, rel_tol, abs_tol, message)
    class(TestCase), intent(inout)                  :: this
    real(MACRO_REAL_TYPE), intent(in)               :: a
    real(MACRO_REAL_TYPE), intent(in)               :: b
    real(MACRO_REAL_TYPE), intent(in), optional     :: rel_tol
    real(MACRO_REAL_TYPE), intent(in), optional     :: abs_tol
    character(*), intent(in), optional  :: message

    if(areclose(a, b, rel_tol, abs_tol)) then
        this%pcount = this%pcount + 1_ki4
    else
        this%fcount = this%fcount + 1_ki4
        if(present(message)) then
            call this%appendmessage(message)
        end if
    end if

end subroutine MACRO_ASSERTEQUALREAL_NAME(MACRO_REAL_TYPE)
