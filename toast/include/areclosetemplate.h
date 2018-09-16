#include "toastmacros.h"

#ifndef MACRO_ARECLOSE_NAME
#define MACRO_ARECLOSE_NAME(type) MACRO_CAT2(areclose_,type)
#endif

logical pure function MACRO_ARECLOSE_NAME(MACRO_REAL_TYPE)(a, b, rel_tol, abs_tol)
    real(MACRO_REAL_TYPE), intent(in)               :: a
    real(MACRO_REAL_TYPE), intent(in)               :: b
    real(MACRO_REAL_TYPE), intent(in), optional     :: rel_tol
    real(MACRO_REAL_TYPE), intent(in), optional     :: abs_tol

    real(MACRO_REAL_TYPE) :: rt, at

    rt = MACRO_CAT2(0.0_,MACRO_REAL_TYPE)
    at = MACRO_CAT2(0.0_,MACRO_REAL_TYPE)
    if(present(rel_tol)) then
        rt = rel_tol
    end if
    if(present(abs_tol)) then
        at = abs_tol
    end if

    MACRO_ARECLOSE_NAME(MACRO_REAL_TYPE) = abs(a - b) <= max(rt * max(abs(a), abs(b)), at)

end function MACRO_ARECLOSE_NAME(MACRO_REAL_TYPE)

#undef MACRO_REAL_TYPE
