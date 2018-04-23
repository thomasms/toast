!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                               !!
!!                    TOAST                      !!
!!                                               !!
!!      Copyright (c) 2018, Thomas Stainer       !!
!!                                               !!
!!            All rights reserved.               !!
!!    Licensed under the 3-clause BSD license.   !!
!!                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Main module for TOAST
module toast_m
    use fork_m
    use toast_util_m
    implicit none
    private

    !> The test case type used for assertions
    type, public :: TestCase
    private
        integer(ki4) :: passcount    = 0_ki4
        integer(ki4) :: failcount    = 0_ki4
        integer(ki4) :: ignoredcount = 0_ki4
        integer(ki4) :: totalcount   = 0_ki4
    contains
        procedure :: passrate
        procedure :: failrate
        procedure :: printsummary
        procedure :: assertequal_ki1
        procedure :: assertequal_ki2
        procedure :: assertequal_ki4
        procedure :: assertequal_ki8
        procedure :: assertequal_kr4
        procedure :: assertequal_kr8
        procedure :: assertequal_kr16
          generic :: assertequal => assertequal_ki1, &
                                    assertequal_ki2, &
                                    assertequal_ki4, &
                                    assertequal_ki8, &
                                    assertequal_kr4, &
                                    assertequal_kr8, &
                                    assertequal_kr16
    end type TestCase

contains

    pure function passrate(this) result(rate)
        class(TestCase), intent(in) :: this
        real(kr4) :: rate

        rate = getrate(this%passcount, this%totalcount)

    end function passrate

    pure function failrate(this) result(rate)
        class(TestCase), intent(in) :: this
        real(kr4) :: rate

        rate = getrate(this%failcount, this%totalcount)

    end function failrate

    subroutine printsummary(this)
        class(TestCase), intent(in) :: this
        write(*, "(A)") "=========================="
        write(*, "(A, ES14.3)") "PASS RATE:", this%passrate()
        write(*, "(A, ES14.3)") "FAIL RATE:", this%failrate()
        write(*, "(A)") "=========================="
    end subroutine printsummary

!! Integer asserts
#define MACRO_INT_TYPE ki1
#include "assertequalintegertemplate.h"
#undef MACRO_INT_TYPE

#define MACRO_INT_TYPE ki2
#include "assertequalintegertemplate.h"
#undef MACRO_INT_TYPE

#define MACRO_INT_TYPE ki4
#include "assertequalintegertemplate.h"
#undef MACRO_INT_TYPE

#define MACRO_INT_TYPE ki8
#include "assertequalintegertemplate.h"
#undef MACRO_INT_TYPE

!! Real asserts
#define MACRO_REAL_TYPE kr4
#include "assertequalrealtemplate.h"
#undef MACRO_REAL_TYPE

#define MACRO_REAL_TYPE kr8
#include "assertequalrealtemplate.h"
#undef MACRO_REAL_TYPE

#define MACRO_REAL_TYPE kr16
#include "assertequalrealtemplate.h"
#undef MACRO_REAL_TYPE

end module toast_m
