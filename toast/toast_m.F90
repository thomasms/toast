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

    !> Maximum number of messages
    integer(ki4), parameter :: MAX_ASSERT_COUNT = 1000000_ki4

    !> The test case type used for assertions
    type, public :: TestCase
    private
        integer(ki4) :: passcount    = 0_ki4
        integer(ki4) :: failcount    = 0_ki4
        integer(ki4) :: ignoredcount = 0_ki4
        integer(ki4) :: totalcount   = 0_ki4

        type(string_t), dimension(MAX_ASSERT_COUNT) :: messages
        integer(ki4) :: lastmessageindex = 1_ki4
    contains
        procedure :: reset
        procedure :: passrate
        procedure :: failrate
        procedure :: printsummary
        procedure :: asserttrue
        procedure :: assertfalse
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
        procedure, private :: appendmessage
    end type TestCase

contains

    !> Reset all counts and messages
    subroutine reset(this)
        class(TestCase), intent(inout) :: this

        integer(ki4) :: i

        this%passcount    = 0_ki4
        this%failcount    = 0_ki4
        this%ignoredcount = 0_ki4
        this%totalcount   = 0_ki4

        this%lastmessageindex = 1_ki4
        do concurrent (i = lbound(this%messages, 1) : this%lastmessageindex - 1_ki4)
            this%messages(i)%raw = ""
        end do

    end subroutine reset

    !> Fractional pass rate
    pure function passrate(this) result(rate)
        class(TestCase), intent(in) :: this
        real(kr4) :: rate

        rate = getrate(this%passcount, this%totalcount)

    end function passrate

    !> Fractional fail rate
    pure function failrate(this) result(rate)
        class(TestCase), intent(in) :: this
        real(kr4) :: rate

        rate = getrate(this%failcount, this%totalcount)

    end function failrate

    !> Pretty print summary
    subroutine printsummary(this)
        class(TestCase), intent(in) :: this

        integer(ki4) :: i

        do i = lbound(this%messages, 1), this%lastmessageindex - 1_ki4
            write(*, "(A)") "FAILED - "//this%messages(i)%raw
        end do

        write(*, "(A)") "=========================="
        write(*, "(A, I5.1, A, I5.1)") "Passed tests:", &
              & this%passcount, " / ", this%totalcount
        write(*, "(A, I5.1, A, I5.1)") "Failed tests:",  &
              & this%failcount, " / ", this%totalcount
        write(*, "(A)") "=========================="
    end subroutine printsummary

    !> Assert true
    subroutine asserttrue(this, condition, message)
        class(TestCase), intent(inout)      :: this
        logical, intent(in)                 :: condition
        character(*), intent(in), optional  :: message

        if(condition) then
            this%passcount = this%passcount + 1
        else
            this%failcount = this%failcount + 1
            if(present(message)) then
                call this%appendmessage(message)
            end if
        end if
        this%totalcount = this%totalcount + 1

    end subroutine asserttrue

    !> Assert false
    subroutine assertfalse(this, condition, message)
        class(TestCase), intent(inout)      :: this
        logical, intent(in)                 :: condition
        character(*), intent(in), optional  :: message

        call this%asserttrue(.not. condition, message)

    end subroutine assertfalse

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

    subroutine appendmessage(this, message)
        class(TestCase), intent(inout) :: this
        character(*), intent(in)       :: message

        this%messages(this%lastmessageindex)%raw = message
        this%lastmessageindex = this%lastmessageindex + 1_ki4

    end subroutine appendmessage

end module toast_m
