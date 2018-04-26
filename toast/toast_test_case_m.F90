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

!> Test case module for TOAST
module toast_test_case_m
    use fork_m
    use toast_util_m
    implicit none
    private

    !> The test case type used for assertions
    type, public :: TestCase
    private
        integer(ki4) :: passcount        = 0_ki4
        integer(ki4) :: failcount        = 0_ki4
        integer(ki4) :: ignoredcount     = 0_ki4
        integer(ki4) :: totalcount       = 0_ki4
        integer(ki4) :: arraysize        = 0_ki4
        logical      :: isinit           = .false.
        type(string_t), dimension(:), allocatable :: messages
    contains
        procedure :: init                       !< Initialise
        procedure :: reset                      !< Reset test case and counts
        procedure :: passrate                   !< passcount/(passcount + failcount)
        procedure :: failrate                   !< failcount/(passcount + failcount)
        procedure :: printsummary               !< Print the counts
        procedure :: asserttrue                 !< Assert condition is true
        procedure :: assertfalse                !< Assert condition is false
        procedure :: assertequal_ki1            !< Assert integers are equal (ki1)
        procedure :: assertequal_ki2            !< Assert integers are equal (ki2)
        procedure :: assertequal_ki4            !< Assert integers are equal (ki4)
        procedure :: assertequal_ki8            !< Assert integers are equal (ki8)
        procedure :: assertequal_kr4            !< Assert reals are equal with tolerance (kr4)
        procedure :: assertequal_kr8            !< Assert reals are equal with tolerance (kr8)
        procedure :: assertequal_kr16           !< Assert reals are equal with tolerance (kr16)
          generic :: assertequal => assertequal_ki1, &
                                    assertequal_ki2, &
                                    assertequal_ki4, &
                                    assertequal_ki8, &
                                    assertequal_kr4, &
                                    assertequal_kr8, &
                                    assertequal_kr16
        procedure, private :: appendmessage
        procedure, private :: cleanup
                     final :: finalize
    end type TestCase

contains

    !> Initialise
    subroutine init(this)
        class(TestCase), intent(inout) :: this

        if(this%isinit .eqv. .false.) then
            allocate(this%messages(0))
            this%isinit = .true.
        endif

    end subroutine init

    !> Finalize
    subroutine finalize(this)
        type(TestCase), intent(inout) :: this

        call this%cleanup()

    end subroutine finalize

    !> Reset all counts and messages
    subroutine reset(this)
        class(TestCase), intent(inout) :: this

        ! reset counts
        this%passcount    = 0_ki4
        this%failcount    = 0_ki4
        this%ignoredcount = 0_ki4
        this%totalcount   = 0_ki4

        ! reset messages by init then cleanup
        !! this maybe slow - not sure
        call this%cleanup()
        call this%init()

    end subroutine reset

    !> Cleanup
    subroutine cleanup(this)
        class(TestCase), intent(inout) :: this

        if(this%isinit .eqv. .true.) then
            deallocate(this%messages)
            this%isinit = .false.
        endif

        this%arraysize = 0_ki4

    end subroutine cleanup

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

        write(*, "(A)") "==============================="
        write(*, "(A, I5.1, A, I5.1)") "Passed assertions:", &
              & this%passcount, " / ", this%totalcount
        write(*, "(A, I5.1, A, I5.1)") "Failed assertions:",  &
              & this%failcount, " / ", this%totalcount
        write(*, "(A)") "==============================="

        do i = 1_ki4, this%arraysize
            write(*, "(A)") "FAILED - "//trim(this%messages(i)%raw)
        end do

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

    !> Add a message to the test case
    subroutine appendmessage(this, message)
        class(TestCase), intent(inout) :: this
        character(*), intent(in)       :: message

        type(string_t), dimension(:), allocatable :: tmp
        integer(ki4) :: prevsize

        if(this%isinit .eqv. .true.) then
            prevsize = this%arraysize
            allocate(tmp(prevsize + 1_ki4))
            tmp(1_ki4:prevsize) = this%messages
            !deallocate(this%testcases)
            call move_alloc(tmp, this%messages)
            this%arraysize = this%arraysize + 1_ki4
            this%messages(this%arraysize)%raw = trim(message)
        endif

    end subroutine appendmessage

end module toast_test_case_m
