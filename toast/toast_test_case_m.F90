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

    !> The base test case type used for assertions
    !! This can be used in two ways
    !! 1. Use it to do simple assertions without run
    !! 2. Extend this and implement run subroutine
    type, public :: TestCase
    private
        logical      :: isinit    = .false.
        integer(ki4) :: arraysize = 0_ki4
        integer(ki4) :: pcount    = 0_ki4
        integer(ki4) :: fcount    = 0_ki4
        type(string_t), dimension(:), allocatable :: messages
        character(25), public :: name = "Test Case"
    contains
        procedure :: init                       !< Initialise
        procedure :: reset                      !< Reset test case and counts
        procedure :: passcount                  !< Pass count
        procedure :: failcount                  !< Fail count
        procedure :: printsummary               !< Print the counts
        procedure :: run                        !< Runs test case by calling init and test
        procedure :: test                       !< The test case assertions - does nothing for base type
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

    !> Create a constructor for the test case
    interface TestCase
        procedure constructor       !< construct and initialize the suite
    end interface

    !> Polymorphic type wrapping testcase, must be allocatable
    type, public :: TestCasePoly
        class(TestCase), allocatable :: raw
    contains
        procedure, private :: cleanup => poly_cleanup
                     final :: poly_finalize
    end type TestCasePoly

contains

    !> Initialise
    subroutine init(this)
        class(TestCase), intent(inout) :: this         !< Test case type

        if(this%isinit .eqv. .false.) then
            this%arraysize = 0_ki4
            allocate(this%messages(this%arraysize))
            this%isinit = .true.
        endif

    end subroutine init

    !> Finalize
    subroutine finalize(this)
        type(TestCase), intent(inout) :: this         !< Test case type

        call this%cleanup()

    end subroutine finalize

    !> Test case Finalize
    subroutine poly_finalize(this)
        type(TestCasePoly), intent(inout) :: this         !< Test case type

        call this%cleanup()

    end subroutine poly_finalize

    !> Reset all counts and messages
    subroutine reset(this)
        class(TestCase), intent(inout) :: this         !< Test case type

        ! reset counts
        this%pcount = 0_ki4
        this%fcount = 0_ki4

        ! reset messages by init then cleanup
        !! this maybe slow - not sure
        deallocate(this%messages)
        this%arraysize = 0_ki4
        allocate(this%messages(this%arraysize))

    end subroutine reset

    !> Cleanup
    subroutine cleanup(this)
        class(TestCase), intent(inout) :: this         !< Test case type

        if(this%isinit .eqv. .true.) then
            deallocate(this%messages)
            this%isinit = .false.
            this%arraysize = 0_ki4
        endif

    end subroutine cleanup

    !> Cleanup
    subroutine poly_cleanup(this)
        class(TestCasePoly), intent(inout) :: this         !< Test case type

        if(allocated(this%raw)) then
            deallocate(this%raw)
        endif

    end subroutine poly_cleanup

    !> Run the test by calling init and then test which must be implemented
    !! in subclasses
    !! returns true if no failures (failed asserts) occurred
    logical function run(this)
        class(TestCase), intent(inout) :: this     !< Test case type

        run = .false.
        call this%init()
        call this%test()
        if(this%fcount == 0_ki4)then
            run = .true.
        end if
    end function run

    !> The test case assertions go here - does nothing for this base class
    !! must be extended for use in test suite
    subroutine test(this)
        class(TestCase), intent(inout) :: this     !< Test case type

        ! does nothing here
    end subroutine test

    !> Return the pass count
    integer(ki4) function passcount(this)
        class(TestCase), intent(in) :: this     !< Test case type

        passcount = this%pcount
    end function passcount

    !> Return the fail count
    integer(ki4) function failcount(this)
        class(TestCase), intent(in) :: this     !< Test case type

        failcount = this%fcount
    end function failcount

    !> Pretty print summary
    subroutine printsummary(this)
        class(TestCase), intent(in) :: this     !< Test case type

        integer(ki4) :: i

        write(*, "(A)") ""
        write(*, "(A)") "==============================="
        write(*, "(A27)") " "//this%name//" "
        write(*, "(A)") "==============================="
        write(*, "(A, I5.1, A, I5.1)") "Passed assertions:", &
              & this%pcount, " / ", this%pcount + this%fcount
        write(*, "(A, I5.1, A, I5.1)") "Failed assertions:",  &
              & this%fcount, " / ", this%pcount + this%fcount
        write(*, "(A)") "==============================="

        do i = 1_ki4, this%arraysize
            write(*, "(A)") " !!! FAILED - "//trim(this%messages(i)%raw)
        end do

    end subroutine printsummary

    !> Assert true
    subroutine asserttrue(this, condition, message)
        class(TestCase), intent(inout)      :: this         !< Test case type
        logical, intent(in)                 :: condition
        character(*), intent(in), optional  :: message

        if(condition) then
            this%pcount = this%pcount + 1_ki4
        else
            this%fcount = this%fcount + 1_ki4
            if(present(message)) then
                call this%appendmessage(message)
            end if
        end if

    end subroutine asserttrue

    !> Assert false
    subroutine assertfalse(this, condition, message)
        class(TestCase), intent(inout)      :: this         !< Test case type
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
        class(TestCase), intent(inout) :: this         !< Test case type
        character(*), intent(in)       :: message

        type(string_t), dimension(:), allocatable :: tmp
        integer(ki4) :: prevsize

        if(this%isinit .eqv. .true.) then
            prevsize = this%arraysize
            allocate(tmp(prevsize + 1_ki4))
            tmp(1_ki4:prevsize) = this%messages
            call move_alloc(tmp, this%messages)
            this%arraysize = this%arraysize + 1_ki4
            this%messages(this%arraysize)%raw = trim(message)
        endif

    end subroutine appendmessage

    !> constructor
    function constructor()
        type(TestCase) :: constructor      !< test case type to construct

        call constructor%init()
    end function constructor

end module toast_test_case_m