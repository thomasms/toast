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

!! With fortran we cannot have protected type inheritance like in C++
!! the alternative is to put everything in one module, but this is dirty
!! and hard to maintain.
!! So we use include with a private type in each module that inherits from it.
!! Since it is private there are no issues with duplicate declarations
!! and we still only define it once.
#include "types/testobjectbase.h"

    !> The base test case type used for assertions
    !! This can be used in two ways
    !! 1. Use it to do simple assertions without run
    !! 2. Extend this and implement run subroutine
    type, extends(TestObject), public :: TestCase
        type(string_t), dimension(:), allocatable :: messages
    contains
        procedure :: init                       !< Initialise
        procedure :: reset                      !< Reset test case and counts
        procedure :: run                        !< Runs test case by calling init and test
        procedure :: getnrofmessages            !< Gets the number of messages
        procedure :: getmessage                 !< Gets the message at a given index
        procedure :: test                       !< The test case assertions - does nothing for base type
        procedure :: asserttrue                 !< Assert condition is true
        procedure :: assertfalse                !< Assert condition is false
        procedure :: assertequal_str            !< Assert character arrays are equal
        procedure :: assertequal_ki1            !< Assert integers are equal (ki1)
        procedure :: assertequal_ki2            !< Assert integers are equal (ki2)
        procedure :: assertequal_ki4            !< Assert integers are equal (ki4)
        procedure :: assertequal_ki8            !< Assert integers are equal (ki8)
        procedure :: assertequal_kr4            !< Assert reals are equal with tolerance (kr4)
        procedure :: assertequal_kr8            !< Assert reals are equal with tolerance (kr8)
        procedure :: assertequal_kr16           !< Assert reals are equal with tolerance (kr16)
        procedure :: assertequalarray_ki1       !< Assert integer arrays are equal (ki1)
        procedure :: assertequalarray_ki2       !< Assert integer arrays are equal (ki2)
        procedure :: assertequalarray_ki4       !< Assert integer arrays are equal (ki4)
        procedure :: assertequalarray_ki8       !< Assert integer arrays are equal (ki8)
          generic :: assertequal => assertequal_str, &
                                    assertequal_ki1, &
                                    assertequal_ki2, &
                                    assertequal_ki4, &
                                    assertequal_ki8, &
                                    assertequalarray_ki1, &
                                    assertequalarray_ki2, &
                                    assertequalarray_ki4, &
                                    assertequalarray_ki8, &
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
    subroutine init(this, name)
        class(TestCase), intent(inout) :: this         !< Test case type
        character(*), optional :: name

        if(present(name))then
            this%name = name
        end if

    end subroutine init

    !> Finalize
    subroutine finalize(this)
        type(TestCase), intent(inout) :: this         !< Test case type

        call this%cleanup()

#ifndef TOAST_NO_STOP
        call this%checkfailure()
#endif

    end subroutine finalize

    !> Check for failures and stops with an error code is so.
    subroutine checkfailure(this)
        class(TestObject), intent(in) :: this         !< Test case type

        if(this%fcount > 0)then
            stop 1
        end if

    end subroutine checkfailure

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

        ! Cleanup
        call this%cleanup()

    end subroutine reset

    !> Cleanup
    subroutine cleanup(this)
        class(TestCase), intent(inout) :: this         !< Test case type

        if(this%arraysize > 0 .and. allocated(this%messages))then
            deallocate(this%messages)
            this%arraysize = 0
        endif

    end subroutine cleanup

    !> Cleanup
    subroutine poly_cleanup(this)
        class(TestCasePoly), intent(inout) :: this         !< Test case type

        if(allocated(this%raw)) deallocate(this%raw)

    end subroutine poly_cleanup

    !> Run the test by calling init and then test which must be implemented
    !! in subclasses
    !! returns true if no failures (failed asserts) occurred
    logical function run(this)
        class(TestCase), intent(inout) :: this     !< Test case type

        run = .false.
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

    !> Get the number of messages
    integer(ki4) function getnrofmessages(this)
        class(TestCase), intent(in) :: this        !< Test case type

        getnrofmessages = this%arraysize

    end function getnrofmessages

    !> Get the message at index
    function getmessage(this, index) result(message)
        class(TestCase), intent(in) :: this        !< Test case type
        integer(ki4), intent(in)    :: index       !< index of message must be >0 and <= getnrofmessages
        character(:), allocatable   :: message

        if(index > 0 .and. index <= this%arraysize)then
            message = trim(this%messages(index)%raw)
        else
            message = ""
        end if

    end function getmessage

#define MACRO_TEST_TYPE TestObject
#include "definecounts.h"

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

    !> Assert strings are equal
    subroutine assertequal_str(this, str1, str2, message)
        class(TestCase), intent(inout)      :: this         !< Test case type
        character(*), intent(in)            :: str1
        character(*), intent(in)            :: str2
        character(*), intent(in), optional  :: message

        if(trim(adjustl(str1)) == trim(adjustl(str2)))then
            this%pcount = this%pcount + 1_ki4
        else
            this%fcount = this%fcount + 1_ki4
            if(present(message)) then
                call this%appendmessage(message)
            end if
        end if

    end subroutine assertequal_str

!! Integer asserts
#define MACRO_INT_TYPE ki1
#include "asserts/assertequalintegertemplate.h"

#define MACRO_INT_TYPE ki2
#include "asserts/assertequalintegertemplate.h"

#define MACRO_INT_TYPE ki4
#include "asserts/assertequalintegertemplate.h"

#define MACRO_INT_TYPE ki8
#include "asserts/assertequalintegertemplate.h"

!! Integer 1d array asserts
#define MACRO_INT_TYPE ki1
#include "asserts/assertequalintegerarraytemplate.h"

#define MACRO_INT_TYPE ki2
#include "asserts/assertequalintegerarraytemplate.h"

#define MACRO_INT_TYPE ki4
#include "asserts/assertequalintegerarraytemplate.h"

#define MACRO_INT_TYPE ki8
#include "asserts/assertequalintegerarraytemplate.h"

!! Real asserts
#define MACRO_REAL_TYPE kr4
#include "asserts/assertequalrealtemplate.h"

#define MACRO_REAL_TYPE kr8
#include "asserts/assertequalrealtemplate.h"

#define MACRO_REAL_TYPE kr16
#include "asserts/assertequalrealtemplate.h"

    !> Add a message to the test case
    subroutine appendmessage(this, message)
        class(TestCase), intent(inout) :: this         !< Test case type
        character(*), intent(in)       :: message

        type(string_t), dimension(:), allocatable :: tmp
        integer(ki4) :: prevcount
        integer(ki4) :: prevsize
        integer(ki4) :: newsize

        ! the actual number of items appended
        prevcount = this%arraysize

        ! the actual size of the container - not necessarily the same as the count
        prevsize = 0_ki4
        if(allocated(this%messages))then
            prevsize = size(this%messages)
        end if

        if(prevcount >= prevsize)then
            newsize = max(prevsize*this%scalefactor, prevsize+1_ki4)
            allocate(tmp(1_ki4:newsize))
            if(prevsize > 0_ki4)then
                tmp(1_ki4:prevsize) = this%messages
            end if
            call move_alloc(tmp, this%messages)
        end if

        this%arraysize = this%arraysize + 1_ki4
        this%messages(this%arraysize)%raw = trim(message)

    end subroutine appendmessage

    !> constructor
    function constructor()
        type(TestCase) :: constructor      !< test case type to construct

        call constructor%init()
    end function constructor

end module toast_test_case_m
