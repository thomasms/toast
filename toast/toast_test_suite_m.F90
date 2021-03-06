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

!> Test suite module for TOAST
module toast_test_suite_m
    use fork_m
    use toast_test_case_m
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

    !> Test suite - a collection of test cases
    type, extends(TestObject), public :: TestSuite
    private
        type(TestCasePoly), dimension(:), allocatable  :: testcases
    contains
        procedure :: init                       !< Initialise
        procedure :: length                     !< Get the size
        procedure :: append                     !< Append a test case
        procedure :: iterate                    !< Iterate through test cases - intent(inout)
        procedure :: iterate_const              !< Iterate through test cases - intent(in)
        procedure :: runall                     !< Run all test cases
            final :: finalize
        procedure, private :: cleanup
    end type TestSuite

    !> Create a constructor for the suite
    interface TestSuite
        procedure constructor       !< construct and initialize the suite
    end interface

contains

    !> Initialise
    subroutine init(this, name)
        class(TestSuite), intent(inout)    :: this
        character(*), intent(in), optional :: name

        if(present(name))then
            this%name = name
        end if

    end subroutine init

    !> Finalize
    subroutine finalize(this)
        type(TestSuite), intent(inout) :: this

        call this%cleanup()

#ifndef TOAST_NO_STOP
        call this%checkfailure()
#endif

    end subroutine finalize

    !> Check for failures and stops with an error code is so.
    subroutine checkfailure(this)
        class(TestObject), intent(in) :: this         !< Test suite type

        if(this%fcount > 0)then
            stop 1
        end if

    end subroutine checkfailure

    !> Cleanup
    subroutine cleanup(this)
        class(TestSuite), intent(inout) :: this
        integer(ki4) :: i

        if(this%arraysize > 0 .and. allocated(this%testcases))then

            do i = 1_ki4, this%arraysize
                deallocate(this%testcases(i)%raw)
            end do

            deallocate(this%testcases)
            this%arraysize = 0
        endif
    end subroutine cleanup

    !> Size
    integer(ki4) function length(this)
        class(TestSuite), intent(in) :: this

        length = this%arraysize

    end function length

    !> Iterate through test cases
    subroutine iterate(this, iterator_func)
        class(TestSuite), intent(inout) :: this
        interface
            subroutine iterator_func(test_case)
                import TestCase
                class(TestCase), intent(inout)  :: test_case
            end subroutine iterator_func
        end interface

        integer(ki4) :: i

        do i = 1_ki4, this%arraysize
            call iterator_func(this%testcases(i)%raw)
        end do

    end subroutine iterate

    !> Iterate through test cases
    subroutine iterate_const(this, iterator_func)
        class(TestSuite), intent(in) :: this
        interface
            subroutine iterator_func(test_case)
                import TestCase
                class(TestCase), intent(in)  :: test_case
            end subroutine iterator_func
        end interface

        integer(ki4) :: i

        do i = 1_ki4, this%arraysize
            call iterator_func(this%testcases(i)%raw)
        end do

    end subroutine iterate_const

    !> Run all the test cases
    subroutine runall(this)
        class(TestSuite), intent(inout) :: this

        call this%iterate(run)

        contains
            subroutine run(test_case)
                class(TestCase), intent(inout) :: test_case

                if(test_case%run())then
                    this%pcount = this%pcount + 1_ki4
                else
                    this%fcount = this%fcount + 1_ki4
                end if

            end subroutine run

    end subroutine runall

#define MACRO_TEST_TYPE TestObject
#include "definecounts.h"

    !> Append - a bit slow for large appends
    subroutine append(this, test)
        class(TestSuite), intent(inout) :: this
        class(TestCase), intent(in)     :: test

        type(TestCasePoly), dimension(:), allocatable :: tmp
        type(TestCasePoly) :: testpoly
        integer(ki4) :: prevcount
        integer(ki4) :: prevsize
        integer(ki4) :: newsize

        ! the actual number of items appended
        prevcount = this%arraysize

        ! the actual size of the container - not necessarily the same as the count
        prevsize = 0_ki4
        if(allocated(this%testcases))then
            prevsize = size(this%testcases)
        end if

        if(prevcount >= prevsize)then
            newsize = max(prevsize*this%scalefactor, prevsize+1_ki4)
            allocate(tmp(1_ki4:newsize))
            if(prevsize > 0_ki4)then
                tmp(1_ki4:prevsize) = this%testcases
            end if
            call move_alloc(tmp, this%testcases)
        end if

        this%arraysize = this%arraysize + 1_ki4
        ! Since we must wrap the test case in the polymorphic
        ! type TestCasePoly we need to allocate the raw here
        ! TestCasePoly finalize will cleanup for us
        allocate(testpoly%raw, source = test)
        this%testcases(this%arraysize) = testpoly

    end subroutine append

    !> constructor
    function constructor(name)
        character(*), intent(in), optional :: name

        type(TestSuite) :: constructor      !< test suite type to construct

        if(present(name))then
            call constructor%init(name)
        else
            call constructor%init()
        end if

    end function constructor

end module toast_test_suite_m
