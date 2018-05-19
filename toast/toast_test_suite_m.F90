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
        integer(ki4)                                   :: arraysize = 0_ki4
    contains
        procedure :: init                       !< Initialise
        procedure :: size                       !< Get the size
        procedure :: append                     !< Append a test case
        procedure :: iterate                    !< Iterate through test cases - intent(inout)
        procedure :: iterate_const              !< Iterate through test cases - intent(in)
        procedure :: runall                     !< Run all test cases
#include "declarecounts.h"
        procedure :: printsummary
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

        if(this%isinit .eqv. .false.) then
            allocate(this%testcases(0))
            this%isinit = .true.
        endif

    end subroutine init

    !> Finalize
    subroutine finalize(this)
        type(TestSuite), intent(inout) :: this

        call this%cleanup()

    end subroutine finalize

    !> Cleanup
    subroutine cleanup(this)
        class(TestSuite), intent(inout) :: this

        if(this%isinit .eqv. .true.) then
            deallocate(this%testcases)
            this%isinit = .false.
        endif

    end subroutine cleanup

    !> Size
    integer(ki4) function size(this)
        class(TestSuite), intent(in) :: this

        size = this%arraysize

    end function size

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

#define MACRO_TEST_TYPE TestSuite
#include "definecounts.h"
#undef MACRO_TEST_TYPE

    !> Pretty print summary
    subroutine printsummary(this)
        class(TestSuite), intent(in) :: this     !< Test case type

        integer(ki4) :: i, passerts, fasserts

        passerts = 0_ki4
        fasserts = 0_ki4

        do i = 1_ki4, this%arraysize
            passerts = passerts + this%testcases(i)%raw%passcount()
            fasserts = fasserts + this%testcases(i)%raw%failcount()
        end do

        !! print results
        write(*, "(A)") " TOAST RESULTS "
        write(*, "(A, I5.1, A, I5.1, A, I8.1, A, I8.1, A)") "[Passed test cases: ", &
              & this%pcount, " / ", this%pcount + this%fcount, "] (", passerts, &
              & " / ", passerts + fasserts, " asserts)"
        write(*, "(A, I5.1, A, I5.1, A, I8.1, A, I8.1, A)") "[Failed test cases: ", &
              & this%fcount, " / ", this%pcount + this%fcount, "] (", fasserts, &
              & " / ", passerts + fasserts, " asserts)"

    end subroutine printsummary

    !> Append - a bit slow for large appends
    subroutine append(this, test)
        class(TestSuite), intent(inout) :: this
        class(TestCase), intent(in)     :: test

        type(TestCasePoly), dimension(:), allocatable :: tmp
        type(TestCasePoly) :: testpoly
        integer(ki4) :: prevsize

        if(this%isinit .eqv. .true.) then
            prevsize = this%size()
            allocate(tmp(prevsize + 1_ki4))
            tmp(1_ki4:prevsize) = this%testcases
            call move_alloc(tmp, this%testcases)
            this%arraysize = this%arraysize + 1_ki4

            ! Since we must wrap the test case in the polymorphic
            ! type TestCasePoly we need to allocate the raw here
            ! TestCasePoly finalize will cleanup for us
            allocate(testpoly%raw, source = test)
            this%testcases(this%arraysize) = testpoly
        endif

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
