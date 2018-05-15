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
#include "json/includes.h"
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
        procedure :: runall                     !< Run all test cases
        procedure :: serialize => json_serialize !< Serialize to JSON
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

    !> Run all the test cases
    subroutine runall(this)
        class(TestSuite), intent(inout) :: this

        integer(ki4) :: i

        do i = 1, this%size()
            if(this%testcases(i)%raw%run())then
                this%pcount = this%pcount + 1_ki4
            else
                this%fcount = this%fcount + 1_ki4
            end if
        end do

    end subroutine runall

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

    !> Serialize dose rate data to JSON
    subroutine json_serialize(this, core, parent)
        class(TestSuite), intent(in)             :: this      !< Object instance
        type(json_core), intent(inout)           :: core      !< The JSON core object
        type(json_value), pointer, intent(inout) :: parent    !< The parent node - cannot be null

        type(json_value), pointer :: child
        integer(ki4) :: i

        call core%create_object(child, 'test_suite')
        call core%add(parent, child)

        call core%add(child, 'name', trim(this%name))
        call core%add(child, 'passcount', this%pcount)
        call core%add(child, 'failcount', this%fcount)

        do i = 1, this%size()
            call this%testcases(i)%raw%serialize(core, child)
        end do

    end subroutine json_serialize

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
