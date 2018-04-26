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

    !> Test suite - a collection of test cases
    type, public :: TestSuite
    private
        type(TestCase), dimension(:), allocatable :: testcases
        logical                                   :: isinit = .false.
        integer(ki4)                              :: arraysize = 0_ki4
    contains
        procedure :: init                       !< Initialise
        procedure :: size                       !< Get the size
        procedure :: append                     !< Append a test case
        !procedure :: runall                     !< Run all test cases
            final :: finalize
        procedure, private :: cleanup
    end type TestSuite
    
contains

    !> Initialise
    subroutine init(this)
        class(TestSuite), intent(inout) :: this

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

    !> Append - a bit slow for large appends
    !! ToDo: improve performance
    subroutine append(this, test)
        class(TestSuite), intent(inout) :: this
        type(TestCase), intent(in)      :: test

        type(TestCase), dimension(:), allocatable :: tmp
        integer(ki4) :: prevsize

        if(this%isinit .eqv. .true.) then
            prevsize = this%size()
            allocate(tmp(prevsize + 1_ki4))
            tmp(1_ki4:prevsize) = this%testcases
            !deallocate(this%testcases)
            call move_alloc(tmp, this%testcases)
            this%arraysize = this%arraysize + 1_ki4
            this%testcases(this%arraysize) = test
        endif

    end subroutine append

end module toast_test_suite_m
