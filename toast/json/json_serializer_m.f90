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

!> Module for writing JSON file
module json_serializer_m
    use fork_m
    use json_module
    use toast_test_case_m
    use toast_test_suite_m
    implicit none
    private

    !! We define the JSON serialization methods in a separate module
    !! since we don't want the TestCase and TestSuite to depend on JSON
    !! and it should be kept separate.
    !! That way if new serializers want to be added it is easier
    !! Remember: Open for extension, closed for modification!!

    public :: jsonwritetofile     !< Write to JSON file format
    public :: jsonserialize       !< Serialize data to JSON

    interface jsonwritetofile
        module procedure jsonwritetofile_testcase
        module procedure jsonwritetofile_testsuite
    end interface

    interface jsonserialize
        module procedure jsonserialize_testcase
        module procedure jsonserialize_testsuite
    end interface

contains

    !> Serialize dose rate data to JSON
    subroutine jsonserialize_testcase(test_case, core, parent)
        class(TestCase), intent(in)              :: test_case !< Object instance
        type(json_core), intent(inout)           :: core      !< The JSON core object
        type(json_value), pointer, intent(inout) :: parent    !< The parent node - cannot be null

        type(json_value), pointer :: child

        call core%create_object(child, 'test_case')
        call core%add(parent, child)

        call core%add(child, 'name', trim(test_case%name))
        call core%add(child, 'passcount', test_case%passcount())
        call core%add(child, 'failcount', test_case%failcount())

    end subroutine jsonserialize_testcase

    !> Serialize dose rate data to JSON
    subroutine jsonserialize_testsuite(test_suite, core, parent)
        class(TestSuite), intent(in)             :: test_suite  !< Object instance
        type(json_core), intent(inout)           :: core        !< The JSON core object
        type(json_value), pointer, intent(inout) :: parent      !< The parent node - cannot be null

        type(json_value), pointer :: child
        integer(ki4) :: i

        call core%create_object(child, 'test_suite')
        call core%add(parent, child)

        call core%add(child, 'name', trim(test_suite%name))
        call core%add(child, 'passcount', test_suite%passcount())
        call core%add(child, 'failcount', test_suite%failcount())

!        do i = 1, test_suite%size()
!            call test_suite%testcases(i)%raw%serialize(core, child)
!        end do

    end subroutine jsonserialize_testsuite

    !> Perform serialisation of test case to JSON format
    subroutine jsonwritetofile_testcase(output, filename)
        class(TestCase), intent(in)          :: output   !< Test case instance
        character(len=*), intent(in)         :: filename !< The name of the file to write to.
                                                         !! Doesn't include the extension and
                                                         !! file doesn't have to exist.
                                                         !! Existing files with same name are overwritten.

        type(json_core) :: core
        type(json_value), pointer :: base => null()

        ! initialise JSON core
        call jsonwritetofile_init(core, base)

        ! write the output
        call jsonserialize(output, core, base)

        ! finalise JSON writing
        call jsonwritetofile_final(core, base, filename)

    end subroutine jsonwritetofile_testcase

    !> Perform serialisation of test suite to JSON format
    subroutine jsonwritetofile_testsuite(output, filename)
        class(TestSuite), intent(in)         :: output   !< Test suite instance
        character(len=*), intent(in)         :: filename !< The name of the file to write to.
                                                         !! Doesn't include the extension and
                                                         !! file doesn't have to exist.
                                                         !! Existing files with same name are overwritten.

        type(json_core) :: core
        type(json_value), pointer :: base => null()

        ! initialise JSON core
        call jsonwritetofile_init(core, base)

        ! write the output
        call jsonserialize(output, core, base)

        ! finalise JSON writing
        call jsonwritetofile_final(core, base, filename)

    end subroutine jsonwritetofile_testsuite

    !> private JSON init subroutine for writing to file
    subroutine jsonwritetofile_init(core, base)
        type(json_core), intent(inout)           :: core
        type(json_value), pointer, intent(inout) :: base

        ! initialise JSON core
        call core%initialize()
        call core%create_object(base,'')

    end subroutine jsonwritetofile_init

    !> private JSON finalize subroutine for writing to file
    subroutine jsonwritetofile_final(core, base, filename)
        type(json_core), intent(inout)           :: core
        type(json_value), pointer, intent(inout) :: base
        character(len=*), intent(in)             :: filename

        logical :: status
        character(len=:), allocatable :: error_msg

        ! Write JSON to file
        call core%print(base, filename)

        ! Check for failure
        if (core%failed())then
            call core%check_for_errors(status, error_msg)
            stop 'JSON failed'
        endif

        ! Clean up
        if(associated(base))then
            call core%destroy(base)
            nullify(base)
        endif

    end subroutine jsonwritetofile_final

end module json_serializer_m
