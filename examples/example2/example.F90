!> Define tests here
module exampletestcase_m
    use toast     !< testing library
    implicit none
    private

    type, extends(TestCase), public :: PassingTestCaseExample
    contains
        procedure :: test => ptest
    end type PassingTestCaseExample

    type, extends(TestCase), public :: FailingTestCaseExample
    contains
        procedure :: test => ftest
    end type FailingTestCaseExample

contains

    !passing test example
    subroutine ptest(this)
        class(PassingTestCaseExample), intent(inout) :: this

        this%name = "Passing test case example"

        ! integer asserts
        call this%assertequal(127_ki1, 127_ki1, message = "127 should equal 127")
        call this%assertequal(32767_ki2, 32767_ki2, message = "32767 should equal 32767")
        call this%assertequal(3.0_kr8, 3.0_kr8, message = "3 should equal 3")
        call this%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4, &
                             & message = "3 should equal 3.1 with higher tolerance")
        call this%asserttrue(2_ki4*3_ki4 == 6_ki4, "2*3 == 6 should be true.")
        call this%assertfalse(.false., "False should be false.")

    end subroutine ptest

    ! failing test example
    subroutine ftest(this)
        class(FailingTestCaseExample), intent(inout) :: this

        this%name = "Failing test case example"

        ! integer asserts
        call this%assertequal(127_ki1, 12_ki1, message = "127 should equal 127")
        call this%assertequal(32767_ki2, 327_ki2, message = "32767 should equal 32767")
        call this%assertequal(3.0_kr8, 3.4_kr8, message = "3 should equal 3")
        call this%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.09_kr4, &
                             & message = "3 should equal 3.1 with higher tolerance")
        call this%asserttrue(2_ki4*3_ki4 == 7_ki4, "2*3 == 6 should be true.")
        call this%assertfalse(.true., "False should be false.")

    end subroutine ftest
end module exampletestcase_m

!> Main test program
program example
    use toast
    use exampletestcase_m
    implicit none

    type(TestSuite) :: suite

    suite = TestSuite()

    call suite%append(PassingTestCaseExample())
    call suite%append(FailingTestCaseExample())
    call suite%append(FailingTestCaseExample())
    call suite%append(PassingTestCaseExample())
    call suite%append(PassingTestCaseExample())

    call suite%runall()

end program example
