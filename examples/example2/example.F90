!> Define tests here
module exampletestcases
    use toast     !< testing library
    implicit none
    private

    !> We define the test case here
    type, extends(TestCase), public :: PassingTestCaseExample
    contains
        procedure :: test => PassingTestCaseExampleProc
    end type PassingTestCaseExample

!> Alternatively, we can define a test case using the include macro
!! Fortran PP doesn't support multiline definitions
#define MACRO_TESTCASE_NAME FailingTestCaseExample
#define MACRO_TESTCASE_NAME_PROC FailingTestCaseExampleProc
#include "definetestcase.h"

#define MACRO_TESTCASE_NAME PassAgainTestCaseExample
#define MACRO_TESTCASE_NAME_PROC PassAgainTestCaseExampleProc
#include "definetestcase.h"

#define MACRO_TESTCASE_NAME FailAgainTestCaseExample
#define MACRO_TESTCASE_NAME_PROC FailAgainTestCaseExampleProc
#include "definetestcase.h"

contains

    !> passing test example
    subroutine PassingTestCaseExampleProc(this)
        class(PassingTestCaseExample), intent(inout) :: this

        ! asserts
        call this%assertequal(127_ki1, 127_ki1, message = "127 should equal 127")
        call this%assertequal(32767_ki2, 32767_ki2, message = "32767 should equal 32767")
        call this%assertequal(3.0_kr8, 3.0_kr8, message = "3 should equal 3")
        call this%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4, &
                             & message = "3 should equal 3.1 with higher tolerance")
        call this%asserttrue(2_ki4*3_ki4 == 6_ki4, "2*3 == 6 should be true.")
        call this%assertfalse(.false., "False should be false.")

    end subroutine

    !> failing test example
    subroutine FailingTestCaseExampleProc(this)
        class(FailingTestCaseExample), intent(inout) :: this

        ! asserts
        call this%assertequal(127_ki1, 12_ki1, message = "127 should equal 127")
        call this%assertequal(32767_ki2, 327_ki2, message = "32767 should equal 32767")
        call this%assertequal(3.0_kr8, 3.4_kr8, message = "3 should equal 3")
        call this%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.09_kr4, &
                             & message = "3 should equal 3.1 with higher tolerance")
        call this%asserttrue(2_ki4*3_ki4 == 7_ki4, "2*3 == 6 should be true.")
        call this%assertfalse(.true., "False should be false.")
    end subroutine

    !> passing test example
    subroutine PassAgainTestCaseExampleProc(this)
        class(PassAgainTestCaseExample), intent(inout) :: this

        call this%asserttrue(.not. .false., "Not false should be true.")
        call this%asserttrue(.true., "True should be true.")
        ! integer array 1d asserts
        call this%assertequal([32767_ki2, 2_ki2], &
                            & [32767_ki2, 2_ki2], &
                            & message = "arrays should match")
    end subroutine

    !> failing test example
    subroutine FailAgainTestCaseExampleProc(this)
        class(FailAgainTestCaseExample), intent(inout) :: this

        call this%asserttrue(.false., "false cannot be true.")
        call this%assertequal([32767_ki2, 2_ki2, 34_ki2], &
                            & [32767_ki2, 2_ki2, 324_ki2], &
                            & message = "arrays should not match")
    end subroutine

end module exampletestcases

!> Main test program
program example
    use toast
    use exampletestcases
    implicit none

    type(TestSuite) :: suite
    suite = TestSuite(name="SuiteExample")

    ! add the test cases here
    call suite%append(PassingTestCaseExample(name="passing_case1"))
    call suite%append(FailingTestCaseExample(name="failing_case1"))
    call suite%append(PassAgainTestCaseExample(name="passing_case2"))
    call suite%append(FailAgainTestCaseExample(name="failing_case2"))
    call suite%append(PassingTestCaseExample(name="passing_case1_again"))

    ! Run them
    call suite%runall()

    ! print summary
    call printsummary(suite)

    ! write to JSON
    call jsonwritetofile(suite, "example2.json")

    ! check the result
    call suite%checkfailure()

end program example
