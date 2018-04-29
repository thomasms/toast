## TOAST -Testing Or ASsertion Toolkit

[![Build Status](https://travis-ci.org/thomasms/toast.svg?branch=master)](https://travis-ci.org/thomasms/toast)

A much needed library for Fortran.

Yes I know that pFUnit and FRUIT exist however:
- pFUnit maybe great but it is too complex and messy to use
- FRUIT is much simpler but a bit limited and not maintained

Hence the need for Toast!

### Status:
Still very early, API likely to change.
- [x] Basic Test type
- [x] Support Int, Real and logical assert
- [x] Tolerances (absolute + relative) for real
- [x] Setup CI
- [ ] Testing needs testing
- [ ] Support for other types - character arrays need to be implemented
- [ ] Support for derived data types
- [ ] Support for arrays
- [ ] Support for mpi
- [ ] Ensure threadsafety
- [ ] JSON output
- [ ] File comparision
- [ ] Regression testing framework


Toast can be used in 2 main ways:
1. Using the base TestCase for assertions only and a summary. It's very simple to use but limited
2. Extend the base TestCase to implement the run subroutine and make use of TestSuites. This works similar to Python unit testing framework.

A simple example of type 1 usage is given in the example below:

```fortran
program example
    use toast     !< testing library
    implicit none

    type(TestCase) :: test

    ! Note the need for init here
    call test%init()

    call test%assertequal(3_ki4, 5_ki4)
    call test%assertequal(3_ki4, 3_ki4)

    call test%assertequal(3.0_kr8, 3.0_kr8)
    call test%assertequal(3.0_kr16, 3.0_kr16)

    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4)
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4)
    
    call test%printsummary()

end program example
```

A simple example of type 2 usage is given in the example below:

```fortran
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

        !> Name is optional but helps with output
        this%name = "Passing test case example"

        ! integer asserts
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

        !> Name is optional but helps with output
        this%name = "Failing test case example"

        ! integer asserts
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

        !> Name is optional but helps with output
        this%name = "Passing test case, another example"

        call this%asserttrue(.not. .false., "Not false should be true.")

    end subroutine

    !> failing test example
    subroutine FailAgainTestCaseExampleProc(this)
        class(FailAgainTestCaseExample), intent(inout) :: this

        !> Name is optional but helps with output
        this%name = "Failing test case, another example"

        call this%asserttrue(.false., "false cannot be true.")

    end subroutine

end module exampletestcases

!> Main test program
program example
    use toast
    use exampletestcases
    implicit none

    type(TestSuite) :: suite
    suite = TestSuite()

    ! add the tests here
    call suite%append(PassingTestCaseExample())
    call suite%append(FailingTestCaseExample())
    call suite%append(PassAgainTestCaseExample())
    call suite%append(FailAgainTestCaseExample())

    call suite%runall()

end program example
```

Due to generic type bound procedures on the TestCase type, the compiler catches errors when trying to compare different types, for example, the following will not compile:

```fortran
program example
    use toast     !< testing library
    implicit none
    
    type(TestCase) :: test

    ! Note the need for init here
    call test%init()

    !> Will not compile since kr16 and kr4 are different types
    call test%assertequal(3.0_kr16, 3.0_kr4)
    call test%printsummary()

end program example
```
