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
- [x] Support for comparing character arrays
- [ ] Support Complex assert
- [x] Tolerances (absolute + relative) for real
- [x] Setup CI
- [ ] Testing needs testing
- [ ] Support for derived data types
- [ ] Support for arrays
- [ ] Support for mpi
- [ ] Ensure threadsafety
- [x] JSON output
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

    call test%assertequal(3_ki4, 5_ki4)
    call test%assertequal(3_ki4, 3_ki4)

    call test%assertequal(3.0_kr8, 3.0_kr8)
    call test%assertequal(3.0_kr16, 3.0_kr16)

    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4)
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4)
    
    call printsummary(test)

end program example
```

This will output:
```bash
example1 - FAILURE
[Passed assertions:     16 /     19 ] ++++++++++++++++
[Failed assertions:      3 /     19 ] ---

 Failure @ 3 does not equal 5
 Failure @ arrays should not match
 Failure @ 3 should not equal 3.1 with low tolerance

example1 - FAILURE
[Passed assertions:      4 /      6 ] ++++
[Failed assertions:      2 /      6 ] --

 Failure @ False should not be true.
 Failure @ Assert strings are not equal

1
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

        call this%asserttrue(.not. .false., "Not false should be true.")

    end subroutine

    !> failing test example
    subroutine FailAgainTestCaseExampleProc(this)
        class(FailAgainTestCaseExample), intent(inout) :: this

        call this%asserttrue(.false., "false cannot be true.")

    end subroutine

end module exampletestcases

!> Main test program
program example
    use toast
    use exampletestcases
    implicit none

    type(TestSuite) :: suite
    suite = TestSuite(name="My test suite")

    ! add the tests here
    call suite%append(PassingTestCaseExample(name="passing_case1"))
    call suite%append(FailingTestCaseExample(name="failing_case1"))
    call suite%append(PassAgainTestCaseExample(name="passing_case2"))
    call suite%append(FailAgainTestCaseExample(name="failing_case2"))
    call suite%append(PassingTestCaseExample(name="passing_case1_again"))

    call suite%runall()
    
    call printsummary(suite)

end program example
```

This will output:
```bash
[2136][tom@Unknown:~/Dev/toast/buildifort/bin]$ ./toastexample2
                      -- test case results --
passing_case1 - SUCCESS
[Passed assertions:      6 /      6 ] ++++++
[Failed assertions:      0 /      6 ] 


failing_case1 - FAILURE
[Passed assertions:      0 /      6 ] 
[Failed assertions:      6 /      6 ] ------

 Failure @ 127 should equal 127
 Failure @ 32767 should equal 32767
 Failure @ 3 should equal 3
 Failure @ 3 should equal 3.1 with higher tolerance
 Failure @ 2*3 == 6 should be true.
 Failure @ False should be false.

passing_case2 - SUCCESS
[Passed assertions:      3 /      3 ] +++
[Failed assertions:      0 /      3 ] 


failing_case2 - FAILURE
[Passed assertions:      0 /      2 ] 
[Failed assertions:      2 /      2 ] --

 Failure @ false cannot be true.
 Failure @ arrays should not match

passing_case1_again - SUCCESS
[Passed assertions:      6 /      6 ] ++++++
[Failed assertions:      0 /      6 ] 


              -------------------------------------
                     -- TOAST test results --

                          -- FAILURE --

[Passed test cases:      3 /     5] (      15 /      23 asserts)
[Failed test cases:      2 /     5] (       8 /      23 asserts)
```


Due to generic type bound procedures on the TestCase type, the compiler catches errors when trying to compare different types, for example, the following will not compile:

```fortran
program example
    use toast     !< testing library
    implicit none
    
    type(TestCase) :: test

    !> Will not compile since kr16 and kr4 are different types
    call test%assertequal(3.0_kr16, 3.0_kr4)
    
    call printsummary(test)

end program example
```
