## TOAST -Testing Or ASsertion Toolkit

A much needed library for Fortran.

Yes I know that pFUnit and FRUIT exist however:
- pFUnit maybe great but it is too complex and messy to use
- FRUIT is much simpler but a bit limited and not maintained

Hence the need for Toast!

### ToDo:
Still very early, API likely to change.
- Testing needs testing
- Setup CI
- Support for other types
- Support for derived data types
- Support for arrays
- Support for mpi
- Ensure threadsafety


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
