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
module exampletestcase_m
    use toast     !< testing library
    implicit none
    private

    type, extends(TestCase), public :: ExampleTestCase
    contains
        procedure :: test
    end type ExampleTestCase

contains

    subroutine test(this)
        class(ExampleTestCase), intent(inout) :: this

        ! integer asserts
        call this%assertequal(127_ki1, 127_ki1, message = "127 should equal 127")
        call this%assertequal(32767_ki2, 32767_ki2, message = "32767 should equal 32767")
        call this%assertequal(3_ki4, 5_ki4, message = "3 does not equal 5")
        call this%assertequal(3_ki4, 3_ki4, message = "3 should equal 3")

        ! real asserts
        call this%assertequal(3.0_kr8, 3.0_kr8, message = "3 should equal 3")
        call this%assertequal(3.0_kr16, 3.0_kr16, message = "3 should equal 3")
        call this%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4, &
                             & message = "3 should not equal 3.1 with low tolerance")
        call this%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4, &
                             & message = "3 should equal 3.1 with higher tolerance")

        ! logical asserts
        call this%asserttrue(.true., message = "True should be true.")
        call this%asserttrue(.false., "False should not be true.")
        call this%asserttrue(2_ki4*3_ki4 == 6_ki4, "2*3 == 6 should be true.")
        call this%assertfalse(.false., "False should be false.")

    end subroutine test

end module exampletestcase_m

program example
    use exampletestcase_m
    implicit none

    type(ExampleTestCase) :: test

    ! no need to init, just run the test
    call test%run()
    call test%printsummary()

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
