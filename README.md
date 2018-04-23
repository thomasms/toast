# Toast
# Testing Or ASsertion Toolkit

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


Simple example is given in examples and below:

```fortran
program example
    use fork_m    !< type definitions
    use toast     !< testing library
    implicit none

    type(TestCase) :: test

    call test%assertequal(3_ki4, 5_ki4)
    call test%assertequal(3_ki4, 3_ki4)

    call test%assertequal(3.0_kr8, 3.0_kr8)
    call test%assertequal(3.0_kr16, 3.0_kr16)

    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4)
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4)
    
    call test%printsummary()

end program example
```
Due to generic type bound procedures on the TestCase type, the compiler catches errors when trying to compare different types, for example, the following will not compile:

```fortran
program example
    use fork_m    !< type definitions
    use toast     !< testing library
    implicit none
    
    !> Will not compile since kr16 and kr4 are different types
    call test%assertequal(3.0_kr16, 3.0_kr4)
    call test%printsummary()

end program example
```
