module exampletestcase_m
    use toast     !< testing library
    implicit none
    private

    type, extends(TestCase), public :: ExampleTestCase
    contains
        procedure :: test
    end type exampletestcase

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

    call test%run()
    call test%printsummary()

end program example
