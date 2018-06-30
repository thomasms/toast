program example
    use toast     !< testing library
    implicit none

    integer(ki4) :: i
    type(TestCase) :: test

    call test%init(name="example1")

    ! integer asserts
    do i=1, 10
        call test%assertequal(127_ki1, 127_ki1, message = "127 should equal 127")
    end do
    call test%assertequal(32767_ki2, 32767_ki2, message = "32767 should equal 32767")
    call test%assertequal(3_ki4, 5_ki4, message = "3 does not equal 5")
    call test%assertequal(3_ki4, 3_ki4, message = "3 should equal 3")

    ! integer array 1d asserts
    call test%assertequal([32767_ki2, 2_ki2], [32767_ki2, 2_ki2], message = "arrays should match")
    call test%assertequal([32767_ki2, 2_ki2, 1_ki2], [32767_ki2, 2_ki2, 2_ki2], message = "arrays should not match")

    ! real asserts
    call test%assertequal(3.0_kr8, 3.0_kr8, message = "3 should equal 3")
    call test%assertequal(3.0_kr16, 3.0_kr16, message = "3 should equal 3")
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4, &
                         & message = "3 should not equal 3.1 with low tolerance")
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4, &
                         & message = "3 should equal 3.1 with higher tolerance")

    ! Print summary and reset
    call test%printsummary()
    call test%reset()

    ! logical asserts
    call test%asserttrue(.true., message = "True should be true.")
    call test%asserttrue(.false., "False should not be true.")
    call test%asserttrue(2_ki4*3_ki4 == 6_ki4, "2*3 == 6 should be true.")
    call test%assertfalse(.false., "False should be false.")

    ! Print summary at the end
    call test%printsummary()
    call jsonwritetofile(test, "example1.json")

    call test%checkfailure()
end program example
