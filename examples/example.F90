program example
    use fork_m    !< type definitions
    use toast     !< testing library
    implicit none

    type(TestCase) :: test

    ! integer asserts
    call test%assertequal(127_ki1, 127_ki1, message = "127 should equal 127")
    call test%assertequal(32767_ki2, 32767_ki2)
    call test%assertequal(3_ki4, 5_ki4)
    call test%assertequal(3_ki4, 3_ki4)

    ! real asserts
    call test%assertequal(3.0_kr8, 3.0_kr8)
    call test%assertequal(3.0_kr16, 3.0_kr16)
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4)
    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4)

    ! logical asserts
    call test%asserttrue(.true., message = "True should be true.")
    call test%asserttrue(.false., "False should not be true.")
    call test%asserttrue(2_ki4*3_ki4 == 6_ki4, "2*3 == 6 should be true.")
    call test%assertfalse(.false., "False should be false.")

    ! Print summary at the end
    call test%printsummary()
end program example
