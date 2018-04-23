program example
    use fork_m    !< type definitions
    use toast     !< testing library
    implicit none

    type(TestCase) :: test

    call test%assertequal(127_ki1, 127_ki1)
    call test%printsummary()

    call test%assertequal(32767_ki2, 32767_ki2)
    call test%printsummary()

    call test%assertequal(3_ki4, 5_ki4)
    call test%printsummary()

    call test%assertequal(3_ki4, 3_ki4)
    call test%printsummary()

    call test%assertequal(3.0_kr8, 3.0_kr8)
    call test%printsummary()

    call test%assertequal(3.0_kr16, 3.0_kr16)
    call test%printsummary()

    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.099_kr4)
    call test%printsummary()

    call test%assertequal(3.0_kr4, 3.1_kr4, abs_tol=0.1_kr4)
    call test%printsummary()

end program example
