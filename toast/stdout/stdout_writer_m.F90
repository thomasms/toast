!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                               !!
!!                    TOAST                      !!
!!                                               !!
!!      Copyright (c) 2018, Thomas Stainer       !!
!!                                               !!
!!            All rights reserved.               !!
!!    Licensed under the 3-clause BSD license.   !!
!!                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Test case module for handling std output information for TOAST
module stdout_writer_m
    use fork_m
    use stdout_ansi_colours_m
    use toast_test_case_m
    use toast_test_suite_m
    implicit none
    private

    public :: printsummary     !< Write to JSON file format

    interface printsummary
        module procedure printsummary_testcase
        module procedure printsummary_testsuite
    end interface

contains

    !> Pretty print summary for testcase
    subroutine printsummary_testcase(test)
        class(TestCase), intent(in) :: test     !< Test case type

        integer(ki4) :: i

        if(test%failcount() == 0)then
            write(*, "(A)") colourstring(trim(test%name)//" - SUCCESS", CHAR_COLOUR_GREEN, bold=.true.)
        else
            write(*, "(A)") colourstring(trim(test%name)//" - FAILURE", CHAR_COLOUR_RED, bold=.true.)
        end if

        write(*, "(A, I7.1, A, I6.1, A, A)") "[Passed assertions:", &
              & test%passcount(), " / ", test%totalcount(), " ] ", repeat("+", test%passcount())
        write(*, "(A, I7.1, A, I6.1, A, A)") "[Failed assertions:",  &
              & test%failcount(), " / ", test%totalcount(), " ] ", repeat("-", test%failcount())

        ! print failed messages
        write(*, "(A)") ""
        do i = 1_ki4, test%getnrofmessages()
            write(*, "(A)") colourstring(" Failure @ ", CHAR_COLOUR_BLACK, bold=.true.)//trim(test%getmessage(i))
        end do
        write(*, "(A)") ""

    end subroutine printsummary_testcase

    !> Pretty print summary for test suite
    subroutine printsummary_testsuite(test)
        class(TestSuite), intent(in) :: test     !< Test suite type

        integer(ki4) :: passerts, fasserts

        passerts = 0_ki4
        fasserts = 0_ki4

        call test%iterate_const(getcounts)

        !! print results
        write(*, "(A56)") colourstring("-- TOAST test results --", CHAR_COLOUR_BLACK, bold=.true.)
        write(*, "(A)") ""
        if(test%failcount() == 0)then
            write(*, "(A50)")colourstring("-- SUCCESS --", CHAR_COLOUR_GREEN, bold=.true.)
        else
            write(*, "(A50)")colourstring("-- FAILURE --", CHAR_COLOUR_RED, bold=.true.)
        end if

        write(*, "(A)") ""
        write(*, "(A, I6.1, A, I5.1, A, I8.1, A, I7.1, A)") "[Passed test cases: ", &
              & test%passcount(), " / ", test%totalcount(), "] (", passerts, &
              & " / ", passerts + fasserts, " asserts)"
        write(*, "(A, I6.1, A, I5.1, A, I8.1, A, I7.1, A)") "[Failed test cases: ", &
              & test%failcount(), " / ", test%totalcount(), "] (", fasserts, &
              & " / ", passerts + fasserts, " asserts)"

        contains
            subroutine getcounts(test_case)
                class(TestCase), intent(in) :: test_case

                passerts = passerts + test_case%passcount()
                fasserts = fasserts + test_case%failcount()

            end subroutine getcounts

    end subroutine printsummary_testsuite

end module stdout_writer_m
