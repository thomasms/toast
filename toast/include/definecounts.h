
    !> Return the pass count
    integer(ki4) function passcount(this)
        class(MACRO_TEST_TYPE), intent(in) :: this     !< Test case type

        passcount = this%pcount
    end function passcount

    !> Return the fail count
    integer(ki4) function failcount(this)
        class(MACRO_TEST_TYPE), intent(in) :: this     !< Test case type

        failcount = this%fcount
    end function failcount

    !> Return the total count
    integer(ki4) function totalcount(this)
        class(MACRO_TEST_TYPE), intent(in) :: this     !< Test case type

        totalcount = this%fcount + this%pcount
    end function totalcount
