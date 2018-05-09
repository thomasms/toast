program example
    use toast
    implicit none

    type(TestCase) :: test

    write(*, "(A)") " *** TestCase::reset ***"

    call test%reset()
    write(*, "(A)") "TestCase::reset - without init call once OK"

    call test%reset()
    call test%reset()
    write(*, "(A)") "TestCase::reset - without init call twice OK"

    call test%init()
    call test%reset()
    write(*, "(A)") "TestCase::reset - with init call once OK"

    write(*, "(A)") " ******************"

end program example
