program example
    use toast
    implicit none

    type(TestCase) :: test

    call test%reset()
    write(*, "(A)") "TestCase::reset - without init call once OK"

    call test%reset()
    call test%reset()
    write(*, "(A)") "TestCase::reset - without init call twice OK"

    call test%init()
    call test%reset()
    write(*, "(A)") "TestCase::reset - with init call once OK"

end program example
