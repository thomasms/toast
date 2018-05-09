program example
    use toast
    implicit none

    type(TestCase) :: test

    write(*, "(A)") " *** TestCase::init ***"

    call test%init(name="example1")
    write(*, "(A)") "TestCase::init - call once OK"

    call test%init(name="example1")
    write(*, "(A)") "TestCase::init - call again OK"

    call test%init(name="example2")
    call test%init(name="example3")
    write(*, "(A)") "TestCase::init - call twice with different names OK"

    call test%init()
    call test%init(name="example3")
    call test%init()
    write(*, "(A)") "TestCase::init - call three times with optional names OK"

    write(*, "(A)") " ******************"

end program example
