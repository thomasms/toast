!> Base test object
type, abstract :: TestObject
private
    logical       :: isinit    = .false.
    integer(ki4)  :: pcount    = 0_ki4
    integer(ki4)  :: fcount    = 0_ki4
    character(25), public :: name = "<TestObject>"
end type TestObject

