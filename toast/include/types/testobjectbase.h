!> Base test object
type, abstract :: TestObject
private
    logical       :: isinit    = .false.
    integer(ki4)  :: pcount    = 0_ki4
    integer(ki4)  :: fcount    = 0_ki4
    integer(ki4)  :: arraysize = 0_ki4
    character(25), public :: name = "<TestObject>"
contains
    procedure :: checkfailure               !< Checks if any failures occurred and stops with error code
    procedure :: passcount                  !< Pass count
    procedure :: failcount                  !< Fail count
    procedure :: totalcount                 !< Total count
end type TestObject

