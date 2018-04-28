#include "toastmacros.h"

type, extends(TestCase), public :: MACRO_TESTCASE_NAME
contains
    procedure :: test => MACRO_TESTCASE_NAME_PROC
end type MACRO_TESTCASE_NAME

#undef MACRO_TESTCASE_NAME
#undef MACRO_TESTCASE_NAME_PROC
