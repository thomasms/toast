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

!> Utility module for TOAST
module toast_util_m
    use fork_m
    implicit none
    private

    public :: getrate
    public :: areclose

    !> areclose overloads
    interface areclose
        module procedure areclose_kr4
        module procedure areclose_kr8
        module procedure areclose_kr16
    end interface

    !> Maximum number of messages
    integer(ki4), public, parameter :: MAX_STRING_LENGTH = 128_ki4

    !> define a string type
    type, public :: string_t
        character(MAX_STRING_LENGTH) :: raw
    end type string_t

contains

    pure function getrate(top, bottom) result(rate)
        integer(ki4), intent(in)    :: top
        integer(ki4), intent(in)    :: bottom
        real(kr4) :: rate

        rate = 0.0_kr4
        if(bottom /= 0_ki4) then
            rate = real(top, kr4)/real(bottom, kr4)
        end if

    end function getrate

#define MACRO_REAL_TYPE kr4
#include "areclosetemplate.h"
#undef MACRO_REAL_TYPE

#define MACRO_REAL_TYPE kr8
#include "areclosetemplate.h"
#undef MACRO_REAL_TYPE

#define MACRO_REAL_TYPE kr16
#include "areclosetemplate.h"
#undef MACRO_REAL_TYPE

end module toast_util_m
