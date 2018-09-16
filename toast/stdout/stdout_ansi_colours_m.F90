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

!> Test case module ansi colours
module stdout_ansi_colours_m
    implicit none
    private

    !> special chars for colours
    character(len=1), public, parameter :: CHAR_ESC             = achar(27)
    character(len=2), public, parameter :: CHAR_START           = CHAR_ESC//'['
    character(len=1), public, parameter :: CHAR_END             = 'm'
    character(len=1), public, parameter :: CHAR_SEP             = ';'
    character(len=*), public, parameter :: CHAR_CLEAR           = CHAR_START//'0'//CHAR_END

    !> colours
    character(len=*), public, parameter :: CHAR_COLOUR_BLACK    = '30'
    character(len=*), public, parameter :: CHAR_COLOUR_RED      = '31'
    character(len=*), public, parameter :: CHAR_COLOUR_GREEN    = '32'
    character(len=*), public, parameter :: CHAR_COLOUR_YELLOW   = '33'
    character(len=*), public, parameter :: CHAR_COLOUR_BLUE     = '34'
    character(len=*), public, parameter :: CHAR_COLOUR_MAGENTA  = '35'
    character(len=*), public, parameter :: CHAR_COLOUR_CYAN     = '36'
    character(len=*), public, parameter :: CHAR_COLOUR_WHITE    = '37'

    public :: colourstring

contains

    function colourstring(str, colourcode, bold) result(colourstr)
        character(len=*), intent(in)  :: str
        character(len=*), intent(in)  :: colourcode
        logical, optional, intent(in) :: bold
        character(len=:), allocatable :: colourstr

        character(len=:), allocatable :: temp

        temp = ""
        if(present(bold))then
            if(bold)then
                temp = '1'//CHAR_SEP
            end if
        end if
        colourstr = CHAR_START//temp//colourcode//CHAR_END//str//CHAR_CLEAR

    end function colourstring

end module stdout_ansi_colours_m
