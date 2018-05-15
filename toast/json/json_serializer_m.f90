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

!> Module for writing JSON file
module json_serializer_m
    use json_serializable_m
    use json_module
    implicit none
    private

    public :: jsonwritetofile     !< Write to JSON file format

contains

    !> Perform serialisation of output to JSON format
    subroutine jsonwritetofile(output, filename)
        class(JsonSerializable), intent(in)  :: output   !< The json output object
        character(len=*), intent(in)         :: filename !< The name of the file to write to.
                                                         !! Doesn't include the extension and
                                                         !! file doesn't have to exist.
                                                         !! Existing files with same name are overwritten.

        type(json_core) :: core
        type(json_value), pointer :: base => null()
        logical :: status
        character(len=:), allocatable :: error_msg

        ! initialise JSON core
        call core%initialize()
        call core%create_object(base,'')

        ! write the output
        call output%serialize(core, base)

        ! Write JSON to file
        call core%print(base, filename)

        ! Check for failure
        if (core%failed())then
            call core%check_for_errors(status, error_msg)
            stop 'JSON failed.'
        endif

        ! Clean up
        if(associated(base))then
            call core%destroy(base)
            nullify(base)
        endif

    end subroutine jsonwritetofile

end module json_serializer_m
