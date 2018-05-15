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

!> Module for declaring a data type that can be JSON serialized
module json_serializable_m
    use json_module
    implicit none
    private

    !> Abstract base class for json serializable objects
    type, abstract, public :: JsonSerializable
    contains
        procedure(serialize), deferred  :: serialize            !< Serialize to JSON
    end type JsonSerializable

    abstract interface

        !> Serialize to JSON (abstract method)
        subroutine serialize(this, core, parent)
            import JsonSerializable
            import json_core
            import json_value
            class(JsonSerializable), intent(in)         :: this         !< Object instance
            type(json_core), intent(inout)              :: core         !< The JSON core
            type(json_value), pointer, intent(inout)    :: parent       !< The parent node - cannot be null
        end subroutine serialize

    end interface

end module json_serializable_m
