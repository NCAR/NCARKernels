!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:55
!KGEN version : 0.6.2

! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
! ESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module ESMF_BaseMod
 
!BOP
! !MODULE: ESMF_BaseMod - Base class for all ESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
!
! See the ESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          IMPLICIT NONE
!
! !PRIVATE TYPES:
          PRIVATE

!------------------------------------------------------------------------------
!
!    Global integer parameters, used frequently

      integer, parameter :: ESMF_SUCCESS = 0, ESMF_FAILURE = -1
     

!------------------------------------------------------------------------------
!


 
!------------------------------------------------------------------------------
!




!------------------------------------------------------------------------------
!
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")
      !!private


!------------------------------------------------------------------------------

      integer, parameter :: &
                   ESMF_KIND_I1 = selected_int_kind(2), &
                   ESMF_KIND_I2 = selected_int_kind(4), &
                   ESMF_KIND_I4 = selected_int_kind(9), &
                   ESMF_KIND_I8 = selected_int_kind(18), &
                   ESMF_KIND_R4 = selected_real_kind(3,25), &
                   ESMF_KIND_R8 = selected_real_kind(6,45), &
                   ESMF_KIND_C8 = selected_real_kind(3,25), &
                   ESMF_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------

          ! how do you do values of all types here ? TODO
          ! in C++ i'd do a union w/ overloaded access funcs
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=ESMF_MAXSTR) :: vc
          !character, pointer :: vcp

!------------------------------------------------------------------------------
!


!------------------------------------------------------------------------------
!
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.
!     !!private

      !! TODO: same comment as above.
!     !!private

!------------------------------------------------------------------------------
!



!------------------------------------------------------------------------------
!
!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h



!------------------------------------------------------------------------------
!


! !PUBLIC TYPES:

!      public ESMF_STATE_UNINIT, ESMF_STATE_READY, &
!             ESMF_STATE_UNALLOCATED, ESMF_STATE_ALLOCATED, &
!             ESMF_STATE_BUSY


      PUBLIC esmf_kind_i8



      PUBLIC esmf_success
     

!      public ESMF_MemIndex
!      public ESMF_BasePointer

!      public ESMF_AxisIndexInit
!      public ESMF_TF_TRUE, ESMF_TF_FALSE

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public ESMF_BaseInit
   
!      public ESMF_BaseGetConfig
!      public ESMF_BaseSetConfig

!      public ESMF_BaseGetInstCount

!      public ESMF_BaseSetID
!      public ESMF_BaseGetID

!      public ESMF_BaseSetRefCount
!      public ESMF_BaseGetRefCount

!      public ESMF_BaseSetStatus
!      public ESMF_BaseGetStatus

!   Virtual methods to be defined by derived classes
!      public ESMF_Read
!      public ESMF_Write
!      public ESMF_Validate
!      public ESMF_Print

!  Attribute methods
 
!  Misc methods

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)

!  Overloaded = operator functions
!
!
!EOP

!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 







!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! function to compare two ESMF_Status flags to see if they're the same or not





!------------------------------------------------------------------------------
! function to compare two ESMF_DataTypes to see if they're the same or not








!------------------------------------------------------------------------------
! function to compare two ESMF_Pointers to see if they're the same or not







!------------------------------------------------------------------------------
! function to compare two ESMF_Logicals to see if they're the same or not
! also need assignment to real f90 logical?





!------------------------------------------------------------------------------
! function to compare two ESMF_AxisIndex to see if they're the same or not







!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BaseInit - initialize a Base object
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP


!     !Initialize return code






!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_SetName - set the name of this object
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [anytype]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [[name]]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt ESMF_GetName} routine.
!     \item [[namespace]]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [[rc]]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
! 

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
                                        ! but not coordinated across procs

!     !Initialize return code


!     ! TODO: this code should generate a unique name if a name
!     !   is not given.  If a namespace is given, the name has to
!     !   be unique within that namespace.  Example namespaces could
!     !   be: Applications, Components, Fields/Bundles, Grids.
!      
!     ! Construct a default namespace if one is not given

!     ! Construct a default name if one is not given





!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GetName - get the name of this object
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3





!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeSet - set attribute on an ESMF type
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3



!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGet - get attribute from an ESMF type
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3



!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetCount - get an ESMF object's number of attributes
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Returns number of attributes present.

!
!EOP
! !REQUIREMENTS:  FLD1.7.5



!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetbyNumber - get an ESMF object's attribute by num ber
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS: 



!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_AttributeGetNameList - get an ESMF object's attribute name list
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3



!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeSetList - set an ESMF object's attributes 
!
! !INTERFACE:

!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)



!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeGetList - get an ESMF object's attributes
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4



!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeSetObjectList - set an attribute on multiple ESMF objects 
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)



!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  ESMF_AttributeGetObjectList - get an attribute from multiple ESMF objects 
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)



!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  ESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4



!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4


!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:





!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:










!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_SetPointer - set an opaque value

!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:



!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMF_SetNullPointer - set an opaque value

!
! !INTERFACE:
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:



!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  ESMF_GetPointer - get an opaque value 
!  
! !INTERFACE: 
!
! !RETURN VALUE:

! !ARGUMENTS:

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:



!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  ESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!   Return a status variable as a string.

!
!EOP
! !REQUIREMENTS:







 



!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  ESMF_DataTypeString - Return DataType as a string
!  
! !INTERFACE: 
!
! !ARGUMENTS:

!
! !DESCRIPTION:
!   Return a datatype variable as a string.

!
!EOP
! !REQUIREMENTS:





 



!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module ESMF_BaseMod