!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  
! $Id$
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
! ESMF Base Module
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! module definition


!
!
!


      module ESMF_BaseMod
!BOP
! !MODULE: ESMF_BaseMod - Base class for all ESMF classes
! !DESCRIPTION:
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
! See the ESMF Developers Guide document for more details.
!------------------------------------------------------------------------------
! !USES:
          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
          USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

!
!
!
!

          IMPLICIT NONE 
! !PRIVATE TYPES:
!
          PRIVATE 
!------------------------------------------------------------------------------
!    Global integer parameters, used frequently

!

      integer, parameter :: ESMF_SUCCESS = 0, ESMF_FAILURE = -1

!------------------------------------------------------------------------------

!


!------------------------------------------------------------------------------

!


!------------------------------------------------------------------------------
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")


!


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


!------------------------------------------------------------------------------

!

!------------------------------------------------------------------------------
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.

!

      !! TODO: same comment as above.


!------------------------------------------------------------------------------

!


!------------------------------------------------------------------------------
!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h

!


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
! !DESCRIPTION:
!     The following routines apply to any type in the system.
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
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

!
!


!  Misc methods

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)

!  Overloaded = operator functions

!EOP
!------------------------------------------------------------------------------
! leave the following line as-is; it will insert the cvs ident string
! into the object file for tracking purposes.
!
!

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
! Base methods
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_BaseInit - initialize a Base object
! !INTERFACE:

!
!
!


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_SetName - set the name of this object
! !INTERFACE:

!


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_GetName - get the name of this object
! !INTERFACE:

!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeSet - set attribute on an ESMF type
! !INTERFACE:


!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGet - get attribute from an ESMF type
! !INTERFACE:


!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGetCount - get an ESMF object's number of attributes
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGetbyNumber - get an ESMF object's attribute by num ber
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
!IROUTINE:  ESMF_AttributeGetNameList - get an ESMF object's attribute name list
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeSetList - set an ESMF object's attributes
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGetList - get an ESMF object's attributes
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeSetObjectList - set an attribute on multiple ESMF objects
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeGetObjectList - get an attribute from multiple ESMF objects
! !INTERFACE:


!
!
!

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AttributeCopy - copy an attribute between two objects
! !INTERFACE:


!
!

!-------------------------------------------------------------------------
!BOP
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects
! !INTERFACE:


!

!

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object
! !INTERFACE:

!

!

!BOP
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object
! !INTERFACE:

!

!


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!IROUTINE:  ESMF_SetPointer - set an opaque value
! !INTERFACE:

!

!

!-------------------------------------------------------------------------
!BOP
!IROUTINE:  ESMF_SetNullPointer - set an opaque value
! !INTERFACE:

!

!

!-------------------------------------------------------------------------
!BOP
!  !IROUTINE:  ESMF_GetPointer - get an opaque value
! !INTERFACE:
!

!-------------------------------------------------------------------------
! misc print routines
!-------------------------------------------------------------------------
!BOP
!  !IROUTINE:  ESMF_StatusString - Return status as a string
! !INTERFACE:

!


!-------------------------------------------------------------------------
!BOP
!  !IROUTINE:  ESMF_DataTypeString - Return DataType as a string
! !INTERFACE:

!


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

!

      end module ESMF_BaseMod