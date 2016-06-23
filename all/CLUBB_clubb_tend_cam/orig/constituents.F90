!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:49:59 
!KGEN version : 0.7.0 
  

module constituents

!----------------------------------------------------------------------------------------------
! 
! Purpose: Contains data and functions for manipulating advected and non-advected constituents.
!
! Revision history:
!             B.A. Boville    Original version
! June 2003   P. Rasch        Add wet/dry m.r. specifier
! 2004-08-28  B. Eaton        Add query function to allow turning off the default 1 output of
!                             constituents so that chemistry module can make the outfld calls.
!                             Allow cnst_get_ind to return without aborting when constituent not
!                             found.
! 2006-10-31  B. Eaton        Remove 'non-advected' constituent functionality.
!----------------------------------------------------------------------------------------------

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    IMPLICIT NONE 
    PRIVATE 
    SAVE 
!
! Public interfaces
!

! Public data

  integer, parameter, public :: pcnst  = 29      ! number of advected constituents (including water vapor)


! Namelist variables

!
! Constants for each tracer

!++bee - temporary... These names should be declared in the module that makes the addfld and outfld calls.
! Lists of tracer names and diagnostics

! Private data

                                           ! false => chemistry is responsible for making outfld
                                           !          calls for constituents

!==============================================================================================
!==============================================================================================














!----------------------------------------------------------------------------------------------




!==============================================================================



!==============================================================================





!==============================================================================================



!==============================================================================================





!==============================================================================


!==============================================================================




!==============================================================================



!==============================================================================

end module constituents