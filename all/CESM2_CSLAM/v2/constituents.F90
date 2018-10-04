!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  


module constituents
! Metadata manager for the advected constituents.


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 
! Public interfaces

! Public data


integer, parameter, public :: pcnst  = 200      ! number of advected constituents (including water vapor)

! Namelist variables


! Constants for each tracer

!

                                                                                       ! (cldphysics, aerosol, gas )
!++bee - temporary... These names should be declared in the module that makes the addfld and outfld calls.
! Lists of tracer names and diagnostics

! Private data


                                         ! false => chemistry is responsible for making outfld
                                         !          calls for constituents
!==============================================================================================

!==============================================================================================


!=========================================================================================


!----------------------------------------------------------------------------------------------


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