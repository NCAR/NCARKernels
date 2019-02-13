!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: types_mod.f90 12939 2018-11-27 15:34:34Z nancy@ucar.edu $
!> Should only have fortran "kinds".  the constants should
!> go into their own module, or this should be renamed 'constants'
!> or something more descriptive than 'types' (which overlaps
!> into our obs kinds and types).


!


module types_mod
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC i4, i8, r4, r8, digits12 
    PUBLIC pi, missing_r8 
! version controlled file description for error handling, do not edit

!----------------------------------------------------------------------------
! constants that need to be shared - really has nothing to do with types ...
!----------------------------------------------------------------------------


                                              ! in F90/95. increased to 63 in F2003
                                              ! use this for obs types/kinds
                                           
                                              ! based on WRF's maximum number of domains

!----------------------------------------------------------------------------
! Attributes for variable kinds -- declaring sizes explicitly means we know
! exactly what precision we are using and are not relying on compiler flags
! to set the defaults for real and int.
! from the MPI documentation, they say:
! for reals, p and r values result in:
!   if      (p > 33) or (r > 4931) then  external32 representation 
!                                        is undefined   
!   else if (p > 15) or (r >  307) then  external32_size = 16 
!   else if (p >  6) or (r >   37) then  external32_size =  8 
!   else                                 external32_size =  4 
! for integers, r results in:
!   if      (r > 38) then  external32 representation is undefined 
!   else if (r > 18) then  external32_size =  16  
!   else if (r >  9) then  external32_size =  8  
!   else if (r >  4) then  external32_size =  4 
!   else if (r >  2) then  external32_size =  2  
!   else                   external32_size =  1  
!----------------------------------------------------------------------------
! integer precision:

!
!
!
!
!

integer, parameter :: i4 = SELECTED_INT_KIND(8)
integer, parameter :: i8 = SELECTED_INT_KIND(13)
! real precision:
! TO RUN WITH REDUCED PRECISION REALS (and use correspondingly less memory)
! comment OUT the r8 definition below and use the second one:

integer, parameter :: r4 = SELECTED_REAL_KIND(6,30)
!integer, parameter :: r8 = SELECTED_REAL_KIND(12)   ! real r8
integer, parameter :: r8 = r4                      ! alias r8 to r4
! complex precision:

! 'digits12' is reserved for real variables that MUST retain 64 bits of
! precision. DO NOT CHANGE '12' to a smaller number. BAD BAD BAD things happen.
! This is a small subset of the variables. Changing this will ruin the ability
! to distinguish timesteps that are a few seconds apart, for instance.


integer, parameter :: digits12 = SELECTED_REAL_KIND(12)
!----------------------------------------------------------------------------
! Constants, some of which are model dependent and shouldn't be in this
! module.
!----------------------------------------------------------------------------


real(kind=r8), parameter :: PI = 3.14159265358979323846_r8

real(kind=r8),    parameter ::  MISSING_R8   = -888888.0_r8


end module types_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/types_mod.f90 $
! $Id: types_mod.f90 12939 2018-11-27 15:34:34Z nancy@ucar.edu $
! $Revision: 12939 $
! $Date: 2018-11-27 08:34:34 -0700 (Tue, 27 Nov 2018) $
