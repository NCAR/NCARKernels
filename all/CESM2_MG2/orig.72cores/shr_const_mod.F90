!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-20 09:52:51 
!KGEN version : 0.7.3 
  
!===============================================================================
! SVN $Id: shr_const_mod.F90 61510 2014-06-26 21:58:56Z tcraig $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/trunk_tags/share3_150116/shr/shr_const_mod.F90 $
!===============================================================================


MODULE shr_const_mod

    USE shr_kind_mod 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

   integer(SHR_KIND_IN),parameter,private :: R8 = SHR_KIND_R8 ! rename for local readability only
   !----------------------------------------------------------------------------
   ! physical constants (all data public)
   !----------------------------------------------------------------------------

   PUBLIC 

   real(R8),parameter :: SHR_CONST_PI      = 3.14159265358979323846_R8  ! pi


   !Water Isotope Ratios in Vienna Standard Mean Ocean Water (VSMOW):
   
   ! For best numerics in CAM5

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


END MODULE shr_const_mod