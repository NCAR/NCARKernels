!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  
!===============================================================================
! SVN $Id: shr_const_mod.F90 61510 2014-06-26 21:58:56Z tcraig $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/trunk_tags/share3_150116/shr/shr_const_mod.F90 $
!===============================================================================


MODULE shr_const_mod

    USE shr_kind_mod 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE shr_kind_mod, ONLY: rkind_comp

   !----------------------------------------------------------------------------
   ! physical constants (all data public)
   !----------------------------------------------------------------------------

   PUBLIC 

   real(rkind_comp),parameter :: SHR_CONST_PI      = 3.14159265358979323846_rkind_comp  ! pi


   !Water Isotope Ratios in Vienna Standard Mean Ocean Water (VSMOW):
   
   ! For best numerics in CAM5

!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


END MODULE shr_const_mod
