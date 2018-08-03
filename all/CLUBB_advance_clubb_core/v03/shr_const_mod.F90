!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:46
!KGEN version : 0.6.1

!===============================================================================
! SVN $Id: shr_const_mod.F90 61510 2014-06-26 21:58:56Z tcraig $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/trunk_tags/share3_150116/shr/shr_const_mod.F90 $
!===============================================================================

MODULE shr_const_mod

    USE shr_kind_mod, only : rkind_comp

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck

   !----------------------------------------------------------------------------
   ! physical constants (all data public)
   !----------------------------------------------------------------------------
   PUBLIC

   real(rkind_comp),parameter :: SHR_CONST_G       = 9.80616_rkind_comp      ! acceleration of gravity ~ m/s^2

   real(rkind_comp),parameter :: SHR_CONST_BOLTZ   = 1.38065e-23_rkind_comp  ! Boltzmann's constant ~ J/K/molecule
   real(rkind_comp),parameter :: SHR_CONST_AVOGAD  = 6.02214e26_rkind_comp   ! Avogadro's number ~ molecules/kmole
   real(rkind_comp),parameter :: SHR_CONST_RGAS    = SHR_CONST_AVOGAD*SHR_CONST_BOLTZ       ! Universal gas constant ~ J/K/kmole
   real(rkind_comp),parameter :: SHR_CONST_MWDAIR  = 28.966_rkind_comp       ! molecular weight dry air ~ kg/kmole
   real(rkind_comp),parameter :: SHR_CONST_MWWV    = 18.016_rkind_comp       ! molecular weight water vapor
   real(rkind_comp),parameter :: SHR_CONST_RDAIR   = SHR_CONST_RGAS/SHR_CONST_MWDAIR        ! Dry air gas constant     ~ J/K/kg
 
   real(rkind_comp),parameter :: SHR_CONST_TKFRZ   = 273.15_rkind_comp       ! freezing T of fresh water          ~ K 

   real(rkind_comp),parameter :: SHR_CONST_CPDAIR  = 1.00464e3_rkind_comp    ! specific heat of dry air   ~ J/kg/K
   real(rkind_comp),parameter :: SHR_CONST_LATICE  = 3.337e5_rkind_comp      ! latent heat of fusion      ~ J/kg
   real(rkind_comp),parameter :: SHR_CONST_LATVAP  = 2.501e6_rkind_comp      ! latent heat of evaporation ~ J/kg
   real(rkind_comp),parameter :: SHR_CONST_LATSUB  = &               ! latent heat of sublimation ~ J/kg
                         SHR_CONST_LATICE + SHR_CONST_LATVAP



!-----------------------------------------------------------------------------






!-----------------------------------------------------------------------------

END MODULE shr_const_mod
