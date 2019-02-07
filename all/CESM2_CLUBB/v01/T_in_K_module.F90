!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:36 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------
! $Id: T_in_K_module.F90 6849 2014-04-22 21:52:30Z charlass@uwm.edu $ 
!===============================================================================


module T_in_K_module
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC thlm2t_in_k 

  contains
!-------------------------------------------------------------------------------

  elemental function thlm2T_in_K( thlm, exner, rcm )  & 
    result( T_in_K )
! Description:
!   Calculates absolute temperature from liquid water potential
!   temperature.  (Does not include ice.)
! References: 
!   Cotton and Anthes (1989), "Storm and Cloud Dynamics", Eqn. (2.51). 
!-------------------------------------------------------------------------------


      USE constants_clubb, ONLY: cp, lv 
      ! Variable(s) 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input 

    real( kind = core_rknd ), intent(in) :: & 
      thlm,   & ! Liquid potential temperature  [K]
      exner,  & ! Exner function                [-]
      rcm       ! Liquid water mixing ratio     [kg/kg]

    real( kind = core_rknd ) :: & 
      T_in_K ! Result temperature [K]
    ! ---- Begin Code ----


    T_in_K = thlm * exner + Lv * rcm / Cp

    return
  end function thlm2T_in_K
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

end module T_in_K_module