MODULE mo_aero_volc
  USE mo_kind, ONLY: wp 
  IMPLICIT NONE
CONTAINS
  SUBROUTINE add_aop_volc ( &
            & kproma,                 kbdim,              klev,             &
            & krow,                   nb_lw,              nb_sw,            &
            & paer_tau_lw_vr,         paer_tau_sw_vr,     paer_piz_sw_vr,   &
            & paer_cg_sw_vr,          ppd_hl,             pp_fl,            &
            & tk_fl)
  ! !INPUT PARAMETERS
    INTEGER,INTENT(in)  :: kproma, &! actual block length
                           kbdim,  &! maximum block length
                           krow,   &! block index
                           klev,   &! number of vertical levels
                           nb_lw,  &! number of wave length bands (far IR)
                           nb_sw    ! number of wave length bands (solar)
    REAL(wp),INTENT(in) :: ppd_hl(kbdim,klev)  ,& ! layer pressure thickness 
                           pp_fl(kbdim,klev)   ,& ! pressure at "full levels"
                           tk_fl(kbdim,klev)      ! temperature at "full lev."
  ! !OUTPUT PARAMETERS
    REAL(wp),INTENT(inout),DIMENSION(kbdim,klev,nb_lw):: &
     paer_tau_lw_vr      !aerosol optical depth (far IR)
    REAL(wp),INTENT(inout),DIMENSION(kbdim,klev,nb_sw):: &
     paer_tau_sw_vr,   & !aerosol optical depth (solar), sum_i(tau_i)
     paer_piz_sw_vr,   & !weighted sum of single scattering albedos, 
                         !sum_i(tau_i*omega_i)
     paer_cg_sw_vr       !weighted sum of asymmetry factors, 
                         !sum_i(tau_i*omega_i*g_i)
     if(kproma < 1) print *, "Error calling add_aop_volc" 
  END SUBROUTINE add_aop_volc
END MODULE mo_aero_volc

MODULE mo_aero_volc_tab
  USE mo_kind, ONLY: wp 
  IMPLICIT NONE
CONTAINS
  SUBROUTINE add_aop_volc_ham( &
             & kproma           ,kbdim                 ,klev             ,&
             & krow             ,nb_lw                 ,nb_sw            ,&
             & paer_tau_lw_vr   ,paer_tau_sw_vr        ,paer_piz_sw_vr   ,&
             & paer_cg_sw_vr                                              )

  ! !INPUT PARAMETERS:
    INTEGER, INTENT(in)    :: kproma, kbdim, klev, krow
    INTEGER, INTENT(in)    :: nb_lw, nb_sw !< number of wavelengths for IR, VIS
  
  ! !OUTPUT PARAMETERS:
    REAL(wp),INTENT(inout),DIMENSION(kbdim,klev,nb_lw):: &
     paer_tau_lw_vr        !<aerosol optical depth (far IR)
    REAL(wp),INTENT(inout),DIMENSION(kbdim,klev,nb_sw):: &
     paer_tau_sw_vr,   &   !<aerosol optical depth (solar), sum_i(tau_i)
     paer_piz_sw_vr,   &   !<weighted sum of single scattering albedos, 
                           !<sum_i(tau_i*omega_i)
     paer_cg_sw_vr         !<weighted sum of asymmetry factors, 
                           !<sum_i(tau_i*omega_i*g_i)
     if(kproma < 1) print *, "Error calling add_aop_volc_ham" 
  END SUBROUTINE add_aop_volc_ham

  SUBROUTINE add_aop_volc_crow( &
             & kproma           ,kbdim                 ,klev             ,&
             & krow             ,nb_lw                 ,nb_sw            ,&
             & paer_tau_lw_vr   ,paer_tau_sw_vr        ,paer_piz_sw_vr   ,&
             & paer_cg_sw_vr                                              )

  ! !INPUT PARAMETERS:
    INTEGER, INTENT(in)    :: kproma, kbdim, klev, krow
    INTEGER, INTENT(in)    :: nb_lw, nb_sw !< number of wavelengths for IR, VIS
  
  ! !OUTPUT PARAMETERS:
    REAL(wp),INTENT(inout),DIMENSION(kbdim,klev,nb_lw):: &
     paer_tau_lw_vr        !<aerosol optical depth (far IR)
    REAL(wp),INTENT(inout),DIMENSION(kbdim,klev,nb_sw):: &
     paer_tau_sw_vr,   &   !<aerosol optical depth (solar), sum_i(tau_i)
     paer_piz_sw_vr,   &   !<weighted sum of single scattering albedos, 
                           !<sum_i(tau_i*omega_i)
     paer_cg_sw_vr         !<weighted sum of asymmetry factors, 
                           !<sum_i(tau_i*omega_i*g_i)
     if(kproma < 1) print *, "Error calling add_aop_volc_crow" 
  END SUBROUTINE add_aop_volc_crow

END MODULE mo_aero_volc_tab
