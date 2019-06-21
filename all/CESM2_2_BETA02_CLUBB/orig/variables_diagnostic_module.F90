!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:37 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------
! $Id$
!===============================================================================


module variables_diagnostic_module
! Description:
!   This module contains definitions of all diagnostic
!   arrays used in the single column model, as well as subroutines
!   to allocate, deallocate and initialize them.
!   Note that while these are all same dimension, there is a
!   thermodynamic and momentum grid and they have different levels
!-----------------------------------------------------------------------


    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PRIVATE 

  ! Diagnostic variables


    REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: sigma_sqd_w_zt, skw_zm, skw_zt, ug, vg, um_ref, vm_ref, &
    &thlm_ref, rtm_ref, thvm 
!!! Important Note !!!
! Do not indent the omp comments, they need to be in the first 4 columns
!!! End Important Note !!!

!$omp threadprivate(sigma_sqd_w_zt, Skw_zm, Skw_zt, Skthl_zm, Skthl_zt, Skrt_zm,  &
!$omp Skrt_zt, ug, vg, um_ref, vm_ref, thlm_ref, rtm_ref, thvm )

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    rsat ! Saturation mixing ratio  ! Brian

!$omp threadprivate(rsat)


!$omp threadprivate(pdf_params_zm, pdf_params_zm_frz)

  REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: frad 
! Second order moments

!$omp threadprivate(Frad, radht, Frad_SW_up, Frad_SW_down, Frad_LW_up, Frad_LW_down)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    thlprcp,  & ! thl'rc'              [K kg/kg]
    rtprcp,   & ! rt'rc'               [kg^2/kg^2]
    rcp2        ! rc'^2                [kg^2/kg^2]
! Third order moments

!$omp threadprivate(thlprcp, rtprcp, rcp2)

  REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: wpthlp2, wp2thlp, wprtp2, wp2rtp, wprtpthlp, wp2rcp, wp3_zm 
! Fourth order moments

!$omp threadprivate(wpthlp2, wp2thlp, wprtp2, wp2rtp, &
!$omp   wprtpthlp, wp2rcp, wp3_zm, thlp3, thlp3_zm, rtp3, rtp3_zm )

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    wp4 ! w'^4      [m^4/s^4]
! Buoyancy related moments

!$omp threadprivate(wp4)


!$omp threadprivate(rtpthvp, thlpthvp, wpthvp, wp2thvp)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: &
    Kh_zt, & ! Eddy diffusivity coefficient on thermodynamic levels   [m^2/s]
    Kh_zm    ! Eddy diffusivity coefficient on momentum levels        [m^2/s]

!$omp threadprivate(Kh_zt, Kh_zm)

! Mixing lengths

!$omp threadprivate(K_hm)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    Lscale, Lscale_up, Lscale_down ! [m]

!$omp threadprivate(Lscale, Lscale_up, Lscale_down)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    em,     & ! Turbulent Kinetic Energy (TKE)                        [m^2/s^2]
    tau_zm, & ! Eddy dissipation time scale on momentum levels        [s]
    tau_zt    ! Eddy dissipation time scale on thermodynamic levels   [s]
! hydrometeors variable arrays

!$omp threadprivate(em, tau_zm, tau_zt)

! Cloud droplet concentration arrays
!$omp threadprivate( hydromet, hydrometp2, wphydrometp )

!$omp threadprivate(Ncm,wpNcp)

! Surface data
!$omp threadprivate(Nccnm)


! Passive scalar variables
!$omp threadprivate(ustar, soil_heat_flux)


  real( kind = core_rknd ), target, allocatable, dimension(:,:), public :: & 
    wpedsclrp   ! w'edsclr'
!$omp threadprivate(wpedsclrp)

  REAL(KIND=core_rknd), target, allocatable, dimension(:,:), public :: sclrprcp, wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp 
! Interpolated variables for tuning

!$omp threadprivate(sclrpthvp, sclrprcp, &
!$omp   wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp )

!
  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    wp2_zt,     & ! w'^2 on thermo. grid     [m^2/s^2]
    thlp2_zt,   & ! thl'^2 on thermo. grid   [K^2]
    wpthlp_zt,  & ! w'thl' on thermo. grid   [m K/s]
    wprtp_zt,   & ! w'rt' on thermo. grid    [m kg/(kg s)]
    rtp2_zt,    & ! rt'^2 on therm. grid     [(kg/kg)^2]
    rtpthlp_zt, & ! rt'thl' on thermo. grid  [kg K/kg]
    up2_zt,     & ! u'^2 on thermo. grid     [m^2/s^2]
    vp2_zt,     & ! v'^2 on thermo. grid     [m^2/s^2]
    upwp_zt,    & ! u'w' on thermo. grid     [m^2/s^2]
    vpwp_zt       ! v'w' on thermo. grid     [m^2/s^2]
! Latin Hypercube arrays.  Vince Larson 22 May 2005

!$omp threadprivate(wp2_zt, thlp2_zt, wpthlp_zt, wprtp_zt, &
!$omp   rtp2_zt, rtpthlp_zt, &
!$omp   up2_zt, vp2_zt, upwp_zt, vpwp_zt)


!$omp threadprivate(lh_AKm, AKm, AKstd, AKstd_cld, lh_rcm_avg, AKm_rcm, &
!$omp   AKm_rcc)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    Skw_velocity, & ! Skewness velocity    [m/s]
    a3_coef,      & ! The a3 coefficient from CLUBB eqns                [-]
    a3_coef_zt      ! The a3 coefficient interpolated to the zt grid    [-]

!$omp threadprivate(Skw_velocity, a3_coef, a3_coef_zt)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    wp3_on_wp2,   &  ! w'^3 / w'^2 on the zm grid [m/s]
    wp3_on_wp2_zt    ! w'^3 / w'^2 on the zt grid [m/s]
  PUBLIC kr_externs_in_variables_diagnostic_module 
  PUBLIC kr_externs_out_variables_diagnostic_module 
  PUBLIC kv_externs_variables_diagnostic_module 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_sigma_sqd_w_zt, kgenref_thvm 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_rsat 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp3_zm 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_kh_zt, kgenref_kh_zm 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_lscale 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_em, kgenref_tau_zm, kgenref_tau_zt 
  REAL(KIND=core_rknd), allocatable, dimension(:,:) :: kgenref_wpedsclrp 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp2_zt, kgenref_thlp2_zt, kgenref_wpthlp_zt, kgenref_wprtp_zt, &
  &kgenref_rtp2_zt, kgenref_rtpthlp_zt, kgenref_up2_zt, kgenref_vp2_zt, kgenref_upwp_zt, kgenref_vpwp_zt 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_a3_coef, kgenref_a3_coef_zt 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp3_on_wp2, kgenref_wp3_on_wp2_zt 

!$omp threadprivate(wp3_on_wp2, wp3_on_wp2_zt)

!-----------------------------------------------------------------------
    
  CONTAINS 
    


!------------------------------------------------------------------------


  !read state subroutine for kr_externs_in_variables_diagnostic_module 
  SUBROUTINE kr_externs_in_variables_diagnostic_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_kgen_variables_diagnostic_module_subp3(sigma_sqd_w_zt, kgen_unit, "sigma_sqd_w_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(skw_zm, kgen_unit, "skw_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(vg, kgen_unit, "vg", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(rtm_ref, kgen_unit, "rtm_ref", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(thvm, kgen_unit, "thvm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(skw_zt, kgen_unit, "skw_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(ug, kgen_unit, "ug", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(vm_ref, kgen_unit, "vm_ref", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(um_ref, kgen_unit, "um_ref", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(thlm_ref, kgen_unit, "thlm_ref", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(rsat, kgen_unit, "rsat", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(frad, kgen_unit, "frad", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(thlprcp, kgen_unit, "thlprcp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(rcp2, kgen_unit, "rcp2", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(rtprcp, kgen_unit, "rtprcp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp2rcp, kgen_unit, "wp2rcp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp2thlp, kgen_unit, "wp2thlp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp3_zm, kgen_unit, "wp3_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wprtp2, kgen_unit, "wprtp2", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wprtpthlp, kgen_unit, "wprtpthlp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp2rtp, kgen_unit, "wp2rtp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wpthlp2, kgen_unit, "wpthlp2", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp4, kgen_unit, "wp4", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kh_zm, kgen_unit, "kh_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kh_zt, kgen_unit, "kh_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(lscale_up, kgen_unit, "lscale_up", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(lscale, kgen_unit, "lscale", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(lscale_down, kgen_unit, "lscale_down", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(em, kgen_unit, "em", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(tau_zt, kgen_unit, "tau_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(tau_zm, kgen_unit, "tau_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(wpedsclrp, kgen_unit, "wpedsclrp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(wpsclrpthlp, kgen_unit, "wpsclrpthlp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(wp2sclrp, kgen_unit, "wp2sclrp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(wpsclrp2, kgen_unit, "wpsclrp2", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(sclrprcp, kgen_unit, "sclrprcp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(wpsclrprtp, kgen_unit, "wpsclrprtp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp2_zt, kgen_unit, "wp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(up2_zt, kgen_unit, "up2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(rtpthlp_zt, kgen_unit, "rtpthlp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(vpwp_zt, kgen_unit, "vpwp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(upwp_zt, kgen_unit, "upwp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wpthlp_zt, kgen_unit, "wpthlp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(rtp2_zt, kgen_unit, "rtp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(vp2_zt, kgen_unit, "vp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wprtp_zt, kgen_unit, "wprtp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(thlp2_zt, kgen_unit, "thlp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(skw_velocity, kgen_unit, "skw_velocity", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(a3_coef_zt, kgen_unit, "a3_coef_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(a3_coef, kgen_unit, "a3_coef", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp3_on_wp2_zt, kgen_unit, "wp3_on_wp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(wp3_on_wp2, kgen_unit, "wp3_on_wp2", .FALSE.) 
  END SUBROUTINE kr_externs_in_variables_diagnostic_module 
    
  !read state subroutine for kr_externs_out_variables_diagnostic_module 
  SUBROUTINE kr_externs_out_variables_diagnostic_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_sigma_sqd_w_zt, kgen_unit, "kgenref_sigma_sqd_w_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_thvm, kgen_unit, "kgenref_thvm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_rsat, kgen_unit, "kgenref_rsat", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_wp3_zm, kgen_unit, "kgenref_wp3_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_kh_zm, kgen_unit, "kgenref_kh_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_kh_zt, kgen_unit, "kgenref_kh_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_lscale, kgen_unit, "kgenref_lscale", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_em, kgen_unit, "kgenref_em", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_tau_zt, kgen_unit, "kgenref_tau_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_tau_zm, kgen_unit, "kgenref_tau_zm", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp4(kgenref_wpedsclrp, kgen_unit, "kgenref_wpedsclrp", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_wp2_zt, kgen_unit, "kgenref_wp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_up2_zt, kgen_unit, "kgenref_up2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_rtpthlp_zt, kgen_unit, "kgenref_rtpthlp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_vpwp_zt, kgen_unit, "kgenref_vpwp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_upwp_zt, kgen_unit, "kgenref_upwp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_wpthlp_zt, kgen_unit, "kgenref_wpthlp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_rtp2_zt, kgen_unit, "kgenref_rtp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_vp2_zt, kgen_unit, "kgenref_vp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_wprtp_zt, kgen_unit, "kgenref_wprtp_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_thlp2_zt, kgen_unit, "kgenref_thlp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_a3_coef_zt, kgen_unit, "kgenref_a3_coef_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_a3_coef, kgen_unit, "kgenref_a3_coef", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_wp3_on_wp2_zt, kgen_unit, "kgenref_wp3_on_wp2_zt", .FALSE.) 
      CALL kr_kgen_variables_diagnostic_module_subp3(kgenref_wp3_on_wp2, kgen_unit, "kgenref_wp3_on_wp2", .FALSE.) 
  END SUBROUTINE kr_externs_out_variables_diagnostic_module 
    
  !verify state subroutine for kv_externs_variables_diagnostic_module 
  SUBROUTINE kv_externs_variables_diagnostic_module(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_kgen_variables_diagnostic_module_subp2("sigma_sqd_w_zt", check_status, sigma_sqd_w_zt, kgenref_sigma_sqd_w_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("thvm", check_status, thvm, kgenref_thvm) 
      CALL kv_kgen_variables_diagnostic_module_subp2("rsat", check_status, rsat, kgenref_rsat) 
      CALL kv_kgen_variables_diagnostic_module_subp2("wp3_zm", check_status, wp3_zm, kgenref_wp3_zm) 
      CALL kv_kgen_variables_diagnostic_module_subp2("kh_zm", check_status, kh_zm, kgenref_kh_zm) 
      CALL kv_kgen_variables_diagnostic_module_subp2("kh_zt", check_status, kh_zt, kgenref_kh_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("lscale", check_status, lscale, kgenref_lscale) 
      CALL kv_kgen_variables_diagnostic_module_subp2("em", check_status, em, kgenref_em) 
      CALL kv_kgen_variables_diagnostic_module_subp2("tau_zt", check_status, tau_zt, kgenref_tau_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("tau_zm", check_status, tau_zm, kgenref_tau_zm) 
      CALL kv_kgen_variables_diagnostic_module_subp3("wpedsclrp", check_status, wpedsclrp, kgenref_wpedsclrp) 
      CALL kv_kgen_variables_diagnostic_module_subp2("wp2_zt", check_status, wp2_zt, kgenref_wp2_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("up2_zt", check_status, up2_zt, kgenref_up2_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("rtpthlp_zt", check_status, rtpthlp_zt, kgenref_rtpthlp_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("vpwp_zt", check_status, vpwp_zt, kgenref_vpwp_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("upwp_zt", check_status, upwp_zt, kgenref_upwp_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("wpthlp_zt", check_status, wpthlp_zt, kgenref_wpthlp_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("rtp2_zt", check_status, rtp2_zt, kgenref_rtp2_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("vp2_zt", check_status, vp2_zt, kgenref_vp2_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("wprtp_zt", check_status, wprtp_zt, kgenref_wprtp_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("thlp2_zt", check_status, thlp2_zt, kgenref_thlp2_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("a3_coef_zt", check_status, a3_coef_zt, kgenref_a3_coef_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("a3_coef", check_status, a3_coef, kgenref_a3_coef) 
      CALL kv_kgen_variables_diagnostic_module_subp2("wp3_on_wp2_zt", check_status, wp3_on_wp2_zt, kgenref_wp3_on_wp2_zt) 
      CALL kv_kgen_variables_diagnostic_module_subp2("wp3_on_wp2", check_status, wp3_on_wp2, kgenref_wp3_on_wp2) 
  END SUBROUTINE kv_externs_variables_diagnostic_module 
    
  !read state subroutine for kr_kgen_variables_diagnostic_module_subp3 
  SUBROUTINE kr_kgen_variables_diagnostic_module_subp3(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
  END SUBROUTINE kr_kgen_variables_diagnostic_module_subp3 
    
  !read state subroutine for kr_kgen_variables_diagnostic_module_subp4 
  SUBROUTINE kr_kgen_variables_diagnostic_module_subp4(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
  END SUBROUTINE kr_kgen_variables_diagnostic_module_subp4 
    
  !verify state subroutine for kv_kgen_variables_diagnostic_module_subp2 
  RECURSIVE SUBROUTINE kv_kgen_variables_diagnostic_module_subp2(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      REAL(KIND=core_rknd), allocatable, INTENT(IN), DIMENSION(:) :: var, kgenref_var 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: idx1 
      INTEGER :: n 
      real(KIND=core_rknd) :: nrmsdiff, rmsdiff 
      real(KIND=core_rknd), ALLOCATABLE :: buf1(:), buf2(:) 
        
      IF (ALLOCATED(var)) THEN 
          check_status%numTotal = check_status%numTotal + 1 
            
          IF (ALL(var == kgenref_var)) THEN 
              check_status%numIdentical = check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1(SIZE(var,dim=1))) 
              ALLOCATE (buf2(SIZE(var,dim=1))) 
              n = SIZE(var) 
              WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                  buf1 = ((var-kgenref_var)/kgenref_var)**2 
                  buf2 = (var-kgenref_var)**2 
              ELSEWHERE 
                  buf1 = (var-kgenref_var)**2 
                  buf2 = buf1 
              END WHERE   
              nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
              rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
              IF (rmsdiff > kgen_tolerance) THEN 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 0) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  check_status%numInTol = check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 0) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
  END SUBROUTINE kv_kgen_variables_diagnostic_module_subp2 
    
  !verify state subroutine for kv_kgen_variables_diagnostic_module_subp3 
  RECURSIVE SUBROUTINE kv_kgen_variables_diagnostic_module_subp3(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      REAL(KIND=core_rknd), allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: idx1, idx2 
      INTEGER :: n 
      real(KIND=core_rknd) :: nrmsdiff, rmsdiff 
      real(KIND=core_rknd), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
        
      IF (ALLOCATED(var)) THEN 
          check_status%numTotal = check_status%numTotal + 1 
            
          IF (ALL(var == kgenref_var)) THEN 
              check_status%numIdentical = check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
              ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
              n = SIZE(var) 
              WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                  buf1 = ((var-kgenref_var)/kgenref_var)**2 
                  buf2 = (var-kgenref_var)**2 
              ELSEWHERE 
                  buf1 = (var-kgenref_var)**2 
                  buf2 = buf1 
              END WHERE   
              nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
              rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
              IF (rmsdiff > kgen_tolerance) THEN 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 0) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  check_status%numInTol = check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 0) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
  END SUBROUTINE kv_kgen_variables_diagnostic_module_subp3 
    
end module variables_diagnostic_module