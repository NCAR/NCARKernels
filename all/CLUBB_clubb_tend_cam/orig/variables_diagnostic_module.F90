!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:50:01 
!KGEN version : 0.7.0 
  
!-------------------------------------------------------------------------
! $Id: variables_diagnostic_module.F90 7376 2014-11-09 02:55:23Z bmg2@uwm.edu $
!===============================================================================
module variables_diagnostic_module

! Description:
!   This module contains definitions of all diagnostic
!   arrays used in the single column model, as well as subroutines
!   to allocate, deallocate and initialize them.

!   Note that while these are all same dimension, there is a
!   thermodynamic and momentum grid and they have different levels
!-----------------------------------------------------------------------

    USE pdf_parameter_module, ONLY: pdf_parameter 

    USE clubb_precision, ONLY: core_rknd 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
    USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
    IMPLICIT NONE 

    PRIVATE 



  ! Diagnostic variables

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: &
    sigma_sqd_w_zt, & ! PDF width parameter interpolated to t-levs.  [-]
    Skw_zm,         & ! Skewness of w on momentum levels             [-]
    Skw_zt,         & ! Skewness of w on thermodynamic levels        [-]
    ug,             & ! u geostrophic wind                           [m/s]
    vg,             & ! v geostrophic wind                           [m/s]
    um_ref,         & ! Initial u wind; Michael Falk                 [m/s]
    vm_ref,         & ! Initial v wind; Michael Falk                 [m/s]
    thlm_ref,       & ! Initial liquid water potential temperature   [K]
    rtm_ref,        & ! Initial total water mixing ratio             [kg/kg]
    thvm              ! Virtual potential temperature                [K]

!!! Important Note !!!
! Do not indent the omp comments, they need to be in the first 4 columns
!!! End Important Note !!!
!$omp threadprivate(sigma_sqd_w_zt, Skw_zm, Skw_zt, ug, vg, &
!$omp   um_ref, vm_ref, thlm_ref, rtm_ref, thvm )

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    rsat ! Saturation mixing ratio  ! Brian

!$omp threadprivate(rsat)

  TYPE(pdf_parameter), allocatable, dimension(:), target, public :: pdf_params_zm 

!$omp threadprivate(pdf_params_zm, pdf_params_zm_frz)

  REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: frad 

!$omp threadprivate(Frad, radht, Frad_SW_up, Frad_SW_down, Frad_LW_up, Frad_LW_down)

! Second order moments
  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    thlprcp,  & ! thl'rc'              [K kg/kg]
    rtprcp,   & ! rt'rc'               [kg^2/kg^2]
    rcp2        ! rc'^2                [kg^2/kg^2]

!$omp threadprivate(thlprcp, rtprcp, rcp2)

! Third order moments
  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    wpthlp2,   & ! w'thl'^2    [m K^2/s]
    wp2thlp,   & ! w'^2 thl'   [m^2 K/s^2]
    wprtp2,    & ! w'rt'^2     [m kg^2/kg^2]
    wp2rtp,    & ! w'^2rt'     [m^2 kg/kg]
    wprtpthlp, & ! w'rt'thl'   [m kg K/kg s]
    wp2rcp,    & ! w'^2 rc'    [m^2 kg/kg s^2]
    wp3_zm       ! w'^3        [m^3/s^3]

!$omp threadprivate(wpthlp2, wp2thlp, wprtp2, wp2rtp, &
!$omp   wprtpthlp, wp2rcp, wp3_zm )

! Fourth order moments
  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    wp4 ! w'^4      [m^4/s^4]

!$omp threadprivate(wp4)

! Buoyancy related moments
  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    rtpthvp,  & ! rt'thv'     [K kg/kg]
    thlpthvp, & ! thl'thv'    [K^2]
    wpthvp,   & ! w'thv'      [K m/s]
    wp2thvp     ! w'^2thv'    [K m^2/s^2]

!$omp threadprivate(rtpthvp, thlpthvp, wpthvp, wp2thvp)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: &
    Kh_zt, & ! Eddy diffusivity coefficient on thermodynamic levels   [m^2/s]
    Kh_zm    ! Eddy diffusivity coefficient on momentum levels        [m^2/s]

!$omp threadprivate(Kh_zt, Kh_zm)


!$omp threadprivate(K_hm)

! Mixing lengths
  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    Lscale, Lscale_up, Lscale_down ! [m]

!$omp threadprivate(Lscale, Lscale_up, Lscale_down)

  real( kind = core_rknd ), target, allocatable, dimension(:), public :: & 
    em,     & ! Turbulent Kinetic Energy (TKE)                        [m^2/s^2]
    tau_zm, & ! Eddy dissipation time scale on momentum levels        [s]
    tau_zt    ! Eddy dissipation time scale on thermodynamic levels   [s]

!$omp threadprivate(em, tau_zm, tau_zt)

! hydrometeors variable arrays
!$omp threadprivate( hydromet, hydrometp2, wphydrometp )

! Cloud droplet concentration arrays
!$omp threadprivate(Ncm,wpNcp)

!$omp threadprivate(Nccnm)


! Surface data

!$omp threadprivate(ustar, soil_heat_flux)

! Passive scalar variables

  real( kind = core_rknd ), target, allocatable, dimension(:,:), public :: & 
    wpedsclrp   ! w'edsclr'
!$omp threadprivate(wpedsclrp)

  real( kind = core_rknd ), target, allocatable, dimension(:,:), public :: & 
    sclrpthvp,   & ! sclr'th_v'
    sclrprcp,    & ! sclr'rc'
    wp2sclrp,    & ! w'^2 sclr'
    wpsclrp2,    & ! w'sclr'^2
    wpsclrprtp,  & ! w'sclr'rt'
    wpsclrpthlp    ! w'sclr'thl'

!$omp threadprivate(sclrpthvp, sclrprcp, &
!$omp   wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp )

! Interpolated variables for tuning
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

!$omp threadprivate(wp2_zt, thlp2_zt, wpthlp_zt, wprtp_zt, &
!$omp   rtp2_zt, rtpthlp_zt, &
!$omp   up2_zt, vp2_zt, upwp_zt, vpwp_zt)


! Latin Hypercube arrays.  Vince Larson 22 May 2005

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

!$omp threadprivate(wp3_on_wp2, wp3_on_wp2_zt)

  PUBLIC kr_externs_in_variables_diagnostic_module 
  PUBLIC kr_externs_out_variables_diagnostic_module 
  PUBLIC kr_pdf_parameter_module_pdf_parameter 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_sigma_sqd_w_zt, kgenref_thvm 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_rsat 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_thlprcp, kgenref_rtprcp, kgenref_rcp2 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp3_zm 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp4 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_rtpthvp, kgenref_thlpthvp, kgenref_wpthvp, kgenref_wp2thvp 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_kh_zt, kgenref_kh_zm 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_lscale 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_em, kgenref_tau_zm, kgenref_tau_zt 
  REAL(KIND=core_rknd), allocatable, dimension(:,:) :: kgenref_wpedsclrp 
  REAL(KIND=core_rknd), allocatable, dimension(:,:) :: kgenref_sclrpthvp, kgenref_sclrprcp 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp2_zt, kgenref_thlp2_zt, kgenref_wpthlp_zt, kgenref_wprtp_zt, &
  &kgenref_rtp2_zt, kgenref_rtpthlp_zt, kgenref_up2_zt, kgenref_vp2_zt, kgenref_upwp_zt, kgenref_vpwp_zt 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_skw_velocity, kgenref_a3_coef, kgenref_a3_coef_zt 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp3_on_wp2, kgenref_wp3_on_wp2_zt 
  PUBLIC kv_externs_variables_diagnostic_module 
  PUBLIC kv_pdf_parameter_module_pdf_parameter 

!-----------------------------------------------------------------------
    
  CONTAINS 
    





!------------------------------------------------------------------------


  !read state subroutine for kr_externs_in_variables_diagnostic_module 
  SUBROUTINE kr_externs_in_variables_diagnostic_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(skw_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(sigma_sqd_w_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(vg, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtm_ref, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(thvm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(skw_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(ug, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(vm_ref, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(um_ref, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(thlm_ref, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rsat, kgen_unit) 
      CALL kr_kgen_subpname_0(pdf_params_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(frad, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(thlprcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rcp2, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtprcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wpthlp2, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp3_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2rcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wprtp2, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wprtpthlp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2rtp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2thlp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp4, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2thvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(thlpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kh_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kh_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(lscale_up, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(lscale, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(lscale_down, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(em, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(tau_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(tau_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpedsclrp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(sclrpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpsclrp2, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(sclrprcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpsclrprtp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpsclrpthlp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wp2sclrp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(up2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtpthlp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(vpwp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(upwp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wpthlp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(vp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wprtp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(thlp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(skw_velocity, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(a3_coef_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(a3_coef, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp3_on_wp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp3_on_wp2, kgen_unit) 
  END SUBROUTINE kr_externs_in_variables_diagnostic_module 
    
  !read state subroutine for kr_externs_out_variables_diagnostic_module 
  SUBROUTINE kr_externs_out_variables_diagnostic_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_sigma_sqd_w_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_thvm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_rsat, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_thlprcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_rcp2, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_rtprcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp3_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp4, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp2thvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_thlpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_rtpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_kh_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_kh_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_lscale, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_em, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_tau_zm, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_tau_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_wpedsclrp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_sclrpthvp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_sclrprcp, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_up2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_rtpthlp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_vpwp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_upwp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wpthlp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_rtp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_vp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wprtp_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_thlp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_skw_velocity, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_a3_coef_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_a3_coef, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp3_on_wp2_zt, kgen_unit) 
      CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp3_on_wp2, kgen_unit) 
  END SUBROUTINE kr_externs_out_variables_diagnostic_module 
    
  !read state subroutine for kr_variables_diagnostic_module_real__core_rknd_dim1 
  SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim1(var, kgen_unit, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
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
          CALL kgen_array_sumcheck("skw_zm", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
          IF (PRESENT( printvar )) THEN 
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
          END IF   
      END IF   
  END SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim1 
    
  !read state subroutine for kr_kgen_subpname_0 
  SUBROUTINE kr_kgen_subpname_0(var, kgen_unit, printvar) 
      TYPE(pdf_parameter), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              IF (PRESENT( printvar )) THEN 
                  CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printvar // "(idx1)") 
              ELSE 
                  CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit) 
              END IF   
          END DO   
      END IF   
  END SUBROUTINE kr_kgen_subpname_0 
    
  !read state subroutine for kr_variables_diagnostic_module_real__core_rknd_dim2 
  SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim2(var, kgen_unit, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
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
          CALL kgen_array_sumcheck("wpedsclrp", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
          IF (PRESENT( printvar )) THEN 
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
          END IF   
      END IF   
  END SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim2 
    
  !verify state subroutine for kv_externs_variables_diagnostic_module 
  SUBROUTINE kv_externs_variables_diagnostic_module(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("sigma_sqd_w_zt", check_status, sigma_sqd_w_zt, &
      &kgenref_sigma_sqd_w_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("thvm", check_status, thvm, kgenref_thvm) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("rsat", check_status, rsat, kgenref_rsat) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("thlprcp", check_status, thlprcp, kgenref_thlprcp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("rcp2", check_status, rcp2, kgenref_rcp2) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("rtprcp", check_status, rtprcp, kgenref_rtprcp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp3_zm", check_status, wp3_zm, kgenref_wp3_zm) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp4", check_status, wp4, kgenref_wp4) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp2thvp", check_status, wp2thvp, kgenref_wp2thvp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wpthvp", check_status, wpthvp, kgenref_wpthvp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("thlpthvp", check_status, thlpthvp, kgenref_thlpthvp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("rtpthvp", check_status, rtpthvp, kgenref_rtpthvp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("kh_zm", check_status, kh_zm, kgenref_kh_zm) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("kh_zt", check_status, kh_zt, kgenref_kh_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("lscale", check_status, lscale, kgenref_lscale) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("em", check_status, em, kgenref_em) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("tau_zt", check_status, tau_zt, kgenref_tau_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("tau_zm", check_status, tau_zm, kgenref_tau_zm) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim2("wpedsclrp", check_status, wpedsclrp, kgenref_wpedsclrp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim2("sclrpthvp", check_status, sclrpthvp, kgenref_sclrpthvp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim2("sclrprcp", check_status, sclrprcp, kgenref_sclrprcp) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp2_zt", check_status, wp2_zt, kgenref_wp2_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("up2_zt", check_status, up2_zt, kgenref_up2_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("rtpthlp_zt", check_status, rtpthlp_zt, kgenref_rtpthlp_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("vpwp_zt", check_status, vpwp_zt, kgenref_vpwp_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("upwp_zt", check_status, upwp_zt, kgenref_upwp_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wpthlp_zt", check_status, wpthlp_zt, kgenref_wpthlp_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("rtp2_zt", check_status, rtp2_zt, kgenref_rtp2_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("vp2_zt", check_status, vp2_zt, kgenref_vp2_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wprtp_zt", check_status, wprtp_zt, kgenref_wprtp_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("thlp2_zt", check_status, thlp2_zt, kgenref_thlp2_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("skw_velocity", check_status, skw_velocity, kgenref_skw_velocity) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("a3_coef_zt", check_status, a3_coef_zt, kgenref_a3_coef_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("a3_coef", check_status, a3_coef, kgenref_a3_coef) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp3_on_wp2_zt", check_status, wp3_on_wp2_zt, &
      &kgenref_wp3_on_wp2_zt) 
      CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp3_on_wp2", check_status, wp3_on_wp2, kgenref_wp3_on_wp2) 
  END SUBROUTINE kv_externs_variables_diagnostic_module 
    
  !verify state subroutine for kv_variables_diagnostic_module_real__core_rknd_dim1 
  RECURSIVE SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim1(varname, check_status, var, kgenref_var) 
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
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1(SIZE(var,dim=1))) 
              ALLOCATE (buf2(SIZE(var,dim=1))) 
              n = COUNT(var /= kgenref_var) 
              WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                  buf1 = ((var-kgenref_var)/kgenref_var)**2 
                  buf2 = (var-kgenref_var)**2 
              ELSEWHERE 
                  buf1 = (var-kgenref_var)**2 
                  buf2 = buf1 
              END WHERE   
              nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
              rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
              IF (nrmsdiff > check_status%tolerance) THEN 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  check_status%numInTol = check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                  WRITE (*, *) "RMS of difference is ", 0 
                  WRITE (*, *) "Normalized RMS of difference is ", 0 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 0) THEN 
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
  END SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim1 
    
  !verify state subroutine for kv_variables_diagnostic_module_real__core_rknd_dim2 
  RECURSIVE SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim2(varname, check_status, var, kgenref_var) 
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
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
              ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
              n = COUNT(var /= kgenref_var) 
              WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                  buf1 = ((var-kgenref_var)/kgenref_var)**2 
                  buf2 = (var-kgenref_var)**2 
              ELSEWHERE 
                  buf1 = (var-kgenref_var)**2 
                  buf2 = buf1 
              END WHERE   
              nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
              rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
              IF (nrmsdiff > check_status%tolerance) THEN 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  check_status%numInTol = check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                  WRITE (*, *) "RMS of difference is ", 0 
                  WRITE (*, *) "Normalized RMS of difference is ", 0 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 0) THEN 
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
  END SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim2 
    
end module variables_diagnostic_module