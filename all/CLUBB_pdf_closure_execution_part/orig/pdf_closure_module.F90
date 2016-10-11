!KGEN-generated Fortran source file

!Generated at : 2016-01-07 08:45:17
!KGEN version : 0.6.1

!---------------------------------------------------------------------------
! $Id: pdf_closure_module.F90 7309 2014-09-20 17:06:28Z betlej@uwm.edu $
!===============================================================================
module pdf_closure_module

    IMPLICIT NONE

    PUBLIC pdf_closure

    PRIVATE

  contains
!------------------------------------------------------------------------

  !#######################################################################
  !#######################################################################
  ! If you change the argument list of pdf_closure you also have to
  ! change the calls to this function in the host models 1, WRF, SAM
  ! and GFDL.
  !#######################################################################
  !#######################################################################
  SUBROUTINE pdf_closure(kgen_unit, kgen_total_time, hydromet_dim, wp2, wm, rtm, thlm, skw, &
sigma_sqd_w, thlp2, wpthlp, rtp2, wprtp, rtpthlp, exner, p_in_pa, thv_ds, wp3, &
sclrm, sclrp2, wpsclrp, sclrpthlp, sclrprtp, level, wp2hmp, wphydrometp, thlphmp, rtphmp, &
wp2thlp, wp2rtp, wp2rcp, wprcp, thlprcp, rtprcp, rcm, rcp2, wp4, wprtp2, wpthlp2, cloud_frac, &
wpthvp, wp2thvp, rtpthvp, thlpthvp, wprtpthlp, ice_supersat_frac, pdf_params, err_code, &
sclrprcp, sclrpthvp, wpsclrp2, wpsclrprtp, wpsclrpthlp, wp2sclrp, rc_coef)


    ! Description:
    ! Subroutine that computes pdf parameters analytically.
    !
    ! Based of the original formulation, but with some tweaks
    ! to remove some of the less realistic assumptions and
    ! improve transport terms.

    !   Corrected version that should remove inconsistency

    ! References:
    !   Eqn. 29, 30, 31, 32 & 33  on p. 3547 of
    !   ``A PDF-Based Model for Boundary Layer Clouds. Part I:
    !   Method and Model Description'' Golaz, et al. (2002)
    !   JAS, Vol. 59, pp. 3540--3551.
    !----------------------------------------------------------------------

      USE constants_clubb, ONLY: two, one, one_half, zero, cp, lv, rd, ep, ep1, ep2, w_tol_sqd, rt_tol, thl_tol, t_freeze_k, fstderr, zero_threshold, chi_tol

      USE parameters_model, ONLY: sclr_tol, sclr_dim, mixt_frac_max_mag

      USE parameters_tunable, ONLY: beta
    ! Plume widths for th_l and r_t [-]

      USE pdf_parameter_module, ONLY: pdf_parameter

      USE array_index, ONLY: l_mix_rat_hm

    ! The error function

      USE numerical_check, ONLY: pdf_closure_check

      USE saturation, ONLY: sat_mixrat_liq, sat_mixrat_ice

      USE error_code, ONLY: clubb_no_error

      USE error_code, ONLY: clubb_at_least_debug_level, fatal_error

      USE stats_variables, ONLY: iwp4, iwprtp2, iwprtpthlp, iwpthlp2

      USE clubb_precision, ONLY: core_rknd

      USE kgen_utils_mod, ONLY: kgen_dp
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter
      USE parameters_model, ONLY: kr_externs_in_parameters_model
      USE error_code, ONLY: kr_externs_in_error_code
      USE error_code, ONLY: kr_externs_out_error_code
      USE parameters_tunable, ONLY: kr_externs_in_parameters_tunable
      USE stats_variables, ONLY: kr_externs_in_stats_variables
      USE model_flags, ONLY: kr_externs_in_model_flags
      USE array_index, ONLY: kr_externs_in_array_index
      USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter
      USE kgen_utils_mod, ONLY: kgen_perturb_real
      IMPLICIT NONE


    ! Input Variables
      INTEGER, INTENT(INOUT) :: hydromet_dim

      REAL(KIND=core_rknd), INTENT(INOUT) :: p_in_pa, exner, thv_ds, wm, wp2, wp3, sigma_sqd_w, skw, rtm, rtp2, wprtp, thlm, thlp2, wpthlp, rtpthlp

      REAL(KIND=core_rknd), dimension(sclr_dim), INTENT(INOUT) :: sclrm, wpsclrp, sclrp2, sclrprtp, sclrpthlp


      INTEGER, INTENT(INOUT) :: level

      REAL(KIND=core_rknd), dimension(hydromet_dim), INTENT(INOUT) :: wphydrometp, wp2hmp, rtphmp, thlphmp

    ! Output Variables
      REAL(KIND=core_rknd), INTENT(INOUT) :: wp4, wprtp2, wp2rtp, wpthlp2, wp2thlp, cloud_frac, ice_supersat_frac, rcm, wpthvp, wp2thvp, rtpthvp, thlpthvp, wprcp, wp2rcp, rtprcp, thlprcp, rcp2, wprtpthlp

      TYPE(pdf_parameter), INTENT(INOUT) :: pdf_params

      INTEGER, INTENT(INOUT) :: err_code

    ! Output (passive scalar variables)

      REAL(KIND=core_rknd), dimension(sclr_dim), INTENT(INOUT) :: sclrpthvp, sclrprcp, wpsclrp2, wpsclrprtp, wpsclrpthlp, wp2sclrp

    ! Local Variables

      REAL(KIND=core_rknd) :: w_1_n, w_2_n
!     thl_1_n, thl_2_n,
!     rt_1_n, rt_2_n

    ! Variables that are stored in derived data type pdf_params.
      REAL(KIND=core_rknd) :: w_1, w_2, varnce_w_1, varnce_w_2, rt_1, rt_2, varnce_rt_1, varnce_rt_2, thl_1, thl_2, varnce_thl_1, varnce_thl_2, rrtthl, alpha_thl, alpha_rt, crt_1, crt_2, cthl_1, cthl_2

      REAL(KIND=core_rknd) :: chi_1, chi_2, stdev_chi_1, stdev_chi_2, stdev_eta_1, stdev_eta_2, covar_chi_eta_1, covar_chi_eta_2, corr_chi_eta_1, corr_chi_eta_2, rsatl_1, rsatl_2, rc_1, rc_2, cloud_frac_1, cloud_frac_2, mixt_frac

    ! Note:  alpha coefficients = 0.5 * ( 1 - correlations^2 ).
    !        These are used to calculate the scalar widths
    !        varnce_thl_1, varnce_thl_2, varnce_rt_1, and varnce_rt_2 as in Eq. (34)
    !        of Larson and Golaz (2005)

    ! Passive scalar local variables

      REAL(KIND=core_rknd), dimension(sclr_dim) :: sclr1, sclr2, varnce_sclr1, varnce_sclr2, alpha_sclr, rsclrthl, rsclrrt
!     sclr1_n, sclr2_n,

      LOGICAL :: l_scalar_calc, l_calc_ice_supersat_frac

    ! Quantities needed to predict higher order moments
      REAL(KIND=core_rknd) :: tl1, tl2, beta1, beta2

      REAL(KIND=core_rknd) :: sqrt_wp2

    ! Thermodynamic quantity

      REAL(KIND=core_rknd), INTENT(INOUT) :: rc_coef

      REAL(KIND=core_rknd) :: wp2rxp, wprxp, thlprxp, rtprxp

    ! variables for a generalization of Chris Golaz' closure
    ! varies width of plumes in theta_l, rt
      REAL(KIND=core_rknd) :: width_factor_1, width_factor_2
    
    ! variables for computing ice cloud fraction
      REAL(KIND=core_rknd) :: ice_supersat_frac1, ice_supersat_frac2, rt_at_ice_sat1, rt_at_ice_sat2, &
chi_at_ice_sat1, chi_at_ice_sat2, rc_1_ice, rc_2_ice
    
    real( kind = core_rknd ), parameter :: chi_at_liq_sat  = 0.0_core_rknd    ! Always zero

    logical, parameter :: l_liq_ice_loading_test = .false. ! Temp. flag liq./ice water loading test

    INTEGER :: i, hm_idx







!------------------------ Code Begins ----------------------------------

!$kgen begin_callsite pdf_closure_part3
    ! Check whether the passive scalars are present.

    INTEGER, INTENT(IN) :: kgen_unit
    REAL(KIND=kgen_dp), INTENT(INOUT) :: kgen_total_time
    LOGICAL :: kgen_istrue
    
    REAL(KIND=core_rknd) :: kgenref_wp4, kgenref_wprtp2, kgenref_wp2rtp, kgenref_wpthlp2, kgenref_wp2thlp, &
kgenref_cloud_frac, kgenref_ice_supersat_frac, kgenref_rcm, kgenref_wpthvp, kgenref_wp2thvp, &
kgenref_rtpthvp, kgenref_thlpthvp, kgenref_wprcp, kgenref_wp2rcp, kgenref_rtprcp, kgenref_thlprcp, &
kgenref_rcp2, kgenref_wprtpthlp
    TYPE(pdf_parameter) :: kgenref_pdf_params
    INTEGER :: kgenref_err_code
    REAL(KIND=core_rknd), dimension(sclr_dim) :: kgenref_sclrpthvp, kgenref_sclrprcp, kgenref_wpsclrp2, &
kgenref_wpsclrprtp, kgenref_wpsclrpthlp, kgenref_wp2sclrp
    REAL(KIND=core_rknd) :: kgenref_w_1_n, kgenref_w_2_n
    REAL(KIND=core_rknd) :: kgenref_w_1, kgenref_w_2, kgenref_varnce_w_1, kgenref_varnce_w_2, kgenref_rt_1, &
kgenref_rt_2, kgenref_varnce_rt_1, kgenref_varnce_rt_2, kgenref_thl_1, kgenref_thl_2, kgenref_varnce_thl_1, &
kgenref_varnce_thl_2, kgenref_rrtthl, kgenref_alpha_thl, kgenref_alpha_rt, kgenref_crt_1, kgenref_crt_2, &
kgenref_cthl_1, kgenref_cthl_2
    REAL(KIND=core_rknd) :: kgenref_chi_1, kgenref_chi_2, kgenref_stdev_chi_1, kgenref_stdev_chi_2, &
kgenref_stdev_eta_1, kgenref_stdev_eta_2, kgenref_covar_chi_eta_1, kgenref_covar_chi_eta_2, &
kgenref_corr_chi_eta_1, kgenref_corr_chi_eta_2, kgenref_rsatl_1, kgenref_rsatl_2, kgenref_rc_1, &
kgenref_rc_2, kgenref_cloud_frac_1, kgenref_cloud_frac_2, kgenref_mixt_frac
    REAL(KIND=core_rknd), dimension(sclr_dim) :: kgenref_sclr1, kgenref_sclr2, kgenref_varnce_sclr1, &
kgenref_varnce_sclr2, kgenref_alpha_sclr, kgenref_rsclrthl, kgenref_rsclrrt
    LOGICAL :: kgenref_l_scalar_calc, kgenref_l_calc_ice_supersat_frac
    REAL(KIND=core_rknd) :: kgenref_tl1, kgenref_tl2, kgenref_beta1, kgenref_beta2
    REAL(KIND=core_rknd) :: kgenref_sqrt_wp2
    REAL(KIND=core_rknd) :: kgenref_rc_coef
    REAL(KIND=core_rknd) :: kgenref_wp2rxp, kgenref_wprxp, kgenref_thlprxp, kgenref_rtprxp
    REAL(KIND=core_rknd) :: kgenref_width_factor_1, kgenref_width_factor_2
    REAL(KIND=core_rknd) :: kgenref_ice_supersat_frac1, kgenref_ice_supersat_frac2, kgenref_rt_at_ice_sat1, &
kgenref_rt_at_ice_sat2, kgenref_chi_at_ice_sat1, kgenref_chi_at_ice_sat2, kgenref_rc_1_ice, kgenref_rc_2_ice
    INTEGER :: kgenref_i, kgenref_hm_idx
    TYPE(check_t) :: check_status
    INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock
    INTEGER, PARAMETER :: kgen_maxiter = 1000
    REAL(KIND=kgen_dp) :: kgen_elapsed_time
    
    !extern input variables
    CALL kr_externs_in_parameters_model(kgen_unit)
    CALL kr_externs_in_error_code(kgen_unit)
    CALL kr_externs_in_parameters_tunable(kgen_unit)
    CALL kr_externs_in_stats_variables(kgen_unit)
    CALL kr_externs_in_model_flags(kgen_unit)
    CALL kr_externs_in_array_index(kgen_unit)
    
    !local input variables
    READ (UNIT = kgen_unit) w_1_n
    READ (UNIT = kgen_unit) w_2_n
    READ (UNIT = kgen_unit) alpha_thl
    READ (UNIT = kgen_unit) alpha_rt
    READ (UNIT = kgen_unit) varnce_rt_1
    READ (UNIT = kgen_unit) varnce_thl_1
    READ (UNIT = kgen_unit) varnce_rt_2
    READ (UNIT = kgen_unit) varnce_thl_2
    READ (UNIT = kgen_unit) rt_1
    READ (UNIT = kgen_unit) thl_1
    READ (UNIT = kgen_unit) rt_2
    READ (UNIT = kgen_unit) thl_2
    READ (UNIT = kgen_unit) rrtthl
    READ (UNIT = kgen_unit) w_1
    READ (UNIT = kgen_unit) varnce_w_1
    READ (UNIT = kgen_unit) w_2
    READ (UNIT = kgen_unit) varnce_w_2
    READ (UNIT = kgen_unit) crt_1
    READ (UNIT = kgen_unit) cthl_1
    READ (UNIT = kgen_unit) crt_2
    READ (UNIT = kgen_unit) cthl_2
    READ (UNIT = kgen_unit) mixt_frac
    READ (UNIT = kgen_unit) rsatl_1
    READ (UNIT = kgen_unit) rsatl_2
    READ (UNIT = kgen_unit) stdev_chi_1
    READ (UNIT = kgen_unit) stdev_chi_2
    READ (UNIT = kgen_unit) stdev_eta_1
    READ (UNIT = kgen_unit) covar_chi_eta_1
    READ (UNIT = kgen_unit) stdev_eta_2
    READ (UNIT = kgen_unit) covar_chi_eta_2
    READ (UNIT = kgen_unit) chi_1
    READ (UNIT = kgen_unit) cloud_frac_1
    READ (UNIT = kgen_unit) rc_1
    READ (UNIT = kgen_unit) chi_2
    READ (UNIT = kgen_unit) cloud_frac_2
    READ (UNIT = kgen_unit) rc_2
    READ (UNIT = kgen_unit) corr_chi_eta_1
    READ (UNIT = kgen_unit) corr_chi_eta_2
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) alpha_sclr
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) varnce_sclr1
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) varnce_sclr2
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) sclr1
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) sclr2
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) rsclrthl
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) rsclrrt
    END IF 
    READ (UNIT = kgen_unit) l_scalar_calc
    READ (UNIT = kgen_unit) l_calc_ice_supersat_frac
    READ (UNIT = kgen_unit) tl1
    READ (UNIT = kgen_unit) tl2
    READ (UNIT = kgen_unit) beta1
    READ (UNIT = kgen_unit) beta2
    READ (UNIT = kgen_unit) sqrt_wp2
    READ (UNIT = kgen_unit) wp2rxp
    READ (UNIT = kgen_unit) wprxp
    READ (UNIT = kgen_unit) thlprxp
    READ (UNIT = kgen_unit) rtprxp
    READ (UNIT = kgen_unit) width_factor_1
    READ (UNIT = kgen_unit) width_factor_2
    READ (UNIT = kgen_unit) rt_at_ice_sat1
    READ (UNIT = kgen_unit) rt_at_ice_sat2
    READ (UNIT = kgen_unit) chi_at_ice_sat1
    READ (UNIT = kgen_unit) ice_supersat_frac1
    READ (UNIT = kgen_unit) rc_1_ice
    READ (UNIT = kgen_unit) chi_at_ice_sat2
    READ (UNIT = kgen_unit) ice_supersat_frac2
    READ (UNIT = kgen_unit) rc_2_ice
    READ (UNIT = kgen_unit) i
    READ (UNIT = kgen_unit) hm_idx
    
    !extern output variables
    CALL kr_externs_out_error_code(kgen_unit)
    
    !local output variables
    READ (UNIT = kgen_unit) kgenref_wp2rtp
    READ (UNIT = kgen_unit) kgenref_wp2thlp
    READ (UNIT = kgen_unit) kgenref_wp4
    READ (UNIT = kgen_unit) kgenref_wprtp2
    READ (UNIT = kgen_unit) kgenref_wpthlp2
    READ (UNIT = kgen_unit) kgenref_wprtpthlp
    READ (UNIT = kgen_unit) kgenref_wp2rcp
    READ (UNIT = kgen_unit) kgenref_wp2thvp
    READ (UNIT = kgen_unit) kgenref_wprcp
    READ (UNIT = kgen_unit) kgenref_wpthvp
    READ (UNIT = kgen_unit) kgenref_thlprcp
    READ (UNIT = kgen_unit) kgenref_thlpthvp
    READ (UNIT = kgen_unit) kgenref_rtprcp
    READ (UNIT = kgen_unit) kgenref_rtpthvp
    READ (UNIT = kgen_unit) kgenref_cloud_frac
    READ (UNIT = kgen_unit) kgenref_rcm
    READ (UNIT = kgen_unit) kgenref_ice_supersat_frac
    READ (UNIT = kgen_unit) kgenref_rcp2
    CALL kr_pdf_parameter_module_pdf_parameter(kgenref_pdf_params, kgen_unit)
    READ (UNIT = kgen_unit) kgenref_err_code
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_wp2sclrp
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_wpsclrp2
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_wpsclrprtp
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_wpsclrpthlp
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_sclrprcp
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_sclrpthvp
    END IF 
    READ (UNIT = kgen_unit) kgenref_w_1_n
    READ (UNIT = kgen_unit) kgenref_w_2_n
    READ (UNIT = kgen_unit) kgenref_w_1
    READ (UNIT = kgen_unit) kgenref_w_2
    READ (UNIT = kgen_unit) kgenref_varnce_w_1
    READ (UNIT = kgen_unit) kgenref_varnce_w_2
    READ (UNIT = kgen_unit) kgenref_rt_1
    READ (UNIT = kgen_unit) kgenref_rt_2
    READ (UNIT = kgen_unit) kgenref_alpha_rt
    READ (UNIT = kgen_unit) kgenref_varnce_rt_1
    READ (UNIT = kgen_unit) kgenref_varnce_rt_2
    READ (UNIT = kgen_unit) kgenref_thl_1
    READ (UNIT = kgen_unit) kgenref_thl_2
    READ (UNIT = kgen_unit) kgenref_alpha_thl
    READ (UNIT = kgen_unit) kgenref_varnce_thl_1
    READ (UNIT = kgen_unit) kgenref_varnce_thl_2
    READ (UNIT = kgen_unit) kgenref_rrtthl
    READ (UNIT = kgen_unit) kgenref_crt_1
    READ (UNIT = kgen_unit) kgenref_crt_2
    READ (UNIT = kgen_unit) kgenref_cthl_1
    READ (UNIT = kgen_unit) kgenref_cthl_2
    READ (UNIT = kgen_unit) kgenref_mixt_frac
    READ (UNIT = kgen_unit) kgenref_rsatl_1
    READ (UNIT = kgen_unit) kgenref_rsatl_2
    READ (UNIT = kgen_unit) kgenref_chi_1
    READ (UNIT = kgen_unit) kgenref_chi_2
    READ (UNIT = kgen_unit) kgenref_stdev_chi_1
    READ (UNIT = kgen_unit) kgenref_stdev_chi_2
    READ (UNIT = kgen_unit) kgenref_stdev_eta_1
    READ (UNIT = kgen_unit) kgenref_stdev_eta_2
    READ (UNIT = kgen_unit) kgenref_covar_chi_eta_1
    READ (UNIT = kgen_unit) kgenref_covar_chi_eta_2
    READ (UNIT = kgen_unit) kgenref_corr_chi_eta_1
    READ (UNIT = kgen_unit) kgenref_corr_chi_eta_2
    READ (UNIT = kgen_unit) kgenref_cloud_frac_1
    READ (UNIT = kgen_unit) kgenref_rc_1
    READ (UNIT = kgen_unit) kgenref_cloud_frac_2
    READ (UNIT = kgen_unit) kgenref_rc_2
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_sclr1
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_sclr2
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_varnce_sclr1
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_varnce_sclr2
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_alpha_sclr
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_rsclrrt
    END IF 
    READ (UNIT = kgen_unit) kgen_istrue
    IF (kgen_istrue) THEN
        READ (UNIT = kgen_unit) kgenref_rsclrthl
    END IF 
    READ (UNIT = kgen_unit) kgenref_l_scalar_calc
    READ (UNIT = kgen_unit) kgenref_l_calc_ice_supersat_frac
    READ (UNIT = kgen_unit) kgenref_tl1
    READ (UNIT = kgen_unit) kgenref_tl2
    READ (UNIT = kgen_unit) kgenref_beta1
    READ (UNIT = kgen_unit) kgenref_beta2
    READ (UNIT = kgen_unit) kgenref_sqrt_wp2
    READ (UNIT = kgen_unit) kgenref_rc_coef
    READ (UNIT = kgen_unit) kgenref_wp2rxp
    READ (UNIT = kgen_unit) kgenref_wprxp
    READ (UNIT = kgen_unit) kgenref_thlprxp
    READ (UNIT = kgen_unit) kgenref_rtprxp
    READ (UNIT = kgen_unit) kgenref_width_factor_1
    READ (UNIT = kgen_unit) kgenref_width_factor_2
    READ (UNIT = kgen_unit) kgenref_rt_at_ice_sat1
    READ (UNIT = kgen_unit) kgenref_chi_at_ice_sat1
    READ (UNIT = kgen_unit) kgenref_rt_at_ice_sat2
    READ (UNIT = kgen_unit) kgenref_chi_at_ice_sat2
    READ (UNIT = kgen_unit) kgenref_ice_supersat_frac1
    READ (UNIT = kgen_unit) kgenref_rc_1_ice
    READ (UNIT = kgen_unit) kgenref_ice_supersat_frac2
    READ (UNIT = kgen_unit) kgenref_rc_2_ice
    READ (UNIT = kgen_unit) kgenref_i
    READ (UNIT = kgen_unit) kgenref_hm_idx
    
    !Uncomment following call statement to turn on perturbation experiment.
    !Adjust perturbation value and/or kind parameter if required.
    !CALL kgen_perturb_real( your_variable, 1.0E-15_8 )
    
    
    !call to kgen kernel
    CALL kgen_kernel



    ! If there is no variance in vertical velocity, then treat rt and theta-l as
    ! constant, as well.  Otherwise width parameters (e.g. varnce_w_1,
    ! varnce_w_2, etc.) are non-zero.





       ! The variable "mixt_frac" is the weight of the 1st PDF component.  The
       ! weight of the 2nd PDF component is "1-mixt_frac".  If there isn't any
       ! skewness of w (Sk_w = 0 because w'^3 = 0), mixt_frac = 0.5, and both
       ! PDF components are equally weighted.  If there is positive skewness of
       ! w (Sk_w > 0 because w'^3 > 0), 0 < mixt_frac < 0.5, and the 2nd PDF
       ! component has greater weight than does the 1st PDF component.  If there
       ! is negative skewness of w (Sk_w < 0 because w'^3 < 0),
       ! 0.5 < mixt_frac < 1, and the 1st PDF component has greater weight than
       ! does the 2nd PDF component.


      ! Determine sqrt( wp2 ) here to avoid re-computing it

      ! Clip mixt_frac, 1-mixt_frac, to avoid dividing by zero
      ! Formula for mixt_frac_max_mag =
      ! 1 - ( 1/2 * ( 1 - Skw_max/sqrt( 4*( 1 - sigma_sqd_w )^3 + Skw_max^2 ) ) )
      ! Where sigma_sqd_w is fixed at 0.4.

      ! The normalized mean of w for Gaussian "plume" 1 is w_1_n.  It's value
      ! will always be greater than 0.  As an example, a value of 1.0 would
      ! indicate that the actual mean of w for Gaussian "plume" 1 is found
      ! 1.0 standard deviation above the overall mean for w.
      ! The normalized mean of w for Gaussian "plume" 2 is w_2_n.  It's value
      ! will always be less than 0.  As an example, a value of -0.5 would
      ! indicate that the actual mean of w for Gaussian "plume" 2 is found
      ! 0.5 standard deviations below the overall mean for w.
      ! The mean of w for Gaussian "plume" 1 is w_1.
      ! The mean of w for Gaussian "plume" 2 is w_2.

      ! The variance of w for Gaussian "plume" 1 for varnce_w_1.
      ! The variance of w for Gaussian "plume" 2 for varnce_w_2.
      ! The variance in both Gaussian "plumes" is defined to be the same.


      ! The normalized variance for thl, rt, and sclr for "plume" 1 is:
      !
      ! { 1 - [1/(1-sigma_sqd_w)]*[ (w'x')^2 / (w'^2 * x'^2) ] / mixt_frac }
      ! * { (1/3)*beta + mixt_frac*( 1 - (2/3)*beta ) };
      !
      ! where "x" stands for thl, rt, or sclr; "mixt_frac" is the weight of Gaussian
      ! "plume" 1, and 0 <= beta <= 3.
      !
      ! The factor { (1/3)*beta + mixt_frac*( 1 - (2/3)*beta ) } does not depend on
      ! which varable "x" stands for.  The factor is multiplied by 2 and defined
      ! as width_factor_1.
      !
      ! The factor { 1 - [1/(1-sigma_sqd_w)]*[ (w'x')^2 / (w'^2 * x'^2) ] / mixt_frac }
      ! depends on which variable "x" stands for.  It is multiplied by one_half and
      ! defined as alpha_x, where "x" stands for thl, rt, or sclr.

      ! Vince Larson added a dimensionless factor so that the
      ! width of plumes in theta_l, rt can vary.
      ! beta is a constant defined in module parameters_tunable
      !   Set 0<beta<3.
      ! beta=1.5_core_rknd recovers Chris Golaz' simplified formula.
      ! 3 Nov 2003


!       thl_1_n = - (wpthlp/(sqrt( wp2 )*sqrt( thlp2 )))/w_2_n
!       thl_2_n = - (wpthlp/(sqrt( wp2 )*sqrt( thlp2 )))/w_1_n




        ! Vince Larson multiplied original expressions by width_factor_1,2
        !   to generalize scalar skewnesses.  05 Nov 03


!       rt_1_n = -( wprtp / ( sqrt( wp2 )*sqrt( rtp2 ) ) ) / w_2_n
!       rt_2_n = -( wprtp / ( sqrt( wp2 )*sqrt( rtp2 ) ) ) / w_1_n




        ! Vince Larson multiplied original expressions by width_factor_1,2
        !   to generalize scalar skewnesses.  05 Nov 03


      ! Compute pdf parameters for passive scalars
            ! Set plume sclr for plume 1,2 to the mean
            ! Set the variance to zero

!           sclr1_n(i) = - ( wpsclrp(i) / (sqrt( wp2 ) &
!                        * sqrt( sclrp2(i) )) )/w_2_n
!           sclr2_n(i) = - ( wpsclrp(i) / (sqrt( wp2 ) &
!                        * sqrt( sclrp2(i) )) )/w_1_n




            ! Vince Larson multiplied original expressions by width_factor_1,2
            !  to generalize scalar skewnesses.  05 Nov 03

      ! We include sub-plume correlation with coeff rrtthl.




      ! Sub-plume correlation, rsclrthl, of passive scalar and theta_l.



          ! Sub-plume correlation, rsclrrt, of passive scalar and total water.





    ! Compute higher order moments (these are interactive)


    ! Compute higher order moments (these are non-interactive diagnostics)









    ! Scalar Addition to higher order moments






    ! Compute higher order moments that include theta_v.

    ! First compute some preliminary quantities.
    ! "1" denotes first Gaussian; "2" denotes 2nd Gaussian
    ! liq water temp (Sommeria & Deardorff 1977 (SD), eqn. 3)




    ! SD's beta (eqn. 8)

    ! s from Lewellen and Yoh 1993 (LY) eqn. 1

    ! Coefficients for s'
    ! For each normal distribution in the sum of two normal distributions,
    ! s' = crt * rt'  +  cthl * thl';
    ! therefore, x's' = crt * x'rt'  +  cthl * x'thl'.
    ! Larson et al. May, 2001.



    ! Standard deviation of chi for each component.
    ! Include subplume correlation of qt, thl
    ! Because of round-off error,
    ! stdev_chi_1 (and probably stdev_chi_2) can become negative when rrtthl=1
    ! One could also write this as a squared term
    ! plus a postive correction; this might be a neater format


    ! We need to introduce a threshold value for the variance of chi
      ! Treat chi as a delta function in this component.

      ! Treat chi as a delta function in this component.

    ! Standard deviation of eta for each component.


    ! Covariance of chi and eta for each component.


    ! Correlation of chi and eta for each component.



    
    ! Determine whether to compute ice_supersat_frac. We do not compute
    ! ice_supersat_frac for GFDL (unless do_liquid_only_in_clubb is true),
    ! because liquid and ice are both fed into rtm, ruining the calculation.









    ! Calculate cloud_frac_1 and rc_1

    ! Calculate cloud_frac_2 and rc_2

      ! We must compute chi_at_ice_sat1 and chi_at_ice_sat2
        ! If the temperature is warmer than freezing (> 0C) then ice_supersat_frac
        ! is not defined, so we use chi_at_liq_sat

        ! If the temperature is warmer than freezing (> 0C) then ice_supersat_frac
        ! is not defined, so we use chi_at_liq_sat

      ! Calculate ice_supersat_frac1
      
      ! Calculate ice_supersat_frac2

    ! Compute moments that depend on theta_v
    !
    ! The moments that depend on th_v' are calculated based on an approximated
    ! and linearized form of the theta_v equation:
    !
    ! theta_v = theta_l + { (R_v/R_d) - 1 } * thv_ds * r_t
    !                   + [ {L_v/(C_p*exner)} - (R_v/R_d) * thv_ds ] * r_c;
    !
    ! and therefore:
    !
    ! th_v' = th_l' + { (R_v/R_d) - 1 } * thv_ds * r_t'
    !               + [ {L_v/(C_p*exner)} - (R_v/R_d) * thv_ds ] * r_c';
    !
    ! where thv_ds is used as a reference value to approximate theta_l.








    ! Account for subplume correlation in qt-thl

    ! Account for subplume correlation in qt-thl


    ! Account for subplume correlation of scalar, theta_v.
    ! See Eqs. A13, A8 from Larson et al. (2002) ``Small-scale...''
    !  where the ``scalar'' in this paper is w.


    ! Compute mean cloud fraction and cloud water
    
    
      ! Compute ice cloud fraction, ice_supersat_frac
      ! ice_supersat_frac will be garbage if computed as above

    ! Compute variance of liquid water mixing ratio.
    ! This is not needed for closure.  Statistical Analysis only.













    ! Save PDF parameters




      ! Error Reporting
      ! Joshua Fasching February 2008














!$kgen end_callsite
    
    !verify init
    CALL kgen_init_check(check_status, verboseLevel=1)
    
    !extern verify variables
    
    !local verify variables
    CALL kv_pdf_closure_real__core_rknd("cloud_frac", check_status, cloud_frac, kgenref_cloud_frac)
    CALL kv_pdf_closure_real__core_rknd("wp2rcp", check_status, wp2rcp, kgenref_wp2rcp)
    CALL kv_pdf_closure_real__core_rknd("rcp2", check_status, rcp2, kgenref_rcp2)
    CALL kv_pdf_closure_real__core_rknd("thlprcp", check_status, thlprcp, kgenref_thlprcp)
    CALL kv_pdf_closure_real__core_rknd("thlpthvp", check_status, thlpthvp, kgenref_thlpthvp)
    CALL kv_pdf_closure_real__core_rknd("rcm", check_status, rcm, kgenref_rcm)
    CALL kv_pdf_closure_real__core_rknd("wp4", check_status, wp4, kgenref_wp4)
    CALL kv_pdf_closure_real__core_rknd("wp2thlp", check_status, wp2thlp, kgenref_wp2thlp)
    CALL kv_pdf_closure_real__core_rknd("wprtp2", check_status, wprtp2, kgenref_wprtp2)
    CALL kv_pdf_closure_real__core_rknd("wprtpthlp", check_status, wprtpthlp, kgenref_wprtpthlp)
    CALL kv_pdf_closure_real__core_rknd("wpthvp", check_status, wpthvp, kgenref_wpthvp)
    CALL kv_pdf_closure_real__core_rknd("rtpthvp", check_status, rtpthvp, kgenref_rtpthvp)
    CALL kv_pdf_closure_real__core_rknd("wp2rtp", check_status, wp2rtp, kgenref_wp2rtp)
    CALL kv_pdf_closure_real__core_rknd("wp2thvp", check_status, wp2thvp, kgenref_wp2thvp)
    CALL kv_pdf_closure_real__core_rknd("wprcp", check_status, wprcp, kgenref_wprcp)
    CALL kv_pdf_closure_real__core_rknd("rtprcp", check_status, rtprcp, kgenref_rtprcp)
    CALL kv_pdf_closure_real__core_rknd("wpthlp2", check_status, wpthlp2, kgenref_wpthlp2)
    CALL kv_pdf_closure_real__core_rknd("ice_supersat_frac", check_status, ice_supersat_frac, kgenref_ice_supersat_frac)
    CALL kv_pdf_parameter_module_pdf_parameter("pdf_params", check_status, pdf_params, kgenref_pdf_params)
    CALL kv_pdf_closure_integer__("err_code", check_status, err_code, kgenref_err_code)
    CALL kv_pdf_closure_real__core_rknd_dim1("wp2sclrp", check_status, wp2sclrp, kgenref_wp2sclrp)
    CALL kv_pdf_closure_real__core_rknd_dim1("wpsclrp2", check_status, wpsclrp2, kgenref_wpsclrp2)
    CALL kv_pdf_closure_real__core_rknd_dim1("wpsclrprtp", check_status, wpsclrprtp, kgenref_wpsclrprtp)
    CALL kv_pdf_closure_real__core_rknd_dim1("wpsclrpthlp", check_status, wpsclrpthlp, kgenref_wpsclrpthlp)
    CALL kv_pdf_closure_real__core_rknd_dim1("sclrpthvp", check_status, sclrpthvp, kgenref_sclrpthvp)
    CALL kv_pdf_closure_real__core_rknd_dim1("sclrprcp", check_status, sclrprcp, kgenref_sclrprcp)
    CALL kv_pdf_closure_real__core_rknd("w_2_n", check_status, w_2_n, kgenref_w_2_n)
    CALL kv_pdf_closure_real__core_rknd("w_1_n", check_status, w_1_n, kgenref_w_1_n)
    CALL kv_pdf_closure_real__core_rknd("alpha_thl", check_status, alpha_thl, kgenref_alpha_thl)
    CALL kv_pdf_closure_real__core_rknd("crt_1", check_status, crt_1, kgenref_crt_1)
    CALL kv_pdf_closure_real__core_rknd("alpha_rt", check_status, alpha_rt, kgenref_alpha_rt)
    CALL kv_pdf_closure_real__core_rknd("crt_2", check_status, crt_2, kgenref_crt_2)
    CALL kv_pdf_closure_real__core_rknd("w_1", check_status, w_1, kgenref_w_1)
    CALL kv_pdf_closure_real__core_rknd("varnce_thl_1", check_status, varnce_thl_1, kgenref_varnce_thl_1)
    CALL kv_pdf_closure_real__core_rknd("varnce_rt_2", check_status, varnce_rt_2, kgenref_varnce_rt_2)
    CALL kv_pdf_closure_real__core_rknd("varnce_rt_1", check_status, varnce_rt_1, kgenref_varnce_rt_1)
    CALL kv_pdf_closure_real__core_rknd("cthl_2", check_status, cthl_2, kgenref_cthl_2)
    CALL kv_pdf_closure_real__core_rknd("cthl_1", check_status, cthl_1, kgenref_cthl_1)
    CALL kv_pdf_closure_real__core_rknd("rrtthl", check_status, rrtthl, kgenref_rrtthl)
    CALL kv_pdf_closure_real__core_rknd("varnce_thl_2", check_status, varnce_thl_2, kgenref_varnce_thl_2)
    CALL kv_pdf_closure_real__core_rknd("thl_1", check_status, thl_1, kgenref_thl_1)
    CALL kv_pdf_closure_real__core_rknd("thl_2", check_status, thl_2, kgenref_thl_2)
    CALL kv_pdf_closure_real__core_rknd("w_2", check_status, w_2, kgenref_w_2)
    CALL kv_pdf_closure_real__core_rknd("varnce_w_2", check_status, varnce_w_2, kgenref_varnce_w_2)
    CALL kv_pdf_closure_real__core_rknd("rt_2", check_status, rt_2, kgenref_rt_2)
    CALL kv_pdf_closure_real__core_rknd("rt_1", check_status, rt_1, kgenref_rt_1)
    CALL kv_pdf_closure_real__core_rknd("varnce_w_1", check_status, varnce_w_1, kgenref_varnce_w_1)
    CALL kv_pdf_closure_real__core_rknd("stdev_eta_1", check_status, stdev_eta_1, kgenref_stdev_eta_1)
    CALL kv_pdf_closure_real__core_rknd("rsatl_1", check_status, rsatl_1, kgenref_rsatl_1)
    CALL kv_pdf_closure_real__core_rknd("rsatl_2", check_status, rsatl_2, kgenref_rsatl_2)
    CALL kv_pdf_closure_real__core_rknd("stdev_eta_2", check_status, stdev_eta_2, kgenref_stdev_eta_2)
    CALL kv_pdf_closure_real__core_rknd("rc_2", check_status, rc_2, kgenref_rc_2)
    CALL kv_pdf_closure_real__core_rknd("covar_chi_eta_1", check_status, covar_chi_eta_1, kgenref_covar_chi_eta_1)
    CALL kv_pdf_closure_real__core_rknd("corr_chi_eta_2", check_status, corr_chi_eta_2, kgenref_corr_chi_eta_2)
    CALL kv_pdf_closure_real__core_rknd("corr_chi_eta_1", check_status, corr_chi_eta_1, kgenref_corr_chi_eta_1)
    CALL kv_pdf_closure_real__core_rknd("covar_chi_eta_2", check_status, covar_chi_eta_2, kgenref_covar_chi_eta_2)
    CALL kv_pdf_closure_real__core_rknd("mixt_frac", check_status, mixt_frac, kgenref_mixt_frac)
    CALL kv_pdf_closure_real__core_rknd("chi_2", check_status, chi_2, kgenref_chi_2)
    CALL kv_pdf_closure_real__core_rknd("stdev_chi_2", check_status, stdev_chi_2, kgenref_stdev_chi_2)
    CALL kv_pdf_closure_real__core_rknd("stdev_chi_1", check_status, stdev_chi_1, kgenref_stdev_chi_1)
    CALL kv_pdf_closure_real__core_rknd("chi_1", check_status, chi_1, kgenref_chi_1)
    CALL kv_pdf_closure_real__core_rknd("cloud_frac_2", check_status, cloud_frac_2, kgenref_cloud_frac_2)
    CALL kv_pdf_closure_real__core_rknd("cloud_frac_1", check_status, cloud_frac_1, kgenref_cloud_frac_1)
    CALL kv_pdf_closure_real__core_rknd("rc_1", check_status, rc_1, kgenref_rc_1)
    CALL kv_pdf_closure_real__core_rknd_dim1("alpha_sclr", check_status, alpha_sclr, kgenref_alpha_sclr)
    CALL kv_pdf_closure_real__core_rknd_dim1("rsclrthl", check_status, rsclrthl, kgenref_rsclrthl)
    CALL kv_pdf_closure_real__core_rknd_dim1("sclr1", check_status, sclr1, kgenref_sclr1)
    CALL kv_pdf_closure_real__core_rknd_dim1("sclr2", check_status, sclr2, kgenref_sclr2)
    CALL kv_pdf_closure_real__core_rknd_dim1("varnce_sclr2", check_status, varnce_sclr2, kgenref_varnce_sclr2)
    CALL kv_pdf_closure_real__core_rknd_dim1("varnce_sclr1", check_status, varnce_sclr1, kgenref_varnce_sclr1)
    CALL kv_pdf_closure_real__core_rknd_dim1("rsclrrt", check_status, rsclrrt, kgenref_rsclrrt)
    CALL kv_pdf_closure_logical__("l_scalar_calc", check_status, l_scalar_calc, kgenref_l_scalar_calc)
    CALL kv_pdf_closure_logical__("l_calc_ice_supersat_frac", check_status, l_calc_ice_supersat_frac, kgenref_l_calc_ice_supersat_frac)
    CALL kv_pdf_closure_real__core_rknd("tl2", check_status, tl2, kgenref_tl2)
    CALL kv_pdf_closure_real__core_rknd("tl1", check_status, tl1, kgenref_tl1)
    CALL kv_pdf_closure_real__core_rknd("beta2", check_status, beta2, kgenref_beta2)
    CALL kv_pdf_closure_real__core_rknd("beta1", check_status, beta1, kgenref_beta1)
    CALL kv_pdf_closure_real__core_rknd("sqrt_wp2", check_status, sqrt_wp2, kgenref_sqrt_wp2)
    CALL kv_pdf_closure_real__core_rknd("rc_coef", check_status, rc_coef, kgenref_rc_coef)
    CALL kv_pdf_closure_real__core_rknd("wprxp", check_status, wprxp, kgenref_wprxp)
    CALL kv_pdf_closure_real__core_rknd("wp2rxp", check_status, wp2rxp, kgenref_wp2rxp)
    CALL kv_pdf_closure_real__core_rknd("thlprxp", check_status, thlprxp, kgenref_thlprxp)
    CALL kv_pdf_closure_real__core_rknd("rtprxp", check_status, rtprxp, kgenref_rtprxp)
    CALL kv_pdf_closure_real__core_rknd("width_factor_1", check_status, width_factor_1, kgenref_width_factor_1)
    CALL kv_pdf_closure_real__core_rknd("width_factor_2", check_status, width_factor_2, kgenref_width_factor_2)
    CALL kv_pdf_closure_real__core_rknd("rt_at_ice_sat2", check_status, rt_at_ice_sat2, kgenref_rt_at_ice_sat2)
    CALL kv_pdf_closure_real__core_rknd("rt_at_ice_sat1", check_status, rt_at_ice_sat1, kgenref_rt_at_ice_sat1)
    CALL kv_pdf_closure_real__core_rknd("rc_1_ice", check_status, rc_1_ice, kgenref_rc_1_ice)
    CALL kv_pdf_closure_real__core_rknd("ice_supersat_frac1", check_status, ice_supersat_frac1, kgenref_ice_supersat_frac1)
    CALL kv_pdf_closure_real__core_rknd("ice_supersat_frac2", check_status, ice_supersat_frac2, kgenref_ice_supersat_frac2)
    CALL kv_pdf_closure_real__core_rknd("rc_2_ice", check_status, rc_2_ice, kgenref_rc_2_ice)
    CALL kv_pdf_closure_real__core_rknd("chi_at_ice_sat2", check_status, chi_at_ice_sat2, kgenref_chi_at_ice_sat2)
    CALL kv_pdf_closure_real__core_rknd("chi_at_ice_sat1", check_status, chi_at_ice_sat1, kgenref_chi_at_ice_sat1)
    !CALL kv_pdf_closure_integer__("i", check_status, i, kgenref_i)
    CALL kv_pdf_closure_integer__("hm_idx", check_status, hm_idx, kgenref_hm_idx)
    WRITE (*, *) ""
    IF (check_status%verboseLevel > 0) THEN
        WRITE (*, *) "Number of output variables: ", check_status%numTotal
        WRITE (*, *) "Number of identical variables: ", check_status%numIdentical
        WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol
        WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol
        WRITE (*, *) "Tolerance: ", check_status%tolerance
    END IF 
    WRITE (*, *) ""
    IF (check_status%numOutTol > 0) THEN
        WRITE (*, *) "Verification FAILED"
        check_status%Passed = .FALSE.
    ELSE
        WRITE (*, *) "Verification PASSED"
        check_status%Passed = .TRUE.
    END IF 
    WRITE (*, *) ""
    
    !Measuring elapsed time. Please increase the value of kgen_maxiter to get improve timing measurment resolution.
    CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock)
    DO kgen_intvar = 1, kgen_maxiter
        CALL kgen_kernel
    END DO 
    CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock)
    kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)
    WRITE (*, *) "pdf_closure_part3 : Time per call (usec): ", kgen_elapsed_time
    kgen_total_time = kgen_total_time + kgen_elapsed_time
    
    CONTAINS
    
    !kgen kernel subroutine
    SUBROUTINE kgen_kernel()
    if ( sclr_dim > 0 ) then
      l_scalar_calc = .true.
    else
      l_scalar_calc = .false.
    end if

    err_code = clubb_no_error ! Initialize to the value for no errors

    ! If there is no variance in vertical velocity, then treat rt and theta-l as
    ! constant, as well.  Otherwise width parameters (e.g. varnce_w_1,
    ! varnce_w_2, etc.) are non-zero.
    if ( wp2 <= w_tol_sqd )  then

      mixt_frac    = one_half
      w_1          = wm
      w_2          = wm
      varnce_w_1   = 0._core_rknd
      varnce_w_2   = 0._core_rknd
      rt_1         = rtm
      rt_2         = rtm
      alpha_rt     = one_half
      varnce_rt_1  = 0._core_rknd
      varnce_rt_2  = 0._core_rknd
      thl_1        = thlm
      thl_2        = thlm
      alpha_thl    = one_half
      varnce_thl_1 = 0._core_rknd
      varnce_thl_2 = 0._core_rknd
      rrtthl       = 0._core_rknd

      if ( l_scalar_calc ) then
        do i = 1, sclr_dim, 1
          sclr1(i)        = sclrm(i)
          sclr2(i)        = sclrm(i)
          varnce_sclr1(i) = 0.0_core_rknd
          varnce_sclr2(i) = 0.0_core_rknd
          alpha_sclr(i)   = one_half
          rsclrrt(i)      = 0.0_core_rknd
          rsclrthl(i)     = 0.0_core_rknd
        end do ! 1..sclr_dim
      end if

    else ! Width (standard deviation) parameters are non-zero

       ! The variable "mixt_frac" is the weight of the 1st PDF component.  The
       ! weight of the 2nd PDF component is "1-mixt_frac".  If there isn't any
       ! skewness of w (Sk_w = 0 because w'^3 = 0), mixt_frac = 0.5, and both
       ! PDF components are equally weighted.  If there is positive skewness of
       ! w (Sk_w > 0 because w'^3 > 0), 0 < mixt_frac < 0.5, and the 2nd PDF
       ! component has greater weight than does the 1st PDF component.  If there
       ! is negative skewness of w (Sk_w < 0 because w'^3 < 0),
       ! 0.5 < mixt_frac < 1, and the 1st PDF component has greater weight than
       ! does the 2nd PDF component.
       if ( abs( Skw ) <= 1e-5_core_rknd ) then
          mixt_frac = one_half
       else
          mixt_frac = one_half * ( one - Skw/ &
             sqrt( 4.0_core_rknd*( one - sigma_sqd_w )**3 + Skw**2 ) )
       endif

      ! Determine sqrt( wp2 ) here to avoid re-computing it
      sqrt_wp2 = sqrt( wp2 )

      ! Clip mixt_frac, 1-mixt_frac, to avoid dividing by zero
      ! Formula for mixt_frac_max_mag =
      ! 1 - ( 1/2 * ( 1 - Skw_max/sqrt( 4*( 1 - sigma_sqd_w )^3 + Skw_max^2 ) ) )
      ! Where sigma_sqd_w is fixed at 0.4.
      mixt_frac = min( max( mixt_frac, one-mixt_frac_max_mag ), mixt_frac_max_mag )

      ! The normalized mean of w for Gaussian "plume" 1 is w_1_n.  It's value
      ! will always be greater than 0.  As an example, a value of 1.0 would
      ! indicate that the actual mean of w for Gaussian "plume" 1 is found
      ! 1.0 standard deviation above the overall mean for w.
      w_1_n = sqrt( ( (one-mixt_frac)/mixt_frac )*(one-sigma_sqd_w) )
      ! The normalized mean of w for Gaussian "plume" 2 is w_2_n.  It's value
      ! will always be less than 0.  As an example, a value of -0.5 would
      ! indicate that the actual mean of w for Gaussian "plume" 2 is found
      ! 0.5 standard deviations below the overall mean for w.
      w_2_n = -sqrt( ( mixt_frac/(one-mixt_frac) )*(one-sigma_sqd_w) )
      ! The mean of w for Gaussian "plume" 1 is w_1.
      w_1 = wm + sqrt_wp2*w_1_n
      ! The mean of w for Gaussian "plume" 2 is w_2.
      w_2 = wm + sqrt_wp2*w_2_n

      ! The variance of w for Gaussian "plume" 1 for varnce_w_1.
      varnce_w_1  = sigma_sqd_w*wp2
      ! The variance of w for Gaussian "plume" 2 for varnce_w_2.
      ! The variance in both Gaussian "plumes" is defined to be the same.
      varnce_w_2  = sigma_sqd_w*wp2


      ! The normalized variance for thl, rt, and sclr for "plume" 1 is:
      !
      ! { 1 - [1/(1-sigma_sqd_w)]*[ (w'x')^2 / (w'^2 * x'^2) ] / mixt_frac }
      ! * { (1/3)*beta + mixt_frac*( 1 - (2/3)*beta ) };
      !
      ! where "x" stands for thl, rt, or sclr; "mixt_frac" is the weight of Gaussian
      ! "plume" 1, and 0 <= beta <= 3.
      !
      ! The factor { (1/3)*beta + mixt_frac*( 1 - (2/3)*beta ) } does not depend on
      ! which varable "x" stands for.  The factor is multiplied by 2 and defined
      ! as width_factor_1.
      !
      ! The factor { 1 - [1/(1-sigma_sqd_w)]*[ (w'x')^2 / (w'^2 * x'^2) ] / mixt_frac }
      ! depends on which variable "x" stands for.  It is multiplied by one_half and
      ! defined as alpha_x, where "x" stands for thl, rt, or sclr.

      ! Vince Larson added a dimensionless factor so that the
      ! width of plumes in theta_l, rt can vary.
      ! beta is a constant defined in module parameters_tunable
      !   Set 0<beta<3.
      ! beta=1.5_core_rknd recovers Chris Golaz' simplified formula.
      ! 3 Nov 2003

      width_factor_1 = ( 2.0_core_rknd/3.0_core_rknd )*beta + 2.0_core_rknd&
           *mixt_frac*( one - ( 2.0_core_rknd/3.0_core_rknd )*beta )
      width_factor_2 = 2.0_core_rknd - width_factor_1

      if ( thlp2 <= thl_tol**2 ) then
        thl_1        = thlm
        thl_2        = thlm
        varnce_thl_1 = 0.0_core_rknd
        varnce_thl_2 = 0.0_core_rknd
        alpha_thl    = one_half
      else
!       thl_1_n = - (wpthlp/(sqrt( wp2 )*sqrt( thlp2 )))/w_2_n
!       thl_2_n = - (wpthlp/(sqrt( wp2 )*sqrt( thlp2 )))/w_1_n

        thl_1 = thlm - ( wpthlp/sqrt_wp2 )/w_2_n
        thl_2 = thlm - ( wpthlp/sqrt_wp2 )/w_1_n

        alpha_thl = one_half * ( one - wpthlp*wpthlp / &
           ((one-sigma_sqd_w)*wp2*thlp2) )

        alpha_thl = max( min( alpha_thl, one ), zero_threshold )

        ! Vince Larson multiplied original expressions by width_factor_1,2
        !   to generalize scalar skewnesses.  05 Nov 03
        varnce_thl_1 = ( alpha_thl / mixt_frac * thlp2 ) * width_factor_1
        varnce_thl_2 = ( alpha_thl / (one-mixt_frac) * thlp2 ) * width_factor_2

      end if ! thlp2 <= thl_tol**2

      if ( rtp2 <= rt_tol**2 ) then
        rt_1        = rtm
        rt_2        = rtm
        varnce_rt_1 = 0.0_core_rknd
        varnce_rt_2 = 0.0_core_rknd
        alpha_rt    = one_half
      else
!       rt_1_n = -( wprtp / ( sqrt( wp2 )*sqrt( rtp2 ) ) ) / w_2_n
!       rt_2_n = -( wprtp / ( sqrt( wp2 )*sqrt( rtp2 ) ) ) / w_1_n

        rt_1 = rtm - ( wprtp / sqrt_wp2 ) / w_2_n
        rt_2 = rtm - ( wprtp / sqrt_wp2 ) / w_1_n

        alpha_rt = one_half * ( one - wprtp*wprtp / &
           ((one-sigma_sqd_w)*wp2*rtp2) )

        alpha_rt = max( min( alpha_rt, one ), zero_threshold )

        ! Vince Larson multiplied original expressions by width_factor_1,2
        !   to generalize scalar skewnesses.  05 Nov 03
        varnce_rt_1 = ( alpha_rt / mixt_frac * rtp2 ) * width_factor_1
        varnce_rt_2 = ( alpha_rt / (one-mixt_frac) * rtp2 ) * width_factor_2

      end if ! rtp2 <= rt_tol**2

      ! Compute pdf parameters for passive scalars
      if ( l_scalar_calc ) then
        do i = 1, sclr_dim
          if ( sclrp2(i) <= sclr_tol(i)**2 ) then
            ! Set plume sclr for plume 1,2 to the mean
            sclr1(i)= sclrm(i)
            sclr2(i)= sclrm(i)
            ! Set the variance to zero
            varnce_sclr1(i) = 0.0_core_rknd
            varnce_sclr2(i) = 0.0_core_rknd

            alpha_sclr(i) = one_half
          else
!           sclr1_n(i) = - ( wpsclrp(i) / (sqrt( wp2 ) &
!                        * sqrt( sclrp2(i) )) )/w_2_n
!           sclr2_n(i) = - ( wpsclrp(i) / (sqrt( wp2 ) &
!                        * sqrt( sclrp2(i) )) )/w_1_n

            sclr1(i) = sclrm(i)  & 
                     - ( wpsclrp(i) / sqrt_wp2 ) / w_2_n
            sclr2(i) = sclrm(i)  & 
                     - ( wpsclrp(i) / sqrt_wp2 ) / w_1_n

            alpha_sclr(i) = one_half * ( one - wpsclrp(i)*wpsclrp(i) & 
                    / ((one-sigma_sqd_w)*wp2*sclrp2(i)) )

            alpha_sclr(i) = max( min( alpha_sclr(i), one ), zero_threshold )

            ! Vince Larson multiplied original expressions by width_factor_1,2
            !  to generalize scalar skewnesses.  05 Nov 03
            varnce_sclr1(i) = ( alpha_sclr(i) / mixt_frac * sclrp2(i) ) * width_factor_1
            varnce_sclr2(i) = ( alpha_sclr(i) / (one-mixt_frac) * &
                sclrp2(i) ) * width_factor_2
          end if ! sclrp2(i) <= sclr_tol(i)**2
        end do ! i=1, sclr_dim
      end if ! l_scalar_calc

      ! We include sub-plume correlation with coeff rrtthl.

      if ( varnce_rt_1*varnce_thl_1 > 0._core_rknd .and. &
             varnce_rt_2*varnce_thl_2 > 0._core_rknd ) then
        rrtthl = ( rtpthlp - mixt_frac * ( rt_1-rtm ) * ( thl_1-thlm ) & 
                   - (one-mixt_frac) * ( rt_2-rtm ) * ( thl_2-thlm ) ) & 
                / ( mixt_frac*sqrt( varnce_rt_1*varnce_thl_1 ) &
                   + (one-mixt_frac)*sqrt( varnce_rt_2*varnce_thl_2 ) )
        if ( rrtthl < -one ) then
          rrtthl = -one
        end if
        if ( rrtthl > one ) then
          rrtthl = one
        end if
      else
        rrtthl = 0.0_core_rknd
      end if ! varnce_rt_1*varnce_thl_1 > 0 .and. varnce_rt_2*varnce_thl_2 > 0

      ! Sub-plume correlation, rsclrthl, of passive scalar and theta_l.
      if ( l_scalar_calc ) then
        do i=1, sclr_dim
          if ( varnce_sclr1(i)*varnce_thl_1 > 0._core_rknd .and. &
               varnce_sclr2(i)*varnce_thl_2 > 0._core_rknd ) then
            rsclrthl(i) = ( sclrpthlp(i)  & 
            - mixt_frac * ( sclr1(i)-sclrm(i) ) * ( thl_1-thlm ) & 
            - (one-mixt_frac) * ( sclr2(i)-sclrm(i) ) * ( thl_2-thlm ) ) & 
                / ( mixt_frac*sqrt( varnce_sclr1(i)*varnce_thl_1 )  & 
                         + (one-mixt_frac)*sqrt( varnce_sclr2(i)*varnce_thl_2 ) )
            if ( rsclrthl(i) < -one ) then
              rsclrthl(i) = -one
            end if
            if ( rsclrthl(i) > one ) then
              rsclrthl(i) = one
            end if
          else
            rsclrthl(i) = 0.0_core_rknd
          end if

          ! Sub-plume correlation, rsclrrt, of passive scalar and total water.

          if ( varnce_sclr1(i)*varnce_rt_1 > 0._core_rknd .and. &
               varnce_sclr2(i)*varnce_rt_2 > 0._core_rknd ) then
            rsclrrt(i) = ( sclrprtp(i) - mixt_frac * ( sclr1(i)-sclrm(i) ) * ( rt_1-rtm )&
                         - (one-mixt_frac) * ( sclr2(i)-sclrm(i) ) * ( rt_2-rtm ) ) & 
                       / ( mixt_frac*sqrt( varnce_sclr1(i)*varnce_rt_1 ) &
                         + (one-mixt_frac)*sqrt( varnce_sclr2(i)*varnce_rt_2 ) )
            if ( rsclrrt(i) < -one ) then
              rsclrrt(i) = -one
            end if
            if ( rsclrrt(i) > one ) then
              rsclrrt(i) = one
            end if
          else
            rsclrrt(i) = 0.0_core_rknd
          end if
        end do ! i=1, sclr_dim
      end if ! l_scalar_calc

    end if  ! Widths non-zero

    ! Compute higher order moments (these are interactive)
    wp2rtp  = mixt_frac * ( (w_1-wm)**2+varnce_w_1 ) * ( rt_1-rtm ) & 
            + (one-mixt_frac) * ( (w_2-wm)**2+varnce_w_2 ) * ( rt_2-rtm )

    wp2thlp = mixt_frac * ( (w_1-wm)**2+varnce_w_1 ) * ( thl_1-thlm ) & 
            + (one-mixt_frac) * ( (w_2-wm)**2+varnce_w_2 ) * ( thl_2-thlm )

    ! Compute higher order moments (these are non-interactive diagnostics)
    if ( iwp4 > 0 ) then
      wp4 = mixt_frac * ( 3._core_rknd*varnce_w_1**2 + &
          6._core_rknd*((w_1-wm)**2)*varnce_w_1 + (w_1-wm)**4 ) & 
          + (one-mixt_frac) * ( 3._core_rknd*varnce_w_2**2 + &
          6._core_rknd*((w_2-wm)**2)*varnce_w_2 + (w_2-wm)**4 )
    end if

    if ( iwprtp2 > 0 ) then
      wprtp2  = mixt_frac * ( w_1-wm )*( (rt_1-rtm)**2 + varnce_rt_1 )  & 
              + (one-mixt_frac) * ( w_2-wm )*( (rt_2-rtm)**2 + varnce_rt_2)
    end if

    if ( iwpthlp2 > 0 ) then
      wpthlp2 = mixt_frac * ( w_1-wm )*( (thl_1-thlm)**2 + varnce_thl_1 )  & 
              + (one-mixt_frac) * ( w_2-wm )*( (thl_2-thlm)**2+varnce_thl_2 )
    end if

    if ( iwprtpthlp > 0 ) then
      wprtpthlp = mixt_frac * ( w_1-wm )*( (rt_1-rtm)*(thl_1-thlm)  & 
                + rrtthl*sqrt( varnce_rt_1*varnce_thl_1 ) ) & 
                + ( one-mixt_frac ) * ( w_2-wm )*( (rt_2-rtm)*(thl_2-thlm) & 
                + rrtthl*sqrt( varnce_rt_2*varnce_thl_2 ) )
    end if


    ! Scalar Addition to higher order moments
    if ( l_scalar_calc ) then
      do i=1, sclr_dim

        wp2sclrp(i)  = mixt_frac * ( (w_1-wm)**2+varnce_w_1 )*( sclr1(i)-sclrm(i) ) & 
                     + (one-mixt_frac) * ( (w_2-wm)**2+varnce_w_2 ) * ( sclr2(i)-sclrm(i) )

        wpsclrp2(i) = mixt_frac * ( w_1-wm ) * ( (sclr1(i)-sclrm(i))**2 + varnce_sclr1(i) )  & 
                    + (one-mixt_frac) * ( w_2-wm ) * &
                    ( (sclr2(i)-sclrm(i))**2 + varnce_sclr2(i) )

        wpsclrprtp(i) = mixt_frac * ( w_1-wm ) * ( ( rt_1-rtm )*( sclr1(i)-sclrm(i) )  & 
          + rsclrrt(i)*sqrt( varnce_rt_1*varnce_sclr1(i) ) ) &
          + ( one-mixt_frac )*( w_2-wm ) *  &
            ( ( rt_2-rtm )*( sclr2(i)-sclrm(i) ) + rsclrrt(i)*sqrt( varnce_rt_2*varnce_sclr2(i) ) )

        wpsclrpthlp(i) = mixt_frac * ( w_1-wm ) * ( ( sclr1(i)-sclrm(i) )*( thl_1-thlm )  & 
          + rsclrthl(i)*sqrt( varnce_sclr1(i)*varnce_thl_1 ) ) & 
          + ( one-mixt_frac ) * ( w_2-wm ) * &
            ( ( sclr2(i)-sclrm(i) )*( thl_2-thlm ) &
              + rsclrthl(i)*sqrt( varnce_sclr2(i)*varnce_thl_2 ) )

      end do ! i=1, sclr_dim
    end if ! l_scalar_calc

    ! Compute higher order moments that include theta_v.

    ! First compute some preliminary quantities.
    ! "1" denotes first Gaussian; "2" denotes 2nd Gaussian
    ! liq water temp (Sommeria & Deardorff 1977 (SD), eqn. 3)

    tl1  = thl_1*exner
    tl2  = thl_2*exner

!KGEN# 691 "<stdin>"
    rsatl_1 = sat_mixrat_liq( p_in_Pa, tl1 )
    rsatl_2 = sat_mixrat_liq( p_in_Pa, tl2 ) ! h1g, 2010-06-16 end mod


    ! SD's beta (eqn. 8)
    beta1 = ep * ( Lv/(Rd*tl1) ) * ( Lv/(Cp*tl1) )
    beta2 = ep * ( Lv/(Rd*tl2) ) * ( Lv/(Cp*tl2) )

    ! s from Lewellen and Yoh 1993 (LY) eqn. 1
    chi_1 = ( rt_1 - rsatl_1 ) / ( one + beta1 * rsatl_1 )
    chi_2 = ( rt_2 - rsatl_2 ) / ( one + beta2 * rsatl_2 )

    ! Coefficients for s'
    ! For each normal distribution in the sum of two normal distributions,
    ! s' = crt * rt'  +  cthl * thl';
    ! therefore, x's' = crt * x'rt'  +  cthl * x'thl'.
    ! Larson et al. May, 2001.

    crt_1  = one/( one + beta1*rsatl_1)
    crt_2  = one/( one + beta2*rsatl_2)

    cthl_1 = ( (one + beta1 * rt_1) / ( one + beta1*rsatl_1)**2 ) & 
             * ( Cp/Lv ) * beta1 * rsatl_1 * exner
    cthl_2 = ( (one + beta2 * rt_2) / ( one + beta2*rsatl_2 )**2 ) & 
             * ( Cp/Lv ) * beta2 * rsatl_2 * exner

    ! Standard deviation of chi for each component.
    ! Include subplume correlation of qt, thl
    ! Because of round-off error,
    ! stdev_chi_1 (and probably stdev_chi_2) can become negative when rrtthl=1
    ! One could also write this as a squared term
    ! plus a postive correction; this might be a neater format
    stdev_chi_1 = sqrt( max( crt_1**2 * varnce_rt_1  &
                          - two * rrtthl * crt_1 * cthl_1  &
                                * sqrt( varnce_rt_1 * varnce_thl_1 )  &
                          + cthl_1**2 * varnce_thl_1,  &
                          zero_threshold )  )

    stdev_chi_2 = sqrt( max( crt_2**2 * varnce_rt_2  &
                          - two * rrtthl * crt_2 * cthl_2  &
                                * sqrt( varnce_rt_2 * varnce_thl_2 )  &
                          + cthl_2**2 * varnce_thl_2,  &
                          zero_threshold )  )

    ! We need to introduce a threshold value for the variance of chi
    if ( stdev_chi_1 <= chi_tol ) then
      ! Treat chi as a delta function in this component.
      stdev_chi_1 = zero
    end if

    if ( stdev_chi_2 <= chi_tol ) then
      ! Treat chi as a delta function in this component.
      stdev_chi_2 = zero
    end if

    ! Standard deviation of eta for each component.
    stdev_eta_1 = sqrt( max( crt_1**2 * varnce_rt_1  &
                          + two * rrtthl * crt_1 * cthl_1  &
                                * sqrt( varnce_rt_1 * varnce_thl_1 )  &
                          + cthl_1**2 * varnce_thl_1,  &
                          zero_threshold )  )

    stdev_eta_2 = sqrt( max( crt_2**2 * varnce_rt_2  &
                          + two * rrtthl * crt_2 * cthl_2  &
                                * sqrt( varnce_rt_2 * varnce_thl_2 )  &
                          + cthl_2**2 * varnce_thl_2,  &
                          zero_threshold )  )

    ! Covariance of chi and eta for each component.
    covar_chi_eta_1 = crt_1**2 * varnce_rt_1 - cthl_1**2 * varnce_thl_1

    covar_chi_eta_2 = crt_2**2 * varnce_rt_2 - cthl_2**2 * varnce_thl_2

    ! Correlation of chi and eta for each component.
    if ( stdev_chi_1 * stdev_eta_1 > zero ) then
      corr_chi_eta_1 = covar_chi_eta_1 / ( stdev_chi_1 * stdev_eta_1 )
    else
      corr_chi_eta_1 = zero
    endif

    if ( stdev_chi_2 * stdev_eta_2 > zero ) then
      corr_chi_eta_2 = covar_chi_eta_2 / ( stdev_chi_2 * stdev_eta_2 )
    else
      corr_chi_eta_2 = zero
    endif
    
    ! Determine whether to compute ice_supersat_frac. We do not compute
    ! ice_supersat_frac for GFDL (unless do_liquid_only_in_clubb is true),
    ! because liquid and ice are both fed into rtm, ruining the calculation.







    l_calc_ice_supersat_frac = .true.


    ! Calculate cloud_frac_1 and rc_1
    call calc_cloud_frac_component(chi_1, stdev_chi_1, chi_at_liq_sat, cloud_frac_1, rc_1)

    ! Calculate cloud_frac_2 and rc_2
    call calc_cloud_frac_component(chi_2, stdev_chi_2, chi_at_liq_sat, cloud_frac_2, rc_2)

    if ( l_calc_ice_supersat_frac ) then
      ! We must compute chi_at_ice_sat1 and chi_at_ice_sat2
      if (tl1 <= T_freeze_K) then
        rt_at_ice_sat1 = sat_mixrat_ice( p_in_Pa, tl1 )
        chi_at_ice_sat1 = ( rt_at_ice_sat1 - rsatl_1 ) / ( one + beta1 * rsatl_1 )
      else
        ! If the temperature is warmer than freezing (> 0C) then ice_supersat_frac
        ! is not defined, so we use chi_at_liq_sat
        chi_at_ice_sat1 = chi_at_liq_sat
      end if

      if (tl2 <= T_freeze_K) then
        rt_at_ice_sat2 = sat_mixrat_ice( p_in_Pa, tl2 )
        chi_at_ice_sat2 = ( rt_at_ice_sat2 - rsatl_2 ) / ( one + beta2 * rsatl_2 )
      else
        ! If the temperature is warmer than freezing (> 0C) then ice_supersat_frac
        ! is not defined, so we use chi_at_liq_sat
        chi_at_ice_sat2 = chi_at_liq_sat
      end if

      ! Calculate ice_supersat_frac1
      call calc_cloud_frac_component( chi_1, stdev_chi_1, chi_at_ice_sat1, &
                                      ice_supersat_frac1, rc_1_ice )
      
      ! Calculate ice_supersat_frac2
      call calc_cloud_frac_component( chi_2, stdev_chi_2, chi_at_ice_sat2, &
                                      ice_supersat_frac2, rc_2_ice )
    end if

    ! Compute moments that depend on theta_v
    !
    ! The moments that depend on th_v' are calculated based on an approximated
    ! and linearized form of the theta_v equation:
    !
    ! theta_v = theta_l + { (R_v/R_d) - 1 } * thv_ds * r_t
    !                   + [ {L_v/(C_p*exner)} - (R_v/R_d) * thv_ds ] * r_c;
    !
    ! and therefore:
    !
    ! th_v' = th_l' + { (R_v/R_d) - 1 } * thv_ds * r_t'
    !               + [ {L_v/(C_p*exner)} - (R_v/R_d) * thv_ds ] * r_c';
    !
    ! where thv_ds is used as a reference value to approximate theta_l.

    rc_coef = Lv / (exner*Cp) - ep2 * thv_ds

    wp2rxp  = zero
    wprxp   = zero
    thlprxp = zero
    rtprxp  = zero
    if ( l_liq_ice_loading_test ) then
       do hm_idx = 1, hydromet_dim, 1
          if ( l_mix_rat_hm(hm_idx) ) then
             wp2rxp  = wp2rxp + wp2hmp(hm_idx)
             wprxp   = wprxp + wphydrometp(hm_idx)
             thlprxp = thlprxp + thlphmp(hm_idx)
             rtprxp  = rtprxp + rtphmp(hm_idx)
          endif
       enddo ! hm_idx = 1, hydromet_dim, 1
    endif ! l_liq_ice_loading_test

    wp2rcp = mixt_frac * ((w_1-wm)**2 + varnce_w_1)*rc_1 &
               + (one-mixt_frac) * ((w_2-wm)**2 + varnce_w_2)*rc_2 & 
             - wp2 * (mixt_frac*rc_1+(one-mixt_frac)*rc_2)

    wp2thvp = wp2thlp + ep1*thv_ds*wp2rtp + rc_coef*wp2rcp - thv_ds * wp2rxp

    wprcp = mixt_frac * (w_1-wm)*rc_1 + (one-mixt_frac) * (w_2-wm)*rc_2

    wpthvp = wpthlp + ep1*thv_ds*wprtp + rc_coef*wprcp - thv_ds * wprxp

    ! Account for subplume correlation in qt-thl
    thlprcp  = mixt_frac * ( (thl_1-thlm)*rc_1 - (cthl_1*varnce_thl_1)*cloud_frac_1 ) & 
             + (one-mixt_frac) * ( (thl_2-thlm)*rc_2 - (cthl_2*varnce_thl_2)*cloud_frac_2 ) & 
             + mixt_frac*rrtthl*crt_1*sqrt( varnce_rt_1*varnce_thl_1 )*cloud_frac_1 & 
             + (one-mixt_frac)*rrtthl*crt_2*sqrt( varnce_rt_2*varnce_thl_2 )*cloud_frac_2
    thlpthvp = thlp2 + ep1*thv_ds*rtpthlp + rc_coef*thlprcp - thv_ds * thlprxp

    ! Account for subplume correlation in qt-thl
    rtprcp = mixt_frac * ( (rt_1-rtm)*rc_1 + (crt_1*varnce_rt_1)*cloud_frac_1 ) & 
           + (one-mixt_frac) * ( (rt_2-rtm)*rc_2 + (crt_2*varnce_rt_2)*cloud_frac_2 ) & 
           - mixt_frac*rrtthl*cthl_1*sqrt( varnce_rt_1*varnce_thl_1 )*cloud_frac_1 & 
           - (one-mixt_frac)*rrtthl*cthl_2*sqrt( varnce_rt_2*varnce_thl_2 )*cloud_frac_2

    rtpthvp  = rtpthlp + ep1*thv_ds*rtp2 + rc_coef*rtprcp - thv_ds * rtprxp

    ! Account for subplume correlation of scalar, theta_v.
    ! See Eqs. A13, A8 from Larson et al. (2002) ``Small-scale...''
    !  where the ``scalar'' in this paper is w.
    if ( l_scalar_calc ) then
      do i=1, sclr_dim
        sclrprcp(i) &
        = mixt_frac * ( ( sclr1(i)-sclrm(i) ) * rc_1 ) &
          + (one-mixt_frac) * ( ( sclr2(i)-sclrm(i) ) * rc_2 ) & 
          + mixt_frac*rsclrrt(i) * crt_1 &
            * sqrt( varnce_sclr1(i) * varnce_rt_1 ) * cloud_frac_1 & 
          + (one-mixt_frac) * rsclrrt(i) * crt_2 &
            * sqrt( varnce_sclr2(i) * varnce_rt_2 ) * cloud_frac_2 & 
          - mixt_frac * rsclrthl(i) * cthl_1 &
            * sqrt( varnce_sclr1(i) * varnce_thl_1 ) * cloud_frac_1 & 
          - (one-mixt_frac) * rsclrthl(i) * cthl_2 &
            * sqrt( varnce_sclr2(i) * varnce_thl_2 ) * cloud_frac_2

        sclrpthvp(i) = sclrpthlp(i) + ep1*thv_ds*sclrprtp(i) + rc_coef*sclrprcp(i)
      end do ! i=1, sclr_dim
    end if ! l_scalar_calc

    ! Compute mean cloud fraction and cloud water
    cloud_frac = calc_cloud_frac(cloud_frac_1, cloud_frac_2, mixt_frac)
    rcm        = mixt_frac * rc_1         + (one-mixt_frac) * rc_2
    
    rcm = max( zero_threshold, rcm )
    
    if (l_calc_ice_supersat_frac) then
      ! Compute ice cloud fraction, ice_supersat_frac
      ice_supersat_frac = calc_cloud_frac(ice_supersat_frac1, ice_supersat_frac2, mixt_frac)
    else
      ! ice_supersat_frac will be garbage if computed as above
      ice_supersat_frac = 0.0_core_rknd
      if (clubb_at_least_debug_level( 1 )) then
         write(fstderr,*) "Warning: ice_supersat_frac has garbage values if &
                         & do_liquid_only_in_clubb = .false."
      end if
    end if
    ! Compute variance of liquid water mixing ratio.
    ! This is not needed for closure.  Statistical Analysis only.






      rcp2 = mixt_frac * ( chi_1*rc_1 + cloud_frac_1*stdev_chi_1**2 ) &
             + ( one-mixt_frac ) * ( chi_2*rc_2 + cloud_frac_2*stdev_chi_2**2 ) - rcm**2
      rcp2 = max( zero_threshold, rcp2 )







    ! Save PDF parameters
    pdf_params%w_1             = w_1
    pdf_params%w_2             = w_2
    pdf_params%varnce_w_1      = varnce_w_1
    pdf_params%varnce_w_2      = varnce_w_2
    pdf_params%rt_1            = rt_1
    pdf_params%rt_2            = rt_2
    pdf_params%varnce_rt_1     = varnce_rt_1
    pdf_params%varnce_rt_2     = varnce_rt_2
    pdf_params%thl_1           = thl_1
    pdf_params%thl_2           = thl_2
    pdf_params%varnce_thl_1    = varnce_thl_1
    pdf_params%varnce_thl_2    = varnce_thl_2
    pdf_params%rrtthl          = rrtthl
    pdf_params%alpha_thl       = alpha_thl
    pdf_params%alpha_rt        = alpha_rt
    pdf_params%crt_1           = crt_1
    pdf_params%crt_2           = crt_2
    pdf_params%cthl_1          = cthl_1
    pdf_params%cthl_2          = cthl_2
    pdf_params%chi_1           = chi_1
    pdf_params%chi_2           = chi_2
    pdf_params%stdev_chi_1     = stdev_chi_1
    pdf_params%stdev_chi_2     = stdev_chi_2
    pdf_params%stdev_eta_1     = stdev_eta_1
    pdf_params%stdev_eta_2     = stdev_eta_2
    pdf_params%covar_chi_eta_1 = covar_chi_eta_1
    pdf_params%covar_chi_eta_2 = covar_chi_eta_2
    pdf_params%corr_chi_eta_1  = corr_chi_eta_1
    pdf_params%corr_chi_eta_2  = corr_chi_eta_2
    pdf_params%rsatl_1         = rsatl_1
    pdf_params%rsatl_2         = rsatl_2
    pdf_params%rc_1            = rc_1
    pdf_params%rc_2            = rc_2
    pdf_params%cloud_frac_1    = cloud_frac_1
    pdf_params%cloud_frac_2    = cloud_frac_2
    pdf_params%mixt_frac       = mixt_frac


    if ( clubb_at_least_debug_level( 2 ) ) then

      call pdf_closure_check & 
           ( wp4, wprtp2, wp2rtp, wpthlp2, & 
             wp2thlp, cloud_frac, rcm, wpthvp, wp2thvp, & 
             rtpthvp, thlpthvp, wprcp, wp2rcp, & 
             rtprcp, thlprcp, rcp2, wprtpthlp, & 
             crt_1, crt_2, cthl_1, cthl_2, pdf_params, &
             sclrpthvp, sclrprcp, wpsclrp2, & 
             wpsclrprtp, wpsclrpthlp, wp2sclrp, &
             err_code )

      ! Error Reporting
      ! Joshua Fasching February 2008

      if ( fatal_error( err_code ) ) then

        write(fstderr,*) "Error in pdf_closure_new"

        write(fstderr,*) "Intent(in)"

        write(fstderr,*) "p_in_Pa = ", p_in_Pa
        write(fstderr,*) "exner = ", exner
        write(fstderr,*) "thv_ds = ", thv_ds
        write(fstderr,*) "wm = ", wm
        write(fstderr,*) "wp2 = ", wp2
        write(fstderr,*) "wp3 = ", wp3
        write(fstderr,*) "sigma_sqd_w = ", sigma_sqd_w
        write(fstderr,*) "rtm = ", rtm
        write(fstderr,*) "rtp2 = ", rtp2
        write(fstderr,*) "wprtp = ", wprtp
        write(fstderr,*) "thlm = ", thlm
        write(fstderr,*) "thlp2 = ", thlp2
        write(fstderr,*) "wpthlp = ", wpthlp
        write(fstderr,*) "rtpthlp = ", rtpthlp

        if ( sclr_dim > 0 ) then
          write(fstderr,*) "sclrm = ", sclrm
          write(fstderr,*) "wpsclrp = ", wpsclrp
          write(fstderr,*) "sclrp2 = ", sclrp2
          write(fstderr,*) "sclrprtp = ", sclrprtp
          write(fstderr,*) "sclrpthlp = ", sclrpthlp
        end if

        write(fstderr,*) "level = ", level

        write(fstderr,*) "Intent(out)"

        write(fstderr,*) "wp4 = ", wp4
        write(fstderr,*) "wprtp2 = ", wprtp2
        write(fstderr,*) "wp2rtp = ", wp2rtp
        write(fstderr,*) "wpthlp2 = ", wpthlp2
        write(fstderr,*) "cloud_frac = ", cloud_frac
        write(fstderr,*) "ice_supersat_frac = ", ice_supersat_frac
        write(fstderr,*) "rcm = ", rcm
        write(fstderr,*) "wpthvp = ", wpthvp
        write(fstderr,*) "wp2thvp = ", wp2thvp
        write(fstderr,*) "rtpthvp = ", rtpthvp
        write(fstderr,*) "thlpthvp = ", thlpthvp
        write(fstderr,*) "wprcp = ", wprcp
        write(fstderr,*) "wp2rcp = ", wp2rcp
        write(fstderr,*) "rtprcp = ", rtprcp
        write(fstderr,*) "thlprcp = ", thlprcp
        write(fstderr,*) "rcp2 = ", rcp2
        write(fstderr,*) "wprtpthlp = ", wprtpthlp
        write(fstderr,*) "pdf_params%w_1 = ", pdf_params%w_1
        write(fstderr,*) "pdf_params%w_2 = ", pdf_params%w_2
        write(fstderr,*) "pdf_params%varnce_w_1 = ", pdf_params%varnce_w_1
        write(fstderr,*) "pdf_params%varnce_w_2 = ", pdf_params%varnce_w_2
        write(fstderr,*) "pdf_params%rt_1 = ", pdf_params%rt_1
        write(fstderr,*) "pdf_params%rt_2 = ", pdf_params%rt_2
        write(fstderr,*) "pdf_params%varnce_rt_1 = ", pdf_params%varnce_rt_1
        write(fstderr,*) "pdf_params%varnce_rt_2 = ", pdf_params%varnce_rt_2
        write(fstderr,*) "pdf_params%thl_1 = ", pdf_params%thl_1
        write(fstderr,*) "pdf_params%thl_2 = ", pdf_params%thl_2
        write(fstderr,*) "pdf_params%varnce_thl_1 = ", pdf_params%varnce_thl_1
        write(fstderr,*) "pdf_params%varnce_thl_2 = ", pdf_params%varnce_thl_2
        write(fstderr,*) "pdf_params%rrtthl = ", pdf_params%rrtthl
        write(fstderr,*) "pdf_params%alpha_thl = ", pdf_params%alpha_thl
        write(fstderr,*) "pdf_params%alpha_rt = ", pdf_params%alpha_rt
        write(fstderr,*) "pdf_params%crt_1 = ", pdf_params%crt_1
        write(fstderr,*) "pdf_params%crt_2 = ", pdf_params%crt_2
        write(fstderr,*) "pdf_params%cthl_1 = ", pdf_params%cthl_1
        write(fstderr,*) "pdf_params%cthl_2 = ", pdf_params%cthl_2
        write(fstderr,*) "pdf_params%chi_1 = ", pdf_params%chi_1
        write(fstderr,*) "pdf_params%chi_2 = ", pdf_params%chi_2
        write(fstderr,*) "pdf_params%stdev_chi_1 = ", pdf_params%stdev_chi_1
        write(fstderr,*) "pdf_params%stdev_chi_2 = ", pdf_params%stdev_chi_2
        write(fstderr,*) "pdf_params%stdev_eta_1 = ", pdf_params%stdev_eta_1
        write(fstderr,*) "pdf_params%stdev_eta_2 = ", pdf_params%stdev_eta_2
        write(fstderr,*) "pdf_params%covar_chi_eta_1 = ", pdf_params%covar_chi_eta_1
        write(fstderr,*) "pdf_params%covar_chi_eta_2 = ", pdf_params%covar_chi_eta_2
        write(fstderr,*) "pdf_params%corr_chi_eta_1 = ", pdf_params%corr_chi_eta_1
        write(fstderr,*) "pdf_params%corr_chi_eta_2 = ", pdf_params%corr_chi_eta_2
        write(fstderr,*) "pdf_params%rsatl_1 = ", pdf_params%rsatl_1
        write(fstderr,*) "pdf_params%rsatl_2 = ", pdf_params%rsatl_2
        write(fstderr,*) "pdf_params%rc_1 = ", pdf_params%rc_1
        write(fstderr,*) "pdf_params%rc_2 = ", pdf_params%rc_2
        write(fstderr,*) "pdf_params%cloud_frac_1 = ", pdf_params%cloud_frac_1
        write(fstderr,*) "pdf_params%cloud_frac_2 = ", pdf_params%cloud_frac_2
        write(fstderr,*) "pdf_params%mixt_frac = ", pdf_params%mixt_frac

        if ( sclr_dim > 0 )then
          write(fstderr,*) "sclrpthvp = ", sclrpthvp
          write(fstderr,*) "sclrprcp = ", sclrprcp
          write(fstderr,*) "wpsclrp2 = ", wpsclrp2
          write(fstderr,*) "wpsclrprtp = ", wpsclrprtp
          write(fstderr,*) "wpsclrpthlp = ", wpsclrpthlp
          write(fstderr,*) "wp2sclrp = ", wp2sclrp
        end if

      end if ! Fatal error

    end if ! clubb_at_least_debug_level
    END SUBROUTINE kgen_kernel
    
    !verify state subroutine for kv_pdf_closure_real__core_rknd
    RECURSIVE SUBROUTINE kv_pdf_closure_real__core_rknd(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        REAL(KIND=core_rknd), INTENT(IN) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        real(KIND=core_rknd) :: diff
        
        check_status%numTotal = check_status%numTotal + 1
        
        IF (var == kgenref_var) THEN
            check_status%numIdentical = check_status%numIdentical + 1
            IF (check_status%verboseLevel > 1) THEN
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
            END IF 
            check_result = CHECK_IDENTICAL
        ELSE
            diff = ABS(var - kgenref_var)
            IF (diff <= check_status%tolerance) THEN
                check_status%numInTol = check_status%numInTol + 1
                IF (check_status%verboseLevel > 0) THEN
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                END IF 
                check_result = CHECK_IN_TOL
            ELSE
                check_status%numOutTol = check_status%numOutTol + 1
                IF (check_status%verboseLevel > 0) THEN
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                END IF 
                check_result = CHECK_OUT_TOL
            END IF 
        END IF 
        IF (check_result == CHECK_IDENTICAL) THEN
            IF (check_status%verboseLevel > 2) THEN
                WRITE (*, *) "Difference is ", 0
                WRITE (*, *) ""
            END IF 
        ELSE IF (check_result == CHECK_OUT_TOL) THEN
            IF (check_status%verboseLevel > 0) THEN
                WRITE (*, *) "Difference is ", diff
                WRITE (*, *) ""
            END IF 
        ELSE IF (check_result == CHECK_IN_TOL) THEN
            IF (check_status%verboseLevel > 1) THEN
                WRITE (*, *) "Difference is ", diff
                WRITE (*, *) ""
            END IF 
        END IF 
        
    END SUBROUTINE kv_pdf_closure_real__core_rknd
    
    !verify state subroutine for kv_pdf_closure_integer__
    RECURSIVE SUBROUTINE kv_pdf_closure_integer__(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        INTEGER, INTENT(IN) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        integer :: diff
        
        check_status%numTotal = check_status%numTotal + 1
        
        IF (var == kgenref_var) THEN
            check_status%numIdentical = check_status%numIdentical + 1
            IF (check_status%verboseLevel > 1) THEN
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
            END IF 
            check_result = CHECK_IDENTICAL
        ELSE
            diff = ABS(var - kgenref_var)
            IF (diff <= check_status%tolerance) THEN
                check_status%numInTol = check_status%numInTol + 1
                IF (check_status%verboseLevel > 0) THEN
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                END IF 
                check_result = CHECK_IN_TOL
            ELSE
                check_status%numOutTol = check_status%numOutTol + 1
                IF (check_status%verboseLevel > 0) THEN
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                END IF 
                check_result = CHECK_OUT_TOL
            END IF 
        END IF 
        IF (check_result == CHECK_IDENTICAL) THEN
            IF (check_status%verboseLevel > 2) THEN
                WRITE (*, *) "Difference is ", 0
                WRITE (*, *) ""
            END IF 
        ELSE IF (check_result == CHECK_OUT_TOL) THEN
            IF (check_status%verboseLevel > 0) THEN
                WRITE (*, *) "Difference is ", diff
                WRITE (*, *) ""
            END IF 
        ELSE IF (check_result == CHECK_IN_TOL) THEN
            IF (check_status%verboseLevel > 1) THEN
                WRITE (*, *) "Difference is ", diff
                WRITE (*, *) ""
            END IF 
        END IF 
        
    END SUBROUTINE kv_pdf_closure_integer__
    
    !verify state subroutine for kv_pdf_closure_real__core_rknd_dim1
    RECURSIVE SUBROUTINE kv_pdf_closure_real__core_rknd_dim1(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        REAL(KIND=core_rknd), INTENT(IN), DIMENSION(:) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        INTEGER :: idx1
        INTEGER :: n
        real(KIND=core_rknd) :: nrmsdiff, rmsdiff
        real(KIND=core_rknd), ALLOCATABLE :: buf1(:), buf2(:)
        
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
        
    END SUBROUTINE kv_pdf_closure_real__core_rknd_dim1
    
    !verify state subroutine for kv_pdf_closure_logical__
    RECURSIVE SUBROUTINE kv_pdf_closure_logical__(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        LOGICAL, INTENT(IN) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        logical :: diff
        
        check_status%numTotal = check_status%numTotal + 1
        
        IF (var .EQV. kgenref_var) THEN
            check_status%numIdentical = check_status%numIdentical + 1
            IF (check_status%verboseLevel > 1) THEN
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
            END IF 
            check_result = CHECK_IDENTICAL
        ELSE
            check_status%numOutTol = check_status%numOutTol + 1
            IF (check_status%verboseLevel > 0) THEN
                WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL."
            END IF 
            check_result = CHECK_OUT_TOL
        END IF 
        IF (check_result == CHECK_IDENTICAL) THEN
            IF (check_status%verboseLevel > 2) THEN
                WRITE (*, *) "NOT IMPLEMENTED"
                WRITE (*, *) ""
            END IF 
        ELSE IF (check_result == CHECK_OUT_TOL) THEN
            IF (check_status%verboseLevel > 0) THEN
                WRITE (*, *) "NOT IMPLEMENTED"
                WRITE (*, *) ""
            END IF 
        ELSE IF (check_result == CHECK_IN_TOL) THEN
            IF (check_status%verboseLevel > 1) THEN
                WRITE (*, *) "NOT IMPLEMENTED"
                WRITE (*, *) ""
            END IF 
        END IF 
        
    END SUBROUTINE kv_pdf_closure_logical__
    
  END SUBROUTINE pdf_closure
  
  !=============================================================================
  elemental subroutine calc_cloud_frac_component( mean_chi_i, stdev_chi_i, &
                                                  chi_at_sat, &
                                                  cloud_frac_i, rc_i )

    ! Description:
    ! Calculates the PDF component cloud water mixing ratio, rc_i, and cloud
    ! fraction, cloud_frac_i, for the ith PDF component.
    !
    ! The equation for cloud water mixing ratio, rc, at any point is:
    !
    ! rc = chi * H(chi);
    !
    ! and the equation for cloud fraction at a point, fc, is:
    !
    ! fc = H(chi);
    !
    ! where where extended liquid water mixing ratio, chi, is equal to cloud
    ! water mixing ratio, rc, when positive.  When the atmosphere is saturated
    ! at this point, cloud water is found, and rc = chi, while fc = 1.
    ! Otherwise, clear air is found at this point, and rc = fc = 0.
    !
    ! The mean of rc and fc is calculated by integrating over the PDF, such
    ! that:
    !
    ! <rc> = INT(-inf:inf) chi * H(chi) * P(chi) dchi; and
    !
    ! cloud_frac = <fc> = INT(-inf:inf) H(chi) * P(chi) dchi.
    !
    ! This can be rewritten as:
    !
    ! <rc> = INT(0:inf) chi * P(chi) dchi; and
    !
    ! cloud_frac = <fc> = INT(0:inf) P(chi) dchi;
    !
    ! and further rewritten as:
    !
    ! <rc> = SUM(i=1,N) mixt_frac_i INT(0:inf) chi * P_i(chi) dchi; and
    !
    ! cloud_frac = SUM(i=1,N) mixt_frac_i INT(0:inf) P_i(chi) dchi;
    !
    ! where N is the number of PDF components.  The equation for mean rc in the
    ! ith PDF component is:
    !
    ! rc_i = INT(0:inf) chi * P_i(chi) dchi;
    !
    ! and the equation for cloud fraction in the ith PDF component is:
    ! 
    ! cloud_frac_i = INT(0:inf) P_i(chi) dchi.
    !
    ! The component values are related to the overall values by:
    !
    ! <rc> = SUM(i=1,N) mixt_frac_i * rc_i; and
    !
    ! cloud_frac = SUM(i=1,N) mixt_frac_i * cloud_frac_i.

    ! References:
    !-----------------------------------------------------------------------
    
      USE constants_clubb, ONLY: chi_tol, sqrt_2pi, sqrt_2, one, one_half, zero

      USE anl_erf, ONLY: erf

      USE clubb_precision, ONLY: core_rknd

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: &
      mean_chi_i,  & ! Mean of chi (old s) (ith PDF component)           [kg/kg]
      stdev_chi_i, & ! Standard deviation of chi (ith PDF component)     [kg/kg]
      chi_at_sat     ! Value of chi at saturation (0--liquid; neg.--ice) [kg/kg]

    ! Output Variables
    real( kind = core_rknd ), intent(out) :: &
      cloud_frac_i, & ! Cloud fraction (ith PDF component)               [-]
      rc_i            ! Mean cloud water mixing ratio (ith PDF comp.)    [kg/kg]

    ! Local Variables
    real( kind = core_rknd) :: zeta_i

    !----- Begin Code -----
    if ( stdev_chi_i > chi_tol ) then

       ! The value of chi varies in the ith PDF component.

       zeta_i = ( mean_chi_i - chi_at_sat ) / stdev_chi_i

       cloud_frac_i = one_half * ( one + erf( zeta_i / sqrt_2 )  )

       rc_i = ( mean_chi_i - chi_at_sat ) * cloud_frac_i &
              + stdev_chi_i * exp( - one_half * zeta_i**2 ) / ( sqrt_2pi )

    else ! stdev_chi_i <= chi_tol

       ! The value of chi does not vary in the ith PDF component.
       if ( ( mean_chi_i - chi_at_sat ) < zero ) then
          ! All clear air in the ith PDF component.
          cloud_frac_i = zero
          rc_i         = zero
       else ! mean_chi_i >= 0
          ! All cloud in the ith PDF component.
          cloud_frac_i = one
          rc_i         = mean_chi_i - chi_at_sat
       endif ! mean_chi_i < 0

    endif ! stdev_chi_i > chi_tol


    return
    
  end subroutine calc_cloud_frac_component

  !=============================================================================
  function calc_cloud_frac( cloud_frac_1, cloud_frac_2, mixt_frac )

  ! Description:
  !   Given the the two pdf components of a cloud fraction, and the weight
  !   of the first component, this fuction calculates the cloud fraction,
  !   cloud_frac
  !
  ! References:
  !-----------------------------------------------------------------------
    
      USE constants_clubb, ONLY: one, fstderr, zero_threshold
    
      USE clubb_precision, ONLY: core_rknd
      
      USE error_code, ONLY: clubb_at_least_debug_level
                                   !  at least the specified debug level 
      
    implicit none
    
    ! Input Variables
    real( kind = core_rknd ), intent(in) :: &
      cloud_frac_1, & ! First PDF component of cloud_frac
      cloud_frac_2, & ! Second PDF component of cloud_frac
      mixt_frac       ! Weight of 1st PDF component (Sk_w dependent)
    
    ! Output Variables
    real( kind = core_rknd) :: &
      calc_cloud_frac ! Cloud fraction
    
    ! Local Variables
    real( kind = core_rknd) :: &
      cloud_frac      ! Cloud fraction (used as a holding variable for
                      !                    output)

  !-----------------------------------------------------------------------
    !----- Begin Code -----
    cloud_frac = mixt_frac * cloud_frac_1 + (one-mixt_frac) * cloud_frac_2
    
    ! Note: Brian added the following lines to ensure that there
    ! are never any negative liquid water values (or any negative
    ! cloud fraction values, for that matter).  According to
    ! Vince Larson, the analytic formula should not produce any
    ! negative results, but such computer-induced errors such as
    ! round-off error may produce such a value.  This has been
    ! corrected because Brian found a small negative value of
    ! rcm in the first timestep of the FIRE case.

    cloud_frac  = max( zero_threshold, cloud_frac )
    if ( clubb_at_least_debug_level( 2 ) ) then
      if ( cloud_frac > one ) then
        write(fstderr,*) "Cloud fraction > 1"
      end if
    end if
    cloud_frac = min( one, cloud_frac )

    calc_cloud_frac = cloud_frac
    return
    
  end function calc_cloud_frac
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Description:
  !   This subroutine is similar to calc_cloud_frac_component, but
  !   resolves cloud_frac and rc at an arbitrary number of vertical levels
  !   in the vicinity of the desired level. This may give a better
  !   parameterization of sub-grid atmospheric conditions.
  !
  ! References:
  !-----------------------------------------------------------------------




    ! Local Constants
                         ! (arbitrary, but must be odd)

    ! Input Variables


    ! Output Variables

    ! Local Variables
      
  !-----------------------------------------------------------------------

    !----- Begin Code -----
    ! We could optionally compute chi_at_sat in an analogous manner. For now,
    ! use chi_at_sat(k) as an approximation.

    ! Compute cloud_frac and r_c at each refined grid level


  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------

  ! Description:
  !   Interpolates a variable to an array of values about a given level

  ! References
  !-----------------------------------------------------------------------



  ! Input Variables


  ! Output Variables

  ! Local Variables


                              ! two defined grid levels

  !-----------------------------------------------------------------------

    !----- Begin Code -----

    ! Place a point at each of k-1, k, and k+1.


    ! Lower half



    ! Upper half



  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------

  ! Description
  !   Returns the value of a variable at an integer height between 0 and
  !   nz+1 inclusive, using extrapolation when k==0 or k==nz+1

  ! References
  !-----------------------------------------------------------------------




    ! Input Variables


    ! Output Variables

    ! Local Variables
  !-----------------------------------------------------------------------

    !----- Begin Code -----

      ! This is the simple case. No extrapolation necessary.
      ! Extrapolate below the lower boundary
      ! Extrapolate above the upper boundary
      ! Invalid height requested
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------

  ! Description
  !   Interpolates (or extrapolates) a variable to a value between grid
  !   levels

  ! References
  !-----------------------------------------------------------------------




    ! Input Variables




    ! Output Variable

    ! Local Variables
  !----------------------------------------------------------------------

    !----- Begin Code -----







    ! Now perform the interpolation

  !-----------------------------------------------------------------------

end module pdf_closure_module
