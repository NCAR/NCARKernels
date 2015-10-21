
! KGEN-generated Fortran source file
!
! Filename    : pdf_closure_module.F90
! Generated at: 2015-10-20 14:27:09
! KGEN version: 0.5.3



    MODULE pdf_closure_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PUBLIC pdf_closure, calc_vert_avg_cf_component
        PRIVATE ! Set Default Scope
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!------------------------------------------------------------------------
!#######################################################################
!#######################################################################
! If you change the argument list of pdf_closure you also have to
! change the calls to this function in the host models CAM, WRF, SAM
! and GFDL.
!#######################################################################
!#######################################################################

        SUBROUTINE pdf_closure(hydromet_dim, p_in_pa, exner, thv_ds, wm, wp2, wp3, sigma_sqd_w, skw, rtm, rtp2, wprtp, thlm, &
        thlp2, wpthlp, rtpthlp, sclrm, wpsclrp, sclrp2, sclrprtp, sclrpthlp, level, wphydrometp, wp2hmp, rtphmp, thlphmp, wp4, &
        wprtp2, wp2rtp, wpthlp2, wp2thlp, wprtpthlp, cloud_frac, ice_supersat_frac, rcm, wpthvp, wp2thvp, rtpthvp, thlpthvp, &
        wprcp, wp2rcp, rtprcp, thlprcp, rcp2, pdf_params, err_code, wpsclrprtp, wpsclrp2, sclrpthvp, wpsclrpthlp, sclrprcp, &
        wp2sclrp, rc_coef)
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
            USE constants_clubb, ONLY: w_tol_sqd
            USE constants_clubb, ONLY: one_half
            USE constants_clubb, ONLY: one
            USE constants_clubb, ONLY: thl_tol
            USE constants_clubb, ONLY: zero_threshold
            USE constants_clubb, ONLY: rt_tol
            USE constants_clubb, ONLY: cp
            USE constants_clubb, ONLY: ep
            USE constants_clubb, ONLY: lv
            USE constants_clubb, ONLY: rd
            USE constants_clubb, ONLY: two
            USE constants_clubb, ONLY: chi_tol
            USE constants_clubb, ONLY: zero
            USE constants_clubb, ONLY: t_freeze_k
            USE constants_clubb, ONLY: ep2
            USE constants_clubb, ONLY: ep1
            USE constants_clubb, ONLY: fstderr ! Constants
! 2
! 1
! 1/2
! 0
! Dry air specific heat at constant p [J/kg/K]
! Latent heat of vaporization         [J/kg]
! Dry air gas constant                [J/kg/K]
! Rd / Rv;     ep  = 0.622            [-]
! (1.0-ep)/ep; ep1 = 0.61             [-]
! 1.0/ep;      ep2 = 1.61             [-]
! Tolerance for w'^2                  [m^2/s^2]
! Tolerance for r_t                   [kg/kg]
! Tolerance for th_l                  [K]
! Freezing point of water             [K]
            USE parameters_model, ONLY: sclr_dim
            USE parameters_model, ONLY: mixt_frac_max_mag
            USE parameters_model, ONLY: sclr_tol
! Array of passive scalar tolerances  [units vary]
! Number of passive scalar variables
! Maximum values for PDF parameter 'mixt_frac'
            USE parameters_tunable, ONLY: beta
! Variable(s)
! Plume widths for th_l and r_t [-]
            USE pdf_parameter_module, ONLY: pdf_parameter
! type
            USE array_index, ONLY: l_mix_rat_hm
! Variable(s)
! Procedure(s)
! The error function
            USE numerical_check, ONLY: pdf_closure_check
! Procedure(s)
            USE saturation, ONLY: sat_mixrat_liq
            USE saturation, ONLY: sat_mixrat_ice
! Procedure(s)
            USE error_code, ONLY: clubb_no_error
! Constant(s)
            USE error_code, ONLY: clubb_at_least_debug_level
            USE error_code, ONLY: fatal_error
! Procedure(s)
            USE stats_variables, ONLY: iwp4
            USE stats_variables, ONLY: iwprtp2
            USE stats_variables, ONLY: iwpthlp2
            USE stats_variables, ONLY: iwprtpthlp
! Variables
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
            INTRINSIC sqrt, exp, min, max, abs, present
! Input Variables
            INTEGER, intent(in) :: hydromet_dim
! Number of hydrometeor species              [#]
            REAL(KIND=core_rknd), intent(in) :: rtpthlp
            REAL(KIND=core_rknd), intent(in) :: wp3
            REAL(KIND=core_rknd), intent(in) :: p_in_pa
            REAL(KIND=core_rknd), intent(in) :: wprtp
            REAL(KIND=core_rknd), intent(in) :: sigma_sqd_w
            REAL(KIND=core_rknd), intent(in) :: rtm
            REAL(KIND=core_rknd), intent(in) :: exner
            REAL(KIND=core_rknd), intent(in) :: rtp2
            REAL(KIND=core_rknd), intent(in) :: skw
            REAL(KIND=core_rknd), intent(in) :: thv_ds
            REAL(KIND=core_rknd), intent(in) :: thlm
            REAL(KIND=core_rknd), intent(in) :: wm
            REAL(KIND=core_rknd), intent(in) :: thlp2
            REAL(KIND=core_rknd), intent(in) :: wp2
            REAL(KIND=core_rknd), intent(in) :: wpthlp
! Pressure                                   [Pa]
! Exner function                             [-]
! Dry, base-state theta_v (ref. th_l here)   [K]
! mean w-wind component (vertical velocity)  [m/s]
! w'^2                                       [m^2/s^2]
! w'^3                                       [m^3/s^3]
! Width of individual w plumes               [-]
! Skewness of w                              [-]
! Mean total water mixing ratio              [kg/kg]
! r_t'^2                                     [(kg/kg)^2]
! w'r_t'                                     [(kg/kg)(m/s)]
! Mean liquid water potential temperature    [K]
! th_l'^2                                    [K^2]
! w'th_l'                                    [K(m/s)]
! r_t'th_l'                                  [K(kg/kg)]
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrm
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: wpsclrp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrp2
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrprtp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrpthlp
! Mean passive scalar        [units vary]
! w' sclr'                   [units vary]
! sclr'^2                    [units vary]
! sclr' r_t'                 [units vary]
! sclr' th_l'                [units vary]
            INTEGER, intent(in) :: level
! Thermodynamic level for which calculations are taking place.
            REAL(KIND=core_rknd), dimension(hydromet_dim), intent(in) :: rtphmp
            REAL(KIND=core_rknd), dimension(hydromet_dim), intent(in) :: wphydrometp
            REAL(KIND=core_rknd), dimension(hydromet_dim), intent(in) :: wp2hmp
            REAL(KIND=core_rknd), dimension(hydromet_dim), intent(in) :: thlphmp
! Covariance of w and a hydrometeor    [(m/s) <hm units>]
! Third-order moment:  < w'^2 hm' >    [(m/s)^2 <hm units>]
! Covariance of rt and a hydrometeor   [(kg/kg) <hm units>]
! Covariance of thl and a hydrometeor  [K <hm units>]
! Output Variables
            REAL(KIND=core_rknd), intent(out) :: wp2thlp
            REAL(KIND=core_rknd), intent(out) :: cloud_frac
            REAL(KIND=core_rknd), intent(out) :: wprtpthlp
            REAL(KIND=core_rknd), intent(out) :: wpthlp2
            REAL(KIND=core_rknd), intent(out) :: ice_supersat_frac
            REAL(KIND=core_rknd), intent(out) :: rcm
            REAL(KIND=core_rknd), intent(out) :: wpthvp
            REAL(KIND=core_rknd), intent(out) :: wp2thvp
            REAL(KIND=core_rknd), intent(out) :: rtpthvp
            REAL(KIND=core_rknd), intent(out) :: thlpthvp
            REAL(KIND=core_rknd), intent(out) :: wprcp
            REAL(KIND=core_rknd), intent(out) :: wp2rcp
            REAL(KIND=core_rknd), intent(out) :: rtprcp
            REAL(KIND=core_rknd), intent(out) :: wp4
            REAL(KIND=core_rknd), intent(out) :: wprtp2
            REAL(KIND=core_rknd), intent(out) :: thlprcp
            REAL(KIND=core_rknd), intent(out) :: rcp2
            REAL(KIND=core_rknd), intent(out) :: wp2rtp
! w'^4                  [m^4/s^4]
! w' r_t'               [(m kg)/(s kg)]
! w'^2 r_t'             [(m^2 kg)/(s^2 kg)]
! w' th_l'^2            [(m K^2)/s]
! w'^2 th_l'            [(m^2 K)/s^2]
! Cloud fraction        [-]
! Ice cloud fracion     [-]
! Mean liquid water     [kg/kg]
! Buoyancy flux         [(K m)/s]
! w'^2 th_v'            [(m^2 K)/s^2]
! r_t' th_v'            [(kg K)/kg]
! th_l' th_v'           [K^2]
! w' r_c'               [(m kg)/(s kg)]
! w'^2 r_c'             [(m^2 kg)/(s^2 kg)]
! r_t' r_c'             [(kg^2)/(kg^2)]
! th_l' r_c'            [(K kg)/kg]
! r_c'^2                [(kg^2)/(kg^2)]
! w' r_t' th_l'         [(m kg K)/(s kg)]
            TYPE(pdf_parameter), intent(out) :: pdf_params
! pdf paramters         [units vary]
            INTEGER, intent(out) :: err_code
! Are the outputs usable numbers?
! Output (passive scalar variables)
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: wpsclrprtp
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: wpsclrpthlp
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: sclrprcp
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: wp2sclrp
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: sclrpthvp
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: wpsclrp2
! Local Variables
            REAL(KIND=core_rknd) :: w_1_n
            REAL(KIND=core_rknd) :: w_2_n
!     thl_1_n, thl_2_n,
!     rt_1_n, rt_2_n
! Variables that are stored in derived data type pdf_params.
            REAL(KIND=core_rknd) :: w_1
            REAL(KIND=core_rknd) :: w_2
            REAL(KIND=core_rknd) :: varnce_w_1
            REAL(KIND=core_rknd) :: varnce_w_2
            REAL(KIND=core_rknd) :: rt_1
            REAL(KIND=core_rknd) :: rt_2
            REAL(KIND=core_rknd) :: alpha_rt
            REAL(KIND=core_rknd) :: varnce_rt_1
            REAL(KIND=core_rknd) :: varnce_rt_2
            REAL(KIND=core_rknd) :: thl_1
            REAL(KIND=core_rknd) :: thl_2
            REAL(KIND=core_rknd) :: alpha_thl
            REAL(KIND=core_rknd) :: varnce_thl_1
            REAL(KIND=core_rknd) :: varnce_thl_2
            REAL(KIND=core_rknd) :: rrtthl
            REAL(KIND=core_rknd) :: crt_1
            REAL(KIND=core_rknd) :: crt_2
            REAL(KIND=core_rknd) :: cthl_1
            REAL(KIND=core_rknd) :: cthl_2
! Mean of w (1st PDF component)                       [m/s]
! Mean of w (2nd PDF component)                       [m/s]
! Variance of w (1st PDF component)               [m^2/s^2]
! Variance of w (2nd PDF component)               [m^2/s^2]
! Mean of r_t (1st PDF component)                   [kg/kg]
! Mean of r_t (2nd PDF component)                   [kg/kg]
! Variance of r_t (1st PDF component)           [kg^2/kg^2]
! Variance of r_t (2nd PDF component)           [kg^2/kg^2]
! Mean of th_l (1st PDF component)                      [K]
! Mean of th_l (2nd PDF component)                      [K]
! Variance of th_l (1st PDF component)                [K^2]
! Variance of th_l (2nd PDF component)                [K^2]
! Correlation of r_t and th_l (both components)         [-]
! Factor relating to normalized variance for th_l       [-]
! Factor relating to normalized variance for r_t        [-]
! Coef. on r_t in s/t eqns. (1st PDF comp.)             [-]
! Coef. on r_t in s/t eqns. (2nd PDF comp.)             [-]
! Coef. on th_l in s/t eqns. (1st PDF comp.)    [(kg/kg)/K]
! Coef. on th_l in s/t eqns. (2nd PDF comp.)    [(kg/kg)/K]
            REAL(KIND=core_rknd) :: mixt_frac
            REAL(KIND=core_rknd) :: rsatl_1
            REAL(KIND=core_rknd) :: rsatl_2
            REAL(KIND=core_rknd) :: chi_1
            REAL(KIND=core_rknd) :: chi_2
            REAL(KIND=core_rknd) :: stdev_chi_1
            REAL(KIND=core_rknd) :: stdev_chi_2
            REAL(KIND=core_rknd) :: stdev_eta_1
            REAL(KIND=core_rknd) :: stdev_eta_2
            REAL(KIND=core_rknd) :: covar_chi_eta_1
            REAL(KIND=core_rknd) :: covar_chi_eta_2
            REAL(KIND=core_rknd) :: corr_chi_eta_1
            REAL(KIND=core_rknd) :: corr_chi_eta_2
            REAL(KIND=core_rknd) :: rc_1
            REAL(KIND=core_rknd) :: cloud_frac_1
            REAL(KIND=core_rknd) :: cloud_frac_2
            REAL(KIND=core_rknd) :: rc_2
! Mean of chi (old s) (1st PDF component)       [kg/kg]
! Mean of chi (old s) (2nd PDF component)       [kg/kg]
! Standard deviation of chi (1st PDF component) [kg/kg]
! Standard deviation of chi (2nd PDF component) [kg/kg]
! Standard dev. of eta (old t) (1st PDF comp.)  [kg/kg]
! Standard dev. of eta (old t) (2nd PDF comp.)  [kg/kg]
! Covariance of chi and eta (1st PDF comp.) [kg^2/kg^2]
! Covariance of chi and eta (2nd PDF comp.) [kg^2/kg^2]
! Correlation of chi and eta (1st PDF component)    [-]
! Correlation of chi and eta (2nd PDF component)    [-]
! Mean of r_sl (1st PDF component)              [kg/kg]
! Mean of r_sl (2nd PDF component)              [kg/kg]
! Mean of r_c (1st PDF component)               [kg/kg]
! Mean of r_c (2nd PDF component)               [kg/kg]
! Cloud fraction (1st PDF component)                [-]
! Cloud fraction (2nd PDF component)                [-]
! Weight of 1st PDF component (Sk_w dependent)      [-]
! Note:  alpha coefficients = 0.5 * ( 1 - correlations^2 ).
!        These are used to calculate the scalar widths
!        varnce_thl_1, varnce_thl_2, varnce_rt_1, and varnce_rt_2 as in Eq. (34)
!        of Larson and Golaz (2005)
! Passive scalar local variables
            REAL(KIND=core_rknd), dimension(sclr_dim) :: sclr1
            REAL(KIND=core_rknd), dimension(sclr_dim) :: sclr2
            REAL(KIND=core_rknd), dimension(sclr_dim) :: varnce_sclr1
            REAL(KIND=core_rknd), dimension(sclr_dim) :: varnce_sclr2
            REAL(KIND=core_rknd), dimension(sclr_dim) :: alpha_sclr
            REAL(KIND=core_rknd), dimension(sclr_dim) :: rsclrrt
            REAL(KIND=core_rknd), dimension(sclr_dim) :: rsclrthl
!     sclr1_n, sclr2_n,
            LOGICAL :: l_scalar_calc
            LOGICAL :: l_calc_ice_supersat_frac
! True if sclr_dim > 0
! True if we should calculate ice_supersat_frac
! Quantities needed to predict higher order moments
            REAL(KIND=core_rknd) :: tl1
            REAL(KIND=core_rknd) :: tl2
            REAL(KIND=core_rknd) :: beta1
            REAL(KIND=core_rknd) :: beta2
            REAL(KIND=core_rknd) :: sqrt_wp2
! Thermodynamic quantity
            REAL(KIND=core_rknd), intent(out) :: rc_coef
            REAL(KIND=core_rknd) :: wp2rxp
            REAL(KIND=core_rknd) :: wprxp
            REAL(KIND=core_rknd) :: thlprxp
            REAL(KIND=core_rknd) :: rtprxp
! Sum total < w'^2 r_x' > for all hm species x [(m/s)^2(kg/kg)]
! Sum total < w'r_x' > for all hm species x      [(m/s)(kg/kg)]
! Sum total < th_l'r_x' > for all hm species x       [K(kg/kg)]
! Sum total < r_t'r_x' > for all hm species x       [(kg/kg)^2]
! variables for a generalization of Chris Golaz' closure
! varies width of plumes in theta_l, rt
            REAL(KIND=core_rknd) :: width_factor_1
            REAL(KIND=core_rknd) :: width_factor_2
! variables for computing ice cloud fraction
            REAL(KIND=core_rknd) :: rt_at_ice_sat1
            REAL(KIND=core_rknd) :: chi_at_ice_sat1
            REAL(KIND=core_rknd) :: rt_at_ice_sat2
            REAL(KIND=core_rknd) :: chi_at_ice_sat2
            REAL(KIND=core_rknd) :: rc_1_ice
            REAL(KIND=core_rknd) :: ice_supersat_frac1
            REAL(KIND=core_rknd) :: ice_supersat_frac2
            REAL(KIND=core_rknd) :: rc_2_ice
! first  pdf component of ice_supersat_frac
! second pdf component of ice_supersat_frac
            REAL(KIND=core_rknd), parameter :: chi_at_liq_sat  = 0.0_core_rknd
! Always zero
            LOGICAL, parameter :: l_liq_ice_loading_test = .false.
! Temp. flag liq./ice water loading test
            INTEGER :: i
            INTEGER :: hm_idx ! Indices
!------------------------ Code Begins ----------------------------------
! Check whether the passive scalars are present.
    if ( sclr_dim > 0 ) then
      l_scalar_calc = .true.
    else
      l_scalar_calc = .false.
    end if
    err_code = clubb_no_error ! Initialize to the value for no errors ! Initialize to the value for no errors
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
        end do ! 1..sclr_dim ! 1..sclr_dim
      end if
    else ! Width (standard deviation) parameters are non-zero ! Width (standard deviation) parameters are non-zero
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
      end if ! thlp2 <= thl_tol**2 ! thlp2 <= thl_tol**2
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
      end if ! rtp2 <= rt_tol**2 ! rtp2 <= rt_tol**2
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
          end if ! sclrp2(i) <= sclr_tol(i)**2 ! sclrp2(i) <= sclr_tol(i)**2
        end do ! i=1, sclr_dim ! i=1, sclr_dim
      end if ! l_scalar_calc ! l_scalar_calc
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
      end if ! varnce_rt_1*varnce_thl_1 > 0 .and. varnce_rt_2*varnce_thl_2 > 0 ! varnce_rt_1*varnce_thl_1 > 0 .and. varnce_rt_2*varnce_thl_2 > 0
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
        end do ! i=1, sclr_dim ! i=1, sclr_dim
      end if ! l_scalar_calc ! l_scalar_calc
    end if  ! Widths non-zero ! Widths non-zero
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
      end do ! i=1, sclr_dim ! i=1, sclr_dim
    end if ! l_scalar_calc ! l_scalar_calc
! Compute higher order moments that include theta_v.
! First compute some preliminary quantities.
! "1" denotes first Gaussian; "2" denotes 2nd Gaussian
! liq water temp (Sommeria & Deardorff 1977 (SD), eqn. 3)
    tl1  = thl_1*exner
    tl2  = thl_2*exner
    rsatl_1 = sat_mixrat_liq( p_in_Pa, tl1 )
    rsatl_2 = sat_mixrat_liq( p_in_Pa, tl2 ) ! h1g, 2010-06-16 end mod ! h1g, 2010-06-16 end mod
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
       enddo ! hm_idx = 1, hydromet_dim, 1 ! hm_idx = 1, hydromet_dim, 1
    endif ! l_liq_ice_loading_test ! l_liq_ice_loading_test
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
      end do ! i=1, sclr_dim ! i=1, sclr_dim
    end if ! l_scalar_calc ! l_scalar_calc
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
      end if ! Fatal error ! Fatal error
    end if ! clubb_at_least_debug_level ! clubb_at_least_debug_level
    return
        END SUBROUTINE pdf_closure
!=============================================================================

        elemental SUBROUTINE calc_cloud_frac_component(mean_chi_i, stdev_chi_i, chi_at_sat, cloud_frac_i, rc_i)
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
            USE constants_clubb, ONLY: chi_tol
            USE constants_clubb, ONLY: one_half
            USE constants_clubb, ONLY: sqrt_2
            USE constants_clubb, ONLY: one
            USE constants_clubb, ONLY: sqrt_2pi
            USE constants_clubb, ONLY: zero
! Tolerance for pdf parameter chi       [kg/kg]
! sqrt(2*pi)
! sqrt(2)
! 1
! 1/2
! 0
            USE anl_erf, ONLY: erf
! Procedure(s) -- The error function
            USE clubb_precision, ONLY: core_rknd
! Precision
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: mean_chi_i
            REAL(KIND=core_rknd), intent(in) :: stdev_chi_i
            REAL(KIND=core_rknd), intent(in) :: chi_at_sat
! Mean of chi (old s) (ith PDF component)           [kg/kg]
! Standard deviation of chi (ith PDF component)     [kg/kg]
! Value of chi at saturation (0--liquid; neg.--ice) [kg/kg]
! Output Variables
            REAL(KIND=core_rknd), intent(out) :: rc_i
            REAL(KIND=core_rknd), intent(out) :: cloud_frac_i
! Cloud fraction (ith PDF component)               [-]
! Mean cloud water mixing ratio (ith PDF comp.)    [kg/kg]
! Local Variables
            REAL(KIND=core_rknd) :: zeta_i
!----- Begin Code -----
    if ( stdev_chi_i > chi_tol ) then
! The value of chi varies in the ith PDF component.
       zeta_i = ( mean_chi_i - chi_at_sat ) / stdev_chi_i
       cloud_frac_i = one_half * ( one + erf( zeta_i / sqrt_2 )  )
       rc_i = ( mean_chi_i - chi_at_sat ) * cloud_frac_i &
              + stdev_chi_i * exp( - one_half * zeta_i**2 ) / ( sqrt_2pi )
    else ! stdev_chi_i <= chi_tol ! stdev_chi_i <= chi_tol
! The value of chi does not vary in the ith PDF component.
       if ( ( mean_chi_i - chi_at_sat ) < zero ) then
! All clear air in the ith PDF component.
          cloud_frac_i = zero
          rc_i         = zero
       else ! mean_chi_i >= 0 ! mean_chi_i >= 0
! All cloud in the ith PDF component.
          cloud_frac_i = one
          rc_i         = mean_chi_i - chi_at_sat
       endif ! mean_chi_i < 0 ! mean_chi_i < 0
    endif ! stdev_chi_i > chi_tol ! stdev_chi_i > chi_tol
    return
        END SUBROUTINE calc_cloud_frac_component
!=============================================================================

        FUNCTION calc_cloud_frac(cloud_frac_1, cloud_frac_2, mixt_frac)
! Description:
!   Given the the two pdf components of a cloud fraction, and the weight
!   of the first component, this fuction calculates the cloud fraction,
!   cloud_frac
!
! References:
!-----------------------------------------------------------------------
            USE constants_clubb, ONLY: one
            USE constants_clubb, ONLY: zero_threshold
            USE constants_clubb, ONLY: fstderr ! Constant(s)
! 1
! Standard error output
! A physical quantity equal to zero
            USE clubb_precision, ONLY: core_rknd
! Precision
            USE error_code, ONLY: clubb_at_least_debug_level
! Function to check whether clubb is in
!  at least the specified debug level
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: cloud_frac_1
            REAL(KIND=core_rknd), intent(in) :: cloud_frac_2
            REAL(KIND=core_rknd), intent(in) :: mixt_frac
! First PDF component of cloud_frac
! Second PDF component of cloud_frac
! Weight of 1st PDF component (Sk_w dependent)
! Output Variables
            REAL(KIND=core_rknd) :: calc_cloud_frac
! Cloud fraction
! Local Variables
            REAL(KIND=core_rknd) :: cloud_frac
! Cloud fraction (used as a holding variable for
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
        END FUNCTION calc_cloud_frac
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

        SUBROUTINE calc_vert_avg_cf_component(nz, k, z_vals, chi, stdev_chi, chi_at_sat, cloud_frac_i, rc_i)
! Description:
!   This subroutine is similar to calc_cloud_frac_component, but
!   resolves cloud_frac and rc at an arbitrary number of vertical levels
!   in the vicinity of the desired level. This may give a better
!   parameterization of sub-grid atmospheric conditions.
!
! References:
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
            IMPLICIT NONE
            INTRINSIC sum
! Local Constants
            INTEGER, parameter :: n_points = 9
! Number of vertical levels to use in averaging
! (arbitrary, but must be odd)
! Input Variables
            INTEGER, intent(in) :: nz
            INTEGER, intent(in) :: k
! Number of vertical levels                         [count]
! Level at which cloud_frac is to be computed       [count]
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: z_vals
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: chi
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: stdev_chi
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: chi_at_sat
! Height at each vertical level                   [m]
! Value of chi (old s)                            [kg/kg]
! Standard deviation of chi                       [kg/kg]
! Value of chi at saturation with respect to ice  [kg/kg]
! Output Variables
            REAL(KIND=core_rknd), intent(out) :: cloud_frac_i
            REAL(KIND=core_rknd), intent(out) :: rc_i
! Vertically averaged cloud fraction               [-]
! Vertically averaged cloud water mixing ratio     [kg/kg]
! Local Variables
            REAL(KIND=core_rknd), dimension(n_points) :: chi_ref
            REAL(KIND=core_rknd), dimension(n_points) :: stdev_chi_ref
            REAL(KIND=core_rknd), dimension(n_points) :: rc_ref
            REAL(KIND=core_rknd), dimension(n_points) :: cloud_frac_ref
! chi (old s) evaluated on refined grid     [kg/kg]
! stdev_chi evaluated on refined grid       [kg/kg]
! cloud_frac evaluated on refined grid      [-]
! r_c evaluated on refined grid             [kg/kg]
!-----------------------------------------------------------------------
!----- Begin Code -----
    chi_ref = interp_var_array( n_points, nz, k, z_vals, chi )
    stdev_chi_ref = interp_var_array( n_points, nz, k, z_vals, stdev_chi )
! We could optionally compute chi_at_sat in an analogous manner. For now,
! use chi_at_sat(k) as an approximation.
! Compute cloud_frac and r_c at each refined grid level
    call calc_cloud_frac_component( chi_ref(:), stdev_chi_ref(:), chi_at_sat(k), & ! Intent(in)
                                    cloud_frac_ref(:), rc_ref(:) )                  ! Intent(out) ! Intent(in)
! Intent(out)
    cloud_frac_i = sum( cloud_frac_ref(:) ) / real( n_points, kind=core_rknd )
    rc_i = sum( rc_ref(:) ) / real( n_points, kind=core_rknd )
    return
        END SUBROUTINE calc_vert_avg_cf_component
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

        FUNCTION interp_var_array(n_points, nz, k, z_vals, var)
! Description:
!   Interpolates a variable to an array of values about a given level
! References
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Constant
            IMPLICIT NONE
! Input Variables
            INTEGER, intent(in) :: n_points
            INTEGER, intent(in) :: nz
            INTEGER, intent(in) :: k
! Number of points to interpolate to (must be odd and >= 3)
! Total number of vertical levels
! Center of interpolation array
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: var
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: z_vals
! Height at each vertical level           [m]
! Variable values on grid                 [units vary]
! Output Variables
            REAL(KIND=core_rknd), dimension(n_points) :: interp_var_array
! Interpolated values of variable         [units vary]
! Local Variables
            REAL(KIND=core_rknd) :: dz
! Distance between vertical levels
            REAL(KIND=core_rknd) :: z_val
! Height at some sub-grid level
            INTEGER :: subgrid_lev_count
            INTEGER :: i
! Loop iterator
! Number of refined grid points located between
! two defined grid levels
!-----------------------------------------------------------------------
!----- Begin Code -----
! Place a point at each of k-1, k, and k+1.
    interp_var_array(1) = var_value_integer_height( nz, k-1, z_vals, var )
    interp_var_array((n_points+1)/2) = var_value_integer_height( nz, k, z_vals, var )
    interp_var_array(n_points) = var_value_integer_height( nz, k+1, z_vals, var )
    subgrid_lev_count = (n_points - 3) / 2
! Lower half
    if ( k == 1 ) then
      dz = (z_vals(2) - z_vals(1)) / real( subgrid_lev_count+1, kind=core_rknd )
    else
      dz = (z_vals(k) - z_vals(k-1)) / real( subgrid_lev_count+1, kind=core_rknd )
    end if
    do i=1, subgrid_lev_count
      z_val = z_vals(k) - real( i, kind=core_rknd ) * dz
      interp_var_array(1+i) &
      = var_subgrid_interp( nz, k, z_vals, var, z_val, l_below=.true. )
    end do
! Upper half
    if ( k == nz ) then
      dz = ( z_vals(nz) - z_vals(nz-1) ) / real( subgrid_lev_count+1, kind=core_rknd )
    else
      dz = ( z_vals(k+1) - z_vals(k) ) / real( subgrid_lev_count+1, kind=core_rknd )
    end if
    do i=1, (n_points-3)/2
      z_val = z_vals(k) + real( i, kind=core_rknd ) * dz
      interp_var_array((n_points+1)/2+i) &
      = var_subgrid_interp( nz, k, z_vals, var, z_val, l_below=.false. )
    end do
    return
        END FUNCTION interp_var_array
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

        FUNCTION var_value_integer_height(nz, k, z_vals, var_grid_value) RESULT ( var_value )
! Description
!   Returns the value of a variable at an integer height between 0 and
!   nz+1 inclusive, using extrapolation when k==0 or k==nz+1
! References
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Constant
            USE interpolation, ONLY: mono_cubic_interp
! Procedure
            IMPLICIT NONE
! Input Variables
            INTEGER, intent(in) :: nz
            INTEGER, intent(in) :: k
! Total number of vertical levels
! Level to resolve variable value
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: z_vals
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: var_grid_value
! Height at each vertical level                  [m]
! Value of variable at each grid level           [units vary]
! Output Variables
            REAL(KIND=core_rknd) :: var_value
! Value of variable at height level              [units vary]
! Local Variables
            INTEGER :: km1
            INTEGER :: k00
            INTEGER :: kp1
            INTEGER :: kp2
!-----------------------------------------------------------------------
!----- Begin Code -----
    if ( k >= 1 .and. k <= nz ) then
! This is the simple case. No extrapolation necessary.
      var_value = var_grid_value(k)
    else if ( k == 0 ) then
! Extrapolate below the lower boundary
      km1 = nz
      k00 = 1
      kp1 = 2
      kp2 = 3
      var_value = mono_cubic_interp( z_vals(1)-(z_vals(2)-z_vals(1)), &
                                     km1, k00, kp1, kp2, &
                                     z_vals(km1), z_vals(k00), z_vals(kp1), z_vals(kp2), &
                                     var_grid_value(km1), var_grid_value(k00), &
                                     var_grid_value(kp1), var_grid_value(kp2) )
    else if ( k == nz+1 ) then
! Extrapolate above the upper boundary
      km1 = nz
      k00 = nz-1
      kp1 = nz
      kp2 = nz
      var_value = mono_cubic_interp( z_vals(nz)+(z_vals(nz)-z_vals(nz-1)), &
                                     km1, k00, kp1, kp2, &
                                     z_vals(km1), z_vals(k00), z_vals(kp1), z_vals(kp2), &
                                     var_grid_value(km1), var_grid_value(k00), &
                                     var_grid_value(kp1), var_grid_value(kp2) )
    else
! Invalid height requested
      var_value = -999._core_rknd
    end if ! k > 1 .and. k < nz ! k > 1 .and. k < nz
    return
        END FUNCTION var_value_integer_height
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

        FUNCTION var_subgrid_interp(nz, k, z_vals, var, z_interp, l_below) RESULT ( var_value )
! Description
!   Interpolates (or extrapolates) a variable to a value between grid
!   levels
! References
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Constant
            USE interpolation, ONLY: mono_cubic_interp
! Procedure
            IMPLICIT NONE
! Input Variables
            INTEGER, intent(in) :: k
            INTEGER, intent(in) :: nz
! Number of vertical levels
! Grid level near interpolation target
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: z_vals
            REAL(KIND=core_rknd), dimension(nz), intent(in) :: var
! Height at each grid level          [m]
! Variable values at grid levels     [units vary]
            REAL(KIND=core_rknd), intent(in) :: z_interp
! Interpolation target height        [m]
            LOGICAL, intent(in) :: l_below
! True if z_interp < z_vals(k), false otherwise
! Output Variable
            REAL(KIND=core_rknd) :: var_value
! Interpolated value of variable     [units vary]
! Local Variables
            INTEGER :: km1
            INTEGER :: k00
            INTEGER :: kp1
            INTEGER :: kp2 ! Parameters for call to mono_cubic_interp
!----------------------------------------------------------------------
!----- Begin Code -----
    if ( l_below ) then
      if ( k == 1 ) then ! Extrapolation ! Extrapolation
        km1 = nz
        k00 = 1
        kp1 = 2
        kp2 = 3
      else if ( k == 2 ) then
        km1 = 1
        k00 = 1
        kp1 = 2
        kp2 = 3
      else if ( k == nz ) then
        km1 = nz-2
        k00 = nz-1
        kp1 = nz
        kp2 = nz
      else
        km1 = k-2
        k00 = k-1
        kp1 = k
        kp2 = k+1
      end if ! k == 1 ! k == 1
    else ! .not. l_below ! .not. l_below
      if ( k == 1 ) then
        km1 = 1
        k00 = 1
        kp1 = 2
        kp2 = 3
      else if ( k == nz-1 ) then
        km1 = nz-2
        k00 = nz-1
        kp1 = nz
        kp2 = nz
      else if ( k == nz ) then ! Extrapolation ! Extrapolation
        km1 = nz
        k00 = nz-1
        kp1 = nz
        kp2 = nz
      else
        km1 = k-1
        k00 = k
        kp1 = k+1
        kp2 = k+2
      end if ! k == 1 ! k == 1
    end if ! l_below ! l_below
! Now perform the interpolation
    var_value = mono_cubic_interp( z_interp, km1, k00, kp1, kp2, &
                                   z_vals(km1), z_vals(k00), z_vals(kp1), z_vals(kp2), &
                                   var(km1), var(k00), var(kp1), var(kp2) )
    return
        END FUNCTION var_subgrid_interp
!-----------------------------------------------------------------------
    END MODULE pdf_closure_module
