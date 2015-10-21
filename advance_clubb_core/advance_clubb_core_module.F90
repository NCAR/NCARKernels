
! KGEN-generated Fortran source file
!
! Filename    : advance_clubb_core_module.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE advance_clubb_core_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   The module containing the `core' of the CLUBB parameterization.
!   A host model implementing CLUBB should only require this subroutine
!   and the functions and subroutines it calls.
!
! References:
!  ``A PDF-Based Model for Boundary Layer Clouds. Part I:
!    Method and Model Description'' Golaz, et al. (2002)
!    JAS, Vol. 59, pp. 3540--3551.
!
!                         Copyright Notice:
!
!   This code and the source code it references are (C) 2006-2014
!   Jean-Christophe Golaz, Vincent E. Larson, Brian M. Griffin,
!   David P. Schanen, Adam J. Smith, and Michael J. Falk.
!
!   The distribution of this code and derived works thereof
!                   should include this notice.
!
!   Portions of this code derived from other sources (Hugh Morrison,
!   ACM TOMS, Numerical Recipes, et cetera) are the intellectual
!   property of their respective authors as noted and are also subject
!   to copyright.
!-----------------------------------------------------------------------
        IMPLICIT NONE
        PUBLIC advance_clubb_core, set_lscale_max
        PRIVATE ! Default Scope
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!-----------------------------------------------------------------------
!#######################################################################
!#######################################################################
! If you change the argument list of advance_clubb_core you also have to
! change the calls to this function in the host models CAM, WRF, SAM
! and GFDL.
!#######################################################################
!#######################################################################

        SUBROUTINE advance_clubb_core(l_implemented, dt, fcor, sfc_elevation, hydromet_dim, thlm_forcing, rtm_forcing, um_forcing,&
         vm_forcing, sclrm_forcing, edsclrm_forcing, wprtp_forcing, wpthlp_forcing, rtp2_forcing, thlp2_forcing, rtpthlp_forcing, &
        wm_zm, wm_zt, wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, wpsclrp_sfc, wpedsclrp_sfc, p_in_pa, rho_zm, rho, exner, &
        rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, rfrzm, radf, do_expldiff, &
        wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, host_dx, host_dy, um, vm, upwp, vpwp, up2, vp2, thlm, rtm, wprtp, wpthlp, wp2,&
         wp3, rtp2, thlp2, rtpthlp, sclrm, sclrp2, sclrprtp, sclrpthlp, wpsclrp, edsclrm, err_code, rcm, wprcp, cloud_frac, &
        ice_supersat_frac, rcm_in_layer, cloud_cover, khzm, khzt, qclvar, thlprcp_out, pdf_params)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! Description:
!   Subroutine to advance the model one timestep
! References:
!   ``A PDF-Based Model for Boundary Layer Clouds. Part I:
!     Method and Model Description'' Golaz, et al. (2002)
!   JAS, Vol. 59, pp. 3540--3551.
!-----------------------------------------------------------------------
! Modules to be included
            USE constants_clubb, ONLY: w_tol_sqd
            USE constants_clubb, ONLY: zero_threshold
            USE constants_clubb, ONLY: thl_tol
            USE constants_clubb, ONLY: rt_tol
            USE constants_clubb, ONLY: fstderr
            USE constants_clubb, ONLY: kappa
            USE constants_clubb, ONLY: p0
            USE constants_clubb, ONLY: zero
            USE constants_clubb, ONLY: cp
            USE constants_clubb, ONLY: ls
            USE constants_clubb, ONLY: lv
            USE constants_clubb, ONLY: ep1
            USE constants_clubb, ONLY: ep2
            USE constants_clubb, ONLY: three_halves
            USE constants_clubb, ONLY: unused_var
            USE constants_clubb, ONLY: em_min
            USE parameters_tunable, ONLY: mu
            USE parameters_tunable, ONLY: gamma_coef
            USE parameters_tunable, ONLY: gamma_coefb
            USE parameters_tunable, ONLY: gamma_coefc
            USE parameters_tunable, ONLY: lscale_pert_coef
            USE parameters_tunable, ONLY: lscale_mu_coef
            USE parameters_tunable, ONLY: taumax
            USE parameters_tunable, ONLY: c_k
            USE parameters_tunable, ONLY: c_k10
! Variable(s)
            USE parameters_model, ONLY: edsclr_dim
            USE parameters_model, ONLY: sclr_dim
            USE parameters_model, ONLY: rtm_min
            USE parameters_model, ONLY: rtm_nudge_max_altitude
            USE parameters_model, ONLY: ts_nudge
            USE parameters_model, ONLY: sclr_tol
! Variable(s)
            USE model_flags, ONLY: l_host_applies_sfc_fluxes
            USE model_flags, ONLY: l_gamma_skw
            USE model_flags, ONLY: l_rtm_nudge
            USE model_flags, ONLY: l_call_pdf_closure_twice
            USE model_flags, ONLY: l_trapezoidal_rule_zt
            USE model_flags, ONLY: l_trapezoidal_rule_zm
            USE model_flags, ONLY: l_use_cloud_cover
            USE model_flags, ONLY: l_tke_aniso
! Variable(s)
            USE grid_class, ONLY: gr
            USE grid_class, ONLY: zm2zt
            USE grid_class, ONLY: zt2zm
! Variable(s)
! Procedure(s)
            USE numerical_check, ONLY: parameterization_check
            USE numerical_check, ONLY: calculate_spurious_source
! Procedure(s)
            USE variables_diagnostic_module, ONLY: skw_zt
            USE variables_diagnostic_module, ONLY: skw_zm
            USE variables_diagnostic_module, ONLY: sigma_sqd_w_zt
            USE variables_diagnostic_module, ONLY: wp2thvp
            USE variables_diagnostic_module, ONLY: wpthlp2
            USE variables_diagnostic_module, ONLY: wp2thlp
            USE variables_diagnostic_module, ONLY: wprtpthlp
            USE variables_diagnostic_module, ONLY: wp2rcp
            USE variables_diagnostic_module, ONLY: wp2rtp
            USE variables_diagnostic_module, ONLY: wprtp2
            USE variables_diagnostic_module, ONLY: rtpthvp
            USE variables_diagnostic_module, ONLY: wp4
            USE variables_diagnostic_module, ONLY: pdf_params_zm
            USE variables_diagnostic_module, ONLY: wpthvp
            USE variables_diagnostic_module, ONLY: thlpthvp
            USE variables_diagnostic_module, ONLY: rtprcp
            USE variables_diagnostic_module, ONLY: thlprcp
            USE variables_diagnostic_module, ONLY: rcp2
            USE variables_diagnostic_module, ONLY: rsat
! Variable(s)
            USE variables_diagnostic_module, ONLY: thvm
            USE variables_diagnostic_module, ONLY: em
            USE variables_diagnostic_module, ONLY: lscale_up
            USE variables_diagnostic_module, ONLY: lscale_down
            USE variables_diagnostic_module, ONLY: lscale
            USE variables_diagnostic_module, ONLY: tau_zt
            USE variables_diagnostic_module, ONLY: tau_zm
            USE variables_diagnostic_module, ONLY: kh_zt
            USE variables_diagnostic_module, ONLY: kh_zm
            USE variables_diagnostic_module, ONLY: vm_ref
            USE variables_diagnostic_module, ONLY: ug
            USE variables_diagnostic_module, ONLY: vg
            USE variables_diagnostic_module, ONLY: um_ref
            USE variables_diagnostic_module, ONLY: wp2_zt
            USE variables_diagnostic_module, ONLY: thlp2_zt
            USE variables_diagnostic_module, ONLY: rtp2_zt
            USE variables_diagnostic_module, ONLY: rtpthlp_zt
            USE variables_diagnostic_module, ONLY: rtm_ref
            USE variables_diagnostic_module, ONLY: thlm_ref
            USE variables_diagnostic_module, ONLY: wpthlp_zt
            USE variables_diagnostic_module, ONLY: wprtp_zt
            USE variables_diagnostic_module, ONLY: up2_zt
            USE variables_diagnostic_module, ONLY: vp2_zt
            USE variables_diagnostic_module, ONLY: upwp_zt
            USE variables_diagnostic_module, ONLY: vpwp_zt
            USE variables_diagnostic_module, ONLY: wpedsclrp
            USE variables_diagnostic_module, ONLY: wp3_zm
            USE variables_diagnostic_module, ONLY: a3_coef
            USE variables_diagnostic_module, ONLY: a3_coef_zt
            USE variables_diagnostic_module, ONLY: skw_velocity
            USE variables_diagnostic_module, ONLY: wp2sclrp
            USE variables_diagnostic_module, ONLY: wpsclrprtp
            USE variables_diagnostic_module, ONLY: wpsclrp2
            USE variables_diagnostic_module, ONLY: wpsclrpthlp
            USE variables_diagnostic_module, ONLY: sclrpthvp
            USE variables_diagnostic_module, ONLY: sclrprcp
! sclr'th_v'
! sclr'rc'
! w'^2 sclr'
! w'sclr'^2
! w'sclr'rt'
! w'sclr'thl'
! wp3 interpolated to momentum levels
! Skewness velocity       [m/s]
! The a3 coefficient      [-]
! The a3 coefficient interp. to the zt grid [-]
            USE variables_diagnostic_module, ONLY: wp3_on_wp2_zt
            USE variables_diagnostic_module, ONLY: wp3_on_wp2
! Variable(s)
            USE pdf_parameter_module, ONLY: pdf_parameter
! Type
            USE advance_xm_wpxp_module, ONLY: advance_xm_wpxp
! Variable(s)
! Compute mean/flux terms
            USE advance_xp2_xpyp_module, ONLY: advance_xp2_xpyp
! Variable(s)
! Computes variance terms
            USE surface_varnce_module, ONLY: surface_varnce
! Procedure
            USE pdf_closure_module, ONLY: pdf_closure
            USE pdf_closure_module, ONLY: calc_vert_avg_cf_component
! Procedure
! Prob. density function
            USE mixing_length, ONLY: compute_length
! Procedure
            USE advance_windm_edsclrm_module, ONLY: advance_windm_edsclrm
! Procedure(s)
            USE saturation, ONLY: sat_mixrat_liq
! Procedure
! Saturation mixing ratio
            USE advance_wp2_wp3_module, ONLY: advance_wp2_wp3
! Procedure
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE error_code, ONLY: clubb_at_least_debug_level
            USE error_code, ONLY: fatal_error
            USE error_code, ONLY: report_error
! Procedure(s)
            USE skw_module, ONLY: skw_func
! Procedure
            USE clip_explicit, ONLY: clip_covars_denom
! Procedure(s)
            USE t_in_k_module, ONLY: thlm2t_in_k
! Read values from namelist
! Procedure
            USE stats_clubb_utilities, ONLY: stats_accumulate
! Procedure
            USE stats_type_utilities, ONLY: stat_update_var
            USE stats_type_utilities, ONLY: stat_begin_update
            USE stats_type_utilities, ONLY: stat_update_var_pt
            USE stats_type_utilities, ONLY: stat_begin_update_pt
            USE stats_type_utilities, ONLY: stat_end_update_pt
            USE stats_type_utilities, ONLY: stat_end_update
! Procedure(s)
            USE stats_variables, ONLY: iwp2_bt
            USE stats_variables, ONLY: ivp2_bt
            USE stats_variables, ONLY: iup2_bt
            USE stats_variables, ONLY: iwprtp_bt
            USE stats_variables, ONLY: iwpthlp_bt
            USE stats_variables, ONLY: irtp2_bt
            USE stats_variables, ONLY: ithlp2_bt
            USE stats_variables, ONLY: irtpthlp_bt
            USE stats_variables, ONLY: irtm_bt
            USE stats_variables, ONLY: ithlm_bt
            USE stats_variables, ONLY: ium_bt
            USE stats_variables, ONLY: ivm_bt
            USE stats_variables, ONLY: iwp3_bt
            USE stats_variables, ONLY: iskw_zt
            USE stats_variables, ONLY: iskw_zm
            USE stats_variables, ONLY: iwp4
            USE stats_variables, ONLY: irsat
            USE stats_variables, ONLY: irel_humidity
            USE stats_variables, ONLY: irvm
            USE stats_variables, ONLY: iwpthlp_zt
! Variable(s)
            USE stats_variables, ONLY: l_stats
            USE stats_variables, ONLY: l_stats_samp
            USE stats_variables, ONLY: stats_zt
            USE stats_variables, ONLY: stats_zm
            USE stats_variables, ONLY: ithlp2_sf
            USE stats_variables, ONLY: irtp2_sf
            USE stats_variables, ONLY: irtpthlp_sf
            USE stats_variables, ONLY: iup2_sf
            USE stats_variables, ONLY: ivp2_sf
            USE stats_variables, ONLY: iwp2_sf
            USE stats_variables, ONLY: iwprtp_zt
            USE stats_variables, ONLY: iup2_zt
            USE stats_variables, ONLY: ivp2_zt
            USE stats_variables, ONLY: iupwp_zt
            USE stats_variables, ONLY: ivpwp_zt
            USE stats_variables, ONLY: irtm_spur_src
            USE stats_variables, ONLY: stats_sfc
            USE stats_variables, ONLY: ithlm_spur_src
            USE stats_variables, ONLY: irfrzm
            USE stats_variables, ONLY: icloud_frac_refined
            USE stats_variables, ONLY: ircm_refined
            USE stats_variables, ONLY: istability_correction
! Variable(s)
            USE stats_variables, ONLY: igamma_skw_fnc
            USE stats_variables, ONLY: iskw_velocity
            USE stats_variables, ONLY: ilscale_pert_1
            USE stats_variables, ONLY: ilscale_pert_2
! Variable(s)
            USE fill_holes, ONLY: vertical_integral
            USE fill_holes, ONLY: fill_holes_vertical
! Procedure(s)
            USE sigma_sqd_w_module, ONLY: compute_sigma_sqd_w
! Procedure(s)
            USE array_index, ONLY: iirrm
! Variable
            USE pdf_utilities, ONLY: compute_mean_binormal
            USE advance_helper_module, ONLY: calc_stability_correction
! Procedure(s)
            USE interpolation, ONLY: pvertinterp
            IMPLICIT NONE
!!! External
            INTRINSIC sqrt, min, max, exp, mod, real
! Constant Parameters
            LOGICAL, parameter :: l_avg_lscale = .false. ! Lscale is calculated in subroutine compute_length; if l_avg_Lscale
! is true, compute_length is called two additional times with
! perturbed values of rtm and thlm.  An average value of Lscale
! from the three calls to compute_length is then calculated.
! This reduces temporal noise in RICO, BOMEX, LBA, and other cases.
            LOGICAL, parameter :: l_lscale_plume_centered = .false.
! Alternate that uses the PDF to
! compute the perturbed values
            LOGICAL, parameter :: l_use_ice_latent = .false.
!Includes the effects of ice latent heating in turbulence terms
            LOGICAL, parameter :: l_iter_xp2_xpyp = .true.
! Set to true when rtp2/thlp2/rtpthlp, et cetera are prognostic
            LOGICAL, parameter :: l_refine_grid_in_cloud = .false.
            LOGICAL, parameter :: l_interactive_refined  = .false.
! Compute cloud_frac and rcm on a refined grid
! Should the refined grid code feed into the model?
! Only has meaning if l_refined_grid_in_cloud is .true.
            REAL(KIND=core_rknd), parameter :: chi_at_liq_sat = 0._core_rknd
! Value of chi(s) at saturation with respect to ice
! (zero for liquid)
            LOGICAL, parameter :: l_stability_correct_tau_zm = .true.
! Use tau_N2_zm instead of tau_zm in wpxp_pr1
!!! Input Variables
            LOGICAL, intent(in) :: l_implemented
! Is this part of a larger host model (T/F) ?
            REAL(KIND=core_rknd), intent(in) :: dt
! Current timestep duration    [s]
            REAL(KIND=core_rknd), intent(in) :: sfc_elevation
            REAL(KIND=core_rknd), intent(in) :: fcor
! Coriolis forcing             [s^-1]
! Elevation of ground level    [m AMSL]
            INTEGER, intent(in) :: hydromet_dim
! Total number of hydrometeors          [#]
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: p_in_pa
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thv_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: exner
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: invrs_rho_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wprtp_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtp2_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlp2_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rfrzm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wpthlp_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtpthlp_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: invrs_rho_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: um_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thv_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zm
! theta_l forcing (thermodynamic levels)    [K/s]
! r_t forcing (thermodynamic levels)        [(kg/kg)/s]
! u wind forcing (thermodynamic levels)     [m/s/s]
! v wind forcing (thermodynamic levels)     [m/s/s]
! <w'r_t'> forcing (momentum levels)    [m*K/s^2]
! <w'th_l'> forcing (momentum levels)   [m*(kg/kg)/s^2]
! <r_t'^2> forcing (momentum levels)    [(kg/kg)^2/s]
! <th_l'^2> forcing (momentum levels)   [K^2/s]
! <r_t'th_l'> forcing (momentum levels) [K*(kg/kg)/s]
! w mean wind component on momentum levels  [m/s]
! w mean wind component on thermo. levels   [m/s]
! Air pressure (thermodynamic levels)       [Pa]
! Air density on momentum levels            [kg/m^3]
! Air density on thermodynamic levels       [kg/m^3]
! Exner function (thermodynamic levels)     [-]
! Dry, static density on momentum levels    [kg/m^3]
! Dry, static density on thermo. levels     [kg/m^3]
! Inv. dry, static density @ momentum levs. [m^3/kg]
! Inv. dry, static density @ thermo. levs.  [m^3/kg]
! Dry, base-state theta_v on momentum levs. [K]
! Dry, base-state theta_v on thermo. levs.  [K]
! Total ice-phase water mixing ratio        [kg/kg]
            LOGICAL, intent(in) :: do_expldiff
            REAL(KIND=core_rknd), dimension(gr%nz,hydromet_dim), intent(in) :: hydromet
! Collection of hydrometeors                [units vary]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: radf
! Buoyancy production at the CL top due to LW radiative cooling [m^2/s^3]
            REAL(KIND=core_rknd), dimension(gr%nz, hydromet_dim), intent(in) :: thlphmp_zt
            REAL(KIND=core_rknd), dimension(gr%nz, hydromet_dim), intent(in) :: wphydrometp
            REAL(KIND=core_rknd), dimension(gr%nz, hydromet_dim), intent(in) :: wp2hmp
            REAL(KIND=core_rknd), dimension(gr%nz, hydromet_dim), intent(in) :: rtphmp_zt
! Covariance of w and a hydrometeor      [(m/s) <hm units>]
! Third-order moment:  < w'^2 hm' >    [(m/s)^2 <hm units>]
! Covariance of rt and hm (on t-levs.) [(kg/kg) <hm units>]
! Covariance of thl and hm (on t-levs.)      [K <hm units>]
            REAL(KIND=core_rknd), intent(in) :: wpthlp_sfc
            REAL(KIND=core_rknd), intent(in) :: wprtp_sfc
            REAL(KIND=core_rknd), intent(in) :: upwp_sfc
            REAL(KIND=core_rknd), intent(in) :: vpwp_sfc
! w' theta_l' at surface   [(m K)/s]
! w' r_t' at surface       [(kg m)/( kg s)]
! u'w' at surface          [m^2/s^2]
! v'w' at surface          [m^2/s^2]
! Passive scalar variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrm_forcing
! Passive scalar forcing         [{units vary}/s]
            REAL(KIND=core_rknd), intent(in), dimension(sclr_dim) :: wpsclrp_sfc
! Scalar flux at surface         [{units vary} m/s]
! Eddy passive scalar variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,edsclr_dim) :: edsclrm_forcing
! Eddy passive scalar forcing    [{units vary}/s]
            REAL(KIND=core_rknd), intent(in), dimension(edsclr_dim) :: wpedsclrp_sfc
! Eddy-Scalar flux at surface    [{units vary} m/s]
! Host model horizontal grid spacing, if part of host model.
            REAL(KIND=core_rknd), intent(in) :: host_dx
            REAL(KIND=core_rknd), intent(in) :: host_dy
! East-West horizontal grid spacing     [m]
! North-South horizontal grid spacing   [m]
!!! Input/Output Variables
! These are prognostic or are planned to be in the future
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: wpthlp
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: um
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: vm
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: vpwp
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: wp2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: thlm
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: thlp2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: rtpthlp
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: wp3
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: rtm
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: rtp2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: up2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: wprtp
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: vp2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: upwp
! u mean wind component (thermodynamic levels)   [m/s]
! u'w' (momentum levels)                         [m^2/s^2]
! v mean wind component (thermodynamic levels)   [m/s]
! v'w' (momentum levels)                         [m^2/s^2]
! u'^2 (momentum levels)                         [m^2/s^2]
! v'^2 (momentum levels)                         [m^2/s^2]
! total water mixing ratio, r_t (thermo. levels) [kg/kg]
! w' r_t' (momentum levels)                      [(kg/kg) m/s]
! liq. water pot. temp., th_l (thermo. levels)   [K]
! w' th_l' (momentum levels)                     [(m/s) K]
! r_t'^2 (momentum levels)                       [(kg/kg)^2]
! th_l'^2 (momentum levels)                      [K^2]
! r_t' th_l' (momentum levels)                   [(kg/kg) K]
! w'^2 (momentum levels)                         [m^2/s^2]
! w'^3 (thermodynamic levels)                    [m^3/s^3]
! Passive scalar variables
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz,sclr_dim) :: sclrp2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz,sclr_dim) :: sclrpthlp
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz,sclr_dim) :: wpsclrp
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz,sclr_dim) :: sclrm
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz,sclr_dim) :: sclrprtp
! Passive scalar mean (thermo. levels) [units vary]
! w'sclr' (momentum levels)            [{units vary} m/s]
! sclr'^2 (momentum levels)            [{units vary}^2]
! sclr'rt' (momentum levels)           [{units vary} (kg/kg)]
! sclr'thl' (momentum levels)          [{units vary} K]
! Eddy passive scalar variable
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz,edsclr_dim) :: edsclrm
! Eddy passive scalar mean (thermo. levels)   [units vary]
! Variables that need to be output for use in other parts of the CLUBB
! code, such as microphysics (rcm, pdf_params), forcings (rcm), and/or
! BUGSrad (cloud_cover).
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: rcm_in_layer
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: rcm
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: cloud_cover
! cloud water mixing ratio, r_c (thermo. levels)  [kg/kg]
! rcm in cloud layer                              [kg/kg]
! cloud cover                                     [-]
            TYPE(pdf_parameter), dimension(gr%nz), intent(out) :: pdf_params
! PDF parameters   [units vary]
! Variables that need to be output for use in host models
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: cloud_frac
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: wprcp
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: ice_supersat_frac
! w'r_c' (momentum levels)                  [(kg/kg) m/s]
! cloud fraction (thermodynamic levels)     [-]
! ice cloud fraction (thermodynamic levels) [-]
! Eric Raut declared this variable solely for output to disk
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_coef
! Coefficient of X' R_l' in Eq. (34)        [-]
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: khzm
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: khzt
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: thlprcp_out
! eddy diffusivity on thermo levels
! eddy diffusivity on momentum levels
            REAL(KIND=core_rknd), intent(out), dimension(gr%nz) :: qclvar
! cloud water variance
            REAL(KIND=core_rknd), dimension(gr%nz) :: km_zm
            REAL(KIND=core_rknd) :: newmu
!!! Output Variable
! Diagnostic, for if some calculation goes amiss.
            INTEGER, intent(inout) :: err_code
!!! Local Variables
            INTEGER :: k
            INTEGER :: i
            INTEGER :: err_code_pdf_closure
            INTEGER :: err_code_surface
            INTEGER :: ixind
            REAL(KIND=core_rknd), dimension(gr%nz) :: gamma_skw_fnc
            REAL(KIND=core_rknd), dimension(gr%nz) :: sigma_sqd_w
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_pert_1
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_pert_1
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_pert_2
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_pert_2
            REAL(KIND=core_rknd), dimension(gr%nz) :: lscale_pert_1
            REAL(KIND=core_rknd), dimension(gr%nz) :: lscale_pert_2
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_pert_pos_rt
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_pert_pos_rt
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_pert_neg_rt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_pert_neg_rt
            REAL(KIND=core_rknd), dimension(gr%nz) :: sqrt_em_zt
! PDF width parameter (momentum levels)    [-]
! sqrt( em ) on zt levels; where em is TKE [m/s]
! Gamma as a function of skewness          [???]
! For avg. calculation of Lscale  [m]
! For avg. calculation of Lscale  [K]
! For avg. calculation of Lscale  [kg/kg]
! For avg. calculation of Lscale  [K]
! For avg. calculation of Lscale  [kg/kg]
!Lscale_weight Uncomment this if you need to use this vairable at some point.
! For pdf_closure
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrp_zt
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrp2_zt
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrprtp_zt
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrpthlp_zt
! w' sclr' on thermo. levels
! sclr'^2 on thermo. levels
! sclr' r_t' on thermo. levels
! sclr' th_l' on thermo. levels
            REAL(KIND=core_rknd), dimension(gr%nz) :: p_in_pa_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: exner_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: w_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: w_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_w_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_w_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: mixt_frac_zm
! Pressure interpolated to momentum levels  [Pa]
! Exner interpolated to momentum levels     [-]
! Mean w (1st PDF component)                   [m/s]
! Mean w (2nd PDF component)                   [m/s]
! Variance of w (1st PDF component)            [m^2/s^2]
! Variance of w (2nd PDF component)            [m^2/s^2]
! Weight of 1st PDF component (Sk_w dependent) [-]
            REAL(KIND=core_rknd), dimension(gr%nz,hydromet_dim) :: wphydrometp_zt
            REAL(KIND=core_rknd), dimension(gr%nz,hydromet_dim) :: rtphmp
            REAL(KIND=core_rknd), dimension(gr%nz,hydromet_dim) :: thlphmp
            REAL(KIND=core_rknd), dimension(gr%nz,hydromet_dim) :: wp2hmp_zm
! Covariance of w and hm (on t-levs.) [(m/s) <hm units>]
! Moment <w'^2 hm'> (on m-levs.)    [(m/s)^2 <hm units>]
! Covariance of rt and hm           [(kg/kg) <hm units>]
! Covariance of thl and hm                [K <hm units>]
            INTEGER :: wprtp_cl_num
            INTEGER :: wpthlp_cl_num
            INTEGER :: wpsclrp_cl_num
            INTEGER :: upwp_cl_num
            INTEGER :: vpwp_cl_num
! Instance of w'r_t' clipping (1st or 3rd).
! Instance of w'th_l' clipping (1st or 3rd).
! Instance of w'sclr' clipping (1st or 3rd).
! Instance of u'w' clipping (1st or 2nd).
! Instance of v'w' clipping (1st or 2nd).
! These local variables are declared because they originally belong on the momentum
! grid levels, but pdf_closure outputs them on the thermodynamic grid levels.
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtpthvp_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlpthvp_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprcp_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtprcp_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlprcp_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthvp_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_coef_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcp2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp4_zt
! w'^4 (on thermo. grid)           [m^4/s^4]
! Buoyancy flux (on thermo. grid)  [(K m)/s]
! r_t' th_v' (on thermo. grid)     [(kg K)/kg]
! th_l' th_v' (on thermo. grid)    [K^2]
! w' r_c' (on thermo. grid)        [(m kg)/(s kg)]
! r_t' r_c' (on thermo. grid)      [(kg^2)/(kg^2)]
! th_l' r_c' (on thermo. grid)     [(K kg)/kg]
! r_c'^2 (on thermo. grid)         [(kg^2)/(kg^2)]
! X'R_l' coef. (on thermo. grid)   [-]
            REAL(KIND=core_rknd), dimension(gr%nz, sclr_dim) :: sclrpthvp_zt
            REAL(KIND=core_rknd), dimension(gr%nz, sclr_dim) :: sclrprcp_zt
! sclr'th_v' (on thermo. grid)
! sclr'rc' (on thermo. grid)
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtpthlp_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: ice_supersat_frac_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcm_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2rtp_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtp2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2thlp_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2thvp_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2rcp_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthlp2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: sign_rtpthlp
! w'rt'^2 on momentum grid                   [m kg^2/kg^2]
! w'^2 rt' on momentum grid                  [m^2 kg/kg]
! w'thl'^2 on momentum grid                  [m K^2/s]
! w'^2 thl' on momentum grid                 [m^2 K/s^2]
! w'rt'thl' on momentum grid                 [m kg K/kg s]
! Cloud Fraction on momentum grid            [-]
! Ice Cloud Fraction on momentum grid        [-]
! Total water mixing ratio                   [kg/kg]
! Liquid potential temperature               [kg/kg]
! Liquid water mixing ratio on momentum grid [kg/kg]
! w'^2 th_v' on momentum grid                [m^2 K/s^2]
! w'^2 rc' on momentum grid                  [m^2 kg/kg s^2]
! sign of the covariance rtpthlp             [-]
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrm_zm
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrprtp_zm
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrp2_zm
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrpthlp_zm
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wp2sclrp_zm
! w'sclr'rt' on momentum grid
! w'sclr'^2 on momentum grid
! w'sclr'thl' on momentum grid
! w'^2 sclr' on momentum grid
! Passive scalar mean on momentum grid
            REAL(KIND=core_rknd) :: rtm_integral_before
            REAL(KIND=core_rknd) :: thlm_integral_before
            REAL(KIND=core_rknd) :: mu_pert_1
            REAL(KIND=core_rknd) :: mu_pert_2
            REAL(KIND=core_rknd) :: mu_pert_pos_rt
            REAL(KIND=core_rknd) :: mu_pert_neg_rt
            REAL(KIND=core_rknd) :: rtm_flux_top
            REAL(KIND=core_rknd) :: rtm_flux_sfc
            REAL(KIND=core_rknd) :: rtm_integral_after
            REAL(KIND=core_rknd) :: rtm_integral_forcing
            REAL(KIND=core_rknd) :: rtm_spur_src
            REAL(KIND=core_rknd) :: thlm_flux_top
            REAL(KIND=core_rknd) :: thlm_flux_sfc
            REAL(KIND=core_rknd) :: thlm_integral_after
            REAL(KIND=core_rknd) :: thlm_integral_forcing
            REAL(KIND=core_rknd) :: thlm_spur_src
! For l_avg_Lscale
! For l_Lscale_plume_centered
!The following variables are defined for use when l_use_ice_latent = .true.
            TYPE(pdf_parameter), dimension(gr%nz) :: pdf_params_frz
            TYPE(pdf_parameter), dimension(gr%nz) :: pdf_params_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthvp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthlp2_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: ice_supersat_frac_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtp2_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp4_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2thvp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtpthvp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlpthvp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2rtp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2thlp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtpthlp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprcp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2rcp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2thvp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtpthvp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlpthvp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthvp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_coef_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlprcp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcp2_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtprcp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtm_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlm_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprcp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2rcp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rtprcp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2thlp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtpthlp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcm_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_coef_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2rtp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: thlprcp_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthlp2_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcp2_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: ice_supersat_frac_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtp2_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp4_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrp2_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrprtp_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrpthvp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrpthlp_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrprcp_zt_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wp2sclrp_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrprcp_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrp2_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrpthlp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: sclrpthvp_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wp2sclrp_zm_frz
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrprtp_zm_frz
            REAL(KIND=core_rknd) :: rc_1_refined
            REAL(KIND=core_rknd) :: cloud_frac_1_refined
            REAL(KIND=core_rknd) :: cloud_frac_2_refined
            REAL(KIND=core_rknd) :: rc_2_refined
            REAL(KIND=core_rknd) :: cloud_frac_refined
            REAL(KIND=core_rknd) :: rcm_refined
            REAL(KIND=core_rknd) :: thlm700
            REAL(KIND=core_rknd) :: thlm1000
! cloud_frac_1 computed on refined grid
! cloud_frac_2 computed on refined grid
! rc_1 computed on refined grid
! rc_2 computed on refined grid
! cloud_frac gridbox mean on refined grid
! rcm gridbox mean on refined grid
            REAL(KIND=core_rknd), dimension(gr%nz) :: rrm
! Rain water mixing ratio
            REAL(KIND=core_rknd), dimension(gr%nz) :: tau_n2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: stability_correction
            REAL(KIND=core_rknd), dimension(gr%nz) :: tau_c6_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: tau_c1_zm
! Stability correction factor
! Tau with a static stability correction applied to it [s]
! Tau values used for the C6 (pr1) term in wpxp [s]
! Tau values used for the C1 (dp1) term in wp2 [s]
            REAL(KIND=core_rknd) :: lscale_max
!----- Begin Code -----
! Determine the maximum allowable value for Lscale (in meters).
    call set_Lscale_max( l_implemented, host_dx, host_dy, & ! intent(in)
                         Lscale_max )                       ! intent(out) ! intent(in)
! intent(out)
    if ( l_stats .and. l_stats_samp ) then
! Spurious source will only be calculated if rtm_ma and thlm_ma are zero.
! Therefore, wm must be zero or l_implemented must be true.
      if ( l_implemented .or. ( all( wm_zt == 0._core_rknd ) .and. &
           all( wm_zm == 0._core_rknd ) ) ) then
! Get the vertical integral of rtm and thlm before this function begins
! so that spurious source can be calculated
        rtm_integral_before  &
        = vertical_integral( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                             rtm(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
        thlm_integral_before  &
        = vertical_integral( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                             thlm(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
      end if
    end if
!----------------------------------------------------------------
! Test input variables
!----------------------------------------------------------------
    if ( clubb_at_least_debug_level( 2 ) ) then
      call parameterization_check & 
           ( thlm_forcing, rtm_forcing, um_forcing, vm_forcing, & ! intent(in)
             wm_zm, wm_zt, p_in_Pa, rho_zm, rho, exner,         & ! intent(in)
             rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm,             & ! intent(in)
             invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt,             & ! intent(in)
             wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc,         & ! intent(in)
             um, upwp, vm, vpwp, up2, vp2,                      & ! intent(in)
             rtm, wprtp, thlm, wpthlp,                          & ! intent(in)
             wp2, wp3, rtp2, thlp2, rtpthlp,                    & ! intent(in)
             "beginning of ",        & ! intent(in)
             wpsclrp_sfc, wpedsclrp_sfc,                        & ! intent(in)
             sclrm, wpsclrp, sclrp2, sclrprtp, sclrpthlp,       & ! intent(in)
             sclrm_forcing, edsclrm, edsclrm_forcing,           & ! intent(in)
             err_code )                                           ! intent(inout)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
    end if
!-----------------------------------------------------------------------
    if ( l_stats_samp ) then
      call stat_update_var( irfrzm, rfrzm, & ! intent(in)
                            stats_zt ) ! intent(inout) ! intent(in)
! intent(inout)
    end if
! Set up budget stats variables.
    if ( l_stats_samp ) then
      call stat_begin_update( iwp2_bt, wp2 / dt, &                  ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( ivp2_bt, vp2 / dt, &                  ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( iup2_bt, up2 / dt,  &                 ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( iwprtp_bt, wprtp / dt, &              ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( iwpthlp_bt, wpthlp / dt,  &           ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( irtp2_bt, rtp2 / dt, &                ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( ithlp2_bt, thlp2 / dt, &              ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( irtpthlp_bt, rtpthlp / dt, &          ! intent(in)
                              stats_zm )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( irtm_bt, rtm / dt, &                  ! intent(in)
                              stats_zt )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( ithlm_bt, thlm / dt, &                ! intent(in)
                              stats_zt )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( ium_bt, um / dt, &                    ! intent(in)
                              stats_zt )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( ivm_bt, vm / dt, &                    ! intent(in)
                              stats_zt )                                  ! intent(inout) ! intent(in)
! intent(inout)
      call stat_begin_update( iwp3_bt, wp3 / dt, &                  ! intent(in)
                              stats_zt )                                  ! intent(inout) ! intent(in)
! intent(inout)
    end if
! SET SURFACE VALUES OF FLUXES (BROUGHT IN)
! We only do this for host models that do not apply the flux
! elsewhere in the code (e.g. WRF).  In other cases the _sfc variables will
! only be used to compute the variance at the surface. -dschanen 8 Sept 2009
    if ( .not. l_host_applies_sfc_fluxes ) then
      wpthlp(1) = wpthlp_sfc
      wprtp(1)  = wprtp_sfc
      upwp(1)   = upwp_sfc
      vpwp(1)   = vpwp_sfc
! Set fluxes for passive scalars (if enabled)
      if ( sclr_dim > 0 ) then
        wpsclrp(1,1:sclr_dim)   = wpsclrp_sfc(1:sclr_dim)
      end if
      if ( edsclr_dim > 0 ) then
        wpedsclrp(1,1:edsclr_dim) = wpedsclrp_sfc(1:edsclr_dim)
      end if
    else
      wpthlp(1) = 0.0_core_rknd
      wprtp(1)  = 0.0_core_rknd
      upwp(1)   = 0.0_core_rknd
      vpwp(1)   = 0.0_core_rknd
! Set fluxes for passive scalars (if enabled)
      if ( sclr_dim > 0 ) then
        wpsclrp(1,1:sclr_dim) = 0.0_core_rknd
      end if
      if ( edsclr_dim > 0 ) then
        wpedsclrp(1,1:edsclr_dim) = 0.0_core_rknd
      end if
    end if ! ~l_host_applies_sfc_fluxes ! ~l_host_applies_sfc_fluxes
    newmu = mu
!---------------------------------------------------------------------------
! Interpolate wp3 to momentum levels, and wp2 to thermodynamic levels
! and then compute Skw for m & t grid
!---------------------------------------------------------------------------
    wp2_zt = max( zm2zt( wp2 ), w_tol_sqd ) ! Positive definite quantity ! Positive definite quantity
    wp3_zm = zt2zm( wp3 )
    Skw_zt(1:gr%nz) = Skw_func( wp2_zt(1:gr%nz), wp3(1:gr%nz) )
    Skw_zm(1:gr%nz) = Skw_func( wp2(1:gr%nz), wp3_zm(1:gr%nz) )
    if ( l_stats_samp ) then
      call stat_update_var( iSkw_zt, Skw_zt, & ! In
                            stats_zt ) ! In/Out ! In
! In/Out
      call stat_update_var( iSkw_zm, Skw_zm, &
                            stats_zm ) ! In/Out
! In/Out
    end if
! The right hand side of this conjunction is only for reducing cpu time,
! since the more complicated formula is mathematically equivalent
    if ( l_gamma_Skw .and. ( gamma_coef /= gamma_coefb ) ) then
!----------------------------------------------------------------
! Compute gamma as a function of Skw  - 14 April 06 dschanen
!----------------------------------------------------------------
      gamma_Skw_fnc = gamma_coefb + (gamma_coef-gamma_coefb) &
            *exp( -(1.0_core_rknd/2.0_core_rknd) * (Skw_zm/gamma_coefc)**2 )
    else
      gamma_Skw_fnc = gamma_coef
    end if
! Compute sigma_sqd_w (dimensionless PDF width parameter)
    sigma_sqd_w = compute_sigma_sqd_w( gamma_Skw_fnc, wp2, thlp2, rtp2, wpthlp, wprtp )
    if ( l_stats_samp ) then
      call stat_update_var( igamma_Skw_fnc, gamma_Skw_fnc, & ! intent(in)
                            stats_zm )                       ! intent(inout) ! intent(in)
! intent(inout)
    endif
! Smooth in the vertical using interpolation
    sigma_sqd_w = zt2zm( zm2zt( sigma_sqd_w ) )
! Interpolate the the stats_zt grid
    sigma_sqd_w_zt = max( zm2zt( sigma_sqd_w ), zero_threshold )  ! Pos. def. quantity ! Pos. def. quantity
! Compute the a3 coefficient (formula 25 in `Equations for CLUBB')
!   a3_coef = 3.0_core_rknd * sigma_sqd_w*sigma_sqd_w  &
!      + 6.0_core_rknd*(1.0_core_rknd-sigma_sqd_w)*sigma_sqd_w  &
!      + (1.0_core_rknd-sigma_sqd_w)*(1.0_core_rknd-sigma_sqd_w) &
!      - 3.0_core_rknd
! This is a simplified version of the formula above.
    a3_coef = -2._core_rknd * ( 1._core_rknd - sigma_sqd_w )**2
! We found we obtain fewer spikes in wp3 when we clip a3 to be no greater
! than -1.4 -dschanen 4 Jan 2011
    a3_coef = max( a3_coef, -1.4_core_rknd ) ! Known magic number ! Known magic number
    a3_coef_zt = zm2zt( a3_coef )
!---------------------------------------------------------------------------
! Interpolate thlp2, rtp2, and rtpthlp to thermodynamic levels,
!---------------------------------------------------------------------------
! Interpolate variances to the stats_zt grid (statistics and closure)
    thlp2_zt   = max( zm2zt( thlp2 ), thl_tol**2 ) ! Positive def. quantity ! Positive def. quantity
    rtp2_zt    = max( zm2zt( rtp2 ), rt_tol**2 )   ! Positive def. quantity ! Positive def. quantity
    rtpthlp_zt = zm2zt( rtpthlp )
! Compute skewness velocity for stats output purposes
    if ( iSkw_velocity > 0 ) then
      Skw_velocity = ( 1.0_core_rknd / ( 1.0_core_rknd - sigma_sqd_w(1:gr%nz) ) ) & 
                   * ( wp3_zm(1:gr%nz) / max( wp2(1:gr%nz), w_tol_sqd ) )
    end if
! Compute wp3 / wp2 on zt levels.  Always use the interpolated value in the
! denominator since it's less likely to create spikes
    wp3_on_wp2_zt = ( wp3(1:gr%nz) / max( wp2_zt(1:gr%nz), w_tol_sqd ) )
! Clip wp3_on_wp2_zt if it's too large
    do k=1, gr%nz
      if( wp3_on_wp2_zt(k) < 0._core_rknd ) then
        wp3_on_wp2_zt = max( -1000._core_rknd, wp3_on_wp2_zt )
      else
        wp3_on_wp2_zt = min( 1000._core_rknd, wp3_on_wp2_zt )
      end if
    end do
! Compute wp3_on_wp2 by interpolating wp3_on_wp2_zt
    wp3_on_wp2 = zt2zm( wp3_on_wp2_zt )
! Smooth again as above
    wp3_on_wp2_zt = zm2zt( wp3_on_wp2 )
!----------------------------------------------------------------
! Call closure scheme
!----------------------------------------------------------------
! Put passive scalar input on the t grid for the PDF
    do i = 1, sclr_dim, 1
      wpsclrp_zt(:,i)   = zm2zt( wpsclrp(:,i) )
      sclrp2_zt(:,i)    = max( zm2zt( sclrp2(:,i) ), zero_threshold ) ! Pos. def. quantity ! Pos. def. quantity
      sclrprtp_zt(:,i)  = zm2zt( sclrprtp(:,i) )
      sclrpthlp_zt(:,i) = zm2zt( sclrpthlp(:,i) )
    end do ! i = 1, sclr_dim, 1 ! i = 1, sclr_dim, 1
! Interpolate hydrometeor mixed moments to momentum levels.
    do i = 1, hydromet_dim, 1
       wphydrometp_zt(:,i) = zm2zt( wphydrometp(:,i) )
    enddo ! i = 1, hydromet_dim, 1 ! i = 1, hydromet_dim, 1
    do k = 1, gr%nz, 1
      call pdf_closure & 
        ( hydromet_dim, p_in_Pa(k), exner(k), thv_ds_zt(k), wm_zt(k), & ! intent(in)
          wp2_zt(k), wp3(k), sigma_sqd_w_zt(k),                       & ! intent(in)
          Skw_zt(k), rtm(k), rtp2_zt(k),                              & ! intent(in)
          zm2zt( wprtp, k ), thlm(k), thlp2_zt(k),                    & ! intent(in)
          zm2zt( wpthlp, k ), rtpthlp_zt(k), sclrm(k,:),              & ! intent(in)
          wpsclrp_zt(k,:), sclrp2_zt(k,:), sclrprtp_zt(k,:),          & ! intent(in)
          sclrpthlp_zt(k,:), k,                                       & ! intent(in)
!KGEN# 1023

          wphydrometp_zt(k,:), wp2hmp(k,:),                           & ! intent(in)
          rtphmp_zt(k,:), thlphmp_zt(k,:),                            & ! intent(in)
          wp4_zt(k), wprtp2(k), wp2rtp(k),                            & ! intent(out)
          wpthlp2(k), wp2thlp(k), wprtpthlp(k),                       & ! intent(out)
          cloud_frac(k), ice_supersat_frac(k),                        & ! intent(out)
          rcm(k), wpthvp_zt(k), wp2thvp(k), rtpthvp_zt(k),            & ! intent(out)
          thlpthvp_zt(k), wprcp_zt(k), wp2rcp(k), rtprcp_zt(k),       & ! intent(out)
          thlprcp_zt(k), rcp2_zt(k), pdf_params(k),                   & ! intent(out)
          err_code_pdf_closure,                                       & ! intent(out)
          wpsclrprtp(k,:), wpsclrp2(k,:), sclrpthvp_zt(k,:),          & ! intent(out)
          wpsclrpthlp(k,:), sclrprcp_zt(k,:), wp2sclrp(k,:),          & ! intent(out)
          rc_coef_zt(k)                                               ) ! intent(out)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! Subroutine may produce NaN values, and if so, exit
! gracefully.
! Joshua Fasching March 2008
      if ( fatal_error( err_code_pdf_closure ) ) then
        if ( clubb_at_least_debug_level( 1 ) ) then
          write(fstderr,*) "At grid level = ",k
        end if
        err_code = err_code_pdf_closure
      end if
    end do ! k = 1, gr%nz, 1 ! k = 1, gr%nz, 1
    if ( l_refine_grid_in_cloud ) then
! Compute cloud_frac and rcm on a refined grid to improve parameterization
! of subgrid clouds
      do k=1, gr%nz
        if ( pdf_params(k)%chi_1/pdf_params(k)%stdev_chi_1 > -1._core_rknd ) then
! Recalculate cloud_frac and r_c for each PDF component
          call calc_vert_avg_cf_component &
               ( gr%nz, k, gr%zt, pdf_params%chi_1, &                    ! Intent(in)
                 pdf_params%stdev_chi_1, (/(chi_at_liq_sat,i=1,gr%nz)/), & ! Intent(in)
                 cloud_frac_1_refined, rc_1_refined )                   ! Intent(out)
! Intent(in)
! Intent(in)
! Intent(out)
          call calc_vert_avg_cf_component & 
               ( gr%nz, k, gr%zt, pdf_params%chi_2, &                     ! Intent(in)
                 pdf_params%stdev_chi_2, (/(chi_at_liq_sat,i=1,gr%nz)/), &  ! Intent(in)
                 cloud_frac_2_refined, rc_2_refined )                    ! Intent(out)
! Intent(in)
! Intent(in)
! Intent(out)
          cloud_frac_refined = compute_mean_binormal &
                               ( cloud_frac_1_refined, cloud_frac_2_refined, &
                                 pdf_params(k)%mixt_frac )
          rcm_refined = compute_mean_binormal &
                        ( rc_1_refined, rc_2_refined, pdf_params(k)%mixt_frac )
          if ( l_interactive_refined ) then
! I commented out the lines that modify the values in pdf_params, as it seems that
! these values need to remain consistent with the rest of the PDF.
! Eric Raut Jun 2014
! Replace pdf_closure estimates with refined estimates
! pdf_params(k)%rc_1 = rc_1_refined
! pdf_params(k)%rc_2 = rc_2_refined
            rcm(k) = rcm_refined
! pdf_params(k)%cloud_frac_1 = cloud_frac_1_refined
! pdf_params(k)%cloud_frac_2 = cloud_frac_2_refined
            cloud_frac(k) = cloud_frac_refined
          end if
        else
! Set these equal to the non-refined values so we have something to
! output to stats!
          cloud_frac_refined = cloud_frac(k)
          rcm_refined = rcm(k)
        end if ! pdf_params(k)%chi_1/pdf_params(k)%stdev_chi_1 > -1._core_rknd ! pdf_params(k)%chi_1/pdf_params(k)%stdev_chi_1 > -1._core_rknd
! Stats output
        if ( l_stats_samp ) then
          call stat_update_var_pt( icloud_frac_refined, k, cloud_frac_refined, stats_zt )
          call stat_update_var_pt( ircm_refined, k, rcm_refined, stats_zt )
        end if
      end do ! k=1, gr%nz ! k=1, gr%nz
    end if ! l_refine_grid_in_cloud ! l_refine_grid_in_cloud
    if( l_rtm_nudge ) then
! Nudge rtm to prevent excessive drying
      where( rtm < rtm_min .and. gr%zt < rtm_nudge_max_altitude )
        rtm = rtm + (rtm_ref - rtm) * ( dt / ts_nudge )
                END WHERE 
    end if
    if ( l_call_pdf_closure_twice ) then
! Call pdf_closure a second time on momentum levels, to
! output (rather than interpolate) the variables which
! belong on the momentum levels.
! Interpolate sclrm to the momentum level for use in
! the second call to pdf_closure
      do i = 1, sclr_dim
        sclrm_zm(:,i) = zt2zm( sclrm(:,i) )
! Clip if extrap. causes sclrm_zm to be less than sclr_tol
        sclrm_zm(gr%nz,i) = max( sclrm_zm(gr%nz,i), sclr_tol(i) )
      end do ! i = 1, sclr_dim ! i = 1, sclr_dim
! Interpolate pressure, p_in_Pa, to momentum levels.
! The pressure at thermodynamic level k = 1 has been set to be the surface
! (or model lower boundary) pressure.  Since the surface (or model lower
! boundary) is located at momentum level k = 1, the pressure there is
! p_sfc, which is p_in_Pa(1).  Thus, p_in_Pa_zm(1) = p_in_Pa(1).
      p_in_Pa_zm(:) = zt2zm( p_in_Pa )
      p_in_Pa_zm(1) = p_in_Pa(1)
! Clip pressure if the extrapolation leads to a negative value of pressure
      p_in_Pa_zm(gr%nz) = max( p_in_Pa_zm(gr%nz), 0.5_core_rknd*p_in_Pa(gr%nz) )
! Set exner at momentum levels, exner_zm, based on p_in_Pa_zm.
      exner_zm(:) = (p_in_Pa_zm(:)/p0)**kappa
      rtm_zm = zt2zm( rtm )
! Clip if extrapolation at the top level causes rtm_zm to be < rt_tol
      rtm_zm(gr%nz) = max( rtm_zm(gr%nz), rt_tol )
      thlm_zm = zt2zm( thlm )
! Clip if extrapolation at the top level causes thlm_zm to be < thl_tol
      thlm_zm(gr%nz) = max( thlm_zm(gr%nz), thl_tol )
! Interpolate hydrometeor mixed moments to momentum levels.
      do i = 1, hydromet_dim, 1
         rtphmp(:,i)    = zt2zm( rtphmp_zt(:,i) )
         thlphmp(:,i)   = zt2zm( thlphmp_zt(:,i) )
         wp2hmp_zm(:,i) = zt2zm( wp2hmp(:,i) )
      enddo ! i = 1, hydromet_dim, 1 ! i = 1, hydromet_dim, 1
! Call pdf_closure to output the variables which belong on the momentum grid.
      do k = 1, gr%nz, 1
        call pdf_closure & 
          ( hydromet_dim, p_in_Pa_zm(k), exner_zm(k), thv_ds_zm(k), wm_zm(k), & ! intent(in)
            wp2(k), wp3_zm(k), sigma_sqd_w(k),                                & ! intent(in)
            Skw_zm(k), rtm_zm(k), rtp2(k),                                    & ! intent(in)
            wprtp(k),  thlm_zm(k), thlp2(k),                                  & ! intent(in)
            wpthlp(k), rtpthlp(k), sclrm_zm(k,:),                             & ! intent(in)
            wpsclrp(k,:), sclrp2(k,:), sclrprtp(k,:),                         & ! intent(in)
            sclrpthlp(k,:), k,                                                & ! intent(in)
!KGEN# 1171

            wphydrometp(k,:), wp2hmp_zm(k,:),                                 & ! intent(in)
            rtphmp(k,:), thlphmp(k,:),                                        & ! intent(in)
            wp4(k), wprtp2_zm(k), wp2rtp_zm(k),                               & ! intent(out)
            wpthlp2_zm(k), wp2thlp_zm(k), wprtpthlp_zm(k),                    & ! intent(out)
            cloud_frac_zm(k), ice_supersat_frac_zm(k),                        & ! intent(out)
            rcm_zm(k), wpthvp(k), wp2thvp_zm(k), rtpthvp(k),                  & ! intent(out)
            thlpthvp(k), wprcp(k), wp2rcp_zm(k), rtprcp(k),                   & ! intent(out)
            thlprcp(k), rcp2(k), pdf_params_zm(k),                            & ! intent(out)
            err_code_pdf_closure,                                             & ! intent(out)
            wpsclrprtp_zm(k,:), wpsclrp2_zm(k,:), sclrpthvp(k,:),             & ! intent(out)
            wpsclrpthlp_zm(k,:), sclrprcp(k,:), wp2sclrp_zm(k,:),             & ! intent(out)
            rc_coef(k)                                                        ) ! intent(out)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! Subroutine may produce NaN values, and if so, exit
! gracefully.
! Joshua Fasching March 2008
        if ( fatal_error( err_code_pdf_closure ) ) then
          if ( clubb_at_least_debug_level( 1 ) ) then
            write(fstderr,*) "At grid level = ",k
          end if
          err_code = err_code_pdf_closure
        end if
      end do ! k = 1, gr%nz, 1 ! k = 1, gr%nz, 1
    else ! l_call_pdf_closure_twice is false ! l_call_pdf_closure_twice is false
! Interpolate momentum variables output from the first call to
! pdf_closure back to momentum grid.
! Since top momentum level is higher than top thermo level,
! Set variables at top momentum level to 0.
! Only do this for wp4 and rcp2 if we're saving stats, since they are not
! used elsewhere in the parameterization
      if ( iwp4 > 0 ) then
        wp4 = max( zt2zm( wp4_zt ), zero_threshold )  ! Pos. def. quantity ! Pos. def. quantity
        wp4(gr%nz)  = 0.0_core_rknd
      end if
        rcp2 = max( zt2zm( rcp2_zt ), zero_threshold )  ! Pos. def. quantity ! Pos. def. quantity
      wpthvp            = zt2zm( wpthvp_zt )
      wpthvp(gr%nz)   = 0.0_core_rknd
      thlpthvp          = zt2zm( thlpthvp_zt )
      thlpthvp(gr%nz) = 0.0_core_rknd
      rtpthvp           = zt2zm( rtpthvp_zt )
      rtpthvp(gr%nz)  = 0.0_core_rknd
      wprcp             = zt2zm( wprcp_zt )
      wprcp(gr%nz)    = 0.0_core_rknd
      rc_coef           = zt2zm( rc_coef_zt )
      rc_coef(gr%nz)  = 0.0_core_rknd
      rtprcp            = zt2zm( rtprcp_zt )
      rtprcp(gr%nz)   = 0.0_core_rknd
      thlprcp           = zt2zm( thlprcp_zt )
      thlprcp(gr%nz)  = 0.0_core_rknd
! Interpolate passive scalars back onto the m grid
      do i = 1, sclr_dim
        sclrpthvp(:,i)       = zt2zm( sclrpthvp_zt(:,i) )
        sclrpthvp(gr%nz,i) = 0.0_core_rknd
        sclrprcp(:,i)        = zt2zm( sclrprcp_zt(:,i) )
        sclrprcp(gr%nz,i)  = 0.0_core_rknd
      end do ! i=1, sclr_dim ! i=1, sclr_dim
    end if ! l_call_pdf_closure_twice ! l_call_pdf_closure_twice
! If l_trapezoidal_rule_zt is true, call trapezoidal_rule_zt for
! thermodynamic-level variables output from pdf_closure.
! ldgrant June 2009
    if ( l_trapezoidal_rule_zt ) then
      call trapezoidal_rule_zt &
           ( l_call_pdf_closure_twice,                    & ! intent(in)
             wprtp2, wpthlp2,                             & ! intent(inout)
             wprtpthlp, cloud_frac, ice_supersat_frac,    & ! intent(inout)
             rcm, wp2thvp, wpsclrprtp, wpsclrp2,          & ! intent(inout)
             wpsclrpthlp, pdf_params,                     & ! intent(inout)
             wprtp2_zm, wpthlp2_zm,                       & ! intent(inout)
             wprtpthlp_zm, cloud_frac_zm,                 & ! intent(inout)
             ice_supersat_frac_zm, rcm_zm, wp2thvp_zm,    & ! intent(inout)
             wpsclrprtp_zm, wpsclrp2_zm, wpsclrpthlp_zm,  & ! intent(inout)
             pdf_params_zm )                                ! intent(inout)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
    end if ! l_trapezoidal_rule_zt ! l_trapezoidal_rule_zt
! If l_trapezoidal_rule_zm is true, call trapezoidal_rule_zm for
! the important momentum-level variabes output from pdf_closure.
! ldgrant Feb. 2010
    if ( l_trapezoidal_rule_zm ) then
      call trapezoidal_rule_zm &
         ( wpthvp_zt, thlpthvp_zt, rtpthvp_zt, & ! intent(in)
           wpthvp, thlpthvp, rtpthvp )           ! intent(inout)
! intent(in)
! intent(inout)
    end if ! l_trapezoidal_rule_zm ! l_trapezoidal_rule_zm
! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
! This code won't work unless rtm >= 0 !!!
! We do not clip rcm_in_layer because rcm_in_layer only influences
! radiation, and we do not want to bother recomputing it.
! Code is duplicated from below to ensure that relative humidity
! is calculated properly.  3 Sep 2009
    call clip_rcm( rtm, 'rtm < rcm after pdf_closure', & ! intent (in)
                   rcm )                                 ! intent (inout) ! intent (in)
! intent (inout)
! Compute variables cloud_cover and rcm_in_layer.
! Added July 2009
    call compute_cloud_cover &
       ( pdf_params, cloud_frac, rcm, & ! intent(in)
         cloud_cover, rcm_in_layer )    ! intent(out)
! intent(in)
! intent(out)
! Use cloud_cover and rcm_in_layer to help boost cloud_frac and rcm to help
! increase cloudiness at coarser grid resolutions.
    if ( l_use_cloud_cover ) then
      cloud_frac = cloud_cover
      rcm = rcm_in_layer
    end if
! Clip cloud fraction here if it still exceeds 1.0 due to round off
    cloud_frac = min( 1.0_core_rknd, cloud_frac )
! Ditto with ice cloud fraction
    ice_supersat_frac = min( 1.0_core_rknd, ice_supersat_frac )
    if (l_use_ice_latent) then
!A third call to pdf_closure, with terms modified to include the effects
!of latent heating due to ice.  Thlm and rtm add the effects of ice, and
!the terms are all renamed with "_frz" appended. The modified terms will
!be fed into the calculations of the turbulence terms. storer-3/14/13
!Also added rain for completeness. storer-3/4/14
      if ( iirrm > 0 ) then
        rrm = hydromet(:,iirrm)
      else
        rrm = zero
      end if
      thlm_frz = thlm - (Lv / (Cp*exner) ) * rrm - (Ls / (Cp*exner) ) * rfrzm 
      rtm_frz = rtm + rrm + rfrzm
      do k = 1, gr%nz, 1
        call pdf_closure & 
          ( hydromet_dim, p_in_Pa(k), exner(k), thv_ds_zt(k), wm_zt(k),           & ! intent(in)
            wp2_zt(k), wp3(k), sigma_sqd_w_zt(k),                                 & ! intent(in)
            Skw_zt(k), rtm_frz(k), rtp2_zt(k),                                    & ! intent(in)
            zm2zt( wprtp, k ), thlm_frz(k), thlp2_zt(k),                          & ! intent(in)
            zm2zt( wpthlp, k ), rtpthlp_zt(k), sclrm(k,:),                        & ! intent(in)
            wpsclrp_zt(k,:), sclrp2_zt(k,:), sclrprtp_zt(k,:),                    & ! intent(in)
            sclrpthlp_zt(k,:), k,                                                 & ! intent(in)
!KGEN# 1333

            wphydrometp_zt(k,:), wp2hmp(k,:),                                     & ! intent(in)
            rtphmp_zt(k,:), thlphmp_zt(k,:),                                      & ! intent(in)
            wp4_zt_frz(k), wprtp2_frz(k), wp2rtp_frz(k),                          & ! intent(out)
            wpthlp2_frz(k), wp2thlp_frz(k), wprtpthlp_frz(k),                     & ! intent(out)
            cloud_frac_frz(k), ice_supersat_frac_frz(k),                          & ! intent(out)
            rcm_frz(k), wpthvp_zt_frz(k), wp2thvp_frz(k), rtpthvp_zt_frz(k),      & ! intent(out)
            thlpthvp_zt_frz(k), wprcp_zt_frz(k), wp2rcp_frz(k), rtprcp_zt_frz(k), & ! intent(out)
            thlprcp_zt_frz(k), rcp2_zt_frz(k), pdf_params_frz(k),                 & ! intent(out)
            err_code_pdf_closure,                                                 & ! intent(out)
            wpsclrprtp_frz(k,:), wpsclrp2_frz(k,:), sclrpthvp_zt_frz(k,:),        & ! intent(out)
            wpsclrpthlp_frz(k,:), sclrprcp_zt_frz(k,:), wp2sclrp_frz(k,:),        & ! intent(out)
            rc_coef_zt_frz(k)                                                     ) ! intent(out)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! Subroutine may produce NaN values, and if so, exit gracefully.
! Joshua Fasching March 2008
        if ( fatal_error( err_code_pdf_closure ) ) then
          if ( clubb_at_least_debug_level ( 1 ) )then
            write(fstderr,*) "At grid level = ", k
          end if
          err_code = err_code_pdf_closure
        end if
      end do !k=1, gr%nz, 1 !k=1, gr%nz, 1
      if( l_rtm_nudge ) then
! Nudge rtm to prevent excessive drying
        where( rtm < rtm_min .and. gr%zt < rtm_nudge_max_altitude )
          rtm = rtm + (rtm_ref - rtm) * ( dt / ts_nudge )
                    END WHERE 
      end if
      rtm_zm_frz = zt2zm( rtm_frz )
! Clip if extrapolation at the top level causes rtm_zm to be < rt_tol
      rtm_zm_frz(gr%nz) = max( rtm_zm_frz(gr%nz), rt_tol )
      thlm_zm_frz = zt2zm( thlm_frz )
! Clip if extrapolation at the top level causes thlm_zm to be < thl_tol
      thlm_zm_frz(gr%nz) = max( thlm_zm_frz(gr%nz), thl_tol )
      if ( l_call_pdf_closure_twice ) then
! Call pdf_closure again to output the variables which belong on the momentum grid.
        do k=1, gr%nz, 1
          call pdf_closure & 
            ( hydromet_dim, p_in_Pa_zm(k), exner_zm(k), thv_ds_zm(k), wm_zm(k), & ! intent(in)
              wp2(k), wp3_zm(k), sigma_sqd_w(k),                                & ! intent(in)
              Skw_zm(k), rtm_zm_frz(k), rtp2(k),                                & ! intent(in)
              wprtp(k),  thlm_zm_frz(k), thlp2(k),                              & ! intent(in)
              wpthlp(k), rtpthlp(k), sclrm_zm(k,:),                             & ! intent(in)
              wpsclrp(k,:), sclrp2(k,:), sclrprtp(k,:),                         & ! intent(in)
              sclrpthlp(k,:), k,                                                & ! intent(in)
!KGEN# 1389

              wphydrometp(k,:), wp2hmp_zm(k,:),                                 & ! intent(in)
              rtphmp(k,:), thlphmp(k,:),                                        & ! intent(in)
              wp4_frz(k), wprtp2_zm_frz(k), wp2rtp_zm_frz(k),                   & ! intent(out)
              wpthlp2_zm_frz(k), wp2thlp_zm_frz(k), wprtpthlp_zm_frz(k),        & ! intent(out)
              cloud_frac_zm_frz(k), ice_supersat_frac_zm_frz(k),                & ! intent(out)
              rcm_zm_frz(k), wpthvp_frz(k), wp2thvp_zm_frz(k), rtpthvp_frz(k),  & ! intent(out)
              thlpthvp_frz(k), wprcp_frz(k), wp2rcp_zm_frz(k), rtprcp_frz(k),   & ! intent(out)
              thlprcp_frz(k), rcp2_frz(k), pdf_params_zm_frz(k),                & ! intent(out)
              err_code_pdf_closure,                                             & ! intent(out)
              wpsclrprtp_zm_frz(k,:), wpsclrp2_zm_frz(k,:), sclrpthvp_frz(k,:), & ! intent(out)
              wpsclrpthlp_zm_frz(k,:), sclrprcp_frz(k,:), wp2sclrp_zm_frz(k,:), & ! intent(out)
              rc_coef_frz(k)                                                    ) ! intent(out)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! Subroutine may produce NaN values, and if so, exit
! gracefully.
! Joshua Fasching March 2008
          if ( fatal_error( err_code_pdf_closure ) ) then
            if ( clubb_at_least_debug_level( 1 ) ) then
              write(fstderr,*) "At grid level = ",k
            end if
            err_code = err_code_pdf_closure
          end if
        end do ! k = 1, gr%nz, 1 ! k = 1, gr%nz, 1
      else ! l_call_pdf_closure_twice is false ! l_call_pdf_closure_twice is false
        wpthvp_frz            = zt2zm( wpthvp_zt_frz )
        wpthvp_frz(gr%nz)   = 0.0_core_rknd
        thlpthvp_frz          = zt2zm( thlpthvp_zt_frz )
        thlpthvp_frz(gr%nz) = 0.0_core_rknd
        rtpthvp_frz           = zt2zm( rtpthvp_zt_frz )
        rtpthvp_frz(gr%nz)  = 0.0_core_rknd
      end if ! l_call_pdf_closure_twice ! l_call_pdf_closure_twice
      if ( l_trapezoidal_rule_zt ) then
        call trapezoidal_rule_zt &
           ( l_call_pdf_closure_twice,                                & ! intent(in)
             wprtp2_frz, wpthlp2_frz,                                 & ! intent(inout)
             wprtpthlp_frz, cloud_frac_frz, ice_supersat_frac_frz,    & ! intent(inout)
             rcm_frz, wp2thvp_frz, wpsclrprtp_frz, wpsclrp2_frz,      & ! intent(inout)
             wpsclrpthlp_frz, pdf_params_frz,                         & ! intent(inout)
             wprtp2_zm_frz, wpthlp2_zm_frz,                           & ! intent(inout)
             wprtpthlp_zm_frz, cloud_frac_zm_frz,                     & ! intent(inout)
             ice_supersat_frac_zm_frz, rcm_zm_frz, wp2thvp_zm_frz,    & ! intent(inout)
             wpsclrprtp_zm_frz, wpsclrp2_zm_frz, wpsclrpthlp_zm_frz,  & ! intent(inout)
             pdf_params_zm_frz                                        ) ! intent(inout)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
      end if ! l_trapezoidal_rule_zt ! l_trapezoidal_rule_zt
! If l_trapezoidal_rule_zm is true, call trapezoidal_rule_zm for
! the important momentum-level variabes output from pdf_closure.
! ldgrant Feb. 2010
        if ( l_trapezoidal_rule_zm ) then
          call trapezoidal_rule_zm &
             ( wpthvp_zt_frz, thlpthvp_zt_frz, rtpthvp_zt_frz, & ! intent(in)
               wpthvp_frz, thlpthvp_frz, rtpthvp_frz )           ! intent(inout)
! intent(in)
! intent(inout)
        end if ! l_trapezoidal_rule_zm ! l_trapezoidal_rule_zm
        wpthvp = wpthvp_frz
        wp2thvp = wp2thvp_frz
        thlpthvp = thlpthvp_frz
        rtpthvp = rtpthvp_frz
      end if ! l_use_ice_latent = .true. ! l_use_ice_latent = .true.
!----------------------------------------------------------------
! Compute thvm
!----------------------------------------------------------------
      thvm = thlm + ep1 * thv_ds_zt * rtm &
                  + ( Lv/(Cp*exner) - ep2 * thv_ds_zt ) * rcm
!----------------------------------------------------------------
! Compute tke (turbulent kinetic energy)
!----------------------------------------------------------------
      if ( .not. l_tke_aniso ) then
! tke is assumed to be 3/2 of wp2
        em = three_halves * wp2 ! Known magic number ! Known magic number
      else
        em = 0.5_core_rknd * ( wp2 + vp2 + up2 )
      end if
!----------------------------------------------------------------
! Compute mixing length
!----------------------------------------------------------------
      if ( l_avg_Lscale .and. .not. l_Lscale_plume_centered ) then
! Call compute length two additional times with perturbed values
! of rtm and thlm so that an average value of Lscale may be calculated.
        if ( l_use_ice_latent ) then
!Include the effects of ice in the length scale calculation
          thlm_pert_1 = thlm_frz + Lscale_pert_coef * sqrt( max( thlp2, thl_tol**2 ) )
          rtm_pert_1  = rtm_frz  + Lscale_pert_coef * sqrt( max( rtp2, rt_tol**2 ) )
          mu_pert_1  = newmu / Lscale_mu_coef
          thlm_pert_2 = thlm_frz - Lscale_pert_coef * sqrt( max( thlp2, thl_tol**2 ) )
          rtm_pert_2  = rtm_frz  - Lscale_pert_coef * sqrt( max( rtp2, rt_tol**2 ) )
          mu_pert_2  = newmu * Lscale_mu_coef
        else
          thlm_pert_1 = thlm + Lscale_pert_coef * sqrt( max( thlp2, thl_tol**2 ) )
          rtm_pert_1  = rtm  + Lscale_pert_coef * sqrt( max( rtp2, rt_tol**2 ) )
          mu_pert_1  = newmu / Lscale_mu_coef
          thlm_pert_2 = thlm - Lscale_pert_coef * sqrt( max( thlp2, thl_tol**2 ) )
          rtm_pert_2  = rtm  - Lscale_pert_coef * sqrt( max( rtp2, rt_tol**2 ) )
          mu_pert_2  = newmu * Lscale_mu_coef
        end if
        call compute_length( thvm, thlm_pert_1, rtm_pert_1, em, Lscale_max,       & ! intent(in)
                             p_in_Pa, exner, thv_ds_zt, mu_pert_1, l_implemented, & ! intent(in)
                             err_code,                                            & ! intent(inout)
                             Lscale_pert_1, Lscale_up, Lscale_down )                ! intent(out) ! intent(in)
! intent(in)
! intent(inout)
! intent(out)
        call compute_length( thvm, thlm_pert_2, rtm_pert_2, em, Lscale_max,       & ! intent(in)
                             p_in_Pa, exner, thv_ds_zt, mu_pert_2, l_implemented, & ! intent(in)
                             err_code,                                            & ! intent(inout)
                             Lscale_pert_2, Lscale_up, Lscale_down )                ! intent(out) ! intent(in)
! intent(in)
! intent(inout)
! intent(out)
      else if ( l_avg_Lscale .and. l_Lscale_plume_centered ) then
! Take the values of thl and rt based one 1st or 2nd plume
        do k = 1, gr%nz, 1
          sign_rtpthlp(k) = sign(1.0_core_rknd, rtpthlp(k))
        end do
        if ( l_use_ice_latent ) then
          where ( pdf_params_frz%rt_1 > pdf_params_frz%rt_2 )
            rtm_pert_pos_rt = pdf_params_frz%rt_1 &
                       + Lscale_pert_coef * sqrt( max( pdf_params_frz%varnce_rt_1, rt_tol**2 ) )
            thlm_pert_pos_rt = pdf_params_frz%thl_1 + ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params_frz%varnce_thl_1, thl_tol**2 ) ) )
            thlm_pert_neg_rt = pdf_params_frz%thl_2 - ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params_frz%varnce_thl_2, thl_tol**2 ) ) )
            rtm_pert_neg_rt = pdf_params_frz%rt_2 & 
                       - Lscale_pert_coef * sqrt( max( pdf_params_frz%varnce_rt_2, rt_tol**2 ) )
!Lscale_weight = pdf_params%mixt_frac
          else where
            rtm_pert_pos_rt = pdf_params_frz%rt_2 &
                       + Lscale_pert_coef * sqrt( max( pdf_params_frz%varnce_rt_2, rt_tol**2 ) )
            thlm_pert_pos_rt = pdf_params_frz%thl_2 + ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params_frz%varnce_thl_2, thl_tol**2 ) ) )
            thlm_pert_neg_rt = pdf_params_frz%thl_1 - ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params_frz%varnce_thl_1, thl_tol**2 ) ) )
            rtm_pert_neg_rt = pdf_params_frz%rt_1 & 
                       - Lscale_pert_coef * sqrt( max( pdf_params_frz%varnce_rt_1, rt_tol**2 ) )
!Lscale_weight = 1.0_core_rknd - pdf_params%mixt_frac
                    END WHERE 
        else
          where ( pdf_params%rt_1 > pdf_params%rt_2 )
            rtm_pert_pos_rt = pdf_params%rt_1 &
                       + Lscale_pert_coef * sqrt( max( pdf_params%varnce_rt_1, rt_tol**2 ) )
            thlm_pert_pos_rt = pdf_params%thl_1 + ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params%varnce_thl_1, thl_tol**2 ) ) )
            thlm_pert_neg_rt = pdf_params%thl_2 - ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params%varnce_thl_2, thl_tol**2 ) ) )
            rtm_pert_neg_rt = pdf_params%rt_2 & 
                       - Lscale_pert_coef * sqrt( max( pdf_params%varnce_rt_2, rt_tol**2 ) )
!Lscale_weight = pdf_params%mixt_frac
          else where
            rtm_pert_pos_rt = pdf_params%rt_2 &
                       + Lscale_pert_coef * sqrt( max( pdf_params%varnce_rt_2, rt_tol**2 ) )
            thlm_pert_pos_rt = pdf_params%thl_2 + ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params%varnce_thl_2, thl_tol**2 ) ) )
            thlm_pert_neg_rt = pdf_params%thl_1 - ( sign_rtpthlp * Lscale_pert_coef &
                       * sqrt( max( pdf_params%varnce_thl_1, thl_tol**2 ) ) )
            rtm_pert_neg_rt = pdf_params%rt_1 & 
                       - Lscale_pert_coef * sqrt( max( pdf_params%varnce_rt_1, rt_tol**2 ) )
!Lscale_weight = 1.0_core_rknd - pdf_params%mixt_frac
                    END WHERE 
        end if
        mu_pert_pos_rt  = newmu / Lscale_mu_coef
        mu_pert_neg_rt  = newmu * Lscale_mu_coef
! Call length with perturbed values of thl and rt
        call compute_length( thvm, thlm_pert_pos_rt, rtm_pert_pos_rt, em, Lscale_max, &!intent(in)
                           p_in_Pa, exner, thv_ds_zt, mu_pert_pos_rt, l_implemented, & !intent(in)
                           err_code, &                                             ! intent(inout)
                           Lscale_pert_1, Lscale_up, Lscale_down )                 ! intent(out) !intent(in)
!intent(in)
! intent(inout)
! intent(out)
        call compute_length( thvm, thlm_pert_neg_rt, rtm_pert_neg_rt, em, Lscale_max, &!intent(in)
                           p_in_Pa, exner, thv_ds_zt, mu_pert_neg_rt, l_implemented, & !intent(in)
                           err_code, &                                             ! intent(inout)
                           Lscale_pert_2, Lscale_up, Lscale_down )                 ! intent(out) !intent(in)
!intent(in)
! intent(inout)
! intent(out)
      else
        Lscale_pert_1 = unused_var ! Undefined ! Undefined
        Lscale_pert_2 = unused_var ! Undefined ! Undefined
      end if ! l_avg_Lscale ! l_avg_Lscale
      if ( l_stats_samp ) then
        call stat_update_var( iLscale_pert_1, Lscale_pert_1, & ! intent(in)
                              stats_zt )                             ! intent(inout) ! intent(in)
! intent(inout)
        call stat_update_var( iLscale_pert_2, Lscale_pert_2, & ! intent(in)
                              stats_zt )                             ! intent(inout) ! intent(in)
! intent(inout)
      end if ! l_stats_samp ! l_stats_samp
! ********** NOTE: **********
! This call to compute_length must be last.  Otherwise, the values of
! Lscale_up and Lscale_down in stats will be based on perturbation length scales
! rather than the mean length scale.
      call compute_length( thvm, thlm, rtm, em, Lscale_max,              & ! intent(in)
                           p_in_Pa, exner, thv_ds_zt, newmu, l_implemented, & ! intent(in)
                           err_code,                                     & ! intent(inout)
                           Lscale, Lscale_up, Lscale_down )                ! intent(out) ! intent(in)
! intent(in)
! intent(inout)
! intent(out)
      if ( l_avg_Lscale ) then
        if ( l_Lscale_plume_centered ) then
! Weighted average of mean, pert_1, & pert_2
!       Lscale = 0.5_core_rknd * ( Lscale + Lscale_weight*Lscale_pert_1 &
!                                  + (1.0_core_rknd-Lscale_weight)*Lscale_pert_2 )
! Weighted average of just the perturbed values
!       Lscale = Lscale_weight*Lscale_pert_1 + (1.0_core_rknd-Lscale_weight)*Lscale_pert_2
! Un-weighted average of just the perturbed values
          Lscale = 0.5_core_rknd*( Lscale_pert_1 + Lscale_pert_2 )
        else
          Lscale = (1.0_core_rknd/3.0_core_rknd) * ( Lscale + Lscale_pert_1 + Lscale_pert_2 )
        end if
      end if
!----------------------------------------------------------------
! Dissipation time
!----------------------------------------------------------------
! Vince Larson replaced the cutoff of em_min by w_tol**2.  7 Jul 2007
!     This is to prevent tau from being too large (producing little damping)
!     in stably stratified layers with little turbulence.
!       sqrt_em_zt = SQRT( MAX( em_min, zm2zt( em ) ) )
!       tau_zt = MIN( Lscale / sqrt_em_zt, taumax )
!       tau_zm &
!       = MIN( ( zt2zm( Lscale ) / SQRT( MAX( em_min, em ) ) ), taumax )
!   Addition by Brian:  Model constant em_min is now set to (3/2)*w_tol_sqd.
!                       Thus, em_min can replace w_tol_sqd here.
      sqrt_em_zt = SQRT( MAX( em_min, zm2zt( em ) ) )
      tau_zt = MIN( Lscale / sqrt_em_zt, taumax )
      tau_zm = MIN( ( MAX( zt2zm( Lscale ), zero_threshold )  & 
                     / SQRT( MAX( em_min, em ) ) ), taumax )
! End Vince Larson's replacement.
! Determine the static stability corrected version of tau_zm
! Create a damping time scale that is more strongly damped at the
! altitudes where the Brunt-Vaisala frequency (N^2) is large.
      tau_N2_zm = tau_zm / calc_stability_correction( thlm, Lscale, em )
! Modification to damp noise in stable region
! Vince Larson commented out because it may prevent turbulence from
!    initiating in unstable regions.  7 Jul 2007
!       do k = 1, gr%nz
!         if ( wp2(k) <= 0.005_core_rknd ) then
!           tau_zt(k) = taumin
!           tau_zm(k) = taumin
!         end if
!       end do
! End Vince Larson's commenting.
!----------------------------------------------------------------
! Eddy diffusivity coefficient
!----------------------------------------------------------------
! c_K is 0.548 usually (Duynkerke and Driedonks 1987)
! CLUBB uses a smaller value to better fit empirical data.
      Kh_zt = c_K * Lscale * sqrt_em_zt
      Kh_zm = c_K * max( zt2zm( Lscale ), zero_threshold )  & 
                  * sqrt( max( em, em_min ) )
      khzt(:) = Kh_zt(:)
      khzm(:) = Kh_zm(:)
      thlprcp_out(:) = thlprcp(:)
      qclvar(:) = rcp2_zt(:)
!----------------------------------------------------------------
! Set Surface variances
!----------------------------------------------------------------
! Surface variances should be set here, before the call to either
! advance_xp2_xpyp or advance_wp2_wp3.
! Surface effects should not be included with any case where the lowest
! level is not the ground level.  Brian Griffin.  December 22, 2005.
      if ( gr%zm(1) == sfc_elevation ) then
! Reflect surface varnce changes in budget
        if ( l_stats_samp ) then
          call stat_begin_update_pt( ithlp2_sf, 1,      &      ! intent(in)
           thlp2(1) / dt,    &                                 ! intent(in)
                                     stats_zm )                      ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_begin_update_pt( irtp2_sf, 1,       &      ! intent(in)
            rtp2(1) / dt,    &                                 ! intent(in)
                                     stats_zm )                      ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_begin_update_pt( irtpthlp_sf, 1,    &      ! intent(in)
            rtpthlp(1) / dt, &                                 ! intent(in)
                                     stats_zm )                      ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_begin_update_pt( iup2_sf, 1,        &      ! intent(in)
            up2(1) / dt,     &                                 ! intent(in)
                                     stats_zm )                      ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_begin_update_pt( ivp2_sf, 1,        &      ! intent(in)
            vp2(1) / dt,     &                                 ! intent(in)
                                     stats_zm )                      ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_begin_update_pt( iwp2_sf, 1,        &      ! intent(in)
            wp2(1) / dt,     &                                 ! intent(in)
                                     stats_zm )                      ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
        end if
        call surface_varnce( upwp_sfc, vpwp_sfc, wpthlp_sfc, wprtp_sfc, &      ! intent(in)
                             um(2), vm(2), Lscale_up(2), wpsclrp_sfc,   &      ! intent(in)
                             wp2(1), up2(1), vp2(1),                    &      ! intent(out)
                             thlp2(1), rtp2(1), rtpthlp(1), err_code_surface,& ! intent(out)
                             sclrp2(1,1:sclr_dim),                      &      ! intent(out)
                             sclrprtp(1,1:sclr_dim),                    &      ! intent(out)
                             sclrpthlp(1,1:sclr_dim) )                         ! intent(out) ! intent(in)
! intent(in)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
! intent(out)
        if ( fatal_error( err_code_surface ) ) then
          call report_error( err_code_surface ) ! intent(in) ! intent(in)
          err_code = err_code_surface
        end if
! Update surface stats
        if ( l_stats_samp ) then
          call stat_end_update_pt( ithlp2_sf, 1, &                ! intent(in)
            thlp2(1) / dt, &                                      ! intent(in)
                                   stats_zm )                           ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_end_update_pt( irtp2_sf, 1, &                 ! intent(in)
            rtp2(1) / dt, &                                       ! intent(in)
                                   stats_zm )                           ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_end_update_pt( irtpthlp_sf, 1, &              ! intent(in)
            rtpthlp(1) / dt, &                                    ! intent(in)
                                   stats_zm )                           ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_end_update_pt( iup2_sf, 1, &                  ! intent(in)
            up2(1) / dt, &                                        ! intent(in)
                                   stats_zm )                           ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_end_update_pt( ivp2_sf, 1, &                  ! intent(in)
            vp2(1) / dt, &                                        ! intent(in)
                                   stats_zm )                           ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
          call stat_end_update_pt( iwp2_sf, 1, &                  ! intent(in)
            wp2(1) / dt, &                                        ! intent(in)
                                   stats_zm )                           ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
        end if
      else
! Variances for cases where the lowest level is not at the surface.
! Eliminate surface effects on lowest level variances.
        wp2(1)     = w_tol_sqd
        up2(1)     = w_tol_sqd
        vp2(1)     = w_tol_sqd
        thlp2(1)   = thl_tol**2
        rtp2(1)    = rt_tol**2
        rtpthlp(1) = 0.0_core_rknd
        do i = 1, sclr_dim, 1
          sclrp2(1,i)    = 0.0_core_rknd
          sclrprtp(1,i)  = 0.0_core_rknd
          sclrpthlp(1,i) = 0.0_core_rknd
        end do
      end if ! gr%zm(1) == sfc_elevation ! gr%zm(1) == sfc_elevation
!#######################################################################
!############## ADVANCE PROGNOSTIC VARIABLES ONE TIMESTEP ##############
!#######################################################################
! Store the saturation mixing ratio for output purposes.  Brian
! Compute rsat if either rsat or rel_humidity is to be saved.  ldgrant
      if ( ( irsat > 0 ) .or. ( irel_humidity > 0 ) ) then
        rsat = sat_mixrat_liq( p_in_Pa, thlm2T_in_K( thlm, exner, rcm ) )
      end if
      if ( l_stats_samp ) then
        call stat_update_var( irvm, rtm - rcm, & !intent(in)
                              stats_zt )               !intent(inout) !intent(in)
!intent(inout)
! Output relative humidity (q/q∗ where q∗ is the saturation mixing ratio over liquid)
! Added an extra check for irel_humidity > 0; otherwise, if both irsat = 0 and
! irel_humidity = 0, rsat is not computed, leading to a floating-point exception
! when stat_update_var is called for rel_humidity.  ldgrant
        if ( irel_humidity > 0 ) then
          call stat_update_var( irel_humidity, (rtm - rcm) / rsat, & !intent(in)
                                stats_zt)                                  !intent(inout) !intent(in)
!intent(inout)
        end if ! irel_humidity > 0 ! irel_humidity > 0
      end if ! l_stats_samp ! l_stats_samp
!----------------------------------------------------------------
! Advance rtm/wprtp and thlm/wpthlp one time step
!----------------------------------------------------------------
      if ( l_call_pdf_closure_twice ) then
        w_1_zm        = pdf_params_zm%w_1
        w_2_zm        = pdf_params_zm%w_2
        varnce_w_1_zm = pdf_params_zm%varnce_w_1
        varnce_w_2_zm = pdf_params_zm%varnce_w_2
        mixt_frac_zm = pdf_params_zm%mixt_frac
      else
        w_1_zm        = zt2zm( pdf_params%w_1 )
        w_2_zm        = zt2zm( pdf_params%w_2 )
        varnce_w_1_zm = zt2zm( pdf_params%varnce_w_1 )
        varnce_w_2_zm = zt2zm( pdf_params%varnce_w_2 )
        mixt_frac_zm  = zt2zm( pdf_params%mixt_frac )
      end if
! Determine stability correction factor
      stability_correction = calc_stability_correction( thlm, Lscale, em ) ! In ! In
      if ( l_stats_samp ) then
        call stat_update_var( istability_correction, stability_correction, & ! In
                              stats_zm ) ! In/Out ! In
! In/Out
      end if
! Here we determine if we're using tau_zm or tau_N2_zm, which is tau
! that has been stability corrected for stably stratified regions.
! -dschanen 7 Nov 2014
      if ( l_stability_correct_tau_zm ) then
        tau_N2_zm = tau_zm / stability_correction
        tau_C6_zm = tau_N2_zm
        tau_C1_zm = tau_N2_zm
      else
        tau_N2_zm = unused_var 
        tau_C6_zm = tau_zm
        tau_C1_zm = tau_zm
      end if ! l_stability_correction ! l_stability_correction
      call advance_xm_wpxp( dt, sigma_sqd_w, wm_zm, wm_zt, wp2,     & ! intent(in)
                            Lscale, wp3_on_wp2, wp3_on_wp2_zt, Kh_zt, Kh_zm, & ! intent(in)
                            tau_C6_zm, Skw_zm, rtpthvp, rtm_forcing,  & ! intent(in)
                            wprtp_forcing, rtm_ref, thlpthvp,         & ! intent(in)
                            thlm_forcing, wpthlp_forcing, thlm_ref,   & ! intent(in)
                            rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm,    & ! intent(in)
                            invrs_rho_ds_zt, thv_ds_zm, rtp2, thlp2,  & ! intent(in)
                            w_1_zm, w_2_zm, varnce_w_1_zm, varnce_w_2_zm, & ! intent(in)
                            mixt_frac_zm, l_implemented, em,          & ! intent(in)
                            sclrpthvp, sclrm_forcing, sclrp2,         & ! intent(in)
                            rtm, wprtp, thlm, wpthlp,                 & ! intent(inout)
                            err_code,                                 & ! intent(inout)
                            sclrm, wpsclrp                            ) ! intent(inout) ! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
! This code won't work unless rtm >= 0 !!!
! We do not clip rcm_in_layer because rcm_in_layer only influences
! radiation, and we do not want to bother recomputing it.  6 Aug 2009
      call clip_rcm( rtm, 'rtm < rcm in advance_xm_wpxp',             & ! intent(in)
                     rcm )                                              ! intent(inout) ! intent(in)
! intent(inout)
!----------------------------------------------------------------
! Compute some of the variances and covariances.  These include the variance of
! total water (rtp2), liquid potential termperature (thlp2), their
! covariance (rtpthlp), and the variance of horizontal wind (up2 and vp2).
! The variance of vertical velocity is computed later.
!----------------------------------------------------------------
! We found that certain cases require a time tendency to run
! at shorter timesteps so these are prognosed now.
! We found that if we call advance_xp2_xpyp first, we can use a longer timestep.
      call advance_xp2_xpyp( tau_zm, wm_zm, rtm, wprtp, thlm,       & ! intent(in)
                             wpthlp, wpthvp, um, vm, wp2, wp2_zt,     & ! intent(in)
                             wp3, upwp, vpwp, sigma_sqd_w, Skw_zm,    & ! intent(in)
                             Kh_zt, rtp2_forcing, thlp2_forcing,      & ! intent(in)
                             rtpthlp_forcing, rho_ds_zm, rho_ds_zt,   & ! intent(in)
                             invrs_rho_ds_zm, thv_ds_zm,              & ! intent(in)
                             Lscale, wp3_on_wp2, wp3_on_wp2_zt,       & ! intent(in)
                             l_iter_xp2_xpyp, dt,                     & ! intent(in)
                             sclrm, wpsclrp,                          & ! intent(in)
                             rtp2, thlp2, rtpthlp, up2, vp2,          & ! intent(inout)
                             err_code,                                & ! intent(inout)
                             sclrp2, sclrprtp, sclrpthlp              ) ! intent(inout) ! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
!----------------------------------------------------------------
! Covariance clipping for wprtp, wpthlp, wpsclrp, upwp, and vpwp
! after subroutine advance_xp2_xpyp updated xp2.
!----------------------------------------------------------------
      wprtp_cl_num   = 2 ! Second instance of w'r_t' clipping. ! Second instance of w'r_t' clipping.
      wpthlp_cl_num  = 2 ! Second instance of w'th_l' clipping. ! Second instance of w'th_l' clipping.
      wpsclrp_cl_num = 2 ! Second instance of w'sclr' clipping. ! Second instance of w'sclr' clipping.
      upwp_cl_num    = 1 ! First instance of u'w' clipping. ! First instance of u'w' clipping.
      vpwp_cl_num    = 1 ! First instance of v'w' clipping. ! First instance of v'w' clipping.
      call clip_covars_denom( dt, rtp2, thlp2, up2, vp2, wp2,           & ! intent(in)
                              sclrp2, wprtp_cl_num, wpthlp_cl_num,      & ! intent(in)
                              wpsclrp_cl_num, upwp_cl_num, vpwp_cl_num, & ! intent(in)
                              wprtp, wpthlp, upwp, vpwp, wpsclrp )        ! intent(inout) ! intent(in)
! intent(in)
! intent(in)
! intent(inout)
!----------------------------------------------------------------
! Advance 2nd and 3rd order moment of vertical velocity (wp2 / wp3)
! by one timestep
!----------------------------------------------------------------
      call advance_wp2_wp3 &
           ( dt, sfc_elevation, sigma_sqd_w, wm_zm, wm_zt,      & ! intent(in)
             a3_coef, a3_coef_zt, wp3_on_wp2,                   & ! intent(in)
             wpthvp, wp2thvp, um, vm, upwp, vpwp,               & ! intent(in)
             up2, vp2, Kh_zm, Kh_zt, tau_zm, tau_zt, tau_C1_zm, & ! intent(in)
             Skw_zm, Skw_zt, rho_ds_zm, rho_ds_zt,              & ! intent(in)
             invrs_rho_ds_zm, invrs_rho_ds_zt, radf,            & ! intent(in)
             thv_ds_zm, thv_ds_zt, pdf_params%mixt_frac,        & ! intent(in)
             wp2, wp3, wp3_zm, wp2_zt, err_code               )  ! intent(inout)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
!----------------------------------------------------------------
! Covariance clipping for wprtp, wpthlp, wpsclrp, upwp, and vpwp
! after subroutine advance_wp2_wp3 updated wp2.
!----------------------------------------------------------------
      wprtp_cl_num   = 3 ! Third instance of w'r_t' clipping. ! Third instance of w'r_t' clipping.
      wpthlp_cl_num  = 3 ! Third instance of w'th_l' clipping. ! Third instance of w'th_l' clipping.
      wpsclrp_cl_num = 3 ! Third instance of w'sclr' clipping. ! Third instance of w'sclr' clipping.
      upwp_cl_num    = 2 ! Second instance of u'w' clipping. ! Second instance of u'w' clipping.
      vpwp_cl_num    = 2 ! Second instance of v'w' clipping. ! Second instance of v'w' clipping.
      call clip_covars_denom( dt, rtp2, thlp2, up2, vp2, wp2,           & ! intent(in)
                              sclrp2, wprtp_cl_num, wpthlp_cl_num,      & ! intent(in)
                              wpsclrp_cl_num, upwp_cl_num, vpwp_cl_num, & ! intent(in)
                              wprtp, wpthlp, upwp, vpwp, wpsclrp )        ! intent(inout) ! intent(in)
! intent(in)
! intent(in)
! intent(inout)
!----------------------------------------------------------------
! Advance the horizontal mean of the wind in the x-y directions
! (i.e. um, vm) and the mean of the eddy-diffusivity scalars
! (i.e. edsclrm) by one time step
!----------------------------------------------------------------i
      Km_zm = Kh_zm * c_K10
      if (do_expldiff) then
        edsclrm(:,edsclr_dim-1)=thlm(:)
	edsclrm(:,edsclr_dim)=rtm(:)
      endif      
      call advance_windm_edsclrm( dt, wm_zt, Km_zm, ug, vg, um_ref, vm_ref, & ! intent(in)
                                  wp2, up2, vp2, um_forcing, vm_forcing,    & ! intent(in)
                                  edsclrm_forcing,                          & ! intent(in)
                                  rho_ds_zm, invrs_rho_ds_zt,               & ! intent(in)
                                  fcor, l_implemented,                      & ! intent(in)
                                  um, vm, edsclrm,                          & ! intent(inout)
                                  upwp, vpwp, wpedsclrp,                    & ! intent(inout)
                                  err_code )                                  ! intent(inout) ! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
      call pvertinterp(gr%nz, p_in_Pa, 70000.0_core_rknd, thlm, thlm700)
      call pvertinterp(gr%nz, p_in_Pa, 100000.0_core_rknd, thlm, thlm1000)			  
      if (do_expldiff .and. thlm700 - thlm1000 .lt. 20.0_core_rknd) then
        thlm(:) = edsclrm(:,edsclr_dim-1)
	rtm(:) = edsclrm(:,edsclr_dim)
      endif	
      do ixind=1,edsclr_dim
        call fill_holes_vertical(2,0.0_core_rknd,"zt",rho_ds_zt,rho_ds_zm,edsclrm(:,ixind))
      enddo  				  
!#######################################################################
!#############            ACCUMULATE STATISTICS            #############
!#######################################################################
      if ( l_stats_samp ) then
        call stat_end_update( iwp2_bt, wp2 / dt, &                ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( ivp2_bt, vp2 / dt,&                 ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( iup2_bt, up2 / dt, &                ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( iwprtp_bt, wprtp / dt, &            ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( iwpthlp_bt, wpthlp / dt, &          ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( irtp2_bt, rtp2 / dt, &              ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( ithlp2_bt, thlp2 / dt, &            ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( irtpthlp_bt, rtpthlp / dt, &        ! intent(in)
                              stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( irtm_bt, rtm / dt, &                ! intent(in)
                              stats_zt )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( ithlm_bt, thlm / dt, &              ! intent(in)
                              stats_zt )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( ium_bt, um / dt, &                  ! intent(in)
                              stats_zt )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( ivm_bt, vm / dt, &                  ! intent(in)
                              stats_zt )                                ! intent(inout) ! intent(in)
! intent(inout)
        call stat_end_update( iwp3_bt, wp3 / dt, &                ! intent(in)
                              stats_zt )                                ! intent(inout) ! intent(in)
! intent(inout)
      end if ! l_stats_samp ! l_stats_samp
      if ( iwpthlp_zt > 0 ) then
        wpthlp_zt  = zm2zt( wpthlp )
      end if
      if ( iwprtp_zt > 0 ) then
        wprtp_zt   = zm2zt( wprtp )
      end if
      if ( iup2_zt > 0 ) then
        up2_zt = max( zm2zt( up2 ), w_tol_sqd )
      end if
      if (ivp2_zt > 0 ) then
        vp2_zt = max( zm2zt( vp2 ), w_tol_sqd )
      end if
      if ( iupwp_zt > 0 ) then
        upwp_zt = zm2zt( upwp )
      end if
      if ( ivpwp_zt > 0 ) then
        vpwp_zt = zm2zt( vpwp )
      end if
      call stats_accumulate & 
           ( um, vm, upwp, vpwp, up2, vp2,                          & ! intent(in)
             thlm, rtm, wprtp, wpthlp,                              & ! intent(in)
             wp2, wp3, rtp2, thlp2, rtpthlp,                        & ! intent(in)
             p_in_Pa, exner, rho, rho_zm,                           & ! intent(in)
             rho_ds_zm, rho_ds_zt, thv_ds_zm,                       & ! intent(in)
             thv_ds_zt, wm_zt, wm_zm, rcm, wprcp, rc_coef,          & ! intent(in)
             rcm_zm, rtm_zm, thlm_zm, cloud_frac, ice_supersat_frac,& ! intent(in)
             cloud_frac_zm, ice_supersat_frac_zm, rcm_in_layer,     & ! intent(in)
             cloud_cover, sigma_sqd_w, pdf_params,                  & ! intent(in)
             sclrm, sclrp2, sclrprtp, sclrpthlp, sclrm_forcing,     & ! intent(in)
             wpsclrp, edsclrm, edsclrm_forcing                  )     ! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
      if ( clubb_at_least_debug_level( 2 ) ) then
        call parameterization_check & 
             ( thlm_forcing, rtm_forcing, um_forcing, vm_forcing, & ! intent(in)
               wm_zm, wm_zt, p_in_Pa, rho_zm, rho, exner,         & ! intent(in)
               rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm,             & ! intent(in)
               invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt,             & ! intent(in)
               wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc,         & ! intent(in)
               um, upwp, vm, vpwp, up2, vp2,                      & ! intent(in)
               rtm, wprtp, thlm, wpthlp,                          & ! intent(in)
               wp2, wp3, rtp2, thlp2, rtpthlp,                    & ! intent(in)
               "end of ",                                  & ! intent(in)
               wpsclrp_sfc, wpedsclrp_sfc,                        & ! intent(in)
               sclrm, wpsclrp, sclrp2, sclrprtp, sclrpthlp,       & ! intent(in)
               sclrm_forcing, edsclrm, edsclrm_forcing,           & ! intent(in)
               err_code ) ! intent(inout)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
      end if
      if ( l_stats .and. l_stats_samp ) then
! Spurious source will only be calculated if rtm_ma and thlm_ma are zero.
! Therefore, wm must be zero or l_implemented must be true.
        if ( l_implemented .or. ( all( wm_zt == 0._core_rknd ) .and. &
            all( wm_zm == 0._core_rknd ) ) ) then
! Calculate the spurious source for rtm
          rtm_flux_top = rho_ds_zm(gr%nz) * wprtp(gr%nz)
          if ( .not. l_host_applies_sfc_fluxes ) then
            rtm_flux_sfc = rho_ds_zm(1) * wprtp_sfc
          else
            rtm_flux_sfc = 0.0_core_rknd
          end if
          rtm_integral_after  &
          = vertical_integral( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                               rtm(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
          rtm_integral_forcing  &
          = vertical_integral( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                               rtm_forcing(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
          rtm_spur_src  &
          = calculate_spurious_source( rtm_integral_after, &
                                       rtm_integral_before, &
                                       rtm_flux_top, rtm_flux_sfc, &
                                       rtm_integral_forcing, &
                                       dt )
! Calculate the spurious source for thlm
          thlm_flux_top = rho_ds_zm(gr%nz) * wpthlp(gr%nz)
          if ( .not. l_host_applies_sfc_fluxes ) then
            thlm_flux_sfc = rho_ds_zm(1) * wpthlp_sfc
          else
            thlm_flux_sfc = 0.0_core_rknd
          end if
          thlm_integral_after  &
          = vertical_integral( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                               thlm(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
          thlm_integral_forcing  &
          = vertical_integral( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                               thlm_forcing(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
          thlm_spur_src  &
          = calculate_spurious_source( thlm_integral_after, &
                                       thlm_integral_before, &
                                       thlm_flux_top, thlm_flux_sfc, &
                                       thlm_integral_forcing, &
                                       dt )
        else ! If l_implemented is false, we don't want spurious source output ! If l_implemented is false, we don't want spurious source output
          rtm_spur_src = -9999.0_core_rknd
          thlm_spur_src = -9999.0_core_rknd
        end if
! Write the var to stats
        call stat_update_var_pt( irtm_spur_src, 1, rtm_spur_src,   & ! intent(in)
                                 stats_sfc )                               ! intent(inout) ! intent(in)
! intent(inout)
        call stat_update_var_pt( ithlm_spur_src, 1, thlm_spur_src, & ! intent(in)
                                 stats_sfc )                               ! intent(inout) ! intent(in)
! intent(inout)
      end if
      return
        END SUBROUTINE advance_clubb_core
!-----------------------------------------------------------------------

!----------------------------------------------------------------------------

!-----------------------------------------------------------------------

        SUBROUTINE trapezoidal_rule_zt(l_call_pdf_closure_twice, wprtp2, wpthlp2, wprtpthlp, cloud_frac, ice_supersat_frac, rcm, &
        wp2thvp, wpsclrprtp, wpsclrp2, wpsclrpthlp, pdf_params, wprtp2_zm, wpthlp2_zm, wprtpthlp_zm, cloud_frac_zm, &
        ice_supersat_frac_zm, rcm_zm, wp2thvp_zm, wpsclrprtp_zm, wpsclrp2_zm, wpsclrpthlp_zm, pdf_params_zm)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
! intent(inout)
!
! Description:
!   This subroutine takes the output variables on the thermo.
!   grid and either: interpolates them to the momentum grid, or uses the
!   values output from the second call to pdf_closure on momentum levels if
!   l_call_pdf_closure_twice is true.  It then calls the function
!   trapezoid_zt to recompute the variables on the thermo. grid.
!
!   ldgrant June 2009
!
! Note:
!   The argument variables in the last 5 lines of the subroutine
!   (wprtp2_zm through pdf_params_zm) are declared intent(inout) because
!   if l_call_pdf_closure_twice is true, these variables will already have
!   values from pdf_closure on momentum levels and will not be altered in
!   this subroutine.  However, if l_call_pdf_closure_twice is false, these
!   variables will not have values yet and will be interpolated to
!   momentum levels in this subroutine.
! References:
!   None
!-----------------------------------------------------------------------
            USE constants_clubb, ONLY: fstderr
! Constant(s)
            USE stats_variables, ONLY: l_stats
            USE stats_variables, ONLY: iwprtp2
            USE stats_variables, ONLY: iwpthlp2
            USE stats_variables, ONLY: iwprtpthlp
            USE stats_variables, ONLY: iwpsclrprtp
            USE stats_variables, ONLY: iwpsclrpthlp
            USE stats_variables, ONLY: iwpsclrp2
! Varibles
            USE grid_class, ONLY: gr
            USE grid_class, ONLY: zt2zm
! Variable
! Procedure
            USE parameters_model, ONLY: sclr_dim
! Number of passive scalar variables
            USE pdf_parameter_module, ONLY: pdf_parameter
! Derived data type
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Constant parameters
            LOGICAL, parameter :: l_apply_rule_to_pdf_params = .false.
! Apply the trapezoidal rule to pdf_params
! Input variables
            LOGICAL, intent(in) :: l_call_pdf_closure_twice
! Input/Output variables
! Thermodynamic level variables output from the first call to pdf_closure
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wpthlp2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wprtpthlp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wprtp2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: cloud_frac
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: ice_supersat_frac
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wp2thvp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: rcm
! w'rt'^2                   [m kg^2/kg^2]
! w'thl'^2                  [m K^2/s]
! w'rt'thl'                 [m kg K/kg s]
! Cloud Fraction            [-]
! Ice Cloud Fraction        [-]
! Liquid water mixing ratio [kg/kg]
! w'^2 th_v'                [m^2 K/s^2]
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrprtp
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrp2
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrpthlp
! w'sclr'rt'
! w'sclr'^2
! w'sclr'thl'
            TYPE(pdf_parameter), dimension(gr%nz), intent(inout) :: pdf_params
! PDF parameters [units vary]
! Thermo. level variables brought to momentum levels either by
! interpolation (in subroutine trapezoidal_rule_zt) or by
! the second call to pdf_closure (in subroutine advance_clubb_core)
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wprtp2_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wpthlp2_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: ice_supersat_frac_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wprtpthlp_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: cloud_frac_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: rcm_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wp2thvp_zm
! w'rt'^2 on momentum grid                   [m kg^2/kg^2]
! w'thl'^2 on momentum grid                  [m K^2/s]
! w'rt'thl' on momentum grid                 [m kg K/kg s]
! Cloud Fraction on momentum grid            [-]
! Ice Cloud Fraction on momentum grid        [-]
! Liquid water mixing ratio on momentum grid [kg/kg]
! w'^2 th_v' on momentum grid                [m^2 K/s^2]
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrpthlp_zm
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrp2_zm
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrprtp_zm
! w'sclr'rt' on momentum grid
! w'sclr'^2 on momentum grid
! w'sclr'thl' on momentum grid
            TYPE(pdf_parameter), dimension(gr%nz), intent(inout) :: pdf_params_zm
! PDF parameters on momentum grid [units vary]
! Local variables
! Components of PDF_parameters on the momentum grid (_zm) and on the thermo. grid (_zt)
            REAL(KIND=core_rknd), dimension(gr%nz) :: w_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: w_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_w_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_w_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rt_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rt_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_rt_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_rt_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: crt_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: w_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: w_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_w_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_w_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rt_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rt_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_rt_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_rt_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: crt_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: crt_2_zm
! Mean of w for 1st normal distribution                 [m/s]
! Mean of w for 1st normal distribution                 [m/s]
! Mean of w for 2nd normal distribution                 [m/s]
! Mean of w for 2nd normal distribution                 [m/s]
! Variance of w for 1st normal distribution         [m^2/s^2]
! Variance of w for 1st normal distribution         [m^2/s^2]
! Variance of w for 2nd normal distribution         [m^2/s^2]
! Variance of w for 2nd normal distribution         [m^2/s^2]
! Mean of r_t for 1st normal distribution             [kg/kg]
! Mean of r_t for 1st normal distribution             [kg/kg]
! Mean of r_t for 2nd normal distribution             [kg/kg]
! Mean of r_t for 2nd normal distribution             [kg/kg]
! Variance of r_t for 1st normal distribution     [kg^2/kg^2]
! Variance of r_t for 1st normal distribution     [kg^2/kg^2]
! Variance of r_t for 2nd normal distribution     [kg^2/kg^2]
! Variance of r_t for 2nd normal distribution     [kg^2/kg^2]
! Coefficient for s'                                      [-]
! Coefficient for s'                                      [-]
! Coefficient for s'                                      [-]
            REAL(KIND=core_rknd), dimension(gr%nz) :: crt_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: cthl_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: cthl_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: thl_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: thl_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_thl_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_thl_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: cthl_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: cthl_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: thl_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: thl_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_thl_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: varnce_thl_2_zm
! Coefficient for s'                                      [-]
! Coefficient for s'                                    [1/K]
! Coefficient for s'                                    [1/K]
! Coefficient for s'                                    [1/K]
! Coefficient for s'                                    [1/K]
! Mean of th_l for 1st normal distribution                [K]
! Mean of th_l for 1st normal distribution                [K]
! Mean of th_l for 2nd normal distribution                [K]
! Mean of th_l for 2nd normal distribution
! Variance of th_l for 1st normal distribution          [K^2]
! Variance of th_l for 1st normal distribution          [K^2]
! Variance of th_l for 2nd normal distribution          [K^2]
! Variance of th_l for 2nd normal distribution          [K^2]
            REAL(KIND=core_rknd), dimension(gr%nz) :: mixt_frac_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rsatl_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rsatl_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: chi_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: chi_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: mixt_frac_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rc_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rsatl_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rsatl_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: cloud_frac_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: chi_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: chi_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_chi_1_zm
! Weight of 1st normal distribution (Sk_w dependent)      [-]
! Weight of 1st normal distribution (Sk_w dependent)      [-]
! Mean of r_c for 1st normal distribution             [kg/kg]
! Mean of r_c for 1st normal distribution             [kg/kg]
! Mean of r_c for 2nd normal distribution             [kg/kg]
! Mean of r_c for 2nd normal distribution             [kg/kg]
! Mean of r_sl for 1st normal distribution            [kg/kg]
! Mean of r_sl for 1st normal distribution            [kg/kg]
! Mean of r_sl for 2nd normal distribution            [kg/kg]
! Mean of r_sl for 2nd normal distribution            [kg/kg]
! Cloud fraction for 1st normal distribution              [-]
! Cloud fraction for 1st normal distribution              [-]
! Cloud fraction for 2nd normal distribution              [-]
! Cloud fraction for 2nd normal distribution              [-]
! Mean of chi(s) for 1st normal distribution               [kg/kg]
! Mean of chi(s) for 1st normal distribution               [kg/kg]
! Mean of chi(s) for 2nd normal distribution               [kg/kg]
! Mean of chi(s) for 2nd normal distribution               [kg/kg]
! Standard deviation of chi(s) for 1st normal distribution [kg/kg]
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_chi_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_chi_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_eta_1_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_eta_2_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: rrtthl_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: alpha_thl_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: alpha_rt_zt
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_chi_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_eta_1_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: stdev_eta_2_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: rrtthl_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: alpha_thl_zm
            REAL(KIND=core_rknd), dimension(gr%nz) :: alpha_rt_zm
! Standard deviation of chi(s) for 1st normal distribution [kg/kg]
! Standard deviation of chi(s) for 2nd normal distribution [kg/kg]
! Standard deviation of chi(s) for 2nd normal distribution [kg/kg]
! Standard deviation of eta(t) for 1st normal distribution [kg/kg]
! Standard deviation of eta(t) for 1st normal distribution [kg/kg]
! Standard deviation of eta(t) for 2nd normal distribution [kg/kg]
! Standard deviation of eta(t) for 2nd normal distribution [kg/kg]
! Within-a-normal correlation of r_t and th_l             [-]
! Within-a-normal correlation of r_t and th_l             [-]
! Factor relating to normalized variance for th_l         [-]
! Factor relating to normalized variance for th_l         [-]
! Factor relating to normalized variance for r_t          [-]
! Factor relating to normalized variance for r_t          [-]
            INTEGER :: i
!----------------------- Begin Code -----------------------------
! Store components of pdf_params in the locally declared variables
! We only apply the trapezoidal rule to these when
! l_apply_rule_to_pdf_params is true.  This is because when we apply the
! rule to the final result of pdf_closure rather than the intermediate
! results it can lead to an inconsistency in how we determine which
! PDF component a point is in and whether the point is in or out of cloud,
! which is turn will break the latin hypercube code that samples
! preferentially in cloud. -dschanen 13 Feb 2012
      if ( l_apply_rule_to_pdf_params ) then
        w_1_zt          = pdf_params%w_1
        w_2_zt          = pdf_params%w_2
        varnce_w_1_zt   = pdf_params%varnce_w_1
        varnce_w_2_zt   = pdf_params%varnce_w_2
        rt_1_zt         = pdf_params%rt_1
        rt_2_zt         = pdf_params%rt_2
        varnce_rt_1_zt  = pdf_params%varnce_rt_1
        varnce_rt_2_zt  = pdf_params%varnce_rt_2
        crt_1_zt        = pdf_params%crt_1
        crt_2_zt        = pdf_params%crt_2
        cthl_1_zt       = pdf_params%cthl_1
        cthl_2_zt       = pdf_params%cthl_2
        thl_1_zt        = pdf_params%thl_1
        thl_2_zt        = pdf_params%thl_2
        varnce_thl_1_zt = pdf_params%varnce_thl_1
        varnce_thl_2_zt = pdf_params%varnce_thl_2
        mixt_frac_zt   = pdf_params%mixt_frac
        rc_1_zt         = pdf_params%rc_1
        rc_2_zt         = pdf_params%rc_2
        rsatl_1_zt        = pdf_params%rsatl_1
        rsatl_2_zt        = pdf_params%rsatl_2
        cloud_frac_1_zt = pdf_params%cloud_frac_1
        cloud_frac_2_zt = pdf_params%cloud_frac_2
        chi_1_zt          = pdf_params%chi_1
        chi_2_zt          = pdf_params%chi_2
        stdev_chi_1_zt    = pdf_params%stdev_chi_1
        stdev_chi_2_zt    = pdf_params%stdev_chi_2
        stdev_eta_1_zt    = pdf_params%stdev_eta_1
        stdev_eta_2_zt    = pdf_params%stdev_eta_2
        rrtthl_zt      = pdf_params%rrtthl
        alpha_thl_zt   = pdf_params%alpha_thl
        alpha_rt_zt    = pdf_params%alpha_rt
      end if
! If l_call_pdf_closure_twice is true, the _zm variables already have
! values from the second call to pdf_closure in advance_clubb_core.
! If it is false, the variables are interpolated to the _zm levels.
      if ( l_call_pdf_closure_twice ) then
! Store, in locally declared variables, the pdf_params output
! from the second call to pdf_closure
        if ( l_apply_rule_to_pdf_params ) then
          w_1_zm          = pdf_params_zm%w_1
          w_2_zm          = pdf_params_zm%w_2
          varnce_w_1_zm   = pdf_params_zm%varnce_w_1
          varnce_w_2_zm   = pdf_params_zm%varnce_w_2
          rt_1_zm         = pdf_params_zm%rt_1
          rt_2_zm         = pdf_params_zm%rt_2
          varnce_rt_1_zm  = pdf_params_zm%varnce_rt_1
          varnce_rt_2_zm  = pdf_params_zm%varnce_rt_2
          crt_1_zm        = pdf_params_zm%crt_1
          crt_2_zm        = pdf_params_zm%crt_2
          cthl_1_zm       = pdf_params_zm%cthl_1
          cthl_2_zm       = pdf_params_zm%cthl_2
          thl_1_zm        = pdf_params_zm%thl_1
          thl_2_zm        = pdf_params_zm%thl_2
          varnce_thl_1_zm = pdf_params_zm%varnce_thl_1
          varnce_thl_2_zm = pdf_params_zm%varnce_thl_2
          mixt_frac_zm   = pdf_params_zm%mixt_frac
          rc_1_zm         = pdf_params_zm%rc_1
          rc_2_zm         = pdf_params_zm%rc_2
          rsatl_1_zm        = pdf_params_zm%rsatl_1
          rsatl_2_zm        = pdf_params_zm%rsatl_2
          cloud_frac_1_zm = pdf_params_zm%cloud_frac_1
          cloud_frac_2_zm = pdf_params_zm%cloud_frac_2
          chi_1_zm          = pdf_params_zm%chi_1
          chi_2_zm          = pdf_params_zm%chi_2
          stdev_chi_1_zm    = pdf_params_zm%stdev_chi_1
          stdev_chi_2_zm    = pdf_params_zm%stdev_chi_2
          stdev_eta_1_zm    = pdf_params_zm%stdev_eta_1
          stdev_eta_2_zm    = pdf_params_zm%stdev_eta_2
          rrtthl_zm      = pdf_params_zm%rrtthl
          alpha_thl_zm   = pdf_params_zm%alpha_thl
          alpha_rt_zm    = pdf_params_zm%alpha_rt
        end if
      else
! Interpolate thermodynamic variables to the momentum grid.
! Since top momentum level is higher than top thermo. level,
! set variables at top momentum level to 0.
        wprtp2_zm           = zt2zm( wprtp2 )
        wprtp2_zm(gr%nz) = 0.0_core_rknd
        wpthlp2_zm           = zt2zm( wpthlp2 )
        wpthlp2_zm(gr%nz) = 0.0_core_rknd
        wprtpthlp_zm           = zt2zm( wprtpthlp )
        wprtpthlp_zm(gr%nz)  = 0.0_core_rknd
        cloud_frac_zm          = zt2zm( cloud_frac )
        cloud_frac_zm(gr%nz) = 0.0_core_rknd
        ice_supersat_frac_zm   = zt2zm( ice_supersat_frac )
        ice_supersat_frac_zm(gr%nz) = 0.0_core_rknd
        rcm_zm                 = zt2zm( rcm )
        rcm_zm(gr%nz)        = 0.0_core_rknd
        wp2thvp_zm             = zt2zm( wp2thvp )
        wp2thvp_zm(gr%nz)    = 0.0_core_rknd
        do i = 1, sclr_dim
          wpsclrprtp_zm(:,i)        = zt2zm( wpsclrprtp(:,i) )
          wpsclrprtp_zm(gr%nz,i)  = 0.0_core_rknd
          wpsclrp2_zm(:,i)          = zt2zm( wpsclrp2(:,i) )
          wpsclrp2_zm(gr%nz,i)    = 0.0_core_rknd
          wpsclrpthlp_zm(:,i)       = zt2zm( wpsclrpthlp(:,i) )
          wpsclrpthlp_zm(gr%nz,i) = 0.0_core_rknd
        end do ! i = 1, sclr_dim ! i = 1, sclr_dim
        if ( l_apply_rule_to_pdf_params ) then
          w_1_zm                 = zt2zm( pdf_params%w_1 )
          w_1_zm(gr%nz)          = 0.0_core_rknd
          w_2_zm                 = zt2zm( pdf_params%w_2 )
          w_2_zm(gr%nz)          = 0.0_core_rknd
          varnce_w_1_zm          = zt2zm( pdf_params%varnce_w_1 )
          varnce_w_1_zm(gr%nz)   = 0.0_core_rknd
          varnce_w_2_zm          = zt2zm( pdf_params%varnce_w_2 )
          varnce_w_2_zm(gr%nz)   = 0.0_core_rknd
          rt_1_zm                = zt2zm( pdf_params%rt_1 )
          rt_1_zm(gr%nz)         = 0.0_core_rknd
          rt_2_zm                = zt2zm( pdf_params%rt_2 )
          rt_2_zm(gr%nz)         = 0.0_core_rknd
          varnce_rt_1_zm         = zt2zm( pdf_params%varnce_rt_1 )
          varnce_rt_1_zm(gr%nz)  = 0.0_core_rknd
          varnce_rt_2_zm         = zt2zm( pdf_params%varnce_rt_2 )
          varnce_rt_2_zm(gr%nz)  = 0.0_core_rknd
          crt_1_zm               = zt2zm( pdf_params%crt_1 )
          crt_1_zm(gr%nz)        = 0.0_core_rknd
          crt_2_zm               = zt2zm( pdf_params%crt_2 )
          crt_2_zm(gr%nz)        = 0.0_core_rknd
          cthl_1_zm              = zt2zm( pdf_params%cthl_1 )
          cthl_1_zm(gr%nz)       = 0.0_core_rknd
          cthl_2_zm              = zt2zm( pdf_params%cthl_2 )
          cthl_2_zm(gr%nz)       = 0.0_core_rknd
          thl_1_zm               = zt2zm( pdf_params%thl_1 )
          thl_1_zm(gr%nz)        = 0.0_core_rknd
          thl_2_zm               = zt2zm( pdf_params%thl_2 )
          thl_2_zm(gr%nz)        = 0.0_core_rknd
          varnce_thl_1_zm        = zt2zm( pdf_params%varnce_thl_1 )
          varnce_thl_1_zm(gr%nz) = 0.0_core_rknd
          varnce_thl_2_zm        = zt2zm( pdf_params%varnce_thl_2 )
          varnce_thl_2_zm(gr%nz) = 0.0_core_rknd
          mixt_frac_zm          = zt2zm( pdf_params%mixt_frac )
          mixt_frac_zm(gr%nz)   = 0.0_core_rknd
          rc_1_zm                = zt2zm( pdf_params%rc_1 )
          rc_1_zm(gr%nz)         = 0.0_core_rknd
          rc_2_zm                = zt2zm( pdf_params%rc_2 )
          rc_2_zm(gr%nz)         = 0.0_core_rknd
          rsatl_1_zm               = zt2zm( pdf_params%rsatl_1 )
          rsatl_1_zm(gr%nz)        = 0.0_core_rknd
          rsatl_2_zm               = zt2zm( pdf_params%rsatl_2 )
          rsatl_2_zm(gr%nz)        = 0.0_core_rknd
          cloud_frac_1_zm        = zt2zm( pdf_params%cloud_frac_1 )
          cloud_frac_1_zm(gr%nz) = 0.0_core_rknd
          cloud_frac_2_zm        = zt2zm( pdf_params%cloud_frac_2 )
          cloud_frac_2_zm(gr%nz) = 0.0_core_rknd
          chi_1_zm                 = zt2zm( pdf_params%chi_1 )
          chi_1_zm(gr%nz)          = 0.0_core_rknd
          chi_2_zm                 = zt2zm( pdf_params%chi_2 )
          chi_2_zm(gr%nz)          = 0.0_core_rknd
          stdev_chi_1_zm           = zt2zm( pdf_params%stdev_chi_1 )
          stdev_chi_1_zm(gr%nz)    = 0.0_core_rknd
          stdev_chi_2_zm           = zt2zm( pdf_params%stdev_chi_2 )
          stdev_chi_2_zm(gr%nz)    = 0.0_core_rknd
          stdev_eta_1_zm           = zt2zm( pdf_params%stdev_eta_1 )
          stdev_eta_1_zm(gr%nz)    = 0.0_core_rknd
          stdev_eta_2_zm           = zt2zm( pdf_params%stdev_eta_2 )
          stdev_eta_2_zm(gr%nz)    = 0.0_core_rknd
          rrtthl_zm             = zt2zm( pdf_params%rrtthl )
          rrtthl_zm(gr%nz)      = 0.0_core_rknd
          alpha_thl_zm          = zt2zm( pdf_params%alpha_thl )
          alpha_thl_zm(gr%nz)   = 0.0_core_rknd
          alpha_rt_zm           = zt2zm( pdf_params%alpha_rt )
          alpha_rt_zm(gr%nz)    = 0.0_core_rknd
        end if
      end if ! l_call_pdf_closure_twice ! l_call_pdf_closure_twice
      if ( l_stats ) then
! Use the trapezoidal rule to recompute the variables on the stats_zt level
        if ( iwprtp2 > 0 ) then
          wprtp2     = trapezoid_zt( wprtp2, wprtp2_zm )
        end if
        if ( iwpthlp2 > 0 ) then
          wpthlp2    = trapezoid_zt( wpthlp2, wpthlp2_zm )
        end if
        if ( iwprtpthlp > 0 ) then
          wprtpthlp  = trapezoid_zt( wprtpthlp, wprtpthlp_zm )
        end if
        do i = 1, sclr_dim
          if ( iwpsclrprtp(i) > 0 ) then
            wpsclrprtp(:,i)  = trapezoid_zt( wpsclrprtp(:,i), wpsclrprtp_zm(:,i) )
          end if
          if ( iwpsclrpthlp(i) > 0 ) then
            wpsclrpthlp(:,i) = trapezoid_zt( wpsclrpthlp(:,i), wpsclrpthlp_zm(:,i) )
          end if
          if ( iwpsclrp2(i) > 0 ) then
            wpsclrp2(:,i)    = trapezoid_zt( wpsclrp2(:,i), wpsclrp2_zm(:,i) )
          end if
        end do ! i = 1, sclr_dim ! i = 1, sclr_dim
      end if ! l_stats ! l_stats
      cloud_frac = trapezoid_zt( cloud_frac, cloud_frac_zm )
      ice_supersat_frac = trapezoid_zt( ice_supersat_frac, ice_supersat_frac_zm )
      rcm        = trapezoid_zt( rcm, rcm_zm )
      wp2thvp    = trapezoid_zt( wp2thvp, wp2thvp_zm )
      if ( l_apply_rule_to_pdf_params ) then
! Note: this code makes PDF component cloud water mixing ratios and
!       cloud fractions inconsistent with the PDF.  Other parts of
!       CLUBB require PDF component cloud fractions to remain
!       consistent with the PDF.  This code needs to be refactored
!       so that cloud_frac_1 and cloud_frac_2 are preserved.
        write(fstderr,*) "The code in l_apply_rule_to_pdf_params does not " &
                         // "preserve cloud_frac_1 and cloud_frac_2 in a " &
                         // "manner consistent with the PDF as required " &
                         // "by other parts of CLUBB."
        stop "Please refactor before continuing."
        pdf_params%w_1          = trapezoid_zt( w_1_zt, w_1_zm )
        pdf_params%w_2          = trapezoid_zt( w_2_zt, w_2_zm )
        pdf_params%varnce_w_1   = trapezoid_zt( varnce_w_1_zt, varnce_w_1_zm )
        pdf_params%varnce_w_2   = trapezoid_zt( varnce_w_2_zt, varnce_w_2_zm )
        pdf_params%rt_1         = trapezoid_zt( rt_1_zt, rt_1_zm )
        pdf_params%rt_2         = trapezoid_zt( rt_2_zt, rt_2_zm )
        pdf_params%varnce_rt_1  = trapezoid_zt( varnce_rt_1_zt, varnce_rt_1_zm )
        pdf_params%varnce_rt_2  = trapezoid_zt( varnce_rt_2_zt, varnce_rt_2_zm )
        pdf_params%crt_1        = trapezoid_zt( crt_1_zt, crt_1_zm )
        pdf_params%crt_2        = trapezoid_zt( crt_2_zt, crt_2_zm )
        pdf_params%cthl_1       = trapezoid_zt( cthl_1_zt, cthl_1_zm )
        pdf_params%cthl_2       = trapezoid_zt( cthl_2_zt, cthl_2_zm )
        pdf_params%thl_1        = trapezoid_zt( thl_1_zt, thl_1_zm )
        pdf_params%thl_2        = trapezoid_zt( thl_2_zt, thl_2_zm )
        pdf_params%varnce_thl_1 = trapezoid_zt( varnce_thl_1_zt, varnce_thl_1_zm )
        pdf_params%varnce_thl_2 = trapezoid_zt( varnce_thl_2_zt, varnce_thl_2_zm )
        pdf_params%mixt_frac   = trapezoid_zt( mixt_frac_zt, mixt_frac_zm )
        pdf_params%rc_1         = trapezoid_zt( rc_1_zt, rc_1_zm )
        pdf_params%rc_2         = trapezoid_zt( rc_2_zt, rc_2_zm )
        pdf_params%rsatl_1        = trapezoid_zt( rsatl_1_zt, rsatl_1_zm )
        pdf_params%rsatl_2        = trapezoid_zt( rsatl_2_zt, rsatl_2_zm )
        pdf_params%cloud_frac_1 = trapezoid_zt( cloud_frac_1_zt, cloud_frac_1_zm )
        pdf_params%cloud_frac_2 = trapezoid_zt( cloud_frac_2_zt, cloud_frac_2_zm )
        pdf_params%chi_1          = trapezoid_zt( chi_1_zt, chi_1_zm )
        pdf_params%chi_2          = trapezoid_zt( chi_2_zt, chi_2_zm )
        pdf_params%rrtthl      = trapezoid_zt( rrtthl_zt, rrtthl_zm )
        pdf_params%alpha_thl   = trapezoid_zt( alpha_thl_zt, alpha_thl_zm )
        pdf_params%alpha_rt    = trapezoid_zt( alpha_rt_zt, alpha_rt_zm )
        pdf_params%stdev_chi_1    = trapezoid_zt( stdev_chi_1_zt, stdev_chi_1_zm )
        pdf_params%stdev_chi_2    = trapezoid_zt( stdev_chi_2_zt, stdev_chi_2_zm )
        pdf_params%stdev_eta_1    = trapezoid_zt( stdev_eta_1_zt, stdev_eta_1_zm )
        pdf_params%stdev_eta_2    = trapezoid_zt( stdev_eta_2_zt, stdev_eta_2_zm )
      end if
! End of trapezoidal rule
      return
        END SUBROUTINE trapezoidal_rule_zt
!-----------------------------------------------------------------------

        SUBROUTINE trapezoidal_rule_zm(wpthvp_zt, thlpthvp_zt, rtpthvp_zt, wpthvp, thlpthvp, rtpthvp)
! intent(in)
! intent(inout)
!
! Description:
!   This subroutine recomputes three variables on the
!   momentum grid from pdf_closure -- wpthvp, thlpthvp, and
!   rtpthvp -- by calling the function trapezoid_zm.  Only these three
!   variables are used in this subroutine because they are the only
!   pdf_closure momentum variables used elsewhere in CLUBB.
!
!   The _zt variables are output from the first call to pdf_closure.
!   The _zm variables are output from the second call to pdf_closure
!   on the momentum levels.
!   This is done before the call to this subroutine.
!
!   ldgrant Feb. 2010
!
!  References:
!    None
!-----------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable
            USE clubb_precision, ONLY: core_rknd
! variable(s)
            IMPLICIT NONE
! Input variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: thlpthvp_zt
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: rtpthvp_zt
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: wpthvp_zt
! Buoyancy flux (on thermo. grid)  [(K m)/s]
! th_l' th_v' (on thermo. grid)    [K^2]
! r_t' th_v' (on thermo. grid)     [(kg K)/kg]
! Input/Output variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: thlpthvp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wpthvp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: rtpthvp
! Buoyancy flux   [(K m)/s]
! th_l' th_v'     [K^2]
! r_t' th_v'      [(kg K)/kg]
!----------------------- Begin Code -----------------------------
! Use the trapezoidal rule to recompute the variables on the zm level
      wpthvp     = trapezoid_zm( wpthvp, wpthvp_zt )
      thlpthvp   = trapezoid_zm( thlpthvp, thlpthvp_zt )
      rtpthvp    = trapezoid_zm( rtpthvp, rtpthvp_zt )
      return
        END SUBROUTINE trapezoidal_rule_zm
!-----------------------------------------------------------------------

        pure FUNCTION trapezoid_zt(variable_zt, variable_zm)
!
! Description:
!   Function which uses the trapezoidal rule from calculus
!   to recompute the values for the variables on the thermo. grid which
!   are output from the first call to pdf_closure in module clubb_core.
!
!   ldgrant June 2009
!--------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: variable_zt
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: variable_zm
! Variable on the zt grid
! Variable on the zm grid
! Result
            REAL(KIND=core_rknd), dimension(gr%nz) :: trapezoid_zt
! Local Variable
            INTEGER :: k ! Loop index
!------------ Begin Code --------------
! Boundary condition: trapezoidal rule not valid at zt level 1
      trapezoid_zt(1) = variable_zt(1)
      do k = 2, gr%nz
! Trapezoidal rule from calculus
        trapezoid_zt(k) =  0.5_core_rknd * ( variable_zm(k) + variable_zt(k) ) &
                               * ( gr%zm(k) - gr%zt(k) ) * gr%invrs_dzt(k) &
                         + 0.5_core_rknd * ( variable_zt(k) + variable_zm(k-1) ) &
                               * ( gr%zt(k) - gr%zm(k-1) ) * gr%invrs_dzt(k)
      end do ! k = 2, gr%nz ! k = 2, gr%nz
      return
        END FUNCTION trapezoid_zt
!-----------------------------------------------------------------------

        pure FUNCTION trapezoid_zm(variable_zm, variable_zt)
!
! Description:
!   Function which uses the trapezoidal rule from calculus
!   to recompute the values for the important variables on the momentum
!   grid which are output from pdf_closure in module clubb_core.
!   These momentum variables only include wpthvp, thlpthvp, and rtpthvp.
!
!   ldgrant Feb. 2010
!--------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: variable_zm
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: variable_zt
! Variable on the zm grid
! Variable on the zt grid
! Result
            REAL(KIND=core_rknd), dimension(gr%nz) :: trapezoid_zm
! Local Variable
            INTEGER :: k ! Loop index
!------------ Begin Code --------------
! Boundary conditions: trapezoidal rule not valid at top zm level, nzmax.
! Trapezoidal rule also not used at zm level 1.
      trapezoid_zm(1)       = variable_zm(1)
      trapezoid_zm(gr%nz) = variable_zm(gr%nz)
      do k = 2, gr%nz-1
! Trapezoidal rule from calculus
        trapezoid_zm(k) =  0.5_core_rknd * ( variable_zt(k+1) + variable_zm(k) ) &
                               * ( gr%zt(k+1) - gr%zm(k) ) * gr%invrs_dzm(k) &
                         + 0.5_core_rknd * ( variable_zm(k) + variable_zt(k) ) &
                               * ( gr%zm(k) - gr%zt(k) ) * gr%invrs_dzm(k)
      end do ! k = 2, gr%nz-1 ! k = 2, gr%nz-1
      return
        END FUNCTION trapezoid_zm
!-----------------------------------------------------------------------

        SUBROUTINE compute_cloud_cover(pdf_params, cloud_frac, rcm, cloud_cover, rcm_in_layer)
! intent(in)
! intent(out)
!
! Description:
!   Subroutine to compute cloud cover (the amount of sky
!   covered by cloud) and rcm in layer (liquid water mixing ratio in
!   the portion of the grid box filled by cloud).
!
! References:
!   Definition of 's' comes from:
!   ``The Gaussian Cloud Model Relations'' G. L. Mellor (1977)
!   JAS, Vol. 34, pp. 356--358.
!
! Notes:
!   Added July 2009
!---------------------------------------------------------------------
            USE constants_clubb, ONLY: rc_tol
            USE constants_clubb, ONLY: fstderr
! Variable(s)
            USE grid_class, ONLY: gr ! Variable
            USE pdf_parameter_module, ONLY: pdf_parameter
! Derived data type
            USE error_code, ONLY: clubb_at_least_debug_level
! Procedure
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External functions
            INTRINSIC abs, min, max
! Input variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: cloud_frac
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: rcm
! Cloud fraction             [-]
! Liquid water mixing ratio  [kg/kg]
            TYPE(pdf_parameter), dimension(gr%nz), intent(in) :: pdf_params
! PDF Parameters  [units vary]
! Output variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(out) :: cloud_cover
            REAL(KIND=core_rknd), dimension(gr%nz), intent(out) :: rcm_in_layer
! Cloud cover                               [-]
! Liquid water mixing ratio in cloud layer  [kg/kg]
! Local variables
            REAL(KIND=core_rknd), dimension(gr%nz) :: chi_mean
            REAL(KIND=core_rknd), dimension(gr%nz) :: vert_cloud_frac_upper
            REAL(KIND=core_rknd), dimension(gr%nz) :: vert_cloud_frac_lower
            REAL(KIND=core_rknd), dimension(gr%nz) :: vert_cloud_frac
! Mean extended cloud water mixing ratio of the
!                            two Gaussian distributions
! Fraction of cloud in top half of grid box
! Fraction of cloud in bottom half of grid box
! Fraction of cloud filling the grid box in the vertical
            INTEGER :: k
! ------------ Begin code ---------------
      do k = 1, gr%nz
        chi_mean(k) =      pdf_params(k)%mixt_frac  * pdf_params(k)%chi_1 + &
                    (1.0_core_rknd-pdf_params(k)%mixt_frac) * pdf_params(k)%chi_2
      end do
      do k = 2, gr%nz-1, 1
        if ( rcm(k) < rc_tol ) then ! No cloud at this level ! No cloud at this level
          cloud_cover(k)  = cloud_frac(k)
          rcm_in_layer(k) = rcm(k)
        else if ( ( rcm(k+1) >= rc_tol ) .and. ( rcm(k-1) >= rc_tol ) ) then
! There is cloud above and below,
!   so assume cloud fills grid box from top to bottom
          cloud_cover(k) = cloud_frac(k)
          rcm_in_layer(k) = rcm(k)
        else if ( ( rcm(k+1) < rc_tol ) .or. ( rcm(k-1) < rc_tol) ) then
! Cloud may fail to reach gridbox top or base or both
! First let the cloud fill the entire grid box, then overwrite
! vert_cloud_frac_upper(k) and/or vert_cloud_frac_lower(k)
! for a cloud top, cloud base, or one-point cloud.
          vert_cloud_frac_upper(k) = 0.5_core_rknd
          vert_cloud_frac_lower(k) = 0.5_core_rknd
          if ( rcm(k+1) < rc_tol ) then ! Cloud top ! Cloud top
            vert_cloud_frac_upper(k) = &
                     ( ( 0.5_core_rknd / gr%invrs_dzm(k) ) / ( gr%zm(k) - gr%zt(k) ) ) &
                     * ( rcm(k) / ( rcm(k) + abs( chi_mean(k+1) ) ) )
            vert_cloud_frac_upper(k) = min( 0.5_core_rknd, vert_cloud_frac_upper(k) )
! Make the transition in cloudiness more gradual than using
! the above min statement alone.
            vert_cloud_frac_upper(k) = vert_cloud_frac_upper(k) + &
              ( ( rcm(k+1)/rc_tol )*( 0.5_core_rknd -vert_cloud_frac_upper(k) ) )
          else
            vert_cloud_frac_upper(k) = 0.5_core_rknd
          end if
          if ( rcm(k-1) < rc_tol ) then ! Cloud base ! Cloud base
            vert_cloud_frac_lower(k) = &
                     ( ( 0.5_core_rknd / gr%invrs_dzm(k-1) ) / ( gr%zt(k) - gr%zm(k-1) ) ) &
                     * ( rcm(k) / ( rcm(k) + abs( chi_mean(k-1) ) ) )
            vert_cloud_frac_lower(k) = min( 0.5_core_rknd, vert_cloud_frac_lower(k) )
! Make the transition in cloudiness more gradual than using
! the above min statement alone.
            vert_cloud_frac_lower(k) = vert_cloud_frac_lower(k) + &
              ( ( rcm(k-1)/rc_tol )*( 0.5_core_rknd -vert_cloud_frac_lower(k) ) )
          else
            vert_cloud_frac_lower(k) = 0.5_core_rknd
          end if
          vert_cloud_frac(k) = &
            vert_cloud_frac_upper(k) + vert_cloud_frac_lower(k)
          vert_cloud_frac(k) = &
            max( cloud_frac(k), min( 1.0_core_rknd, vert_cloud_frac(k) ) )
          cloud_cover(k)  = cloud_frac(k) / vert_cloud_frac(k)
          rcm_in_layer(k) = rcm(k) / vert_cloud_frac(k)
        else
          if ( clubb_at_least_debug_level( 1 ) ) then
            write(fstderr,*)  &
               "Error: Should not arrive here in computation of cloud_cover"
            write(fstderr,*) "At grid level k = ", k
            write(fstderr,*) "pdf_params(k)%mixt_frac = ", pdf_params(k)%mixt_frac
            write(fstderr,*) "pdf_params(k)%chi_1 = ", pdf_params(k)%chi_1
            write(fstderr,*) "pdf_params(k)%chi_2 = ", pdf_params(k)%chi_2
            write(fstderr,*) "cloud_frac(k) = ", cloud_frac(k)
            write(fstderr,*) "rcm(k) = ", rcm(k)
            write(fstderr,*) "rcm(k+1) = ", rcm(k+1)
            write(fstderr,*) "rcm(k-1) = ", rcm(k-1)
          end if
          return
        end if ! rcm(k) < rc_tol ! rcm(k) < rc_tol
      end do ! k = 2, gr%nz-1, 1 ! k = 2, gr%nz-1, 1
      cloud_cover(1)       = cloud_frac(1)
      cloud_cover(gr%nz) = cloud_frac(gr%nz)
      rcm_in_layer(1)       = rcm(1)
      rcm_in_layer(gr%nz) = rcm(gr%nz)
      return
        END SUBROUTINE compute_cloud_cover
!-----------------------------------------------------------------------

        SUBROUTINE clip_rcm(rtm, message, rcm)
! intent(in)
! intent(inout)
!
! Description:
!   Subroutine that reduces cloud water (rcm) whenever
!   it exceeds total water (rtm = vapor + liquid).
!   This avoids negative values of rvm = water vapor mixing ratio.
!   However, it will not ensure that rcm <= rtm if rtm <= 0.
!
! References:
!   None
!---------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable
            USE error_code, ONLY: clubb_at_least_debug_level
! Procedure(s)
            USE constants_clubb, ONLY: fstderr
            USE constants_clubb, ONLY: zero_threshold
! Variable(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External functions
            INTRINSIC max, epsilon
! Input variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: rtm
! Total water mixing ratio             [kg/kg]
            CHARACTER(LEN=*), intent(in) :: message
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: rcm
! Cloud water mixing ratio  [kg/kg]
            INTEGER :: k
! ------------ Begin code ---------------
! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
! This code won't work unless rtm >= 0 !!!
! We do not clip rcm_in_layer because rcm_in_layer only influences
! radiation, and we do not want to bother recomputing it.  6 Aug 2009
      do k = 1, gr%nz
        if ( rtm(k) < rcm(k) ) then
          if ( clubb_at_least_debug_level(1) ) then
            write(fstderr,*) message, ' at k=', k, 'rcm(k) = ', rcm(k), &
              'rtm(k) = ', rtm(k), '.',  '  Clipping rcm.'
          end if ! clubb_at_least_debug_level(1) ! clubb_at_least_debug_level(1)
          rcm(k) = max( zero_threshold, rtm(k) - epsilon( rtm(k) ) )
        end if ! rtm(k) < rcm(k) ! rtm(k) < rcm(k)
      end do ! k=1..gr%nz ! k=1..gr%nz
      return
        END SUBROUTINE clip_rcm
!-----------------------------------------------------------------------------

        SUBROUTINE set_lscale_max(l_implemented, host_dx, host_dy, lscale_max)
! Description:
!   This subroutine sets the value of Lscale_max, which is the maximum
!   allowable value of Lscale.  For standard CLUBB, it is set to a very large
!   value so that Lscale will not be limited.  However, when CLUBB is running
!   as part of a host model, the value of Lscale_max is dependent on the size
!   of the host model's horizontal grid spacing.  The smaller the host model's
!   horizontal grid spacing, the smaller the value of Lscale_max.  When Lscale
!   is limited to a small value, the value of time-scale Tau is reduced, which
!   in turn produces greater damping on CLUBB's turbulent parameters.  This
!   is the desired effect on turbulent parameters for a host model with small
!   horizontal grid spacing, for small areas usually contain much less
!   variation in meteorological quantities than large areas.
! References:
!   None
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            LOGICAL, intent(in) :: l_implemented
! Flag to see if CLUBB is running on it's own,
!                    or if it's implemented as part of a host model.
            REAL(KIND=core_rknd), intent(in) :: host_dx
            REAL(KIND=core_rknd), intent(in) :: host_dy
! Host model's east-west horizontal grid spacing     [m]
! Host model's north-south horizontal grid spacing   [m]
! Output Variable
            REAL(KIND=core_rknd), intent(out) :: lscale_max
! Maximum allowable value for Lscale   [m]
! ---- Begin Code ----
! Determine the maximum allowable value for Lscale (in meters).
      if ( l_implemented ) then
        Lscale_max = 0.25_core_rknd * min( host_dx, host_dy )
      else
        Lscale_max = 1.0e5_core_rknd
      end if
      return
        END SUBROUTINE set_lscale_max
!===============================================================================

!-----------------------------------------------------------------------
    END MODULE advance_clubb_core_module
