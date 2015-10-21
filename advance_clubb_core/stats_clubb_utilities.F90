
! KGEN-generated Fortran source file
!
! Filename    : stats_clubb_utilities.F90
! Generated at: 2015-10-20 14:27:09
! KGEN version: 0.5.3



    MODULE stats_clubb_utilities
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE ! Set Default Scope
        PUBLIC stats_accumulate
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!----------------------------------------------------------------------

        SUBROUTINE stats_accumulate(um, vm, upwp, vpwp, up2, vp2, thlm, rtm, wprtp, wpthlp, wp2, wp3, rtp2, thlp2, rtpthlp, &
        p_in_pa, exner, rho, rho_zm, rho_ds_zm, rho_ds_zt, thv_ds_zm, thv_ds_zt, wm_zt, wm_zm, rcm, wprcp, rc_coef, rcm_zm, &
        rtm_zm, thlm_zm, cloud_frac, ice_supersat_frac, cloud_frac_zm, ice_supersat_frac_zm, rcm_in_layer, cloud_cover, &
        sigma_sqd_w, pdf_params, sclrm, sclrp2, sclrprtp, sclrpthlp, sclrm_forcing, wpsclrp, edsclrm, edsclrm_forcing)
! Description:
!   Accumulate those stats variables that are preserved in CLUBB from timestep to
!   timestep, but not those stats that are not, (e.g. budget terms, longwave and
!   shortwave components, etc.)
!
! References:
!   None
!----------------------------------------------------------------------
            USE constants_clubb, ONLY: cloud_frac_min
! Constant
            USE pdf_utilities, ONLY: compute_variance_binormal
! Procedure
            USE stats_variables, ONLY: l_stats_samp
            USE stats_variables, ONLY: it_in_k
            USE stats_variables, ONLY: stats_zt
            USE stats_variables, ONLY: ithlm
            USE stats_variables, ONLY: ithvm
            USE stats_variables, ONLY: irtm
            USE stats_variables, ONLY: ircm
            USE stats_variables, ONLY: ium
            USE stats_variables, ONLY: ivm
            USE stats_variables, ONLY: iwm_zt
            USE stats_variables, ONLY: iwm_zm
            USE stats_variables, ONLY: stats_zm
            USE stats_variables, ONLY: iug
            USE stats_variables, ONLY: ivg
            USE stats_variables, ONLY: icloud_frac
            USE stats_variables, ONLY: iice_supersat_frac
            USE stats_variables, ONLY: ircm_in_layer
            USE stats_variables, ONLY: icloud_cover
            USE stats_variables, ONLY: stats_sfc
! Variables
            USE stats_variables, ONLY: ip_in_pa
            USE stats_variables, ONLY: iexner
            USE stats_variables, ONLY: irho_ds_zt
            USE stats_variables, ONLY: ithv_ds_zt
            USE stats_variables, ONLY: ilscale
            USE stats_variables, ONLY: iwp3
            USE stats_variables, ONLY: iwpthlp2
            USE stats_variables, ONLY: iwp2thlp
            USE stats_variables, ONLY: iwprtp2
            USE stats_variables, ONLY: iwp2rtp
            USE stats_variables, ONLY: ilscale_up
            USE stats_variables, ONLY: ilscale_down
            USE stats_variables, ONLY: itau_zt
            USE stats_variables, ONLY: ikh_zt
            USE stats_variables, ONLY: iwp3_zm
            USE stats_variables, ONLY: irsati
            USE stats_variables, ONLY: iwp2thvp
            USE stats_variables, ONLY: iwp2rcp
            USE stats_variables, ONLY: iwprtpthlp
            USE stats_variables, ONLY: isigma_sqd_w_zt
            USE stats_variables, ONLY: irho
            USE stats_variables, ONLY: irsat
! Variable(s)
            USE stats_variables, ONLY: imixt_frac
            USE stats_variables, ONLY: iw_1
            USE stats_variables, ONLY: iw_2
            USE stats_variables, ONLY: ivarnce_w_1
            USE stats_variables, ONLY: ivarnce_w_2
            USE stats_variables, ONLY: ithl_1
            USE stats_variables, ONLY: ithl_2
            USE stats_variables, ONLY: ivarnce_thl_1
            USE stats_variables, ONLY: ivarnce_thl_2
            USE stats_variables, ONLY: irt_1
            USE stats_variables, ONLY: irt_2
            USE stats_variables, ONLY: ivarnce_rt_1
            USE stats_variables, ONLY: ivarnce_rt_2
            USE stats_variables, ONLY: irc_1
            USE stats_variables, ONLY: irc_2
            USE stats_variables, ONLY: irsatl_1
            USE stats_variables, ONLY: irsatl_2
            USE stats_variables, ONLY: icloud_frac_1
            USE stats_variables, ONLY: icloud_frac_2
! Variable(s)
            USE stats_variables, ONLY: ichi_1
            USE stats_variables, ONLY: ichi_2
            USE stats_variables, ONLY: istdev_chi_1
            USE stats_variables, ONLY: istdev_chi_2
            USE stats_variables, ONLY: istdev_eta_1
            USE stats_variables, ONLY: istdev_eta_2
            USE stats_variables, ONLY: icovar_chi_eta_1
            USE stats_variables, ONLY: icovar_chi_eta_2
            USE stats_variables, ONLY: icorr_chi_eta_1
            USE stats_variables, ONLY: icorr_chi_eta_2
            USE stats_variables, ONLY: irrtthl
            USE stats_variables, ONLY: icrt_1
            USE stats_variables, ONLY: icrt_2
            USE stats_variables, ONLY: icthl_1
            USE stats_variables, ONLY: icthl_2
            USE stats_variables, ONLY: ichi
            USE stats_variables, ONLY: ichip2
! Variable(s)
            USE stats_variables, ONLY: iwp2_zt
            USE stats_variables, ONLY: ithlp2_zt
            USE stats_variables, ONLY: iwpthlp_zt
            USE stats_variables, ONLY: iwprtp_zt
            USE stats_variables, ONLY: irtp2_zt
            USE stats_variables, ONLY: irtpthlp_zt
            USE stats_variables, ONLY: iup2_zt
            USE stats_variables, ONLY: ivp2_zt
            USE stats_variables, ONLY: iupwp_zt
            USE stats_variables, ONLY: ivpwp_zt
            USE stats_variables, ONLY: iwp2
            USE stats_variables, ONLY: irtp2
            USE stats_variables, ONLY: ithlp2
            USE stats_variables, ONLY: irtpthlp
            USE stats_variables, ONLY: iwprtp
            USE stats_variables, ONLY: iwpthlp
            USE stats_variables, ONLY: iwp4
            USE stats_variables, ONLY: iwpthvp
            USE stats_variables, ONLY: irtpthvp
! Variable(s)
            USE stats_variables, ONLY: ithlpthvp
            USE stats_variables, ONLY: itau_zm
            USE stats_variables, ONLY: ikh_zm
            USE stats_variables, ONLY: iwprcp
            USE stats_variables, ONLY: irc_coef
            USE stats_variables, ONLY: ithlprcp
            USE stats_variables, ONLY: irtprcp
            USE stats_variables, ONLY: ircp2
            USE stats_variables, ONLY: iupwp
            USE stats_variables, ONLY: ivpwp
            USE stats_variables, ONLY: ivp2
            USE stats_variables, ONLY: iup2
            USE stats_variables, ONLY: irho_zm
            USE stats_variables, ONLY: isigma_sqd_w
            USE stats_variables, ONLY: irho_ds_zm
            USE stats_variables, ONLY: ithv_ds_zm
            USE stats_variables, ONLY: iem
! Variable(s)
            USE stats_variables, ONLY: ifrad
            USE stats_variables, ONLY: ishear
            USE stats_variables, ONLY: icc
            USE stats_variables, ONLY: iz_cloud_base
            USE stats_variables, ONLY: ilwp
            USE stats_variables, ONLY: ivwp
            USE stats_variables, ONLY: ithlm_vert_avg
            USE stats_variables, ONLY: irtm_vert_avg
            USE stats_variables, ONLY: ium_vert_avg
            USE stats_variables, ONLY: ivm_vert_avg
            USE stats_variables, ONLY: iwp2_vert_avg
            USE stats_variables, ONLY: iup2_vert_avg
            USE stats_variables, ONLY: ivp2_vert_avg
            USE stats_variables, ONLY: irtp2_vert_avg
            USE stats_variables, ONLY: ithlp2_vert_avg
! Variable(s)
            USE stats_variables, ONLY: isclrm
            USE stats_variables, ONLY: isclrm_f
            USE stats_variables, ONLY: iedsclrm
            USE stats_variables, ONLY: iedsclrm_f
            USE stats_variables, ONLY: isclrp2
            USE stats_variables, ONLY: isclrprtp
            USE stats_variables, ONLY: isclrpthvp
            USE stats_variables, ONLY: isclrpthlp
            USE stats_variables, ONLY: isclrprcp
            USE stats_variables, ONLY: iwpsclrp
            USE stats_variables, ONLY: iwp2sclrp
            USE stats_variables, ONLY: iwpsclrp2
            USE stats_variables, ONLY: iwpsclrprtp
            USE stats_variables, ONLY: iwpsclrpthlp
            USE stats_variables, ONLY: iwpedsclrp
! Variable(s)
            USE stats_variables, ONLY: icloud_frac_zm
            USE stats_variables, ONLY: iice_supersat_frac_zm
            USE stats_variables, ONLY: ircm_zm
            USE stats_variables, ONLY: irtm_zm
            USE stats_variables, ONLY: ithlm_zm
            USE stats_variables, ONLY: iwp3_on_wp2_zt
            USE stats_variables, ONLY: iskw_velocity
            USE stats_variables, ONLY: iwp3_on_wp2
            USE stats_variables, ONLY: ia3_coef_zt
            USE stats_variables, ONLY: ircm_in_cloud
            USE stats_variables, ONLY: ia3_coef
! Variables
            USE grid_class, ONLY: gr
! Variable
! Procedure(s)
            USE variables_diagnostic_module, ONLY: thvm
            USE variables_diagnostic_module, ONLY: ug
            USE variables_diagnostic_module, ONLY: vg
            USE variables_diagnostic_module, ONLY: lscale
            USE variables_diagnostic_module, ONLY: wpthlp2
            USE variables_diagnostic_module, ONLY: wp2thlp
            USE variables_diagnostic_module, ONLY: wprtp2
            USE variables_diagnostic_module, ONLY: wp2rtp
            USE variables_diagnostic_module, ONLY: lscale_up
            USE variables_diagnostic_module, ONLY: lscale_down
            USE variables_diagnostic_module, ONLY: tau_zt
            USE variables_diagnostic_module, ONLY: kh_zt
            USE variables_diagnostic_module, ONLY: wp2thvp
            USE variables_diagnostic_module, ONLY: wp2rcp
            USE variables_diagnostic_module, ONLY: wprtpthlp
            USE variables_diagnostic_module, ONLY: sigma_sqd_w_zt
            USE variables_diagnostic_module, ONLY: rsat
! Variable(s)
            USE variables_diagnostic_module, ONLY: wp2_zt
            USE variables_diagnostic_module, ONLY: thlp2_zt
            USE variables_diagnostic_module, ONLY: wpthlp_zt
            USE variables_diagnostic_module, ONLY: wprtp_zt
            USE variables_diagnostic_module, ONLY: rtp2_zt
            USE variables_diagnostic_module, ONLY: rtpthlp_zt
            USE variables_diagnostic_module, ONLY: up2_zt
            USE variables_diagnostic_module, ONLY: vp2_zt
            USE variables_diagnostic_module, ONLY: upwp_zt
            USE variables_diagnostic_module, ONLY: vpwp_zt
            USE variables_diagnostic_module, ONLY: wp4
            USE variables_diagnostic_module, ONLY: wpthvp
            USE variables_diagnostic_module, ONLY: rtpthvp
            USE variables_diagnostic_module, ONLY: thlpthvp
            USE variables_diagnostic_module, ONLY: tau_zm
            USE variables_diagnostic_module, ONLY: kh_zm
            USE variables_diagnostic_module, ONLY: thlprcp
            USE variables_diagnostic_module, ONLY: rtprcp
            USE variables_diagnostic_module, ONLY: rcp2
            USE variables_diagnostic_module, ONLY: em
            USE variables_diagnostic_module, ONLY: frad
            USE variables_diagnostic_module, ONLY: sclrpthvp
            USE variables_diagnostic_module, ONLY: sclrprcp
            USE variables_diagnostic_module, ONLY: wp2sclrp
            USE variables_diagnostic_module, ONLY: wpsclrp2
            USE variables_diagnostic_module, ONLY: wpsclrprtp
            USE variables_diagnostic_module, ONLY: wpsclrpthlp
            USE variables_diagnostic_module, ONLY: wpedsclrp
! Variable(s)
            USE variables_diagnostic_module, ONLY: a3_coef_zt
            USE variables_diagnostic_module, ONLY: wp3_on_wp2_zt
            USE variables_diagnostic_module, ONLY: wp3_zm
            USE variables_diagnostic_module, ONLY: skw_velocity
            USE variables_diagnostic_module, ONLY: a3_coef
            USE variables_diagnostic_module, ONLY: wp3_on_wp2
! Variable(s)
            USE pdf_parameter_module, ONLY: pdf_parameter
! Type
            USE t_in_k_module, ONLY: thlm2t_in_k
! Procedure
            USE constants_clubb, ONLY: rc_tol
! Constant(s)
            USE parameters_model, ONLY: sclr_dim
            USE parameters_model, ONLY: edsclr_dim
! Variable(s)
            USE stats_type_utilities, ONLY: stat_update_var
            USE stats_type_utilities, ONLY: stat_update_var_pt
! Procedure(s)
            USE fill_holes, ONLY: vertical_integral
            USE fill_holes, ONLY: vertical_avg
! Procedure(s)
            USE interpolation, ONLY: lin_interpolate_two_points
! Procedure
            USE saturation, ONLY: sat_mixrat_ice
! Procedure
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variable(s)
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wprtp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: um
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wp3
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtpthlp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vpwp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: upwp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wpthlp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: up2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vp2
! u wind                        [m/s]
! v wind                        [m/s]
! vertical u momentum flux      [m^2/s^2]
! vertical v momentum flux      [m^2/s^2]
! u'^2                          [m^2/s^2]
! v'^2                          [m^2/s^2]
! liquid potential temperature  [K]
! total water mixing ratio      [kg/kg]
! w'rt'                         [(kg/kg) m/s]
! w'thl'                        [m K /s]
! w'^2                          [m^2/s^2]
! w'^3                          [m^3/s^3]
! rt'^2                         [(kg/kg)^2]
! thl'^2                        [K^2]
! rt'thl'                       [kg/kg K]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: p_in_pa
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thv_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thv_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: exner
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zt
! Pressure (Pa) on thermodynamic points    [Pa]
! Exner function = ( p / p0 ) ** kappa     [-]
! Density                                  [kg/m^3]
! Density                                  [kg/m^3]
! Dry, static density (momentum levels)    [kg/m^3]
! Dry, static density (thermo. levs.)      [kg/m^3]
! Dry, base-state theta_v (momentum levs.) [K]
! Dry, base-state theta_v (thermo. levs.)  [K]
! w on thermodynamic levels                [m/s]
! w on momentum levels                     [m/s]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: ice_supersat_frac_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rcm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rc_coef
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rcm_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtm_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wprcp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlm_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: cloud_frac
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rcm_in_layer
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: ice_supersat_frac
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: cloud_frac_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: cloud_cover
! Total water mixing ratio                 [kg/kg]
! Total water mixing ratio                 [kg/kg]
! Liquid potential temperature             [K]
! Cloud water mixing ratio                 [kg/kg]
! w'rc'                                    [(kg/kg) m/s]
! Coefficient of X' R_l' in Eq. (34)       [-]
! Cloud fraction                           [-]
! Ice cloud fracion                        [-]
! Cloud fraction on zm levels              [-]
! Ice cloud fraction on zm levels          [-]
! Cloud water mixing ratio in cloud layer  [kg/kg]
! Cloud cover                              [-]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: sigma_sqd_w
! PDF width parameter (momentum levels)    [-]
            TYPE(pdf_parameter), dimension(gr%nz), intent(in) :: pdf_params
! PDF parameters [units vary]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: wpsclrp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrprtp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrpthlp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrm
! High-order passive scalar            [units vary]
! High-order passive scalar variance   [units^2]
! High-order passive scalar covariance [units kg/kg]
! High-order passive scalar covariance [units K]
! Large-scale forcing of scalar        [units/s]
! w'sclr'                              [units m/s]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,edsclr_dim) :: edsclrm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,edsclr_dim) :: edsclrm
! Eddy-diff passive scalar      [units vary]
! Large-scale forcing of edscalar  [units vary]
! Local Variables
            INTEGER :: isclr
            INTEGER :: k
            REAL(KIND=core_rknd), dimension(gr%nz) :: t_in_k
            REAL(KIND=core_rknd), dimension(gr%nz) :: rsati
            REAL(KIND=core_rknd), dimension(gr%nz) :: chi
            REAL(KIND=core_rknd), dimension(gr%nz) :: chip2
            REAL(KIND=core_rknd), dimension(gr%nz) :: rcm_in_cloud
            REAL(KIND=core_rknd), dimension(gr%nz) :: shear
! Absolute temperature         [K]
! Saturation w.r.t ice         [kg/kg]
! Wind shear production term   [m^2/s^3]
! Mellor's 's'                 [kg/kg]
! Variance of Mellor's 's'     [kg/kg]
! rcm in cloud                 [kg/kg]
            REAL(KIND=core_rknd) :: xtmp
! ---- Begin Code ----
! Sample fields
    if ( l_stats_samp ) then
! stats_zt variables
      if ( iT_in_K > 0 .or. irsati > 0 ) then
        T_in_K = thlm2T_in_K( thlm, exner, rcm )
      else
        T_in_K = -999._core_rknd
      end if
      call stat_update_var( iT_in_K, T_in_K, stats_zt )
      call stat_update_var( ithlm, thlm, stats_zt )
      call stat_update_var( ithvm, thvm, stats_zt )
      call stat_update_var( irtm, rtm, stats_zt )
      call stat_update_var( ircm, rcm, stats_zt )
      call stat_update_var( ium, um, stats_zt )
      call stat_update_var( ivm, vm, stats_zt )
      call stat_update_var( iwm_zt, wm_zt, stats_zt )
      call stat_update_var( iwm_zm, wm_zm, stats_zm )
      call stat_update_var( iug, ug, stats_zt )
      call stat_update_var( ivg, vg, stats_zt )
      call stat_update_var( icloud_frac, cloud_frac, stats_zt )
      call stat_update_var( iice_supersat_frac, ice_supersat_frac, stats_zt)
      call stat_update_var( ircm_in_layer, rcm_in_layer, stats_zt )
      call stat_update_var( icloud_cover, cloud_cover, stats_zt )
      call stat_update_var( ip_in_Pa, p_in_Pa, stats_zt )
      call stat_update_var( iexner, exner, stats_zt )
      call stat_update_var( irho_ds_zt, rho_ds_zt, stats_zt )
      call stat_update_var( ithv_ds_zt, thv_ds_zt, stats_zt )
      call stat_update_var( iLscale, Lscale, stats_zt )
      call stat_update_var( iwp3, wp3, stats_zt )
      call stat_update_var( iwpthlp2, wpthlp2, stats_zt )
      call stat_update_var( iwp2thlp, wp2thlp, stats_zt )
      call stat_update_var( iwprtp2, wprtp2, stats_zt )
      call stat_update_var( iwp2rtp, wp2rtp, stats_zt )
      call stat_update_var( iLscale_up, Lscale_up, stats_zt )
      call stat_update_var( iLscale_down, Lscale_down, stats_zt )
      call stat_update_var( itau_zt, tau_zt, stats_zt )
      call stat_update_var( iKh_zt, Kh_zt, stats_zt )
      call stat_update_var( iwp2thvp, wp2thvp, stats_zt )
      call stat_update_var( iwp2rcp, wp2rcp, stats_zt )
      call stat_update_var( iwprtpthlp, wprtpthlp, stats_zt )
      call stat_update_var( isigma_sqd_w_zt, sigma_sqd_w_zt, stats_zt )
      call stat_update_var( irho, rho, stats_zt )
      call stat_update_var( irsat, rsat, stats_zt )
      if ( irsati > 0 ) then
        rsati = sat_mixrat_ice( p_in_Pa, T_in_K )
        call stat_update_var( irsati, rsati, stats_zt )
      end if
      call stat_update_var( imixt_frac, pdf_params%mixt_frac, stats_zt )
      call stat_update_var( iw_1, pdf_params%w_1, stats_zt )
      call stat_update_var( iw_2, pdf_params%w_2, stats_zt )
      call stat_update_var( ivarnce_w_1, pdf_params%varnce_w_1, stats_zt )
      call stat_update_var( ivarnce_w_2, pdf_params%varnce_w_2, stats_zt )
      call stat_update_var( ithl_1, pdf_params%thl_1, stats_zt )
      call stat_update_var( ithl_2, pdf_params%thl_2, stats_zt )
      call stat_update_var( ivarnce_thl_1, pdf_params%varnce_thl_1, stats_zt )
      call stat_update_var( ivarnce_thl_2, pdf_params%varnce_thl_2, stats_zt )
      call stat_update_var( irt_1, pdf_params%rt_1, stats_zt )
      call stat_update_var( irt_2, pdf_params%rt_2, stats_zt )
      call stat_update_var( ivarnce_rt_1, pdf_params%varnce_rt_1, stats_zt )
      call stat_update_var( ivarnce_rt_2, pdf_params%varnce_rt_2, stats_zt )
      call stat_update_var( irc_1, pdf_params%rc_1, stats_zt )
      call stat_update_var( irc_2, pdf_params%rc_2, stats_zt )
      call stat_update_var( irsatl_1, pdf_params%rsatl_1, stats_zt )
      call stat_update_var( irsatl_2, pdf_params%rsatl_2, stats_zt )
      call stat_update_var( icloud_frac_1, pdf_params%cloud_frac_1, stats_zt )
      call stat_update_var( icloud_frac_2, pdf_params%cloud_frac_2, stats_zt )
      call stat_update_var( ichi_1, pdf_params%chi_1, stats_zt )
      call stat_update_var( ichi_2, pdf_params%chi_2, stats_zt )
      call stat_update_var( istdev_chi_1, pdf_params%stdev_chi_1, stats_zt )
      call stat_update_var( istdev_chi_2, pdf_params%stdev_chi_2, stats_zt )
      call stat_update_var( istdev_eta_1, pdf_params%stdev_eta_1, stats_zt )
      call stat_update_var( istdev_eta_2, pdf_params%stdev_eta_2, stats_zt )
      call stat_update_var( icovar_chi_eta_1, pdf_params%covar_chi_eta_1, stats_zt )
      call stat_update_var( icovar_chi_eta_2, pdf_params%covar_chi_eta_2, stats_zt )
      call stat_update_var( icorr_chi_eta_1, pdf_params%corr_chi_eta_1, stats_zt )
      call stat_update_var( icorr_chi_eta_2, pdf_params%corr_chi_eta_2, stats_zt )
      call stat_update_var( irrtthl, pdf_params%rrtthl, stats_zt )
      call stat_update_var( icrt_1, pdf_params%crt_1, stats_zt )
      call stat_update_var( icrt_2, pdf_params%crt_2, stats_zt )
      call stat_update_var( icthl_1, pdf_params%cthl_1, stats_zt )
      call stat_update_var( icthl_2, pdf_params%cthl_2, stats_zt )
      call stat_update_var( iwp2_zt, wp2_zt, stats_zt )
      call stat_update_var( ithlp2_zt, thlp2_zt, stats_zt )
      call stat_update_var( iwpthlp_zt, wpthlp_zt, stats_zt )
      call stat_update_var( iwprtp_zt, wprtp_zt, stats_zt )
      call stat_update_var( irtp2_zt, rtp2_zt, stats_zt )
      call stat_update_var( irtpthlp_zt, rtpthlp_zt, stats_zt )
      call stat_update_var( iup2_zt, up2_zt, stats_zt )
      call stat_update_var( ivp2_zt, vp2_zt, stats_zt )
      call stat_update_var( iupwp_zt, upwp_zt, stats_zt )
      call stat_update_var( ivpwp_zt, vpwp_zt, stats_zt )
      call stat_update_var( ia3_coef_zt, a3_coef_zt, stats_zt )
      call stat_update_var( iwp3_on_wp2_zt, wp3_on_wp2_zt, stats_zt )
      if ( ichi > 0 ) then
! Determine 's' from Mellor (1977) (extended liquid water)
        chi(:) = pdf_params%mixt_frac * pdf_params%chi_1 &
                    + (1.0_core_rknd-pdf_params%mixt_frac) * pdf_params%chi_2
        call stat_update_var( ichi, chi, stats_zt )
      end if
! Calculate variance of chi
      if ( ichip2 > 0 ) then
        chip2 = compute_variance_binormal( chi, pdf_params%chi_1, pdf_params%chi_2, &
                                         pdf_params%stdev_chi_1, pdf_params%stdev_chi_2, &
                                         pdf_params%mixt_frac )
        call stat_update_var( ichip2, chip2, stats_zt )
      end if
      if ( sclr_dim > 0 ) then
        do isclr=1, sclr_dim
          call stat_update_var( isclrm(isclr), sclrm(:,isclr), stats_zt )
          call stat_update_var( isclrm_f(isclr), sclrm_forcing(:,isclr),  stats_zt )
        end do
      end if
      if ( edsclr_dim > 0 ) then
        do isclr = 1, edsclr_dim
          call stat_update_var( iedsclrm(isclr), edsclrm(:,isclr), stats_zt )
          call stat_update_var( iedsclrm_f(isclr), edsclrm_forcing(:,isclr), stats_zt )
        end do
      end if
! Calculate rcm in cloud
      if ( ircm_in_cloud > 0 ) then
        where ( cloud_frac(:) > cloud_frac_min )
            rcm_in_cloud(:) = rcm / cloud_frac
        else where
            rcm_in_cloud(:) = rcm
                    END WHERE 
        call stat_update_var( ircm_in_cloud, rcm_in_cloud, stats_zt )
      end if
! stats_zm variables
      call stat_update_var( iwp2, wp2, stats_zm )
      call stat_update_var( iwp3_zm, wp3_zm, stats_zm )
      call stat_update_var( irtp2, rtp2, stats_zm )
      call stat_update_var( ithlp2, thlp2, stats_zm )
      call stat_update_var( irtpthlp, rtpthlp, stats_zm )
      call stat_update_var( iwprtp, wprtp, stats_zm )
      call stat_update_var( iwpthlp, wpthlp, stats_zm )
      call stat_update_var( iwp4, wp4, stats_zm )
      call stat_update_var( iwpthvp, wpthvp, stats_zm )
      call stat_update_var( irtpthvp, rtpthvp, stats_zm )
      call stat_update_var( ithlpthvp, thlpthvp, stats_zm )
      call stat_update_var( itau_zm, tau_zm, stats_zm )
      call stat_update_var( iKh_zm, Kh_zm, stats_zm )
      call stat_update_var( iwprcp, wprcp, stats_zm )
      call stat_update_var( irc_coef, rc_coef, stats_zm )
      call stat_update_var( ithlprcp, thlprcp, stats_zm )
      call stat_update_var( irtprcp, rtprcp, stats_zm )
      call stat_update_var( ircp2, rcp2, stats_zm )
      call stat_update_var( iupwp, upwp, stats_zm )
      call stat_update_var( ivpwp, vpwp, stats_zm )
      call stat_update_var( ivp2, vp2, stats_zm )
      call stat_update_var( iup2, up2, stats_zm )
      call stat_update_var( irho_zm, rho_zm, stats_zm )
      call stat_update_var( isigma_sqd_w, sigma_sqd_w, stats_zm )
      call stat_update_var( irho_ds_zm, rho_ds_zm, stats_zm )
      call stat_update_var( ithv_ds_zm, thv_ds_zm, stats_zm )
      call stat_update_var( iem, em, stats_zm )
      call stat_update_var( iFrad, Frad, stats_zm )
      call stat_update_var( iSkw_velocity, Skw_velocity, stats_zm )
      call stat_update_var( ia3_coef, a3_coef, stats_zm )
      call stat_update_var( iwp3_on_wp2, wp3_on_wp2, stats_zm )
      call stat_update_var( icloud_frac_zm, cloud_frac_zm, stats_zm )
      call stat_update_var( iice_supersat_frac_zm, ice_supersat_frac_zm, stats_zm )
      call stat_update_var( ircm_zm, rcm_zm, stats_zm )
      call stat_update_var( irtm_zm, rtm_zm, stats_zm )
      call stat_update_var( ithlm_zm, thlm_zm, stats_zm )
      if ( sclr_dim > 0 ) then
        do isclr=1, sclr_dim
          call stat_update_var( isclrp2(isclr), sclrp2(:,isclr), stats_zm )
          call stat_update_var( isclrprtp(isclr), sclrprtp(:,isclr), stats_zm )
          call stat_update_var( isclrpthvp(isclr), sclrpthvp(:,isclr), stats_zm )
          call stat_update_var( isclrpthlp(isclr), sclrpthlp(:,isclr), stats_zm )
          call stat_update_var( isclrprcp(isclr), sclrprcp(:,isclr), stats_zm )
          call stat_update_var( iwpsclrp(isclr), wpsclrp(:,isclr), stats_zm )
          call stat_update_var( iwp2sclrp(isclr), wp2sclrp(:,isclr), stats_zm )
          call stat_update_var( iwpsclrp2(isclr), wpsclrp2(:,isclr), stats_zm )
          call stat_update_var( iwpsclrprtp(isclr), wpsclrprtp(:,isclr), stats_zm )
          call stat_update_var( iwpsclrpthlp(isclr), wpsclrpthlp(:,isclr), stats_zm )
        end do
      end if
      if ( edsclr_dim > 0 ) then
        do isclr = 1, edsclr_dim
          call stat_update_var( iwpedsclrp(isclr), wpedsclrp(:,isclr), stats_zm )
        end do
      end if
! Calculate shear production
      if ( ishear > 0 ) then
        do k = 1, gr%nz-1, 1
          shear(k) = - upwp(k) * ( um(k+1) - um(k) ) * gr%invrs_dzm(k)  &
                     - vpwp(k) * ( vm(k+1) - vm(k) ) * gr%invrs_dzm(k)
        enddo
        shear(gr%nz) = 0.0_core_rknd
      end if
      call stat_update_var( ishear, shear, stats_zm )
! stats_sfc variables
! Cloud cover
      call stat_update_var_pt( icc, 1, maxval( cloud_frac(1:gr%nz) ), stats_sfc )
! Cloud base
      if ( iz_cloud_base > 0 ) then
        k = 1
        do while ( rcm(k) < rc_tol .and. k < gr%nz )
          k = k + 1
        enddo
        if ( k > 1 .and. k < gr%nz) then
! Use linear interpolation to find the exact height of the
! rc_tol kg/kg level.  Brian.
          call stat_update_var_pt( iz_cloud_base, 1, lin_interpolate_two_points( rc_tol, rcm(k), &
                                   rcm(k-1), gr%zt(k), gr%zt(k-1) ), stats_sfc )
        else
! Set the cloud base output to -10m, if it's clear.
! Known magic number
          call stat_update_var_pt( iz_cloud_base, 1, -10.0_core_rknd , stats_sfc )
        end if ! k > 1 and k < gr%nz ! k > 1 and k < gr%nz
      end if ! iz_cloud_base > 0 ! iz_cloud_base > 0
! Liquid Water Path
      if ( ilwp > 0 ) then
        xtmp &
        = vertical_integral &
               ( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                 rcm(2:gr%nz), gr%invrs_dzt(2:gr%nz) )
        call stat_update_var_pt( ilwp, 1, xtmp, stats_sfc )
      end if
! Vapor Water Path (Preciptable Water)
      if ( ivwp > 0 ) then
        xtmp &
        = vertical_integral &
               ( (gr%nz - 2 + 1), rho_ds_zt(2:gr%nz), &
                 ( rtm(2:gr%nz) - rcm(2:gr%nz) ), gr%invrs_dzt(2:gr%nz) )
        call stat_update_var_pt( ivwp, 1, xtmp, stats_sfc )
      end if
! Vertical average of thermodynamic level variables.
! Find the vertical average of thermodynamic level variables, averaged from
! level 2 (the first thermodynamic level above model surface) through
! level gr%nz (the top of the model).  Use the vertical averaging function
! found in fill_holes.F90.
! Vertical average of thlm.
      call stat_update_var_pt( ithlm_vert_avg, 1,  &
           vertical_avg( (gr%nz-2+1), rho_ds_zt(2:gr%nz), &
                         thlm(2:gr%nz), gr%invrs_dzt(2:gr%nz) ), &
                               stats_sfc )
! Vertical average of rtm.
      call stat_update_var_pt( irtm_vert_avg, 1,  &
           vertical_avg( (gr%nz-2+1), rho_ds_zt(2:gr%nz), &
                         rtm(2:gr%nz), gr%invrs_dzt(2:gr%nz) ), &
                               stats_sfc )
! Vertical average of um.
      call stat_update_var_pt( ium_vert_avg, 1,  &
           vertical_avg( (gr%nz-2+1), rho_ds_zt(2:gr%nz), &
                         um(2:gr%nz), gr%invrs_dzt(2:gr%nz) ), &
                               stats_sfc )
! Vertical average of vm.
      call stat_update_var_pt( ivm_vert_avg, 1,  &
           vertical_avg( (gr%nz-2+1), rho_ds_zt(2:gr%nz), &
                         vm(2:gr%nz), gr%invrs_dzt(2:gr%nz) ), &
                               stats_sfc )
! Vertical average of momentum level variables.
! Find the vertical average of momentum level variables, averaged over the
! entire vertical profile (level 1 through level gr%nz).  Use the vertical
! averaging function found in fill_holes.F90.
! Vertical average of wp2.
      call stat_update_var_pt( iwp2_vert_avg, 1,  &
           vertical_avg( (gr%nz-1+1), rho_ds_zm(1:gr%nz), &
                         wp2(1:gr%nz), gr%invrs_dzm(1:gr%nz) ), &
                               stats_sfc )
! Vertical average of up2.
      call stat_update_var_pt( iup2_vert_avg, 1,  &
           vertical_avg( (gr%nz-1+1), rho_ds_zm(1:gr%nz), &
                         up2(1:gr%nz), gr%invrs_dzm(1:gr%nz) ), &
                               stats_sfc )
! Vertical average of vp2.
      call stat_update_var_pt( ivp2_vert_avg, 1,  &
           vertical_avg( (gr%nz-1+1), rho_ds_zm(1:gr%nz), &
                         vp2(1:gr%nz), gr%invrs_dzm(1:gr%nz) ), &
                               stats_sfc )
! Vertical average of rtp2.
      call stat_update_var_pt( irtp2_vert_avg, 1,  &
           vertical_avg( (gr%nz-1+1), rho_ds_zm(1:gr%nz), &
                         rtp2(1:gr%nz), gr%invrs_dzm(1:gr%nz) ), &
                               stats_sfc )
! Vertical average of thlp2.
      call stat_update_var_pt( ithlp2_vert_avg, 1,  &
           vertical_avg( (gr%nz-1+1), rho_ds_zm(1:gr%nz), &
                         thlp2(1:gr%nz), gr%invrs_dzm(1:gr%nz) ), &
                               stats_sfc )
    end if ! l_stats_samp ! l_stats_samp
    return
        END SUBROUTINE stats_accumulate
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

!-----------------------------------------------------------------------

!===============================================================================
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
    END MODULE stats_clubb_utilities
