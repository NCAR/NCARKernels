
! KGEN-generated Fortran source file
!
! Filename    : stats_variables.F90
! Generated at: 2015-10-21 08:59:10
! KGEN version: 0.5.3



    MODULE stats_variables
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
    USE stats_type, ONLY : kgen_read_mod9 => kgen_read
    USE stats_type, ONLY : kgen_verify_mod9 => kgen_verify
        USE stats_type, ONLY: stats
! Type
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
        PRIVATE ! Set Default Scope
! Sampling and output frequencies
! Sampling interval   [s]
! Output interval     [s]
!$omp   threadprivate(stats_tsamp, stats_tout)
! Main flag to turn statistics on/off
! Flag to turn off radiation statistics output
! Output to NetCDF format
! Output to GrADS format
! Output SILHS files (stats_lh_zt and stats_lh_sfc)
! Do not stop if output timestep is too low for
! requested format, e.g. l_grads = .true. and
! stats_tout < 60.0
!$omp   threadprivate(l_stats, l_output_rad_files, l_netcdf, l_grads, l_silhs_out, &
!$omp     l_allow_small_stats_tout)
        LOGICAL, public :: l_stats_samp = .false.
! Sample flag for current time step
! Last time step of output period
!$omp   threadprivate(l_stats_samp, l_stats_last)
! Name of the stats file for thermodynamic grid fields
! Name of the stats file for LH variables on the stats_zt grid
! Name of the stats file for LH variables on the stats_zt grid
! Name of the stats file for momentum grid fields
! Name of the stats file for the stats_zt radiation grid fields
! Name of the stats file for the stats_zm radiation grid fields
! Name of the stats file for surface only fields
!$omp   threadprivate(fname_zt, fname_lh_zt, fname_lh_sfc, fname_zm, fname_rad_zt, &
!$omp     fname_rad_zm, fname_sfc)
!       Indices for statistics in stats_zt file
        INTEGER, public :: ium_ref = 0
        INTEGER, public :: ivm_ref = 0
!$omp threadprivate(ithlm, ithvm, irtm, ircm, irvm, ium, ivm, ium_ref, ivm_ref, &
!$omp   iwm_zt, iwm_zm, iug, ivg, icloud_frac, iice_supersat_frac, ircm_in_layer, &
!$omp   ircm_in_cloud, icloud_cover, &
!$omp   ip_in_Pa, iexner, irho_ds_zt, ithv_ds_zt, iLscale, iwp3, &
!$omp   iwpthlp2, iwp2thlp, iwprtp2, iwp2rtp, iSkw_zt, iSkw_zm )
!$omp threadprivate( iLscale_up, iLscale_down, &
!$omp   iLscale_pert_1, iLscale_pert_2, &
!$omp   itau_zt, iKh_zt, iwp2thvp, iwp2rcp, iwprtpthlp, isigma_sqd_w_zt, irho )
!$omp threadprivate( icorr_w_hm_ov_adj, ihm1, ihm2 )
!$omp threadprivate( iLWP1, iLWP2, iprecip_frac, &
!$omp   iprecip_frac_1, iprecip_frac_2, iNcnm )
!$omp threadprivate( imu_hm_1, imu_hm_2, imu_hm_1_n, imu_hm_2_n, &
!$omp   isigma_hm_1, isigma_hm_2, isigma_hm_1_n, isigma_hm_2_n, &
!$omp   icorr_w_hm_1, icorr_w_hm_2, icorr_chi_hm_1, icorr_chi_hm_2, &
!$omp   icorr_eta_hm_1, icorr_eta_hm_2, icorr_Ncn_hm_1, icorr_Ncn_hm_2, &
!$omp   icorr_w_hm_1_n, icorr_w_hm_2_n, icorr_chi_hm_1_n, icorr_chi_hm_2_n, &
!$omp   icorr_eta_hm_1_n, icorr_eta_hm_2_n, icorr_Ncn_hm_1_n, icorr_Ncn_hm_2_n )
!$omp threadprivate( icorr_hmx_hmy_1, icorr_hmx_hmy_2, &
!$omp   icorr_hmx_hmy_1_n, icorr_hmx_hmy_2_n )
!$omp threadprivate( imu_Ncn_1, imu_Ncn_2, imu_Ncn_1_n, imu_Ncn_2_n, &
!$omp   isigma_Ncn_1, isigma_Ncn_2, isigma_Ncn_1_n, isigma_Ncn_2_n )
!$omp threadprivate( icorr_w_chi_1, icorr_w_chi_2, icorr_w_eta_1, &
!$omp   icorr_w_eta_2, icorr_w_Ncn_1, icorr_w_Ncn_2, icorr_chi_eta_1_ca, &
!$omp   icorr_chi_eta_2_ca, icorr_chi_Ncn_1, icorr_chi_Ncn_2, icorr_eta_Ncn_1, &
!$omp   icorr_eta_Ncn_2 )
!$omp threadprivate( icorr_w_Ncn_1_n, icorr_w_Ncn_2_n, icorr_chi_Ncn_1_n, &
!$omp   icorr_chi_Ncn_2_n, icorr_eta_Ncn_1_n, icorr_eta_Ncn_2_n )
! Brian
! Adam Smith, 22 April 2008
! Brian
! Brian
! Brian
! Brian
! COAMPS only. dschanen 6 Dec 2006
! Brian
! analytic Kessler.  Vince Larson 22 May 2005
! LH Kessler.  Vince Larson  22 May 2005
! Radiative heating.
!   "           "   Long-wave component
!   "           "   Short-wave component
!$omp  threadprivate( iNcm, iNccnm, iNc_in_cloud, iNc_activated, isnowslope, &
!$omp    ised_rcm, irsat, irsati, irrm, &
!$omp    im_vol_rad_rain, im_vol_rad_cloud, &
!$omp    iprecip_rate_zt, iAKm, ilh_AKm, &
!$omp    iradht, iradht_LW, iradht_SW, &
!$omp    irel_humidity )
!$omp threadprivate( iAKstd, iAKstd_cld, iAKm_rcm, iAKm_rcc )
!$omp threadprivate(irfrzm)
! Skewness functions on stats_zt grid
!$omp threadprivate(iC11_Skw_fnc)
!$omp threadprivate(icloud_frac_zm, iice_supersat_frac_zm, ircm_zm, irtm_zm, ithlm_zm)
!$omp threadprivate(ilh_rcm_avg, ik_lh_start)
! Rain droplet number concentration
! Ice number concentration
! Snow number concentration
! Graupel number concentration
!$omp   threadprivate(iNrm, iNim, iNsm, iNgm)
! Absolute temperature
!$omp   threadprivate(iT_in_K)
!$omp   threadprivate(ieff_rad_cloud, ieff_rad_ice, ieff_rad_snow)
!$omp   threadprivate(ieff_rad_rain, ieff_rad_graupel)
! Diameter of ice crystal           [m]
! Mass of a single ice crystal      [kg]
! Change in liquid water due to ice [kg/kg/s]
! Fallspeed of ice crystal in cm/s  [cm s^{-1}]
!$omp threadprivate(irsm, irgm, irim, idiam, &
!$omp   imass_ice_cryst, ircm_icedfs, iu_T_cm)
! thlm/rtm budget terms
! rtm total time tendency
! rtm mean advect. term
! rtm turb. advect. term
! rtm large scale forcing term
! rtm change from microphysics
! rtm change from sponge damping
! rvm change from microphysics
! rcm change from microphysics
! rcm sedimentation tendency
! rtm change due to monotonic flux limiter
! rtm correction from turbulent advection (wprtp) clipping
! rtm clipping term
! thlm postive definite adj term
! thlm total time tendency
! thlm mean advect. term
! thlm turb. advect. term
! thlm large scale forcing term
! thlm change from sponge damping
! thlm change from microphysics
! thlm change due to monotonic flux limiter
! thlm correction from turbulent advection (wpthlp) clipping
! thlm clipping term
!$omp   threadprivate(irtm_bt, irtm_ma, irtm_ta, irtm_forcing, &
!$omp     irtm_mc, irtm_sdmp, irtm_mfl, irtm_tacl, irtm_cl, irtm_pd, &
!$omp     irvm_mc, ircm_mc, ircm_sd_mg_morr, &
!$omp     ithlm_bt, ithlm_ma, ithlm_ta, ithlm_forcing, &
!$omp     ithlm_mc, ithlm_sdmp, ithlm_mfl, ithlm_tacl, ithlm_cl)
!monatonic flux limiter diagnostic terms
!$omp   threadprivate(ithlm_mfl_min, ithlm_mfl_max, iwpthlp_entermfl)
!$omp   threadprivate(iwpthlp_exit_mfl, iwpthlp_mfl_min, iwpthlp_mfl_max)
!$omp   threadprivate(irtm_mfl_min, irtm_mfl_max, iwprtp_enter_mfl)
!$omp   threadprivate(iwprtp_exit_mfl, iwprtp_mfl_min, iwprtp_mfl_max)
!$omp   threadprivate(ithlm_enter_mfl, ithlm_exit_mfl, ithlm_old, ithlm_without_ta)
!$omp   threadprivate(irtm_enter_mfl, irtm_exit_mfl, irtm_old, irtm_without_ta)
!$omp   threadprivate(iwp3_bt, iwp3_ma, iwp3_ta, iwp3_tp, iwp3_ac, iwp3_bp1)
!$omp   threadprivate(iwp3_bp2, iwp3_pr1, iwp3_pr2, iwp3_dp1, iwp3_cl)
! Rain mixing ratio budgets
!$omp   threadprivate(irrm_bt, irrm_ma, irrm_ta, irrm_sd)
!$omp   threadprivate(irrm_ts, irrm_sd_morr)
!$omp   threadprivate(irrm_cond, irrm_auto, irrm_accr)
!$omp   threadprivate(irrm_cond_adj, irrm_src_adj )
!$omp   threadprivate(irrm_mc, irrm_hf, irrm_wvhf, irrm_cl)
!$omp   threadprivate(iNrm_bt, iNrm_ma, iNrm_ta, iNrm_sd, iNrm_ts, iNrm_cond)
!$omp   threadprivate(iNrm_auto, iNrm_cond_adj, iNrm_src_adj )
!$omp   threadprivate(iNrm_mc, iNrm_cl)
! Snow/Ice/Graupel mixing ratio budgets
!$omp   threadprivate(irsm_bt, irsm_ma, irsm_sd, irsm_sd_morr, irsm_ta)
!$omp   threadprivate(irsm_mc, irsm_hf, irsm_wvhf, irsm_cl)
!$omp   threadprivate(irsm_sd_morr_int)
!$omp   threadprivate(irgm_bt, irgm_ma, irgm_sd, irgm_sd_morr)
!$omp   threadprivate(irgm_ta, irgm_mc)
!$omp   threadprivate(irgm_hf, irgm_wvhf, irgm_cl)
!$omp   threadprivate(irim_bt, irim_ma, irim_sd, irim_sd_mg_morr, irim_ta)
!$omp   threadprivate(irim_mc, irim_hf, irim_wvhf, irim_cl)
!$omp threadprivate(iNsm_bt, iNsm_ma, iNsm_sd, iNsm_ta, &
!$omp   iNsm_mc, iNsm_cl)
!$omp threadprivate(iNgm_bt, iNgm_ma, iNgm_sd, &
!$omp   iNgm_ta, iNgm_mc, iNgm_cl)
!$omp threadprivate(iNim_bt, iNim_ma, iNim_sd, iNim_ta, &
!$omp   iNim_mc, iNim_cl)
!$omp threadprivate(iNcm_bt, iNcm_ma, iNcm_ta, &
!$omp   iNcm_mc, iNcm_cl, iNcm_act)
! Covariances between w, r_t, theta_l and KK microphysics tendencies.
! Additionally, covariances between r_r and N_r and KK rain drop mean
! volume radius.  These are all calculated on thermodynamic grid levels.
! Covariance of w and KK evaporation tendency.
! Covariance of r_t and KK evaporation tendency.
! Covariance of theta_l and KK evap. tendency.
! Covariance of w and KK autoconversion tendency.
! Covariance of r_t and KK autoconversion tendency.
! Covariance of theta_l and KK autoconv. tendency.
! Covariance of w and KK accretion tendency.
! Covariance of r_t and KK accretion tendency.
! Covariance of theta_l and KK accretion tendency.
! Covariance of r_r and KK mean volume radius.
! Covariance of N_r and KK mean volume radius.
! Variance of KK rain drop mean volume radius.
!$omp threadprivate( iw_KK_evap_covar_zt, irt_KK_evap_covar_zt, &
!$omp   ithl_KK_evap_covar_zt, iw_KK_auto_covar_zt, irt_KK_auto_covar_zt, &
!$omp   ithl_KK_auto_covar_zt, iw_KK_accr_covar_zt, irt_KK_accr_covar_zt, &
!$omp   ithl_KK_accr_covar_zt, irr_KK_mvr_covar_zt, iNr_KK_mvr_covar_zt, &
!$omp   iKK_mvr_variance_zt )
! Wind budgets
        INTEGER, public :: ivm_gf = 0
        INTEGER, public :: ivm_cf = 0
        INTEGER, public :: ivm_f = 0
        INTEGER, public :: ivm_ta = 0
        INTEGER, public :: ivm_ma = 0
        INTEGER, public :: ivm_sdmp = 0
        INTEGER, public :: ivm_ndg = 0
!$omp   threadprivate(ivm_bt, ivm_ma, ivm_ta, ivm_gf, ivm_cf, ivm_f, ivm_sdmp, ivm_ndg)
        INTEGER, public :: ium_gf = 0
        INTEGER, public :: ium_cf = 0
        INTEGER, public :: ium_f = 0
        INTEGER, public :: ium_ta = 0
        INTEGER, public :: ium_ma = 0
        INTEGER, public :: ium_sdmp = 0
        INTEGER, public :: ium_ndg = 0
!$omp   threadprivate(ium_bt, ium_ma, ium_ta, ium_gf, ium_cf, ium_f, ium_sdmp, ium_ndg)
! PDF parameters
!$omp  threadprivate(imixt_frac, iw_1, iw_2, ivarnce_w_1, ivarnce_w_2, ithl_1, ithl_2, &
!$omp  ivarnce_thl_1, ivarnce_thl_2, irt_1, irt_2, ivarnce_rt_1, ivarnce_rt_2, irc_1, irc_2, &
!$omp  irsatl_1, irsatl_2, icloud_frac_1, icloud_frac_2 )
!$omp  threadprivate( ichi_1, ichi_2, istdev_chi_1, istdev_chi_2, ichip2, &
!$omp    istdev_eta_1, istdev_eta_2, icovar_chi_eta_1, icovar_chi_eta_2, &
!$omp    icorr_chi_eta_1, icorr_chi_eta_2, irrtthl, icrt_1, icrt_2, icthl_1, &
!$omp    icthl_2 )
!$omp   threadprivate( iwp2_zt, ithlp2_zt, iwpthlp_zt, iwprtp_zt, irtp2_zt, &
!$omp                  irtpthlp_zt, iup2_zt, ivp2_zt, iupwp_zt, ivpwp_zt )
!$omp   threadprivate( iwp2hmp )
!$omp   threadprivate( ihydrometp2, iwphydrometp, irtphmp, ithlphmp )
!$omp  threadprivate( ihmp2_zt )
!$omp threadprivate(ichi)
! Passive scalar mean (1)
! Passive scalar forcing (1)
!$omp   threadprivate(isclrm, isclrm_f)
! Used to calculate clear-sky radiative fluxes.
!$omp   threadprivate(ifulwcl, ifdlwcl, ifdswcl, ifuswcl)
! Eddy-diff. scalar term (1)
! Eddy-diffusivity scalar forcing (1)
!$omp   threadprivate(iedsclrm, iedsclrm_f)
! Latin hypercube estimate of thlm_mc
! Latin hypercube estimate of rvm_mc
! Latin hypercube estimate of rcm_mc
! Latin hypercube estimate of Ncm_mc
! Latin hypercube estimate of rrm_mc
! Latin hypercube estimate of Nrm_mc
! Latin hypercube estimate of rsm_mc
! Latin hypercube estimate of Nsm_mc
! Latin hypercube estimate of rgm_mc
! Latin hypercube estimate of Ngm_mc
! Latin hypercube estimate of rim_mc
! Latin hypercube estimate of Nim_mc
!$omp   threadprivate( ilh_thlm_mc, ilh_rvm_mc, ilh_rcm_mc, ilh_Ncm_mc, &
!$omp     ilh_rrm_mc,  ilh_Nrm_mc, ilh_rsm_mc, ilh_Nsm_mc, &
!$omp     ilh_rgm_mc, ilh_Ngm_mc, ilh_rim_mc, ilh_Nim_mc )
! Latin hypercube estimate of autoconversion
! Latin hypercube estimate of accretion
! Latin hypercube estimate of evaporation
! Latin hypercube estimate of Nrm autoconversion
! Latin hypercube estimate of Nrm evaporation
!$omp   threadprivate( ilh_rrm_auto, ilh_rrm_accr, ilh_rrm_evap, &
!$omp                  ilh_Nrm_auto, ilh_Nrm_cond, &
!$omp                  ilh_m_vol_rad_rain )
! Latin hypercube estimate of source adjustment (KK only!)
! Latin hypercube estimate of evap adjustment (KK only!)
! Latin hypercube estimate of Nrm source adjustmet (KK only!)
! Latin hypercube estimate of Nrm evap adjustment (KK only!)
!$omp   threadprivate( ilh_rrm_src_adj, ilh_rrm_cond_adj, ilh_Nrm_src_adj, &
!$omp                  ilh_Nrm_cond_adj     )
! Latin hypercube estimate of rrm sedimentation velocity
! Latin hypercube estimate of Nrm sedimentation velocity
!$omp   threadprivate(ilh_Vrr,  ilh_VNr)
!$omp threadprivate(ilh_rrm, ilh_Nrm, ilh_rim, ilh_Nim, ilh_rsm, ilh_Nsm, &
!$omp   ilh_rgm, ilh_Ngm, &
!$omp   ilh_thlm, ilh_rcm, ilh_Ncm, ilh_Ncnm, ilh_rvm, ilh_wm, ilh_cloud_frac, &
!$omp   ilh_chi, ilh_eta, ilh_precip_frac, ilh_mixt_frac )
!$omp threadprivate( ilh_cloud_frac_unweighted, ilh_precip_frac_unweighted, &
!$omp                ilh_mixt_frac_unweighted )
! Eric Raut
!$omp threadprivate( ilh_wp2_zt, ilh_Nrp2_zt, ilh_Ncnp2_zt, ilh_Ncp2_zt, &
!$omp                ilh_rcp2_zt, ilh_rtp2_zt, ilh_thlp2_zt, ilh_rrp2_zt, ilh_chip2 )
! Indices for Morrison budgets
!$omp threadprivate( iPSMLT, iEVPMS, iPRACS, iEVPMG, iPRACG, iPGMLT, iMNUCCC, iPSACWS, iPSACWI, &
!$omp   iQMULTS, iQMULTG, iPSACWG, iPGSACW, iPRD, iPRCI, iPRAI, iQMULTR, &
!$omp   iQMULTRG, iMNUCCD, iPRACI, iPRACIS, iEPRD, iMNUCCR, iPIACR, iPIACRS, &
!$omp   iPGRACS, iPRDS, iEPRDS, iPSACR, iPRDG, iEPRDG  )
! More indices for Morrison budgets!!
!$omp threadprivate( iNGSTEN, iNRSTEN, iNISTEN, iNSSTEN, iNCSTEN, iNPRC1, iNRAGG, &
!$omp   iNPRACG, iNSUBR,  iNSMLTR, iNGMLTR, iNPRACS, iNNUCCR, iNIACR, &
!$omp   iNIACRS, iNGRACS, iNSMLTS, iNSAGG, iNPRCI, iNSCNG, iNSUBS, iPRC, iPRA, iPRE )
! More indices for Morrison budgets!!
!$omp threadprivate(iPCC, iNNUCCC, iNPSACWS, iNPRA, iNPRC, iNPSACWI, iNPSACWG, iNPRAI, &
!$omp   iNMULTS, iNMULTG, iNMULTR, iNMULTRG, iNNUCCD, iNSUBI, iNGMLTG, iNSUBG, iNACT, &
!$omp   iSIZEFIX_NR, iSIZEFIX_NC, iSIZEFIX_NI, iSIZEFIX_NS, iSIZEFIX_NG, iNEGFIX_NR, &
!$omp   iNEGFIX_NC, iNEGFIX_NI, iNEGFIX_NS, iNEGFIX_NG, iNIM_MORR_CL, iQC_INST, iQR_INST, &
!$omp   iQI_INST, iQS_INST, iQG_INST, iNC_INST, iNR_INST, iNI_INST, iNS_INST, &
!$omp   iNG_INST, iT_in_K_mc, ihl_on_Cp_residual, iqto_residual  )
! Indices for statistics in stats_zm file
! Brian
! Brian
! Brian
! Brian
! Brian
! Stability correction applied to Kh_N2_zm (diffusion on rtm and thlm)
! schemena
!$omp   threadprivate(istability_correction)
!$omp   threadprivate(iwp2, irtp2, ithlp2, irtpthlp, iwprtp, iwpthlp)
!$omp   threadprivate(iwp4, iwpthvp, irtpthvp, ithlpthvp, itau_zm, iKh_zm)
!$omp   threadprivate(iwprcp, irc_coef, ithlprcp, irtprcp, ircp2, iupwp, ivpwp)
!$omp   threadprivate(irho_zm, isigma_sqd_w, irho_ds_zm, ithv_ds_zm, iem, ishear)
!$omp   threadprivate(imean_w_up, imean_w_down)
!$omp   threadprivate(iFrad, iFrad_LW, iFrad_SW, iFrad_SW_up, iFrad_SW_down)
!$omp   threadprivate(iFrad_LW_up, iFrad_LW_down, iFprec, iFcsed)
!$omp   threadprivate(iK_hm)
! Skewness Functions on stats_zm grid
!$omp   threadprivate(igamma_Skw_fnc, iC6rt_Skw_fnc, iC6thl_Skw_fnc)
!$omp   threadprivate(iC7_Skw_fnc, iC1_Skw_fnc)
! Covariance of w and cloud droplet concentration, < w'N_c' >
!$omp   threadprivate( iwpNcp )
! Sedimentation velocities
!$omp   threadprivate(iVNr, iVrr, iVNc, iVrc, iVNs, iVrs, iVNi, iVri, iVrg)
! Covariance of sedimentation velocity and hydrometeor, <V_xx'x_x'>.
!$omp   threadprivate(iVrrprrp, iVNrpNrp, iVrrprrp_expcalc, iVNrpNrp_expcalc)
!$omp   threadprivate(iwp2_bt, iwp2_ma, iwp2_ta, iwp2_ac, iwp2_bp)
!$omp   threadprivate(iwp2_pr1, iwp2_pr2, iwp2_pr3)
!$omp   threadprivate(iwp2_dp1, iwp2_dp2)
!$omp   threadprivate(iwp2_pd, iwp2_cl, iwp2_sf)
        INTEGER, public :: iwprtp_cl = 0
!$omp   threadprivate(iwprtp_bt, iwprtp_ma, iwprtp_ta, iwprtp_tp)
!$omp   threadprivate(iwprtp_ac, iwprtp_bp, iwprtp_pr1, iwprtp_pr2)
!$omp   threadprivate(iwprtp_pr3, iwprtp_dp1, iwprtp_mfl, iwprtp_cl)
!$omp   threadprivate(iwprtp_sicl, iwprtp_pd, iwprtp_forcing, iwprtp_mc)
        INTEGER, public :: iwpthlp_cl = 0
!$omp   threadprivate(iwpthlp_bt, iwpthlp_ma, iwpthlp_ta, iwpthlp_tp)
!$omp   threadprivate(iwpthlp_ac, iwpthlp_bp, iwpthlp_pr1, iwpthlp_pr2)
!$omp   threadprivate(iwpthlp_pr3, iwpthlp_dp1, iwpthlp_mfl, iwpthlp_cl)
!$omp   threadprivate(iwpthlp_sicl, iwpthlp_forcing, iwpthlp_mc)
!    Dr. Golaz's new variance budget terms
!    qt was changed to rt to avoid confusion
!$omp   threadprivate(irtp2_bt, irtp2_ma, irtp2_ta, irtp2_tp, irtp2_dp1)
!$omp   threadprivate(irtp2_dp2, irtp2_pd, irtp2_cl, irtp2_sf, irtp2_forcing)
!$omp   threadprivate(irtp2_mc)
!$omp   threadprivate(ithlp2_bt, ithlp2_ma, ithlp2_ta, ithlp2_tp, ithlp2_dp1)
!$omp   threadprivate(ithlp2_dp2, ithlp2_pd, ithlp2_cl, ithlp2_sf)
!$omp   threadprivate(ithlp2_forcing, ithlp2_mc)
        INTEGER, public :: irtpthlp_cl = 0
!$omp   threadprivate(irtpthlp_bt, irtpthlp_ma, irtpthlp_ta)
!$omp   threadprivate(irtpthlp_tp1, irtpthlp_tp2, irtpthlp_dp1)
!$omp   threadprivate(irtpthlp_dp2, irtpthlp_cl, irtpthlp_sf, irtpthlp_forcing)
!$omp   threadprivate(irtpthlp_mc)
!$omp   threadprivate(iup2, ivp2)
!$omp   threadprivate(iup2_bt, iup2_ta, iup2_tp, iup2_ma, iup2_dp1)
!$omp   threadprivate(iup2_dp2, iup2_pr1, iup2_pr2, iup2_cl, iup2_sf)
!$omp   threadprivate(ivp2_bt, ivp2_ta, ivp2_tp, ivp2_ma, ivp2_dp1)
!$omp   threadprivate(ivp2_dp2, ivp2_pr1, ivp2_pr2, ivp2_cl)
!$omp   threadprivate(iup2_pd, ivp2_pd, ivp2_sf)
!       Passive scalars.  Note that floating point roundoff may make
!       mathematically equivalent variables different values.
! sclr'(1)rt'     / rt'^2
! sclr'(1)^2      / rt'^2
! sclr'(1)th_v'   / rt'th_v'
! sclr'(1)th_l'   / rt'th_l'
! sclr'(1)rc'     / rt'rc'
! w'slcr'(1)      / w'rt'
! w'^2 sclr'(1)   / w'^2 rt'
! w'sclr'(1)^2    / w'rt'^2
! w'sclr'(1)rt'   / w'rt'^2
! w'sclr'(1)th_l' / w'rt'th_l'
!$omp   threadprivate(isclrprtp, isclrp2, isclrpthvp, isclrpthlp)
!$omp   threadprivate(isclrprcp, iwpsclrp, iwp2sclrp, iwpsclrp2)
!$omp   threadprivate(iwpsclrprtp, iwpsclrpthlp)
! eddy sclr'(1)w'
!$omp threadprivate(iwpedsclrp)
! Indices for statistics in stats_rad_zt file
!$omp threadprivate( iT_in_K_rad, ircil_rad, io3l_rad, &
!$omp   irsm_rad, ircm_in_cloud_rad, icloud_frac_rad, &
!$omp   iice_supersat_frac_rad, &
!$omp   iradht_rad, iradht_LW_rad, iradht_SW_rad, &
!$omp   ip_in_mb_rad, isp_humidity_rad )
! Indices for statistics in stats_rad_zm file
!$omp threadprivate(iFrad_LW_rad, iFrad_SW_rad, iFrad_SW_up_rad)
!$omp threadprivate(iFrad_LW_up_rad, iFrad_SW_down_rad, iFrad_LW_down_rad)
! Indices for statistics in stats_sfc file
! nielsenb
! nielsenb
! nielsenb
! Brian
! Brian
! Brian
!$omp threadprivate(iustar, isoil_heat_flux, iveg_T_in_K, isfc_soil_T_in_K, ideep_soil_T_in_K, &
!$omp   ilh, ish, icc, ilwp, ivwp, iiwp, iswp, irwp, iz_cloud_base, iz_inversion, &
!$omp   iprecip_rate_sfc, irain_flux_sfc, irrm_sfc, &
!$omp   iwpthlp_sfc )
! nielsenb
! kcwhite
!$omp threadprivate(iwprtp_sfc, iupwp_sfc, ivpwp_sfc, &
!$omp   ithlm_vert_avg, irtm_vert_avg, ium_vert_avg, ivm_vert_avg, &
!$omp   iwp2_vert_avg, iup2_vert_avg, ivp2_vert_avg, irtp2_vert_avg, ithlp2_vert_avg, iT_sfc)
        INTEGER, public :: iwindm_matrix_condt_num = 0
!$omp threadprivate(iwp23_matrix_condt_num, irtm_matrix_condt_num, ithlm_matrix_condt_num, &
!$omp   irtp2_matrix_condt_num, ithlp2_matrix_condt_num, irtpthlp_matrix_condt_num, &
!$omp   iup2_vp2_matrix_condt_num, iwindm_matrix_condt_num)
!$omp threadprivate( imorr_snow_rate)
!$omp threadprivate(irtm_spur_src, ithlm_spur_src)
! Skewness velocity
!$omp threadprivate(iSkw_velocity, iwp3_zm, ia3_coef, ia3_coef_zt)
! w'^3 / w'^2 [m/s]
! w'^3 / w'^2 [m/s]
!$omp threadprivate(iwp3_on_wp2, iwp3_on_wp2_zt)
!$omp threadprivate( ilh_morr_snow_rate )
!$omp threadprivate( ilh_vwp, ilh_lwp )
!$omp threadprivate( icloud_frac_refined, ircm_refined )
!$omp threadprivate( irtp2_from_chi )
! Variables that contains all the statistics
        TYPE(stats), target, public :: stats_zt
        TYPE(stats), target, public :: stats_sfc
        TYPE(stats), target, public :: stats_zm ! stats_zt grid
! stats_zm grid
! stats_lh_zt grid
! stats_lh_sfc grid
! stats_rad_zt grid
! stats_rad_zm grid
! stats_sfc
!$omp threadprivate(stats_zt, stats_zm, stats_lh_zt, stats_lh_sfc)
!$omp threadprivate(stats_rad_zt, stats_rad_zm, stats_sfc)
! Scratch space
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr01
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr02
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr03
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr04
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr05
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr06
!$omp threadprivate(ztscr01, ztscr02, ztscr03, ztscr04, ztscr05)
!$omp threadprivate(ztscr06, ztscr07, ztscr08, ztscr09, ztscr10)
!$omp threadprivate(ztscr11, ztscr12, ztscr13, ztscr14, ztscr15)
!$omp threadprivate(ztscr16, ztscr17, ztscr18, ztscr19, ztscr20)
!$omp threadprivate(ztscr21)
!$omp   threadprivate(zmscr01, zmscr02, zmscr03, zmscr04, zmscr05)
!$omp   threadprivate(zmscr06, zmscr07, zmscr08, zmscr09, zmscr10)
!$omp   threadprivate(zmscr11, zmscr12, zmscr13, zmscr14, zmscr15)
!$omp   threadprivate(zmscr16, zmscr17)
        PUBLIC kgen_read_externs_stats_variables
    CONTAINS

    ! write subroutines
        SUBROUTINE kgen_read_real_core_rknd_dim1_alloc(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1
            INTEGER, DIMENSION(2,1) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_real_core_rknd_dim1_alloc


    ! module extern variables

    SUBROUTINE kgen_read_externs_stats_variables(kgen_unit)
        INTEGER, INTENT(IN) :: kgen_unit
        READ(UNIT=kgen_unit) ium_gf
        READ(UNIT=kgen_unit) ium_cf
        READ(UNIT=kgen_unit) ium_f
        READ(UNIT=kgen_unit) ivm_gf
        READ(UNIT=kgen_unit) ivm_cf
        READ(UNIT=kgen_unit) ivm_f
        READ(UNIT=kgen_unit) l_stats_samp
        READ(UNIT=kgen_unit) ium_ta
        READ(UNIT=kgen_unit) ivm_ta
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr03, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr02, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr01, kgen_unit)
        READ(UNIT=kgen_unit) ium_ma
        READ(UNIT=kgen_unit) ivm_ma
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr04, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr05, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr06, kgen_unit)
        READ(UNIT=kgen_unit) iwindm_matrix_condt_num
        READ(UNIT=kgen_unit) ium_sdmp
        READ(UNIT=kgen_unit) ivm_sdmp
        READ(UNIT=kgen_unit) ium_ndg
        READ(UNIT=kgen_unit) ivm_ndg
        READ(UNIT=kgen_unit) ium_ref
        READ(UNIT=kgen_unit) ivm_ref
        READ(UNIT=kgen_unit) iwprtp_cl
        READ(UNIT=kgen_unit) iwpthlp_cl
        READ(UNIT=kgen_unit) irtpthlp_cl
        CALL kgen_read_mod9(stats_zt, kgen_unit)
        CALL kgen_read_mod9(stats_sfc, kgen_unit)
        CALL kgen_read_mod9(stats_zm, kgen_unit)
    END SUBROUTINE kgen_read_externs_stats_variables

    END MODULE stats_variables
