
! KGEN-generated Fortran source file
!
! Filename    : stats_variables.F90
! Generated at: 2015-10-20 14:26:59
! KGEN version: 0.5.3



    MODULE stats_variables
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
    USE stats_type, ONLY : kgen_read_mod13 => kgen_read
    USE stats_type, ONLY : kgen_verify_mod13 => kgen_verify
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
        LOGICAL, public :: l_stats            = .false.
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
        INTEGER, public :: iskw_zt = 0
        INTEGER, public :: iwprtp2 = 0
        INTEGER, public :: iwpthlp2 = 0
        INTEGER, public :: irvm = 0
        INTEGER, public :: ium_ref = 0
        INTEGER, public :: ivm_ref = 0
        INTEGER, public :: ithlm = 0
        INTEGER, public :: ithvm = 0
        INTEGER, public :: irtm = 0
        INTEGER, public :: ircm = 0
        INTEGER, public :: ium = 0
        INTEGER, public :: ivm = 0
        INTEGER, public :: iwm_zt = 0
        INTEGER, public :: iwm_zm = 0
        INTEGER, public :: iug = 0
        INTEGER, public :: ivg = 0
        INTEGER, public :: icloud_frac = 0
        INTEGER, public :: iice_supersat_frac = 0
        INTEGER, public :: ircm_in_layer = 0
        INTEGER, public :: icloud_cover = 0
        INTEGER, public :: ip_in_pa = 0
        INTEGER, public :: iexner = 0
        INTEGER, public :: irho_ds_zt = 0
        INTEGER, public :: ithv_ds_zt = 0
        INTEGER, public :: ilscale = 0
        INTEGER, public :: iwp3 = 0
        INTEGER, public :: iwp2thlp = 0
        INTEGER, public :: iwp2rtp = 0
        INTEGER, public :: ircm_in_cloud = 0
!$omp threadprivate(ithlm, ithvm, irtm, ircm, irvm, ium, ivm, ium_ref, ivm_ref, &
!$omp   iwm_zt, iwm_zm, iug, ivg, icloud_frac, iice_supersat_frac, ircm_in_layer, &
!$omp   ircm_in_cloud, icloud_cover, &
!$omp   ip_in_Pa, iexner, irho_ds_zt, ithv_ds_zt, iLscale, iwp3, &
!$omp   iwpthlp2, iwp2thlp, iwprtp2, iwp2rtp, iSkw_zt, iSkw_zm )
        INTEGER, public :: iwprtpthlp = 0
        INTEGER, public :: ilscale_pert_1 = 0
        INTEGER, public :: ilscale_pert_2 = 0
        INTEGER, public :: ilscale_up = 0
        INTEGER, public :: ilscale_down = 0
        INTEGER, public :: itau_zt = 0
        INTEGER, public :: ikh_zt = 0
        INTEGER, public :: iwp2thvp = 0
        INTEGER, public :: iwp2rcp = 0
        INTEGER, public :: isigma_sqd_w_zt = 0
        INTEGER, public :: irho = 0
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
        INTEGER, public :: irsat = 0
        INTEGER, public :: irel_humidity = 0
        INTEGER, public :: irsati = 0
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
        INTEGER, public :: irfrzm = 0
!$omp threadprivate(irfrzm)
! Skewness functions on stats_zt grid
        INTEGER, public :: ic11_skw_fnc = 0
!$omp threadprivate(iC11_Skw_fnc)
        INTEGER, public :: icloud_frac_zm = 0
        INTEGER, public :: iice_supersat_frac_zm = 0
        INTEGER, public :: ircm_zm = 0
        INTEGER, public :: irtm_zm = 0
        INTEGER, public :: ithlm_zm = 0
!$omp threadprivate(icloud_frac_zm, iice_supersat_frac_zm, ircm_zm, irtm_zm, ithlm_zm)
!$omp threadprivate(ilh_rcm_avg, ik_lh_start)
! Rain droplet number concentration
! Ice number concentration
! Snow number concentration
! Graupel number concentration
!$omp   threadprivate(iNrm, iNim, iNsm, iNgm)
        INTEGER, public :: it_in_k
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
        INTEGER, public :: irtm_bt = 0
        INTEGER, public :: ithlm_bt = 0
        INTEGER, public :: irtm_ma = 0
        INTEGER, public :: ithlm_ma = 0
        INTEGER, public :: irtm_ta = 0
        INTEGER, public :: ithlm_ta = 0
        INTEGER, public :: irtm_forcing = 0
        INTEGER, public :: ithlm_forcing = 0
        INTEGER, public :: irtm_pd = 0
        INTEGER, public :: irtm_cl = 0
        INTEGER, public :: ithlm_cl = 0
        INTEGER, public :: irtm_mfl = 0
        INTEGER, public :: ithlm_mfl = 0
        INTEGER, public :: irtm_tacl = 0
        INTEGER, public :: ithlm_tacl = 0
        INTEGER, public :: irtm_sdmp = 0
        INTEGER, public :: ithlm_sdmp = 0
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
        INTEGER, public :: ithlm_enter_mfl = 0
        INTEGER, public :: ithlm_old = 0
        INTEGER, public :: iwpthlp_entermfl = 0
        INTEGER, public :: irtm_enter_mfl = 0
        INTEGER, public :: irtm_old = 0
        INTEGER, public :: iwprtp_enter_mfl = 0
        INTEGER, public :: ithlm_without_ta = 0
        INTEGER, public :: ithlm_mfl_min = 0
        INTEGER, public :: ithlm_mfl_max = 0
        INTEGER, public :: iwpthlp_mfl_min = 0
        INTEGER, public :: iwpthlp_mfl_max = 0
        INTEGER, public :: irtm_without_ta = 0
        INTEGER, public :: irtm_mfl_min = 0
        INTEGER, public :: irtm_mfl_max = 0
        INTEGER, public :: iwprtp_mfl_min = 0
        INTEGER, public :: iwprtp_mfl_max = 0
        INTEGER, public :: ithlm_exit_mfl = 0
        INTEGER, public :: iwpthlp_exit_mfl = 0
        INTEGER, public :: irtm_exit_mfl = 0
        INTEGER, public :: iwprtp_exit_mfl = 0
!$omp   threadprivate(ithlm_mfl_min, ithlm_mfl_max, iwpthlp_entermfl)
!$omp   threadprivate(iwpthlp_exit_mfl, iwpthlp_mfl_min, iwpthlp_mfl_max)
!$omp   threadprivate(irtm_mfl_min, irtm_mfl_max, iwprtp_enter_mfl)
!$omp   threadprivate(iwprtp_exit_mfl, iwprtp_mfl_min, iwprtp_mfl_max)
!$omp   threadprivate(ithlm_enter_mfl, ithlm_exit_mfl, ithlm_old, ithlm_without_ta)
!$omp   threadprivate(irtm_enter_mfl, irtm_exit_mfl, irtm_old, irtm_without_ta)
        INTEGER, public :: iwp3_bt  = 0
        INTEGER, public :: iwp3_ta  = 0
        INTEGER, public :: iwp3_tp  = 0
        INTEGER, public :: iwp3_bp1 = 0
        INTEGER, public :: iwp3_pr2 = 0
        INTEGER, public :: iwp3_pr1 = 0
        INTEGER, public :: iwp3_dp1 = 0
        INTEGER, public :: iwp3_bp2 = 0
        INTEGER, public :: iwp3_ma  = 0
        INTEGER, public :: iwp3_ac  = 0
        INTEGER, public :: iwp3_cl  = 0
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
        INTEGER, public :: ivm_bt = 0
        INTEGER, public :: ivm_gf = 0
        INTEGER, public :: ivm_cf = 0
        INTEGER, public :: ivm_f = 0
        INTEGER, public :: ivm_ta = 0
        INTEGER, public :: ivm_ma = 0
        INTEGER, public :: ivm_sdmp = 0
        INTEGER, public :: ivm_ndg = 0
!$omp   threadprivate(ivm_bt, ivm_ma, ivm_ta, ivm_gf, ivm_cf, ivm_f, ivm_sdmp, ivm_ndg)
        INTEGER, public :: ium_bt = 0
        INTEGER, public :: ium_gf = 0
        INTEGER, public :: ium_cf = 0
        INTEGER, public :: ium_f = 0
        INTEGER, public :: ium_ta = 0
        INTEGER, public :: ium_ma = 0
        INTEGER, public :: ium_sdmp = 0
        INTEGER, public :: ium_ndg = 0
!$omp   threadprivate(ium_bt, ium_ma, ium_ta, ium_gf, ium_cf, ium_f, ium_sdmp, ium_ndg)
! PDF parameters
        INTEGER, public :: imixt_frac = 0
        INTEGER, public :: iw_1 = 0
        INTEGER, public :: iw_2 = 0
        INTEGER, public :: ivarnce_w_1 = 0
        INTEGER, public :: ivarnce_w_2 = 0
        INTEGER, public :: ithl_1 = 0
        INTEGER, public :: ithl_2 = 0
        INTEGER, public :: ivarnce_thl_1 = 0
        INTEGER, public :: ivarnce_thl_2 = 0
        INTEGER, public :: irt_1 = 0
        INTEGER, public :: irt_2 = 0
        INTEGER, public :: ivarnce_rt_1 = 0
        INTEGER, public :: ivarnce_rt_2 = 0
        INTEGER, public :: irc_1 = 0
        INTEGER, public :: irc_2 = 0
        INTEGER, public :: irsatl_1 = 0
        INTEGER, public :: irsatl_2 = 0
        INTEGER, public :: icloud_frac_1 = 0
        INTEGER, public :: icloud_frac_2 = 0
!$omp  threadprivate(imixt_frac, iw_1, iw_2, ivarnce_w_1, ivarnce_w_2, ithl_1, ithl_2, &
!$omp  ivarnce_thl_1, ivarnce_thl_2, irt_1, irt_2, ivarnce_rt_1, ivarnce_rt_2, irc_1, irc_2, &
!$omp  irsatl_1, irsatl_2, icloud_frac_1, icloud_frac_2 )
        INTEGER, public :: ichi_1 = 0
        INTEGER, public :: ichi_2 = 0
        INTEGER, public :: istdev_chi_1 = 0
        INTEGER, public :: istdev_chi_2 = 0
        INTEGER, public :: istdev_eta_1 = 0
        INTEGER, public :: istdev_eta_2 = 0
        INTEGER, public :: icovar_chi_eta_1 = 0
        INTEGER, public :: icovar_chi_eta_2 = 0
        INTEGER, public :: icorr_chi_eta_1 = 0
        INTEGER, public :: icorr_chi_eta_2 = 0
        INTEGER, public :: irrtthl = 0
        INTEGER, public :: icrt_1 = 0
        INTEGER, public :: icrt_2 = 0
        INTEGER, public :: icthl_1 = 0
        INTEGER, public :: icthl_2 = 0
        INTEGER, public :: ichip2 = 0
!$omp  threadprivate( ichi_1, ichi_2, istdev_chi_1, istdev_chi_2, ichip2, &
!$omp    istdev_eta_1, istdev_eta_2, icovar_chi_eta_1, icovar_chi_eta_2, &
!$omp    icorr_chi_eta_1, icorr_chi_eta_2, irrtthl, icrt_1, icrt_2, icthl_1, &
!$omp    icthl_2 )
        INTEGER, public :: iwpthlp_zt = 0
        INTEGER, public :: iwprtp_zt = 0
        INTEGER, public :: iup2_zt = 0
        INTEGER, public :: ivp2_zt = 0
        INTEGER, public :: iupwp_zt = 0
        INTEGER, public :: ivpwp_zt = 0
        INTEGER, public :: iwp2_zt = 0
        INTEGER, public :: ithlp2_zt = 0
        INTEGER, public :: irtp2_zt = 0
        INTEGER, public :: irtpthlp_zt = 0
!$omp   threadprivate( iwp2_zt, ithlp2_zt, iwpthlp_zt, iwprtp_zt, irtp2_zt, &
!$omp                  irtpthlp_zt, iup2_zt, ivp2_zt, iupwp_zt, ivpwp_zt )
!$omp   threadprivate( iwp2hmp )
!$omp   threadprivate( ihydrometp2, iwphydrometp, irtphmp, ithlphmp )
!$omp  threadprivate( ihmp2_zt )
        INTEGER, public :: ichi = 0
!$omp threadprivate(ichi)
        INTEGER, target, allocatable, dimension(:), public :: isclrm
        INTEGER, target, allocatable, dimension(:), public :: isclrm_f
! Passive scalar mean (1)
! Passive scalar forcing (1)
!$omp   threadprivate(isclrm, isclrm_f)
! Used to calculate clear-sky radiative fluxes.
!$omp   threadprivate(ifulwcl, ifdlwcl, ifdswcl, ifuswcl)
        INTEGER, target, allocatable, dimension(:), public :: iedsclrm
        INTEGER, target, allocatable, dimension(:), public :: iedsclrm_f
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
        INTEGER, public :: iskw_zm = 0
        INTEGER, public :: iwp4 = 0
        INTEGER, public :: ircp2 = 0
        INTEGER, public :: iwp2 = 0
        INTEGER, public :: irtp2 = 0
        INTEGER, public :: ithlp2 = 0
        INTEGER, public :: irtpthlp = 0
        INTEGER, public :: iwprtp = 0
        INTEGER, public :: iwpthlp = 0
        INTEGER, public :: iwpthvp = 0
        INTEGER, public :: irtpthvp = 0
        INTEGER, public :: ithlpthvp = 0
        INTEGER, public :: itau_zm = 0
        INTEGER, public :: ikh_zm = 0
        INTEGER, public :: iwprcp = 0
        INTEGER, public :: irc_coef = 0
        INTEGER, public :: ithlprcp = 0
        INTEGER, public :: irtprcp = 0
        INTEGER, public :: iupwp = 0
        INTEGER, public :: ivpwp = 0
        INTEGER, public :: imean_w_up = 0
        INTEGER, public :: imean_w_down = 0
        INTEGER, public :: irho_zm = 0
        INTEGER, public :: isigma_sqd_w = 0
        INTEGER, public :: irho_ds_zm = 0
        INTEGER, public :: ithv_ds_zm = 0
        INTEGER, public :: iem = 0
        INTEGER, public :: ifrad = 0
        INTEGER, public :: ishear = 0
! Brian
! Brian
! Brian
! Brian
! Brian
! Stability correction applied to Kh_N2_zm (diffusion on rtm and thlm)
        INTEGER, public :: istability_correction = 0
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
        INTEGER, public :: igamma_skw_fnc = 0
        INTEGER, public :: ic7_skw_fnc = 0
        INTEGER, public :: ic6rt_skw_fnc = 0
        INTEGER, public :: ic6thl_skw_fnc = 0
        INTEGER, public :: ic1_skw_fnc = 0
!$omp   threadprivate(igamma_Skw_fnc, iC6rt_Skw_fnc, iC6thl_Skw_fnc)
!$omp   threadprivate(iC7_Skw_fnc, iC1_Skw_fnc)
! Covariance of w and cloud droplet concentration, < w'N_c' >
!$omp   threadprivate( iwpNcp )
! Sedimentation velocities
!$omp   threadprivate(iVNr, iVrr, iVNc, iVrc, iVNs, iVrs, iVNi, iVri, iVrg)
! Covariance of sedimentation velocity and hydrometeor, <V_xx'x_x'>.
!$omp   threadprivate(iVrrprrp, iVNrpNrp, iVrrprrp_expcalc, iVNrpNrp_expcalc)
        INTEGER, public :: iwp2_bt = 0
        INTEGER, public :: iwp2_sf = 0
        INTEGER, public :: iwp2_cl = 0
        INTEGER, public :: iwp2_dp2 = 0
        INTEGER, public :: iwp2_bp = 0
        INTEGER, public :: iwp2_pr1 = 0
        INTEGER, public :: iwp2_pr2 = 0
        INTEGER, public :: iwp2_dp1 = 0
        INTEGER, public :: iwp2_pr3 = 0
        INTEGER, public :: iwp2_ta = 0
        INTEGER, public :: iwp2_ma = 0
        INTEGER, public :: iwp2_ac = 0
        INTEGER, public :: iwp2_pd = 0
!$omp   threadprivate(iwp2_bt, iwp2_ma, iwp2_ta, iwp2_ac, iwp2_bp)
!$omp   threadprivate(iwp2_pr1, iwp2_pr2, iwp2_pr3)
!$omp   threadprivate(iwp2_dp1, iwp2_dp2)
!$omp   threadprivate(iwp2_pd, iwp2_cl, iwp2_sf)
        INTEGER, public :: iwprtp_bt = 0
        INTEGER, public :: iwprtp_ma = 0
        INTEGER, public :: iwprtp_ta = 0
        INTEGER, public :: iwprtp_tp = 0
        INTEGER, public :: iwprtp_ac = 0
        INTEGER, public :: iwprtp_pr1 = 0
        INTEGER, public :: iwprtp_pr2 = 0
        INTEGER, public :: iwprtp_dp1 = 0
        INTEGER, public :: iwprtp_sicl = 0
        INTEGER, public :: iwprtp_bp = 0
        INTEGER, public :: iwprtp_pr3 = 0
        INTEGER, public :: iwprtp_forcing = 0
        INTEGER, public :: iwprtp_pd = 0
        INTEGER, public :: iwprtp_mfl = 0
        INTEGER, public :: iwprtp_cl = 0
!$omp   threadprivate(iwprtp_bt, iwprtp_ma, iwprtp_ta, iwprtp_tp)
!$omp   threadprivate(iwprtp_ac, iwprtp_bp, iwprtp_pr1, iwprtp_pr2)
!$omp   threadprivate(iwprtp_pr3, iwprtp_dp1, iwprtp_mfl, iwprtp_cl)
!$omp   threadprivate(iwprtp_sicl, iwprtp_pd, iwprtp_forcing, iwprtp_mc)
        INTEGER, public :: iwpthlp_bt = 0
        INTEGER, public :: iwpthlp_ma = 0
        INTEGER, public :: iwpthlp_ta = 0
        INTEGER, public :: iwpthlp_tp = 0
        INTEGER, public :: iwpthlp_ac = 0
        INTEGER, public :: iwpthlp_pr1 = 0
        INTEGER, public :: iwpthlp_pr2 = 0
        INTEGER, public :: iwpthlp_dp1 = 0
        INTEGER, public :: iwpthlp_sicl = 0
        INTEGER, public :: iwpthlp_bp = 0
        INTEGER, public :: iwpthlp_pr3 = 0
        INTEGER, public :: iwpthlp_forcing = 0
        INTEGER, public :: iwpthlp_mfl = 0
        INTEGER, public :: iwpthlp_cl = 0
!$omp   threadprivate(iwpthlp_bt, iwpthlp_ma, iwpthlp_ta, iwpthlp_tp)
!$omp   threadprivate(iwpthlp_ac, iwpthlp_bp, iwpthlp_pr1, iwpthlp_pr2)
!$omp   threadprivate(iwpthlp_pr3, iwpthlp_dp1, iwpthlp_mfl, iwpthlp_cl)
!$omp   threadprivate(iwpthlp_sicl, iwpthlp_forcing, iwpthlp_mc)
!    Dr. Golaz's new variance budget terms
!    qt was changed to rt to avoid confusion
        INTEGER, public :: irtp2_bt = 0
        INTEGER, public :: irtp2_sf = 0
        INTEGER, public :: irtp2_dp1 = 0
        INTEGER, public :: irtp2_dp2 = 0
        INTEGER, public :: irtp2_ta = 0
        INTEGER, public :: irtp2_ma = 0
        INTEGER, public :: irtp2_tp = 0
        INTEGER, public :: irtp2_forcing = 0
        INTEGER, public :: irtp2_pd = 0
        INTEGER, public :: irtp2_cl = 0
!$omp   threadprivate(irtp2_bt, irtp2_ma, irtp2_ta, irtp2_tp, irtp2_dp1)
!$omp   threadprivate(irtp2_dp2, irtp2_pd, irtp2_cl, irtp2_sf, irtp2_forcing)
!$omp   threadprivate(irtp2_mc)
        INTEGER, public :: ithlp2_bt = 0
        INTEGER, public :: ithlp2_sf = 0
        INTEGER, public :: ithlp2_dp1 = 0
        INTEGER, public :: ithlp2_dp2 = 0
        INTEGER, public :: ithlp2_ta = 0
        INTEGER, public :: ithlp2_ma = 0
        INTEGER, public :: ithlp2_tp = 0
        INTEGER, public :: ithlp2_forcing = 0
        INTEGER, public :: ithlp2_pd = 0
        INTEGER, public :: ithlp2_cl = 0
!$omp   threadprivate(ithlp2_bt, ithlp2_ma, ithlp2_ta, ithlp2_tp, ithlp2_dp1)
!$omp   threadprivate(ithlp2_dp2, ithlp2_pd, ithlp2_cl, ithlp2_sf)
!$omp   threadprivate(ithlp2_forcing, ithlp2_mc)
        INTEGER, public :: irtpthlp_bt = 0
        INTEGER, public :: irtpthlp_sf = 0
        INTEGER, public :: irtpthlp_cl = 0
        INTEGER, public :: irtpthlp_dp1 = 0
        INTEGER, public :: irtpthlp_dp2 = 0
        INTEGER, public :: irtpthlp_ta = 0
        INTEGER, public :: irtpthlp_ma = 0
        INTEGER, public :: irtpthlp_tp1 = 0
        INTEGER, public :: irtpthlp_tp2 = 0
        INTEGER, public :: irtpthlp_forcing = 0
!$omp   threadprivate(irtpthlp_bt, irtpthlp_ma, irtpthlp_ta)
!$omp   threadprivate(irtpthlp_tp1, irtpthlp_tp2, irtpthlp_dp1)
!$omp   threadprivate(irtpthlp_dp2, irtpthlp_cl, irtpthlp_sf, irtpthlp_forcing)
!$omp   threadprivate(irtpthlp_mc)
        INTEGER, public :: ivp2 = 0
        INTEGER, public :: iup2 = 0
!$omp   threadprivate(iup2, ivp2)
        INTEGER, public :: ivp2_bt = 0
        INTEGER, public :: iup2_bt = 0
        INTEGER, public :: iup2_sf = 0
        INTEGER, public :: ivp2_sf = 0
        INTEGER, public :: iup2_dp2 = 0
        INTEGER, public :: ivp2_dp2 = 0
        INTEGER, public :: ivp2_ta = 0
        INTEGER, public :: iup2_ta = 0
        INTEGER, public :: ivp2_ma = 0
        INTEGER, public :: iup2_ma = 0
        INTEGER, public :: iup2_dp1 = 0
        INTEGER, public :: iup2_pr1 = 0
        INTEGER, public :: ivp2_dp1 = 0
        INTEGER, public :: ivp2_pr1 = 0
        INTEGER, public :: ivp2_tp = 0
        INTEGER, public :: ivp2_pr2 = 0
        INTEGER, public :: iup2_tp = 0
        INTEGER, public :: iup2_pr2 = 0
        INTEGER, public :: iup2_pd = 0
        INTEGER, public :: ivp2_pd = 0
        INTEGER, public :: iup2_cl = 0
        INTEGER, public :: ivp2_cl = 0
!$omp   threadprivate(iup2_bt, iup2_ta, iup2_tp, iup2_ma, iup2_dp1)
!$omp   threadprivate(iup2_dp2, iup2_pr1, iup2_pr2, iup2_cl, iup2_sf)
!$omp   threadprivate(ivp2_bt, ivp2_ta, ivp2_tp, ivp2_ma, ivp2_dp1)
!$omp   threadprivate(ivp2_dp2, ivp2_pr1, ivp2_pr2, ivp2_cl)
!$omp   threadprivate(iup2_pd, ivp2_pd, ivp2_sf)
!       Passive scalars.  Note that floating point roundoff may make
!       mathematically equivalent variables different values.
        INTEGER, target, allocatable, dimension(:), public :: iwpsclrprtp
        INTEGER, target, allocatable, dimension(:), public :: iwpsclrpthlp
        INTEGER, target, allocatable, dimension(:), public :: iwpsclrp2
        INTEGER, target, allocatable, dimension(:), public :: isclrp2
        INTEGER, target, allocatable, dimension(:), public :: isclrprtp
        INTEGER, target, allocatable, dimension(:), public :: isclrpthvp
        INTEGER, target, allocatable, dimension(:), public :: isclrpthlp
        INTEGER, target, allocatable, dimension(:), public :: isclrprcp
        INTEGER, target, allocatable, dimension(:), public :: iwpsclrp
        INTEGER, target, allocatable, dimension(:), public :: iwp2sclrp
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
        INTEGER, target, allocatable, dimension(:), public :: iwpedsclrp
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
        INTEGER, public :: icc = 0
        INTEGER, public :: iz_cloud_base = 0
        INTEGER, public :: ilwp = 0
        INTEGER, public :: ivwp = 0
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
        INTEGER, public :: ithlm_vert_avg = 0
        INTEGER, public :: irtm_vert_avg = 0
        INTEGER, public :: ium_vert_avg = 0
        INTEGER, public :: ivm_vert_avg = 0
        INTEGER, public :: iwp2_vert_avg = 0
        INTEGER, public :: iup2_vert_avg = 0
        INTEGER, public :: ivp2_vert_avg = 0
        INTEGER, public :: irtp2_vert_avg = 0
        INTEGER, public :: ithlp2_vert_avg = 0
! nielsenb
! kcwhite
!$omp threadprivate(iwprtp_sfc, iupwp_sfc, ivpwp_sfc, &
!$omp   ithlm_vert_avg, irtm_vert_avg, ium_vert_avg, ivm_vert_avg, &
!$omp   iwp2_vert_avg, iup2_vert_avg, ivp2_vert_avg, irtp2_vert_avg, ithlp2_vert_avg, iT_sfc)
        INTEGER, public :: irtm_matrix_condt_num = 0
        INTEGER, public :: ithlm_matrix_condt_num = 0
        INTEGER, public :: irtp2_matrix_condt_num = 0
        INTEGER, public :: ithlp2_matrix_condt_num = 0
        INTEGER, public :: irtpthlp_matrix_condt_num = 0
        INTEGER, public :: iup2_vp2_matrix_condt_num = 0
        INTEGER, public :: iwp23_matrix_condt_num = 0
        INTEGER, public :: iwindm_matrix_condt_num = 0
!$omp threadprivate(iwp23_matrix_condt_num, irtm_matrix_condt_num, ithlm_matrix_condt_num, &
!$omp   irtp2_matrix_condt_num, ithlp2_matrix_condt_num, irtpthlp_matrix_condt_num, &
!$omp   iup2_vp2_matrix_condt_num, iwindm_matrix_condt_num)
!$omp threadprivate( imorr_snow_rate)
        INTEGER, public :: irtm_spur_src = 0
        INTEGER, public :: ithlm_spur_src = 0
!$omp threadprivate(irtm_spur_src, ithlm_spur_src)
        INTEGER, public :: iskw_velocity = 0
        INTEGER, public :: ia3_coef_zt = 0
        INTEGER, public :: iwp3_zm = 0
        INTEGER, public :: ia3_coef = 0
! Skewness velocity
!$omp threadprivate(iSkw_velocity, iwp3_zm, ia3_coef, ia3_coef_zt)
        INTEGER, public :: iwp3_on_wp2_zt = 0
        INTEGER, public :: iwp3_on_wp2 = 0
! w'^3 / w'^2 [m/s]
! w'^3 / w'^2 [m/s]
!$omp threadprivate(iwp3_on_wp2, iwp3_on_wp2_zt)
!$omp threadprivate( ilh_morr_snow_rate )
!$omp threadprivate( ilh_vwp, ilh_lwp )
        INTEGER, public :: icloud_frac_refined = 0
        INTEGER, public :: ircm_refined = 0
!$omp threadprivate( icloud_frac_refined, ircm_refined )
!$omp threadprivate( irtp2_from_chi )
! Variables that contains all the statistics
        TYPE(stats), target, public :: stats_zt
        TYPE(stats), target, public :: stats_zm
        TYPE(stats), target, public :: stats_sfc ! stats_zt grid
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
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr07
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr08
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr09
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr10
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr11
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr12
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr13
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr14
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr15
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: ztscr16
!$omp threadprivate(ztscr01, ztscr02, ztscr03, ztscr04, ztscr05)
!$omp threadprivate(ztscr06, ztscr07, ztscr08, ztscr09, ztscr10)
!$omp threadprivate(ztscr11, ztscr12, ztscr13, ztscr14, ztscr15)
!$omp threadprivate(ztscr16, ztscr17, ztscr18, ztscr19, ztscr20)
!$omp threadprivate(ztscr21)
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr01
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr02
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr03
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr04
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr05
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr06
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr07
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr08
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr09
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr10
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr11
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr12
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr13
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr14
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: zmscr15
!$omp   threadprivate(zmscr01, zmscr02, zmscr03, zmscr04, zmscr05)
!$omp   threadprivate(zmscr06, zmscr07, zmscr08, zmscr09, zmscr10)
!$omp   threadprivate(zmscr11, zmscr12, zmscr13, zmscr14, zmscr15)
!$omp   threadprivate(zmscr16, zmscr17)
        PUBLIC kgen_read_externs_stats_variables
    CONTAINS

    ! write subroutines
        SUBROUTINE kgen_read_integer_4_dim1_alloc(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            integer(KIND=4), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
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
        END SUBROUTINE kgen_read_integer_4_dim1_alloc

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
        READ(UNIT=kgen_unit) l_stats
        READ(UNIT=kgen_unit) l_stats_samp
        READ(UNIT=kgen_unit) irfrzm
        READ(UNIT=kgen_unit) iwp2_bt
        READ(UNIT=kgen_unit) ivp2_bt
        READ(UNIT=kgen_unit) iup2_bt
        READ(UNIT=kgen_unit) iwprtp_bt
        READ(UNIT=kgen_unit) iwpthlp_bt
        READ(UNIT=kgen_unit) irtp2_bt
        READ(UNIT=kgen_unit) ithlp2_bt
        READ(UNIT=kgen_unit) irtpthlp_bt
        READ(UNIT=kgen_unit) irtm_bt
        READ(UNIT=kgen_unit) ithlm_bt
        READ(UNIT=kgen_unit) ium_bt
        READ(UNIT=kgen_unit) ivm_bt
        READ(UNIT=kgen_unit) iwp3_bt
        READ(UNIT=kgen_unit) iskw_zt
        READ(UNIT=kgen_unit) iskw_zm
        READ(UNIT=kgen_unit) igamma_skw_fnc
        READ(UNIT=kgen_unit) iskw_velocity
        READ(UNIT=kgen_unit) iwp4
        READ(UNIT=kgen_unit) iwprtp2
        READ(UNIT=kgen_unit) iwpthlp2
        READ(UNIT=kgen_unit) iwprtpthlp
        READ(UNIT=kgen_unit) ircp2
        READ(UNIT=kgen_unit) icloud_frac_refined
        READ(UNIT=kgen_unit) ircm_refined
        CALL kgen_read_integer_4_dim1_alloc(iwpsclrp2, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(iwpsclrprtp, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(iwpsclrpthlp, kgen_unit)
        READ(UNIT=kgen_unit) ilscale_pert_1
        READ(UNIT=kgen_unit) ilscale_pert_2
        READ(UNIT=kgen_unit) irsat
        READ(UNIT=kgen_unit) ithlp2_sf
        READ(UNIT=kgen_unit) irtp2_sf
        READ(UNIT=kgen_unit) irtpthlp_sf
        READ(UNIT=kgen_unit) iup2_sf
        READ(UNIT=kgen_unit) ivp2_sf
        READ(UNIT=kgen_unit) iwp2_sf
        READ(UNIT=kgen_unit) irel_humidity
        READ(UNIT=kgen_unit) irvm
        READ(UNIT=kgen_unit) istability_correction
        READ(UNIT=kgen_unit) ic7_skw_fnc
        READ(UNIT=kgen_unit) ic6rt_skw_fnc
        READ(UNIT=kgen_unit) ic6thl_skw_fnc
        READ(UNIT=kgen_unit) imean_w_up
        READ(UNIT=kgen_unit) imean_w_down
        READ(UNIT=kgen_unit) irtm_ma
        READ(UNIT=kgen_unit) ithlm_ma
        READ(UNIT=kgen_unit) irtm_ta
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr01, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr02, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr03, kgen_unit)
        READ(UNIT=kgen_unit) ithlm_ta
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr04, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr05, kgen_unit)
        READ(UNIT=kgen_unit) iwprtp_ma
        READ(UNIT=kgen_unit) iwpthlp_ma
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr01, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr03, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr02, kgen_unit)
        READ(UNIT=kgen_unit) iwprtp_ta
        READ(UNIT=kgen_unit) iwpthlp_ta
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr05, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr04, kgen_unit)
        READ(UNIT=kgen_unit) iwprtp_ac
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr06, kgen_unit)
        READ(UNIT=kgen_unit) iwpthlp_tp
        READ(UNIT=kgen_unit) iwprtp_tp
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr07, kgen_unit)
        READ(UNIT=kgen_unit) iwprtp_pr2
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr08, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr09, kgen_unit)
        READ(UNIT=kgen_unit) iwpthlp_ac
        READ(UNIT=kgen_unit) iwprtp_pr1
        READ(UNIT=kgen_unit) iwpthlp_pr1
        READ(UNIT=kgen_unit) iwprtp_dp1
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr10, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr11, kgen_unit)
        READ(UNIT=kgen_unit) iwpthlp_pr2
        READ(UNIT=kgen_unit) iwpthlp_dp1
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr13, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr12, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr14, kgen_unit)
        READ(UNIT=kgen_unit) iwprtp_sicl
        READ(UNIT=kgen_unit) iwpthlp_sicl
        CALL kgen_read_real_core_rknd_dim1_alloc(zmscr15, kgen_unit)
        READ(UNIT=kgen_unit) irtm_forcing
        READ(UNIT=kgen_unit) iwprtp_bp
        READ(UNIT=kgen_unit) iwprtp_pr3
        READ(UNIT=kgen_unit) iwprtp_forcing
        READ(UNIT=kgen_unit) ithlm_forcing
        READ(UNIT=kgen_unit) iwpthlp_bp
        READ(UNIT=kgen_unit) iwpthlp_pr3
        READ(UNIT=kgen_unit) iwpthlp_forcing
        READ(UNIT=kgen_unit) irtm_matrix_condt_num
        READ(UNIT=kgen_unit) irtm_pd
        READ(UNIT=kgen_unit) irtm_cl
        READ(UNIT=kgen_unit) iwprtp_pd
        READ(UNIT=kgen_unit) ithlm_cl
        READ(UNIT=kgen_unit) ithlm_matrix_condt_num
        READ(UNIT=kgen_unit) iwprtp_mfl
        READ(UNIT=kgen_unit) irtm_mfl
        READ(UNIT=kgen_unit) iwpthlp_mfl
        READ(UNIT=kgen_unit) ithlm_mfl
        READ(UNIT=kgen_unit) ithlm_enter_mfl
        READ(UNIT=kgen_unit) ithlm_old
        READ(UNIT=kgen_unit) iwpthlp_entermfl
        READ(UNIT=kgen_unit) irtm_enter_mfl
        READ(UNIT=kgen_unit) irtm_old
        READ(UNIT=kgen_unit) iwprtp_enter_mfl
        READ(UNIT=kgen_unit) ithlm_without_ta
        READ(UNIT=kgen_unit) ithlm_mfl_min
        READ(UNIT=kgen_unit) ithlm_mfl_max
        READ(UNIT=kgen_unit) iwpthlp_mfl_min
        READ(UNIT=kgen_unit) iwpthlp_mfl_max
        READ(UNIT=kgen_unit) irtm_without_ta
        READ(UNIT=kgen_unit) irtm_mfl_min
        READ(UNIT=kgen_unit) irtm_mfl_max
        READ(UNIT=kgen_unit) iwprtp_mfl_min
        READ(UNIT=kgen_unit) iwprtp_mfl_max
        READ(UNIT=kgen_unit) ithlm_exit_mfl
        READ(UNIT=kgen_unit) iwpthlp_exit_mfl
        READ(UNIT=kgen_unit) irtm_exit_mfl
        READ(UNIT=kgen_unit) iwprtp_exit_mfl
        READ(UNIT=kgen_unit) iwprtp_cl
        READ(UNIT=kgen_unit) iwpthlp_cl
        READ(UNIT=kgen_unit) irtpthlp_cl
        READ(UNIT=kgen_unit) irtm_tacl
        READ(UNIT=kgen_unit) ithlm_tacl
        READ(UNIT=kgen_unit) irtm_sdmp
        READ(UNIT=kgen_unit) ithlm_sdmp
        READ(UNIT=kgen_unit) irtp2_ma
        READ(UNIT=kgen_unit) irtp2_dp1
        READ(UNIT=kgen_unit) ithlp2_dp1
        READ(UNIT=kgen_unit) irtpthlp_dp1
        READ(UNIT=kgen_unit) irtp2_dp2
        READ(UNIT=kgen_unit) ithlp2_dp2
        READ(UNIT=kgen_unit) irtpthlp_dp2
        READ(UNIT=kgen_unit) iup2_dp2
        READ(UNIT=kgen_unit) ivp2_dp2
        READ(UNIT=kgen_unit) irtp2_ta
        READ(UNIT=kgen_unit) ithlp2_ta
        READ(UNIT=kgen_unit) irtpthlp_ta
        READ(UNIT=kgen_unit) iup2_ta
        READ(UNIT=kgen_unit) ivp2_ta
        READ(UNIT=kgen_unit) ithlp2_ma
        READ(UNIT=kgen_unit) irtpthlp_ma
        READ(UNIT=kgen_unit) iup2_ma
        READ(UNIT=kgen_unit) ivp2_ma
        READ(UNIT=kgen_unit) irtp2_tp
        READ(UNIT=kgen_unit) irtp2_forcing
        READ(UNIT=kgen_unit) ithlp2_tp
        READ(UNIT=kgen_unit) ithlp2_forcing
        READ(UNIT=kgen_unit) irtpthlp_tp1
        READ(UNIT=kgen_unit) irtpthlp_tp2
        READ(UNIT=kgen_unit) irtpthlp_forcing
        READ(UNIT=kgen_unit) irtp2_matrix_condt_num
        READ(UNIT=kgen_unit) ithlp2_matrix_condt_num
        READ(UNIT=kgen_unit) irtpthlp_matrix_condt_num
        READ(UNIT=kgen_unit) iup2_vp2_matrix_condt_num
        READ(UNIT=kgen_unit) iup2_dp1
        READ(UNIT=kgen_unit) iup2_pr1
        READ(UNIT=kgen_unit) ivp2_dp1
        READ(UNIT=kgen_unit) ivp2_pr1
        READ(UNIT=kgen_unit) ivp2_tp
        READ(UNIT=kgen_unit) ivp2_pr2
        READ(UNIT=kgen_unit) iup2_tp
        READ(UNIT=kgen_unit) iup2_pr2
        READ(UNIT=kgen_unit) irtp2_pd
        READ(UNIT=kgen_unit) ithlp2_pd
        READ(UNIT=kgen_unit) iup2_pd
        READ(UNIT=kgen_unit) ivp2_pd
        READ(UNIT=kgen_unit) iwp2_cl
        READ(UNIT=kgen_unit) irtp2_cl
        READ(UNIT=kgen_unit) ithlp2_cl
        READ(UNIT=kgen_unit) iup2_cl
        READ(UNIT=kgen_unit) ivp2_cl
        READ(UNIT=kgen_unit) ic11_skw_fnc
        READ(UNIT=kgen_unit) ic1_skw_fnc
        READ(UNIT=kgen_unit) iwp2_dp2
        READ(UNIT=kgen_unit) iwp2_bp
        READ(UNIT=kgen_unit) iwp2_pr1
        READ(UNIT=kgen_unit) iwp2_pr2
        READ(UNIT=kgen_unit) iwp2_dp1
        READ(UNIT=kgen_unit) iwp2_pr3
        READ(UNIT=kgen_unit) iwp3_ta
        READ(UNIT=kgen_unit) iwp3_tp
        READ(UNIT=kgen_unit) iwp3_bp1
        READ(UNIT=kgen_unit) iwp3_pr2
        READ(UNIT=kgen_unit) iwp3_pr1
        READ(UNIT=kgen_unit) iwp3_dp1
        READ(UNIT=kgen_unit) iwp3_bp2
        READ(UNIT=kgen_unit) iwp2_ta
        READ(UNIT=kgen_unit) iwp2_ma
        READ(UNIT=kgen_unit) iwp2_ac
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr06, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr07, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr09, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr08, kgen_unit)
        READ(UNIT=kgen_unit) iwp3_ma
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr11, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr10, kgen_unit)
        READ(UNIT=kgen_unit) iwp3_ac
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr13, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr12, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr14, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr15, kgen_unit)
        CALL kgen_read_real_core_rknd_dim1_alloc(ztscr16, kgen_unit)
        READ(UNIT=kgen_unit) iwp23_matrix_condt_num
        READ(UNIT=kgen_unit) iwp2_pd
        READ(UNIT=kgen_unit) iwp3_cl
        READ(UNIT=kgen_unit) ium_gf
        READ(UNIT=kgen_unit) ium_cf
        READ(UNIT=kgen_unit) ium_f
        READ(UNIT=kgen_unit) ivm_gf
        READ(UNIT=kgen_unit) ivm_cf
        READ(UNIT=kgen_unit) ivm_f
        READ(UNIT=kgen_unit) ium_ta
        READ(UNIT=kgen_unit) ivm_ta
        READ(UNIT=kgen_unit) ium_ma
        READ(UNIT=kgen_unit) ivm_ma
        READ(UNIT=kgen_unit) iwindm_matrix_condt_num
        READ(UNIT=kgen_unit) ium_sdmp
        READ(UNIT=kgen_unit) ivm_sdmp
        READ(UNIT=kgen_unit) ium_ndg
        READ(UNIT=kgen_unit) ivm_ndg
        READ(UNIT=kgen_unit) ium_ref
        READ(UNIT=kgen_unit) ivm_ref
        READ(UNIT=kgen_unit) iwpthlp_zt
        READ(UNIT=kgen_unit) iwprtp_zt
        READ(UNIT=kgen_unit) iupwp_zt
        READ(UNIT=kgen_unit) iup2_zt
        READ(UNIT=kgen_unit) ivp2_zt
        READ(UNIT=kgen_unit) ivpwp_zt
        READ(UNIT=kgen_unit) it_in_k
        READ(UNIT=kgen_unit) irsati
        READ(UNIT=kgen_unit) ithlm
        READ(UNIT=kgen_unit) ithvm
        READ(UNIT=kgen_unit) irtm
        READ(UNIT=kgen_unit) ircm
        READ(UNIT=kgen_unit) ium
        READ(UNIT=kgen_unit) ivm
        READ(UNIT=kgen_unit) iwm_zt
        READ(UNIT=kgen_unit) iwm_zm
        READ(UNIT=kgen_unit) iug
        READ(UNIT=kgen_unit) ivg
        READ(UNIT=kgen_unit) icloud_frac
        READ(UNIT=kgen_unit) iice_supersat_frac
        READ(UNIT=kgen_unit) ircm_in_layer
        READ(UNIT=kgen_unit) icloud_cover
        READ(UNIT=kgen_unit) ip_in_pa
        READ(UNIT=kgen_unit) iexner
        READ(UNIT=kgen_unit) irho_ds_zt
        READ(UNIT=kgen_unit) ithv_ds_zt
        READ(UNIT=kgen_unit) ilscale
        READ(UNIT=kgen_unit) iwp3
        READ(UNIT=kgen_unit) iwp2thlp
        READ(UNIT=kgen_unit) iwp2rtp
        READ(UNIT=kgen_unit) ilscale_up
        READ(UNIT=kgen_unit) ilscale_down
        READ(UNIT=kgen_unit) itau_zt
        READ(UNIT=kgen_unit) ikh_zt
        READ(UNIT=kgen_unit) iwp2thvp
        READ(UNIT=kgen_unit) iwp2rcp
        READ(UNIT=kgen_unit) isigma_sqd_w_zt
        READ(UNIT=kgen_unit) irho
        READ(UNIT=kgen_unit) imixt_frac
        READ(UNIT=kgen_unit) iw_1
        READ(UNIT=kgen_unit) iw_2
        READ(UNIT=kgen_unit) ivarnce_w_1
        READ(UNIT=kgen_unit) ivarnce_w_2
        READ(UNIT=kgen_unit) ithl_1
        READ(UNIT=kgen_unit) ithl_2
        READ(UNIT=kgen_unit) ivarnce_thl_1
        READ(UNIT=kgen_unit) ivarnce_thl_2
        READ(UNIT=kgen_unit) irt_1
        READ(UNIT=kgen_unit) irt_2
        READ(UNIT=kgen_unit) ivarnce_rt_1
        READ(UNIT=kgen_unit) ivarnce_rt_2
        READ(UNIT=kgen_unit) irc_1
        READ(UNIT=kgen_unit) irc_2
        READ(UNIT=kgen_unit) irsatl_1
        READ(UNIT=kgen_unit) irsatl_2
        READ(UNIT=kgen_unit) icloud_frac_1
        READ(UNIT=kgen_unit) icloud_frac_2
        READ(UNIT=kgen_unit) ichi_1
        READ(UNIT=kgen_unit) ichi_2
        READ(UNIT=kgen_unit) istdev_chi_1
        READ(UNIT=kgen_unit) istdev_chi_2
        READ(UNIT=kgen_unit) istdev_eta_1
        READ(UNIT=kgen_unit) istdev_eta_2
        READ(UNIT=kgen_unit) icovar_chi_eta_1
        READ(UNIT=kgen_unit) icovar_chi_eta_2
        READ(UNIT=kgen_unit) icorr_chi_eta_1
        READ(UNIT=kgen_unit) icorr_chi_eta_2
        READ(UNIT=kgen_unit) irrtthl
        READ(UNIT=kgen_unit) icrt_1
        READ(UNIT=kgen_unit) icrt_2
        READ(UNIT=kgen_unit) icthl_1
        READ(UNIT=kgen_unit) icthl_2
        READ(UNIT=kgen_unit) iwp2_zt
        READ(UNIT=kgen_unit) ithlp2_zt
        READ(UNIT=kgen_unit) irtp2_zt
        READ(UNIT=kgen_unit) irtpthlp_zt
        READ(UNIT=kgen_unit) ichip2
        READ(UNIT=kgen_unit) ia3_coef_zt
        READ(UNIT=kgen_unit) iwp3_on_wp2_zt
        READ(UNIT=kgen_unit) ichi
        READ(UNIT=kgen_unit) ircm_in_cloud
        CALL kgen_read_integer_4_dim1_alloc(isclrm, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(isclrm_f, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(iedsclrm, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(iedsclrm_f, kgen_unit)
        READ(UNIT=kgen_unit) iwp2
        READ(UNIT=kgen_unit) iwp3_zm
        READ(UNIT=kgen_unit) irtp2
        READ(UNIT=kgen_unit) ithlp2
        READ(UNIT=kgen_unit) irtpthlp
        READ(UNIT=kgen_unit) iwprtp
        READ(UNIT=kgen_unit) iwpthlp
        READ(UNIT=kgen_unit) iwpthvp
        READ(UNIT=kgen_unit) irtpthvp
        READ(UNIT=kgen_unit) ithlpthvp
        READ(UNIT=kgen_unit) itau_zm
        READ(UNIT=kgen_unit) ikh_zm
        READ(UNIT=kgen_unit) iwprcp
        READ(UNIT=kgen_unit) irc_coef
        READ(UNIT=kgen_unit) ithlprcp
        READ(UNIT=kgen_unit) irtprcp
        READ(UNIT=kgen_unit) iupwp
        READ(UNIT=kgen_unit) ivpwp
        READ(UNIT=kgen_unit) ivp2
        READ(UNIT=kgen_unit) iup2
        READ(UNIT=kgen_unit) irho_zm
        READ(UNIT=kgen_unit) isigma_sqd_w
        READ(UNIT=kgen_unit) irho_ds_zm
        READ(UNIT=kgen_unit) ithv_ds_zm
        READ(UNIT=kgen_unit) iem
        READ(UNIT=kgen_unit) ifrad
        READ(UNIT=kgen_unit) ia3_coef
        READ(UNIT=kgen_unit) iwp3_on_wp2
        READ(UNIT=kgen_unit) icloud_frac_zm
        READ(UNIT=kgen_unit) iice_supersat_frac_zm
        READ(UNIT=kgen_unit) ircm_zm
        READ(UNIT=kgen_unit) irtm_zm
        READ(UNIT=kgen_unit) ithlm_zm
        CALL kgen_read_integer_4_dim1_alloc(isclrp2, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(isclrprtp, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(isclrpthvp, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(isclrpthlp, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(isclrprcp, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(iwpsclrp, kgen_unit)
        CALL kgen_read_integer_4_dim1_alloc(iwp2sclrp, kgen_unit)
        READ(UNIT=kgen_unit) ishear
        CALL kgen_read_integer_4_dim1_alloc(iwpedsclrp, kgen_unit)
        READ(UNIT=kgen_unit) iz_cloud_base
        READ(UNIT=kgen_unit) ivwp
        READ(UNIT=kgen_unit) icc
        READ(UNIT=kgen_unit) ilwp
        READ(UNIT=kgen_unit) ithlm_vert_avg
        READ(UNIT=kgen_unit) irtm_vert_avg
        READ(UNIT=kgen_unit) ium_vert_avg
        READ(UNIT=kgen_unit) ivm_vert_avg
        READ(UNIT=kgen_unit) iwp2_vert_avg
        READ(UNIT=kgen_unit) iup2_vert_avg
        READ(UNIT=kgen_unit) ivp2_vert_avg
        READ(UNIT=kgen_unit) irtp2_vert_avg
        READ(UNIT=kgen_unit) ithlp2_vert_avg
        READ(UNIT=kgen_unit) irtm_spur_src
        READ(UNIT=kgen_unit) ithlm_spur_src
        CALL kgen_read_mod13(stats_zt, kgen_unit)
        CALL kgen_read_mod13(stats_zm, kgen_unit)
        CALL kgen_read_mod13(stats_sfc, kgen_unit)
    END SUBROUTINE kgen_read_externs_stats_variables

    END MODULE stats_variables
