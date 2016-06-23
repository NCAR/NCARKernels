!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:50:00 
!KGEN version : 0.7.0 
  
!-----------------------------------------------------------------------
!  $Id: stats_clubb_utilities.F90 7377 2014-11-11 02:43:45Z bmg2@uwm.edu $
!===============================================================================
module stats_clubb_utilities

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    IMPLICIT NONE 

    PRIVATE 

    PUBLIC stats_begin_timestep, stats_accumulate 


  contains

  !-----------------------------------------------------------------------



































































  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------
  subroutine stats_begin_timestep( itime, stats_nsamp, stats_nout)

    !     Description:
    !       Given the elapsed time, set flags determining specifics such as
    !       if this time set should be sampled or if this is the first or
    !       last time step.
    !-----------------------------------------------------------------------

      USE stats_variables, ONLY: l_stats, l_stats_samp, l_stats_last 


    implicit none

    ! External
    intrinsic :: mod

    ! Input Variable(s)
    integer, intent(in) ::  & 
      itime, &       ! Elapsed model time       [timestep]
      stats_nsamp, & ! Stats sampling interval  [timestep]
      stats_nout     ! Stats output interval    [timestep]

    if ( .not. l_stats ) return

    ! Only sample time steps that are multiples of "stats_tsamp"
    ! in a case's "model.in" file to shorten length of run
    if ( mod( itime, stats_nsamp ) == 0 ) then
      l_stats_samp = .true.
    else
      l_stats_samp = .false.
    end if

    ! Indicates the end of the sampling time period. Signals to start writing to the file
    if ( mod( itime, stats_nout ) == 0 ) then
      l_stats_last = .true.
    else
      l_stats_last = .false.
    end if
   
    return

  end subroutine stats_begin_timestep

  !-----------------------------------------------------------------------











  !----------------------------------------------------------------------
  subroutine stats_accumulate & 
                   ( um, vm, upwp, vpwp, up2, vp2, &
                     thlm, rtm, wprtp, wpthlp, &
                     wp2, wp3, rtp2, thlp2, rtpthlp, &
                     p_in_Pa, exner, rho, rho_zm, &
                     rho_ds_zm, rho_ds_zt, thv_ds_zm, &
                     thv_ds_zt, wm_zt, wm_zm, rcm, wprcp, rc_coef, &
                     rcm_zm, rtm_zm, thlm_zm, cloud_frac, ice_supersat_frac, &
                     cloud_frac_zm, ice_supersat_frac_zm, rcm_in_layer, &
                     cloud_cover, sigma_sqd_w, pdf_params, &
                     sclrm, sclrp2, sclrprtp, sclrpthlp, sclrm_forcing, &
                     wpsclrp, edsclrm, edsclrm_forcing )

    ! Description:
    !   Accumulate those stats variables that are preserved in CLUBB from timestep to
    !   timestep, but not those stats that are not, (e.g. budget terms, longwave and
    !   shortwave components, etc.)
    !
    ! References:
    !   None
    !----------------------------------------------------------------------

      USE constants_clubb, ONLY: cloud_frac_min 


      USE pdf_utilities, ONLY: compute_variance_binormal 

      USE stats_variables, ONLY: stats_zt, stats_zm, stats_sfc, l_stats_samp, ithlm, it_in_k, ithvm, irtm, ircm, ium, ivm, &
      &iwm_zt, iwm_zm, iug, ivg, icloud_frac, iice_supersat_frac, ircm_in_layer, icloud_cover 

      USE stats_variables, ONLY: ip_in_pa, iexner, irho_ds_zt, ithv_ds_zt, ilscale, iwp3, iwp3_zm, iwpthlp2, iwp2thlp, iwprtp2, &
      &iwp2rtp, ilscale_up, ilscale_down, itau_zt, ikh_zt 

      USE stats_variables, ONLY: iwp2thvp, iwp2rcp, iwprtpthlp, isigma_sqd_w_zt, irho, irsat, irsati 

      USE stats_variables, ONLY: imixt_frac, iw_1, iw_2, ivarnce_w_1, ivarnce_w_2, ithl_1, ithl_2, ivarnce_thl_1, ivarnce_thl_2, &
      &irt_1, irt_2, ivarnce_rt_1, ivarnce_rt_2, irc_1, irc_2, irsatl_1, irsatl_2, icloud_frac_1, icloud_frac_2 

      USE stats_variables, ONLY: ichi_1, ichi_2, istdev_chi_1, istdev_chi_2, ichip2, istdev_eta_1, istdev_eta_2, &
      &icovar_chi_eta_1, icovar_chi_eta_2, icorr_chi_eta_1, icorr_chi_eta_2, icrt_1, icrt_2, icthl_1, icthl_2, irrtthl, ichi 

      USE stats_variables, ONLY: iwp2_zt, ithlp2_zt, iwpthlp_zt, iwprtp_zt, irtp2_zt, irtpthlp_zt, iup2_zt, ivp2_zt, iupwp_zt, &
      &ivpwp_zt, iwp2, irtp2, ithlp2, irtpthlp, iwprtp, iwpthlp, iwp4, iwpthvp, irtpthvp 

      USE stats_variables, ONLY: ithlpthvp, itau_zm, ikh_zm, iwprcp, irc_coef, ithlprcp, irtprcp, ircp2, iupwp, ivpwp, iup2, &
      &ivp2, irho_zm, isigma_sqd_w, irho_ds_zm, ithv_ds_zm, iem 

      USE stats_variables, ONLY: ishear, ifrad, icc, iz_cloud_base, ilwp, ivwp, ithlm_vert_avg, irtm_vert_avg, ium_vert_avg, &
      &ivm_vert_avg, iwp2_vert_avg, iup2_vert_avg, ivp2_vert_avg, irtp2_vert_avg, ithlp2_vert_avg 

      USE stats_variables, ONLY: isclrm, isclrm_f, iedsclrm, iedsclrm_f, isclrprtp, isclrp2, isclrpthvp, isclrpthlp, isclrprcp, &
      &iwpsclrp, iwp2sclrp, iwpsclrp2, iwpsclrprtp, iwpsclrpthlp, iwpedsclrp 

      USE stats_variables, ONLY: icloud_frac_zm, iice_supersat_frac_zm, ircm_zm, irtm_zm, ithlm_zm 

      USE stats_variables, ONLY: iwp3_on_wp2, iwp3_on_wp2_zt, iskw_velocity 

      USE stats_variables, ONLY: ia3_coef, ia3_coef_zt, ircm_in_cloud 

      USE grid_class, ONLY: gr 


      USE variables_diagnostic_module, ONLY: thvm, ug, vg, lscale, wpthlp2, wp2thlp, wprtp2, wp2rtp, lscale_up, lscale_down, &
      &tau_zt, kh_zt, wp2thvp, wp2rcp, wprtpthlp, sigma_sqd_w_zt, rsat 

      USE variables_diagnostic_module, ONLY: wp2_zt, thlp2_zt, wpthlp_zt, wprtp_zt, rtp2_zt, rtpthlp_zt, up2_zt, vp2_zt, upwp_zt, &
      &vpwp_zt, wp4, rtpthvp, thlpthvp, wpthvp, tau_zm, kh_zm, thlprcp, rtprcp, rcp2, em, frad, sclrpthvp, sclrprcp, wp2sclrp, &
      &wpsclrp2, wpsclrprtp, wpsclrpthlp, wpedsclrp 

      USE variables_diagnostic_module, ONLY: a3_coef, a3_coef_zt, wp3_zm, wp3_on_wp2, wp3_on_wp2_zt, skw_velocity 

      USE pdf_parameter_module, ONLY: pdf_parameter 

      USE t_in_k_module, ONLY: thlm2t_in_k 

      USE constants_clubb, ONLY: rc_tol 

      USE parameters_model, ONLY: sclr_dim, edsclr_dim 

      USE stats_type_utilities, ONLY: stat_update_var, stat_update_var_pt 

      USE fill_holes, ONLY: vertical_avg, vertical_integral 

      USE interpolation, ONLY: lin_interpolate_two_points 

      USE saturation, ONLY: sat_mixrat_ice 

      USE clubb_precision, ONLY: core_rknd 

      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
    implicit none

    ! Input Variable(s)
    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: & 
      um,      & ! u wind                        [m/s]
      vm,      & ! v wind                        [m/s]
      upwp,    & ! vertical u momentum flux      [m^2/s^2]
      vpwp,    & ! vertical v momentum flux      [m^2/s^2]
      up2,     & ! u'^2                          [m^2/s^2]
      vp2,     & ! v'^2                          [m^2/s^2]
      thlm,    & ! liquid potential temperature  [K]
      rtm,     & ! total water mixing ratio      [kg/kg]
      wprtp,   & ! w'rt'                         [(kg/kg) m/s]
      wpthlp,  & ! w'thl'                        [m K /s]
      wp2,     & ! w'^2                          [m^2/s^2]
      wp3,     & ! w'^3                          [m^3/s^3]
      rtp2,    & ! rt'^2                         [(kg/kg)^2]
      thlp2,   & ! thl'^2                        [K^2]
      rtpthlp    ! rt'thl'                       [kg/kg K]

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: & 
      p_in_Pa,      & ! Pressure (Pa) on thermodynamic points    [Pa]
      exner,        & ! Exner function = ( p / p0 ) ** kappa     [-]
      rho,          & ! Density                                  [kg/m^3]
      rho_zm,       & ! Density                                  [kg/m^3]
      rho_ds_zm,    & ! Dry, static density (momentum levels)    [kg/m^3]
      rho_ds_zt,    & ! Dry, static density (thermo. levs.)      [kg/m^3]
      thv_ds_zm,    & ! Dry, base-state theta_v (momentum levs.) [K]
      thv_ds_zt,    & ! Dry, base-state theta_v (thermo. levs.)  [K]
      wm_zt,        & ! w on thermodynamic levels                [m/s]
      wm_zm           ! w on momentum levels                     [m/s]

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: & 
      rcm_zm,               & ! Total water mixing ratio                 [kg/kg]
      rtm_zm,               & ! Total water mixing ratio                 [kg/kg]
      thlm_zm,              & ! Liquid potential temperature             [K]
      rcm,                  & ! Cloud water mixing ratio                 [kg/kg]
      wprcp,                & ! w'rc'                                    [(kg/kg) m/s]
      rc_coef,              & ! Coefficient of X' R_l' in Eq. (34)       [-]
      cloud_frac,           & ! Cloud fraction                           [-]
      ice_supersat_frac,    & ! Ice cloud fracion                        [-]
      cloud_frac_zm,        & ! Cloud fraction on zm levels              [-]
      ice_supersat_frac_zm, & ! Ice cloud fraction on zm levels          [-]
      rcm_in_layer,         & ! Cloud water mixing ratio in cloud layer  [kg/kg]
      cloud_cover             ! Cloud cover                              [-]

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      sigma_sqd_w    ! PDF width parameter (momentum levels)    [-]

    type(pdf_parameter), dimension(gr%nz), intent(in) :: & 
      pdf_params ! PDF parameters [units vary]

    real( kind = core_rknd ), intent(in), dimension(gr%nz,sclr_dim) :: & 
      sclrm,           & ! High-order passive scalar            [units vary]
      sclrp2,          & ! High-order passive scalar variance   [units^2]
      sclrprtp,        & ! High-order passive scalar covariance [units kg/kg]
      sclrpthlp,       & ! High-order passive scalar covariance [units K]
      sclrm_forcing,   & ! Large-scale forcing of scalar        [units/s]
      wpsclrp            ! w'sclr'                              [units m/s]

    real( kind = core_rknd ), intent(in), dimension(gr%nz,edsclr_dim) :: & 
      edsclrm,         & ! Eddy-diff passive scalar      [units vary] 
      edsclrm_forcing    ! Large-scale forcing of edscalar  [units vary]

    ! Local Variables

    integer :: isclr, k

    real( kind = core_rknd ), dimension(gr%nz) :: &
      T_in_K,      &  ! Absolute temperature         [K]
      rsati,       &  ! Saturation w.r.t ice         [kg/kg]
      shear,       &  ! Wind shear production term   [m^2/s^3]
      chi,         &  ! Mellor's 's'                 [kg/kg]
      chip2,         &  ! Variance of Mellor's 's'     [kg/kg]
      rcm_in_cloud    ! rcm in cloud                 [kg/kg]

    real( kind = core_rknd ) :: xtmp

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
        end where

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
 
        end if ! k > 1 and k < gr%nz

      end if ! iz_cloud_base > 0

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


    end if ! l_stats_samp


    return
  end subroutine stats_accumulate
!------------------------------------------------------------------------------











!------------------------------------------------------------------------------








    
  !-----------------------------------------------------------------------




!===============================================================================

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

end module stats_clubb_utilities