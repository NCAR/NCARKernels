
! KGEN-generated Fortran source file
!
! Filename    : advance_clubb_core_module.F90
! Generated at: 2015-10-21 08:59:09
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
        PUBLIC advance_clubb_core
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

        SUBROUTINE advance_clubb_core(dt, wm_zt, wp2, up2, vp2, um_forcing, vm_forcing, edsclrm_forcing, rho_ds_zm, &
        invrs_rho_ds_zt, fcor, l_implemented, um, vm, edsclrm, upwp, vpwp, err_code, kgen_unit, total_time)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
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
! Variable(s)
            USE parameters_model, ONLY: edsclr_dim
! Variable(s)
! Variable(s)
            USE grid_class, ONLY: gr
! Variable(s)
! Procedure(s)
! Procedure(s)
! Variable(s)
            USE variables_diagnostic_module, ONLY: vg
            USE variables_diagnostic_module, ONLY: um_ref
            USE variables_diagnostic_module, ONLY: vm_ref
            USE variables_diagnostic_module, ONLY: ug
            USE variables_diagnostic_module, ONLY: wpedsclrp
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
! Variable(s)
! Type
! Variable(s)
! Compute mean/flux terms
! Variable(s)
! Computes variance terms
! Procedure
! Procedure
! Prob. density function
! Procedure
            USE advance_windm_edsclrm_module, ONLY: advance_windm_edsclrm ! Procedure(s)
! Procedure
! Saturation mixing ratio
! Procedure
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
! Procedure(s)
! Procedure
! Procedure(s)
! Read values from namelist
! Procedure
! Procedure
! Procedure(s)
! Variable(s)
! Variable(s)
! Variable(s)
! Procedure(s)
! Procedure(s)
! Variable
! Procedure(s)
            IMPLICIT NONE
!!! External
            INTEGER, INTENT(IN) :: kgen_unit
            REAL(KIND=kgen_dp), INTENT(INOUT) :: total_time
            REAL(KIND=kgen_dp) :: elapsed_time
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            INTEGER, PARAMETER :: maxiter=10000
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
! Constant Parameters
! Lscale is calculated in subroutine compute_length; if l_avg_Lscale
! is true, compute_length is called two additional times with
! perturbed values of rtm and thlm.  An average value of Lscale
! from the three calls to compute_length is then calculated.
! This reduces temporal noise in RICO, BOMEX, LBA, and other cases.
! Alternate that uses the PDF to
! compute the perturbed values
!Includes the effects of ice latent heating in turbulence terms
! Set to true when rtp2/thlp2/rtpthlp, et cetera are prognostic
! Compute cloud_frac and rcm on a refined grid
! Should the refined grid code feed into the model?
! Only has meaning if l_refined_grid_in_cloud is .true.
! Value of chi(s) at saturation with respect to ice
! (zero for liquid)
! Use tau_N2_zm instead of tau_zm in wpxp_pr1
!!! Input Variables
            LOGICAL, intent(in) :: l_implemented
! Is this part of a larger host model (T/F) ?
            REAL(KIND=core_rknd), intent(in) :: dt
! Current timestep duration    [s]
            REAL(KIND=core_rknd), intent(in) :: fcor
! Coriolis forcing             [s^-1]
! Elevation of ground level    [m AMSL]
! Total number of hydrometeors          [#]
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: um_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: invrs_rho_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zt
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
! Collection of hydrometeors                [units vary]
! Buoyancy production at the CL top due to LW radiative cooling [m^2/s^3]
! Covariance of w and a hydrometeor      [(m/s) <hm units>]
! Third-order moment:  < w'^2 hm' >    [(m/s)^2 <hm units>]
! Covariance of rt and hm (on t-levs.) [(kg/kg) <hm units>]
! Covariance of thl and hm (on t-levs.)      [K <hm units>]
! w' theta_l' at surface   [(m K)/s]
! w' r_t' at surface       [(kg m)/( kg s)]
! u'w' at surface          [m^2/s^2]
! v'w' at surface          [m^2/s^2]
! Passive scalar variables
! Passive scalar forcing         [{units vary}/s]
! Scalar flux at surface         [{units vary} m/s]
! Eddy passive scalar variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,edsclr_dim) :: edsclrm_forcing
! Eddy passive scalar forcing    [{units vary}/s]
! Eddy-Scalar flux at surface    [{units vary} m/s]
! Host model horizontal grid spacing, if part of host model.
! East-West horizontal grid spacing     [m]
! North-South horizontal grid spacing   [m]
!!! Input/Output Variables
! These are prognostic or are planned to be in the future
            REAL(KIND=core_rknd) :: upwp(gr%nz)
            REAL(KIND=core_rknd) :: ref_upwp(gr%nz)
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: wp2
            REAL(KIND=core_rknd) :: vpwp(gr%nz)
            REAL(KIND=core_rknd) :: ref_vpwp(gr%nz)
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: up2
            REAL(KIND=core_rknd), intent(inout), dimension(gr%nz) :: vp2
            REAL(KIND=core_rknd) :: vm(gr%nz)
            REAL(KIND=core_rknd) :: ref_vm(gr%nz)
            REAL(KIND=core_rknd) :: um(gr%nz)
            REAL(KIND=core_rknd) :: ref_um(gr%nz)
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
! Passive scalar mean (thermo. levels) [units vary]
! w'sclr' (momentum levels)            [{units vary} m/s]
! sclr'^2 (momentum levels)            [{units vary}^2]
! sclr'rt' (momentum levels)           [{units vary} (kg/kg)]
! sclr'thl' (momentum levels)          [{units vary} K]
! Eddy passive scalar variable
            REAL(KIND=core_rknd) :: edsclrm(gr%nz,edsclr_dim)
            REAL(KIND=core_rknd) :: ref_edsclrm(gr%nz,edsclr_dim)
! Eddy passive scalar mean (thermo. levels)   [units vary]
! Variables that need to be output for use in other parts of the CLUBB
! code, such as microphysics (rcm, pdf_params), forcings (rcm), and/or
! BUGSrad (cloud_cover).
! cloud water mixing ratio, r_c (thermo. levels)  [kg/kg]
! rcm in cloud layer                              [kg/kg]
! cloud cover                                     [-]
! PDF parameters   [units vary]
! Variables that need to be output for use in host models
! w'r_c' (momentum levels)                  [(kg/kg) m/s]
! cloud fraction (thermodynamic levels)     [-]
! ice cloud fraction (thermodynamic levels) [-]
! Eric Raut declared this variable solely for output to disk
! Coefficient of X' R_l' in Eq. (34)        [-]
! eddy diffusivity on thermo levels
! eddy diffusivity on momentum levels
! cloud water variance
            REAL(KIND=core_rknd), dimension(gr%nz) :: km_zm
!!! Output Variable
! Diagnostic, for if some calculation goes amiss.
            INTEGER :: err_code
            INTEGER :: ref_err_code
!!! Local Variables
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
! w' sclr' on thermo. levels
! sclr'^2 on thermo. levels
! sclr' r_t' on thermo. levels
! sclr' th_l' on thermo. levels
! Pressure interpolated to momentum levels  [Pa]
! Exner interpolated to momentum levels     [-]
! Mean w (1st PDF component)                   [m/s]
! Mean w (2nd PDF component)                   [m/s]
! Variance of w (1st PDF component)            [m^2/s^2]
! Variance of w (2nd PDF component)            [m^2/s^2]
! Weight of 1st PDF component (Sk_w dependent) [-]
! Covariance of w and hm (on t-levs.) [(m/s) <hm units>]
! Moment <w'^2 hm'> (on m-levs.)    [(m/s)^2 <hm units>]
! Covariance of rt and hm           [(kg/kg) <hm units>]
! Covariance of thl and hm                [K <hm units>]
! Instance of w'r_t' clipping (1st or 3rd).
! Instance of w'th_l' clipping (1st or 3rd).
! Instance of w'sclr' clipping (1st or 3rd).
! Instance of u'w' clipping (1st or 2nd).
! Instance of v'w' clipping (1st or 2nd).
! These local variables are declared because they originally belong on the momentum
! grid levels, but pdf_closure outputs them on the thermodynamic grid levels.
! w'^4 (on thermo. grid)           [m^4/s^4]
! Buoyancy flux (on thermo. grid)  [(K m)/s]
! r_t' th_v' (on thermo. grid)     [(kg K)/kg]
! th_l' th_v' (on thermo. grid)    [K^2]
! w' r_c' (on thermo. grid)        [(m kg)/(s kg)]
! r_t' r_c' (on thermo. grid)      [(kg^2)/(kg^2)]
! th_l' r_c' (on thermo. grid)     [(K kg)/kg]
! r_c'^2 (on thermo. grid)         [(kg^2)/(kg^2)]
! X'R_l' coef. (on thermo. grid)   [-]
! sclr'th_v' (on thermo. grid)
! sclr'rc' (on thermo. grid)
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
! w'sclr'rt' on momentum grid
! w'sclr'^2 on momentum grid
! w'sclr'thl' on momentum grid
! w'^2 sclr' on momentum grid
! Passive scalar mean on momentum grid
! For l_avg_Lscale
! For l_Lscale_plume_centered
!The following variables are defined for use when l_use_ice_latent = .true.
! cloud_frac_1 computed on refined grid
! cloud_frac_2 computed on refined grid
! rc_1 computed on refined grid
! rc_2 computed on refined grid
! cloud_frac gridbox mean on refined grid
! rcm gridbox mean on refined grid
! Rain water mixing ratio
! Stability correction factor
! Tau with a static stability correction applied to it [s]
! Tau values used for the C6 (pr1) term in wpxp [s]
! Tau values used for the C1 (dp1) term in wp2 [s]
!----- Begin Code -----
! Determine the maximum allowable value for Lscale (in meters).
! intent(in)
! intent(out)
!----------------------------------------------------------------
! Test input variables
!----------------------------------------------------------------
!-----------------------------------------------------------------------
! Set up budget stats variables.
! SET SURFACE VALUES OF FLUXES (BROUGHT IN)
! We only do this for host models that do not apply the flux
! elsewhere in the code (e.g. WRF).  In other cases the _sfc variables will
! only be used to compute the variance at the surface. -dschanen 8 Sept 2009
! ~l_host_applies_sfc_fluxes
!---------------------------------------------------------------------------
! Interpolate wp3 to momentum levels, and wp2 to thermodynamic levels
! and then compute Skw for m & t grid
!---------------------------------------------------------------------------
! Positive definite quantity
! The right hand side of this conjunction is only for reducing cpu time,
! since the more complicated formula is mathematically equivalent
! Compute sigma_sqd_w (dimensionless PDF width parameter)
! Smooth in the vertical using interpolation
! Interpolate the the stats_zt grid
! Pos. def. quantity
! Compute the a3 coefficient (formula 25 in `Equations for CLUBB')
!   a3_coef = 3.0_core_rknd * sigma_sqd_w*sigma_sqd_w  &
!      + 6.0_core_rknd*(1.0_core_rknd-sigma_sqd_w)*sigma_sqd_w  &
!      + (1.0_core_rknd-sigma_sqd_w)*(1.0_core_rknd-sigma_sqd_w) &
!      - 3.0_core_rknd
! This is a simplified version of the formula above.
! We found we obtain fewer spikes in wp3 when we clip a3 to be no greater
! than -1.4 -dschanen 4 Jan 2011
! Known magic number
!---------------------------------------------------------------------------
! Interpolate thlp2, rtp2, and rtpthlp to thermodynamic levels,
!---------------------------------------------------------------------------
! Interpolate variances to the stats_zt grid (statistics and closure)
! Positive def. quantity
! Positive def. quantity
! Compute skewness velocity for stats output purposes
! Compute wp3 / wp2 on zt levels.  Always use the interpolated value in the
! denominator since it's less likely to create spikes
! Clip wp3_on_wp2_zt if it's too large
! Compute wp3_on_wp2 by interpolating wp3_on_wp2_zt
! Smooth again as above
!----------------------------------------------------------------
! Call closure scheme
!----------------------------------------------------------------
! Put passive scalar input on the t grid for the PDF
! i = 1, sclr_dim, 1
! Interpolate hydrometeor mixed moments to momentum levels.
! i = 1, hydromet_dim, 1
! k = 1, gr%nz, 1
! l_refine_grid_in_cloud
! l_call_pdf_closure_twice
! If l_trapezoidal_rule_zt is true, call trapezoidal_rule_zt for
! thermodynamic-level variables output from pdf_closure.
! ldgrant June 2009
! l_trapezoidal_rule_zt
! If l_trapezoidal_rule_zm is true, call trapezoidal_rule_zm for
! the important momentum-level variabes output from pdf_closure.
! ldgrant Feb. 2010
! l_trapezoidal_rule_zm
! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
! This code won't work unless rtm >= 0 !!!
! We do not clip rcm_in_layer because rcm_in_layer only influences
! radiation, and we do not want to bother recomputing it.
! Code is duplicated from below to ensure that relative humidity
! is calculated properly.  3 Sep 2009
! intent (in)
! intent (inout)
! Compute variables cloud_cover and rcm_in_layer.
! Added July 2009
! intent(in)
! intent(out)
! Use cloud_cover and rcm_in_layer to help boost cloud_frac and rcm to help
! increase cloudiness at coarser grid resolutions.
! Clip cloud fraction here if it still exceeds 1.0 due to round off
! Ditto with ice cloud fraction
! l_use_ice_latent = .true.
!----------------------------------------------------------------
! Compute thvm
!----------------------------------------------------------------
!----------------------------------------------------------------
! Compute tke (turbulent kinetic energy)
!----------------------------------------------------------------
!----------------------------------------------------------------
! Compute mixing length
!----------------------------------------------------------------
! l_avg_Lscale
! l_stats_samp
! ********** NOTE: **********
! This call to compute_length must be last.  Otherwise, the values of
! Lscale_up and Lscale_down in stats will be based on perturbation length scales
! rather than the mean length scale.
! intent(in)
! intent(in)
! intent(inout)
! intent(out)
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
! End Vince Larson's replacement.
! Determine the static stability corrected version of tau_zm
! Create a damping time scale that is more strongly damped at the
! altitudes where the Brunt-Vaisala frequency (N^2) is large.
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
!----------------------------------------------------------------
! Set Surface variances
!----------------------------------------------------------------
! Surface variances should be set here, before the call to either
! advance_xp2_xpyp or advance_wp2_wp3.
! Surface effects should not be included with any case where the lowest
! level is not the ground level.  Brian Griffin.  December 22, 2005.
! gr%zm(1) == sfc_elevation
!#######################################################################
!############## ADVANCE PROGNOSTIC VARIABLES ONE TIMESTEP ##############
!#######################################################################
! Store the saturation mixing ratio for output purposes.  Brian
! Compute rsat if either rsat or rel_humidity is to be saved.  ldgrant
! l_stats_samp
!----------------------------------------------------------------
! Advance rtm/wprtp and thlm/wpthlp one time step
!----------------------------------------------------------------
! Determine stability correction factor
! In
! Here we determine if we're using tau_zm or tau_N2_zm, which is tau
! that has been stability corrected for stably stratified regions.
! -dschanen 7 Nov 2014
! l_stability_correction
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
! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
! This code won't work unless rtm >= 0 !!!
! We do not clip rcm_in_layer because rcm_in_layer only influences
! radiation, and we do not want to bother recomputing it.  6 Aug 2009
! intent(in)
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
!----------------------------------------------------------------
! Covariance clipping for wprtp, wpthlp, wpsclrp, upwp, and vpwp
! after subroutine advance_xp2_xpyp updated xp2.
!----------------------------------------------------------------
! Second instance of w'r_t' clipping.
! Second instance of w'th_l' clipping.
! Second instance of w'sclr' clipping.
! First instance of u'w' clipping.
! First instance of v'w' clipping.
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
!----------------------------------------------------------------
! Advance 2nd and 3rd order moment of vertical velocity (wp2 / wp3)
! by one timestep
!----------------------------------------------------------------
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
! Third instance of w'r_t' clipping.
! Third instance of w'th_l' clipping.
! Third instance of w'sclr' clipping.
! Second instance of u'w' clipping.
! Second instance of v'w' clipping.
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
!----------------------------------------------------------------
! Advance the horizontal mean of the wind in the x-y directions
! (i.e. um, vm) and the mean of the eddy-diffusivity scalars
! (i.e. edsclrm) by one time step
!----------------------------------------------------------------i
            tolerance = 1.E-14
            CALL kgen_init_check(check_status, tolerance)
            READ(UNIT=kgen_unit) km_zm

            READ(UNIT=kgen_unit) ref_um
            READ(UNIT=kgen_unit) ref_vm
            READ(UNIT=kgen_unit) ref_edsclrm
            READ(UNIT=kgen_unit) ref_upwp
            READ(UNIT=kgen_unit) ref_vpwp
            READ(UNIT=kgen_unit) ref_err_code

            !Perturbation 
            !Add calls to kgen_perturb to perturbation test similar to below line.
            !CALL kgen_perturb(variable_name, perturbation_value)
            !EXAMPLE: CALL kgen_perturb(var, 1.0E-15_8)

            ! call to kernel
            CALL advance_windm_edsclrm(dt, wm_zt, km_zm, ug, vg, um_ref, vm_ref, wp2, up2, vp2, um_forcing, vm_forcing, edsclrm_forcing, rho_ds_zm, invrs_rho_ds_zt, fcor, l_implemented, um, vm, edsclrm, upwp, vpwp, wpedsclrp, err_code)
            ! kernel verification for output variables
            CALL kgen_verify_real_core_rknd_dim1( "um", check_status, um, ref_um)
            CALL kgen_verify_real_core_rknd_dim1( "vm", check_status, vm, ref_vm)
            CALL kgen_verify_real_core_rknd_dim2( "edsclrm", check_status, edsclrm, ref_edsclrm)
            CALL kgen_verify_real_core_rknd_dim1( "upwp", check_status, upwp, ref_upwp)
            CALL kgen_verify_real_core_rknd_dim1( "vpwp", check_status, vpwp, ref_vpwp)
            CALL kgen_verify_integer( "err_code", check_status, err_code, ref_err_code)
            CALL kgen_print_check("advance_windm_edsclrm", check_status)
            CALL system_clock(start_clock, rate_clock)
            DO kgen_intvar=1,maxiter
            CALL advance_windm_edsclrm(dt, wm_zt, km_zm, ug, vg, um_ref, vm_ref, wp2, up2, vp2, um_forcing, vm_forcing, edsclrm_forcing, rho_ds_zm, invrs_rho_ds_zt, fcor, l_implemented, um, vm, edsclrm, upwp, vpwp, wpedsclrp, err_code)
            END DO
            CALL system_clock(stop_clock, rate_clock)
            elapsed_time = 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)
            total_time = total_time + elapsed_time
            WRITE(*,*)
            PRINT *, "advance_windm_edsclrm : Time per call (usec): ", elapsed_time
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(in)
! intent(inout)
! intent(inout)
! intent(inout)
!#######################################################################
!#############            ACCUMULATE STATISTICS            #############
!#######################################################################
! l_stats_samp
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
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_core_rknd_dim1(var, kgen_unit, printvar)
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
            END SUBROUTINE kgen_read_real_core_rknd_dim1

            SUBROUTINE kgen_read_real_core_rknd_dim2(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2
                INTEGER, DIMENSION(2,2) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim2


        ! verify subroutines
            SUBROUTINE kgen_verify_real_core_rknd_dim1( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=core_rknd), intent(in), DIMENSION(:) :: var, ref_var
                real(KIND=core_rknd) :: nrmsdiff, rmsdiff
                real(KIND=core_rknd), allocatable, DIMENSION(:) :: temp, temp2
                integer :: n
                check_status%numTotal = check_status%numTotal + 1
                IF ( ALL( var == ref_var ) ) THEN
                
                    check_status%numIdentical = check_status%numIdentical + 1            
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                        !WRITE(*,*) "KERNEL: ", var
                        !WRITE(*,*) "REF.  : ", ref_var
                        IF ( ALL( var == 0 ) ) THEN
                            if(check_status%verboseLevel > 2) then
                                WRITE(*,*) "All values are zero."
                            end if
                        END IF
                    end if
                ELSE
                    allocate(temp(SIZE(var,dim=1)))
                    allocate(temp2(SIZE(var,dim=1)))
                
                    n = count(var/=ref_var)
                    where(abs(ref_var) > check_status%minvalue)
                        temp  = ((var-ref_var)/ref_var)**2
                        temp2 = (var-ref_var)**2
                    elsewhere
                        temp  = (var-ref_var)**2
                        temp2 = temp
                    endwhere
                    nrmsdiff = sqrt(sum(temp)/real(n))
                    rmsdiff = sqrt(sum(temp2)/real(n))
                
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                        if(check_status%verboseLevel > 1) then
                            WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                            WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                        endif
                        WRITE(*,*) "RMS of difference is ",rmsdiff
                        WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                    end if
                
                    if (nrmsdiff > check_status%tolerance) then
                        check_status%numFatal = check_status%numFatal+1
                    else
                        check_status%numWarning = check_status%numWarning+1
                    endif
                
                    deallocate(temp,temp2)
                END IF
            END SUBROUTINE kgen_verify_real_core_rknd_dim1

            SUBROUTINE kgen_verify_real_core_rknd_dim2( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=core_rknd), intent(in), DIMENSION(:,:) :: var, ref_var
                real(KIND=core_rknd) :: nrmsdiff, rmsdiff
                real(KIND=core_rknd), allocatable, DIMENSION(:,:) :: temp, temp2
                integer :: n
                check_status%numTotal = check_status%numTotal + 1
                IF ( ALL( var == ref_var ) ) THEN
                
                    check_status%numIdentical = check_status%numIdentical + 1            
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                        !WRITE(*,*) "KERNEL: ", var
                        !WRITE(*,*) "REF.  : ", ref_var
                        IF ( ALL( var == 0 ) ) THEN
                            if(check_status%verboseLevel > 2) then
                                WRITE(*,*) "All values are zero."
                            end if
                        END IF
                    end if
                ELSE
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2)))
                
                    n = count(var/=ref_var)
                    where(abs(ref_var) > check_status%minvalue)
                        temp  = ((var-ref_var)/ref_var)**2
                        temp2 = (var-ref_var)**2
                    elsewhere
                        temp  = (var-ref_var)**2
                        temp2 = temp
                    endwhere
                    nrmsdiff = sqrt(sum(temp)/real(n))
                    rmsdiff = sqrt(sum(temp2)/real(n))
                
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                        if(check_status%verboseLevel > 1) then
                            WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                            WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                        endif
                        WRITE(*,*) "RMS of difference is ",rmsdiff
                        WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                    end if
                
                    if (nrmsdiff > check_status%tolerance) then
                        check_status%numFatal = check_status%numFatal+1
                    else
                        check_status%numWarning = check_status%numWarning+1
                    endif
                
                    deallocate(temp,temp2)
                END IF
            END SUBROUTINE kgen_verify_real_core_rknd_dim2

            SUBROUTINE kgen_verify_integer( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer, intent(in) :: var, ref_var
                check_status%numTotal = check_status%numTotal + 1
                IF ( var == ref_var ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                    endif
                ELSE
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "KERNEL: ", var
                            WRITE(*,*) "REF.  : ", ref_var
                        end if
                    end if
                    check_status%numFatal = check_status%numFatal + 1
                END IF
            END SUBROUTINE kgen_verify_integer

        END SUBROUTINE advance_clubb_core
!-----------------------------------------------------------------------

!----------------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------------

!===============================================================================

!-----------------------------------------------------------------------
    END MODULE advance_clubb_core_module
