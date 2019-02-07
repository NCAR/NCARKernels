!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:34 
!KGEN version : 0.8.1 
  
!--------------------------------------------------------------------------------------------------
! $Id: clubb_api_module.F90 7361 2014-11-04 21:51:02Z bmg2@uwm.edu $
!==================================================================================================
!       ########  ###       ###    ### #########  #########           ###     ######### ###########
!     ###    ### ###       ###    ### ###    ### ###    ###        ### ###   ###    ###    ###
!    ###        ###       ###    ### ###    ### ###    ###       ###   ###  ###    ###    ###
!   ###        ###       ###    ### #########  #########       ########### #########     ###
!  ###        ###       ###    ### ###    ### ###    ###      ###     ### ###           ###
! ###    ### ###       ###    ### ###    ### ###    ###      ###     ### ###           ###
! ########  ########## ########  #########  #########       ###     ### ###       ###########
! The CLUBB API serves as the doorway through which external models can interact with CLUBB.
!               PLEASE REMEMBER, IF ANY CODE IS CHANGED IN THIS DOCUMENT,
!                   THE CHANGES MUST BE PROPOGATED TO ALL HOST MODELS.


!
!
!
!
module clubb_api_module


    USE clubb_precision, ONLY: core_rknd 

    USE constants_clubb, ONLY: rt_tol, thl_tol, w_tol_sqd 
    ! Tolerances


    USE grid_class, ONLY: gr 


    USE parameter_indices, ONLY: nparams 

    USE pdf_parameter_module, ONLY: pdf_parameter 


    USE stats_variables, ONLY: stats_tsamp, stats_tout, l_stats 
    ! These are used in CAM only


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
    USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC read_parameters_api 
    ! To Implement CLUBB:
        ! CLUBB can be set more specifically using these flags:
        ! The parameters of CLUBB can be retrieved and tuned using these indices:


    PUBLIC advance_clubb_core_api, pdf_parameter 
        ! A hydromet array is required, and these variables are required for a hydromet array:

    ! To Implement SILHS:
    ! lh_subcolumn_generator - SILHS API
    ! To use the results, you will need these variables:

    PUBLIC gr, setup_grid_heights_api 
    ! To Interact With CLUBB's Grid:
    ! For Varying Grids

    PUBLIC stats_begin_timestep_api, l_stats, stats_tsamp, stats_tout 
    ! To Obtain More Output from CLUBB for Diagnostics:

    PUBLIC calculate_thlp2_rad_api, update_xp2_mc_api 

    PUBLIC zt2zm_api, zm2zt_api 
    ! To Convert Between Common CLUBB-related quantities:

    ! To Check For and Handle CLUBB's Errors:

    PUBLIC core_rknd, rt_tol, thl_tol, w_tol_sqd 
    ! Constants That May be Helpful:
    ! Tolerances

    ! Attempt to Not Use the Following:
    PUBLIC nparams, setup_parameters_api 

  interface zt2zm_api
    module procedure zt2zm_scalar_api, zt2zm_prof_api
  end interface

  interface zm2zt_api
    module procedure zm2zt_scalar_api, zm2zt_prof_api
  end interface
  PUBLIC kr_pdf_parameter_module_pdf_parameter 
  PUBLIC kv_pdf_parameter_module_pdf_parameter 

contains
  !================================================================================================
  ! advance_clubb_core - Advances the model one timestep.
  !================================================================================================


  subroutine advance_clubb_core_api( &
    l_implemented, dt, fcor, sfc_elevation, hydromet_dim, & ! intent(in)
    thlm_forcing, rtm_forcing, um_forcing, vm_forcing, &    ! intent(in)
    sclrm_forcing, edsclrm_forcing, wprtp_forcing, &        ! intent(in)
    wpthlp_forcing, rtp2_forcing, thlp2_forcing, &          ! intent(in)
    rtpthlp_forcing, wm_zm, wm_zt, &                        ! intent(in)
    wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, &            ! intent(in)
    wpsclrp_sfc, wpedsclrp_sfc, &                           ! intent(in)
    p_in_Pa, rho_zm, rho, exner, &                          ! intent(in)
    rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, &                ! intent(in)
    invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, &      ! intent(in)
    rfrzm, radf, &                                          ! intent(in)
    wphydrometp, wp2hmp, rtphmp, thlphmp, &    ! intent(in)
    host_dx, host_dy, &                                     ! intent(in)
    um, vm, upwp, vpwp, up2, vp2, &                         ! intent(inout)
    thlm, rtm, wprtp, wpthlp, &                             ! intent(inout)
    wp2, wp3, rtp2, rtp3, thlp2, thlp3, rtpthlp, &          ! intent(inout)
    sclrm,   &
    sclrp2, sclrprtp, sclrpthlp, &                          ! intent(inout)
    wpsclrp, edsclrm, err_code, &                           ! intent(inout)
    rcm, wprcp, cloud_frac, ice_supersat_frac, &            ! intent(out)
    rcm_in_layer, cloud_cover, &                            ! intent(out)
    khzm, khzt, &                                           ! intent(out)
    qclvar, thlprcp_out, &                                  ! intent(out)
    pdf_params )                                            ! intent(out)

      USE advance_clubb_core_module, ONLY: advance_clubb_core 

      USE parameters_model, ONLY: sclr_dim, edsclr_dim 

    implicit none
      !!! Input Variables
    logical, intent(in) ::  &
      l_implemented ! Is this part of a larger host model (T/F) ?

    real( kind = core_rknd ), intent(in) ::  &
      dt  ! Current timestep duration    [s]

    real( kind = core_rknd ), intent(in) ::  &
      fcor,  &          ! Coriolis forcing             [s^-1]
      sfc_elevation     ! Elevation of ground level    [m AMSL]

    integer, intent(in) :: &
      hydromet_dim      ! Total number of hydrometeors          [#]
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) ::  &
      thlm_forcing,    & ! theta_l forcing (thermodynamic levels)    [K/s]
      rtm_forcing,     & ! r_t forcing (thermodynamic levels)        [(kg/kg)/s]
      um_forcing,      & ! u wind forcing (thermodynamic levels)     [m/s/s]
      vm_forcing,      & ! v wind forcing (thermodynamic levels)     [m/s/s]
      wprtp_forcing,   & ! <w'r_t'> forcing (momentum levels)    [m*K/s^2]
      wpthlp_forcing,  & ! <w'th_l'> forcing (momentum levels)   [m*(kg/kg)/s^2]
      rtp2_forcing,    & ! <r_t'^2> forcing (momentum levels)    [(kg/kg)^2/s]
      thlp2_forcing,   & ! <th_l'^2> forcing (momentum levels)   [K^2/s]
      rtpthlp_forcing, & ! <r_t'th_l'> forcing (momentum levels) [K*(kg/kg)/s]
      wm_zm,           & ! w mean wind component on momentum levels  [m/s]
      wm_zt,           & ! w mean wind component on thermo. levels   [m/s]
      p_in_Pa,         & ! Air pressure (thermodynamic levels)       [Pa]
      rho_zm,          & ! Air density on momentum levels            [kg/m^3]
      rho,             & ! Air density on thermodynamic levels       [kg/m^3]
      exner,           & ! Exner function (thermodynamic levels)     [-]
      rho_ds_zm,       & ! Dry, static density on momentum levels    [kg/m^3]
      rho_ds_zt,       & ! Dry, static density on thermo. levels     [kg/m^3]
      invrs_rho_ds_zm, & ! Inv. dry, static density @ momentum levs. [m^3/kg]
      invrs_rho_ds_zt, & ! Inv. dry, static density @ thermo. levs.  [m^3/kg]
      thv_ds_zm,       & ! Dry, base-state theta_v on momentum levs. [K]
      thv_ds_zt,       & ! Dry, base-state theta_v on thermo. levs.  [K]
      rfrzm              ! Total ice-phase water mixing ratio        [kg/kg]

    real( kind = core_rknd ), dimension(gr%nz,hydromet_dim), intent(in) :: &
      hydromet           ! Collection of hydrometeors                [units vary]

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      radf          ! Buoyancy production at the CL top due to LW radiative cooling [m^2/s^3]


    real( kind = core_rknd ), dimension(gr%nz, hydromet_dim), intent(in) :: &
      wphydrometp, & ! Covariance of w and a hydrometeor   [(m/s) <hm units>]
      wp2hmp,      & ! Third moment: <w'^2> * <hydro.'>    [(m/s)^2 <hm units>]
      rtphmp,      & ! Covariance of rt and a hydrometeor  [(kg/kg) <hm units>]
      thlphmp        ! Covariance of thl and a hydrometeor [K <hm units>]

    real( kind = core_rknd ), intent(in) ::  &
      wpthlp_sfc,   & ! w' theta_l' at surface   [(m K)/s]
      wprtp_sfc,    & ! w' r_t' at surface       [(kg m)/( kg s)]
      upwp_sfc,     & ! u'w' at surface          [m^2/s^2]
      vpwp_sfc        ! v'w' at surface          [m^2/s^2]
    ! Passive scalar variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz,sclr_dim) :: &
      sclrm_forcing    ! Passive scalar forcing         [{units vary}/s]

    real( kind = core_rknd ), intent(in),  dimension(sclr_dim) ::  &
      wpsclrp_sfc      ! Scalar flux at surface         [{units vary} m/s]
    ! Eddy passive scalar variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz,edsclr_dim) :: &
      edsclrm_forcing  ! Eddy passive scalar forcing    [{units vary}/s]

    real( kind = core_rknd ), intent(in),  dimension(edsclr_dim) ::  &
      wpedsclrp_sfc    ! Eddy-Scalar flux at surface    [{units vary} m/s]
    ! Host model horizontal grid spacing, if part of host model.

    real( kind = core_rknd ), intent(in) :: &
      host_dx,  & ! East-West horizontal grid spacing     [m]
      host_dy     ! North-South horizontal grid spacing   [m]
    !!! Input/Output Variables
    ! These are prognostic or are planned to be in the future


    real( kind = core_rknd ), intent(inout), dimension(gr%nz) ::  &
      um,      & ! u mean wind component (thermodynamic levels)   [m/s]
      upwp,    & ! u'w' (momentum levels)                         [m^2/s^2]
      vm,      & ! v mean wind component (thermodynamic levels)   [m/s]
      vpwp,    & ! v'w' (momentum levels)                         [m^2/s^2]
      up2,     & ! u'^2 (momentum levels)                         [m^2/s^2]
      vp2,     & ! v'^2 (momentum levels)                         [m^2/s^2]
      rtm,     & ! total water mixing ratio, r_t (thermo. levels) [kg/kg]
      wprtp,   & ! w' r_t' (momentum levels)                      [(kg/kg) m/s]
      thlm,    & ! liq. water pot. temp., th_l (thermo. levels)   [K]
      wpthlp,  & ! w' th_l' (momentum levels)                     [(m/s) K]
      rtp2,    & ! r_t'^2 (momentum levels)                       [(kg/kg)^2]
      rtp3,    & ! r_t'^3 (thermodynamic levels)                  [(kg/kg)^3]
      thlp2,   & ! th_l'^2 (momentum levels)                      [K^2]
      thlp3,   & ! th_l'^3 (thermodynamic levels)                 [K^3]
      rtpthlp, & ! r_t' th_l' (momentum levels)                   [(kg/kg) K]
      wp2,     & ! w'^2 (momentum levels)                         [m^2/s^2]
      wp3        ! w'^3 (thermodynamic levels)                    [m^3/s^3]
    ! Passive scalar variables

    real( kind = core_rknd ), intent(inout), dimension(gr%nz,sclr_dim) :: &
      sclrm,     & ! Passive scalar mean (thermo. levels) [units vary]
      wpsclrp,   & ! w'sclr' (momentum levels)            [{units vary} m/s]
      sclrp2,    & ! sclr'^2 (momentum levels)            [{units vary}^2]
      sclrprtp,  & ! sclr'rt' (momentum levels)           [{units vary} (kg/kg)]
      sclrpthlp    ! sclr'thl' (momentum levels)          [{units vary} K]


      real( kind = core_rknd ), intent(inout), dimension(gr%nz,edsclr_dim) :: &
      edsclrm   ! Eddy passive scalar mean (thermo. levels)   [units vary]

    real( kind = core_rknd ), intent(out), dimension(gr%nz) ::  &
      rcm,          & ! cloud water mixing ratio, r_c (thermo. levels)  [kg/kg]
      rcm_in_layer, & ! rcm in cloud layer                              [kg/kg]
      cloud_cover     ! cloud cover                                     [-]

    type(pdf_parameter), dimension(gr%nz), intent(out) :: &
      pdf_params      ! PDF parameters   [units vary]
    ! Variables that need to be output for use in host models

    real( kind = core_rknd ), intent(out), dimension(gr%nz) ::  &
      wprcp,            & ! w'r_c' (momentum levels)                  [(kg/kg) m/s]
      cloud_frac,       & ! cloud fraction (thermodynamic levels)     [-]
      ice_supersat_frac   ! ice cloud fraction (thermodynamic levels) [-]

    real( kind = core_rknd ), intent(out), dimension(gr%nz) :: &
      khzt, &       ! eddy diffusivity on thermo levels
      khzm          ! eddy diffusivity on momentum levels

    real( kind = core_rknd), intent(out), dimension(gr%nz) :: &
      qclvar, &     ! cloud water variance
      thlprcp_out
      !!! Output Variable

      integer, intent(inout) :: err_code ! Diagnostic, for if some calculation goes amiss.

    call advance_clubb_core( &
      l_implemented, dt, fcor, sfc_elevation, hydromet_dim, & ! intent(in)
      thlm_forcing, rtm_forcing, um_forcing, vm_forcing, &    ! intent(in)
      sclrm_forcing, edsclrm_forcing, wprtp_forcing, &        ! intent(in)
      wpthlp_forcing, rtp2_forcing, thlp2_forcing, &          ! intent(in)
      rtpthlp_forcing, wm_zm, wm_zt, &                        ! intent(in)
      wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, &            ! intent(in)
      wpsclrp_sfc, wpedsclrp_sfc, &                           ! intent(in)
      p_in_Pa, rho_zm, rho, exner, &                          ! intent(in)
      rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, &                ! intent(in)
      invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, &      ! intent(in)
      rfrzm, radf, &                                          ! intent(in)
      wphydrometp, wp2hmp, rtphmp, thlphmp, &                 ! intent(in)
      host_dx, host_dy, &                                     ! intent(in)
      um, vm, upwp, vpwp, up2, vp2, &                         ! intent(inout)
      thlm, rtm, wprtp, wpthlp, &                             ! intent(inout)
      wp2, wp3, rtp2, rtp3, thlp2, thlp3, rtpthlp, &          ! intent(inout)
      sclrm,   &
      sclrp2, sclrprtp, sclrpthlp, &                          ! intent(inout)
      wpsclrp, edsclrm, err_code, &                           ! intent(inout)
      rcm, wprcp, cloud_frac, ice_supersat_frac, &            ! intent(out)
      rcm_in_layer, cloud_cover, &                            ! intent(out)
               khzm, khzt, &                                           ! intent(out)
               qclvar, thlprcp_out, &                                               ! intent(out)
      pdf_params )                                            ! intent(out)
  end subroutine advance_clubb_core_api
  !================================================================================================
  ! setup_clubb_core - Sets up the model for execution.
  !================================================================================================


  !================================================================================================
  ! cleanup_clubb_core_api - Frees memory used by the model.
  !================================================================================================


  !================================================================================================
  ! gregorian2julian_day - Computes the number of days since 1 January 4713 BC.
  !================================================================================================


  !================================================================================================
  ! compute_current_date - Computes the current date and the seconds since that date.
  !================================================================================================


  !================================================================================================
  ! leap_year - Determines if the given year is a leap year.
  !================================================================================================


  !================================================================================================
  ! setup_corr_varnce_array - Creates a correlation array with x'^2/xm^2 variables on the diagonal
  !================================================================================================


  !================================================================================================
  ! setup_pdf_indices - Sets up the iiPDF indices.
  !================================================================================================


  !================================================================================================
  ! report_error - Reports the meaning of an error code to the console.
  !================================================================================================


  !================================================================================================
  ! fatal_error - Checks to see if an error code is usually one which causes an exit elsewhere.
  !================================================================================================


  !================================================================================================
  ! set_clubb_debug_level - Controls the importance of error messages sent to the console.
  !================================================================================================


  !================================================================================================
  ! clubb_at_least_debug_level - Checks to see if clubb has been set to a specified debug level.
  !================================================================================================


  !================================================================================================
  ! fill_holes_driver - Fills holes between same-phase hydrometeors(i.e. for frozen hydrometeors).
  !================================================================================================


  !================================================================================================
  ! fill_holes_vertical - clips values of 'field' that are below 'threshold' as much as possible.
  !================================================================================================


  !================================================================================================
  ! vertical_integral - Computes the vertical integral.
  !================================================================================================


  !================================================================================================
  ! setup_grid_heights - Sets the heights and interpolation weights of the column.
  !================================================================================================


  subroutine setup_grid_heights_api( &
    l_implemented, grid_type,  &
    deltaz, zm_init, momentum_heights,  &
    thermodynamic_heights )

      USE grid_class, ONLY: setup_grid_heights 

    implicit none
    ! Input Variables
    ! Flag to see if CLUBB is running on it's own,
    ! or if it's implemented as part of a host model.


    logical, intent(in) :: l_implemented
    ! If CLUBB is running on it's own, this option determines if it is using:
    ! 1) an evenly-spaced grid;
    ! 2) a stretched (unevenly-spaced) grid entered on the thermodynamic grid
    !    levels (with momentum levels set halfway between thermodynamic levels);
    !    or
    ! 3) a stretched (unevenly-spaced) grid entered on the momentum grid levels
    !    (with thermodynamic levels set halfway between momentum levels).

    integer, intent(in) :: grid_type
    ! If the CLUBB model is running by itself, and is using an evenly-spaced
    ! grid (grid_type = 1), it needs the vertical grid spacing and
    ! momentum-level starting altitude as input.

    real( kind = core_rknd ), intent(in) ::  &
      deltaz,   & ! Vertical grid spacing                  [m]
      zm_init     ! Initial grid altitude (momentum level) [m]
    ! If the CLUBB parameterization is implemented in a host model, it needs to
    ! use the host model's momentum level altitudes and thermodynamic level
    ! altitudes.
    ! If the CLUBB model is running by itself, but is using a stretched grid
    ! entered on thermodynamic levels (grid_type = 2), it needs to use the
    ! thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a stretched grid
    ! entered on momentum levels (grid_type = 3), it needs to use the momentum
    ! level altitudes as input.


    real( kind = core_rknd ), intent(in), dimension(gr%nz) ::  &
      momentum_heights,   & ! Momentum level altitudes (input)      [m]
      thermodynamic_heights ! Thermodynamic level altitudes (input) [m]

    call setup_grid_heights( &
      l_implemented, grid_type,  &
      deltaz, zm_init, momentum_heights,  &
      thermodynamic_heights )

  end subroutine setup_grid_heights_api
  !================================================================================================
  ! lin_interpolate_two_points - Computes a linear interpolation of the value of a variable.
  !================================================================================================


  !================================================================================================
  ! lin_interpolate_on_grid - Linear interpolation for 25 June 1996 altocumulus case.
  !================================================================================================


  !================================================================================================
  ! read_parameters - Read a namelist containing the model parameters.
  !================================================================================================


  subroutine read_parameters_api( &
    iunit, filename, params )

      USE parameters_tunable, ONLY: read_parameters 

      USE parameter_indices, ONLY: nparams 

    implicit none
    ! Input variables

    integer, intent(in) :: iunit

    character(len=*), intent(in) :: filename
    ! Output variables

    real( kind = core_rknd ), intent(out), dimension(nparams) :: params

    call read_parameters( &
      iunit, filename, params )

  end subroutine read_parameters_api
  !================================================================================================
  ! setup_parameters - Sets up model parameters.
  !================================================================================================


  subroutine setup_parameters_api( &
    deltaz, params, nzmax, &
    grid_type, momentum_heights, thermodynamic_heights, &
    err_code )

      USE parameters_tunable, ONLY: setup_parameters 


      USE parameter_indices, ONLY: nparams 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in) ::  &
      deltaz  ! Change per height level        [m]

    real( kind = core_rknd ), intent(in), dimension(nparams) :: &
      params  ! Tuneable model parameters      [-]
    ! Grid definition

    integer, intent(in) :: nzmax  ! Vertical grid levels            [#]
    ! If CLUBB is running on its own, this option determines
    ! if it is using:
    ! 1) an evenly-spaced grid,
    ! 2) a stretched (unevenly-spaced) grid entered on the
    !    thermodynamic grid levels (with momentum levels set
    !    halfway between thermodynamic levels), or
    ! 3) a stretched (unevenly-spaced) grid entered on the
    !    momentum grid levels (with thermodynamic levels set
    !    halfway between momentum levels).

    integer, intent(in) :: grid_type
    ! If the CLUBB parameterization is implemented in a host model,
    ! it needs to use the host model's momentum level altitudes
    ! and thermodynamic level altitudes.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on thermodynamic levels (grid_type = 2),
    ! it needs to use the thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on momentum levels (grid_type = 3),
    ! it needs to use the momentum level altitudes as input.

    real( kind = core_rknd ), intent(in), dimension(nzmax) :: &
      momentum_heights,      & ! Momentum level altitudes (input)      [m]
      thermodynamic_heights    ! Thermodynamic level altitudes (input) [m]
    ! Output Variables

    integer, intent(out) ::  &
      err_code ! Error condition

    call setup_parameters( &
      deltaz, params, nzmax, &
      grid_type, momentum_heights, thermodynamic_heights, &
      err_code )

  end subroutine setup_parameters_api
  !================================================================================================
  ! adj_low_res_nu - Adjusts values of background eddy diffusivity based on vertical grid spacing.
  !================================================================================================


  !================================================================================================
  ! pack_pdf_params - Returns a two dimensional real array with all values.
  !================================================================================================


  !================================================================================================
  ! unpack_pdf_params - Returns a pdf_parameter array with nz instances of pdf_parameter.
  !================================================================================================


  !================================================================================================
  ! setup_pdf_parameters
  !================================================================================================


  !================================================================================================
  ! stats_init - Initializes the statistics saving functionality of the CLUBB model.
  !================================================================================================


  !================================================================================================
  ! stats_begin_timestep - Sets flags determining specific timestep info.
  !================================================================================================


  subroutine stats_begin_timestep_api( &
    itime, stats_nsamp, stats_nout )


      USE stats_clubb_utilities, ONLY: stats_begin_timestep 

    implicit none
    ! External

    intrinsic :: mod
    ! Input Variable(s)

    integer, intent(in) ::  &
      itime,        & ! Elapsed model time       [timestep]
      stats_nsamp,  & ! Stats sampling interval  [timestep]
      stats_nout      ! Stats output interval    [timestep]

    call stats_begin_timestep( &
      itime, stats_nsamp, stats_nout )
  end subroutine stats_begin_timestep_api
  !================================================================================================
  ! stats_end_timestep - Calls statistics to be written to the output format.
  !================================================================================================


  !================================================================================================
  ! stats_accumulate_hydromet - Computes stats related the hydrometeors.
  !================================================================================================


  !================================================================================================
  ! stats_finalize - Close NetCDF files and deallocate scratch space and stats file structures.
  !================================================================================================


  !================================================================================================
  ! stats_init_rad_zm - Initializes array indices for rad_zm variables.
  !================================================================================================


  !================================================================================================
  ! stats_init_rad_zt - Initializes array indices for zt.
  !================================================================================================


  !================================================================================================
  ! stats_init_zm - Initializes array indices for zm.
  !================================================================================================


  !================================================================================================
  ! stats_init_zt - Initializes array indices for zt.
  !================================================================================================


  !================================================================================================
  ! stats_init_sfc - Initializes array indices for sfc.
  !================================================================================================


  !================================================================================================
  ! thlm2T_in_K - Calculates absolute temperature from liquid water potential temperature.
  !================================================================================================


  !================================================================================================
  ! T_in_K2thlm - Calculates liquid water potential temperature from absolute temperature
  !================================================================================================


  !================================================================================================
  ! calculate_spurious_source - Checks whether there is conservation within the column.
  !================================================================================================


  !================================================================================================
  ! zm2zt_scalar - Interpolates a variable from zm to zt grid at one height level
  !================================================================================================

  function zm2zt_scalar_api( azm, k )

      USE grid_class, ONLY: zm2zt 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azm    ! Variable on momentum grid levels    [units vary]

    integer, intent(in) :: &
      k      ! Vertical level index
    ! Return Variable

    real( kind = core_rknd ) :: &
      zm2zt_scalar_api   ! Variable when interp. to thermo. levels

    zm2zt_scalar_api = zm2zt( azm, k )

  end function zm2zt_scalar_api
  !================================================================================================
  ! zt2zm_scalar - Interpolates a variable from zt to zm grid at one height level
  !================================================================================================

  function zt2zm_scalar_api( azt, k )

      USE grid_class, ONLY: zt2zm 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]

    integer, intent(in) :: &
      k      ! Vertical level index
    ! Return Variable

    real( kind = core_rknd ) :: &
      zt2zm_scalar_api   ! Variable when interp. to momentum levels

    zt2zm_scalar_api = zt2zm( azt, k )

  end function zt2zm_scalar_api
  !================================================================================================
  ! zt2zm_prof - Interpolates a variable (profile) from zt to zm grid
  !================================================================================================

  function zt2zm_prof_api( azt )

      USE grid_class, ONLY: zt2zm 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      zt2zm_prof_api   ! Variable when interp. to momentum levels

    zt2zm_prof_api = zt2zm( azt )

  end function zt2zm_prof_api
  !================================================================================================
  ! zm2zt_prof - Interpolates a variable (profile) from zm to zt grid
  !================================================================================================

  function zm2zt_prof_api( azm )

      USE grid_class, ONLY: zm2zt 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azm    ! Variable on momentum grid levels    [units vary]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      zm2zt_prof_api   ! Variable when interp. to thermo. levels

    zm2zt_prof_api = zm2zt( azm )

  end function zm2zt_prof_api
  !================================================================================================
  ! calculate_thlp2_rad - Computes the contribution of radiative cooling to thlp2
  !================================================================================================

  pure subroutine calculate_thlp2_rad_api &
                  ( nz, rcm_zm, thlprcp, radht_zm, &      ! Intent(in)
                    thlp2_forcing )                       ! Intent(inout)

      USE clubb_precision, ONLY: core_rknd 

      USE advance_clubb_core_module, ONLY: calculate_thlp2_rad 

  implicit none
  ! Input Variables

  integer, intent(in) :: &
    nz                    ! Number of vertical levels                      [-]

  real( kind = core_rknd ), dimension(nz), intent(in) :: &
    rcm_zm, &             ! Cloud water mixing ratio on momentum grid      [kg/kg]
    thlprcp, &            ! thl'rc'                                        [K kg/kg]
    radht_zm              ! SW + LW heating rate (on momentum grid)        [K/s]
  ! Input/Output Variables

  real( kind = core_rknd ), dimension(nz), intent(inout) :: &
    thlp2_forcing         ! <th_l'^2> forcing (momentum levels)            [K^2/s]
  !----------------------------------------------------------------------

    call calculate_thlp2_rad( nz, rcm_zm, thlprcp, radht_zm, &
                    thlp2_forcing )

    return
  end subroutine calculate_thlp2_rad_api
  !================================================================================================
  ! update_xp2_mc - Calculates the effects of rain evaporation on rtp2 and thlp2
  !================================================================================================

  subroutine update_xp2_mc_api( nz, dt, cloud_frac, rcm, rvm, thlm,        &
                            wm, exner, rrm_evap, pdf_params,        &
                            rtp2_mc, thlp2_mc, wprtp_mc, wpthlp_mc,    &
                            rtpthlp_mc )

      USE advance_xp2_xpyp_module, ONLY: update_xp2_mc 

    implicit none
    !input parameters

    integer, intent(in) :: nz ! Points in the Vertical        [-]

    real( kind = core_rknd ), intent(in) :: dt ! Model timestep        [s]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      cloud_frac, &       !Cloud fraction                        [-]
      rcm, &              !Cloud water mixing ratio              [kg/kg]
      rvm, &              !Vapor water mixing ratio              [kg/kg]
      thlm, &             !Liquid potential temperature          [K]
      wm, &               !Mean vertical velocity                [m/s]
      exner, &            !Exner function                        [-]
      rrm_evap         !Evaporation of rain                   [kg/kg/s]
                          !It is expected that this variable is negative, as
                          !that is the convention in Morrison microphysics

    type(pdf_parameter), dimension(nz), intent(in) :: &
      pdf_params ! PDF parameters
    !input/output variables

    real( kind = core_rknd ), dimension(nz), intent(inout) :: &
      rtp2_mc, &    !Tendency of <rt'^2> due to evaporation   [(kg/kg)^2/s]
      thlp2_mc, &   !Tendency of <thl'^2> due to evaporation  [K^2/s]
      wprtp_mc, &   !Tendency of <w'rt'> due to evaporation   [m*(kg/kg)/s^2]
      wpthlp_mc, &  !Tendency of <w'thl'> due to evaporation  [m*K/s^2] 
      rtpthlp_mc    !Tendency of <rt'thl'> due to evaporation [K*(kg/kg)/s]

    call update_xp2_mc( nz, dt, cloud_frac, rcm, rvm, thlm,        &
                        wm, exner, rrm_evap, pdf_params,        &
                        rtp2_mc, thlp2_mc, wprtp_mc, wpthlp_mc,    &
                        rtpthlp_mc )
    return
  end subroutine update_xp2_mc_api
  !================================================================================================
  ! sat_mixrat_liq - computes the saturation mixing ratio of liquid water
  !================================================================================================


end module clubb_api_module