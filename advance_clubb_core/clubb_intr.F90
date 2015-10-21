
! KGEN-generated Fortran source file
!
! Filename    : clubb_intr.F90
! Generated at: 2015-10-20 14:26:59
! KGEN version: 0.5.3



    MODULE clubb_intr
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
    USE pdf_parameter_module, ONLY : kgen_read_mod3 => kgen_read
    USE pdf_parameter_module, ONLY : kgen_verify_mod3 => kgen_verify
!----------------------------------------------------------------------------------------------------- !
! Module to interface CAM with Cloud Layers Unified by Bi-normals (CLUBB), developed                   !
!    by the University of Wisconsin Milwaukee Group (UWM).                                             !
!                                                                                                      !
! CLUBB replaces the exisiting turbulence, shallow convection, and macrophysics in CAM5                !
!                                                                                                      !
! Lastly, a implicit diffusion solver is called, and tendencies retrieved by                           !
! differencing the diffused and initial states.                                                        !
!                                                                                                      !
! Calling sequence:                                                                                    !
!                                                                                                      !
!---------------------------Code history-------------------------------------------------------------- !
! Authors:  P. Bogenschutz, C. Craig, A. Gettelman                                                     !
!                                                                                                      !
!----------------------------------------------------------------------------------------------------- !
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        IMPLICIT NONE
        PRIVATE
        PUBLIC kgen_read_externs_clubb_intr
        PUBLIC clubb_tend_cam
! ----------------- !
! Public interfaces !
! ----------------- !
! This utilizes CLUBB specific variables in its interface
! Both of these utilize CLUBB specific variables in their interface
! ------------ !
! Private data !
! ------------ !
        INTEGER, parameter :: ref_hydromet_dim = 0
        INTEGER, parameter :: hydromet_dim = 0
! The 2 option specifies stretched thermodynamic levels
! The hydromet array in SAM-CLUBB is currently 0 elements
! Total water in kg/kg
! Flatau polynomial approximation for SVP
! Reference temperature                     [K]
! Time scale for u/v nudging (not used)     [s]
        REAL(KIND=r8), parameter :: ref_host_dx = 100000._r8
        REAL(KIND=r8), parameter :: host_dx = 100000._r8
        REAL(KIND=r8), parameter :: ref_host_dy = 100000._r8
        REAL(KIND=r8), parameter :: host_dy = 100000._r8
! Host model deltax [m]
! Host model deltay [m]
        INTEGER, parameter :: ref_sclr_dim = 0
        INTEGER, parameter :: sclr_dim = 0
! Higher-order scalars, set to zero
! Constant to add to wp3 when moments are advected
! Constant to add to wpthlp when moments are advected
! Constant to add to wprtp when moments are advected
! Constant to add to rtpthlp when moments are advected
! Default CLUBB timestep, unless overwriten by namelist
!  Constant parameters
        LOGICAL, parameter, private :: l_implemented    = .true.
! Use u/v nudging (not used)
! Implemented in a host model (always true)
! Whether the host model applies the surface fluxes
        LOGICAL :: do_expldiff
        INTEGER :: ref_edsclr_dim
        INTEGER :: edsclr_dim ! Number of scalars to transport in CLUBB
!  define physics buffer indicies here
! vertical velocity variances
! third moment of vertical velocity
! turbulent flux of thetal
! turbulent flux of total water
! covariance of thetal and rt
! variance of total water
! variance of thetal
! variance of east-west wind
! variance of north-south wind
! east-west momentum flux
! north-south momentum flux
! mean thetal
! mean total water mixing ratio
! mean of east-west wind
! mean of north-south wind
! Cloud fraction
! Convective cloud fraction
! Stratiform cloud fraction
! Liquid stratiform cloud fraction
! Ice stratiform cloud fraction
! Physical in-cloud LWC
! Physical in-cloud IWC
! deep convection cloud fraction
! shallow convection cloud fraction
! Rel
! CLUBB eddy diffusivity on thermo levels
! CLUBB eddy diffusivity on mom levels
! PBL pbuf
! In cloud mixing ratio for deep convection
! turbulent kinetic energy
! temperature perturbation from PBL
! fice_idx index in physics buffer
! cmeliq_idx index in physics buffer
! relative cloud water variance
! optional accretion enhancement factor for MG
! ice number concentration
! rain evaporation rate
! longwave cooling rate
!  Output arrays for CLUBB statistics
! Default set in phys_control.F90
! Default set in phys_control.F90
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_clubb_intr(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) do_expldiff
            READ(UNIT=kgen_unit) edsclr_dim
        END SUBROUTINE kgen_read_externs_clubb_intr

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

        SUBROUTINE clubb_tend_cam(kgen_unit, total_time)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
!-------------------------------------------------------------------------------
! Description: Provide tendencies of shallow convection, turbulence, and
!              macrophysics from CLUBB to CAM
!
! Author: Cheryl Craig, March 2011
! Modifications: Pete Bogenschutz, March 2011 and onward
! Origin: Based heavily on UWM clubb_init.F90
! References:
!   None
!-------------------------------------------------------------------------------
            USE ppgrid, ONLY: pverp
! Subroutine
            USE advance_clubb_core_module, ONLY: advance_clubb_core
            USE pdf_parameter_module, ONLY: pdf_parameter ! Type
            IMPLICIT NONE
! --------------- !
! Input Auguments !
! --------------- !
            INTEGER, INTENT(IN) :: kgen_unit
            REAL(KIND=kgen_dp), INTENT(INOUT) :: total_time
            REAL(KIND=kgen_dp) :: elapsed_time
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            INTEGER, PARAMETER :: maxiter=10000
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
! Physics state variables                 [vary]
! Host model timestep                     [s]
! Detraining cld H20 from deep convection [kg/ks/s]
! convective mass flux--m sub c           [kg/m2/s]
! std deviation of orography              [m]
! number of mac-mic iterations
! number of mac-mic iterations
! ---------------------- !
! Input-Output Auguments !
! ---------------------- !
! ---------------------- !
! Output Auguments !
! ---------------------- !
! package tendencies
! These two variables are needed for energy check
! Integral of detrained static energy from ice
! Integral of detrained ice for energy check
! --------------- !
! Local Variables !
! --------------- !
! Local copy of state variable
! Local tendency from processes, added up to return as ptend_all
! # of columns, and chunk identifier
            INTEGER :: err_code
            INTEGER :: ref_err_code ! Diagnostic, for if some calculation goes amiss.
            REAL(KIND=r8) :: dtime ! CLUBB time step                              [s]
            REAL(KIND=r8) :: edsclr_in(pverp,edsclr_dim)
            REAL(KIND=r8) :: ref_edsclr_in(pverp,edsclr_dim) ! Scalars to be diffused through CLUBB         [units vary]
            REAL(KIND=r8) :: wp2_in(pverp)
            REAL(KIND=r8) :: ref_wp2_in(pverp) ! vertical velocity variance (CLUBB)           [m^2/s^2]
            REAL(KIND=r8) :: wp3_in(pverp)
            REAL(KIND=r8) :: ref_wp3_in(pverp) ! third moment vertical velocity               [m^3/s^3]
            REAL(KIND=r8) :: wpthlp_in(pverp)
            REAL(KIND=r8) :: ref_wpthlp_in(pverp) ! turbulent flux of thetal                     [K m/s]
            REAL(KIND=r8) :: wprtp_in(pverp)
            REAL(KIND=r8) :: ref_wprtp_in(pverp) ! turbulent flux of total water                [kg/kg m/s]
            REAL(KIND=r8) :: rtpthlp_in(pverp)
            REAL(KIND=r8) :: ref_rtpthlp_in(pverp) ! covariance of thetal and qt                  [kg/kg K]
            REAL(KIND=r8) :: rtp2_in(pverp)
            REAL(KIND=r8) :: ref_rtp2_in(pverp) ! total water variance                         [kg^2/k^2]
            REAL(KIND=r8) :: thlp2_in(pverp)
            REAL(KIND=r8) :: ref_thlp2_in(pverp) ! thetal variance                              [K^2]
            REAL(KIND=r8) :: up2_in(pverp)
            REAL(KIND=r8) :: ref_up2_in(pverp) ! meridional wind variance                     [m^2/s^2]
            REAL(KIND=r8) :: vp2_in(pverp)
            REAL(KIND=r8) :: ref_vp2_in(pverp) ! zonal wind variance                          [m^2/s^2]
            REAL(KIND=r8) :: upwp_in(pverp)
            REAL(KIND=r8) :: ref_upwp_in(pverp) ! meridional wind flux                         [m^2/s^2]
            REAL(KIND=r8) :: vpwp_in(pverp)
            REAL(KIND=r8) :: ref_vpwp_in(pverp) ! zonal wind flux                              [m^2/s^2]
            REAL(KIND=r8) :: thlm_in(pverp)
            REAL(KIND=r8) :: ref_thlm_in(pverp) ! liquid water potential temperature (thetal)  [K]
            REAL(KIND=r8) :: rtm_in(pverp)
            REAL(KIND=r8) :: ref_rtm_in(pverp) ! total water mixing ratio                     [kg/kg]
! water vapor mixing ratio                     [kg/kg]
            REAL(KIND=r8) :: um_in(pverp)
            REAL(KIND=r8) :: ref_um_in(pverp) ! meridional wind                              [m/s]
            REAL(KIND=r8) :: vm_in(pverp)
            REAL(KIND=r8) :: ref_vm_in(pverp) ! zonal wind                                   [m/s]
            REAL(KIND=r8) :: rho_in(pverp) ! mid-point density                            [kg/m^3]
! input for precip evaporation
! total water tendency from rain evap
! thetal tendency from rain evap
            REAL(KIND=r8) :: rcm_out(pverp)
            REAL(KIND=r8) :: ref_rcm_out(pverp) ! CLUBB output of liquid water mixing ratio     [kg/kg]
            REAL(KIND=r8) :: wprcp_out(pverp)
            REAL(KIND=r8) :: ref_wprcp_out(pverp) ! CLUBB output of flux of liquid water          [kg/kg m/s]
            REAL(KIND=r8) :: cloud_frac_out(pverp)
            REAL(KIND=r8) :: ref_cloud_frac_out(pverp) ! CLUBB output of cloud fraction                [fraction]
            REAL(KIND=r8) :: rcm_in_layer_out(pverp)
            REAL(KIND=r8) :: ref_rcm_in_layer_out(pverp) ! CLUBB output of in-cloud liq. wat. mix. ratio [kg/kg]
            REAL(KIND=r8) :: cloud_cover_out(pverp)
            REAL(KIND=r8) :: ref_cloud_cover_out(pverp) ! CLUBB output of in-cloud cloud fraction       [fraction]
            REAL(KIND=r8) :: thlprcp_out(pverp)
            REAL(KIND=r8) :: ref_thlprcp_out(pverp)
            REAL(KIND=r8) :: rho_ds_zm(pverp) ! Dry, static density on momentum levels        [kg/m^3]
            REAL(KIND=r8) :: rho_ds_zt(pverp) ! Dry, static density on thermodynamic levels   [kg/m^3]
            REAL(KIND=r8) :: invrs_rho_ds_zm(pverp) ! Inv. dry, static density on momentum levels   [m^3/kg]
            REAL(KIND=r8) :: invrs_rho_ds_zt(pverp) ! Inv. dry, static density on thermo. levels    [m^3/kg]
            REAL(KIND=r8) :: thv_ds_zm(pverp) ! Dry, base-state theta_v on momentum levels    [K]
            REAL(KIND=r8) :: thv_ds_zt(pverp) ! Dry, base-state theta_v on thermo. levels     [K]
            REAL(KIND=r8) :: rfrzm(pverp)
            REAL(KIND=r8) :: radf(pverp)
            REAL(KIND=r8) :: wprtp_forcing(pverp)
            REAL(KIND=r8) :: wpthlp_forcing(pverp)
            REAL(KIND=r8) :: rtp2_forcing(pverp)
            REAL(KIND=r8) :: thlp2_forcing(pverp)
            REAL(KIND=r8) :: rtpthlp_forcing(pverp)
            REAL(KIND=r8) :: ice_supersat_frac(pverp)
            REAL(KIND=r8) :: ref_ice_supersat_frac(pverp)
! Thermodynamic grid of CLUBB                   [m]
! Momentum grid of CLUBB                        [m]
! output for the thermo CLUBB grid              [m]
! output for momentum CLUBB grid                [m]
            REAL(KIND=r8) :: fcor ! Coriolis forcing                              [s^-1]
            REAL(KIND=r8) :: sfc_elevation ! Elevation of ground                           [m AMSL]
! surface wind                                  [m/s]
! surface stress                                [m/s]
! roughness height                              [m]
            REAL(KIND=r8) :: thlm_forcing(pverp) ! theta_l forcing (thermodynamic levels)        [K/s]
            REAL(KIND=r8) :: rtm_forcing(pverp) ! r_t forcing (thermodynamic levels)            [(kg/kg)/s]
            REAL(KIND=r8) :: um_forcing(pverp) ! u wind forcing (thermodynamic levels)         [m/s/s]
            REAL(KIND=r8) :: vm_forcing(pverp) ! v wind forcing (thermodynamic levels)         [m/s/s]
            REAL(KIND=r8) :: wm_zm(pverp) ! w mean wind component on momentum levels      [m/s]
            REAL(KIND=r8) :: wm_zt(pverp) ! w mean wind component on thermo. levels       [m/s]
            REAL(KIND=r8) :: p_in_pa(pverp) ! Air pressure (thermodynamic levels)           [Pa]
! Air density on thermo levels                  [kt/m^3]
            REAL(KIND=r8) :: rho_zm(pverp) ! Air density on momentum levels                [kg/m^3]
            REAL(KIND=r8) :: exner(pverp) ! Exner function (thermodynamic levels)         [-]
            REAL(KIND=r8) :: wpthlp_sfc ! w' theta_l' at surface                        [(m K)/s]
            REAL(KIND=r8) :: wprtp_sfc ! w' r_t' at surface                            [(kg m)/( kg s)]
            REAL(KIND=r8) :: upwp_sfc ! u'w' at surface                               [m^2/s^2]
            REAL(KIND=r8) :: vpwp_sfc ! v'w' at surface                               [m^2/s^2]
            REAL(KIND=r8) :: sclrm_forcing(pverp,sclr_dim) ! Passive scalar forcing                        [{units vary}/s]
            REAL(KIND=r8) :: wpsclrp_sfc(sclr_dim) ! Scalar flux at surface                        [{units vary} m/s]
            REAL(KIND=r8) :: edsclrm_forcing(pverp,edsclr_dim) ! Eddy passive scalar forcing                   [{units vary}/s]
            REAL(KIND=r8) :: wpedsclrp_sfc(edsclr_dim) ! Eddy-scalar flux at surface                   [{units vary} m/s]
            REAL(KIND=r8) :: sclrm(pverp,sclr_dim)
            REAL(KIND=r8) :: ref_sclrm(pverp,sclr_dim) ! Passive scalar mean (thermo. levels)          [units vary]
            REAL(KIND=r8) :: wpsclrp(pverp,sclr_dim)
            REAL(KIND=r8) :: ref_wpsclrp(pverp,sclr_dim) ! w'sclr' (momentum levels)                     [{units vary} m/s]
            REAL(KIND=r8) :: sclrp2(pverp,sclr_dim)
            REAL(KIND=r8) :: ref_sclrp2(pverp,sclr_dim) ! sclr'^2 (momentum levels)                     [{units vary}^2]
            REAL(KIND=r8) :: sclrprtp(pverp,sclr_dim)
            REAL(KIND=r8) :: ref_sclrprtp(pverp,sclr_dim) ! sclr'rt' (momentum levels)                    [{units vary} (kg/kg)]
            REAL(KIND=r8) :: sclrpthlp(pverp,sclr_dim)
            REAL(KIND=r8) :: ref_sclrpthlp(pverp,sclr_dim) ! sclr'thlp' (momentum levels)                  [{units vary} (K)]
            REAL(KIND=r8) :: hydromet(pverp,hydromet_dim)
            REAL(KIND=r8) :: wphydrometp(pverp,hydromet_dim)
            REAL(KIND=r8) :: wp2hmp(pverp,hydromet_dim)
            REAL(KIND=r8) :: rtphmp_zt(pverp,hydromet_dim)
            REAL(KIND=r8) :: thlphmp_zt (pverp,hydromet_dim)
! Variable for buoyancy flux for pbl            [K m/s]
! transfer coefficient                          [-]
            REAL(KIND=r8) :: khzm_out(pverp)
            REAL(KIND=r8) :: ref_khzm_out(pverp) ! eddy diffusivity on momentum grids            [m^2/s]
            REAL(KIND=r8) :: khzt_out(pverp)
            REAL(KIND=r8) :: ref_khzt_out(pverp) ! eddy diffusivity on thermo grids              [m^2/s]
            REAL(KIND=r8) :: qclvar_out(pverp)
            REAL(KIND=r8) :: ref_qclvar_out(pverp) ! cloud water variance                          [kg^2/kg^2]
! cloud water variance                          [kg^2/kg^2]
! roughness height                              [m]
! thickness of layer                            [m]
! fraction of ice in cloud at CLUBB start       [-]
! minimum total cloud liquid + ice threshold    [kg/kg]
! temporary total cloud liquid + ice            [kg/kg]
! threshold to determin cloud fraction          [kg/kg]
! Variables below are needed to compute energy integrals for conservation
! Exner function consistent with CLUBB          [-]
! Heat flux output variable                     [W/m2]
! Total water flux output variable              [W/m2]
! wp3 output                                    [m^3/s^3]
! rtpthlp ouptut                                [K kg/kg]
! Total water mixing ratio for output           [kg/kg]
! Liquid water potential temperature output     [K]
! Liquid water static energy                    [J/kg]
! Surface stress for PBL height                 [m2/s2]
! Midpoint density in CAM                       [kg/m^3]
! virtual potential temperature                 [K]
! Scalars to be diffused through CLUBB          [units vary]
! CLUBB cloud water mixing ratio                [kg/kg]
! CLUBB cloud fraction                          [fraction]
! CLUBB in-cloud liquid water mixing ratio      [kg/kg]
! CLUBB in-cloud cloud fraction                 [fraction]
! CLUBB liquid water flux                       [m/s kg/kg]
! CLUBB buoyancy flux                           [W/m^2]
! Detraining cld H20 from shallow convection    [kg/kg/day]
! Rv/Rd                                         [-]
! dummy variable                                [units vary]
! Obukov length                                 [m]
! Kinematic Surface heat flux                   [K m/s]
! potential temperature                         [K]
! dummy variable                                [units vary]
! dummy variable                                [units vary]
! Kinematic Surface heat flux                   [K m/s]
! Turbulent mountain stress surface drag        [kg/s/m2]
! U component of turbulent mountain stress      [N/m2]
! V component of turbulent mountain stress      [N/m2]
! Inverse of air density                        [1/kg/m^3]
! Kinematic water vapor flux                    [m/s]
! time keep track of stats          [s]
! These adjustable CLUBB parameters (C1, C2 ...)
! Tolerance on passive scalar       [units vary]
            TYPE(pdf_parameter) :: pdf_params(pverp)
            TYPE(pdf_parameter) :: ref_pdf_params(pverp) ! PDF parameters                    [units vary]
! Strings needed for CLUBB output
! --------------- !
! Pointers        !
! --------------- !
! vertical velocity variance                   [m^2/s^2]
! third moment of vertical velocity            [m^3/s^3]
! turbulent flux of thetal                     [m/s K]
! turbulent flux of moisture                   [m/s kg/kg]
! covariance of thetal and qt                  [kg/kg K]
! moisture variance                            [kg^2/kg^2]
! temperature variance                         [K^2]
! east-west wind variance                      [m^2/s^2]
! north-south wind variance                    [m^2/s^2]
! east-west momentum flux                      [m^2/s^2]
! north-south momentum flux                    [m^2/s^2]
! mean temperature                             [K]
! mean moisture mixing ratio                   [kg/kg]
! mean east-west wind                          [m/s]
! mean north-south wind                        [m/s]
! cloud fraction                               [fraction]
! convective cloud fraction                    [fraction]
! stratiform cloud fraction                    [fraction]
! liquid stratiform cloud fraction             [fraction]
! ice stratiform cloud fraction                [fraction]
! Physical in-stratus LWC                      [kg/kg]
! Physical in-stratus IWC                      [kg/kg]
! deep convection cloud fraction               [fraction]
! shallow convection cloud fraction            [fraction]
! eddy diffusivity on thermo levels            [m^2/s]
! eddy diffusivity on momentum levels          [m^2/s]
! planetary boundary layer height                [m]
! turbulent kinetic energy                     [m^2/s^2]
! deep convection in cloud mixing ratio        [kg/kg]
! relative cloud water variance                [-]
! accretion enhancement factor              [-]
! Shallow convective mass flux--m subc (pcols,pverp) [kg/m2/s/]
!-----------------------------------------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------!
!       MAIN COMPUTATION BEGINS HERE                                                            !
!-----------------------------------------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------!
!  Get indicees for cloud and ice mass and cloud and ice number
!  Initialize physics tendency arrays, copy the state to state1 array to use in this routine
!  Determine number of columns and which chunk computation is to be performed on
!  Determine time step of physics buffer
!  Establish associations between pointers and physics buffer fields
! Intialize the apply_const variable (note special logic is due to eularian backstepping)
!  Determine CLUBB time step and make it sub-step friendly
!  For now we want CLUBB time step to be 5 min since that is
!  what has been scientifically validated.  However, there are certain
!  instances when a 5 min time step will not be possible (based on
!  host model time step or on macro-micro sub-stepping
!  Now check to see if dtime is greater than the host model
!    (or sub stepped) time step.  If it is, then simply
!    set it equal to the host (or sub step) time step.
!    This section is mostly to deal with small host model
!    time steps (or small sub-steps)
!  Now check to see if CLUBB time step divides evenly into
!    the host model time step.  If not, force it to divide evenly.
!    We also want it to be 5 minutes or less.  This section is
!    mainly for host model time steps that are not evenly divisible
!    by 5 minutes
!  If resulting host model time step and CLUBB time step do not divide evenly
!    into each other, have model throw a fit.
!  determine number of timesteps CLUBB core should be advanced,
!  host time step divided by CLUBB time step
!  Initialize forcings for transported scalars to zero
!  Compute exner function consistent with CLUBB's definition, which uses a constant
!  surface pressure.  CAM's exner (in state does not).  Therefore, for consistent
!  treatment with CLUBB code, anytime exner is needed to treat CLUBB variables
!  (such as thlm), use "exner_clubb" other wise use the exner in state
!  At each CLUBB call, initialize mean momentum  and thermo CLUBB state
!  from the CAM state
! Compute integrals of static energy, kinetic energy, water vapor, and liquid water
! for the computation of total energy before CLUBB is called.  This is for an
! effort to conserve energy since liquid water potential temperature (which CLUBB
! conserves) and static energy (which CAM conserves) are not exactly equal.
!  Compute virtual potential temperature, which is needed for CLUBB
! ------------------------------------------------- !
! Begin module to compute turbulent mountain stress !
! ------------------------------------------------- !
! ------------------------------------------------- !
! End module to compute turbulent mountain stress   !
! ------------------------------------------------- !
!  Loop over all columns in lchnk to advance CLUBB core
                    tolerance = 1.E-14
                    CALL kgen_init_check(check_status, tolerance)
                    READ(UNIT=kgen_unit) dtime
                    READ(UNIT=kgen_unit) fcor
                    READ(UNIT=kgen_unit) sfc_elevation
                    READ(UNIT=kgen_unit) thlm_forcing
                    READ(UNIT=kgen_unit) rtm_forcing
                    READ(UNIT=kgen_unit) um_forcing
                    READ(UNIT=kgen_unit) vm_forcing
                    READ(UNIT=kgen_unit) sclrm_forcing
                    READ(UNIT=kgen_unit) edsclrm_forcing
                    READ(UNIT=kgen_unit) wprtp_forcing
                    READ(UNIT=kgen_unit) wpthlp_forcing
                    READ(UNIT=kgen_unit) rtp2_forcing
                    READ(UNIT=kgen_unit) thlp2_forcing
                    READ(UNIT=kgen_unit) rtpthlp_forcing
                    READ(UNIT=kgen_unit) wm_zm
                    READ(UNIT=kgen_unit) wm_zt
                    READ(UNIT=kgen_unit) wpthlp_sfc
                    READ(UNIT=kgen_unit) wprtp_sfc
                    READ(UNIT=kgen_unit) upwp_sfc
                    READ(UNIT=kgen_unit) vpwp_sfc
                    READ(UNIT=kgen_unit) wpsclrp_sfc
                    READ(UNIT=kgen_unit) wpedsclrp_sfc
                    READ(UNIT=kgen_unit) p_in_pa
                    READ(UNIT=kgen_unit) rho_zm
                    READ(UNIT=kgen_unit) rho_in
                    READ(UNIT=kgen_unit) exner
                    READ(UNIT=kgen_unit) rho_ds_zm
                    READ(UNIT=kgen_unit) rho_ds_zt
                    READ(UNIT=kgen_unit) invrs_rho_ds_zm
                    READ(UNIT=kgen_unit) invrs_rho_ds_zt
                    READ(UNIT=kgen_unit) thv_ds_zm
                    READ(UNIT=kgen_unit) thv_ds_zt
                    READ(UNIT=kgen_unit) hydromet
                    READ(UNIT=kgen_unit) rfrzm
                    READ(UNIT=kgen_unit) radf
                    READ(UNIT=kgen_unit) wphydrometp
                    READ(UNIT=kgen_unit) wp2hmp
                    READ(UNIT=kgen_unit) rtphmp_zt
                    READ(UNIT=kgen_unit) thlphmp_zt
                    READ(UNIT=kgen_unit) um_in
                    READ(UNIT=kgen_unit) vm_in
                    READ(UNIT=kgen_unit) upwp_in
                    READ(UNIT=kgen_unit) vpwp_in
                    READ(UNIT=kgen_unit) up2_in
                    READ(UNIT=kgen_unit) vp2_in
                    READ(UNIT=kgen_unit) thlm_in
                    READ(UNIT=kgen_unit) rtm_in
                    READ(UNIT=kgen_unit) wprtp_in
                    READ(UNIT=kgen_unit) wpthlp_in
                    READ(UNIT=kgen_unit) wp2_in
                    READ(UNIT=kgen_unit) wp3_in
                    READ(UNIT=kgen_unit) rtp2_in
                    READ(UNIT=kgen_unit) thlp2_in
                    READ(UNIT=kgen_unit) rtpthlp_in
                    READ(UNIT=kgen_unit) sclrm
                    READ(UNIT=kgen_unit) sclrp2
                    READ(UNIT=kgen_unit) sclrprtp
                    READ(UNIT=kgen_unit) sclrpthlp
                    READ(UNIT=kgen_unit) wpsclrp
                    READ(UNIT=kgen_unit) edsclr_in
                    READ(UNIT=kgen_unit) err_code
                    READ(UNIT=kgen_unit) rcm_out
                    READ(UNIT=kgen_unit) wprcp_out
                    READ(UNIT=kgen_unit) cloud_frac_out
                    READ(UNIT=kgen_unit) ice_supersat_frac
                    READ(UNIT=kgen_unit) rcm_in_layer_out
                    READ(UNIT=kgen_unit) cloud_cover_out
                    READ(UNIT=kgen_unit) khzm_out
                    READ(UNIT=kgen_unit) khzt_out
                    READ(UNIT=kgen_unit) qclvar_out
                    READ(UNIT=kgen_unit) thlprcp_out
                    CALL kgen_read_pdf_parameter_dim1(pdf_params, kgen_unit)

                    READ(UNIT=kgen_unit) ref_um_in
                    READ(UNIT=kgen_unit) ref_vm_in
                    READ(UNIT=kgen_unit) ref_upwp_in
                    READ(UNIT=kgen_unit) ref_vpwp_in
                    READ(UNIT=kgen_unit) ref_up2_in
                    READ(UNIT=kgen_unit) ref_vp2_in
                    READ(UNIT=kgen_unit) ref_thlm_in
                    READ(UNIT=kgen_unit) ref_rtm_in
                    READ(UNIT=kgen_unit) ref_wprtp_in
                    READ(UNIT=kgen_unit) ref_wpthlp_in
                    READ(UNIT=kgen_unit) ref_wp2_in
                    READ(UNIT=kgen_unit) ref_wp3_in
                    READ(UNIT=kgen_unit) ref_rtp2_in
                    READ(UNIT=kgen_unit) ref_thlp2_in
                    READ(UNIT=kgen_unit) ref_rtpthlp_in
                    READ(UNIT=kgen_unit) ref_sclrm
                    READ(UNIT=kgen_unit) ref_sclrp2
                    READ(UNIT=kgen_unit) ref_sclrprtp
                    READ(UNIT=kgen_unit) ref_sclrpthlp
                    READ(UNIT=kgen_unit) ref_wpsclrp
                    READ(UNIT=kgen_unit) ref_edsclr_in
                    READ(UNIT=kgen_unit) ref_err_code
                    READ(UNIT=kgen_unit) ref_rcm_out
                    READ(UNIT=kgen_unit) ref_wprcp_out
                    READ(UNIT=kgen_unit) ref_cloud_frac_out
                    READ(UNIT=kgen_unit) ref_ice_supersat_frac
                    READ(UNIT=kgen_unit) ref_rcm_in_layer_out
                    READ(UNIT=kgen_unit) ref_cloud_cover_out
                    READ(UNIT=kgen_unit) ref_khzm_out
                    READ(UNIT=kgen_unit) ref_khzt_out
                    READ(UNIT=kgen_unit) ref_qclvar_out
                    READ(UNIT=kgen_unit) ref_thlprcp_out
                    CALL kgen_read_pdf_parameter_dim1(ref_pdf_params, kgen_unit)

                    !Perturbation 
                    !Add calls to kgen_perturb to perturbation test similar to below line.
                    !CALL kgen_perturb(variable_name, perturbation_value)
                    !EXAMPLE: CALL kgen_perturb(var, 1.0E-15_8)

                    ! call to kernel
                    CALL advance_clubb_core(l_implemented, dtime, fcor, sfc_elevation, hydromet_dim, thlm_forcing, rtm_forcing, um_forcing, vm_forcing, sclrm_forcing, edsclrm_forcing, wprtp_forcing, wpthlp_forcing, rtp2_forcing, thlp2_forcing, rtpthlp_forcing, wm_zm, wm_zt, wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, wpsclrp_sfc, wpedsclrp_sfc, p_in_pa, rho_zm, rho_in, exner, rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, rfrzm, radf, do_expldiff, wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, host_dx, host_dy, um_in, vm_in, upwp_in, vpwp_in, up2_in, vp2_in, thlm_in, rtm_in, wprtp_in, wpthlp_in, wp2_in, wp3_in, rtp2_in, thlp2_in, rtpthlp_in, sclrm, sclrp2, sclrprtp, sclrpthlp, wpsclrp, edsclr_in, err_code, rcm_out, wprcp_out, cloud_frac_out, ice_supersat_frac, rcm_in_layer_out, cloud_cover_out, khzm_out, khzt_out, qclvar_out, thlprcp_out, pdf_params)
                    ! kernel verification for output variables
                    CALL kgen_verify_real_r8_dim1( "um_in", check_status, um_in, ref_um_in)
                    CALL kgen_verify_real_r8_dim1( "vm_in", check_status, vm_in, ref_vm_in)
                    CALL kgen_verify_real_r8_dim1( "upwp_in", check_status, upwp_in, ref_upwp_in)
                    CALL kgen_verify_real_r8_dim1( "vpwp_in", check_status, vpwp_in, ref_vpwp_in)
                    CALL kgen_verify_real_r8_dim1( "up2_in", check_status, up2_in, ref_up2_in)
                    CALL kgen_verify_real_r8_dim1( "vp2_in", check_status, vp2_in, ref_vp2_in)
                    CALL kgen_verify_real_r8_dim1( "thlm_in", check_status, thlm_in, ref_thlm_in)
                    CALL kgen_verify_real_r8_dim1( "rtm_in", check_status, rtm_in, ref_rtm_in)
                    CALL kgen_verify_real_r8_dim1( "wprtp_in", check_status, wprtp_in, ref_wprtp_in)
                    CALL kgen_verify_real_r8_dim1( "wpthlp_in", check_status, wpthlp_in, ref_wpthlp_in)
                    CALL kgen_verify_real_r8_dim1( "wp2_in", check_status, wp2_in, ref_wp2_in)
                    CALL kgen_verify_real_r8_dim1( "wp3_in", check_status, wp3_in, ref_wp3_in)
                    CALL kgen_verify_real_r8_dim1( "rtp2_in", check_status, rtp2_in, ref_rtp2_in)
                    CALL kgen_verify_real_r8_dim1( "thlp2_in", check_status, thlp2_in, ref_thlp2_in)
                    CALL kgen_verify_real_r8_dim1( "rtpthlp_in", check_status, rtpthlp_in, ref_rtpthlp_in)
                    CALL kgen_verify_real_r8_dim2( "sclrm", check_status, sclrm, ref_sclrm)
                    CALL kgen_verify_real_r8_dim2( "sclrp2", check_status, sclrp2, ref_sclrp2)
                    CALL kgen_verify_real_r8_dim2( "sclrprtp", check_status, sclrprtp, ref_sclrprtp)
                    CALL kgen_verify_real_r8_dim2( "sclrpthlp", check_status, sclrpthlp, ref_sclrpthlp)
                    CALL kgen_verify_real_r8_dim2( "wpsclrp", check_status, wpsclrp, ref_wpsclrp)
                    CALL kgen_verify_real_r8_dim2( "edsclr_in", check_status, edsclr_in, ref_edsclr_in)
                    CALL kgen_verify_integer( "err_code", check_status, err_code, ref_err_code)
                    CALL kgen_verify_real_r8_dim1( "rcm_out", check_status, rcm_out, ref_rcm_out)
                    CALL kgen_verify_real_r8_dim1( "wprcp_out", check_status, wprcp_out, ref_wprcp_out)
                    CALL kgen_verify_real_r8_dim1( "cloud_frac_out", check_status, cloud_frac_out, ref_cloud_frac_out)
                    CALL kgen_verify_real_r8_dim1( "ice_supersat_frac", check_status, ice_supersat_frac, ref_ice_supersat_frac)
                    CALL kgen_verify_real_r8_dim1( "rcm_in_layer_out", check_status, rcm_in_layer_out, ref_rcm_in_layer_out)
                    CALL kgen_verify_real_r8_dim1( "cloud_cover_out", check_status, cloud_cover_out, ref_cloud_cover_out)
                    CALL kgen_verify_real_r8_dim1( "khzm_out", check_status, khzm_out, ref_khzm_out)
                    CALL kgen_verify_real_r8_dim1( "khzt_out", check_status, khzt_out, ref_khzt_out)
                    CALL kgen_verify_real_r8_dim1( "qclvar_out", check_status, qclvar_out, ref_qclvar_out)
                    CALL kgen_verify_real_r8_dim1( "thlprcp_out", check_status, thlprcp_out, ref_thlprcp_out)
                    CALL kgen_verify_pdf_parameter_dim1( "pdf_params", check_status, pdf_params, ref_pdf_params)
                    CALL kgen_print_check("advance_clubb_core", check_status)
                    CALL system_clock(start_clock, rate_clock)
                    DO kgen_intvar=1,maxiter
                    CALL advance_clubb_core(l_implemented, dtime, fcor, sfc_elevation, hydromet_dim, thlm_forcing, rtm_forcing, um_forcing, vm_forcing, sclrm_forcing, edsclrm_forcing, wprtp_forcing, wpthlp_forcing, rtp2_forcing, thlp2_forcing, rtpthlp_forcing, wm_zm, wm_zt, wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, wpsclrp_sfc, wpedsclrp_sfc, p_in_pa, rho_zm, rho_in, exner, rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, rfrzm, radf, do_expldiff, wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, host_dx, host_dy, um_in, vm_in, upwp_in, vpwp_in, up2_in, vp2_in, thlm_in, rtm_in, wprtp_in, wpthlp_in, wp2_in, wp3_in, rtp2_in, thlp2_in, rtpthlp_in, sclrm, sclrp2, sclrprtp, sclrpthlp, wpsclrp, edsclr_in, err_code, rcm_out, wprcp_out, cloud_frac_out, ice_supersat_frac, rcm_in_layer_out, cloud_cover_out, khzm_out, khzt_out, qclvar_out, thlprcp_out, pdf_params)
                    END DO
                    CALL system_clock(stop_clock, rate_clock)
                    elapsed_time = 1.0e6*(stop_clock - start_clock)/REAL(rate_clock*maxiter)
                    total_time = total_time + elapsed_time
                    WRITE(*,*)
                    PRINT *, "advance_clubb_core : Time per call (usec): ", elapsed_time
! end column loop
! Add constant to ghost point so that output is not corrupted
! ------------------------------------------------- !
! End column computation of CLUBB, begin to apply   !
! and compute output, etc                           !
! ------------------------------------------------- !
!  Output CLUBB tendencies
!  Update physics tendencies
! ------------------------------------------------------------ !
! ------------------------------------------------------------ !
! ------------------------------------------------------------ !
! The rest of the code deals with diagnosing variables         !
! for microphysics/radiation computation and macrophysics      !
! ------------------------------------------------------------ !
! ------------------------------------------------------------ !
! ------------------------------------------------------------ !
! --------------------------------------------------------------------------------- !
!  COMPUTE THE ICE CLOUD DETRAINMENT                                                !
!  Detrainment of convective condensate into the environment or stratiform cloud    !
! --------------------------------------------------------------------------------- !
!  Initialize the shallow convective detrainment rate, will always be zero
! divide by density of water
! ------------------------------------------------- !
! Diagnose relative cloud water variance            !
! ------------------------------------------------- !
! default
! ------------------------------------------------- !
! Optional Accretion enhancement factor             !
! ------------------------------------------------- !
! ------------------------------------------------- !
! Diagnose some output variables                    !
! ------------------------------------------------- !
!  density
! --------------------------------------------------------------------------------- !
!  Diagnose some quantities that are computed in macrop_tend here.                  !
!  These are inputs required for the microphysics calculation.                      !
!                                                                                   !
!  FIRST PART COMPUTES THE STRATIFORM CLOUD FRACTION FROM CLUBB CLOUD FRACTION      !
! --------------------------------------------------------------------------------- !
!  initialize variables
! --------------------------------------------------------------------------------- !
!  THIS PART COMPUTES CONVECTIVE AND DEEP CONVECTIVE CLOUD FRACTION                 !
! --------------------------------------------------------------------------------- !
! --------------------------------------------------------------------------------- !
!  COMPUTE THE ICE CLOUD FRACTION PORTION                                           !
!  use the aist_vector function to compute the ice cloud fraction                   !
! --------------------------------------------------------------------------------- !
! --------------------------------------------------------------------------------- !
!  THIS PART COMPUTES THE LIQUID STRATUS FRACTION                                   !
!                                                                                   !
!  For now leave the computation of ice stratus fraction from macrop_driver intact  !
!  because CLUBB does nothing with ice.  Here I simply overwrite the liquid stratus !
!  fraction that was coded in macrop_driver                                         !
! --------------------------------------------------------------------------------- !
!  Recompute net stratus fraction using maximum over-lapping assumption, as done
!  in macrophysics code, using alst computed above and aist read in from physics buffer
!  Probably need to add deepcu cloud fraction to the cloud fraction array, else would just
!  be outputting the shallow convective cloud fraction
! --------------------------------------------------------------------------------- !
!  DIAGNOSE THE PBL DEPTH                                                           !
!  this is needed for aerosol code                                                  !
! --------------------------------------------------------------------------------- !
! diagnose surface friction and obukhov length (inputs to diagnose PBL depth)
!  Compute PBL depth according to Holtslag-Boville Scheme
!  Output the PBL depth
! Assign the first pver levels of cloud_frac back to cld
! --------------------------------------------------------------------------------- !
!  END CLOUD FRACTION DIAGNOSIS, begin to store variables back into buffer          !
! --------------------------------------------------------------------------------- !
!  Output calls of variables goes here
!  Output CLUBB history here
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_r8_dim2(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
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
            END SUBROUTINE kgen_read_real_r8_dim2

            SUBROUTINE kgen_read_real_r8_dim1(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
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
            END SUBROUTINE kgen_read_real_r8_dim1

            SUBROUTINE kgen_read_pdf_parameter_dim1(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                TYPE(pdf_parameter), INTENT(OUT), DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1
                INTEGER, DIMENSION(2,1) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    DO idx1=kgen_bound(1,1), kgen_bound(2, 1)
                    IF ( PRESENT(printvar) ) THEN
                            CALL kgen_read_mod3(var(idx1), kgen_unit, printvar=printvar)
                    ELSE
                            CALL kgen_read_mod3(var(idx1), kgen_unit)
                    END IF
                    END DO
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_pdf_parameter_dim1


        ! verify subroutines
            SUBROUTINE kgen_verify_real_r8_dim1( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:) :: temp, temp2
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
            END SUBROUTINE kgen_verify_real_r8_dim1

            SUBROUTINE kgen_verify_real_r8_dim2( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:) :: temp, temp2
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
            END SUBROUTINE kgen_verify_real_r8_dim2

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

            RECURSIVE SUBROUTINE kgen_verify_pdf_parameter_dim1( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                type(check_t) :: dtype_check_status
                TYPE(pdf_parameter), intent(in), DIMENSION(:) :: var, ref_var
                integer :: idx1
                check_status%numTotal = check_status%numTotal + 1
                CALL kgen_init_check(dtype_check_status)
                DO idx1=LBOUND(var,1), UBOUND(var,1)
                    CALL kgen_verify_mod3(varname, dtype_check_status, var(idx1), ref_var(idx1))
                END DO
                IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                    check_status%numFatal = check_status%numFatal + 1
                ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                    check_status%numWarning = check_status%numWarning + 1
                END IF
            END SUBROUTINE kgen_verify_pdf_parameter_dim1

        END SUBROUTINE clubb_tend_cam
! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! ----------------------------------------------------------------------
!
! DISCLAIMER : this code appears to be correct but has not been
!              very thouroughly tested. If you do notice any
!              anomalous behaviour then please contact Andy and/or
!              Bjorn
!
! Function diag_ustar:  returns value of ustar using the below
! similarity functions and a specified buoyancy flux (bflx) given in
! kinematic units
!
! phi_m (zeta > 0) =  (1 + am * zeta)
! phi_m (zeta < 0) =  (1 - bm * zeta)^(-1/4)
!
! where zeta = z/lmo and lmo = (theta_rev/g*vonk) * (ustar^2/tstar)
!
! Ref: Businger, 1973, Turbulent Transfer in the Atmospheric Surface
! Layer, in Workshop on Micormeteorology, pages 67-100.
!
! Code writen March, 1999 by Bjorn Stevens
!

! =============================================================================== !
!                                                                                 !
! =============================================================================== !

! =============================================================================== !
!                                                                                 !
! =============================================================================== !
!-----------------------------------------------------------------------

! =============================================================================== !
!                                                                                 !
! =============================================================================== !
!-----------------------------------------------------------------------

! =============================================================================== !
!                                                                                 !
! =============================================================================== !
!-----------------------------------------------------------------------

    END MODULE clubb_intr
