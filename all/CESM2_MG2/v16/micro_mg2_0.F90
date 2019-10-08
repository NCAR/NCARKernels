!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  
#define NEC_BEGIN(x) !call ftrace_region_begin(x)
#define NEC_END(x)   !call ftrace_region_end(x)
!!#define USE_OLD_TENDENCIES 1
!!#define USE_NEW_TENDENCIES 1


module micro_mg2_0
!---------------------------------------------------------------------------------
! Purpose:
!   MG microphysics version 2.0 - Update of MG microphysics with
!                                 prognostic precipitation.
! Author: Andrew Gettelman, Hugh Morrison, Sean Santos
! Contributions from: Peter Caldwell, Xiaohong Liu and Steve Ghan
! Version 2 history: Sep 2011: Development begun.
!                    Feb 2013: Added of prognostic precipitation.
!                    Aug 2015: Published and released version
! invoked in CAM by specifying -microphys=mg2.0
! References: 
!           Gettelman, A. and H. Morrison, Advanced Two-Moment Microphysics for Global Models. 
!           Part I: Off line tests and comparisons with other schemes. 
!           J. Climate, 28, 1268-1287. doi: 10.1175/JCLI-D-14-00102.1, 2015. 
!           Gettelman, A., H. Morrison, S. Santos, P. Bogenschutz and P. H. Caldwell 
!           Advanced Two-Moment Microphysics for Global Models. 
!           Part II: Global model solutions and Aerosol-Cloud Interactions. 
!           J. Climate, 28, 1288-1307. doi:10.1175/JCLI-D-14-00103.1 , 2015. 
! for questions contact Hugh Morrison, Andrew Gettelman
! e-mail: morrison@ucar.edu, andrew@ucar.edu
!---------------------------------------------------------------------------------
! NOTE: Modified to allow other microphysics packages (e.g. CARMA) to do ice
! microphysics in cooperation with the MG liquid microphysics. This is
! controlled by the do_cldice variable.
! If do_cldice is false, then MG microphysics should not update CLDICE or
! NUMICE; it is assumed that the other microphysics scheme will have updated
! CLDICE and NUMICE. The other microphysics should handle the following
! processes that would have been done by MG:
!   - Detrainment (liquid and ice)
!   - Homogeneous ice nucleation
!   - Heterogeneous ice nucleation
!   - Bergeron process
!   - Melting of ice
!   - Freezing of cloud drops
!   - Autoconversion (ice -> snow)
!   - Growth/Sublimation of ice
!   - Sedimentation of ice
! This option has not been updated since the introduction of prognostic
! precipitation, and probably should be adjusted to cover snow as well.
!---------------------------------------------------------------------------------
! Based on micro_mg (restructuring of former cldwat2m_micro)
! Author: Andrew Gettelman, Hugh Morrison.
! Contributions from: Xiaohong Liu and Steve Ghan
! December 2005-May 2010
! Description in: Morrison and Gettelman, 2008. J. Climate (MG2008)
!                 Gettelman et al., 2010 J. Geophys. Res. - Atmospheres (G2010)
! for questions contact Hugh Morrison, Andrew Gettelman
! e-mail: morrison@ucar.edu, andrew@ucar.edu
!---------------------------------------------------------------------------------
! Code comments added by HM, 093011
! General code structure:
! Code is divided into two main subroutines:
!   subroutine micro_mg_init --> initializes microphysics routine, should be called
!                                  once at start of simulation
!   subroutine micro_mg_tend --> main microphysics routine to be called each time step
!                                this also calls several smaller subroutines to calculate
!                                microphysical processes and other utilities
! List of external functions:
!   qsat_water --> for calculating saturation vapor pressure with respect to liquid water
!   qsat_ice --> for calculating saturation vapor pressure with respect to ice
!   gamma   --> standard mathematical gamma function
! .........................................................................
! List of inputs through use statement in fortran90:
! Variable Name                      Description                Units
! .........................................................................
! gravit          acceleration due to gravity                    m s-2
! rair            dry air gas constant for air                  J kg-1 K-1
! tmelt           temperature of melting point for water          K
! cpair           specific heat at constant pressure for dry air J kg-1 K-1
! rh2o            gas constant for water vapor                  J kg-1 K-1
! latvap          latent heat of vaporization                   J kg-1
! latice          latent heat of fusion                         J kg-1
! qsat_water      external function for calculating liquid water
!                 saturation vapor pressure/humidity              -
! qsat_ice        external function for calculating ice
!                 saturation vapor pressure/humidity              pa
! rhmini          relative humidity threshold parameter for
!                 nucleating ice                                  -
! .........................................................................
! NOTE: List of all inputs/outputs passed through the call/subroutine statement
!       for micro_mg_tend is given below at the start of subroutine micro_mg_tend.
!---------------------------------------------------------------------------------
! Procedures required:
! 1) An implementation of the gamma function (if not intrinsic).
! 2) saturation vapor pressure and specific humidity over water
! 3) svp over ice
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!

#if defined(__NEC__)
#else
    USE shr_spfn_mod, ONLY: gamma => shr_spfn_gamma 
#endif
    USE shr_kind_mod, ONLY: rkind_comp, rkind_io

    USE wv_sat_methods, ONLY: qsat_water_vector => wv_sat_qsat_water_vector,  &
                              qsat_water_scalar => wv_sat_qsat_water_scalar
    USE wv_sat_methods, ONLY: qsat_ice_vector => wv_sat_qsat_ice_vector, & 
                              qsat_ice_scalar => wv_sat_qsat_ice_scalar
! Parameters from the utilities module.

    USE micro_mg_utils, ONLY:  pi, omsm, qsmall, mincld, rhosn, rhoi, rhow, rhows, bc, ai, bi, aj, bj, ar, br, as, bs, mi0, &
    &rising_factorial 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

    PUBLIC micro_mg_tend 
! Switches for specification rather than prediction of droplet and crystal number
! note: number will be adjusted as needed to keep mean size within bounds,
! even when specified droplet or ice number is used
! If constant cloud ice number is set (nicons = .true.),
! then all microphysical processes except mass transfer due to ice nucleation
! (mnuccd) are based on the fixed cloud ice number. Calculation of
! mnuccd follows from the prognosed ice crystal number ni.

!

logical :: nccons  ! nccons = .true. to specify constant cloud droplet number
logical :: nicons  ! nicons = .true. to specify constant cloud ice number
! specified ice and droplet number concentrations
! note: these are local in-cloud values, not grid-mean

real(rkind_comp) :: ncnst  ! droplet num concentration when nccons=.true. (m-3)
real(rkind_comp) :: ninst  ! ice num concentration when nicons=.true. (m-3)
!=========================================================
! Private module parameters
!=========================================================
!Range of cloudsat reflectivities (dBz) for analytic simulator


real(rkind_comp), parameter :: csmin = -30._rkind_comp
real(rkind_comp), parameter :: csmax = 26._rkind_comp
real(rkind_comp), parameter :: mindbz = -99._rkind_comp
real(rkind_comp), parameter :: minrefl = 1.26e-10_rkind_comp    ! minrefl = 10._rkind_comp**(mindbz/10._rkind_comp)
! autoconversion size threshold for cloud ice to snow (m)

integer, parameter ::   MG_PRECIP_FRAC_INCLOUD = 101
integer, parameter ::   MG_PRECIP_FRAC_OVERLAP = 102

real(rkind_comp) :: dcs
! minimum mass of new crystal due to freezing of cloud droplets done
! externally (kg)

real(rkind_comp), parameter :: mi0l_min = 4._rkind_comp/3._rkind_comp*pi*rhow*(4.e-6_rkind_comp)**3
! Ice number sublimation parameter. Assume some decrease in ice number with sublimation if non-zero. Else, no decrease in number with sublimation. 

  real(rkind_comp), parameter :: sublim_factor =0.0_rkind_comp      !number sublimation factor.  
!=========================================================
! Constants set in initialization
!=========================================================
! Set using arguments to micro_mg_init


real(rkind_comp) :: g           ! gravity
real(rkind_comp) :: r           ! dry air gas constant
real(rkind_comp) :: rv          ! water vapor gas constant
real(rkind_comp) :: cpp         ! specific heat of dry air
real(rkind_comp) :: tmelt       ! freezing point of water (K)
! latent heats of:

real(rkind_comp) :: xxlv        ! vaporization
real(rkind_comp) :: xlf         ! freezing
real(rkind_comp) :: xxls        ! sublimation

! flags

logical :: microp_uniform
logical :: do_cldice
logical :: use_hetfrz_classnuc

real(rkind_comp) :: rhosu       ! typical 850mn air density

real(rkind_comp) :: icenuct     ! ice nucleation temperature: currently -5 degrees C

real(rkind_comp) :: snowmelt    ! what temp to melt all snow: currently 2 degrees C
real(rkind_comp) :: rainfrze    ! what temp to freeze all rain: currently -5 degrees C
! additional constants to help speed up code

real(rkind_comp) :: gamma_br_plus1
real(rkind_comp) :: gamma_br_plus4
real(rkind_comp) :: gamma_bs_plus1
real(rkind_comp) :: gamma_bs_plus4
real(rkind_comp) :: gamma_bi_plus1
real(rkind_comp) :: gamma_bi_plus4
real(rkind_comp) :: gamma_bj_plus1
real(rkind_comp) :: gamma_bj_plus4
real(rkind_comp) :: xxlv_squared
real(rkind_comp) :: xxls_squared

character(len=16)  :: micro_mg_precip_frac_method  ! type of precipitation fraction method
character(len=16)  :: trimed_micro_mg_precip_frac_method  ! YSK
real(rkind_comp)           :: micro_mg_berg_eff_factor     ! berg efficiency factor

logical  :: allow_sed_supersat ! Allow supersaturated conditions after sedimentation loop
logical  :: do_sb_physics ! do SB 2001 autoconversion or accretion physics
!===============================================================================
PUBLIC kr_externs_in_micro_mg2_0 
PUBLIC kr_externs_out_micro_mg2_0 


contains
!===============================================================================


!===============================================================================
!microphysics routine for each timestep goes here...


subroutine micro_mg_tend ( &
     mgncol,             nlev,               deltatin,           &
     t,                            q,                            &
     qcn,                          qin,                          &
     ncn,                          nin,                          &
     qrn,                          qsn,                          &
     nrn,                          nsn,                          &
     relvar,                       accre_enhan,                  &
     p,                            pdel,                         &
     cldn,    liqcldf,        icecldf,       qsatfac,            &
     qcsinksum_rate1ord,                                         &
     naai,                         npccn,                        &
     rndst,                        nacon,                        &
     tlat,                         qvlat,                        &
     qctend,                       qitend,                       &
     nctend,                       nitend,                       &
     qrtend,                       qstend,                       &
     nrtend,                       nstend,                       &
     effc,               effc_fn,            effi,               &
     sadice,                       sadsnow,                      &
     prect,                        preci,                        &
     nevapr,                       evapsnow,                     &
     am_evp_st,                                                  &
     prain,                        prodsnow,                     &
     cmeout,                       deffi,                        &
     pgamrad,                      lamcrad,                      &
     qsout,                        dsout,                        &
     lflx,               iflx,                                   &
     rflx,               sflx,               qrout,              &
     reff_rain,                    reff_snow,                    &
     qcsevap,            qisevap,            qvres,              &
     cmeitot,            vtrmc,              vtrmi,              &
     umr,                          ums,                          &
     qcsedten,                     qisedten,                     &
     qrsedten,                     qssedten,                     &
     pratot,                       prctot,                       &
     mnuccctot,          mnuccttot,          msacwitot,          &
     psacwstot,          bergstot,           bergtot,            &
     melttot,                      homotot,                      &
     qcrestot,           prcitot,            praitot,            &
     qirestot,           mnuccrtot,          pracstot,           &
     meltsdttot,         frzrdttot,          mnuccdtot,          &
     nrout,                        nsout,                        &
     refl,               arefl,              areflz,             &
     frefl,              csrfl,              acsrfl,             &
     fcsrfl,                       rercld,                       &
     ncai,                         ncal,                         &
     qrout2,                       qsout2,                       &
     nrout2,                       nsout2,                       &
     drout2,                       dsout2,                       &
     freqs,                        freqr,                        &
     nfice,                        qcrat,                        &
     errstring, & ! Below arguments are "optional" (pass null pointers to omit).
     tnd_qsnow,          tnd_nsnow,          re_ice,             &
     prer_evap,                                                      &
     frzimm,             frzcnt,             frzdep)
  ! Constituent properties.

    USE micro_mg_utils, ONLY: mg_liq_props, mg_ice_props, mg_rain_props, mg_snow_props 
  ! Size calculation functions.

    USE micro_mg_utils, ONLY: size_dist_param_liq, size_dist_param_liq_vec
    USE micro_mg_utils, ONLY: size_dist_param_basic, size_dist_param_basic_vec 
    USE micro_mg_utils, ONLY: avg_diameter, avg_diameter_vec
  ! Microphysical processes.

    USE micro_mg_utils, ONLY: ice_deposition_sublimation, sb2001v2_liq_autoconversion, sb2001v2_accre_cld_water_rain, &
    &kk2000_liq_autoconversion, ice_autoconversion, immersion_freezing, contact_freezing, snow_self_aggregation, &
    &accrete_cloud_water_snow, secondary_ice_production, accrete_rain_snow, heterogeneous_rain_freezing, &
    &accrete_cloud_water_rain, self_collection_rain, accrete_cloud_ice_snow, evaporate_sublimate_precip, bergeron_process_snow 
  !Authors: Hugh Morrison, Andrew Gettelman, NCAR, Peter Caldwell, LLNL
  ! e-mail: morrison@ucar.edu, andrew@ucar.edu
  ! input arguments


  integer,  intent(in) :: mgncol         ! number of microphysics columns
  integer,  intent(in) :: nlev           ! number of layers
  real(rkind_comp), intent(in) :: deltatin       ! time step (s)
  real(rkind_comp), intent(in) :: t(mgncol,nlev) ! input temperature (K)
  real(rkind_comp), intent(in) :: q(mgncol,nlev) ! input h20 vapor mixing ratio (kg/kg)
  ! note: all input cloud variables are grid-averaged

  real(rkind_comp), intent(in) :: qcn(mgncol,nlev)       ! cloud water mixing ratio (kg/kg)
  real(rkind_comp), intent(in) :: qin(mgncol,nlev)       ! cloud ice mixing ratio (kg/kg)
  real(rkind_comp), intent(in) :: ncn(mgncol,nlev)       ! cloud water number conc (1/kg)
  real(rkind_comp), intent(in) :: nin(mgncol,nlev)       ! cloud ice number conc (1/kg)

  real(rkind_comp), intent(in) :: qrn(mgncol,nlev)       ! rain mixing ratio (kg/kg)
  real(rkind_comp), intent(in) :: qsn(mgncol,nlev)       ! snow mixing ratio (kg/kg)
  real(rkind_comp), intent(in) :: nrn(mgncol,nlev)       ! rain number conc (1/kg)
  real(rkind_comp), intent(in) :: nsn(mgncol,nlev)       ! snow number conc (1/kg)

  real(rkind_comp), intent(in) :: relvar(mgncol,nlev)      ! cloud water relative variance (-)
  real(rkind_comp), intent(in) :: accre_enhan(mgncol,nlev) ! optional accretion
                                             ! enhancement factor (-)

  real(rkind_comp), intent(in) :: p(mgncol,nlev)        ! air pressure (pa)
  real(rkind_comp), intent(in) :: pdel(mgncol,nlev)     ! pressure difference across level (pa)

  real(rkind_comp), intent(in) :: cldn(mgncol,nlev)      ! cloud fraction (no units)
  real(rkind_comp), intent(in) :: liqcldf(mgncol,nlev)   ! liquid cloud fraction (no units)
  real(rkind_comp), intent(in) :: icecldf(mgncol,nlev)   ! ice cloud fraction (no units)
  real(rkind_comp), intent(in) :: qsatfac(mgncol,nlev)   ! subgrid cloud water saturation scaling factor (no units)
  ! used for scavenging
  ! Inputs for aerosol activation

  real(rkind_comp), intent(in) :: naai(mgncol,nlev)     ! ice nucleation number (from microp_aero_ts) (1/kg)
  real(rkind_comp), intent(in) :: npccn(mgncol,nlev)   ! ccn activated number tendency (from microp_aero_ts) (1/kg*s)
  ! Note that for these variables, the dust bin is assumed to be the last index.
  ! (For example, in CAM, the last dimension is always size 4.)

  real(rkind_comp), intent(in) :: rndst(:,:,:)  ! radius of each dust bin, for contact freezing (from microp_aero_ts) (m)
  real(rkind_comp), intent(in) :: nacon(:,:,:) ! number in each dust bin, for contact freezing  (from microp_aero_ts) (1/m^3)
  ! output arguments
  

  real(rkind_comp), intent(out) :: qcsinksum_rate1ord(mgncol,nlev) ! 1st order rate for
  ! direct cw to precip conversion
  real(rkind_comp), intent(out) :: tlat(mgncol,nlev)         ! latent heating rate       (W/kg)
  real(rkind_comp), intent(out) :: qvlat(mgncol,nlev)        ! microphysical tendency qv (1/s)
  real(rkind_comp), intent(out) :: qctend(mgncol,nlev)       ! microphysical tendency qc (1/s)
  real(rkind_comp), intent(out) :: qitend(mgncol,nlev)       ! microphysical tendency qi (1/s)
  real(rkind_comp), intent(out) :: nctend(mgncol,nlev)       ! microphysical tendency nc (1/(kg*s))
  real(rkind_comp), intent(out) :: nitend(mgncol,nlev)       ! microphysical tendency ni (1/(kg*s))

  real(rkind_comp), intent(out) :: qrtend(mgncol,nlev)       ! microphysical tendency qr (1/s)
  real(rkind_comp), intent(out) :: qstend(mgncol,nlev)       ! microphysical tendency qs (1/s)
  real(rkind_comp), intent(out) :: nrtend(mgncol,nlev)       ! microphysical tendency nr (1/(kg*s))
  real(rkind_comp), intent(out) :: nstend(mgncol,nlev)       ! microphysical tendency ns (1/(kg*s))
  real(rkind_comp), intent(out) :: effc(mgncol,nlev)         ! droplet effective radius (micron)
  real(rkind_comp), intent(out) :: effc_fn(mgncol,nlev)      ! droplet effective radius, assuming nc = 1.e8 kg-1
  real(rkind_comp), intent(out) :: effi(mgncol,nlev)         ! cloud ice effective radius (micron)
  real(rkind_comp), intent(out) :: sadice(mgncol,nlev)       ! cloud ice surface area density (cm2/cm3)
  real(rkind_comp), intent(out) :: sadsnow(mgncol,nlev)      ! cloud snow surface area density (cm2/cm3)
  real(rkind_comp), intent(out) :: prect(mgncol)             ! surface precip rate (m/s)
  real(rkind_comp), intent(out) :: preci(mgncol)             ! cloud ice/snow precip rate (m/s)
  real(rkind_comp), intent(out) :: nevapr(mgncol,nlev)       ! evaporation rate of rain + snow (1/s)
  real(rkind_comp), intent(out) :: evapsnow(mgncol,nlev)     ! sublimation rate of snow (1/s)
  real(rkind_comp), intent(out) :: am_evp_st(mgncol,nlev)    ! stratiform evaporation area (frac)
  real(rkind_comp), intent(out) :: prain(mgncol,nlev)        ! production of rain + snow (1/s)
  real(rkind_comp), intent(out) :: prodsnow(mgncol,nlev)     ! production of snow (1/s)
  real(rkind_comp), intent(out) :: cmeout(mgncol,nlev)       ! evap/sub of cloud (1/s)
  real(rkind_comp), intent(out) :: deffi(mgncol,nlev)        ! ice effective diameter for optics (radiation) (micron)
  real(rkind_comp), intent(out) :: pgamrad(mgncol,nlev)      ! ice gamma parameter for optics (radiation) (no units)
  real(rkind_comp), intent(out) :: lamcrad(mgncol,nlev)      ! slope of droplet distribution for optics (radiation) (1/m)
  real(rkind_comp), intent(out) :: qsout(mgncol,nlev)        ! snow mixing ratio (kg/kg)
  real(rkind_comp), intent(out) :: dsout(mgncol,nlev)        ! snow diameter (m)
  real(rkind_comp), intent(out) :: lflx(mgncol,nlev+1)       ! grid-box average liquid condensate flux (kg m^-2 s^-1)
  real(rkind_comp), intent(out) :: iflx(mgncol,nlev+1)       ! grid-box average ice condensate flux (kg m^-2 s^-1)
  real(rkind_comp), intent(out) :: rflx(mgncol,nlev+1)       ! grid-box average rain flux (kg m^-2 s^-1)
  real(rkind_comp), intent(out) :: sflx(mgncol,nlev+1)       ! grid-box average snow flux (kg m^-2 s^-1)
  real(rkind_comp), intent(out) :: qrout(mgncol,nlev)        ! grid-box average rain mixing ratio (kg/kg)
  real(rkind_comp), intent(out) :: reff_rain(mgncol,nlev)    ! rain effective radius (micron)
  real(rkind_comp), intent(out) :: reff_snow(mgncol,nlev)    ! snow effective radius (micron)
  real(rkind_comp), intent(out) :: qcsevap(mgncol,nlev)      ! cloud water evaporation due to sedimentation (1/s)
  real(rkind_comp), intent(out) :: qisevap(mgncol,nlev)      ! cloud ice sublimation due to sublimation (1/s)
  real(rkind_comp), intent(out) :: qvres(mgncol,nlev)        ! residual condensation term to ensure RH < 100% (1/s)
  real(rkind_comp), intent(out) :: cmeitot(mgncol,nlev)      ! grid-mean cloud ice sub/dep (1/s)
  real(rkind_comp), intent(out) :: vtrmc(mgncol,nlev)        ! mass-weighted cloud water fallspeed (m/s)
  real(rkind_comp), intent(out) :: vtrmi(mgncol,nlev)        ! mass-weighted cloud ice fallspeed (m/s)
  real(rkind_comp), intent(out) :: umr(mgncol,nlev)          ! mass weighted rain fallspeed (m/s)
  real(rkind_comp), intent(out) :: ums(mgncol,nlev)          ! mass weighted snow fallspeed (m/s)
  real(rkind_comp), intent(out) :: qcsedten(mgncol,nlev)     ! qc sedimentation tendency (1/s)
  real(rkind_comp), intent(out) :: qisedten(mgncol,nlev)     ! qi sedimentation tendency (1/s)
  real(rkind_comp), intent(out) :: qrsedten(mgncol,nlev)     ! qr sedimentation tendency (1/s)
  real(rkind_comp), intent(out) :: qssedten(mgncol,nlev)     ! qs sedimentation tendency (1/s)
  ! microphysical process rates for output (mixing ratio tendencies) (all have units of 1/s)

  real(rkind_comp), intent(out) :: pratot(mgncol,nlev)          ! accretion of cloud by rain
  real(rkind_comp), intent(out) :: prctot(mgncol,nlev)          ! autoconversion of cloud to rain
  real(rkind_comp), intent(out) :: mnuccctot(mgncol,nlev)       ! mixing ratio tend due to immersion freezing
  real(rkind_comp), intent(out) :: mnuccttot(mgncol,nlev)       ! mixing ratio tend due to contact freezing
  real(rkind_comp), intent(out) :: msacwitot(mgncol,nlev)       ! mixing ratio tend due to H-M splintering
  real(rkind_comp), intent(out) :: psacwstot(mgncol,nlev)       ! collection of cloud water by snow
  real(rkind_comp), intent(out) :: bergstot(mgncol,nlev)        ! bergeron process on snow
  real(rkind_comp), intent(out) :: bergtot(mgncol,nlev)         ! bergeron process on cloud ice
  real(rkind_comp), intent(out) :: melttot(mgncol,nlev)         ! melting of cloud ice
  real(rkind_comp), intent(out) :: homotot(mgncol,nlev)         ! homogeneous freezing cloud water
  real(rkind_comp), intent(out) :: qcrestot(mgncol,nlev)        ! residual cloud condensation due to removal of excess supersat
  real(rkind_comp), intent(out) :: prcitot(mgncol,nlev)         ! autoconversion of cloud ice to snow
  real(rkind_comp), intent(out) :: praitot(mgncol,nlev)         ! accretion of cloud ice by snow
  real(rkind_comp), intent(out) :: qirestot(mgncol,nlev)        ! residual ice deposition due to removal of excess supersat
  real(rkind_comp), intent(out) :: mnuccrtot(mgncol,nlev)       ! mixing ratio tendency due to heterogeneous freezing of rain to snow (1/s)
  real(rkind_comp), intent(out) :: pracstot(mgncol,nlev)        ! mixing ratio tendency due to accretion of rain by snow (1/s)
  real(rkind_comp), intent(out) :: meltsdttot(mgncol,nlev)      ! latent heating rate due to melting of snow  (W/kg)
  real(rkind_comp), intent(out) :: frzrdttot(mgncol,nlev)       ! latent heating rate due to homogeneous freezing of rain (W/kg)
  real(rkind_comp), intent(out) :: mnuccdtot(mgncol,nlev)       ! mass tendency from ice nucleation
  real(rkind_comp), intent(out) :: nrout(mgncol,nlev)        ! rain number concentration (1/m3)
  real(rkind_comp), intent(out) :: nsout(mgncol,nlev)        ! snow number concentration (1/m3)
  real(rkind_comp), intent(out) :: refl(mgncol,nlev)         ! analytic radar reflectivity
  real(rkind_comp), intent(out) :: arefl(mgncol,nlev)        ! average reflectivity will zero points outside valid range
  real(rkind_comp), intent(out) :: areflz(mgncol,nlev)       ! average reflectivity in z.
  real(rkind_comp), intent(out) :: frefl(mgncol,nlev)        ! fractional occurrence of radar reflectivity
  real(rkind_comp), intent(out) :: csrfl(mgncol,nlev)        ! cloudsat reflectivity
  real(rkind_comp), intent(out) :: acsrfl(mgncol,nlev)       ! cloudsat average
  real(rkind_comp), intent(out) :: fcsrfl(mgncol,nlev)       ! cloudsat fractional occurrence of radar reflectivity
  real(rkind_comp), intent(out) :: rercld(mgncol,nlev)       ! effective radius calculation for rain + cloud
  real(rkind_comp), intent(out) :: ncai(mgncol,nlev)         ! output number conc of ice nuclei available (1/m3)
  real(rkind_comp), intent(out) :: ncal(mgncol,nlev)         ! output number conc of CCN (1/m3)
  real(rkind_comp), intent(out) :: qrout2(mgncol,nlev)       ! copy of qrout as used to compute drout2
  real(rkind_comp), intent(out) :: qsout2(mgncol,nlev)       ! copy of qsout as used to compute dsout2
  real(rkind_comp), intent(out) :: nrout2(mgncol,nlev)       ! copy of nrout as used to compute drout2
  real(rkind_comp), intent(out) :: nsout2(mgncol,nlev)       ! copy of nsout as used to compute dsout2
  real(rkind_comp), intent(out) :: drout2(mgncol,nlev)       ! mean rain particle diameter (m)
  real(rkind_comp), intent(out) :: dsout2(mgncol,nlev)       ! mean snow particle diameter (m)
  real(rkind_comp), intent(out) :: freqs(mgncol,nlev)        ! fractional occurrence of snow
  real(rkind_comp), intent(out) :: freqr(mgncol,nlev)        ! fractional occurrence of rain
  real(rkind_comp), intent(out) :: nfice(mgncol,nlev)        ! fractional occurrence of ice
  real(rkind_comp), intent(out) :: qcrat(mgncol,nlev)        ! limiter for qc process rates (1=no limit --> 0. no qc)

  real(rkind_comp), intent(out) :: prer_evap(mgncol,nlev)

  character(128),   intent(out) :: errstring  ! output status (non-blank for error return)
  ! Tendencies calculated by external schemes that can replace MG's native
  ! process tendencies.
  ! Used with CARMA cirrus microphysics
  ! (or similar external microphysics model)


  real(rkind_comp), intent(in) :: tnd_qsnow(:,:) ! snow mass tendency (kg/kg/s)
  real(rkind_comp), intent(in) :: tnd_nsnow(:,:) ! snow number tendency (#/kg/s)
  real(rkind_comp), intent(in) :: re_ice(:,:)    ! ice effective radius (m)
  ! From external ice nucleation.

  real(rkind_comp), intent(in) :: frzimm(:,:) ! Number tendency due to immersion freezing (1/cm3)
  real(rkind_comp), intent(in) :: frzcnt(:,:) ! Number tendency due to contact freezing (1/cm3)
  real(rkind_comp), intent(in) :: frzdep(:,:) ! Number tendency due to deposition nucleation (1/cm3)
  ! local workspace
  ! all units mks unless otherwise stated
  ! local copies of input variables


  real(rkind_comp) :: qc(mgncol,nlev)      ! cloud liquid mixing ratio (kg/kg)
  real(rkind_comp) :: qi(mgncol,nlev)      ! cloud ice mixing ratio (kg/kg)
  real(rkind_comp) :: nc(mgncol,nlev)      ! cloud liquid number concentration (1/kg)
  real(rkind_comp) :: ni(mgncol,nlev)      ! cloud liquid number concentration (1/kg)
  real(rkind_comp) :: qr(mgncol,nlev)      ! rain mixing ratio (kg/kg)
  real(rkind_comp) :: qs(mgncol,nlev)      ! snow mixing ratio (kg/kg)
  real(rkind_comp) :: nr(mgncol,nlev)      ! rain number concentration (1/kg)
  real(rkind_comp) :: ns(mgncol,nlev)      ! snow number concentration (1/kg)
  ! general purpose variables

  real(rkind_comp) :: deltat            ! sub-time step (s)
  real(rkind_comp) :: mtime             ! the assumed ice nucleation timescale
  ! physical properties of the air at a given point

  real(rkind_comp) :: rho(mgncol,nlev)    ! density (kg m-3)
  real(rkind_comp) :: dv(mgncol,nlev)     ! diffusivity of water vapor
  real(rkind_comp) :: mu(mgncol,nlev)     ! viscosity
  real(rkind_comp) :: sc(mgncol,nlev)     ! schmidt number
  real(rkind_comp) :: rhof(mgncol,nlev)   ! density correction factor for fallspeed
  ! cloud fractions

  real(rkind_comp) :: precip_frac(mgncol,nlev) ! precip fraction assuming maximum overlap
  real(rkind_comp) :: cldm(mgncol,nlev)   ! cloud fraction
  real(rkind_comp) :: icldm(mgncol,nlev)  ! ice cloud fraction
  real(rkind_comp) :: lcldm(mgncol,nlev)  ! liq cloud fraction
  real(rkind_comp) :: qsfm(mgncol,nlev)   ! subgrid cloud water saturation scaling factor
  ! mass mixing ratios

  real(rkind_comp) :: qcic(mgncol,nlev)   ! in-cloud cloud liquid
  real(rkind_comp) :: qiic(mgncol,nlev)   ! in-cloud cloud ice
  real(rkind_comp) :: qsic(mgncol,nlev)   ! in-precip snow
  real(rkind_comp) :: qric(mgncol,nlev)   ! in-precip rain
  ! number concentrations

  real(rkind_comp) :: ncic(mgncol,nlev)   ! in-cloud droplet
  real(rkind_comp) :: niic(mgncol,nlev)   ! in-cloud cloud ice
  real(rkind_comp) :: nsic(mgncol,nlev)   ! in-precip snow
  real(rkind_comp) :: nric(mgncol,nlev)   ! in-precip rain
  ! maximum allowed ni value
  real(rkind_comp) :: nimax(mgncol,nlev)
  ! Size distribution parameters for:
  ! cloud ice

  real(rkind_comp) :: lami(mgncol,nlev)   ! slope
  real(rkind_comp) :: n0i(mgncol,nlev)    ! intercept
  ! cloud liquid
  real(rkind_comp) :: lamc(mgncol,nlev)   ! slope
  real(rkind_comp) :: pgam(mgncol,nlev)   ! spectral width parameter
  ! snow
  real(rkind_comp) :: lams(mgncol,nlev)   ! slope
  real(rkind_comp) :: n0s(mgncol,nlev)    ! intercept
  ! rain
  real(rkind_comp) :: lamr(mgncol,nlev)   ! slope
  real(rkind_comp) :: n0r(mgncol,nlev)    ! intercept
  ! Rates/tendencies due to:
  ! Instantaneous snow melting


  real(rkind_comp) :: minstsm(mgncol,nlev)    ! mass mixing ratio
  real(rkind_comp) :: ninstsm(mgncol,nlev)    ! number concentration
  ! Instantaneous rain freezing
  real(rkind_comp) :: minstrf(mgncol,nlev)    ! mass mixing ratio
  real(rkind_comp) :: ninstrf(mgncol,nlev)    ! number concentration
  ! deposition of cloud ice

  real(rkind_comp) :: vap_dep(mgncol,nlev)    ! deposition from vapor to ice PMC 12/3/12
  ! sublimation of cloud ice
  real(rkind_comp) :: ice_sublim(mgncol,nlev) ! sublimation from ice to vapor PMC 12/3/12
  ! ice nucleation
  real(rkind_comp) :: nnuccd(mgncol,nlev) ! number rate from deposition/cond.-freezing
  real(rkind_comp) :: mnuccd(mgncol,nlev) ! mass mixing ratio
  ! freezing of cloud water
  real(rkind_comp) :: mnuccc(mgncol,nlev) ! mass mixing ratio
  real(rkind_comp) :: nnuccc(mgncol,nlev) ! number concentration
  ! contact freezing of cloud water
  real(rkind_comp) :: mnucct(mgncol,nlev) ! mass mixing ratio
  real(rkind_comp) :: nnucct(mgncol,nlev) ! number concentration
  ! deposition nucleation in mixed-phase clouds (from external scheme)
  real(rkind_comp) :: mnudep(mgncol,nlev) ! mass mixing ratio
  real(rkind_comp) :: nnudep(mgncol,nlev) ! number concentration
  ! ice multiplication
  real(rkind_comp) :: msacwi(mgncol,nlev) ! mass mixing ratio
  real(rkind_comp) :: nsacwi(mgncol,nlev) ! number concentration
  ! autoconversion of cloud droplets
  real(rkind_comp) :: prc(mgncol,nlev)    ! mass mixing ratio
  real(rkind_comp) :: nprc(mgncol,nlev)   ! number concentration (rain)
  real(rkind_comp) :: nprc1(mgncol,nlev)  ! number concentration (cloud droplets)
  ! self-aggregation of snow
  real(rkind_comp) :: nsagg(mgncol,nlev)  ! number concentration
  ! self-collection of rain
  real(rkind_comp) :: nragg(mgncol,nlev)  ! number concentration
  ! collection of droplets by snow
  real(rkind_comp) :: psacws(mgncol,nlev)     ! mass mixing ratio
  real(rkind_comp) :: npsacws(mgncol,nlev)    ! number concentration
  ! collection of rain by snow
  real(rkind_comp) :: pracs(mgncol,nlev)  ! mass mixing ratio
  real(rkind_comp) :: npracs(mgncol,nlev) ! number concentration
  ! freezing of rain
  real(rkind_comp) :: mnuccr(mgncol,nlev) ! mass mixing ratio
  real(rkind_comp) :: nnuccr(mgncol,nlev) ! number concentration
  ! freezing of rain to form ice (mg add 4/26/13)
  real(rkind_comp) :: mnuccri(mgncol,nlev)    ! mass mixing ratio
  real(rkind_comp) :: nnuccri(mgncol,nlev)    ! number concentration
  ! accretion of droplets by rain
  real(rkind_comp) :: pra(mgncol,nlev)    ! mass mixing ratio
  real(rkind_comp) :: npra(mgncol,nlev)   ! number concentration
  ! autoconversion of cloud ice to snow
  real(rkind_comp) :: prci(mgncol,nlev)   ! mass mixing ratio
  real(rkind_comp) :: nprci(mgncol,nlev)  ! number concentration
  ! accretion of cloud ice by snow
  real(rkind_comp) :: prai(mgncol,nlev)   ! mass mixing ratio
  real(rkind_comp) :: nprai(mgncol,nlev)  ! number concentration
  ! evaporation of rain
  real(rkind_comp) :: pre(mgncol,nlev)    ! mass mixing ratio
  ! sublimation of snow
  real(rkind_comp) :: prds(mgncol,nlev)   ! mass mixing ratio
  ! number evaporation
  real(rkind_comp) :: nsubi(mgncol,nlev)  ! cloud ice
  real(rkind_comp) :: nsubc(mgncol,nlev)  ! droplet
  real(rkind_comp) :: nsubs(mgncol,nlev)  ! snow
  real(rkind_comp) :: nsubr(mgncol,nlev)  ! rain
  ! bergeron process
  real(rkind_comp) :: berg(mgncol,nlev)   ! mass mixing ratio (cloud ice)
  real(rkind_comp) :: bergs(mgncol,nlev)  ! mass mixing ratio (snow)
  ! fallspeeds
  ! number-weighted
 
  real(rkind_comp) :: uns(mgncol,nlev)    ! snow
  real(rkind_comp) :: unr(mgncol,nlev)    ! rain
  ! air density corrected fallspeed parameters
  real(rkind_comp) :: arn(mgncol,nlev)    ! rain
  real(rkind_comp) :: asn(mgncol,nlev)    ! snow
  real(rkind_comp) :: acn(mgncol,nlev)    ! cloud droplet
  real(rkind_comp) :: ain(mgncol,nlev)    ! cloud ice
  real(rkind_comp) :: ajn(mgncol,nlev)    ! cloud small ice
  ! Mass of liquid droplets used with external heterogeneous freezing.

  real(rkind_comp) :: mi0l(mgncol)
  ! saturation vapor pressures

  real(rkind_comp) :: esl(mgncol,nlev)    ! liquid
  real(rkind_comp) :: esi(mgncol,nlev)    ! ice
  real(rkind_comp) :: esn, esnA(mgncol)   ! checking for RH after rain evap
  
  ! saturation vapor mixing ratios

  real(rkind_comp) :: qvl(mgncol,nlev)    ! liquid
  real(rkind_comp) :: qvi(mgncol,nlev)    ! ice
  real(rkind_comp) :: qvn, qvnA(mgncol),qvnAI(mgncol)   ! checking for RH after rain evap
  ! relative humidity

  real(rkind_comp) :: relhum(mgncol,nlev)
  ! parameters for cloud water and cloud ice sedimentation calculations

  real(rkind_comp) :: fc(mgncol,nlev)
  real(rkind_comp) :: fnc(mgncol,nlev)
  real(rkind_comp) :: fi(mgncol,nlev)
  real(rkind_comp) :: fni(mgncol,nlev)

  real(rkind_comp) :: fr(mgncol,nlev)
  real(rkind_comp) :: fnr(mgncol,nlev)
  real(rkind_comp) :: fs(mgncol,nlev)
  real(rkind_comp) :: fns(mgncol,nlev)

!  real(rkind_comp) :: faloutc(nlev)
!  real(rkind_comp) :: faloutc2D(mgncol,nlev)
!  real(rkind_comp) :: faloutnc(nlev)
!  real(rkind_comp) :: faloutnc2D(mgncol,nlev)
!  real(rkind_comp) :: falouti(nlev)
!  real(rkind_comp) :: falouti2D(mgncol,nlev)
!  real(rkind_comp) :: faloutni(nlev)
!  real(rkind_comp) :: faloutni2D(mgncol,nlev)

!  real(rkind_comp) :: faloutr(nlev)
!  real(rkind_comp) :: faloutr2D(mgncol,nlev)
!  real(rkind_comp) :: faloutnr(nlev)
!  real(rkind_comp) :: faloutnr2D(mgncol,nlev)
!  real(rkind_comp) :: falouts(nlev)
!  real(rkind_comp) :: falouts2D(mgncol,nlev)
!  real(rkind_comp) :: faloutns(nlev)
!  real(rkind_comp) :: faloutns2D(mgncol,nlev)

!  real(rkind_comp) :: faltndc
!  real(rkind_comp) :: faltndnc
!  real(rkind_comp) :: faltndi
!  real(rkind_comp) :: faltndni
!  real(rkind_comp) :: faltndni1D(mgncol)
!  real(rkind_comp) :: faltndqie
!  real(rkind_comp) :: faltndqce

  real(rkind_comp) :: faltndr
  real(rkind_comp) :: faltndnr
  real(rkind_comp) :: faltnds
  real(rkind_comp) :: faltndns

  real(rkind_comp) :: rainrt(mgncol,nlev)     ! rain rate for reflectivity calculation
  ! dummy variables
  real(rkind_comp) :: tmpk1(nlev),tmpk2(nlev)

  real(rkind_comp) :: dum
  real(rkind_comp) :: dum1
  real(rkind_comp) :: dum2
  real(rkind_comp) :: dum1A(mgncol), dum2A(mgncol)
   
  real(rkind_comp) :: dumni0, dumni0A(mgncol), dumni0A2D(mgncol,nlev)
  real(rkind_comp) :: dumns0, dumns0A(mgncol), dumns0A2D(mgncol,nlev)
  ! dummies for checking RH
  real(rkind_comp) :: qtmp
  real(rkind_comp) :: ttmp
  real(rkind_comp) :: qtmpA(mgncol), ttmpA(mgncol)
  ! dummies for conservation check
  real(rkind_comp) :: ratio
  real(rkind_comp) :: tmpfrz
  ! dummies for in-cloud variables
  real(rkind_comp) :: dumc(mgncol,nlev)   ! qc
  real(rkind_comp) :: dumnc(mgncol,nlev)  ! nc
  real(rkind_comp) :: dumi(mgncol,nlev)   ! qi
  real(rkind_comp) :: dumni(mgncol,nlev)  ! ni
  real(rkind_comp) :: dumr(mgncol,nlev)   ! rain mixing ratio
  real(rkind_comp) :: dumnr(mgncol,nlev)  ! rain number concentration
  real(rkind_comp) :: dums(mgncol,nlev)   ! snow mixing ratio
  real(rkind_comp) :: dumns(mgncol,nlev)  ! snow number concentration
  ! Array dummy variable
  real(rkind_comp) :: dum_2D(mgncol,nlev)
  real(rkind_comp) :: pdel_inv(mgncol,nlev)
  ! loop array variables
  ! "i" and "k" are column/level iterators for internal (MG) variables
  ! "n" is used for other looping (currently just sedimentation)

  integer i, k, n
  ! number of sub-steps for loops over "n" (for sedimentation)

  integer nstep,nstepMax
  integer mdust
  integer :: precip_frac_method
  ! Varaibles to scale fall velocity between small and regular ice regimes.

  real(rkind_comp) :: irad
  real(rkind_comp) :: ifrac
  real(rkind_comp) :: rnstep

  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Return error message

  trimed_micro_mg_precip_frac_method = trim(micro_mg_precip_frac_method)

  !print *, "BEFORE PARALLEL REGION"


  errstring = ' '
  ! Process inputs
  ! assign variable deltat to deltatin


  deltat = deltatin
  ! Copies of input concentrations that may be changed internally.

  qc = qcn
  nc = ncn
  qi = qin
  ni = nin
  qr = qrn
  nr = nrn
  qs = qsn
  ns = nsn
  ! cldn: used to set cldm, unused for subcolumns
  ! liqcldf: used to set lcldm, unused for subcolumns
  ! icecldf: used to set icldm, unused for subcolumns


  if (microp_uniform) then
     ! subcolumns, set cloud fraction variables to one
     ! if cloud water or ice is present, if not present
     ! set to mincld (mincld used instead of zero, to prevent
     ! possible division by zero errors).

     where (qc >= qsmall)
        lcldm = 1._rkind_comp
     elsewhere
        lcldm = mincld
     end where

     where (qi >= qsmall)
        icldm = 1._rkind_comp
     elsewhere
        icldm = mincld
     end where

     cldm = max(icldm, lcldm)
     qsfm = 1._rkind_comp

  else
     ! get cloud fraction, check for minimum
     cldm = max(cldn,mincld)
     lcldm = max(liqcldf,mincld)
     icldm = max(icecldf,mincld)
     qsfm = qsatfac
  end if
  ! Initialize local variables
  ! local physical properties


  rho = p/(r*t)
  dv = 8.794E-5_rkind_comp * t**1.81_rkind_comp / p
  mu = 1.496E-6_rkind_comp * t**1.5_rkind_comp / (t + 120._rkind_comp)
  sc = mu/(rho*dv)
  ! air density adjustment for fallspeed parameters
  ! includes air density correction factor to the
  ! power of 0.54 following Heymsfield and Bansemer 2007


  rhof=(rhosu/rho)**0.54_rkind_comp

  arn=ar*rhof
  asn=as*rhof
  acn=g*rhow/(18._rkind_comp*mu)
  ain=ai*(rhosu/rho)**0.35_rkind_comp
  ajn=aj*(rhosu/rho)**0.35_rkind_comp
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Get humidity and saturation vapor pressures



  !$acc parallel num_gangs(128)
  ! print *,'mgncol*nlev: ',mgncol*nlev
  call qsat_water_vector(t, p, esl, qvl,mgncol*nlev)
  call qsat_ice_vector(t, p, esi, qvi,mgncol*nlev)
  !$acc loop gang collapse(2)
  do k=1,nlev
     !IDEA what if we pushed the K loop into this routines
!     call qsat_water(t(:,k), p(:,k), esl(:,k), qvl(:,k),mgncol)
!     call qsat_ice(t(:,k), p(:,k), esi(:,k), qvi(:,k),mgncol)
     do i=1,mgncol
        ! make sure when above freezing that esi=esl, not active yet

        if (t(i,k) >= tmelt) then
           esi(i,k)=esl(i,k)
           qvi(i,k)=qvl(i,k)
        else
           ! Scale the water saturation values to reflect subgrid scale
           ! ice cloud fraction, where ice clouds begin forming at a
           ! gridbox average relative humidity of rhmini (not 1).
           ! NOTE: For subcolumns and other non-subgrid clouds, qsfm willi
           ! be 1.

           !
           qvi(i,k) = qsfm(i,k) * qvi(i,k)
           esi(i,k) = qsfm(i,k) * esi(i,k)
           qvl(i,k) = qsfm(i,k) * qvl(i,k)
           esl(i,k) = qsfm(i,k) * esl(i,k)
        end if

     end do
  end do
  !$acc end parallel

  relhum = q / max(qvl, qsmall)
  !===============================================
  ! set mtime here to avoid answer-changing


  mtime=deltat
  ! initialize microphysics output

  NEC_BEGIN("initialize_to_zero")
  qcsevap=0._rkind_comp
  qisevap=0._rkind_comp
  qvres  =0._rkind_comp
  cmeitot =0._rkind_comp
  vtrmc =0._rkind_comp
  vtrmi =0._rkind_comp
  qcsedten =0._rkind_comp
  qisedten =0._rkind_comp
  qrsedten =0._rkind_comp
  qssedten =0._rkind_comp

  pratot=0._rkind_comp
  prctot=0._rkind_comp
  mnuccctot=0._rkind_comp
  mnuccttot=0._rkind_comp
  msacwitot=0._rkind_comp
  psacwstot=0._rkind_comp
  bergstot=0._rkind_comp
  bergtot=0._rkind_comp
  melttot=0._rkind_comp
  homotot=0._rkind_comp
  qcrestot=0._rkind_comp
  prcitot=0._rkind_comp
  praitot=0._rkind_comp
  qirestot=0._rkind_comp
  mnuccrtot=0._rkind_comp
  pracstot=0._rkind_comp
  meltsdttot=0._rkind_comp
  frzrdttot=0._rkind_comp
  mnuccdtot=0._rkind_comp

  rflx=0._rkind_comp
  sflx=0._rkind_comp
  lflx=0._rkind_comp
  iflx=0._rkind_comp
  ! initialize precip output


  qrout=0._rkind_comp
  qsout=0._rkind_comp
  nrout=0._rkind_comp
  nsout=0._rkind_comp
  ! for refl calc

  rainrt = 0._rkind_comp
  ! initialize rain size

  rercld=0._rkind_comp

  qcsinksum_rate1ord = 0._rkind_comp
  ! initialize variables for trop_mozart

  nevapr = 0._rkind_comp
  prer_evap = 0._rkind_comp
  evapsnow = 0._rkind_comp
  am_evp_st = 0._rkind_comp
  prain = 0._rkind_comp
  prodsnow = 0._rkind_comp
  cmeout = 0._rkind_comp

  precip_frac = mincld

  lamc=0._rkind_comp
  ! initialize microphysical tendencies


  tlat=0._rkind_comp
  qvlat=0._rkind_comp
  qctend=0._rkind_comp
  qitend=0._rkind_comp
  qstend = 0._rkind_comp
  qrtend = 0._rkind_comp
  nctend=0._rkind_comp
  nitend=0._rkind_comp
  nrtend = 0._rkind_comp
  nstend = 0._rkind_comp
  ! initialize in-cloud and in-precip quantities to zero

  qcic  = 0._rkind_comp
  qiic  = 0._rkind_comp
  qsic  = 0._rkind_comp
  qric  = 0._rkind_comp

  ncic  = 0._rkind_comp
  niic  = 0._rkind_comp
  nsic  = 0._rkind_comp
  nric  = 0._rkind_comp
  ! initialize precip at surface


  prect = 0._rkind_comp
  preci = 0._rkind_comp
  ! initialize precip fallspeeds to zero

  ums = 0._rkind_comp
  uns = 0._rkind_comp
  umr = 0._rkind_comp
  unr = 0._rkind_comp
  ! initialize limiter for output

  qcrat = 1._rkind_comp
  ! Many outputs have to be initialized here at the top to work around
  ! ifort problems, even if they are always overwritten later.

  effc = 10._rkind_comp
  lamcrad = 0._rkind_comp
  pgamrad = 0._rkind_comp
  effc_fn = 10._rkind_comp
  effi = 25._rkind_comp
  sadice = 0._rkind_comp
  sadsnow = 0._rkind_comp
  deffi = 50._rkind_comp

  qrout2 = 0._rkind_comp
  nrout2 = 0._rkind_comp
  drout2 = 0._rkind_comp
  qsout2 = 0._rkind_comp
  nsout2 = 0._rkind_comp
  dsout = 0._rkind_comp
  dsout2 = 0._rkind_comp

  freqr = 0._rkind_comp
  freqs = 0._rkind_comp

  reff_rain = 0._rkind_comp
  reff_snow = 0._rkind_comp

  refl = -9999._rkind_comp
  arefl = 0._rkind_comp
  areflz = 0._rkind_comp
  frefl = 0._rkind_comp
  csrfl = 0._rkind_comp
  acsrfl = 0._rkind_comp
  fcsrfl = 0._rkind_comp

  ncal = 0._rkind_comp
  ncai = 0._rkind_comp

  nfice = 0._rkind_comp
  NEC_END("initialize_to_zero")
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! droplet activation
  ! get provisional droplet number after activation. This is used for
  ! all microphysical process calculations, for consistency with update of
  ! droplet mass before microphysics
  ! calculate potential for droplet activation if cloud water is present
  ! tendency from activation (npccn) is read in from companion routine
  ! output activated liquid and ice (convert from #/kg -> #/m3)
  !--------------------------------------------------

  
  !NEC_BEGIN("where_block_#1")
  where (qc >= qsmall)
     nc = max(nc + npccn*deltat, 0._rkind_comp)
     ncal = nc*rho/lcldm ! sghan minimum in #/cm3
  elsewhere
     ncal = 0._rkind_comp
  end where

  where (t < icenuct)
     ncai = naai*rho
  elsewhere
     ncai = 0._rkind_comp
  end where
  !NEC_END("where_block_#1")
  !===============================================
  ! ice nucleation if activated nuclei exist at t<-5C AND rhmini + 5%
  ! NOTE: If using gridbox average values, condensation will not occur until rh=1,
  ! so the threshold seems like it should be 1.05 and not rhmini + 0.05. For subgrid
  ! clouds (using rhmini and qsfacm), the relhum has already been adjusted, and thus
  ! the nucleation threshold should also be 1.05 and not rhmini + 0.05.
  !-------------------------------------------------------


  !


  if (do_cldice) then
    !NEC_BEGIN("where_block_#2")
     where (naai > 0._rkind_comp .and. t < icenuct .and. &
          relhum*esl/esi > 1.05_rkind_comp)
        !if NAAI > 0. then set numice = naai (as before)
        !note: this is gridbox averaged

        nnuccd = (naai-ni/icldm)/mtime*icldm
        nnuccd = max(nnuccd,0._rkind_comp)
        nimax = naai*icldm
        !Calc mass of new particles using new crystal mass...
        !also this will be multiplied by mtime as nnuccd is...


        mnuccd = nnuccd * mi0

     elsewhere
        nnuccd = 0._rkind_comp
        nimax = 0._rkind_comp
        mnuccd = 0._rkind_comp
     end where
    !NEC_END("where_block_#2")

  end if
  !=============================================================================


  !NEC_BEGIN("loop_#1")
  do k=1,nlev

     do i=1,mgncol
        ! calculate instantaneous precip processes (melting and homogeneous freezing)
        ! melting of snow at +2 C


        if (t(i,k) > snowmelt) then
           if (qs(i,k) > 0._rkind_comp) then
              ! make sure melting snow doesn't reduce temperature below threshold

              dum = -xlf/cpp*qs(i,k)
              if (t(i,k)+dum < snowmelt) then
                 dum = (t(i,k)-snowmelt)*cpp/xlf
                 dum = dum/qs(i,k)
                 dum = max(0._rkind_comp,dum)
                 dum = min(1._rkind_comp,dum)
              else
                 dum = 1._rkind_comp
              end if

              minstsm(i,k) = dum*qs(i,k)
              ninstsm(i,k) = dum*ns(i,k)

              dum1=-xlf*minstsm(i,k)/deltat
              tlat(i,k)=tlat(i,k)+dum1
              meltsdttot(i,k)=meltsdttot(i,k) + dum1

              qs(i,k) = max(qs(i,k) - minstsm(i,k), 0._rkind_comp)
              ns(i,k) = max(ns(i,k) - ninstsm(i,k), 0._rkind_comp)
              qr(i,k) = max(qr(i,k) + minstsm(i,k), 0._rkind_comp)
              nr(i,k) = max(nr(i,k) + ninstsm(i,k), 0._rkind_comp)
           end if
        end if

     end do
  end do 

  do k=1,nlev
    do i=1,mgncol
        ! freezing of rain at -5 C

        if (t(i,k) < rainfrze) then

           if (qr(i,k) > 0._rkind_comp) then
              ! make sure freezing rain doesn't increase temperature above threshold

              dum = xlf/cpp*qr(i,k)
              if (t(i,k)+dum > rainfrze) then
                 dum = -(t(i,k)-rainfrze)*cpp/xlf
                 dum = dum/qr(i,k)
                 dum = max(0._rkind_comp,dum)
                 dum = min(1._rkind_comp,dum)
              else
                 dum = 1._rkind_comp
              end if

              minstrf(i,k) = dum*qr(i,k)
              ninstrf(i,k) = dum*nr(i,k)
              ! heating tendency

              dum1 = xlf*minstrf(i,k)/deltat
              tlat(i,k)=tlat(i,k)+dum1
              frzrdttot(i,k)=frzrdttot(i,k) + dum1

              qr(i,k) = max(qr(i,k) - minstrf(i,k), 0._rkind_comp)
              nr(i,k) = max(nr(i,k) - ninstrf(i,k), 0._rkind_comp)
              qs(i,k) = max(qs(i,k) + minstrf(i,k), 0._rkind_comp)
              ns(i,k) = max(ns(i,k) + ninstrf(i,k), 0._rkind_comp)

           end if
        end if
     end do
  end do 

  do k=1,nlev
    do i=1,mgncol
        ! obtain in-cloud values of cloud water/ice mixing ratios and number concentrations
        !-------------------------------------------------------
        ! for microphysical process calculations
        ! units are kg/kg for mixing ratio, 1/kg for number conc

        if (qc(i,k).ge.qsmall) then
           ! limit in-cloud values to 0.005 kg/kg
           qcic(i,k)=min(qc(i,k)/lcldm(i,k),5.e-3_rkind_comp)
           ncic(i,k)=max(nc(i,k)/lcldm(i,k),0._rkind_comp)
           ! specify droplet concentration

           if (nccons) then
              ncic(i,k)=ncnst/rho(i,k)
           end if
        else
           qcic(i,k)=0._rkind_comp
           ncic(i,k)=0._rkind_comp
        end if

        if (qi(i,k).ge.qsmall) then
           ! limit in-cloud values to 0.005 kg/kg
           qiic(i,k)=min(qi(i,k)/icldm(i,k),5.e-3_rkind_comp)
           niic(i,k)=max(ni(i,k)/icldm(i,k),0._rkind_comp)
           ! switch for specification of cloud ice number

           if (nicons) then
              niic(i,k)=ninst/rho(i,k)
           end if
        else
           qiic(i,k)=0._rkind_comp
           niic(i,k)=0._rkind_comp
        end if

     end do
  end do
  !NEC_END("loop_#1")
  !========================================================================
  ! for sub-columns cldm has already been set to 1 if cloud
  ! water or ice is present, so precip_frac will be correctly set below
  ! and nothing extra needs to be done here


  precip_frac = cldm
!  MG_PRECIP_FRAC_INCLOUD
!  MG_PRECIP_FRAC_OVERLAP

  !YSK if (trim(micro_mg_precip_frac_method) == 'in_cloud') then
  if (trimed_micro_mg_precip_frac_method == 'in_cloud') then
     precip_frac_method =  MG_PRECIP_FRAC_INCLOUD
  !else if(trim(micro_mg_precip_frac_method) == 'max_overlap') then
  else if(trimed_micro_mg_precip_frac_method == 'max_overlap') then
     precip_frac_method = MG_PRECIP_FRAC_OVERLAP
  endif

  do k=1,nlev

     NEC_BEGIN("precip_frac_method")
     !if (trim(micro_mg_precip_frac_method) == 'in_cloud') then
     if (precip_frac_method == MG_PRECIP_FRAC_INCLOUD) then

        if (k /= 1) then
           where (qc(:,k) < qsmall .and. qi(:,k) < qsmall)
              precip_frac(:,k) = precip_frac(:,k-1)
           end where
        endif

     !else if (trim(micro_mg_precip_frac_method) == 'max_overlap') then
     else if (precip_frac_method ==  MG_PRECIP_FRAC_OVERLAP) then
        ! calculate precip fraction based on maximum overlap assumption
        ! if rain or snow mix ratios are smaller than threshold,
        ! then leave precip_frac as cloud fraction at current level


        if (k /= 1) then
           where (qr(:,k-1) >= qsmall .or. qs(:,k-1) >= qsmall)
              precip_frac(:,k)=max(precip_frac(:,k-1),precip_frac(:,k))
           end where
        end if

     endif
     NEC_END("precip_frac_method")
     !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     ! get size distribution parameters based on in-cloud cloud water
     ! these calculations also ensure consistency between number and mixing ratio
     !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     ! cloud liquid
     !-------------------------------------------


     call size_dist_param_liq_vec(mg_liq_props, qcic(1:mgncol,k), ncic(1:mgncol,k),&
          rho(1:mgncol,k), pgam(1:mgncol,k), lamc(1:mgncol,k), mgncol)
     !========================================================================
     ! autoconversion of cloud liquid water to rain
     ! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
     ! minimum qc of 1 x 10^-8 prevents floating point error


     if (.not. do_sb_physics) then
       call kk2000_liq_autoconversion(microp_uniform, qcic(1:mgncol,k), &
          ncic(:,k), rho(:,k), relvar(:,k), prc(:,k), nprc(:,k), nprc1(:,k), mgncol)
     endif
     ! assign qric based on prognostic qr, using assumed precip fraction
     ! note: this could be moved above for consistency with qcic and qiic calculations

     qric(:,k) = qr(:,k)/precip_frac(:,k)
     nric(:,k) = nr(:,k)/precip_frac(:,k)
     ! limit in-precip mixing ratios to 10 g/kg

     qric(:,k)=min(qric(:,k),0.01_rkind_comp)
     ! add autoconversion to precip from above to get provisional rain mixing ratio
     ! and number concentration (qric and nric)


     where (qric(:,k).lt.qsmall)
        qric(:,k)=0._rkind_comp
        nric(:,k)=0._rkind_comp
     end where
     ! make sure number concentration is a positive number to avoid
     ! taking root of negative later


     nric(:,k)=max(nric(:,k),0._rkind_comp)
     ! Get size distribution parameters for cloud ice

     call size_dist_param_basic_vec(mg_ice_props, qiic(:,k), niic(:,k), lami(:,k), mgncol, n0=n0i(:,k))
     ! Alternative autoconversion 
     if (do_sb_physics) then
       call sb2001v2_liq_autoconversion(pgam(:,k),qcic(1:mgncol,k),ncic(:,k), &
            qric(:,k),rho(:,k),relvar(:,k),prc(:,k),nprc(:,k),nprc1(:,k), mgncol)     
     endif	  
     !.......................................................................
     ! Autoconversion of cloud ice to snow
     ! similar to Ferrier (1994)


     if (do_cldice) then
        call ice_autoconversion(t(:,k), qiic(:,k), lami(:,k), n0i(:,k), &
             dcs, prci(:,k), nprci(:,k), mgncol)
     else
        ! Add in the particles that we have already converted to snow, and
        ! don't do any further autoconversion of ice.
        prci(:,k)  = tnd_qsnow(:,k) / cldm(:,k)
        nprci(:,k) = tnd_nsnow(:,k) / cldm(:,k)
     end if
     ! note, currently we don't have this
     ! inside the do_cldice block, should be changed later
     ! assign qsic based on prognostic qs, using assumed precip fraction

     qsic(:,k) = qs(:,k)/precip_frac(:,k)
     nsic(:,k) = ns(:,k)/precip_frac(:,k)
     ! limit in-precip mixing ratios to 10 g/kg

     qsic(:,k)=min(qsic(:,k),0.01_rkind_comp)
     ! if precip mix ratio is zero so should number concentration


     where (qsic(:,k) < qsmall)
        qsic(:,k)=0._rkind_comp
        nsic(:,k)=0._rkind_comp
     end where
     ! make sure number concentration is a positive number to avoid
     ! taking root of negative later


     nsic(:,k)=max(nsic(:,k),0._rkind_comp)
     !.......................................................................
     ! get size distribution parameters for precip
     !......................................................................
     ! rain


     call size_dist_param_basic_vec(mg_rain_props, qric(:,k), nric(:,k), lamr(:,k), mgncol, n0=n0r(:,k))

     NEC_BEGIN("where_block_#3")
     qtmpA = lamr(:,k)**br
     where (lamr(:,k) >= qsmall)
        ! provisional rain number and mass weighted mean fallspeed (m/s)


        unr(:,k) = min(arn(:,k)*gamma_br_plus1/qtmpA(:),9.1_rkind_comp*rhof(:,k))
        umr(:,k) = min(arn(:,k)*gamma_br_plus4/(6._rkind_comp*qtmpA(:)),9.1_rkind_comp*rhof(:,k))

!        unr(:,k) = min(arn(:,k)*gamma_br_plus1/lamr(:,k)**br,9.1_rkind_comp*rhof(:,k))
!        umr(:,k) = min(arn(:,k)*gamma_br_plus4/(6._rkind_comp*lamr(:,k)**br),9.1_rkind_comp*rhof(:,k))

     elsewhere
        umr(:,k) = 0._rkind_comp
        unr(:,k) = 0._rkind_comp
     end where
     NEC_END("where_block_#3")
     !......................................................................
     ! snow


     call size_dist_param_basic_vec(mg_snow_props, qsic(:,k), nsic(:,k), &
          lams(:,k), mgncol, n0=n0s(:,k))

     NEC_BEGIN("where_block_#4")
     where (lams(:,k) > 0._rkind_comp)
        ! provisional snow number and mass weighted mean fallspeed (m/s)


        ums(:,k) = min(asn(:,k)*gamma_bs_plus4/(6._rkind_comp*lams(:,k)**bs),1.2_rkind_comp*rhof(:,k))
        uns(:,k) = min(asn(:,k)*gamma_bs_plus1/lams(:,k)**bs,1.2_rkind_comp*rhof(:,k))

     elsewhere
        ums(:,k) = 0._rkind_comp
        uns(:,k) = 0._rkind_comp
     end where
     NEC_END("where_block_#4")

     if (do_cldice) then
        if (.not. use_hetfrz_classnuc) then
           ! heterogeneous freezing of cloud water
           !----------------------------------------------


           call immersion_freezing(microp_uniform, t(:,k), pgam(:,k), lamc(:,k), &
                qcic(1:mgncol,k), ncic(:,k), relvar(:,k), mnuccc(:,k), nnuccc(:,k), mgncol)
           ! make sure number of droplets frozen does not exceed available ice nuclei concentration
           ! this prevents 'runaway' droplet freezing


           NEC_BEGIN("where_block_#5")
           where (qcic(1:mgncol,k).ge.qsmall .and. t(:,k).lt.269.15_rkind_comp)
              where (nnuccc(:,k)*lcldm(:,k).gt.nnuccd(:,k))
                 ! scale mixing ratio of droplet freezing with limit
                 mnuccc(:,k)=mnuccc(:,k)*(nnuccd(:,k)/(nnuccc(:,k)*lcldm(:,k)))
                 nnuccc(:,k)=nnuccd(:,k)/lcldm(:,k)
              end where
           end where
           NEC_END("where_block_#5")

           mdust = size(rndst,3)
           call contact_freezing(microp_uniform, t(:,k), p(:,k), rndst(:,k,:), &
                nacon(:,k,:), pgam(:,k), lamc(:,k), qcic(1:mgncol,k), ncic(:,k), &
                relvar(:,k), mnucct(:,k), nnucct(:,k), mgncol, mdust)

           mnudep(:,k)=0._rkind_comp
           nnudep(:,k)=0._rkind_comp

        else
           ! Mass of droplets frozen is the average droplet mass, except
           ! with two limiters: concentration must be at least 1/cm^3, and
           ! mass must be at least the minimum defined above.

           !NEC_BEGIN("where_block_#6")
           mi0l = qcic(1:mgncol,k)/max(ncic(:,k), 1.0e6_rkind_comp/rho(:,k))
           mi0l = max(mi0l_min, mi0l)

           where (qcic(1:mgncol,k) >= qsmall)
              nnuccc(:,k) = frzimm(:,k)*1.0e6_rkind_comp/rho(:,k)
              mnuccc(:,k) = nnuccc(:,k)*mi0l

              nnucct(:,k) = frzcnt(:,k)*1.0e6_rkind_comp/rho(:,k)
              mnucct(:,k) = nnucct(:,k)*mi0l

              nnudep(:,k) = frzdep(:,k)*1.0e6_rkind_comp/rho(:,k)
              mnudep(:,k) = nnudep(:,k)*mi0
           elsewhere
              nnuccc(:,k) = 0._rkind_comp
              mnuccc(:,k) = 0._rkind_comp

              nnucct(:,k) = 0._rkind_comp
              mnucct(:,k) = 0._rkind_comp

              nnudep(:,k) = 0._rkind_comp
              mnudep(:,k) = 0._rkind_comp
           end where
           !NEC_END("where_block_#6")

        end if

     else
        mnuccc(:,k)=0._rkind_comp
        nnuccc(:,k)=0._rkind_comp
        mnucct(:,k)=0._rkind_comp
        nnucct(:,k)=0._rkind_comp
        mnudep(:,k)=0._rkind_comp
        nnudep(:,k)=0._rkind_comp
     end if

     call snow_self_aggregation(t(:,k), rho(:,k), asn(:,k), rhosn, qsic(:,k), nsic(:,k), &
          nsagg(:,k), mgncol)

     call accrete_cloud_water_snow(t(:,k), rho(:,k), asn(:,k), uns(:,k), mu(:,k), &
          qcic(1:mgncol,k), ncic(:,k), qsic(:,k), pgam(:,k), lamc(:,k), lams(:,k), n0s(:,k), &
          psacws(:,k), npsacws(:,k), mgncol)

     if (do_cldice) then
        call secondary_ice_production(t(:,k), psacws(:,k), msacwi(:,k), nsacwi(:,k), mgncol)
     else
        nsacwi(:,k) = 0.0_rkind_comp
        msacwi(:,k) = 0.0_rkind_comp
     end if

     call accrete_rain_snow(t(:,k), rho(:,k), umr(:,k), ums(:,k), unr(:,k), uns(:,k), &
          qric(:,k), qsic(:,k), lamr(:,k), n0r(:,k), lams(:,k), n0s(:,k), &
          pracs(:,k), npracs(:,k), mgncol)

     call heterogeneous_rain_freezing(t(:,k), qric(:,k), nric(:,k), lamr(:,k), &
          mnuccr(:,k), nnuccr(:,k), mgncol)

     if (do_sb_physics) then
       call sb2001v2_accre_cld_water_rain(qcic(1:mgncol,k), ncic(:,k), qric(:,k), &
            rho(:,k), relvar(:,k), pra(:,k), npra(:,k), mgncol)     
     else
       call accrete_cloud_water_rain(microp_uniform, qric(:,k), qcic(1:mgncol,k), &
            ncic(:,k), relvar(:,k), accre_enhan(:,k), pra(:,k), npra(:,k), mgncol)
     endif

     call self_collection_rain(rho(:,k), qric(:,k), nric(:,k), nragg(:,k), mgncol)

     if (do_cldice) then
        call accrete_cloud_ice_snow(t(:,k), rho(:,k), asn(:,k), qiic(:,k), niic(:,k), &
             qsic(:,k), lams(:,k), n0s(:,k), prai(:,k), nprai(:,k), mgncol)
     else
        prai(:,k) = 0._rkind_comp
        nprai(:,k) = 0._rkind_comp
     end if

     call evaporate_sublimate_precip(t(:,k), rho(:,k), &
          dv(:,k), mu(:,k), sc(:,k), q(:,k), qvl(:,k), qvi(:,k), &
          lcldm(:,k), precip_frac(:,k), arn(:,k), asn(:,k), qcic(1:mgncol,k), qiic(:,k), &
          qric(:,k), qsic(:,k), lamr(:,k), n0r(:,k), lams(:,k), n0s(:,k), &
          pre(:,k), prds(:,k), am_evp_st(:,k), mgncol)

     call bergeron_process_snow(t(:,k), rho(:,k), dv(:,k), mu(:,k), sc(:,k), &
          qvl(:,k), qvi(:,k), asn(:,k), qcic(1:mgncol,k), qsic(:,k), lams(:,k), n0s(:,k), &
          bergs(:,k), mgncol)

     bergs(:,k)=bergs(:,k)*micro_mg_berg_eff_factor
     !+++PMC 12/3/12 - NEW VAPOR DEP/SUBLIMATION GOES HERE!!!

     if (do_cldice) then

        call ice_deposition_sublimation(t(:,k), q(:,k), qi(:,k), ni(:,k), &
             icldm(:,k), rho(:,k), dv(:,k), qvl(:,k), qvi(:,k), &
             berg(:,k), vap_dep(:,k), ice_sublim(:,k), mgncol)

        berg(:,k)=berg(:,k)*micro_mg_berg_eff_factor

        where (ice_sublim(:,k) < 0._rkind_comp .and. qi(:,k) > qsmall .and. icldm(:,k) > mincld)
           nsubi(:,k) = sublim_factor*ice_sublim(:,k) / qi(:,k) * ni(:,k) / icldm(:,k)

        elsewhere
           nsubi(:,k) = 0._rkind_comp
        end where
        ! bergeron process should not reduce nc unless
        ! all ql is removed (which is handled elsewhere)
        !in fact, nothing in this entire file makes nsubc nonzero.

        nsubc(:,k) = 0._rkind_comp

     end if !do_cldice
     !---PMC 12/3/12

     NEC_BEGIN("loop_#2")
     do i=1,mgncol
        ! conservation to ensure no negative values of cloud water/precipitation
        ! in case microphysical process rates are large
        !===================================================================
        ! note: for check on conservation, processes are multiplied by omsm
        ! to prevent problems due to round off error
        ! conservation of qc
        !-------------------------------------------------------------------


        dum = ((prc(i,k)+pra(i,k)+mnuccc(i,k)+mnucct(i,k)+msacwi(i,k)+ &
             psacws(i,k)+bergs(i,k))*lcldm(i,k)+berg(i,k))*deltat

        if (dum.gt.qc(i,k)) then
           ratio = qc(i,k)/deltat/((prc(i,k)+pra(i,k)+mnuccc(i,k)+mnucct(i,k)+ &
                msacwi(i,k)+psacws(i,k)+bergs(i,k))*lcldm(i,k)+berg(i,k))*omsm
           prc(i,k) = prc(i,k)*ratio
           pra(i,k) = pra(i,k)*ratio
           mnuccc(i,k) = mnuccc(i,k)*ratio
           mnucct(i,k) = mnucct(i,k)*ratio
           msacwi(i,k) = msacwi(i,k)*ratio
           psacws(i,k) = psacws(i,k)*ratio
           bergs(i,k) = bergs(i,k)*ratio
           berg(i,k) = berg(i,k)*ratio
           qcrat(i,k) = ratio
        else
           qcrat(i,k) = 1._rkind_comp
        end if
        !PMC 12/3/12: ratio is also frac of step w/ liquid.
        !thus we apply berg for "ratio" of timestep and vapor
        !deposition for the remaining frac of the timestep.

        if (qc(i,k) >= qsmall) then
           vap_dep(i,k) = vap_dep(i,k)*(1._rkind_comp-qcrat(i,k))
        end if

     end do
     NEC_END("loop_#2")

     NEC_BEGIN("loop_#3")
     do i=1,mgncol
        !=================================================================
        ! apply limiter to ensure that ice/snow sublimation and rain evap
        ! don't push conditions into supersaturation, and ice deposition/nucleation don't
        ! push conditions into sub-saturation
        ! note this is done after qc conservation since we don't know how large
        ! vap_dep is before then
        ! estimates are only approximate since other process terms haven't been limited
        ! for conservation yet
        ! first limit ice deposition/nucleation vap_dep + mnuccd


        dum1 = vap_dep(i,k) + mnuccd(i,k)
        if (dum1 > 1.e-20_rkind_comp) then
           dum = (q(i,k)-qvi(i,k))/(1._rkind_comp + xxls_squared*qvi(i,k)/(cpp*rv*t(i,k)**2))/deltat
           dum = max(dum,0._rkind_comp)
           if (dum1 > dum) then
              ! Allocate the limited "dum" tendency to mnuccd and vap_dep
              ! processes. Don't divide by cloud fraction; these are grid-
              ! mean rates.
              dum1 = mnuccd(i,k) / (vap_dep(i,k)+mnuccd(i,k))
              mnuccd(i,k) = dum*dum1
              vap_dep(i,k) = dum - mnuccd(i,k)
           end if
        end if

     end do
     NEC_END("loop_#3")
     NEC_BEGIN("loop_#4")

     do i=1,mgncol
        !===================================================================
        ! conservation of nc
        !-------------------------------------------------------------------

        dum = (nprc1(i,k)+npra(i,k)+nnuccc(i,k)+nnucct(i,k)+ &
             npsacws(i,k)-nsubc(i,k))*lcldm(i,k)*deltat

        if (dum.gt.nc(i,k)) then
           ratio = nc(i,k)/deltat/((nprc1(i,k)+npra(i,k)+nnuccc(i,k)+nnucct(i,k)+&
                npsacws(i,k)-nsubc(i,k))*lcldm(i,k))*omsm

           nprc1(i,k) = nprc1(i,k)*ratio
           npra(i,k) = npra(i,k)*ratio
           nnuccc(i,k) = nnuccc(i,k)*ratio
           nnucct(i,k) = nnucct(i,k)*ratio
           npsacws(i,k) = npsacws(i,k)*ratio
           nsubc(i,k)=nsubc(i,k)*ratio
        end if

        mnuccri(i,k)=0._rkind_comp
        nnuccri(i,k)=0._rkind_comp

        if (do_cldice) then
           ! freezing of rain to produce ice if mean rain size is smaller than Dcs

           if (lamr(i,k) > qsmall .and. 1._rkind_comp/lamr(i,k) < Dcs) then
              mnuccri(i,k)=mnuccr(i,k)
              nnuccri(i,k)=nnuccr(i,k)
              mnuccr(i,k)=0._rkind_comp
              nnuccr(i,k)=0._rkind_comp
           end if
        end if

     end do
     NEC_END("loop_#4")
     !NEC_BEGIN("loop_#5")
     do i=1,mgncol
        ! conservation of rain mixing ratio
        !-------------------------------------------------------------------

        dum = ((-pre(i,k)+pracs(i,k)+mnuccr(i,k)+mnuccri(i,k))*precip_frac(i,k)- &
             (pra(i,k)+prc(i,k))*lcldm(i,k))*deltat
        ! note that qrtend is included below because of instantaneous freezing/melt

        if (dum.gt.qr(i,k).and. &
             (-pre(i,k)+pracs(i,k)+mnuccr(i,k)+mnuccri(i,k)).ge.qsmall) then
           ratio = (qr(i,k)/deltat+(pra(i,k)+prc(i,k))*lcldm(i,k))/   &
                precip_frac(i,k)/(-pre(i,k)+pracs(i,k)+mnuccr(i,k)+mnuccri(i,k))*omsm
           pre(i,k)=pre(i,k)*ratio
           pracs(i,k)=pracs(i,k)*ratio
           mnuccr(i,k)=mnuccr(i,k)*ratio
           mnuccri(i,k)=mnuccri(i,k)*ratio
        end if

     end do

     !NEC_END("loop_#5")
     NEC_BEGIN("loop_#6")
     do i=1,mgncol
        ! conservation of rain number
        !-------------------------------------------------------------------
        ! Add evaporation of rain number.


        if (pre(i,k) < 0._rkind_comp) then
           dum = pre(i,k)*deltat/qr(i,k)
           dum = max(-1._rkind_comp,dum)
           nsubr(i,k) = dum*nr(i,k)/deltat
        else
           nsubr(i,k) = 0._rkind_comp
        end if

     end do
     NEC_END("loop_#6")
     !NEC_BEGIN("loop_#7")

     do i=1,mgncol

        dum = ((-nsubr(i,k)+npracs(i,k)+nnuccr(i,k)+nnuccri(i,k)-nragg(i,k))*precip_frac(i,k)- &
             nprc(i,k)*lcldm(i,k))*deltat

        if (dum.gt.nr(i,k)) then
           ratio = (nr(i,k)/deltat+nprc(i,k)*lcldm(i,k))/precip_frac(i,k)/ &
                (-nsubr(i,k)+npracs(i,k)+nnuccr(i,k)+nnuccri(i,k)-nragg(i,k))*omsm

           nragg(i,k)=nragg(i,k)*ratio
           npracs(i,k)=npracs(i,k)*ratio
           nnuccr(i,k)=nnuccr(i,k)*ratio
           nsubr(i,k)=nsubr(i,k)*ratio
           nnuccri(i,k)=nnuccri(i,k)*ratio
        end if

     end do
     !NEC_END("loop_#7")
     !NEC_BEGIN("loop_#8")

     if (do_cldice) then

        do i=1,mgncol
           ! conservation of qi
           !-------------------------------------------------------------------


           dum = ((-mnuccc(i,k)-mnucct(i,k)-mnudep(i,k)-msacwi(i,k))*lcldm(i,k)+(prci(i,k)+ &
                prai(i,k))*icldm(i,k)-mnuccri(i,k)*precip_frac(i,k) &
                -ice_sublim(i,k)-vap_dep(i,k)-berg(i,k)-mnuccd(i,k))*deltat

           if (dum.gt.qi(i,k)) then
              ratio = (qi(i,k)/deltat+vap_dep(i,k)+berg(i,k)+mnuccd(i,k)+ &
                   (mnuccc(i,k)+mnucct(i,k)+mnudep(i,k)+msacwi(i,k))*lcldm(i,k)+ &
                   mnuccri(i,k)*precip_frac(i,k))/ &
                   ((prci(i,k)+prai(i,k))*icldm(i,k)-ice_sublim(i,k))*omsm
              prci(i,k) = prci(i,k)*ratio
              prai(i,k) = prai(i,k)*ratio
              ice_sublim(i,k) = ice_sublim(i,k)*ratio
           end if

        end do

     end if
     !NEC_END("loop_#8")

     !NEC_BEGIN("loop_#9")

     if (do_cldice) then

        do i=1,mgncol
           ! conservation of ni
           !-------------------------------------------------------------------

           if (use_hetfrz_classnuc) then
              tmpfrz = nnuccc(i,k)
           else
              tmpfrz = 0._rkind_comp
           end if
           dum = ((-nnucct(i,k)-tmpfrz-nnudep(i,k)-nsacwi(i,k))*lcldm(i,k)+(nprci(i,k)+ &
                nprai(i,k)-nsubi(i,k))*icldm(i,k)-nnuccri(i,k)*precip_frac(i,k)- &
                nnuccd(i,k))*deltat

           if (dum.gt.ni(i,k)) then
              ratio = (ni(i,k)/deltat+nnuccd(i,k)+ &
                   (nnucct(i,k)+tmpfrz+nnudep(i,k)+nsacwi(i,k))*lcldm(i,k)+ &
                   nnuccri(i,k)*precip_frac(i,k))/ &
                   ((nprci(i,k)+nprai(i,k)-nsubi(i,k))*icldm(i,k))*omsm
              nprci(i,k) = nprci(i,k)*ratio
              nprai(i,k) = nprai(i,k)*ratio
              nsubi(i,k) = nsubi(i,k)*ratio
           end if

        end do

     end if

     !NEC_END("loop_#9")
     !NEC_BEGIN("loop_#10")
     do i=1,mgncol
        ! conservation of snow mixing ratio
        !-------------------------------------------------------------------

        dum = (-(prds(i,k)+pracs(i,k)+mnuccr(i,k))*precip_frac(i,k)-(prai(i,k)+prci(i,k))*icldm(i,k) &
             -(bergs(i,k)+psacws(i,k))*lcldm(i,k))*deltat

        if (dum.gt.qs(i,k).and.-prds(i,k).ge.qsmall) then
           ratio = (qs(i,k)/deltat+(prai(i,k)+prci(i,k))*icldm(i,k)+ &
                (bergs(i,k)+psacws(i,k))*lcldm(i,k)+(pracs(i,k)+mnuccr(i,k))*precip_frac(i,k))/ &
                precip_frac(i,k)/(-prds(i,k))*omsm
           prds(i,k)=prds(i,k)*ratio
        end if

     end do
     !NEC_END("loop_#10")
     !NEC_BEGIN("loop_#11")

     do i=1,mgncol
        ! conservation of snow number
        !-------------------------------------------------------------------
        ! calculate loss of number due to sublimation
        ! for now neglect sublimation of ns

        nsubs(i,k)=0._rkind_comp

        dum = ((-nsagg(i,k)-nsubs(i,k)-nnuccr(i,k))*precip_frac(i,k)-nprci(i,k)*icldm(i,k))*deltat

        if (dum.gt.ns(i,k)) then
           ratio = (ns(i,k)/deltat+nnuccr(i,k)* &
                precip_frac(i,k)+nprci(i,k)*icldm(i,k))/precip_frac(i,k)/ &
                (-nsubs(i,k)-nsagg(i,k))*omsm
           nsubs(i,k)=nsubs(i,k)*ratio
           nsagg(i,k)=nsagg(i,k)*ratio
        end if

     end do

     !NEC_END("loop_#11")
     NEC_BEGIN("loop_#12")
     do i=1,mgncol
        ! next limit ice and snow sublimation and rain evaporation
        ! get estimate of q and t at end of time step
        ! don't include other microphysical processes since they haven't
        ! been limited via conservation checks yet



           qtmpA(i)=q(i,k)-(ice_sublim(i,k)+vap_dep(i,k)+mnuccd(i,k)+ &
                (pre(i,k)+prds(i,k))*precip_frac(i,k))*deltat
           ttmpA(i)=t(i,k)+((pre(i,k)*precip_frac(i,k))*xxlv+ &
                (prds(i,k)*precip_frac(i,k)+vap_dep(i,k)+ice_sublim(i,k)+mnuccd(i,k))*xxls)*deltat/cpp
           ! use rhw to allow ice supersaturation
           call qsat_water_scalar(ttmpA(i), p(i,k), esnA(i), qvnAI(i))
     enddo
     NEC_END("loop_#12")
     ! modify ice/precip evaporation rate if q > qsat
     !call qsat_water_vector(ttmpA, p(:,k), esnA, qvnAI,mgncol)

     NEC_BEGIN("loop_#13")
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_rkind_comp) then
           if (qtmpA(i) > qvnAI(i)) then

              dum1A(i)=pre(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
              dum2A(i)=prds(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
              ! recalculate q and t after vap_dep and mnuccd but without evap or sublim
              qtmpA(i)=q(i,k)-(vap_dep(i,k)+mnuccd(i,k))*deltat
              ttmpA(i)=t(i,k)+((vap_dep(i,k)+mnuccd(i,k))*xxls)*deltat/cpp
           endif
         endif
         call qsat_water_scalar(ttmpA(i), p(i,k), esnA(i), qvnA(i))
      enddo
     NEC_END("loop_#13")
     ! use rhw to allow ice supersaturation
     !call qsat_water_vector(ttmpA, p(:,k), esnA, qvnA,mgncol)

     NEC_BEGIN("loop_#14")
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_rkind_comp) then
           if (qtmpA(i) > qvnAI(i)) then

              dum=(qtmpA(i)-qvnA(i))/(1._rkind_comp + xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
              dum=min(dum,0._rkind_comp)
              ! modify rates if needed, divide by precip_frac to get local (in-precip) value

              pre(i,k)=dum*dum1A(i)/deltat/precip_frac(i,k)
              ! do separately using RHI for prds and ice_sublim
           endif
        endif
        call qsat_ice_scalar(ttmpA(i), p(i,k), esnA(i), qvnA(i))
     enddo
     NEC_END("loop_#14")

     !call qsat_ice_vector(ttmpA, p(:,k), esnA, qvnA,mgncol)

     NEC_BEGIN("loop_#15")
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_rkind_comp) then
           if (qtmpA(i) > qvnAI(i)) then

              dum=(qtmpA(i)-qvnA(i))/(1._rkind_comp + xxls_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
              dum=min(dum,0._rkind_comp)
              ! modify rates if needed, divide by precip_frac to get local (in-precip) value

              prds(i,k) = dum*dum2A(i)/deltat/precip_frac(i,k)
              ! don't divide ice_sublim by cloud fraction since it is grid-averaged

              dum1A(i) = (1._rkind_comp-dum1A(i)-dum2A(i))
              ice_sublim(i,k) = dum*dum1A(i)/deltat
           end if
        end if

     end do
     NEC_END("loop_#15")
     ! Big "administration" loop enforces conservation, updates variables
     ! that accumulate over substeps, and sets output variables.


     NEC_BEGIN("loop_#16")
     do i=1,mgncol
        ! get tendencies due to microphysical conversion processes
        !==========================================================
        ! note: tendencies are multiplied by appropriate cloud/precip
        ! fraction to get grid-scale values
        ! note: vap_dep is already grid-average values
        ! The net tendencies need to be added to rather than overwritten,
        ! because they may have a value already set for instantaneous
        ! melting/freezing.


        qvlat(i,k) = qvlat(i,k)-(pre(i,k)+prds(i,k))*precip_frac(i,k)-&
             vap_dep(i,k)-ice_sublim(i,k)-mnuccd(i,k)-mnudep(i,k)*lcldm(i,k)

        tlat(i,k) = tlat(i,k)+((pre(i,k)*precip_frac(i,k)) &
             *xxlv+(prds(i,k)*precip_frac(i,k)+vap_dep(i,k)+ice_sublim(i,k)+mnuccd(i,k)+mnudep(i,k)*lcldm(i,k))*xxls+ &
             ((bergs(i,k)+psacws(i,k)+mnuccc(i,k)+mnucct(i,k)+msacwi(i,k))*lcldm(i,k)+(mnuccr(i,k)+ &
             pracs(i,k)+mnuccri(i,k))*precip_frac(i,k)+berg(i,k))*xlf)

        qctend(i,k) = qctend(i,k)+ &
             (-pra(i,k)-prc(i,k)-mnuccc(i,k)-mnucct(i,k)-msacwi(i,k)- &
             psacws(i,k)-bergs(i,k))*lcldm(i,k)-berg(i,k)

        if (do_cldice) then
           qitend(i,k) = qitend(i,k)+ &
                (mnuccc(i,k)+mnucct(i,k)+mnudep(i,k)+msacwi(i,k))*lcldm(i,k)+(-prci(i,k)- &
                prai(i,k))*icldm(i,k)+vap_dep(i,k)+berg(i,k)+ice_sublim(i,k)+ &
                mnuccd(i,k)+mnuccri(i,k)*precip_frac(i,k)
        end if

        qrtend(i,k) = qrtend(i,k)+ &
             (pra(i,k)+prc(i,k))*lcldm(i,k)+(pre(i,k)-pracs(i,k)- &
             mnuccr(i,k)-mnuccri(i,k))*precip_frac(i,k)

        qstend(i,k) = qstend(i,k)+ &
             (prai(i,k)+prci(i,k))*icldm(i,k)+(psacws(i,k)+bergs(i,k))*lcldm(i,k)+(prds(i,k)+ &
             pracs(i,k)+mnuccr(i,k))*precip_frac(i,k)


        cmeout(i,k) = vap_dep(i,k) + ice_sublim(i,k) + mnuccd(i,k)
        ! add output for cmei (accumulate)

        cmeitot(i,k) = vap_dep(i,k) + ice_sublim(i,k) + mnuccd(i,k)
        ! assign variables for trop_mozart, these are grid-average
        !-------------------------------------------------------------------
        ! evaporation/sublimation is stored here as positive term


        evapsnow(i,k) = -prds(i,k)*precip_frac(i,k)
        nevapr(i,k) = -pre(i,k)*precip_frac(i,k)
        prer_evap(i,k) = -pre(i,k)*precip_frac(i,k)
        ! change to make sure prain is positive: do not remove snow from
        ! prain used for wet deposition

        prain(i,k) = (pra(i,k)+prc(i,k))*lcldm(i,k)+(-pracs(i,k)- &
             mnuccr(i,k)-mnuccri(i,k))*precip_frac(i,k)
        prodsnow(i,k) = (prai(i,k)+prci(i,k))*icldm(i,k)+(psacws(i,k)+bergs(i,k))*lcldm(i,k)+(&
             pracs(i,k)+mnuccr(i,k))*precip_frac(i,k)
        ! following are used to calculate 1st order conversion rate of cloud water
        !    to rain and snow (1/s), for later use in aerosol wet removal routine
        ! previously, wetdepa used (prain/qc) for this, and the qc in wetdepa may be smaller than the qc
        !    used to calculate pra, prc, ... in this routine
        ! qcsinksum_rate1ord = { rate of direct transfer of cloud water to rain & snow }
        !                      (no cloud ice or bergeron terms)

        qcsinksum_rate1ord(i,k) = (pra(i,k)+prc(i,k)+psacws(i,k))*lcldm(i,k)
        ! Avoid zero/near-zero division.
        qcsinksum_rate1ord(i,k) = qcsinksum_rate1ord(i,k) / &
             max(qc(i,k),1.0e-30_rkind_comp)
        ! microphysics output, note this is grid-averaged


        pratot(i,k) = pra(i,k)*lcldm(i,k)
        prctot(i,k) = prc(i,k)*lcldm(i,k)
        mnuccctot(i,k) = mnuccc(i,k)*lcldm(i,k)
        mnuccttot(i,k) = mnucct(i,k)*lcldm(i,k)
        msacwitot(i,k) = msacwi(i,k)*lcldm(i,k)
        psacwstot(i,k) = psacws(i,k)*lcldm(i,k)
        bergstot(i,k) = bergs(i,k)*lcldm(i,k)
        bergtot(i,k) = berg(i,k)
        prcitot(i,k) = prci(i,k)*icldm(i,k)
        praitot(i,k) = prai(i,k)*icldm(i,k)
        mnuccdtot(i,k) = mnuccd(i,k)*icldm(i,k)

        pracstot(i,k) = pracs(i,k)*precip_frac(i,k)
        mnuccrtot(i,k) = mnuccr(i,k)*precip_frac(i,k)


        nctend(i,k) = nctend(i,k)+&
             (-nnuccc(i,k)-nnucct(i,k)-npsacws(i,k)+nsubc(i,k) &
             -npra(i,k)-nprc1(i,k))*lcldm(i,k)

        if (do_cldice) then
           if (use_hetfrz_classnuc) then
              tmpfrz = nnuccc(i,k)
           else
              tmpfrz = 0._rkind_comp
           end if
           nitend(i,k) = nitend(i,k)+ nnuccd(i,k)+ &
                (nnucct(i,k)+tmpfrz+nnudep(i,k)+nsacwi(i,k))*lcldm(i,k)+(nsubi(i,k)-nprci(i,k)- &
                nprai(i,k))*icldm(i,k)+nnuccri(i,k)*precip_frac(i,k)
        end if

        nstend(i,k) = nstend(i,k)+(nsubs(i,k)+ &
             nsagg(i,k)+nnuccr(i,k))*precip_frac(i,k)+nprci(i,k)*icldm(i,k)

        nrtend(i,k) = nrtend(i,k)+ &
             nprc(i,k)*lcldm(i,k)+(nsubr(i,k)-npracs(i,k)-nnuccr(i,k) &
             -nnuccri(i,k)+nragg(i,k))*precip_frac(i,k)
        ! make sure that ni at advanced time step does not exceed
        ! maximum (existing N + source terms*dt), which is possible if mtime < deltat
        ! note that currently mtime = deltat
        !================================================================


        if (do_cldice .and. nitend(i,k).gt.0._rkind_comp.and.ni(i,k)+nitend(i,k)*deltat.gt.nimax(i,k)) then
           nitend(i,k)=max(0._rkind_comp,(nimax(i,k)-ni(i,k))/deltat)
        end if

     end do
     NEC_END("loop_#16")
     ! End of "administration" loop


  end do ! end k loop
  !-----------------------------------------------------
  ! convert rain/snow q and N for output to history, note,
  ! output is for gridbox average


  qrout = qr
  nrout = nr * rho
  qsout = qs
  nsout = ns * rho
  ! calculate n0r and lamr from rain mass and number
  ! divide by precip fraction to get in-precip (local) values of
  ! rain mass and number, divide by rhow to get rain number in kg^-1


  call size_dist_param_basic_vec(mg_rain_props, qric, nric, lamr, mgncol*nlev, n0=n0r)
  do k=1,nlev
!     call size_dist_param_basic_vec(mg_rain_props, qric(:,k), nric(:,k), lamr(:,k), mgncol, n0=n0r(:,k))
     ! Calculate rercld
     ! calculate mean size of combined rain and cloud water


     call calc_rercld(lamr(:,k), n0r(:,k), lamc(:,k), pgam(:,k), qric(:,k), qcic(1:mgncol,k), ncic(:,k), &
          rercld(:,k), mgncol)

  enddo
  ! Assign variables back to start-of-timestep values
  ! Some state variables are changed before the main microphysics loop
  ! to make "instantaneous" adjustments. Afterward, we must move those changes
  ! back into the tendencies.
  ! These processes:
  !  - Droplet activation (npccn, impacts nc)
  !  - Instantaneous snow melting  (minstsm/ninstsm, impacts qr/qs/nr/ns)
  !  - Instantaneous rain freezing (minstfr/ninstrf, impacts qr/qs/nr/ns)
  !================================================================================
  ! Re-apply droplet activation tendency


  nc = ncn
  nctend = nctend + npccn
  ! Re-apply rain freezing and snow melting.

  dum_2D = qs
  qs = qsn
  qstend = qstend + (dum_2D-qs)/deltat

  dum_2D = ns
  ns = nsn
  nstend = nstend + (dum_2D-ns)/deltat

  dum_2D = qr
  qr = qrn
  qrtend = qrtend + (dum_2D-qr)/deltat

  dum_2D = nr
  nr = nrn
  nrtend = nrtend + (dum_2D-nr)/deltat
  !.............................................................................
  !================================================================================
  ! modify to include snow. in prain & evap (diagnostic here: for wet dep)


  nevapr = nevapr + evapsnow
  prain = prain + prodsnow


  do k=1,nlev
     do i=1,mgncol
        ! calculate sedimentation for cloud water and ice
        !================================================================================
        ! update in-cloud cloud mixing ratio and number concentration
        ! with microphysical tendencies to calculate sedimentation, assign to dummy vars
        ! note: these are in-cloud values***, hence we divide by cloud fraction


        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)/lcldm(i,k)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)/icldm(i,k)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k),0._rkind_comp)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat)/icldm(i,k),0._rkind_comp)

        dumr(i,k) = (qr(i,k)+qrtend(i,k)*deltat)/precip_frac(i,k)
        dumnr(i,k) = max((nr(i,k)+nrtend(i,k)*deltat)/precip_frac(i,k),0._rkind_comp)
        dums(i,k) = (qs(i,k)+qstend(i,k)*deltat)/precip_frac(i,k)
        dumns(i,k) = max((ns(i,k)+nstend(i,k)*deltat)/precip_frac(i,k),0._rkind_comp)
        ! switch for specification of droplet and crystal number


        if (nccons) then
           dumnc(i,k)=ncnst/rho(i,k)
        end if
        ! switch for specification of cloud ice number

        if (nicons) then
           dumni(i,k)=ninst/rho(i,k)
        end if
     enddo
  enddo

  call size_dist_param_basic_vec(mg_ice_props, dumi, dumni, lami, mgncol*nlev)
  call size_dist_param_liq_vec(mg_liq_props, dumc, dumnc, rho, pgam, lamc, mgncol*nlev)

  !NEC_BEGIN("loop_#18")
  do k=1,nlev
     do i=1,mgncol
        ! calculate number and mass weighted fall velocity for droplets and cloud ice
        !-------------------------------------------------------------------


        if (dumc(i,k).ge.qsmall) then

           vtrmc(i,k)=acn(i,k)*gamma(4._rkind_comp+bc+pgam(i,k))/ &
                (lamc(i,k)**bc*gamma(pgam(i,k)+4._rkind_comp))

           fc(i,k) = g*rho(i,k)*vtrmc(i,k)

           fnc(i,k) = g*rho(i,k)* &
                acn(i,k)*gamma(1._rkind_comp+bc+pgam(i,k))/ &
                (lamc(i,k)**bc*gamma(pgam(i,k)+1._rkind_comp))
        else
           fc(i,k) = 0._rkind_comp
           fnc(i,k)= 0._rkind_comp
        end if
        ! calculate number and mass weighted fall velocity for cloud ice


        if (dumi(i,k).ge.qsmall) then

           vtrmi(i,k)=min(ain(i,k)*gamma_bi_plus4/(6._rkind_comp*lami(i,k)**bi), &
                1.2_rkind_comp*rhof(i,k))

           fi(i,k) = g*rho(i,k)*vtrmi(i,k)
           fni(i,k) = g*rho(i,k)* &
                min(ain(i,k)*gamma_bi_plus1/lami(i,k)**bi,1.2_rkind_comp*rhof(i,k))
           ! adjust the ice fall velocity for smaller (r < 20 um) ice
           ! particles (blend over 18-20 um)

           irad = 1.5_rkind_comp / lami(i,k) * 1e6_rkind_comp
           ifrac = min(1._rkind_comp, max(0._rkind_comp, (irad - 18._rkind_comp) / 2._rkind_comp))
 
           if (ifrac .lt. 1._rkind_comp) then
              vtrmi(i,k) = ifrac * vtrmi(i,k) + & 
                 (1._rkind_comp - ifrac) * &
                 min(ajn(i,k)*gamma_bj_plus4/(6._rkind_comp*lami(i,k)**bj), &
                 1.2_rkind_comp*rhof(i,k))

              fi(i,k) = g*rho(i,k)*vtrmi(i,k)
              fni(i,k) = ifrac * fni(i,k) + & 
                 (1._rkind_comp - ifrac) * &
                 g*rho(i,k)* &
                 min(ajn(i,k)*gamma_bj_plus1/lami(i,k)**bj,1.2_rkind_comp*rhof(i,k))
           end if
        else
           fi(i,k) = 0._rkind_comp
           fni(i,k)= 0._rkind_comp
        end if

     enddo
  enddo
  !NEC_END("loop_#18")

  ! fallspeed for rain
  call size_dist_param_basic_vec(mg_rain_props, dumr, dumnr, lamr, mgncol*nlev)
  ! fallspeed for snow
  call size_dist_param_basic_vec(mg_snow_props, dums, dumns, lams, mgncol*nlev)
  !do k=1,nlev
     ! fallspeed for rain
     !call size_dist_param_basic_vec(mg_rain_props, dumr(:,k), dumnr(:,k), lamr(:,k), mgncol)
     ! fallspeed for snow
     !call size_dist_param_basic_vec(mg_snow_props, dums(:,k), dumns(:,k), lams(:,k), mgncol)
  !enddo

  !NEC_BEGIN("loop_#19")
  do k=1,nlev
     do i=1,mgncol
        if (lamr(i,k).ge.qsmall) then
           ! 'final' values of number and mass weighted mean fallspeed for rain (m/s)


           unr(i,k) = min(arn(i,k)*gamma_br_plus1/lamr(i,k)**br,9.1_rkind_comp*rhof(i,k))
           umr(i,k) = min(arn(i,k)*gamma_br_plus4/(6._rkind_comp*lamr(i,k)**br),9.1_rkind_comp*rhof(i,k))

           fr(i,k) = g*rho(i,k)*umr(i,k)
           fnr(i,k) = g*rho(i,k)*unr(i,k)

        else
           fr(i,k)=0._rkind_comp
           fnr(i,k)=0._rkind_comp
        end if



        if (lams(i,k).ge.qsmall) then
           ! 'final' values of number and mass weighted mean fallspeed for snow (m/s)

           ums(i,k) = min(asn(i,k)*gamma_bs_plus4/(6._rkind_comp*lams(i,k)**bs),1.2_rkind_comp*rhof(i,k))
           uns(i,k) = min(asn(i,k)*gamma_bs_plus1/lams(i,k)**bs,1.2_rkind_comp*rhof(i,k))

           fs(i,k) = g*rho(i,k)*ums(i,k)
           fns(i,k) = g*rho(i,k)*uns(i,k)

        else
           fs(i,k)=0._rkind_comp
           fns(i,k)=0._rkind_comp
        end if
        ! redefine dummy variables - sedimentation is calculated over grid-scale
        ! quantities to ensure conservation


        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat),0._rkind_comp)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat),0._rkind_comp)
        dumr(i,k) = (qr(i,k)+qrtend(i,k)*deltat)
        dumnr(i,k) = max((nr(i,k)+nrtend(i,k)*deltat),0._rkind_comp)
        dums(i,k) = (qs(i,k)+qstend(i,k)*deltat)
        dumns(i,k) = max((ns(i,k)+nstend(i,k)*deltat),0._rkind_comp)

        if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._rkind_comp
        if (dumi(i,k).lt.qsmall) dumni(i,k)=0._rkind_comp
        if (dumr(i,k).lt.qsmall) dumnr(i,k)=0._rkind_comp
        if (dums(i,k).lt.qsmall) dumns(i,k)=0._rkind_comp

        pdel_inv(i,k) = 1._rkind_comp/pdel(i,k)

     enddo
  end do       !!! vertical loop

  !NEC_END("loop_#19")
  ! initialize nstep for sedimentation sub-steps
  ! calculate number of split time steps to ensure courant stability criteria
  ! for sedimentation calculations
  !-------------------------------------------------------------------


  NEC_BEGIN("sedim#1")
  call UpdateTendencies_vec(mgncol,nlev,do_cldice,deltat,fi,fni,pdel_inv, &
                       qitend,nitend,qisedten,dumi,dumni,prect,iflx, &
                       xxlx=xxls,qxsevap=qisevap,tlat=tlat,qvlat=qvlat, &
                       xcldm=icldm,preci=preci)
  NEC_END("sedim#1")
  NEC_BEGIN("sedim#2")
  call UpdateTendencies_vec(mgncol,nlev,.TRUE.,deltat,fc,fnc,pdel_inv, &
                       qctend,nctend,qcsedten,dumc,dumnc,prect,lflx, &
                       xxlx=xxlv,qxsevap=qcsevap,tlat=tlat,qvlat=qvlat,xcldm=lcldm)
  NEC_END("sedim#2")
  NEC_BEGIN("sedim#3")
  call UpdateTendencies_vec(mgncol,nlev,.TRUE.,deltat,fr,fnr,pdel_inv, &
                       qrtend,nrtend,qrsedten,dumr,dumnr,prect,rflx)
  NEC_END("sedim#3")
  NEC_BEGIN("sedim#4")
!  call UpdateTendencies(mgncol,nlev,.TRUE.,deltat,fs,fns,pdel_inv,pdel, &
!                       qstend,nstend,qssedten,dums,dumns,prect,sflx,preci=preci)
!  print *,'preci: ', preci
  call UpdateTendencies_vec(mgncol,nlev,.TRUE.,deltat,fs,fns,pdel_inv, &
                       qstend,nstend,qssedten,dums,dumns,prect,sflx,preci=preci)
!  stop
  NEC_END("sedim#4")
  ! end sedimentation
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! get new update for variables that includes sedimentation tendency
  ! note : here dum variables are grid-average, NOT in-cloud


  !NEC_BEGIN("loop_#20")
  do k=1,nlev
     do i=1,mgncol
        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._rkind_comp)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._rkind_comp)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._rkind_comp)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._rkind_comp)

        dumr(i,k) = max(qr(i,k)+qrtend(i,k)*deltat,0._rkind_comp)
        dumnr(i,k) = max(nr(i,k)+nrtend(i,k)*deltat,0._rkind_comp)
        dums(i,k) = max(qs(i,k)+qstend(i,k)*deltat,0._rkind_comp)
        dumns(i,k) = max(ns(i,k)+nstend(i,k)*deltat,0._rkind_comp)
        ! switch for specification of droplet and crystal number

        if (nccons) then
           dumnc(i,k)=ncnst/rho(i,k)*lcldm(i,k)
        end if
        ! switch for specification of cloud ice number

        if (nicons) then
           dumni(i,k)=ninst/rho(i,k)*icldm(i,k)
        end if

        if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._rkind_comp
        if (dumi(i,k).lt.qsmall) dumni(i,k)=0._rkind_comp
        if (dumr(i,k).lt.qsmall) dumnr(i,k)=0._rkind_comp
        if (dums(i,k).lt.qsmall) dumns(i,k)=0._rkind_comp

     enddo
  enddo
  !NEC_END("loop_#20")
  ! calculate instantaneous processes (melting, homogeneous freezing)
  !====================================================================
  ! melting of snow at +2 C


  !NEC_BEGIN("loop_#21")
  do k=1,nlev
     do i=1,mgncol

        if (t(i,k)+tlat(i,k)/cpp*deltat > snowmelt) then
           if (dums(i,k) > 0._rkind_comp) then
              ! make sure melting snow doesn't reduce temperature below threshold

              dum = -xlf/cpp*dums(i,k)
              if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt. snowmelt) then
                 dum = (t(i,k)+tlat(i,k)/cpp*deltat-snowmelt)*cpp/xlf
                 dum = dum/dums(i,k)
                 dum = max(0._rkind_comp,dum)
                 dum = min(1._rkind_comp,dum)
              else
                 dum = 1._rkind_comp
              end if

              qstend(i,k)=qstend(i,k)-dum*dums(i,k)/deltat
              nstend(i,k)=nstend(i,k)-dum*dumns(i,k)/deltat
              qrtend(i,k)=qrtend(i,k)+dum*dums(i,k)/deltat
              nrtend(i,k)=nrtend(i,k)+dum*dumns(i,k)/deltat

              dum1=-xlf*dum*dums(i,k)/deltat
              tlat(i,k)=tlat(i,k)+dum1
              meltsdttot(i,k)=meltsdttot(i,k) + dum1
           end if
        end if
     enddo
  enddo
  !NEC_END("loop_#21")
  NEC_BEGIN("loop_#22")
   do k=1,nlev
      do i=1,mgncol
        ! freezing of rain at -5 C


        if (t(i,k)+tlat(i,k)/cpp*deltat < rainfrze) then
           if (dumr(i,k) > 0._rkind_comp) then
              ! make sure freezing rain doesn't increase temperature above threshold

              dum_2D(i,k) = xlf/cpp*dumr(i,k)
              if (t(i,k)+tlat(i,k)/cpp*deltat+dum_2D(i,k).gt.rainfrze) then
                 dum_2D(i,k) = -(t(i,k)+tlat(i,k)/cpp*deltat-rainfrze)*cpp/xlf
                 dum_2D(i,k) = dum_2D(i,k)/dumr(i,k)
                 dum_2D(i,k) = max(0._rkind_comp,dum_2D(i,k))
                 dum_2D(i,k) = min(1._rkind_comp,dum_2D(i,k))
              else
                 dum_2D(i,k) = 1._rkind_comp
              end if

              qrtend(i,k)=qrtend(i,k)-dum_2D(i,k)*dumr(i,k)/deltat
              nrtend(i,k)=nrtend(i,k)-dum_2D(i,k)*dumnr(i,k)/deltat
              ! get mean size of rain = 1/lamr, add frozen rain to either snow or cloud ice
              ! depending on mean rain size


           endif
        endif
      enddo
    enddo
    NEC_END("loop_#22")
    call size_dist_param_basic_vec(mg_rain_props, dumr, dumnr, lamr,mgncol*nlev)
    NEC_BEGIN("loop_#23")
    do k=1,nlev
      do i=1,mgncol
        if (t(i,k)+tlat(i,k)/cpp*deltat < rainfrze) then
           if (dumr(i,k) > 0._rkind_comp) then
              if (lamr(i,k) < 1._rkind_comp/Dcs) then
                 qstend(i,k)=qstend(i,k)+dum_2D(i,k)*dumr(i,k)/deltat
                 nstend(i,k)=nstend(i,k)+dum_2D(i,k)*dumnr(i,k)/deltat
              else
                 qitend(i,k)=qitend(i,k)+dum_2D(i,k)*dumr(i,k)/deltat
                 nitend(i,k)=nitend(i,k)+dum_2D(i,k)*dumnr(i,k)/deltat
              end if
              ! heating tendency

              dum1 = xlf*dum_2D(i,k)*dumr(i,k)/deltat
              frzrdttot(i,k)=frzrdttot(i,k) + dum1
              tlat(i,k)=tlat(i,k)+dum1

           end if
        end if

      enddo
   enddo
   NEC_END("loop_#23")
   if (do_cldice) then
      do k=1,nlev
        do i=1,mgncol
           if (t(i,k)+tlat(i,k)/cpp*deltat > tmelt) then
              if (dumi(i,k) > 0._rkind_comp) then
                 ! limit so that melting does not push temperature below freezing
                 !-----------------------------------------------------------------

                 dum = -dumi(i,k)*xlf/cpp
                 if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.tmelt) then
                    dum = (t(i,k)+tlat(i,k)/cpp*deltat-tmelt)*cpp/xlf
                    dum = dum/dumi(i,k)
                    dum = max(0._rkind_comp,dum)
                    dum = min(1._rkind_comp,dum)
                 else
                    dum = 1._rkind_comp
                 end if

                 qctend(i,k)=qctend(i,k)+dum*dumi(i,k)/deltat
                 ! for output

                 melttot(i,k)=dum*dumi(i,k)/deltat
                 ! assume melting ice produces droplet
                 ! mean volume radius of 8 micron


                 nctend(i,k)=nctend(i,k)+3._rkind_comp*dum*dumi(i,k)/deltat/ &
                      (4._rkind_comp*pi*5.12e-16_rkind_comp*rhow)

                 qitend(i,k)=((1._rkind_comp-dum)*dumi(i,k)-qi(i,k))/deltat
                 nitend(i,k)=((1._rkind_comp-dum)*dumni(i,k)-ni(i,k))/deltat
                 tlat(i,k)=tlat(i,k)-xlf*dum*dumi(i,k)/deltat
              end if
           end if
        enddo
     enddo
     ! homogeneously freeze droplets at -40 C
     !-----------------------------------------------------------------


     do k=1,nlev
        do i=1,mgncol
           if (t(i,k)+tlat(i,k)/cpp*deltat < 233.15_rkind_comp) then
              if (dumc(i,k) > 0._rkind_comp) then
                 ! limit so that freezing does not push temperature above threshold

                 dum = dumc(i,k)*xlf/cpp
                 if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.233.15_rkind_comp) then
                    dum = -(t(i,k)+tlat(i,k)/cpp*deltat-233.15_rkind_comp)*cpp/xlf
                    dum = dum/dumc(i,k)
                    dum = max(0._rkind_comp,dum)
                    dum = min(1._rkind_comp,dum)
                 else
                    dum = 1._rkind_comp
                 end if

                 qitend(i,k)=qitend(i,k)+dum*dumc(i,k)/deltat
                 ! for output
                 homotot(i,k)=dum*dumc(i,k)/deltat
                 ! assume 25 micron mean volume radius of homogeneously frozen droplets
                 ! consistent with size of detrained ice in stratiform.F90

                 nitend(i,k)=nitend(i,k)+dum*3._rkind_comp*dumc(i,k)/(4._rkind_comp*3.14_rkind_comp*1.563e-14_rkind_comp* &
                      500._rkind_comp)/deltat
                 qctend(i,k)=((1._rkind_comp-dum)*dumc(i,k)-qc(i,k))/deltat
                 nctend(i,k)=((1._rkind_comp-dum)*dumnc(i,k)-nc(i,k))/deltat
                 tlat(i,k)=tlat(i,k)+xlf*dum*dumc(i,k)/deltat
              end if
           end if
        enddo 
     enddo 
     ! remove any excess over-saturation, which is possible due to non-linearity when adding
     ! together all microphysical processes
     !-----------------------------------------------------------------
     ! follow code similar to old CAM scheme
     do k=1,nlev
        do i=1,mgncol
           qtmpA(i)=q(i,k)+qvlat(i,k)*deltat
           ttmpA(i)=t(i,k)+tlat(i,k)/cpp*deltat
           ! use rhw to allow ice supersaturation
           call qsat_water_scalar(ttmpA(i), p(i,k), esnA(i), qvnA(i))
        enddo
        !call qsat_water_vector(ttmpA, p(:,k), esnA, qvnA,mgncol)

      NEC_BEGIN("loop_#26")
        do i=1,mgncol
           if (qtmpA(i) > qvnA(i) .and. qvnA(i) > 0 .and. allow_sed_supersat) then
              ! expression below is approximate since there may be ice deposition
              dum = (qtmpA(i)-qvnA(i))/(1._rkind_comp+xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))/deltat
              ! add to output cme
              cmeout(i,k) = cmeout(i,k)+dum
              ! now add to tendencies, partition between liquid and ice based on temperature
              if (ttmpA(i) > 268.15_rkind_comp) then
                 dum1=0.0_rkind_comp
                 ! now add to tendencies, partition between liquid and ice based on te
                 !-------------------------------------------------------
              else if (ttmpA(i) < 238.15_rkind_comp) then
                 dum1=1.0_rkind_comp
              else
                 dum1=(268.15_rkind_comp-ttmpA(i))/30._rkind_comp
              end if

              dum = (qtmpA(i)-qvnA(i))/(1._rkind_comp+(xxls*dum1+xxlv*(1._rkind_comp-dum1))**2 &
                   *qvnA(i)/(cpp*rv*ttmpA(i)**2))/deltat
              qctend(i,k)=qctend(i,k)+dum*(1._rkind_comp-dum1)
              ! for output
              qcrestot(i,k)=dum*(1._rkind_comp-dum1)
              qitend(i,k)=qitend(i,k)+dum*dum1
              qirestot(i,k)=dum*dum1
              qvlat(i,k)=qvlat(i,k)-dum
              ! for output
              qvres(i,k)=-dum
              tlat(i,k)=tlat(i,k)+dum*(1._rkind_comp-dum1)*xxlv+dum*dum1*xxls
           end if
        enddo 
      NEC_END("loop_#26")
     enddo 

  end if
  ! calculate effective radius for pass to radiation code
  !=========================================================
  ! if no cloud water, default value is 10 micron for droplets,
  ! 25 micron for cloud ice
  ! update cloud variables after instantaneous processes to get effective radius
  ! variables are in-cloud to calculate size dist parameters


  do k=1,nlev
     do i=1,mgncol
        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._rkind_comp)/lcldm(i,k)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._rkind_comp)/icldm(i,k)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._rkind_comp)/lcldm(i,k)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._rkind_comp)/icldm(i,k)

        dumr(i,k) = max(qr(i,k)+qrtend(i,k)*deltat,0._rkind_comp)/precip_frac(i,k)
        dumnr(i,k) = max(nr(i,k)+nrtend(i,k)*deltat,0._rkind_comp)/precip_frac(i,k)
        dums(i,k) = max(qs(i,k)+qstend(i,k)*deltat,0._rkind_comp)/precip_frac(i,k)
        dumns(i,k) = max(ns(i,k)+nstend(i,k)*deltat,0._rkind_comp)/precip_frac(i,k)
        ! switch for specification of droplet and crystal number

        if (nccons) then
           dumnc(i,k)=ncnst/rho(i,k)
        end if
        ! switch for specification of cloud ice number

        if (nicons) then
           dumni(i,k)=ninst/rho(i,k)
        end if
        ! limit in-cloud mixing ratio to reasonable value of 5 g kg-1

        dumc(i,k)=min(dumc(i,k),5.e-3_rkind_comp)
        dumi(i,k)=min(dumi(i,k),5.e-3_rkind_comp)
        ! limit in-precip mixing ratios
        dumr(i,k)=min(dumr(i,k),10.e-3_rkind_comp)
        dums(i,k)=min(dums(i,k),10.e-3_rkind_comp)
     enddo
  enddo
  ! cloud ice effective radius
  !-----------------------------------------------------------------

  if (do_cldice) then
     !do k=1,nlev
     dum_2D = dumni
     call size_dist_param_basic_vec(mg_ice_props, dumi, dumni, lami, mgncol*nlev, dumni0A2D)
     NEC_BEGIN("loop_#28")
     do k=1,nlev
     !   dum_2D(:,k) = dumni(:,k)
     !   call size_dist_param_basic_vec(mg_ice_props, dumi(:,k), dumni(:,k), lami(:,k), mgncol, dumni0A)
        do i=1,mgncol
           if (dumi(i,k).ge.qsmall) then


              if (dumni(i,k) /=dum_2D(i,k)) then
                 ! adjust number conc if needed to keep mean size in reasonable range
                 nitend(i,k)=(dumni(i,k)*icldm(i,k)-ni(i,k))/deltat
              end if

              effi(i,k) = 1.5_rkind_comp/lami(i,k)*1.e6_rkind_comp
              sadice(i,k) = 2._rkind_comp*pi*(lami(i,k)**(-3))*dumni0A2D(i,k)*rho(i,k)*1.e-2_rkind_comp  ! m2/m3 -> cm2/cm3

           else
              effi(i,k) = 25._rkind_comp
              sadice(i,k) = 0._rkind_comp
           end if
           ! ice effective diameter for david mitchell's optics

           deffi(i,k)=effi(i,k)*rhoi/rhows*2._rkind_comp
        enddo
     enddo
     NEC_END("loop_#28")
  else
     NEC_BEGIN("loop_#29")
     do k=1,nlev
        do i=1,mgncol
           ! NOTE: If CARMA is doing the ice microphysics, then the ice effective
           ! radius has already been determined from the size distribution.
           effi(i,k) = re_ice(i,k) * 1.e6_rkind_comp      ! m -> um
           deffi(i,k)=effi(i,k) * 2._rkind_comp
           sadice(i,k) = 4._rkind_comp*pi*(effi(i,k)**2)*ni(i,k)*rho(i,k)*1e-2_rkind_comp
        enddo
     enddo
     NEC_END("loop_#29")
  end if
  ! cloud droplet effective radius
  !-----------------------------------------------------------------

  dum_2D = dumnc
  call size_dist_param_liq_vec(mg_liq_props, dumc, dumnc, rho, pgam, lamc,mgncol*nlev)
  do k=1,nlev
     !dum_2D(:,k) = dumnc(:,k)
     !call size_dist_param_liq_vec(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), pgam(:,k), lamc(:,k),mgncol)
      NEC_BEGIN("loop_#30")
     do i=1,mgncol
        if (dumc(i,k).ge.qsmall) then
           ! switch for specification of droplet and crystal number


           if (nccons) then
              ! make sure nc is consistence with the constant N by adjusting tendency, need
              ! to multiply by cloud fraction
              ! note that nctend may be further adjusted below if mean droplet size is
              ! out of bounds

              nctend(i,k)=(ncnst/rho(i,k)*lcldm(i,k)-nc(i,k))/deltat

           end if

           if (dum_2D(i,k) /= dumnc(i,k)) then
              ! adjust number conc if needed to keep mean size in reasonable range
              nctend(i,k)=(dumnc(i,k)*lcldm(i,k)-nc(i,k))/deltat
           end if

           effc(i,k) = (pgam(i,k)+3._rkind_comp)/lamc(i,k)/2._rkind_comp*1.e6_rkind_comp
           !assign output fields for shape here
           lamcrad(i,k)=lamc(i,k)
           pgamrad(i,k)=pgam(i,k)
           ! recalculate effective radius for constant number, in order to separate
           ! first and second indirect effects
           !======================================
           ! assume constant number of 10^8 kg-1


           dumnc(i,k)=1.e8_rkind_comp
           ! Pass in "false" adjust flag to prevent number from being changed within
           ! size distribution subroutine.
        endif
     enddo
      NEC_END("loop_#30")

     call size_dist_param_liq_vec(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), pgam(:,k), lamc(:,k),mgncol)

     do i=1,mgncol
        if (dumc(i,k).ge.qsmall) then

           effc_fn(i,k) = (pgam(i,k)+3._rkind_comp)/lamc(i,k)/2._rkind_comp*1.e6_rkind_comp

        else
           effc(i,k) = 10._rkind_comp
           lamcrad(i,k)=0._rkind_comp
           pgamrad(i,k)=0._rkind_comp
           effc_fn(i,k) = 10._rkind_comp
        end if
     enddo
  enddo
  ! recalculate 'final' rain size distribution parameters
  ! to ensure that rain size is in bounds, adjust rain number if needed
  dum_2D = dumnr
  call size_dist_param_basic_vec(mg_rain_props, dumr, dumnr, lamr,mgncol*nlev)
  do k=1,nlev
     !dum_2D(:,k) = dumnr(:,k)
     !call size_dist_param_basic_vec(mg_rain_props, dumr(:,k), dumnr(:,k), lamr(:,k),mgncol)
     do i=1,mgncol

        if (dumr(i,k).ge.qsmall) then


           if (dum_2D(i,k) /= dumnr(i,k)) then
              ! adjust number conc if needed to keep mean size in reasonable range
              nrtend(i,k)=(dumnr(i,k)*precip_frac(i,k)-nr(i,k))/deltat
           end if

        end if
     enddo
  enddo
  ! recalculate 'final' snow size distribution parameters
  ! to ensure that snow size is in bounds, adjust snow number if needed
  dum_2D = dumns
  call size_dist_param_basic_vec(mg_snow_props, dums, dumns, lams, mgncol*nlev, n0=dumns0A2D)
  do k=1,nlev
     ! dum_2D(:,k) = dumns(:,k)
     ! call size_dist_param_basic_vec(mg_snow_props, dums(:,k), dumns(:,k), lams(:,k), mgncol, n0=dumns0A)
     do i=1,mgncol
        if (dums(i,k).ge.qsmall) then


           if (dum_2D(i,k) /= dumns(i,k)) then
              ! adjust number conc if needed to keep mean size in reasonable range
              nstend(i,k)=(dumns(i,k)*precip_frac(i,k)-ns(i,k))/deltat
           end if

           sadsnow(i,k) = 2._rkind_comp*pi*(lams(i,k)**(-3))*dumns0A2D(i,k)*rho(i,k)*1.e-2_rkind_comp  ! m2/m3 -> cm2/cm3

        end if


     end do ! vertical k loop
  enddo
  do k=1,nlev
     do i=1,mgncol
        ! if updated q (after microphysics) is zero, then ensure updated n is also zero
        !=================================================================================
        if (qc(i,k)+qctend(i,k)*deltat.lt.qsmall) nctend(i,k)=-nc(i,k)/deltat
        if (do_cldice .and. qi(i,k)+qitend(i,k)*deltat.lt.qsmall) nitend(i,k)=-ni(i,k)/deltat
        if (qr(i,k)+qrtend(i,k)*deltat.lt.qsmall) nrtend(i,k)=-nr(i,k)/deltat
        if (qs(i,k)+qstend(i,k)*deltat.lt.qsmall) nstend(i,k)=-ns(i,k)/deltat

     end do
  end do
  ! DO STUFF FOR OUTPUT:
  !==================================================
  ! qc and qi are only used for output calculations past here,
  ! so add qctend and qitend back in one more time


  qc = qc + qctend*deltat
  qi = qi + qitend*deltat
  ! averaging for snow and rain number and diameter
  !--------------------------------------------------
  ! drout2/dsout2:
  ! diameter of rain and snow
  ! dsout:
  ! scaled diameter of snow (passed to radiation in CAM)
  ! reff_rain/reff_snow:
  ! calculate effective radius of rain and snow in microns for COSP using Eq. 9 of COSP v1.3 manual

!  print *,'where #1: count: ',count(qrout .gt. 1.e-7_rkind_comp .and. nrout.gt.0._rkind_comp), mgncol*nlev
  
  ! necessary to prevent divide by zero in aerage_diameter_vec

  ! avoid divide by zero in avg_diameter_vec
  where(nrout .eq. 0._rkind_comp) nrout=1e-34_rkind_comp
  call avg_diameter_vec(qrout,nrout,rho,rhow,drout2,mgncol*nlev)
  where (qrout .gt. 1.e-7_rkind_comp &
       .and. nrout.gt.0._rkind_comp)
     qrout2 = qrout * precip_frac
     nrout2 = nrout * precip_frac
     ! The avg_diameter call does the actual calculation; other diameter
     ! outputs are just drout2 times constants.
     ! drout2 = avg_diameter(qrout, nrout, rho, rhow)
     freqr = precip_frac

     reff_rain=1.5_rkind_comp*drout2*1.e6_rkind_comp
  elsewhere
     qrout2 = 0._rkind_comp
     nrout2 = 0._rkind_comp
     drout2 = 0._rkind_comp
     freqr = 0._rkind_comp
     reff_rain = 0._rkind_comp
  end where

  ! avoid divide by zero in avg_diameter_vec
  where(nsout .eq. 0._rkind_comp) nsout = 1.e-34_rkind_comp
  call avg_diameter_vec(qsout, nsout, rho, rhosn,dsout2,mgncol*nlev)
  where (qsout .gt. 1.e-7_rkind_comp &
       .and. nsout.gt.0._rkind_comp)
     qsout2 = qsout * precip_frac
     nsout2 = nsout * precip_frac
     ! The avg_diameter call does the actual calculation; other diameter
     ! outputs are just dsout2 times constants.
     ! dsout2 = avg_diameter(qsout, nsout, rho, rhosn)
     freqs = precip_frac

     dsout=3._rkind_comp*rhosn/rhows*dsout2

     reff_snow=1.5_rkind_comp*dsout2*1.e6_rkind_comp
  elsewhere
     dsout  = 0._rkind_comp
     qsout2 = 0._rkind_comp
     nsout2 = 0._rkind_comp
     dsout2 = 0._rkind_comp
     freqs  = 0._rkind_comp
     reff_snow=0._rkind_comp
  end where
  ! analytic radar reflectivity
  !--------------------------------------------------
  ! formulas from Matthew Shupe, NOAA/CERES
  ! *****note: radar reflectivity is local (in-precip average)
  ! units of mm^6/m^3


  !NEC_BEGIN("loop_#31")
  do k=1,nlev
     do i = 1,mgncol
        if (qc(i,k).ge.qsmall .and. (nc(i,k)+nctend(i,k)*deltat).gt.10._rkind_comp) then
           dum=(qc(i,k)/lcldm(i,k)*rho(i,k)*1000._rkind_comp)**2 &
                /(0.109_rkind_comp*(nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)/1.e6_rkind_comp)*lcldm(i,k)/precip_frac(i,k)
        else
           dum=0._rkind_comp
        end if
        if (qi(i,k).ge.qsmall) then
           dum1=(qi(i,k)*rho(i,k)/icldm(i,k)*1000._rkind_comp/0.1_rkind_comp)**(1._rkind_comp/0.63_rkind_comp)*icldm(i,k)/precip_frac(i,k)
        else
           dum1=0._rkind_comp
        end if

        if (qsout(i,k).ge.qsmall) then
           dum1=dum1+(qsout(i,k)*rho(i,k)*1000._rkind_comp/0.1_rkind_comp)**(1._rkind_comp/0.63_rkind_comp)
        end if

        refl(i,k)=dum+dum1
        ! add rain rate, but for 37 GHz formulation instead of 94 GHz
        ! formula approximated from data of Matrasov (2007)
        ! rainrt is the rain rate in mm/hr
        ! reflectivity (dum) is in DBz


        if (rainrt(i,k).ge.0.001_rkind_comp) then
           dum=log10(rainrt(i,k)**6._rkind_comp)+16._rkind_comp
           ! convert from DBz to mm^6/m^3


           dum = 10._rkind_comp**(dum/10._rkind_comp)
        else
           ! don't include rain rate in R calculation for values less than 0.001 mm/hr
           dum=0._rkind_comp
        end if
        ! add to refl


        refl(i,k)=refl(i,k)+dum
        !output reflectivity in Z.

        areflz(i,k)=refl(i,k) * precip_frac(i,k)
        ! convert back to DBz


        if (refl(i,k).gt.minrefl) then
           refl(i,k)=10._rkind_comp*log10(refl(i,k))
        else
           refl(i,k)=-9999._rkind_comp
        end if
        !set averaging flag

        if (refl(i,k).gt.mindbz) then
           arefl(i,k)=refl(i,k) * precip_frac(i,k)
           frefl(i,k)=precip_frac(i,k)
        else
           arefl(i,k)=0._rkind_comp
           areflz(i,k)=0._rkind_comp
           frefl(i,k)=0._rkind_comp
        end if
        ! bound cloudsat reflectivity


        csrfl(i,k)=min(csmax,refl(i,k))
        !set averaging flag

        if (csrfl(i,k).gt.csmin) then
           acsrfl(i,k)=refl(i,k) * precip_frac(i,k)
           fcsrfl(i,k)=precip_frac(i,k)
        else
           acsrfl(i,k)=0._rkind_comp
           fcsrfl(i,k)=0._rkind_comp
        end if

     end do
  end do
  !NEC_END("loop_#31")
  !redefine fice here....

  dum_2D = qsout + qrout + qc + qi
  dumi = qsout + qi
  where (dumi .gt. qsmall .and. dum_2D .gt. qsmall)
     nfice=min(dumi/dum_2D,1._rkind_comp)
  elsewhere
     nfice=0._rkind_comp
  end where


  !print *, "AFTER PARALLEL REGION"

end subroutine micro_mg_tend
!========================================================================
!OUTPUT CALCULATIONS
!========================================================================


subroutine calc_rercld(lamr, n0r, lamc, pgam, qric, qcic, ncic, rercld, mgncol)

  integer, intent(in) :: mgncol
  real(rkind_comp), dimension(mgncol), intent(in) :: lamr          ! rain size parameter (slope)
  real(rkind_comp), dimension(mgncol), intent(in) :: n0r           ! rain size parameter (intercept)
  real(rkind_comp), dimension(mgncol), intent(in) :: lamc          ! size distribution parameter (slope)
  real(rkind_comp), dimension(mgncol), intent(in) :: pgam          ! droplet size parameter
  real(rkind_comp), dimension(mgncol), intent(in) :: qric          ! in-cloud rain mass mixing ratio
  real(rkind_comp), dimension(mgncol), intent(in) :: qcic          ! in-cloud cloud liquid
  real(rkind_comp), dimension(mgncol), intent(in) :: ncic          ! in-cloud droplet number concentration

  real(rkind_comp), dimension(mgncol), intent(inout) :: rercld     ! effective radius calculation for rain + cloud
  ! combined size of precip & cloud drops

  real(rkind_comp) :: Atmp(mgncol),tmp(mgncol), pgamp1(mgncol)

  integer :: i

  pgamp1 = pgam+1._rkind_comp
  call rising_factorial(pgamp1, 2,tmp,mgncol)
  do i=1,mgncol
     ! Rain drops
     if (lamr(i) > 0._rkind_comp) then
        Atmp(i) = n0r(i) * pi / (2._rkind_comp * lamr(i)**3._rkind_comp)
     else
        Atmp(i) = 0._rkind_comp
     end if
     ! Add cloud drops

     if (lamc(i) > 0._rkind_comp) then
        Atmp(i) = Atmp(i) + &
             ncic(i) * pi * tmp(i)/(4._rkind_comp * lamc(i)**2._rkind_comp)
     end if

     if (Atmp(i) > 0._rkind_comp) then
        rercld(i) = rercld(i) + 3._rkind_comp *(qric(i) + qcic(i)) / (4._rkind_comp * rhow * Atmp(i))
     end if
  enddo
end subroutine calc_rercld
subroutine UpdateTendencies(mgncol,nlev,do_cldice,deltat,fx,fnx,pdelInv,pdel,qxtend,nxtend, &
                              qxsedten,dumx,dumnx,prect,xflx,xxlx,qxsevap,xcldm,tlat,qvlat,preci)

   integer, intent(in)               :: mgncol,nlev
   logical, intent(in)               :: do_cldice
   real(rkind_comp),intent(in)               :: deltat
   real(rkind_comp), intent(in)              :: fx(mgncol,nlev)
   real(rkind_comp), intent(in)              :: fnx(mgncol,nlev)
   real(rkind_comp), intent(in)              :: pdelInv(mgncol,nlev)
   real(rkind_comp), intent(in)              :: pdel(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: qxtend(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: nxtend(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: qxsedten(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: dumx(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: dumnx(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: prect(mgncol)
   real(rkind_comp), intent(inout)           :: xflx(mgncol,nlev+1)
   real(rkind_comp), intent(in)   , optional :: xxlx
   real(rkind_comp), intent(inout), optional :: qxsevap(mgncol,nlev)
   real(rkind_comp), intent(in)   , optional :: xcldm(mgncol,nlev)
   real(rkind_comp), intent(inout), optional :: tlat(mgncol,nlev)
   real(rkind_comp), intent(inout), optional :: qvlat(mgncol,nlev)
   real(rkind_comp), intent(inout), optional :: preci(mgncol)
   integer :: i,k,n,nstep
   real(rkind_comp) :: faltndx,faltndnx,rnstep,dum1,faltndqxe
   real(rkind_comp) :: faloutx(nlev),faloutnx(nlev)

   do i=1,mgncol
     nstep = 1 + int(max( &
          maxval( fx(i,:)*pdelInv(i,:)), &
          maxval(fnx(i,:)*pdelInv(i,:))) &
          * deltat)
     ! loop over sedimentation sub-time step to ensure stability
     !==============================================================
     rnstep = 1._rkind_comp/real(nstep)
     do n = 1,nstep

        if (do_cldice) then
           faloutx  = fx(i,:)  * dumx(i,:)
           faloutnx = fnx(i,:) * dumnx(i,:)
        else
           faloutx  = 0._rkind_comp
           faloutnx = 0._rkind_comp
        end if
        ! top of model


        k = 1
        ! add fallout terms to microphysical tendencies

        faltndx = faloutx(k)/pdel(i,k)
        faltndnx = faloutnx(k)/pdel(i,k)
        qxtend(i,k) = qxtend(i,k)-faltndx*rnstep
        nxtend(i,k) = nxtend(i,k)-faltndnx*rnstep
        ! sedimentation tendency for output

        qxsedten(i,k)=qxsedten(i,k)-faltndx*rnstep

        dumx(i,k)  = dumx(i,k)-faltndx*deltat*rnstep
        dumnx(i,k) = dumnx(i,k)-faltndnx*deltat*rnstep

        do k = 2,nlev
           ! for cloud liquid and ice, if cloud fraction increases with height
           ! then add flux from above to both vapor and cloud water of current level
           ! this means that flux entering clear portion of cell from above evaporates
           ! instantly
           ! note: this is not an issue with precip, since we assume max overlap


           if(present(xcldm)) then
              dum1=xcldm(i,k)/xcldm(i,k-1)
              dum1=min(dum1,1._rkind_comp)
           else
              dum1=1.0
           endif
           faltndqxe=(faloutx(k)-faloutx(k-1))/pdel(i,k)
           faltndx=(faloutx(k)-dum1*faloutx(k-1))/pdel(i,k)
           faltndnx=(faloutnx(k)-dum1*faloutnx(k-1))/pdel(i,k)
           ! add fallout terms to eulerian tendencies


           qxtend(i,k) = qxtend(i,k)-faltndx*rnstep
           nxtend(i,k) = nxtend(i,k)-faltndnx*rnstep
           ! sedimentation tendency for output

           qxsedten(i,k)=qxsedten(i,k)-faltndx*rnstep
           ! add terms to to evap/sub of cloud water


           ! for output
           if(present(qxsevap)) qxsevap(i,k)=qxsevap(i,k)-(faltndqxe-faltndx)*rnstep

           if(present(qvlat)) qvlat(i,k)=qvlat(i,k)-(faltndqxe-faltndx)*rnstep
           if(present(tlat))  tlat(i,k)=tlat(i,k)+(faltndqxe-faltndx)*xxlx*rnstep

           dumx(i,k) = dumx(i,k)-faltndx*deltat*rnstep
           dumnx(i,k) = dumnx(i,k)-faltndnx*deltat*rnstep

        end do
        do k = 1,nlev
          xflx(i,k+1) = xflx(i,k+1) + faloutx(k) / g * rnstep
        end do
        ! units below are m/s
        ! sedimentation flux at surface is added to precip flux at surface
        ! to get total precip (cloud + precip water) rate

        prect(i) = prect(i)+faloutx(nlev)/g*rnstep/1000._rkind_comp
        if(present(preci)) preci(i) = preci(i)+faloutx(nlev)/g*rnstep/1000._rkind_comp


     end do
     ! calculate number of split time steps to ensure courant stability criteria
     ! for sedimentation calculations
     !-------------------------------------------------------------------

  enddo

end subroutine UpdateTendencies

subroutine UpdateTendencies_vec(mgncol,nlev,do_cldice,deltat,fx,fnx,pdelInv,qxtend,nxtend, &
                              qxsedten,dumx,dumnx,prect,xflx,xxlx,qxsevap,xcldm,tlat,qvlat,preci)

   integer, intent(in)               :: mgncol,nlev
   logical, intent(in)               :: do_cldice
   real(rkind_comp),intent(in)               :: deltat
   real(rkind_comp), intent(in)              :: fx(mgncol,nlev)
   real(rkind_comp), intent(in)              :: fnx(mgncol,nlev)
   real(rkind_comp), intent(in)              :: pdelInv(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: qxtend(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: nxtend(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: qxsedten(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: dumx(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: dumnx(mgncol,nlev)
   real(rkind_comp), intent(inout)           :: prect(mgncol)
   real(rkind_comp), intent(inout)           :: xflx(mgncol,nlev+1)
   real(rkind_comp), intent(in)   , optional :: xxlx
   real(rkind_comp), intent(inout), optional :: qxsevap(mgncol,nlev)
   real(rkind_comp), intent(in)   , optional :: xcldm(mgncol,nlev)
   real(rkind_comp), intent(inout), optional :: tlat(mgncol,nlev)
   real(rkind_comp), intent(inout), optional :: qvlat(mgncol,nlev)
   real(rkind_comp), intent(inout), optional :: preci(mgncol)
   integer :: i,k,n,nstep,nstepMax
   real(rkind_comp) :: faltndx(mgncol),faltndnx(mgncol)
   real(rkind_comp) :: rnstep(mgncol),faltndqxe(mgncol)
   real(rkind_comp) :: faltndqxe2(mgncol)
   real(rkind_comp) :: faloutx(mgncol,nlev),faloutnx(mgncol,nlev),dum1(mgncol)
   real(rkind_comp) :: tmpk1(nlev),tmpk2(nlev)
   real(rkind_comp) :: tmp1(mgncol,nlev),tmp2(mgncol,nlev)
   real(rkind_comp) :: mask(mgncol)
   integer  :: iters(mgncol)
   logical  :: present_tlat,present_qvlat 


   tmp1 = fx*pdelInv*deltat
   tmp2 = fnx*pdelInv*deltat
   iters = 1 + max(maxval(tmp1,dim=2),maxval(tmp2,dim=2))
   nstepMax = maxval(iters)
   present_tlat  = present(tlat)
   present_qvlat = present(qvlat)
     ! loop over sedimentation sub-time step to ensure stability
     !==============================================================
     !rnstep = 1._rkind_comp/real(nstepMax)
     rnstep(:) = 1._rkind_comp/real(iters)
     do n = 1,nstepMax

        !---------------------------------------------
        ! mask out any additional changes to points 
        ! that should have already converged. 
        ! This code modification makes this 
        ! reproduces existing answer
        !---------------------------------------------
        mask=1._rkind_comp
        where(n>iters) 
          mask=0._rkind_comp
        end where 

        if (do_cldice) then
           do k=1,nlev
              faloutx(:,k)  = fx(:,k)  * dumx(:,k)  * mask
              faloutnx(:,k) = fnx(:,k) * dumnx(:,k) * mask
           enddo
        else
           faloutx  = 0._rkind_comp
           faloutnx = 0._rkind_comp
        end if

#if 0
        do i=1,mgncol
           if(n > iters(i)) then 
             faloutx(i,:) = 0._rkind_comp
             faloutnx(i,:) = 0._rkind_comp
           endif
        enddo
#endif

        ! top of model
        k = 1
        ! add fallout terms to microphysical tendencies

        faltndx = faloutx(:,k)*pdelInv(:,k)
        faltndnx = faloutnx(:,k)*pdelInv(:,k)
        qxtend(:,k) = qxtend(:,k)-faltndx*rnstep
        nxtend(:,k) = nxtend(:,k)-faltndnx*rnstep
        ! sedimentation tendency for output

        qxsedten(:,k)=qxsedten(:,k)-faltndx*rnstep

        dumx(:,k)  = dumx(:,k)-faltndx*deltat*rnstep
        dumnx(:,k) = dumnx(:,k)-faltndnx*deltat*rnstep

        do k = 2,nlev
           ! for cloud liquid and ice, if cloud fraction increases with height
           ! then add flux from above to both vapor and cloud water of current level
           ! this means that flux entering clear portion of cell from above evaporates
           ! instantly
           ! note: this is not an issue with precip, since we assume max overlap


           if(present(xcldm)) then
              dum1=xcldm(:,k)/xcldm(:,k-1)
              dum1=min(dum1,1._rkind_comp)
           else
              dum1=1.0
           endif
           faltndx(:)=(faloutx(:,k)-dum1(:)*faloutx(:,k-1))*pdelInv(:,k)
           faltndnx(:)=(faloutnx(:,k)-dum1(:)*faloutnx(:,k-1))*pdelInv(:,k)
           ! faltndqxe  = (faloutx(:,k)-faloutx(:,k-1))/pdel(:,k)
           !(faltndqxe-faltndx) == (faloutx(:,k) - faloutx(:,k-1) - faloutx(:,k)  + dum1*faloutx(:,k-1)  )/pdel(:,k)
           !(faltndqxe-faltndx) == ( (dum1-1.)*faloutx(:,k-1))/pdel(:,k)
           ! faltnqxe2 = 
           faltndqxe2(:) = (dum1(:)-1._rkind_comp)*faloutx(:,k-1)*pdelInv(:,k)
           ! add fallout terms to eulerian tendencies
            

           qxtend(:,k) = qxtend(:,k)-faltndx*rnstep
           nxtend(:,k) = nxtend(:,k)-faltndnx*rnstep
           ! sedimentation tendency for output

           qxsedten(:,k)=qxsedten(:,k)-faltndx*rnstep
           ! add terms to to evap/sub of cloud water

           ! for output
           if(present(qxsevap)) then 
              ! qxsevap(:,k)=qxsevap(:,k)-(faltndqxe-faltndx)*rnstep
              qxsevap(:,k)=qxsevap(:,k)- faltndqxe2*rnstep
           endif

           !if(present(qvlat)) then 
           if(present_qvlat) then 
              ! qvlat(:,k)=qvlat(:,k)-(faltndqxe-faltndx)*rnstep
              qvlat(:,k)=qvlat(:,k)-faltndqxe2*rnstep
           endif
           ! if(present(tlat)) then 
           if(present_tlat) then 
              ! tlat(:,k)=tlat(:,k)+(faltndqxe-faltndx)*xxlx*rnstep
              tlat(:,k)=tlat(:,k)+faltndqxe2*xxlx*rnstep
           endif

           dumx(:,k)  = dumx(:,k)-faltndx*deltat*rnstep
           dumnx(:,k) = dumnx(:,k)-faltndnx*deltat*rnstep

        end do
        do k = 1,nlev
           xflx(:,k+1) = xflx(:,k+1) + faloutx(:,k) / g * rnstep
        end do
        ! units below are m/s
        ! sedimentation flux at surface is added to precip flux at surface
        ! to get total precip (cloud + precip water) rate

        prect = prect+faloutx(:,nlev)/g*rnstep/1000._rkind_comp
        if(present(preci)) then 
          preci = preci+faloutx(:,nlev)/g*rnstep/1000._rkind_comp
        endif

!     print *,'preci: ',preci
     ! calculate number of split time steps to ensure courant stability criteria
     ! for sedimentation calculations
     !-------------------------------------------------------------------
     end do

!   print *,'qxtend: ',qxtend



end subroutine UpdateTendencies_vec

!========================================================================
!UTILITIES
!========================================================================


!read state subroutine for kr_externs_in_micro_mg2_0 
SUBROUTINE kr_externs_in_micro_mg2_0(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    real(kind=rkind_io) :: tmp
      
    READ (UNIT = kgen_unit) nccons 
!    print *,'kr_externs_in_micro_mg2_0: nccons: ',nccons
    READ (UNIT = kgen_unit) nicons 
!    print *,'kr_externs_in_micro_mg2_0: nicons: ',nicons
    READ (UNIT = kgen_unit) tmp; ncnst = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg2_0: ncnst: ',ncnst
    READ (UNIT = kgen_unit) tmp; ninst = real(tmp,kind=rkind_comp) 
!    print *,'kr_externs_in_micro_mg2_0: ninst: ',ninst
    READ (UNIT = kgen_unit) tmp; dcs   = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; g     = real(tmp,kind=rkind_comp) 
    READ (UNIT = kgen_unit) tmp; r     = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; rv    = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; cpp   = real(tmp,kind=rkind_comp) 
    READ (UNIT = kgen_unit) tmp; tmelt = real(tmp,kind=rkind_comp) 
    READ (UNIT = kgen_unit) tmp; xxlv  = real(tmp,kind=rkind_comp) 
    READ (UNIT = kgen_unit) tmp; xlf   = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; xxls  = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) microp_uniform 
    READ (UNIT = kgen_unit) do_cldice 
    READ (UNIT = kgen_unit) use_hetfrz_classnuc 
    READ (UNIT = kgen_unit) tmp; rhosu   = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; icenuct = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; snowmelt = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; rainfrze = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_br_plus1 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_br_plus4 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_bs_plus1 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_bs_plus4 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_bi_plus1 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_bi_plus4 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_bj_plus1 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; gamma_bj_plus4 = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; xxlv_squared   = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) tmp; xxls_squared   = real(tmp,kind=rkind_comp)
    READ (UNIT = kgen_unit) micro_mg_precip_frac_method 
    READ (UNIT = kgen_unit) tmp; micro_mg_berg_eff_factor = real(tmp,kind=rkind_comp) 
    READ (UNIT = kgen_unit) allow_sed_supersat 
    READ (UNIT = kgen_unit) do_sb_physics 
END SUBROUTINE kr_externs_in_micro_mg2_0 
  
!read state subroutine for kr_externs_out_micro_mg2_0 
SUBROUTINE kr_externs_out_micro_mg2_0(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_micro_mg2_0 
  
end module micro_mg2_0
