!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  
#define NEC_BEGIN(x) !call ftrace_region_begin(x)
#define NEC_END(x) !call ftrace_region_end(x)



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


#if defined(_NEC)
#else
    USE shr_spfn_mod, ONLY: gamma => shr_spfn_gamma 
#endif

    USE wv_sat_methods, ONLY: qsat_water => wv_sat_qsat_water, qsat_ice => wv_sat_qsat_ice 
! Parameters from the utilities module.

    USE micro_mg_utils, ONLY: r8, pi, omsm, qsmall, mincld, rhosn, rhoi, rhow, rhows, bc, ai, bi, aj, bj, ar, br, as, bs, mi0, &
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

real(r8) :: ncnst  ! droplet num concentration when nccons=.true. (m-3)
real(r8) :: ninst  ! ice num concentration when nicons=.true. (m-3)
!=========================================================
! Private module parameters
!=========================================================
!Range of cloudsat reflectivities (dBz) for analytic simulator


real(r8), parameter :: csmin = -30._r8
real(r8), parameter :: csmax = 26._r8
real(r8), parameter :: mindbz = -99._r8
real(r8), parameter :: minrefl = 1.26e-10_r8    ! minrefl = 10._r8**(mindbz/10._r8)
! autoconversion size threshold for cloud ice to snow (m)

integer, parameter ::   MG_PRECIP_FRAC_INCLOUD = 101
integer, parameter ::   MG_PRECIP_FRAC_OVERLAP = 102

real(r8) :: dcs
! minimum mass of new crystal due to freezing of cloud droplets done
! externally (kg)

real(r8), parameter :: mi0l_min = 4._r8/3._r8*pi*rhow*(4.e-6_r8)**3
! Ice number sublimation parameter. Assume some decrease in ice number with sublimation if non-zero. Else, no decrease in number with sublimation. 

  real(r8), parameter :: sublim_factor =0.0_r8      !number sublimation factor.  
!=========================================================
! Constants set in initialization
!=========================================================
! Set using arguments to micro_mg_init


real(r8) :: g           ! gravity
real(r8) :: r           ! dry air gas constant
real(r8) :: rv          ! water vapor gas constant
real(r8) :: cpp         ! specific heat of dry air
real(r8) :: tmelt       ! freezing point of water (K)
! latent heats of:

real(r8) :: xxlv        ! vaporization
real(r8) :: xlf         ! freezing
real(r8) :: xxls        ! sublimation

! flags

logical :: microp_uniform
logical :: do_cldice
logical :: use_hetfrz_classnuc

real(r8) :: rhosu       ! typical 850mn air density

real(r8) :: icenuct     ! ice nucleation temperature: currently -5 degrees C

real(r8) :: snowmelt    ! what temp to melt all snow: currently 2 degrees C
real(r8) :: rainfrze    ! what temp to freeze all rain: currently -5 degrees C
! additional constants to help speed up code

real(r8) :: gamma_br_plus1
real(r8) :: gamma_br_plus4
real(r8) :: gamma_bs_plus1
real(r8) :: gamma_bs_plus4
real(r8) :: gamma_bi_plus1
real(r8) :: gamma_bi_plus4
real(r8) :: gamma_bj_plus1
real(r8) :: gamma_bj_plus4
real(r8) :: xxlv_squared
real(r8) :: xxls_squared

character(len=16)  :: micro_mg_precip_frac_method  ! type of precipitation fraction method
real(r8)           :: micro_mg_berg_eff_factor     ! berg efficiency factor

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

    USE micro_mg_utils, ONLY: size_dist_param_liq, size_dist_param_basic, avg_diameter, avg_diameter_vec
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
  real(r8), intent(in) :: deltatin       ! time step (s)
  real(r8), intent(in) :: t(mgncol,nlev) ! input temperature (K)
  real(r8), intent(in) :: q(mgncol,nlev) ! input h20 vapor mixing ratio (kg/kg)
  ! note: all input cloud variables are grid-averaged

  real(r8), intent(in) :: qcn(mgncol,nlev)       ! cloud water mixing ratio (kg/kg)
  real(r8), intent(in) :: qin(mgncol,nlev)       ! cloud ice mixing ratio (kg/kg)
  real(r8), intent(in) :: ncn(mgncol,nlev)       ! cloud water number conc (1/kg)
  real(r8), intent(in) :: nin(mgncol,nlev)       ! cloud ice number conc (1/kg)

  real(r8), intent(in) :: qrn(mgncol,nlev)       ! rain mixing ratio (kg/kg)
  real(r8), intent(in) :: qsn(mgncol,nlev)       ! snow mixing ratio (kg/kg)
  real(r8), intent(in) :: nrn(mgncol,nlev)       ! rain number conc (1/kg)
  real(r8), intent(in) :: nsn(mgncol,nlev)       ! snow number conc (1/kg)

  real(r8), intent(in) :: relvar(mgncol,nlev)      ! cloud water relative variance (-)
  real(r8), intent(in) :: accre_enhan(mgncol,nlev) ! optional accretion
                                             ! enhancement factor (-)

  real(r8), intent(in) :: p(mgncol,nlev)        ! air pressure (pa)
  real(r8), intent(in) :: pdel(mgncol,nlev)     ! pressure difference across level (pa)

  real(r8), intent(in) :: cldn(mgncol,nlev)      ! cloud fraction (no units)
  real(r8), intent(in) :: liqcldf(mgncol,nlev)   ! liquid cloud fraction (no units)
  real(r8), intent(in) :: icecldf(mgncol,nlev)   ! ice cloud fraction (no units)
  real(r8), intent(in) :: qsatfac(mgncol,nlev)   ! subgrid cloud water saturation scaling factor (no units)
  ! used for scavenging
  ! Inputs for aerosol activation

  real(r8), intent(in) :: naai(mgncol,nlev)     ! ice nucleation number (from microp_aero_ts) (1/kg)
  real(r8), intent(in) :: npccn(mgncol,nlev)   ! ccn activated number tendency (from microp_aero_ts) (1/kg*s)
  ! Note that for these variables, the dust bin is assumed to be the last index.
  ! (For example, in CAM, the last dimension is always size 4.)

  real(r8), intent(in) :: rndst(:,:,:)  ! radius of each dust bin, for contact freezing (from microp_aero_ts) (m)
  real(r8), intent(in) :: nacon(:,:,:) ! number in each dust bin, for contact freezing  (from microp_aero_ts) (1/m^3)
  ! output arguments
  

  real(r8), intent(out) :: qcsinksum_rate1ord(mgncol,nlev) ! 1st order rate for
  ! direct cw to precip conversion
  real(r8), intent(out) :: tlat(mgncol,nlev)         ! latent heating rate       (W/kg)
  real(r8), intent(out) :: qvlat(mgncol,nlev)        ! microphysical tendency qv (1/s)
  real(r8), intent(out) :: qctend(mgncol,nlev)       ! microphysical tendency qc (1/s)
  real(r8), intent(out) :: qitend(mgncol,nlev)       ! microphysical tendency qi (1/s)
  real(r8), intent(out) :: nctend(mgncol,nlev)       ! microphysical tendency nc (1/(kg*s))
  real(r8), intent(out) :: nitend(mgncol,nlev)       ! microphysical tendency ni (1/(kg*s))

  real(r8), intent(out) :: qrtend(mgncol,nlev)       ! microphysical tendency qr (1/s)
  real(r8), intent(out) :: qstend(mgncol,nlev)       ! microphysical tendency qs (1/s)
  real(r8), intent(out) :: nrtend(mgncol,nlev)       ! microphysical tendency nr (1/(kg*s))
  real(r8), intent(out) :: nstend(mgncol,nlev)       ! microphysical tendency ns (1/(kg*s))
  real(r8), intent(out) :: effc(mgncol,nlev)         ! droplet effective radius (micron)
  real(r8), intent(out) :: effc_fn(mgncol,nlev)      ! droplet effective radius, assuming nc = 1.e8 kg-1
  real(r8), intent(out) :: effi(mgncol,nlev)         ! cloud ice effective radius (micron)
  real(r8), intent(out) :: sadice(mgncol,nlev)       ! cloud ice surface area density (cm2/cm3)
  real(r8), intent(out) :: sadsnow(mgncol,nlev)      ! cloud snow surface area density (cm2/cm3)
  real(r8), intent(out) :: prect(mgncol)             ! surface precip rate (m/s)
  real(r8), intent(out) :: preci(mgncol)             ! cloud ice/snow precip rate (m/s)
  real(r8), intent(out) :: nevapr(mgncol,nlev)       ! evaporation rate of rain + snow (1/s)
  real(r8), intent(out) :: evapsnow(mgncol,nlev)     ! sublimation rate of snow (1/s)
  real(r8), intent(out) :: am_evp_st(mgncol,nlev)    ! stratiform evaporation area (frac)
  real(r8), intent(out) :: prain(mgncol,nlev)        ! production of rain + snow (1/s)
  real(r8), intent(out) :: prodsnow(mgncol,nlev)     ! production of snow (1/s)
  real(r8), intent(out) :: cmeout(mgncol,nlev)       ! evap/sub of cloud (1/s)
  real(r8), intent(out) :: deffi(mgncol,nlev)        ! ice effective diameter for optics (radiation) (micron)
  real(r8), intent(out) :: pgamrad(mgncol,nlev)      ! ice gamma parameter for optics (radiation) (no units)
  real(r8), intent(out) :: lamcrad(mgncol,nlev)      ! slope of droplet distribution for optics (radiation) (1/m)
  real(r8), intent(out) :: qsout(mgncol,nlev)        ! snow mixing ratio (kg/kg)
  real(r8), intent(out) :: dsout(mgncol,nlev)        ! snow diameter (m)
  real(r8), intent(out) :: lflx(mgncol,nlev+1)       ! grid-box average liquid condensate flux (kg m^-2 s^-1)
  real(r8), intent(out) :: iflx(mgncol,nlev+1)       ! grid-box average ice condensate flux (kg m^-2 s^-1)
  real(r8), intent(out) :: rflx(mgncol,nlev+1)       ! grid-box average rain flux (kg m^-2 s^-1)
  real(r8), intent(out) :: sflx(mgncol,nlev+1)       ! grid-box average snow flux (kg m^-2 s^-1)
  real(r8), intent(out) :: qrout(mgncol,nlev)        ! grid-box average rain mixing ratio (kg/kg)
  real(r8), intent(out) :: reff_rain(mgncol,nlev)    ! rain effective radius (micron)
  real(r8), intent(out) :: reff_snow(mgncol,nlev)    ! snow effective radius (micron)
  real(r8), intent(out) :: qcsevap(mgncol,nlev)      ! cloud water evaporation due to sedimentation (1/s)
  real(r8), intent(out) :: qisevap(mgncol,nlev)      ! cloud ice sublimation due to sublimation (1/s)
  real(r8), intent(out) :: qvres(mgncol,nlev)        ! residual condensation term to ensure RH < 100% (1/s)
  real(r8), intent(out) :: cmeitot(mgncol,nlev)      ! grid-mean cloud ice sub/dep (1/s)
  real(r8), intent(out) :: vtrmc(mgncol,nlev)        ! mass-weighted cloud water fallspeed (m/s)
  real(r8), intent(out) :: vtrmi(mgncol,nlev)        ! mass-weighted cloud ice fallspeed (m/s)
  real(r8), intent(out) :: umr(mgncol,nlev)          ! mass weighted rain fallspeed (m/s)
  real(r8), intent(out) :: ums(mgncol,nlev)          ! mass weighted snow fallspeed (m/s)
  real(r8), intent(out) :: qcsedten(mgncol,nlev)     ! qc sedimentation tendency (1/s)
  real(r8), intent(out) :: qisedten(mgncol,nlev)     ! qi sedimentation tendency (1/s)
  real(r8), intent(out) :: qrsedten(mgncol,nlev)     ! qr sedimentation tendency (1/s)
  real(r8), intent(out) :: qssedten(mgncol,nlev)     ! qs sedimentation tendency (1/s)
  ! microphysical process rates for output (mixing ratio tendencies) (all have units of 1/s)

  real(r8), intent(out) :: pratot(mgncol,nlev)          ! accretion of cloud by rain
  real(r8), intent(out) :: prctot(mgncol,nlev)          ! autoconversion of cloud to rain
  real(r8), intent(out) :: mnuccctot(mgncol,nlev)       ! mixing ratio tend due to immersion freezing
  real(r8), intent(out) :: mnuccttot(mgncol,nlev)       ! mixing ratio tend due to contact freezing
  real(r8), intent(out) :: msacwitot(mgncol,nlev)       ! mixing ratio tend due to H-M splintering
  real(r8), intent(out) :: psacwstot(mgncol,nlev)       ! collection of cloud water by snow
  real(r8), intent(out) :: bergstot(mgncol,nlev)        ! bergeron process on snow
  real(r8), intent(out) :: bergtot(mgncol,nlev)         ! bergeron process on cloud ice
  real(r8), intent(out) :: melttot(mgncol,nlev)         ! melting of cloud ice
  real(r8), intent(out) :: homotot(mgncol,nlev)         ! homogeneous freezing cloud water
  real(r8), intent(out) :: qcrestot(mgncol,nlev)        ! residual cloud condensation due to removal of excess supersat
  real(r8), intent(out) :: prcitot(mgncol,nlev)         ! autoconversion of cloud ice to snow
  real(r8), intent(out) :: praitot(mgncol,nlev)         ! accretion of cloud ice by snow
  real(r8), intent(out) :: qirestot(mgncol,nlev)        ! residual ice deposition due to removal of excess supersat
  real(r8), intent(out) :: mnuccrtot(mgncol,nlev)       ! mixing ratio tendency due to heterogeneous freezing of rain to snow (1/s)
  real(r8), intent(out) :: pracstot(mgncol,nlev)        ! mixing ratio tendency due to accretion of rain by snow (1/s)
  real(r8), intent(out) :: meltsdttot(mgncol,nlev)      ! latent heating rate due to melting of snow  (W/kg)
  real(r8), intent(out) :: frzrdttot(mgncol,nlev)       ! latent heating rate due to homogeneous freezing of rain (W/kg)
  real(r8), intent(out) :: mnuccdtot(mgncol,nlev)       ! mass tendency from ice nucleation
  real(r8), intent(out) :: nrout(mgncol,nlev)        ! rain number concentration (1/m3)
  real(r8), intent(out) :: nsout(mgncol,nlev)        ! snow number concentration (1/m3)
  real(r8), intent(out) :: refl(mgncol,nlev)         ! analytic radar reflectivity
  real(r8), intent(out) :: arefl(mgncol,nlev)        ! average reflectivity will zero points outside valid range
  real(r8), intent(out) :: areflz(mgncol,nlev)       ! average reflectivity in z.
  real(r8), intent(out) :: frefl(mgncol,nlev)        ! fractional occurrence of radar reflectivity
  real(r8), intent(out) :: csrfl(mgncol,nlev)        ! cloudsat reflectivity
  real(r8), intent(out) :: acsrfl(mgncol,nlev)       ! cloudsat average
  real(r8), intent(out) :: fcsrfl(mgncol,nlev)       ! cloudsat fractional occurrence of radar reflectivity
  real(r8), intent(out) :: rercld(mgncol,nlev)       ! effective radius calculation for rain + cloud
  real(r8), intent(out) :: ncai(mgncol,nlev)         ! output number conc of ice nuclei available (1/m3)
  real(r8), intent(out) :: ncal(mgncol,nlev)         ! output number conc of CCN (1/m3)
  real(r8), intent(out) :: qrout2(mgncol,nlev)       ! copy of qrout as used to compute drout2
  real(r8), intent(out) :: qsout2(mgncol,nlev)       ! copy of qsout as used to compute dsout2
  real(r8), intent(out) :: nrout2(mgncol,nlev)       ! copy of nrout as used to compute drout2
  real(r8), intent(out) :: nsout2(mgncol,nlev)       ! copy of nsout as used to compute dsout2
  real(r8), intent(out) :: drout2(mgncol,nlev)       ! mean rain particle diameter (m)
  real(r8), intent(out) :: dsout2(mgncol,nlev)       ! mean snow particle diameter (m)
  real(r8), intent(out) :: freqs(mgncol,nlev)        ! fractional occurrence of snow
  real(r8), intent(out) :: freqr(mgncol,nlev)        ! fractional occurrence of rain
  real(r8), intent(out) :: nfice(mgncol,nlev)        ! fractional occurrence of ice
  real(r8), intent(out) :: qcrat(mgncol,nlev)        ! limiter for qc process rates (1=no limit --> 0. no qc)

  real(r8), intent(out) :: prer_evap(mgncol,nlev)

  character(128),   intent(out) :: errstring  ! output status (non-blank for error return)
  ! Tendencies calculated by external schemes that can replace MG's native
  ! process tendencies.
  ! Used with CARMA cirrus microphysics
  ! (or similar external microphysics model)


  real(r8), intent(in) :: tnd_qsnow(:,:) ! snow mass tendency (kg/kg/s)
  real(r8), intent(in) :: tnd_nsnow(:,:) ! snow number tendency (#/kg/s)
  real(r8), intent(in) :: re_ice(:,:)    ! ice effective radius (m)
  ! From external ice nucleation.

  real(r8), intent(in) :: frzimm(:,:) ! Number tendency due to immersion freezing (1/cm3)
  real(r8), intent(in) :: frzcnt(:,:) ! Number tendency due to contact freezing (1/cm3)
  real(r8), intent(in) :: frzdep(:,:) ! Number tendency due to deposition nucleation (1/cm3)
  ! local workspace
  ! all units mks unless otherwise stated
  ! local copies of input variables


  real(r8) :: qc(mgncol,nlev)      ! cloud liquid mixing ratio (kg/kg)
  real(r8) :: qi(mgncol,nlev)      ! cloud ice mixing ratio (kg/kg)
  real(r8) :: nc(mgncol,nlev)      ! cloud liquid number concentration (1/kg)
  real(r8) :: ni(mgncol,nlev)      ! cloud liquid number concentration (1/kg)
  real(r8) :: qr(mgncol,nlev)      ! rain mixing ratio (kg/kg)
  real(r8) :: qs(mgncol,nlev)      ! snow mixing ratio (kg/kg)
  real(r8) :: nr(mgncol,nlev)      ! rain number concentration (1/kg)
  real(r8) :: ns(mgncol,nlev)      ! snow number concentration (1/kg)
  ! general purpose variables

  real(r8) :: deltat            ! sub-time step (s)
  real(r8) :: mtime             ! the assumed ice nucleation timescale
  ! physical properties of the air at a given point

  real(r8) :: rho(mgncol,nlev)    ! density (kg m-3)
  real(r8) :: dv(mgncol,nlev)     ! diffusivity of water vapor
  real(r8) :: mu(mgncol,nlev)     ! viscosity
  real(r8) :: sc(mgncol,nlev)     ! schmidt number
  real(r8) :: rhof(mgncol,nlev)   ! density correction factor for fallspeed
  ! cloud fractions

  real(r8) :: precip_frac(mgncol,nlev) ! precip fraction assuming maximum overlap
  real(r8) :: cldm(mgncol,nlev)   ! cloud fraction
  real(r8) :: icldm(mgncol,nlev)  ! ice cloud fraction
  real(r8) :: lcldm(mgncol,nlev)  ! liq cloud fraction
  real(r8) :: qsfm(mgncol,nlev)   ! subgrid cloud water saturation scaling factor
  ! mass mixing ratios

  real(r8) :: qcic(mgncol,nlev)   ! in-cloud cloud liquid
  real(r8) :: qiic(mgncol,nlev)   ! in-cloud cloud ice
  real(r8) :: qsic(mgncol,nlev)   ! in-precip snow
  real(r8) :: qric(mgncol,nlev)   ! in-precip rain
  ! number concentrations

  real(r8) :: ncic(mgncol,nlev)   ! in-cloud droplet
  real(r8) :: niic(mgncol,nlev)   ! in-cloud cloud ice
  real(r8) :: nsic(mgncol,nlev)   ! in-precip snow
  real(r8) :: nric(mgncol,nlev)   ! in-precip rain
  ! maximum allowed ni value
  real(r8) :: nimax(mgncol,nlev)
  ! Size distribution parameters for:
  ! cloud ice

  real(r8) :: lami(mgncol,nlev)   ! slope
  real(r8) :: n0i(mgncol,nlev)    ! intercept
  ! cloud liquid
  real(r8) :: lamc(mgncol,nlev)   ! slope
  real(r8) :: pgam(mgncol,nlev)   ! spectral width parameter
  ! snow
  real(r8) :: lams(mgncol,nlev)   ! slope
  real(r8) :: n0s(mgncol,nlev)    ! intercept
  ! rain
  real(r8) :: lamr(mgncol,nlev)   ! slope
  real(r8) :: n0r(mgncol,nlev)    ! intercept
  ! Rates/tendencies due to:
  ! Instantaneous snow melting


  real(r8) :: minstsm(mgncol,nlev)    ! mass mixing ratio
  real(r8) :: ninstsm(mgncol,nlev)    ! number concentration
  ! Instantaneous rain freezing
  real(r8) :: minstrf(mgncol,nlev)    ! mass mixing ratio
  real(r8) :: ninstrf(mgncol,nlev)    ! number concentration
  ! deposition of cloud ice

  real(r8) :: vap_dep(mgncol,nlev)    ! deposition from vapor to ice PMC 12/3/12
  ! sublimation of cloud ice
  real(r8) :: ice_sublim(mgncol,nlev) ! sublimation from ice to vapor PMC 12/3/12
  ! ice nucleation
  real(r8) :: nnuccd(mgncol,nlev) ! number rate from deposition/cond.-freezing
  real(r8) :: mnuccd(mgncol,nlev) ! mass mixing ratio
  ! freezing of cloud water
  real(r8) :: mnuccc(mgncol,nlev) ! mass mixing ratio
  real(r8) :: nnuccc(mgncol,nlev) ! number concentration
  ! contact freezing of cloud water
  real(r8) :: mnucct(mgncol,nlev) ! mass mixing ratio
  real(r8) :: nnucct(mgncol,nlev) ! number concentration
  ! deposition nucleation in mixed-phase clouds (from external scheme)
  real(r8) :: mnudep(mgncol,nlev) ! mass mixing ratio
  real(r8) :: nnudep(mgncol,nlev) ! number concentration
  ! ice multiplication
  real(r8) :: msacwi(mgncol,nlev) ! mass mixing ratio
  real(r8) :: nsacwi(mgncol,nlev) ! number concentration
  ! autoconversion of cloud droplets
  real(r8) :: prc(mgncol,nlev)    ! mass mixing ratio
  real(r8) :: nprc(mgncol,nlev)   ! number concentration (rain)
  real(r8) :: nprc1(mgncol,nlev)  ! number concentration (cloud droplets)
  ! self-aggregation of snow
  real(r8) :: nsagg(mgncol,nlev)  ! number concentration
  ! self-collection of rain
  real(r8) :: nragg(mgncol,nlev)  ! number concentration
  ! collection of droplets by snow
  real(r8) :: psacws(mgncol,nlev)     ! mass mixing ratio
  real(r8) :: npsacws(mgncol,nlev)    ! number concentration
  ! collection of rain by snow
  real(r8) :: pracs(mgncol,nlev)  ! mass mixing ratio
  real(r8) :: npracs(mgncol,nlev) ! number concentration
  ! freezing of rain
  real(r8) :: mnuccr(mgncol,nlev) ! mass mixing ratio
  real(r8) :: nnuccr(mgncol,nlev) ! number concentration
  ! freezing of rain to form ice (mg add 4/26/13)
  real(r8) :: mnuccri(mgncol,nlev)    ! mass mixing ratio
  real(r8) :: nnuccri(mgncol,nlev)    ! number concentration
  ! accretion of droplets by rain
  real(r8) :: pra(mgncol,nlev)    ! mass mixing ratio
  real(r8) :: npra(mgncol,nlev)   ! number concentration
  ! autoconversion of cloud ice to snow
  real(r8) :: prci(mgncol,nlev)   ! mass mixing ratio
  real(r8) :: nprci(mgncol,nlev)  ! number concentration
  ! accretion of cloud ice by snow
  real(r8) :: prai(mgncol,nlev)   ! mass mixing ratio
  real(r8) :: nprai(mgncol,nlev)  ! number concentration
  ! evaporation of rain
  real(r8) :: pre(mgncol,nlev)    ! mass mixing ratio
  ! sublimation of snow
  real(r8) :: prds(mgncol,nlev)   ! mass mixing ratio
  ! number evaporation
  real(r8) :: nsubi(mgncol,nlev)  ! cloud ice
  real(r8) :: nsubc(mgncol,nlev)  ! droplet
  real(r8) :: nsubs(mgncol,nlev)  ! snow
  real(r8) :: nsubr(mgncol,nlev)  ! rain
  ! bergeron process
  real(r8) :: berg(mgncol,nlev)   ! mass mixing ratio (cloud ice)
  real(r8) :: bergs(mgncol,nlev)  ! mass mixing ratio (snow)
  ! fallspeeds
  ! number-weighted
 
  real(r8) :: uns(mgncol,nlev)    ! snow
  real(r8) :: unr(mgncol,nlev)    ! rain
  ! air density corrected fallspeed parameters
  real(r8) :: arn(mgncol,nlev)    ! rain
  real(r8) :: asn(mgncol,nlev)    ! snow
  real(r8) :: acn(mgncol,nlev)    ! cloud droplet
  real(r8) :: ain(mgncol,nlev)    ! cloud ice
  real(r8) :: ajn(mgncol,nlev)    ! cloud small ice
  ! Mass of liquid droplets used with external heterogeneous freezing.

  real(r8) :: mi0l(mgncol)
  ! saturation vapor pressures

  real(r8) :: esl(mgncol,nlev)    ! liquid
  real(r8) :: esi(mgncol,nlev)    ! ice
  real(r8) :: esn, esnA(mgncol)   ! checking for RH after rain evap
  
  ! saturation vapor mixing ratios

  real(r8) :: qvl(mgncol,nlev)    ! liquid
  real(r8) :: qvi(mgncol,nlev)    ! ice
  real(r8) :: qvn, qvnA(mgncol),qvnAI(mgncol)   ! checking for RH after rain evap
  ! relative humidity

  real(r8) :: relhum(mgncol,nlev)
  ! parameters for cloud water and cloud ice sedimentation calculations

  real(r8) :: fc(mgncol,nlev)
  real(r8) :: fnc(mgncol,nlev)
  real(r8) :: fi(mgncol,nlev)
  real(r8) :: fni(mgncol,nlev)

  real(r8) :: fr(mgncol,nlev)
  real(r8) :: fnr(mgncol,nlev)
  real(r8) :: fs(mgncol,nlev)
  real(r8) :: fns(mgncol,nlev)

  real(r8) :: faloutc(nlev)
  real(r8) :: faloutc2D(mgncol,nlev)
  real(r8) :: faloutnc(nlev)
  real(r8) :: faloutnc2D(mgncol,nlev)
  real(r8) :: falouti(nlev)
  real(r8) :: falouti2D(mgncol,nlev)
  real(r8) :: faloutni(nlev)
  real(r8) :: faloutni2D(mgncol,nlev)

  real(r8) :: faloutr(nlev)
  real(r8) :: faloutr2D(mgncol,nlev)
  real(r8) :: faloutnr(nlev)
  real(r8) :: faloutnr2D(mgncol,nlev)
  real(r8) :: falouts(nlev)
  real(r8) :: falouts2D(mgncol,nlev)
  real(r8) :: faloutns(nlev)
  real(r8) :: faloutns2D(mgncol,nlev)

  real(r8) :: faltndc
  real(r8) :: faltndnc
  real(r8) :: faltndi
  real(r8) :: faltndni
  real(r8) :: faltndni1D(mgncol)
  real(r8) :: faltndqie
  real(r8) :: faltndqce

  real(r8) :: faltndr
  real(r8) :: faltndnr
  real(r8) :: faltnds
  real(r8) :: faltndns

  real(r8) :: rainrt(mgncol,nlev)     ! rain rate for reflectivity calculation
  ! dummy variables
  real(r8) :: tmpk1(nlev),tmpk2(nlev)

  real(r8) :: dum
  real(r8) :: dum1
  real(r8) :: dum2
  real(r8) :: dum1A(mgncol), dum2A(mgncol)
   
  real(r8) :: dumni0, dumni0A(mgncol)
  real(r8) :: dumns0, dumns0A(mgncol)
  ! dummies for checking RH
  real(r8) :: qtmp
  real(r8) :: ttmp
  real(r8) :: qtmpA(mgncol), ttmpA(mgncol)
  ! dummies for conservation check
  real(r8) :: ratio
  real(r8) :: tmpfrz
  ! dummies for in-cloud variables
  real(r8) :: dumc(mgncol,nlev)   ! qc
  real(r8) :: dumnc(mgncol,nlev)  ! nc
  real(r8) :: dumi(mgncol,nlev)   ! qi
  real(r8) :: dumni(mgncol,nlev)  ! ni
  real(r8) :: dumr(mgncol,nlev)   ! rain mixing ratio
  real(r8) :: dumnr(mgncol,nlev)  ! rain number concentration
  real(r8) :: dums(mgncol,nlev)   ! snow mixing ratio
  real(r8) :: dumns(mgncol,nlev)  ! snow number concentration
  ! Array dummy variable
  real(r8) :: dum_2D(mgncol,nlev)
  real(r8) :: pdel_inv(mgncol,nlev)
  ! loop array variables
  ! "i" and "k" are column/level iterators for internal (MG) variables
  ! "n" is used for other looping (currently just sedimentation)

  integer i, k, n
  ! number of sub-steps for loops over "n" (for sedimentation)

  integer nstep,nstepMax
  integer mdust
  integer :: precip_frac_method
  ! Varaibles to scale fall velocity between small and regular ice regimes.

  real(r8) :: irad
  real(r8) :: ifrac
  real(r8) :: rnstep

  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Return error message


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
        lcldm = 1._r8
     elsewhere
        lcldm = mincld
     end where

     where (qi >= qsmall)
        icldm = 1._r8
     elsewhere
        icldm = mincld
     end where

     cldm = max(icldm, lcldm)
     qsfm = 1._r8

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
  dv = 8.794E-5_r8 * t**1.81_r8 / p
  mu = 1.496E-6_r8 * t**1.5_r8 / (t + 120._r8)
  sc = mu/(rho*dv)
  ! air density adjustment for fallspeed parameters
  ! includes air density correction factor to the
  ! power of 0.54 following Heymsfield and Bansemer 2007


  rhof=(rhosu/rho)**0.54_r8

  arn=ar*rhof
  asn=as*rhof
  acn=g*rhow/(18._r8*mu)
  ain=ai*(rhosu/rho)**0.35_r8
  ajn=aj*(rhosu/rho)**0.35_r8
  !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Get humidity and saturation vapor pressures



  call qsat_water(t, p, esl, qvl,mgncol*nlev)
  call qsat_ice(t, p, esi, qvi,mgncol*nlev)
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

  relhum = q / max(qvl, qsmall)
  !===============================================
  ! set mtime here to avoid answer-changing


  mtime=deltat
  ! initialize microphysics output

  NEC_BEGIN("initialize_to_zero")
  qcsevap=0._r8
  qisevap=0._r8
  qvres  =0._r8
  cmeitot =0._r8
  vtrmc =0._r8
  vtrmi =0._r8
  qcsedten =0._r8
  qisedten =0._r8
  qrsedten =0._r8
  qssedten =0._r8

  pratot=0._r8
  prctot=0._r8
  mnuccctot=0._r8
  mnuccttot=0._r8
  msacwitot=0._r8
  psacwstot=0._r8
  bergstot=0._r8
  bergtot=0._r8
  melttot=0._r8
  homotot=0._r8
  qcrestot=0._r8
  prcitot=0._r8
  praitot=0._r8
  qirestot=0._r8
  mnuccrtot=0._r8
  pracstot=0._r8
  meltsdttot=0._r8
  frzrdttot=0._r8
  mnuccdtot=0._r8

  rflx=0._r8
  sflx=0._r8
  lflx=0._r8
  iflx=0._r8
  ! initialize precip output


  qrout=0._r8
  qsout=0._r8
  nrout=0._r8
  nsout=0._r8
  ! for refl calc

  rainrt = 0._r8
  ! initialize rain size

  rercld=0._r8

  qcsinksum_rate1ord = 0._r8
  ! initialize variables for trop_mozart

  nevapr = 0._r8
  prer_evap = 0._r8
  evapsnow = 0._r8
  am_evp_st = 0._r8
  prain = 0._r8
  prodsnow = 0._r8
  cmeout = 0._r8

  precip_frac = mincld

  lamc=0._r8
  ! initialize microphysical tendencies


  tlat=0._r8
  qvlat=0._r8
  qctend=0._r8
  qitend=0._r8
  qstend = 0._r8
  qrtend = 0._r8
  nctend=0._r8
  nitend=0._r8
  nrtend = 0._r8
  nstend = 0._r8
  ! initialize in-cloud and in-precip quantities to zero

  qcic  = 0._r8
  qiic  = 0._r8
  qsic  = 0._r8
  qric  = 0._r8

  ncic  = 0._r8
  niic  = 0._r8
  nsic  = 0._r8
  nric  = 0._r8
  ! initialize precip at surface


  prect = 0._r8
  preci = 0._r8
  ! initialize precip fallspeeds to zero

  ums = 0._r8
  uns = 0._r8
  umr = 0._r8
  unr = 0._r8
  ! initialize limiter for output

  qcrat = 1._r8
  ! Many outputs have to be initialized here at the top to work around
  ! ifort problems, even if they are always overwritten later.

  effc = 10._r8
  lamcrad = 0._r8
  pgamrad = 0._r8
  effc_fn = 10._r8
  effi = 25._r8
  sadice = 0._r8
  sadsnow = 0._r8
  deffi = 50._r8

  qrout2 = 0._r8
  nrout2 = 0._r8
  drout2 = 0._r8
  qsout2 = 0._r8
  nsout2 = 0._r8
  dsout = 0._r8
  dsout2 = 0._r8

  freqr = 0._r8
  freqs = 0._r8

  reff_rain = 0._r8
  reff_snow = 0._r8

  refl = -9999._r8
  arefl = 0._r8
  areflz = 0._r8
  frefl = 0._r8
  csrfl = 0._r8
  acsrfl = 0._r8
  fcsrfl = 0._r8

  ncal = 0._r8
  ncai = 0._r8

  nfice = 0._r8
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
     nc = max(nc + npccn*deltat, 0._r8)
     ncal = nc*rho/lcldm ! sghan minimum in #/cm3
  elsewhere
     ncal = 0._r8
  end where

  where (t < icenuct)
     ncai = naai*rho
  elsewhere
     ncai = 0._r8
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
     where (naai > 0._r8 .and. t < icenuct .and. &
          relhum*esl/esi > 1.05_r8)
        !if NAAI > 0. then set numice = naai (as before)
        !note: this is gridbox averaged

        nnuccd = (naai-ni/icldm)/mtime*icldm
        nnuccd = max(nnuccd,0._r8)
        nimax = naai*icldm
        !Calc mass of new particles using new crystal mass...
        !also this will be multiplied by mtime as nnuccd is...


        mnuccd = nnuccd * mi0

     elsewhere
        nnuccd = 0._r8
        nimax = 0._r8
        mnuccd = 0._r8
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
           if (qs(i,k) > 0._r8) then
              ! make sure melting snow doesn't reduce temperature below threshold

              dum = -xlf/cpp*qs(i,k)
              if (t(i,k)+dum < snowmelt) then
                 dum = (t(i,k)-snowmelt)*cpp/xlf
                 dum = dum/qs(i,k)
                 dum = max(0._r8,dum)
                 dum = min(1._r8,dum)
              else
                 dum = 1._r8
              end if

              minstsm(i,k) = dum*qs(i,k)
              ninstsm(i,k) = dum*ns(i,k)

              dum1=-xlf*minstsm(i,k)/deltat
              tlat(i,k)=tlat(i,k)+dum1
              meltsdttot(i,k)=meltsdttot(i,k) + dum1

              qs(i,k) = max(qs(i,k) - minstsm(i,k), 0._r8)
              ns(i,k) = max(ns(i,k) - ninstsm(i,k), 0._r8)
              qr(i,k) = max(qr(i,k) + minstsm(i,k), 0._r8)
              nr(i,k) = max(nr(i,k) + ninstsm(i,k), 0._r8)
           end if
        end if

     end do
  end do 

  do k=1,nlev
    do i=1,mgncol
        ! freezing of rain at -5 C

        if (t(i,k) < rainfrze) then

           if (qr(i,k) > 0._r8) then
              ! make sure freezing rain doesn't increase temperature above threshold

              dum = xlf/cpp*qr(i,k)
              if (t(i,k)+dum > rainfrze) then
                 dum = -(t(i,k)-rainfrze)*cpp/xlf
                 dum = dum/qr(i,k)
                 dum = max(0._r8,dum)
                 dum = min(1._r8,dum)
              else
                 dum = 1._r8
              end if

              minstrf(i,k) = dum*qr(i,k)
              ninstrf(i,k) = dum*nr(i,k)
              ! heating tendency

              dum1 = xlf*minstrf(i,k)/deltat
              tlat(i,k)=tlat(i,k)+dum1
              frzrdttot(i,k)=frzrdttot(i,k) + dum1

              qr(i,k) = max(qr(i,k) - minstrf(i,k), 0._r8)
              nr(i,k) = max(nr(i,k) - ninstrf(i,k), 0._r8)
              qs(i,k) = max(qs(i,k) + minstrf(i,k), 0._r8)
              ns(i,k) = max(ns(i,k) + ninstrf(i,k), 0._r8)

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
           qcic(i,k)=min(qc(i,k)/lcldm(i,k),5.e-3_r8)
           ncic(i,k)=max(nc(i,k)/lcldm(i,k),0._r8)
           ! specify droplet concentration

           if (nccons) then
              ncic(i,k)=ncnst/rho(i,k)
           end if
        else
           qcic(i,k)=0._r8
           ncic(i,k)=0._r8
        end if

        if (qi(i,k).ge.qsmall) then
           ! limit in-cloud values to 0.005 kg/kg
           qiic(i,k)=min(qi(i,k)/icldm(i,k),5.e-3_r8)
           niic(i,k)=max(ni(i,k)/icldm(i,k),0._r8)
           ! switch for specification of cloud ice number

           if (nicons) then
              niic(i,k)=ninst/rho(i,k)
           end if
        else
           qiic(i,k)=0._r8
           niic(i,k)=0._r8
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

  if (trim(micro_mg_precip_frac_method) == 'in_cloud') then
     precip_frac_method =  MG_PRECIP_FRAC_INCLOUD
  else if(trim(micro_mg_precip_frac_method) == 'max_overlap') then
     precip_frac_method = MG_PRECIP_FRAC_OVERLAP
  endif

  micro_vert_loop: do k=1,nlev

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


     call size_dist_param_liq(mg_liq_props, qcic(1:mgncol,k), ncic(1:mgncol,k),&
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

     qric(:,k)=min(qric(:,k),0.01_r8)
     ! add autoconversion to precip from above to get provisional rain mixing ratio
     ! and number concentration (qric and nric)


     where (qric(:,k).lt.qsmall)
        qric(:,k)=0._r8
        nric(:,k)=0._r8
     end where
     ! make sure number concentration is a positive number to avoid
     ! taking root of negative later


     nric(:,k)=max(nric(:,k),0._r8)
     ! Get size distribution parameters for cloud ice


     call size_dist_param_basic(mg_ice_props, qiic(:,k), niic(:,k), &
          lami(:,k), mgncol, n0=n0i(:,k))
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

     qsic(:,k)=min(qsic(:,k),0.01_r8)
     ! if precip mix ratio is zero so should number concentration


     where (qsic(:,k) < qsmall)
        qsic(:,k)=0._r8
        nsic(:,k)=0._r8
     end where
     ! make sure number concentration is a positive number to avoid
     ! taking root of negative later


     nsic(:,k)=max(nsic(:,k),0._r8)
     !.......................................................................
     ! get size distribution parameters for precip
     !......................................................................
     ! rain


     call size_dist_param_basic(mg_rain_props, qric(:,k), nric(:,k), &
          lamr(:,k), mgncol, n0=n0r(:,k))

     NEC_BEGIN("where_block_#3")
     where (lamr(:,k) >= qsmall)
        ! provisional rain number and mass weighted mean fallspeed (m/s)


        unr(:,k) = min(arn(:,k)*gamma_br_plus1/lamr(:,k)**br,9.1_r8*rhof(:,k))
        umr(:,k) = min(arn(:,k)*gamma_br_plus4/(6._r8*lamr(:,k)**br),9.1_r8*rhof(:,k))

     elsewhere
        umr(:,k) = 0._r8
        unr(:,k) = 0._r8
     end where
     NEC_END("where_block_#3")
     !......................................................................
     ! snow


     call size_dist_param_basic(mg_snow_props, qsic(:,k), nsic(:,k), &
          lams(:,k), mgncol, n0=n0s(:,k))

     NEC_BEGIN("where_block_#4")
     where (lams(:,k) > 0._r8)
        ! provisional snow number and mass weighted mean fallspeed (m/s)


        ums(:,k) = min(asn(:,k)*gamma_bs_plus4/(6._r8*lams(:,k)**bs),1.2_r8*rhof(:,k))
        uns(:,k) = min(asn(:,k)*gamma_bs_plus1/lams(:,k)**bs,1.2_r8*rhof(:,k))

     elsewhere
        ums(:,k) = 0._r8
        uns(:,k) = 0._r8
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
           where (qcic(1:mgncol,k).ge.qsmall .and. t(:,k).lt.269.15_r8)
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

           mnudep(:,k)=0._r8
           nnudep(:,k)=0._r8

        else
           ! Mass of droplets frozen is the average droplet mass, except
           ! with two limiters: concentration must be at least 1/cm^3, and
           ! mass must be at least the minimum defined above.

           !NEC_BEGIN("where_block_#6")
           mi0l = qcic(1:mgncol,k)/max(ncic(:,k), 1.0e6_r8/rho(:,k))
           mi0l = max(mi0l_min, mi0l)

           where (qcic(1:mgncol,k) >= qsmall)
              nnuccc(:,k) = frzimm(:,k)*1.0e6_r8/rho(:,k)
              mnuccc(:,k) = nnuccc(:,k)*mi0l

              nnucct(:,k) = frzcnt(:,k)*1.0e6_r8/rho(:,k)
              mnucct(:,k) = nnucct(:,k)*mi0l

              nnudep(:,k) = frzdep(:,k)*1.0e6_r8/rho(:,k)
              mnudep(:,k) = nnudep(:,k)*mi0
           elsewhere
              nnuccc(:,k) = 0._r8
              mnuccc(:,k) = 0._r8

              nnucct(:,k) = 0._r8
              mnucct(:,k) = 0._r8

              nnudep(:,k) = 0._r8
              mnudep(:,k) = 0._r8
           end where
           !NEC_END("where_block_#6")

        end if

     else
        mnuccc(:,k)=0._r8
        nnuccc(:,k)=0._r8
        mnucct(:,k)=0._r8
        nnucct(:,k)=0._r8
        mnudep(:,k)=0._r8
        nnudep(:,k)=0._r8
     end if

     call snow_self_aggregation(t(:,k), rho(:,k), asn(:,k), rhosn, qsic(:,k), nsic(:,k), &
          nsagg(:,k), mgncol)

     call accrete_cloud_water_snow(t(:,k), rho(:,k), asn(:,k), uns(:,k), mu(:,k), &
          qcic(1:mgncol,k), ncic(:,k), qsic(:,k), pgam(:,k), lamc(:,k), lams(:,k), n0s(:,k), &
          psacws(:,k), npsacws(:,k), mgncol)

     if (do_cldice) then
        call secondary_ice_production(t(:,k), psacws(:,k), msacwi(:,k), nsacwi(:,k), mgncol)
     else
        nsacwi(:,k) = 0.0_r8
        msacwi(:,k) = 0.0_r8
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
        prai(:,k) = 0._r8
        nprai(:,k) = 0._r8
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

        where (ice_sublim(:,k) < 0._r8 .and. qi(:,k) > qsmall .and. icldm(:,k) > mincld)
           nsubi(:,k) = sublim_factor*ice_sublim(:,k) / qi(:,k) * ni(:,k) / icldm(:,k)

        elsewhere
           nsubi(:,k) = 0._r8
        end where
        ! bergeron process should not reduce nc unless
        ! all ql is removed (which is handled elsewhere)
        !in fact, nothing in this entire file makes nsubc nonzero.

        nsubc(:,k) = 0._r8

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
           qcrat(i,k) = 1._r8
        end if
        !PMC 12/3/12: ratio is also frac of step w/ liquid.
        !thus we apply berg for "ratio" of timestep and vapor
        !deposition for the remaining frac of the timestep.

        if (qc(i,k) >= qsmall) then
           vap_dep(i,k) = vap_dep(i,k)*(1._r8-qcrat(i,k))
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
        if (dum1 > 1.e-20_r8) then
           dum = (q(i,k)-qvi(i,k))/(1._r8 + xxls_squared*qvi(i,k)/(cpp*rv*t(i,k)**2))/deltat
           dum = max(dum,0._r8)
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

        mnuccri(i,k)=0._r8
        nnuccri(i,k)=0._r8

        if (do_cldice) then
           ! freezing of rain to produce ice if mean rain size is smaller than Dcs

           if (lamr(i,k) > qsmall .and. 1._r8/lamr(i,k) < Dcs) then
              mnuccri(i,k)=mnuccr(i,k)
              nnuccri(i,k)=nnuccr(i,k)
              mnuccr(i,k)=0._r8
              nnuccr(i,k)=0._r8
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


        if (pre(i,k) < 0._r8) then
           dum = pre(i,k)*deltat/qr(i,k)
           dum = max(-1._r8,dum)
           nsubr(i,k) = dum*nr(i,k)/deltat
        else
           nsubr(i,k) = 0._r8
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
              tmpfrz = 0._r8
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

        nsubs(i,k)=0._r8

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
     enddo
     NEC_END("loop_#12")
     ! modify ice/precip evaporation rate if q > qsat
     call qsat_water(ttmpA, p(:,k), esnA, qvnAI,mgncol)

     NEC_BEGIN("loop_#13")
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
           if (qtmpA(i) > qvnAI(i)) then

              dum1A(i)=pre(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
              dum2A(i)=prds(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
              ! recalculate q and t after vap_dep and mnuccd but without evap or sublim
              qtmpA(i)=q(i,k)-(vap_dep(i,k)+mnuccd(i,k))*deltat
              ttmpA(i)=t(i,k)+((vap_dep(i,k)+mnuccd(i,k))*xxls)*deltat/cpp
           endif
         endif
      enddo
     NEC_END("loop_#13")
      ! use rhw to allow ice supersaturation
      call qsat_water(ttmpA, p(:,k), esnA, qvnA,mgncol)

     NEC_BEGIN("loop_#14")
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
           if (qtmpA(i) > qvnAI(i)) then

              dum=(qtmpA(i)-qvnA(i))/(1._r8 + xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
              dum=min(dum,0._r8)
              ! modify rates if needed, divide by precip_frac to get local (in-precip) value

              pre(i,k)=dum*dum1A(i)/deltat/precip_frac(i,k)
              ! do separately using RHI for prds and ice_sublim
#if 1
           endif
        endif
     enddo
     NEC_END("loop_#14")

          call qsat_ice(ttmpA, p(:,k), esnA, qvnA,mgncol)

     NEC_BEGIN("loop_#15")
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
           if (qtmpA(i) > qvnAI(i)) then
#else

          call qsat_ice(ttmpA(i), p(i,k), esnA(i), qvnA(i))
#endif

              dum=(qtmpA(i)-qvnA(i))/(1._r8 + xxls_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
              dum=min(dum,0._r8)
              ! modify rates if needed, divide by precip_frac to get local (in-precip) value

              prds(i,k) = dum*dum2A(i)/deltat/precip_frac(i,k)
              ! don't divide ice_sublim by cloud fraction since it is grid-averaged

              dum1A(i) = (1._r8-dum1A(i)-dum2A(i))
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
             max(qc(i,k),1.0e-30_r8)
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
              tmpfrz = 0._r8
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


        if (do_cldice .and. nitend(i,k).gt.0._r8.and.ni(i,k)+nitend(i,k)*deltat.gt.nimax(i,k)) then
           nitend(i,k)=max(0._r8,(nimax(i,k)-ni(i,k))/deltat)
        end if

     end do
     NEC_END("loop_#16")
     ! End of "administration" loop


  end do micro_vert_loop ! end k loop
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


  do k=1,nlev

     call size_dist_param_basic(mg_rain_props, qric(:,k), nric(:,k), lamr(:,k), mgncol, n0=n0r(:,k))
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
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k),0._r8)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat)/icldm(i,k),0._r8)

        dumr(i,k) = (qr(i,k)+qrtend(i,k)*deltat)/precip_frac(i,k)
        dumnr(i,k) = max((nr(i,k)+nrtend(i,k)*deltat)/precip_frac(i,k),0._r8)
        dums(i,k) = (qs(i,k)+qstend(i,k)*deltat)/precip_frac(i,k)
        dumns(i,k) = max((ns(i,k)+nstend(i,k)*deltat)/precip_frac(i,k),0._r8)
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

  do k=1,nlev
     ! obtain new slope parameter to avoid possible singularity


     call size_dist_param_basic(mg_ice_props, dumi(:,k), dumni(:,k), &
          lami(:,k), mgncol)

     call size_dist_param_liq(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), &
          pgam(:,k), lamc(:,k), mgncol)

  enddo

  !NEC_BEGIN("loop_#18")
  do k=1,nlev
     do i=1,mgncol
        ! calculate number and mass weighted fall velocity for droplets and cloud ice
        !-------------------------------------------------------------------


        if (dumc(i,k).ge.qsmall) then

           vtrmc(i,k)=acn(i,k)*gamma(4._r8+bc+pgam(i,k))/ &
                (lamc(i,k)**bc*gamma(pgam(i,k)+4._r8))

           fc(i,k) = g*rho(i,k)*vtrmc(i,k)

           fnc(i,k) = g*rho(i,k)* &
                acn(i,k)*gamma(1._r8+bc+pgam(i,k))/ &
                (lamc(i,k)**bc*gamma(pgam(i,k)+1._r8))
        else
           fc(i,k) = 0._r8
           fnc(i,k)= 0._r8
        end if
        ! calculate number and mass weighted fall velocity for cloud ice


        if (dumi(i,k).ge.qsmall) then

           vtrmi(i,k)=min(ain(i,k)*gamma_bi_plus4/(6._r8*lami(i,k)**bi), &
                1.2_r8*rhof(i,k))

           fi(i,k) = g*rho(i,k)*vtrmi(i,k)
           fni(i,k) = g*rho(i,k)* &
                min(ain(i,k)*gamma_bi_plus1/lami(i,k)**bi,1.2_r8*rhof(i,k))
           ! adjust the ice fall velocity for smaller (r < 20 um) ice
           ! particles (blend over 18-20 um)

           irad = 1.5_r8 / lami(i,k) * 1e6_r8
           ifrac = min(1._r8, max(0._r8, (irad - 18._r8) / 2._r8))
 
           if (ifrac .lt. 1._r8) then
              vtrmi(i,k) = ifrac * vtrmi(i,k) + & 
                 (1._r8 - ifrac) * &
                 min(ajn(i,k)*gamma_bj_plus4/(6._r8*lami(i,k)**bj), &
                 1.2_r8*rhof(i,k))

              fi(i,k) = g*rho(i,k)*vtrmi(i,k)
              fni(i,k) = ifrac * fni(i,k) + & 
                 (1._r8 - ifrac) * &
                 g*rho(i,k)* &
                 min(ajn(i,k)*gamma_bj_plus1/lami(i,k)**bj,1.2_r8*rhof(i,k))
           end if
        else
           fi(i,k) = 0._r8
           fni(i,k)= 0._r8
        end if

     enddo

  enddo
  !NEC_END("loop_#18")

  do k=1,nlev
     ! fallspeed for rain
     call size_dist_param_basic(mg_rain_props, dumr(:,k), dumnr(:,k), lamr(:,k), mgncol)
     ! fallspeed for snow
     call size_dist_param_basic(mg_snow_props, dums(:,k), dumns(:,k), lams(:,k), mgncol)
  enddo

  !NEC_BEGIN("loop_#19")
  do k=1,nlev

     do i=1,mgncol
        if (lamr(i,k).ge.qsmall) then
           ! 'final' values of number and mass weighted mean fallspeed for rain (m/s)


           unr(i,k) = min(arn(i,k)*gamma_br_plus1/lamr(i,k)**br,9.1_r8*rhof(i,k))
           umr(i,k) = min(arn(i,k)*gamma_br_plus4/(6._r8*lamr(i,k)**br),9.1_r8*rhof(i,k))

           fr(i,k) = g*rho(i,k)*umr(i,k)
           fnr(i,k) = g*rho(i,k)*unr(i,k)

        else
           fr(i,k)=0._r8
           fnr(i,k)=0._r8
        end if



        if (lams(i,k).ge.qsmall) then
           ! 'final' values of number and mass weighted mean fallspeed for snow (m/s)

           ums(i,k) = min(asn(i,k)*gamma_bs_plus4/(6._r8*lams(i,k)**bs),1.2_r8*rhof(i,k))
           uns(i,k) = min(asn(i,k)*gamma_bs_plus1/lams(i,k)**bs,1.2_r8*rhof(i,k))

           fs(i,k) = g*rho(i,k)*ums(i,k)
           fns(i,k) = g*rho(i,k)*uns(i,k)

        else
           fs(i,k)=0._r8
           fns(i,k)=0._r8
        end if
        ! redefine dummy variables - sedimentation is calculated over grid-scale
        ! quantities to ensure conservation


        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat),0._r8)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat),0._r8)
        dumr(i,k) = (qr(i,k)+qrtend(i,k)*deltat)
        dumnr(i,k) = max((nr(i,k)+nrtend(i,k)*deltat),0._r8)
        dums(i,k) = (qs(i,k)+qstend(i,k)*deltat)
        dumns(i,k) = max((ns(i,k)+nstend(i,k)*deltat),0._r8)

        if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
        if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8
        if (dumr(i,k).lt.qsmall) dumnr(i,k)=0._r8
        if (dums(i,k).lt.qsmall) dumns(i,k)=0._r8

     enddo
  end do       !!! vertical loop

  do k=1,nlev
     do i=1,mgncol
       pdel_inv(i,k) = 1._r8/pdel(i,k)
     enddo
  enddo
  !NEC_END("loop_#19")
  ! initialize nstep for sedimentation sub-steps
  ! calculate number of split time steps to ensure courant stability criteria
  ! for sedimentation calculations
  !-------------------------------------------------------------------


  NEC_BEGIN("sedim#1")
  nstepMax=0
  do i=1,mgncol
     tmpk1(:) = fi(i,:)*pdel_inv(i,:)*deltat
     tmpk2(:) = fni(i,:)*pdel_inv(i,:)*deltat
     nstep = 1 + int(max(maxval(tmpk1),maxval(tmpk2)))
     nstepMax = max(nstepMax,nstep)
  enddo
     ! loop over sedimentation sub-time step to ensure stability
     !==============================================================

  !   PRINT *,'i: ',i,'nstep: ',nstep
     rnstep = 1._r8/real(nstepMax)

     do n = 1,nstepMax

        if (do_cldice) then
           falouti2D  = fi(:,:)  * dumi(:,:)
           faloutni2D = fni(:,:) * dumni(:,:)
        else
           falouti2D  = 0._r8
           faloutni2D = 0._r8
        end if
        ! top of model


        k = 1
        ! add fallout terms to microphysical tendencies
        do i=1,mgncol

        faltndi = falouti2D(i,k)/pdel(i,k)
        faltndni = faloutni2D(i,k)/pdel(i,k)
        qitend(i,k) = qitend(i,k)-faltndi*rnstep
        nitend(i,k) = nitend(i,k)-faltndni*rnstep
        ! sedimentation tendency for output

        qisedten(i,k)=qisedten(i,k)-faltndi*rnstep

        dumi(i,k) = dumi(i,k)-faltndi*deltat*rnstep
        dumni(i,k) = dumni(i,k)-faltndni*deltat*rnstep
        enddo

        do k = 2,nlev
           ! for cloud liquid and ice, if cloud fraction increases with height
           ! then add flux from above to both vapor and cloud water of current level
           ! this means that flux entering clear portion of cell from above evaporates
           ! instantly
           ! note: this is not an issue with precip, since we assume max overlap


           do i=1,mgncol
           dum1=icldm(i,k)/icldm(i,k-1)
           dum1=min(dum1,1._r8)

           faltndqie=(falouti2D(i,k)-falouti2D(i,k-1))/pdel(i,k)
           faltndi=(falouti2D(i,k)-dum1*falouti2D(i,k-1))/pdel(i,k)
           faltndni=(faloutni2D(i,k)-dum1*faloutni2D(i,k-1))/pdel(i,k)
           ! add fallout terms to eulerian tendencies


           qitend(i,k) = qitend(i,k)-faltndi*rnstep
           nitend(i,k) = nitend(i,k)-faltndni*rnstep
           ! sedimentation tendency for output

           qisedten(i,k)=qisedten(i,k)-faltndi*rnstep
           ! add terms to to evap/sub of cloud water


           qvlat(i,k)=qvlat(i,k)-(faltndqie-faltndi)*rnstep
           ! for output
           qisevap(i,k)=qisevap(i,k)-(faltndqie-faltndi)*rnstep

           tlat(i,k)=tlat(i,k)+(faltndqie-faltndi)*xxls*rnstep

           dumi(i,k) = dumi(i,k)-faltndi*deltat*rnstep
           dumni(i,k) = dumni(i,k)-faltndni*deltat*rnstep
           enddo

        end do
        ! Ice flux

        do k = 1,nlev
          do i=1,mgncol 
          iflx(i,k+1) = iflx(i,k+1) + falouti2D(i,k) / g * rnstep
          enddo
        end do
        ! units below are m/s
        ! sedimentation flux at surface is added to precip flux at surface
        ! to get total precip (cloud + precip water) rate

        do i=1,mgncol 
        prect(i) = prect(i)+falouti2D(i,nlev)/g*rnstep/1000._r8
        preci(i) = preci(i)+falouti2D(i,nlev)/g*rnstep/1000._r8
        enddo

     end do
     ! calculate number of split time steps to ensure courant stability criteria
     ! for sedimentation calculations
     !-------------------------------------------------------------------

  NEC_END("sedim#1")
  NEC_BEGIN("sedim#2")
  do i=1,mgncol
     tmpk1 = fc(i,:)*pdel_inv(i,:)*deltat
     tmpk2 = fnc(i,:)*pdel_inv(i,:)*deltat
     nstep = 1 + int(max(maxval( tmpk1), maxval(tmpk2)))
     nstepMax = max(nstepMax,nstep)
  enddo

     ! loop over sedimentation sub-time step to ensure stability
     !==============================================================
     rnstep = 1._r8/real(nstepMax)

     do n = 1,nstepMax

        faloutc2D  = fc(:,:)  * dumc(:,:)
        faloutnc2D = fnc(:,:) * dumnc(:,:)
        ! top of model

        k = 1
        ! add fallout terms to microphysical tendencies

        do i=1,mgncol
        faltndc = faloutc2D(i,k)/pdel(i,k)
        faltndnc = faloutnc2D(i,k)/pdel(i,k)
        qctend(i,k) = qctend(i,k)-faltndc*rnstep
        nctend(i,k) = nctend(i,k)-faltndnc*rnstep
        ! sedimentation tendency for output

        qcsedten(i,k)=qcsedten(i,k)-faltndc*rnstep

        dumc(i,k) = dumc(i,k)-faltndc*deltat*rnstep
        dumnc(i,k) = dumnc(i,k)-faltndnc*deltat*rnstep
        enddo

        do k = 2,nlev

           do i=1,mgncol
           dum=lcldm(i,k)/lcldm(i,k-1)
           dum=min(dum,1._r8)
           faltndqce=(faloutc2D(i,k)-faloutc2D(i,k-1))/pdel(i,k)
           faltndc=(faloutc2D(i,k)-dum*faloutc2D(i,k-1))/pdel(i,k)
           faltndnc=(faloutnc2D(i,k)-dum*faloutnc2D(i,k-1))/pdel(i,k)
           ! add fallout terms to eulerian tendencies

           qctend(i,k) = qctend(i,k)-faltndc*rnstep
           nctend(i,k) = nctend(i,k)-faltndnc*rnstep
           ! sedimentation tendency for output

           qcsedten(i,k)=qcsedten(i,k)-faltndc*rnstep
           ! add terms to to evap/sub of cloud water

           qvlat(i,k)=qvlat(i,k)-(faltndqce-faltndc)*rnstep
           ! for output
           qcsevap(i,k)=qcsevap(i,k)-(faltndqce-faltndc)*rnstep

           tlat(i,k)=tlat(i,k)+(faltndqce-faltndc)*xxlv*rnstep

           dumc(i,k) = dumc(i,k)-faltndc*deltat*rnstep
           dumnc(i,k) = dumnc(i,k)-faltndnc*deltat*rnstep
           enddo

        end do
        !Liquid condensate flux here

        do k = 1,nlev
           do i=1,mgncol
           lflx(i,k+1) = lflx(i,k+1) + faloutc2D(i,k) / g * rnstep
           enddo
        end do

        do i=1,mgncol
        prect(i) = prect(i)+faloutc2D(i,nlev)/g*rnstep/1000._r8
        enddo

     end do
  NEC_END("sedim#2")
  NEC_BEGIN("sedim#3")
  nstepMax = 0
  do i=1,mgncol
     tmpk1 = fr(i,:)*pdel_inv(i,:)*deltat
     tmpk2 = fnr(i,:)*pdel_inv(i,:)*deltat
     nstep = 1 + int(max(maxval( tmpk1), maxval(tmpk2)))
     nstepMax = max(nstepMax,nstep)
  enddo

     ! calculate number of split time steps to ensure courant stability criteria
     ! for sedimentation calculations
     !-------------------------------------------------------------------

     ! loop over sedimentation sub-time step to ensure stability
     !==============================================================
     rnstep = 1._r8/real(nstepMax)

     do n = 1,nstepMax

        faloutr2D  = fr(:,:)  * dumr(:,:)
        faloutnr2D = fnr(:,:) * dumnr(:,:)
        ! top of model

        k = 1
        ! add fallout terms to microphysical tendencies

        do i=1,mgncol
        faltndr = faloutr2D(i,k)/pdel(i,k)
        faltndnr = faloutnr2D(i,k)/pdel(i,k)
        qrtend(i,k) = qrtend(i,k)-faltndr*rnstep
        nrtend(i,k) = nrtend(i,k)-faltndnr*rnstep
        ! sedimentation tendency for output

        qrsedten(i,k)=qrsedten(i,k)-faltndr*rnstep

        dumr(i,k) = dumr(i,k)-faltndr*deltat*rnstep
        dumnr(i,k) = dumnr(i,k)-faltndnr*deltat*rnstep
        enddo

        do k = 2,nlev

           do i=1,mgncol
           faltndr=(faloutr2D(i,k)-faloutr2D(i,k-1))/pdel(i,k)
           faltndnr=(faloutnr2D(i,k)-faloutnr2D(i,k-1))/pdel(i,k)
           ! add fallout terms to eulerian tendencies

           qrtend(i,k) = qrtend(i,k)-faltndr*rnstep
           nrtend(i,k) = nrtend(i,k)-faltndnr*rnstep
           ! sedimentation tendency for output

           qrsedten(i,k)=qrsedten(i,k)-faltndr*rnstep

           dumr(i,k) = dumr(i,k)-faltndr*deltat*rnstep
           dumnr(i,k) = dumnr(i,k)-faltndnr*deltat*rnstep
           enddo

        end do
        ! Rain Flux

        do k = 1,nlev
           do i=1,mgncol
           rflx(i,k+1) = rflx(i,k+1) + faloutr2D(i,k) / g * rnstep
           enddo
        end do

        do i=1,mgncol
        prect(i) = prect(i)+faloutr2D(i,nlev)/g*rnstep/1000._r8
        enddo
  enddo
  NEC_END("sedim#3")
  NEC_BEGIN("sedim#4")
  nstepMax = 0
  do i=1,mgncol
     tmpk1 = fs(i,:)*pdel_inv(i,:)*deltat
     tmpk2 = fns(i,:)*pdel_inv(i,:)*deltat
     nstep = 1 + int(max(maxval( tmpk1), maxval(tmpk2)))
     nstepMax = max(nstepMax,nstep)
  enddo

     ! calculate number of split time steps to ensure courant stability criteria
     ! for sedimentation calculations
     !-------------------------------------------------------------------

     ! loop over sedimentation sub-time step to ensure stability
     !==============================================================
     rnstep = 1._r8/real(nstepMax)

     do n = 1,nstepMax

        falouts2D  = fs(:,:)  * dums(:,:)
        faloutns2D = fns(:,:) * dumns(:,:)
        ! top of model

        k = 1
        ! add fallout terms to microphysical tendencies

        do i=1,mgncol
        faltnds  = (falouts2D(i,k)/pdel(i,k))*rnstep
        faltndns = (faloutns2D(i,k)/pdel(i,k))*rnstep
        qstend(i,k) = qstend(i,k)-faltnds
        nstend(i,k) = nstend(i,k)-faltndns
       
        ! sedimentation tendency for output

        qssedten(i,k)=qssedten(i,k)-faltnds

           dums(i,k) = dums(i,k)-faltnds*deltat
           dumns(i,k) = dumns(i,k)-faltndns*deltat
        enddo

        do k = 2,nlev

           do i=1,mgncol
           faltnds=(falouts2D(i,k)-falouts2D(i,k-1))/pdel(i,k)*rnstep
           faltndns=(faloutns2D(i,k)-faloutns2D(i,k-1))/pdel(i,k)*rnstep
           ! add fallout terms to eulerian tendencies

           qstend(i,k) = qstend(i,k)-faltnds
           nstend(i,k) = nstend(i,k)-faltndns
           ! sedimentation tendency for output

           qssedten(i,k)=qssedten(i,k)-faltnds

           dums(i,k) = dums(i,k)-faltnds*deltat
           dumns(i,k) = dumns(i,k)-faltndns*deltat
           enddo 

        end do   !! k loop
        ! Snow Flux

        do k = 1,nlev
           do i=1,mgncol
           sflx(i,k+1) = sflx(i,k+1) + falouts2D(i,k) / g * rnstep
           enddo
        end do
  
        do i=1,mgncol
        prect(i) = prect(i)+falouts2D(i,nlev)/g*rnstep/1000._r8
        preci(i) = preci(i)+falouts2D(i,nlev)/g*rnstep/1000._r8
        enddo

     end do   !! nstep loop

  NEC_END("sedim#4")
  ! end sedimentation
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! get new update for variables that includes sedimentation tendency
  ! note : here dum variables are grid-average, NOT in-cloud


  !NEC_BEGIN("loop_#20")
  do k=1,nlev
     do i=1,mgncol
        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)

        dumr(i,k) = max(qr(i,k)+qrtend(i,k)*deltat,0._r8)
        dumnr(i,k) = max(nr(i,k)+nrtend(i,k)*deltat,0._r8)
        dums(i,k) = max(qs(i,k)+qstend(i,k)*deltat,0._r8)
        dumns(i,k) = max(ns(i,k)+nstend(i,k)*deltat,0._r8)
        ! switch for specification of droplet and crystal number

        if (nccons) then
           dumnc(i,k)=ncnst/rho(i,k)*lcldm(i,k)
        end if
        ! switch for specification of cloud ice number

        if (nicons) then
           dumni(i,k)=ninst/rho(i,k)*icldm(i,k)
        end if

        if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
        if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8
        if (dumr(i,k).lt.qsmall) dumnr(i,k)=0._r8
        if (dums(i,k).lt.qsmall) dumns(i,k)=0._r8

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
           if (dums(i,k) > 0._r8) then
              ! make sure melting snow doesn't reduce temperature below threshold

              dum = -xlf/cpp*dums(i,k)
              if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt. snowmelt) then
                 dum = (t(i,k)+tlat(i,k)/cpp*deltat-snowmelt)*cpp/xlf
                 dum = dum/dums(i,k)
                 dum = max(0._r8,dum)
                 dum = min(1._r8,dum)
              else
                 dum = 1._r8
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
   do k=1,nlev
      NEC_BEGIN("loop_#22")
      do i=1,mgncol
        ! freezing of rain at -5 C


        if (t(i,k)+tlat(i,k)/cpp*deltat < rainfrze) then
           if (dumr(i,k) > 0._r8) then
              ! make sure freezing rain doesn't increase temperature above threshold

              dum = xlf/cpp*dumr(i,k)
              if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.rainfrze) then
                 dum = -(t(i,k)+tlat(i,k)/cpp*deltat-rainfrze)*cpp/xlf
                 dum = dum/dumr(i,k)
                 dum = max(0._r8,dum)
                 dum = min(1._r8,dum)
              else
                 dum = 1._r8
              end if

              qrtend(i,k)=qrtend(i,k)-dum*dumr(i,k)/deltat
              nrtend(i,k)=nrtend(i,k)-dum*dumnr(i,k)/deltat
              ! get mean size of rain = 1/lamr, add frozen rain to either snow or cloud ice
              ! depending on mean rain size


           endif
        endif
      enddo
      NEC_END("loop_#22")
      call size_dist_param_basic(mg_rain_props, dumr(:,k), dumnr(:,k), lamr(:,k),mgncol)
      NEC_BEGIN("loop_#23")
      do i=1,mgncol
        if (t(i,k)+tlat(i,k)/cpp*deltat < rainfrze) then
           if (dumr(i,k) > 0._r8) then
              if (lamr(i,k) < 1._r8/Dcs) then
                 qstend(i,k)=qstend(i,k)+dum*dumr(i,k)/deltat
                 nstend(i,k)=nstend(i,k)+dum*dumnr(i,k)/deltat
              else
                 qitend(i,k)=qitend(i,k)+dum*dumr(i,k)/deltat
                 nitend(i,k)=nitend(i,k)+dum*dumnr(i,k)/deltat
              end if
              ! heating tendency

              dum1 = xlf*dum*dumr(i,k)/deltat
              frzrdttot(i,k)=frzrdttot(i,k) + dum1
              tlat(i,k)=tlat(i,k)+dum1

           end if
        end if

      enddo
      NEC_END("loop_#23")
   enddo
   if (do_cldice) then
      do k=1,nlev
        do i=1,mgncol
           if (t(i,k)+tlat(i,k)/cpp*deltat > tmelt) then
              if (dumi(i,k) > 0._r8) then
                 ! limit so that melting does not push temperature below freezing
                 !-----------------------------------------------------------------

                 dum = -dumi(i,k)*xlf/cpp
                 if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.tmelt) then
                    dum = (t(i,k)+tlat(i,k)/cpp*deltat-tmelt)*cpp/xlf
                    dum = dum/dumi(i,k)
                    dum = max(0._r8,dum)
                    dum = min(1._r8,dum)
                 else
                    dum = 1._r8
                 end if

                 qctend(i,k)=qctend(i,k)+dum*dumi(i,k)/deltat
                 ! for output

                 melttot(i,k)=dum*dumi(i,k)/deltat
                 ! assume melting ice produces droplet
                 ! mean volume radius of 8 micron


                 nctend(i,k)=nctend(i,k)+3._r8*dum*dumi(i,k)/deltat/ &
                      (4._r8*pi*5.12e-16_r8*rhow)

                 qitend(i,k)=((1._r8-dum)*dumi(i,k)-qi(i,k))/deltat
                 nitend(i,k)=((1._r8-dum)*dumni(i,k)-ni(i,k))/deltat
                 tlat(i,k)=tlat(i,k)-xlf*dum*dumi(i,k)/deltat
              end if
           end if
        enddo
     enddo
     ! homogeneously freeze droplets at -40 C
     !-----------------------------------------------------------------


     do k=1,nlev
        do i=1,mgncol
           if (t(i,k)+tlat(i,k)/cpp*deltat < 233.15_r8) then
              if (dumc(i,k) > 0._r8) then
                 ! limit so that freezing does not push temperature above threshold

                 dum = dumc(i,k)*xlf/cpp
                 if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.233.15_r8) then
                    dum = -(t(i,k)+tlat(i,k)/cpp*deltat-233.15_r8)*cpp/xlf
                    dum = dum/dumc(i,k)
                    dum = max(0._r8,dum)
                    dum = min(1._r8,dum)
                 else
                    dum = 1._r8
                 end if

                 qitend(i,k)=qitend(i,k)+dum*dumc(i,k)/deltat
                 ! for output
                 homotot(i,k)=dum*dumc(i,k)/deltat
                 ! assume 25 micron mean volume radius of homogeneously frozen droplets
                 ! consistent with size of detrained ice in stratiform.F90

                 nitend(i,k)=nitend(i,k)+dum*3._r8*dumc(i,k)/(4._r8*3.14_r8*1.563e-14_r8* &
                      500._r8)/deltat
                 qctend(i,k)=((1._r8-dum)*dumc(i,k)-qc(i,k))/deltat
                 nctend(i,k)=((1._r8-dum)*dumnc(i,k)-nc(i,k))/deltat
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
        enddo
        call qsat_water(ttmpA, p(:,k), esnA, qvnA,mgncol)

      NEC_BEGIN("loop_#26")
        do i=1,mgncol
           if (qtmpA(i) > qvnA(i) .and. qvnA(i) > 0 .and. allow_sed_supersat) then
              ! expression below is approximate since there may be ice deposition
              dum = (qtmpA(i)-qvnA(i))/(1._r8+xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))/deltat
              ! add to output cme
              cmeout(i,k) = cmeout(i,k)+dum
              ! now add to tendencies, partition between liquid and ice based on temperature
              if (ttmpA(i) > 268.15_r8) then
                 dum1=0.0_r8
                 ! now add to tendencies, partition between liquid and ice based on te
                 !-------------------------------------------------------
              else if (ttmpA(i) < 238.15_r8) then
                 dum1=1.0_r8
              else
                 dum1=(268.15_r8-ttmpA(i))/30._r8
              end if

              dum = (qtmpA(i)-qvnA(i))/(1._r8+(xxls*dum1+xxlv*(1._r8-dum1))**2 &
                   *qvnA(i)/(cpp*rv*ttmpA(i)**2))/deltat
              qctend(i,k)=qctend(i,k)+dum*(1._r8-dum1)
              ! for output
              qcrestot(i,k)=dum*(1._r8-dum1)
              qitend(i,k)=qitend(i,k)+dum*dum1
              qirestot(i,k)=dum*dum1
              qvlat(i,k)=qvlat(i,k)-dum
              ! for output
              qvres(i,k)=-dum
              tlat(i,k)=tlat(i,k)+dum*(1._r8-dum1)*xxlv+dum*dum1*xxls
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
        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)/lcldm(i,k)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)/icldm(i,k)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)/lcldm(i,k)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)/icldm(i,k)

        dumr(i,k) = max(qr(i,k)+qrtend(i,k)*deltat,0._r8)/precip_frac(i,k)
        dumnr(i,k) = max(nr(i,k)+nrtend(i,k)*deltat,0._r8)/precip_frac(i,k)
        dums(i,k) = max(qs(i,k)+qstend(i,k)*deltat,0._r8)/precip_frac(i,k)
        dumns(i,k) = max(ns(i,k)+nstend(i,k)*deltat,0._r8)/precip_frac(i,k)
        ! switch for specification of droplet and crystal number

        if (nccons) then
           dumnc(i,k)=ncnst/rho(i,k)
        end if
        ! switch for specification of cloud ice number

        if (nicons) then
           dumni(i,k)=ninst/rho(i,k)
        end if
        ! limit in-cloud mixing ratio to reasonable value of 5 g kg-1

        dumc(i,k)=min(dumc(i,k),5.e-3_r8)
        dumi(i,k)=min(dumi(i,k),5.e-3_r8)
        ! limit in-precip mixing ratios
        dumr(i,k)=min(dumr(i,k),10.e-3_r8)
        dums(i,k)=min(dums(i,k),10.e-3_r8)
     enddo
  enddo
  ! cloud ice effective radius
  !-----------------------------------------------------------------

  if (do_cldice) then
     do k=1,nlev
        dum_2D(:,k) = dumni(:,k)
        call size_dist_param_basic(mg_ice_props, dumi(:,k), dumni(:,k), lami(:,k), mgncol, dumni0A)
      NEC_BEGIN("loop_#28")
        do i=1,mgncol
           if (dumi(i,k).ge.qsmall) then


              if (dumni(i,k) /=dum_2D(i,k)) then
                 ! adjust number conc if needed to keep mean size in reasonable range
                 nitend(i,k)=(dumni(i,k)*icldm(i,k)-ni(i,k))/deltat
              end if

              effi(i,k) = 1.5_r8/lami(i,k)*1.e6_r8
              sadice(i,k) = 2._r8*pi*(lami(i,k)**(-3))*dumni0A(i)*rho(i,k)*1.e-2_r8  ! m2/m3 -> cm2/cm3

           else
              effi(i,k) = 25._r8
              sadice(i,k) = 0._r8
           end if
           ! ice effective diameter for david mitchell's optics

           deffi(i,k)=effi(i,k)*rhoi/rhows*2._r8
        enddo
      NEC_END("loop_#28")
     enddo
  else
      NEC_BEGIN("loop_#29")
     do k=1,nlev
        do i=1,mgncol
           ! NOTE: If CARMA is doing the ice microphysics, then the ice effective
           ! radius has already been determined from the size distribution.
           effi(i,k) = re_ice(i,k) * 1.e6_r8      ! m -> um
           deffi(i,k)=effi(i,k) * 2._r8
           sadice(i,k) = 4._r8*pi*(effi(i,k)**2)*ni(i,k)*rho(i,k)*1e-2_r8
        enddo
     enddo
      NEC_END("loop_#29")
  end if
  ! cloud droplet effective radius
  !-----------------------------------------------------------------

  do k=1,nlev
     dum_2D(:,k) = dumnc(:,k)
     call size_dist_param_liq(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), pgam(:,k), lamc(:,k),mgncol)
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

           effc(i,k) = (pgam(i,k)+3._r8)/lamc(i,k)/2._r8*1.e6_r8
           !assign output fields for shape here
           lamcrad(i,k)=lamc(i,k)
           pgamrad(i,k)=pgam(i,k)
           ! recalculate effective radius for constant number, in order to separate
           ! first and second indirect effects
           !======================================
           ! assume constant number of 10^8 kg-1


           dumnc(i,k)=1.e8_r8
           ! Pass in "false" adjust flag to prevent number from being changed within
           ! size distribution subroutine.
        endif
     enddo
      NEC_END("loop_#30")

     call size_dist_param_liq(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), pgam(:,k), lamc(:,k),mgncol)

     do i=1,mgncol
        if (dumc(i,k).ge.qsmall) then

           effc_fn(i,k) = (pgam(i,k)+3._r8)/lamc(i,k)/2._r8*1.e6_r8

        else
           effc(i,k) = 10._r8
           lamcrad(i,k)=0._r8
           pgamrad(i,k)=0._r8
           effc_fn(i,k) = 10._r8
        end if
     enddo
  enddo
  ! recalculate 'final' rain size distribution parameters
  ! to ensure that rain size is in bounds, adjust rain number if needed
  do k=1,nlev
     dum_2D(:,k) = dumnr(:,k)
     call size_dist_param_basic(mg_rain_props, dumr(:,k), dumnr(:,k), lamr(:,k),mgncol)
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
  do k=1,nlev
     dum_2D(:,k) = dumns(:,k)
     call size_dist_param_basic(mg_snow_props, dums(:,k), dumns(:,k), lams(:,k), mgncol, n0=dumns0A)
     do i=1,mgncol
        if (dums(i,k).ge.qsmall) then


           if (dum_2D(i,k) /= dumns(i,k)) then
              ! adjust number conc if needed to keep mean size in reasonable range
              nstend(i,k)=(dumns(i,k)*precip_frac(i,k)-ns(i,k))/deltat
           end if

           sadsnow(i,k) = 2._r8*pi*(lams(i,k)**(-3))*dumns0A(i)*rho(i,k)*1.e-2_r8  ! m2/m3 -> cm2/cm3

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

!  print *,'where #1: count: ',count(qrout .gt. 1.e-7_r8 .and. nrout.gt.0._r8), mgncol*nlev
  call avg_diameter_vec(qrout,nrout,rho,rhow,drout2,mgncol*nlev)
  where (qrout .gt. 1.e-7_r8 &
       .and. nrout.gt.0._r8)
     qrout2 = qrout * precip_frac
     nrout2 = nrout * precip_frac
     ! The avg_diameter call does the actual calculation; other diameter
     ! outputs are just drout2 times constants.
     ! drout2 = avg_diameter(qrout, nrout, rho, rhow)
     freqr = precip_frac

     reff_rain=1.5_r8*drout2*1.e6_r8
  elsewhere
     qrout2 = 0._r8
     nrout2 = 0._r8
     drout2 = 0._r8
     freqr = 0._r8
     reff_rain = 0._r8
  end where

!  print *,'where #2: count: ',count(qsout .gt. 1.e-7_r8 .and. nsout.gt.0._r8), mgncol*nlev
  call avg_diameter_vec(qsout, nsout, rho, rhosn,dsout2,mgncol*nlev)
  where (qsout .gt. 1.e-7_r8 &
       .and. nsout.gt.0._r8)
     qsout2 = qsout * precip_frac
     nsout2 = nsout * precip_frac
     ! The avg_diameter call does the actual calculation; other diameter
     ! outputs are just dsout2 times constants.
     ! dsout2 = avg_diameter(qsout, nsout, rho, rhosn)
     freqs = precip_frac

     dsout=3._r8*rhosn/rhows*dsout2

     reff_snow=1.5_r8*dsout2*1.e6_r8
  elsewhere
     dsout  = 0._r8
     qsout2 = 0._r8
     nsout2 = 0._r8
     dsout2 = 0._r8
     freqs  = 0._r8
     reff_snow=0._r8
  end where
  ! analytic radar reflectivity
  !--------------------------------------------------
  ! formulas from Matthew Shupe, NOAA/CERES
  ! *****note: radar reflectivity is local (in-precip average)
  ! units of mm^6/m^3


  !NEC_BEGIN("loop_#31")
  do k=1,nlev
     do i = 1,mgncol
        if (qc(i,k).ge.qsmall .and. (nc(i,k)+nctend(i,k)*deltat).gt.10._r8) then
           dum=(qc(i,k)/lcldm(i,k)*rho(i,k)*1000._r8)**2 &
                /(0.109_r8*(nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)/1.e6_r8)*lcldm(i,k)/precip_frac(i,k)
        else
           dum=0._r8
        end if
        if (qi(i,k).ge.qsmall) then
           dum1=(qi(i,k)*rho(i,k)/icldm(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)*icldm(i,k)/precip_frac(i,k)
        else
           dum1=0._r8
        end if

        if (qsout(i,k).ge.qsmall) then
           dum1=dum1+(qsout(i,k)*rho(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)
        end if

        refl(i,k)=dum+dum1
        ! add rain rate, but for 37 GHz formulation instead of 94 GHz
        ! formula approximated from data of Matrasov (2007)
        ! rainrt is the rain rate in mm/hr
        ! reflectivity (dum) is in DBz


        if (rainrt(i,k).ge.0.001_r8) then
           dum=log10(rainrt(i,k)**6._r8)+16._r8
           ! convert from DBz to mm^6/m^3


           dum = 10._r8**(dum/10._r8)
        else
           ! don't include rain rate in R calculation for values less than 0.001 mm/hr
           dum=0._r8
        end if
        ! add to refl


        refl(i,k)=refl(i,k)+dum
        !output reflectivity in Z.

        areflz(i,k)=refl(i,k) * precip_frac(i,k)
        ! convert back to DBz


        if (refl(i,k).gt.minrefl) then
           refl(i,k)=10._r8*log10(refl(i,k))
        else
           refl(i,k)=-9999._r8
        end if
        !set averaging flag

        if (refl(i,k).gt.mindbz) then
           arefl(i,k)=refl(i,k) * precip_frac(i,k)
           frefl(i,k)=precip_frac(i,k)
        else
           arefl(i,k)=0._r8
           areflz(i,k)=0._r8
           frefl(i,k)=0._r8
        end if
        ! bound cloudsat reflectivity


        csrfl(i,k)=min(csmax,refl(i,k))
        !set averaging flag

        if (csrfl(i,k).gt.csmin) then
           acsrfl(i,k)=refl(i,k) * precip_frac(i,k)
           fcsrfl(i,k)=precip_frac(i,k)
        else
           acsrfl(i,k)=0._r8
           fcsrfl(i,k)=0._r8
        end if

     end do
  end do
  !NEC_END("loop_#31")
  !redefine fice here....

  dum_2D = qsout + qrout + qc + qi
  dumi = qsout + qi
  where (dumi .gt. qsmall .and. dum_2D .gt. qsmall)
     nfice=min(dumi/dum_2D,1._r8)
  elsewhere
     nfice=0._r8
  end where

end subroutine micro_mg_tend
!========================================================================
!OUTPUT CALCULATIONS
!========================================================================


subroutine calc_rercld(lamr, n0r, lamc, pgam, qric, qcic, ncic, rercld, mgncol)
  integer, intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: lamr          ! rain size parameter (slope)
  real(r8), dimension(mgncol), intent(in) :: n0r           ! rain size parameter (intercept)
  real(r8), dimension(mgncol), intent(in) :: lamc          ! size distribution parameter (slope)
  real(r8), dimension(mgncol), intent(in) :: pgam          ! droplet size parameter
  real(r8), dimension(mgncol), intent(in) :: qric          ! in-cloud rain mass mixing ratio
  real(r8), dimension(mgncol), intent(in) :: qcic          ! in-cloud cloud liquid
  real(r8), dimension(mgncol), intent(in) :: ncic          ! in-cloud droplet number concentration

  real(r8), dimension(mgncol), intent(inout) :: rercld     ! effective radius calculation for rain + cloud
  ! combined size of precip & cloud drops

  real(r8) :: Atmp(mgncol),tmp(mgncol), pgamp1(mgncol)

  integer :: i

  pgamp1 = pgam+1._r8
  call rising_factorial(pgamp1, 2,tmp,mgncol)
  do i=1,mgncol
     ! Rain drops
     if (lamr(i) > 0._r8) then
        Atmp(i) = n0r(i) * pi / (2._r8 * lamr(i)**3._r8)
     else
        Atmp(i) = 0._r8
     end if
     ! Add cloud drops

     if (lamc(i) > 0._r8) then
        Atmp(i) = Atmp(i) + &
             ncic(i) * pi * tmp(i)/(4._r8 * lamc(i)**2._r8)
     end if

     if (Atmp(i) > 0._r8) then
        rercld(i) = rercld(i) + 3._r8 *(qric(i) + qcic(i)) / (4._r8 * rhow * Atmp(i))
     end if
  enddo
end subroutine calc_rercld
!========================================================================
!UTILITIES
!========================================================================


!read state subroutine for kr_externs_in_micro_mg2_0 
SUBROUTINE kr_externs_in_micro_mg2_0(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) nccons 
    READ (UNIT = kgen_unit) nicons 
    READ (UNIT = kgen_unit) ncnst 
    READ (UNIT = kgen_unit) ninst 
    READ (UNIT = kgen_unit) dcs 
    READ (UNIT = kgen_unit) g 
    READ (UNIT = kgen_unit) r 
    READ (UNIT = kgen_unit) rv 
    READ (UNIT = kgen_unit) cpp 
    READ (UNIT = kgen_unit) tmelt 
    READ (UNIT = kgen_unit) xxlv 
    READ (UNIT = kgen_unit) xlf 
    READ (UNIT = kgen_unit) xxls 
    READ (UNIT = kgen_unit) microp_uniform 
    READ (UNIT = kgen_unit) do_cldice 
    READ (UNIT = kgen_unit) use_hetfrz_classnuc 
    READ (UNIT = kgen_unit) rhosu 
    READ (UNIT = kgen_unit) icenuct 
    READ (UNIT = kgen_unit) snowmelt 
    READ (UNIT = kgen_unit) rainfrze 
    READ (UNIT = kgen_unit) gamma_br_plus1 
    READ (UNIT = kgen_unit) gamma_br_plus4 
    READ (UNIT = kgen_unit) gamma_bs_plus1 
    READ (UNIT = kgen_unit) gamma_bs_plus4 
    READ (UNIT = kgen_unit) gamma_bi_plus1 
    READ (UNIT = kgen_unit) gamma_bi_plus4 
    READ (UNIT = kgen_unit) gamma_bj_plus1 
    READ (UNIT = kgen_unit) gamma_bj_plus4 
    READ (UNIT = kgen_unit) xxlv_squared 
    READ (UNIT = kgen_unit) xxls_squared 
    READ (UNIT = kgen_unit) micro_mg_precip_frac_method 
    READ (UNIT = kgen_unit) micro_mg_berg_eff_factor 
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
