
! KGEN-generated Fortran source file
!
! Filename    : micro_mg2_0.F90
! Generated at: 2015-10-08 11:52:40
! KGEN version: 0.5.2



    MODULE micro_mg2_0
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !---------------------------------------------------------------------------------
        ! Purpose:
        !   MG microphysics version 2.0 - Update of MG microphysics with
        !                                 prognostic precipitation.
        !
        ! Author: Andrew Gettelman, Hugh Morrison.
        ! Contributions from: Peter Caldwell, Xiaohong Liu and Steve Ghan
        ! Version 2 history: Sep 2011: Development begun.
        !                    Feb 2013: Added of prognostic precipitation.
        ! invoked in CAM by specifying -microphys=mg2.0
        !
        ! for questions contact Hugh Morrison, Andrew Gettelman
        ! e-mail: morrison@ucar.edu, andrew@ucar.edu
        !---------------------------------------------------------------------------------
        !
        ! NOTE: Modified to allow other microphysics packages (e.g. CARMA) to do ice
        ! microphysics in cooperation with the MG liquid microphysics. This is
        ! controlled by the do_cldice variable.
        !
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
        !
        ! This option has not been updated since the introduction of prognostic
        ! precipitation, and probably should be adjusted to cover snow as well.
        !
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
        !
        ! Code is divided into two main subroutines:
        !   subroutine micro_mg_init --> initializes microphysics routine, should be called
        !                                  once at start of simulation
        !   subroutine micro_mg_tend --> main microphysics routine to be called each time step
        !                                this also calls several smaller subroutines to calculate
        !                                microphysical processes and other utilities
        !
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
!        USE shr_spfn_mod, ONLY: gamma => shr_spfn_gamma
        USE wv_sat_methods, ONLY: qsat_water => wv_sat_qsat_water
        USE wv_sat_methods, ONLY: qsat_ice => wv_sat_qsat_ice
        ! Parameters from the utilities module.
        USE micro_mg_utils, ONLY: r8
        USE micro_mg_utils, ONLY: qsmall
        USE micro_mg_utils, ONLY: mincld
        USE micro_mg_utils, ONLY: ar
        USE micro_mg_utils, ONLY: as
        USE micro_mg_utils, ONLY: rhow
        USE micro_mg_utils, ONLY: ai
        USE micro_mg_utils, ONLY: mi0
        USE micro_mg_utils, ONLY: pi
        USE micro_mg_utils, ONLY: rising_factorial
        USE params, only: mgncol

        IMPLICIT NONE
        PRIVATE
        PUBLIC micro_mg_tend
        ! switch for specification rather than prediction of droplet and crystal number
        ! note: number will be adjusted as needed to keep mean size within bounds,
        ! even when specified droplet or ice number is used
        ! If constant cloud ice number is set (nicons = .true.),
        ! then all microphysical processes except mass transfer due to ice nucleation
        ! (mnuccd) are based on the fixed cloud ice number. Calculation of
        ! mnuccd follows from the prognosed ice crystal number ni.
        ! nccons = .true. to specify constant cloud droplet number
        ! nicons = .true. to specify constant cloud ice number
        LOGICAL, parameter, public :: nccons = .false.
        LOGICAL, parameter, public :: nicons = .false.
        !=========================================================
        ! Private module parameters
        !=========================================================
        ! parameters for specified ice and droplet number concentration
        ! note: these are local in-cloud values, not grid-mean
        REAL(KIND=r8), parameter :: ncnst = 100.e6_r8 ! droplet num concentration when nccons=.true. (m-3)
        REAL(KIND=r8), parameter :: ninst = 0.1e6_r8 ! ice num concentration when nicons=.true. (m-3)
        !Range of cloudsat reflectivities (dBz) for analytic simulator
        REAL(KIND=r8), parameter :: csmin = -30._r8
        REAL(KIND=r8), parameter :: csmax = 26._r8
        REAL(KIND=r8), parameter :: mindbz = -99._r8
        REAL(KIND=r8), parameter :: minrefl = 1.26e-10_r8 ! minrefl = 10._r8**(mindbz/10._r8)
        ! autoconversion size threshold for cloud ice to snow (m)
        REAL(KIND=r8) :: dcs
        ! minimum mass of new crystal due to freezing of cloud droplets done
        ! externally (kg)
        REAL(KIND=r8), parameter :: mi0l_min = 4._r8/3._r8*pi*rhow*(4.e-6_r8)**3
        !=========================================================
        ! Constants set in initialization
        !=========================================================
        ! Set using arguments to micro_mg_init
        REAL(KIND=r8) :: g ! gravity
        REAL(KIND=r8) :: r ! dry air gas constant
        REAL(KIND=r8) :: rv ! water vapor gas constant
        REAL(KIND=r8) :: cpp ! specific heat of dry air
        REAL(KIND=r8) :: tmelt ! freezing point of water (K)
        ! latent heats of:
        REAL(KIND=r8) :: xxlv ! vaporization
        REAL(KIND=r8) :: xlf ! freezing
        REAL(KIND=r8) :: xxls ! sublimation
        REAL(KIND=r8) :: rhmini ! Minimum rh for ice cloud fraction > 0.
        ! flags
        LOGICAL :: microp_uniform
        LOGICAL :: do_cldice
        LOGICAL :: use_hetfrz_classnuc
        REAL(KIND=r8) :: rhosu ! typical 850mn air density
        REAL(KIND=r8) :: icenuct ! ice nucleation temperature: currently -5 degrees C
        REAL(KIND=r8) :: snowmelt ! what temp to melt all snow: currently 2 degrees C
        REAL(KIND=r8) :: rainfrze ! what temp to freeze all rain: currently -5 degrees C
        ! additional constants to help speed up code
        REAL(KIND=r8) :: gamma_br_plus1
        REAL(KIND=r8) :: gamma_br_plus4
        REAL(KIND=r8) :: gamma_bs_plus1
        REAL(KIND=r8) :: gamma_bs_plus4
        REAL(KIND=r8) :: gamma_bi_plus1
        REAL(KIND=r8) :: gamma_bi_plus4
        REAL(KIND=r8) :: xxlv_squared
        REAL(KIND=r8) :: xxls_squared
        CHARACTER(LEN=16) :: micro_mg_precip_frac_method ! type of precipitation fraction method
        REAL(KIND=r8) :: micro_mg_berg_eff_factor ! berg efficiency factor
        LOGICAL :: allow_sed_supersat ! Allow supersaturated conditions after sedimentation loop
        !===============================================================================
            PUBLIC kgen_read_externs_micro_mg2_0
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_micro_mg2_0(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) use_hetfrz_classnuc
            READ(UNIT=kgen_unit) do_cldice
            READ(UNIT=kgen_unit) microp_uniform
            READ(UNIT=kgen_unit) r
            READ(UNIT=kgen_unit) rhosu
            READ(UNIT=kgen_unit) g
            READ(UNIT=kgen_unit) tmelt
            READ(UNIT=kgen_unit) icenuct
            READ(UNIT=kgen_unit) rhmini
            READ(UNIT=kgen_unit) snowmelt
            READ(UNIT=kgen_unit) xlf
            READ(UNIT=kgen_unit) cpp
            READ(UNIT=kgen_unit) rainfrze
            READ(UNIT=kgen_unit) micro_mg_precip_frac_method
            READ(UNIT=kgen_unit) dcs
            READ(UNIT=kgen_unit) gamma_br_plus1
            READ(UNIT=kgen_unit) gamma_br_plus4
            READ(UNIT=kgen_unit) gamma_bs_plus4
            READ(UNIT=kgen_unit) gamma_bs_plus1
            READ(UNIT=kgen_unit) micro_mg_berg_eff_factor
            READ(UNIT=kgen_unit) xxls_squared
            READ(UNIT=kgen_unit) rv
            READ(UNIT=kgen_unit) xxlv
            READ(UNIT=kgen_unit) xxls
            READ(UNIT=kgen_unit) xxlv_squared
            READ(UNIT=kgen_unit) gamma_bi_plus4
            READ(UNIT=kgen_unit) gamma_bi_plus1
            READ(UNIT=kgen_unit) allow_sed_supersat
        END SUBROUTINE kgen_read_externs_micro_mg2_0

        !===============================================================================

        !===============================================================================
        !microphysics routine for each timestep goes here...

        SUBROUTINE micro_mg_tend(nlev, deltatin, t, q, qcn, qin, ncn, nin, qrn, qsn, nrn, nsn, relvar, accre_enhan, p, &
        pdel, cldn, liqcldf, icecldf, qcsinksum_rate1ord, naai, npccn, rndst, nacon, tlat, qvlat, qctend, qitend, nctend, nitend, &
        qrtend, qstend, nrtend, nstend, effc, effc_fn, effi, prect, preci, nevapr, evapsnow, prain, prodsnow, cmeout, deffi, &
        pgamrad, lamcrad, qsout, dsout, rflx, sflx, qrout, reff_rain, reff_snow, qcsevap, qisevap, qvres, cmeitot, vtrmc, vtrmi, &
        umr, ums, qcsedten, qisedten, qrsedten, qssedten, pratot, prctot, mnuccctot, mnuccttot, msacwitot, psacwstot, bergstot, &
        bergtot, melttot, homotot, qcrestot, prcitot, praitot, qirestot, mnuccrtot, pracstot, meltsdttot, frzrdttot, mnuccdtot, &
        nrout, nsout, refl, arefl, areflz, frefl, csrfl, acsrfl, fcsrfl, rercld, ncai, ncal, qrout2, qsout2, nrout2, nsout2, &
        drout2, dsout2, freqs, freqr, nfice, qcrat, errstring, tnd_qsnow, tnd_nsnow, re_ice, prer_evap, frzimm, frzcnt, frzdep)
            ! Below arguments are "optional" (pass null pointers to omit).
            ! Constituent properties.
            USE micro_mg_utils, ONLY: mg_liq_props
            USE micro_mg_utils, only : r8
            USE micro_mg_utils, only : qsmall
            USE micro_mg_utils, ONLY: mg_ice_props
            USE micro_mg_utils, ONLY: mg_rain_props
            USE micro_mg_utils, only : br
            USE micro_mg_utils, ONLY: mg_snow_props
            USE micro_mg_utils, only : bs
            USE micro_mg_utils, only : mi0
            USE micro_mg_utils, only : rhosn
            USE micro_mg_utils, only : mincld
            USE micro_mg_utils, only : omsm
            USE micro_mg_utils, only : bc
            USE micro_mg_utils, only : bi
            USE micro_mg_utils, only : rhow
            USE micro_mg_utils, only : pi
            USE micro_mg_utils, only : rhows
            USE micro_mg_utils, only : rhoi
            ! Size calculation functions.
            USE micro_mg_utils, ONLY: size_dist_param_liq
            USE micro_mg_utils, ONLY: size_dist_param_basic
            USE micro_mg_utils, ONLY: size_dist_param_basic_vec
            USE micro_mg_utils, ONLY: avg_diameter, avg_diameter_func
            ! Microphysical processes.
            USE micro_mg_utils, ONLY: kk2000_liq_autoconversion
            USE micro_mg_utils, ONLY: ice_autoconversion
            USE micro_mg_utils, ONLY: immersion_freezing
            USE micro_mg_utils, ONLY: contact_freezing
            USE micro_mg_utils, ONLY: snow_self_aggregation
            USE micro_mg_utils, ONLY: accrete_cloud_water_snow
            USE micro_mg_utils, ONLY: secondary_ice_production
            USE micro_mg_utils, ONLY: accrete_rain_snow
            USE micro_mg_utils, ONLY: heterogeneous_rain_freezing
            USE micro_mg_utils, ONLY: accrete_cloud_water_rain
            USE micro_mg_utils, ONLY: self_collection_rain
            USE micro_mg_utils, ONLY: accrete_cloud_ice_snow
            USE micro_mg_utils, ONLY: evaporate_sublimate_precip
            USE micro_mg_utils, ONLY: bergeron_process_snow
            USE micro_mg_utils, ONLY: ice_deposition_sublimation

            !Authors: Hugh Morrison, Andrew Gettelman, NCAR, Peter Caldwell, LLNL
            ! e-mail: morrison@ucar.edu, andrew@ucar.edu
            ! input arguments
!            INTEGER, intent(in) :: mgncol ! number of microphysics columns
            INTEGER, intent(in) :: nlev ! number of layers
            REAL(KIND=r8), intent(in) :: deltatin ! time step (s)
            REAL(KIND=r8), intent(in) :: t(mgncol,nlev) ! input temperature (K)
            REAL(KIND=r8), intent(in) :: q(mgncol,nlev) ! input h20 vapor mixing ratio (kg/kg)
            ! note: all input cloud variables are grid-averaged
            REAL(KIND=r8), intent(in) :: qcn(mgncol,nlev) ! cloud water mixing ratio (kg/kg)
            REAL(KIND=r8), intent(in) :: qin(mgncol,nlev) ! cloud ice mixing ratio (kg/kg)
            REAL(KIND=r8), intent(in) :: ncn(mgncol,nlev) ! cloud water number conc (1/kg)
            REAL(KIND=r8), intent(in) :: nin(mgncol,nlev) ! cloud ice number conc (1/kg)
            REAL(KIND=r8), intent(in) :: qrn(mgncol,nlev) ! rain mixing ratio (kg/kg)
            REAL(KIND=r8), intent(in) :: qsn(mgncol,nlev) ! snow mixing ratio (kg/kg)
            REAL(KIND=r8), intent(in) :: nrn(mgncol,nlev) ! rain number conc (1/kg)
            REAL(KIND=r8), intent(in) :: nsn(mgncol,nlev) ! snow number conc (1/kg)
            REAL(KIND=r8), intent(in) :: relvar(mgncol,nlev) ! cloud water relative variance (-)
            REAL(KIND=r8), intent(in) :: accre_enhan(mgncol,nlev) ! optional accretion
            ! enhancement factor (-)
            REAL(KIND=r8), intent(in) :: p(mgncol,nlev) ! air pressure (pa)
            REAL(KIND=r8), intent(in) :: pdel(mgncol,nlev) ! pressure difference across level (pa)
            REAL(KIND=r8), intent(in) :: cldn(mgncol,nlev) ! cloud fraction (no units)
            REAL(KIND=r8), intent(in) :: liqcldf(mgncol,nlev) ! liquid cloud fraction (no units)
            REAL(KIND=r8), intent(in) :: icecldf(mgncol,nlev) ! ice cloud fraction (no units)
            ! used for scavenging
            ! Inputs for aerosol activation
            REAL(KIND=r8), intent(in) :: naai(mgncol,nlev) ! ice nucleation number (from microp_aero_ts) (1/kg)
            REAL(KIND=r8), intent(in) :: npccn(mgncol,nlev) ! ccn activated number tendency (from microp_aero_ts) (1/kg*s)
            ! Note that for these variables, the dust bin is assumed to be the last index.
            ! (For example, in CAM, the last dimension is always size 4.)
            REAL(KIND=r8), intent(in) :: rndst(:,:,:) ! radius of each dust bin, for contact freezing (from microp_aero_ts) (m)
            REAL(KIND=r8), intent(in) :: nacon(:,:,:) ! number in each dust bin, for contact freezing  (from microp_aero_ts) (1/m^3)
            ! output arguments
            REAL(KIND=r8), intent(out) :: qcsinksum_rate1ord(mgncol,nlev) ! 1st order rate for
            ! direct cw to precip conversion
            REAL(KIND=r8), intent(out) :: tlat(mgncol,nlev) ! latent heating rate       (W/kg)
            REAL(KIND=r8), intent(out) :: qvlat(mgncol,nlev) ! microphysical tendency qv (1/s)
            REAL(KIND=r8), intent(out) :: qctend(mgncol,nlev) ! microphysical tendency qc (1/s)
            REAL(KIND=r8), intent(out) :: qitend(mgncol,nlev) ! microphysical tendency qi (1/s)
            REAL(KIND=r8), intent(out) :: nctend(mgncol,nlev) ! microphysical tendency nc (1/(kg*s))
            REAL(KIND=r8), intent(out) :: nitend(mgncol,nlev) ! microphysical tendency ni (1/(kg*s))
            REAL(KIND=r8), intent(out) :: qrtend(mgncol,nlev) ! microphysical tendency qr (1/s)
            REAL(KIND=r8), intent(out) :: qstend(mgncol,nlev) ! microphysical tendency qs (1/s)
            REAL(KIND=r8), intent(out) :: nrtend(mgncol,nlev) ! microphysical tendency nr (1/(kg*s))
            REAL(KIND=r8), intent(out) :: nstend(mgncol,nlev) ! microphysical tendency ns (1/(kg*s))
            REAL(KIND=r8), intent(out) :: effc(mgncol,nlev) ! droplet effective radius (micron)
            REAL(KIND=r8), intent(out) :: effc_fn(mgncol,nlev) ! droplet effective radius, assuming nc = 1.e8 kg-1
            REAL(KIND=r8), intent(out) :: effi(mgncol,nlev) ! cloud ice effective radius (micron)
            REAL(KIND=r8), intent(out) :: prect(mgncol) ! surface precip rate (m/s)
            REAL(KIND=r8), intent(out) :: preci(mgncol) ! cloud ice/snow precip rate (m/s)
            REAL(KIND=r8), intent(out) :: nevapr(mgncol,nlev) ! evaporation rate of rain + snow (1/s)
            REAL(KIND=r8), intent(out) :: evapsnow(mgncol,nlev) ! sublimation rate of snow (1/s)
            REAL(KIND=r8), intent(out) :: prain(mgncol,nlev) ! production of rain + snow (1/s)
            REAL(KIND=r8), intent(out) :: prodsnow(mgncol,nlev) ! production of snow (1/s)
            REAL(KIND=r8), intent(out) :: cmeout(mgncol,nlev) ! evap/sub of cloud (1/s)
            REAL(KIND=r8), intent(out) :: deffi(mgncol,nlev) ! ice effective diameter for optics (radiation) (micron)
            REAL(KIND=r8), intent(out) :: pgamrad(mgncol,nlev) ! ice gamma parameter for optics (radiation) (no units)
            REAL(KIND=r8), intent(out) :: lamcrad(mgncol,nlev) ! slope of droplet distribution for optics (radiation) (1/m)
            REAL(KIND=r8), intent(out) :: qsout(mgncol,nlev) ! snow mixing ratio (kg/kg)
            REAL(KIND=r8), intent(out) :: dsout(mgncol,nlev) ! snow diameter (m)
            REAL(KIND=r8), intent(out) :: rflx(mgncol,nlev) ! grid-box average rain flux (kg m^-2 s^-1)
            REAL(KIND=r8), intent(out) :: sflx(mgncol,nlev) ! grid-box average snow flux (kg m^-2 s^-1)
            REAL(KIND=r8), intent(out) :: qrout(mgncol,nlev) ! grid-box average rain mixing ratio (kg/kg)
            REAL(KIND=r8), intent(out) :: reff_rain(mgncol,nlev) ! rain effective radius (micron)
            REAL(KIND=r8), intent(out) :: reff_snow(mgncol,nlev) ! snow effective radius (micron)
            REAL(KIND=r8), intent(out) :: qcsevap(mgncol,nlev) ! cloud water evaporation due to sedimentation (1/s)
            REAL(KIND=r8), intent(out) :: qisevap(mgncol,nlev) ! cloud ice sublimation due to sublimation (1/s)
            REAL(KIND=r8), intent(out) :: qvres(mgncol,nlev) ! residual condensation term to ensure RH < 100% (1/s)
            REAL(KIND=r8), intent(out) :: cmeitot(mgncol,nlev) ! grid-mean cloud ice sub/dep (1/s)
            REAL(KIND=r8), intent(out) :: vtrmc(mgncol,nlev) ! mass-weighted cloud water fallspeed (m/s)
            REAL(KIND=r8), intent(out) :: vtrmi(mgncol,nlev) ! mass-weighted cloud ice fallspeed (m/s)
            REAL(KIND=r8), intent(out) :: umr(mgncol,nlev) ! mass weighted rain fallspeed (m/s)
            REAL(KIND=r8), intent(out) :: ums(mgncol,nlev) ! mass weighted snow fallspeed (m/s)
            REAL(KIND=r8), intent(out) :: qcsedten(mgncol,nlev) ! qc sedimentation tendency (1/s)
            REAL(KIND=r8), intent(out) :: qisedten(mgncol,nlev) ! qi sedimentation tendency (1/s)
            REAL(KIND=r8), intent(out) :: qrsedten(mgncol,nlev) ! qr sedimentation tendency (1/s)
            REAL(KIND=r8), intent(out) :: qssedten(mgncol,nlev) ! qs sedimentation tendency (1/s)
            ! microphysical process rates for output (mixing ratio tendencies) (all have units of 1/s)
            REAL(KIND=r8), intent(out) :: pratot(mgncol,nlev) ! accretion of cloud by rain
            REAL(KIND=r8), intent(out) :: prctot(mgncol,nlev) ! autoconversion of cloud to rain
            REAL(KIND=r8), intent(out) :: mnuccctot(mgncol,nlev) ! mixing ratio tend due to immersion freezing
            REAL(KIND=r8), intent(out) :: mnuccttot(mgncol,nlev) ! mixing ratio tend due to contact freezing
            REAL(KIND=r8), intent(out) :: msacwitot(mgncol,nlev) ! mixing ratio tend due to H-M splintering
            REAL(KIND=r8), intent(out) :: psacwstot(mgncol,nlev) ! collection of cloud water by snow
            REAL(KIND=r8), intent(out) :: bergstot(mgncol,nlev) ! bergeron process on snow
            REAL(KIND=r8), intent(out) :: bergtot(mgncol,nlev) ! bergeron process on cloud ice
            REAL(KIND=r8), intent(out) :: melttot(mgncol,nlev) ! melting of cloud ice
            REAL(KIND=r8), intent(out) :: homotot(mgncol,nlev) ! homogeneous freezing cloud water
            REAL(KIND=r8), intent(out) :: qcrestot(mgncol,nlev) ! residual cloud condensation due to removal of excess supersat
            REAL(KIND=r8), intent(out) :: prcitot(mgncol,nlev) ! autoconversion of cloud ice to snow
            REAL(KIND=r8), intent(out) :: praitot(mgncol,nlev) ! accretion of cloud ice by snow
            REAL(KIND=r8), intent(out) :: qirestot(mgncol,nlev) ! residual ice deposition due to removal of excess supersat
            REAL(KIND=r8), intent(out) :: mnuccrtot(mgncol,nlev) ! mixing ratio tendency due to heterogeneous freezing of rain to snow (1/s)
            REAL(KIND=r8), intent(out) :: pracstot(mgncol,nlev) ! mixing ratio tendency due to accretion of rain by snow (1/s)
            REAL(KIND=r8), intent(out) :: meltsdttot(mgncol,nlev) ! latent heating rate due to melting of snow  (W/kg)
            REAL(KIND=r8), intent(out) :: frzrdttot(mgncol,nlev) ! latent heating rate due to homogeneous freezing of rain (W/kg)
            REAL(KIND=r8), intent(out) :: mnuccdtot(mgncol,nlev) ! mass tendency from ice nucleation
            REAL(KIND=r8), intent(out) :: nrout(mgncol,nlev) ! rain number concentration (1/m3)
            REAL(KIND=r8), intent(out) :: nsout(mgncol,nlev) ! snow number concentration (1/m3)
            REAL(KIND=r8), intent(out) :: refl(mgncol,nlev) ! analytic radar reflectivity
            REAL(KIND=r8), intent(out) :: arefl(mgncol,nlev) ! average reflectivity will zero points outside valid range
            REAL(KIND=r8), intent(out) :: areflz(mgncol,nlev) ! average reflectivity in z.
            REAL(KIND=r8), intent(out) :: frefl(mgncol,nlev) ! fractional occurrence of radar reflectivity
            REAL(KIND=r8), intent(out) :: csrfl(mgncol,nlev) ! cloudsat reflectivity
            REAL(KIND=r8), intent(out) :: acsrfl(mgncol,nlev) ! cloudsat average
            REAL(KIND=r8), intent(out) :: fcsrfl(mgncol,nlev) ! cloudsat fractional occurrence of radar reflectivity
            REAL(KIND=r8), intent(out) :: rercld(mgncol,nlev) ! effective radius calculation for rain + cloud
            REAL(KIND=r8), intent(out) :: ncai(mgncol,nlev) ! output number conc of ice nuclei available (1/m3)
            REAL(KIND=r8), intent(out) :: ncal(mgncol,nlev) ! output number conc of CCN (1/m3)
            REAL(KIND=r8), intent(out) :: qrout2(mgncol,nlev) ! copy of qrout as used to compute drout2
            REAL(KIND=r8), intent(out) :: qsout2(mgncol,nlev) ! copy of qsout as used to compute dsout2
            REAL(KIND=r8), intent(out) :: nrout2(mgncol,nlev) ! copy of nrout as used to compute drout2
            REAL(KIND=r8), intent(out) :: nsout2(mgncol,nlev) ! copy of nsout as used to compute dsout2
            REAL(KIND=r8), intent(out) :: drout2(mgncol,nlev) ! mean rain particle diameter (m)
            REAL(KIND=r8), intent(out) :: dsout2(mgncol,nlev) ! mean snow particle diameter (m)
            REAL(KIND=r8), intent(out) :: freqs(mgncol,nlev) ! fractional occurrence of snow
            REAL(KIND=r8), intent(out) :: freqr(mgncol,nlev) ! fractional occurrence of rain
            REAL(KIND=r8), intent(out) :: nfice(mgncol,nlev) ! fractional occurrence of ice
            REAL(KIND=r8), intent(out) :: qcrat(mgncol,nlev) ! limiter for qc process rates (1=no limit --> 0. no qc)
            REAL(KIND=r8), intent(out) :: prer_evap(mgncol,nlev)
            CHARACTER(LEN=128), intent(out) :: errstring ! output status (non-blank for error return)
            ! Tendencies calculated by external schemes that can replace MG's native
            ! process tendencies.
            ! Used with CARMA cirrus microphysics
            ! (or similar external microphysics model)
            REAL(KIND=r8), intent(in), pointer :: tnd_qsnow(:,:) ! snow mass tendency (kg/kg/s)
            REAL(KIND=r8), intent(in), pointer :: tnd_nsnow(:,:) ! snow number tendency (#/kg/s)
            REAL(KIND=r8), intent(in), pointer :: re_ice(:,:) ! ice effective radius (m)
            ! From external ice nucleation.
            REAL(KIND=r8), intent(in), pointer :: frzimm(:,:) ! Number tendency due to immersion freezing (1/cm3)
            REAL(KIND=r8), intent(in), pointer :: frzcnt(:,:) ! Number tendency due to contact freezing (1/cm3)
            REAL(KIND=r8), intent(in), pointer :: frzdep(:,:) ! Number tendency due to deposition nucleation (1/cm3)
            ! local workspace
            ! all units mks unless otherwise stated
            ! local copies of input variables
            REAL(KIND=r8) :: qc(mgncol,nlev) ! cloud liquid mixing ratio (kg/kg)
            REAL(KIND=r8) :: qi(mgncol,nlev) ! cloud ice mixing ratio (kg/kg)
            REAL(KIND=r8) :: nc(mgncol,nlev) ! cloud liquid number concentration (1/kg)
            REAL(KIND=r8) :: ni(mgncol,nlev) ! cloud liquid number concentration (1/kg)
            REAL(KIND=r8) :: qr(mgncol,nlev) ! rain mixing ratio (kg/kg)
            REAL(KIND=r8) :: qs(mgncol,nlev) ! snow mixing ratio (kg/kg)
            REAL(KIND=r8) :: nr(mgncol,nlev) ! rain number concentration (1/kg)
            REAL(KIND=r8) :: ns(mgncol,nlev) ! snow number concentration (1/kg)
            ! general purpose variables
            REAL(KIND=r8) :: deltat ! sub-time step (s)
            REAL(KIND=r8) :: mtime ! the assumed ice nucleation timescale
            ! physical properties of the air at a given point
            REAL(KIND=r8) :: rho(mgncol,nlev) ! density (kg m-3)
            REAL(KIND=r8) :: dv(mgncol,nlev) ! diffusivity of water vapor
            REAL(KIND=r8) :: mu(mgncol,nlev) ! viscosity
            REAL(KIND=r8) :: sc(mgncol,nlev) ! schmidt number
            REAL(KIND=r8) :: rhof(mgncol,nlev) ! density correction factor for fallspeed
            ! cloud fractions
            REAL(KIND=r8) :: precip_frac(mgncol,nlev) ! precip fraction assuming maximum overlap
            REAL(KIND=r8) :: cldm(mgncol,nlev) ! cloud fraction
            REAL(KIND=r8) :: icldm(mgncol,nlev) ! ice cloud fraction
            REAL(KIND=r8) :: lcldm(mgncol,nlev) ! liq cloud fraction
            ! mass mixing ratios
            REAL(KIND=r8) :: qcic(mgncol,nlev) ! in-cloud cloud liquid
            REAL(KIND=r8) :: qiic(mgncol,nlev) ! in-cloud cloud ice
            REAL(KIND=r8) :: qsic(mgncol,nlev) ! in-precip snow
            REAL(KIND=r8) :: qric(mgncol,nlev) ! in-precip rain
            ! number concentrations
            REAL(KIND=r8) :: ncic(mgncol,nlev) ! in-cloud droplet
            REAL(KIND=r8) :: niic(mgncol,nlev) ! in-cloud cloud ice
            REAL(KIND=r8) :: nsic(mgncol,nlev) ! in-precip snow
            REAL(KIND=r8) :: nric(mgncol,nlev) ! in-precip rain
            ! maximum allowed ni value
            REAL(KIND=r8) :: nimax(mgncol,nlev)
            ! Size distribution parameters for:
            ! cloud ice
            REAL(KIND=r8) :: lami(mgncol,nlev) ! slope
            REAL(KIND=r8) :: n0i(mgncol,nlev) ! intercept
            ! cloud liquid
            REAL(KIND=r8) :: lamc(mgncol,nlev) ! slope
            REAL(KIND=r8) :: pgam(mgncol,nlev) ! spectral width parameter
            ! snow
            REAL(KIND=r8) :: lams(mgncol,nlev) ! slope
            REAL(KIND=r8) :: n0s(mgncol,nlev) ! intercept
            ! rain
            REAL(KIND=r8) :: lamr(mgncol,nlev) ! slope
            REAL(KIND=r8) :: n0r(mgncol,nlev) ! intercept
            ! Rates/tendencies due to:
            ! Instantaneous snow melting
            REAL(KIND=r8) :: minstsm(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: ninstsm(mgncol,nlev) ! number concentration
            ! Instantaneous rain freezing
            REAL(KIND=r8) :: minstrf(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: ninstrf(mgncol,nlev) ! number concentration
            ! deposition of cloud ice
            REAL(KIND=r8) :: vap_dep(mgncol,nlev) ! deposition from vapor to ice PMC 12/3/12
            ! sublimation of cloud ice
            REAL(KIND=r8) :: ice_sublim(mgncol,nlev) ! sublimation from ice to vapor PMC 12/3/12
            ! ice nucleation
            REAL(KIND=r8) :: nnuccd(mgncol,nlev) ! number rate from deposition/cond.-freezing
            REAL(KIND=r8) :: mnuccd(mgncol,nlev) ! mass mixing ratio
            ! freezing of cloud water
            REAL(KIND=r8) :: mnuccc(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nnuccc(mgncol,nlev) ! number concentration
            ! contact freezing of cloud water
            REAL(KIND=r8) :: mnucct(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nnucct(mgncol,nlev) ! number concentration
            ! deposition nucleation in mixed-phase clouds (from external scheme)
            REAL(KIND=r8) :: mnudep(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nnudep(mgncol,nlev) ! number concentration
            ! ice multiplication
            REAL(KIND=r8) :: msacwi(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nsacwi(mgncol,nlev) ! number concentration
            ! autoconversion of cloud droplets
            REAL(KIND=r8) :: prc(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nprc(mgncol,nlev) ! number concentration (rain)
            REAL(KIND=r8) :: nprc1(mgncol,nlev) ! number concentration (cloud droplets)
            ! self-aggregation of snow
            REAL(KIND=r8) :: nsagg(mgncol,nlev) ! number concentration
            ! self-collection of rain
            REAL(KIND=r8) :: nragg(mgncol,nlev) ! number concentration
            ! collection of droplets by snow
            REAL(KIND=r8) :: psacws(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: npsacws(mgncol,nlev) ! number concentration
            ! collection of rain by snow
            REAL(KIND=r8) :: pracs(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: npracs(mgncol,nlev) ! number concentration
            ! freezing of rain
            REAL(KIND=r8) :: mnuccr(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nnuccr(mgncol,nlev) ! number concentration
            ! freezing of rain to form ice (mg add 4/26/13)
            REAL(KIND=r8) :: mnuccri(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nnuccri(mgncol,nlev) ! number concentration
            ! accretion of droplets by rain
            REAL(KIND=r8) :: pra(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: npra(mgncol,nlev) ! number concentration
            ! autoconversion of cloud ice to snow
            REAL(KIND=r8) :: prci(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nprci(mgncol,nlev) ! number concentration
            ! accretion of cloud ice by snow
            REAL(KIND=r8) :: prai(mgncol,nlev) ! mass mixing ratio
            REAL(KIND=r8) :: nprai(mgncol,nlev) ! number concentration
            ! evaporation of rain
            REAL(KIND=r8) :: pre(mgncol,nlev) ! mass mixing ratio
            ! sublimation of snow
            REAL(KIND=r8) :: prds(mgncol,nlev) ! mass mixing ratio
            ! number evaporation
            REAL(KIND=r8) :: nsubi(mgncol,nlev) ! cloud ice
            REAL(KIND=r8) :: nsubc(mgncol,nlev) ! droplet
            REAL(KIND=r8) :: nsubs(mgncol,nlev) ! snow
            REAL(KIND=r8) :: nsubr(mgncol,nlev) ! rain
            ! bergeron process
            REAL(KIND=r8) :: berg(mgncol,nlev) ! mass mixing ratio (cloud ice)
            REAL(KIND=r8) :: bergs(mgncol,nlev) ! mass mixing ratio (snow)
            ! fallspeeds
            ! number-weighted
            REAL(KIND=r8) :: uns(mgncol,nlev) ! snow
            REAL(KIND=r8) :: unr(mgncol,nlev) ! rain
            ! air density corrected fallspeed parameters
            REAL(KIND=r8) :: arn(mgncol,nlev) ! rain
            REAL(KIND=r8) :: asn(mgncol,nlev) ! snow
            REAL(KIND=r8) :: acn(mgncol,nlev) ! cloud droplet
            REAL(KIND=r8) :: ain(mgncol,nlev) ! cloud ice
            ! Mass of liquid droplets used with external heterogeneous freezing.
            REAL(KIND=r8) :: mi0l(mgncol)
            ! saturation vapor pressures
            REAL(KIND=r8) :: esl(mgncol,nlev) ! liquid
            REAL(KIND=r8) :: esi(mgncol,nlev) ! ice
            REAL(KIND=r8) :: esn ! checking for RH after rain evap
            ! saturation vapor mixing ratios
            REAL(KIND=r8) :: qvl(mgncol,nlev) ! liquid
            REAL(KIND=r8) :: qvi(mgncol,nlev) ! ice
            REAL(KIND=r8) :: qvn ! checking for RH after rain evap

            REAL(KIND=r8) :: qvnA(mgncol), esnA(mgncol)
            ! relative humidity
            REAL(KIND=r8) :: relhum(mgncol,nlev)
            ! parameters for cloud water and cloud ice sedimentation calculations
            REAL(KIND=r8) :: fc(mgncol,nlev)
            REAL(KIND=r8) :: fnc(mgncol,nlev)
            REAL(KIND=r8) :: fi(mgncol,nlev)
            REAL(KIND=r8) :: fni(mgncol,nlev)
            REAL(KIND=r8) :: fr(mgncol,nlev)
            REAL(KIND=r8) :: fnr(mgncol,nlev)
            REAL(KIND=r8) :: fs(mgncol,nlev)
            REAL(KIND=r8) :: fns(mgncol,nlev)
            REAL(KIND=r8) :: faloutc(nlev)
            REAL(KIND=r8) :: faloutnc(nlev)
            REAL(KIND=r8) :: falouti(nlev)
            REAL(KIND=r8) :: faloutni(nlev)
            REAL(KIND=r8) :: faloutr(nlev)
            REAL(KIND=r8) :: faloutnr(nlev)
            REAL(KIND=r8) :: falouts(nlev)
            REAL(KIND=r8) :: faloutns(nlev)
            REAL(KIND=r8) :: faltndc
            REAL(KIND=r8) :: faltndnc
            REAL(KIND=r8) :: faltndi
            REAL(KIND=r8) :: faltndni
            REAL(KIND=r8) :: faltndqie
            REAL(KIND=r8) :: faltndqce
            REAL(KIND=r8) :: faltndr
            REAL(KIND=r8) :: faltndnr
            REAL(KIND=r8) :: faltnds
            REAL(KIND=r8) :: faltndns
            REAL(KIND=r8) :: rainrt(mgncol,nlev) ! rain rate for reflectivity calculation
            ! dummy variables
            REAL(KIND=r8) :: dum
            REAL(KIND=r8) :: dumA(mgncol)
            REAL(KIND=r8) :: dum1
            REAL(KIND=r8) :: dum2
            ! dummies for checking RH
            REAL(KIND=r8) :: qtmp
            REAL(KIND=r8) :: ttmp
            REAL(KIND=r8) :: qtmpA(mgncol), ttmpA(mgncol)
            ! dummies for conservation check
            REAL(KIND=r8) :: ratio
            REAL(KIND=r8) :: tmpfrz
            ! dummies for in-cloud variables
            REAL(KIND=r8) :: dumc(mgncol,nlev) ! qc
            REAL(KIND=r8) :: dumnc(mgncol,nlev) ! nc
            REAL(KIND=r8) :: dumi(mgncol,nlev) ! qi
            REAL(KIND=r8) :: dumni(mgncol,nlev) ! ni
            REAL(KIND=r8) :: dumr(mgncol,nlev) ! rain mixing ratio
            REAL(KIND=r8) :: dumnr(mgncol,nlev) ! rain number concentration
            REAL(KIND=r8) :: dums(mgncol,nlev) ! snow mixing ratio
            REAL(KIND=r8) :: dumns(mgncol,nlev) ! snow number concentration
            ! Array dummy variable
            REAL(KIND=r8) :: dum_2d(mgncol,nlev)
            REAL(KIND=r8) :: pdel_inv(mgncol,nlev)
            ! loop array variables
            ! "i" and "k" are column/level iterators for internal (MG) variables
            ! "n" is used for other looping (currently just sedimentation)
            INTEGER :: k
            INTEGER :: i
            INTEGER :: n
            ! number of sub-steps for loops over "n" (for sedimentation)
            INTEGER :: nstep
            INTEGER :: mdust
            INTEGER :: foo,bar,cnt
            LOGICAL :: condition(mgncol)
            INTEGER, parameter :: vthreshold = 4

            !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            ! default return error message
  errstring = ' '
  if (.not. (do_cldice .or. &
       (associated(tnd_qsnow) .and. associated(tnd_nsnow) .and. associated(re_ice)))) then
     errstring = "MG's native cloud ice processes are disabled, but &
          &no replacement values were passed in."
  end if
  if (use_hetfrz_classnuc .and. (.not. &
       (associated(frzimm) .and. associated(frzcnt) .and. associated(frzdep)))) then
     errstring = "External heterogeneous freezing is enabled, but the &
          &required tendencies were not all passed in."
  end if
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
                END WHERE 
     where (qi >= qsmall)
        icldm = 1._r8
     elsewhere
        icldm = mincld
                END WHERE 
     cldm = max(icldm, lcldm)
  else
                ! get cloud fraction, check for minimum
     cldm = max(cldn,mincld)
     lcldm = max(liqcldf,mincld)
     icldm = max(icecldf,mincld)
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
            !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            ! Get humidity and saturation vapor pressures
  do k=1,nlev
     call qsat_water(t(:,k), p(:,k), esl(:,k), qvl(:,k),mgncol)
                    ! make sure when above freezing that esi=esl, not active yet
     call qsat_ice(t(:,k), p(:,k), esi(:,k), qvi(:,k),mgncol)
  enddo
  where (t >= tmelt)
       esi = esl
       qvi = qvl
  end where
!     do i=1,mgncol
!        if (t(i,k) >= tmelt) then
!           esi(i,k)=esl(i,k)
!           qvi(i,k)=qvl(i,k)
!        else
!           call qsat_ice(t(i,k), p(i,k), esi(i,k), qvi(i,k))
!        end if
!     end do
!  end do
  relhum = q / max(qvl, qsmall)
            !===============================================
            ! set mtime here to avoid answer-changing
  mtime=deltat
            ! initialize microphysics output
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
            !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            ! droplet activation
            ! get provisional droplet number after activation. This is used for
            ! all microphysical process calculations, for consistency with update of
            ! droplet mass before microphysics
            ! calculate potential for droplet activation if cloud water is present
            ! tendency from activation (npccn) is read in from companion routine
            ! output activated liquid and ice (convert from #/kg -> #/m3)
            !--------------------------------------------------
  where (qc >= qsmall)
     nc = max(nc + npccn*deltat, 0._r8)
     ncal = nc*rho/lcldm ! sghan minimum in #/cm3 ! sghan minimum in #/cm3
  elsewhere
     ncal = 0._r8
            END WHERE 
  where (t < icenuct)
     ncai = naai*rho
  elsewhere
     ncai = 0._r8
            END WHERE 
            !===============================================
            ! ice nucleation if activated nuclei exist at t<-5C AND rhmini + 5%
            !-------------------------------------------------------
  if (do_cldice) then
     where (naai > 0._r8 .and. t < icenuct .and. &
          relhum*esl/esi > rhmini+0.05_r8)
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
                END WHERE 
  end if
            !=============================================================================
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
            !========================================================================
            ! for sub-columns cldm has already been set to 1 if cloud
            ! water or ice is present, so precip_frac will be correctly set below
            ! and nothing extra needs to be done here
  precip_frac = cldm
  micro_vert_loop: do k=1,nlev
!JMD The following appears loop invariant...  Maybe we can move this out and calculate only once
     if (trim(micro_mg_precip_frac_method) == 'in_cloud') then
        if (k /= 1) then
           where (qc(:,k) < qsmall .and. qi(:,k) < qsmall)
              precip_frac(:,k) = precip_frac(:,k-1)
                        END WHERE 
        endif
     else if (trim(micro_mg_precip_frac_method) == 'max_overlap') then
                    ! calculate precip fraction based on maximum overlap assumption
                    ! if rain or snow mix ratios are smaller than threshold,
                    ! then leave precip_frac as cloud fraction at current level
        if (k /= 1) then
           where (qr(:,k-1) >= qsmall .or. qs(:,k-1) >= qsmall)
              precip_frac(:,k)=max(precip_frac(:,k-1),precip_frac(:,k))
                        END WHERE 
        end if
     endif
                    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                    ! get size distribution parameters based on in-cloud cloud water
                    ! these calculations also ensure consistency between number and mixing ratio
                    !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                    ! cloud liquid
                    !-------------------------------------------
     call size_dist_param_liq(mg_liq_props, qcic(:,k), ncic(:,k), rho(:,k), &
          pgam(:,k), lamc(:,k), mgncol)
                !========================================================================
                ! autoconversion of cloud liquid water to rain
                ! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
                ! minimum qc of 1 x 10^-8 prevents floating point error
     call kk2000_liq_autoconversion(microp_uniform, qcic(:,k), &
          ncic(:,k), rho(:,k), relvar(:,k), prc(:,k), nprc(:,k), nprc1(:,k), mgncol)
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
                END WHERE 
                ! make sure number concentration is a positive number to avoid
                ! taking root of negative later
     nric(:,k)=max(nric(:,k),0._r8)
                ! Get size distribution parameters for cloud ice
     call size_dist_param_basic_vec(mg_ice_props, qiic(:,k), niic(:,k), &
          lami(:,k), mgncol, n0=n0i(:,k))
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
                END WHERE 
                ! make sure number concentration is a positive number to avoid
                ! taking root of negative later
     nsic(:,k)=max(nsic(:,k),0._r8)
                !.......................................................................
                ! get size distribution parameters for precip
                !......................................................................
                ! rain
     call size_dist_param_basic_vec(mg_rain_props, qric(:,k), nric(:,k), &
          lamr(:,k), mgncol, n0=n0r(:,k))
     where (lamr(:,k) >= qsmall)
                    ! provisional rain number and mass weighted mean fallspeed (m/s)
        unr(:,k) = min(arn(:,k)*gamma_br_plus1/lamr(:,k)**br,9.1_r8*rhof(:,k))
        umr(:,k) = min(arn(:,k)*gamma_br_plus4/(6._r8*lamr(:,k)**br),9.1_r8*rhof(:,k))
     elsewhere
        umr(:,k) = 0._r8
        unr(:,k) = 0._r8
                END WHERE 
                !......................................................................
                ! snow
     call size_dist_param_basic_vec(mg_snow_props, qsic(:,k), nsic(:,k), &
          lams(:,k), mgncol, n0=n0s(:,k))
     where (lams(:,k) > 0._r8)
                    ! provisional snow number and mass weighted mean fallspeed (m/s)
        ums(:,k) = min(asn(:,k)*gamma_bs_plus4/(6._r8*lams(:,k)**bs),1.2_r8*rhof(:,k))
        uns(:,k) = min(asn(:,k)*gamma_bs_plus1/lams(:,k)**bs,1.2_r8*rhof(:,k))
     elsewhere
        ums(:,k) = 0._r8
        uns(:,k) = 0._r8
                END WHERE 
     if (do_cldice) then
        if (.not. use_hetfrz_classnuc) then
                        ! heterogeneous freezing of cloud water
                        !----------------------------------------------
           call immersion_freezing(microp_uniform, t(:,k), pgam(:,k), lamc(:,k), &
                qcic(:,k), ncic(:,k), relvar(:,k), mnuccc(:,k), nnuccc(:,k), mgncol)
                        ! make sure number of droplets frozen does not exceed available ice nuclei concentration
                        ! this prevents 'runaway' droplet freezing
           where (qcic(:,k).ge.qsmall .and. t(:,k).lt.269.15_r8)
              where (nnuccc(:,k)*lcldm(:,k).gt.nnuccd(:,k))
                                ! scale mixing ratio of droplet freezing with limit
                 mnuccc(:,k)=mnuccc(:,k)*(nnuccd(:,k)/(nnuccc(:,k)*lcldm(:,k)))
                 nnuccc(:,k)=nnuccd(:,k)/lcldm(:,k)
                            END WHERE 
                        END WHERE 
           mdust = size(rndst,3)
           call contact_freezing(microp_uniform, t(:,k), p(:,k), rndst(:,k,:), &
                nacon(:,k,:), pgam(:,k), lamc(:,k), qcic(:,k), ncic(:,k), &
                relvar(:,k), mnucct(:,k), nnucct(:,k), mgncol, mdust)
           mnudep(:,k)=0._r8
           nnudep(:,k)=0._r8
        else
                        ! Mass of droplets frozen is the average droplet mass, except
                        ! with two limiters: concentration must be at least 1/cm^3, and
                        ! mass must be at least the minimum defined above.
           mi0l = qcic(:,k)/max(ncic(:,k), 1.0e6_r8/rho(:,k))
           mi0l = max(mi0l_min, mi0l)
           where (qcic(:,k) >= qsmall)
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
                        END WHERE 
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
          qcic(:,k), ncic(:,k), qsic(:,k), pgam(:,k), lamc(:,k), lams(:,k), n0s(:,k), &
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
     call accrete_cloud_water_rain(microp_uniform, qric(:,k), qcic(:,k), &
          ncic(:,k), relvar(:,k), accre_enhan(:,k), pra(:,k), npra(:,k), mgncol)
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
          lcldm(:,k), precip_frac(:,k), arn(:,k), asn(:,k), qcic(:,k), qiic(:,k), &
          qric(:,k), qsic(:,k), lamr(:,k), n0r(:,k), lams(:,k), n0s(:,k), &
          pre(:,k), prds(:,k), mgncol)
     call bergeron_process_snow(t(:,k), rho(:,k), dv(:,k), mu(:,k), sc(:,k), &
          qvl(:,k), qvi(:,k), asn(:,k), qcic(:,k), qsic(:,k), lams(:,k), n0s(:,k), &
          bergs(:,k), mgncol)
     bergs(:,k)=bergs(:,k)*micro_mg_berg_eff_factor
                !+++PMC 12/3/12 - NEW VAPOR DEP/SUBLIMATION GOES HERE!!!
     if (do_cldice) then
        call ice_deposition_sublimation(t(:,k), q(:,k), qi(:,k), ni(:,k), &
             icldm(:,k), rho(:,k), dv(:,k), qvl(:,k), qvi(:,k), &
             berg(:,k), vap_dep(:,k), ice_sublim(:,k), mgncol)
        berg(:,k)=berg(:,k)*micro_mg_berg_eff_factor
        where (vap_dep(:,k) < 0._r8 .and. qi(:,k) > qsmall .and. icldm(:,k) > mincld)
           nsubi(:,k) = vap_dep(:,k) / qi(:,k) * ni(:,k) / icldm(:,k)
        elsewhere
           nsubi(:,k) = 0._r8
                    END WHERE 
                    ! bergeron process should not reduce nc unless
                    ! all ql is removed (which is handled elsewhere)
                    !in fact, nothing in this entire file makes nsubc nonzero.
        nsubc(:,k) = 0._r8
     end if !do_cldice !do_cldice
                !---PMC 12/3/12
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
     do i=1,mgncol
        dum = ((-nsubr(i,k)+npracs(i,k)+nnuccr(i,k)+nnuccri(i,k)-nragg(i,k))*precip_frac(i,k)- &
             nprc(i,k)*lcldm(i,k))*deltat
        if (dum.gt.nr(i,k)) then
           ratio = (nr(i,k)/deltat+nprc(i,k)*lcldm(i,k)/precip_frac(i,k))/ &
                (-nsubr(i,k)+npracs(i,k)+nnuccr(i,k)+nnuccri(i,k)-nragg(i,k))*omsm
           nragg(i,k)=nragg(i,k)*ratio
           npracs(i,k)=npracs(i,k)*ratio
           nnuccr(i,k)=nnuccr(i,k)*ratio
           nsubr(i,k)=nsubr(i,k)*ratio
           nnuccri(i,k)=nnuccri(i,k)*ratio
        end if
     end do
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
     call qsat_water(ttmpA, p(:,k), esnA, qvnA,mgncol)
                        ! modify ice/precip evaporation rate if q > qsat
     do i=1,mgncol
        if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
           if (qtmpA(i) > qvnA(i)) then
              dum1=pre(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
              dum2=prds(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
                            ! recalculate q and t after vap_dep and mnuccd but without evap or sublim
              qtmpA(i)=q(i,k)-(vap_dep(i,k)+mnuccd(i,k))*deltat
              ttmpA(i)=t(i,k)+((vap_dep(i,k)+mnuccd(i,k))*xxls)*deltat/cpp
                            ! use rhw to allow ice supersaturation
              call qsat_water(ttmpA(i), p(i,k), esnA(i), qvnA(i))
              dum=(qtmpA(i)-qvnA(i))/(1._r8 + xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
              dum=min(dum,0._r8)
                            ! modify rates if needed, divide by precip_frac to get local (in-precip) value
              pre(i,k)=dum*dum1/deltat/precip_frac(i,k)
                            ! do separately using RHI for prds and ice_sublim
              call qsat_ice(ttmpA(i), p(i,k), esnA(i), qvnA(i))
              dum=(qtmpA(i)-qvnA(i))/(1._r8 + xxls_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
              dum=min(dum,0._r8)
                            ! modify rates if needed, divide by precip_frac to get local (in-precip) value
              prds(i,k) = dum*dum2/deltat/precip_frac(i,k)
                            ! don't divide ice_sublim by cloud fraction since it is grid-averaged
              dum1 = (1._r8-dum1-dum2)
              ice_sublim(i,k) = dum*dum1/deltat
           end if
        end if
     end do
                ! Big "administration" loop enforces conservation, updates variables
                ! that accumulate over substeps, and sets output variables.
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
                ! End of "administration" loop
  end do micro_vert_loop ! end k loop ! end k loop
            !-----------------------------------------------------
            ! convert rain/snow q and N for output to history, note,
            ! output is for gridbox average
  qrout = qr
  nrout = nr * rho
  qsout = qs
  nsout = ns * rho
            ! calculate precip fluxes
            ! calculate the precip flux (kg/m2/s) as mixingratio(kg/kg)*airdensity(kg/m3)*massweightedfallspeed(m/s)
            ! ---------------------------------------------------------------------
  rflx(:,2:) = rflx(:,2:) + (qric*rho*umr*precip_frac)
  sflx(:,2:) = sflx(:,2:) + (qsic*rho*ums*precip_frac)
            ! calculate n0r and lamr from rain mass and number
            ! divide by precip fraction to get in-precip (local) values of
            ! rain mass and number, divide by rhow to get rain number in kg^-1
  call size_dist_param_basic_vec(mg_rain_props, qric, nric, lamr, mgncol*nlev, n0=n0r)
            ! Calculate rercld
            ! calculate mean size of combined rain and cloud water
  do k=1,nlev
  call calc_rercld(lamr(:,k), n0r(:,k), lamc(:,k), pgam(:,k), qric(:,k), qcic(:,k), ncic(:,k), &
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
                    ! obtain new slope parameter to avoid possible singularity
  call size_dist_param_basic_vec(mg_ice_props, dumi, dumni, lami, mgncol*nlev)
  do k=1,nlev
        call size_dist_param_liq(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), &
             pgam(:,k), lamc(:,k), mgncol)
  enddo
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
        else
           fi(i,k) = 0._r8
           fni(i,k)= 0._r8
        end if
     enddo
  enddo
  ! fallspeed for rain
  call size_dist_param_basic_vec(mg_rain_props, dumr, dumnr, lamr, mgncol*nlev)
  ! fallspeed for snow
  call size_dist_param_basic_vec(mg_snow_props, dums, dumns, lams, mgncol*nlev)
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
  end do 
  do k=1,nlev
     do i=1,mgncol
       pdel_inv(i,k) = 1._r8/pdel(i,k)
     enddo
  enddo

                ! initialize nstep for sedimentation sub-steps
                ! calculate number of split time steps to ensure courant stability criteria
                ! for sedimentation calculations
                !-------------------------------------------------------------------
  do i=1,mgncol
     nstep = 1 + int(max( &
          maxval( fi(i,:)*pdel_inv(i,:)), &
          maxval(fni(i,:)*pdel_inv(i,:))) &
          * deltat)
                ! loop over sedimentation sub-time step to ensure stability
                !==============================================================
     do n = 1,nstep
        if (do_cldice) then
           falouti  = fi(i,:)  * dumi(i,:)
           faloutni = fni(i,:) * dumni(i,:)
        else
           falouti  = 0._r8
           faloutni = 0._r8
        end if
                    ! top of model
        k = 1
                    ! add fallout terms to microphysical tendencies
        faltndi = falouti(k)/pdel(i,k)
        faltndni = faloutni(k)/pdel(i,k)
        qitend(i,k) = qitend(i,k)-faltndi/nstep
        nitend(i,k) = nitend(i,k)-faltndni/nstep
                    ! sedimentation tendency for output
        qisedten(i,k)=qisedten(i,k)-faltndi/nstep
        dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
        dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
        do k = 2,nlev
                        ! for cloud liquid and ice, if cloud fraction increases with height
                        ! then add flux from above to both vapor and cloud water of current level
                        ! this means that flux entering clear portion of cell from above evaporates
                        ! instantly
                        ! note: this is not an issue with precip, since we assume max overlap
           dum1=icldm(i,k)/icldm(i,k-1)
           dum1=min(dum1,1._r8)
           faltndqie=(falouti(k)-falouti(k-1))/pdel(i,k)
           faltndi=(falouti(k)-dum1*falouti(k-1))/pdel(i,k)
           faltndni=(faloutni(k)-dum1*faloutni(k-1))/pdel(i,k)
                        ! add fallout terms to eulerian tendencies
           qitend(i,k) = qitend(i,k)-faltndi/nstep
           nitend(i,k) = nitend(i,k)-faltndni/nstep
                        ! sedimentation tendency for output
           qisedten(i,k)=qisedten(i,k)-faltndi/nstep
                        ! add terms to to evap/sub of cloud water
           qvlat(i,k)=qvlat(i,k)-(faltndqie-faltndi)/nstep
                        ! for output
           qisevap(i,k)=qisevap(i,k)-(faltndqie-faltndi)/nstep
           tlat(i,k)=tlat(i,k)+(faltndqie-faltndi)*xxls/nstep
           dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
           dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
        end do
                    ! units below are m/s
                    ! sedimentation flux at surface is added to precip flux at surface
                    ! to get total precip (cloud + precip water) rate
        prect(i) = prect(i)+falouti(nlev)/g/real(nstep)/1000._r8
        preci(i) = preci(i)+falouti(nlev)/g/real(nstep)/1000._r8
     end do
                ! calculate number of split time steps to ensure courant stability criteria
                ! for sedimentation calculations
                !-------------------------------------------------------------------
     nstep = 1 + int(max( &
          maxval( fc(i,:)*pdel_inv(i,:)), &
          maxval(fnc(i,:)*pdel_inv(i,:))) &
          * deltat)
                ! loop over sedimentation sub-time step to ensure stability
                !==============================================================
     do n = 1,nstep
        faloutc  = fc(i,:)  * dumc(i,:)
        faloutnc = fnc(i,:) * dumnc(i,:)
                    ! top of model
        k = 1
                    ! add fallout terms to microphysical tendencies
        faltndc = faloutc(k)/pdel(i,k)
        faltndnc = faloutnc(k)/pdel(i,k)
        qctend(i,k) = qctend(i,k)-faltndc/nstep
        nctend(i,k) = nctend(i,k)-faltndnc/nstep
                    ! sedimentation tendency for output
        qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
        dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
        dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep
        do k = 2,nlev
           dum=lcldm(i,k)/lcldm(i,k-1)
           dum=min(dum,1._r8)
           faltndqce=(faloutc(k)-faloutc(k-1))/pdel(i,k)
           faltndc=(faloutc(k)-dum*faloutc(k-1))/pdel(i,k)
           faltndnc=(faloutnc(k)-dum*faloutnc(k-1))/pdel(i,k)
                        ! add fallout terms to eulerian tendencies
           qctend(i,k) = qctend(i,k)-faltndc/nstep
           nctend(i,k) = nctend(i,k)-faltndnc/nstep
                        ! sedimentation tendency for output
           qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
                        ! add terms to to evap/sub of cloud water
           qvlat(i,k)=qvlat(i,k)-(faltndqce-faltndc)/nstep
                        ! for output
           qcsevap(i,k)=qcsevap(i,k)-(faltndqce-faltndc)/nstep
           tlat(i,k)=tlat(i,k)+(faltndqce-faltndc)*xxlv/nstep
           dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
           dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep
        end do
        prect(i) = prect(i)+faloutc(nlev)/g/real(nstep)/1000._r8
     end do
                ! calculate number of split time steps to ensure courant stability criteria
                ! for sedimentation calculations
                !-------------------------------------------------------------------
     nstep = 1 + int(max( &
          maxval( fr(i,:)*pdel_inv(i,:)), &
          maxval(fnr(i,:)*pdel_inv(i,:))) &
          * deltat)
                ! loop over sedimentation sub-time step to ensure stability
                !==============================================================
     do n = 1,nstep
        faloutr  = fr(i,:)  * dumr(i,:)
        faloutnr = fnr(i,:) * dumnr(i,:)
                    ! top of model
        k = 1
                    ! add fallout terms to microphysical tendencies
        faltndr = faloutr(k)/pdel(i,k)
        faltndnr = faloutnr(k)/pdel(i,k)
        qrtend(i,k) = qrtend(i,k)-faltndr/nstep
        nrtend(i,k) = nrtend(i,k)-faltndnr/nstep
                    ! sedimentation tendency for output
        qrsedten(i,k)=qrsedten(i,k)-faltndr/nstep
        dumr(i,k) = dumr(i,k)-faltndr*deltat/real(nstep)
        dumnr(i,k) = dumnr(i,k)-faltndnr*deltat/real(nstep)
        do k = 2,nlev
           faltndr=(faloutr(k)-faloutr(k-1))/pdel(i,k)
           faltndnr=(faloutnr(k)-faloutnr(k-1))/pdel(i,k)
                        ! add fallout terms to eulerian tendencies
           qrtend(i,k) = qrtend(i,k)-faltndr/nstep
           nrtend(i,k) = nrtend(i,k)-faltndnr/nstep
                        ! sedimentation tendency for output
           qrsedten(i,k)=qrsedten(i,k)-faltndr/nstep
           dumr(i,k) = dumr(i,k)-faltndr*deltat/real(nstep)
           dumnr(i,k) = dumnr(i,k)-faltndnr*deltat/real(nstep)
        end do
        prect(i) = prect(i)+faloutr(nlev)/g/real(nstep)/1000._r8
     end do
                ! calculate number of split time steps to ensure courant stability criteria
                ! for sedimentation calculations
                !-------------------------------------------------------------------
     nstep = 1 + int(max( &
          maxval( fs(i,:)*pdel_inv(i,:)), &
          maxval(fns(i,:)*pdel_inv(i,:))) &
          * deltat)
                ! loop over sedimentation sub-time step to ensure stability
                !==============================================================
     do n = 1,nstep
        falouts  = fs(i,:)  * dums(i,:)
        faloutns = fns(i,:) * dumns(i,:)
                    ! top of model
        k = 1
                    ! add fallout terms to microphysical tendencies
        faltnds = falouts(k)/pdel(i,k)
        faltndns = faloutns(k)/pdel(i,k)
        qstend(i,k) = qstend(i,k)-faltnds/nstep
        nstend(i,k) = nstend(i,k)-faltndns/nstep
                    ! sedimentation tendency for output
        qssedten(i,k)=qssedten(i,k)-faltnds/nstep
           dums(i,k) = dums(i,k)-faltnds*deltat/real(nstep)
           dumns(i,k) = dumns(i,k)-faltndns*deltat/real(nstep)
        do k = 2,nlev
           faltnds=(falouts(k)-falouts(k-1))/pdel(i,k)
           faltndns=(faloutns(k)-faloutns(k-1))/pdel(i,k)
                        ! add fallout terms to eulerian tendencies
           qstend(i,k) = qstend(i,k)-faltnds/nstep
           nstend(i,k) = nstend(i,k)-faltndns/nstep
                        ! sedimentation tendency for output
           qssedten(i,k)=qssedten(i,k)-faltnds/nstep
           dums(i,k) = dums(i,k)-faltnds*deltat/real(nstep)
           dumns(i,k) = dumns(i,k)-faltndns*deltat/real(nstep)
        end do   !! k loop !! k loop
        prect(i) = prect(i)+falouts(nlev)/g/real(nstep)/1000._r8
        preci(i) = preci(i)+falouts(nlev)/g/real(nstep)/1000._r8
     end do   !! nstep loop !! nstep loop
   enddo
                ! end sedimentation
                !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                ! get new update for variables that includes sedimentation tendency
                ! note : here dum variables are grid-average, NOT in-cloud
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
                    ! calculate instantaneous processes (melting, homogeneous freezing)
                    !====================================================================
                    ! melting of snow at +2 C
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
   call size_dist_param_basic_vec(mg_rain_props, dumr, dumnr, lamr,mgncol*nlev)
   do k=1,nlev
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
              ! call size_dist_param_basic(mg_rain_props, dumr(i,k), dumnr(i,k), lamr(i,k))
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
   enddo
        if (do_cldice) then
!        print *,'do_cldice: ',do_cldice
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
   dum_2D = dumni
   call size_dist_param_basic_vec(mg_ice_props, dumi, dumni, lami,mgncol*nlev)
   do k=1,nlev
     do i=1,mgncol
           if (dumi(i,k).ge.qsmall) then
 !             dum_2D(i,k) = dumni(i,k)
 !             call size_dist_param_basic(mg_ice_props, dumi(i,k), dumni(i,k), lami(i,k))
              if (dumni(i,k) /=dum_2D(i,k)) then
                                ! adjust number conc if needed to keep mean size in reasonable range
                 nitend(i,k)=(dumni(i,k)*icldm(i,k)-ni(i,k))/deltat
              end if
              effi(i,k) = 1.5_r8/lami(i,k)*1.e6_r8
           else
              effi(i,k) = 25._r8
           end if
                        ! ice effective diameter for david mitchell's optics
           deffi(i,k)=effi(i,k)*rhoi/rhows*2._r8
     enddo
   enddo
        else
   do k=1,nlev
     do i=1,mgncol
                        ! NOTE: If CARMA is doing the ice microphysics, then the ice effective
                        ! radius has already been determined from the size distribution.
           effi(i,k) = re_ice(i,k) * 1.e6_r8      ! m -> um ! m -> um
           deffi(i,k)=effi(i,k) * 2._r8
     enddo
   enddo
        end if
                    ! cloud droplet effective radius
                    !-----------------------------------------------------------------
   do k=1,nlev
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
           dum = dumnc(i,k)
           call size_dist_param_liq(mg_liq_props, dumc(i,k), dumnc(i,k), rho(i,k), &
                pgam(i,k), lamc(i,k))
           if (dum /= dumnc(i,k)) then
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
           call size_dist_param_liq(mg_liq_props, dumc(i,k), dumnc(i,k), rho(i,k), &
                pgam(i,k), lamc(i,k))
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
   dum_2D = dumnr
   call size_dist_param_basic_vec(mg_rain_props, dumr, dumnr, lamr,mgncol*nlev)
   do k=1,nlev
     do i=1,mgncol
        if (dumr(i,k).ge.qsmall) then
!           dum_2D(i,k) = dumnr(i,k)
!           call size_dist_param_basic(mg_rain_props, dumr(i,k), dumnr(i,k), lamr(i,k))
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
   call size_dist_param_basic_vec(mg_snow_props, dums, dumns,lams,mgncol*nlev)
   do k=1,nlev
     do i=1,mgncol
        if (dums(i,k).ge.qsmall) then
!           dum_2D(i,k) = dumns(i,k)
!           call size_dist_param_basic(mg_snow_props, dums(i,k), dumns(i,k),lams(i,k))
           if (dum_2D(i,k) /= dumns(i,k)) then
                            ! adjust number conc if needed to keep mean size in reasonable range
              nstend(i,k)=(dumns(i,k)*precip_frac(i,k)-ns(i,k))/deltat
           end if
        end if
     enddo
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
  
#if 0
  call avg_diameter(qrout, nrout, rho, rhow, drout2,mgncol*nlev)
  do k=1,nlev
     foo = foo + COUNT(qrout(:,k) .gt. 1.e-7_r8 .and. nrout(:,k).gt.0._r8)
     bar = bar + mgncol
  enddo
  print *,'Fraction that 1st where is true: ',real(foo)/real(bar)
#endif
   
  qrout2 = 0._r8
  nrout2 = 0._r8
  drout2 = 0._r8
  freqr = 0._r8
  reff_rain = 0._r8
  where (qrout .gt. 1.e-7_r8 &
       .and. nrout.gt.0._r8)
     qrout2 = qrout * precip_frac
     nrout2 = nrout * precip_frac
                ! The avg_diameter call does the actual calculation; other diameter
                ! outputs are just drout2 times constants.
     drout2 = avg_diameter_func(qrout, nrout, rho, rhow)
     freqr = precip_frac
     reff_rain=1.5_r8*drout2*1.e6_r8
  END WHERE 

  dsout  = 0._r8
  qsout2 = 0._r8
  nsout2 = 0._r8
  dsout2 = 0._r8
  freqs  = 0._r8
  reff_snow=0._r8
  do k=1,nlev
     condition = (qsout(:,k) .gt. 1.e-7_r8 .and. nsout(:,k).gt.0._r8)
     cnt = COUNT(condition)
     ! Calculate drout2 where necesary using either a vector or a scalar code
     if (cnt > 0) then 
       if (cnt >= vthreshold) then 
          call avg_diameter(qsout(:,k), nsout(:,k), rho(:,k), rhosn, dsout2(:,k),mgncol)
       else
          do i=1,mgncol
             if(condition(i)) then 
               dsout2(i,k) = avg_diameter_func(qsout(i,k), nsout(i,k), rho(i,k), rhosn)
             endif
          enddo
       endif
       where (condition)
          qsout2(:,k) = qsout(:,k) * precip_frac(:,k)
          nsout2(:,k) = nsout(:,k) * precip_frac(:,k)
             ! The avg_diameter call does the actual calculation; other diameter
             ! outputs are just dsout2 times constants.
          freqs(:,k) = precip_frac(:,k)
          dsout(:,k)=3._r8*rhosn/rhows*dsout2(:,k)
          reff_snow(:,k)=1.5_r8*dsout2(:,k)*1.e6_r8
       END WHERE 
     endif
  enddo
            ! analytic radar reflectivity
            !--------------------------------------------------
            ! formulas from Matthew Shupe, NOAA/CERES
            ! *****note: radar reflectivity is local (in-precip average)
            ! units of mm^6/m^3
  do i = 1,mgncol
     do k=1,nlev
        if (qc(i,k).ge.qsmall) then
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
            !redefine fice here....
  dum_2D = qsout + qrout + qc + qi
  dumi = qsout + qi
  where (dumi .gt. qsmall .and. dum_2D .gt. qsmall)
     nfice=min(dumi/dum_2D,1._r8)
  elsewhere
     nfice=0._r8
            END WHERE 
        END SUBROUTINE micro_mg_tend
        !========================================================================
        !OUTPUT CALCULATIONS
        !========================================================================

        SUBROUTINE calc_rercld(lamr, n0r, lamc, pgam, qric, qcic, ncic, rercld, mgncol)
            INTEGER, intent(in) :: mgncol
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamr ! rain size parameter (slope)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0r ! rain size parameter (intercept)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamc ! size distribution parameter (slope)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: pgam ! droplet size parameter
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qric ! in-cloud rain mass mixing ratio
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic ! in-cloud cloud liquid
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ncic ! in-cloud droplet number concentration
            REAL(KIND=r8), dimension(mgncol), intent(inout) :: rercld ! effective radius calculation for rain + cloud
            ! combined size of precip & cloud drops
            REAL(KIND=r8) :: atmp
            REAL(KIND=r8) :: tmp
            INTEGER :: i
  do i=1,mgncol
            ! Rain drops
  if (lamr(i) > 0._r8) then
     Atmp = n0r(i) * pi / (2._r8 * lamr(i)**3._r8)
  else
     Atmp = 0._r8
  end if
            ! Add cloud drops
  if (lamc(i) > 0._r8) then
     ! Atmp = Atmp + ncic(i) * pi * rising_factorial(pgam(i)+1._r8, 2)/(4._r8 * lamc(i)**2._r8)
     call rising_factorial(pgam(i)+1._r8, 2,tmp)
     Atmp = Atmp + ncic(i) * pi * tmp/(4._r8 * lamc(i)**2._r8)
  end if
  if (Atmp > 0._r8) then
     rercld(i) = rercld(i) + 3._r8 *(qric(i) + qcic(i)) / (4._r8 * rhow * Atmp)
  end if
  enddo
        END SUBROUTINE calc_rercld
        !========================================================================
        !UTILITIES
        !========================================================================

    END MODULE micro_mg2_0
