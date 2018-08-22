!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  


module micro_mg_utils
!--------------------------------------------------------------------------
! This module contains process rates and utility functions used by the MG
! microphysics.
! Original MG authors: Andrew Gettelman, Hugh Morrison
! Contributions from: Peter Caldwell, Xiaohong Liu and Steve Ghan
! Separated from MG 1.5 by B. Eaton.
! Separated module switched to MG 2.0 and further changes by S. Santos.
! for questions contact Hugh Morrison, Andrew Gettelman
! e-mail: morrison@ucar.edu, andrew@ucar.edu
!--------------------------------------------------------------------------
! List of required external functions that must be supplied:
!   gamma --> standard mathematical gamma function (if gamma is an
!       intrinsic, define HAVE_GAMMA_INTRINSICS)
!--------------------------------------------------------------------------
! Constants that must be specified in the "init" method (module variables):
! kind            kind of reals (to verify correct linkage only) -
! gravit          acceleration due to gravity                    m s-2
! rair            dry air gas constant for air                   J kg-1 K-1
! rh2o            gas constant for water vapor                   J kg-1 K-1
! cpair           specific heat at constant pressure for dry air J kg-1 K-1
! tmelt           temperature of melting point for water         K
! latvap          latent heat of vaporization                    J kg-1
! latice          latent heat of fusion                          J kg-1
!--------------------------------------------------------------------------

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
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

    PUBLIC size_dist_param_liq, size_dist_param_basic, avg_diameter, rising_factorial, ice_deposition_sublimation, &
    &sb2001v2_liq_autoconversion, sb2001v2_accre_cld_water_rain, kk2000_liq_autoconversion, ice_autoconversion, &
    &immersion_freezing, contact_freezing, snow_self_aggregation, accrete_cloud_water_snow, secondary_ice_production, &
    &accrete_rain_snow, heterogeneous_rain_freezing, accrete_cloud_water_rain, self_collection_rain, accrete_cloud_ice_snow, &
    &evaporate_sublimate_precip, bergeron_process_snow 
    PUBLIC avg_diameter_vec
! 8 byte real and integer

integer, parameter, public :: r8 = selected_real_kind(12)
integer, parameter, public :: i8 = selected_int_kind(18)

PUBLIC mghydrometeorprops 

type :: MGHydrometeorProps
   ! Density (kg/m^3)
   real(r8) :: rho
   ! Information for size calculations.
   ! Basic calculation of mean size is:
   !     lambda = (shape_coef*nic/qic)^(1/eff_dim)
   ! Then lambda is constrained by bounds.
   real(r8) :: eff_dim
   real(r8) :: shape_coef
   real(r8) :: lambda_bounds(2)
   ! Minimum average particle mass (kg).
   ! Limit is applied at the beginning of the size distribution calculations.
   real(r8) :: min_mean_mass
end type MGHydrometeorProps


type(MGHydrometeorProps), public :: mg_liq_props
type(MGHydrometeorProps), public :: mg_ice_props
type(MGHydrometeorProps), public :: mg_rain_props
type(MGHydrometeorProps), public :: mg_snow_props

interface size_dist_param_liq
  module procedure size_dist_param_liq_vect
  module procedure size_dist_param_liq_line
end interface
interface size_dist_param_basic
  module procedure size_dist_param_basic_vect
  module procedure size_dist_param_basic_line
end interface

interface calc_ab
  module procedure calc_ab_r8
  module procedure calc_ab_v8
end interface

!=================================================
! Public module parameters (mostly for MG itself)
!=================================================
! Pi to 20 digits; more than enough to reach the limit of double precision.


real(r8), parameter, public :: pi = 3.14159265358979323846_r8
! "One minus small number": number near unity for round-off issues.

real(r8), parameter, public :: omsm   = 1._r8 - 1.e-5_r8
! Smallest mixing ratio considered in microphysics.

real(r8), parameter, public :: qsmall = 1.e-18_r8
! minimum allowed cloud fraction

real(r8), parameter, public :: mincld = 0.0001_r8

real(r8), parameter, public :: rhosn = 250._r8  ! bulk density snow
real(r8), parameter, public :: rhoi = 500._r8   ! bulk density ice
real(r8), parameter, public :: rhow = 1000._r8  ! bulk density liquid
real(r8), parameter, public :: rhows = 917._r8  ! bulk density water solid
! fall speed parameters, V = aD^b (V is in m/s)
! droplets

real(r8), parameter, public :: ac = 3.e7_r8
real(r8), parameter, public :: bc = 2._r8
! snow
real(r8), parameter, public :: as = 11.72_r8
real(r8), parameter, public :: bs = 0.41_r8
! cloud ice
real(r8), parameter, public :: ai = 700._r8
real(r8), parameter, public :: bi = 1._r8
! small cloud ice (r< 10 um) - sphere, bulk density
real(r8), parameter, public :: aj = ac*((rhoi/rhows)**(bc/3._r8))*rhows/rhow
real(r8), parameter, public :: bj = bc
! rain
real(r8), parameter, public :: ar = 841.99667_r8
real(r8), parameter, public :: br = 0.8_r8
! mass of new crystal due to aerosol freezing and growth (kg)
! Make this consistent with the lower bound, to support UTLS and
! stratospheric ice, and the smaller ice size limit.

real(r8), parameter, public :: mi0 = 4._r8/3._r8*pi*rhoi*(1.e-6_r8)**3
!=================================================
! Private module parameters
!=================================================
! Signaling NaN bit pattern that represents a limiter that's turned off.


integer(i8), parameter :: limiter_off = int(Z'7FF1111111111111', i8)
! alternate threshold used for some in-cloud mmr

real(r8), parameter :: icsmall = 1.e-8_r8
! particle mass-diameter relationship
! currently we assume spherical particles for cloud ice/snow
! m = cD^d
! exponent

! Bounds for mean diameter for different constituents.

! Minimum average mass of particles.

! ventilation parameters
! for snow

real(r8), parameter :: f1s = 0.86_r8
real(r8), parameter :: f2s = 0.28_r8
! for rain
real(r8), parameter :: f1r = 0.78_r8
real(r8), parameter :: f2r = 0.308_r8
! collection efficiencies
! aggregation of cloud ice and snow

real(r8), parameter :: eii = 0.5_r8
! immersion freezing parameters, bigg 1953

real(r8), parameter :: bimm = 100._r8
real(r8), parameter :: aimm = 0.66_r8
! Mass of each raindrop created from autoconversion.

real(r8), parameter :: droplet_mass_25um = 4._r8/3._r8*pi*rhow*(25.e-6_r8)**3
real(r8), parameter :: droplet_mass_40um = 4._r8/3._r8*pi*rhow*(40.e-6_r8)**3
!=========================================================
! Constants set in initialization
!=========================================================
! Set using arguments to micro_mg_init


real(r8) :: rv          ! water vapor gas constant
real(r8) :: cpp         ! specific heat of dry air
real(r8) :: tmelt       ! freezing point of water (K)
! latent heats of:

real(r8) :: xxlv        ! vaporization
real(r8) :: xxls        ! sublimation
! additional constants to help speed up code

real(r8) :: gamma_bs_plus3
real(r8) :: gamma_half_br_plus5
real(r8) :: gamma_half_bs_plus5
!=========================================================
! Utilities that are cheaper if the compiler knows that
! some argument is an integer.
!=========================================================


interface rising_factorial
   module procedure rising_factorial_r8
   module procedure rising_factorial_v8
   module procedure rising_factorial_int
   module procedure rising_factorial_vint
end interface rising_factorial

interface var_coef
   module procedure var_coef_r8
   module procedure var_coef_v8
   module procedure var_coef_int
   module procedure var_coef_vint
end interface var_coef
!==========================================================================
PUBLIC kr_externs_in_micro_mg_utils 
PUBLIC kr_externs_out_micro_mg_utils 
PUBLIC kr_micro_mg_utils_mghydrometeorprops 
PUBLIC kv_micro_mg_utils_mghydrometeorprops 

contains
!==========================================================================
! Initialize module variables.
! "kind" serves no purpose here except to check for unlikely linking
! issues; always pass in the kind for a double precision real.
! "errstring" is the only output; it is blank if there is no error, or set
! to a message if there is an error.
! Check the list at the top of this module for descriptions of all other
! arguments.

!
!
!

! Constructor for a constituent property object.


!========================================================================
!FORMULAS
!========================================================================
! Use gamma function to implement rising factorial extended to the reals.


subroutine  rising_factorial_r8(x, n, res)
  real(r8), intent(in)  :: x, n
  real(r8), intent(out) :: res

  res = gamma(x+n)/gamma(x)

end subroutine rising_factorial_r8

subroutine  rising_factorial_v8(x, n, res,vlen)
  integer, intent(in)   :: vlen
  real(r8), intent(in)  :: x(vlen), n
  real(r8), intent(out) :: res(vlen)
  integer :: i
  real(r8) :: tmp(vlen)

#if 0
  do i=1,vlen
    tmp(i) = x(i)+n
    res(i) = gamma(tmp(i))/gamma(x(i))
  enddo
#else
  tmp = x+n
  res = gamma(tmp)
  tmp = gamma(x)
  res = res/tmp
#endif

end subroutine rising_factorial_v8
! Rising factorial can be performed much cheaper if n is a small integer.

subroutine rising_factorial_int(x, n, res)
  real(r8), intent(in) :: x
  integer, intent(in) :: n
  real(r8), intent(out) :: res

  integer :: i
  real(r8) :: factor

  res = 1._r8
  factor = x

  do i = 1, n
     res = res * factor
     factor = factor + 1._r8
  end do

end subroutine rising_factorial_int

subroutine rising_factorial_vint(x, n, res,vlen)
  integer, intent(in) :: vlen
  real(r8), intent(in) :: x(vlen)
  integer, intent(in) :: n
  real(r8), intent(out) :: res(vlen)

  integer :: i,j
  real(r8) :: factor(vlen)

  res    = 1._r8
  factor = x

  if (n == 3) then 
    res    = res * factor
    factor = factor + 1._r8
    res    = res * factor
    factor = factor + 1._r8
    res    = res * factor
  elseif (n==2) then 
    res    = res * factor
    factor = factor + 1._r8
    res    = res * factor
  else
    do i = 1, n
       res = res * factor
       factor = factor + 1._r8
    enddo
  endif

end subroutine rising_factorial_vint

! Calculate correction due to latent heat for evaporation/sublimation
subroutine calc_ab_r8(t, qv, xxl,ab)
  real(r8), intent(in) :: t     ! Temperature
  real(r8), intent(in) :: qv    ! Saturation vapor pressure
  real(r8), intent(in) :: xxl   ! Latent heat

  real(r8), intent(out) :: ab

  real(r8) :: dqsdt

  dqsdt = xxl*qv / (rv * t**2)
  ab = 1._r8 + dqsdt*xxl/cpp

end subroutine  calc_ab_r8

! Calculate correction due to latent heat for evaporation/sublimation
subroutine calc_ab_v8(t, qv, xxl,ab,vlen)
  integer,  intent(in) :: vlen
  real(r8), intent(in) :: t(vlen)     ! Temperature
  real(r8), intent(in) :: qv(vlen)    ! Saturation vapor pressure
  real(r8), intent(in) :: xxl         ! Latent heat

  real(r8), intent(out) :: ab(vlen)
  real(r8) :: dqsdt(vlen)
  integer :: i

  do i=1,vlen
     dqsdt(i) = xxl*qv(i) / (rv * t(i)**2)
     ab(i) = 1._r8 + dqsdt(i)*xxl/cpp
  enddo

end subroutine  calc_ab_v8

! get cloud droplet size distribution parameters

subroutine size_dist_param_liq_line(props, qcic, ncic, rho, pgam, lamc)
  type(MGHydrometeorProps), intent(in) :: props
  real(r8), intent(in) :: qcic
  real(r8), intent(inout) :: ncic
  real(r8), intent(in) :: rho

  real(r8), intent(out) :: pgam
  real(r8), intent(out) :: lamc

  type(MGHydrometeorProps) :: props_loc
  real(r8) :: tmp

  if (qcic > qsmall) then
     ! Local copy of properties that can be modified.
     ! (Elemental routines that operate on arrays can't modify scalar
     ! arguments.)

     props_loc = props
     ! Get pgam from fit to observations of martin et al. 1994

     pgam = 1.0_r8 - 0.7_r8 * exp(-0.008_r8*1.e-6_r8*ncic*rho)
     pgam = 1._r8/(pgam**2) - 1._r8
     pgam = max(pgam, 2._r8)
     ! Set coefficient for use in size_dist_param_basic.
     ! The 3D case is so common and optimizable that we specialize it:

     if (props_loc%eff_dim == 3._r8) then
        call rising_factorial(pgam+1._r8, 3,tmp)
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
     else
        call rising_factorial(pgam+1._r8, props_loc%eff_dim,tmp)
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
     end if
     ! Limit to between 2 and 50 microns mean size.

     props_loc%lambda_bounds = (pgam+1._r8)*1._r8/[50.e-6_r8, 2.e-6_r8]

     call size_dist_param_basic(props_loc, qcic, ncic, lamc)

  else
     ! pgam not calculated in this case, so set it to a value likely to
     ! cause an error if it is accidentally used
     ! (gamma function undefined for negative integers)
     pgam = -100._r8
     lamc = 0._r8
  end if

end subroutine size_dist_param_liq_line
! get cloud droplet size distribution parameters


subroutine size_dist_param_liq_vect(props, qcic, ncic, rho, pgam, lamc, mgncol)

  type(mghydrometeorprops), intent(in) :: props
  integer,                          intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: qcic
  real(r8), dimension(mgncol), intent(inout) :: ncic
  real(r8), dimension(mgncol), intent(in) :: rho
  real(r8), dimension(mgncol), intent(out) :: pgam
  real(r8), dimension(mgncol), intent(out) :: lamc
  type(mghydrometeorprops) :: props_loc
  integer :: i, cnt
  real(r8) :: tmp(mgncol),pgamp1(mgncol)

  do i=1,mgncol
     if (qcic(i) > qsmall) then
        ! Local copy of properties that can be modified.
        ! (Elemental routines that operate on arrays can't modify scalar
        ! arguments.)
        props_loc = props
        ! Get pgam from fit to observations of martin et al. 1994
        pgam(i) = 1.0_r8 - 0.7_r8 * exp(-0.008_r8*1.e-6_r8*ncic(i)*rho(i))
        pgam(i) = 1._r8/(pgam(i)**2) - 1._r8
        pgam(i) = max(pgam(i), 2._r8)
        pgamp1(i) = pgam(i)+1._r8
     endif
  enddo
#if 0
  cnt = COUNT(qcic>qsmall)
  if(cnt>0) then 
     if (props_loc%eff_dim == 3._r8) then
        call rising_factorial(pgamp1,3,tmp,mgncol)
     else
        call rising_factorial(pgamp1, props_loc%eff_dim,tmp,mgncol)
     endif
  endif
  do i=1,mgncol
     if (qcic(i) > qsmall) then
#else
  do i=1,mgncol
     if (qcic(i) > qsmall) then
        if (props_loc%eff_dim == 3._r8) then
           call rising_factorial(pgam(i)+1._r8, 3,tmp(i))
        else
           call rising_factorial(pgam(i)+1._r8, props_loc%eff_dim,tmp(i))
        end if
#endif
        ! Set coefficient for use in size_dist_param_basic.
        ! The 3D case is so common and optimizable that we specialize
        ! it:
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp(i)
        ! Limit to between 2 and 50 microns mean size.
        props_loc%lambda_bounds(1) = (pgam(i)+1._r8)*1._r8/50.e-6_r8
        props_loc%lambda_bounds(2) = (pgam(i)+1._r8)*1._r8/2.e-6_r8
        call size_dist_param_basic(props_loc, qcic(i), ncic(i), lamc(i))
     endif
  enddo
  do i=1,mgncol
     if (qcic(i) <= qsmall) then
        ! pgam not calculated in this case, so set it to a value likely to
        ! cause an error if it is accidentally used
        ! (gamma function undefined for negative integers)
        pgam(i) = -100._r8
        lamc(i) = 0._r8
     end if
  enddo

end subroutine size_dist_param_liq_vect
! Basic routine for getting size distribution parameters.

elemental subroutine size_dist_param_basic_line(props, qic, nic, lam, n0)
  type(MGHydrometeorProps), intent(in) :: props
  real(r8), intent(in) :: qic
  real(r8), intent(inout) :: nic

  real(r8), intent(out) :: lam
  real(r8), intent(out), optional :: n0

  if (qic > qsmall) then
     ! add upper limit to in-cloud number concentration to prevent
     ! numerical error

     if (limiter_is_on(props%min_mean_mass)) then
        nic = min(nic, qic / props%min_mean_mass)
     end if
     ! lambda = (c n/q)^(1/d)

     lam = (props%shape_coef * nic/qic)**(1._r8/props%eff_dim)
     ! check for slope
     ! adjust vars

     if (lam < props%lambda_bounds(1)) then
        lam = props%lambda_bounds(1)
        nic = lam**(props%eff_dim) * qic/props%shape_coef
     else if (lam > props%lambda_bounds(2)) then
        lam = props%lambda_bounds(2)
        nic = lam**(props%eff_dim) * qic/props%shape_coef
     end if

  else
     lam = 0._r8
  end if

  if (present(n0)) n0 = nic * lam

end subroutine size_dist_param_basic_line

subroutine size_dist_param_basic_vect(props, qic, nic, lam, mgncol, n0)

  type (mghydrometeorprops), intent(in) :: props
  integer,                          intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: qic
  real(r8), dimension(mgncol), intent(inout) :: nic
  real(r8), dimension(mgncol), intent(out) :: lam
  real(r8), dimension(mgncol), intent(out), optional :: n0
  integer :: i
  integer :: cnt
!  cnt = count(qic > qsmall) 
!  print *,'param_basic: ',cnt,mgncol
!NEC$ IVDEP
  do i=1,mgncol

     if (qic(i) > qsmall) then
        ! add upper limit to in-cloud number concentration to prevent
        ! numerical error

        if (limiter_is_on(props%min_mean_mass)) then
           nic(i) = min(nic(i), qic(i) / props%min_mean_mass)
        end if
        ! lambda = (c n/q)^(1/d)

        lam(i) = (props%shape_coef * nic(i)/qic(i))**(1._r8/props%eff_dim)
        ! check for slope
        ! adjust vars

        if (lam(i) < props%lambda_bounds(1)) then
           lam(i) = props%lambda_bounds(1)
           nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
        else if (lam(i) > props%lambda_bounds(2)) then
           lam(i) = props%lambda_bounds(2)
           nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
        end if

     else
        lam(i) = 0._r8
     end if

  enddo

  if (present(n0)) n0 = nic * lam

end subroutine size_dist_param_basic_vect


real(r8) elemental function avg_diameter(q, n, rho_air, rho_sub)
  ! Finds the average diameter of particles given their density, and
  ! mass/number concentrations in the air.
  ! Assumes that diameter follows an exponential distribution.
  real(r8), intent(in) :: q         ! mass mixing ratio
  real(r8), intent(in) :: n         ! number concentration (per volume)
  real(r8), intent(in) :: rho_air   ! local density of the air
  real(r8), intent(in) :: rho_sub   ! density of the particle substance

  avg_diameter = (pi * rho_sub * n/(q*rho_air))**(-1._r8/3._r8)

end function avg_diameter

subroutine  avg_diameter_vec(q, n, rho_air, rho_sub, avg_diameter, vlen)
  ! Finds the average diameter of particles given their density, and
  ! mass/number concentrations in the air.
  ! Assumes that diameter follows an exponential distribution.
  integer,  intent(in) :: vlen
  real(r8), intent(in) :: q(vlen)         ! mass mixing ratio
  real(r8), intent(in) :: n(vlen)         ! number concentration (per volume)
  real(r8), intent(in) :: rho_air(vlen)   ! local density of the air
  real(r8), intent(in) :: rho_sub   ! density of the particle substance
  real(r8), intent(out) :: avg_diameter(vlen)
  integer :: i

  do i=1,vlen
      avg_diameter(i) = (pi * rho_sub * n(i)/(q(i)*rho_air(i)))**(-1._r8/3._r8)
  enddo

end subroutine avg_diameter_vec


subroutine var_coef_r8(relvar, a, res)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  real(r8), intent(in) :: relvar
  real(r8), intent(in) :: a
  real(r8), intent(out) :: res
  real(r8) :: tmp

  call rising_factorial(relvar, a,tmp)
  res = tmp / relvar**a

end subroutine var_coef_r8

subroutine var_coef_v8(relvar, a, res, vlen)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  integer :: vlen
  real(r8), intent(in) :: relvar(vlen)
  real(r8), intent(in) :: a
  real(r8), intent(out) :: res(vlen)
  integer :: i
  real(r8) :: tmpA(vlen)

   call rising_factorial(relvar,a,tmpA,vlen)
   do i=1,vlen
      res(i) = tmpA(i)/relvar(i)**a
   enddo

end subroutine var_coef_v8

subroutine var_coef_int(relvar, a, res)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  real(r8), intent(in) :: relvar
  integer, intent(in) :: a
  real(r8), intent(out) :: res
  real(r8) :: tmp

  call rising_factorial(relvar, a,tmp)
  res = tmp / relvar**a

end subroutine var_coef_int
subroutine var_coef_vint(relvar, a, res, vlen)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  integer, intent(in) :: vlen
  real(r8), intent(in) :: relvar(vlen)
  integer, intent(in) :: a
  real(r8), intent(out) :: res(vlen)
  integer :: i
  real(r8) :: tmp(vlen)
  call rising_factorial(relvar, a,tmp,vlen)
  do i=1,vlen
     res(i) = tmp(i) / relvar(i)**a
  enddo

end subroutine var_coef_vint

!========================================================================
!MICROPHYSICAL PROCESS CALCULATIONS
!========================================================================
!========================================================================
! Initial ice deposition and sublimation loop.
! Run before the main loop
! This subroutine written by Peter Caldwell


subroutine ice_deposition_sublimation(t, qv, qi, ni, &
                                      icldm, rho, dv,qvl, qvi, &
                                      berg, vap_dep, ice_sublim, mgncol)
  !INPUT VARS:
  !===============================================

  integer,  intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t
  real(r8), dimension(mgncol), intent(in) :: qv
  real(r8), dimension(mgncol), intent(in) :: qi
  real(r8), dimension(mgncol), intent(in) :: ni
  real(r8), dimension(mgncol), intent(in) :: icldm
  real(r8), dimension(mgncol), intent(in) :: rho
  real(r8), dimension(mgncol), intent(in) :: dv
  real(r8), dimension(mgncol), intent(in) :: qvl
  real(r8), dimension(mgncol), intent(in) :: qvi
  !OUTPUT VARS:
  !===============================================

  real(r8), dimension(mgncol), intent(out) :: vap_dep !ice deposition (cell-ave value)
  real(r8), dimension(mgncol), intent(out) :: ice_sublim !ice sublimation (cell-ave value)
  real(r8), dimension(mgncol), intent(out) :: berg !bergeron enhancement (cell-ave value)
  !INTERNAL VARS:
  !===============================================

  real(r8) :: ab(mgncol)
  real(r8) :: epsi
  real(r8) :: qiic(mgncol)
  real(r8) :: niic(mgncol)
  real(r8) :: lami(mgncol)
  real(r8) :: n0i(mgncol)
  integer :: i

!!NEC$ IVDEP
!  do i=1,mgncol
!    !Compute linearized condensational heating correction
!  enddo
!NEC$ IVDEP
  do i=1,mgncol
     if (qi(i)>=qsmall) then
        !GET IN-CLOUD qi, ni
        !===============================================
        qiic(i) = qi(i)/icldm(i)
        niic(i) = ni(i)/icldm(i)
        !Get depletion timescale=1/eps
        call calc_ab(t(i), qvi(i), xxls, ab(i))
        !Get slope and intercept of gamma distn for ice.
        call size_dist_param_basic(mg_ice_props, qiic(i), niic(i), lami(i),n0i(i))

        epsi = 2._r8*pi*n0i(i)*rho(i)*Dv(i)/(lami(i)*lami(i))
        !Compute deposition/sublimation

        vap_dep(i) = epsi/ab(i)*(qv(i) - qvi(i))
        !Make this a grid-averaged quantity

        vap_dep(i)=vap_dep(i)*icldm(i)
        !Split into deposition or sublimation.

        if (t(i) < tmelt .and. vap_dep(i)>0._r8) then
           ice_sublim(i)=0._r8
        else
        ! make ice_sublim negative for consistency with other evap/sub processes
           ice_sublim(i)=min(vap_dep(i),0._r8)
           vap_dep(i)=0._r8
        end if
        !sublimation occurs @ any T. Not so for berg.

        if (t(i) < tmelt) then
           !Compute bergeron rate assuming cloud for whole step.

           berg(i) = max(epsi/ab(i)*(qvl(i) - qvi(i)), 0._r8)
        else !T>frz
           berg(i)=0._r8
        end if !T<frz

     else !where qi<qsmall
        berg(i)=0._r8
        vap_dep(i)=0._r8
        ice_sublim(i)=0._r8
     end if !qi>qsmall
  enddo
end subroutine ice_deposition_sublimation
!========================================================================
! autoconversion of cloud liquid water to rain
! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
! minimum qc of 1 x 10^-8 prevents floating point error


subroutine kk2000_liq_autoconversion(microp_uniform, qcic, &
     ncic, rho, relvar, prc, nprc, nprc1, mgncol)

  integer, intent(in) :: mgncol
  logical, intent(in) :: microp_uniform

  real(r8), dimension(mgncol), intent(in) :: qcic
  real(r8), dimension(mgncol), intent(in) :: ncic
  real(r8), dimension(mgncol), intent(in) :: rho

  real(r8), dimension(mgncol), intent(in) :: relvar

  real(r8), dimension(mgncol), intent(out) :: prc
  real(r8), dimension(mgncol), intent(out) :: nprc
  real(r8), dimension(mgncol), intent(out) :: nprc1

  real(r8), dimension(mgncol) :: prc_coef
  integer :: i
  ! Take variance into account, or use uniform value.

  if (.not. microp_uniform) then
     call var_coef(relvar, 2.47_r8, prc_coef,mgncol)
  else
     prc_coef = 1._r8
  end if
  do i=1,mgncol
     if (qcic(i) >= icsmall) then
        ! nprc is increase in rain number conc due to autoconversion
        ! nprc1 is decrease in cloud droplet conc due to autoconversion
        ! assume exponential sub-grid distribution of qc, resulting in additional
        ! factor related to qcvar below
        ! switch for sub-columns, don't include sub-grid qc


        prc(i) = prc_coef(i) * &
             0.01_r8 * 1350._r8 * qcic(i)**2.47_r8 * (ncic(i)*1.e-6_r8*rho(i))**(-1.1_r8)
        nprc(i) = prc(i) * (1._r8/droplet_mass_25um)
        nprc1(i) = prc(i)*ncic(i)/qcic(i)

     else
        prc(i)=0._r8
        nprc(i)=0._r8
        nprc1(i)=0._r8
     end if
  enddo
end subroutine kk2000_liq_autoconversion
  !========================================================================
  
subroutine sb2001v2_liq_autoconversion(pgam,qc,nc,qr,rho,relvar,au,nprc,nprc1,mgncol)
  ! ---------------------------------------------------------------------
  ! AUTO_SB:  calculates the evolution of mass- and number mxg-ratio for
  ! drizzle drops due to autoconversion. The autoconversion rate assumes
  ! f(x)=A*x**(nu_c)*exp(-Bx) in drop MASS x. 
  ! Code from Hugh Morrison, Sept 2014
  ! autoconversion
  ! use simple lookup table of dnu values to get mass spectral shape parameter
  ! equivalent to the size spectral shape parameter pgam
  !


    
  integer, intent(in) :: mgncol  
  
  real(r8), dimension(mgncol), intent (in)    :: pgam
  real(r8), dimension(mgncol), intent (in)    :: qc  ! = qc (cld water mixing ratio)
  real(r8), dimension(mgncol), intent (in)    :: nc  ! = nc (cld water number conc /kg)    
  real(r8), dimension(mgncol), intent (in)    :: qr  ! = qr (rain water mixing ratio)
  real(r8), dimension(mgncol), intent (in)    :: rho ! = rho : density profile
  real(r8), dimension(mgncol), intent (in)    :: relvar 
  
  real(r8), dimension(mgncol), intent (out)   :: au ! = prc autoconversion rate
  real(r8), dimension(mgncol), intent (out)   :: nprc1 ! = number tendency
  real(r8), dimension(mgncol), intent (out)   :: nprc ! = number tendency fixed size for rain
  ! parameters for droplet mass spectral shape, 
  !used by Seifert and Beheng (2001)                             
  ! warm rain scheme only (iparam = 1)                                                                        
 
  real(r8), parameter :: dnu(16) = [0._r8,-0.557_r8,-0.430_r8,-0.307_r8, & 
     -0.186_r8,-0.067_r8,0.050_r8,0.167_r8,0.282_r8,0.397_r8,0.512_r8, &
     0.626_r8,0.739_r8,0.853_r8,0.966_r8,0.966_r8]
  ! parameters for Seifert and Beheng (2001) autoconversion/accretion                                         

  real(r8), parameter :: kc = 9.44e9_r8
  real(r8), parameter :: kr = 5.78e3_r8
  real(r8) :: dum, dum1, nu, pra_coef
  integer :: dumi, i

  do i=1,mgncol

    !pra_coef = var_coef(relvar(i), 2.47_r8)
    ! Why are we calling var_coef here??
    ! call  var_coef(relvar(i), 2.47_r8,pra_coef)

     if (qc(i) > qsmall) then
       dumi=int(pgam(i))
       nu=dnu(dumi)+(dnu(dumi+1)-dnu(dumi))* &
               (pgam(i)-dumi)

       dum = 1._r8-qc(i)/(qc(i)+qr(i))
       dum1 = 600._r8*dum**0.68_r8*(1._r8-dum**0.68_r8)**3

       au(i) = kc/(20._r8*2.6e-7_r8)* &
         (nu+2._r8)*(nu+4._r8)/(nu+1._r8)**2._r8* &
         (rho(i)*qc(i)/1000._r8)**4._r8/(rho(i)*nc(i)/1.e6_r8)**2._r8* &
         (1._r8+dum1/(1._r8-dum)**2)*1000._r8 / rho(i)

       nprc1(i) = au(i)*2._r8/2.6e-7_r8*1000._r8
       nprc(i) = au(i)/droplet_mass_40um
     else
       au(i) = 0._r8
       nprc1(i) = 0._r8
       nprc(i)=0._r8
     end if
  
  enddo

  end subroutine sb2001v2_liq_autoconversion 
!========================================================================
!SB2001 Accretion V2
  

subroutine sb2001v2_accre_cld_water_rain(qc,nc,qr,rho,relvar,pra,npra,mgncol)
  ! ---------------------------------------------------------------------
  ! ACCR_SB calculates the evolution of mass mxng-ratio due to accretion
  ! and self collection following Seifert & Beheng (2001).  
  !
  !
  
  integer, intent(in) :: mgncol
  
  real(r8), dimension(mgncol), intent (in)    :: qc  ! = qc (cld water mixing ratio)
  real(r8), dimension(mgncol), intent (in)    :: nc  ! = nc (cld water number conc /kg)    
  real(r8), dimension(mgncol), intent (in)    :: qr  ! = qr (rain water mixing ratio)
  real(r8), dimension(mgncol), intent (in)    :: rho ! = rho : density profile
  real(r8), dimension(mgncol), intent (in)    :: relvar
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: pra  ! MMR
  real(r8), dimension(mgncol), intent(out) :: npra ! Number
  ! parameters for Seifert and Beheng (2001) autoconversion/accretion                                         

  real(r8), parameter :: kc = 9.44e9_r8
  real(r8), parameter :: kr = 5.78e3_r8

  real(r8) :: dum, dum1
  integer :: i
  ! accretion


  do i =1,mgncol

    if (qc(i) > qsmall) then
      dum = 1._r8-qc(i)/(qc(i)+qr(i))
      dum1 = (dum/(dum+5.e-4_r8))**4._r8
      pra(i) = kr*rho(i)*0.001_r8*qc(i)*qr(i)*dum1
      npra(i) = pra(i)*rho(i)*0.001_r8*(nc(i)*rho(i)*1.e-6_r8)/ &
           (qc(i)*rho(i)*0.001_r8)*1.e6_r8 / rho(i)
    else
      pra(i) = 0._r8
      npra(i) = 0._r8
    end if 
  
  enddo
 
  end subroutine sb2001v2_accre_cld_water_rain   
!========================================================================
! Autoconversion of cloud ice to snow
! similar to Ferrier (1994)


subroutine ice_autoconversion(t, qiic, lami, n0i, dcs, prci, nprci, mgncol)

  integer, intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t
  real(r8), dimension(mgncol), intent(in) :: qiic
  real(r8), dimension(mgncol), intent(in) :: lami
  real(r8), dimension(mgncol), intent(in) :: n0i
  real(r8),                    intent(in) :: dcs

  real(r8), dimension(mgncol), intent(out) :: prci
  real(r8), dimension(mgncol), intent(out) :: nprci
  ! Assume autoconversion timescale of 180 seconds.

  real(r8), parameter :: ac_time = 180._r8
  ! Average mass of an ice particle.

  real(r8) :: m_ip
  ! Ratio of autoconversion diameter to average diameter.
  real(r8) :: d_rat
  integer :: i

  do i=1,mgncol
     if (t(i) <= tmelt .and. qiic(i) >= qsmall) then

        d_rat = lami(i)*dcs
        ! Rate of ice particle conversion (number).

        nprci(i) = n0i(i)/(lami(i)*ac_time)*exp(-d_rat)

        m_ip = (rhoi*pi/6._r8) / lami(i)**3
        ! Rate of mass conversion.
        ! Note that this is:
        ! m n (d^3 + 3 d^2 + 6 d + 6)

        prci(i) = m_ip * nprci(i) * &
             (((d_rat + 3._r8)*d_rat + 6._r8)*d_rat + 6._r8)

     else
        prci(i) = 0._r8
        nprci(i) = 0._r8
     end if
  enddo
end subroutine ice_autoconversion
! immersion freezing (Bigg, 1953)
!===================================


subroutine immersion_freezing(microp_uniform, t, pgam, lamc, &
     qcic, ncic, relvar, mnuccc, nnuccc, mgncol)

  integer, intent(in) :: mgncol
  logical, intent(in) :: microp_uniform
  ! Temperature

  real(r8), dimension(mgncol), intent(in) :: t
  ! Cloud droplet size distribution parameters

  real(r8), dimension(mgncol), intent(in) :: pgam
  real(r8), dimension(mgncol), intent(in) :: lamc
  ! MMR and number concentration of in-cloud liquid water

  real(r8), dimension(mgncol), intent(in) :: qcic
  real(r8), dimension(mgncol), intent(in) :: ncic
  ! Relative variance of cloud water

  real(r8), dimension(mgncol), intent(in) :: relvar
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: mnuccc ! MMR
  real(r8), dimension(mgncol), intent(out) :: nnuccc ! Number
  ! Coefficients that will be omitted for sub-columns

  real(r8), dimension(mgncol) :: dum
  integer :: i
  real(r8) :: tmp

  if (.not. microp_uniform) then
     do i=1,mgncol
        call  var_coef(relvar(i), 2,dum(i))
     enddo
  else
     dum = 1._r8
  end if
  do i=1,mgncol
     if (qcic(i) >= qsmall .and. t(i) < 269.15_r8) then

        call rising_factorial(pgam(i)+1._r8, 3,tmp)
        nnuccc(i) = &
             pi/6._r8*ncic(i)*tmp* &
             bimm*(exp(aimm*(tmelt - t(i)))-1._r8)/lamc(i)**3

        call rising_factorial(pgam(i)+4._r8, 3,tmp)
        mnuccc(i) = dum(i) * nnuccc(i) * &
             pi/6._r8*rhow* &
             tmp/lamc(i)**3

     else
        mnuccc(i) = 0._r8
        nnuccc(i) = 0._r8
     end if ! qcic > qsmall and t < 4 deg C
  enddo

end subroutine immersion_freezing
! contact freezing (-40<T<-3 C) (Young, 1974) with hooks into simulated dust
!===================================================================
! dust size and number in multiple bins are read in from companion routine


subroutine contact_freezing (microp_uniform, t, p, rndst, nacon, &
     pgam, lamc, qcic, ncic, relvar, mnucct, nnucct, mgncol, mdust)

  logical, intent(in) :: microp_uniform

  integer, intent(in) :: mgncol
  integer, intent(in) :: mdust

  real(r8), dimension(mgncol), intent(in) :: t            ! Temperature
  real(r8), dimension(mgncol), intent(in) :: p            ! Pressure
  real(r8), dimension(mgncol, mdust), intent(in) :: rndst ! Radius (for multiple dust bins)
  real(r8), dimension(mgncol, mdust), intent(in) :: nacon ! Number (for multiple dust bins)
  ! Size distribution parameters for cloud droplets

  real(r8), dimension(mgncol), intent(in) :: pgam
  real(r8), dimension(mgncol), intent(in) :: lamc
  ! MMR and number concentration of in-cloud liquid water

  real(r8), dimension(mgncol), intent(in) :: qcic
  real(r8), dimension(mgncol), intent(in) :: ncic
  ! Relative cloud water variance

  real(r8), dimension(mgncol), intent(in) :: relvar
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: mnucct ! MMR
  real(r8), dimension(mgncol), intent(out) :: nnucct ! Number

  real(r8) :: tcnt                  ! scaled relative temperature
  real(r8) :: viscosity             ! temperature-specific viscosity (kg/m/s)
  real(r8) :: mfp                   ! temperature-specific mean free path (m)
  ! Dimension these according to number of dust bins, inferred from rndst size

  real(r8) :: nslip(size(rndst,2))  ! slip correction factors
  real(r8) :: ndfaer(size(rndst,2)) ! aerosol diffusivities (m^2/sec)
  ! Coefficients not used for subcolumns

  real(r8) :: dum, dum1,tmp
  ! Common factor between mass and number.

  real(r8) :: contact_factor

  integer  :: i

  do i = 1,mgncol

     if (qcic(i) >= qsmall .and. t(i) < 269.15_r8) then

        if (.not. microp_uniform) then
           !dum = var_coef(relvar(i), 4._r8/3._r8)
           call var_coef(relvar(i), 4._r8/3._r8,dum)
           !dum1 = var_coef(relvar(i), 1._r8/3._r8)
           call var_coef(relvar(i), 1._r8/3._r8, dum1)
        else
           dum = 1._r8
           dum1 = 1._r8
        endif

        tcnt=(270.16_r8-t(i))**1.3_r8
        viscosity = 1.8e-5_r8*(t(i)/298.0_r8)**0.85_r8    ! Viscosity (kg/m/s)
        mfp = 2.0_r8*viscosity/ &                         ! Mean free path (m)
                     (p(i)*sqrt( 8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i)) ))
        ! Note that these two are vectors.

        nslip = 1.0_r8+(mfp/rndst(i,:))*(1.257_r8+(0.4_r8*exp(-(1.1_r8*rndst(i,:)/mfp))))! Slip correction factor

        ndfaer = 1.381e-23_r8*t(i)*nslip/(6._r8*pi*viscosity*rndst(i,:))  ! aerosol diffusivity (m2/s)

        contact_factor = dot_product(ndfaer,nacon(i,:)*tcnt) * pi * &
             ncic(i) * (pgam(i) + 1._r8) / lamc(i)

        call rising_factorial(pgam(i)+2._r8, 3,tmp)
        mnucct(i) = dum * contact_factor * &
             pi/3._r8*rhow*tmp/lamc(i)**3

        nnucct(i) =  dum1 * 2._r8 * contact_factor

     else

        mnucct(i)=0._r8
        nnucct(i)=0._r8

     end if ! qcic > qsmall and t < 4 deg C
  end do

end subroutine contact_freezing
! snow self-aggregation from passarelli, 1978, used by reisner, 1998
!===================================================================
! this is hard-wired for bs = 0.4 for now
! ignore self-collection of cloud ice


subroutine snow_self_aggregation(t, rho, asn, rhosn, qsic, nsic, nsagg, mgncol)

  integer,                          intent(in) :: mgncol

  real(r8), dimension(mgncol), intent(in) :: t     ! Temperature
  real(r8), dimension(mgncol), intent(in) :: rho   ! Density
  real(r8), dimension(mgncol), intent(in) :: asn   ! fall speed parameter for snow
  real(r8),                    intent(in) :: rhosn ! density of snow
  ! In-cloud snow

  real(r8), dimension(mgncol), intent(in) :: qsic ! MMR
  real(r8), dimension(mgncol), intent(in) :: nsic ! Number
  ! Output number tendency

  real(r8), dimension(mgncol), intent(out) :: nsagg

  integer :: i

  do i=1,mgncol
     if (qsic(i) >= qsmall .and. t(i) <= tmelt) then
        nsagg(i) = -1108._r8*eii/(4._r8*720._r8*rhosn)*asn(i)*qsic(i)*nsic(i)*rho(i)*&
             ((qsic(i)/nsic(i))*(1._r8/(rhosn*pi)))**((bs-1._r8)/3._r8)
     else
        nsagg(i)=0._r8
     end if
  enddo
end subroutine snow_self_aggregation
! accretion of cloud droplets onto snow/graupel
!===================================================================
! here use continuous collection equation with
! simple gravitational collection kernel
! ignore collisions between droplets/cloud ice
! since minimum size ice particle for accretion is 50 - 150 micron


subroutine accrete_cloud_water_snow(t, rho, asn, uns, mu, qcic, ncic, qsic, &
     pgam, lamc, lams, n0s, psacws, npsacws, mgncol)

  integer, intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t   ! Temperature
  real(r8), dimension(mgncol), intent(in) :: rho ! Density
  real(r8), dimension(mgncol), intent(in) :: asn ! Fallspeed parameter (snow)
  real(r8), dimension(mgncol), intent(in) :: uns ! Current fallspeed   (snow)
  real(r8), dimension(mgncol), intent(in) :: mu  ! Viscosity
  ! In-cloud liquid water

  real(r8), dimension(mgncol), intent(in) :: qcic ! MMR
  real(r8), dimension(mgncol), intent(in) :: ncic ! Number
  ! In-cloud snow

  real(r8), dimension(mgncol), intent(in) :: qsic ! MMR
  ! Cloud droplet size parameters

  real(r8), dimension(mgncol), intent(in) :: pgam
  real(r8), dimension(mgncol), intent(in) :: lamc
  ! Snow size parameters

  real(r8), dimension(mgncol), intent(in) :: lams
  real(r8), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: psacws  ! Mass mixing ratio
  real(r8), dimension(mgncol), intent(out) :: npsacws ! Number concentration

  real(r8) :: dc0 ! Provisional mean droplet size
  real(r8) :: dum
  real(r8) :: eci ! collection efficiency for riming of snow by droplets
  ! Fraction of cloud droplets accreted per second

  real(r8) :: accrete_rate
  integer :: i
  ! ignore collision of snow with droplets above freezing


  do i=1,mgncol
     if (qsic(i) >= qsmall .and. t(i) <= tmelt .and. qcic(i) >= qsmall) then
        ! put in size dependent collection efficiency
        ! mean diameter of snow is area-weighted, since
        ! accretion is function of crystal geometric area
        ! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)


        dc0 = (pgam(i)+1._r8)/lamc(i)
        dum = dc0*dc0*uns(i)*rhow*lams(i)/(9._r8*mu(i))
        eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))

        eci = max(eci,0._r8)
        eci = min(eci,1._r8)
        ! no impact of sub-grid distribution of qc since psacws
        ! is linear in qc

        accrete_rate = pi/4._r8*asn(i)*rho(i)*n0s(i)*eci*gamma_bs_plus3 / lams(i)**(bs+3._r8)
        psacws(i) = accrete_rate*qcic(i)
        npsacws(i) = accrete_rate*ncic(i)
     else
        psacws(i) = 0._r8
        npsacws(i) = 0._r8
     end if
  enddo
end subroutine accrete_cloud_water_snow
! add secondary ice production due to accretion of droplets by snow
!===================================================================
! (Hallet-Mossop process) (from Cotton et al., 1986)


subroutine secondary_ice_production(t, psacws, msacwi, nsacwi, mgncol)

  integer, intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t ! Temperature
  ! Accretion of cloud water to snow tendencies

  real(r8), dimension(mgncol), intent(inout) :: psacws ! MMR
  ! Output (ice) tendencies

  real(r8), dimension(mgncol), intent(out) :: msacwi ! MMR
  real(r8), dimension(mgncol), intent(out) :: nsacwi ! Number
  integer :: i

  do i=1,mgncol
     if((t(i) < 270.16_r8) .and. (t(i) >= 268.16_r8)) then
        nsacwi(i) = 3.5e8_r8*(270.16_r8-t(i))/2.0_r8*psacws(i)
     else if((t(i) < 268.16_r8) .and. (t(i) >= 265.16_r8)) then
        nsacwi(i) = 3.5e8_r8*(t(i)-265.16_r8)/3.0_r8*psacws(i)
     else
        nsacwi(i) = 0.0_r8
     endif
  enddo

  do i=1,mgncol
     msacwi(i) = min(nsacwi(i)*mi0, psacws(i))
     psacws(i) = psacws(i) - msacwi(i)
  enddo
end subroutine secondary_ice_production
! accretion of rain water by snow
!===================================================================
! formula from ikawa and saito, 1991, used by reisner et al., 1998


subroutine accrete_rain_snow(t, rho, umr, ums, unr, uns, qric, qsic, &
     lamr, n0r, lams, n0s, pracs, npracs, mgncol)

  integer,                          intent(in) :: mgncol

  real(r8), dimension(mgncol), intent(in) :: t   ! Temperature
  real(r8), dimension(mgncol), intent(in) :: rho ! Density
  ! Fallspeeds
  ! mass-weighted

  real(r8), dimension(mgncol), intent(in) :: umr ! rain
  real(r8), dimension(mgncol), intent(in) :: ums ! snow
  ! number-weighted
  real(r8), dimension(mgncol), intent(in) :: unr ! rain
  real(r8), dimension(mgncol), intent(in) :: uns ! snow
  ! In cloud MMRs

  real(r8), dimension(mgncol), intent(in) :: qric ! rain
  real(r8), dimension(mgncol), intent(in) :: qsic ! snow
  ! Size distribution parameters
  ! rain

  real(r8), dimension(mgncol), intent(in) :: lamr
  real(r8), dimension(mgncol), intent(in) :: n0r
  ! snow
  real(r8), dimension(mgncol), intent(in) :: lams
  real(r8), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: pracs  ! MMR
  real(r8), dimension(mgncol), intent(out) :: npracs ! Number
  ! Collection efficiency for accretion of rain by snow

  real(r8), parameter :: ecr = 1.0_r8
  ! Ratio of average snow diameter to average rain diameter.

  real(r8) :: d_rat
  ! Common factor between mass and number expressions
  real(r8) :: common_factor
  integer :: i

  do i=1,mgncol
     if (qric(i) >= icsmall .and. qsic(i) >= icsmall .and. t(i) <= tmelt) then

        common_factor = pi*ecr*rho(i)*n0r(i)*n0s(i)/(lamr(i)**3 * lams(i))

        d_rat = lamr(i)/lams(i)

        pracs(i) = common_factor*pi*rhow* &
             sqrt((1.2_r8*umr(i)-0.95_r8*ums(i))**2 + 0.08_r8*ums(i)*umr(i)) / lamr(i)**3 * &
             ((0.5_r8*d_rat + 2._r8)*d_rat + 5._r8)

        npracs(i) = common_factor*0.5_r8* &
             sqrt(1.7_r8*(unr(i)-uns(i))**2 + 0.3_r8*unr(i)*uns(i)) * &
             ((d_rat + 1._r8)*d_rat + 1._r8)

     else
        pracs(i) = 0._r8
        npracs(i) = 0._r8
     end if
  enddo
end subroutine accrete_rain_snow
! heterogeneous freezing of rain drops
!===================================================================
! follows from Bigg (1953)


subroutine heterogeneous_rain_freezing(t, qric, nric, lamr, mnuccr, nnuccr, mgncol)

  integer,                          intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t    ! Temperature
  ! In-cloud rain

  real(r8), dimension(mgncol), intent(in) :: qric ! MMR
  real(r8), dimension(mgncol), intent(in) :: nric ! Number
  real(r8), dimension(mgncol), intent(in) :: lamr ! size parameter
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: mnuccr ! MMR
  real(r8), dimension(mgncol), intent(out) :: nnuccr ! Number
  integer :: i

  do i=1,mgncol

     if (t(i) < 269.15_r8 .and. qric(i) >= qsmall) then
        nnuccr(i) = pi*nric(i)*bimm* &
             (exp(aimm*(tmelt - t(i)))-1._r8)/lamr(i)**3

        mnuccr(i) = nnuccr(i) * 20._r8*pi*rhow/lamr(i)**3

     else
        mnuccr(i) = 0._r8
        nnuccr(i) = 0._r8
     end if
  enddo
end subroutine heterogeneous_rain_freezing
! accretion of cloud liquid water by rain
!===================================================================
! formula from Khrouditnov and Kogan (2000)
! gravitational collection kernel, droplet fall speed neglected


subroutine accrete_cloud_water_rain(microp_uniform, qric, qcic, &
     ncic, relvar, accre_enhan, pra, npra, mgncol)

  logical, intent(in) :: microp_uniform
  integer, intent(in) :: mgncol
  ! In-cloud rain
  real(r8), dimension(mgncol), intent(in) :: qric ! MMR
  ! Cloud droplets

  real(r8), dimension(mgncol), intent(in) :: qcic ! MMR
  real(r8), dimension(mgncol), intent(in) :: ncic ! Number
  ! SGS variability

  real(r8), dimension(mgncol), intent(in) :: relvar
  real(r8), dimension(mgncol), intent(in) :: accre_enhan
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: pra  ! MMR
  real(r8), dimension(mgncol), intent(out) :: npra ! Number
  ! Coefficient that varies for subcolumns

  real(r8), dimension(mgncol) :: pra_coef

  integer :: i

  if (.not. microp_uniform) then
       call  var_coef(relvar, 1.15_r8, pra_coef,mgncol)
       pra_coef = pra_coef*accre_enhan
  else
    pra_coef = 1._r8
  end if
  do i=1,mgncol
    if (qric(i) >= qsmall .and. qcic(i) >= qsmall) then
      ! include sub-grid distribution of cloud water

      pra(i) = pra_coef(i) * 67._r8*(qcic(i)*qric(i))**1.15_r8

      npra(i) = pra(i)*ncic(i)/qcic(i)

    else
      pra(i) = 0._r8
      npra(i) = 0._r8
    end if
  end do
end subroutine accrete_cloud_water_rain
! Self-collection of rain drops
!===================================================================
! from Beheng(1994)


subroutine self_collection_rain(rho, qric, nric, nragg, mgncol)

  integer,                          intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: rho  ! Air density
  ! Rain

  real(r8), dimension(mgncol), intent(in) :: qric ! MMR
  real(r8), dimension(mgncol), intent(in) :: nric ! Number
  ! Output number tendency

  real(r8), dimension(mgncol), intent(out) :: nragg

  integer :: i

  do i=1,mgncol
     if (qric(i) >= qsmall) then
        nragg(i) = -8._r8*nric(i)*qric(i)*rho(i)
     else
        nragg(i) = 0._r8
     end if
  enddo
end subroutine self_collection_rain
! Accretion of cloud ice by snow
!===================================================================
! For this calculation, it is assumed that the Vs >> Vi
! and Ds >> Di for continuous collection


subroutine accrete_cloud_ice_snow(t, rho, asn, qiic, niic, qsic, &
     lams, n0s, prai, nprai, mgncol)

  integer,                          intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t    ! Temperature
  real(r8), dimension(mgncol), intent(in) :: rho   ! Density

  real(r8), dimension(mgncol), intent(in) :: asn  ! Snow fallspeed parameter
  ! Cloud ice

  real(r8), dimension(mgncol), intent(in) :: qiic ! MMR
  real(r8), dimension(mgncol), intent(in) :: niic ! Number

  real(r8), dimension(mgncol), intent(in) :: qsic ! Snow MMR
  ! Snow size parameters

  real(r8), dimension(mgncol), intent(in) :: lams
  real(r8), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: prai ! MMR
  real(r8), dimension(mgncol), intent(out) :: nprai ! Number
  ! Fraction of cloud ice particles accreted per second

  real(r8) :: accrete_rate

  integer :: i

  do i=1,mgncol
     if (qsic(i) >= qsmall .and. qiic(i) >= qsmall .and. t(i) <= tmelt) then

        accrete_rate = pi/4._r8 * eii * asn(i) * rho(i) * n0s(i) * gamma_bs_plus3/ &
             lams(i)**(bs+3._r8)

        prai(i) = accrete_rate * qiic(i)
        nprai(i) = accrete_rate * niic(i)

     else
        prai(i) = 0._r8
        nprai(i) = 0._r8
     end if
  enddo
end subroutine accrete_cloud_ice_snow
! calculate evaporation/sublimation of rain and snow
!===================================================================
! note: evaporation/sublimation occurs only in cloud-free portion of grid cell
! in-cloud condensation/deposition of rain and snow is neglected
! except for transfer of cloud water to snow through bergeron process


subroutine evaporate_sublimate_precip(t, rho, dv, mu, sc, q, qvl, qvi, &
     lcldm, precip_frac, arn, asn, qcic, qiic, qric, qsic, lamr, n0r, lams, n0s, &
     pre, prds, am_evp_st, mgncol)

  integer,  intent(in) :: mgncol

  real(r8), dimension(mgncol), intent(in) :: t    ! temperature
  real(r8), dimension(mgncol), intent(in) :: rho  ! air density
  real(r8), dimension(mgncol), intent(in) :: dv   ! water vapor diffusivity
  real(r8), dimension(mgncol), intent(in) :: mu   ! viscosity
  real(r8), dimension(mgncol), intent(in) :: sc   ! schmidt number
  real(r8), dimension(mgncol), intent(in) :: q    ! humidity
  real(r8), dimension(mgncol), intent(in) :: qvl  ! saturation humidity (water)
  real(r8), dimension(mgncol), intent(in) :: qvi  ! saturation humidity (ice)
  real(r8), dimension(mgncol), intent(in) :: lcldm  ! liquid cloud fraction
  real(r8), dimension(mgncol), intent(in) :: precip_frac ! precipitation fraction (maximum overlap)
  ! fallspeed parameters

  real(r8), dimension(mgncol), intent(in) :: arn  ! rain
  real(r8), dimension(mgncol), intent(in) :: asn  ! snow
  ! In-cloud MMRs

  real(r8), dimension(mgncol), intent(in) :: qcic ! cloud liquid
  real(r8), dimension(mgncol), intent(in) :: qiic ! cloud ice
  real(r8), dimension(mgncol), intent(in) :: qric ! rain
  real(r8), dimension(mgncol), intent(in) :: qsic ! snow
  ! Size parameters
  ! rain

  real(r8), dimension(mgncol), intent(in) :: lamr
  real(r8), dimension(mgncol), intent(in) :: n0r
  ! snow
  real(r8), dimension(mgncol), intent(in) :: lams
  real(r8), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: pre
  real(r8), dimension(mgncol), intent(out) :: prds
  real(r8), dimension(mgncol), intent(out) :: am_evp_st ! Fractional area where rain evaporates.

  real(r8) :: qclr   ! water vapor mixing ratio in clear air
  real(r8) :: abr(mgncol),abs(mgncol)     ! correction to account for latent heat
  real(r8) :: eps    ! 1/ sat relaxation timescale

  real(r8), dimension(mgncol) :: dum

  integer :: i
  logical, dimension(mgncol) :: cond1,cond2,cond3

  am_evp_st = 0._r8
  ! set temporary cloud fraction to zero if cloud water + ice is very small
  ! this will ensure that evaporation/sublimation of precip occurs over
  ! entire grid cell, since min cloud fraction is specified otherwise
  do i=1,mgncol
     if (qcic(i)+qiic(i) < 1.e-6_r8) then
        dum(i) = 0._r8
     else
        dum(i) = lcldm(i)
     end if
  enddo
#if 0
  cond1 = (precip_frac > dum)
  cond2 = cond1 .and. (qric >= qsmall)
  cond3 = cond1 .and. (qsic >= qsmall)
  print *,'evaporate_sublimate_precip: mgncol: ',mgncol
  print *,'evaporate_sublimate_precip: COND2: ',COUNT(cond2)
  print *,'evaporate_sublimate_precip: COND3: ',COUNT(cond3)
  print *,'evaporate_sublimate_precip: COND4: ',COUNT(cond3 .or. cond2)
#endif
  do i=1,mgncol
  ! only calculate if there is some precip fraction > cloud fraction

     if (precip_frac(i) > dum(i)) then

        ! evaporation of rain
        if (qric(i) >= qsmall) then
           am_evp_st(i) = precip_frac(i) - dum(i)
           ! calculate q for out-of-cloud region

           qclr=(q(i)-dum(i)*qvl(i))/(1._r8-dum(i))

           !ab = calc_ab(t(i), qvl(i), xxlv)
           call calc_ab(t(i), qvl(i), xxlv, abr(i))
           eps = 2._r8*pi*n0r(i)*rho(i)*Dv(i)* &
                (f1r/(lamr(i)*lamr(i))+ &
                f2r*(arn(i)*rho(i)/mu(i))**0.5_r8* &
                sc(i)**(1._r8/3._r8)*gamma_half_br_plus5/ &
                (lamr(i)**(5._r8/2._r8+br/2._r8)))

           pre(i) = eps*(qclr-qvl(i))/abr(i)
           ! only evaporate in out-of-cloud region
           ! and distribute across precip_frac

           pre(i)=min(pre(i)*am_evp_st(i),0._r8)
           pre(i)=pre(i)/precip_frac(i)
        else
           pre(i) = 0._r8
        end if

        ! sublimation of snow
        if (qsic(i) >= qsmall) then
           am_evp_st(i) = precip_frac(i) - dum(i)
           ! calculate q for out-of-cloud region

           qclr=(q(i)-dum(i)*qvl(i))/(1._r8-dum(i))
           ! ab = calc_ab(t(i), qvi(i), xxls)
           call calc_ab(t(i), qvi(i), xxls, abs(i))
           eps = 2._r8*pi*n0s(i)*rho(i)*Dv(i)* &
                (f1s/(lams(i)*lams(i))+ &
                f2s*(asn(i)*rho(i)/mu(i))**0.5_r8* &
                sc(i)**(1._r8/3._r8)*gamma_half_bs_plus5/ &
                (lams(i)**(5._r8/2._r8+bs/2._r8)))
           prds(i) = eps*(qclr-qvi(i))/abs(i)
           ! only sublimate in out-of-cloud region and distribute over precip_frac

           prds(i)=min(prds(i)*am_evp_st(i),0._r8)
           prds(i)=prds(i)/precip_frac(i)
        else
           prds(i) = 0._r8
        end if

     else
        prds(i) = 0._r8
        pre(i) = 0._r8
     end if
  enddo

end subroutine evaporate_sublimate_precip
! bergeron process - evaporation of droplets and deposition onto snow
!===================================================================


subroutine bergeron_process_snow(t, rho, dv, mu, sc, qvl, qvi, asn, &
     qcic, qsic, lams, n0s, bergs, mgncol)

  integer, intent(in) :: mgncol

  real(r8), dimension(mgncol), intent(in) :: t    ! temperature
  real(r8), dimension(mgncol), intent(in) :: rho  ! air density
  real(r8), dimension(mgncol), intent(in) :: dv   ! water vapor diffusivity
  real(r8), dimension(mgncol), intent(in) :: mu   ! viscosity
  real(r8), dimension(mgncol), intent(in) :: sc   ! schmidt number
  real(r8), dimension(mgncol), intent(in) :: qvl  ! saturation humidity (water)
  real(r8), dimension(mgncol), intent(in) :: qvi  ! saturation humidity (ice)
  ! fallspeed parameter for snow

  real(r8), dimension(mgncol), intent(in) :: asn
  ! In-cloud MMRs

  real(r8), dimension(mgncol), intent(in) :: qcic ! cloud liquid
  real(r8), dimension(mgncol), intent(in) :: qsic ! snow
  ! Size parameters for snow

  real(r8), dimension(mgncol), intent(in) :: lams
  real(r8), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(r8), dimension(mgncol), intent(out) :: bergs

  real(r8) :: ab     ! correction to account for latent heat
  real(r8) :: eps    ! 1/ sat relaxation timescale

  integer :: i

  do i=1,mgncol
     if (qsic(i) >= qsmall.and. qcic(i) >= qsmall .and. t(i) < tmelt) then
        ! ab = calc_ab(t(i), qvi(i), xxls)
        call calc_ab(t(i), qvi(i), xxls, ab)
        eps = 2._r8*pi*n0s(i)*rho(i)*Dv(i)* &
             (f1s/(lams(i)*lams(i))+ &
             f2s*(asn(i)*rho(i)/mu(i))**0.5_r8* &
             sc(i)**(1._r8/3._r8)*gamma_half_bs_plus5/ &
             (lams(i)**(5._r8/2._r8+bs/2._r8)))
        bergs(i) = eps*(qvl(i)-qvi(i))/ab
     else
        bergs(i) = 0._r8
     end if
  enddo
end subroutine bergeron_process_snow
!========================================================================
!UTILITIES
!========================================================================


pure function limiter_is_on(lim)
  real(r8), intent(in) :: lim
  logical :: limiter_is_on

  limiter_is_on = transfer(lim, limiter_off) /= limiter_off

end function limiter_is_on

!read state subroutine for kr_externs_in_micro_mg_utils 
SUBROUTINE kr_externs_in_micro_mg_utils(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_liq_props, kgen_unit, "mg_liq_props", .FALSE.) 
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_ice_props, kgen_unit, "mg_ice_props", .FALSE.) 
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_rain_props, kgen_unit, "mg_rain_props", .FALSE.) 
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_snow_props, kgen_unit, "mg_snow_props", .FALSE.) 
    READ (UNIT = kgen_unit) rv 
    READ (UNIT = kgen_unit) cpp 
    READ (UNIT = kgen_unit) tmelt 
    READ (UNIT = kgen_unit) xxlv 
    READ (UNIT = kgen_unit) xxls 
    READ (UNIT = kgen_unit) gamma_bs_plus3 
    READ (UNIT = kgen_unit) gamma_half_br_plus5 
    READ (UNIT = kgen_unit) gamma_half_bs_plus5 
END SUBROUTINE kr_externs_in_micro_mg_utils 
  
!read state subroutine for kr_externs_out_micro_mg_utils 
SUBROUTINE kr_externs_out_micro_mg_utils(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_micro_mg_utils 
  
!read state subroutine for kr_micro_mg_utils_mghydrometeorprops 
RECURSIVE SUBROUTINE kr_micro_mg_utils_mghydrometeorprops(var, kgen_unit, printname, printvar) 
    TYPE(mghydrometeorprops), INTENT(INOUT) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) var%rho 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%rho = ", var%rho 
    END IF   
      
    READ (UNIT = kgen_unit) var%eff_dim 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%eff_dim = ", var%eff_dim 
    END IF   
      
    READ (UNIT = kgen_unit) var%shape_coef 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%shape_coef = ", var%shape_coef 
    END IF   
      
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) var%lambda_bounds 
        CALL kgen_array_sumcheck(printname // "%lambda_bounds", kgen_array_sum, DBLE(SUM(var%lambda_bounds, &
        &mask=(var%lambda_bounds .eq. var%lambda_bounds))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%lambda_bounds)) = ", DBLE(SUM(var%lambda_bounds, &
            &mask=(var%lambda_bounds .eq. var%lambda_bounds))) 
        END IF   
    END IF   
      
    READ (UNIT = kgen_unit) var%min_mean_mass 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%min_mean_mass = ", var%min_mean_mass 
    END IF   
      
END SUBROUTINE kr_micro_mg_utils_mghydrometeorprops 
  
!verify state subroutine for kv_micro_mg_utils_mghydrometeorprops 
RECURSIVE SUBROUTINE kv_micro_mg_utils_mghydrometeorprops(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    TYPE(mghydrometeorprops), INTENT(IN) :: var, kgenref_var 
    TYPE(check_t) :: dtype_check_status, comp_check_status 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    real(KIND=r8) :: diff_rho 
    real(KIND=r8) :: diff_eff_dim 
    real(KIND=r8) :: diff_shape_coef 
    INTEGER :: n_lambda_bounds 
    real(KIND=r8) :: nrmsdiff_lambda_bounds, rmsdiff_lambda_bounds 
    real(KIND=r8), ALLOCATABLE :: buf1_lambda_bounds(:), buf2_lambda_bounds(:) 
    real(KIND=r8) :: diff_min_mean_mass 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%rho == kgenref_var%rho) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) trim(adjustl(varname)), "%rho is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_rho = ABS(var%rho - kgenref_var%rho) 
        IF (diff_rho <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%rho is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%rho is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_rho 
            WRITE (*, *) "" 
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_rho 
            WRITE (*, *) "" 
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%eff_dim == kgenref_var%eff_dim) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) trim(adjustl(varname)), "%eff_dim is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_eff_dim = ABS(var%eff_dim - kgenref_var%eff_dim) 
        IF (diff_eff_dim <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%eff_dim is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%eff_dim is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_eff_dim 
            WRITE (*, *) "" 
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_eff_dim 
            WRITE (*, *) "" 
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%shape_coef == kgenref_var%shape_coef) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) trim(adjustl(varname)), "%shape_coef is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_shape_coef = ABS(var%shape_coef - kgenref_var%shape_coef) 
        IF (diff_shape_coef <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%shape_coef is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%shape_coef is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_shape_coef 
            WRITE (*, *) "" 
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_shape_coef 
            WRITE (*, *) "" 
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (ALL(var%lambda_bounds == kgenref_var%lambda_bounds)) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        ALLOCATE (buf1_lambda_bounds(SIZE(var%lambda_bounds,dim=1))) 
        ALLOCATE (buf2_lambda_bounds(SIZE(var%lambda_bounds,dim=1))) 
        n_lambda_bounds = COUNT(var%lambda_bounds /= kgenref_var%lambda_bounds) 
        WHERE ( ABS(kgenref_var%lambda_bounds) > kgen_minvalue ) 
            buf1_lambda_bounds = ((var%lambda_bounds-kgenref_var%lambda_bounds)/kgenref_var%lambda_bounds)**2 
            buf2_lambda_bounds = (var%lambda_bounds-kgenref_var%lambda_bounds)**2 
        ELSEWHERE 
            buf1_lambda_bounds = (var%lambda_bounds-kgenref_var%lambda_bounds)**2 
            buf2_lambda_bounds = buf1_lambda_bounds 
        END WHERE   
        nrmsdiff_lambda_bounds = SQRT(SUM(buf1_lambda_bounds)/REAL(n_lambda_bounds)) 
        rmsdiff_lambda_bounds = SQRT(SUM(buf2_lambda_bounds)/REAL(n_lambda_bounds)) 
        IF (nrmsdiff_lambda_bounds > kgen_tolerance) THEN 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        ELSE 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) count( var%lambda_bounds /= kgenref_var%lambda_bounds), " of ", size( var%lambda_bounds ), " elements &
            &are different." 
            WRITE (*, *) "Average - kernel ", sum(var%lambda_bounds)/real(size(var%lambda_bounds)) 
            WRITE (*, *) "Average - reference ", sum(kgenref_var%lambda_bounds)/real(size(kgenref_var%lambda_bounds)) 
            WRITE (*, *) "RMS of difference is ", rmsdiff_lambda_bounds 
            WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lambda_bounds 
            WRITE (*, *) "" 
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) count( var%lambda_bounds /= kgenref_var%lambda_bounds), " of ", size( var%lambda_bounds ), " elements &
            &are different." 
            WRITE (*, *) "Average - kernel ", sum(var%lambda_bounds)/real(size(var%lambda_bounds)) 
            WRITE (*, *) "Average - reference ", sum(kgenref_var%lambda_bounds)/real(size(kgenref_var%lambda_bounds)) 
            WRITE (*, *) "RMS of difference is ", rmsdiff_lambda_bounds 
            WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lambda_bounds 
            WRITE (*, *) "" 
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%min_mean_mass == kgenref_var%min_mean_mass) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_min_mean_mass = ABS(var%min_mean_mass - kgenref_var%min_mean_mass) 
        IF (diff_min_mean_mass <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_min_mean_mass 
            WRITE (*, *) "" 
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            WRITE (*, *) "Difference is ", diff_min_mean_mass 
            WRITE (*, *) "" 
        END IF   
    END IF   
      
    IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
    ELSE IF (dtype_check_status%numOutTol > 0) THEN 
        check_status%numOutTol = check_status%numOutTol + 1 
    ELSE IF (dtype_check_status%numInTol > 0) THEN 
        check_status%numInTol = check_status%numInTol + 1 
    END IF   
END SUBROUTINE kv_micro_mg_utils_mghydrometeorprops 
  
end module micro_mg_utils
