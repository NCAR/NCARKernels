!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  
#define ALLOCERROR 1


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

#if defined(__NEC__) || defined(__OPENACC__)
#else
    USE shr_spfn_mod, ONLY: gamma => shr_spfn_gamma 
#endif
    USE shr_kind_mod, ONLY: rkind_comp, rkind_io
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
    &evaporate_sublimate_precip, bergeron_process_snow, accrete_cloud_water_snow_v2
    PUBLIC avg_diameter_vec
    PUBLIC size_dist_param_basic_vec 
    PUBLIC size_dist_param_liq_vec
! 8 byte real and integer

!integer, parameter, public :: rkind_comp = selected_real_kind(12)
integer, parameter, public :: i8 = selected_int_kind(18)

PUBLIC mghydrometeorprops 

type :: MGHydrometeorProps
   ! Density (kg/m^3)
   real(rkind_comp) :: rho
   ! Information for size calculations.
   ! Basic calculation of mean size is:
   !     lambda = (shape_coef*nic/qic)^(1/eff_dim)
   ! Then lambda is constrained by bounds.
   real(rkind_comp) :: eff_dim
   real(rkind_comp) :: shape_coef
   real(rkind_comp) :: lambda_bounds(2)
   ! Minimum average particle mass (kg).
   ! Limit is applied at the beginning of the size distribution calculations.
   real(rkind_comp) :: min_mean_mass
end type MGHydrometeorProps


type(MGHydrometeorProps), public :: mg_liq_props
type(MGHydrometeorProps), public :: mg_ice_props
type(MGHydrometeorProps), public :: mg_rain_props
type(MGHydrometeorProps), public :: mg_snow_props

!$acc declare copyin(mg_liq_props,mg_ice_props,mg_rain_props,mg_snow_props)

interface size_dist_param_liq
  module procedure size_dist_param_liq_line
end interface
interface size_dist_param_basic
  module procedure size_dist_param_basic_vect2
  module procedure size_dist_param_basic_line
end interface

interface calc_ab
  module procedure calc_ab_rkind_comp
  module procedure calc_ab_v8
end interface

!=================================================
! Public module parameters (mostly for MG itself)
!=================================================
! Pi to 20 digits; more than enough to reach the limit of double precision.


real(rkind_comp), parameter, public :: pi = 3.14159265358979323846_rkind_comp
! "One minus small number": number near unity for round-off issues.

real(rkind_comp), parameter, public :: omsm   = 1._rkind_comp - 1.e-5_rkind_comp
! Smallest mixing ratio considered in microphysics.

#ifdef USE_R4
real(rkind_comp), parameter, public :: qsmall = 1.e-5_rkind_comp
#else
real(rkind_comp), parameter, public :: qsmall = 1.e-18_rkind_comp
#endif
! minimum allowed cloud fraction

real(rkind_comp), parameter, public :: mincld = 0.0001_rkind_comp

real(rkind_comp), parameter, public :: rhosn = 250._rkind_comp  ! bulk density snow
real(rkind_comp), parameter, public :: rhoi = 500._rkind_comp   ! bulk density ice
real(rkind_comp), parameter, public :: rhow = 1000._rkind_comp  ! bulk density liquid
real(rkind_comp), parameter, public :: rhows = 917._rkind_comp  ! bulk density water solid
! fall speed parameters, V = aD^b (V is in m/s)
! droplets

real(rkind_comp), parameter, public :: ac = 3.e7_rkind_comp
real(rkind_comp), parameter, public :: bc = 2._rkind_comp
! snow
real(rkind_comp), parameter, public :: as = 11.72_rkind_comp
real(rkind_comp), parameter, public :: bs = 0.41_rkind_comp
! cloud ice
real(rkind_comp), parameter, public :: ai = 700._rkind_comp
real(rkind_comp), parameter, public :: bi = 1._rkind_comp
! small cloud ice (r< 10 um) - sphere, bulk density
real(rkind_comp), parameter, public :: aj = ac*((rhoi/rhows)**(bc/3._rkind_comp))*rhows/rhow
real(rkind_comp), parameter, public :: bj = bc
! rain
real(rkind_comp), parameter, public :: ar = 841.99667_rkind_comp
real(rkind_comp), parameter, public :: br = 0.8_rkind_comp
! mass of new crystal due to aerosol freezing and growth (kg)
! Make this consistent with the lower bound, to support UTLS and
! stratospheric ice, and the smaller ice size limit.

real(rkind_comp), parameter, public :: mi0 = 4._rkind_comp/3._rkind_comp*pi*rhoi*(1.e-6_rkind_comp)**3
!=================================================
! Private module parameters
!=================================================
! Signaling NaN bit pattern that represents a limiter that's turned off.


integer(i8), parameter :: limiter_off = int(Z'7FF1111111111111', i8)
! alternate threshold used for some in-cloud mmr

real(rkind_comp), parameter :: icsmall = 1.e-8_rkind_comp
! particle mass-diameter relationship
! currently we assume spherical particles for cloud ice/snow
! m = cD^d
! exponent

! Bounds for mean diameter for different constituents.

! Minimum average mass of particles.

! ventilation parameters
! for snow

real(rkind_comp), parameter :: f1s = 0.86_rkind_comp
real(rkind_comp), parameter :: f2s = 0.28_rkind_comp
! for rain
real(rkind_comp), parameter :: f1r = 0.78_rkind_comp
real(rkind_comp), parameter :: f2r = 0.308_rkind_comp
! collection efficiencies
! aggregation of cloud ice and snow

real(rkind_comp), parameter :: eii = 0.5_rkind_comp
! immersion freezing parameters, bigg 1953

real(rkind_comp), parameter :: bimm = 100._rkind_comp
real(rkind_comp), parameter :: aimm = 0.66_rkind_comp
! Mass of each raindrop created from autoconversion.

real(rkind_comp), parameter :: droplet_mass_25um = 4._rkind_comp/3._rkind_comp*pi*rhow*(25.e-6_rkind_comp)**3
real(rkind_comp), parameter :: droplet_mass_40um = 4._rkind_comp/3._rkind_comp*pi*rhow*(40.e-6_rkind_comp)**3
!=========================================================
! Constants set in initialization
!=========================================================
! Set using arguments to micro_mg_init


real(rkind_comp) :: rv          ! water vapor gas constant
real(rkind_comp) :: cpp         ! specific heat of dry air
real(rkind_comp) :: tmelt       ! freezing point of water (K)
! latent heats of:

real(rkind_comp) :: xxlv        ! vaporization
real(rkind_comp) :: xxls        ! sublimation
! additional constants to help speed up code

real(rkind_comp) :: gamma_bs_plus3
real(rkind_comp) :: gamma_half_br_plus5
real(rkind_comp) :: gamma_half_bs_plus5
!=========================================================
! Utilities that are cheaper if the compiler knows that
! some argument is an integer.
!=========================================================

!$acc declare copyin(rv,cpp,tmelt,xxls,xxlv)
!$acc declare copyin(gamma_bs_plus3,gamma_half_br_plus5,gamma_half_bs_plus5)


interface rising_factorial
   module procedure rising_factorial_rkind_comp
   module procedure rising_factorial_v8
   module procedure rising_factorial_int
   module procedure rising_factorial_vint
end interface rising_factorial

interface var_coef
   module procedure var_coef_rkind_comp
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


subroutine  rising_factorial_rkind_comp(x, n, res)
  !$acc routine seq
  real(rkind_comp), intent(in)  :: x, n
  real(rkind_comp), intent(out) :: res

  res = gamma(x+n)/gamma(x)

end subroutine rising_factorial_rkind_comp

pure function intfuncgamma(x, y) result(z)
    !$acc routine seq

    real(rkind_comp) :: z
    real(rkind_comp), intent(in) :: x, y

    z = x**(y-1.0) * exp(-x)
end function intfuncgamma

!!! source: https://rosettacode.org/wiki/Gamma_function#Fortran
function my_gamma(a) result(g)
    !$acc routine vector

    real(rkind_comp) :: g
    real(rkind_comp), intent(in) :: a

    real(rkind_comp), parameter :: small = 1.0e-4
    integer, parameter :: points = 100000

    real(rkind_comp) :: infty, dx, p, sp(2, points), x
    real(rkind_comp) :: tmp
    integer :: i
    logical :: correction

    x = a

    correction = .false.
    ! value with x<1 gives \infty, so we use
    ! \Gamma(x+1) = x\Gamma(x)
    ! to avoid the problem
    if ( x < 1.0 ) then
       correction = .true.
       x = x + 1
    end if

    ! find a "reasonable" infinity...
    ! we compute this integral indeed
    ! \int_0^M dt t^{x-1} e^{-t}
    ! where M is such that M^{x-1} e^{-M} ≤ \epsilon
    infty = 1.0e4_rkind_comp
    do while ( intfuncgamma(infty, x) > small )
       infty = infty * 10.0
    end do

    ! using simpson
    dx = infty/real(points)
    sp = 0.0

    !forall(i=1:points/2-1) sp(1, 2*i) = intfuncgamma(2.0*(i)*dx, x)
    do i=1,points/2-1 
      tmp=2.0*real(i,kind=rkind_comp)*dx
      print *,'i, tmp, dx, x: ',i,tmp,dx,x
      sp(1,2*i) = intfuncgamma(tmp, x)
    enddo
#if 0
    forall(i=1:points/2) sp(2, 2*i - 1) = intfuncgamma((2.0*(i)-1.0)*dx, x)
    g = (intfuncgamma(0.0_rkind_comp, x) + 2.0*sum(sp(1,:)) + 4.0*sum(sp(2,:)) + &
         intfuncgamma(infty, x))*dx/3.0

    if ( correction ) g = g/a

#endif
    g = 0.0d0

end function my_gamma


subroutine  rising_factorial_v8(x, n, res,vlen)
  integer, intent(in)   :: vlen
  real(rkind_comp), intent(in)  :: x(vlen), n
  real(rkind_comp), intent(out) :: res(vlen)
  integer :: i
  real(rkind_comp) :: tmp(vlen)
  !$acc declare create(tmp)
  !$acc declare present(x,res)

#if defined(__OPENACC__)
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
    tmp(i) = x(i)+n
    res(i) = gamma(tmp(i))/gamma(x(i))
    !res(i) = my_gamma(tmp(i))/my_gamma(x(i))
  enddo
  !$acc end parallel
#else
  tmp = x+n
#ifdef _MKL
  call vdtgamma(vlen,tmp,res) 
  call vdtgamma(vlen,x,tmp)   
#else
  res = gamma(tmp)
  tmp = gamma(x)
#endif
  res = res/tmp
#endif

end subroutine rising_factorial_v8
! Rising factorial can be performed much cheaper if n is a small integer.

subroutine rising_factorial_int(x, n, res)
  !$acc routine seq
  real(rkind_comp), intent(in) :: x
  integer, intent(in) :: n
  real(rkind_comp), intent(out) :: res

  integer :: i
  real(rkind_comp) :: factor

  res = 1._rkind_comp
  factor = x

  do i = 1, n
     res = res * factor
     factor = factor + 1._rkind_comp
  end do

end subroutine rising_factorial_int

subroutine rising_factorial_vint(x, n, res,vlen)
  integer, intent(in) :: vlen
  real(rkind_comp), intent(in) :: x(vlen)
  integer, intent(in) :: n
  real(rkind_comp), intent(out) :: res(vlen)

  integer :: i,j
  real(rkind_comp) :: factor(vlen)
  !$acc declare create(factor)
  !$acc declare present(x,res)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
    res(i)    = 1._rkind_comp
    factor(i) = x(i)
  enddo

  if (n == 3) then 
    !$acc loop vector
    do i=1,vlen
      res(i)    = res(i) * factor(i)
      factor(i) = factor(i) + 1._rkind_comp
      res(i)    = res(i) * factor(i)
      factor(i) = factor(i) + 1._rkind_comp
      res(i)    = res(i) * factor(i)
    enddo
  elseif (n==2) then 
    !$acc loop vector
    do i=1,vlen
      res(i)    = res(i) * factor(i)
      factor(i) = factor(i) + 1._rkind_comp
      res(i)    = res(i) * factor(i)
    enddo
  else
    !$acc loop seq
    do j = 1, n
       !$acc loop vector
       do i=1,vlen
         res(i) = res(i) * factor(i)
         factor(i) = factor(i) + 1._rkind_comp
       enddo
    enddo
  endif
  !$acc end parallel

end subroutine rising_factorial_vint

! Calculate correction due to latent heat for evaporation/sublimation
subroutine calc_ab_rkind_comp(t, qv, xxl,ab)
  !$acc routine seq

  real(rkind_comp), intent(in) :: t     ! Temperature
  real(rkind_comp), intent(in) :: qv    ! Saturation vapor pressure
  real(rkind_comp), intent(in) :: xxl   ! Latent heat

  real(rkind_comp), intent(out) :: ab

  real(rkind_comp) :: dqsdt

  dqsdt = xxl*qv / (rv * t**2)
  ab = 1._rkind_comp + dqsdt*xxl/cpp

end subroutine  calc_ab_rkind_comp

! Calculate correction due to latent heat for evaporation/sublimation
subroutine calc_ab_v8(t, qv, xxl,ab,vlen)
  integer,  intent(in) :: vlen
  real(rkind_comp), intent(in) :: t(vlen)     ! Temperature
  real(rkind_comp), intent(in) :: qv(vlen)    ! Saturation vapor pressure
  real(rkind_comp), intent(in) :: xxl         ! Latent heat

  real(rkind_comp), intent(out) :: ab(vlen)
  real(rkind_comp) :: dqsdt(vlen)
  integer :: i
  !$acc declare create(dqsdt)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     dqsdt(i) = xxl*qv(i) / (rv * t(i)**2)
     ab(i) = 1._rkind_comp + dqsdt(i)*xxl/cpp
  enddo
  !$acc end parallel

end subroutine  calc_ab_v8

! get cloud droplet size distribution parameters

subroutine size_dist_param_liq_line(props, qcic, ncic, rho, pgam, lamc)
  type(MGHydrometeorProps), intent(in) :: props
  real(rkind_comp), intent(in) :: qcic
  real(rkind_comp), intent(inout) :: ncic
  real(rkind_comp), intent(in) :: rho

  real(rkind_comp), intent(out) :: pgam
  real(rkind_comp), intent(out) :: lamc

  type(MGHydrometeorProps) :: props_loc
  real(rkind_comp) :: tmp

  if (qcic > qsmall) then
     ! Local copy of properties that can be modified.
     ! (Elemental routines that operate on arrays can't modify scalar
     ! arguments.)

     props_loc = props
     ! Get pgam from fit to observations of martin et al. 1994

     pgam = 1.0_rkind_comp - 0.7_rkind_comp * exp(-0.008_rkind_comp*1.e-6_rkind_comp*ncic*rho)
     pgam = 1._rkind_comp/(pgam**2) - 1._rkind_comp
     pgam = max(pgam, 2._rkind_comp)
     ! Set coefficient for use in size_dist_param_basic.
     ! The 3D case is so common and optimizable that we specialize it:

     if (props_loc%eff_dim == 3._rkind_comp) then
        call rising_factorial(pgam+1._rkind_comp, 3,tmp)
        props_loc%shape_coef = pi / 6._rkind_comp * props_loc%rho * tmp
     else
        call rising_factorial(pgam+1._rkind_comp, props_loc%eff_dim,tmp)
        props_loc%shape_coef = pi / 6._rkind_comp * props_loc%rho * tmp
     end if
     ! Limit to between 2 and 50 microns mean size.

     props_loc%lambda_bounds = (pgam+1._rkind_comp)*1._rkind_comp/[50.e-6_rkind_comp, 2.e-6_rkind_comp]

     call size_dist_param_basic(props_loc, qcic, ncic, lamc)

  else
     ! pgam not calculated in this case, so set it to a value likely to
     ! cause an error if it is accidentally used
     ! (gamma function undefined for negative integers)
     pgam = -100._rkind_comp
     lamc = 0._rkind_comp
  end if

end subroutine size_dist_param_liq_line
! get cloud droplet size distribution parameters


subroutine size_dist_param_liq_vec(props, qcic, ncic, rho, pgam, lamc, veclen)

  integer, intent(in) :: veclen
  type(mghydrometeorprops), intent(in) :: props
  real(rkind_comp), dimension(veclen), intent(in) :: qcic
  real(rkind_comp), dimension(veclen), intent(inout) :: ncic
  real(rkind_comp), dimension(veclen), intent(in) :: rho
  real(rkind_comp), dimension(veclen), intent(out) :: pgam
  real(rkind_comp), dimension(veclen), intent(out) :: lamc
  type(mghydrometeorprops) :: props_loc
  integer :: i, cnt
  real(rkind_comp) :: tmp(veclen),pgamp1(veclen)
  real(rkind_comp) :: shapeC(veclen),lbnd(veclen),ubnd(veclen)
  !$acc declare create(tmp,pgamp1,shapeC,lbnd,ubnd)
  !$acc declare present(qcic,ncic,rho,pgam,lamc)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,veclen
    if (qcic(i) > qsmall) then
      ! Local copy of properties that can be modified.
      ! (Elemental routines that operate on arrays can't modify scalar
      ! arguments.)
      ! Get pgam from fit to observations of martin et al. 1994
      pgam(i) = 1.0_rkind_comp - 0.7_rkind_comp * exp(-0.008_rkind_comp*1.e-6_rkind_comp*ncic(i)*rho(i))
      pgam(i) = 1._rkind_comp/(pgam(i)**2) - 1._rkind_comp
      pgam(i) = max(pgam(i), 2._rkind_comp)
      pgamp1(i) = pgam(i)+1._rkind_comp
    endif
  enddo
  !$acc end parallel
  if (props%eff_dim == 3._rkind_comp) then
    !do i=1,veclen
    !   call rising_factorial(pgamp1(i),3,tmp(i))
    !enddo
    call rising_factorial(pgamp1,3,tmp,veclen)
  else
    call rising_factorial(pgamp1, props%eff_dim,tmp,veclen)
  endif
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,veclen
    if (qcic(i) > qsmall) then
      ! Set coefficient for use in size_dist_param_basic.
      ! The 3D case is so common and optimizable that we specialize
      ! it:
      shapeC(i) = pi / 6._rkind_comp * props%rho * tmp(i)
      ! Limit to between 2 and 50 microns mean size.
      lbnd(i)   = pgamp1(i)*1._rkind_comp/50.e-6_rkind_comp
      ubnd(i)   = pgamp1(i)*1._rkind_comp/2.e-6_rkind_comp
    endif
  enddo
  !$acc end parallel 
  call size_dist_param_basic(props,qcic,ncic,shapeC,lbnd,ubnd,lamc,veclen)
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,veclen
    if (qcic(i) <= qsmall) then
      ! pgam not calculated in this case, so set it to a value likely to
      ! cause an error if it is accidentally used
      ! (gamma function undefined for negative integers)
      pgam(i) = -100._rkind_comp
      lamc(i) = 0._rkind_comp
    end if
  enddo
  !$acc end parallel

end subroutine size_dist_param_liq_vec

! Basic routine for getting size distribution parameters.

elemental subroutine size_dist_param_basic_line(props, qic, nic, lam, n0)
  !$acc routine seq

  type(MGHydrometeorProps), intent(in) :: props
  real(rkind_comp), intent(in) :: qic
  real(rkind_comp), intent(inout) :: nic

  real(rkind_comp), intent(out) :: lam
  real(rkind_comp), intent(out), optional :: n0

  if (qic > qsmall) then
     ! add upper limit to in-cloud number concentration to prevent
     ! numerical error

     if (limiter_is_on(props%min_mean_mass)) then
        nic = min(nic, qic / props%min_mean_mass)
     end if
     ! lambda = (c n/q)^(1/d)

     lam = (props%shape_coef * nic/qic)**(1._rkind_comp/props%eff_dim)
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
     lam = 0._rkind_comp
  end if

  if (present(n0)) n0 = nic * lam

end subroutine size_dist_param_basic_line

subroutine size_dist_param_basic_vec(props, qic, nic, lam, vlen, n0)

  integer,  intent(in) :: vlen
  type (mghydrometeorprops), intent(in) :: props
  real(rkind_comp), dimension(vlen), intent(in) :: qic
  real(rkind_comp), dimension(vlen), intent(inout) :: nic
  real(rkind_comp), dimension(vlen), intent(out) :: lam
  real(rkind_comp), dimension(vlen), intent(out), optional :: n0
  integer :: i
  integer :: cnt
  logical :: limiterActive
  real(rkind_comp) :: effDim,shapeCoef,ubnd,lbnd, minMass
  !$acc declare present(qic,nic,lam,n0)
  
  !$acc parallel num_gangs(32)
  limiterActive = limiter_is_on(props%min_mean_mass)
  effDim    = props%eff_dim
  shapeCoef = props%shape_coef
  lbnd      = props%lambda_bounds(1)
  ubnd      = props%lambda_bounds(2)
  minMass   = props%min_mean_mass

!NEC$ IVDEP
  !$acc loop vector 
  do i=1,vlen

     if (qic(i) > qsmall) then
        ! add upper limit to in-cloud number concentration to prevent
        ! numerical error

        if (limiterActive) then
           nic(i) = min(nic(i), qic(i) / minMass)
        end if
        ! lambda = (c n/q)^(1/d)

        lam(i) = (shapeCoef * nic(i)/qic(i))**(1._rkind_comp/effDim)
        ! check for slope
        ! adjust vars

        if (lam(i) < lbnd) then
           lam(i) = lbnd
        else if (lam(i) > ubnd) then
           lam(i) = ubnd
        end if
        nic(i) = lam(i)**(effDim) * qic(i)/shapeCoef

     else
        lam(i) = 0._rkind_comp
     end if

  enddo

  if (present(n0)) then 
    !$acc loop vector
    do i=1,vlen
      n0(i) = nic(i) * lam(i)
    enddo
  endif
  !$acc end parallel

end subroutine size_dist_param_basic_vec

subroutine size_dist_param_basic_vect2(props, qic, nic, shapeC,lbnd,ubnd, lam, vlen, n0)

  type (mghydrometeorprops), intent(in) :: props
  integer,                          intent(in) :: vlen
  real(rkind_comp), dimension(vlen), intent(in) :: qic
  real(rkind_comp), dimension(vlen), intent(inout) :: nic
  real(rkind_comp), dimension(vlen), intent(in) :: shapeC,lbnd,ubnd
  real(rkind_comp), dimension(vlen), intent(out) :: lam
  real(rkind_comp), dimension(vlen), intent(out), optional :: n0
  integer :: i
  integer :: cnt
  logical :: limiterActive
  real(rkind_comp) :: effDim,shapeCoef, minMass
  !$acc declare present(qic,nic,shapeC,lbnd,ubnd,lam,n0)
  !$acc parallel num_gangs(32)
  limiterActive = limiter_is_on(props%min_mean_mass)
  effDim    = props%eff_dim
  minMass   = props%min_mean_mass
  !$acc loop vector
  !NEC$ IVDEP
  do i=1,vlen

     if (qic(i) > qsmall) then
        ! add upper limit to in-cloud number concentration to prevent
        ! numerical error

        if (limiterActive) then
           nic(i) = min(nic(i), qic(i) / minMass)
        end if
        ! lambda = (c n/q)^(1/d)

        lam(i) = (shapeC(i) * nic(i)/qic(i))**(1._rkind_comp/effDim)
        ! check for slope
        ! adjust vars

        if (lam(i) < lbnd(i)) then
           lam(i) = lbnd(i)
        !   nic(i) = lam(i)**(effDim) * qic(i)/shapeC(i)
        else if (lam(i) > ubnd(i)) then
           lam(i) = ubnd(i)
        end if
        nic(i) = lam(i)**(effDim) * qic(i)/shapeC(i)

     else
        lam(i) = 0._rkind_comp
     end if

  enddo

  if (present(n0)) then 
    !$acc loop vector
    do i=1,vlen
      n0(i) = nic(i) * lam(i)
    enddo
  endif
  !$acc end parallel

end subroutine size_dist_param_basic_vect2

real(rkind_comp) elemental function avg_diameter(q, n, rho_air, rho_sub)
  ! Finds the average diameter of particles given their density, and
  ! mass/number concentrations in the air.
  ! Assumes that diameter follows an exponential distribution.
  real(rkind_comp), intent(in) :: q         ! mass mixing ratio
  real(rkind_comp), intent(in) :: n         ! number concentration (per volume)
  real(rkind_comp), intent(in) :: rho_air   ! local density of the air
  real(rkind_comp), intent(in) :: rho_sub   ! density of the particle substance

  avg_diameter = (pi * rho_sub * n/(q*rho_air))**(-1._rkind_comp/3._rkind_comp)

end function avg_diameter

subroutine  avg_diameter_vec(q, n, rho_air, rho_sub, avg_diameter, vlen)

   ! Finds the average diameter of particles given their density, and
   ! mass/number concentrations in the air.
   ! Assumes that diameter follows an exponential distribution.
   integer,  intent(in) :: vlen
   real(rkind_comp), intent(in) :: q(vlen)         ! mass mixing ratio
   real(rkind_comp), intent(in) :: n(vlen)         ! number concentration (per volume)
   real(rkind_comp), intent(in) :: rho_air(vlen)   ! local density of the air
   real(rkind_comp), intent(in) :: rho_sub   ! density of the particle substance
   real(rkind_comp), intent(out) :: avg_diameter(vlen)
   integer :: i
   real(rkind_comp) :: den
   !$acc parallel num_gangs(32) 
   !$acc loop vector
   do i=1,vlen
       !original 
       !avg_diameter(i) = (pi * rho_sub * n(i)/(q(i)*rho_air(i)))**(-1._rkind_comp/3._rkind_comp)
       !modified
       avg_diameter(i) = ((q(i)*rho_air(i))/(pi * rho_sub * n(i)))**(1._rkind_comp/3._rkind_comp)
   enddo
   !$acc end parallel
 
end subroutine avg_diameter_vec


subroutine var_coef_rkind_comp(relvar, a, res)
  !$acc routine seq
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  real(rkind_comp), intent(in) :: relvar
  real(rkind_comp), intent(in) :: a
  real(rkind_comp), intent(out) :: res
  real(rkind_comp) :: tmp

  call rising_factorial(relvar, a,tmp)
  res = tmp / relvar**a

end subroutine var_coef_rkind_comp

subroutine var_coef_v8(relvar, a, res, vlen)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  integer :: vlen
  real(rkind_comp), intent(in) :: relvar(vlen)
  real(rkind_comp), intent(in) :: a
  real(rkind_comp), intent(out) :: res(vlen)
  integer :: i
  real(rkind_comp) :: tmpA(vlen)
  !$acc declare create(tmpA)
  !$acc declare present(relvar,res)

   call rising_factorial(relvar,a,tmpA,vlen)
   !$acc parallel num_gangs(32)
   !$acc loop vector 
   do i=1,vlen
      res(i) = tmpA(i)/relvar(i)**a
   enddo
   !$acc end parallel

end subroutine var_coef_v8

subroutine var_coef_bug(relvar, a, res, vlen)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  integer :: vlen
  real(rkind_comp), intent(in) :: relvar(vlen)
  real(rkind_comp), intent(in) :: a
  real(rkind_comp), intent(out) :: res(vlen)
  integer :: i
  real(rkind_comp) :: tmpA(vlen)
  real(rkind_comp) :: tmp


#ifdef ALLOCERROR
   call rising_factorial(relvar,a,tmpA,vlen)
   !$acc parallel num_gangs(32)
   !$acc loop vector
   do i=1,vlen
      res(i) = tmpA(i)/relvar(i)**a
   enddo
   !$acc end parallel
#endif

end subroutine var_coef_bug



subroutine var_coef_int(relvar, a, res)

  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  real(rkind_comp), intent(in) :: relvar
  integer, intent(in) :: a
  real(rkind_comp), intent(out) :: res
  real(rkind_comp) :: tmp

  call rising_factorial(relvar, a,tmp)
  res = tmp / relvar**a

end subroutine var_coef_int
subroutine var_coef_vint(relvar, a, res, vlen)
  ! Finds a coefficient for process rates based on the relative variance
  ! of cloud water.
  integer, intent(in) :: vlen
  real(rkind_comp), intent(in) :: relvar(vlen)
  integer, intent(in) :: a
  real(rkind_comp), intent(out) :: res(vlen)
  integer :: i
  real(rkind_comp) :: tmp(vlen)
  !$acc declare create(tmp)

  call rising_factorial(relvar, a,tmp,vlen)
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     res(i) = tmp(i) / relvar(i)**a
  enddo
  !$acc end parallel

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
  real(rkind_comp), dimension(mgncol), intent(in) :: t
  real(rkind_comp), dimension(mgncol), intent(in) :: qv
  real(rkind_comp), dimension(mgncol), intent(in) :: qi
  real(rkind_comp), dimension(mgncol), intent(in) :: ni
  real(rkind_comp), dimension(mgncol), intent(in) :: icldm
  real(rkind_comp), dimension(mgncol), intent(in) :: rho
  real(rkind_comp), dimension(mgncol), intent(in) :: dv
  real(rkind_comp), dimension(mgncol), intent(in) :: qvl
  real(rkind_comp), dimension(mgncol), intent(in) :: qvi
  !OUTPUT VARS:
  !===============================================

  real(rkind_comp), dimension(mgncol), intent(out) :: vap_dep !ice deposition (cell-ave value)
  real(rkind_comp), dimension(mgncol), intent(out) :: ice_sublim !ice sublimation (cell-ave value)
  real(rkind_comp), dimension(mgncol), intent(out) :: berg !bergeron enhancement (cell-ave value)
  !INTERNAL VARS:
  !===============================================

  real(rkind_comp) :: ab(mgncol)
  real(rkind_comp) :: epsi
  real(rkind_comp) :: qiic(mgncol)
  real(rkind_comp) :: niic(mgncol)
  real(rkind_comp) :: lami(mgncol)
  real(rkind_comp) :: n0i(mgncol)
  integer :: i
  !$acc declare create(ab,qiic,niic,lami,n0i)

!  print *,'count(qi>qsmall), mgncol: ',count(qi>qsmall),mgncol
  !NEC$ IVDEP
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qi(i)>=qsmall) then
        !GET IN-CLOUD qi, ni
        !===============================================
        qiic(i) = qi(i)/icldm(i)
        niic(i) = ni(i)/icldm(i)
        call calc_ab(t(i), qvi(i), xxls, ab(i))
        !call size_dist_param_basic(mg_ice_props, qiic(i), niic(i), lami(i),n0=n0i(i))
     endif
  enddo
  !$acc end parallel
  !Get depletion timescale=1/eps
  !call calc_ab(t, qvi, xxls, ab,mgncol)
  !Get slope and intercept of gamma distn for ice.
  !call size_dist_param_basic(mg_ice_props, qiic(i), niic(i), lami(i),n0=n0i(i))
  call size_dist_param_basic_vec(mg_ice_props, qiic, niic, lami,mgncol,n0=n0i)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  !NEC$ IVDEP
  do i=1,mgncol
     if (qi(i)>=qsmall) then

        epsi = 2._rkind_comp*pi*n0i(i)*rho(i)*Dv(i)/(lami(i)*lami(i))
        !Compute deposition/sublimation

        vap_dep(i) = epsi/ab(i)*(qv(i) - qvi(i))
        !Make this a grid-averaged quantity

        vap_dep(i)=vap_dep(i)*icldm(i)
        !Split into deposition or sublimation.

        if (t(i) < tmelt .and. vap_dep(i)>0._rkind_comp) then
           ice_sublim(i)=0._rkind_comp
        else
        ! make ice_sublim negative for consistency with other evap/sub processes
           ice_sublim(i)=min(vap_dep(i),0._rkind_comp)
           vap_dep(i)=0._rkind_comp
        end if
        !sublimation occurs @ any T. Not so for berg.

        if (t(i) < tmelt) then
           !Compute bergeron rate assuming cloud for whole step.

           berg(i) = max(epsi/ab(i)*(qvl(i) - qvi(i)), 0._rkind_comp)
        else !T>frz
           berg(i)=0._rkind_comp
        end if !T<frz

     else !where qi<qsmall
        berg(i)=0._rkind_comp
        vap_dep(i)=0._rkind_comp
        ice_sublim(i)=0._rkind_comp
     end if !qi>qsmall
  enddo
  !$acc end parallel
end subroutine ice_deposition_sublimation
!========================================================================
! autoconversion of cloud liquid water to rain
! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
! minimum qc of 1 x 10^-8 prevents floating point error


subroutine kk2000_liq_autoconversion(microp_uniform, qcic, &
     ncic, rho, relvar, prc, nprc, nprc1, vlen)
  integer, intent(in) :: vlen 
  logical, intent(in) :: microp_uniform

  real(rkind_comp), dimension(vlen), intent(in) :: qcic
  real(rkind_comp), dimension(vlen), intent(in) :: ncic
  real(rkind_comp), dimension(vlen), intent(in) :: rho

  real(rkind_comp), dimension(vlen), intent(in) :: relvar

  real(rkind_comp), dimension(vlen), intent(out) :: prc
  real(rkind_comp), dimension(vlen), intent(out) :: nprc
  real(rkind_comp), dimension(vlen), intent(out) :: nprc1

  real(rkind_comp), dimension(vlen) :: prc_coef
  integer :: i
  ! Take variance into account, or use uniform value.
  !$acc declare create(prc_coef)
  !$acc declare present(qcic,ncic,rho,relvar,prc,nprc,nprc1)

  if (.not. microp_uniform) then
     call var_coef(relvar, 2.47_rkind_comp, prc_coef,vlen)
  else
     !$acc parallel num_gangs(32)
     !$acc loop vector
     do i=1,vlen
       prc_coef(i) = 1._rkind_comp
     enddo
     !$acc end parallel
  end if
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     if (qcic(i) >= icsmall) then
        ! nprc is increase in rain number conc due to autoconversion
        ! nprc1 is decrease in cloud droplet conc due to autoconversion
        ! assume exponential sub-grid distribution of qc, resulting in additional
        ! factor related to qcvar below
        ! switch for sub-columns, don't include sub-grid qc


        prc(i) = prc_coef(i) * &
             0.01_rkind_comp * 1350._rkind_comp * qcic(i)**2.47_rkind_comp * (ncic(i)*1.e-6_rkind_comp*rho(i))**(-1.1_rkind_comp)
        nprc(i) = prc(i) * (1._rkind_comp/droplet_mass_25um)
        nprc1(i) = prc(i)*ncic(i)/qcic(i)

     else
        prc(i)=0._rkind_comp
        nprc(i)=0._rkind_comp
        nprc1(i)=0._rkind_comp
     end if
  enddo
  !$acc end parallel
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
  
  real(rkind_comp), dimension(mgncol), intent (in)    :: pgam
  real(rkind_comp), dimension(mgncol), intent (in)    :: qc  ! = qc (cld water mixing ratio)
  real(rkind_comp), dimension(mgncol), intent (in)    :: nc  ! = nc (cld water number conc /kg)    
  real(rkind_comp), dimension(mgncol), intent (in)    :: qr  ! = qr (rain water mixing ratio)
  real(rkind_comp), dimension(mgncol), intent (in)    :: rho ! = rho : density profile
  real(rkind_comp), dimension(mgncol), intent (in)    :: relvar 
  
  real(rkind_comp), dimension(mgncol), intent (out)   :: au ! = prc autoconversion rate
  real(rkind_comp), dimension(mgncol), intent (out)   :: nprc1 ! = number tendency
  real(rkind_comp), dimension(mgncol), intent (out)   :: nprc ! = number tendency fixed size for rain
  ! parameters for droplet mass spectral shape, 
  !used by Seifert and Beheng (2001)                             
  ! warm rain scheme only (iparam = 1)                                                                        
 
  real(rkind_comp), parameter :: dnu(16) = [0._rkind_comp,-0.557_rkind_comp,-0.430_rkind_comp,-0.307_rkind_comp, & 
     -0.186_rkind_comp,-0.067_rkind_comp,0.050_rkind_comp,0.167_rkind_comp,0.282_rkind_comp,0.397_rkind_comp,0.512_rkind_comp, &
     0.626_rkind_comp,0.739_rkind_comp,0.853_rkind_comp,0.966_rkind_comp,0.966_rkind_comp]
  ! parameters for Seifert and Beheng (2001) autoconversion/accretion                                         

  real(rkind_comp), parameter :: kc = 9.44e9_rkind_comp
  real(rkind_comp), parameter :: kr = 5.78e3_rkind_comp
  real(rkind_comp) :: dum, dum1, nu, pra_coef
  integer :: dumi, i
  !$acc declare create(dnu)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol

    !pra_coef = var_coef(relvar(i), 2.47_rkind_comp)
    ! Why are we calling var_coef here??
    ! call  var_coef(relvar(i), 2.47_rkind_comp,pra_coef)

     if (qc(i) > qsmall) then
       dumi=int(pgam(i))
       nu=dnu(dumi)+(dnu(dumi+1)-dnu(dumi))* &
               (pgam(i)-dumi)

       dum = 1._rkind_comp-qc(i)/(qc(i)+qr(i))
       dum1 = 600._rkind_comp*dum**0.68_rkind_comp*(1._rkind_comp-dum**0.68_rkind_comp)**3

       au(i) = kc/(20._rkind_comp*2.6e-7_rkind_comp)* &
         (nu+2._rkind_comp)*(nu+4._rkind_comp)/(nu+1._rkind_comp)**2._rkind_comp* &
         (rho(i)*qc(i)/1000._rkind_comp)**4._rkind_comp/(rho(i)*nc(i)/1.e6_rkind_comp)**2._rkind_comp* &
         (1._rkind_comp+dum1/(1._rkind_comp-dum)**2)*1000._rkind_comp / rho(i)

       nprc1(i) = au(i)*2._rkind_comp/2.6e-7_rkind_comp*1000._rkind_comp
       nprc(i) = au(i)/droplet_mass_40um
     else
       au(i) = 0._rkind_comp
       nprc1(i) = 0._rkind_comp
       nprc(i)=0._rkind_comp
     end if
  
  enddo
  !$acc end parallel

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
  
  real(rkind_comp), dimension(mgncol), intent (in)    :: qc  ! = qc (cld water mixing ratio)
  real(rkind_comp), dimension(mgncol), intent (in)    :: nc  ! = nc (cld water number conc /kg)    
  real(rkind_comp), dimension(mgncol), intent (in)    :: qr  ! = qr (rain water mixing ratio)
  real(rkind_comp), dimension(mgncol), intent (in)    :: rho ! = rho : density profile
  real(rkind_comp), dimension(mgncol), intent (in)    :: relvar
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: pra  ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: npra ! Number
  ! parameters for Seifert and Beheng (2001) autoconversion/accretion                                         

  real(rkind_comp), parameter :: kc = 9.44e9_rkind_comp
  real(rkind_comp), parameter :: kr = 5.78e3_rkind_comp

  real(rkind_comp) :: dum, dum1
  integer :: i
  ! accretion


  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i =1,mgncol

    if (qc(i) > qsmall) then
      dum = 1._rkind_comp-qc(i)/(qc(i)+qr(i))
      dum1 = (dum/(dum+5.e-4_rkind_comp))**4._rkind_comp
      pra(i) = kr*rho(i)*0.001_rkind_comp*qc(i)*qr(i)*dum1
      npra(i) = pra(i)*rho(i)*0.001_rkind_comp*(nc(i)*rho(i)*1.e-6_rkind_comp)/ &
           (qc(i)*rho(i)*0.001_rkind_comp)*1.e6_rkind_comp / rho(i)
    else
      pra(i) = 0._rkind_comp
      npra(i) = 0._rkind_comp
    end if 
  
  enddo
  !$acc end parallel
 
  end subroutine sb2001v2_accre_cld_water_rain   
!========================================================================
! Autoconversion of cloud ice to snow
! similar to Ferrier (1994)


subroutine ice_autoconversion(t, qiic, lami, n0i, dcs, prci, nprci, mgncol)
  integer, intent(in) :: mgncol
  real(rkind_comp), dimension(mgncol), intent(in) :: t
  real(rkind_comp), dimension(mgncol), intent(in) :: qiic
  real(rkind_comp), dimension(mgncol), intent(in) :: lami
  real(rkind_comp), dimension(mgncol), intent(in) :: n0i
  real(rkind_comp),                    intent(in) :: dcs

  real(rkind_comp), dimension(mgncol), intent(out) :: prci
  real(rkind_comp), dimension(mgncol), intent(out) :: nprci
  ! Assume autoconversion timescale of 180 seconds.

  real(rkind_comp), parameter :: ac_time = 180._rkind_comp
  ! Average mass of an ice particle.

  real(rkind_comp) :: m_ip
  ! Ratio of autoconversion diameter to average diameter.
  real(rkind_comp) :: d_rat
  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (t(i) <= tmelt .and. qiic(i) >= qsmall) then

        d_rat = lami(i)*dcs
        ! Rate of ice particle conversion (number).

        nprci(i) = n0i(i)/(lami(i)*ac_time)*exp(-d_rat)

        m_ip = (rhoi*pi/6._rkind_comp) / lami(i)**3
        ! Rate of mass conversion.
        ! Note that this is:
        ! m n (d^3 + 3 d^2 + 6 d + 6)

        prci(i) = m_ip * nprci(i) * &
             (((d_rat + 3._rkind_comp)*d_rat + 6._rkind_comp)*d_rat + 6._rkind_comp)

     else
        prci(i) = 0._rkind_comp
        nprci(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
end subroutine ice_autoconversion
! immersion freezing (Bigg, 1953)
!===================================


subroutine immersion_freezing(microp_uniform, t, pgam, lamc, &
     qcic, ncic, relvar, mnuccc, nnuccc, mgncol)

  integer, intent(in) :: mgncol
  logical, intent(in) :: microp_uniform
  ! Temperature

  real(rkind_comp), dimension(mgncol), intent(in) :: t
  ! Cloud droplet size distribution parameters

  real(rkind_comp), dimension(mgncol), intent(in) :: pgam
  real(rkind_comp), dimension(mgncol), intent(in) :: lamc
  ! MMR and number concentration of in-cloud liquid water

  real(rkind_comp), dimension(mgncol), intent(in) :: qcic
  real(rkind_comp), dimension(mgncol), intent(in) :: ncic
  ! Relative variance of cloud water

  real(rkind_comp), dimension(mgncol), intent(in) :: relvar
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: mnuccc ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: nnuccc ! Number
  ! Coefficients that will be omitted for sub-columns

  real(rkind_comp), dimension(mgncol) :: dum
  integer :: i
  real(rkind_comp) :: tmp
  !$acc declare create(dum)

  if (.not. microp_uniform) then
     call  var_coef(relvar, 2,dum,mgncol)
  else
     !$acc parallel loop 
     do i=1,mgncol
        dum(i) = 1._rkind_comp
     enddo
  end if
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qcic(i) >= qsmall .and. t(i) < 269.15_rkind_comp) then

        call rising_factorial(pgam(i)+1._rkind_comp, 3,tmp)
        nnuccc(i) = &
             pi/6._rkind_comp*ncic(i)*tmp* &
             bimm*(exp(aimm*(tmelt - t(i)))-1._rkind_comp)/lamc(i)**3

        call rising_factorial(pgam(i)+4._rkind_comp, 3,tmp)
        mnuccc(i) = dum(i) * nnuccc(i) * &
             pi/6._rkind_comp*rhow* &
             tmp/lamc(i)**3

     else
        mnuccc(i) = 0._rkind_comp
        nnuccc(i) = 0._rkind_comp
     end if ! qcic > qsmall and t < 4 deg C
  enddo
  !$acc end parallel

end subroutine immersion_freezing
! contact freezing (-40<T<-3 C) (Young, 1974) with hooks into simulated dust
!===================================================================
! dust size and number in multiple bins are read in from companion routine


subroutine contact_freezing (microp_uniform, t, p, rndst, nacon, &
     pgam, lamc, qcic, ncic, relvar, mnucct, nnucct, mgncol, mdust)

  logical, intent(in) :: microp_uniform

  integer, intent(in) :: mgncol
  integer, intent(in) :: mdust

  real(rkind_comp), dimension(mgncol), intent(in) :: t            ! Temperature
  real(rkind_comp), dimension(mgncol), intent(in) :: p            ! Pressure
  real(rkind_comp), dimension(mgncol, mdust), intent(in) :: rndst ! Radius (for multiple dust bins)
  real(rkind_comp), dimension(mgncol, mdust), intent(in) :: nacon ! Number (for multiple dust bins)
  ! Size distribution parameters for cloud droplets

  real(rkind_comp), dimension(mgncol), intent(in) :: pgam
  real(rkind_comp), dimension(mgncol), intent(in) :: lamc
  ! MMR and number concentration of in-cloud liquid water

  real(rkind_comp), dimension(mgncol), intent(in) :: qcic
  real(rkind_comp), dimension(mgncol), intent(in) :: ncic
  ! Relative cloud water variance

  real(rkind_comp), dimension(mgncol), intent(in) :: relvar
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: mnucct ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: nnucct ! Number

  real(rkind_comp) :: tcnt                  ! scaled relative temperature
  real(rkind_comp) :: viscosity             ! temperature-specific viscosity (kg/m/s)
  real(rkind_comp) :: mfp                   ! temperature-specific mean free path (m)
  ! Dimension these according to number of dust bins, inferred from rndst size

  real(rkind_comp) :: nslip(size(rndst,2))  ! slip correction factors
  real(rkind_comp) :: ndfaer(size(rndst,2)) ! aerosol diffusivities (m^2/sec)
  ! Coefficients not used for subcolumns

  real(rkind_comp) :: dum, dum1,tmp
  ! Common factor between mass and number.

  real(rkind_comp) :: contact_factor

  integer  :: i
  !$acc declare create(nslip,ndfaer)
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i = 1,mgncol

     if (qcic(i) >= qsmall .and. t(i) < 269.15_rkind_comp) then

        if (.not. microp_uniform) then
           !dum = var_coef(relvar(i), 4._rkind_comp/3._rkind_comp)
           call var_coef(relvar(i), 4._rkind_comp/3._rkind_comp,dum)
           !dum1 = var_coef(relvar(i), 1._rkind_comp/3._rkind_comp)
           call var_coef(relvar(i), 1._rkind_comp/3._rkind_comp, dum1)
        else
           dum = 1._rkind_comp
           dum1 = 1._rkind_comp
        endif

        tcnt=(270.16_rkind_comp-t(i))**1.3_rkind_comp
        viscosity = 1.8e-5_rkind_comp*(t(i)/298.0_rkind_comp)**0.85_rkind_comp    ! Viscosity (kg/m/s)
        mfp = 2.0_rkind_comp*viscosity/ &                         ! Mean free path (m)
                     (p(i)*sqrt( 8.0_rkind_comp*28.96e-3_rkind_comp/(pi*8.314409_rkind_comp*t(i)) ))
        ! Note that these two are vectors.

        nslip = 1.0_rkind_comp+(mfp/rndst(i,:))*(1.257_rkind_comp+(0.4_rkind_comp*exp(-(1.1_rkind_comp*rndst(i,:)/mfp))))! Slip correction factor

        ndfaer = 1.381e-23_rkind_comp*t(i)*nslip/(6._rkind_comp*pi*viscosity*rndst(i,:))  ! aerosol diffusivity (m2/s)

        contact_factor = dot_product(ndfaer,nacon(i,:)*tcnt) * pi * &
             ncic(i) * (pgam(i) + 1._rkind_comp) / lamc(i)

        call rising_factorial(pgam(i)+2._rkind_comp, 3,tmp)
        mnucct(i) = dum * contact_factor * &
             pi/3._rkind_comp*rhow*tmp/lamc(i)**3

        nnucct(i) =  dum1 * 2._rkind_comp * contact_factor

     else

        mnucct(i)=0._rkind_comp
        nnucct(i)=0._rkind_comp

     end if ! qcic > qsmall and t < 4 deg C
  end do
  !$acc end parallel

end subroutine contact_freezing
! snow self-aggregation from passarelli, 1978, used by reisner, 1998
!===================================================================
! this is hard-wired for bs = 0.4 for now
! ignore self-collection of cloud ice


subroutine snow_self_aggregation(t, rho, asn, rhosn, qsic, nsic, nsagg, mgncol)

  integer,                          intent(in) :: mgncol

  real(rkind_comp), dimension(mgncol), intent(in) :: t     ! Temperature
  real(rkind_comp), dimension(mgncol), intent(in) :: rho   ! Density
  real(rkind_comp), dimension(mgncol), intent(in) :: asn   ! fall speed parameter for snow
  real(rkind_comp),                    intent(in) :: rhosn ! density of snow
  ! In-cloud snow

  real(rkind_comp), dimension(mgncol), intent(in) :: qsic ! MMR
  real(rkind_comp), dimension(mgncol), intent(in) :: nsic ! Number
  ! Output number tendency

  real(rkind_comp), dimension(mgncol), intent(out) :: nsagg

  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qsic(i) >= qsmall .and. t(i) <= tmelt) then
        nsagg(i) = -1108._rkind_comp*eii/(4._rkind_comp*720._rkind_comp*rhosn)*asn(i)*qsic(i)*nsic(i)*rho(i)*&
             ((qsic(i)/nsic(i))*(1._rkind_comp/(rhosn*pi)))**((bs-1._rkind_comp)/3._rkind_comp)
     else
        nsagg(i)=0._rkind_comp
     end if
  enddo
  !$acc end parallel
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
  real(rkind_comp), dimension(mgncol), intent(in) :: t   ! Temperature
  real(rkind_comp), dimension(mgncol), intent(in) :: rho ! Density
  real(rkind_comp), dimension(mgncol), intent(in) :: asn ! Fallspeed parameter (snow)
  real(rkind_comp), dimension(mgncol), intent(in) :: uns ! Current fallspeed   (snow)
  real(rkind_comp), dimension(mgncol), intent(in) :: mu  ! Viscosity
  ! In-cloud liquid water

  real(rkind_comp), dimension(mgncol), intent(in) :: qcic ! MMR
  real(rkind_comp), dimension(mgncol), intent(in) :: ncic ! Number
  ! In-cloud snow

  real(rkind_comp), dimension(mgncol), intent(in) :: qsic ! MMR
  ! Cloud droplet size parameters

  real(rkind_comp), dimension(mgncol), intent(in) :: pgam
  real(rkind_comp), dimension(mgncol), intent(in) :: lamc
  ! Snow size parameters

  real(rkind_comp), dimension(mgncol), intent(in) :: lams
  real(rkind_comp), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: psacws  ! Mass mixing ratio
  real(rkind_comp), dimension(mgncol), intent(out) :: npsacws ! Number concentration

  real(rkind_comp) :: dc0 ! Provisional mean droplet size
  real(rkind_comp) :: dum
  real(rkind_comp) :: eci ! collection efficiency for riming of snow by droplets
  ! Fraction of cloud droplets accreted per second

  real(rkind_comp) :: accrete_rate
  integer :: i
  ! ignore collision of snow with droplets above freezing


  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qsic(i) >= qsmall .and. t(i) <= tmelt .and. qcic(i) >= qsmall) then
        ! put in size dependent collection efficiency
        ! mean diameter of snow is area-weighted, since
        ! accretion is function of crystal geometric area
        ! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)


        dc0 = (pgam(i)+1._rkind_comp)/lamc(i)
        dum = dc0*dc0*uns(i)*rhow*lams(i)/(9._rkind_comp*mu(i))
        eci = dum*dum/((dum+0.4_rkind_comp)*(dum+0.4_rkind_comp))

        eci = max(eci,0._rkind_comp)
        eci = min(eci,1._rkind_comp)
        ! no impact of sub-grid distribution of qc since psacws
        ! is linear in qc

        accrete_rate = pi/4._rkind_comp*asn(i)*rho(i)*n0s(i)*eci*gamma_bs_plus3 / lams(i)**(bs+3._rkind_comp)
        psacws(i) = accrete_rate*qcic(i)
        npsacws(i) = accrete_rate*ncic(i)
     else
        psacws(i) = 0._rkind_comp
        npsacws(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
end subroutine accrete_cloud_water_snow

subroutine accrete_cloud_water_snow_v2(t, rho, asn, uns, mu, qcic, ncic, qsic, &
     pgam, lamc, lams, n0s, psacws, npsacws,dim1,dim2) 
  !!$acc routine vector

  integer, intent(in) :: dim1,dim2
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: t   ! Temperature
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: rho ! Density
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: asn ! Fallspeed parameter (snow)
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: uns ! Current fallspeed   (snow)
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: mu  ! Viscosity
  ! In-cloud liquid water

  real(rkind_comp), dimension(dim1,dim2), intent(in) :: qcic ! MMR
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: ncic ! Number
  ! In-cloud snow

  real(rkind_comp), dimension(dim1,dim2), intent(in) :: qsic ! MMR
  ! Cloud droplet size parameters

  real(rkind_comp), dimension(dim1,dim2), intent(in) :: pgam
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: lamc
  ! Snow size parameters

  real(rkind_comp), dimension(dim1,dim2), intent(in) :: lams
  real(rkind_comp), dimension(dim1,dim2), intent(in) :: n0s
  ! Output tendencies

  real(rkind_comp), dimension(dim1,dim2), intent(out) :: psacws  ! Mass mixing ratio
  real(rkind_comp), dimension(dim1,dim2), intent(out) :: npsacws ! Number concentration

  real(rkind_comp) :: dc0 ! Provisional mean droplet size
  real(rkind_comp) :: dum
  real(rkind_comp) :: eci ! collection efficiency for riming of snow by droplets
  ! Fraction of cloud droplets accreted per second

  real(rkind_comp) :: accrete_rate
  integer :: i,j
  ! ignore collision of snow with droplets above freezing

  !$acc parallel num_gangs(32)
  !$acc loop vector collapse(2)
  do j=1,dim2
  do i=1,dim1
     if (qsic(i,j) >= qsmall .and. t(i,j) <= tmelt .and. qcic(i,j) >= qsmall) then
        ! put in size dependent collection efficiency
        ! mean diameter of snow is area-weighted, since
        ! accretion is function of crystal geometric area
        ! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)


        dc0 = (pgam(i,j)+1._rkind_comp)/lamc(i,j)
        dum = dc0*dc0*uns(i,j)*rhow*lams(i,j)/(9._rkind_comp*mu(i,j))
        eci = dum*dum/((dum+0.4_rkind_comp)*(dum+0.4_rkind_comp))

        eci = max(eci,0._rkind_comp)
        eci = min(eci,1._rkind_comp)
        ! no impact of sub-grid distribution of qc since psacws
        ! is linear in qc

        accrete_rate = pi/4._rkind_comp*asn(i,j)*rho(i,j)*n0s(i,j)*eci*gamma_bs_plus3 / lams(i,j)**(bs+3._rkind_comp)
        psacws(i,j) = accrete_rate*qcic(i,j)
        npsacws(i,j) = accrete_rate*ncic(i,j)
     else
        psacws(i,j) = 0._rkind_comp
        npsacws(i,j) = 0._rkind_comp
     end if
  enddo
  enddo
  !$acc end parallel
end subroutine accrete_cloud_water_snow_v2
! add secondary ice production due to accretion of droplets by snow
!===================================================================
! (Hallet-Mossop process) (from Cotton et al., 1986)


subroutine secondary_ice_production(t, psacws, msacwi, nsacwi, mgncol)

  integer, intent(in) :: mgncol
  real(rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
  ! Accretion of cloud water to snow tendencies

  real(rkind_comp), dimension(mgncol), intent(inout) :: psacws ! MMR
  ! Output (ice) tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: msacwi ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: nsacwi ! Number
  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if((t(i) < 270.16_rkind_comp) .and. (t(i) >= 268.16_rkind_comp)) then
        nsacwi(i) = 3.5e8_rkind_comp*(270.16_rkind_comp-t(i))/2.0_rkind_comp*psacws(i)
     else if((t(i) < 268.16_rkind_comp) .and. (t(i) >= 265.16_rkind_comp)) then
        nsacwi(i) = 3.5e8_rkind_comp*(t(i)-265.16_rkind_comp)/3.0_rkind_comp*psacws(i)
     else
        nsacwi(i) = 0.0_rkind_comp
     endif
  enddo

  !$acc loop vector
  do i=1,mgncol
     msacwi(i) = min(nsacwi(i)*mi0, psacws(i))
     psacws(i) = psacws(i) - msacwi(i)
  enddo
  !$acc end parallel
end subroutine secondary_ice_production
! accretion of rain water by snow
!===================================================================
! formula from ikawa and saito, 1991, used by reisner et al., 1998


subroutine accrete_rain_snow(t, rho, umr, ums, unr, uns, qric, qsic, &
     lamr, n0r, lams, n0s, pracs, npracs, mgncol)

  integer,                          intent(in) :: mgncol

  real(rkind_comp), dimension(mgncol), intent(in) :: t   ! Temperature
  real(rkind_comp), dimension(mgncol), intent(in) :: rho ! Density
  ! Fallspeeds
  ! mass-weighted

  real(rkind_comp), dimension(mgncol), intent(in) :: umr ! rain
  real(rkind_comp), dimension(mgncol), intent(in) :: ums ! snow
  ! number-weighted
  real(rkind_comp), dimension(mgncol), intent(in) :: unr ! rain
  real(rkind_comp), dimension(mgncol), intent(in) :: uns ! snow
  ! In cloud MMRs

  real(rkind_comp), dimension(mgncol), intent(in) :: qric ! rain
  real(rkind_comp), dimension(mgncol), intent(in) :: qsic ! snow
  ! Size distribution parameters
  ! rain

  real(rkind_comp), dimension(mgncol), intent(in) :: lamr
  real(rkind_comp), dimension(mgncol), intent(in) :: n0r
  ! snow
  real(rkind_comp), dimension(mgncol), intent(in) :: lams
  real(rkind_comp), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: pracs  ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: npracs ! Number
  ! Collection efficiency for accretion of rain by snow

  real(rkind_comp), parameter :: ecr = 1.0_rkind_comp
  ! Ratio of average snow diameter to average rain diameter.

  real(rkind_comp) :: d_rat
  ! Common factor between mass and number expressions
  real(rkind_comp) :: common_factor
  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qric(i) >= icsmall .and. qsic(i) >= icsmall .and. t(i) <= tmelt) then

        common_factor = pi*ecr*rho(i)*n0r(i)*n0s(i)/(lamr(i)**3 * lams(i))

        d_rat = lamr(i)/lams(i)

        pracs(i) = common_factor*pi*rhow* &
             sqrt((1.2_rkind_comp*umr(i)-0.95_rkind_comp*ums(i))**2 + 0.08_rkind_comp*ums(i)*umr(i)) / lamr(i)**3 * &
             ((0.5_rkind_comp*d_rat + 2._rkind_comp)*d_rat + 5._rkind_comp)

        npracs(i) = common_factor*0.5_rkind_comp* &
             sqrt(1.7_rkind_comp*(unr(i)-uns(i))**2 + 0.3_rkind_comp*unr(i)*uns(i)) * &
             ((d_rat + 1._rkind_comp)*d_rat + 1._rkind_comp)

     else
        pracs(i) = 0._rkind_comp
        npracs(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
end subroutine accrete_rain_snow
! heterogeneous freezing of rain drops
!===================================================================
! follows from Bigg (1953)


subroutine heterogeneous_rain_freezing(t, qric, nric, lamr, mnuccr, nnuccr, mgncol)
  integer,                          intent(in) :: mgncol
  real(rkind_comp), dimension(mgncol), intent(in) :: t    ! Temperature
  ! In-cloud rain

  real(rkind_comp), dimension(mgncol), intent(in) :: qric ! MMR
  real(rkind_comp), dimension(mgncol), intent(in) :: nric ! Number
  real(rkind_comp), dimension(mgncol), intent(in) :: lamr ! size parameter
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: mnuccr ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: nnuccr ! Number
  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol

     if (t(i) < 269.15_rkind_comp .and. qric(i) >= qsmall) then
        nnuccr(i) = pi*nric(i)*bimm* &
             (exp(aimm*(tmelt - t(i)))-1._rkind_comp)/lamr(i)**3

        mnuccr(i) = nnuccr(i) * 20._rkind_comp*pi*rhow/lamr(i)**3

     else
        mnuccr(i) = 0._rkind_comp
        nnuccr(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
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
  real(rkind_comp), dimension(mgncol), intent(in) :: qric ! MMR
  ! Cloud droplets

  real(rkind_comp), dimension(mgncol), intent(in) :: qcic ! MMR
  real(rkind_comp), dimension(mgncol), intent(in) :: ncic ! Number
  ! SGS variability

  real(rkind_comp), dimension(mgncol), intent(in) :: relvar
  real(rkind_comp), dimension(mgncol), intent(in) :: accre_enhan
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: pra  ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: npra ! Number
  ! Coefficient that varies for subcolumns

  real(rkind_comp), dimension(mgncol) :: pra_coef

  integer :: i
  !$acc declare present(qric,qcic,ncic,relvar,accre_enhan,pra,npra)
  !$acc declare create(pra_coef)

  if (.not. microp_uniform) then
       ! This subroutine gives a fortran allocation error on the GPU
       call  var_coef(relvar, 1.15_rkind_comp, pra_coef,mgncol)
       !$acc parallel loop 
       do i=1,mgncol
          pra_coef(i) = pra_coef(i)*accre_enhan(i)
       enddo
  else
    !$acc parallel loop 
    do i=1,mgncol
       pra_coef(i) = 1._rkind_comp
    enddo
  end if
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
    if (qric(i) >= qsmall .and. qcic(i) >= qsmall) then
      ! include sub-grid distribution of cloud water

      pra(i) = pra_coef(i) * 67._rkind_comp*(qcic(i)*qric(i))**1.15_rkind_comp

      npra(i) = pra(i)*ncic(i)/qcic(i)

    else
      pra(i) = 0._rkind_comp
      npra(i) = 0._rkind_comp
    end if
  end do
  !$acc end parallel
end subroutine accrete_cloud_water_rain
! Self-collection of rain drops
!===================================================================
! from Beheng(1994)


subroutine self_collection_rain(rho, qric, nric, nragg, mgncol)
  integer,                          intent(in) :: mgncol
  real(rkind_comp), dimension(mgncol), intent(in) :: rho  ! Air density
  ! Rain

  real(rkind_comp), dimension(mgncol), intent(in) :: qric ! MMR
  real(rkind_comp), dimension(mgncol), intent(in) :: nric ! Number
  ! Output number tendency

  real(rkind_comp), dimension(mgncol), intent(out) :: nragg

  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qric(i) >= qsmall) then
        nragg(i) = -8._rkind_comp*nric(i)*qric(i)*rho(i)
     else
        nragg(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
end subroutine self_collection_rain
! Accretion of cloud ice by snow
!===================================================================
! For this calculation, it is assumed that the Vs >> Vi
! and Ds >> Di for continuous collection


subroutine accrete_cloud_ice_snow(t, rho, asn, qiic, niic, qsic, &
     lams, n0s, prai, nprai, mgncol)

  integer,                          intent(in) :: mgncol
  real(rkind_comp), dimension(mgncol), intent(in) :: t    ! Temperature
  real(rkind_comp), dimension(mgncol), intent(in) :: rho   ! Density

  real(rkind_comp), dimension(mgncol), intent(in) :: asn  ! Snow fallspeed parameter
  ! Cloud ice

  real(rkind_comp), dimension(mgncol), intent(in) :: qiic ! MMR
  real(rkind_comp), dimension(mgncol), intent(in) :: niic ! Number

  real(rkind_comp), dimension(mgncol), intent(in) :: qsic ! Snow MMR
  ! Snow size parameters

  real(rkind_comp), dimension(mgncol), intent(in) :: lams
  real(rkind_comp), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: prai ! MMR
  real(rkind_comp), dimension(mgncol), intent(out) :: nprai ! Number
  ! Fraction of cloud ice particles accreted per second

  real(rkind_comp) :: accrete_rate

  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qsic(i) >= qsmall .and. qiic(i) >= qsmall .and. t(i) <= tmelt) then

        accrete_rate = pi/4._rkind_comp * eii * asn(i) * rho(i) * n0s(i) * gamma_bs_plus3/ &
             lams(i)**(bs+3._rkind_comp)

        prai(i) = accrete_rate * qiic(i)
        nprai(i) = accrete_rate * niic(i)

     else
        prai(i) = 0._rkind_comp
        nprai(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
end subroutine accrete_cloud_ice_snow
! calculate evaporation/sublimation of rain and snow
!===================================================================
! note: evaporation/sublimation occurs only in cloud-free portion of grid cell
! in-cloud condensation/deposition of rain and snow is neglected
! except for transfer of cloud water to snow through bergeron process


subroutine evaporate_sublimate_precip(t, rho, dv, mu, sc, q, qvl, qvi, &
     lcldm, precip_frac, arn, asn, qcic, qiic, qric, qsic, lamr, n0r, lams, n0s, &
     pre, prds, am_evp_st, vlen)
  !!$acc routine vector

  integer,  intent(in) :: vlen

  real(rkind_comp), dimension(vlen), intent(in) :: t    ! temperature

  real(rkind_comp), dimension(vlen), intent(in) :: rho  ! air density
  real(rkind_comp), dimension(vlen), intent(in) :: dv   ! water vapor diffusivity
  real(rkind_comp), dimension(vlen), intent(in) :: mu   ! viscosity
  real(rkind_comp), dimension(vlen), intent(in) :: sc   ! schmidt number
  real(rkind_comp), dimension(vlen), intent(in) :: q    ! humidity
  real(rkind_comp), dimension(vlen), intent(in) :: qvl  ! saturation humidity (water)
  real(rkind_comp), dimension(vlen), intent(in) :: qvi  ! saturation humidity (ice)
  real(rkind_comp), dimension(vlen), intent(in) :: lcldm  ! liquid cloud fraction
  real(rkind_comp), dimension(vlen), intent(in) :: precip_frac ! precipitation fraction (maximum overlap)
  ! fallspeed parameters

  real(rkind_comp), dimension(vlen), intent(in) :: arn  ! rain
  real(rkind_comp), dimension(vlen), intent(in) :: asn  ! snow
  ! In-cloud MMRs

  real(rkind_comp), dimension(vlen), intent(in) :: qcic ! cloud liquid
  real(rkind_comp), dimension(vlen), intent(in) :: qiic ! cloud ice
  real(rkind_comp), dimension(vlen), intent(in) :: qric ! rain
  real(rkind_comp), dimension(vlen), intent(in) :: qsic ! snow
  ! Size parameters
  ! rain

  real(rkind_comp), dimension(vlen), intent(in) :: lamr
  real(rkind_comp), dimension(vlen), intent(in) :: n0r
  ! snow
  real(rkind_comp), dimension(vlen), intent(in) :: lams
  real(rkind_comp), dimension(vlen), intent(in) :: n0s
  ! Output tendencies

  real(rkind_comp), dimension(vlen), intent(out) :: pre
  real(rkind_comp), dimension(vlen), intent(out) :: prds
  real(rkind_comp), dimension(vlen), intent(out) :: am_evp_st ! Fractional area where rain evaporates.

  real(rkind_comp) :: qclr   ! water vapor mixing ratio in clear air
  real(rkind_comp) :: abr(vlen),abs(vlen)     ! correction to account for latent heat
  real(rkind_comp) :: eps    ! 1/ sat relaxation timescale

  real(rkind_comp), dimension(vlen) :: dum

  integer :: i
  !$acc declare create(abr,abs,dum)
  !$acc declare present(t,rho,dv,mu,sc,q,qvl,qvi,lcldm,precip_frac,arn,asn,qcic,qiic,qric,qsic)
  !$acc declare present(lamr,n0r,lams,n0s,pre,prds,am_evp_st)
  !logical, dimension(vlen) :: cond1,cond2,cond3

  ! set temporary cloud fraction to zero if cloud water + ice is very small
  ! this will ensure that evaporation/sublimation of precip occurs over
  ! entire grid cell, since min cloud fraction is specified otherwise
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     am_evp_st(i) = 0._rkind_comp
     if (qcic(i)+qiic(i) < 1.e-6_rkind_comp) then
        dum(i) = 0._rkind_comp
     else
        dum(i) = lcldm(i)
     end if
  enddo
  !$acc loop vector
  do i=1,vlen
  ! only calculate if there is some precip fraction > cloud fraction

     if (precip_frac(i) > dum(i)) then

        ! evaporation of rain
        if (qric(i) >= qsmall) then
           am_evp_st(i) = precip_frac(i) - dum(i)
           ! calculate q for out-of-cloud region

           qclr=(q(i)-dum(i)*qvl(i))/(1._rkind_comp-dum(i))

           !ab = calc_ab(t(i), qvl(i), xxlv)
           call calc_ab(t(i), qvl(i), xxlv, abr(i))
           eps = 2._rkind_comp*pi*n0r(i)*rho(i)*Dv(i)* &
                (f1r/(lamr(i)*lamr(i))+ &
                f2r*(arn(i)*rho(i)/mu(i))**0.5_rkind_comp* &
                sc(i)**(1._rkind_comp/3._rkind_comp)*gamma_half_br_plus5/ &
                (lamr(i)**(5._rkind_comp/2._rkind_comp+br/2._rkind_comp)))

           pre(i) = eps*(qclr-qvl(i))/abr(i)
           ! only evaporate in out-of-cloud region
           ! and distribute across precip_frac

           pre(i)=min(pre(i)*am_evp_st(i),0._rkind_comp)
           pre(i)=pre(i)/precip_frac(i)
        else
           pre(i) = 0._rkind_comp
        end if

        ! sublimation of snow
        if (qsic(i) >= qsmall) then
           am_evp_st(i) = precip_frac(i) - dum(i)
           ! calculate q for out-of-cloud region

           qclr=(q(i)-dum(i)*qvl(i))/(1._rkind_comp-dum(i))
           ! ab = calc_ab(t(i), qvi(i), xxls)
           call calc_ab(t(i), qvi(i), xxls, abs(i))
           eps = 2._rkind_comp*pi*n0s(i)*rho(i)*Dv(i)* &
                (f1s/(lams(i)*lams(i))+ &
                f2s*(asn(i)*rho(i)/mu(i))**0.5_rkind_comp* &
                sc(i)**(1._rkind_comp/3._rkind_comp)*gamma_half_bs_plus5/ &
                (lams(i)**(5._rkind_comp/2._rkind_comp+bs/2._rkind_comp)))
           prds(i) = eps*(qclr-qvi(i))/abs(i)
           ! only sublimate in out-of-cloud region and distribute over precip_frac

           prds(i)=min(prds(i)*am_evp_st(i),0._rkind_comp)
           prds(i)=prds(i)/precip_frac(i)
        else
           prds(i) = 0._rkind_comp
        end if

     else
        prds(i) = 0._rkind_comp
        pre(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel

end subroutine evaporate_sublimate_precip
! bergeron process - evaporation of droplets and deposition onto snow
!===================================================================


subroutine bergeron_process_snow(t, rho, dv, mu, sc, qvl, qvi, asn, &
     qcic, qsic, lams, n0s, bergs, mgncol)

  integer, intent(in) :: mgncol

  real(rkind_comp), dimension(mgncol), intent(in) :: t    ! temperature
  real(rkind_comp), dimension(mgncol), intent(in) :: rho  ! air density
  real(rkind_comp), dimension(mgncol), intent(in) :: dv   ! water vapor diffusivity
  real(rkind_comp), dimension(mgncol), intent(in) :: mu   ! viscosity
  real(rkind_comp), dimension(mgncol), intent(in) :: sc   ! schmidt number
  real(rkind_comp), dimension(mgncol), intent(in) :: qvl  ! saturation humidity (water)
  real(rkind_comp), dimension(mgncol), intent(in) :: qvi  ! saturation humidity (ice)
  ! fallspeed parameter for snow

  real(rkind_comp), dimension(mgncol), intent(in) :: asn
  ! In-cloud MMRs

  real(rkind_comp), dimension(mgncol), intent(in) :: qcic ! cloud liquid
  real(rkind_comp), dimension(mgncol), intent(in) :: qsic ! snow
  ! Size parameters for snow

  real(rkind_comp), dimension(mgncol), intent(in) :: lams
  real(rkind_comp), dimension(mgncol), intent(in) :: n0s
  ! Output tendencies

  real(rkind_comp), dimension(mgncol), intent(out) :: bergs

  real(rkind_comp) :: ab     ! correction to account for latent heat
  real(rkind_comp) :: eps    ! 1/ sat relaxation timescale

  integer :: i

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,mgncol
     if (qsic(i) >= qsmall.and. qcic(i) >= qsmall .and. t(i) < tmelt) then
        ! ab = calc_ab(t(i), qvi(i), xxls)
        call calc_ab(t(i), qvi(i), xxls, ab)
        eps = 2._rkind_comp*pi*n0s(i)*rho(i)*Dv(i)* &
             (f1s/(lams(i)*lams(i))+ &
             f2s*(asn(i)*rho(i)/mu(i))**0.5_rkind_comp* &
             sc(i)**(1._rkind_comp/3._rkind_comp)*gamma_half_bs_plus5/ &
             (lams(i)**(5._rkind_comp/2._rkind_comp+bs/2._rkind_comp)))
        bergs(i) = eps*(qvl(i)-qvi(i))/ab
     else
        bergs(i) = 0._rkind_comp
     end if
  enddo
  !$acc end parallel
end subroutine bergeron_process_snow
!========================================================================
!UTILITIES
!========================================================================


pure function limiter_is_on(lim)
!$acc routine seq

  real(rkind_comp), intent(in) :: lim
  logical :: limiter_is_on

  limiter_is_on = transfer(lim, limiter_off) /= limiter_off

end function limiter_is_on

!read state subroutine for kr_externs_in_micro_mg_utils 
SUBROUTINE kr_externs_in_micro_mg_utils(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    real(kind=rkind_io) :: tmp
      
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_liq_props, kgen_unit, "mg_liq_props", .FALSE.) 
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_ice_props, kgen_unit, "mg_ice_props", .FALSE.) 
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_rain_props, kgen_unit, "mg_rain_props", .FALSE.) 
    CALL kr_micro_mg_utils_mghydrometeorprops(mg_snow_props, kgen_unit, "mg_snow_props", .FALSE.) 
    READ (UNIT = kgen_unit) tmp; rv  = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: rv: ',rv
    READ (UNIT = kgen_unit) tmp; cpp = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: cpp: ',cpp
    READ (UNIT = kgen_unit) tmp; tmelt = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: tmelt: ',tmelt
    READ (UNIT = kgen_unit) tmp; xxlv  = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: xxlv: ',xxlv
    READ (UNIT = kgen_unit) tmp; xxls  = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: xxls: ',xxls
    READ (UNIT = kgen_unit) tmp; gamma_bs_plus3  = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: gamma_bs_plus3: ',gamma_bs_plus3
    READ (UNIT = kgen_unit) tmp; gamma_half_br_plus5 = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: gamma_half_br_plus5: ',gamma_half_br_plus5
    READ (UNIT = kgen_unit) tmp; gamma_half_bs_plus5 = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_micro_mg_utils: gamma_half_bs_plus5: ',gamma_half_bs_plus5
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
    REAL(kind=rkind_io) :: tmp,tmp2(2)
      
    READ (UNIT = kgen_unit) tmp; var%rho = real(tmp,kind=rkind_comp)
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%rho = ", var%rho 
    END IF   
      
    READ (UNIT = kgen_unit) tmp; var%eff_dim = real(tmp,kind=rkind_comp)
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%eff_dim = ", var%eff_dim 
    END IF   
      
    READ (UNIT = kgen_unit) tmp; var%shape_coef = real(tmp,kind=rkind_comp)
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%shape_coef = ", var%shape_coef 
    END IF   
      
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) tmp2; var%lambda_bounds = real(tmp2,kind=rkind_comp)
!        print *,'tmp2: ',tmp2
!        print *,'var%lambda_bounds: ',var%lambda_bounds
!        stop 'kr_micro_mg_utils_mghydrometeorprops'
#if 0
        CALL kgen_array_sumcheck(printname // "%lambda_bounds", kgen_array_sum, DBLE(SUM(var%lambda_bounds, &
        &mask=(var%lambda_bounds .eq. var%lambda_bounds))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%lambda_bounds)) = ", DBLE(SUM(var%lambda_bounds, &
            &mask=(var%lambda_bounds .eq. var%lambda_bounds))) 
        END IF   
#endif
    END IF   
      
    READ (UNIT = kgen_unit) tmp; var%min_mean_mass = real(tmp,kind=rkind_comp)
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
      
    real(KIND=rkind_comp) :: diff_rho 
    real(KIND=rkind_comp) :: diff_eff_dim 
    real(KIND=rkind_comp) :: diff_shape_coef 
    INTEGER :: n_lambda_bounds 
    real(KIND=rkind_comp) :: nrmsdiff_lambda_bounds, rmsdiff_lambda_bounds 
    real(KIND=rkind_comp), ALLOCATABLE :: buf1_lambda_bounds(:), buf2_lambda_bounds(:) 
    real(KIND=rkind_comp) :: diff_min_mean_mass 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%rho == kgenref_var%rho) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%rho is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_rho = ABS(var%rho - kgenref_var%rho) 
        IF (diff_rho <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%rho is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%rho is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) "Difference is ", diff_rho 
            WRITE (*, *) "" 
            endif
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) "Difference is ", diff_rho 
            WRITE (*, *) "" 
            endif
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%eff_dim == kgenref_var%eff_dim) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%eff_dim is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_eff_dim = ABS(var%eff_dim - kgenref_var%eff_dim) 
        IF (diff_eff_dim <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%eff_dim is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%eff_dim is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) "Difference is ", diff_eff_dim 
            WRITE (*, *) "" 
            endif
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) "Difference is ", diff_eff_dim 
            WRITE (*, *) "" 
            endif
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%shape_coef == kgenref_var%shape_coef) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%shape_coef is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_shape_coef = ABS(var%shape_coef - kgenref_var%shape_coef) 
        IF (diff_shape_coef <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%shape_coef is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%shape_coef is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) "Difference is ", diff_shape_coef 
            WRITE (*, *) "" 
            endif
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) "Difference is ", diff_shape_coef 
            WRITE (*, *) "" 
            endif
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (ALL(var%lambda_bounds == kgenref_var%lambda_bounds)) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is IDENTICAL." 
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
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        ELSE 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) count( var%lambda_bounds /= kgenref_var%lambda_bounds), " of ", size( var%lambda_bounds ), " elements &
            &are different." 
            WRITE (*, *) "Average - kernel ", sum(var%lambda_bounds)/real(size(var%lambda_bounds)) 
            WRITE (*, *) "Average - reference ", sum(kgenref_var%lambda_bounds)/real(size(kgenref_var%lambda_bounds)) 
            WRITE (*, *) "RMS of difference is ", rmsdiff_lambda_bounds 
            WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lambda_bounds 
            WRITE (*, *) "" 
            endif
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then 
            WRITE (*, *) count( var%lambda_bounds /= kgenref_var%lambda_bounds), " of ", size( var%lambda_bounds ), " elements &
            &are different." 
            WRITE (*, *) "Average - kernel ", sum(var%lambda_bounds)/real(size(var%lambda_bounds)) 
            WRITE (*, *) "Average - reference ", sum(kgenref_var%lambda_bounds)/real(size(kgenref_var%lambda_bounds)) 
            WRITE (*, *) "RMS of difference is ", rmsdiff_lambda_bounds 
            WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lambda_bounds 
            WRITE (*, *) "" 
            endif
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%min_mean_mass == kgenref_var%min_mean_mass) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is IDENTICAL." 
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_min_mean_mass = ABS(var%min_mean_mass - kgenref_var%min_mean_mass) 
        IF (diff_min_mean_mass <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is NOT IDENTICAL(within tolerance)." 
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (check_status%verboseLevel > 1) THEN 
                if(check_status%rank==0) WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is NOT IDENTICAL(out of tolerance)." 
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then
            WRITE (*, *) "Difference is ", diff_min_mean_mass 
            WRITE (*, *) "" 
            endif
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (check_status%verboseLevel > 2) THEN 
            if(check_status%rank==0) then
            WRITE (*, *) "Difference is ", diff_min_mean_mass 
            WRITE (*, *) "" 
            endif
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
