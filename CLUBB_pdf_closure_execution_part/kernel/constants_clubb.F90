!KGEN-generated Fortran source file

!Generated at : 2016-01-04 08:38:23
!KGEN version : 0.6.0

!-----------------------------------------------------------------------------
! $Id: constants_clubb.F90 7140 2014-07-31 19:14:05Z betlej@uwm.edu $
!=============================================================================
module constants_clubb

  ! Description:
  ! Contains frequently occuring model constants

  ! References:
  ! None
  !---------------------------------------------------------------------------

    USE clubb_precision, ONLY: core_rknd


    USE shr_const_mod, ONLY: shr_const_rdair, shr_const_cpdair, shr_const_latvap, shr_const_latice, shr_const_latsub, shr_const_rgas, shr_const_mwwv, shr_const_tkfrz, shr_const_mwdair

    IMPLICIT NONE

    PRIVATE

  !-----------------------------------------------------------------------------
  ! Numerical/Arbitrary Constants
  !-----------------------------------------------------------------------------

  ! Fortran file unit I/O constants
  integer, parameter, public ::  & 
    fstderr = 0, fstdin = 5, fstdout = 6

  ! Maximum variable name length in CLUBB GrADS or netCDF output
  ! The parameter parab_cyl_max_input is the largest magnitude that the input to
  ! the parabolic cylinder function is allowed to have.  When the value of the
  ! input to the parabolic cylinder function is too large in magnitude
  ! (depending on the order of the parabolic cylinder function), overflow
  ! occurs, and the output of the parabolic cylinder function is +/-Inf.  The
  ! parameter parab_cyl_max_input places a limit on the absolute value of the
  ! input to the parabolic cylinder function.  When the value of the potential
  ! input exceeds this parameter (usually due to a very large ratio of ith PDF
  ! component mean of x to ith PDF component standard deviation of x), the
  ! variable x is considered to be constant and a different version of the
  ! equation called.
  !
  ! The largest allowable magnitude of the input to the parabolic cylinder
  ! function (before overflow occurs) is dependent on the order of parabolic
  ! cylinder function.  However, after a lot of testing, it was determined that
  ! an absolute value of 49 works well for an order of 12 or less.

  ! "Over-implicit" weighted time step.
  !
  ! The weight of the implicit portion of a term is controlled by the factor
  ! gamma_over_implicit_ts (abbreviated "gamma" in the expression below).  A
  ! factor is added to the right-hand side of the equation in order to balance a
  ! weight that is not equal to 1, such that:
  !
  !      -y(t) * [ gamma * X(t+1) + ( 1 - gamma ) * X(t) ] + RHS;
  !
  ! where X is the variable that is being solved for in a predictive equation
  ! (such as w'^3, w'th_l', r_t'^2, etc), y(t) is the linearized portion of the
  ! term that gets treated implicitly, and RHS is the portion of the term that
  ! is always treated explicitly.  A weight of greater than 1 can be applied to
  ! make the term more numerically stable.
  !
  !    gamma_over_implicit_ts          Effect on term
  !
  !            0.0               Term becomes completely explicit
  !
  !            1.0               Standard implicit portion of the term;
  !                              as it was without the weighting factor.
  !
  !            1.5               Strongly weighted implicit portion of the term;
  !                              increased numerical stability.
  !
  !            2.0               More strongly weighted implicit portion of the
  !                              term; increased numerical stability.
  !
  ! Note:  The "over-implicit" weighted time step is only applied to terms that
  !        tend to significantly decrease the amount of numerical stability for
  !        variable X.
  !        The "over-implicit" weighted time step is applied to the turbulent
  !        advection term for the following variables:
  !           w'^3 (also applied to the turbulent production term), found in
  !           module advance_wp2_wp3_module;
  !           w'r_t', w'th_l', and w'sclr', found in
  !           module advance_xm_wpxp_module; and
  !           r_t'^2, th_l'^2, r_t'th_l', u'^2, v'^2, sclr'^2, sclr'r_t',
  !           and sclr'th_l', found in module advance_xp2_xpyp_module.

  !-----------------------------------------------------------------------------
  ! Mathematical Constants
  !-----------------------------------------------------------------------------












  real( kind = core_rknd ), parameter, public :: &
    sqrt_2pi = 2.5066282746310005024_core_rknd, &  ! sqrt(2*pi)
    sqrt_2   = 1.4142135623730950488_core_rknd     ! sqrt(2)


  real( kind = core_rknd ), parameter, public :: &
    one_hundred   = 100.0_core_rknd,             & ! 100
    fifty         = 50.0_core_rknd,              & ! 50
    twenty        = 20.0_core_rknd,              & ! 20
    ten           = 10.0_core_rknd,              & ! 10
    five          = 5.0_core_rknd,               & ! 5
    four          = 4.0_core_rknd,               & ! 4
    three         = 3.0_core_rknd,               & ! 3
    two           = 2.0_core_rknd,               & ! 2
    three_halves  = 3.0_core_rknd/2.0_core_rknd, & ! 3/2
    four_thirds   = 4.0_core_rknd/3.0_core_rknd, & ! 4/3
    one           = 1.0_core_rknd,               & ! 1
    three_fourths = 0.75_core_rknd,              & ! 3/4
    two_thirds    = 2.0_core_rknd/3.0_core_rknd, & ! 2/3
    one_half      = 0.5_core_rknd,               & ! 1/2
    one_third     = 1.0_core_rknd/3.0_core_rknd, & ! 1/3
    one_fourth    = 0.25_core_rknd,              & ! 1/4
    zero          = 0.0_core_rknd                  ! 0

  !-----------------------------------------------------------------------------
  ! Physical constants
  !-----------------------------------------------------------------------------



  real( kind = core_rknd ), parameter, public ::  & 
    Cp = shr_const_cpdair,  & ! Dry air specific heat at constant p [J/kg/K]
    Lv = shr_const_latvap,    & ! Latent heat of vaporization         [J/kg]
    Lf = shr_const_latice,   & ! Latent heat of fusion               [J/kg]
    Ls = shr_const_latsub,  & ! Latent heat of sublimation          [J/kg]
    Rd = shr_const_rdair,   & ! Dry air gas constant                [J/kg/K]
    Rv = shr_const_rgas/shr_const_mwwv       ! Water vapor gas constant            [J/kg/K]
    
    
  real( kind = core_rknd ), parameter, public :: &
    T_freeze_K = shr_const_tkfrz ! Freezing point of water [K]
    
  ! Useful combinations of Rd and Rv
  real( kind = core_rknd ), parameter, public ::  & 
    ep  = shr_const_mwwv/shr_const_mwdair,    & ! ep  = 0.622  [-]
    ep1 = (1.0-ep)/ep,& ! ep1 = 0.61   [-]
    ep2 = 1.0/ep        ! ep2 = 1.61   [-]
    
    

  ! Von Karman's constant
  ! Constant of the logarithmic wind profile in the surface layer    




  ! Tolerances below which we consider moments to be zero
  real( kind = core_rknd ), parameter, public ::  & 
    w_tol        = 2.e-2_core_rknd, & ! [m/s]
    thl_tol      = 1.e-2_core_rknd, & ! [K]
    rt_tol       = 1.e-8_core_rknd, & ! [kg/kg]
    chi_tol = 1.e-8_core_rknd, & ! [kg/kg]
    eta_tol = chi_tol       ! [kg/kg]

  ! Tolerances for use by the monatonic flux limiter.
  ! rt_tol_mfl is larger than rt_tol. rt_tol is extremely small
  ! (1e-8) to prevent spurious cloud formation aloft in LBA.
  ! rt_tol_mfl is larger (1e-4) to prevent the mfl from
  ! depositing moisture at the top of the domain.

  ! The tolerance for w'^2 is the square of the tolerance for w.
  real( kind = core_rknd ), parameter, public :: &
    w_tol_sqd = w_tol**2 ! [m^2/s^2]



  ! Set tolerances for Khairoutdinov and Kogan rain microphysics to insure
  ! against numerical errors.  The tolerance values for Nc, rr, and Nr insure
  ! against underflow errors in computing the PDF for l_kk_rain.  Basically,
  ! they insure that those values squared won't be less then 10^-38, which is
  ! the lowest number that can be numerically represented.  However, the
  ! tolerance value for rc doubles as the lowest mixing ratio there can be to
  ! still officially have a cloud at that level.  This is figured to be about
  ! 1.0_core_rknd x 10^-7 kg/kg.  Brian; February 10, 2007.



  ! Precipitating hydrometeor tolerances for mixing ratios.

  ! Maximum allowable values for the average mean volume radius of the various
  ! hydrometeor species.

  ! Precipitating hydrometeor tolerances for concentrations.
  ! Tolerance value for N_r [#/kg]

  ! Tolerance value for N_i [#/kg]

  ! Tolerance value for N_s [#/kg]

  ! Tolerance value for N_s [#/kg]

  ! Minimum value for em (turbulence kinetic energy)
  ! If anisotropic TKE is enabled, em = (1/2) * ( up2 + vp2 + wp2 );
  ! otherwise, em = (3/2) * wp2.  Since up2, vp2, and wp2 all have
  ! the same minimum threshold value of w_tol_sqd, em cannot be less
  ! than (3/2) * w_tol_sqd.  Thus, em_min = (3/2) * w_tol_sqd.


  real( kind = core_rknd ), parameter, public ::  &
    zero_threshold = 0.0_core_rknd ! Defining a threshold on a physical quantity to be 0.

  ! The maximum absolute value (or magnitude) that a correlation is allowed to
  ! have.  Statistically, a correlation is not allowed to be less than -1 or
  ! greater than 1, so the maximum magnitude would be 1.


  !-----------------------------------------------------------------------------
  ! Useful conversion factors.
  !-----------------------------------------------------------------------------




  !-----------------------------------------------------------------------------
  ! Unused variable
  !-----------------------------------------------------------------------------

!=============================================================================

end module constants_clubb