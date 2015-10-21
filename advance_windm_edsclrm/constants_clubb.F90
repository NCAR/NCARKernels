
! KGEN-generated Fortran source file
!
! Filename    : constants_clubb.F90
! Generated at: 2015-10-21 08:59:09
! KGEN version: 0.5.3



    MODULE constants_clubb
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
! Contains frequently occuring model constants
! References:
! None
!---------------------------------------------------------------------------
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
        PRIVATE ! Default scope
!-----------------------------------------------------------------------------
! Numerical/Arbitrary Constants
!-----------------------------------------------------------------------------
! Fortran file unit I/O constants
        INTEGER, parameter, public :: fstderr = 0
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
! Largest allowable input to parab. cyl. fnct.
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
! The ratio of radii to their circumference
! sqrt(2*pi)
! sqrt(2)
! 2
! 1
! 1/2
! 1/4
! 0
        REAL(KIND=core_rknd), parameter, public :: zero          = 0.0_core_rknd
        REAL(KIND=core_rknd), parameter, public :: one           = 1.0_core_rknd
! 100
! 50
! 20
! 10
! 5
! 4
! 3
! 2
! 3/2
! 4/3
! 1
! 3/4
! 2/3
! 1/2
! 1/3
! 1/4
! 0
!-----------------------------------------------------------------------------
! Physical constants
!-----------------------------------------------------------------------------
! Dry air specific heat at constant p [J/kg/K]
! Latent heat of vaporization         [J/kg]
! Latent heat of fusion               [J/kg]
! Latent heat of sublimation          [J/kg]
! Dry air gas constant                [J/kg/K]
! Water vapor gas constant            [J/kg/K]
! Stefan-Boltzmann constant [W/(m^2 K^4)]
! Freezing point of water [K]
! Useful combinations of Rd and Rv
! ep  = 0.622  [-]
! ep1 = 0.61   [-]
! ep2 = 1.61   [-]
! kappa        [-]
! Gravitational acceleration     [m/s^2]
! Reference pressure             [Pa]
! Von Karman's constant
! Constant of the logarithmic wind profile in the surface layer
! Accepted value is 0.40 (+/-) 0.01      [-]
! Density of liquid water                [kg/m^3]
! Density of ice      [kg/m^3]
! Tolerances below which we consider moments to be zero
! [m/s]
! [K]
! [kg/kg]
! [kg/kg]
! [kg/kg]
! Tolerances for use by the monatonic flux limiter.
! rt_tol_mfl is larger than rt_tol. rt_tol is extremely small
! (1e-8) to prevent spurious cloud formation aloft in LBA.
! rt_tol_mfl is larger (1e-4) to prevent the mfl from
! depositing moisture at the top of the domain.
! [K]
! [kg/kg]
! The tolerance for w'^2 is the square of the tolerance for w.
! [m^2/s^2]
! Max magnitude of skewness     [-]
! Max mag. of Skw squared [-]
! Set tolerances for Khairoutdinov and Kogan rain microphysics to insure
! against numerical errors.  The tolerance values for Nc, rr, and Nr insure
! against underflow errors in computing the PDF for l_kk_rain.  Basically,
! they insure that those values squared won't be less then 10^-38, which is
! the lowest number that can be numerically represented.  However, the
! tolerance value for rc doubles as the lowest mixing ratio there can be to
! still officially have a cloud at that level.  This is figured to be about
! 1.0_core_rknd x 10^-7 kg/kg.  Brian; February 10, 2007.
! Tolerance value for r_c  [kg/kg]
! Tolerance value for N_c  [#/kg]
! Tolerance value for N_cn [#/kg]
! Max. avg. mean vol. rad. cloud    [m]
! Precipitating hydrometeor tolerances for mixing ratios.
! Tolerance value for r_r [kg/kg]
! Tolerance value for r_i [kg/kg]
! Tolerance value for r_s [kg/kg]
! Tolerance value for r_g [kg/kg]
! Maximum allowable values for the average mean volume radius of the various
! hydrometeor species.
! Max. avg. mean vol. rad. rain    [m]
! Max. avg. mean vol. rad. ice     [m]
! Max. avg. mean vol. rad. snow    [m]
! Max. avg. mean vol. rad. graupel [m]
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
! [m^2/s^2]
        REAL(KIND=core_rknd), parameter, public :: eps = 1.0e-10_core_rknd
! Small value to prevent a divide by zero
! Defining a threshold on a physical quantity to be 0.
! The maximum absolute value (or magnitude) that a correlation is allowed to
! have.  Statistically, a correlation is not allowed to be less than -1 or
! greater than 1, so the maximum magnitude would be 1.
        REAL(KIND=core_rknd), parameter, public :: max_mag_correlation = 0.99_core_rknd
! Threshold for cloud fractions
!-----------------------------------------------------------------------------
! Useful conversion factors.
!-----------------------------------------------------------------------------
! Seconds in a day.
! Seconds in an hour.
! Seconds in a minute.
! Minutes in an hour.
! Grams in a kilogram.
! Pascals per Millibar
! Cubic centimeters per cubic meter
! Micrometers per meter
! Centimeters per meter
! Millimeters per meter
!-----------------------------------------------------------------------------
! Unused variable
!-----------------------------------------------------------------------------
! The standard value for unused variables
!=============================================================================

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE constants_clubb
