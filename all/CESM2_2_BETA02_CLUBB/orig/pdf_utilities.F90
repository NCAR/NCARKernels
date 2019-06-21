!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:37 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------
! $Id$
!===============================================================================


module pdf_utilities
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC compute_mean_binormal, compute_variance_binormal, calc_comp_corrs_binormal, calc_corr_chi_x, calc_corr_eta_x 

  contains
  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================

  elemental function compute_mean_binormal( mu_x_1, mu_x_2, mixt_frac ) &
  result( xm )
    ! Description:
    ! Computes the overall grid-box mean of a binormal distribution from the
    ! mean of each component
    ! References:
    !   None
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: one 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in) :: &
      mu_x_1,    & ! First PDF component mean of 'x'                       [?]
      mu_x_2,    & ! Second PDF component mean of 'x'                      [?]
      mixt_frac    ! Weight of the first PDF component                     [-]
    ! Output Variables

    real( kind = core_rknd ) :: &
      xm           ! Mean of 'x' (overall)                                 [?]
    !-----------------------------------------------------------------------
    !----- Begin Code -----


    xm = mixt_frac * mu_x_1 + ( one - mixt_frac ) * mu_x_2


    return

  end function compute_mean_binormal
  !=============================================================================

  elemental function compute_variance_binormal( xm, mu_x_1, mu_x_2, &
                                                stdev_x_1, stdev_x_2, &
                                                mixt_frac ) &
  result( xp2 )
    ! Description:
    ! Computes the overall grid-box variance of a binormal distribution from the
    ! variance of each component.
    ! References:
    !   None
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: one 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in) :: &
      xm,        & ! Overall mean of 'x'                                   [?]
      mu_x_1,    & ! First PDF component mean of 'x'                       [?]
      mu_x_2,    & ! Second PDF component mean of 'x'                      [?]
      stdev_x_1, & ! Standard deviation of 'x' in the first PDF component  [?]
      stdev_x_2, & ! Standard deviation of 'x' in the second PDF component [?]
      mixt_frac    ! Weight of the first PDF component                     [-]
    ! Output Variables

    real( kind = core_rknd ) :: &
      xp2          ! Variance of 'x' (overall)                             [?^2]
    !-----------------------------------------------------------------------
    !----- Begin Code -----


    xp2 = mixt_frac * ( ( mu_x_1 - xm )**2 + stdev_x_1**2 ) &
          + ( one - mixt_frac ) * ( ( mu_x_2 - xm )**2 + stdev_x_2**2 )


    return

  end function compute_variance_binormal
  !=============================================================================

  subroutine calc_comp_corrs_binormal( xpyp, xm, ym, mu_x_1, mu_x_2,  & ! In
                                       mu_y_1, mu_y_2, sigma_x_1_sqd, & ! In
                                       sigma_x_2_sqd, sigma_y_1_sqd,  & ! In
                                       sigma_y_2_sqd, mixt_frac,      & ! In
                                       corr_x_y_1, corr_x_y_2         ) ! Out
    ! Description:
    ! Calculates the PDF component correlations of variables x and y, where
    ! x and y are both distributed as two-component normals (or binormals).
    ! The PDF component correlations are set equal to each other.
    ! The overall covariance of x and y, <x'y'>, can be expressed in terms of
    ! PDF parameters by integrating over the PDF:
    ! <x'y'> = INT(-inf:inf) INT(-inf:inf) ( x - <x> ) ( y - <y> ) P(x,y) dy dx;
    ! where <x> is the overall mean of x, <y> is the overall mean of y, and
    ! P(x,y) is the equation for the two-component normal PDF of x and y.
    ! The integral is evaluated, and the equation for <x'y'> is:
    ! <x'y'> = mixt_frac * ( ( mu_x_1 - <x> ) * ( mu_y_1 - <y> )
    !                        + corr_x_y_1 * sigma_x_1 * sigma_y_1 )
    !          + ( 1 - mixt_frac ) * ( ( mu_x_2 - <x> ) * ( mu_y_2 - <y> )
    !                                  + corr_x_y_2 * sigma_x_2 * sigma_y_2 );
    ! where mu_x_1 is the mean of x in the 1st PDF component, mu_x_2 is the mean
    ! of x in the 2nd PDF component, mu_y_1 is the mean of y in the 1st PDF
    ! component, mu_y_2 is the mean of y in the 2nd PDF component, sigma_x_1 is
    ! the standard deviation of x in the 1st PDF component, sigma_x_2 is the
    ! standard deviation of x in the 2nd PDF component, sigma_y_1 is the
    ! standard deviation of y in the 1st PDF component, sigma_y_2 is the
    ! standard deviation of y in the 2nd PDF component, corr_x_y_1 is the
    ! correlation of x and y in the 1st PDF component, corr_x_y_2 is the
    ! correlation of x and y in the 2nd PDF component, and mixt_frac is the
    ! mixture fraction (weight of the 1st PDF component).
    ! This equation can be rewritten as:
    ! <x'y'> = mixt_frac * ( mu_x_1 - <x> ) * ( mu_y_1 - <y> )
    !          + mixt_frac * corr_x_y_1 * sigma_x_1 * sigma_y_1
    !          + ( 1 - mixt_frac ) * ( mu_x_2 - <x> ) * ( mu_y_2 - <y> )
    !          + ( 1 - mixt_frac ) * corr_x_y_2 * sigma_x_2 * sigma_y_2.
    ! Setting the two PDF component correlations equal to each other
    ! (corr_x_y_1 = corr_x_y_2), the equation can be solved for the PDF
    ! component correlations:
    ! corr_x_y_1 = corr_x_y_2
    ! = ( <x'y'> - mixt_frac * ( mu_x_1 - <x> ) * ( mu_y_1 - <y> )
    !            - ( 1 - mixt_frac ) * ( mu_x_2 - <x> ) * ( mu_y_2 - <y> ) )
    !   / ( mixt_frac * sigma_x_1 * sigma_y_1
    !       + ( 1 - mixt_frac ) * sigma_x_2 * sigma_y_2 );
    ! where -1 <= corr_x_y_1 = corr_x_y_2 <= 1.
    ! When sigma_x_1 * sigma_y_1 = 0 and sigma_x_2 * sigma_y_2 = 0, at least one
    ! of x or y are constant within each PDF component, and both PDF component
    ! correlations are undefined.
    ! References:
    !-----------------------------------------------------------------------

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


      USE grid_class, ONLY: gr 

      USE constants_clubb, ONLY: one, zero 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real ( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      xpyp,          & ! Covariance of x and y (overall)    [(x units)(y units)]
      xm,            & ! Mean of x (overall)                [x units]
      ym,            & ! Mean of y (overall)                [y units]
      mu_x_1,        & ! Mean of x (1st PDF component)      [x units]
      mu_x_2,        & ! Mean of x (2nd PDF component)      [x units]
      mu_y_1,        & ! Mean of y (1st PDF component)      [y units]
      mu_y_2,        & ! Mean of y (2nd PDF component)      [y units]
      sigma_x_1_sqd, & ! Variance of x (1st PDF component)  [(x units)^2]
      sigma_x_2_sqd, & ! Variance of x (2nd PDF component)  [(x units)^2]
      sigma_y_1_sqd, & ! Variance of y (1st PDF component)  [(y units)^2]
      sigma_y_2_sqd, & ! Variance of y (2nd PDF component)  [(y units)^2]
      mixt_frac        ! Mixture fraction                   [-]
    ! Output Variables

    real ( kind = core_rknd ), dimension(gr%nz), intent(out) :: &
      corr_x_y_1, & ! Correlation of x and y (1st PDF component)    [-]
      corr_x_y_2    ! Correlation of x and y (2nd PDF component)    [-]


    where ( sigma_x_1_sqd * sigma_y_1_sqd > zero &
            .or. sigma_x_2_sqd * sigma_y_2_sqd > zero )
       ! Calculate corr_x_y_1 (which also equals corr_x_y_2).

       corr_x_y_1 &
       = ( xpyp &
           - mixt_frac * ( mu_x_1 - xm ) * ( mu_y_1 - ym ) &
           - ( one - mixt_frac ) * ( mu_x_2 - xm ) * ( mu_y_2 - ym ) ) &
         / ( mixt_frac * sqrt( sigma_x_1_sqd * sigma_y_1_sqd ) &
             + ( one - mixt_frac ) * sqrt( sigma_x_2_sqd * sigma_y_2_sqd ) )
       ! The correlation must fall within the bounds of
       ! -1 <= corr_x_y_1 (= corr_x_y_2) <= 1.

       where ( corr_x_y_1 > one )
          corr_x_y_1 = one
       elsewhere ( corr_x_y_1 < -one )
          corr_x_y_1 = -one
       endwhere

    elsewhere ! sigma_x_1^2 * sigma_y_1^2 = 0 and sigma_x_2^2 * sigma_y_2^2 = 0.
       ! The correlation is undefined (output as 0).

       corr_x_y_1 = zero

    endwhere
    ! Set corr_x_y_2 equal to corr_x_y_1.

    corr_x_y_2 = corr_x_y_1


    return

  end subroutine calc_comp_corrs_binormal
  !=============================================================================

  pure function calc_corr_chi_x( crt_i, cthl_i, sigma_rt_i, sigma_thl_i,  &
                                 sigma_chi_i, corr_rt_x_i, corr_thl_x_i )  &
  result( corr_chi_x_i )
    ! Description:
    ! This function calculates the correlation of extended liquid water mixing
    ! ratio, chi (old s), and a generic variable x, within the ith component of
    ! the PDF.  The variable chi can be split into mean and turbulent
    ! components, such that:
    ! chi = <chi> + chi';
    ! where < > denotes a mean field an ' denotes a turbulent component.
    ! The linearized equation for chi' is given in Larson et al. (2001), where
    ! within the ith component of the PDF:
    ! chi_(i)' = Coef_rt(i) * r_t(i)' - Coef_thl(i) * th_l(i)'.
    ! The equation for chi' can be multiplied by x'.  The equation becomes:
    ! chi'x'_(i) = Coef_rt(i) * r_t'x'_(i) - Coef_thl(i) * th_l'x'_(i).
    ! Averaging both sides, the covariance <chi'x'> is given by the equation:
    ! <chi'x'_(i)> = Coef_rt(i) * <r_t'x'_(i)> - Coef_thl(i) * <th_l'x'_(i)>.
    ! This equation can be rewritten as:
    ! sigma_chi(i) * sigma_x(i) * corr_chi_x(i)
    !   = Coef_rt(i) * sigma_rt(i) * sigma_x(i) * corr_rt_x(i)
    !     - Coef_thl(i) * sigma_thl(i) * sigma_x(i) * corr_thl_x(i).
    ! This equation can be solved for corr_chi_x(i):
    ! corr_chi_x(i)
    ! = Coef_rt(i) * ( sigma_rt(i) / sigma_chi(i) ) * corr_rt_x(i)
    !   - Coef_thl(i) * ( sigma_thl(i) / sigma_chi(i) ) * corr_thl_x(i).
    ! The correlation of chi and x within the ith component of the PDF is
    ! calculated.
    ! References:
    ! Eq. (13) and Eq. (14) of Larson, V. E., R. Wood, P. R. Field, J.-C. Golaz,
    ! T. H. Vonder Haar, W. R. Cotton, 2001:  Systematic Biases in the
    ! Microphysics and Thermodynamics of Numerical Models That Ignore
    ! Subgrid-Scale Variability. J. Atmos. Sci., 58, 1117--1128,
    ! doi:https://doi.org/10.1175/1520-0469(2001)058%3C1117:SBITMA%3E2.0.CO;2.
    ! Eq. (A29) of Griffin, B. M., 2016:  Improving the Subgrid-Scale
    ! Representation of Hydrometeors and Microphysical Feedback Effects Using a
    ! Multivariate PDF.  Doctoral dissertation, University of
    ! Wisconsin -- Milwaukee, Milwaukee, WI, Paper 1144, 165 pp., URL
    ! http://dc.uwm.edu/cgi/viewcontent.cgi?article=2149&context=etd.
    !-----------------------------------------------------------------------

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

      USE grid_class, ONLY: gr 

      USE constants_clubb, ONLY: zero, chi_tol, max_mag_correlation 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      crt_i,        & ! Coefficient of r_t for chi (old s) (ith PDF comp.)   [-]
      cthl_i,       & ! Coefficient of th_l for chi (ith PDF comp.)  [(kg/kg)/K]
      sigma_rt_i,   & ! Standard deviation of r_t (ith PDF component)    [kg/kg]
      sigma_thl_i,  & ! Standard deviation of th_l (ith PDF component)       [K]
      sigma_chi_i,  & ! Standard deviation of chi (ith PDF component)    [kg/kg]
      corr_rt_x_i,  & ! Correlation of r_t and x (ith PDF component)         [-]
      corr_thl_x_i    ! Correlation of th_l and x (ith PDF component)        [-]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      corr_chi_x_i  ! Correlation of chi and x (ith PDF component)   [-]
    ! Calculate the correlation of chi and x in the ith PDF component.


    where ( sigma_chi_i > chi_tol )

       corr_chi_x_i = crt_i * ( sigma_rt_i / sigma_chi_i ) * corr_rt_x_i  &
                      - cthl_i * ( sigma_thl_i / sigma_chi_i ) * corr_thl_x_i

    elsewhere  ! sigma_chi_i = 0
       ! The standard deviation of chi in the ith PDF component is 0.  This
       ! means that chi is constant within the ith PDF component, and the ith
       ! PDF component covariance of chi and x is also 0.  The correlation of
       ! chi and x is undefined in the ith PDF component, so a value of 0 will
       ! be used.

       corr_chi_x_i = zero

    endwhere
    ! Clip the magnitude of the correlation of chi and x in the ith PDF
    ! component, just in case the correlations and standard deviations used in
    ! calculating it are inconsistent, resulting in an unrealizable value for
    ! corr_chi_x_i.

    where ( corr_chi_x_i > max_mag_correlation )
       corr_chi_x_i = max_mag_correlation
    elsewhere ( corr_chi_x_i < -max_mag_correlation )
       corr_chi_x_i = -max_mag_correlation
    endwhere


    return

  end function calc_corr_chi_x
  !=============================================================================

  pure function calc_corr_eta_x( crt_i, cthl_i, sigma_rt_i, sigma_thl_i,  &
                                 sigma_eta_i, corr_rt_x_i, corr_thl_x_i )  &
  result( corr_eta_x_i )
    ! Description:
    ! This function calculates the correlation of the variable that is
    ! orthogonal to extended liquid water mixing ratio in a PDF transformation,
    ! eta (old t), and a generic variable x, within the ith component of
    ! the PDF.
    ! References:
    ! Eq. (A30) of Griffin, B. M., 2016:  Improving the Subgrid-Scale
    ! Representation of Hydrometeors and Microphysical Feedback Effects Using a
    ! Multivariate PDF.  Doctoral dissertation, University of
    ! Wisconsin -- Milwaukee, Milwaukee, WI, Paper 1144, 165 pp., URL
    ! http://dc.uwm.edu/cgi/viewcontent.cgi?article=2149&context=etd.
    !-----------------------------------------------------------------------


      USE grid_class, ONLY: gr 

      USE constants_clubb, ONLY: zero, eta_tol, max_mag_correlation 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      crt_i,        & ! Coefficient of r_t for chi (old s) (ith PDF comp.)   [-]
      cthl_i,       & ! Coefficient of th_l for chi (ith PDF comp.)  [(kg/kg)/K]
      sigma_rt_i,   & ! Standard deviation of r_t (ith PDF component)    [kg/kg]
      sigma_thl_i,  & ! Standard deviation of th_l (ith PDF component)       [K]
      sigma_eta_i,  & ! Standard deviation of eta (ith PDF component)    [kg/kg]
      corr_rt_x_i,  & ! Correlation of r_t and x (ith PDF component)         [-]
      corr_thl_x_i    ! Correlation of th_l and x (ith PDF component)        [-]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      corr_eta_x_i  ! Correlation of eta and x (ith PDF component)   [-]
    ! Calculate the correlation of eta and x in the ith PDF component.


    where ( sigma_eta_i > eta_tol )

       corr_eta_x_i = crt_i * ( sigma_rt_i / sigma_eta_i ) * corr_rt_x_i  &
                      + cthl_i * ( sigma_thl_i / sigma_eta_i ) * corr_thl_x_i

    elsewhere  ! sigma_eta_i = 0
       ! The standard deviation of eta in the ith PDF component is 0.  This
       ! means that eta is constant within the ith PDF component, and the ith
       ! PDF component covariance of eta and x is also 0.  The correlation of
       ! eta and x is undefined in the ith PDF component, so a value of 0 will
       ! be used.

       corr_eta_x_i = zero

    endwhere
    ! Clip the magnitude of the correlation of eta and x in the ith PDF
    ! component, just in case the correlations and standard deviations used in
    ! calculating it are inconsistent, resulting in an unrealizable value for
    ! corr_eta_x_i.

    where ( corr_eta_x_i > max_mag_correlation )
       corr_eta_x_i = max_mag_correlation
    elsewhere ( corr_eta_x_i < -max_mag_correlation )
       corr_eta_x_i = -max_mag_correlation
    endwhere


    return

  end function calc_corr_eta_x
  !=============================================================================


  !=============================================================================


  !=============================================================================


!===============================================================================


end module pdf_utilities