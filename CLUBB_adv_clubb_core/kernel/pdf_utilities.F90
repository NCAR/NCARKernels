!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:46
!KGEN version : 0.6.1

!-------------------------------------------------------------------------
! $Id: pdf_utilities.F90 7370 2014-11-07 20:59:58Z bmg2@uwm.edu $
!===============================================================================
module pdf_utilities

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PRIVATE

    PUBLIC compute_mean_binormal, compute_variance_binormal

  contains

  !=============================================================================
  
    ! Description:
    ! For a lognormally-distributed variable x, this function finds the mean of
    ! ln x (mu_x_n) for the ith component of the PDF, given the mean of x (mu_x)
    ! and the variance of x (sigma_sqd_x) for the ith component of the PDF. The
    ! value ln x is distributed normally when x is distributed lognormally.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- App. B.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Find the mean of ln x for the ith component of the PDF.




  !=============================================================================
  
    ! Description:
    ! For a lognormally-distributed variable x, this function finds the mean of
    ! ln x (mu_x_n) for the ith component of the PDF, given the mean of x (mu_x)
    ! and the variance of x (sigma_sqd_x) for the ith component of the PDF. The
    ! value ln x is distributed normally when x is distributed lognormally.
    ! This function uses double precision variables.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- App. B.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Find the mean of ln x for the ith component of the PDF.




  !=============================================================================

    ! Description:
    ! For a lognormally-distributed variable x, this function finds the standard
    ! deviation of ln x (sigma_x_n) for the ith component of the PDF, given the
    ! mean of x (mu_x) and the variance of x (sigma_sqd_x) for the ith component
    ! of the PDF.  The value ln x is distributed normally when x is distributed
    ! lognormally.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- App. B.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Find the standard deviation of ln x for the ith component of the PDF.




  !=============================================================================

    ! Description:
    ! For a lognormally-distributed variable x, this function finds the standard
    ! deviation of ln x (sigma_x_n) for the ith component of the PDF, given the
    ! mean of x (mu_x) and the variance of x (sigma_sqd_x) for the ith component
    ! of the PDF.  The value ln x is distributed normally when x is distributed
    ! lognormally.
    ! This function uses double precision variables.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- App. B.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Find the standard deviation of ln x for the ith component of the PDF.




  !=============================================================================

    ! Description:
    ! For a normally-distributed variable x and a lognormally-distributed
    ! variable y, this function finds the correlation of x and ln y (corr_x_y_n)
    ! for the ith component of the PDF, given the correlation of x and y
    ! (corr_x_y) and the standard deviation of ln y (sigma_y_n) for the ith
    ! component of the PDF.  The value ln y is distributed normally when y is
    ! distributed lognormally.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- Eq. B-1.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Find the correlation of x and ln y for the ith component of the PDF.

    ! Clip the magnitude of the correlation of x and ln y in the ith PDF
    ! component, just in case the correlation (ith PDF component) of x and y and
    ! the standard deviation (ith PDF component) of ln y are inconsistent,
    ! resulting in an unrealizable value for corr_x_y_n.





  !=============================================================================

    ! Description:
    ! For a normally-distributed variable x and a lognormally-distributed
    ! variable y, this function finds the correlation of x and ln y (corr_x_y_n)
    ! for the ith component of the PDF, given the correlation of x and y
    ! (corr_x_y) and the standard deviation of ln y (sigma_y_n) for the ith
    ! component of the PDF.  The value ln y is distributed normally when y is
    ! distributed lognormally.
    ! This function uses double precision variables.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- Eq. B-1.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Find the correlation of x and ln y for the ith component of the PDF.

    ! Clip the magnitude of the correlation of x and ln y in the ith PDF
    ! component, just in case the correlation (ith PDF component) of x and y and
    ! the standard deviation (ith PDF component) of ln y are inconsistent,
    ! resulting in an unrealizable value for corr_x_y_n.





  !=============================================================================

    ! Description:
    ! For lognormally-distributed variables x and y, this function finds the
    ! correlation of ln x and ln y (corr_x_y_n) for the ith component of the
    ! PDF, given the correlation of x and y (corr_x_y), the standard deviation
    ! of ln x (sigma_x_n), and the standard deviation of ln y (sigma_y_n) for
    ! the ith component of the PDF.  The value of ln x (or ln y) is distributed
    ! normally when x (or y) is distributed lognormally.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- Eq. C-3.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable

    ! Local Variable


    ! Find the correlation of ln x and ln y for the ith component of the
    ! PDF.
!    corr_x_y_n = log( one + corr_x_y * sqrt( exp( sigma_x_n**2 ) - one )  &
!                                     * sqrt( exp( sigma_y_n**2 ) - one )  )  &
!                 / ( sigma_x_n * sigma_y_n )


    ! Clip the magnitude of the correlation of ln x and ln y in the ith PDF
    ! component, just in case the correlation (ith PDF component) of x and y,
    ! the standard deviation (ith PDF component) of ln x, and the standard
    ! deviation (ith PDF component) of ln y are inconsistent, resulting in an
    ! unrealizable value for corr_x_y_n.





  !=============================================================================

    ! Description:
    ! For lognormally-distributed variables x and y, this function finds the
    ! correlation of ln x and ln y (corr_x_y_n) for the ith component of the
    ! PDF, given the correlation of x and y (corr_x_y), the standard deviation
    ! of ln x (sigma_x_n), and the standard deviation of ln y (sigma_y_n) for
    ! the ith component of the PDF.  The value of ln x (or ln y) is distributed
    ! normally when x (or y) is distributed lognormally.
    ! This function uses double precision variables.

    ! References:
    !  Garvey, P. R., 2000: Probability methods for cost uncertainty analysis.
    !    Marcel Dekker, 401 pp.
    !  -- Eq. C-3.
    !-----------------------------------------------------------------------




    ! Input Variables


    ! Return Variable


    ! Find the correlation of ln x and ln y for the ith component of the
    ! PDF.

    ! Clip the magnitude of the correlation of ln x and ln y in the ith PDF
    ! component, just in case the correlation (ith PDF component) of x and y,
    ! the standard deviation (ith PDF component) of ln x, and the standard
    ! deviation (ith PDF component) of ln y are inconsistent, resulting in an
    ! unrealizable value for corr_x_y_n.





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

    ! Description:
    ! This function calculates the correlation of extended liquid water mixing
    ! ratio, chi (old s), and a generic variable x, within the ith component of
    ! the PDF.  The variable chi can be split into mean and turbulent
    ! components, such that:
    !
    ! chi = <chi> + chi';
    !
    ! where < > denotes a mean field an ' denotes a turbulent component.
    !
    ! The linearized equation for chi' is given in Larson et al. (2001), where
    ! within the ith component of the PDF:
    !
    ! chi_(i)' = Coef_rt(i) * r_t(i)' - Coef_thl(i) * th_l(i)'.
    !
    ! The equation for chi' can be multiplied by x'.  The equation becomes:
    !
    ! chi'x'_(i) = Coef_rt(i) * r_t'x'_(i) - Coef_thl(i) * th_l'x'_(i).
    !
    ! Averaging both sides, the covariance <chi'x'> is given by the equation:
    !
    ! <chi'x'_(i)> = Coef_rt(i) * <r_t'x'_(i)> - Coef_thl(i) * <th_l'x'_(i)>.
    !
    ! This equation can be rewritten as:
    !
    ! sigma_chi(i) * sigma_x(i) * corr_chi_x(i)
    !   = Coef_rt(i) * sigma_rt(i) * sigma_x(i) * corr_rt_x(i)
    !     - Coef_thl(i) * sigma_thl(i) * sigma_x(i) * corr_thl_x(i).
    !
    ! This equation can be solved for corr_chi_x(i):
    !
    ! corr_chi_x(i)
    ! = Coef_rt(i) * ( sigma_rt(i) / sigma_chi(i) ) * corr_rt_x(i)
    !   - Coef_thl(i) * ( sigma_thl(i) / sigma_chi(i) ) * corr_thl_x(i).
    !
    ! The correlation of chi and x within the ith component of the PDF is
    ! calculated.

    ! References:
    !  Larson, V. E., R. Wood, P. R. Field, J.-C. Golaz, T. H. Vonder Haar,
    !    W. R. Cotton, 2001: Systematic Biases in the Microphysics and
    !    Thermodynamics of Numerical Models That Ignore Subgrid-Scale
    !    Variability. J. Atmos. Sci., 58, 1117--1128.
    !  -- Eq. 13 and 14.
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Calculate the correlation of chi and x in the ith PDF component.



       ! The standard deviation of chi in the ith PDF component is 0.  This
       ! means that chi is constant within the ith PDF component, and the ith
       ! PDF component covariance of chi and x is also 0.  The correlation of
       ! chi and x is undefined in the ith PDF component, so a value of 0 will
       ! be used.


    ! Clip the magnitude of the correlation of chi and x in the ith PDF
    ! component, just in case the correlations and standard deviations used in
    ! calculating it are inconsistent, resulting in an unrealizable value for
    ! corr_chi_x_i.





  !=============================================================================

    ! Description:
    ! This function calculates the correlation of rt and x based on the
    ! correlation of chi and x and the correlation of eta and x.

    ! References:
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Calculate the correlation of rt and x in the ith PDF component.



       ! The standard deviation of rt in the ith PDF component is 0.  This means
       ! that rt is constant within the ith PDF component, and the ith PDF
       ! component covariance of rt and x is also 0.  The correlation of rt and
       ! x is undefined in the ith PDF component, so a value of 0 will be used.


    ! Clip the magnitude of the correlation of rt and x in the ith PDF
    ! component, just in case the correlations and standard deviations used in
    ! calculating it are inconsistent, resulting in an unrealizable value for
    ! corr_rt_x_i.





  !=============================================================================

    ! Description:
    ! This function calculates the correlation of thl and x based on the
    ! correlation of chi and x and the correlation of eta and x.

    ! References:
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Calculate the correlation of thl and x in the ith PDF component.



       ! The standard deviation of thl in the ith PDF component is 0.  This
       ! means that thl is constant within the ith PDF component, and the ith
       ! PDF component covariance of thl and x is also 0.  The correlation of
       ! thl and x is undefined in the ith PDF component, so a value of 0 will
       ! be used.


    ! Clip the magnitude of the correlation of thl and x in the ith PDF
    ! component, just in case the correlations and standard deviations used in
    ! calculating it are inconsistent, resulting in an unrealizable value for
    ! corr_thl_x_i.





  !=============================================================================

    ! Description:
    ! Calculates the overall variance of x, <x'^2>, where the distribution of x
    ! is a combination of a lognormal distribution and/or 0 in each PDF
    ! component.  The fraction of each component where x is lognormally
    ! distributed (amd greater than 0) is x_frac_i (x_frac_1 and x_frac_2 for
    ! PDF components 1 and 2, respectively).  The fraction of each component
    ! where x has a value of 0 is ( 1 - x_frac_i ).  This function should be
    ! called to calculate the total variance for x when <x'^2> is not provided
    ! by a predictive (or other) equation.
    !    
    ! This function is used to calculate the overall variance for rain water
    ! mixing ratio, <r_r'^2>, and the overall variance for rain drop
    ! concentration, <N_r'^2>.  The ratio of variance to mean-value-squared is
    ! specified for the in-precip values of r_r and N_r within each PDF
    ! component, allowing for the calculation of sigma_rr_i and sigma_Nr_i,
    ! as well as sigma_rr_i_n and sigma_Nr_i_n.

    ! References:
    !-----------------------------------------------------------------------




    ! Input Variables

    ! Return Variable


    ! Calculate overall variance of x, <x'^2>.

       ! The value of x is constant within both PDF components.



       ! The value of x is constant within the 1st PDF component.



       ! The value of x is constant within the 2nd PDF component.



       ! The value of x varies within both PDF component.




    ! As a check, prevent negative values for hydrometeor variances due to
    ! numerical loss of precision error.





!===============================================================================

end module pdf_utilities