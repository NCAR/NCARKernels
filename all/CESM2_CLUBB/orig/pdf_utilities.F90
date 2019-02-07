!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:36 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------
! $Id: pdf_utilities.F90 7370 2014-11-07 20:59:58Z bmg2@uwm.edu $
!===============================================================================


module pdf_utilities
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC compute_mean_binormal, compute_variance_binormal 

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


  !=============================================================================


  !=============================================================================


  !=============================================================================


!===============================================================================


end module pdf_utilities