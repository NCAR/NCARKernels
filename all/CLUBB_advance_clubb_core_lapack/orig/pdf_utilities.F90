
! KGEN-generated Fortran source file
!
! Filename    : pdf_utilities.F90
! Generated at: 2015-10-20 14:27:07
! KGEN version: 0.5.3



    MODULE pdf_utilities
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE ! Set default scope to private
        PUBLIC compute_mean_binormal, compute_variance_binormal
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

        elemental FUNCTION compute_mean_binormal(mu_x_1, mu_x_2, mixt_frac) RESULT ( xm )
! Description:
! Computes the overall grid-box mean of a binormal distribution from the
! mean of each component
! References:
!   None
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Constant
            USE constants_clubb, ONLY: one
! Constant
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: mu_x_1
            REAL(KIND=core_rknd), intent(in) :: mu_x_2
            REAL(KIND=core_rknd), intent(in) :: mixt_frac
! First PDF component mean of 'x'                       [?]
! Second PDF component mean of 'x'                      [?]
! Weight of the first PDF component                     [-]
! Output Variables
            REAL(KIND=core_rknd) :: xm
! Mean of 'x' (overall)                                 [?]
!-----------------------------------------------------------------------
!----- Begin Code -----
    xm = mixt_frac * mu_x_1 + ( one - mixt_frac ) * mu_x_2
    return
        END FUNCTION compute_mean_binormal
!=============================================================================

        elemental FUNCTION compute_variance_binormal(xm, mu_x_1, mu_x_2, stdev_x_1, stdev_x_2, mixt_frac) RESULT ( xp2 )
! Description:
! Computes the overall grid-box variance of a binormal distribution from the
! variance of each component.
! References:
!   None
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Constant
            USE constants_clubb, ONLY: one
! Constant
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: mu_x_1
            REAL(KIND=core_rknd), intent(in) :: stdev_x_2
            REAL(KIND=core_rknd), intent(in) :: mixt_frac
            REAL(KIND=core_rknd), intent(in) :: xm
            REAL(KIND=core_rknd), intent(in) :: mu_x_2
            REAL(KIND=core_rknd), intent(in) :: stdev_x_1
! Overall mean of 'x'                                   [?]
! First PDF component mean of 'x'                       [?]
! Second PDF component mean of 'x'                      [?]
! Standard deviation of 'x' in the first PDF component  [?]
! Standard deviation of 'x' in the second PDF component [?]
! Weight of the first PDF component                     [-]
! Output Variables
            REAL(KIND=core_rknd) :: xp2
! Variance of 'x' (overall)                             [?^2]
!-----------------------------------------------------------------------
!----- Begin Code -----
    xp2 = mixt_frac * ( ( mu_x_1 - xm )**2 + stdev_x_1**2 ) &
          + ( one - mixt_frac ) * ( ( mu_x_2 - xm )**2 + stdev_x_2**2 )
    return
        END FUNCTION compute_variance_binormal
!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!===============================================================================
    END MODULE pdf_utilities
