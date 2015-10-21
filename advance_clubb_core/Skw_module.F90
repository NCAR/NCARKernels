
! KGEN-generated Fortran source file
!
! Filename    : Skw_module.F90
! Generated at: 2015-10-20 14:26:59
! KGEN version: 0.5.3



    MODULE skw_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE ! Default Scope
        PUBLIC skw_func
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!-------------------------------------------------------------------------------

        elemental FUNCTION skw_func(wp2, wp3) RESULT ( skw )
! Description:
!   Calculate the skewness of w, Skw.
! References:
!   None
!-------------------------------------------------------------------------------
            USE constants_clubb, ONLY: w_tol_sqd
            USE constants_clubb, ONLY: skw_max_mag
! Constant for w_{_tol}^2, i.e. threshold for vertical velocity
! Max magnitude of skewness
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE parameters_tunable, ONLY: skw_denom_coef
            IMPLICIT NONE
! External
            INTRINSIC min, max
! Parameter Constants
! Whether to apply clipping to the final result
            LOGICAL, parameter :: l_clipping_kluge = .false.
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: wp2
            REAL(KIND=core_rknd), intent(in) :: wp3
! w'^2    [m^2/s^2]
! w'^3    [m^3/s^3]
! Output Variable
            REAL(KIND=core_rknd) :: skw
! Result Skw [-]
! ---- Begin Code ----
!Skw = wp3 / ( max( wp2, w_tol_sqd ) )**1.5_core_rknd
! Calculation of skewness to help reduce the sensitivity of this value to
! small values of wp2.
    Skw = wp3 / ( wp2 + Skw_denom_coef * w_tol_sqd )**1.5_core_rknd
! This is no longer needed since clipping is already
! imposed on wp2 and wp3 elsewhere in the code
    if ( l_clipping_kluge ) then
      Skw = min( max( Skw, -Skw_max_mag ), Skw_max_mag )
    end if
    return
        END FUNCTION skw_func
!-----------------------------------------------------------------------
    END MODULE skw_module
