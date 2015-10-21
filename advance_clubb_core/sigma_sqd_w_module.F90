
! KGEN-generated Fortran source file
!
! Filename    : sigma_sqd_w_module.F90
! Generated at: 2015-10-20 14:27:10
! KGEN version: 0.5.3



    MODULE sigma_sqd_w_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PUBLIC compute_sigma_sqd_w
        PRIVATE ! Default scope
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!---------------------------------------------------------------------------------------------------

        elemental FUNCTION compute_sigma_sqd_w(gamma_skw_fnc, wp2, thlp2, rtp2, wpthlp, wprtp) RESULT ( sigma_sqd_w )
! Description:
!   Compute the variable sigma_sqd_w (PDF width parameter)
!
! References:
!   Eqn 22 in ``Equations for CLUBB''
!---------------------------------------------------------------------------------------------------
            USE constants_clubb, ONLY: w_tol
            USE constants_clubb, ONLY: thl_tol
            USE constants_clubb, ONLY: rt_tol
! Constant(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC min, max, sqrt
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: wp2
            REAL(KIND=core_rknd), intent(in) :: wprtp
            REAL(KIND=core_rknd), intent(in) :: thlp2
            REAL(KIND=core_rknd), intent(in) :: gamma_skw_fnc
            REAL(KIND=core_rknd), intent(in) :: rtp2
            REAL(KIND=core_rknd), intent(in) :: wpthlp
! Gamma as a function of skewness   [-]
! Variance of vertical velocity     [m^2/s^2]
! Variance of liquid pot. temp.     [K^2]
! Variance of total water           [kg^2/kg^2]
! Flux of liquid pot. temp.         [m/s K]
! Flux of total water               [m/s kg/kg]
! Output Variable
            REAL(KIND=core_rknd) :: sigma_sqd_w ! PDF width parameter      [-]
! ---- Begin Code ----
!----------------------------------------------------------------
! Compute sigma_sqd_w with new formula from Vince
!----------------------------------------------------------------
    sigma_sqd_w = gamma_Skw_fnc * &
      ( 1.0_core_rknd - min( &
                  max( ( wpthlp / ( sqrt( wp2 * thlp2 )  &
                      + 0.01_core_rknd * w_tol * thl_tol ) )**2, &
                       ( wprtp / ( sqrt( wp2 * rtp2 )  &
                      + 0.01_core_rknd * w_tol * rt_tol ) )**2 &
                     ), & ! max
             1.0_core_rknd ) & ! min - Known magic number (eq. 22 from "Equations for CLUBB")
       )
! max
! min - Known magic number (eq. 22 from "Equations for CLUBB")
    return
        END FUNCTION compute_sigma_sqd_w
    END MODULE sigma_sqd_w_module
