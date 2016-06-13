
! KGEN-generated Fortran source file
!
! Filename    : advance_helper_module.F90
! Generated at: 2015-10-20 14:27:07
! KGEN version: 0.5.3



    MODULE advance_helper_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   This module contains helper methods for the advance_* modules.
!------------------------------------------------------------------------
        IMPLICIT NONE
        PUBLIC calc_stability_correction, set_boundary_conditions_lhs, set_boundary_conditions_rhs
        PRIVATE ! Set Default Scope
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!---------------------------------------------------------------------------

        SUBROUTINE set_boundary_conditions_lhs(diag_index, low_bound, high_bound, lhs, diag_index2, low_bound2, high_bound2)
! Description:
!   Sets the boundary conditions for a left-hand side LAPACK matrix.
!
! References:
!   none
!---------------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Exernal
            INTRINSIC present
! Input Variables
            INTEGER, intent(in) :: low_bound
            INTEGER, intent(in) :: high_bound
            INTEGER, intent(in) :: diag_index
! boundary indexes for the first variable
! Input / Output Variables
            REAL(KIND=core_rknd), dimension(:,:), intent(inout) :: lhs
! left hand side of the LAPACK matrix equation
! Optional Input Variables
            INTEGER, intent(in), optional :: diag_index2
            INTEGER, intent(in), optional :: low_bound2
            INTEGER, intent(in), optional :: high_bound2
! boundary indexes for the second variable
! --------------------- BEGIN CODE ----------------------
    if ( ( present( low_bound2 ) .or. present( high_bound2 ) ) .and. &
         ( .not. present( diag_index2 ) ) ) then
      stop "Boundary index provided without diag_index."
    end if
! Set the lower boundaries for the first variable
    lhs(:,low_bound) = 0.0_core_rknd
    lhs(diag_index,low_bound) = 1.0_core_rknd
! Set the upper boundaries for the first variable
    lhs(:,high_bound) = 0.0_core_rknd
    lhs(diag_index,high_bound) = 1.0_core_rknd
! Set the lower boundaries for the second variable, if it is provided
    if ( present( low_bound2 ) ) then
      lhs(:,low_bound2) = 0.0_core_rknd
      lhs(diag_index2,low_bound2) = 1.0_core_rknd
    end if
! Set the upper boundaries for the second variable, if it is provided
    if ( present( high_bound2 ) ) then
      lhs(:,high_bound2) = 0.0_core_rknd
      lhs(diag_index2,high_bound2) = 1.0_core_rknd
    end if
    return
        END SUBROUTINE set_boundary_conditions_lhs
!--------------------------------------------------------------------------

        SUBROUTINE set_boundary_conditions_rhs(low_value, low_bound, high_value, high_bound, rhs, low_value2, low_bound2, &
        high_value2, high_bound2)
! Description:
!   Sets the boundary conditions for a right-hand side LAPACK vector.
!
! References:
!   none
!---------------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Exernal
            INTRINSIC present
! Input Variables
! The values for the first variable
            REAL(KIND=core_rknd), intent(in) :: low_value
            REAL(KIND=core_rknd), intent(in) :: high_value
! The bounds for the first variable
            INTEGER, intent(in) :: high_bound
            INTEGER, intent(in) :: low_bound
! Input / Output Variables
! The right-hand side vector
            REAL(KIND=core_rknd), dimension(:), intent(inout) :: rhs
! Optional Input Variables
! The values for the second variable
            REAL(KIND=core_rknd), intent(in), optional :: high_value2
            REAL(KIND=core_rknd), intent(in), optional :: low_value2
! The bounds for the second variable
            INTEGER, intent(in), optional :: high_bound2
            INTEGER, intent(in), optional :: low_bound2
! -------------------- BEGIN CODE ------------------------
! Stop execution if a boundary was provided without a value
    if ( (present( low_bound2 ) .and. (.not. present( low_value2 ))) .or. &
         (present( high_bound2 ) .and. (.not. present( high_value2 ))) ) then
      stop "Boundary condition provided without value."
    end if
! Set the lower and upper bounds for the first variable
    rhs(low_bound) = low_value
    rhs(high_bound) = high_value
! If a lower bound was given for the second variable, set it
    if ( present( low_bound2 ) ) then
      rhs(low_bound2) = low_value2
    end if
! If an upper bound was given for the second variable, set it
    if ( present( high_bound2 ) ) then
      rhs(high_bound2) = high_value2
    end if
    return
        END SUBROUTINE set_boundary_conditions_rhs
!===============================================================================

        FUNCTION calc_stability_correction(thlm, lscale, em) RESULT ( stability_correction )
!
! Description:
!   Stability Factor
!
! References:
!
!--------------------------------------------------------------------
            USE parameters_model, ONLY: t0
! Variables(s)
            USE constants_clubb, ONLY: grav
            USE constants_clubb, ONLY: zero
! Constant(s)
            USE grid_class, ONLY: gr
            USE grid_class, ONLY: ddzt
            USE grid_class, ONLY: zt2zm
! Variable(s)
! Procedure(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: lscale
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: em
! Turbulent mixing length                   [m]
! Turbulent Kinetic Energy (TKE)            [m^2/s^2]
! th_l (thermo. levels)                     [K]
! Result
            REAL(KIND=core_rknd), dimension(gr%nz) :: stability_correction
! Local Variables
            REAL(KIND=core_rknd) :: lambda0_stability_coef
! []
            REAL(KIND=core_rknd), dimension(gr%nz) :: brunt_vaisala_freq
            REAL(KIND=core_rknd), dimension(gr%nz) :: lambda0_stability
!  []
!------------ Begin Code --------------
      lambda0_stability_coef = 0.04_core_rknd
      brunt_vaisala_freq = ( grav / T0 ) * ddzt( thlm )
      lambda0_stability = merge( lambda0_stability_coef, zero, brunt_vaisala_freq > zero )
      stability_correction = 1.0_core_rknd &
        + min( lambda0_stability * brunt_vaisala_freq * zt2zm( Lscale )**2 / em, 3.0_core_rknd )
      return
        END FUNCTION calc_stability_correction
    END MODULE advance_helper_module
