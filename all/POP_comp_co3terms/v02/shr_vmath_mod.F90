
! KGEN-generated Fortran source file
!
! Filename    : shr_vmath_mod.F90
! Generated at: 2015-06-05 14:52:13
! KGEN version: 0.4.11



    MODULE shr_vmath_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !----------------------------------------------------------------------------
        ! routines that evaluate various math functions for vector arguments
        ! intended to provide platform independent access to vendor optimized code
        !----------------------------------------------------------------------------
        USE shr_kind_mod, only : shr_kind_r8
        USE shr_kind_mod, only : shr_kind_in
        IMPLICIT NONE
        PRIVATE
        PUBLIC shr_vmath_log, shr_vmath_sqrt, shr_vmath_exp
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !===============================================================================

        SUBROUTINE shr_vmath_sqrt(x, y, n)
            !----- arguments ---
            INTEGER(KIND=shr_kind_in), intent(in) :: n ! vector length
            REAL(KIND=shr_kind_r8), intent(in) :: x(n) ! input vector argument
            REAL(KIND=shr_kind_r8), intent(out) :: y(n) ! output vector argument
            !-------------------------------------------------------------------------------
            ! PURPOSE: sqrt for vector arguments, optimized on different platforms
            !-------------------------------------------------------------------------------
   !call vdsqrt(n, X, Y)
   !return
   Y = sqrt(X)
   return
        END SUBROUTINE shr_vmath_sqrt
        !===============================================================================

        !===============================================================================

        SUBROUTINE shr_vmath_exp(x, y, n)
            !----- arguments ---
            INTEGER(KIND=shr_kind_in), intent(in) :: n ! vector length
            REAL(KIND=shr_kind_r8), intent(in) :: x(n) ! input vector argument
            REAL(KIND=shr_kind_r8), intent(out) :: y(n) ! output vector argument
            !-------------------------------------------------------------------------------
            ! PURPOSE: exp for vector arguments, optimized on different platforms
            !-------------------------------------------------------------------------------
   !call vdexp(n, X, Y)
   !return
   Y = exp(X)
   return
        END SUBROUTINE shr_vmath_exp
        !===============================================================================

        !===============================================================================

        SUBROUTINE shr_vmath_log(x, y, n)
            !----- arguments ---
            INTEGER(KIND=shr_kind_in), intent(in) :: n ! vector length
            REAL(KIND=shr_kind_r8), intent(in) :: x(n) ! input vector argument
            REAL(KIND=shr_kind_r8), intent(out) :: y(n) ! output vector argument
            !-------------------------------------------------------------------------------
            ! PURPOSE: log for vector arguments, optimized on different platforms
            !-------------------------------------------------------------------------------
   !call vdln(n, X, Y)
   !return
   Y = log(X)
   return
        END SUBROUTINE shr_vmath_log
        !===============================================================================

        !===============================================================================

        !===============================================================================
    END MODULE shr_vmath_mod
