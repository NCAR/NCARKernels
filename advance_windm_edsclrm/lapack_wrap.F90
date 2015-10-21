
! KGEN-generated Fortran source file
!
! Filename    : lapack_wrap.F90
! Generated at: 2015-10-21 08:59:10
! KGEN version: 0.5.3



    MODULE lapack_wrap
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   Wrappers for the band diagonal and tridiagonal direct matrix
!   solvers contained in the LAPACK library.
! References:
!   LAPACK--Linear Algebra PACKage
!   URL: <http://www.netlib.org/lapack/>
!-----------------------------------------------------------------------
        USE constants_clubb, ONLY: fstderr
! Variable(s)
        USE error_code, ONLY: clubb_bad_lapack_arg
        USE error_code, ONLY: clubb_var_equals_nan
        USE error_code, ONLY: clubb_no_error
        USE error_code, ONLY: clubb_singular_matrix
! Variable(s)
        USE clubb_precision, ONLY: dp
! Variable(s)
        IMPLICIT NONE
! Simple routines
        PUBLIC tridag_solve
! Expert routines
        PUBLIC tridag_solvex
        PRIVATE lapack_isnan
! A best guess for what the precision of a single precision and double
! precision float is in LAPACK.  Hopefully this will work more portably on
! architectures like Itanium than the old code -dschanen 11 Aug 2011
        INTEGER, parameter, private :: sp = kind (0.0)
        PRIVATE ! Set Default Scope
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!-----------------------------------------------------------------------

        SUBROUTINE tridag_solvex(solve_type, ndim, nrhs, supd, diag, subd, rhs, solution, rcond, err_code)
! Description:
!   Solves a tridiagonal system of equations (expert routine).
! References:
!   <http://www.netlib.org/lapack/single/sgtsvx.f>
!   <http://www.netlib.org/lapack/double/dgtsvx.f>
! Notes:
!   More expensive than the simple routine, but tridiagonal
!   decomposition is still relatively cheap.
!-----------------------------------------------------------------------
            USE error_code, ONLY: clubb_at_least_debug_level
! Logical function
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            EXTERNAL sgtsvx, dgtsvx
! Single-prec. General Tridiagonal Solver eXpert
! Double-prec. General Tridiagonal Solver eXpert
            INTRINSIC kind
! Input variables
            CHARACTER(LEN=*), intent(in) :: solve_type
! Used to write a message if this fails
            INTEGER, intent(in) :: ndim
            INTEGER, intent(in) :: nrhs
! N-dimension of matrix
! # of right hand sides to back subst. after LU-decomp.
! Input/Output variables
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: supd
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: subd
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: diag
! Main diagonal
! Sub and super diagonal
            REAL(KIND=core_rknd), intent(inout), dimension(ndim,nrhs) :: rhs
! RHS input
! The estimate of the reciprocal of the condition number on the LHS matrix.
! If rcond is < machine precision the matrix is singular to working
! precision, and info == ndim+1.  If rcond == 0, then the LHS matrix
! is singular.  This condition is indicated by a return code of info > 0.
            REAL(KIND=core_rknd), intent(out) :: rcond
            INTEGER, intent(out) :: err_code
! Used to determine when a decomp. failed
! Output variables
            REAL(KIND=core_rknd), intent(out), dimension(ndim,nrhs) :: solution
! Solution
! Local Variables
! These contain the decomposition of the matrix
            REAL(KIND=core_rknd), dimension(ndim-1) :: dlf
            REAL(KIND=core_rknd), dimension(ndim-1) :: duf
            REAL(KIND=core_rknd), dimension(ndim) :: df
            REAL(KIND=core_rknd), dimension(ndim-2) :: du2
            INTEGER, dimension(ndim) :: ipivot
! Index of pivots done during decomposition
            INTEGER, dimension(ndim) :: iwork
! `scrap' array
            REAL(KIND=core_rknd), dimension(nrhs) :: ferr
            REAL(KIND=core_rknd), dimension(nrhs) :: berr
! Forward error estimate
! Backward error estimate
            REAL(KIND=core_rknd), dimension(3*ndim) :: work
! `Scrap' array
            INTEGER :: info ! Diagnostic output
            INTEGER :: i ! Array index
!-----------------------------------------------------------------------
!     *** The LAPACK Routine ***
!     SUBROUTINE SGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF, DUF,
!    $                   DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR,
!    $                   WORK, IWORK, INFO )
!-----------------------------------------------------------------------
    if ( kind( diag(1) ) == dp ) then
      call dgtsvx( "Not Factored", "No Transpose lhs", ndim, nrhs,  & 
                   subd(2:ndim), diag, supd(1:ndim-1),  & 
                   dlf, df, duf, du2, ipivot,  & 
                   rhs, ndim, solution, ndim, rcond, & 
                   ferr, berr, work, iwork, info )
    else if ( kind( diag(1) ) == sp ) then
      call sgtsvx( "Not Factored", "No Transpose lhs", ndim, nrhs,  & 
                   subd(2:ndim), diag, supd(1:ndim-1),  & 
                   dlf, df, duf, du2, ipivot,  & 
                   rhs, ndim, solution, ndim, rcond, & 
                   ferr, berr, work, iwork, info )
    else
      stop "tridag_solvex: Cannot resolve the precision of real datatype"
    end if
! Print diagnostics for when ferr is large
    if ( clubb_at_least_debug_level( 2 ) .and. any( ferr > 1.e-3_core_rknd ) ) then
      write(fstderr,*) "Warning, large error est. for: " // trim( solve_type )
      do i = 1, nrhs, 1
        write(fstderr,*) "rhs # ", i, "tridag forward error est. =", ferr(i)
        write(fstderr,*) "rhs # ", i, "tridag backward error est. =", berr(i)
      end do
      write(fstderr,'(2(a20,e15.6))') "rcond est. = ", rcond, & 
        "machine epsilon = ", epsilon( diag(1) )
    end if
    select case( info )
                CASE ( : -1 )
      write(fstderr,*) trim( solve_type )// & 
        "illegal value in argument", -info
      err_code = clubb_bad_lapack_arg
                CASE ( 0 )
! Success!
      if ( lapack_isnan( ndim, nrhs, solution ) ) then
        err_code = clubb_var_equals_NaN 
      else
        err_code = clubb_no_error
      end if
                CASE ( 1 : )
      if ( info == ndim+1 ) then
        write(fstderr,*) trim( solve_type) // & 
          " Warning: matrix is singular to working precision."
        write(fstderr,'(a,e12.5)')  & 
          "Estimate of the reciprocal of the condition number: ", rcond
        err_code = clubb_no_error
      else
        write(fstderr,*) solve_type// & 
          " singular matrix."
        err_code = clubb_singular_matrix
      end if
    end select
    return
        END SUBROUTINE tridag_solvex
!-----------------------------------------------------------------------

        SUBROUTINE tridag_solve(solve_type, ndim, nrhs, supd, diag, subd, rhs, solution, err_code)
! Description:
!   Solves a tridiagonal system of equations (simple routine)
! References:
!   <http://www.netlib.org/lapack/single/sgtsv.f>
!   <http://www.netlib.org/lapack/double/dgtsv.f>
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            EXTERNAL sgtsv, dgtsv
! Single-prec. General Tridiagonal Solver eXpert
! Double-prec. General Tridiagonal Solver eXpert
            INTRINSIC kind
! Input variables
            CHARACTER(LEN=*), intent(in) :: solve_type
! Used to write a message if this fails
            INTEGER, intent(in) :: ndim
            INTEGER, intent(in) :: nrhs
! N-dimension of matrix
! # of right hand sides to back subst. after LU-decomp.
! Input/Output variables
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: subd
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: supd
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: diag
! Main diagonal
! Sub and super diagonal
            REAL(KIND=core_rknd), intent(inout), dimension(ndim,nrhs) :: rhs
! RHS input
! Output variables
            REAL(KIND=core_rknd), intent(out), dimension(ndim,nrhs) :: solution
! Solution
            INTEGER, intent(out) :: err_code
! Used to determine when a decomp. failed
! Local Variables
            REAL(KIND=dp), dimension(ndim) :: subd_dp
            REAL(KIND=dp), dimension(ndim) :: diag_dp
            REAL(KIND=dp), dimension(ndim) :: supd_dp
            REAL(KIND=dp), dimension(ndim,nrhs) :: rhs_dp
            INTEGER :: info ! Diagnostic output
!-----------------------------------------------------------------------
!       *** The LAPACK Routine ***
!       SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
!-----------------------------------------------------------------------
    if ( kind( diag(1) ) == dp ) then
      call dgtsv( ndim, nrhs, subd(2:ndim), diag, supd(1:ndim-1),  & 
                  rhs, ndim, info )
    else if ( kind( diag(1) ) == sp ) then
      call sgtsv( ndim, nrhs, subd(2:ndim), diag, supd(1:ndim-1),  & 
                  rhs, ndim, info )
    else
!stop "tridag_solve: Cannot resolve the precision of real datatype"
! Eric Raut Aug 2013: Force double precision
      subd_dp = real( subd, kind=dp )
      diag_dp = real( diag, kind=dp )
      supd_dp = real( supd, kind=dp )
      rhs_dp = real( rhs, kind=dp )
      call dgtsv( ndim, nrhs, subd_dp(2:ndim), diag_dp, supd_dp(1:ndim-1),  &
                  rhs_dp, ndim, info )
      subd = real( subd_dp, kind=core_rknd )
      diag = real( diag_dp, kind=core_rknd )
      supd = real( supd_dp, kind=core_rknd )
      rhs = real( rhs_dp, kind=core_rknd )
    end if
    select case( info )
                CASE ( : -1 )
      write(fstderr,*) trim( solve_type )// & 
        " illegal value in argument", -info
      err_code = clubb_bad_lapack_arg
      solution = -999._core_rknd
                CASE ( 0 )
! Success!
      if ( lapack_isnan( ndim, nrhs, rhs ) ) then
        err_code = clubb_var_equals_NaN 
      else
        err_code = clubb_no_error
      end if
      solution = rhs
                CASE ( 1 : )
      write(fstderr,*) trim( solve_type )//" singular matrix."
      err_code = clubb_singular_matrix
      solution = -999._core_rknd
    end select
    return
        END SUBROUTINE tridag_solve
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

        logical FUNCTION lapack_isnan(ndim, nrhs, variable)
! Description:
!   Check for NaN values in a variable using the LAPACK subroutines
! References:
!   <http://www.netlib.org/lapack/single/sisnan.f>
!   <http://www.netlib.org/lapack/double/disnan.f>
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
            INTRINSIC any
            INTEGER, intent(in) :: ndim
            INTEGER, intent(in) :: nrhs
! Size of variable
! Number of right hand sides
            REAL(KIND=core_rknd), dimension(ndim,nrhs), intent(in) :: variable
! Variable to check
    lapack_isnan = any( variable(:,1:nrhs) /= variable(:,1:nrhs) )
    return
        END FUNCTION lapack_isnan
    END MODULE lapack_wrap
