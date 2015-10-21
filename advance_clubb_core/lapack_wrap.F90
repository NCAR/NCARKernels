
! KGEN-generated Fortran source file
!
! Filename    : lapack_wrap.F90
! Generated at: 2015-10-20 14:27:07
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
        PUBLIC band_solve, tridag_solve
! Expert routines
        PUBLIC band_solvex, tridag_solvex
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
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: diag
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: subd
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
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: supd
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: diag
            REAL(KIND=core_rknd), intent(inout), dimension(ndim) :: subd
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

        SUBROUTINE band_solvex(solve_type, nsup, nsub, ndim, nrhs, lhs, rhs, solution, rcond, err_code)
! Description:
!   Restructure and then solve a band diagonal system, with
!   diagnostic output
! References:
!   <http://www.netlib.org/lapack/single/sgbsvx.f>
!   <http://www.netlib.org/lapack/double/dgbsvx.f>
! Notes:
!   I found that due to the use of sgbcon/dgbcon it is much
!   more expensive to use this on most systems than the simple
!   driver. Use this version only if you don't case about compute time.
!   Also note that this version equilibrates the lhs and does an iterative
!   refinement of the solutions, which results in a slightly different answer
!   than the simple driver does. -dschanen 24 Sep 2008
!-----------------------------------------------------------------------
            USE error_code, ONLY: clubb_at_least_debug_level
! Logical function
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            EXTERNAL sgbsvx, dgbsvx
! Single-prec. General Band Solver eXpert
! Double-prec. General Band Solver eXpert
            INTRINSIC eoshift, kind, trim
! Input Variables
            CHARACTER(LEN=*), intent(in) :: solve_type
            INTEGER, intent(in) :: nsub
            INTEGER, intent(in) :: nsup
            INTEGER, intent(in) :: ndim
            INTEGER, intent(in) :: nrhs
! Number of superdiagonals
! Number of subdiagonals
! The order of the LHS Matrix, i.e. the # of linear equations
! Number of RHS's to back substitute for
            REAL(KIND=core_rknd), dimension(nsup+nsub+1,ndim), intent(inout) :: lhs
! Left hand side
            REAL(KIND=core_rknd), dimension(ndim,nrhs), intent(inout) :: rhs
! Right hand side(s)
! Output Variables
            REAL(KIND=core_rknd), dimension(ndim,nrhs), intent(out) :: solution
! The estimate of the reciprocal condition number of matrix
! after equilibration (if done).
            REAL(KIND=core_rknd), intent(out) :: rcond
            INTEGER, intent(out) :: err_code ! Valid calculation?
! Local Variables
! Workspaces
            REAL(KIND=core_rknd), dimension(3*ndim) :: work
            INTEGER, dimension(ndim) :: iwork
            REAL(KIND=core_rknd), dimension(2*nsub+nsup+1,ndim) :: lulhs
! LU Decomposition of the LHS
            INTEGER, dimension(ndim) :: ipivot
            REAL(KIND=core_rknd), dimension(nrhs) :: berr
            REAL(KIND=core_rknd), dimension(nrhs) :: ferr
! Forward and backward error estimate
            REAL(KIND=core_rknd), dimension(ndim) :: rscale
            REAL(KIND=core_rknd), dimension(ndim) :: cscale
! Row and column scale factors for the LHS
            INTEGER :: imain
            INTEGER :: offset
            INTEGER :: info
            INTEGER :: i
! If this doesn't come back as 0, something went wrong
! Loop iterator
! Main diagonal of the matrix
! Loop iterator
            CHARACTER :: equed
! Row equilibration status
!-----------------------------------------------------------------------
!       Reorder Matrix to use LAPACK band matrix format (5x6)
!       Shift example:
!       [    *        *     lhs(1,1) lhs(1,2) lhs(1,3) lhs(1,4) ] (2)=>
!       [    *     lhs(2,1) lhs(2,2) lhs(2,3) lhs(2,4) lhs(2,5) ] (1)=>
!       [ lhs(3,1) lhs(3,2) lhs(3,3) lhs(3,4) lhs(3,5) lhs(3,6) ]
! <=(1) [ lhs(4,2) lhs(4,3) lhs(4,4) lhs(4,5) lhs(4,6)    *     ]
! <=(2) [ lhs(5,3) lhs(5,4) lhs(5,5) lhs(5,6)    *        *     ]
!       The '*' indicates unreferenced elements.
!       For additional bands above and below the main diagonal, the
!       shifts to the left or right increases by the distance from the
!       main diagonal of the matrix.
!-----------------------------------------------------------------------
    imain = nsup + 1
! For the offset, (+) is left, and (-) is right
! Sub diagonals
    do offset = 1, nsub, 1
      lhs(imain+offset, 1:ndim) & 
      = eoshift( lhs(imain+offset, 1:ndim), offset )
    end do
! Super diagonals
    do offset = 1, nsup, 1
      lhs(imain-offset, 1:ndim) & 
      = eoshift( lhs(imain-offset, 1:ndim), -offset )
    end do
!-----------------------------------------------------------------------
!     *** The LAPACK Routine ***
!     SUBROUTINE SGBSVX( FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,
!    $                   LDAFB, IPIV, EQUED, R, C, B, LDB, X, LDX,
!    $                   RCOND, FERR, BERR, WORK, IWORK, INFO )
!-----------------------------------------------------------------------
    if ( kind( lhs(1,1) ) == dp ) then
      call dgbsvx( 'Equilibrate lhs', 'No Transpose lhs', & 
                   ndim, nsub, nsup, nrhs, & 
                   lhs, nsup+nsub+1, lulhs, 2*nsub+nsup+1,  & 
                   ipivot, equed, rscale, cscale, & 
                   rhs, ndim, solution, ndim, & 
                   rcond, ferr, berr, work, iwork, info )
    else if ( kind( lhs(1,1) ) == sp ) then
      call sgbsvx( 'Equilibrate lhs', 'No Transpose lhs', & 
                   ndim, nsub, nsup, nrhs, & 
                   lhs, nsup+nsub+1, lulhs, 2*nsub+nsup+1, & 
                   ipivot, equed, rscale, cscale, & 
                   rhs, ndim, solution, ndim, & 
                   rcond, ferr, berr, work, iwork, info )
    else
      stop "band_solvex: Cannot resolve the precision of real datatype"
! One implication of this is that CLUBB cannot be used with quad
! precision variables without a quad precision band diagonal solver
    end if
! %% debug
!       select case ( equed )
!       case ('N')
!         print *, "No equilib. was required for lhs."
!       case ('R')
!         print *, "Row equilib. was done on lhs."
!       case ('C')
!         print *, "Column equilib. was done on lhs."
!       case ('B')
!         print *, "Row and column equilib. was done on lhs."
!       end select
!       write(*,'(a,e12.5)') "Row scale : ", rscale
!       write(*,'(a,e12.5)') "Column scale: ", cscale
!       write(*,'(a,e12.5)') "Estimate of the reciprocal of the "//
!                            "condition number: ", rcond
!       write(*,'(a,e12.5)') "Forward Error Estimate: ", ferr
!       write(*,'(a,e12.5)') "Backward Error Estimate: ", berr
! %% end debug
! Diagnostic information
    if ( clubb_at_least_debug_level( 2 ) .and. any( ferr > 1.e-3_core_rknd ) ) then
      write(fstderr,*) "Warning, large error est. for: " // trim( solve_type )
      do i = 1, nrhs, 1
        write(fstderr,*) "rhs # ", i, "band_solvex forward error est. =", ferr(i)
        write(fstderr,*) "rhs # ", i, "band_solvex backward error est. =", berr(i)
      end do
      write(fstderr,'(2(a20,e15.6))') "rcond est. = ", rcond, & 
        "machine epsilon = ", epsilon( lhs(1,1) )
    end if
    select case( info )
                CASE ( : -1 )
      write(fstderr,*) trim( solve_type )// & 
        " illegal value for argument", -info
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
        write(fstderr,*) trim( solve_type )// & 
          " Warning: matrix singular to working precision."
        write(fstderr,'(a,e12.5)')  & 
          "Estimate of the reciprocal of the"// & 
          " condition number: ", rcond
        err_code = clubb_no_error
      else
        write(fstderr,*) trim( solve_type )// & 
          " band solver: singular matrix"
        err_code = clubb_singular_matrix
      end if
    end select
    return
        END SUBROUTINE band_solvex
!-----------------------------------------------------------------------

        SUBROUTINE band_solve(solve_type, nsup, nsub, ndim, nrhs, lhs, rhs, solution, err_code)
! Description:
!   Restructure and then solve a band diagonal system
! References:
!   <http://www.netlib.org/lapack/single/sgbsv.f>
!   <http://www.netlib.org/lapack/double/dgbsv.f>
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            EXTERNAL sgbsv, dgbsv
! Single-prec. General Band Solver
! Double-prec. General Band Solver
            INTRINSIC eoshift, kind, trim
! Input Variables
            CHARACTER(LEN=*), intent(in) :: solve_type
            INTEGER, intent(in) :: nrhs
            INTEGER, intent(in) :: nsup
            INTEGER, intent(in) :: ndim
            INTEGER, intent(in) :: nsub
! Number of superdiagonals
! Number of subdiagonals
! The order of the LHS Matrix, i.e. the # of linear equations
! Number of RHS's to solve for
! Note: matrix lhs is intent(in), not intent(inout)
! as in the subroutine band_solvex( )
            REAL(KIND=core_rknd), dimension(nsup+nsub+1,ndim), intent(in) :: lhs
! Left hand side
            REAL(KIND=core_rknd), dimension(ndim,nrhs), intent(inout) :: rhs
! Right hand side(s)
! Output Variables
            REAL(KIND=core_rknd), dimension(ndim,nrhs), intent(out) :: solution
            INTEGER, intent(out) :: err_code ! Valid calculation?
! Local Variables
! Workspaces
            REAL(KIND=core_rknd), dimension(2*nsub+nsup+1,ndim) :: lulhs
! LU Decomposition of the LHS
            REAL(KIND=dp), dimension(2*nsub+nsup+1,ndim) :: lulhs_dp
            REAL(KIND=dp), dimension(ndim,nrhs) :: rhs_dp
            INTEGER, dimension(ndim) :: ipivot
            INTEGER :: imain
            INTEGER :: offset
            INTEGER :: info
! If this doesn't come back as 0, something went wrong
! Loop iterator
! Main diagonal of the matrix
! Copy LHS into Decomposition scratch space
    lulhs = 0.0_core_rknd
    lulhs(nsub+1:2*nsub+nsup+1, 1:ndim) = lhs(1:nsub+nsup+1, 1:ndim)
!-----------------------------------------------------------------------
!       Reorder LU Matrix to use LAPACK band matrix format
!       Shift example for lulhs matrix (note the extra bands):
!       [    +        +        +        +        +        +     ]
!       [    +        +        +        +        +        +     ]
!       [    *        *     lhs(1,1) lhs(1,2) lhs(1,3) lhs(1,4) ] (2)=>
!       [    *     lhs(2,1) lhs(2,2) lhs(2,3) lhs(2,4) lhs(2,5) ] (1)=>
!       [ lhs(3,1) lhs(3,2) lhs(3,3) lhs(3,4) lhs(3,5) lhs(3,6) ]
! <=(1) [ lhs(4,2) lhs(4,3) lhs(4,4) lhs(4,5) lhs(4,6)    *     ]
! <=(2) [ lhs(5,3) lhs(5,4) lhs(5,5) lhs(5,6)    *        *     ]
!       [    +        +        +        +        +        +     ]
!       [    +        +        +        +        +        +     ]
!       The '*' indicates unreferenced elements.
!       The '+' indicates an element overwritten during decomposition.
!       For additional bands above and below the main diagonal, the
!       shifts to the left or right increases by the distance from the
!       main diagonal of the matrix.
!-----------------------------------------------------------------------
! Reorder lulhs, omitting the additional 2*nsub bands
! that are used for the LU decomposition of the matrix.
    imain = nsub + nsup + 1
! For the offset, (+) is left, and (-) is right
! Sub diagonals
    do offset = 1, nsub, 1
      lulhs(imain+offset, 1:ndim) & 
      = eoshift( lulhs(imain+offset, 1:ndim), offset )
    end do
! Super diagonals
    do offset = 1, nsup, 1
      lulhs(imain-offset, 1:ndim) & 
      = eoshift( lulhs(imain-offset, 1:ndim), -offset )
    end do
!-----------------------------------------------------------------------
!       *** LAPACK routine ***
!       SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )
!-----------------------------------------------------------------------
    if ( kind( lhs(1,1) ) == dp ) then
      call dgbsv( ndim, nsub, nsup, nrhs, lulhs, nsub*2+nsup+1,  & 
                  ipivot, rhs, ndim, info )
    else if ( kind( lhs(1,1) ) == sp ) then
      call sgbsv( ndim, nsub, nsup, nrhs, lulhs, nsub*2+nsup+1,  & 
                  ipivot, rhs, ndim, info )
    else
!stop "band_solve: Cannot resolve the precision of real datatype"
! One implication of this is that CLUBB cannot be used with quad
! precision variables without a quad precision band diagonal solver
! Eric Raut Aug 2013: force double precision
      lulhs_dp = real( lulhs, kind=dp )
      rhs_dp = real( rhs, kind=dp )
      call dgbsv( ndim, nsub, nsup, nrhs, lulhs_dp, nsub*2+nsup+1,  &
                  ipivot, rhs_dp, ndim, info )
      rhs = real( rhs_dp, kind=core_rknd )
    end if
    select case( info )
                CASE ( : -1 )
      write(fstderr,*) trim( solve_type )// & 
        " illegal value for argument ", -info
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
      write(fstderr,*) trim( solve_type )//" band solver: singular matrix"
      err_code = clubb_singular_matrix
      solution = -999._core_rknd
    end select
    return
        END SUBROUTINE band_solve
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
