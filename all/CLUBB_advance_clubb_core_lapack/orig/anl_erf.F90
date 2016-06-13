
! KGEN-generated Fortran source file
!
! Filename    : anl_erf.F90
! Generated at: 2015-10-20 14:27:06
! KGEN version: 0.5.3



    MODULE anl_erf
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PUBLIC erf, dp_erf
        PRIVATE cr_erf
! The interfaces allow us to avoid a compiler warning about
! shadowing the intrinsic functions

        INTERFACE erf
            MODULE PROCEDURE cr_erf
        END INTERFACE 

        PRIVATE ! Default Scope
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!=============================================================================

        pure FUNCTION cr_erf(x) RESULT ( erfx_core_rknd )
! Description:
!   Calls dp_erf after casting x to double precision.
!   This allows CLUBB to run erf even when core_rknd is in single precision.
!
!  Arguments:
!    Input, real ( kind = dp ) x, the argument of ERF.
!    Output, real ( kind = core_rknd ) erfx_core_rknd, the value of ERF(X).
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
            USE clubb_precision, ONLY: dp
! Constants
            IMPLICIT NONE
! Input Variables(s)
            REAL(KIND=core_rknd), intent(in) :: x
! Return type
            REAL(KIND=core_rknd) :: erfx_core_rknd
! Local Variables
            REAL(KIND=dp) :: x_dp
            REAL(KIND=dp) :: erfx_dp
! Cast the input to dp
    x_dp = real( x, kind = dp )
! Call the function with the correct argument
    erfx_dp = dp_erf( x_dp )
! Get the output in core_rknd
    erfx_core_rknd = real( erfx_dp, kind = core_rknd )
    return
        END FUNCTION cr_erf
!=============================================================================

        pure FUNCTION dp_erf(x) RESULT ( erfx )
! Description:
!   DP_ERF evaluates the error function DP_ERF(X).
!
!   Original Author:
!     William Cody,
!     Mathematics and Computer Science Division,
!     Argonne National Laboratory,
!     Argonne, Illinois, 60439.
!
!   References:
!     William Cody,
!     "Rational Chebyshev approximations for the error function",
!     Mathematics of Computation,
!     1969, pages 631-638.
!
!  Arguments:
!    Input, real ( kind = dp ) X, the argument of ERF.
!    Output, real ( kind = dp ) ERFX, the value of ERF(X).
!
! Modifications:
!   kind = 8 was replaced by the more portable sp and dp by UWM.
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: dp
! Constants
            IMPLICIT NONE
! Input Variables(s)
            REAL(KIND=dp), intent(in) :: x
! External
            INTRINSIC epsilon, exp, aint
! Local Constants
            REAL(KIND=dp), parameter, dimension( 5 ) :: a = (/ 3.16112374387056560e+00_dp,            1.13864154151050156e+02_dp, &
                       3.77485237685302021e+02_dp,            3.20937758913846947e+03_dp,            1.85777706184603153e-01_dp /)
            REAL(KIND=dp), parameter, dimension( 4 ) :: b = (/ 2.36012909523441209e+01_dp,            2.44024637934444173e+02_dp, &
                       1.28261652607737228e+03_dp,            2.84423683343917062e+03_dp /)
            REAL(KIND=dp), parameter, dimension( 9 ) :: c = (/ 5.64188496988670089e-01_dp,            8.88314979438837594e+00_dp, &
                       6.61191906371416295e+01_dp,            2.98635138197400131e+02_dp,            8.81952221241769090e+02_dp,  &
                      1.71204761263407058e+03_dp,            2.05107837782607147e+03_dp,            1.23033935479799725e+03_dp,   &
                     2.15311535474403846e-08_dp /)
            REAL(KIND=dp), parameter, dimension( 8 ) :: d = (/ 1.57449261107098347e+01_dp,            1.17693950891312499e+02_dp, &
                       5.37181101862009858e+02_dp,             1.62138957456669019e+03_dp,            3.29079923573345963e+03_dp, &
                       4.36261909014324716e+03_dp,            3.43936767414372164e+03_dp,            1.23033935480374942e+03_dp /)
            REAL(KIND=dp), parameter, dimension( 6 ) :: p = (/ 3.05326634961232344e-01_dp,            3.60344899949804439e-01_dp, &
                       1.25781726111229246e-01_dp,            1.60837851487422766e-02_dp,            6.58749161529837803e-04_dp,  &
                      1.63153871373020978e-02_dp /)
            REAL(KIND=dp), parameter, dimension( 5 ) :: q = (/ 2.56852019228982242e+00_dp,            1.87295284992346047e+00_dp, &
                       5.27905102951428412e-01_dp,            6.05183413124413191e-02_dp,            2.33520497626869185e-03_dp /)
            REAL(KIND=dp), parameter :: thresh = 0.46875e+00_dp
            REAL(KIND=dp), parameter :: xbig   = 26.543e+00_dp
            REAL(KIND=dp), parameter :: sqrpi  = 0.56418958354775628695e+00_dp
! Return type
            REAL(KIND=dp) :: erfx
! Local variables
            REAL(KIND=dp) :: xabs
            REAL(KIND=dp) :: xsq
            REAL(KIND=dp) :: xnum
            REAL(KIND=dp) :: xden
            REAL(KIND=dp) :: del
            INTEGER :: i ! Index
!-----------------------------------------------------------------------
! Get the abs value of xabs - schemena 20140827
    xabs = abs( x )
!
!  Evaluate ERF(X) for |X| <= 0.46875.
!
    if ( xabs <= THRESH ) then
      if ( epsilon( xabs ) < xabs ) then
        xsq = xabs * xabs
      else
        xsq = 0.0E+00_dp
      end if
      xnum = a(5) * xsq
      xden = xsq
      do i = 1, 3
        xnum = ( xnum + a(i) ) * xsq
        xden = ( xden + b(i) ) * xsq
      end do
      erfx = x * ( xnum + a(4) ) / ( xden + b(4) )
!
!  Evaluate ERFC(X) for 0.46875 <= |X| <= 4.0.
!
    else if ( xabs <= 4.0E+00_dp ) then
      xnum = c(9) * xabs
      xden = xabs
      do i = 1, 7
        xnum = ( xnum + c(i) ) * xabs
        xden = ( xden + d(i) ) * xabs
      end do
      erfx = ( xnum + c(8) ) / ( xden + d(8) )
      xsq = aint( xabs * 16.0E+00_dp ) / 16.0E+00_dp
      del = ( xabs - xsq ) * ( xabs + xsq )
! xsq * xsq in the exponential was changed to xsq**2.
! This seems to decrease runtime by about a half a percent.
! ~~EIHoppe//20090622
      erfx = exp( - xsq**2 ) * exp( - del ) * erfx
      erfx = ( 0.5E+00_dp - erfx ) + 0.5E+00_dp
      if ( x < 0.0E+00_dp ) then
        erfx = - erfx
      end if
!
!  Evaluate ERFC(X) for 4.0 < |X|.
!
    else
      if ( XBIG <= xabs ) then
        if ( 0.0E+00_dp < real(x, kind=dp) ) then
          erfx = 1.0E+00_dp
        else
          erfx = -1.0E+00_dp
        end if
      else
        xsq = 1.0E+00_dp / ( xabs * xabs )
        xnum = p(6) * xsq
        xden = xsq
        do i = 1, 4
          xnum = ( xnum + p(i) ) * xsq
          xden = ( xden + q(i) ) * xsq
        end do
        erfx = xsq * ( xnum + p(5) ) / ( xden + q(5) )
        erfx = ( SQRPI -  erfx ) / xabs
        xsq = aint( xabs * 16.0E+00_dp ) / 16.0E+00_dp
        del = ( xabs - xsq ) * ( xabs + xsq )
        erfx = exp( - xsq * xsq ) * exp( - del ) * erfx
        erfx = ( 0.5E+00_dp - erfx ) + 0.5E+00_dp
        if ( x < 0.0E+00_dp ) then
          erfx = - erfx
        end if
      end if
    end if
    return
        END FUNCTION dp_erf
!=============================================================================

!=============================================================================

!===============================================================================
    END MODULE anl_erf
