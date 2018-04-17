!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:45
!KGEN version : 0.6.1

!-------------------------------------------------------------------------
!$Id: Skw_module.F90 6849 2014-04-22 21:52:30Z charlass@uwm.edu $
!===============================================================================
module Skw_module

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PRIVATE

    PUBLIC skw_func

  contains

!-------------------------------------------------------------------------------
  elemental function Skw_func( wp2, wp3 )  &
    result( Skw )

! Description:
!   Calculate the skewness of w, Skw.

! References:
!   None
!-------------------------------------------------------------------------------

      USE constants_clubb, ONLY: w_tol_sqd, skw_max_mag

      USE clubb_precision, ONLY: core_rknd

      USE parameters_tunable, ONLY: skw_denom_coef

    implicit none

    ! External
    intrinsic :: min, max

    ! Parameter Constants
    ! Whether to apply clipping to the final result
    logical, parameter ::  & 
      l_clipping_kluge = .false.

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: & 
      wp2,  & ! w'^2    [m^2/s^2]
      wp3     ! w'^3    [m^3/s^3]

    ! Output Variable
    real( kind = core_rknd ) :: & 
      Skw     ! Result Skw [-]

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
  end function Skw_func
!-----------------------------------------------------------------------

end module Skw_module