!KGEN-generated Fortran source file

!Generated at : 2016-01-07 11:47:38
!KGEN version : 0.6.1

!-------------------------------------------------------------------------------
!$Id: interpolation.F90 7200 2014-08-13 15:15:12Z betlej@uwm.edu $
!===============================================================================
module interpolation


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PRIVATE

    PUBLIC lin_interpolate_two_points, linear_interp_factor, mono_cubic_interp

  contains

!-------------------------------------------------------------------------------
  function lin_interpolate_two_points( height_int, height_high, height_low, &
    var_high, var_low )

! Description:
! This function computes a linear interpolation of the value of variable.
! Given two known values of a variable at two height values, the value
! of that variable at a height between those two height levels (rather
! than a height outside of those two height levels) is computed.
!
! Here is a diagram:
!
!  ################################ Height high, know variable value
!
!
!
!  -------------------------------- Height to be interpolated to; linear interpolation
!
!
!
!
!
!  ################################ Height low, know variable value
!
!
! FORMULA:
!
! variable(@ Height interpolation) =
!
! [ (variable(@ Height high) - variable(@ Height low)) / (Height high - Height low) ]
! * (Height interpolation - Height low)  +  variable(@ Height low)

! Comments from WRF-HOC, Brian Griffin.

! References:
! None
!-------------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd

      USE constants_clubb, ONLY: fstderr

    implicit none

    ! Input Variables

    real( kind = core_rknd ), intent(in) :: &
      height_int,  & ! Height to be interpolated to     [m]
      height_high, & ! Height above the interpolation   [m]
      height_low,  & ! Height below the interpolation   [m]
      var_high,    & ! Variable above the interpolation [units vary]
      var_low        ! Variable below the interpolation [units vary]

    ! Output Variables
    real( kind = core_rknd ) :: lin_interpolate_two_points
    
    ! Check for valid input
    if ( abs(height_low - height_high) < 1.0e-12_core_rknd ) then
      write(fstderr,*) "lin_interpolate_two_points: height_high and height_low cannot be equal."
      stop
    end if

    ! Compute linear interpolation

    lin_interpolate_two_points = ( ( height_int - height_low )/( height_high - height_low ) ) &
      * ( var_high - var_low ) + var_low

    return
  end function lin_interpolate_two_points

  !-------------------------------------------------------------------------------------------------
  elemental real( kind = core_rknd ) function linear_interp_factor( factor, var_high, var_low )
  ! Description:
  !   Determines the coefficient for a linear interpolation
  ! 
  ! References:
  !   None
  !-------------------------------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd

    implicit none

    real( kind = core_rknd ), intent(in) :: &
      factor,   & ! Factor                           [units vary]  
      var_high, & ! Variable above the interpolation [units vary]
      var_low     ! Variable below the interpolation [units vary]

    linear_interp_factor = factor * ( var_high - var_low ) + var_low

    return
  end function linear_interp_factor
  !-------------------------------------------------------------------------------------------------
  function mono_cubic_interp &
    ( z_in, km1, k00, kp1, kp2, zm1, z00, zp1, zp2, fm1, f00, fp1, fp2 ) result ( f_out )

  ! Description:
  !   Steffen's monotone cubic interpolation method
  !   Returns monotone cubic interpolated value between x00 and xp1

  ! Original Author:
  !   Takanobu Yamaguchi
  !   tak.yamaguchi@noaa.gov
  !
  !   This version has been modified slightly for CLUBB's coding standards and
  !   adds the 3/2 from eqn 21. -dschanen 26 Oct 2011
  !   We have also added a quintic polynomial option.
  !
  ! References:
  !   M. Steffen, Astron. Astrophys. 239, 443-450 (1990)
  !-------------------------------------------------------------------------------------------------

      USE constants_clubb, ONLY: three_halves, eps

      USE clubb_precision, ONLY: core_rknd
    
      USE model_flags, ONLY: l_quintic_poly_interp

    implicit none

    ! Constant Parameters
    logical, parameter :: &
      l_equation_21 = .true.
      
    ! External
    intrinsic :: sign, abs, min

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: & 
      z_in ! The altitude to be interpolated to [m]

    ! k-levels;  their meaning depends on whether we're extrapolating or interpolating
    integer, intent(in) :: &
      km1, k00, kp1, kp2 

    real( kind = core_rknd ), intent(in) :: &
      zm1, z00, zp1, zp2, & ! The altitudes for km1, k00, kp1, kp2      [m]
      fm1, f00, fp1, fp2    ! The field at km1, k00, kp1, and kp2       [units vary]

    ! Output Variables
    real( kind = core_rknd ) :: f_out ! The interpolated field
   
    ! Local Variables 
    real( kind = core_rknd ) :: &
      hm1, h00, hp1, &
      sm1, s00, sp1, &
      p00, pp1, &
      dfdx00, dfdxp1, &
      c1, c2, c3, c4, &
      w00, wp1, &
      coef1, coef2, &
      zprime, beta, alpha, zn
   
    ! ---- Begin Code ---- 

    if ( l_equation_21 ) then
      ! Use the formula from Steffen (1990), which should make the interpolation
      ! less restrictive
      coef1 = three_halves
      coef2 = 1.0_core_rknd/three_halves
    else
      coef1 = 1.0_core_rknd
      coef2 = 1.0_core_rknd
    end if

    if ( km1 <= k00 ) then
      hm1 = z00 - zm1
      h00 = zp1 - z00
      hp1 = zp2 - zp1

      if ( km1 == k00 ) then
        s00 = ( fp1 - f00 ) / ( zp1 - z00 )
        sp1 = ( fp2 - fp1 ) / ( zp2 - zp1 )
        dfdx00 = s00
        pp1 = ( s00 * hp1 + sp1 * h00 ) / ( h00 + hp1 )
        dfdxp1 = coef1*( sign( 1.0_core_rknd, s00 ) + sign( 1.0_core_rknd, sp1 ) ) &
          * min( abs( s00 ), abs( sp1 ), coef2*0.5_core_rknd*abs( pp1 ) )

      else if ( kp1 == kp2 ) then
        sm1 = ( f00 - fm1 ) / ( z00 - zm1 )
        s00 = ( fp1 - f00 ) / ( zp1 - z00 )
        p00 = ( sm1 * h00 + s00 * hm1 ) / ( hm1 + h00 )
        dfdx00 = coef1*( sign( 1.0_core_rknd, sm1 ) + sign( 1.0_core_rknd, s00 ) ) &
          * min( abs( sm1 ), abs( s00 ), coef2*0.5_core_rknd*abs( p00 ) )
        dfdxp1 = s00

      else
        sm1 = ( f00 - fm1 ) / ( z00 - zm1 )
        s00 = ( fp1 - f00 ) / ( zp1 - z00 )
        sp1 = ( fp2 - fp1 ) / ( zp2 - zp1 )
        p00 = ( sm1 * h00 + s00 * hm1 ) / ( hm1 + h00 )
        pp1 = ( s00 * hp1 + sp1 * h00 ) / ( h00 + hp1 )
        dfdx00 = coef1*( sign( 1.0_core_rknd, sm1 ) + sign( 1.0_core_rknd, s00 ) ) &
          * min( abs( sm1 ), abs( s00 ), coef2*0.5_core_rknd*abs( p00 ) )
        dfdxp1 = coef1*( sign( 1.0_core_rknd, s00 ) + sign( 1.0_core_rknd, sp1 ) ) &
          * min( abs( s00 ), abs( sp1 ), coef2*0.5_core_rknd*abs( pp1 ) )

      end if

      c1 = ( dfdx00 + dfdxp1 - 2._core_rknd * s00 ) / ( h00 ** 2 )
      c2 = ( 3._core_rknd * s00 - 2._core_rknd * dfdx00 - dfdxp1 ) / h00
      c3 = dfdx00
      c4 = f00

      if ( .not. l_quintic_poly_interp ) then

        ! Old formula
        !f_out = c1 * ( (z_in - z00)**3 ) + c2 * ( (z_in - z00)**2 ) + c3 * (z_in - z00) + c4

        ! Faster nested multiplication
        zprime = z_in - z00
        f_out =  c4 + zprime*( c3 + zprime*( c2 + ( zprime*c1 ) ) )

      else 

       ! Use a quintic polynomial interpolation instead instead of the Steffen formula.
       ! Unlike the formula above, this formula does not guarantee monotonicity.

        beta = 120._core_rknd * ( (fp1-f00) - 0.5_core_rknd * h00 * (dfdx00 + dfdxp1) )

        ! Prevent an underflow by using a linear interpolation
        if ( abs( beta ) < eps ) then 
          f_out = lin_interpolate_two_points( z00, zp1, zm1, &
                           fp1, fm1 )

        else
          alpha = (6._core_rknd/beta) * h00 * (dfdxp1-dfdx00) + 0.5_core_rknd
          zn = (z_in-z00)/h00

          f_out = ( &
                  (( (beta/20._core_rknd)*zn - (beta*(1._core_rknd+alpha) &
                    / 12._core_rknd)) * zn + (beta*alpha/6._core_rknd)) &
                    * zn**2 + dfdx00*h00 &
                  ) * zn + f00
        end if ! beta < eps
      end if ! ~quintic_polynomial

    else
      ! Linear extrapolation
      wp1 = ( z_in - z00 ) / ( zp1 - z00 )
      w00 = 1._core_rknd - wp1
      f_out = wp1 * fp1 + w00 * f00

    end if

    return
  end function mono_cubic_interp

!-------------------------------------------------------------------------------

    ! Description:
    ! This subroutine performs a binary search to find the closest value greater
    ! than or equal to var in the array.  This function returns the index of the
    ! closest value of array that is greater than or equal to var.  It returns a
    ! value of -1 if var is outside the bounds of array.
    !
    !-----------------------------------------------------------------------



    ! Input Variables

    ! Size of the array

    ! The array being searched (must be sorted from least value to greatest
    ! value).

    ! The value being searched for

    ! Local Variables

    ! Has an index been found?

    ! Bounds of the search

    ! Initialize local variables


    ! The initial value of low has been changed from 1 to 2 due to a problem
    ! that was occuring when var was close to the lower bound.
    !
    ! The lowest value in the array (which is sorted by increasing values) is
    ! found at index 1, while the highest value in the array is found at
    ! index n.  Unless the value of var exactly corresponds with one of the
    ! values found in the array, or unless the value of var is found outside of
    ! the array, the value of var will be found between two levels of the array.
    ! In this scenario, the output of function binary_search is the index of the
    ! HIGHER level.  For example, if the value of var is found between array(1)
    ! and array(2), the output of function binary_search will be 2.
    !
    ! Therefore, the lowest index of a HIGHER level in an interpolation is 2.
    ! Thus, the initial value of low has been changed to 2.  This will prevent
    ! the value of variable "i" below from becoming 1.  If the value of "i"
    ! becomes 1, the code below tries to access array(0) (which is array(i-1)
    ! when i = 1) and produces an error.



    ! This line is here to avoid a false compiler warning about "i" being used
    ! uninitialized in this function.






        ! Special case where var falls exactly on the lowest value in the
        ! array, which is array(1).  This case is not covered by the statement
        ! above.
        ! The value of "i" must be set to 2 because an interpolation is
        ! performed in the subroutine that calls this function that uses
        ! indices "i" and "i-1".











!-------------------------------------------------------------------------------
! Description:
!   Do a linear interpolation in the vertical with pressures.  Assumes
!   values that are less than lowest source point are zero and above the
!   highest source point are zero. Also assumes altitude increases linearly.
!   This function just calls zlinterp_fnc, but negates grid_out and grid_src.

! References:
!   function LIN_INT from WRF-HOC
!-----------------------------------------------------------------------



    ! Input variables



    ! Output variable

    ! ---- Begin Code ----


!-------------------------------------------------------------------------------
! Description:
!   Do a linear interpolation in the vertical.  Assumes values that
!   are less than lowest source point are zero and above the highest
!   source point are zero. Also assumes altitude increases linearly.

! References:
!   function LIN_INT from WRF-HOC
!-----------------------------------------------------------------------



    ! Input variables



    ! Output variable

    ! Local variables

!   integer :: tst, kp1

    ! ---- Begin Code ----



      ! Set to 0 if we're below the input data's lowest point


      ! Increment k until the level is correct
!          do while ( grid_out(kint) > grid_src(k)
!     .                .and. k < dim_src )
!            k = k + 1
!          end do

      ! Changed so a binary search is used instead of a sequential search
!          tst = binary_search(dim_src, grid_src, grid_out(kint))
      ! Joshua Fasching April 2008

!          print *, "k = ", k
!          print *, "tst = ", tst
!          print *, "dim_src = ", dim_src
!          print *,"------------------------------"

      ! If the increment leads to a level above the data, set this
      ! point and all those above it to zero
      !if( k > dim_src ) then


      !kp1 = min( k+1, dim_src )

      ! Interpolate

!          ( var_src(k) - var_src(km1) ) / &
!          ( grid_src(k) - grid_src(km1) ) &
!            * ( grid_out(kint) - grid_src(km1) ) + var_src(km1) &
!            Changed to use a standard function for interpolation

      !! Note this ends up changing the results slightly because
      !the placement of variables has been changed.

!            Joshua Fasching April 2008



!-------------------------------------------------------------------------------
	     
    
    !------------------------------Arguments--------------------------------
    
    !---------------------------Local variables-----------------------------
    !-----------------------------------------------------------------
    !
    ! Initialize index array and logical flags
    !


    !
    ! Store level indices for interpolation.
    ! If all indices for this level have been found,
    ! do the interpolation
    !

    !
    ! If we've fallen through the k=1,nlev-1 loop, we cannot interpolate and
    ! must extrapolate from the bottom or top data level for at least some
    ! of the longitude points.
    !

	     

!-------------------------------------------------------------------------------

! Description:
!   Linear interpolation for 25 June 1996 altocumulus case.

!   For example, to interpolate between two temperatures in space, put
!   your spatial coordinates in x-list and your temperature values in
!   tlist.  The point in question should have its spatial value stored
!   in xvalue, and tvalue will be the temperature at that point.

! Author: Michael Falk for COAMPS.
!-------------------------------------------------------------------------------





    ! Input Variables

    ! Input/Output Variables



    ! Local variables
 

!-------------------------------------------------------------------------------
!
! Assure that the elements are in order so that the interpolation is between 
! the two closest points to the point in question.
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
! If the point in question is larger than the largest x-value or
! smaller than the smallest x-value, crash.
!
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------
!
! Find the correct top and bottom bounds, do the interpolation, return c
! the value.
!
!-------------------------------------------------------------------------------








end module interpolation