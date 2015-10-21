
! KGEN-generated Fortran source file
!
! Filename    : interpolation.F90
! Generated at: 2015-10-20 14:27:04
! KGEN version: 0.5.3



    MODULE interpolation
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
        PRIVATE ! Default Scope
        PUBLIC linear_interp_factor, mono_cubic_interp, lin_interpolate_two_points, binary_search, pvertinterp
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!-------------------------------------------------------------------------------

        FUNCTION lin_interpolate_two_points(height_int, height_high, height_low, var_high, var_low)
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
! Variable(s)
            USE constants_clubb, ONLY: fstderr ! Constant
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: height_int
            REAL(KIND=core_rknd), intent(in) :: height_high
            REAL(KIND=core_rknd), intent(in) :: var_low
            REAL(KIND=core_rknd), intent(in) :: var_high
            REAL(KIND=core_rknd), intent(in) :: height_low
! Height to be interpolated to     [m]
! Height above the interpolation   [m]
! Height below the interpolation   [m]
! Variable above the interpolation [units vary]
! Variable below the interpolation [units vary]
! Output Variables
            REAL(KIND=core_rknd) :: lin_interpolate_two_points
! Check for valid input
    if ( abs(height_low - height_high) < 1.0e-12_core_rknd ) then
      write(fstderr,*) "lin_interpolate_two_points: height_high and height_low cannot be equal."
      stop
    end if
! Compute linear interpolation
    lin_interpolate_two_points = ( ( height_int - height_low )/( height_high - height_low ) ) &
      * ( var_high - var_low ) + var_low
    return
        END FUNCTION lin_interpolate_two_points
!-------------------------------------------------------------------------------------------------

        elemental real(kind = core_rknd) FUNCTION linear_interp_factor(factor, var_high, var_low)
! Description:
!   Determines the coefficient for a linear interpolation
!
! References:
!   None
!-------------------------------------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
            REAL(KIND=core_rknd), intent(in) :: factor
            REAL(KIND=core_rknd), intent(in) :: var_high
            REAL(KIND=core_rknd), intent(in) :: var_low
! Factor                           [units vary]
! Variable above the interpolation [units vary]
! Variable below the interpolation [units vary]
    linear_interp_factor = factor * ( var_high - var_low ) + var_low
    return
        END FUNCTION linear_interp_factor
!-------------------------------------------------------------------------------------------------

        FUNCTION mono_cubic_interp(z_in, km1, k00, kp1, kp2, zm1, z00, zp1, zp2, fm1, f00, fp1, fp2) RESULT ( f_out )
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
            USE constants_clubb, ONLY: three_halves
            USE constants_clubb, ONLY: eps
! Constant(s)
            USE clubb_precision, ONLY: core_rknd
! Constant
            USE model_flags, ONLY: l_quintic_poly_interp
! Variable(s)
            IMPLICIT NONE
! Constant Parameters
            LOGICAL, parameter :: l_equation_21 = .true.
! External
            INTRINSIC sign, abs, min
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: z_in
! The altitude to be interpolated to [m]
! k-levels;  their meaning depends on whether we're extrapolating or interpolating
            INTEGER, intent(in) :: km1
            INTEGER, intent(in) :: k00
            INTEGER, intent(in) :: kp1
            INTEGER, intent(in) :: kp2
            REAL(KIND=core_rknd), intent(in) :: zp2
            REAL(KIND=core_rknd), intent(in) :: fp2
            REAL(KIND=core_rknd), intent(in) :: z00
            REAL(KIND=core_rknd), intent(in) :: fm1
            REAL(KIND=core_rknd), intent(in) :: f00
            REAL(KIND=core_rknd), intent(in) :: zp1
            REAL(KIND=core_rknd), intent(in) :: fp1
            REAL(KIND=core_rknd), intent(in) :: zm1
! The altitudes for km1, k00, kp1, kp2      [m]
! The field at km1, k00, kp1, and kp2       [units vary]
! Output Variables
            REAL(KIND=core_rknd) :: f_out ! The interpolated field
! Local Variables
            REAL(KIND=core_rknd) :: coef1
            REAL(KIND=core_rknd) :: coef2
            REAL(KIND=core_rknd) :: hm1
            REAL(KIND=core_rknd) :: h00
            REAL(KIND=core_rknd) :: hp1
            REAL(KIND=core_rknd) :: s00
            REAL(KIND=core_rknd) :: sp1
            REAL(KIND=core_rknd) :: dfdx00
            REAL(KIND=core_rknd) :: pp1
            REAL(KIND=core_rknd) :: dfdxp1
            REAL(KIND=core_rknd) :: sm1
            REAL(KIND=core_rknd) :: p00
            REAL(KIND=core_rknd) :: c1
            REAL(KIND=core_rknd) :: c2
            REAL(KIND=core_rknd) :: c3
            REAL(KIND=core_rknd) :: c4
            REAL(KIND=core_rknd) :: zprime
            REAL(KIND=core_rknd) :: beta
            REAL(KIND=core_rknd) :: alpha
            REAL(KIND=core_rknd) :: zn
            REAL(KIND=core_rknd) :: wp1
            REAL(KIND=core_rknd) :: w00
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
        end if ! beta < eps ! beta < eps
      end if ! ~quintic_polynomial ! ~quintic_polynomial
    else
! Linear extrapolation
      wp1 = ( z_in - z00 ) / ( zp1 - z00 )
      w00 = 1._core_rknd - wp1
      f_out = wp1 * fp1 + w00 * f00
    end if
    return
        END FUNCTION mono_cubic_interp
!-------------------------------------------------------------------------------

        pure integer FUNCTION binary_search(n, array, var) RESULT ( i )
! Description:
! This subroutine performs a binary search to find the closest value greater
! than or equal to var in the array.  This function returns the index of the
! closest value of array that is greater than or equal to var.  It returns a
! value of -1 if var is outside the bounds of array.
!
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
! Size of the array
            INTEGER, intent(in) :: n
! The array being searched (must be sorted from least value to greatest
! value).
            REAL(KIND=core_rknd), dimension(n), intent(in) :: array
! The value being searched for
            REAL(KIND=core_rknd), intent(in) :: var
! Local Variables
! Has an index been found?
            LOGICAL :: l_found
! Bounds of the search
            INTEGER :: high
            INTEGER :: low
! Initialize local variables
    l_found = .false.
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
    low = 2
    high = n
! This line is here to avoid a false compiler warning about "i" being used
! uninitialized in this function.
    i = (low + high) / 2
    do while( .not. l_found .and. low <= high )
      i = (low + high) / 2
      if ( var > array( i - 1 ) .and. var <= array( i ) ) then
        l_found = .true.
      elseif ( var == array(1) ) then
! Special case where var falls exactly on the lowest value in the
! array, which is array(1).  This case is not covered by the statement
! above.
        l_found = .true.
! The value of "i" must be set to 2 because an interpolation is
! performed in the subroutine that calls this function that uses
! indices "i" and "i-1".
        i = 2
      elseif ( var < array( i ) ) then
        high = i - 1
      elseif ( var > array( i ) ) then
        low = i + 1
      endif
    enddo  ! while ( ~l_found & low <= high ) ! while ( ~l_found & low <= high )
    if ( .not. l_found ) i = -1
    return
        END FUNCTION binary_search
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

        SUBROUTINE pvertinterp(nlev, pmid, pout, arrin, arrout)
            IMPLICIT NONE
!------------------------------Arguments--------------------------------
            INTEGER, intent(in) :: nlev ! vertical dimension
            REAL(KIND=core_rknd), intent(in) :: pmid(nlev) ! input level pressure levels
            REAL(KIND=core_rknd), intent(in) :: pout ! output pressure level
            REAL(KIND=core_rknd), intent(in) :: arrin(nlev) ! input  array
            REAL(KIND=core_rknd), intent(out) :: arrout ! output array (interpolated)
!---------------------------Local variables-----------------------------
            INTEGER :: k ! indices
            INTEGER :: kupper ! Level indices for interpolation
            REAL(KIND=core_rknd) :: dpu ! upper level pressure difference
            REAL(KIND=core_rknd) :: dpl ! lower level pressure difference
            LOGICAL :: found ! true if input levels found
            LOGICAL :: error ! true if error
!-----------------------------------------------------------------
!
! Initialize index array and logical flags
!
    found = .false.
    kupper = 1
    error = .false.
!
! Store level indices for interpolation.
! If all indices for this level have been found,
! do the interpolation
!
    do k=1,nlev-1
      if ((.not. found) .and. pmid(k)>pout .and. pout>=pmid(k+1)) then
        found = .true.
        kupper = k
      end if
    end do
!
! If we've fallen through the k=1,nlev-1 loop, we cannot interpolate and
! must extrapolate from the bottom or top data level for at least some
! of the longitude points.
!
    if (pout >= pmid(1)) then
      arrout = arrin(1)
    else if (pout <= pmid(nlev)) then
      arrout = arrin(nlev)
    else if (found) then
      dpu = pmid(kupper) - pout
      dpl = pout - pmid(kupper+1)
      arrout = (arrin(kupper)*dpl + arrin(kupper+1)*dpu)/(dpl + dpu)
    else
      error = .true.
    end if   
    return
        END SUBROUTINE pvertinterp
!-------------------------------------------------------------------------------

    END MODULE interpolation
