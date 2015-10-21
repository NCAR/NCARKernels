
! KGEN-generated Fortran source file
!
! Filename    : surface_varnce_module.F90
! Generated at: 2015-10-20 14:27:04
! KGEN version: 0.5.3



    MODULE surface_varnce_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE ! Default to private
        PUBLIC surface_varnce
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!=============================================================================

        SUBROUTINE surface_varnce(upwp_sfc, vpwp_sfc, wpthlp_sfc, wprtp_sfc, um_sfc, vm_sfc, lscale_up_sfc, wpsclrp_sfc, wp2_sfc, &
        up2_sfc, vp2_sfc, thlp2_sfc, rtp2_sfc, rtpthlp_sfc, err_code, sclrp2_sfc, sclrprtp_sfc, sclrpthlp_sfc)
! Description:
! This subroutine computes estimate of the surface thermodynamic and wind
! component second order moments.
! References:
! Andre, J. C., G. De Moor, P. Lacarrere, G. Therry, and R. Du Vachat, 1978.
!   Modeling the 24-Hour Evolution of the Mean and Turbulent Structures of
!   the Planetary Boundary Layer.  J. Atmos. Sci., 35, 1861 -- 1883.
!-----------------------------------------------------------------------
            USE parameters_model, ONLY: t0
! Variable(s)
            USE constants_clubb, ONLY: one
            USE constants_clubb, ONLY: one_fourth
            USE constants_clubb, ONLY: zero
            USE constants_clubb, ONLY: grav
            USE constants_clubb, ONLY: two_thirds
            USE constants_clubb, ONLY: four
            USE constants_clubb, ONLY: two
            USE constants_clubb, ONLY: one_third
            USE constants_clubb, ONLY: eps
            USE constants_clubb, ONLY: fstderr
! Variable(s)
            USE parameters_model, ONLY: sclr_dim
! Variable(s)
            USE numerical_check, ONLY: surface_varnce_check
! Procedure
            USE error_code, ONLY: clubb_no_error
            USE error_code, ONLY: clubb_at_least_debug_level
            USE error_code, ONLY: clubb_var_equals_nan
! Variable(s)
! Constant
            USE array_index, ONLY: iisclr_rt
            USE array_index, ONLY: iisclr_thl
! Index for a scalar emulating rt
! Index for a scalar emulating thetal
! Procedure(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC sqrt, max
! Constant Parameters
! Logical for Andre et al., 1978 parameterization.
            LOGICAL, parameter :: l_andre_1978 = .false.
            REAL(KIND=core_rknd), parameter :: ufmin = 0.01_core_rknd
            REAL(KIND=core_rknd), parameter :: z_const = one
            REAL(KIND=core_rknd), parameter :: reduce_coef   = 0.2_core_rknd
            REAL(KIND=core_rknd), parameter :: a_const = 1.8_core_rknd
            REAL(KIND=core_rknd), parameter :: sclr_var_coef = 0.4_core_rknd
! Defined height of 1 meter                [m]
! Vince Larson increased ufmin to stabilize arm_97.  24 Jul 2007
!      ufmin = 0.0001_core_rknd, &
! Minimum allowable value of u*   [m/s]
! End Vince Larson's change.
! Vince Larson changed in order to make correlations between [-1,1].  31 Jan 2008.
!      sclr_var_coef = 0.25_core_rknd, & ! This value is made up! - Vince Larson 12 Jul 2005
! This value is made up! - Vince Larson 12 Jul 2005
! End Vince Larson's change
! Vince Larson reduced surface spike in scalar variances associated
! w/ Andre et al. 1978 scheme
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: wprtp_sfc
            REAL(KIND=core_rknd), intent(in) :: vpwp_sfc
            REAL(KIND=core_rknd), intent(in) :: um_sfc
            REAL(KIND=core_rknd), intent(in) :: vm_sfc
            REAL(KIND=core_rknd), intent(in) :: upwp_sfc
            REAL(KIND=core_rknd), intent(in) :: lscale_up_sfc
            REAL(KIND=core_rknd), intent(in) :: wpthlp_sfc
! Surface u momentum flux, <u'w'>|_sfc   [m^2/s^2]
! Surface v momentum flux, <v'w'>|_sfc   [m^2/s^2]
! Surface thetal flux, <w'thl'>|_sfc     [K m/s]
! Surface moisture flux, <w'rt'>|_sfc    [kg/kg m/s]
! Surface u wind component, <u>          [m/s]
! Surface v wind component, <v>          [m/s]
! Upward component of Lscale at surface  [m]
            REAL(KIND=core_rknd), intent(in), dimension(sclr_dim) :: wpsclrp_sfc
! Passive scalar flux, <w'sclr'>|_sfc   [units m/s]
! Output Variables
            REAL(KIND=core_rknd), intent(out) :: rtpthlp_sfc
            REAL(KIND=core_rknd), intent(out) :: wp2_sfc
            REAL(KIND=core_rknd), intent(out) :: up2_sfc
            REAL(KIND=core_rknd), intent(out) :: vp2_sfc
            REAL(KIND=core_rknd), intent(out) :: rtp2_sfc
            REAL(KIND=core_rknd), intent(out) :: thlp2_sfc
! Surface variance of w, <w'^2>|_sfc            [m^2/s^2]
! Surface variance of u, <u'^2>|_sfc            [m^2/s^2]
! Surface variance of v, <v'^2>|_sfc            [m^2/s^2]
! Surface variance of theta-l, <thl'^2>|_sfc    [K^2]
! Surface variance of rt, <rt'^2>|_sfc          [(kg/kg)^2]
! Surface covariance of rt and theta-l          [kg K/kg]
            INTEGER, intent(out) :: err_code
! Error code
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: sclrprtp_sfc
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: sclrpthlp_sfc
            REAL(KIND=core_rknd), intent(out), dimension(sclr_dim) :: sclrp2_sfc
! Surface variance of passive scalar            [units^2]
! Surface covariance of pssv scalar and rt  [units kg/kg]
! Surface covariance of pssv scalar and theta-l [units K]
! Local Variables
            REAL(KIND=core_rknd) :: wstar
            REAL(KIND=core_rknd) :: ustar2
            REAL(KIND=core_rknd) :: uf
! Square of surface friction velocity, u*       [m^2/s^2]
! Convective velocity, w*                       [m/s]
! Surface friction vel., u*, in older version   [m/s]
! Variables for Andre et al., 1978 parameterization.
            REAL(KIND=core_rknd) :: um_sfc_sqd
            REAL(KIND=core_rknd) :: vm_sfc_sqd
            REAL(KIND=core_rknd) :: usp2_sfc
            REAL(KIND=core_rknd) :: vsp2_sfc
! Surface value of <u>^2                           [m^2/s^2]
! Surface value of <v>^2                           [m^2/s^2]
! u_s (vector oriented w/ mean sfc. wind) variance [m^2/s^2]
! v_s (vector perpen. to mean sfc. wind) variance  [m^2/s^2]
            REAL(KIND=core_rknd) :: ustar
            REAL(KIND=core_rknd) :: lngth
            REAL(KIND=core_rknd) :: zeta
! Surface friction velocity, u*                             [m/s]
! Monin-Obukhov length                                      [m]
! Dimensionless height z_const/Lngth, where z_const = 1 m.  [-]
            INTEGER :: i ! Loop index
    err_code = clubb_no_error
    if ( l_andre_1978 ) then
! Calculate <u>^2 and <v>^2.
       um_sfc_sqd = um_sfc**2
       vm_sfc_sqd = vm_sfc**2
! Calculate surface friction velocity, u*.
       ustar = max( ( upwp_sfc**2 + vpwp_sfc**2 )**(one_fourth), ufmin )
       if ( wpthlp_sfc /= zero ) then
! Find Monin-Obukhov Length (Andre et al., 1978, p. 1866).
          Lngth = - ( ustar**3 ) &
                    / ( 0.35_core_rknd * (one/T0) * grav * wpthlp_sfc )
! Find the value of dimensionless height zeta
! (Andre et al., 1978, p. 1866).
          zeta = z_const / Lngth
       else ! wpthlp_sfc = 0 ! wpthlp_sfc = 0
! The value of Monin-Obukhov length is +inf when ustar < 0 and -inf
! when ustar > 0.  Either way, zeta = 0.
          zeta = zero
       endif ! wpthlp_sfc /= 0 ! wpthlp_sfc /= 0
! Andre et al, 1978, Eq. 29.
! Notes:  1) "reduce_coef" is a reduction coefficient intended to make
!            the values of rtp2, thlp2, and rtpthlp smaller at the
!            surface.
!         2) With the reduction coefficient having a value of 0.2, the
!            surface correlations of both w & rt and w & thl have a value
!            of about 0.845.  These correlations are greater if zeta < 0.
!            The correlations have a value greater than 1 if
!            zeta <= -0.212.
!         3) The surface correlation of rt & thl is 1.
! Brian Griffin; February 2, 2008.
       if ( zeta < zero ) then
          thlp2_sfc   = reduce_coef  & 
                        * ( wpthlp_sfc**2 / ustar**2 ) & 
                        * four * ( one - 8.3_core_rknd * zeta )**(-two_thirds)
          rtp2_sfc    = reduce_coef  & 
                        * ( wprtp_sfc**2 / ustar**2 ) & 
                        * four * ( one - 8.3_core_rknd * zeta )**(-two_thirds)
          rtpthlp_sfc = reduce_coef  & 
                        * ( wprtp_sfc * wpthlp_sfc / ustar**2 ) & 
                        * four * ( one - 8.3_core_rknd * zeta )**(-two_thirds)
          wp2_sfc     = ( ustar**2 ) & 
                        * ( 1.75_core_rknd + two * (-zeta)**(two_thirds) )
       else
          thlp2_sfc   = reduce_coef  & 
                        * four * ( wpthlp_sfc**2 / ustar**2 )
          rtp2_sfc    = reduce_coef  & 
                        * four * ( wprtp_sfc**2 / ustar**2 )
          rtpthlp_sfc = reduce_coef  & 
                        * four * ( wprtp_sfc * wpthlp_sfc / ustar**2 )
          wp2_sfc     = 1.75_core_rknd * ustar**2
       endif
! Calculate wstar following Andre et al., 1978, p. 1866.
! w* = ( ( 1 / T0 ) * g * <w'thl'>|_sfc * z_i )^(1/3);
! where z_i is the height of the mixed layer.  The value of CLUBB's
! upward component of mixing length, Lscale_up, at the surface will be
! used as z_i.
       wstar = ( (one/T0) * grav * wpthlp_sfc * Lscale_up_sfc )**(one_third)
! Andre et al., 1978, Eq. 29.
! Andre et al. (1978) defines horizontal wind surface variances in terms
! of orientation with the mean surface wind.  The vector u_s is the wind
! vector oriented with the mean surface wind.  The vector v_s is the wind
! vector oriented perpendicular to the mean surface wind.  Thus, <u_s> is
! equal to the mean surface wind (both in speed and direction), and <v_s>
! is 0.  Equation 29 gives the formula for the variance of u_s, which is
! <u_s'^2> (usp2_sfc in the code), and the formula for the variance of
! v_s, which is <v_s'^2> (vsp2_sfc in the code).
       if ( wpthlp_sfc > zero ) then
          usp2_sfc = four * ustar**2 + 0.3_core_rknd * wstar**2
          vsp2_sfc = 1.75_core_rknd * ustar**2 + 0.3_core_rknd * wstar**2
       else
          usp2_sfc = four * ustar**2
          vsp2_sfc = 1.75_core_rknd * ustar**2
       endif
! Variance of u, <u'^2>, at the surface can be found from <u_s'^2>,
! <v_s'^2>, and mean winds (at the surface) <u> and <v>, such that:
!    <u'^2>|_sfc = <u_s'^2> * [ <u>^2 / ( <u>^2 + <v>^2 ) ]
!                  + <v_s'^2> * [ <v>^2 / ( <u>^2 + <v>^2 ) ];
! where <u>^2 + <v>^2 /= 0.
       up2_sfc  &
       = usp2_sfc * ( um_sfc_sqd / max( um_sfc_sqd + vm_sfc_sqd , eps ) )  &
         + vsp2_sfc * ( vm_sfc_sqd / max( um_sfc_sqd + vm_sfc_sqd , eps ) )
! Variance of v, <v'^2>, at the surface can be found from <u_s'^2>,
! <v_s'^2>, and mean winds (at the surface) <u> and <v>, such that:
!    <v'^2>|_sfc = <v_s'^2> * [ <u>^2 / ( <u>^2 + <v>^2 ) ]
!                  + <u_s'^2> * [ <v>^2 / ( <u>^2 + <v>^2 ) ];
! where <u>^2 + <v>^2 /= 0.
       vp2_sfc  &
       = vsp2_sfc * ( um_sfc_sqd / max( um_sfc_sqd + vm_sfc_sqd , eps ) )  &
         + usp2_sfc * ( vm_sfc_sqd / max( um_sfc_sqd + vm_sfc_sqd , eps ) )
! Passive scalars
       if ( sclr_dim > 0 ) then
          do i = 1, sclr_dim
! Notes:  1) "reduce_coef" is a reduction coefficient intended to
!            make the values of sclrprtp, sclrpthlp, and sclrp2
!            smaller at the surface.
!         2) With the reduction coefficient having a value of 0.2,
!            the surface correlation of w & sclr has a value of
!            about 0.845.  The correlation is greater if zeta < 0.
!            The correlation has a value greater than 1 if
!            zeta <= -0.212.
!         3) The surface correlations of both rt & sclr and
!            thl & sclr are 1.
! Brian Griffin; February 2, 2008.
             if ( zeta < zero ) then
                sclrprtp_sfc(i)  & 
                = reduce_coef  & 
                  * ( wpsclrp_sfc(i) * wprtp_sfc / ustar**2 ) & 
                  * four * ( one - 8.3_core_rknd * zeta )**(-two_thirds)
                sclrpthlp_sfc(i)  & 
                = reduce_coef  & 
                  * ( wpsclrp_sfc(i) * wpthlp_sfc / ustar**2 ) & 
                  * four * ( one - 8.3_core_rknd * zeta )**(-two_thirds)
                sclrp2_sfc(i)  & 
                = reduce_coef   & 
                  * ( wpsclrp_sfc(i)**2 / ustar**2 ) & 
                  * four * ( one - 8.3_core_rknd * zeta )**(-two_thirds)
             else
                sclrprtp_sfc(i)  & 
                = reduce_coef  & 
                  * four * ( wpsclrp_sfc(i) * wprtp_sfc / ustar**2 )
                sclrpthlp_sfc(i)  & 
                = reduce_coef  & 
                  * four * ( wpsclrp_sfc(i) * wpthlp_sfc / ustar**2 )
                sclrp2_sfc(i)  & 
                = reduce_coef & 
                  * four * ( wpsclrp_sfc(i)**2 / ustar**2 )
             endif
          enddo ! i = 1, sclr_dim ! i = 1, sclr_dim
       endif
    else ! Previous code. ! Previous code.
! Compute ustar^2
       ustar2 = sqrt( upwp_sfc * upwp_sfc + vpwp_sfc * vpwp_sfc )
! Compute wstar following Andre et al., 1976
       if ( wpthlp_sfc > zero ) then
          wstar = ( one/T0 * grav * wpthlp_sfc * z_const )**(one_third)
       else
          wstar = zero
       endif
! Surface friction velocity following Andre et al. 1978
       uf = sqrt( ustar2 + 0.3_core_rknd * wstar * wstar )
       uf = max( ufmin, uf )
! Compute estimate for surface second order moments
       wp2_sfc = a_const * uf**2
       up2_sfc = 2.0_core_rknd * a_const * uf**2  ! From Andre, et al. 1978 ! From Andre, et al. 1978
       vp2_sfc = 2.0_core_rknd * a_const * uf**2  ! "  " ! "  "
! Vince Larson changed to make correlations between [-1,1]  31 Jan 2008
!        thlp2_sfc   = 0.1 * a * ( wpthlp_sfc / uf )**2
!        rtp2_sfc    = 0.4 * a * ( wprtp_sfc / uf )**2
!        rtpthlp_sfc = a * ( wpthlp_sfc / uf ) * ( wprtp_sfc / uf )
! Notes:  1) With "a" having a value of 1.8, the surface correlations of
!            both w & rt and w & thl have a value of about 0.878.
!         2) The surface correlation of rt & thl is 0.5.
! Brian Griffin; February 2, 2008.
       thlp2_sfc = 0.4_core_rknd * a_const * ( wpthlp_sfc / uf )**2
       rtp2_sfc = 0.4_core_rknd * a_const * ( wprtp_sfc / uf )**2
       rtpthlp_sfc = 0.2_core_rknd * a_const &
                     * ( wpthlp_sfc / uf ) * ( wprtp_sfc / uf )
! End Vince Larson's change.
! Passive scalars
       if ( sclr_dim > 0 ) then
          do i = 1, sclr_dim
! Vince Larson changed coeffs to make correlations between [-1,1].
! 31 Jan 2008
!             sclrprtp_sfc(i) &
!             = a * (wprtp_sfc / uf) * (wpsclrp_sfc(i) / uf)
!             sclrpthlp_sfc(i) &
!             = a * (wpthlp_sfc / uf) * (wpsclrp_sfc(i) / uf)
!             sclrp2_sfc(i) &
!             = sclr_var_coef * a * ( wpsclrp_sfc(i) / uf )**2
! Notes:  1) With "a" having a value of 1.8 and "sclr_var_coef"
!            having a value of 0.4, the surface correlation of
!            w & sclr has a value of about 0.878.
!         2) With "sclr_var_coef" having a value of 0.4, the
!            surface correlations of both rt & sclr and
!            thl & sclr are 0.5.
! Brian Griffin; February 2, 2008.
! We use the following if..then's to make sclr_rt and sclr_thl
! close to the actual thlp2/rtp2 at the surface.
! -dschanen 25 Sep 08
             if ( i == iisclr_rt ) then
! If we are trying to emulate rt with the scalar, then we
! use the variance coefficient from above
                sclrprtp_sfc(i) = 0.4_core_rknd * a_const &
                                  * ( wprtp_sfc / uf ) * ( wpsclrp_sfc(i) / uf )
             else
                sclrprtp_sfc(i) = 0.2_core_rknd * a_const &
                                  * ( wprtp_sfc / uf ) * ( wpsclrp_sfc(i) / uf )
             endif
             if ( i == iisclr_thl ) then
! As above, but for thetal
                sclrpthlp_sfc(i) = 0.4_core_rknd * a_const &
                                   * ( wpthlp_sfc / uf ) &
                                   * ( wpsclrp_sfc(i) / uf )
             else
                sclrpthlp_sfc(i) = 0.2_core_rknd * a_const &
                                   * ( wpthlp_sfc / uf ) &
                                   * ( wpsclrp_sfc(i) / uf )
             endif
             sclrp2_sfc(i) = sclr_var_coef * a_const &
                             * ( wpsclrp_sfc(i) / uf )**2
! End Vince Larson's change.
          enddo ! 1,...sclr_dim ! 1,...sclr_dim
       endif ! sclr_dim > 0 ! sclr_dim > 0
    endif ! l_andre_1978 ! l_andre_1978
    if ( clubb_at_least_debug_level( 2 ) ) then
       call surface_varnce_check( wp2_sfc, up2_sfc, vp2_sfc,  & 
                                  thlp2_sfc, rtp2_sfc, rtpthlp_sfc, & 
                                  sclrp2_sfc, sclrprtp_sfc, sclrpthlp_sfc, &
                                  err_code )
!       Error reporting
!       Joshua Fasching February 2008
       if ( err_code == clubb_var_equals_NaN ) then
          write(fstderr,*) "Error in surface_varnce"
          write(fstderr,*) "Intent(in)"
          write(fstderr,*) "upwp_sfc = ", upwp_sfc
          write(fstderr,*) "vpwp_sfc = ", vpwp_sfc
          write(fstderr,*) "wpthlp_sfc = ", wpthlp_sfc
          write(fstderr,*) "wprtp_sfc = ", wprtp_sfc
          if ( sclr_dim > 0 ) then
             write(fstderr,*) "wpsclrp_sfc = ", wpsclrp_sfc
          endif
          write(fstderr,*) "Intent(out)"
          write(fstderr,*) "wp2_sfc = ", wp2_sfc
          write(fstderr,*) "up2_sfc = ", up2_sfc
          write(fstderr,*) "vp2_sfc = ", vp2_sfc
          write(fstderr,*) "thlp2_sfc = ", thlp2_sfc
          write(fstderr,*) "rtp2_sfc = ", rtp2_sfc
          write(fstderr,*) "rtpthlp_sfc = ", rtpthlp_sfc
          if ( sclr_dim > 0 ) then
             write(fstderr,*) "sclrp2_sfc = ", sclrp2_sfc
             write(fstderr,*) "sclrprtp_sfc = ", sclrprtp_sfc
             write(fstderr,*) "sclrpthlp_sfc = ", sclrpthlp_sfc
          endif
       endif ! err_code == clubb_var_equals_NaN ! err_code == clubb_var_equals_NaN
    endif ! clubb_at_least_debug_level ( 2 ) ! clubb_at_least_debug_level ( 2 )
    return
        END SUBROUTINE surface_varnce
!===============================================================================
    END MODULE surface_varnce_module
