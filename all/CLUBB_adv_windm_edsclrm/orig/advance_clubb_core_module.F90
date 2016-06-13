!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:54:29
!KGEN version : 0.6.1

!-----------------------------------------------------------------------
! $Id: advance_clubb_core_module.F90 7416 2014-12-04 20:16:51Z schemena@uwm.edu $
!-----------------------------------------------------------------------
module advance_clubb_core_module

! Description:
!   The module containing the `core' of the CLUBB parameterization.
!   A host model implementing CLUBB should only require this subroutine
!   and the functions and subroutines it calls.
!
! References:
!  ``A PDF-Based Model for Boundary Layer Clouds. Part I:
!    Method and Model Description'' Golaz, et al. (2002)
!    JAS, Vol. 59, pp. 3540--3551.
!
!                         Copyright Notice:
!
!   This code and the source code it references are (C) 2006-2014
!   Jean-Christophe Golaz, Vincent E. Larson, Brian M. Griffin,
!   David P. Schanen, Adam J. Smith, and Michael J. Falk.
!
!   The distribution of this code and derived works thereof
!                   should include this notice.
!
!   Portions of this code derived from other sources (Hugh Morrison,
!   ACM TOMS, Numerical Recipes, et cetera) are the intellectual
!   property of their respective authors as noted and are also subject
!   to copyright.
!-----------------------------------------------------------------------

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PUBLIC advance_clubb_core

    PRIVATE

  contains

  !-----------------------------------------------------------------------

  !#######################################################################
  !#######################################################################
  ! If you change the argument list of advance_clubb_core you also have to
  ! change the calls to this function in the host models 1, WRF, SAM
  ! and GFDL.
  !#######################################################################
  !#######################################################################
  SUBROUTINE advance_clubb_core(kgen_unit, kgen_total_time, l_implemented, dt, fcor, wm_zt, um_forcing, vm_forcing, rho_ds_zm, invrs_rho_ds_zt, edsclrm_forcing, wp2, up2, vp2, um, vm, upwp, vpwp, edsclrm, err_code)

    ! Description:
    !   Subroutine to advance the model one timestep

    ! References:
    !   ``A PDF-Based Model for Boundary Layer Clouds. Part I:
    !     Method and Model Description'' Golaz, et al. (2002)
    !   JAS, Vol. 59, pp. 3540--3551.
    !-----------------------------------------------------------------------

    ! Modules to be included



      USE parameters_model, ONLY: edsclr_dim


      USE grid_class, ONLY: gr



      USE variables_diagnostic_module, ONLY: vg, ug, um_ref, vm_ref

      USE variables_diagnostic_module, ONLY: wpedsclrp










      ! Variable(s) 

      ! Variable(s) 


      ! Procedure 


      USE advance_windm_edsclrm_module, ONLY: advance_windm_edsclrm

      ! Procedure


      USE clubb_precision, ONLY: core_rknd




      ! Read values from namelist











      

      USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
      USE error_code, ONLY: kr_externs_out_error_code
      USE stats_variables, ONLY: kr_externs_out_stats_variables
      USE sponge_layer_damping, ONLY: kr_externs_out_sponge_layer_damping
      USE variables_diagnostic_module, ONLY: kr_externs_out_variables_diagnostic_module
      USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
      USE stats_variables, ONLY: kv_externs_stats_variables
      USE variables_diagnostic_module, ONLY: kv_externs_variables_diagnostic_module
      USE kgen_utils_mod, ONLY: kgen_perturb_real
      IMPLICIT NONE

    !!! External

    ! Constant Parameters
    ! is true, compute_length is called two additional times with
    ! perturbed values of rtm and thlm.  An average value of Lscale
    ! from the three calls to compute_length is then calculated.
    ! This reduces temporal noise in RICO, BOMEX, LBA, and other cases.

                                        ! compute the perturbed values
					
      



                                          ! Only has meaning if l_refined_grid_in_cloud is .true.

                                   ! (zero for liquid)

    !!! Input Variables
      LOGICAL, INTENT(INOUT) :: l_implemented

      REAL(KIND=core_rknd), INTENT(INOUT) :: dt

      REAL(KIND=core_rknd), INTENT(INOUT) :: fcor


    ! Input Variables
      REAL(KIND=core_rknd), dimension(gr%nz), INTENT(INOUT) :: um_forcing, vm_forcing, wm_zt, rho_ds_zm, invrs_rho_ds_zt










    ! Passive scalar variables


    ! Eddy passive scalar variables
      REAL(KIND=core_rknd), dimension(gr%nz,edsclr_dim), INTENT(INOUT) :: edsclrm_forcing


    ! Host model horizontal grid spacing, if part of host model.

    !!! Input/Output Variables
    ! These are prognostic or are planned to be in the future
      REAL(KIND=core_rknd), dimension(gr%nz), INTENT(INOUT) :: um, upwp, vm, vpwp, up2, vp2, wp2

    ! Passive scalar variables






    ! Eddy passive scalar variable
      REAL(KIND=core_rknd), dimension(gr%nz,edsclr_dim), INTENT(INOUT) :: edsclrm

    ! Variables that need to be output for use in other parts of the CLUBB
    ! code, such as microphysics (rcm, pdf_params), forcings (rcm), and/or
    ! BUGSrad (cloud_cover).


    ! Variables that need to be output for use in host models

    ! Eric Raut declared this variable solely for output to disk







      REAL(KIND=core_rknd), dimension(gr%nz) :: km_zm


    !!! Output Variable
    ! Diagnostic, for if some calculation goes amiss.
      INTEGER, INTENT(INOUT) :: err_code


    !!! Local Variables

    !Lscale_weight Uncomment this if you need to use this vairable at some point.

    ! For pdf_closure




    ! These local variables are declared because they originally belong on the momentum
    ! grid levels, but pdf_closure outputs them on the thermodynamic grid levels.





    !The following variables are defined for use when l_use_ice_latent = .true.






    


    !----- Begin Code -----

    ! Determine the maximum allowable value for Lscale (in meters).
      INTEGER, INTENT(IN) :: kgen_unit
      REAL(KIND=kgen_dp), INTENT(INOUT) :: kgen_total_time
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      REAL(KIND=core_rknd), dimension(gr%nz) :: kgenref_um, kgenref_upwp, kgenref_vm, kgenref_vpwp
      REAL(KIND=core_rknd), dimension(gr%nz,edsclr_dim) :: kgenref_edsclrm
      INTEGER :: kgenref_err_code
      TYPE(check_t) :: check_status
      INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock
      INTEGER, PARAMETER :: kgen_maxiter = 1
      REAL(KIND=kgen_dp) :: kgen_elapsed_time

      ! Spurious source will only be calculated if rtm_ma and thlm_ma are zero.
      ! Therefore, wm must be zero or l_implemented must be true.
        ! Get the vertical integral of rtm and thlm before this function begins
        ! so that spurious source can be calculated


    !----------------------------------------------------------------
    ! Test input variables
    !----------------------------------------------------------------

    !-----------------------------------------------------------------------



    ! Set up budget stats variables.




    ! SET SURFACE VALUES OF FLUXES (BROUGHT IN)
    ! We only do this for host models that do not apply the flux
    ! elsewhere in the code (e.g. WRF).  In other cases the _sfc variables will
    ! only be used to compute the variance at the surface. -dschanen 8 Sept 2009


      ! Set fluxes for passive scalars (if enabled)






      ! Set fluxes for passive scalars (if enabled)




    





    !---------------------------------------------------------------------------
    ! Interpolate wp3 to momentum levels, and wp2 to thermodynamic levels
    ! and then compute Skw for m & t grid
    !---------------------------------------------------------------------------





    ! The right hand side of this conjunction is only for reducing cpu time,
    ! since the more complicated formula is mathematically equivalent
      !----------------------------------------------------------------
      ! Compute gamma as a function of Skw  - 14 April 06 dschanen
      !----------------------------------------------------------------





    ! Compute sigma_sqd_w (dimensionless PDF width parameter)



    ! Smooth in the vertical using interpolation

    ! Interpolate the the stats_zt grid

    ! Compute the a3 coefficient (formula 25 in `Equations for CLUBB')
!   a3_coef = 3.0_core_rknd * sigma_sqd_w*sigma_sqd_w  &
!      + 6.0_core_rknd*(1.0_core_rknd-sigma_sqd_w)*sigma_sqd_w  &
!      + (1.0_core_rknd-sigma_sqd_w)*(1.0_core_rknd-sigma_sqd_w) &
!      - 3.0_core_rknd

    ! This is a simplified version of the formula above.

    ! We found we obtain fewer spikes in wp3 when we clip a3 to be no greater
    ! than -1.4 -dschanen 4 Jan 2011


    !---------------------------------------------------------------------------
    ! Interpolate thlp2, rtp2, and rtpthlp to thermodynamic levels,
    !---------------------------------------------------------------------------

    ! Interpolate variances to the stats_zt grid (statistics and closure)

    ! Compute skewness velocity for stats output purposes


    ! Compute wp3 / wp2 on zt levels.  Always use the interpolated value in the
    ! denominator since it's less likely to create spikes

    ! Clip wp3_on_wp2_zt if it's too large


    ! Compute wp3_on_wp2 by interpolating wp3_on_wp2_zt

    ! Smooth again as above

    !----------------------------------------------------------------
    ! Call closure scheme
    !----------------------------------------------------------------

    ! Put passive scalar input on the t grid for the PDF


    ! Interpolate hydrometeor mixed moments to momentum levels.





      ! Subroutine may produce NaN values, and if so, exit
      ! gracefully.
      ! Joshua Fasching March 2008







      ! Compute cloud_frac and rcm on a refined grid to improve parameterization
      ! of subgrid clouds


          ! Recalculate cloud_frac and r_c for each PDF component





            ! I commented out the lines that modify the values in pdf_params, as it seems that
            ! these values need to remain consistent with the rest of the PDF.
            ! Eric Raut Jun 2014
            ! Replace pdf_closure estimates with refined estimates
            ! pdf_params(k)%rc_1 = rc_1_refined
            ! pdf_params(k)%rc_2 = rc_2_refined

            ! pdf_params(k)%cloud_frac_1 = cloud_frac_1_refined
            ! pdf_params(k)%cloud_frac_2 = cloud_frac_2_refined

          ! Set these equal to the non-refined values so we have something to
          ! output to stats!

        ! Stats output




      ! Nudge rtm to prevent excessive drying



      ! Call pdf_closure a second time on momentum levels, to
      ! output (rather than interpolate) the variables which
      ! belong on the momentum levels.

      ! Interpolate sclrm to the momentum level for use in
      ! the second call to pdf_closure
        ! Clip if extrap. causes sclrm_zm to be less than sclr_tol

      ! Interpolate pressure, p_in_Pa, to momentum levels.
      ! The pressure at thermodynamic level k = 1 has been set to be the surface
      ! (or model lower boundary) pressure.  Since the surface (or model lower
      ! boundary) is located at momentum level k = 1, the pressure there is
      ! p_sfc, which is p_in_Pa(1).  Thus, p_in_Pa_zm(1) = p_in_Pa(1).

      ! Clip pressure if the extrapolation leads to a negative value of pressure
      ! Set exner at momentum levels, exner_zm, based on p_in_Pa_zm.

      ! Clip if extrapolation at the top level causes rtm_zm to be < rt_tol
      ! Clip if extrapolation at the top level causes thlm_zm to be < thl_tol

      ! Interpolate hydrometeor mixed moments to momentum levels.


      ! Call pdf_closure to output the variables which belong on the momentum grid.


        ! Subroutine may produce NaN values, and if so, exit
        ! gracefully.
        ! Joshua Fasching March 2008








      ! Interpolate momentum variables output from the first call to
      ! pdf_closure back to momentum grid.
      ! Since top momentum level is higher than top thermo level,
      ! Set variables at top momentum level to 0.

      ! Only do this for wp4 and rcp2 if we're saving stats, since they are not
      ! used elsewhere in the parameterization












      ! Interpolate passive scalars back onto the m grid



    ! If l_trapezoidal_rule_zt is true, call trapezoidal_rule_zt for
    ! thermodynamic-level variables output from pdf_closure.
    ! ldgrant June 2009


    ! If l_trapezoidal_rule_zm is true, call trapezoidal_rule_zm for
    ! the important momentum-level variabes output from pdf_closure.
    ! ldgrant Feb. 2010


    ! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
    ! This code won't work unless rtm >= 0 !!!
    ! We do not clip rcm_in_layer because rcm_in_layer only influences
    ! radiation, and we do not want to bother recomputing it.
    ! Code is duplicated from below to ensure that relative humidity
    ! is calculated properly.  3 Sep 2009

    ! Compute variables cloud_cover and rcm_in_layer.
    ! Added July 2009

    ! Use cloud_cover and rcm_in_layer to help boost cloud_frac and rcm to help
    ! increase cloudiness at coarser grid resolutions.


    ! Clip cloud fraction here if it still exceeds 1.0 due to round off
    ! Ditto with ice cloud fraction

      !A third call to pdf_closure, with terms modified to include the effects
      !of latent heating due to ice.  Thlm and rtm add the effects of ice, and
      !the terms are all renamed with "_frz" appended. The modified terms will
      !be fed into the calculations of the turbulence terms. storer-3/14/13
      
      !Also added rain for completeness. storer-3/4/14







        ! Subroutine may produce NaN values, and if so, exit gracefully.
        ! Joshua Fasching March 2008







        ! Nudge rtm to prevent excessive drying


      ! Clip if extrapolation at the top level causes rtm_zm to be < rt_tol
      ! Clip if extrapolation at the top level causes thlm_zm to be < thl_tol

        ! Call pdf_closure again to output the variables which belong on the momentum grid.

          ! Subroutine may produce NaN values, and if so, exit
          ! gracefully.
          ! Joshua Fasching March 2008











        ! If l_trapezoidal_rule_zm is true, call trapezoidal_rule_zm for
        ! the important momentum-level variabes output from pdf_closure.
        ! ldgrant Feb. 2010








      !----------------------------------------------------------------
      ! Compute thvm
      !----------------------------------------------------------------


      !----------------------------------------------------------------
      ! Compute tke (turbulent kinetic energy)
      !----------------------------------------------------------------

        ! tke is assumed to be 3/2 of wp2

      !----------------------------------------------------------------
      ! Compute mixing length
      !----------------------------------------------------------------

        ! Call compute length two additional times with perturbed values
        ! of rtm and thlm so that an average value of Lscale may be calculated.
          !Include the effects of ice in the length scale calculation






        ! Take the values of thl and rt based one 1st or 2nd plume



            !Lscale_weight = pdf_params%mixt_frac
            !Lscale_weight = 1.0_core_rknd - pdf_params%mixt_frac
            !Lscale_weight = pdf_params%mixt_frac
            !Lscale_weight = 1.0_core_rknd - pdf_params%mixt_frac

        ! Call length with perturbed values of thl and rt





      ! ********** NOTE: **********
      ! This call to compute_length must be last.  Otherwise, the values of
      ! Lscale_up and Lscale_down in stats will be based on perturbation length scales
      ! rather than the mean length scale.

          ! Weighted average of mean, pert_1, & pert_2
!       Lscale = 0.5_core_rknd * ( Lscale + Lscale_weight*Lscale_pert_1 &
!                                  + (1.0_core_rknd-Lscale_weight)*Lscale_pert_2 )

          ! Weighted average of just the perturbed values
!       Lscale = Lscale_weight*Lscale_pert_1 + (1.0_core_rknd-Lscale_weight)*Lscale_pert_2

          ! Un-weighted average of just the perturbed values

      !----------------------------------------------------------------
      ! Dissipation time
      !----------------------------------------------------------------
! Vince Larson replaced the cutoff of em_min by w_tol**2.  7 Jul 2007
!     This is to prevent tau from being too large (producing little damping)
!     in stably stratified layers with little turbulence.
!       sqrt_em_zt = SQRT( MAX( em_min, zm2zt( em ) ) )
!       tau_zt = MIN( Lscale / sqrt_em_zt, taumax )
!       tau_zm &
!       = MIN( ( zt2zm( Lscale ) / SQRT( MAX( em_min, em ) ) ), taumax )
!   Addition by Brian:  Model constant em_min is now set to (3/2)*w_tol_sqd.
!                       Thus, em_min can replace w_tol_sqd here.

! End Vince Larson's replacement.

      ! Determine the static stability corrected version of tau_zm
      ! Create a damping time scale that is more strongly damped at the
      ! altitudes where the Brunt-Vaisala frequency (N^2) is large.

      ! Modification to damp noise in stable region
! Vince Larson commented out because it may prevent turbulence from
!    initiating in unstable regions.  7 Jul 2007
!       do k = 1, gr%nz
!         if ( wp2(k) <= 0.005_core_rknd ) then
!           tau_zt(k) = taumin
!           tau_zm(k) = taumin
!         end if
!       end do
! End Vince Larson's commenting.

      !----------------------------------------------------------------
      ! Eddy diffusivity coefficient
      !----------------------------------------------------------------
      ! c_K is 0.548 usually (Duynkerke and Driedonks 1987)
      ! CLUBB uses a smaller value to better fit empirical data.








      !----------------------------------------------------------------
      ! Set Surface variances
      !----------------------------------------------------------------

      ! Surface variances should be set here, before the call to either
      ! advance_xp2_xpyp or advance_wp2_wp3.
      ! Surface effects should not be included with any case where the lowest
      ! level is not the ground level.  Brian Griffin.  December 22, 2005.

        ! Reflect surface varnce changes in budget





        ! Update surface stats



        ! Variances for cases where the lowest level is not at the surface.
        ! Eliminate surface effects on lowest level variances.





      !#######################################################################
      !############## ADVANCE PROGNOSTIC VARIABLES ONE TIMESTEP ##############
      !#######################################################################

      ! Store the saturation mixing ratio for output purposes.  Brian
      ! Compute rsat if either rsat or rel_humidity is to be saved.  ldgrant




        ! Output relative humidity (q/q∗ where q∗ is the saturation mixing ratio over liquid)
        ! Added an extra check for irel_humidity > 0; otherwise, if both irsat = 0 and
        ! irel_humidity = 0, rsat is not computed, leading to a floating-point exception
        ! when stat_update_var is called for rel_humidity.  ldgrant


      !----------------------------------------------------------------
      ! Advance rtm/wprtp and thlm/wpthlp one time step
      !----------------------------------------------------------------


      ! Determine stability correction factor


      ! Here we determine if we're using tau_zm or tau_N2_zm, which is tau
      ! that has been stability corrected for stably stratified regions.
      ! -dschanen 7 Nov 2014




      ! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
      ! This code won't work unless rtm >= 0 !!!
      ! We do not clip rcm_in_layer because rcm_in_layer only influences
      ! radiation, and we do not want to bother recomputing it.  6 Aug 2009








      !----------------------------------------------------------------
      ! Compute some of the variances and covariances.  These include the variance of
      ! total water (rtp2), liquid potential termperature (thlp2), their
      ! covariance (rtpthlp), and the variance of horizontal wind (up2 and vp2).
      ! The variance of vertical velocity is computed later.
      !----------------------------------------------------------------

      ! We found that certain cases require a time tendency to run
      ! at shorter timesteps so these are prognosed now.

      ! We found that if we call advance_xp2_xpyp first, we can use a longer timestep.

      !----------------------------------------------------------------
      ! Covariance clipping for wprtp, wpthlp, wpsclrp, upwp, and vpwp
      ! after subroutine advance_xp2_xpyp updated xp2.
      !----------------------------------------------------------------




      !----------------------------------------------------------------
      ! Advance 2nd and 3rd order moment of vertical velocity (wp2 / wp3)
      ! by one timestep
      !----------------------------------------------------------------


      !----------------------------------------------------------------
      ! Covariance clipping for wprtp, wpthlp, wpsclrp, upwp, and vpwp
      ! after subroutine advance_wp2_wp3 updated wp2.
      !----------------------------------------------------------------



      !----------------------------------------------------------------
      ! Advance the horizontal mean of the wind in the x-y directions
      ! (i.e. um, vm) and the mean of the eddy-diffusivity scalars
      ! (i.e. edsclrm) by one time step
      !----------------------------------------------------------------i

      


      
      !local input variables
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) km_zm
          CALL kgen_array_sumcheck("km_zm", kgen_array_sum, REAL(SUM(km_zm), 8), .TRUE.)
      END IF 
      
      !extern output variables
      CALL kr_externs_out_error_code(kgen_unit)
      CALL kr_externs_out_stats_variables(kgen_unit)
      CALL kr_externs_out_sponge_layer_damping(kgen_unit)
      CALL kr_externs_out_variables_diagnostic_module(kgen_unit)
      
      !local output variables
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_um
          CALL kgen_array_sumcheck("kgenref_um", kgen_array_sum, REAL(SUM(kgenref_um), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_vm
          CALL kgen_array_sumcheck("kgenref_vm", kgen_array_sum, REAL(SUM(kgenref_vm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_upwp
          CALL kgen_array_sumcheck("kgenref_upwp", kgen_array_sum, REAL(SUM(kgenref_upwp), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_vpwp
          CALL kgen_array_sumcheck("kgenref_vpwp", kgen_array_sum, REAL(SUM(kgenref_vpwp), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_edsclrm
          CALL kgen_array_sumcheck("kgenref_edsclrm", kgen_array_sum, REAL(SUM(kgenref_edsclrm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgenref_err_code
      
      !Uncomment following call statement to turn on perturbation experiment.
      !Adjust perturbation value and/or kind parameter if required.
      !CALL kgen_perturb_real( your_variable, 1.0E-15_8 )
      
      
      !call to kgen kernel
      call advance_windm_edsclrm( dt, wm_zt, Km_zm, ug, vg, um_ref, vm_ref, & ! intent(in)
                                  wp2, up2, vp2, um_forcing, vm_forcing,    & ! intent(in)
                                  edsclrm_forcing,                          & ! intent(in)
                                  rho_ds_zm, invrs_rho_ds_zt,               & ! intent(in)
                                  fcor, l_implemented,                      & ! intent(in)
                                  um, vm, edsclrm,                          & ! intent(inout)
                                  upwp, vpwp, wpedsclrp,                    & ! intent(inout)
                                  err_code )                                  ! intent(inout)
				  

      


      !#######################################################################
      !#############            ACCUMULATE STATISTICS            #############
      !#######################################################################






















        ! Spurious source will only be calculated if rtm_ma and thlm_ma are zero.
        ! Therefore, wm must be zero or l_implemented must be true.
          ! Calculate the spurious source for rtm






          ! Calculate the spurious source for thlm






        ! Write the var to stats

      
      !verify init
      CALL kgen_init_check(check_status, verboseLevel=1)
      
      !extern verify variables
      CALL kv_externs_stats_variables(check_status)
      CALL kv_externs_variables_diagnostic_module(check_status)
      
      !local verify variables
      CALL kv_advance_clubb_core_real__core_rknd_dim1("um", check_status, um, kgenref_um)
      CALL kv_advance_clubb_core_real__core_rknd_dim1("upwp", check_status, upwp, kgenref_upwp)
      CALL kv_advance_clubb_core_real__core_rknd_dim1("vm", check_status, vm, kgenref_vm)
      CALL kv_advance_clubb_core_real__core_rknd_dim1("vpwp", check_status, vpwp, kgenref_vpwp)
      CALL kv_advance_clubb_core_real__core_rknd_dim2("edsclrm", check_status, edsclrm, kgenref_edsclrm)
      CALL kv_advance_clubb_core_integer__("err_code", check_status, err_code, kgenref_err_code)
      WRITE (*, *) ""
      IF (check_status%verboseLevel > 0) THEN
          WRITE (*, *) "Number of verified variables: ", check_status%numTotal
          WRITE (*, *) "Number of identical variables: ", check_status%numIdentical
          WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol
          WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol
          WRITE (*, *) "Tolerance: ", check_status%tolerance
      END IF 
      WRITE (*, *) ""
      IF (check_status%numOutTol > 0) THEN
          WRITE (*, *) "Verification FAILED"
          check_status%Passed = .FALSE.
      ELSE
          WRITE (*, *) "Verification PASSED"
          check_status%Passed = .TRUE.
      END IF 
      WRITE (*, *) ""
      
      !Measuring elapsed time. Please increase the value of kgen_maxiter to get improve timing measurment resolution.
      CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock)
      DO kgen_intvar = 1, kgen_maxiter
      call advance_windm_edsclrm( dt, wm_zt, Km_zm, ug, vg, um_ref, vm_ref, & ! intent(in)
                                  wp2, up2, vp2, um_forcing, vm_forcing,    & ! intent(in)
                                  edsclrm_forcing,                          & ! intent(in)
                                  rho_ds_zm, invrs_rho_ds_zt,               & ! intent(in)
                                  fcor, l_implemented,                      & ! intent(in)
                                  um, vm, edsclrm,                          & ! intent(inout)
                                  upwp, vpwp, wpedsclrp,                    & ! intent(inout)
                                  err_code )                                  ! intent(inout)
      END DO 
      CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock)
      kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)
      WRITE (*, *) "advance_windm_edsclrm : Time per call (usec): ", kgen_elapsed_time
      kgen_total_time = kgen_total_time + kgen_elapsed_time
      
      CONTAINS
      
      !verify state subroutine for kv_advance_clubb_core_real__core_rknd_dim1
      RECURSIVE SUBROUTINE kv_advance_clubb_core_real__core_rknd_dim1(varname, check_status, var, kgenref_var)
          CHARACTER(LEN=*), INTENT(IN) :: varname
          TYPE(check_t), INTENT(INOUT) :: check_status
          REAL(KIND=core_rknd), INTENT(IN), DIMENSION(:) :: var, kgenref_var
          INTEGER :: check_result
          LOGICAL :: is_print = .FALSE.
          
          INTEGER :: idx1
          INTEGER :: n
          real(KIND=core_rknd) :: nrmsdiff, rmsdiff
          real(KIND=core_rknd), ALLOCATABLE :: buf1(:), buf2(:)
          
          check_status%numTotal = check_status%numTotal + 1
          
          IF (ALL(var == kgenref_var)) THEN
              check_status%numIdentical = check_status%numIdentical + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              ALLOCATE (buf1(SIZE(var,dim=1)))
              ALLOCATE (buf2(SIZE(var,dim=1)))
              n = COUNT(var /= kgenref_var)
              WHERE ( ABS(kgenref_var) > check_status%minvalue )
                  buf1 = ((var-kgenref_var)/kgenref_var)**2
                  buf2 = (var-kgenref_var)**2
              ELSEWHERE
                  buf1 = (var-kgenref_var)**2
                  buf2 = buf1
              END WHERE 
              nrmsdiff = SQRT(SUM(buf1)/REAL(n))
              rmsdiff = SQRT(SUM(buf2)/REAL(n))
              IF (nrmsdiff > check_status%tolerance) THEN
                  check_status%numOutTol = check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 0) THEN
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              ELSE
                  check_status%numInTol = check_status%numInTol + 1
                  IF (check_status%verboseLevel > 0) THEN
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                  WRITE (*, *) "RMS of difference is ", 0
                  WRITE (*, *) "Normalized RMS of difference is ", 0
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 0) THEN
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                  WRITE (*, *) "RMS of difference is ", rmsdiff
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                  WRITE (*, *) "RMS of difference is ", rmsdiff
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END SUBROUTINE kv_advance_clubb_core_real__core_rknd_dim1
      
      !verify state subroutine for kv_advance_clubb_core_real__core_rknd_dim2
      RECURSIVE SUBROUTINE kv_advance_clubb_core_real__core_rknd_dim2(varname, check_status, var, kgenref_var)
          CHARACTER(LEN=*), INTENT(IN) :: varname
          TYPE(check_t), INTENT(INOUT) :: check_status
          REAL(KIND=core_rknd), INTENT(IN), DIMENSION(:,:) :: var, kgenref_var
          INTEGER :: check_result
          LOGICAL :: is_print = .FALSE.
          
          INTEGER :: idx1, idx2
          INTEGER :: n
          real(KIND=core_rknd) :: nrmsdiff, rmsdiff
          real(KIND=core_rknd), ALLOCATABLE :: buf1(:,:), buf2(:,:)
          
          check_status%numTotal = check_status%numTotal + 1
          
          IF (ALL(var == kgenref_var)) THEN
              check_status%numIdentical = check_status%numIdentical + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2)))
              ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2)))
              n = COUNT(var /= kgenref_var)
              WHERE ( ABS(kgenref_var) > check_status%minvalue )
                  buf1 = ((var-kgenref_var)/kgenref_var)**2
                  buf2 = (var-kgenref_var)**2
              ELSEWHERE
                  buf1 = (var-kgenref_var)**2
                  buf2 = buf1
              END WHERE 
              nrmsdiff = SQRT(SUM(buf1)/REAL(n))
              rmsdiff = SQRT(SUM(buf2)/REAL(n))
              IF (nrmsdiff > check_status%tolerance) THEN
                  check_status%numOutTol = check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 0) THEN
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              ELSE
                  check_status%numInTol = check_status%numInTol + 1
                  IF (check_status%verboseLevel > 0) THEN
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                  WRITE (*, *) "RMS of difference is ", 0
                  WRITE (*, *) "Normalized RMS of difference is ", 0
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 0) THEN
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                  WRITE (*, *) "RMS of difference is ", rmsdiff
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                  WRITE (*, *) "RMS of difference is ", rmsdiff
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END SUBROUTINE kv_advance_clubb_core_real__core_rknd_dim2
      
      !verify state subroutine for kv_advance_clubb_core_integer__
      RECURSIVE SUBROUTINE kv_advance_clubb_core_integer__(varname, check_status, var, kgenref_var)
          CHARACTER(LEN=*), INTENT(IN) :: varname
          TYPE(check_t), INTENT(INOUT) :: check_status
          INTEGER, INTENT(IN) :: var, kgenref_var
          INTEGER :: check_result
          LOGICAL :: is_print = .FALSE.
          
          integer :: diff
          
          check_status%numTotal = check_status%numTotal + 1
          
          IF (var == kgenref_var) THEN
              check_status%numIdentical = check_status%numIdentical + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff = ABS(var - kgenref_var)
              IF (diff <= check_status%tolerance) THEN
                  check_status%numInTol = check_status%numInTol + 1
                  IF (check_status%verboseLevel > 0) THEN
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  check_status%numOutTol = check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 0) THEN
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", 0
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 0) THEN
                  WRITE (*, *) "Difference is ", diff
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) "Difference is ", diff
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END SUBROUTINE kv_advance_clubb_core_integer__
      
  END SUBROUTINE advance_clubb_core

    !-----------------------------------------------------------------------
      !
      ! Description:
      !   Subroutine to set up the model for execution.
      !
      ! References:
      !   None
      !-------------------------------------------------------------------------












      ! Input Variables

      ! Grid definition
      !                      Only true when used in a host model
      !                      CLUBB determines what nzmax should be
      !                      given zm_init and zm_top when
      !                      running in standalone mode.


      ! Flag to see if CLUBB is running on it's own,
      ! or if it's implemented as part of a host model.

      ! If CLUBB is running on it's own, this option determines
      ! if it is using:
      ! 1) an evenly-spaced grid,
      ! 2) a stretched (unevenly-spaced) grid entered on the
      !    thermodynamic grid levels (with momentum levels set
      !    halfway between thermodynamic levels), or
      ! 3) a stretched (unevenly-spaced) grid entered on the
      !    momentum grid levels (with thermodynamic levels set
      !    halfway between momentum levels).

      ! If the CLUBB model is running by itself, and is using an
      ! evenly-spaced grid (grid_type = 1), it needs the vertical
      ! grid spacing, momentum-level starting altitude, and maximum
      ! altitude as input.

      ! If the CLUBB parameterization is implemented in a host model,
      ! it needs to use the host model's momentum level altitudes
      ! and thermodynamic level altitudes.
      ! If the CLUBB model is running by itself, but is using a
      ! stretched grid entered on thermodynamic levels (grid_type = 2),
      ! it needs to use the thermodynamic level altitudes as input.
      ! If the CLUBB model is running by itself, but is using a
      ! stretched grid entered on momentum levels (grid_type = 3),
      ! it needs to use the momentum level altitudes as input.

      ! Model parameters




      ! Flags










      ! Output variables

      ! Local variables

      !----- Begin Code -----

      ! Sanity check for the saturation formula
        ! Using the Bolton 1980 approximations for SVP over vapor/ice

        ! Using the Flatau, et al. polynomial approximation for SVP over vapor/ice

        ! Using the GFDL SVP formula (Goff-Gratch)

        ! Add new saturation formulas after this


      ! Setup grid

      ! Setup flags










      ! Define model constant parameters








      ! Define tunable constant parameters

      ! Error Report
      ! Joshua Fasching February 2008













      ! The diagnostic variables need to be
      ! declared, allocated, initialized, and deallocated whether CLUBB
      ! is part of a larger model or not.



    !----------------------------------------------------------------------------
      !
      ! Description:
      !   Frees memory used by the model itself.
      !
      ! References:
      !   None
      !---------------------------------------------------------------------------






      ! Flag to see if CLUBB is running on it's own,
      ! or if it's implemented as part of a host model.

      !----- Begin Code -----







      ! The diagnostic variables need to be
      ! declared, allocated, initialized, and deallocated whether CLUBB
      ! is part of a larger model or not.

      ! De-allocate the array for the passive scalar tolerances

      ! De-allocate the arrays for the grid

      ! De-allocate the arrays for nu


    !-----------------------------------------------------------------------
      !
      ! Description:
      !   This subroutine takes the output variables on the thermo.
      !   grid and either: interpolates them to the momentum grid, or uses the
      !   values output from the second call to pdf_closure on momentum levels if
      !   l_call_pdf_closure_twice is true.  It then calls the function
      !   trapezoid_zt to recompute the variables on the thermo. grid.
      !
      !   ldgrant June 2009
      !
      ! Note:
      !   The argument variables in the last 5 lines of the subroutine
      !   (wprtp2_zm through pdf_params_zm) are declared intent(inout) because
      !   if l_call_pdf_closure_twice is true, these variables will already have
      !   values from pdf_closure on momentum levels and will not be altered in
      !   this subroutine.  However, if l_call_pdf_closure_twice is false, these
      !   variables will not have values yet and will be interpolated to
      !   momentum levels in this subroutine.
      ! References:
      !   None
      !-----------------------------------------------------------------------








      ! Constant parameters

      ! Input variables

      ! Input/Output variables
      ! Thermodynamic level variables output from the first call to pdf_closure



      ! Thermo. level variables brought to momentum levels either by
      ! interpolation (in subroutine trapezoidal_rule_zt) or by
      ! the second call to pdf_closure (in subroutine advance_clubb_core)



      ! Local variables

      ! Components of PDF_parameters on the momentum grid (_zm) and on the thermo. grid (_zt)





      !----------------------- Begin Code -----------------------------

      ! Store components of pdf_params in the locally declared variables
      ! We only apply the trapezoidal rule to these when
      ! l_apply_rule_to_pdf_params is true.  This is because when we apply the
      ! rule to the final result of pdf_closure rather than the intermediate
      ! results it can lead to an inconsistency in how we determine which
      ! PDF component a point is in and whether the point is in or out of cloud,
      ! which is turn will break the latin hypercube code that samples
      ! preferentially in cloud. -dschanen 13 Feb 2012



      ! If l_call_pdf_closure_twice is true, the _zm variables already have
      ! values from the second call to pdf_closure in advance_clubb_core.
      ! If it is false, the variables are interpolated to the _zm levels.

        ! Store, in locally declared variables, the pdf_params output
        ! from the second call to pdf_closure



        ! Interpolate thermodynamic variables to the momentum grid.
        ! Since top momentum level is higher than top thermo. level,
        ! set variables at top momentum level to 0.





        ! Use the trapezoidal rule to recompute the variables on the stats_zt level










        ! Note: this code makes PDF component cloud water mixing ratios and
        !       cloud fractions inconsistent with the PDF.  Other parts of
        !       CLUBB require PDF component cloud fractions to remain
        !       consistent with the PDF.  This code needs to be refactored
        !       so that cloud_frac_1 and cloud_frac_2 are preserved.

      ! End of trapezoidal rule


    !-----------------------------------------------------------------------
      !
      ! Description:
      !   This subroutine recomputes three variables on the
      !   momentum grid from pdf_closure -- wpthvp, thlpthvp, and
      !   rtpthvp -- by calling the function trapezoid_zm.  Only these three
      !   variables are used in this subroutine because they are the only
      !   pdf_closure momentum variables used elsewhere in CLUBB.
      !
      !   The _zt variables are output from the first call to pdf_closure.
      !   The _zm variables are output from the second call to pdf_closure
      !   on the momentum levels.
      !   This is done before the call to this subroutine.
      !
      !   ldgrant Feb. 2010
      !
      !  References:
      !    None
      !-----------------------------------------------------------------------




      ! Input variables

      ! Input/Output variables

      !----------------------- Begin Code -----------------------------

      ! Use the trapezoidal rule to recompute the variables on the zm level


    !-----------------------------------------------------------------------
      !
      ! Description:
      !   Function which uses the trapezoidal rule from calculus
      !   to recompute the values for the variables on the thermo. grid which
      !   are output from the first call to pdf_closure in module clubb_core.
      !
      !   ldgrant June 2009
      !--------------------------------------------------------------------




      ! Input Variables

      ! Result

      ! Local Variable

      !------------ Begin Code --------------

      ! Boundary condition: trapezoidal rule not valid at zt level 1

        ! Trapezoidal rule from calculus


    !-----------------------------------------------------------------------
      !
      ! Description:
      !   Function which uses the trapezoidal rule from calculus
      !   to recompute the values for the important variables on the momentum
      !   grid which are output from pdf_closure in module clubb_core.
      !   These momentum variables only include wpthvp, thlpthvp, and rtpthvp.
      !
      !   ldgrant Feb. 2010
      !--------------------------------------------------------------------




      ! Input Variables

      ! Result

      ! Local Variable

      !------------ Begin Code --------------

      ! Boundary conditions: trapezoidal rule not valid at top zm level, nzmax.
      ! Trapezoidal rule also not used at zm level 1.

        ! Trapezoidal rule from calculus


    !-----------------------------------------------------------------------
      !
      ! Description:
      !   Subroutine to compute cloud cover (the amount of sky
      !   covered by cloud) and rcm in layer (liquid water mixing ratio in
      !   the portion of the grid box filled by cloud).
      !
      ! References:
      !   Definition of 's' comes from:
      !   ``The Gaussian Cloud Model Relations'' G. L. Mellor (1977)
      !   JAS, Vol. 34, pp. 356--358.
      !
      ! Notes:
      !   Added July 2009
      !---------------------------------------------------------------------







      ! External functions

      ! Input variables


      ! Output variables

      ! Local variables
    !                            two Gaussian distributions


      ! ------------ Begin code ---------------







          ! There is cloud above and below,
          !   so assume cloud fills grid box from top to bottom


          ! Cloud may fail to reach gridbox top or base or both

          ! First let the cloud fill the entire grid box, then overwrite
          ! vert_cloud_frac_upper(k) and/or vert_cloud_frac_lower(k)
          ! for a cloud top, cloud base, or one-point cloud.




            ! Make the transition in cloudiness more gradual than using
            ! the above min statement alone.







            ! Make the transition in cloudiness more gradual than using
            ! the above min statement alone.

















    !-----------------------------------------------------------------------
      !
      ! Description:
      !   Subroutine that reduces cloud water (rcm) whenever
      !   it exceeds total water (rtm = vapor + liquid).
      !   This avoids negative values of rvm = water vapor mixing ratio.
      !   However, it will not ensure that rcm <= rtm if rtm <= 0.
      !
      ! References:
      !   None
      !---------------------------------------------------------------------







      ! External functions

      ! Input variables




      ! ------------ Begin code ---------------

      ! Vince Larson clipped rcm in order to prevent rvm < 0.  5 Apr 2008.
      ! This code won't work unless rtm >= 0 !!!
      ! We do not clip rcm_in_layer because rcm_in_layer only influences
      ! radiation, and we do not want to bother recomputing it.  6 Aug 2009







    !-----------------------------------------------------------------------------

      ! Description:
      !   This subroutine sets the value of Lscale_max, which is the maximum
      !   allowable value of Lscale.  For standard CLUBB, it is set to a very large
      !   value so that Lscale will not be limited.  However, when CLUBB is running
      !   as part of a host model, the value of Lscale_max is dependent on the size
      !   of the host model's horizontal grid spacing.  The smaller the host model's
      !   horizontal grid spacing, the smaller the value of Lscale_max.  When Lscale
      !   is limited to a small value, the value of time-scale Tau is reduced, which
      !   in turn produces greater damping on CLUBB's turbulent parameters.  This
      !   is the desired effect on turbulent parameters for a host model with small
      !   horizontal grid spacing, for small areas usually contain much less
      !   variation in meteorological quantities than large areas.

      ! References:
      !   None
      !-----------------------------------------------------------------------



      ! Input Variables
      !                    or if it's implemented as part of a host model.


      ! Output Variable

      ! ---- Begin Code ----

      ! Determine the maximum allowable value for Lscale (in meters).



!===============================================================================

  ! Description:
  !   Computes the contribution of radiative cooling to thlp2

  ! References:
  !   See clubb:ticket:632
  !----------------------------------------------------------------------






  ! Input Variables


  ! Input/Output Variables

  ! Local Variables

  !----------------------------------------------------------------------



 






    !-----------------------------------------------------------------------

end module advance_clubb_core_module