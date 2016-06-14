!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:45
!KGEN version : 0.6.1

!-----------------------------------------------------------------------
! $Id: parameters_tunable.F90 7416 2014-12-04 20:16:51Z schemena@uwm.edu $
!===============================================================================
module parameters_tunable

  ! Description:
  !   This module contains tunable model parameters.  The purpose of the module is to make it
  !   easier for the clubb_tuner code to use the params vector without "knowing" any information
  !   about the individual parameters contained in the vector itself.  It makes it easier to add
  !   new parameters to be tuned for, but does not make the CLUBB_core code itself any simpler.
  !   The parameters within the vector do not need to be the same variables used in the rest of
  !   CLUBB_core (see for e.g. nu1_vert_res_dep or lmin_coef).
  !   The parameters in the params vector only need to be those parameters for which we're not
  !   sure the correct value and we'd like to tune for.
  !
  ! References:
  !   None
  ! 
  ! Notes:
  !   To make it easier to verify of code correctness, please keep the omp threadprivate
  !   directives just after the variable declaration.  All parameters in this
  !   module should be declared threadprivate because of the CLUBB tuner.
  !-----------------------------------------------------------------------



    USE clubb_precision, ONLY: core_rknd

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

  ! Default to private
    PRIVATE


  ! Model constant parameters
    REAL(KIND=core_rknd), public :: c1      = 1.000000_core_rknd, c1b     = 1.000000_core_rknd, &
c1c     = 1.000000_core_rknd, c2      = 1.300000_core_rknd, c2rt    = 1.000000_core_rknd, &
c2thl   = 1.000000_core_rknd, c2rtthl = 1.300000_core_rknd, c2b     = 1.300000_core_rknd, &
c2c     = 5.000000_core_rknd, c4      = 5.200000_core_rknd, c5      = 0.300000_core_rknd, &
c6rt    = 4.000000_core_rknd, c6rtb   = 6.000000_core_rknd, c6rtc   = 1.000000_core_rknd, &
c6thl   = 4.000000_core_rknd, c6thlb  = 6.000000_core_rknd, c6thlc  = 1.000000_core_rknd, &
c7      = 0.500000_core_rknd, c7b     = 0.500000_core_rknd, c7c     = 0.500000_core_rknd, &
c8      = 4.200000_core_rknd, c8b     = 0.000000_core_rknd, c11     = 0.70000_core_rknd, &
c11b    = 0.350000_core_rknd, c11c    = 0.500000_core_rknd, c12     = 1.000000_core_rknd, &
c14     = 1.000000_core_rknd, c15     = 0.4_core_rknd
!$omp threadprivate(C1, C1b, C1c, C2, C2b, C2c, &
!$omp   C2rt, C2thl, C2rtthl, C4, C5, C6rt, C6rtb, C6rtc, &
!$omp   C6thl, C6thlb, C6thlc, &
!$omp   C7, C7b, C7c, C8, C8b, C10, C11, C11b, C11c, C12, &
!$omp   C13, C14, C15)

  real( kind = core_rknd ), public ::    &
    C6rt_Lscale0  = 14.0_core_rknd,      & ! Damp C6rt as a fnct. of Lscale  [-]
    C6thl_Lscale0 = 14.0_core_rknd,      & ! Damp C6thl as a fnct. of Lscale [-]
    C7_Lscale0    = 0.8500000_core_rknd, & ! Damp C7 as a fnct. of Lscale    [-]
    wpxp_L_thresh = 60.0_core_rknd         ! Lscale threshold: damp C6 & C7  [m]
!$omp threadprivate(C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh)

  ! Note: DD 1987 is Duynkerke & Driedonks (1987).
  REAL(KIND=core_rknd), public :: c_k         = 0.200000_core_rknd, c_k1        = 0.750000_core_rknd, &
c_k2        = 0.125000_core_rknd, c_k6        = 0.375000_core_rknd, c_k8        = 1.250000_core_rknd, &
c_k9        = 0.250000_core_rknd, gamma_coef  = 0.320000_core_rknd, gamma_coefb = 0.320000_core_rknd, &
gamma_coefc = 5.000000_core_rknd, mu          = 1.000e-3_core_rknd, taumax      = 3600.000_core_rknd, &
lmin        = 20.00000_core_rknd
!$omp threadprivate(c_K, c_K1, c_K2, c_K6, &
!$omp   c_K8, c_K9, c_K_hm, c_K_hmb, K_hm_min_coef, gamma_coef, gamma_coefb, gamma_coefc, &
!$omp   mu, mult_coef, taumin, taumax, lmin)

  real( kind = core_rknd ), public :: &
    Lscale_mu_coef   = 2.0_core_rknd, & ! Coef perturb mu: av calc Lscale    [-]
    Lscale_pert_coef = 0.1_core_rknd    ! Coef pert thlm/rtm: av calc Lscale [-]
!$omp threadprivate(Lscale_mu_coef, Lscale_pert_coef)


!$omp threadprivate(alpha_corr)

!$omp threadprivate(nu1, nu2, nu6, nu8, nu9, nu10, nu_hm)


  REAL(KIND=core_rknd), public, allocatable, dimension(:) :: nu1_vert_res_dep, nu2_vert_res_dep, nu6_vert_res_dep, nu8_vert_res_dep, nu9_vert_res_dep, nu10_vert_res_dep

!$omp threadprivate(nu1_vert_res_dep, nu2_vert_res_dep, nu6_vert_res_dep, &
!$omp   nu8_vert_res_dep, nu9_vert_res_dep, nu10_vert_res_dep, nu_hm_vert_res_dep)

  ! Vince Larson added a constant to set plume widths for theta_l and rt
  ! beta should vary between 0 and 3.

  real( kind = core_rknd ), public :: &
    beta = 2.400000_core_rknd    ! Beta coefficient     [-]

!$omp threadprivate(beta)


!$omp threadprivate(lmin_coef)

  ! Coefficient for adjusted overall correlation in hm_1/hm_2 calculation [-]

!$omp threadprivate( coef_hm_1_hm_2_corr_adj )

  ! Factor to decrease sensitivity in the denominator of Skw calculation
  real( kind = core_rknd ), public :: &




    Skw_denom_coef = 0.0_core_rknd





!$omp threadprivate( Skw_denom_coef )

  ! Coefficient of Kh_zm
  real( kind = core_rknd ), public :: &
    c_K10 = 0.6_core_rknd

!$omp threadprivate( c_K10 )

                                                 ! of thlp2_rad                               [-]

!$omp threadprivate( thlp2_rad_coef, thlp2_rad_cloud_frac_thresh )

  ! used in adj_low_res_nu. If .true., avg_deltaz = deltaz





!$omp threadprivate(l_prescribed_avg_deltaz)

  ! Since we lack a devious way to do this just once, this namelist
  ! must be changed as well when a new parameter is added.

  ! These are referenced together often enough that it made sense to
  ! make a list of them.  Note that lmin_coef is the input parameter,
  ! while the actual lmin model constant is computed from this.
  !***************************************************************
  !                    ***** IMPORTANT *****
  ! If you change the order of the parameters in the parameter_indices,
  ! you will need to change the order of this list as well or the
  ! tuner will break!
  !                    ***** IMPORTANT *****
  !***************************************************************


  PUBLIC kr_externs_in_parameters_tunable

  !=============================================================================
  
  CONTAINS
  

    ! Description:
    ! Subroutine to setup model parameters

    ! References:
    ! None
    !-----------------------------------------------------------------------






    ! Constant Parameters

    ! Input Variables


    ! Grid definition

    ! If CLUBB is running on its own, this option determines
    ! if it is using:
    ! 1) an evenly-spaced grid,
    ! 2) a stretched (unevenly-spaced) grid entered on the
    !    thermodynamic grid levels (with momentum levels set
    !    halfway between thermodynamic levels), or
    ! 3) a stretched (unevenly-spaced) grid entered on the
    !    momentum grid levels (with thermodynamic levels set
    !    halfway between momentum levels).

    ! If the CLUBB parameterization is implemented in a host model,
    ! it needs to use the host model's momentum level altitudes
    ! and thermodynamic level altitudes.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on thermodynamic levels (grid_type = 2),
    ! it needs to use the thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on momentum levels (grid_type = 3),
    ! it needs to use the momentum level altitudes as input.

    ! Output Variables

    !-------------------- Begin code --------------------



    ! It was decided after some experimentation, that the best
    ! way to produce grid independent results is to set lmin to be
    ! some fixed value. -dschanen 21 May 2007
    !lmin = lmin_coef * deltaz  ! Old

    ! ### Adjust Constant Diffusivity Coefficients Based On Grid Spacing ###

    ! Sanity check
    ! Initialize err_code to clubb_no_error.  Only overwrite it if a variable
    ! out-of-bounds error is found.


       ! Constraints on beta



       ! Constraints on coef_hm_1_hm_2_corr_adj



       ! Constraints on entrainment rate, mu.



       ! Constraints on mixing length


!    write(*,nml=initvars) ! %% debug




  !=============================================================================

    ! Description:
    !   Adjust the values of background eddy diffusivity based on
    !   vertical grid spacing.
    !   This code was made into a public subroutine so that it may be
    !   called multiple times per model run in scenarios where grid
    !   altitudes, and hence average grid spacing, change through space
    !   and/or time.  This occurs, for example, when CLUBB is
    !   implemented in WRF.  --ldgrant Jul 2010
    !----------------------------------------------------------------------




    ! Constant Parameters

    ! Flag for adjusting the values of the constant background eddy diffusivity
    ! coefficients based on the average vertical grid spacing.  If this flag is
    ! turned off, the values of the various nu coefficients will remain as they
    ! are declared in the tunable_parameters.in file.

    ! The size of the average vertical grid spacing that serves as a threshold
    ! for when to increase the size of the background eddy diffusivity
    ! coefficients (nus) by a certain factor above what the background
    ! coefficients are specified to be in tunable_parameters.in.  At any average
    ! grid spacing at or below this value, the values of the background
    ! diffusivities remain the same.  However, at any average vertical grid
    ! spacing above this value, the values of the background eddy diffusivities
    ! are increased.  Traditionally, the threshold grid spacing has been set to
    ! 40.0 meters.  This is only relevant if l_adj_low_res_nu is turned on.

    ! Input Variables

    ! Grid definition

    ! If CLUBB is running on it's own, this option determines
    ! if it is using:
    ! 1) an evenly-spaced grid,
    ! 2) a stretched (unevenly-spaced) grid entered on the
    !    thermodynamic grid levels (with momentum levels set
    !    halfway between thermodynamic levels), or
    ! 3) a stretched (unevenly-spaced) grid entered on the
    !    momentum grid levels (with thermodynamic levels set
    !    halfway between momentum levels).


    ! If the CLUBB parameterization is implemented in a host model,
    ! it needs to use the host model's momentum level altitudes
    ! and thermodynamic level altitudes.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on thermodynamic levels (grid_type = 2),
    ! it needs to use the thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on momentum levels (grid_type = 3),
    ! it needs to use the momentum level altitudes as input.

    ! Local Variables

    ! The factor by which to multiply the coefficients of background eddy
    ! diffusivity if the grid spacing threshold is exceeded and l_adj_low_res_nu
    ! is turned on.

    ! Flag to enable nu values that are a function of grid spacing


    !--------------- Begin code -------------------------









    ! Flag for adjusting the values of the constant diffusivity coefficients
    ! based on the grid spacing.  If this flag is turned off, the values of the
    ! various nu coefficients will remain as they are declared in the
    ! parameters.in file.

      ! ### Adjust Constant Diffusivity Coefficients Based On Grid Spacing ###

      ! All of the background coefficients of eddy diffusivity, as well as the
      ! constant coefficient for 4th-order hyper-diffusion, must be adjusted
      ! based on the size of the grid spacing.  For a case that uses an
      ! evenly-spaced grid, the adjustment is based on the constant grid
      ! spacing deltaz.  For a case that uses a stretched grid, the adjustment
      ! is based on avg_deltaz, which is the average grid spacing over the
      ! vertical domain.
 
        


        ! CLUBB is implemented in a host model, or is using grid_type = 3

        ! Find the average deltaz over the grid based on momentum level
        ! inputs.



        ! Evenly-spaced grid.



        ! Stretched (unevenly-spaced) grid:  stretched thermodynamic level
        ! input.

        ! Find the average deltaz over the stretched grid based on
        ! thermodynamic level inputs.

        ! Eric Raut added to remove compiler warning. (Obviously, this value is not used)


      ! The nu's are chosen for deltaz <= 40 m. Looks like they must
      ! be adjusted for larger grid spacings (Vince Larson)
        ! Use a constant mult_factor so nu does not depend on grid spacing

        ! mult_factor will vary to create nu values that vary with grid spacing




      !mult_factor = 1.0_core_rknd + mult_coef * log( avg_deltaz / grid_spacing_thresh )





  !=============================================================================

    ! Description:
    ! Read a namelist containing the model parameters

    ! References:
    ! None
    !-----------------------------------------------------------------------


    ! Input variables


    ! Output variables

    ! Local variables


    ! ---- Begin Code ----

    ! If the filename is empty, assume we're using a `working' set of
    ! parameters that are set statically here (handy for host models).
    ! Read the namelist
      ! Read the namelist




    ! Put the variables in the output array








  !=============================================================================

    ! Description:
    ! Read a namelist containing the amount to vary model parameters.
    ! Used by the downhill simplex / simulated annealing algorithm.

    ! References:
    ! None
    !-----------------------------------------------------------------------



    ! Input variables


    ! Output variables

    ! An array of array indices (i.e. which elements of the array `params'
    ! are contained within the simplex and the spread variable)



    ! Local variables


    ! Amount to change each parameter for the initial simplex
    ! This MUST be changed to match the initvars namelist if parameters are added!

    ! Initialize values to -999.

    ! Read the namelist



    ! Put the variables in the output array






    ! Initialize to zero

    ! Determine how many variables are being changed






  !=============================================================================

    ! Description:
    ! Takes the list of scalar variables and puts them into a 1D vector.
    ! It is here for the purpose of keeping the code generalized
    ! when new variables are added.

    ! References:
    ! None
    !-----------------------------------------------------------------------





    ! Input variables

    ! Output variables













  !=============================================================================

    ! Description:
    ! Takes the 1D vector and returns the list of scalar variables.
    ! Here for the purposes of keeping the code generalized
    ! when new variables are added.

    ! References:
    ! None
    !-----------------------------------------------------------------------





    ! Input variables

    ! Output variables














  !=============================================================================

    ! Description:
    ! Return an array of all tunable parameters

    ! References:
    ! None
    !-----------------------------------------------------------------------


    ! Input Variables




  !=============================================================================

    ! Description:
    ! Set all tunable parameters to NaN

    ! References:
    ! None
    !-----------------------------------------------------------------------


    ! --- Begin Code ---




  !=============================================================================

    ! Description:
    !  De-allocates memory used for the nu arrays
    !
    ! References:
    !  None
    !-----------------------------------------------------------------------



    ! Local Variable(s)

    ! ----- Begin Code -----






!===============================================================================

  !read state subroutine for kr_externs_in_parameters_tunable
  SUBROUTINE kr_externs_in_parameters_tunable(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      READ (UNIT = kgen_unit) c11c
      READ (UNIT = kgen_unit) c11b
      READ (UNIT = kgen_unit) c2
      READ (UNIT = kgen_unit) c6thlc
      READ (UNIT = kgen_unit) c2b
      READ (UNIT = kgen_unit) c2c
      READ (UNIT = kgen_unit) c12
      READ (UNIT = kgen_unit) c11
      READ (UNIT = kgen_unit) c6rt
      READ (UNIT = kgen_unit) c15
      READ (UNIT = kgen_unit) c14
      READ (UNIT = kgen_unit) c6rtb
      READ (UNIT = kgen_unit) c6rtc
      READ (UNIT = kgen_unit) c6thl
      READ (UNIT = kgen_unit) c8b
      READ (UNIT = kgen_unit) c8
      READ (UNIT = kgen_unit) c2rt
      READ (UNIT = kgen_unit) c1
      READ (UNIT = kgen_unit) c6thlb
      READ (UNIT = kgen_unit) c7
      READ (UNIT = kgen_unit) c5
      READ (UNIT = kgen_unit) c4
      READ (UNIT = kgen_unit) c2rtthl
      READ (UNIT = kgen_unit) c1b
      READ (UNIT = kgen_unit) c1c
      READ (UNIT = kgen_unit) c7c
      READ (UNIT = kgen_unit) c7b
      READ (UNIT = kgen_unit) c2thl
      READ (UNIT = kgen_unit) c6thl_lscale0
      READ (UNIT = kgen_unit) wpxp_l_thresh
      READ (UNIT = kgen_unit) c6rt_lscale0
      READ (UNIT = kgen_unit) c7_lscale0
      READ (UNIT = kgen_unit) c_k9
      READ (UNIT = kgen_unit) gamma_coef
      READ (UNIT = kgen_unit) c_k6
      READ (UNIT = kgen_unit) c_k
      READ (UNIT = kgen_unit) c_k8
      READ (UNIT = kgen_unit) c_k2
      READ (UNIT = kgen_unit) gamma_coefb
      READ (UNIT = kgen_unit) gamma_coefc
      READ (UNIT = kgen_unit) mu
      READ (UNIT = kgen_unit) c_k1
      READ (UNIT = kgen_unit) lmin
      READ (UNIT = kgen_unit) taumax
      READ (UNIT = kgen_unit) lscale_pert_coef
      READ (UNIT = kgen_unit) lscale_mu_coef
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu8_vert_res_dep, kgen_unit)
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu2_vert_res_dep, kgen_unit)
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu9_vert_res_dep, kgen_unit)
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu1_vert_res_dep, kgen_unit)
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu6_vert_res_dep, kgen_unit)
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu10_vert_res_dep, kgen_unit)
      READ (UNIT = kgen_unit) beta
      READ (UNIT = kgen_unit) skw_denom_coef
      READ (UNIT = kgen_unit) c_k10
  END SUBROUTINE kr_externs_in_parameters_tunable
  
  !read state subroutine for kr_parameters_tunable_real__core_rknd_dim1
  SUBROUTINE kr_parameters_tunable_real__core_rknd_dim1(var, kgen_unit, printvar)
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      INTEGER :: idx1
      INTEGER, DIMENSION(2,1) :: kgen_bound
      
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          IF (ALLOCATED( var )) THEN
              DEALLOCATE (var)
          END IF 
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgen_bound(1, 1)
          READ (UNIT = kgen_unit) kgen_bound(2, 1)
          ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
          READ (UNIT = kgen_unit) var
          CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var
          END IF 
      END IF 
  END SUBROUTINE kr_parameters_tunable_real__core_rknd_dim1
  
end module parameters_tunable
