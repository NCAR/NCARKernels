!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:49:59 
!KGEN version : 0.7.0 
  
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

    USE parameter_indices, ONLY: nparams 

    USE grid_class, ONLY: gr 

    USE clubb_precision, ONLY: core_rknd 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
    IMPLICIT NONE 

  ! Default to private
    PRIVATE 

    PUBLIC setup_parameters, read_parameters, adj_low_res_nu 

  ! Model constant parameters
  real( kind = core_rknd ), public :: & 
    C1      = 1.000000_core_rknd,    & ! Low Skewness in C1 Skw. Function    [-]
    C1b     = 1.000000_core_rknd,    & ! High Skewness in C1 Skw. Function   [-]
    C1c     = 1.000000_core_rknd,    & ! Degree of Slope of C1 Skw. Function [-]
    C2      = 1.300000_core_rknd,    & ! Low Skewness in C2 Skw. Function    [-]
    C2rt    = 1.000000_core_rknd,    & ! C2 coef. for the rtp2_dp1 term      [-]
    C2thl   = 1.000000_core_rknd,    & ! C2 coef. for the thlp2_dp1 term     [-]
    C2rtthl = 1.300000_core_rknd,    & ! C2 coef. for the rtpthlp_dp1 term   [-]
    C2b     = 1.300000_core_rknd,    & ! High Skewness in C2 Skw. Function   [-]
    C2c     = 5.000000_core_rknd,    & ! Degree of Slope of C2 Skw. Function [-]
    C4      = 5.200000_core_rknd,    & ! Used only when l_tke_aniso is true  [-]
    C5      = 0.300000_core_rknd,    & ! Coef. in pressure terms: w'^2 eqn   [-]
    C6rt    = 4.000000_core_rknd,    & ! Low Skewness in C6rt Skw. Function  [-]
    C6rtb   = 6.000000_core_rknd,    & ! High Skewness in C6rt Skw. Function [-]
    C6rtc   = 1.000000_core_rknd,    & ! Degree of Slope of C6rt Skw. Fnct.  [-]
    C6thl   = 4.000000_core_rknd,    & ! Low Skewness in C6thl Skw. Function [-]
    C6thlb  = 6.000000_core_rknd,    & ! High Skewness in C6thl Skw. Fnct.   [-]
    C6thlc  = 1.000000_core_rknd,    & ! Degree of Slope of C6thl Skw. Fnct. [-]
    C7      = 0.500000_core_rknd,    & ! Low Skewness in C7 Skw. Function    [-]
    C7b     = 0.500000_core_rknd,    & ! High Skewness in C7 Skw. Function   [-]
    C7c     = 0.500000_core_rknd,    & ! Degree of Slope of C7 Skw. Function [-]
    C8      = 4.200000_core_rknd,    & ! Coef. #1 in C8 Skewness Equation    [-]
    C8b     = 0.000000_core_rknd,    & ! Coef. #2 in C8 Skewness Equation    [-]
    C10     = 3.300000_core_rknd,    & ! Currently Not Used in the Model     [-]

    C11     = 0.70000_core_rknd,     & ! Low Skewness in C11 Skw. Function   [-]
    C11b    = 0.350000_core_rknd,    & ! High Skewness in C11 Skw. Function  [-]




    C11c    = 0.500000_core_rknd,    & ! Degree of Slope of C11 Skw. Fnct.   [-]
    C12     = 1.000000_core_rknd,    & ! Constant in w'^3 Crank-Nich. diff.  [-]
    C13     = 0.100000_core_rknd,    & ! Not currently used in model         [-]
    C14     = 1.000000_core_rknd,    & ! Constant for u'^2 and v'^2 terms    [-]
    C15     = 0.4_core_rknd            ! Coefficient for the wp3_bp2 term    [-]
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
  real( kind = core_rknd ), public :: & 
    c_K         = 0.200000_core_rknd, & ! Constant C_mu^(1/4) in DD 1987 [m^2/s]
    c_K1        = 0.750000_core_rknd, & ! Coef. of Eddy Diffusion: wp2   [m^2/s]
    c_K2        = 0.125000_core_rknd, & ! Coef. of Eddy Diffusion: xp2   [m^2/s]
    c_K6        = 0.375000_core_rknd, & ! Coef. of Eddy Diffusion: wpxp  [m^2/s]
    c_K8        = 1.250000_core_rknd, & ! Coef. of Eddy Diffusion: wp3   [m^2/s]
    c_K9        = 0.250000_core_rknd, & ! Coef. of Eddy Diff.: up2/vp2   [m^2/s]
    c_K_hm      = 0.750000_core_rknd, & ! Coef. of Eddy Diffusion: hmm   [m^2/s]
    c_K_hmb     = 0.10000_core_rknd,  & ! Coef. of Non-Local Factor, Eddy Diffusion: hmm   [m^2/s]
    K_hm_min_coef = 0.10000_core_rknd,& ! Min. of Non-Local Factor, Eddy Diffusion: hmm   [m^2/s]
    gamma_coef  = 0.290000_core_rknd, & ! Low Skw.: gamma coef. Skw. Fnct.   [-]
    gamma_coefb = 0.320000_core_rknd, & ! High Skw.: gamma coef. Skw. Fnct.  [-]
    gamma_coefc = 5.000000_core_rknd, & ! Deg. Slope: gamma coef. Skw. Fnct. [-]



    mu          = 1.000E-3_core_rknd, & ! Fract entrain rate per unit alt  [1/m]




    mult_coef   = 1.000000_core_rknd, & ! Coef. applied to log(avg dz/thresh)[-]

    taumin      = 90.00000_core_rknd, & ! Min. allow. value: time-scale tau  [s]
    taumax      = 3600.000_core_rknd, & ! Max. allow. value: time-scale tau  [s]
    lmin        = 20.00000_core_rknd    ! Min. value for the length scale    [m]
!$omp threadprivate(c_K, c_K1, c_K2, c_K6, &
!$omp   c_K8, c_K9, c_K_hm, c_K_hmb, K_hm_min_coef, gamma_coef, gamma_coefb, gamma_coefc, &
!$omp   mu, mult_coef, taumin, taumax, lmin)

  real( kind = core_rknd ), public :: &
    Lscale_mu_coef   = 2.0_core_rknd, & ! Coef perturb mu: av calc Lscale    [-]
    Lscale_pert_coef = 0.1_core_rknd    ! Coef pert thlm/rtm: av calc Lscale [-]
!$omp threadprivate(Lscale_mu_coef, Lscale_pert_coef)

  real( kind = core_rknd ), public :: &
    alpha_corr = 0.15_core_rknd   ! Coef. for the corr. diagnosis algorithm  [-]

!$omp threadprivate(alpha_corr)

  real( kind = core_rknd ), private :: & 
    nu1   = 20.00000_core_rknd, & ! Bg. Coef. Eddy Diffusion: wp2        [m^2/s]
    nu2   = 5.000000_core_rknd, & ! Bg. Coef. Eddy Diffusion: xp2        [m^2/s]
    nu6   = 5.000000_core_rknd, & ! Bg. Coef. Eddy Diffusion: wpxp       [m^2/s]
    nu8   = 20.00000_core_rknd, & ! Bg. Coef. Eddy Diffusion: wp3        [m^2/s]
    nu9   = 20.00000_core_rknd, & ! Bg. Coef. Eddy Diffusion: up2/vp2    [m^2/s]
    nu10  = 0.000000_core_rknd, & ! Bg. Coef. Eddy Diffusion: edsclrm    [m^2/s]
    nu_hm = 1.500000_core_rknd    ! Bg. Coef. Eddy Diffusion: hmm        [m^2/s]
!$omp threadprivate(nu1, nu2, nu6, nu8, nu9, nu10, nu_hm)


  real( kind = core_rknd ), public, allocatable, dimension(:) :: & 
    nu1_vert_res_dep,   & ! Background Coef. of Eddy Diffusion: wp2      [m^2/s]
    nu2_vert_res_dep,   & ! Background Coef. of Eddy Diffusion: xp2      [m^2/s]
    nu6_vert_res_dep,   & ! Background Coef. of Eddy Diffusion: wpxp     [m^2/s]
    nu8_vert_res_dep,   & ! Background Coef. of Eddy Diffusion: wp3      [m^2/s]
    nu9_vert_res_dep,   & ! Background Coef. of Eddy Diffusion: up2/vp2  [m^2/s]
    nu10_vert_res_dep,  & ! Background Coef. of Eddy Diffusion: edsclrm  [m^2/s]
    nu_hm_vert_res_dep    ! Background Coef. of Eddy Diffusion: hydromet [m^2/s]

!$omp threadprivate(nu1_vert_res_dep, nu2_vert_res_dep, nu6_vert_res_dep, &
!$omp   nu8_vert_res_dep, nu9_vert_res_dep, nu10_vert_res_dep, nu_hm_vert_res_dep)

  ! Vince Larson added a constant to set plume widths for theta_l and rt
  ! beta should vary between 0 and 3.

  real( kind = core_rknd ), public :: &
    beta = 2.400000_core_rknd    ! Beta coefficient     [-]

!$omp threadprivate(beta)

  real( kind = core_rknd ), private :: &
    lmin_coef = 0.100000_core_rknd    ! Coefficient of lmin    [-]

!$omp threadprivate(lmin_coef)

  ! Coefficient for adjusted overall correlation in hm_1/hm_2 calculation [-]
  real( kind = core_rknd ), public :: &
    coef_hm_1_hm_2_corr_adj = 1.0_core_rknd

!$omp threadprivate( coef_hm_1_hm_2_corr_adj )

  ! Factor to decrease sensitivity in the denominator of Skw calculation
  real( kind = core_rknd ), public :: &




    Skw_denom_coef = 0.0_core_rknd





!$omp threadprivate( Skw_denom_coef )

  ! Coefficient of Kh_zm
  real( kind = core_rknd ), public :: &
    c_K10 = 0.3_core_rknd

!$omp threadprivate( c_K10 )

  real( kind = core_rknd ), public :: &
    thlp2_rad_coef = 1.0_core_rknd, &            ! Coefficient of thlp2_rad                   [-]
    thlp2_rad_cloud_frac_thresh = 0.1_core_rknd ! Minimum cloud fraction for computation
                                                 ! of thlp2_rad                               [-]

!$omp threadprivate( thlp2_rad_coef, thlp2_rad_cloud_frac_thresh )

  ! used in adj_low_res_nu. If .true., avg_deltaz = deltaz



  logical, public :: l_prescribed_avg_deltaz = .false.


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
  character(len=27), dimension(nparams), parameter, public ::  & 
  params_list = & 
     (/"C1                         ", "C1b                        ", &
       "C1c                        ", "C2                         ", &
       "C2b                        ", "C2c                        ", &
       "C2rt                       ", "C2thl                      ", &
       "C2rtthl                    ", "C4                         ", &
       "C5                         ", "C6rt                       ", &
       "C6rtb                      ", "C6rtc                      ", &
       "C6thl                      ", "C6thlb                     ", &
       "C6thlc                     ", "C7                         ", &
       "C7b                        ", "C7c                        ", &
       "C8                         ", "C8b                        ", &
       "C10                        ", "C11                        ", &
       "C11b                       ", "C11c                       ", &
       "C12                        ", "C13                        ", &
       "C14                        ", "C15                        ", &
       "C6rt_Lscale0               ", "C6thl_Lscale0              ", &
       "C7_Lscale0                 ", "wpxp_L_thresh              ", &
       "c_K                        ", "c_K1                       ", &
       "nu1                        ", "c_K2                       ", &
       "nu2                        ", "c_K6                       ", &
       "nu6                        ", "c_K8                       ", &
       "nu8                        ", "c_K9                       ", &
       "nu9                        ", "nu10                       ", &
       "c_K_hm                     ", "c_K_hmb                    ", &
       "K_hm_min_coef              ", "nu_hm                      ", &
       "gamma_coef                 ", "gamma_coefb                ", &
       "gamma_coefc                ", "mu                         ", &
       "beta                       ", "lmin_coef                  ", &
       "coef_hm_1_hm_2_corr_adj    ", "mult_coef                  ", &
       "taumin                     ", "taumax                     ", &
       "Lscale_mu_coef             ", "Lscale_pert_coef           ", &
       "alpha_corr                 ", "Skw_denom_coef             ", &
       "c_K10                      ", "thlp2_rad_coef             ", &
       "thlp2_rad_cloud_frac_thresh"                                /)

  real( kind = core_rknd ), parameter, private :: &
    init_value = -999._core_rknd ! Initial value for the parameters, used to detect missing values

  PUBLIC kr_externs_in_parameters_tunable 
  PUBLIC kr_externs_out_parameters_tunable 
  REAL(KIND=core_rknd) :: kgenref_lmin               = 20.00000_core_rknd 
  REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_nu1_vert_res_dep, kgenref_nu2_vert_res_dep, &
  &kgenref_nu6_vert_res_dep, kgenref_nu8_vert_res_dep, kgenref_nu9_vert_res_dep, kgenref_nu10_vert_res_dep, &
  &kgenref_nu_hm_vert_res_dep 
  PUBLIC kv_externs_parameters_tunable 
  contains

  !=============================================================================
  subroutine setup_parameters & 
            ( deltaz, params, nzmax, &
              grid_type, momentum_heights, thermodynamic_heights, &
              err_code )

    ! Description:
    ! Subroutine to setup model parameters

    ! References:
    ! None
    !-----------------------------------------------------------------------

      USE constants_clubb, ONLY: fstderr 

      USE error_code, ONLY: clubb_var_out_of_bounds, clubb_no_error 

      USE clubb_precision, ONLY: core_rknd 

    implicit none


    ! Constant Parameters
    real( kind = core_rknd ), parameter :: &
      lmin_deltaz = 40.0_core_rknd ! Fixed value for minimum value for the length scale.

    ! Input Variables
    real( kind = core_rknd ), intent(in) ::  & 
      deltaz  ! Change per height level        [m]

    real( kind = core_rknd ), intent(in), dimension(nparams) :: & 
      params  ! Tuneable model parameters      [-]

    ! Grid definition
    integer, intent(in) :: nzmax  ! Vertical grid levels            [#]

    ! If CLUBB is running on its own, this option determines
    ! if it is using:
    ! 1) an evenly-spaced grid,
    ! 2) a stretched (unevenly-spaced) grid entered on the
    !    thermodynamic grid levels (with momentum levels set
    !    halfway between thermodynamic levels), or
    ! 3) a stretched (unevenly-spaced) grid entered on the
    !    momentum grid levels (with thermodynamic levels set
    !    halfway between momentum levels).
    integer, intent(in) :: grid_type

    ! If the CLUBB parameterization is implemented in a host model,
    ! it needs to use the host model's momentum level altitudes
    ! and thermodynamic level altitudes.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on thermodynamic levels (grid_type = 2),
    ! it needs to use the thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on momentum levels (grid_type = 3),
    ! it needs to use the momentum level altitudes as input.
    real( kind = core_rknd ), intent(in), dimension(nzmax) :: &
      momentum_heights,      & ! Momentum level altitudes (input)      [m]
      thermodynamic_heights    ! Thermodynamic level altitudes (input) [m]

    ! Output Variables
    integer, intent(out) ::  &
      err_code ! Error condition

    !-------------------- Begin code --------------------

    call unpack_parameters( params, & 
                            C1, C1b, C1c, C2, C2b, C2c, C2rt, C2thl, C2rtthl, &
                            C4, C5, C6rt, C6rtb, C6rtc, C6thl, C6thlb, C6thlc, &
                            C7, C7b, C7c, C8, C8b, C10, & 
                            C11, C11b, C11c, C12, C13, C14, C15, & 
                            C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh, &
                            c_K, c_K1, nu1, c_K2, nu2, c_K6, nu6, & 
                            c_K8, nu8, c_K9, nu9, nu10, c_K_hm, c_K_hmb, K_hm_min_coef, &
                            nu_hm, gamma_coef, gamma_coefb, gamma_coefc, & 
                            mu, beta, lmin_coef, coef_hm_1_hm_2_corr_adj, mult_coef, taumin, &
                            taumax, Lscale_mu_coef, Lscale_pert_coef, alpha_corr, &
                            Skw_denom_coef, c_K10, thlp2_rad_coef, &
                            thlp2_rad_cloud_frac_thresh )


    ! It was decided after some experimentation, that the best
    ! way to produce grid independent results is to set lmin to be
    ! some fixed value. -dschanen 21 May 2007
    !lmin = lmin_coef * deltaz  ! Old
    lmin = lmin_coef * lmin_deltaz ! New fixed value

    ! ### Adjust Constant Diffusivity Coefficients Based On Grid Spacing ###
    call adj_low_res_nu &
           ( nzmax, grid_type, deltaz,  & ! Intent(in)
             momentum_heights, thermodynamic_heights )   ! Intent(in)

    ! Sanity check
    ! Initialize err_code to clubb_no_error.  Only overwrite it if a variable
    ! out-of-bounds error is found.
    err_code = clubb_no_error

    if ( beta < 0.0_core_rknd .or. beta > 3.0_core_rknd ) then

       ! Constraints on beta
       write(fstderr,*) "beta = ", beta
       write(fstderr,*) "beta cannot be < 0 or > 3"
       err_code = clubb_var_out_of_bounds

    endif ! beta < 0 or beta > 3

    if ( coef_hm_1_hm_2_corr_adj < 0.0_core_rknd &
         .or. coef_hm_1_hm_2_corr_adj > 1.0_core_rknd ) then

       ! Constraints on coef_hm_1_hm_2_corr_adj
       write(fstderr,*) "coef_hm_1_hm_2_corr_adj = ", coef_hm_1_hm_2_corr_adj
       write(fstderr,*) "coef_hm_1_hm_2_corr_adj cannot be < 0 or > 1"
       err_code = clubb_var_out_of_bounds

    endif ! beta < 0 or beta > 3

    if ( mu < 0.0_core_rknd ) then

       ! Constraints on entrainment rate, mu.
       write(fstderr,*) "mu = ", mu
       write(fstderr,*) "mu cannot be < 0"
       err_code = clubb_var_out_of_bounds

    endif ! mu < 0.0

    if ( lmin < 4.0_core_rknd ) then

       ! Constraints on mixing length
       write(fstderr,*) "lmin = ", lmin
       write(fstderr,*) "lmin is < 4.0_core_rknd"
       err_code = clubb_var_out_of_bounds

    endif ! lmin < 4.0

!    write(*,nml=initvars) ! %% debug


    return

  end subroutine setup_parameters

  !=============================================================================
  subroutine adj_low_res_nu &
               ( nzmax, grid_type, deltaz, & ! Intent(in)
                 momentum_heights, thermodynamic_heights )  ! Intent(in)

    ! Description:
    !   Adjust the values of background eddy diffusivity based on
    !   vertical grid spacing.
    !   This code was made into a public subroutine so that it may be
    !   called multiple times per model run in scenarios where grid
    !   altitudes, and hence average grid spacing, change through space
    !   and/or time.  This occurs, for example, when CLUBB is
    !   implemented in WRF.  --ldgrant Jul 2010
    !----------------------------------------------------------------------

      USE constants_clubb, ONLY: fstderr 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Constant Parameters

    ! Flag for adjusting the values of the constant background eddy diffusivity
    ! coefficients based on the average vertical grid spacing.  If this flag is
    ! turned off, the values of the various nu coefficients will remain as they
    ! are declared in the tunable_parameters.in file.
    logical, parameter :: l_adj_low_res_nu = .true.

    ! The size of the average vertical grid spacing that serves as a threshold
    ! for when to increase the size of the background eddy diffusivity
    ! coefficients (nus) by a certain factor above what the background
    ! coefficients are specified to be in tunable_parameters.in.  At any average
    ! grid spacing at or below this value, the values of the background
    ! diffusivities remain the same.  However, at any average vertical grid
    ! spacing above this value, the values of the background eddy diffusivities
    ! are increased.  Traditionally, the threshold grid spacing has been set to
    ! 40.0 meters.  This is only relevant if l_adj_low_res_nu is turned on.
    real( kind = core_rknd ), parameter :: &
      grid_spacing_thresh = 40.0_core_rknd  ! grid spacing threshold  [m]

    ! Input Variables

    ! Grid definition
    integer, intent(in) :: nzmax  ! Vertical grid levels            [#]

    ! If CLUBB is running on it's own, this option determines
    ! if it is using:
    ! 1) an evenly-spaced grid,
    ! 2) a stretched (unevenly-spaced) grid entered on the
    !    thermodynamic grid levels (with momentum levels set
    !    halfway between thermodynamic levels), or
    ! 3) a stretched (unevenly-spaced) grid entered on the
    !    momentum grid levels (with thermodynamic levels set
    !    halfway between momentum levels).
    integer, intent(in) :: grid_type

    real( kind = core_rknd ), intent(in) ::  & 
      deltaz  ! Change per height level        [m]

    ! If the CLUBB parameterization is implemented in a host model,
    ! it needs to use the host model's momentum level altitudes
    ! and thermodynamic level altitudes.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on thermodynamic levels (grid_type = 2),
    ! it needs to use the thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a
    ! stretched grid entered on momentum levels (grid_type = 3),
    ! it needs to use the momentum level altitudes as input.
    real( kind = core_rknd ), intent(in), dimension(nzmax) :: &
      momentum_heights,      & ! Momentum level altitudes (input)      [m]
      thermodynamic_heights    ! Thermodynamic level altitudes (input) [m]

    ! Local Variables
    real( kind = core_rknd ) :: avg_deltaz  ! Average grid box height   [m]

    ! The factor by which to multiply the coefficients of background eddy
    ! diffusivity if the grid spacing threshold is exceeded and l_adj_low_res_nu
    ! is turned on.
    real( kind = core_rknd ),dimension(gr%nz) :: &
      mult_factor_zt, &  ! Uses gr%dzt for nu values on zt levels
      mult_factor_zm     ! Uses gr%dzm for nu values on zm levels

    ! Flag to enable nu values that are a function of grid spacing
    logical, parameter :: l_nu_grid_dependent = .false.

    integer :: k  ! Loop variable

    !--------------- Begin code -------------------------

    if ( .not. allocated( nu1_vert_res_dep ) ) then
      allocate( nu1_vert_res_dep(1:gr%nz) )
    end if
    if ( .not. allocated( nu2_vert_res_dep ) ) then
      allocate( nu2_vert_res_dep(1:gr%nz) )
    end if
    if ( .not. allocated( nu6_vert_res_dep ) ) then
      allocate( nu6_vert_res_dep(1:gr%nz) )
    end if
    if ( .not. allocated( nu8_vert_res_dep ) ) then
      allocate( nu8_vert_res_dep(1:gr%nz) )
    end if
    if ( .not. allocated( nu9_vert_res_dep ) ) then
      allocate( nu9_vert_res_dep(1:gr%nz) )
    end if
    if ( .not. allocated( nu10_vert_res_dep ) ) then
      allocate( nu10_vert_res_dep(1:gr%nz) )
    end if
    if ( .not. allocated( nu_hm_vert_res_dep ) ) then
      allocate( nu_hm_vert_res_dep(1:gr%nz) )
    end if

    ! Flag for adjusting the values of the constant diffusivity coefficients
    ! based on the grid spacing.  If this flag is turned off, the values of the
    ! various nu coefficients will remain as they are declared in the
    ! parameters.in file.
    if ( l_adj_low_res_nu ) then

      ! ### Adjust Constant Diffusivity Coefficients Based On Grid Spacing ###

      ! All of the background coefficients of eddy diffusivity, as well as the
      ! constant coefficient for 4th-order hyper-diffusion, must be adjusted
      ! based on the size of the grid spacing.  For a case that uses an
      ! evenly-spaced grid, the adjustment is based on the constant grid
      ! spacing deltaz.  For a case that uses a stretched grid, the adjustment
      ! is based on avg_deltaz, which is the average grid spacing over the
      ! vertical domain.
 
      if ( l_prescribed_avg_deltaz ) then
        
        avg_deltaz = deltaz

      else if ( grid_type == 3 ) then

        ! CLUBB is implemented in a host model, or is using grid_type = 3

        ! Find the average deltaz over the grid based on momentum level
        ! inputs.

        avg_deltaz  &
           = ( momentum_heights(nzmax) - momentum_heights(1) )  &
             / real( nzmax - 1, kind = core_rknd )

      else if ( grid_type == 1 ) then

        ! Evenly-spaced grid.

        avg_deltaz = deltaz

      else if ( grid_type == 2 ) then

        ! Stretched (unevenly-spaced) grid:  stretched thermodynamic level
        ! input.

        ! Find the average deltaz over the stretched grid based on
        ! thermodynamic level inputs.

        avg_deltaz  &
          = ( thermodynamic_heights(nzmax) - thermodynamic_heights(1) )  &
             / real( nzmax - 1, kind = core_rknd )
      else
        ! Eric Raut added to remove compiler warning. (Obviously, this value is not used)
        avg_deltaz = 0.0_core_rknd
        write(fstderr,*) "Invalid grid_type:", grid_type
        stop "Fatal error"

      end if ! grid_type

      ! The nu's are chosen for deltaz <= 40 m. Looks like they must
      ! be adjusted for larger grid spacings (Vince Larson)
      if( .not. l_nu_grid_dependent ) then
        ! Use a constant mult_factor so nu does not depend on grid spacing
        if( avg_deltaz > grid_spacing_thresh ) then
          mult_factor_zt = 1.0_core_rknd + mult_coef * log( avg_deltaz / grid_spacing_thresh )
          mult_factor_zm = mult_factor_zt
        else
          mult_factor_zt = 1.0_core_rknd
          mult_factor_zm = 1.0_core_rknd
        end if
      else  ! l_nu_grid_dependent = .true.
        ! mult_factor will vary to create nu values that vary with grid spacing
        do k = 1, gr%nz
          if( gr%dzm(k) > grid_spacing_thresh ) then
            mult_factor_zm(k) = 1.0_core_rknd + mult_coef * log( gr%dzm(k) / grid_spacing_thresh )
          else
            mult_factor_zm(k) = 1.0_core_rknd
          end if

          if( gr%dzt(k) > grid_spacing_thresh ) then
            mult_factor_zt(k) = 1.0_core_rknd + mult_coef * log( gr%dzt(k) / grid_spacing_thresh )
          else
            mult_factor_zt(k) = 1.0_core_rknd
          end if
        end do
      end if ! l_nu_grid_dependent

      !mult_factor = 1.0_core_rknd + mult_coef * log( avg_deltaz / grid_spacing_thresh )
      nu1_vert_res_dep   =  nu1 * mult_factor_zm
      nu2_vert_res_dep   =  nu2 * mult_factor_zm
      nu6_vert_res_dep   =  nu6 * mult_factor_zm
      nu8_vert_res_dep   =  nu8 * mult_factor_zt
      nu9_vert_res_dep   =  nu9 * mult_factor_zm
      nu10_vert_res_dep  =  nu10 * mult_factor_zt !We're unsure of the grid
      nu_hm_vert_res_dep =  nu_hm * mult_factor_zt

    else ! nu values are not adjusted

      nu1_vert_res_dep   =  nu1
      nu2_vert_res_dep   =  nu2
      nu6_vert_res_dep   =  nu6
      nu8_vert_res_dep   =  nu8
      nu9_vert_res_dep   =  nu9
      nu10_vert_res_dep  = nu10
      nu_hm_vert_res_dep = nu_hm

    end if  ! l_adj_low_res_nu

    return
  end subroutine adj_low_res_nu

  !=============================================================================
  subroutine read_parameters( iunit, filename, params )

    ! Description:
    ! Read a namelist containing the model parameters

    ! References:
    ! None
    !-----------------------------------------------------------------------
      USE constants_clubb, ONLY: fstderr 

    implicit none

    ! Input variables
    integer, intent(in) :: iunit

    character(len=*), intent(in) :: filename

    ! Output variables
    real( kind = core_rknd ), intent(out), dimension(nparams) :: params

    ! Local variables
    integer :: i

    logical :: l_error

    ! ---- Begin Code ----

    ! If the filename is empty, assume we're using a `working' set of
    ! parameters that are set statically here (handy for host models).
    ! Read the namelist
    if ( filename /= "" ) then
      ! Read the namelist
      open(unit=iunit, file=filename, status='old', action='read')


      close(unit=iunit)

    end if

    ! Put the variables in the output array
    call pack_parameters( C1, C1b, C1c, C2, C2b, C2c, C2rt, C2thl, C2rtthl, &
                          C4, C5, C6rt, C6rtb, C6rtc, C6thl, C6thlb, C6thlc, &
                          C7, C7b, C7c, C8, C8b, C10, &
                          C11, C11b, C11c, C12, C13, C14, C15, &
                          C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh, &
                          c_K, c_K1, nu1, c_K2, nu2, c_K6, nu6,  &
                          c_K8, nu8, c_K9, nu9, nu10, c_K_hm, c_K_hmb, K_hm_min_coef, &
                          nu_hm, gamma_coef, gamma_coefb, gamma_coefc, &
                          mu, beta, lmin_coef, coef_hm_1_hm_2_corr_adj, mult_coef, &
                          taumin, taumax, Lscale_mu_coef, Lscale_pert_coef, alpha_corr, &
                          Skw_denom_coef, c_K10, thlp2_rad_coef, &
                          thlp2_rad_cloud_frac_thresh, params )

    l_error = .false.

    do i = 1, nparams
      if ( params(i) == init_value ) then
        write(fstderr,*) "Tuning parameter "//trim( params_list(i) )// &
          " was missing from "//trim( filename )
        l_error = .true.
      end if
    end do

    if ( l_error ) stop "Fatal error."

    return

  end subroutine read_parameters

  !=============================================================================




  !=============================================================================
  subroutine pack_parameters &
             ( C1, C1b, C1c, C2, C2b, C2c, C2rt, C2thl, C2rtthl, &
               C4, C5, C6rt, C6rtb, C6rtc, C6thl, C6thlb, C6thlc, &
               C7, C7b, C7c, C8, C8b, C10, &
               C11, C11b, C11c, C12, C13, C14, C15, &
               C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh, &
               c_K, c_K1, nu1, c_K2, nu2, c_K6, nu6,  &
               c_K8, nu8, c_K9, nu9, nu10, c_K_hm, c_K_hmb, K_hm_min_coef, &
               nu_hm, gamma_coef, gamma_coefb, gamma_coefc, &
               mu, beta, lmin_coef, coef_hm_1_hm_2_corr_adj, mult_coef, &
               taumin, taumax, Lscale_mu_coef, Lscale_pert_coef, alpha_corr, &
               Skw_denom_coef, c_K10, thlp2_rad_coef, &
               thlp2_rad_cloud_frac_thresh, params )

    ! Description:
    ! Takes the list of scalar variables and puts them into a 1D vector.
    ! It is here for the purpose of keeping the code generalized
    ! when new variables are added.

    ! References:
    ! None
    !-----------------------------------------------------------------------

      USE parameter_indices, ONLY: ic1, ic1b, ic1c, ic2, ic2b, ic2c, ic2rt, ic2thl, ic2rtthl, ic4, ic5, ic6rt, ic6rtb, ic6rtc, &
      &ic6thl, ic6thlb, ic6thlc, ic7, ic7b, ic7c, ic8, ic8b, ic10, ic11, ic11b, ic11c, ic12, ic13, ic14, ic15 

      USE parameter_indices, ONLY: ic6rt_lscale0, ic6thl_lscale0, ic7_lscale0, iwpxp_l_thresh 

      USE parameter_indices, ONLY: ic_k, ic_k1, inu1, ic_k2, inu2, ic_k6, inu6, ic_k8, inu8, ic_k9, inu9, inu10, ic_k_hm, &
      &ic_k_hmb, ik_hm_min_coef, inu_hm, igamma_coef, igamma_coefb, igamma_coefc, imu, ibeta, ilmin_coef, &
      &icoef_hm_1_hm_2_corr_adj, imult_coef, itaumin, itaumax, ilscale_mu_coef, ilscale_pert_coef, ialpha_corr, iskw_denom_coef, &
      &ic_k10, ithlp2_rad_coef, ithlp2_rad_cloud_frac_thresh, nparams 

    implicit none

    ! Input variables
    real( kind = core_rknd ), intent(in) :: & 
      C1, C1b, C1c, C2, C2b, C2c, C2rt, C2thl, C2rtthl, & 
      C4, C5, C6rt, C6rtb, C6rtc, C6thl, C6thlb, C6thlc, & 
      C7, C7b, C7c, C8, C8b, C10, & 
      C11, C11b, C11c, C12, C13, C14, C15, & 
      C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh, &
      c_K, c_K1, nu1, c_K2, nu2, c_K6, nu6, c_K8, nu8,  & 
      c_K9, nu9, nu10, c_K_hm, c_K_hmb, K_hm_min_coef, nu_hm, gamma_coef, &
      gamma_coefb, gamma_coefc, mu, beta, lmin_coef, coef_hm_1_hm_2_corr_adj, &
      mult_coef, taumin, taumax, Lscale_mu_coef, Lscale_pert_coef, &
      alpha_corr, Skw_denom_coef, c_K10, thlp2_rad_coef, &
      thlp2_rad_cloud_frac_thresh

    ! Output variables
    real( kind = core_rknd ), intent(out), dimension(nparams) :: params

    params(iC1)      = C1
    params(iC1b)     = C1b
    params(iC1c)     = C1c
    params(iC2)      = C2
    params(iC2b)     = C2b
    params(iC2c)     = C2c
    params(iC2rt)    = C2rt
    params(iC2thl)   = C2thl
    params(iC2rtthl) = C2rtthl
    params(iC4)      = C4
    params(iC5)      = C5
    params(iC6rt)    = C6rt
    params(iC6rtb)   = C6rtb
    params(iC6rtc)   = C6rtc
    params(iC6thl)   = C6thl
    params(iC6thlb)  = C6thlb
    params(iC6thlc)  = C6thlc
    params(iC7)      = C7
    params(iC7b)     = C7b
    params(iC7c)     = C7c
    params(iC8)      = C8
    params(iC8b)     = C8b
    params(iC10)     = C10
    params(iC11)     = C11
    params(iC11b)    = C11b
    params(iC11c)    = C11c
    params(iC12)     = C12
    params(iC13)     = C13
    params(iC14)     = C14
    params(iC15)     = C15

    params(iC6rt_Lscale0)       = C6rt_Lscale0
    params(iC6thl_Lscale0)      = C6thl_Lscale0
    params(iC7_Lscale0)         = C7_Lscale0
    params(iwpxp_L_thresh)    = wpxp_L_thresh

    params(ic_K)       = c_K
    params(ic_K1)      = c_K1
    params(inu1)       = nu1
    params(ic_K2)      = c_K2
    params(inu2)       = nu2
    params(ic_K6)      = c_K6
    params(inu6)       = nu6
    params(ic_K8)      = c_K8
    params(inu8)       = nu8
    params(ic_K9)      = c_K9
    params(inu9)       = nu9
    params(inu10)      = nu10
    params(ic_K_hm)    = c_K_hm
    params(ic_K_hmb)   = c_K_hmb
    params(iK_hm_min_coef)   = K_hm_min_coef
    params(inu_hm)     = nu_hm

    params(igamma_coef)  = gamma_coef
    params(igamma_coefb) = gamma_coefb
    params(igamma_coefc) = gamma_coefc

    params(imu) = mu

    params(ibeta) = beta

    params(ilmin_coef) = lmin_coef

    params(icoef_hm_1_hm_2_corr_adj) = coef_hm_1_hm_2_corr_adj

    params(imult_coef) = mult_coef

    params(itaumin) = taumin
    params(itaumax) = taumax

    params(iLscale_mu_coef) = Lscale_mu_coef
    params(iLscale_pert_coef) = Lscale_pert_coef
    params(ialpha_corr) = alpha_corr
    params(iSkw_denom_coef) = Skw_denom_coef
    params(ic_K10) = c_K10
    params(ithlp2_rad_coef) = thlp2_rad_coef
    params(ithlp2_rad_cloud_frac_thresh) = thlp2_rad_cloud_frac_thresh

    return
  end subroutine pack_parameters

  !=============================================================================
  subroutine unpack_parameters & 
             ( params, & 
               C1, C1b, C1c, C2, C2b, C2c, C2rt, C2thl, C2rtthl, & 
               C4, C5, C6rt, C6rtb, C6rtc, C6thl, C6thlb, C6thlc, & 
               C7, C7b, C7c, C8, C8b, C10, & 
               C11, C11b, C11c, C12, C13, C14, C15, & 
               C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh, &
               c_K, c_K1, nu1, c_K2, nu2, c_K6, nu6, & 
               c_K8, nu8, c_K9, nu9, nu10, c_K_hm, c_K_hmb, K_hm_min_coef, &
               nu_hm, gamma_coef, gamma_coefb, gamma_coefc, & 
               mu, beta, lmin_coef, coef_hm_1_hm_2_corr_adj, mult_coef, taumin, &
               taumax, Lscale_mu_coef, Lscale_pert_coef, alpha_corr, &
               Skw_denom_coef, c_K10, thlp2_rad_coef, &
               thlp2_rad_cloud_frac_thresh )

    ! Description:
    ! Takes the 1D vector and returns the list of scalar variables.
    ! Here for the purposes of keeping the code generalized
    ! when new variables are added.

    ! References:
    ! None
    !-----------------------------------------------------------------------

      USE parameter_indices, ONLY: ic1, ic1b, ic1c, ic2, ic2b, ic2c, ic2rt, ic2thl, ic2rtthl, ic4, ic5, ic6rt, ic6rtb, ic6rtc, &
      &ic6thl, ic6thlb, ic6thlc, ic7, ic7b, ic7c, ic8, ic8b, ic10, ic11, ic11b, ic11c, ic12, ic13, ic14, ic15 

      USE parameter_indices, ONLY: ic6rt_lscale0, ic6thl_lscale0, ic7_lscale0, iwpxp_l_thresh 

      USE parameter_indices, ONLY: ic_k, ic_k1, inu1, ic_k2, inu2, ic_k6, inu6, ic_k8, inu8, ic_k9, inu9, inu10, ic_k_hm, &
      &ic_k_hmb, ik_hm_min_coef, inu_hm, igamma_coef, igamma_coefb, igamma_coefc, imu, ibeta, ilmin_coef, &
      &icoef_hm_1_hm_2_corr_adj, imult_coef, itaumin, itaumax, ilscale_mu_coef, ilscale_pert_coef, ialpha_corr, iskw_denom_coef, &
      &ic_k10, ithlp2_rad_coef, ithlp2_rad_cloud_frac_thresh, nparams 

    implicit none

    ! Input variables
    real( kind = core_rknd ), intent(in), dimension(nparams) :: params

    ! Output variables
    real( kind = core_rknd ), intent(out) :: & 
      C1, C1b, C1c, C2, C2b, C2c, C2rt, C2thl, C2rtthl, & 
      C4, C5, C6rt, C6rtb, C6rtc, C6thl, C6thlb, C6thlc, & 
      C7, C7b, C7c, C8, C8b, C10, & 
      C11, C11b, C11c, C12, C13, C14, C15, & 
      C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh, &
      c_K, c_K1, nu1, c_K2, nu2, c_K6, nu6, & 
      c_K8, nu8, c_K9, nu9, nu10, c_K_hm, c_K_hmb, K_hm_min_coef, nu_hm, & 
      gamma_coef, gamma_coefb, gamma_coefc, & 
      mu, beta, lmin_coef, coef_hm_1_hm_2_corr_adj, mult_coef, taumin, taumax, &
      Lscale_mu_coef, Lscale_pert_coef, alpha_corr, Skw_denom_coef, c_K10, &
      thlp2_rad_coef, thlp2_rad_cloud_frac_thresh

    C1      = params(iC1)
    C1b     = params(iC1b)
    C1c     = params(iC1c)
    C2      = params(iC2)
    C2b     = params(iC2b)
    C2c     = params(iC2c)
    C2rt    = params(iC2rt)
    C2thl   = params(iC2thl)
    C2rtthl = params(iC2rtthl)
    C4      = params(iC4)
    C5      = params(iC5)
    C6rt    = params(iC6rt)
    C6rtb   = params(iC6rtb)
    C6rtc   = params(iC6rtc)
    C6thl   = params(iC6thl)
    C6thlb  = params(iC6thlb)
    C6thlc  = params(iC6thlc)
    C7      = params(iC7)
    C7b     = params(iC7b)
    C7c     = params(iC7c)
    C8      = params(iC8)
    C8b     = params(iC8b)
    C10     = params(iC10)
    C11     = params(iC11)
    C11b    = params(iC11b)
    C11c    = params(iC11c)
    C12     = params(iC12)
    C13     = params(iC13)
    C14     = params(iC14)
    C15     = params(iC15)

    C6rt_Lscale0       = params(iC6rt_Lscale0)
    C6thl_Lscale0      = params(iC6thl_Lscale0)
    C7_Lscale0         = params(iC7_Lscale0)
    wpxp_L_thresh    = params(iwpxp_L_thresh)

    c_K       = params(ic_K)
    c_K1      = params(ic_K1)
    nu1       = params(inu1)
    c_K2      = params(ic_K2)
    nu2       = params(inu2)
    c_K6      = params(ic_K6)
    nu6       = params(inu6)
    c_K8      = params(ic_K8)
    nu8       = params(inu8)
    c_K9      = params(ic_K9)
    nu9       = params(inu9)
    nu10      = params(inu10)
    c_K_hm    = params(ic_K_hm)
    c_K_hmb   = params(ic_K_hmb)
    K_hm_min_coef   = params(iK_hm_min_coef)
    nu_hm     = params(inu_hm)

    gamma_coef  = params(igamma_coef)
    gamma_coefb = params(igamma_coefb)
    gamma_coefc = params(igamma_coefc)

    mu = params(imu)

    beta = params(ibeta)

    lmin_coef = params(ilmin_coef)

    coef_hm_1_hm_2_corr_adj = params(icoef_hm_1_hm_2_corr_adj)

    mult_coef = params(imult_coef)

    taumin = params(itaumin)
    taumax = params(itaumax)

    Lscale_mu_coef = params(iLscale_mu_coef)
    Lscale_pert_coef = params(iLscale_pert_coef)
    alpha_corr = params(ialpha_corr)
    Skw_denom_coef = params(iSkw_denom_coef)
    c_K10 = params(ic_K10)

    thlp2_rad_coef = params(ithlp2_rad_coef)
    thlp2_rad_cloud_frac_thresh = params(ithlp2_rad_cloud_frac_thresh)

    return
  end subroutine unpack_parameters

  !=============================================================================


  !=============================================================================


  !=============================================================================


!===============================================================================

  !read state subroutine for kr_externs_in_parameters_tunable 
  SUBROUTINE kr_externs_in_parameters_tunable(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) c6rtb 
      READ (UNIT = kgen_unit) c6rtc 
      READ (UNIT = kgen_unit) c2 
      READ (UNIT = kgen_unit) c1 
      READ (UNIT = kgen_unit) c2b 
      READ (UNIT = kgen_unit) c2c 
      READ (UNIT = kgen_unit) c13 
      READ (UNIT = kgen_unit) c12 
      READ (UNIT = kgen_unit) c11 
      READ (UNIT = kgen_unit) c10 
      READ (UNIT = kgen_unit) c6rt 
      READ (UNIT = kgen_unit) c15 
      READ (UNIT = kgen_unit) c14 
      READ (UNIT = kgen_unit) c11c 
      READ (UNIT = kgen_unit) c11b 
      READ (UNIT = kgen_unit) c6thl 
      READ (UNIT = kgen_unit) c8b 
      READ (UNIT = kgen_unit) c8 
      READ (UNIT = kgen_unit) c2rt 
      READ (UNIT = kgen_unit) c6thlc 
      READ (UNIT = kgen_unit) c6thlb 
      READ (UNIT = kgen_unit) c7 
      READ (UNIT = kgen_unit) c5 
      READ (UNIT = kgen_unit) c4 
      READ (UNIT = kgen_unit) c1c 
      READ (UNIT = kgen_unit) c1b 
      READ (UNIT = kgen_unit) c2rtthl 
      READ (UNIT = kgen_unit) c7c 
      READ (UNIT = kgen_unit) c7b 
      READ (UNIT = kgen_unit) c2thl 
      READ (UNIT = kgen_unit) c6thl_lscale0 
      READ (UNIT = kgen_unit) wpxp_l_thresh 
      READ (UNIT = kgen_unit) c6rt_lscale0 
      READ (UNIT = kgen_unit) c7_lscale0 
      READ (UNIT = kgen_unit) taumin 
      READ (UNIT = kgen_unit) k_hm_min_coef 
      READ (UNIT = kgen_unit) c_k_hmb 
      READ (UNIT = kgen_unit) c_k9 
      READ (UNIT = kgen_unit) c_k8 
      READ (UNIT = kgen_unit) c_k6 
      READ (UNIT = kgen_unit) c_k 
      READ (UNIT = kgen_unit) c_k_hm 
      READ (UNIT = kgen_unit) c_k2 
      READ (UNIT = kgen_unit) c_k1 
      READ (UNIT = kgen_unit) gamma_coefc 
      READ (UNIT = kgen_unit) mu 
      READ (UNIT = kgen_unit) mult_coef 
      READ (UNIT = kgen_unit) gamma_coefb 
      READ (UNIT = kgen_unit) lmin 
      READ (UNIT = kgen_unit) taumax 
      READ (UNIT = kgen_unit) gamma_coef 
      READ (UNIT = kgen_unit) lscale_pert_coef 
      READ (UNIT = kgen_unit) lscale_mu_coef 
      READ (UNIT = kgen_unit) alpha_corr 
      READ (UNIT = kgen_unit) nu_hm 
      READ (UNIT = kgen_unit) nu8 
      READ (UNIT = kgen_unit) nu9 
      READ (UNIT = kgen_unit) nu6 
      READ (UNIT = kgen_unit) nu2 
      READ (UNIT = kgen_unit) nu1 
      READ (UNIT = kgen_unit) nu10 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu8_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu2_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu9_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu1_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu_hm_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu6_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(nu10_vert_res_dep, kgen_unit) 
      READ (UNIT = kgen_unit) beta 
      READ (UNIT = kgen_unit) lmin_coef 
      READ (UNIT = kgen_unit) coef_hm_1_hm_2_corr_adj 
      READ (UNIT = kgen_unit) skw_denom_coef 
      READ (UNIT = kgen_unit) c_k10 
      READ (UNIT = kgen_unit) thlp2_rad_coef 
      READ (UNIT = kgen_unit) thlp2_rad_cloud_frac_thresh 
      READ (UNIT = kgen_unit) l_prescribed_avg_deltaz 
  END SUBROUTINE kr_externs_in_parameters_tunable 
    
  !read state subroutine for kr_externs_out_parameters_tunable 
  SUBROUTINE kr_externs_out_parameters_tunable(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      READ (UNIT = kgen_unit) kgenref_lmin 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu8_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu2_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu9_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu1_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu_hm_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu6_vert_res_dep, kgen_unit) 
      CALL kr_parameters_tunable_real__core_rknd_dim1(kgenref_nu10_vert_res_dep, kgen_unit) 
  END SUBROUTINE kr_externs_out_parameters_tunable 
    
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
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck("nu8_vert_res_dep", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
          IF (PRESENT( printvar )) THEN 
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
          END IF   
      END IF   
  END SUBROUTINE kr_parameters_tunable_real__core_rknd_dim1 
    
  !verify state subroutine for kv_externs_parameters_tunable 
  SUBROUTINE kv_externs_parameters_tunable(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_parameters_tunable_real__core_rknd("lmin", check_status, lmin, kgenref_lmin) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu8_vert_res_dep", check_status, nu8_vert_res_dep, &
      &kgenref_nu8_vert_res_dep) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu2_vert_res_dep", check_status, nu2_vert_res_dep, &
      &kgenref_nu2_vert_res_dep) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu9_vert_res_dep", check_status, nu9_vert_res_dep, &
      &kgenref_nu9_vert_res_dep) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu1_vert_res_dep", check_status, nu1_vert_res_dep, &
      &kgenref_nu1_vert_res_dep) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu_hm_vert_res_dep", check_status, nu_hm_vert_res_dep, &
      &kgenref_nu_hm_vert_res_dep) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu6_vert_res_dep", check_status, nu6_vert_res_dep, &
      &kgenref_nu6_vert_res_dep) 
      CALL kv_parameters_tunable_real__core_rknd_dim1("nu10_vert_res_dep", check_status, nu10_vert_res_dep, &
      &kgenref_nu10_vert_res_dep) 
  END SUBROUTINE kv_externs_parameters_tunable 
    
  !verify state subroutine for kv_parameters_tunable_real__core_rknd 
  RECURSIVE SUBROUTINE kv_parameters_tunable_real__core_rknd(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      REAL(KIND=core_rknd), INTENT(IN) :: var, kgenref_var 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=core_rknd) :: diff 
        
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
        
  END SUBROUTINE kv_parameters_tunable_real__core_rknd 
    
  !verify state subroutine for kv_parameters_tunable_real__core_rknd_dim1 
  RECURSIVE SUBROUTINE kv_parameters_tunable_real__core_rknd_dim1(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      REAL(KIND=core_rknd), allocatable, INTENT(IN), DIMENSION(:) :: var, kgenref_var 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: idx1 
      INTEGER :: n 
      real(KIND=core_rknd) :: nrmsdiff, rmsdiff 
      real(KIND=core_rknd), ALLOCATABLE :: buf1(:), buf2(:) 
        
      IF (ALLOCATED(var)) THEN 
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
            
      END IF   
  END SUBROUTINE kv_parameters_tunable_real__core_rknd_dim1 
    
end module parameters_tunable