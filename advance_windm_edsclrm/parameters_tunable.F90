
! KGEN-generated Fortran source file
!
! Filename    : parameters_tunable.F90
! Generated at: 2015-10-21 08:59:10
! KGEN version: 0.5.3



    MODULE parameters_tunable
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
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
! Variable(s)
! Variable(s)
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
! Default to private
        PRIVATE
! Model constant parameters
! Low Skewness in C1 Skw. Function    [-]
! High Skewness in C1 Skw. Function   [-]
! Degree of Slope of C1 Skw. Function [-]
! Low Skewness in C2 Skw. Function    [-]
! C2 coef. for the rtp2_dp1 term      [-]
! C2 coef. for the thlp2_dp1 term     [-]
! C2 coef. for the rtpthlp_dp1 term   [-]
! High Skewness in C2 Skw. Function   [-]
! Degree of Slope of C2 Skw. Function [-]
! Used only when l_tke_aniso is true  [-]
! Coef. in pressure terms: w'^2 eqn   [-]
! Low Skewness in C6rt Skw. Function  [-]
! High Skewness in C6rt Skw. Function [-]
! Degree of Slope of C6rt Skw. Fnct.  [-]
! Low Skewness in C6thl Skw. Function [-]
! High Skewness in C6thl Skw. Fnct.   [-]
! Degree of Slope of C6thl Skw. Fnct. [-]
! Low Skewness in C7 Skw. Function    [-]
! High Skewness in C7 Skw. Function   [-]
! Degree of Slope of C7 Skw. Function [-]
! Coef. #1 in C8 Skewness Equation    [-]
! Coef. #2 in C8 Skewness Equation    [-]
! Currently Not Used in the Model     [-]
! Low Skewness in C11 Skw. Function   [-]
! High Skewness in C11 Skw. Function  [-]
! Degree of Slope of C11 Skw. Fnct.   [-]
! Constant in w'^3 Crank-Nich. diff.  [-]
! Not currently used in model         [-]
! Constant for u'^2 and v'^2 terms    [-]
! Coefficient for the wp3_bp2 term    [-]
!$omp threadprivate(C1, C1b, C1c, C2, C2b, C2c, &
!$omp   C2rt, C2thl, C2rtthl, C4, C5, C6rt, C6rtb, C6rtc, &
!$omp   C6thl, C6thlb, C6thlc, &
!$omp   C7, C7b, C7c, C8, C8b, C10, C11, C11b, C11c, C12, &
!$omp   C13, C14, C15)
! Damp C6rt as a fnct. of Lscale  [-]
! Damp C6thl as a fnct. of Lscale [-]
! Damp C7 as a fnct. of Lscale    [-]
! Lscale threshold: damp C6 & C7  [m]
!$omp threadprivate(C6rt_Lscale0, C6thl_Lscale0, C7_Lscale0, wpxp_L_thresh)
! Note: DD 1987 is Duynkerke & Driedonks (1987).
! Constant C_mu^(1/4) in DD 1987 [m^2/s]
! Coef. of Eddy Diffusion: wp2   [m^2/s]
! Coef. of Eddy Diffusion: xp2   [m^2/s]
! Coef. of Eddy Diffusion: wpxp  [m^2/s]
! Coef. of Eddy Diffusion: wp3   [m^2/s]
! Coef. of Eddy Diff.: up2/vp2   [m^2/s]
! Coef. of Eddy Diffusion: hmm   [m^2/s]
! Coef. of Non-Local Factor, Eddy Diffusion: hmm   [m^2/s]
! Min. of Non-Local Factor, Eddy Diffusion: hmm   [m^2/s]
! Low Skw.: gamma coef. Skw. Fnct.   [-]
! High Skw.: gamma coef. Skw. Fnct.  [-]
! Deg. Slope: gamma coef. Skw. Fnct. [-]
! Fract entrain rate per unit alt  [1/m]
! Coef. applied to log(avg dz/thresh)[-]
! Min. allow. value: time-scale tau  [s]
! Max. allow. value: time-scale tau  [s]
! Min. value for the length scale    [m]
!$omp threadprivate(c_K, c_K1, c_K2, c_K6, &
!$omp   c_K8, c_K9, c_K_hm, c_K_hmb, K_hm_min_coef, gamma_coef, gamma_coefb, gamma_coefc, &
!$omp   mu, mult_coef, taumin, taumax, lmin)
! Coef perturb mu: av calc Lscale    [-]
! Coef pert thlm/rtm: av calc Lscale [-]
!$omp threadprivate(Lscale_mu_coef, Lscale_pert_coef)
! Coef. for the corr. diagnosis algorithm  [-]
!$omp threadprivate(alpha_corr)
! Bg. Coef. Eddy Diffusion: wp2        [m^2/s]
! Bg. Coef. Eddy Diffusion: xp2        [m^2/s]
! Bg. Coef. Eddy Diffusion: wpxp       [m^2/s]
! Bg. Coef. Eddy Diffusion: wp3        [m^2/s]
! Bg. Coef. Eddy Diffusion: up2/vp2    [m^2/s]
! Bg. Coef. Eddy Diffusion: edsclrm    [m^2/s]
! Bg. Coef. Eddy Diffusion: hmm        [m^2/s]
!$omp threadprivate(nu1, nu2, nu6, nu8, nu9, nu10, nu_hm)
        REAL(KIND=core_rknd), public, allocatable, dimension(:) :: nu10_vert_res_dep
! Background Coef. of Eddy Diffusion: wp2      [m^2/s]
! Background Coef. of Eddy Diffusion: xp2      [m^2/s]
! Background Coef. of Eddy Diffusion: wpxp     [m^2/s]
! Background Coef. of Eddy Diffusion: wp3      [m^2/s]
! Background Coef. of Eddy Diffusion: up2/vp2  [m^2/s]
! Background Coef. of Eddy Diffusion: edsclrm  [m^2/s]
! Background Coef. of Eddy Diffusion: hydromet [m^2/s]
!$omp threadprivate(nu1_vert_res_dep, nu2_vert_res_dep, nu6_vert_res_dep, &
!$omp   nu8_vert_res_dep, nu9_vert_res_dep, nu10_vert_res_dep, nu_hm_vert_res_dep)
! Vince Larson added a constant to set plume widths for theta_l and rt
! beta should vary between 0 and 3.
! Beta coefficient     [-]
!$omp threadprivate(beta)
! Coefficient of lmin    [-]
!$omp threadprivate(lmin_coef)
! Coefficient for adjusted overall correlation in hm_1/hm_2 calculation [-]
!$omp threadprivate( coef_hm_1_hm_2_corr_adj )
! Factor to decrease sensitivity in the denominator of Skw calculation
!$omp threadprivate( Skw_denom_coef )
! Coefficient of Kh_zm
!$omp threadprivate( c_K10 )
! Coefficient of thlp2_rad                   [-]
! Minimum cloud fraction for computation
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
! Initial value for the parameters, used to detect missing values
            PUBLIC kgen_read_externs_parameters_tunable
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_core_rknd_dim1_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1
                INTEGER, DIMENSION(2,1) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim1_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_parameters_tunable(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_real_core_rknd_dim1_alloc(nu10_vert_res_dep, kgen_unit)
        END SUBROUTINE kgen_read_externs_parameters_tunable

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!===============================================================================
    END MODULE parameters_tunable
