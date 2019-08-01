!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:55 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id$
!===============================================================================


module parameters_silhs
! Description:
!   Parameters for None!
! References:
!   None
!-------------------------------------------------------------------------


    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
  ! Cluster allocation strategies!!!

  integer, parameter, public :: &
    ! All eight categories, effectively no clustering
    eight_cluster_allocation_opt = 1, &
    ! Four clusters for the combinations of cloud/no cloud and component 1/2.
    ! Precipitation fraction is ignored.
    four_cluster_allocation_opt  = 2, &
    ! Two clusters, one containing all categories with either cloud or precip,
    ! and the other containing categories with neither
    two_cluster_cp_nocp_opt      = 3
    ! All eight categories, effectively no clustering
    ! Four clusters for the combinations of cloud/no cloud and component 1/2.
    ! Precipitation fraction is ignored.
    ! Two clusters, one containing all categories with either cloud or precip,
    ! and the other containing categories with neither

  integer, public :: &
    cluster_allocation_strategy = two_cluster_cp_nocp_opt
  ! The following type defines parameters that control the sample point
  ! allocation for the clustered sampling scheme
  ! (l_lh_clustered_sampling = .true.).

  !$omp threadprivate( cluster_allocation_strategy )

  type eight_cluster_presc_probs_type

    real( kind = core_rknd ) :: &
      cloud_precip_comp1      = 0.15_core_rknd, &
      cloud_precip_comp2      = 0.15_core_rknd, &
      nocloud_precip_comp1    = 0.15_core_rknd, &
      nocloud_precip_comp2    = 0.15_core_rknd, &
      cloud_noprecip_comp1    = 0.15_core_rknd, &
      cloud_noprecip_comp2    = 0.15_core_rknd, &
      nocloud_noprecip_comp1  = 0.05_core_rknd, &
      nocloud_noprecip_comp2  = 0.05_core_rknd

  end type eight_cluster_presc_probs_type
  ! Flags for the None sampling code 

  LOGICAL, public :: l_lh_importance_sampling     = .true., l_lscale_vert_avg                   = .true., l_lh_straight_mc       &
  &              = .false., l_lh_clustered_sampling       = .true., l_rcm_in_cloud_k_lh_start   = .true., l_random_k_lh_start     &
  &          = .false., l_max_overlap_in_cloud         = .true. 
                                              !  hypercube sampling and no importance sampling
                                              !  scheme with prescribed probabilities
                                              !  rcm
                                              !  maximum rcm and maximum rcm_in_cloud
                                              !  exceeds cloud threshold
                                              !  microphysical source terms, ignoring
                                              !  discretization effects

  !$omp threadprivate( l_lh_importance_sampling, l_Lscale_vert_avg, l_lh_straight_mc, &
  !$omp                l_lh_clustered_sampling, l_rcm_in_cloud_k_lh_start, l_random_k_lh_start, &
  !$omp                l_max_overlap_in_cloud, l_lh_instant_var_covar_src )

  type(eight_cluster_presc_probs_type), public, save :: &
    eight_cluster_presc_probs                 ! Prescribed probabilities for
                                              ! l_lh_clustered_sampling = .true.

  !$omp threadprivate( eight_cluster_presc_probs )

  logical, public :: &
    l_lh_limit_weights = .true. , &           ! Limit None sample point weights for stability
    l_lh_var_frac      = .false., &           ! Prescribe variance fractions
    l_lh_normalize_weights = .true.           ! Scale sample point weights to sum to num_samples
                                              ! (the "ratio estimate")

  !$omp threadprivate( l_lh_limit_weights, l_lh_var_frac, l_lh_normalize_weights )

  real( kind = core_rknd ), public :: &
    importance_prob_thresh = 1.0e-8_core_rknd, & ! Minimum PDF probability of category for
                                                 ! importance sampling
    vert_decorr_coef       = 0.03_core_rknd      ! Empirically defined de-correlation constant [-]
                                                 ! importance sampling

  !$omp threadprivate( importance_prob_thresh, vert_decorr_coef )

  PRIVATE 

  PUBLIC eight_cluster_presc_probs_type 
  PUBLIC kr_externs_in_parameters_silhs 
  PUBLIC kr_kgen_parameters_silhs_typesubp2 
  PUBLIC kv_kgen_parameters_silhs_typesubp2 
    
  CONTAINS 
    

  !read state subroutine for kr_externs_in_parameters_silhs 
  SUBROUTINE kr_externs_in_parameters_silhs(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) cluster_allocation_strategy 
      READ (UNIT = kgen_unit) l_lscale_vert_avg 
      READ (UNIT = kgen_unit) l_rcm_in_cloud_k_lh_start 
      READ (UNIT = kgen_unit) l_lh_straight_mc 
      READ (UNIT = kgen_unit) l_lh_importance_sampling 
      READ (UNIT = kgen_unit) l_random_k_lh_start 
      READ (UNIT = kgen_unit) l_lh_clustered_sampling 
      READ (UNIT = kgen_unit) l_max_overlap_in_cloud 
      CALL kr_kgen_parameters_silhs_typesubp2(eight_cluster_presc_probs, kgen_unit, "eight_cluster_presc_probs", .FALSE.) 
      READ (UNIT = kgen_unit) l_lh_var_frac 
      READ (UNIT = kgen_unit) l_lh_normalize_weights 
      READ (UNIT = kgen_unit) l_lh_limit_weights 
      READ (UNIT = kgen_unit) importance_prob_thresh 
      READ (UNIT = kgen_unit) vert_decorr_coef 
  END SUBROUTINE kr_externs_in_parameters_silhs 
    
  !read state subroutine for kr_kgen_parameters_silhs_typesubp2 
  RECURSIVE SUBROUTINE kr_kgen_parameters_silhs_typesubp2(var, kgen_unit, printname, printvar) 
      TYPE(eight_cluster_presc_probs_type), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%cloud_precip_comp1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cloud_precip_comp1 = ", var%cloud_precip_comp1 
      END IF   
      READ (UNIT = kgen_unit) var%cloud_precip_comp2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cloud_precip_comp2 = ", var%cloud_precip_comp2 
      END IF   
      READ (UNIT = kgen_unit) var%nocloud_precip_comp1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nocloud_precip_comp1 = ", var%nocloud_precip_comp1 
      END IF   
      READ (UNIT = kgen_unit) var%nocloud_precip_comp2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nocloud_precip_comp2 = ", var%nocloud_precip_comp2 
      END IF   
      READ (UNIT = kgen_unit) var%cloud_noprecip_comp1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cloud_noprecip_comp1 = ", var%cloud_noprecip_comp1 
      END IF   
      READ (UNIT = kgen_unit) var%cloud_noprecip_comp2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cloud_noprecip_comp2 = ", var%cloud_noprecip_comp2 
      END IF   
      READ (UNIT = kgen_unit) var%nocloud_noprecip_comp1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nocloud_noprecip_comp1 = ", var%nocloud_noprecip_comp1 
      END IF   
      READ (UNIT = kgen_unit) var%nocloud_noprecip_comp2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nocloud_noprecip_comp2 = ", var%nocloud_noprecip_comp2 
      END IF   
        
  END SUBROUTINE kr_kgen_parameters_silhs_typesubp2 
    
  !verify state subroutine for kv_kgen_parameters_silhs_typesubp2 
  RECURSIVE SUBROUTINE kv_kgen_parameters_silhs_typesubp2(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(eight_cluster_presc_probs_type), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=core_rknd) :: diff_cloud_precip_comp1 
      real(KIND=core_rknd) :: diff_cloud_precip_comp2 
      real(KIND=core_rknd) :: diff_nocloud_precip_comp1 
      real(KIND=core_rknd) :: diff_nocloud_precip_comp2 
      real(KIND=core_rknd) :: diff_cloud_noprecip_comp1 
      real(KIND=core_rknd) :: diff_cloud_noprecip_comp2 
      real(KIND=core_rknd) :: diff_nocloud_noprecip_comp1 
      real(KIND=core_rknd) :: diff_nocloud_noprecip_comp2 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cloud_precip_comp1 == kgenref_var%cloud_precip_comp1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_precip_comp1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cloud_precip_comp1 = ABS(var%cloud_precip_comp1 - kgenref_var%cloud_precip_comp1) 
          IF (diff_cloud_precip_comp1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_precip_comp1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_precip_comp1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_precip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_precip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cloud_precip_comp2 == kgenref_var%cloud_precip_comp2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_precip_comp2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cloud_precip_comp2 = ABS(var%cloud_precip_comp2 - kgenref_var%cloud_precip_comp2) 
          IF (diff_cloud_precip_comp2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_precip_comp2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_precip_comp2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_precip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_precip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nocloud_precip_comp1 == kgenref_var%nocloud_precip_comp1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nocloud_precip_comp1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nocloud_precip_comp1 = ABS(var%nocloud_precip_comp1 - kgenref_var%nocloud_precip_comp1) 
          IF (diff_nocloud_precip_comp1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_precip_comp1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_precip_comp1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_precip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_precip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nocloud_precip_comp2 == kgenref_var%nocloud_precip_comp2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nocloud_precip_comp2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nocloud_precip_comp2 = ABS(var%nocloud_precip_comp2 - kgenref_var%nocloud_precip_comp2) 
          IF (diff_nocloud_precip_comp2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_precip_comp2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_precip_comp2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_precip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_precip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cloud_noprecip_comp1 == kgenref_var%cloud_noprecip_comp1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_noprecip_comp1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cloud_noprecip_comp1 = ABS(var%cloud_noprecip_comp1 - kgenref_var%cloud_noprecip_comp1) 
          IF (diff_cloud_noprecip_comp1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_noprecip_comp1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_noprecip_comp1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_noprecip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_noprecip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cloud_noprecip_comp2 == kgenref_var%cloud_noprecip_comp2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_noprecip_comp2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cloud_noprecip_comp2 = ABS(var%cloud_noprecip_comp2 - kgenref_var%cloud_noprecip_comp2) 
          IF (diff_cloud_noprecip_comp2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_noprecip_comp2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_noprecip_comp2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_noprecip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_noprecip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nocloud_noprecip_comp1 == kgenref_var%nocloud_noprecip_comp1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nocloud_noprecip_comp1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nocloud_noprecip_comp1 = ABS(var%nocloud_noprecip_comp1 - kgenref_var%nocloud_noprecip_comp1) 
          IF (diff_nocloud_noprecip_comp1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_noprecip_comp1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_noprecip_comp1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_noprecip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_noprecip_comp1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nocloud_noprecip_comp2 == kgenref_var%nocloud_noprecip_comp2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nocloud_noprecip_comp2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nocloud_noprecip_comp2 = ABS(var%nocloud_noprecip_comp2 - kgenref_var%nocloud_noprecip_comp2) 
          IF (diff_nocloud_noprecip_comp2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_noprecip_comp2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nocloud_noprecip_comp2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_noprecip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nocloud_noprecip_comp2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_kgen_parameters_silhs_typesubp2 
    
end module parameters_silhs