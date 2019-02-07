!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:36 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id: stats_type.F90 6952 2014-06-17 15:59:47Z schemena@uwm.edu $
!===============================================================================


module stats_type
  ! Description:
  !   Contains derived data type 'stats'.
  !   Used for storing output statistics to disk.
  !-----------------------------------------------------------------------


    USE stat_file_module, ONLY: stat_file 

    USE clubb_precision, ONLY: stat_rknd, stat_nknd, core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE stat_file_module, ONLY: kr_stat_file_module_stat_file 
    USE stat_file_module, ONLY: kv_stat_file_module_stat_file 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC stats 
  ! Derived data types to store GrADS/netCDF statistics

  type stats
    ! Number of fields to sample

    integer ::  num_output_fields    ! Number of variables being output to disk (e.g.
                     ! cloud_frac, rain rate, etc.)

    integer :: &
      ii, & ! Horizontal extent of the variables (Usually 1 for the single-column model)
      jj, & ! Horizontal extent of the variables (Usually 1 for the single-column model)
      kk    ! Vertical extent of the variables (Usually gr%nz from grid_class)
    ! Vertical levels

    real( kind = core_rknd ), allocatable, dimension(:) :: z ! altitude [m]
    ! Array to store sampled fields


    real(kind=stat_rknd), allocatable, dimension(:,:,:,:) :: accum_field_values
        ! The variable accum_field_values contains the cumulative sums
        ! of accum_num_samples sample values of each
        ! of the num_output_fields (e.g. the sum of the sampled rain rate values)

    integer(kind=stat_nknd), allocatable, dimension(:,:,:,:) :: accum_num_samples
        ! accum_num_samples is the number of samples for each of the num_output_fields fields
        ! and each of the kk vertical levels
    ! Tracks if a field is in the process of an update

    logical, allocatable, dimension(:,:,:,:) :: l_in_update
    ! Data for GrADS / netCDF output


    type (stat_file) ::  file

  end type stats
  PUBLIC kr_stat_file_module_stat_file 
  PUBLIC kr_stats_type_stats 
  PUBLIC kv_stat_file_module_stat_file 
  PUBLIC kv_stats_type_stats 
    
  CONTAINS 
    

  !read state subroutine for kr_stats_type_stats 
  RECURSIVE SUBROUTINE kr_stats_type_stats(var, kgen_unit, printname, printvar) 
      TYPE(stats), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%num_output_fields 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%num_output_fields = ", var%num_output_fields 
      END IF   
        
      READ (UNIT = kgen_unit) var%ii 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ii = ", var%ii 
      END IF   
      READ (UNIT = kgen_unit) var%jj 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%jj = ", var%jj 
      END IF   
      READ (UNIT = kgen_unit) var%kk 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%kk = ", var%kk 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_stats_real__core_rknd_dim1(var%z, kgen_unit, printname // "%z", .TRUE.) 
      ELSE 
          CALL kr_stats_real__core_rknd_dim1(var%z, kgen_unit, printname // "%z", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_stats_real__stat_rknd_dim4(var%accum_field_values, kgen_unit, printname // "%accum_field_values", .TRUE.) 
      ELSE 
          CALL kr_stats_real__stat_rknd_dim4(var%accum_field_values, kgen_unit, printname // "%accum_field_values", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_stats_integer__stat_nknd_dim4(var%accum_num_samples, kgen_unit, printname // "%accum_num_samples", .TRUE.) 
      ELSE 
          CALL kr_stats_integer__stat_nknd_dim4(var%accum_num_samples, kgen_unit, printname // "%accum_num_samples", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_stats_logical___dim4(var%l_in_update, kgen_unit, printname // "%l_in_update", .TRUE.) 
      ELSE 
          CALL kr_stats_logical___dim4(var%l_in_update, kgen_unit, printname // "%l_in_update", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_stat_file_module_stat_file(var%file, kgen_unit, printname // "%file", .TRUE.) 
      ELSE 
          CALL kr_stat_file_module_stat_file(var%file, kgen_unit, printname // "%file", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_stats_type_stats 
    
  !write state subroutine for kr_stats_real__core_rknd_dim1 
  SUBROUTINE kr_stats_real__core_rknd_dim1(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
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
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_stats_real__core_rknd_dim1 
    
  !write state subroutine for kr_stats_real__stat_rknd_dim4 
  SUBROUTINE kr_stats_real__stat_rknd_dim4(var, kgen_unit, printname, printvar) 
      REAL(KIND=stat_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3, idx4 
      INTEGER, DIMENSION(2,4) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          READ (UNIT = kgen_unit) kgen_bound(1, 3) 
          READ (UNIT = kgen_unit) kgen_bound(2, 3) 
          READ (UNIT = kgen_unit) kgen_bound(1, 4) 
          READ (UNIT = kgen_unit) kgen_bound(2, 4) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3), &
          &kgen_bound(1,4):kgen_bound(2,4))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_stats_real__stat_rknd_dim4 
    
  !write state subroutine for kr_stats_integer__stat_nknd_dim4 
  SUBROUTINE kr_stats_integer__stat_nknd_dim4(var, kgen_unit, printname, printvar) 
      INTEGER(KIND=stat_nknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3, idx4 
      INTEGER, DIMENSION(2,4) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          READ (UNIT = kgen_unit) kgen_bound(1, 3) 
          READ (UNIT = kgen_unit) kgen_bound(2, 3) 
          READ (UNIT = kgen_unit) kgen_bound(1, 4) 
          READ (UNIT = kgen_unit) kgen_bound(2, 4) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3), &
          &kgen_bound(1,4):kgen_bound(2,4))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_stats_integer__stat_nknd_dim4 
    
  !write state subroutine for kr_stats_logical___dim4 
  SUBROUTINE kr_stats_logical___dim4(var, kgen_unit, printname, printvar) 
      LOGICAL, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3, idx4 
      INTEGER, DIMENSION(2,4) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          READ (UNIT = kgen_unit) kgen_bound(1, 3) 
          READ (UNIT = kgen_unit) kgen_bound(2, 3) 
          READ (UNIT = kgen_unit) kgen_bound(1, 4) 
          READ (UNIT = kgen_unit) kgen_bound(2, 4) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3), &
          &kgen_bound(1,4):kgen_bound(2,4))) 
          READ (UNIT = kgen_unit) var 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // " = ", var 
          END IF   
      END IF   
        
  END SUBROUTINE kr_stats_logical___dim4 
    
  !verify state subroutine for kv_stats_type_stats 
  RECURSIVE SUBROUTINE kv_stats_type_stats(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(stats), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_num_output_fields 
      integer :: diff_ii 
      integer :: diff_jj 
      integer :: diff_kk 
      INTEGER :: n_z 
      real(KIND=core_rknd) :: nrmsdiff_z, rmsdiff_z 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_z(:), buf2_z(:) 
      INTEGER :: n_accum_field_values 
      real(KIND=stat_rknd) :: nrmsdiff_accum_field_values, rmsdiff_accum_field_values 
      real(KIND=stat_rknd), ALLOCATABLE :: buf1_accum_field_values(:,:,:,:), buf2_accum_field_values(:,:,:,:) 
      INTEGER :: n_accum_num_samples 
      integer(KIND=stat_nknd) :: nrmsdiff_accum_num_samples, rmsdiff_accum_num_samples 
      integer(KIND=stat_nknd), ALLOCATABLE :: buf1_accum_num_samples(:,:,:,:), buf2_accum_num_samples(:,:,:,:) 
      INTEGER :: n_l_in_update 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%num_output_fields == kgenref_var%num_output_fields) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%num_output_fields is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_num_output_fields = ABS(var%num_output_fields - kgenref_var%num_output_fields) 
          IF (diff_num_output_fields <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%num_output_fields is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%num_output_fields is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_num_output_fields 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_num_output_fields 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ii == kgenref_var%ii) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ii is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ii = ABS(var%ii - kgenref_var%ii) 
          IF (diff_ii <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ii is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ii is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_ii 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ii 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%jj == kgenref_var%jj) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jj is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_jj = ABS(var%jj - kgenref_var%jj) 
          IF (diff_jj <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%jj is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%jj is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_jj 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_jj 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%kk == kgenref_var%kk) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%kk is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_kk = ABS(var%kk - kgenref_var%kk) 
          IF (diff_kk <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%kk is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%kk is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_kk 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_kk 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ALLOCATED(var%z)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%z == kgenref_var%z)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%z is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_z(SIZE(var%z,dim=1))) 
              ALLOCATE (buf2_z(SIZE(var%z,dim=1))) 
              n_z = COUNT(var%z /= kgenref_var%z) 
              WHERE ( ABS(kgenref_var%z) > kgen_minvalue ) 
                  buf1_z = ((var%z-kgenref_var%z)/kgenref_var%z)**2 
                  buf2_z = (var%z-kgenref_var%z)**2 
              ELSEWHERE 
                  buf1_z = (var%z-kgenref_var%z)**2 
                  buf2_z = buf1_z 
              END WHERE   
              nrmsdiff_z = SQRT(SUM(buf1_z)/REAL(n_z)) 
              rmsdiff_z = SQRT(SUM(buf2_z)/REAL(n_z)) 
              IF (rmsdiff_z > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%z is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%z is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%z /= kgenref_var%z), " of ", size( var%z ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%z)/real(size(var%z)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%z)/real(size(kgenref_var%z)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_z 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_z 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%z /= kgenref_var%z), " of ", size( var%z ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%z)/real(size(var%z)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%z)/real(size(kgenref_var%z)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_z 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_z 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%accum_field_values)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%accum_field_values == kgenref_var%accum_field_values)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%accum_field_values is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_accum_field_values(SIZE(var%accum_field_values,dim=1),SIZE(var%accum_field_values,dim=2),SIZE(var%accum_field&
              &_values,dim=3),SIZE(var%accum_field_values,dim=4))) 
              ALLOCATE &
              &(buf2_accum_field_values(SIZE(var%accum_field_values,dim=1),SIZE(var%accum_field_values,dim=2),SIZE(var%accum_field&
              &_values,dim=3),SIZE(var%accum_field_values,dim=4))) 
              n_accum_field_values = COUNT(var%accum_field_values /= kgenref_var%accum_field_values) 
              WHERE ( ABS(kgenref_var%accum_field_values) > kgen_minvalue ) 
                  buf1_accum_field_values = &
                  &((var%accum_field_values-kgenref_var%accum_field_values)/kgenref_var%accum_field_values)**2 
                  buf2_accum_field_values = (var%accum_field_values-kgenref_var%accum_field_values)**2 
              ELSEWHERE 
                  buf1_accum_field_values = (var%accum_field_values-kgenref_var%accum_field_values)**2 
                  buf2_accum_field_values = buf1_accum_field_values 
              END WHERE   
              nrmsdiff_accum_field_values = SQRT(SUM(buf1_accum_field_values)/REAL(n_accum_field_values)) 
              rmsdiff_accum_field_values = SQRT(SUM(buf2_accum_field_values)/REAL(n_accum_field_values)) 
              IF (rmsdiff_accum_field_values > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%accum_field_values is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%accum_field_values is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%accum_field_values /= kgenref_var%accum_field_values), " of ", size( &
                      &var%accum_field_values ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%accum_field_values)/real(size(var%accum_field_values)) 
                      WRITE (*, *) "Average - reference ", &
                      &sum(kgenref_var%accum_field_values)/real(size(kgenref_var%accum_field_values)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_accum_field_values 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_field_values 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%accum_field_values /= kgenref_var%accum_field_values), " of ", size( &
                      &var%accum_field_values ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%accum_field_values)/real(size(var%accum_field_values)) 
                      WRITE (*, *) "Average - reference ", &
                      &sum(kgenref_var%accum_field_values)/real(size(kgenref_var%accum_field_values)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_accum_field_values 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_field_values 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%accum_num_samples)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%accum_num_samples == kgenref_var%accum_num_samples)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%accum_num_samples is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_accum_num_samples(SIZE(var%accum_num_samples,dim=1),SIZE(var%accum_num_samples,dim=2),SIZE(var%accum_num_samp&
              &les,dim=3),SIZE(var%accum_num_samples,dim=4))) 
              ALLOCATE &
              &(buf2_accum_num_samples(SIZE(var%accum_num_samples,dim=1),SIZE(var%accum_num_samples,dim=2),SIZE(var%accum_num_samp&
              &les,dim=3),SIZE(var%accum_num_samples,dim=4))) 
              n_accum_num_samples = COUNT(var%accum_num_samples /= kgenref_var%accum_num_samples) 
              WHERE ( ABS(kgenref_var%accum_num_samples) > kgen_minvalue ) 
                  buf1_accum_num_samples = &
                  &((var%accum_num_samples-kgenref_var%accum_num_samples)/kgenref_var%accum_num_samples)**2 
                  buf2_accum_num_samples = (var%accum_num_samples-kgenref_var%accum_num_samples)**2 
              ELSEWHERE 
                  buf1_accum_num_samples = (var%accum_num_samples-kgenref_var%accum_num_samples)**2 
                  buf2_accum_num_samples = buf1_accum_num_samples 
              END WHERE   
              nrmsdiff_accum_num_samples = SQRT(SUM(buf1_accum_num_samples)/REAL(n_accum_num_samples)) 
              rmsdiff_accum_num_samples = SQRT(SUM(buf2_accum_num_samples)/REAL(n_accum_num_samples)) 
              IF (rmsdiff_accum_num_samples > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%accum_num_samples is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%accum_num_samples is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%accum_num_samples /= kgenref_var%accum_num_samples), " of ", size( &
                      &var%accum_num_samples ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%accum_num_samples)/real(size(var%accum_num_samples)) 
                      WRITE (*, *) "Average - reference ", &
                      &sum(kgenref_var%accum_num_samples)/real(size(kgenref_var%accum_num_samples)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_accum_num_samples 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_num_samples 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%accum_num_samples /= kgenref_var%accum_num_samples), " of ", size( &
                      &var%accum_num_samples ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%accum_num_samples)/real(size(var%accum_num_samples)) 
                      WRITE (*, *) "Average - reference ", &
                      &sum(kgenref_var%accum_num_samples)/real(size(kgenref_var%accum_num_samples)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_accum_num_samples 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_num_samples 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%l_in_update)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%l_in_update .EQV. kgenref_var%l_in_update)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%l_in_update is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              n_l_in_update = COUNT(var%l_in_update .NEQV. kgenref_var%l_in_update) 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%l_in_update is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED YET" 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED YET" 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_stat_file_module_stat_file("file", comp_check_status, var%file, kgenref_var%file) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%file", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%file is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%file is NOT IDENTICAL(within tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
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
  END SUBROUTINE kv_stats_type_stats 
    
end module stats_type

