!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:54:30
!KGEN version : 0.6.1

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
    USE stat_file_module, ONLY: kr_stat_file_module_stat_file
    USE stat_file_module, ONLY: kv_stat_file_module_stat_file
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
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
    real( kind = core_rknd ), pointer, dimension(:) :: z ! altitude [m]

    ! Array to store sampled fields

    real(kind=stat_rknd), pointer, dimension(:,:,:,:) :: accum_field_values
        ! The variable accum_field_values contains the cumulative sums
        ! of accum_num_samples sample values of each
        ! of the num_output_fields (e.g. the sum of the sampled rain rate values)

    integer(kind=stat_nknd), pointer, dimension(:,:,:,:) :: accum_num_samples
        ! accum_num_samples is the number of samples for each of the num_output_fields fields
        ! and each of the kk vertical levels

    ! Tracks if a field is in the process of an update
    logical, pointer, dimension(:,:,:,:) :: l_in_update

    ! Data for GrADS / netCDF output

    type (stat_file) ::  file

  end type stats

  PUBLIC kr_stat_file_module_stat_file
  PUBLIC kr_stats_type_stats
  PUBLIC kv_stat_file_module_stat_file
  PUBLIC kv_stats_type_stats
  
  CONTAINS
  
  !read state subroutine for kr_stats_type_stats
  RECURSIVE SUBROUTINE kr_stats_type_stats(var, kgen_unit, printvar)
      TYPE(stats), INTENT(INOUT) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      READ (UNIT = kgen_unit) var%num_output_fields
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%num_output_fields **" // NEW_LINE("A"), var%num_output_fields
      END IF 
      
      READ (UNIT = kgen_unit) var%ii
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ii **" // NEW_LINE("A"), var%ii
      END IF 
      READ (UNIT = kgen_unit) var%jj
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%jj **" // NEW_LINE("A"), var%jj
      END IF 
      READ (UNIT = kgen_unit) var%kk
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%kk **" // NEW_LINE("A"), var%kk
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kr_stats_real__core_rknd_dim1_ptr(var%z, kgen_unit, printvar // "%z")
      ELSE
          CALL kr_stats_real__core_rknd_dim1_ptr(var%z, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kr_stats_real__stat_rknd_dim4_ptr(var%accum_field_values, kgen_unit, printvar // "%accum_field_values")
      ELSE
          CALL kr_stats_real__stat_rknd_dim4_ptr(var%accum_field_values, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kr_stats_integer__stat_nknd_dim4_ptr(var%accum_num_samples, kgen_unit, printvar // "%accum_num_samples")
      ELSE
          CALL kr_stats_integer__stat_nknd_dim4_ptr(var%accum_num_samples, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kr_stats_logical___dim4_ptr(var%l_in_update, kgen_unit, printvar // "%l_in_update")
      ELSE
          CALL kr_stats_logical___dim4_ptr(var%l_in_update, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kr_stat_file_module_stat_file(var%file, kgen_unit, printvar // "%file")
      ELSE
          CALL kr_stat_file_module_stat_file(var%file, kgen_unit)
      END IF 
      
  END SUBROUTINE kr_stats_type_stats
  
  !write state subroutine for kr_stats_real__core_rknd_dim1_ptr
  SUBROUTINE kr_stats_real__core_rknd_dim1_ptr(var, kgen_unit, printvar)
      REAL(KIND=core_rknd), INTENT(INOUT), POINTER, DIMENSION(:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      INTEGER :: idx1
      INTEGER, DIMENSION(2,1) :: kgen_bound
      
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          IF (ASSOCIATED( var )) THEN
              NULLIFY (var)
          END IF 
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgen_bound(1, 1)
          READ (UNIT = kgen_unit) kgen_bound(2, 1)
          ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
          READ (UNIT = kgen_unit) var
          CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8)
          END IF 
      END IF 
      
  END SUBROUTINE kr_stats_real__core_rknd_dim1_ptr
  
  !write state subroutine for kr_stats_real__stat_rknd_dim4_ptr
  SUBROUTINE kr_stats_real__stat_rknd_dim4_ptr(var, kgen_unit, printvar)
      REAL(KIND=stat_rknd), INTENT(INOUT), POINTER, DIMENSION(:,:,:,:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      INTEGER :: idx1, idx2, idx3, idx4
      INTEGER, DIMENSION(2,4) :: kgen_bound
      
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          IF (ASSOCIATED( var )) THEN
              NULLIFY (var)
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
          ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1, kgen_bound(2,3)-kgen_bound(1,3)+1, kgen_bound(2,4)-kgen_bound(1,4)+1))
          READ (UNIT = kgen_unit) var
          CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8)
          END IF 
      END IF 
      
  END SUBROUTINE kr_stats_real__stat_rknd_dim4_ptr
  
  !write state subroutine for kr_stats_integer__stat_nknd_dim4_ptr
  SUBROUTINE kr_stats_integer__stat_nknd_dim4_ptr(var, kgen_unit, printvar)
      INTEGER(KIND=stat_nknd), INTENT(INOUT), POINTER, DIMENSION(:,:,:,:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      INTEGER :: idx1, idx2, idx3, idx4
      INTEGER, DIMENSION(2,4) :: kgen_bound
      
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          IF (ASSOCIATED( var )) THEN
              NULLIFY (var)
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
          ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1, kgen_bound(2,3)-kgen_bound(1,3)+1, kgen_bound(2,4)-kgen_bound(1,4)+1))
          READ (UNIT = kgen_unit) var
          CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8)
          END IF 
      END IF 
      
  END SUBROUTINE kr_stats_integer__stat_nknd_dim4_ptr
  
  !write state subroutine for kr_stats_logical___dim4_ptr
  SUBROUTINE kr_stats_logical___dim4_ptr(var, kgen_unit, printvar)
      LOGICAL, INTENT(INOUT), POINTER, DIMENSION(:,:,:,:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      INTEGER :: idx1, idx2, idx3, idx4
      INTEGER, DIMENSION(2,4) :: kgen_bound
      
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          IF (ASSOCIATED( var )) THEN
              NULLIFY (var)
          END IF 
          READ (UNIT = kgen_unit) kgen_bound(1, 1)
          READ (UNIT = kgen_unit) kgen_bound(2, 1)
          READ (UNIT = kgen_unit) kgen_bound(1, 2)
          READ (UNIT = kgen_unit) kgen_bound(2, 2)
          READ (UNIT = kgen_unit) kgen_bound(1, 3)
          READ (UNIT = kgen_unit) kgen_bound(2, 3)
          READ (UNIT = kgen_unit) kgen_bound(1, 4)
          READ (UNIT = kgen_unit) kgen_bound(2, 4)
          ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1, kgen_bound(2,3)-kgen_bound(1,3)+1, kgen_bound(2,4)-kgen_bound(1,4)+1))
          READ (UNIT = kgen_unit) var
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // " **" // NEW_LINE("A"), var
          END IF 
      END IF 
      
  END SUBROUTINE kr_stats_logical___dim4_ptr
  
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
      
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%num_output_fields == kgenref_var%num_output_fields) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%num_output_fields is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_num_output_fields = ABS(var%num_output_fields - kgenref_var%num_output_fields)
          IF (diff_num_output_fields <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%num_output_fields is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%num_output_fields is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_num_output_fields
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_num_output_fields
              WRITE (*, *) ""
          END IF 
      END IF 
      
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%ii == kgenref_var%ii) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%ii is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_ii = ABS(var%ii - kgenref_var%ii)
          IF (diff_ii <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ii is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ii is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_ii
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_ii
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%jj == kgenref_var%jj) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%jj is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_jj = ABS(var%jj - kgenref_var%jj)
          IF (diff_jj <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%jj is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%jj is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_jj
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_jj
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%kk == kgenref_var%kk) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%kk is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_kk = ABS(var%kk - kgenref_var%kk)
          IF (diff_kk <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%kk is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%kk is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_kk
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_kk
              WRITE (*, *) ""
          END IF 
      END IF 
      
      IF (ASSOCIATED(var%z)) THEN
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (ALL(var%z == kgenref_var%z)) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%z is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              ALLOCATE (buf1_z(SIZE(var%z,dim=1)))
              ALLOCATE (buf2_z(SIZE(var%z,dim=1)))
              n_z = COUNT(var%z /= kgenref_var%z)
              WHERE ( ABS(kgenref_var%z) > dtype_check_status%minvalue )
                  buf1_z = ((var%z-kgenref_var%z)/kgenref_var%z)**2
                  buf2_z = (var%z-kgenref_var%z)**2
              ELSEWHERE
                  buf1_z = (var%z-kgenref_var%z)**2
                  buf2_z = buf1_z
              END WHERE 
              nrmsdiff_z = SQRT(SUM(buf1_z)/REAL(n_z))
              rmsdiff_z = SQRT(SUM(buf2_z)/REAL(n_z))
              IF (nrmsdiff_z > dtype_check_status%tolerance) THEN
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%z is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              ELSE
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%z is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var%z /= kgenref_var%z), " of ", size( var%z ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var%z)/real(size(var%z))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%z)/real(size(kgenref_var%z))
                  WRITE (*, *) "RMS of difference is ", rmsdiff_z
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_z
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var%z /= kgenref_var%z), " of ", size( var%z ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var%z)/real(size(var%z))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%z)/real(size(kgenref_var%z))
                  WRITE (*, *) "RMS of difference is ", rmsdiff_z
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_z
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END IF 
      IF (ASSOCIATED(var%accum_field_values)) THEN
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (ALL(var%accum_field_values == kgenref_var%accum_field_values)) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%accum_field_values is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              ALLOCATE (buf1_accum_field_values(SIZE(var%accum_field_values,dim=1),SIZE(var%accum_field_values,dim=2),SIZE(var%accum_field_values,dim=3),SIZE(var%accum_field_values,dim=4)))
              ALLOCATE (buf2_accum_field_values(SIZE(var%accum_field_values,dim=1),SIZE(var%accum_field_values,dim=2),SIZE(var%accum_field_values,dim=3),SIZE(var%accum_field_values,dim=4)))
              n_accum_field_values = COUNT(var%accum_field_values /= kgenref_var%accum_field_values)
              WHERE ( ABS(kgenref_var%accum_field_values) > dtype_check_status%minvalue )
                  buf1_accum_field_values = ((var%accum_field_values-kgenref_var%accum_field_values)/kgenref_var%accum_field_values)**2
                  buf2_accum_field_values = (var%accum_field_values-kgenref_var%accum_field_values)**2
              ELSEWHERE
                  buf1_accum_field_values = (var%accum_field_values-kgenref_var%accum_field_values)**2
                  buf2_accum_field_values = buf1_accum_field_values
              END WHERE 
              nrmsdiff_accum_field_values = SQRT(SUM(buf1_accum_field_values)/REAL(n_accum_field_values))
              rmsdiff_accum_field_values = SQRT(SUM(buf2_accum_field_values)/REAL(n_accum_field_values))
              IF (nrmsdiff_accum_field_values > dtype_check_status%tolerance) THEN
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%accum_field_values is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              ELSE
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%accum_field_values is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var%accum_field_values /= kgenref_var%accum_field_values), " of ", size( var%accum_field_values ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var%accum_field_values)/real(size(var%accum_field_values))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%accum_field_values)/real(size(kgenref_var%accum_field_values))
                  WRITE (*, *) "RMS of difference is ", rmsdiff_accum_field_values
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_field_values
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var%accum_field_values /= kgenref_var%accum_field_values), " of ", size( var%accum_field_values ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var%accum_field_values)/real(size(var%accum_field_values))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%accum_field_values)/real(size(kgenref_var%accum_field_values))
                  WRITE (*, *) "RMS of difference is ", rmsdiff_accum_field_values
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_field_values
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END IF 
      IF (ASSOCIATED(var%accum_num_samples)) THEN
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (ALL(var%accum_num_samples == kgenref_var%accum_num_samples)) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%accum_num_samples is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              ALLOCATE (buf1_accum_num_samples(SIZE(var%accum_num_samples,dim=1),SIZE(var%accum_num_samples,dim=2),SIZE(var%accum_num_samples,dim=3),SIZE(var%accum_num_samples,dim=4)))
              ALLOCATE (buf2_accum_num_samples(SIZE(var%accum_num_samples,dim=1),SIZE(var%accum_num_samples,dim=2),SIZE(var%accum_num_samples,dim=3),SIZE(var%accum_num_samples,dim=4)))
              n_accum_num_samples = COUNT(var%accum_num_samples /= kgenref_var%accum_num_samples)
              WHERE ( ABS(kgenref_var%accum_num_samples) > dtype_check_status%minvalue )
                  buf1_accum_num_samples = ((var%accum_num_samples-kgenref_var%accum_num_samples)/kgenref_var%accum_num_samples)**2
                  buf2_accum_num_samples = (var%accum_num_samples-kgenref_var%accum_num_samples)**2
              ELSEWHERE
                  buf1_accum_num_samples = (var%accum_num_samples-kgenref_var%accum_num_samples)**2
                  buf2_accum_num_samples = buf1_accum_num_samples
              END WHERE 
              nrmsdiff_accum_num_samples = SQRT(SUM(buf1_accum_num_samples)/REAL(n_accum_num_samples))
              rmsdiff_accum_num_samples = SQRT(SUM(buf2_accum_num_samples)/REAL(n_accum_num_samples))
              IF (nrmsdiff_accum_num_samples > dtype_check_status%tolerance) THEN
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%accum_num_samples is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              ELSE
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%accum_num_samples is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var%accum_num_samples /= kgenref_var%accum_num_samples), " of ", size( var%accum_num_samples ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var%accum_num_samples)/real(size(var%accum_num_samples))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%accum_num_samples)/real(size(kgenref_var%accum_num_samples))
                  WRITE (*, *) "RMS of difference is ", rmsdiff_accum_num_samples
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_num_samples
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) count( var%accum_num_samples /= kgenref_var%accum_num_samples), " of ", size( var%accum_num_samples ), " elements are different."
                  WRITE (*, *) "Average - kernel ", sum(var%accum_num_samples)/real(size(var%accum_num_samples))
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%accum_num_samples)/real(size(kgenref_var%accum_num_samples))
                  WRITE (*, *) "RMS of difference is ", rmsdiff_accum_num_samples
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_accum_num_samples
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END IF 
      IF (ASSOCIATED(var%l_in_update)) THEN
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (ALL(var%l_in_update .EQV. kgenref_var%l_in_update)) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%l_in_update is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              n_l_in_update = COUNT(var%l_in_update /= kgenref_var%l_in_update)
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%l_in_update is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          END IF 
          
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "NOT IMPLEMENTED YET"
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "NOT IMPLEMENTED YET"
              WRITE (*, *) ""
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

