!-----------------------------------------------------------------------
! $Id: stats_type.F90 6952 2014-06-17 15:59:47Z schemena@uwm.edu $
!===============================================================================
module stats_type

  ! Description:
  !   Contains derived data type 'stats'.
  !   Used for storing output statistics to disk.
  !-----------------------------------------------------------------------

  use stat_file_module, only: & 
      stat_file ! Type

  use clubb_precision, only: & 
      stat_rknd,  & ! Variable(s)
      stat_nknd,  &
      core_rknd

  USE stat_file_module, ONLY: kw_stat_file_module_stat_file
  implicit none

  private ! Set Default Scope

  public :: stats

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

  PUBLIC kw_stat_file_module_stat_file
  PUBLIC kw_stats_type_stats
  
  CONTAINS
  
  !read state subroutine for kw_stats_type_stats
  RECURSIVE SUBROUTINE kw_stats_type_stats(var, kgen_unit, printvar)
      TYPE(stats), INTENT(IN) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) var%num_output_fields
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%num_output_fields **" // NEW_LINE("A"), var%num_output_fields
      END IF 
      
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) var%ii
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ii **" // NEW_LINE("A"), var%ii
      END IF 
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) var%jj
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%jj **" // NEW_LINE("A"), var%jj
      END IF 
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) var%kk
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%kk **" // NEW_LINE("A"), var%kk
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kw_stats_real__core_rknd_dim1_ptr(var%z, kgen_unit, printvar // "%z")
      ELSE
          CALL kw_stats_real__core_rknd_dim1_ptr(var%z, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kw_stats_real__stat_rknd_dim4_ptr(var%accum_field_values, kgen_unit, printvar // "%accum_field_values")
      ELSE
          CALL kw_stats_real__stat_rknd_dim4_ptr(var%accum_field_values, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kw_stats_integer__stat_nknd_dim4_ptr(var%accum_num_samples, kgen_unit, printvar // "%accum_num_samples")
      ELSE
          CALL kw_stats_integer__stat_nknd_dim4_ptr(var%accum_num_samples, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kw_stats_logical___dim4_ptr(var%l_in_update, kgen_unit, printvar // "%l_in_update")
      ELSE
          CALL kw_stats_logical___dim4_ptr(var%l_in_update, kgen_unit)
      END IF 
      
      IF (PRESENT( printvar )) THEN
          CALL kw_stat_file_module_stat_file(var%file, kgen_unit, printvar // "%file")
      ELSE
          CALL kw_stat_file_module_stat_file(var%file, kgen_unit)
      END IF 
      
  END SUBROUTINE kw_stats_type_stats
  
  !write state subroutine for kw_stats_real__core_rknd_dim1_ptr
  SUBROUTINE kw_stats_real__core_rknd_dim1_ptr(var, kgen_unit, printvar)
      REAL(KIND=core_rknd), INTENT(IN), POINTER, DIMENSION(:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      INTEGER :: idx1
      REAL(KIND=8) :: kgen_array_sum
      
      IF (SIZE(var)==1) THEN
          IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN
              kgen_istrue = .FALSE.
          ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN
              kgen_istrue = .FALSE.
          ELSE
              kgen_istrue = .TRUE.
          END IF 
      ELSE IF (SIZE(var)==0) THEN
          kgen_istrue = .FALSE.
      ELSE
          kgen_istrue = .TRUE.
      END IF 
      IF (.NOT. ASSOCIATED(var)) THEN
          kgen_istrue = .FALSE.
      END IF 
      WRITE (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          kgen_array_sum = REAL(SUM(var), 8)
          WRITE (UNIT = kgen_unit) kgen_array_sum
          WRITE (UNIT = kgen_unit) LBOUND(var, 1)
          WRITE (UNIT = kgen_unit) UBOUND(var, 1)
          WRITE (UNIT = kgen_unit) var
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8)
          END IF 
      END IF 
  END SUBROUTINE kw_stats_real__core_rknd_dim1_ptr
  
  !write state subroutine for kw_stats_real__stat_rknd_dim4_ptr
  SUBROUTINE kw_stats_real__stat_rknd_dim4_ptr(var, kgen_unit, printvar)
      REAL(KIND=stat_rknd), INTENT(IN), POINTER, DIMENSION(:,:,:,:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      INTEGER :: idx1, idx2, idx3, idx4
      REAL(KIND=8) :: kgen_array_sum
      
      IF (SIZE(var)==1) THEN
          IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN
              kgen_istrue = .FALSE.
          ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN
              kgen_istrue = .FALSE.
          ELSE
              kgen_istrue = .TRUE.
          END IF 
      ELSE IF (SIZE(var)==0) THEN
          kgen_istrue = .FALSE.
      ELSE
          kgen_istrue = .TRUE.
      END IF 
      IF (.NOT. ASSOCIATED(var)) THEN
          kgen_istrue = .FALSE.
      END IF 
      WRITE (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          kgen_array_sum = REAL(SUM(var), 8)
          WRITE (UNIT = kgen_unit) kgen_array_sum
          WRITE (UNIT = kgen_unit) LBOUND(var, 1)
          WRITE (UNIT = kgen_unit) UBOUND(var, 1)
          WRITE (UNIT = kgen_unit) LBOUND(var, 2)
          WRITE (UNIT = kgen_unit) UBOUND(var, 2)
          WRITE (UNIT = kgen_unit) LBOUND(var, 3)
          WRITE (UNIT = kgen_unit) UBOUND(var, 3)
          WRITE (UNIT = kgen_unit) LBOUND(var, 4)
          WRITE (UNIT = kgen_unit) UBOUND(var, 4)
          WRITE (UNIT = kgen_unit) var
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8)
          END IF 
      END IF 
  END SUBROUTINE kw_stats_real__stat_rknd_dim4_ptr
  
  !write state subroutine for kw_stats_integer__stat_nknd_dim4_ptr
  SUBROUTINE kw_stats_integer__stat_nknd_dim4_ptr(var, kgen_unit, printvar)
      INTEGER(KIND=stat_nknd), INTENT(IN), POINTER, DIMENSION(:,:,:,:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      INTEGER :: idx1, idx2, idx3, idx4
      REAL(KIND=8) :: kgen_array_sum
      
      IF (SIZE(var)==1) THEN
          IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN
              kgen_istrue = .FALSE.
          ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN
              kgen_istrue = .FALSE.
          ELSE
              kgen_istrue = .TRUE.
          END IF 
      ELSE IF (SIZE(var)==0) THEN
          kgen_istrue = .FALSE.
      ELSE
          kgen_istrue = .TRUE.
      END IF 
      IF (.NOT. ASSOCIATED(var)) THEN
          kgen_istrue = .FALSE.
      END IF 
      WRITE (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          kgen_array_sum = REAL(SUM(var), 8)
          WRITE (UNIT = kgen_unit) kgen_array_sum
          WRITE (UNIT = kgen_unit) LBOUND(var, 1)
          WRITE (UNIT = kgen_unit) UBOUND(var, 1)
          WRITE (UNIT = kgen_unit) LBOUND(var, 2)
          WRITE (UNIT = kgen_unit) UBOUND(var, 2)
          WRITE (UNIT = kgen_unit) LBOUND(var, 3)
          WRITE (UNIT = kgen_unit) UBOUND(var, 3)
          WRITE (UNIT = kgen_unit) LBOUND(var, 4)
          WRITE (UNIT = kgen_unit) UBOUND(var, 4)
          WRITE (UNIT = kgen_unit) var
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8)
          END IF 
      END IF 
  END SUBROUTINE kw_stats_integer__stat_nknd_dim4_ptr
  
  !write state subroutine for kw_stats_logical___dim4_ptr
  SUBROUTINE kw_stats_logical___dim4_ptr(var, kgen_unit, printvar)
      LOGICAL, INTENT(IN), POINTER, DIMENSION(:,:,:,:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      INTEGER :: idx1, idx2, idx3, idx4
      REAL(KIND=8) :: kgen_array_sum
      
      IF (SIZE(var)==1) THEN
          IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN
              kgen_istrue = .FALSE.
          ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN
              kgen_istrue = .FALSE.
          ELSE
              kgen_istrue = .TRUE.
          END IF 
      ELSE IF (SIZE(var)==0) THEN
          kgen_istrue = .FALSE.
      ELSE
          kgen_istrue = .TRUE.
      END IF 
      IF (.NOT. ASSOCIATED(var)) THEN
          kgen_istrue = .FALSE.
      END IF 
      WRITE (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          WRITE (UNIT = kgen_unit) LBOUND(var, 1)
          WRITE (UNIT = kgen_unit) UBOUND(var, 1)
          WRITE (UNIT = kgen_unit) LBOUND(var, 2)
          WRITE (UNIT = kgen_unit) UBOUND(var, 2)
          WRITE (UNIT = kgen_unit) LBOUND(var, 3)
          WRITE (UNIT = kgen_unit) UBOUND(var, 3)
          WRITE (UNIT = kgen_unit) LBOUND(var, 4)
          WRITE (UNIT = kgen_unit) UBOUND(var, 4)
          WRITE (UNIT = kgen_unit) var
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // " **" // NEW_LINE("A"), var
          END IF 
      END IF 
  END SUBROUTINE kw_stats_logical___dim4_ptr
  
end module stats_type

