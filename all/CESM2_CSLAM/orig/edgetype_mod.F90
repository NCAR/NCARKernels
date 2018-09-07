!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:44 
!KGEN version : 0.7.3 
  


module edgetype_mod 

    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
    USE coordinate_systems_mod, ONLY: cartesian3d_t 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE coordinate_systems_mod, ONLY: kr_coordinate_systems_mod_cartesian3d_t 
    USE coordinate_systems_mod, ONLY: kv_coordinate_systems_mod_cartesian3d_t 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 


  type, public :: rotation_t
    integer                 :: nbr                ! nbr direction: north south east west
    integer                 :: reverse            ! 0 = do not reverse order
    ! 1 = reverse order
    real (kind=r8), pointer :: R(:,:,:) => null() !  rotation matrix
  end type rotation_t

  type, public :: EdgeDescriptor_t
     integer                      :: use_rotation
     integer                      :: padding
     integer,             pointer :: putmapP(:) => null()
     integer,             pointer :: getmapP(:) => null()
     integer,             pointer :: putmapP_ghost(:) => null()
     integer,             pointer :: getmapP_ghost(:) => null()
     integer,             pointer :: putmapS(:) => null()
     integer,             pointer :: getmapS(:) => null()
     integer,             pointer :: globalID(:) => null()
     integer,             pointer :: loc2buf(:) => null()
     type(cartesian3D_t), pointer :: neigh_corners(:,:) => null()
     integer                      :: actual_neigh_edges
     logical,             pointer :: reverse(:) => null()
     type (rotation_t),   pointer :: rot(:) => null() !  Identifies list of edges
     !  that must be rotated, and how
  end type EdgeDescriptor_t


  PUBLIC kr_coordinate_systems_mod_cartesian3d_t 
  PUBLIC kr_edgetype_mod_rotation_t 
  PUBLIC kr_edgetype_mod_edgedescriptor_t 
  PUBLIC kv_coordinate_systems_mod_cartesian3d_t 
  PUBLIC kv_edgetype_mod_rotation_t 
  PUBLIC kv_edgetype_mod_edgedescriptor_t 
    
  CONTAINS 
    

 
  !read state subroutine for kr_edgetype_mod_rotation_t 
  RECURSIVE SUBROUTINE kr_edgetype_mod_rotation_t(var, kgen_unit, printname, printvar) 
      TYPE(rotation_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%nbr 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nbr = ", var%nbr 
      END IF   
        
      READ (UNIT = kgen_unit) var%reverse 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%reverse = ", var%reverse 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_rotation_t_real__r8_dim3_ptr(var%r, kgen_unit, printname // "%r", .TRUE.) 
      ELSE 
          CALL kr_rotation_t_real__r8_dim3_ptr(var%r, kgen_unit, printname // "%r", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_edgetype_mod_rotation_t 
    
  !write state subroutine for kr_rotation_t_real__r8_dim3_ptr 
  SUBROUTINE kr_rotation_t_real__r8_dim3_ptr(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), POINTER, DIMENSION(:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3 
      INTEGER, DIMENSION(2,3) :: kgen_bound 
        
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
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_rotation_t_real__r8_dim3_ptr 
    
  !read state subroutine for kr_edgetype_mod_edgedescriptor_t 
  RECURSIVE SUBROUTINE kr_edgetype_mod_edgedescriptor_t(var, kgen_unit, printname, printvar) 
      TYPE(edgedescriptor_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%use_rotation 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%use_rotation = ", var%use_rotation 
      END IF   
        
      READ (UNIT = kgen_unit) var%padding 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%padding = ", var%padding 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%putmapp, kgen_unit, printname // "%putmapp", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%putmapp, kgen_unit, printname // "%putmapp", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%getmapp, kgen_unit, printname // "%getmapp", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%getmapp, kgen_unit, printname // "%getmapp", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%putmapp_ghost, kgen_unit, printname // "%putmapp_ghost", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%putmapp_ghost, kgen_unit, printname // "%putmapp_ghost", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%getmapp_ghost, kgen_unit, printname // "%getmapp_ghost", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%getmapp_ghost, kgen_unit, printname // "%getmapp_ghost", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%putmaps, kgen_unit, printname // "%putmaps", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%putmaps, kgen_unit, printname // "%putmaps", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%getmaps, kgen_unit, printname // "%getmaps", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%getmaps, kgen_unit, printname // "%getmaps", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%globalid, kgen_unit, printname // "%globalid", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%globalid, kgen_unit, printname // "%globalid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%loc2buf, kgen_unit, printname // "%loc2buf", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_integer___dim1_ptr(var%loc2buf, kgen_unit, printname // "%loc2buf", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_edgedescriptor_t_subp9(var%neigh_corners, kgen_unit, printname // "%neigh_corners", .TRUE.) 
      ELSE 
          CALL kr_kgen_edgedescriptor_t_subp9(var%neigh_corners, kgen_unit, printname // "%neigh_corners", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%actual_neigh_edges 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%actual_neigh_edges = ", var%actual_neigh_edges 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgedescriptor_t_logical___dim1_ptr(var%reverse, kgen_unit, printname // "%reverse", .TRUE.) 
      ELSE 
          CALL kr_edgedescriptor_t_logical___dim1_ptr(var%reverse, kgen_unit, printname // "%reverse", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_edgedescriptor_t_subp10(var%rot, kgen_unit, printname // "%rot", .TRUE.) 
      ELSE 
          CALL kr_kgen_edgedescriptor_t_subp10(var%rot, kgen_unit, printname // "%rot", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_edgetype_mod_edgedescriptor_t 
    
  !write state subroutine for kr_edgedescriptor_t_integer___dim1_ptr 
  SUBROUTINE kr_edgedescriptor_t_integer___dim1_ptr(var, kgen_unit, printname, printvar) 
      INTEGER, INTENT(INOUT), POINTER, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
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
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_edgedescriptor_t_integer___dim1_ptr 
    
  !write state subroutine for kr_kgen_edgedescriptor_t_subp9 
  SUBROUTINE kr_kgen_edgedescriptor_t_subp9(var, kgen_unit, printname, printvar) 
      TYPE(cartesian3d_t), INTENT(INOUT), POINTER, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              DO idx2=kgen_bound(1,2), kgen_bound(2,2) 
                  IF (PRESENT( printvar ) .AND. printvar) THEN 
                      CALL kr_coordinate_systems_mod_cartesian3d_t(var(idx1,idx2), kgen_unit, printname // "(idx1,idx2)", .TRUE.) 
                  ELSE 
                      CALL kr_coordinate_systems_mod_cartesian3d_t(var(idx1,idx2), kgen_unit, printname // "(idx1,idx2)", &
                      &.FALSE.) 
                  END IF   
              END DO   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_edgedescriptor_t_subp9 
    
  !write state subroutine for kr_edgedescriptor_t_logical___dim1_ptr 
  SUBROUTINE kr_edgedescriptor_t_logical___dim1_ptr(var, kgen_unit, printname, printvar) 
      LOGICAL, INTENT(INOUT), POINTER, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // " = ", var 
          END IF   
      END IF   
        
  END SUBROUTINE kr_edgedescriptor_t_logical___dim1_ptr 
    
  !write state subroutine for kr_kgen_edgedescriptor_t_subp10 
  SUBROUTINE kr_kgen_edgedescriptor_t_subp10(var, kgen_unit, printname, printvar) 
      TYPE(rotation_t), INTENT(INOUT), POINTER, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              IF (PRESENT( printvar ) .AND. printvar) THEN 
                  CALL kr_edgetype_mod_rotation_t(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
              ELSE 
                  CALL kr_edgetype_mod_rotation_t(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
              END IF   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_edgedescriptor_t_subp10 
    
  !verify state subroutine for kv_edgetype_mod_rotation_t 
  RECURSIVE SUBROUTINE kv_edgetype_mod_rotation_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(rotation_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_nbr 
      integer :: diff_reverse 
      INTEGER :: n_r 
      real(KIND=r8) :: nrmsdiff_r, rmsdiff_r 
      real(KIND=r8), ALLOCATABLE :: buf1_r(:,:,:), buf2_r(:,:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nbr == kgenref_var%nbr) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%nbr is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nbr = ABS(var%nbr - kgenref_var%nbr) 
          IF (diff_nbr <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbr is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbr is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_nbr 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_nbr 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%reverse == kgenref_var%reverse) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%reverse is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_reverse = ABS(var%reverse - kgenref_var%reverse) 
          IF (diff_reverse <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%reverse is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%reverse is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_reverse 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_reverse 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%r)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%r == kgenref_var%r)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%r is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_r(SIZE(var%r,dim=1),SIZE(var%r,dim=2),SIZE(var%r,dim=3))) 
              ALLOCATE (buf2_r(SIZE(var%r,dim=1),SIZE(var%r,dim=2),SIZE(var%r,dim=3))) 
              n_r = COUNT(var%r /= kgenref_var%r) 
              WHERE ( ABS(kgenref_var%r) > kgen_minvalue ) 
                  buf1_r = ((var%r-kgenref_var%r)/kgenref_var%r)**2 
                  buf2_r = (var%r-kgenref_var%r)**2 
              ELSEWHERE 
                  buf1_r = (var%r-kgenref_var%r)**2 
                  buf2_r = buf1_r 
              END WHERE   
              nrmsdiff_r = SQRT(SUM(buf1_r)/REAL(n_r)) 
              rmsdiff_r = SQRT(SUM(buf2_r)/REAL(n_r)) 
              IF (nrmsdiff_r > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%r is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%r is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%r /= kgenref_var%r), " of ", size( var%r ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%r)/real(size(var%r)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%r)/real(size(kgenref_var%r)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_r 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_r 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%r /= kgenref_var%r), " of ", size( var%r ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%r)/real(size(var%r)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%r)/real(size(kgenref_var%r)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_r 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_r 
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
  END SUBROUTINE kv_edgetype_mod_rotation_t 
    
  !verify state subroutine for kv_edgetype_mod_edgedescriptor_t 
  RECURSIVE SUBROUTINE kv_edgetype_mod_edgedescriptor_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(edgedescriptor_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_use_rotation 
      integer :: diff_padding 
      INTEGER :: n_putmapp 
      integer :: nrmsdiff_putmapp, rmsdiff_putmapp 
      integer, ALLOCATABLE :: buf1_putmapp(:), buf2_putmapp(:) 
      INTEGER :: n_getmapp 
      integer :: nrmsdiff_getmapp, rmsdiff_getmapp 
      integer, ALLOCATABLE :: buf1_getmapp(:), buf2_getmapp(:) 
      INTEGER :: n_putmapp_ghost 
      integer :: nrmsdiff_putmapp_ghost, rmsdiff_putmapp_ghost 
      integer, ALLOCATABLE :: buf1_putmapp_ghost(:), buf2_putmapp_ghost(:) 
      INTEGER :: n_getmapp_ghost 
      integer :: nrmsdiff_getmapp_ghost, rmsdiff_getmapp_ghost 
      integer, ALLOCATABLE :: buf1_getmapp_ghost(:), buf2_getmapp_ghost(:) 
      INTEGER :: n_putmaps 
      integer :: nrmsdiff_putmaps, rmsdiff_putmaps 
      integer, ALLOCATABLE :: buf1_putmaps(:), buf2_putmaps(:) 
      INTEGER :: n_getmaps 
      integer :: nrmsdiff_getmaps, rmsdiff_getmaps 
      integer, ALLOCATABLE :: buf1_getmaps(:), buf2_getmaps(:) 
      INTEGER :: n_globalid 
      integer :: nrmsdiff_globalid, rmsdiff_globalid 
      integer, ALLOCATABLE :: buf1_globalid(:), buf2_globalid(:) 
      INTEGER :: n_loc2buf 
      integer :: nrmsdiff_loc2buf, rmsdiff_loc2buf 
      integer, ALLOCATABLE :: buf1_loc2buf(:), buf2_loc2buf(:) 
      INTEGER :: idx1_neigh_corners, idx2_neigh_corners 
      integer :: diff_actual_neigh_edges 
      INTEGER :: n_reverse 
      INTEGER :: idx1_rot 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%use_rotation == kgenref_var%use_rotation) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%use_rotation is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_use_rotation = ABS(var%use_rotation - kgenref_var%use_rotation) 
          IF (diff_use_rotation <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%use_rotation is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%use_rotation is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_use_rotation 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_use_rotation 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%padding == kgenref_var%padding) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%padding is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_padding = ABS(var%padding - kgenref_var%padding) 
          IF (diff_padding <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%padding is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%padding is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_padding 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_padding 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%putmapp)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%putmapp == kgenref_var%putmapp)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%putmapp is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_putmapp(SIZE(var%putmapp,dim=1))) 
              ALLOCATE (buf2_putmapp(SIZE(var%putmapp,dim=1))) 
              n_putmapp = COUNT(var%putmapp /= kgenref_var%putmapp) 
              WHERE ( ABS(kgenref_var%putmapp) > kgen_minvalue ) 
                  buf1_putmapp = ((var%putmapp-kgenref_var%putmapp)/kgenref_var%putmapp)**2 
                  buf2_putmapp = (var%putmapp-kgenref_var%putmapp)**2 
              ELSEWHERE 
                  buf1_putmapp = (var%putmapp-kgenref_var%putmapp)**2 
                  buf2_putmapp = buf1_putmapp 
              END WHERE   
              nrmsdiff_putmapp = SQRT(SUM(buf1_putmapp)/REAL(n_putmapp)) 
              rmsdiff_putmapp = SQRT(SUM(buf2_putmapp)/REAL(n_putmapp)) 
              IF (nrmsdiff_putmapp > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%putmapp is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%putmapp is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%putmapp /= kgenref_var%putmapp), " of ", size( var%putmapp ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%putmapp)/real(size(var%putmapp)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%putmapp)/real(size(kgenref_var%putmapp)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_putmapp 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_putmapp 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%putmapp /= kgenref_var%putmapp), " of ", size( var%putmapp ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%putmapp)/real(size(var%putmapp)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%putmapp)/real(size(kgenref_var%putmapp)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_putmapp 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_putmapp 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%getmapp)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%getmapp == kgenref_var%getmapp)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%getmapp is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_getmapp(SIZE(var%getmapp,dim=1))) 
              ALLOCATE (buf2_getmapp(SIZE(var%getmapp,dim=1))) 
              n_getmapp = COUNT(var%getmapp /= kgenref_var%getmapp) 
              WHERE ( ABS(kgenref_var%getmapp) > kgen_minvalue ) 
                  buf1_getmapp = ((var%getmapp-kgenref_var%getmapp)/kgenref_var%getmapp)**2 
                  buf2_getmapp = (var%getmapp-kgenref_var%getmapp)**2 
              ELSEWHERE 
                  buf1_getmapp = (var%getmapp-kgenref_var%getmapp)**2 
                  buf2_getmapp = buf1_getmapp 
              END WHERE   
              nrmsdiff_getmapp = SQRT(SUM(buf1_getmapp)/REAL(n_getmapp)) 
              rmsdiff_getmapp = SQRT(SUM(buf2_getmapp)/REAL(n_getmapp)) 
              IF (nrmsdiff_getmapp > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%getmapp is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%getmapp is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%getmapp /= kgenref_var%getmapp), " of ", size( var%getmapp ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%getmapp)/real(size(var%getmapp)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%getmapp)/real(size(kgenref_var%getmapp)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_getmapp 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_getmapp 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%getmapp /= kgenref_var%getmapp), " of ", size( var%getmapp ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%getmapp)/real(size(var%getmapp)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%getmapp)/real(size(kgenref_var%getmapp)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_getmapp 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_getmapp 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%putmapp_ghost)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%putmapp_ghost == kgenref_var%putmapp_ghost)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%putmapp_ghost is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_putmapp_ghost(SIZE(var%putmapp_ghost,dim=1))) 
              ALLOCATE (buf2_putmapp_ghost(SIZE(var%putmapp_ghost,dim=1))) 
              n_putmapp_ghost = COUNT(var%putmapp_ghost /= kgenref_var%putmapp_ghost) 
              WHERE ( ABS(kgenref_var%putmapp_ghost) > kgen_minvalue ) 
                  buf1_putmapp_ghost = ((var%putmapp_ghost-kgenref_var%putmapp_ghost)/kgenref_var%putmapp_ghost)**2 
                  buf2_putmapp_ghost = (var%putmapp_ghost-kgenref_var%putmapp_ghost)**2 
              ELSEWHERE 
                  buf1_putmapp_ghost = (var%putmapp_ghost-kgenref_var%putmapp_ghost)**2 
                  buf2_putmapp_ghost = buf1_putmapp_ghost 
              END WHERE   
              nrmsdiff_putmapp_ghost = SQRT(SUM(buf1_putmapp_ghost)/REAL(n_putmapp_ghost)) 
              rmsdiff_putmapp_ghost = SQRT(SUM(buf2_putmapp_ghost)/REAL(n_putmapp_ghost)) 
              IF (nrmsdiff_putmapp_ghost > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%putmapp_ghost is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%putmapp_ghost is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%putmapp_ghost /= kgenref_var%putmapp_ghost), " of ", size( var%putmapp_ghost ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%putmapp_ghost)/real(size(var%putmapp_ghost)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%putmapp_ghost)/real(size(kgenref_var%putmapp_ghost)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_putmapp_ghost 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_putmapp_ghost 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%putmapp_ghost /= kgenref_var%putmapp_ghost), " of ", size( var%putmapp_ghost ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%putmapp_ghost)/real(size(var%putmapp_ghost)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%putmapp_ghost)/real(size(kgenref_var%putmapp_ghost)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_putmapp_ghost 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_putmapp_ghost 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%getmapp_ghost)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%getmapp_ghost == kgenref_var%getmapp_ghost)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%getmapp_ghost is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_getmapp_ghost(SIZE(var%getmapp_ghost,dim=1))) 
              ALLOCATE (buf2_getmapp_ghost(SIZE(var%getmapp_ghost,dim=1))) 
              n_getmapp_ghost = COUNT(var%getmapp_ghost /= kgenref_var%getmapp_ghost) 
              WHERE ( ABS(kgenref_var%getmapp_ghost) > kgen_minvalue ) 
                  buf1_getmapp_ghost = ((var%getmapp_ghost-kgenref_var%getmapp_ghost)/kgenref_var%getmapp_ghost)**2 
                  buf2_getmapp_ghost = (var%getmapp_ghost-kgenref_var%getmapp_ghost)**2 
              ELSEWHERE 
                  buf1_getmapp_ghost = (var%getmapp_ghost-kgenref_var%getmapp_ghost)**2 
                  buf2_getmapp_ghost = buf1_getmapp_ghost 
              END WHERE   
              nrmsdiff_getmapp_ghost = SQRT(SUM(buf1_getmapp_ghost)/REAL(n_getmapp_ghost)) 
              rmsdiff_getmapp_ghost = SQRT(SUM(buf2_getmapp_ghost)/REAL(n_getmapp_ghost)) 
              IF (nrmsdiff_getmapp_ghost > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%getmapp_ghost is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%getmapp_ghost is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%getmapp_ghost /= kgenref_var%getmapp_ghost), " of ", size( var%getmapp_ghost ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%getmapp_ghost)/real(size(var%getmapp_ghost)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%getmapp_ghost)/real(size(kgenref_var%getmapp_ghost)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_getmapp_ghost 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_getmapp_ghost 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%getmapp_ghost /= kgenref_var%getmapp_ghost), " of ", size( var%getmapp_ghost ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%getmapp_ghost)/real(size(var%getmapp_ghost)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%getmapp_ghost)/real(size(kgenref_var%getmapp_ghost)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_getmapp_ghost 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_getmapp_ghost 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%putmaps)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%putmaps == kgenref_var%putmaps)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%putmaps is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_putmaps(SIZE(var%putmaps,dim=1))) 
              ALLOCATE (buf2_putmaps(SIZE(var%putmaps,dim=1))) 
              n_putmaps = COUNT(var%putmaps /= kgenref_var%putmaps) 
              WHERE ( ABS(kgenref_var%putmaps) > kgen_minvalue ) 
                  buf1_putmaps = ((var%putmaps-kgenref_var%putmaps)/kgenref_var%putmaps)**2 
                  buf2_putmaps = (var%putmaps-kgenref_var%putmaps)**2 
              ELSEWHERE 
                  buf1_putmaps = (var%putmaps-kgenref_var%putmaps)**2 
                  buf2_putmaps = buf1_putmaps 
              END WHERE   
              nrmsdiff_putmaps = SQRT(SUM(buf1_putmaps)/REAL(n_putmaps)) 
              rmsdiff_putmaps = SQRT(SUM(buf2_putmaps)/REAL(n_putmaps)) 
              IF (nrmsdiff_putmaps > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%putmaps is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%putmaps is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%putmaps /= kgenref_var%putmaps), " of ", size( var%putmaps ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%putmaps)/real(size(var%putmaps)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%putmaps)/real(size(kgenref_var%putmaps)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_putmaps 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_putmaps 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%putmaps /= kgenref_var%putmaps), " of ", size( var%putmaps ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%putmaps)/real(size(var%putmaps)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%putmaps)/real(size(kgenref_var%putmaps)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_putmaps 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_putmaps 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%getmaps)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%getmaps == kgenref_var%getmaps)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%getmaps is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_getmaps(SIZE(var%getmaps,dim=1))) 
              ALLOCATE (buf2_getmaps(SIZE(var%getmaps,dim=1))) 
              n_getmaps = COUNT(var%getmaps /= kgenref_var%getmaps) 
              WHERE ( ABS(kgenref_var%getmaps) > kgen_minvalue ) 
                  buf1_getmaps = ((var%getmaps-kgenref_var%getmaps)/kgenref_var%getmaps)**2 
                  buf2_getmaps = (var%getmaps-kgenref_var%getmaps)**2 
              ELSEWHERE 
                  buf1_getmaps = (var%getmaps-kgenref_var%getmaps)**2 
                  buf2_getmaps = buf1_getmaps 
              END WHERE   
              nrmsdiff_getmaps = SQRT(SUM(buf1_getmaps)/REAL(n_getmaps)) 
              rmsdiff_getmaps = SQRT(SUM(buf2_getmaps)/REAL(n_getmaps)) 
              IF (nrmsdiff_getmaps > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%getmaps is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%getmaps is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%getmaps /= kgenref_var%getmaps), " of ", size( var%getmaps ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%getmaps)/real(size(var%getmaps)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%getmaps)/real(size(kgenref_var%getmaps)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_getmaps 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_getmaps 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%getmaps /= kgenref_var%getmaps), " of ", size( var%getmaps ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%getmaps)/real(size(var%getmaps)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%getmaps)/real(size(kgenref_var%getmaps)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_getmaps 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_getmaps 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%globalid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%globalid == kgenref_var%globalid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%globalid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_globalid(SIZE(var%globalid,dim=1))) 
              ALLOCATE (buf2_globalid(SIZE(var%globalid,dim=1))) 
              n_globalid = COUNT(var%globalid /= kgenref_var%globalid) 
              WHERE ( ABS(kgenref_var%globalid) > kgen_minvalue ) 
                  buf1_globalid = ((var%globalid-kgenref_var%globalid)/kgenref_var%globalid)**2 
                  buf2_globalid = (var%globalid-kgenref_var%globalid)**2 
              ELSEWHERE 
                  buf1_globalid = (var%globalid-kgenref_var%globalid)**2 
                  buf2_globalid = buf1_globalid 
              END WHERE   
              nrmsdiff_globalid = SQRT(SUM(buf1_globalid)/REAL(n_globalid)) 
              rmsdiff_globalid = SQRT(SUM(buf2_globalid)/REAL(n_globalid)) 
              IF (nrmsdiff_globalid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%globalid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%globalid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%globalid /= kgenref_var%globalid), " of ", size( var%globalid ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%globalid)/real(size(var%globalid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%globalid)/real(size(kgenref_var%globalid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_globalid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_globalid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%globalid /= kgenref_var%globalid), " of ", size( var%globalid ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%globalid)/real(size(var%globalid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%globalid)/real(size(kgenref_var%globalid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_globalid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_globalid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%loc2buf)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%loc2buf == kgenref_var%loc2buf)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%loc2buf is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_loc2buf(SIZE(var%loc2buf,dim=1))) 
              ALLOCATE (buf2_loc2buf(SIZE(var%loc2buf,dim=1))) 
              n_loc2buf = COUNT(var%loc2buf /= kgenref_var%loc2buf) 
              WHERE ( ABS(kgenref_var%loc2buf) > kgen_minvalue ) 
                  buf1_loc2buf = ((var%loc2buf-kgenref_var%loc2buf)/kgenref_var%loc2buf)**2 
                  buf2_loc2buf = (var%loc2buf-kgenref_var%loc2buf)**2 
              ELSEWHERE 
                  buf1_loc2buf = (var%loc2buf-kgenref_var%loc2buf)**2 
                  buf2_loc2buf = buf1_loc2buf 
              END WHERE   
              nrmsdiff_loc2buf = SQRT(SUM(buf1_loc2buf)/REAL(n_loc2buf)) 
              rmsdiff_loc2buf = SQRT(SUM(buf2_loc2buf)/REAL(n_loc2buf)) 
              IF (nrmsdiff_loc2buf > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%loc2buf is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%loc2buf is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%loc2buf /= kgenref_var%loc2buf), " of ", size( var%loc2buf ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%loc2buf)/real(size(var%loc2buf)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%loc2buf)/real(size(kgenref_var%loc2buf)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_loc2buf 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_loc2buf 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%loc2buf /= kgenref_var%loc2buf), " of ", size( var%loc2buf ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%loc2buf)/real(size(var%loc2buf)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%loc2buf)/real(size(kgenref_var%loc2buf)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_loc2buf 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_loc2buf 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%neigh_corners)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
          DO   idx1_neigh_corners = LBOUND(var%neigh_corners,1), UBOUND(var%neigh_corners,1) 
              DO   idx2_neigh_corners = LBOUND(var%neigh_corners,2), UBOUND(var%neigh_corners,2) 
                  CALL kv_coordinate_systems_mod_cartesian3d_t(trim(adjustl(varname))//"%neigh_corners", comp_check_status, &
                  &var%neigh_corners(idx1_neigh_corners,idx2_neigh_corners), &
                  &kgenref_var%neigh_corners(idx1_neigh_corners,idx2_neigh_corners)) 
              END DO   
          END DO   
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%neigh_corners", " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%neigh_corners is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%neigh_corners is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%actual_neigh_edges == kgenref_var%actual_neigh_edges) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%actual_neigh_edges is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_actual_neigh_edges = ABS(var%actual_neigh_edges - kgenref_var%actual_neigh_edges) 
          IF (diff_actual_neigh_edges <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%actual_neigh_edges is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%actual_neigh_edges is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_actual_neigh_edges 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_actual_neigh_edges 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%reverse)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%reverse .EQV. kgenref_var%reverse)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%reverse is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              n_reverse = COUNT(var%reverse .NEQV. kgenref_var%reverse) 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%reverse is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
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
      IF (ASSOCIATED(var%rot)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
          DO   idx1_rot = LBOUND(var%rot,1), UBOUND(var%rot,1) 
              CALL kv_edgetype_mod_rotation_t(trim(adjustl(varname))//"%rot", comp_check_status, var%rot(idx1_rot), &
              &kgenref_var%rot(idx1_rot)) 
          END DO   
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%rot", " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rot is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rot is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
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
  END SUBROUTINE kv_edgetype_mod_edgedescriptor_t 
    
end module edgetype_mod