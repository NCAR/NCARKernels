!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:44 
!KGEN version : 0.7.3 
  


module GridGraph_mod
  !-------------------------
  !-------------------------------
  !-------------------------
  !-----
  !-----
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    IMPLICIT NONE 


    PRIVATE 

  integer, public, parameter :: num_neighbors=8 ! for north, south, east, west, neast, nwest, seast, swest


  type, public :: GridVertex_t

      integer, pointer          :: nbrs(:) => null()           ! The numbers of the neighbor elements
      integer, pointer          :: nbrs_face(:) => null()      ! The cube face number of the neighbor element (nbrs array)
      integer, pointer          :: nbrs_wgt(:) => null()       ! The weights for edges defined by nbrs array
      integer, pointer          :: nbrs_wgt_ghost(:) => null() ! The weights for edges defined by nbrs array
      integer                   :: nbrs_ptr(num_neighbors + 1) !index into the nbrs array for each neighbor direction

      integer                   :: face_number           ! which face of the cube this vertex is on
      integer                   :: number                ! element number
      integer                   :: processor_number      ! processor number
      integer                   :: SpaceCurve  ! index in Space-Filling curve
  end type GridVertex_t


! ==========================================
! Public Interfaces
! ==========================================


  PUBLIC kr_gridgraph_mod_gridvertex_t 
  PUBLIC kv_gridgraph_mod_gridvertex_t 

!======================================================================
    
  CONTAINS 
    


!======================================================================


!======================================================================
! =====================================
! copy edge:
! copy device for overloading = sign.
! =====================================


!======================================================================


!===========================
! search edge list for match
!===========================


!======================================================================


!======================================================================


!======================================================================


!======================================================================


!======================================================================


!======================================================================
! ==========================================
! set_GridVertex_neighbors:
! Set global element number for element elem
! ==========================================

!


!======================================================================


!======================================================================


!======================================================================


!======================================================================

  !read state subroutine for kr_gridgraph_mod_gridvertex_t 
  RECURSIVE SUBROUTINE kr_gridgraph_mod_gridvertex_t(var, kgen_unit, printname, printvar) 
      TYPE(gridvertex_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs, kgen_unit, printname // "%nbrs", .TRUE.) 
      ELSE 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs, kgen_unit, printname // "%nbrs", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs_face, kgen_unit, printname // "%nbrs_face", .TRUE.) 
      ELSE 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs_face, kgen_unit, printname // "%nbrs_face", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs_wgt, kgen_unit, printname // "%nbrs_wgt", .TRUE.) 
      ELSE 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs_wgt, kgen_unit, printname // "%nbrs_wgt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs_wgt_ghost, kgen_unit, printname // "%nbrs_wgt_ghost", .TRUE.) 
      ELSE 
          CALL kr_gridvertex_t_integer___dim1_ptr(var%nbrs_wgt_ghost, kgen_unit, printname // "%nbrs_wgt_ghost", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%nbrs_ptr 
          CALL kgen_array_sumcheck(printname // "%nbrs_ptr", kgen_array_sum, DBLE(SUM(var%nbrs_ptr, mask=(var%nbrs_ptr .eq. &
          &var%nbrs_ptr))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%nbrs_ptr)) = ", DBLE(SUM(var%nbrs_ptr, mask=(var%nbrs_ptr &
              &.eq. var%nbrs_ptr))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%face_number 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%face_number = ", var%face_number 
      END IF   
        
      READ (UNIT = kgen_unit) var%number 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%number = ", var%number 
      END IF   
        
      READ (UNIT = kgen_unit) var%processor_number 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%processor_number = ", var%processor_number 
      END IF   
        
      READ (UNIT = kgen_unit) var%spacecurve 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%spacecurve = ", var%spacecurve 
      END IF   
        
  END SUBROUTINE kr_gridgraph_mod_gridvertex_t 
    
  !write state subroutine for kr_gridvertex_t_integer___dim1_ptr 
  SUBROUTINE kr_gridvertex_t_integer___dim1_ptr(var, kgen_unit, printname, printvar) 
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
        
  END SUBROUTINE kr_gridvertex_t_integer___dim1_ptr 
    
  !verify state subroutine for kv_gridgraph_mod_gridvertex_t 
  RECURSIVE SUBROUTINE kv_gridgraph_mod_gridvertex_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(gridvertex_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_nbrs 
      integer :: nrmsdiff_nbrs, rmsdiff_nbrs 
      integer, ALLOCATABLE :: buf1_nbrs(:), buf2_nbrs(:) 
      INTEGER :: n_nbrs_face 
      integer :: nrmsdiff_nbrs_face, rmsdiff_nbrs_face 
      integer, ALLOCATABLE :: buf1_nbrs_face(:), buf2_nbrs_face(:) 
      INTEGER :: n_nbrs_wgt 
      integer :: nrmsdiff_nbrs_wgt, rmsdiff_nbrs_wgt 
      integer, ALLOCATABLE :: buf1_nbrs_wgt(:), buf2_nbrs_wgt(:) 
      INTEGER :: n_nbrs_wgt_ghost 
      integer :: nrmsdiff_nbrs_wgt_ghost, rmsdiff_nbrs_wgt_ghost 
      integer, ALLOCATABLE :: buf1_nbrs_wgt_ghost(:), buf2_nbrs_wgt_ghost(:) 
      INTEGER :: n_nbrs_ptr 
      integer :: nrmsdiff_nbrs_ptr, rmsdiff_nbrs_ptr 
      integer, ALLOCATABLE :: buf1_nbrs_ptr(:), buf2_nbrs_ptr(:) 
      integer :: diff_face_number 
      integer :: diff_number 
      integer :: diff_processor_number 
      integer :: diff_spacecurve 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      IF (ASSOCIATED(var%nbrs)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%nbrs == kgenref_var%nbrs)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbrs is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_nbrs(SIZE(var%nbrs,dim=1))) 
              ALLOCATE (buf2_nbrs(SIZE(var%nbrs,dim=1))) 
              n_nbrs = COUNT(var%nbrs /= kgenref_var%nbrs) 
              WHERE ( ABS(kgenref_var%nbrs) > kgen_minvalue ) 
                  buf1_nbrs = ((var%nbrs-kgenref_var%nbrs)/kgenref_var%nbrs)**2 
                  buf2_nbrs = (var%nbrs-kgenref_var%nbrs)**2 
              ELSEWHERE 
                  buf1_nbrs = (var%nbrs-kgenref_var%nbrs)**2 
                  buf2_nbrs = buf1_nbrs 
              END WHERE   
              nrmsdiff_nbrs = SQRT(SUM(buf1_nbrs)/REAL(n_nbrs)) 
              rmsdiff_nbrs = SQRT(SUM(buf2_nbrs)/REAL(n_nbrs)) 
              IF (nrmsdiff_nbrs > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs /= kgenref_var%nbrs), " of ", size( var%nbrs ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs)/real(size(var%nbrs)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs)/real(size(kgenref_var%nbrs)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs /= kgenref_var%nbrs), " of ", size( var%nbrs ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs)/real(size(var%nbrs)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs)/real(size(kgenref_var%nbrs)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%nbrs_face)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%nbrs_face == kgenref_var%nbrs_face)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbrs_face is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_nbrs_face(SIZE(var%nbrs_face,dim=1))) 
              ALLOCATE (buf2_nbrs_face(SIZE(var%nbrs_face,dim=1))) 
              n_nbrs_face = COUNT(var%nbrs_face /= kgenref_var%nbrs_face) 
              WHERE ( ABS(kgenref_var%nbrs_face) > kgen_minvalue ) 
                  buf1_nbrs_face = ((var%nbrs_face-kgenref_var%nbrs_face)/kgenref_var%nbrs_face)**2 
                  buf2_nbrs_face = (var%nbrs_face-kgenref_var%nbrs_face)**2 
              ELSEWHERE 
                  buf1_nbrs_face = (var%nbrs_face-kgenref_var%nbrs_face)**2 
                  buf2_nbrs_face = buf1_nbrs_face 
              END WHERE   
              nrmsdiff_nbrs_face = SQRT(SUM(buf1_nbrs_face)/REAL(n_nbrs_face)) 
              rmsdiff_nbrs_face = SQRT(SUM(buf2_nbrs_face)/REAL(n_nbrs_face)) 
              IF (nrmsdiff_nbrs_face > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs_face is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs_face is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs_face /= kgenref_var%nbrs_face), " of ", size( var%nbrs_face ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs_face)/real(size(var%nbrs_face)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_face)/real(size(kgenref_var%nbrs_face)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_face 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_face 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs_face /= kgenref_var%nbrs_face), " of ", size( var%nbrs_face ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs_face)/real(size(var%nbrs_face)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_face)/real(size(kgenref_var%nbrs_face)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_face 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_face 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%nbrs_wgt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%nbrs_wgt == kgenref_var%nbrs_wgt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbrs_wgt is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_nbrs_wgt(SIZE(var%nbrs_wgt,dim=1))) 
              ALLOCATE (buf2_nbrs_wgt(SIZE(var%nbrs_wgt,dim=1))) 
              n_nbrs_wgt = COUNT(var%nbrs_wgt /= kgenref_var%nbrs_wgt) 
              WHERE ( ABS(kgenref_var%nbrs_wgt) > kgen_minvalue ) 
                  buf1_nbrs_wgt = ((var%nbrs_wgt-kgenref_var%nbrs_wgt)/kgenref_var%nbrs_wgt)**2 
                  buf2_nbrs_wgt = (var%nbrs_wgt-kgenref_var%nbrs_wgt)**2 
              ELSEWHERE 
                  buf1_nbrs_wgt = (var%nbrs_wgt-kgenref_var%nbrs_wgt)**2 
                  buf2_nbrs_wgt = buf1_nbrs_wgt 
              END WHERE   
              nrmsdiff_nbrs_wgt = SQRT(SUM(buf1_nbrs_wgt)/REAL(n_nbrs_wgt)) 
              rmsdiff_nbrs_wgt = SQRT(SUM(buf2_nbrs_wgt)/REAL(n_nbrs_wgt)) 
              IF (nrmsdiff_nbrs_wgt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs_wgt is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs_wgt is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs_wgt /= kgenref_var%nbrs_wgt), " of ", size( var%nbrs_wgt ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs_wgt)/real(size(var%nbrs_wgt)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_wgt)/real(size(kgenref_var%nbrs_wgt)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_wgt 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_wgt 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs_wgt /= kgenref_var%nbrs_wgt), " of ", size( var%nbrs_wgt ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs_wgt)/real(size(var%nbrs_wgt)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_wgt)/real(size(kgenref_var%nbrs_wgt)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_wgt 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_wgt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%nbrs_wgt_ghost)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%nbrs_wgt_ghost == kgenref_var%nbrs_wgt_ghost)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbrs_wgt_ghost is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_nbrs_wgt_ghost(SIZE(var%nbrs_wgt_ghost,dim=1))) 
              ALLOCATE (buf2_nbrs_wgt_ghost(SIZE(var%nbrs_wgt_ghost,dim=1))) 
              n_nbrs_wgt_ghost = COUNT(var%nbrs_wgt_ghost /= kgenref_var%nbrs_wgt_ghost) 
              WHERE ( ABS(kgenref_var%nbrs_wgt_ghost) > kgen_minvalue ) 
                  buf1_nbrs_wgt_ghost = ((var%nbrs_wgt_ghost-kgenref_var%nbrs_wgt_ghost)/kgenref_var%nbrs_wgt_ghost)**2 
                  buf2_nbrs_wgt_ghost = (var%nbrs_wgt_ghost-kgenref_var%nbrs_wgt_ghost)**2 
              ELSEWHERE 
                  buf1_nbrs_wgt_ghost = (var%nbrs_wgt_ghost-kgenref_var%nbrs_wgt_ghost)**2 
                  buf2_nbrs_wgt_ghost = buf1_nbrs_wgt_ghost 
              END WHERE   
              nrmsdiff_nbrs_wgt_ghost = SQRT(SUM(buf1_nbrs_wgt_ghost)/REAL(n_nbrs_wgt_ghost)) 
              rmsdiff_nbrs_wgt_ghost = SQRT(SUM(buf2_nbrs_wgt_ghost)/REAL(n_nbrs_wgt_ghost)) 
              IF (nrmsdiff_nbrs_wgt_ghost > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs_wgt_ghost is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nbrs_wgt_ghost is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs_wgt_ghost /= kgenref_var%nbrs_wgt_ghost), " of ", size( var%nbrs_wgt_ghost ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs_wgt_ghost)/real(size(var%nbrs_wgt_ghost)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_wgt_ghost)/real(size(kgenref_var%nbrs_wgt_ghost)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_wgt_ghost 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_wgt_ghost 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%nbrs_wgt_ghost /= kgenref_var%nbrs_wgt_ghost), " of ", size( var%nbrs_wgt_ghost ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%nbrs_wgt_ghost)/real(size(var%nbrs_wgt_ghost)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_wgt_ghost)/real(size(kgenref_var%nbrs_wgt_ghost)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_wgt_ghost 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_wgt_ghost 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%nbrs_ptr == kgenref_var%nbrs_ptr)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%nbrs_ptr is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_nbrs_ptr(SIZE(var%nbrs_ptr,dim=1))) 
          ALLOCATE (buf2_nbrs_ptr(SIZE(var%nbrs_ptr,dim=1))) 
          n_nbrs_ptr = COUNT(var%nbrs_ptr /= kgenref_var%nbrs_ptr) 
          WHERE ( ABS(kgenref_var%nbrs_ptr) > kgen_minvalue ) 
              buf1_nbrs_ptr = ((var%nbrs_ptr-kgenref_var%nbrs_ptr)/kgenref_var%nbrs_ptr)**2 
              buf2_nbrs_ptr = (var%nbrs_ptr-kgenref_var%nbrs_ptr)**2 
          ELSEWHERE 
              buf1_nbrs_ptr = (var%nbrs_ptr-kgenref_var%nbrs_ptr)**2 
              buf2_nbrs_ptr = buf1_nbrs_ptr 
          END WHERE   
          nrmsdiff_nbrs_ptr = SQRT(SUM(buf1_nbrs_ptr)/REAL(n_nbrs_ptr)) 
          rmsdiff_nbrs_ptr = SQRT(SUM(buf2_nbrs_ptr)/REAL(n_nbrs_ptr)) 
          IF (nrmsdiff_nbrs_ptr > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbrs_ptr is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nbrs_ptr is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%nbrs_ptr /= kgenref_var%nbrs_ptr), " of ", size( var%nbrs_ptr ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%nbrs_ptr)/real(size(var%nbrs_ptr)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_ptr)/real(size(kgenref_var%nbrs_ptr)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_ptr 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_ptr 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%nbrs_ptr /= kgenref_var%nbrs_ptr), " of ", size( var%nbrs_ptr ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%nbrs_ptr)/real(size(var%nbrs_ptr)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%nbrs_ptr)/real(size(kgenref_var%nbrs_ptr)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_nbrs_ptr 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nbrs_ptr 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%face_number == kgenref_var%face_number) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%face_number is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_face_number = ABS(var%face_number - kgenref_var%face_number) 
          IF (diff_face_number <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%face_number is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%face_number is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_face_number 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_face_number 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%number == kgenref_var%number) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%number is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_number = ABS(var%number - kgenref_var%number) 
          IF (diff_number <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%number is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%number is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_number 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_number 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%processor_number == kgenref_var%processor_number) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%processor_number is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_processor_number = ABS(var%processor_number - kgenref_var%processor_number) 
          IF (diff_processor_number <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%processor_number is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%processor_number is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_processor_number 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_processor_number 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%spacecurve == kgenref_var%spacecurve) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%spacecurve is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_spacecurve = ABS(var%spacecurve - kgenref_var%spacecurve) 
          IF (diff_spacecurve <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spacecurve is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spacecurve is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_spacecurve 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_spacecurve 
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
  END SUBROUTINE kv_gridgraph_mod_gridvertex_t 
    
end module GridGraph_mod