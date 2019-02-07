!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:36 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id: stat_file_module.F90 7140 2014-07-31 19:14:05Z betlej@uwm.edu $
!===============================================================================


module stat_file_module
! Description:
!   Contains two derived types for describing the contents and location of
!   either NetCDF or GrADS files.
!-------------------------------------------------------------------------------
 

    USE clubb_precision, ONLY: stat_rknd, time_precision, core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
 
    IMPLICIT NONE 

    PUBLIC variable, stat_file 
   ! These are used in a 2D or 3D host model to output multiple columns
   ! Set clubb_i and clubb_j according to the column within the host model;
   ! The indices must not exceed nlon (for i) or nlat (for j).

   integer, save, public :: clubb_i = 1, clubb_j = 1
!$omp threadprivate(clubb_i, clubb_j)

   PRIVATE 
  ! Structure to hold the description of a variable


   type variable
     ! Pointer to the array
     real(kind=stat_rknd), dimension(:,:,:), pointer :: ptr

     character(len = 30) :: name        ! Variable name
     character(len = 100) :: description ! Variable description
     character(len = 20) :: units       ! Variable units

     integer :: indx ! NetCDF module Id for var / GrADS index

     logical :: l_silhs ! If true, we sample this variable once for each SILHS
                        ! sample point per timestep, rather than just once per
                        ! timestep.
   end type variable
  ! Structure to hold the description of a NetCDF output file
  ! This makes the new code as compatible as possible with the
  ! GrADS output code


   type stat_file
     ! File information


     character(len = 200) ::  &
       fname,   & ! File name without suffix
       fdir    ! Path where fname resides

     integer :: iounit  ! This number is used internally by the
                        ! NetCDF module to track the data set, or by
                        ! GrADS to track the actual file unit.
     integer :: &
       nrecord, & ! Number of records written
       ntimes     ! Number of times written

     logical :: &
       l_defined,  &  ! Whether nf90_enddef() has been called
       l_byte_swapped ! Is this a file in the opposite byte ordering?
     ! NetCDF datafile dimensions indices

     integer ::  & 
       LatDimId, LongDimId, AltDimId, TimeDimId, & 
       LatVarId, LongVarId, AltVarId, TimeVarId
     ! Grid information


     integer :: ia, iz  ! Vertical extent

     integer :: nlat, nlon ! The number of points in the X and Y

     real( kind = core_rknd ), dimension(:), allocatable ::  & 
       z ! Height of vertical levels [m]
     ! Time information


     integer :: day, month, year ! Date of starting time

     real( kind = core_rknd ), dimension(:), allocatable :: & 
       rlat, & ! Latitude                   [Degrees N]
       rlon    ! Longitude                  [Degrees E]

     real( kind = core_rknd ) :: & 
       dtwrite ! Interval between output    [Seconds]

     real( kind = time_precision ) ::  & 
       time    ! Start time                 [Seconds]
     ! Statistical Variables


     integer :: nvar  ! Number of variables for this file

     type (variable), dimension(:), allocatable ::  &
       var ! List and variable description

   end type stat_file
   PUBLIC kr_externs_in_stat_file_module 
   PUBLIC kr_stat_file_module_variable 
   PUBLIC kr_stat_file_module_stat_file 
   PUBLIC kv_stat_file_module_variable 
   PUBLIC kv_stat_file_module_stat_file 
     
   CONTAINS 
     

   !read state subroutine for kr_externs_in_stat_file_module 
   SUBROUTINE kr_externs_in_stat_file_module(kgen_unit) 
       INTEGER, INTENT(IN) :: kgen_unit 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       READ (UNIT = kgen_unit) clubb_i 
       READ (UNIT = kgen_unit) clubb_j 
   END SUBROUTINE kr_externs_in_stat_file_module 
     
   !read state subroutine for kr_stat_file_module_variable 
   RECURSIVE SUBROUTINE kr_stat_file_module_variable(var, kgen_unit, printname, printvar) 
       TYPE(variable), INTENT(INOUT) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN) :: printname 
       LOGICAL, INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           CALL kr_variable_real__stat_rknd_dim3_ptr(var%ptr, kgen_unit, printname // "%ptr", .TRUE.) 
       ELSE 
           CALL kr_variable_real__stat_rknd_dim3_ptr(var%ptr, kgen_unit, printname // "%ptr", .FALSE.) 
       END IF   
         
       READ (UNIT = kgen_unit) var%name 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%name = ", var%name 
       END IF   
         
       READ (UNIT = kgen_unit) var%description 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%description = ", var%description 
       END IF   
         
       READ (UNIT = kgen_unit) var%units 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%units = ", var%units 
       END IF   
         
       READ (UNIT = kgen_unit) var%indx 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%indx = ", var%indx 
       END IF   
         
       READ (UNIT = kgen_unit) var%l_silhs 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%l_silhs = ", var%l_silhs 
       END IF   
         
   END SUBROUTINE kr_stat_file_module_variable 
     
   !write state subroutine for kr_variable_real__stat_rknd_dim3_ptr 
   SUBROUTINE kr_variable_real__stat_rknd_dim3_ptr(var, kgen_unit, printname, printvar) 
       REAL(KIND=stat_rknd), INTENT(INOUT), POINTER, DIMENSION(:,:,:) :: var 
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
         
   END SUBROUTINE kr_variable_real__stat_rknd_dim3_ptr 
     
   !read state subroutine for kr_stat_file_module_stat_file 
   RECURSIVE SUBROUTINE kr_stat_file_module_stat_file(var, kgen_unit, printname, printvar) 
       TYPE(stat_file), INTENT(INOUT) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN) :: printname 
       LOGICAL, INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       READ (UNIT = kgen_unit) var%fname 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%fname = ", var%fname 
       END IF   
       READ (UNIT = kgen_unit) var%fdir 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%fdir = ", var%fdir 
       END IF   
         
       READ (UNIT = kgen_unit) var%iounit 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%iounit = ", var%iounit 
       END IF   
         
       READ (UNIT = kgen_unit) var%nrecord 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%nrecord = ", var%nrecord 
       END IF   
       READ (UNIT = kgen_unit) var%ntimes 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%ntimes = ", var%ntimes 
       END IF   
         
       READ (UNIT = kgen_unit) var%l_defined 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%l_defined = ", var%l_defined 
       END IF   
       READ (UNIT = kgen_unit) var%l_byte_swapped 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%l_byte_swapped = ", var%l_byte_swapped 
       END IF   
         
       READ (UNIT = kgen_unit) var%latdimid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%latdimid = ", var%latdimid 
       END IF   
       READ (UNIT = kgen_unit) var%longdimid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%longdimid = ", var%longdimid 
       END IF   
       READ (UNIT = kgen_unit) var%altdimid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%altdimid = ", var%altdimid 
       END IF   
       READ (UNIT = kgen_unit) var%timedimid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%timedimid = ", var%timedimid 
       END IF   
       READ (UNIT = kgen_unit) var%latvarid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%latvarid = ", var%latvarid 
       END IF   
       READ (UNIT = kgen_unit) var%longvarid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%longvarid = ", var%longvarid 
       END IF   
       READ (UNIT = kgen_unit) var%altvarid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%altvarid = ", var%altvarid 
       END IF   
       READ (UNIT = kgen_unit) var%timevarid 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%timevarid = ", var%timevarid 
       END IF   
         
       READ (UNIT = kgen_unit) var%ia 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%ia = ", var%ia 
       END IF   
       READ (UNIT = kgen_unit) var%iz 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%iz = ", var%iz 
       END IF   
         
       READ (UNIT = kgen_unit) var%nlat 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%nlat = ", var%nlat 
       END IF   
       READ (UNIT = kgen_unit) var%nlon 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%nlon = ", var%nlon 
       END IF   
         
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           CALL kr_stat_file_real__core_rknd_dim1(var%z, kgen_unit, printname // "%z", .TRUE.) 
       ELSE 
           CALL kr_stat_file_real__core_rknd_dim1(var%z, kgen_unit, printname // "%z", .FALSE.) 
       END IF   
         
       READ (UNIT = kgen_unit) var%day 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%day = ", var%day 
       END IF   
       READ (UNIT = kgen_unit) var%month 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%month = ", var%month 
       END IF   
       READ (UNIT = kgen_unit) var%year 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%year = ", var%year 
       END IF   
         
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           CALL kr_stat_file_real__core_rknd_dim1(var%rlat, kgen_unit, printname // "%rlat", .TRUE.) 
       ELSE 
           CALL kr_stat_file_real__core_rknd_dim1(var%rlat, kgen_unit, printname // "%rlat", .FALSE.) 
       END IF   
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           CALL kr_stat_file_real__core_rknd_dim1(var%rlon, kgen_unit, printname // "%rlon", .TRUE.) 
       ELSE 
           CALL kr_stat_file_real__core_rknd_dim1(var%rlon, kgen_unit, printname // "%rlon", .FALSE.) 
       END IF   
         
       READ (UNIT = kgen_unit) var%dtwrite 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%dtwrite = ", var%dtwrite 
       END IF   
         
       READ (UNIT = kgen_unit) var%time 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%time = ", var%time 
       END IF   
         
       READ (UNIT = kgen_unit) var%nvar 
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           WRITE (*, *) "KGEN DEBUG: " // printname // "%nvar = ", var%nvar 
       END IF   
         
       IF (PRESENT( printvar ) .AND. printvar) THEN 
           CALL kr_stat_file_variable__variable_dim1(var%var, kgen_unit, printname // "%var", .TRUE.) 
       ELSE 
           CALL kr_stat_file_variable__variable_dim1(var%var, kgen_unit, printname // "%var", .FALSE.) 
       END IF   
         
   END SUBROUTINE kr_stat_file_module_stat_file 
     
   !write state subroutine for kr_stat_file_real__core_rknd_dim1 
   SUBROUTINE kr_stat_file_real__core_rknd_dim1(var, kgen_unit, printname, printvar) 
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
         
   END SUBROUTINE kr_stat_file_real__core_rknd_dim1 
     
   !write state subroutine for kr_stat_file_variable__variable_dim1 
   SUBROUTINE kr_stat_file_variable__variable_dim1(var, kgen_unit, printname, printvar) 
       TYPE(variable), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
           READ (UNIT = kgen_unit) kgen_bound(1, 1) 
           READ (UNIT = kgen_unit) kgen_bound(2, 1) 
           ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
           DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
               IF (PRESENT( printvar ) .AND. printvar) THEN 
                   CALL kr_stat_file_module_variable(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
               ELSE 
                   CALL kr_stat_file_module_variable(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
               END IF   
           END DO   
       END IF   
         
   END SUBROUTINE kr_stat_file_variable__variable_dim1 
     
   !verify state subroutine for kv_stat_file_module_variable 
   RECURSIVE SUBROUTINE kv_stat_file_module_variable(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       TYPE(variable), INTENT(IN) :: var, kgenref_var 
       TYPE(check_t) :: dtype_check_status, comp_check_status 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: n_ptr 
       real(KIND=stat_rknd) :: nrmsdiff_ptr, rmsdiff_ptr 
       real(KIND=stat_rknd), ALLOCATABLE :: buf1_ptr(:,:,:), buf2_ptr(:,:,:) 
       character(LEN=30) :: diff_name 
       character(LEN=100) :: diff_description 
       character(LEN=20) :: diff_units 
       integer :: diff_indx 
       logical :: diff_l_silhs 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
       IF (ASSOCIATED(var%ptr)) THEN 
           dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
           IF (ALL(var%ptr == kgenref_var%ptr)) THEN 
               dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%ptr is IDENTICAL." 
                   END IF   
               END IF   
               check_result = CHECK_IDENTICAL 
           ELSE 
               ALLOCATE (buf1_ptr(SIZE(var%ptr,dim=1),SIZE(var%ptr,dim=2),SIZE(var%ptr,dim=3))) 
               ALLOCATE (buf2_ptr(SIZE(var%ptr,dim=1),SIZE(var%ptr,dim=2),SIZE(var%ptr,dim=3))) 
               n_ptr = COUNT(var%ptr /= kgenref_var%ptr) 
               WHERE ( ABS(kgenref_var%ptr) > kgen_minvalue ) 
                   buf1_ptr = ((var%ptr-kgenref_var%ptr)/kgenref_var%ptr)**2 
                   buf2_ptr = (var%ptr-kgenref_var%ptr)**2 
               ELSEWHERE 
                   buf1_ptr = (var%ptr-kgenref_var%ptr)**2 
                   buf2_ptr = buf1_ptr 
               END WHERE   
               nrmsdiff_ptr = SQRT(SUM(buf1_ptr)/REAL(n_ptr)) 
               rmsdiff_ptr = SQRT(SUM(buf2_ptr)/REAL(n_ptr)) 
               IF (rmsdiff_ptr > kgen_tolerance) THEN 
                   dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), "%ptr is NOT IDENTICAL(out of tolerance)." 
                       END IF   
                   END IF   
                   check_result = CHECK_OUT_TOL 
               ELSE 
                   dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), "%ptr is NOT IDENTICAL(within tolerance)." 
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
                       WRITE (*, *) count( var%ptr /= kgenref_var%ptr), " of ", size( var%ptr ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var%ptr)/real(size(var%ptr)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var%ptr)/real(size(kgenref_var%ptr)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff_ptr 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ptr 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           ELSE IF (check_result == CHECK_IN_TOL) THEN 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank ==0) THEN 
                       WRITE (*, *) count( var%ptr /= kgenref_var%ptr), " of ", size( var%ptr ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var%ptr)/real(size(var%ptr)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var%ptr)/real(size(kgenref_var%ptr)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff_ptr 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ptr 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           END IF   
             
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%name == kgenref_var%name) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%name is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%name is NOT IDENTICAL." 
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
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%description == kgenref_var%description) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%description is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%description is NOT IDENTICAL." 
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
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%units == kgenref_var%units) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%units is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%units is NOT IDENTICAL." 
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
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%indx == kgenref_var%indx) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%indx is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_indx = ABS(var%indx - kgenref_var%indx) 
           IF (diff_indx <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%indx is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%indx is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_indx 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_indx 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%l_silhs .EQV. kgenref_var%l_silhs) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%l_silhs is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%l_silhs is NOT IDENTICAL." 
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
         
       IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
       ELSE IF (dtype_check_status%numOutTol > 0) THEN 
           check_status%numOutTol = check_status%numOutTol + 1 
       ELSE IF (dtype_check_status%numInTol > 0) THEN 
           check_status%numInTol = check_status%numInTol + 1 
       END IF   
   END SUBROUTINE kv_stat_file_module_variable 
     
   !verify state subroutine for kv_stat_file_module_stat_file 
   RECURSIVE SUBROUTINE kv_stat_file_module_stat_file(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       TYPE(stat_file), INTENT(IN) :: var, kgenref_var 
       TYPE(check_t) :: dtype_check_status, comp_check_status 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       character(LEN=200) :: diff_fname 
       character(LEN=200) :: diff_fdir 
       integer :: diff_iounit 
       integer :: diff_nrecord 
       integer :: diff_ntimes 
       logical :: diff_l_defined 
       logical :: diff_l_byte_swapped 
       integer :: diff_latdimid 
       integer :: diff_longdimid 
       integer :: diff_altdimid 
       integer :: diff_timedimid 
       integer :: diff_latvarid 
       integer :: diff_longvarid 
       integer :: diff_altvarid 
       integer :: diff_timevarid 
       integer :: diff_ia 
       integer :: diff_iz 
       integer :: diff_nlat 
       integer :: diff_nlon 
       INTEGER :: n_z 
       real(KIND=core_rknd) :: nrmsdiff_z, rmsdiff_z 
       real(KIND=core_rknd), ALLOCATABLE :: buf1_z(:), buf2_z(:) 
       integer :: diff_day 
       integer :: diff_month 
       integer :: diff_year 
       INTEGER :: n_rlat 
       real(KIND=core_rknd) :: nrmsdiff_rlat, rmsdiff_rlat 
       real(KIND=core_rknd), ALLOCATABLE :: buf1_rlat(:), buf2_rlat(:) 
       INTEGER :: n_rlon 
       real(KIND=core_rknd) :: nrmsdiff_rlon, rmsdiff_rlon 
       real(KIND=core_rknd), ALLOCATABLE :: buf1_rlon(:), buf2_rlon(:) 
       real(KIND=core_rknd) :: diff_dtwrite 
       real(KIND=time_precision) :: diff_time 
       integer :: diff_nvar 
       INTEGER :: idx1_var 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%fname == kgenref_var%fname) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%fname is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%fname is NOT IDENTICAL." 
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
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%fdir == kgenref_var%fdir) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%fdir is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%fdir is NOT IDENTICAL." 
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
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%iounit == kgenref_var%iounit) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%iounit is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_iounit = ABS(var%iounit - kgenref_var%iounit) 
           IF (diff_iounit <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%iounit is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%iounit is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_iounit 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_iounit 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%nrecord == kgenref_var%nrecord) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%nrecord is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_nrecord = ABS(var%nrecord - kgenref_var%nrecord) 
           IF (diff_nrecord <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nrecord is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nrecord is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_nrecord 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_nrecord 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%ntimes == kgenref_var%ntimes) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%ntimes is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_ntimes = ABS(var%ntimes - kgenref_var%ntimes) 
           IF (diff_ntimes <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%ntimes is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%ntimes is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_ntimes 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_ntimes 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%l_defined .EQV. kgenref_var%l_defined) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%l_defined is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%l_defined is NOT IDENTICAL." 
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
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%l_byte_swapped .EQV. kgenref_var%l_byte_swapped) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%l_byte_swapped is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%l_byte_swapped is NOT IDENTICAL." 
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
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%latdimid == kgenref_var%latdimid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%latdimid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_latdimid = ABS(var%latdimid - kgenref_var%latdimid) 
           IF (diff_latdimid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%latdimid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%latdimid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_latdimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_latdimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%longdimid == kgenref_var%longdimid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%longdimid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_longdimid = ABS(var%longdimid - kgenref_var%longdimid) 
           IF (diff_longdimid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%longdimid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%longdimid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_longdimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_longdimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%altdimid == kgenref_var%altdimid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%altdimid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_altdimid = ABS(var%altdimid - kgenref_var%altdimid) 
           IF (diff_altdimid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%altdimid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%altdimid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_altdimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_altdimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%timedimid == kgenref_var%timedimid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%timedimid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_timedimid = ABS(var%timedimid - kgenref_var%timedimid) 
           IF (diff_timedimid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%timedimid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%timedimid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_timedimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_timedimid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%latvarid == kgenref_var%latvarid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%latvarid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_latvarid = ABS(var%latvarid - kgenref_var%latvarid) 
           IF (diff_latvarid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%latvarid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%latvarid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_latvarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_latvarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%longvarid == kgenref_var%longvarid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%longvarid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_longvarid = ABS(var%longvarid - kgenref_var%longvarid) 
           IF (diff_longvarid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%longvarid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%longvarid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_longvarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_longvarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%altvarid == kgenref_var%altvarid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%altvarid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_altvarid = ABS(var%altvarid - kgenref_var%altvarid) 
           IF (diff_altvarid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%altvarid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%altvarid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_altvarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_altvarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%timevarid == kgenref_var%timevarid) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%timevarid is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_timevarid = ABS(var%timevarid - kgenref_var%timevarid) 
           IF (diff_timevarid <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%timevarid is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%timevarid is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_timevarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_timevarid 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%ia == kgenref_var%ia) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%ia is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_ia = ABS(var%ia - kgenref_var%ia) 
           IF (diff_ia <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%ia is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%ia is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_ia 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_ia 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%iz == kgenref_var%iz) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%iz is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_iz = ABS(var%iz - kgenref_var%iz) 
           IF (diff_iz <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%iz is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%iz is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_iz 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_iz 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%nlat == kgenref_var%nlat) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%nlat is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_nlat = ABS(var%nlat - kgenref_var%nlat) 
           IF (diff_nlat <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nlat is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nlat is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_nlat 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_nlat 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%nlon == kgenref_var%nlon) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%nlon is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_nlon = ABS(var%nlon - kgenref_var%nlon) 
           IF (diff_nlon <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nlon is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nlon is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_nlon 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_nlon 
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
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%day == kgenref_var%day) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%day is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_day = ABS(var%day - kgenref_var%day) 
           IF (diff_day <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%day is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%day is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_day 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_day 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%month == kgenref_var%month) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%month is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_month = ABS(var%month - kgenref_var%month) 
           IF (diff_month <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%month is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%month is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_month 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_month 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%year == kgenref_var%year) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%year is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_year = ABS(var%year - kgenref_var%year) 
           IF (diff_year <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%year is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%year is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_year 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_year 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       IF (ALLOCATED(var%rlat)) THEN 
           dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
           IF (ALL(var%rlat == kgenref_var%rlat)) THEN 
               dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%rlat is IDENTICAL." 
                   END IF   
               END IF   
               check_result = CHECK_IDENTICAL 
           ELSE 
               ALLOCATE (buf1_rlat(SIZE(var%rlat,dim=1))) 
               ALLOCATE (buf2_rlat(SIZE(var%rlat,dim=1))) 
               n_rlat = COUNT(var%rlat /= kgenref_var%rlat) 
               WHERE ( ABS(kgenref_var%rlat) > kgen_minvalue ) 
                   buf1_rlat = ((var%rlat-kgenref_var%rlat)/kgenref_var%rlat)**2 
                   buf2_rlat = (var%rlat-kgenref_var%rlat)**2 
               ELSEWHERE 
                   buf1_rlat = (var%rlat-kgenref_var%rlat)**2 
                   buf2_rlat = buf1_rlat 
               END WHERE   
               nrmsdiff_rlat = SQRT(SUM(buf1_rlat)/REAL(n_rlat)) 
               rmsdiff_rlat = SQRT(SUM(buf2_rlat)/REAL(n_rlat)) 
               IF (rmsdiff_rlat > kgen_tolerance) THEN 
                   dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), "%rlat is NOT IDENTICAL(out of tolerance)." 
                       END IF   
                   END IF   
                   check_result = CHECK_OUT_TOL 
               ELSE 
                   dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), "%rlat is NOT IDENTICAL(within tolerance)." 
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
                       WRITE (*, *) count( var%rlat /= kgenref_var%rlat), " of ", size( var%rlat ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var%rlat)/real(size(var%rlat)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var%rlat)/real(size(kgenref_var%rlat)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff_rlat 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rlat 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           ELSE IF (check_result == CHECK_IN_TOL) THEN 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank ==0) THEN 
                       WRITE (*, *) count( var%rlat /= kgenref_var%rlat), " of ", size( var%rlat ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var%rlat)/real(size(var%rlat)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var%rlat)/real(size(kgenref_var%rlat)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff_rlat 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rlat 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           END IF   
       END IF   
       IF (ALLOCATED(var%rlon)) THEN 
           dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
           IF (ALL(var%rlon == kgenref_var%rlon)) THEN 
               dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%rlon is IDENTICAL." 
                   END IF   
               END IF   
               check_result = CHECK_IDENTICAL 
           ELSE 
               ALLOCATE (buf1_rlon(SIZE(var%rlon,dim=1))) 
               ALLOCATE (buf2_rlon(SIZE(var%rlon,dim=1))) 
               n_rlon = COUNT(var%rlon /= kgenref_var%rlon) 
               WHERE ( ABS(kgenref_var%rlon) > kgen_minvalue ) 
                   buf1_rlon = ((var%rlon-kgenref_var%rlon)/kgenref_var%rlon)**2 
                   buf2_rlon = (var%rlon-kgenref_var%rlon)**2 
               ELSEWHERE 
                   buf1_rlon = (var%rlon-kgenref_var%rlon)**2 
                   buf2_rlon = buf1_rlon 
               END WHERE   
               nrmsdiff_rlon = SQRT(SUM(buf1_rlon)/REAL(n_rlon)) 
               rmsdiff_rlon = SQRT(SUM(buf2_rlon)/REAL(n_rlon)) 
               IF (rmsdiff_rlon > kgen_tolerance) THEN 
                   dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), "%rlon is NOT IDENTICAL(out of tolerance)." 
                       END IF   
                   END IF   
                   check_result = CHECK_OUT_TOL 
               ELSE 
                   dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                   IF (kgen_verboseLevel > 1) THEN 
                       IF (check_status%rank == 0) THEN 
                           WRITE (*, *) trim(adjustl(varname)), "%rlon is NOT IDENTICAL(within tolerance)." 
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
                       WRITE (*, *) count( var%rlon /= kgenref_var%rlon), " of ", size( var%rlon ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var%rlon)/real(size(var%rlon)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var%rlon)/real(size(kgenref_var%rlon)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff_rlon 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rlon 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           ELSE IF (check_result == CHECK_IN_TOL) THEN 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank ==0) THEN 
                       WRITE (*, *) count( var%rlon /= kgenref_var%rlon), " of ", size( var%rlon ), " elements are different." 
                       WRITE (*, *) "Average - kernel ", sum(var%rlon)/real(size(var%rlon)) 
                       WRITE (*, *) "Average - reference ", sum(kgenref_var%rlon)/real(size(kgenref_var%rlon)) 
                       WRITE (*, *) "RMS of difference is ", rmsdiff_rlon 
                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rlon 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           END IF   
             
       END IF   
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%dtwrite == kgenref_var%dtwrite) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%dtwrite is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_dtwrite = ABS(var%dtwrite - kgenref_var%dtwrite) 
           IF (diff_dtwrite <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%dtwrite is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%dtwrite is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_dtwrite 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_dtwrite 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%time == kgenref_var%time) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%time is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_time = ABS(var%time - kgenref_var%time) 
           IF (diff_time <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%time is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%time is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_time 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_time 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
       IF (var%nvar == kgenref_var%nvar) THEN 
           dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), "%nvar is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff_nvar = ABS(var%nvar - kgenref_var%nvar) 
           IF (diff_nvar <= kgen_tolerance) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nvar is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%nvar is NOT IDENTICAL(out of tolerance)." 
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
                   WRITE (*, *) "Difference is ", diff_nvar 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank ==0) THEN 
                   WRITE (*, *) "Difference is ", diff_nvar 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
       IF (ALLOCATED(var%var)) THEN 
           dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
           CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
           DO   idx1_var = LBOUND(var%var,1), UBOUND(var%var,1) 
               CALL kv_stat_file_module_variable(trim(adjustl(varname))//"%var", comp_check_status, var%var(idx1_var), &
               &kgenref_var%var(idx1_var)) 
           END DO   
           IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
               dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname))//"%var", " is IDENTICAL." 
                   END IF   
               END IF   
               check_result = CHECK_IDENTICAL 
           ELSE IF (comp_check_status%numOutTol > 0) THEN 
               dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%var is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           ELSE IF (comp_check_status%numInTol > 0) THEN 
               dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), "%var is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           END IF   
           IF (check_result == CHECK_IDENTICAL) THEN 
               CONTINUE 
           ELSE IF (check_result == CHECK_OUT_TOL) THEN 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank ==0) THEN 
                       WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                       WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                       WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                       WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                       WRITE (*, *) "" 
                   END IF   
               END IF   
           ELSE IF (check_result == CHECK_IN_TOL) THEN 
               IF (kgen_verboseLevel > 2) THEN 
                   IF (check_status%rank ==0) THEN 
                       WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                       WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                       WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                       WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                       WRITE (*, *) "" 
                   END IF   
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
   END SUBROUTINE kv_stat_file_module_stat_file 
     
 end module stat_file_module