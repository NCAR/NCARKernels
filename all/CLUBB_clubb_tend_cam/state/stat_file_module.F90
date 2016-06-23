!-------------------------------------------------------------------------------
! $Id: stat_file_module.F90 7140 2014-07-31 19:14:05Z betlej@uwm.edu $
!===============================================================================
module stat_file_module
 

! Description:
!   Contains two derived types for describing the contents and location of
!   either NetCDF or GrADS files.
!-------------------------------------------------------------------------------
   use clubb_precision, only: & 
       stat_rknd,  & ! Variable
       time_precision, &
       core_rknd
 
   implicit none

   public :: variable, stat_file

   ! These are used in a 2D or 3D host model to output multiple columns
   ! Set clubb_i and clubb_j according to the column within the host model;
   ! The indices must not exceed nlon (for i) or nlat (for j).
   integer, save, public :: clubb_i = 1, clubb_j = 1
!$omp threadprivate(clubb_i, clubb_j)

   private ! Default scope

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

     real( kind = core_rknd ), dimension(:), pointer ::  & 
       z ! Height of vertical levels [m]

     ! Time information

     integer :: day, month, year ! Date of starting time

     real( kind = core_rknd ), dimension(:), pointer :: & 
       rlat, & ! Latitude                   [Degrees N]
       rlon    ! Longitude                  [Degrees E]

     real( kind = core_rknd ) :: & 
       dtwrite ! Interval between output    [Seconds]

     real( kind = time_precision ) ::  & 
       time    ! Start time                 [Seconds]

     ! Statistical Variables

     integer :: nvar  ! Number of variables for this file

     type (variable), dimension(:), pointer ::  & 
       var ! List and variable description

   end type stat_file

   PUBLIC kw_externs_in_stat_file_module 
   PUBLIC kw_stat_file_module_variable 
   PUBLIC kw_stat_file_module_stat_file 
     
   CONTAINS 
     
   !write in state subroutine for kw_externs_in_stat_file_module 
   SUBROUTINE kw_externs_in_stat_file_module(kgen_unit) 
       INTEGER, INTENT(IN) :: kgen_unit 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) clubb_i 
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) clubb_j 
   END SUBROUTINE kw_externs_in_stat_file_module 
     
   !read state subroutine for kw_stat_file_module_variable 
   RECURSIVE SUBROUTINE kw_stat_file_module_variable(var, kgen_unit, printvar) 
       TYPE(variable), INTENT(IN) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       IF (PRESENT( printvar )) THEN 
           CALL kw_variable_real__stat_rknd_dim3_ptr(var%ptr, kgen_unit, printvar // "%ptr") 
       ELSE 
           CALL kw_variable_real__stat_rknd_dim3_ptr(var%ptr, kgen_unit) 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%name 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%name **" // NEW_LINE("A"), var%name 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%description 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%description **" // NEW_LINE("A"), var%description 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%units 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%units **" // NEW_LINE("A"), var%units 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%indx 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%indx **" // NEW_LINE("A"), var%indx 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%l_silhs 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%l_silhs **" // NEW_LINE("A"), var%l_silhs 
       END IF   
         
   END SUBROUTINE kw_stat_file_module_variable 
     
   !write state subroutine for kw_variable_real__stat_rknd_dim3_ptr 
   SUBROUTINE kw_variable_real__stat_rknd_dim3_ptr(var, kgen_unit, printvar) 
       REAL(KIND=stat_rknd), INTENT(IN), POINTER, DIMENSION(:,:,:) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       INTEGER :: idx1, idx2, idx3 
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
           kgen_array_sum = REAL(SUM(var, mask=(var .eq. var)), 8) 
           WRITE (UNIT = kgen_unit) kgen_array_sum 
           WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
           WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
           WRITE (UNIT = kgen_unit) LBOUND(var, 2) 
           WRITE (UNIT = kgen_unit) UBOUND(var, 2) 
           WRITE (UNIT = kgen_unit) LBOUND(var, 3) 
           WRITE (UNIT = kgen_unit) UBOUND(var, 3) 
           WRITE (UNIT = kgen_unit) var 
           IF (PRESENT( printvar )) THEN 
               WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8) 
           END IF   
       END IF   
   END SUBROUTINE kw_variable_real__stat_rknd_dim3_ptr 
     
   !read state subroutine for kw_stat_file_module_stat_file 
   RECURSIVE SUBROUTINE kw_stat_file_module_stat_file(var, kgen_unit, printvar) 
       TYPE(stat_file), INTENT(IN) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%fname 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%fname **" // NEW_LINE("A"), var%fname 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%fdir 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%fdir **" // NEW_LINE("A"), var%fdir 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%iounit 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%iounit **" // NEW_LINE("A"), var%iounit 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%nrecord 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%nrecord **" // NEW_LINE("A"), var%nrecord 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%ntimes 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ntimes **" // NEW_LINE("A"), var%ntimes 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%l_defined 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%l_defined **" // NEW_LINE("A"), var%l_defined 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%l_byte_swapped 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%l_byte_swapped **" // NEW_LINE("A"), var%l_byte_swapped 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%latdimid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%latdimid **" // NEW_LINE("A"), var%latdimid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%longdimid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%longdimid **" // NEW_LINE("A"), var%longdimid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%altdimid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%altdimid **" // NEW_LINE("A"), var%altdimid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%timedimid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%timedimid **" // NEW_LINE("A"), var%timedimid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%latvarid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%latvarid **" // NEW_LINE("A"), var%latvarid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%longvarid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%longvarid **" // NEW_LINE("A"), var%longvarid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%altvarid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%altvarid **" // NEW_LINE("A"), var%altvarid 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%timevarid 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%timevarid **" // NEW_LINE("A"), var%timevarid 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%ia 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ia **" // NEW_LINE("A"), var%ia 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%iz 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%iz **" // NEW_LINE("A"), var%iz 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%nlat 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%nlat **" // NEW_LINE("A"), var%nlat 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%nlon 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%nlon **" // NEW_LINE("A"), var%nlon 
       END IF   
         
       IF (PRESENT( printvar )) THEN 
           CALL kw_stat_file_real__core_rknd_dim1_ptr(var%z, kgen_unit, printvar // "%z") 
       ELSE 
           CALL kw_stat_file_real__core_rknd_dim1_ptr(var%z, kgen_unit) 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%day 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%day **" // NEW_LINE("A"), var%day 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%month 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%month **" // NEW_LINE("A"), var%month 
       END IF   
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%year 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%year **" // NEW_LINE("A"), var%year 
       END IF   
         
       IF (PRESENT( printvar )) THEN 
           CALL kw_stat_file_real__core_rknd_dim1_ptr(var%rlat, kgen_unit, printvar // "%rlat") 
       ELSE 
           CALL kw_stat_file_real__core_rknd_dim1_ptr(var%rlat, kgen_unit) 
       END IF   
       IF (PRESENT( printvar )) THEN 
           CALL kw_stat_file_real__core_rknd_dim1_ptr(var%rlon, kgen_unit, printvar // "%rlon") 
       ELSE 
           CALL kw_stat_file_real__core_rknd_dim1_ptr(var%rlon, kgen_unit) 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%dtwrite 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%dtwrite **" // NEW_LINE("A"), var%dtwrite 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%time 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%time **" // NEW_LINE("A"), var%time 
       END IF   
         
       kgen_istrue = .TRUE. 
       WRITE (UNIT = kgen_unit) var%nvar 
       IF (PRESENT( printvar )) THEN 
           WRITE (*, *) "** KGEN DEBUG: " // printvar // "%nvar **" // NEW_LINE("A"), var%nvar 
       END IF   
         
       IF (PRESENT( printvar )) THEN 
           CALL kw_stat_file_variable__variable_dim1_ptr(var%var, kgen_unit, printvar // "%var") 
       ELSE 
           CALL kw_stat_file_variable__variable_dim1_ptr(var%var, kgen_unit) 
       END IF   
         
   END SUBROUTINE kw_stat_file_module_stat_file 
     
   !write state subroutine for kw_stat_file_real__core_rknd_dim1_ptr 
   SUBROUTINE kw_stat_file_real__core_rknd_dim1_ptr(var, kgen_unit, printvar) 
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
           kgen_array_sum = REAL(SUM(var, mask=(var .eq. var)), 8) 
           WRITE (UNIT = kgen_unit) kgen_array_sum 
           WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
           WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
           WRITE (UNIT = kgen_unit) var 
           IF (PRESENT( printvar )) THEN 
               WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8) 
           END IF   
       END IF   
   END SUBROUTINE kw_stat_file_real__core_rknd_dim1_ptr 
     
   !write state subroutine for kw_stat_file_variable__variable_dim1_ptr 
   SUBROUTINE kw_stat_file_variable__variable_dim1_ptr(var, kgen_unit, printvar) 
       TYPE(variable), INTENT(IN), POINTER, DIMENSION(:) :: var 
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
           WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
           WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
           DO idx1=LBOUND(var,1), UBOUND(var,1) 
               IF (PRESENT( printvar )) THEN 
                   CALL kw_stat_file_module_variable(var(idx1), kgen_unit, printvar // "(idx1)") 
               ELSE 
                   CALL kw_stat_file_module_variable(var(idx1), kgen_unit) 
               END IF   
           END DO   
       END IF   
   END SUBROUTINE kw_stat_file_variable__variable_dim1_ptr 
     
 end module stat_file_module