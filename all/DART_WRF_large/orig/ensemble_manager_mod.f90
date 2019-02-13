!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: ensemble_manager_mod.f90 12591 2018-05-21 20:49:26Z nancy@ucar.edu $


!

module ensemble_manager_mod
! Manages a data structure that is composed of copies of a vector of variables.
! The general data structure simply represents this two-dimensional array but
! provides tools for storing it with one dimension (copies) or the other 
! (variables) complete on each process of a multiprocess implementation. Some
! operations that are specifically aimed at supporting ensemble filter applications
! have been placed here for efficiency even though they might be more 
! appropriately abstracted at a higher level of code.


    USE types_mod, ONLY: r8, i8 

    USE time_manager_mod, ONLY: time_type 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE time_manager_mod, ONLY: kr_time_manager_mod_time_type 
    USE time_manager_mod, ONLY: kv_time_manager_mod_time_type 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 


    IMPLICIT NONE 
    PRIVATE 


    PUBLIC ensemble_type 
! version controlled file description for error handling, do not edit


type ensemble_type
!>@todo update documentation with regard to 'single_restart_file_[in,out]'
!>@todo FIXME the rule here should be that we only access %copies and %vars for efficiency
!>but every other part of this structure should go through accessor routines.
   !DIRECT ACCESS INTO STORAGE IS USED TO REDUCE COPYING: BE CAREFUL
   !!!private


   integer(i8)                  :: num_vars
   integer                      :: num_copies, my_num_copies, my_num_vars
   integer,        allocatable  :: my_copies(:)
   integer(i8),    allocatable  :: my_vars(:)
   ! Storage in next line is to be used when each pe has all copies of subset of vars
   real(r8),       allocatable  :: copies(:, :)         ! Dimensioned (num_copies, my_num_vars)
   ! Storage on next line is used when each pe has subset of copies of all vars
   real(r8),       allocatable  :: vars(:, :)           ! Dimensioned (num_vars, my_num_copies)
   ! Time is only related to var complete
   type(time_type), allocatable :: time(:)
   integer                      :: distribution_type
   integer                      :: valid     ! copies modified last, vars modified last, both same
   integer                      :: id_num
   integer, allocatable         :: task_to_pe_list(:), pe_to_task_list(:) ! List of tasks
   ! Flexible my_pe, layout_type which allows different task layouts for different ensemble handles
   integer                      :: my_pe
   integer                      :: layout_type
   integer                      :: transpose_type
   integer                      :: num_extras
   type(time_type)              :: current_time ! The current time, constant across the ensemble

end type ensemble_type
!PAR other storage option control can be implemented here. In particular, want to find
!PAR some way, either allocating or multiple addressing, to use same chunk of storage
!PAR for both copy and var complete representations.
! track if copies modified last, vars modified last, both are in sync
! (and therefore both valid to be used r/o), or unknown.


! unique counter per ensemble handle

! Logical flag for initialization of module

! Module storage for writing error messages

! Module storage for pe information for this process avoids recomputation

! Control order of communication loops in the transpose routines

!-----------------------------------------------------------------
! namelist with default values
! Complain if unneeded transposes are done
!>@todo remove all things related to this
! logical  :: flag_unneeded_transposes = .false.
! Communication configuration:
!  1 = usual default, 2 - 4 are valid and depend on the machine, ensemble count, and task count

!

! task layout options:

!-----------------------------------------------------------------
PUBLIC kr_time_manager_mod_time_type 
PUBLIC kr_ensemble_manager_mod_ensemble_type 
PUBLIC kv_time_manager_mod_time_type 
PUBLIC kv_ensemble_manager_mod_ensemble_type 
                                  

!-----------------------------------------------------------------
  
CONTAINS 
  


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------
!> accessor function


!--------------------------------------------------------------------------------
!> Return the physical task for my_pe


!--------------------------------------------------------------------------------
!> return the number of actual ensemble members (not extra copies)


!--------------------------------------------------------------------------------
!> return the index of the mean row
!> mean row is the row in state_ens_handle%copies(:,:) which is the mean. Typically
!> has been state_ens_handle%copies -6 ( just the regular ensemble members


!--------------------------------------------------------------------------------
!> Aim: allow filter to set the number of extra copies in this module
!> This is necessary for copies_in_window, mean_row
!> This is really ugly.


!-----------------------------------------------------------------
!-----------------------------------------------------------------


!-----------------------------------------------------------------


!-----------------------------------------------------------------


!--------------------------------------------------------------------------------


!--------------------------------------------------------------------------------


!--------------------------------------------------------------------------------


!--------------------------------------------------------------------------------
! print an ensemble handle file type.  normally won't print unless 'debug' in the
! namelist is true, but 'force' will override that and print no matter what.
! if 'contents' is true, print the %copies and %vars arrays.  set integer 'limit'
! to print only the first N values for each.


!--------------------------------------------------------------------------------


!------------------------------------------------------------------------------


!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!------------------------------------------------------------------------------
!> sorts an array and returns the sorted array, and the index of the original
!> array


!--------------------------------------------------------------------------------
!> ! Return my_pe corresponding to the physical task


!--------------------------------------------------------------------------------
!> if allow_transpose is ok, allocate the vars if they aren't already allocated,
!> error out if allow_transpose is false.


!--------------------------------------------------------------------------------
!> not clear if we want to deallocate the vars array - if we needed it once
!> we'll probably need it again.  but for completeness, make an explicit dealloc.


!--------------------------------------------------------------------------------
!> allocate enough space to an allocatable array to hold a single copy
!> requires the ens_handle be copy-complete.  must know copy number to
!> know how many items are on this task.  must be a collective call.


!--------------------------------------------------------------------------------
!> get the data from the ensemble handle for this single copy number
!> requires the ens_handle be copy-complete


!--------------------------------------------------------------------------------
!> put the data from an array into the ensemble handle for this single copy number
!> requires the ens_handle be copy-complete


!--------------------------------------------------------------------------------
!> cleanup routine


!--------------------------------------------------------------------------------
!> accessor routines for the single 'current_time'.  all mpi tasks must call this
!> so there's a consistent view of the current time, even if they didn't advance
!> a model.


!--------------------------------------------------------------------------------


!---------------------------------------------------------------------------------


!read state subroutine for kr_ensemble_manager_mod_ensemble_type 
RECURSIVE SUBROUTINE kr_ensemble_manager_mod_ensemble_type(var, kgen_unit, printname, printvar) 
    TYPE(ensemble_type), INTENT(INOUT) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) var%num_vars 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%num_vars = ", var%num_vars 
    END IF   
      
    READ (UNIT = kgen_unit) var%num_copies 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%num_copies = ", var%num_copies 
    END IF   
    READ (UNIT = kgen_unit) var%my_num_copies 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%my_num_copies = ", var%my_num_copies 
    END IF   
    READ (UNIT = kgen_unit) var%my_num_vars 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%my_num_vars = ", var%my_num_vars 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_integer___dim1(var%my_copies, kgen_unit, printname // "%my_copies", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_integer___dim1(var%my_copies, kgen_unit, printname // "%my_copies", .FALSE.) 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_integer__i8_dim1(var%my_vars, kgen_unit, printname // "%my_vars", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_integer__i8_dim1(var%my_vars, kgen_unit, printname // "%my_vars", .FALSE.) 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_real__r8_dim2(var%copies, kgen_unit, printname // "%copies", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_real__r8_dim2(var%copies, kgen_unit, printname // "%copies", .FALSE.) 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_real__r8_dim2(var%vars, kgen_unit, printname // "%vars", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_real__r8_dim2(var%vars, kgen_unit, printname // "%vars", .FALSE.) 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_time_type__time_type_dim1(var%time, kgen_unit, printname // "%time", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_time_type__time_type_dim1(var%time, kgen_unit, printname // "%time", .FALSE.) 
    END IF   
      
    READ (UNIT = kgen_unit) var%distribution_type 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%distribution_type = ", var%distribution_type 
    END IF   
      
    READ (UNIT = kgen_unit) var%valid 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%valid = ", var%valid 
    END IF   
      
    READ (UNIT = kgen_unit) var%id_num 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%id_num = ", var%id_num 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_integer___dim1(var%task_to_pe_list, kgen_unit, printname // "%task_to_pe_list", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_integer___dim1(var%task_to_pe_list, kgen_unit, printname // "%task_to_pe_list", .FALSE.) 
    END IF   
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_ensemble_type_integer___dim1(var%pe_to_task_list, kgen_unit, printname // "%pe_to_task_list", .TRUE.) 
    ELSE 
        CALL kr_ensemble_type_integer___dim1(var%pe_to_task_list, kgen_unit, printname // "%pe_to_task_list", .FALSE.) 
    END IF   
      
    READ (UNIT = kgen_unit) var%my_pe 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%my_pe = ", var%my_pe 
    END IF   
      
    READ (UNIT = kgen_unit) var%layout_type 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%layout_type = ", var%layout_type 
    END IF   
      
    READ (UNIT = kgen_unit) var%transpose_type 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%transpose_type = ", var%transpose_type 
    END IF   
      
    READ (UNIT = kgen_unit) var%num_extras 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%num_extras = ", var%num_extras 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_time_manager_mod_time_type(var%current_time, kgen_unit, printname // "%current_time", .TRUE.) 
    ELSE 
        CALL kr_time_manager_mod_time_type(var%current_time, kgen_unit, printname // "%current_time", .FALSE.) 
    END IF   
      
END SUBROUTINE kr_ensemble_manager_mod_ensemble_type 
  
!write state subroutine for kr_ensemble_type_integer___dim1 
SUBROUTINE kr_ensemble_type_integer___dim1(var, kgen_unit, printname, printvar) 
    INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
      
END SUBROUTINE kr_ensemble_type_integer___dim1 
  
!write state subroutine for kr_ensemble_type_integer__i8_dim1 
SUBROUTINE kr_ensemble_type_integer__i8_dim1(var, kgen_unit, printname, printvar) 
    INTEGER(KIND=i8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
      
END SUBROUTINE kr_ensemble_type_integer__i8_dim1 
  
!write state subroutine for kr_ensemble_type_real__r8_dim2 
SUBROUTINE kr_ensemble_type_real__r8_dim2(var, kgen_unit, printname, printvar) 
    REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: idx1, idx2 
    INTEGER, DIMENSION(2,2) :: kgen_bound 
      
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
        ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
        READ (UNIT = kgen_unit) var 
        CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
        END IF   
    END IF   
      
END SUBROUTINE kr_ensemble_type_real__r8_dim2 
  
!write state subroutine for kr_ensemble_type_time_type__time_type_dim1 
SUBROUTINE kr_ensemble_type_time_type__time_type_dim1(var, kgen_unit, printname, printvar) 
    TYPE(time_type), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
                CALL kr_time_manager_mod_time_type(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
            ELSE 
                CALL kr_time_manager_mod_time_type(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
            END IF   
        END DO   
    END IF   
      
END SUBROUTINE kr_ensemble_type_time_type__time_type_dim1 
  
!verify state subroutine for kv_ensemble_manager_mod_ensemble_type 
RECURSIVE SUBROUTINE kv_ensemble_manager_mod_ensemble_type(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    TYPE(ensemble_type), INTENT(IN) :: var, kgenref_var 
    TYPE(check_t) :: dtype_check_status, comp_check_status 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    integer(KIND=i8) :: diff_num_vars 
    integer :: diff_num_copies 
    integer :: diff_my_num_copies 
    integer :: diff_my_num_vars 
    INTEGER :: n_my_copies 
    integer :: nrmsdiff_my_copies, rmsdiff_my_copies 
    integer, ALLOCATABLE :: buf1_my_copies(:), buf2_my_copies(:) 
    INTEGER :: n_my_vars 
    integer(KIND=i8) :: nrmsdiff_my_vars, rmsdiff_my_vars 
    integer(KIND=i8), ALLOCATABLE :: buf1_my_vars(:), buf2_my_vars(:) 
    INTEGER :: n_copies 
    real(KIND=r8) :: nrmsdiff_copies, rmsdiff_copies 
    real(KIND=r8), ALLOCATABLE :: buf1_copies(:,:), buf2_copies(:,:) 
    INTEGER :: n_vars 
    real(KIND=r8) :: nrmsdiff_vars, rmsdiff_vars 
    real(KIND=r8), ALLOCATABLE :: buf1_vars(:,:), buf2_vars(:,:) 
    INTEGER :: idx1_time 
    integer :: diff_distribution_type 
    integer :: diff_valid 
    integer :: diff_id_num 
    INTEGER :: n_task_to_pe_list 
    integer :: nrmsdiff_task_to_pe_list, rmsdiff_task_to_pe_list 
    integer, ALLOCATABLE :: buf1_task_to_pe_list(:), buf2_task_to_pe_list(:) 
    INTEGER :: n_pe_to_task_list 
    integer :: nrmsdiff_pe_to_task_list, rmsdiff_pe_to_task_list 
    integer, ALLOCATABLE :: buf1_pe_to_task_list(:), buf2_pe_to_task_list(:) 
    integer :: diff_my_pe 
    integer :: diff_layout_type 
    integer :: diff_transpose_type 
    integer :: diff_num_extras 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%num_vars == kgenref_var%num_vars) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%num_vars is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_num_vars = ABS(var%num_vars - kgenref_var%num_vars) 
        IF (diff_num_vars <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%num_vars is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%num_vars is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_num_vars 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_num_vars 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%num_copies == kgenref_var%num_copies) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%num_copies is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_num_copies = ABS(var%num_copies - kgenref_var%num_copies) 
        IF (diff_num_copies <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%num_copies is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%num_copies is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_num_copies 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_num_copies 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%my_num_copies == kgenref_var%my_num_copies) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%my_num_copies is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_my_num_copies = ABS(var%my_num_copies - kgenref_var%my_num_copies) 
        IF (diff_my_num_copies <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_num_copies is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_num_copies is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_my_num_copies 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_my_num_copies 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%my_num_vars == kgenref_var%my_num_vars) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%my_num_vars is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_my_num_vars = ABS(var%my_num_vars - kgenref_var%my_num_vars) 
        IF (diff_my_num_vars <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_num_vars is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_num_vars is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_my_num_vars 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_my_num_vars 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    IF (ALLOCATED(var%my_copies)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        IF (ALL(var%my_copies == kgenref_var%my_copies)) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_copies is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1_my_copies(SIZE(var%my_copies,dim=1))) 
            ALLOCATE (buf2_my_copies(SIZE(var%my_copies,dim=1))) 
            n_my_copies = COUNT(var%my_copies /= kgenref_var%my_copies) 
            WHERE ( ABS(kgenref_var%my_copies) > kgen_minvalue ) 
                buf1_my_copies = ((var%my_copies-kgenref_var%my_copies)/kgenref_var%my_copies)**2 
                buf2_my_copies = (var%my_copies-kgenref_var%my_copies)**2 
            ELSEWHERE 
                buf1_my_copies = (var%my_copies-kgenref_var%my_copies)**2 
                buf2_my_copies = buf1_my_copies 
            END WHERE   
            nrmsdiff_my_copies = SQRT(SUM(buf1_my_copies)/REAL(n_my_copies)) 
            rmsdiff_my_copies = SQRT(SUM(buf2_my_copies)/REAL(n_my_copies)) 
            IF (rmsdiff_my_copies > kgen_tolerance) THEN 
                dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%my_copies is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            ELSE 
                dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%my_copies is NOT IDENTICAL(within tolerance)." 
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
                    WRITE (*, *) count( var%my_copies /= kgenref_var%my_copies), " of ", size( var%my_copies ), " elements are &
                    &different." 
                    WRITE (*, *) "Average - kernel ", sum(var%my_copies)/real(size(var%my_copies)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%my_copies)/real(size(kgenref_var%my_copies)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_my_copies 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_my_copies 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank ==0) THEN 
                    WRITE (*, *) count( var%my_copies /= kgenref_var%my_copies), " of ", size( var%my_copies ), " elements are &
                    &different." 
                    WRITE (*, *) "Average - kernel ", sum(var%my_copies)/real(size(var%my_copies)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%my_copies)/real(size(kgenref_var%my_copies)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_my_copies 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_my_copies 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
          
    END IF   
    IF (ALLOCATED(var%my_vars)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        IF (ALL(var%my_vars == kgenref_var%my_vars)) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_vars is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1_my_vars(SIZE(var%my_vars,dim=1))) 
            ALLOCATE (buf2_my_vars(SIZE(var%my_vars,dim=1))) 
            n_my_vars = COUNT(var%my_vars /= kgenref_var%my_vars) 
            WHERE ( ABS(kgenref_var%my_vars) > kgen_minvalue ) 
                buf1_my_vars = ((var%my_vars-kgenref_var%my_vars)/kgenref_var%my_vars)**2 
                buf2_my_vars = (var%my_vars-kgenref_var%my_vars)**2 
            ELSEWHERE 
                buf1_my_vars = (var%my_vars-kgenref_var%my_vars)**2 
                buf2_my_vars = buf1_my_vars 
            END WHERE   
            nrmsdiff_my_vars = SQRT(SUM(buf1_my_vars)/REAL(n_my_vars)) 
            rmsdiff_my_vars = SQRT(SUM(buf2_my_vars)/REAL(n_my_vars)) 
            IF (rmsdiff_my_vars > kgen_tolerance) THEN 
                dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%my_vars is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            ELSE 
                dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%my_vars is NOT IDENTICAL(within tolerance)." 
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
                    WRITE (*, *) count( var%my_vars /= kgenref_var%my_vars), " of ", size( var%my_vars ), " elements are &
                    &different." 
                    WRITE (*, *) "Average - kernel ", sum(var%my_vars)/real(size(var%my_vars)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%my_vars)/real(size(kgenref_var%my_vars)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_my_vars 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_my_vars 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank ==0) THEN 
                    WRITE (*, *) count( var%my_vars /= kgenref_var%my_vars), " of ", size( var%my_vars ), " elements are &
                    &different." 
                    WRITE (*, *) "Average - kernel ", sum(var%my_vars)/real(size(var%my_vars)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%my_vars)/real(size(kgenref_var%my_vars)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_my_vars 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_my_vars 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
          
    END IF   
    IF (ALLOCATED(var%copies)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        IF (ALL(var%copies == kgenref_var%copies)) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%copies is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1_copies(SIZE(var%copies,dim=1),SIZE(var%copies,dim=2))) 
            ALLOCATE (buf2_copies(SIZE(var%copies,dim=1),SIZE(var%copies,dim=2))) 
            n_copies = COUNT(var%copies /= kgenref_var%copies) 
            WHERE ( ABS(kgenref_var%copies) > kgen_minvalue ) 
                buf1_copies = ((var%copies-kgenref_var%copies)/kgenref_var%copies)**2 
                buf2_copies = (var%copies-kgenref_var%copies)**2 
            ELSEWHERE 
                buf1_copies = (var%copies-kgenref_var%copies)**2 
                buf2_copies = buf1_copies 
            END WHERE   
            nrmsdiff_copies = SQRT(SUM(buf1_copies)/REAL(n_copies)) 
            rmsdiff_copies = SQRT(SUM(buf2_copies)/REAL(n_copies)) 
            IF (rmsdiff_copies > kgen_tolerance) THEN 
                dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%copies is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            ELSE 
                dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%copies is NOT IDENTICAL(within tolerance)." 
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
                    WRITE (*, *) count( var%copies /= kgenref_var%copies), " of ", size( var%copies ), " elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%copies)/real(size(var%copies)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%copies)/real(size(kgenref_var%copies)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_copies 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_copies 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank ==0) THEN 
                    WRITE (*, *) count( var%copies /= kgenref_var%copies), " of ", size( var%copies ), " elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%copies)/real(size(var%copies)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%copies)/real(size(kgenref_var%copies)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_copies 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_copies 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
          
    END IF   
    IF (ALLOCATED(var%vars)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        IF (ALL(var%vars == kgenref_var%vars)) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%vars is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1_vars(SIZE(var%vars,dim=1),SIZE(var%vars,dim=2))) 
            ALLOCATE (buf2_vars(SIZE(var%vars,dim=1),SIZE(var%vars,dim=2))) 
            n_vars = COUNT(var%vars /= kgenref_var%vars) 
            WHERE ( ABS(kgenref_var%vars) > kgen_minvalue ) 
                buf1_vars = ((var%vars-kgenref_var%vars)/kgenref_var%vars)**2 
                buf2_vars = (var%vars-kgenref_var%vars)**2 
            ELSEWHERE 
                buf1_vars = (var%vars-kgenref_var%vars)**2 
                buf2_vars = buf1_vars 
            END WHERE   
            nrmsdiff_vars = SQRT(SUM(buf1_vars)/REAL(n_vars)) 
            rmsdiff_vars = SQRT(SUM(buf2_vars)/REAL(n_vars)) 
            IF (rmsdiff_vars > kgen_tolerance) THEN 
                dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%vars is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            ELSE 
                dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%vars is NOT IDENTICAL(within tolerance)." 
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
                    WRITE (*, *) count( var%vars /= kgenref_var%vars), " of ", size( var%vars ), " elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%vars)/real(size(var%vars)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%vars)/real(size(kgenref_var%vars)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_vars 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vars 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank ==0) THEN 
                    WRITE (*, *) count( var%vars /= kgenref_var%vars), " of ", size( var%vars ), " elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%vars)/real(size(var%vars)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%vars)/real(size(kgenref_var%vars)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_vars 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vars 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
          
    END IF   
    IF (ALLOCATED(var%time)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
        DO   idx1_time = LBOUND(var%time,1), UBOUND(var%time,1) 
            CALL kv_time_manager_mod_time_type(trim(adjustl(varname))//"%time", comp_check_status, var%time(idx1_time), &
            &kgenref_var%time(idx1_time)) 
        END DO   
        IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname))//"%time", " is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE IF (comp_check_status%numOutTol > 0) THEN 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%time is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        ELSE IF (comp_check_status%numInTol > 0) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%time is NOT IDENTICAL(within tolerance)." 
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
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%distribution_type == kgenref_var%distribution_type) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%distribution_type is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_distribution_type = ABS(var%distribution_type - kgenref_var%distribution_type) 
        IF (diff_distribution_type <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%distribution_type is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%distribution_type is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_distribution_type 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_distribution_type 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%valid == kgenref_var%valid) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%valid is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_valid = ABS(var%valid - kgenref_var%valid) 
        IF (diff_valid <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%valid is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%valid is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_valid 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_valid 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%id_num == kgenref_var%id_num) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%id_num is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_id_num = ABS(var%id_num - kgenref_var%id_num) 
        IF (diff_id_num <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%id_num is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%id_num is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_id_num 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_id_num 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    IF (ALLOCATED(var%task_to_pe_list)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        IF (ALL(var%task_to_pe_list == kgenref_var%task_to_pe_list)) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%task_to_pe_list is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1_task_to_pe_list(SIZE(var%task_to_pe_list,dim=1))) 
            ALLOCATE (buf2_task_to_pe_list(SIZE(var%task_to_pe_list,dim=1))) 
            n_task_to_pe_list = COUNT(var%task_to_pe_list /= kgenref_var%task_to_pe_list) 
            WHERE ( ABS(kgenref_var%task_to_pe_list) > kgen_minvalue ) 
                buf1_task_to_pe_list = ((var%task_to_pe_list-kgenref_var%task_to_pe_list)/kgenref_var%task_to_pe_list)**2 
                buf2_task_to_pe_list = (var%task_to_pe_list-kgenref_var%task_to_pe_list)**2 
            ELSEWHERE 
                buf1_task_to_pe_list = (var%task_to_pe_list-kgenref_var%task_to_pe_list)**2 
                buf2_task_to_pe_list = buf1_task_to_pe_list 
            END WHERE   
            nrmsdiff_task_to_pe_list = SQRT(SUM(buf1_task_to_pe_list)/REAL(n_task_to_pe_list)) 
            rmsdiff_task_to_pe_list = SQRT(SUM(buf2_task_to_pe_list)/REAL(n_task_to_pe_list)) 
            IF (rmsdiff_task_to_pe_list > kgen_tolerance) THEN 
                dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%task_to_pe_list is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            ELSE 
                dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%task_to_pe_list is NOT IDENTICAL(within tolerance)." 
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
                    WRITE (*, *) count( var%task_to_pe_list /= kgenref_var%task_to_pe_list), " of ", size( var%task_to_pe_list ), &
                    &" elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%task_to_pe_list)/real(size(var%task_to_pe_list)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%task_to_pe_list)/real(size(kgenref_var%task_to_pe_list)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_task_to_pe_list 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_task_to_pe_list 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank ==0) THEN 
                    WRITE (*, *) count( var%task_to_pe_list /= kgenref_var%task_to_pe_list), " of ", size( var%task_to_pe_list ), &
                    &" elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%task_to_pe_list)/real(size(var%task_to_pe_list)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%task_to_pe_list)/real(size(kgenref_var%task_to_pe_list)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_task_to_pe_list 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_task_to_pe_list 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
    END IF   
    IF (ALLOCATED(var%pe_to_task_list)) THEN 
        dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
        IF (ALL(var%pe_to_task_list == kgenref_var%pe_to_task_list)) THEN 
            dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%pe_to_task_list is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1_pe_to_task_list(SIZE(var%pe_to_task_list,dim=1))) 
            ALLOCATE (buf2_pe_to_task_list(SIZE(var%pe_to_task_list,dim=1))) 
            n_pe_to_task_list = COUNT(var%pe_to_task_list /= kgenref_var%pe_to_task_list) 
            WHERE ( ABS(kgenref_var%pe_to_task_list) > kgen_minvalue ) 
                buf1_pe_to_task_list = ((var%pe_to_task_list-kgenref_var%pe_to_task_list)/kgenref_var%pe_to_task_list)**2 
                buf2_pe_to_task_list = (var%pe_to_task_list-kgenref_var%pe_to_task_list)**2 
            ELSEWHERE 
                buf1_pe_to_task_list = (var%pe_to_task_list-kgenref_var%pe_to_task_list)**2 
                buf2_pe_to_task_list = buf1_pe_to_task_list 
            END WHERE   
            nrmsdiff_pe_to_task_list = SQRT(SUM(buf1_pe_to_task_list)/REAL(n_pe_to_task_list)) 
            rmsdiff_pe_to_task_list = SQRT(SUM(buf2_pe_to_task_list)/REAL(n_pe_to_task_list)) 
            IF (rmsdiff_pe_to_task_list > kgen_tolerance) THEN 
                dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%pe_to_task_list is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            ELSE 
                dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), "%pe_to_task_list is NOT IDENTICAL(within tolerance)." 
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
                    WRITE (*, *) count( var%pe_to_task_list /= kgenref_var%pe_to_task_list), " of ", size( var%pe_to_task_list ), &
                    &" elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%pe_to_task_list)/real(size(var%pe_to_task_list)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%pe_to_task_list)/real(size(kgenref_var%pe_to_task_list)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_pe_to_task_list 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pe_to_task_list 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank ==0) THEN 
                    WRITE (*, *) count( var%pe_to_task_list /= kgenref_var%pe_to_task_list), " of ", size( var%pe_to_task_list ), &
                    &" elements are different." 
                    WRITE (*, *) "Average - kernel ", sum(var%pe_to_task_list)/real(size(var%pe_to_task_list)) 
                    WRITE (*, *) "Average - reference ", sum(kgenref_var%pe_to_task_list)/real(size(kgenref_var%pe_to_task_list)) 
                    WRITE (*, *) "RMS of difference is ", rmsdiff_pe_to_task_list 
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pe_to_task_list 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
          
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%my_pe == kgenref_var%my_pe) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%my_pe is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_my_pe = ABS(var%my_pe - kgenref_var%my_pe) 
        IF (diff_my_pe <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_pe is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%my_pe is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_my_pe 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_my_pe 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%layout_type == kgenref_var%layout_type) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%layout_type is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_layout_type = ABS(var%layout_type - kgenref_var%layout_type) 
        IF (diff_layout_type <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%layout_type is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%layout_type is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_layout_type 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_layout_type 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%transpose_type == kgenref_var%transpose_type) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%transpose_type is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_transpose_type = ABS(var%transpose_type - kgenref_var%transpose_type) 
        IF (diff_transpose_type <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%transpose_type is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%transpose_type is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_transpose_type 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_transpose_type 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%num_extras == kgenref_var%num_extras) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%num_extras is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_num_extras = ABS(var%num_extras - kgenref_var%num_extras) 
        IF (diff_num_extras <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%num_extras is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%num_extras is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_num_extras 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_num_extras 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
    CALL kv_time_manager_mod_time_type("current_time", comp_check_status, var%current_time, kgenref_var%current_time) 
    IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname))//"%current_time", " is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE IF (comp_check_status%numOutTol > 0) THEN 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%current_time is NOT IDENTICAL(out of tolerance)." 
            END IF   
        END IF   
        check_result = CHECK_OUT_TOL 
    ELSE IF (comp_check_status%numInTol > 0) THEN 
        dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%current_time is NOT IDENTICAL(within tolerance)." 
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
END SUBROUTINE kv_ensemble_manager_mod_ensemble_type 
  
end module ensemble_manager_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/ensemble_manager_mod.f90 $
! $Id: ensemble_manager_mod.f90 12591 2018-05-21 20:49:26Z nancy@ucar.edu $
! $Revision: 12591 $
! $Date: 2018-05-21 14:49:26 -0600 (Mon, 21 May 2018) $
