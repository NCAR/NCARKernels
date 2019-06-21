!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  


module ESMF_ShrTimeMod
  !==============================================================================
  ! This file contains types and methods that are shared in the hierarchy
  !------------------------------------------------------------------------------
  ! INCLUDES
  !==============================================================================
  !BOPI
  ! !MODULE: ESMF_ShrTimeMod
  ! !DESCRIPTION:
  !------------------------------------------------------------------------------
  ! !USES:
  ! inherit from ESMF base class
  !
  !
  !

  !
  !
  ! inherit from base time class

    USE esmf_basetimemod 
    USE esmf_calendarmod 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
  !------------------------------------------------------------------------------
  ! !PRIVATE TYPES:
  !
    PRIVATE 
  !------------------------------------------------------------------------------
  !     ! ESMF_Time
  !     ! F90 class type to match C++ Time class in size only;
  !     !  all dereferencing within class is performed by C++ implementation
  !

  type ESMF_Time
     type(ESMF_BaseTime) :: basetime           ! inherit base class
     ! time instant is expressed as year + basetime
     integer :: YR
     type(ESMF_Calendar), pointer :: calendar => null() ! associated calendar
  end type ESMF_Time

  PUBLIC esmf_time 
  !==============================================================================
  PUBLIC kr_esmf_shrtimemod_esmf_time 
  PUBLIC kv_esmf_shrtimemod_esmf_time 
    
  CONTAINS 
    
  !read state subroutine for kr_esmf_shrtimemod_esmf_time 
  RECURSIVE SUBROUTINE kr_esmf_shrtimemod_esmf_time(var, kgen_unit, printname, printvar) 
      TYPE(esmf_time), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_basetimemod_esmf_basetime(var%basetime, kgen_unit, printname // "%basetime", .TRUE.) 
      ELSE 
          CALL kr_esmf_basetimemod_esmf_basetime(var%basetime, kgen_unit, printname // "%basetime", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%yr 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%yr = ", var%yr 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_esmf_time_subp11(var%calendar, kgen_unit, printname // "%calendar", .TRUE.) 
      ELSE 
          CALL kr_kgen_esmf_time_subp11(var%calendar, kgen_unit, printname // "%calendar", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_esmf_shrtimemod_esmf_time 
    
  !write state subroutine for kr_kgen_esmf_time_subp11 
  SUBROUTINE kr_kgen_esmf_time_subp11(var, kgen_unit, printname, printvar) 
      TYPE(esmf_calendar), INTENT(INOUT), POINTER :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          ALLOCATE (var) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              CALL kr_esmf_calendarmod_esmf_calendar(var, kgen_unit, printname, .TRUE.) 
          ELSE 
              CALL kr_esmf_calendarmod_esmf_calendar(var, kgen_unit, printname, .FALSE.) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_kgen_esmf_time_subp11 
    
  !verify state subroutine for kv_esmf_shrtimemod_esmf_time 
  RECURSIVE SUBROUTINE kv_esmf_shrtimemod_esmf_time(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_time), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_yr 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_basetimemod_esmf_basetime("basetime", comp_check_status, var%basetime, kgenref_var%basetime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%basetime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%basetime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%basetime is NOT IDENTICAL(within tolerance)." 
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
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%yr == kgenref_var%yr) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%yr is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_yr = ABS(var%yr - kgenref_var%yr) 
          IF (diff_yr <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%yr is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%yr is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_yr 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_yr 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%calendar)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
          CALL kv_esmf_calendarmod_esmf_calendar("calendar", comp_check_status, var%calendar, kgenref_var%calendar) 
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname))//"%calendar", " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%calendar is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%calendar is NOT IDENTICAL(within tolerance)." 
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
            
      END IF   
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_esmf_shrtimemod_esmf_time 
    
end module ESMF_ShrTimeMod