!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  
! $Id$
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
!     ESMF TimeInterval Module


!
!
!

module ESMF_TimeIntervalMod
  !==============================================================================
  ! This file contains the TimeInterval class definition and all TimeInterval
  ! class methods.
  !------------------------------------------------------------------------------
  ! INCLUDES
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
  !===============================================================================
  !BOPI
  ! !MODULE: ESMF_TimeIntervalMod
  ! !DESCRIPTION:
  ! Part of Time Manager F90 API wrapper of C++ implemenation
  ! Defines F90 wrapper entry points for corresponding
  ! C++ implementaion of class {\tt ESMC\_TimeInterval}
  ! See {\tt ../include/ESMC\_TimeInterval.h} for complete description
  !------------------------------------------------------------------------------
  ! !USES:
  ! inherit from ESMF base class

  !
  !
  !


  !
  !
  !
  !
  !
  ! inherit from base time class

    USE esmf_basetimemod 
  ! associated derived types

    USE esmf_shrtimemod, ONLY: esmf_time 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE esmf_shrtimemod, ONLY: kr_esmf_shrtimemod_esmf_time 
    USE esmf_shrtimemod, ONLY: kv_esmf_shrtimemod_esmf_time 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
  !------------------------------------------------------------------------------
  ! !PRIVATE TYPES:
  !
    PRIVATE 
  !------------------------------------------------------------------------------
  !     ! ESMF_TimeInterval
  !     ! F90 class type to match C++ TimeInterval class in size only;
  !     !  all dereferencing within class is performed by C++ implementation
  !

  type ESMF_TimeInterval
     ! time interval is expressed as basetime
     type(ESMF_BaseTime) :: basetime  ! inherit base class
     ! Relative year and month fields support monthly or yearly time
     ! intervals.  Many operations are undefined when these fields are
     ! non-zero!
     INTEGER :: YR                    ! relative year
     INTEGER :: MM                    ! relative month
     logical :: starttime_set           ! reference time set
     type(ESMF_Time) :: starttime       ! reference time
  end type ESMF_TimeInterval
  !------------------------------------------------------------------------------
  ! !PUBLIC TYPES:

  PUBLIC esmf_timeinterval 
  !------------------------------------------------------------------------------
  ! for running WRF, add three subroutines or functions (WRFADDITION_TimeIntervalGet,
  ! ESMF_TimeIntervalDIVQuot, ESMF_TimeIntervalIsPositive), by jhe
  ! !PUBLIC MEMBER FUNCTIONS:
  !
  ! Required inherited and overridden ESMF_Base class methods
!!!!!!!!! added by jhe


  ! !PRIVATE MEMBER FUNCTIONS:
  ! overloaded operator functions
  !


  ! Inherited and overloaded from ESMF_BaseTime


  !EOPI
  !------------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.

  !==============================================================================
  ! INTERFACE BLOCKS
  !==============================================================================
  !BOP
  ! !INTERFACE:

  !
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !==============================================================================
  PUBLIC kr_esmf_shrtimemod_esmf_time 
  PUBLIC kr_esmf_timeintervalmod_esmf_timeinterval 
  PUBLIC kv_esmf_shrtimemod_esmf_time 
  PUBLIC kv_esmf_timeintervalmod_esmf_timeinterval 
  !


  !==============================================================================
  ! Generic Get/Set routines which use F90 optional arguments
  !---------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeIntervalGet - Get value in user-specified units
  ! !INTERFACE:
    
  CONTAINS 
    

  !
  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeIntervalSet - Initialize via user-specified unit set
  ! !INTERFACE:
  !      subroutine ESMF_TimeIntervalSet(timeinterval, YY, YYl, MM, MOl, D, Dl, &
  !                                      H, M, S, Sl, MS, US, NS, &
  !                                      d_, d_r8, h_, m_, s_, ms_, us_, ns_, &
  !                                      Sn, Sd, startTime, rc)


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMFold_TimeIntervalGetString - Get time interval value in string format
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a time interval
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Get the negative absolute value of a time interval
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  ! This section includes overloaded operators defined only for TimeInterval
  ! (not inherited from BaseTime)
  ! Note:  these functions do not have a return code, since F90 forbids more
  ! than 2 arguments for arithmetic overloaded operators
  !------------------------------------------------------------------------------
  ! new WRF-specific function, Divide two time intervals and return the whole integer, without remainder

  !
  !


  ! added by jhe
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:   WRFADDITION_TimeIntervalProdI - Multiply a time interval by an
  ! integer
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide time interval by an integer, return time interval result
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:   ESMF_TimeIntervalProdI - Multiply a time interval by an integer
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:   ESMF_TimeIntervalProdI8 - Multiply a time interval by an integer
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  ! This section includes the inherited ESMF_BaseTime class overloaded operators
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalSum - Add two time intervals together
  ! !INTERFACE:


  !
  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one time interval from another
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeIntervalEQ - Compare two time intervals for equality
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalNE - Compare two time intervals for inequality
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalLT - Time interval 1 less than time interval 2 ?
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalGT - Time interval 1 greater than time interval 2?
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalLE - Time interval 1 less than or equal to time interval 2 ?
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalGE - Time interval 1 greater than or equal to time interval 2 ?
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalIsPositive - Time interval greater than zero?
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeIntervalPrint - Print out a time interval's properties
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  ! Exits with error message if timeInt is not normalized.


  !==============================================================================


  !==============================================================================


  !==============================================================================


  !==============================================================================


  !==============================================================================

  !read state subroutine for kr_esmf_timeintervalmod_esmf_timeinterval 
  RECURSIVE SUBROUTINE kr_esmf_timeintervalmod_esmf_timeinterval(var, kgen_unit, printname, printvar) 
      TYPE(esmf_timeinterval), INTENT(INOUT) :: var 
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
        
      READ (UNIT = kgen_unit) var%mm 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%mm = ", var%mm 
      END IF   
        
      READ (UNIT = kgen_unit) var%starttime_set 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%starttime_set = ", var%starttime_set 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%starttime, kgen_unit, printname // "%starttime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%starttime, kgen_unit, printname // "%starttime", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_esmf_timeintervalmod_esmf_timeinterval 
    
  !verify state subroutine for kv_esmf_timeintervalmod_esmf_timeinterval 
  RECURSIVE SUBROUTINE kv_esmf_timeintervalmod_esmf_timeinterval(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_timeinterval), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_yr 
      integer :: diff_mm 
      logical :: diff_starttime_set 
        
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
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%mm == kgenref_var%mm) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mm is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_mm = ABS(var%mm - kgenref_var%mm) 
          IF (diff_mm <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mm is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mm is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_mm 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_mm 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%starttime_set .EQV. kgenref_var%starttime_set) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%starttime_set is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%starttime_set is NOT IDENTICAL." 
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
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_shrtimemod_esmf_time("starttime", comp_check_status, var%starttime, kgenref_var%starttime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%starttime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%starttime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%starttime is NOT IDENTICAL(within tolerance)." 
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
  END SUBROUTINE kv_esmf_timeintervalmod_esmf_timeinterval 
    
end module ESMF_TimeIntervalMod