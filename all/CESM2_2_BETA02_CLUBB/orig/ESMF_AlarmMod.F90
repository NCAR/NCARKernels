!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
!     ESMF Alarm Module


!
!
module ESMF_AlarmMod
  !==============================================================================
  ! This file contains the Alarm class definition and all Alarm class
  ! methods.
  !------------------------------------------------------------------------------
  ! INCLUDES
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
  !===============================================================================
  !BOPI
  ! !MODULE: ESMF_AlarmMod
  ! !DESCRIPTION:
  ! Part of Time Manager F90 API wrapper of C++ implemenation
  ! Defines F90 wrapper entry points for corresponding
  ! C++ class {\tt ESMC\_Alarm}
  ! See {\tt ../include/ESMC\_Alarm.h} for complete description
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
  ! associated derived types

    USE esmf_timeintervalmod 
    USE esmf_timemod 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
  !------------------------------------------------------------------------------
  ! !PRIVATE TYPES:

    PRIVATE 
  !------------------------------------------------------------------------------
  !     ! ESMF_Alarm
  !     ! F90 class type to match C++ Alarm class in size only;
  !     !  all dereferencing within class is performed by C++ implementation
  ! internals for ESMF_Alarm
  !

  type ESMF_AlarmInt
     character(len=256) :: name = " "
     type(ESMF_TimeInterval) :: RingInterval
     type(ESMF_Time)  :: RingTime
     type(ESMF_Time)  :: PrevRingTime
     type(ESMF_Time)  :: StopTime
     integer :: ID
     integer :: AlarmMutex
     logical :: Ringing
     logical :: Enabled
     logical :: RingTimeSet
     logical :: RingIntervalSet
     logical :: StopTimeSet
  end type ESMF_AlarmInt
  ! Actual public type:  this bit allows easy mimic of "deep" ESMF_AlarmCreate
  ! in ESMF 2.1.0+.  Note that ESMF_AlarmCreate is in a separate module to avoid
  ! cyclic dependence.
  ! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF
  !        shallow-copy-masquerading-as-reference-copy insanity.

  type ESMF_Alarm
     type(ESMF_AlarmInt), pointer :: alarmint => null()
  end type ESMF_Alarm
  !------------------------------------------------------------------------------
  ! !PUBLIC TYPES:

  PUBLIC esmf_alarm 
  PUBLIC esmf_alarmint 
  !------------------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:

  !      public ESMF_AlarmGetRingInterval
  !      public ESMF_AlarmSetRingInterval
  !      public ESMF_AlarmGetRingTime
  !      public ESMF_AlarmSetRingTime
  !      public ESMF_AlarmGetPrevRingTime
  !      public ESMF_AlarmSetPrevRingTime
  !      public ESMF_AlarmGetStopTime
  !      public ESMF_AlarmSetStopTime
  !      public ESMF_AlarmCheckRingTime
  ! Required inherited and overridden ESMF_Base class methods
  !      public ESMF_AlarmRead
  !      public ESMF_AlarmWrite


  ! !PRIVATE MEMBER FUNCTIONS:

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
  !==============================================================================
  PUBLIC kr_esmf_alarmmod_esmf_alarmint 
  PUBLIC kr_esmf_alarmmod_esmf_alarm 
  PUBLIC kv_esmf_alarmmod_esmf_alarmint 
  PUBLIC kv_esmf_alarmmod_esmf_alarm 
  !


  !==============================================================================
  !------------------------------------------------------------------------------
  ! This section includes the Set methods.
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmSet - Initializes an alarm
  ! !INTERFACE:
    
  CONTAINS 
    


  !
  !


  ! Deallocate memory for ESMF_Alarm


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmGetRingInterval - Get an alarm's ring interval
  ! !INTERFACE:


  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmSetRingInterval - Set an alarm's ring interval
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmGetRingTime - Get an alarm's time to ring
  ! !INTERFACE:

  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmSetRingTime - Set an alarm's time to ring
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmGet - Get an alarm's parameters -- compatibility with ESMF 2.0.1
  ! !INTERFACE:

  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
  ! !INTERFACE:

  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmGetStopTime - Get an alarm's stop time
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmSetStopTime - Set an alarm's stop time
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmEnable - Enables an alarm
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmDisable - Disables an alarm
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmRingerOn - Turn on an alarm
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmRingerOff - Turn off an alarm
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmIsRinging - Check if alarm is ringing
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmEQ - Compare two alarms for equality
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  ! This section defines the overridden Read, Write, Validate and Print methods
  ! from the ESMF_Base class
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmRead - restores an alarm
  ! !INTERFACE:

  !
  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_AlarmWrite - saves an alarm
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmValidate - Validate an Alarm's properties
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_AlarmPrint - Print out an Alarm's properties
  ! !INTERFACE:


  !------------------------------------------------------------------------------


  !read state subroutine for kr_esmf_alarmmod_esmf_alarmint 
  RECURSIVE SUBROUTINE kr_esmf_alarmmod_esmf_alarmint(var, kgen_unit, printname, printvar) 
      TYPE(esmf_alarmint), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%name 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%name = ", var%name 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_timeintervalmod_esmf_timeinterval(var%ringinterval, kgen_unit, printname // "%ringinterval", .TRUE.) 
      ELSE 
          CALL kr_esmf_timeintervalmod_esmf_timeinterval(var%ringinterval, kgen_unit, printname // "%ringinterval", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%ringtime, kgen_unit, printname // "%ringtime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%ringtime, kgen_unit, printname // "%ringtime", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%prevringtime, kgen_unit, printname // "%prevringtime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%prevringtime, kgen_unit, printname // "%prevringtime", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%stoptime, kgen_unit, printname // "%stoptime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%stoptime, kgen_unit, printname // "%stoptime", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%id 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%id = ", var%id 
      END IF   
        
      READ (UNIT = kgen_unit) var%alarmmutex 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%alarmmutex = ", var%alarmmutex 
      END IF   
        
      READ (UNIT = kgen_unit) var%ringing 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ringing = ", var%ringing 
      END IF   
        
      READ (UNIT = kgen_unit) var%enabled 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%enabled = ", var%enabled 
      END IF   
        
      READ (UNIT = kgen_unit) var%ringtimeset 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ringtimeset = ", var%ringtimeset 
      END IF   
        
      READ (UNIT = kgen_unit) var%ringintervalset 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ringintervalset = ", var%ringintervalset 
      END IF   
        
      READ (UNIT = kgen_unit) var%stoptimeset 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%stoptimeset = ", var%stoptimeset 
      END IF   
        
  END SUBROUTINE kr_esmf_alarmmod_esmf_alarmint 
    
  !read state subroutine for kr_esmf_alarmmod_esmf_alarm 
  RECURSIVE SUBROUTINE kr_esmf_alarmmod_esmf_alarm(var, kgen_unit, printname, printvar) 
      TYPE(esmf_alarm), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_esmf_alarm_subp10(var%alarmint, kgen_unit, printname // "%alarmint", .TRUE.) 
      ELSE 
          CALL kr_kgen_esmf_alarm_subp10(var%alarmint, kgen_unit, printname // "%alarmint", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_esmf_alarmmod_esmf_alarm 
    
  !write state subroutine for kr_kgen_esmf_alarm_subp10 
  SUBROUTINE kr_kgen_esmf_alarm_subp10(var, kgen_unit, printname, printvar) 
      TYPE(esmf_alarmint), INTENT(INOUT), POINTER :: var 
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
              CALL kr_esmf_alarmmod_esmf_alarmint(var, kgen_unit, printname, .TRUE.) 
          ELSE 
              CALL kr_esmf_alarmmod_esmf_alarmint(var, kgen_unit, printname, .FALSE.) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_kgen_esmf_alarm_subp10 
    
  !verify state subroutine for kv_esmf_alarmmod_esmf_alarmint 
  RECURSIVE SUBROUTINE kv_esmf_alarmmod_esmf_alarmint(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_alarmint), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      character(LEN=256) :: diff_name 
      integer :: diff_id 
      integer :: diff_alarmmutex 
      logical :: diff_ringing 
      logical :: diff_enabled 
      logical :: diff_ringtimeset 
      logical :: diff_ringintervalset 
      logical :: diff_stoptimeset 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
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
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_timeintervalmod_esmf_timeinterval("ringinterval", comp_check_status, var%ringinterval, &
      &kgenref_var%ringinterval) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%ringinterval", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringinterval is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringinterval is NOT IDENTICAL(within tolerance)." 
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
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_shrtimemod_esmf_time("ringtime", comp_check_status, var%ringtime, kgenref_var%ringtime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%ringtime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringtime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringtime is NOT IDENTICAL(within tolerance)." 
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
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_shrtimemod_esmf_time("prevringtime", comp_check_status, var%prevringtime, kgenref_var%prevringtime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%prevringtime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%prevringtime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%prevringtime is NOT IDENTICAL(within tolerance)." 
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
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_shrtimemod_esmf_time("stoptime", comp_check_status, var%stoptime, kgenref_var%stoptime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%stoptime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stoptime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stoptime is NOT IDENTICAL(within tolerance)." 
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
      IF (var%id == kgenref_var%id) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%id is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_id = ABS(var%id - kgenref_var%id) 
          IF (diff_id <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%id is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%id is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_id 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_id 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%alarmmutex == kgenref_var%alarmmutex) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%alarmmutex is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_alarmmutex = ABS(var%alarmmutex - kgenref_var%alarmmutex) 
          IF (diff_alarmmutex <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alarmmutex is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alarmmutex is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_alarmmutex 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_alarmmutex 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ringing .EQV. kgenref_var%ringing) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringing is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringing is NOT IDENTICAL." 
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
      IF (var%enabled .EQV. kgenref_var%enabled) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%enabled is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%enabled is NOT IDENTICAL." 
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
      IF (var%ringtimeset .EQV. kgenref_var%ringtimeset) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringtimeset is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringtimeset is NOT IDENTICAL." 
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
      IF (var%ringintervalset .EQV. kgenref_var%ringintervalset) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringintervalset is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ringintervalset is NOT IDENTICAL." 
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
      IF (var%stoptimeset .EQV. kgenref_var%stoptimeset) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stoptimeset is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stoptimeset is NOT IDENTICAL." 
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
  END SUBROUTINE kv_esmf_alarmmod_esmf_alarmint 
    
  !verify state subroutine for kv_esmf_alarmmod_esmf_alarm 
  RECURSIVE SUBROUTINE kv_esmf_alarmmod_esmf_alarm(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_alarm), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      IF (ASSOCIATED(var%alarmint)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
          CALL kv_esmf_alarmmod_esmf_alarmint("alarmint", comp_check_status, var%alarmint, kgenref_var%alarmint) 
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname))//"%alarmint", " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alarmint is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alarmint is NOT IDENTICAL(within tolerance)." 
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
  END SUBROUTINE kv_esmf_alarmmod_esmf_alarm 
    
end module ESMF_AlarmMod