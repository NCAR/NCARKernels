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
!     ESMF Clock Module


!
!
module ESMF_ClockMod
  !==============================================================================
  ! This file contains the Clock class definition and all Clock class methods.
  !------------------------------------------------------------------------------
  ! INCLUDES
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
  !==============================================================================
  !BOPI
  ! !MODULE: ESMF_ClockMod
  ! !DESCRIPTION:
  ! Part of Time Manager F90 API wrapper of C++ implemenation
  ! Defines F90 wrapper entry points for corresponding
  ! C++ class {\tt ESMC\_Time} implementation
  ! See {\tt ../include/ESMC\_Clock.h} for complete description
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
    USE esmf_basemod 
  ! associated derived types

    USE esmf_timeintervalmod 
    USE esmf_timemod 
    USE esmf_alarmmod, ONLY: esmf_alarm 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE esmf_alarmmod, ONLY: kr_esmf_alarmmod_esmf_alarm 
    USE esmf_alarmmod, ONLY: kv_esmf_alarmmod_esmf_alarm 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
  !------------------------------------------------------------------------------
  ! !PRIVATE TYPES:
  !
    PRIVATE 
  !------------------------------------------------------------------------------
  !     ! ESMF_Clock
  !     ! F90 class type to match C++ Clock class in size only;
  !     !  all dereferencing within class is performed by C++ implementation
  ! internals for ESMF_Clock
  !


  type ESMF_ClockInt
     type(ESMF_TimeInterval) :: TimeStep
     type(ESMF_Time)  :: StartTime
     type(ESMF_Time)  :: StopTime
     type(ESMF_Time)  :: RefTime
     type(ESMF_Time)  :: CurrTime
     type(ESMF_Time)  :: PrevTime
     integer(ESMF_KIND_I8) :: AdvanceCount
     integer :: ClockMutex
     integer :: NumAlarms
     ! Note:  to mimic ESMF 2.1.0+, AlarmList is maintained
     ! within ESMF_Clock even though copies of each alarm are
     ! returned from ESMF_AlarmCreate() at the same time they
     ! are copied into the AlarmList!  This duplication is not
     ! as hideous as it might be because the ESMF_Alarm type
     ! has data members that are all POINTERs (thus the horrible
     ! shallow-copy-masquerading-as-reference-copy hack works).
     type(ESMF_Alarm), pointer, dimension(:) :: AlarmList => null()
  end type ESMF_ClockInt
  ! Actual public type:  this bit allows easy mimic of "deep" ESMF_ClockCreate
  ! in ESMF 2.1.0+
  ! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF
  !        shallow-copy-masquerading-as-reference-copy.

  type ESMF_Clock
     type(ESMF_ClockInt), pointer  :: clockint => null()
  end type ESMF_Clock
  !------------------------------------------------------------------------------
  ! !PUBLIC TYPES:

  PUBLIC esmf_clock 
  PUBLIC esmf_clockint 
  !------------------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !
  !      public ESMF_ClockSetOLD
  PUBLIC esmf_clockget 
  !      public ESMF_ClockGetAdvanceCount
  !      public ESMF_ClockGetTimeStep
  !      public ESMF_ClockSetTimeStep
  !      public ESMF_ClockGetCurrTime
  !      public ESMF_ClockSetCurrTime
  !      public ESMF_ClockGetStartTime
  !      public ESMF_ClockGetStopTime
  !      public ESMF_ClockGetRefTime
  !      public ESMF_ClockGetPrevTime
  !      public ESMF_ClockGetCurrSimTime
  !      public ESMF_ClockGetPrevSimTime
  ! This must be public for ESMF_AlarmClockMod...
  !      public ESMF_ClockGetNumAlarms
  !      public ESMF_ClockSyncToWallClock
  ! Required inherited and overridden ESMF_Base class methods
  !      public ESMF_ClockRead
  !      public ESMF_ClockWrite


  !EOPI
  !------------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.

  !==============================================================================
  PUBLIC kr_esmf_alarmmod_esmf_alarm 
  PUBLIC kr_esmf_clockmod_esmf_clockint 
  PUBLIC kr_esmf_clockmod_esmf_clock 
  PUBLIC kv_esmf_alarmmod_esmf_alarm 
  PUBLIC kv_esmf_clockmod_esmf_clockint 
  PUBLIC kv_esmf_clockmod_esmf_clock 


contains
  !==============================================================================
  ! This section includes the Set methods.
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockSetOLD - Initialize a clockint
  ! !INTERFACE:

  !
  !


  ! !IROUTINE: ESMF_ClockSet - Set clock properties -- for compatibility with ESMF 2.0.1
  ! !INTERFACE:


  ! Create ESMF_Clock using ESMF 2.1.0+ semantics


  ! Deallocate memory for ESMF_Clock

  !
  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGet - Get clock properties -- for compatibility with ESMF 2.0.1
  ! tcraig added alarmCount for ccsm4, consistent with ESMF3 interface
  ! !INTERFACE:


  subroutine ESMF_ClockGet(clock, StartTime, CurrTime,       &
       AdvanceCount, StopTime, TimeStep, &
       PrevTime, RefTime, AlarmCount, &
       rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time), intent(out), optional :: StartTime
    type(ESMF_Time), intent(out), optional :: CurrTime
    type(ESMF_Time), intent(out), optional :: StopTime
    type(ESMF_Time), intent(out), optional :: PrevTime
    type(ESMF_Time), intent(out), optional :: RefTime
    integer(ESMF_KIND_I8), intent(out), optional :: AdvanceCount
    integer,         intent(out), optional :: AlarmCount
    type(ESMF_TimeInterval), intent(out), optional :: TimeStep
    integer, intent(out), optional :: rc
    integer :: ierr
    ! !DESCRIPTION:
    !     Returns the number of times the {\tt ESMF\_Clock} has been advanced
    !     (time stepped)
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the advance count from
    !     \item[StartTime]
    !          The start time
    !     \item[CurrTime]
    !          The current time
    !     \item[AdvanceCount]
    !          The number of times the {\tt ESMF\_Clock} has been advanced
    !     \item[StopTime]
    !          The {\tt ESMF\_Clock}'s stopping time
    !     \item[{[TimeStep]}]
    !          The {\tt ESMF\_Clock}'s time step interval
    !     \item[{[PrevTime]}]
    !          The {\tt ESMF\_Clock}'s previous current time
    !     \item[{[PrevTime]}]
    !          The {\tt ESMF\_Clock}'s reference time
    !     \item[{[AlarmCount]}]
    !          The {\tt ESMF\_Clock}'s number of valid alarms
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.1
    !EOP

    !

    ierr = ESMF_SUCCESS

    IF ( PRESENT (StartTime) ) THEN
       CALL ESMF_ClockGetStartTime( clock, StartTime=StartTime, rc=ierr )
    ENDIF
    IF ( PRESENT (CurrTime) ) THEN
       CALL ESMF_ClockGetCurrTime( clock , CurrTime, ierr )
    ENDIF
    IF ( PRESENT (StopTime) ) THEN
       CALL ESMF_ClockGetStopTime( clock , StopTime, ierr )
    ENDIF
    IF ( PRESENT (AdvanceCount) ) THEN
       CALL ESMF_ClockGetAdvanceCount(clock, AdvanceCount, ierr)
    ENDIF
    IF ( PRESENT (TimeStep) ) THEN
       CALL ESMF_ClockGetTimeStep(clock, TimeStep, ierr)
    ENDIF
    IF ( PRESENT (PrevTime) ) THEN
       CALL ESMF_ClockGetPrevTime(clock, PrevTime, ierr)
    ENDIF
    IF ( PRESENT (RefTime) ) THEN
       CALL ESMF_ClockGetRefTime(clock, RefTime, ierr)
    ENDIF
    IF ( PRESENT (AlarmCount) ) THEN
       CALL ESMF_ClockGetNumAlarms(clock, AlarmCount, ierr)
    ENDIF

    IF ( PRESENT (rc) ) THEN
       rc = ierr
    ENDIF

  end subroutine ESMF_ClockGet
  ! !IROUTINE: ESMF_ClockGetAdvanceCount - Get the clock's advance count
  ! !INTERFACE:


  subroutine ESMF_ClockGetAdvanceCount(clock, AdvanceCount, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    integer(ESMF_KIND_I8), intent(out) :: AdvanceCount
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Returns the number of times the {\tt ESMF\_Clock} has been advanced
    !     (time stepped)
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the advance count from
    !     \item[AdvanceCount]
    !          The number of times the {\tt ESMF\_Clock} has been advanced
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.1
    !EOP

    !


    AdvanceCount = clock%clockint%AdvanceCount

    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

  end subroutine ESMF_ClockGetAdvanceCount
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetTimeStep - Get a clock's timestep interval
  ! !INTERFACE:


  subroutine ESMF_ClockGetTimeStep(clock, TimeStep, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_TimeInterval), intent(out) :: TimeStep
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get an {\tt ESMF\_Clock}'s timestep interval
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the time step from
    !     \item[TimeStep]
    !          The time step
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.2
    !EOP

    !
    !

    TimeStep = clock%clockint%TimeStep
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

  end subroutine ESMF_ClockGetTimeStep
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockSetTimeStep - Set a clock's timestep interval
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetCurrTime - Get a clock's current time
  ! !INTERFACE:


  subroutine ESMF_ClockGetCurrTime(clock, CurrTime, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time), intent(out) :: CurrTime
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get an {\tt ESMF\_Clock}'s current time
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the current time from
    !     \item[CurrTime]
    !          The current time
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.4
    !EOP

    !
    !

    CurrTime = clock%clockint%CurrTime
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
  end subroutine ESMF_ClockGetCurrTime
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockSetCurrTime - Set a clock's current time
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetStartTime - Get a clock's start time
  ! !INTERFACE:


  subroutine ESMF_ClockGetStartTime(clock, StartTime, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time), intent(out) :: StartTime
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get an {\tt ESMF\_Clock}'s start time
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the start time from
    !     \item[StartTime]
    !          The start time
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.3
    !EOP

    !
    !

    StartTime = clock%clockint%StartTime
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

  end subroutine ESMF_ClockGetStartTime
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetStopTime - Get a clock's stop time
  ! !INTERFACE:


  subroutine ESMF_ClockGetStopTime(clock, StopTime, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time), intent(out) :: StopTime
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get an {\tt ESMF\_Clock}'s stop time
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the stop time from
    !     \item[StopTime]
    !          The stop time
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.3
    !EOP

    !
    !

    StopTime = clock%clockint%StopTime
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

  end subroutine ESMF_ClockGetStopTime
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetRefTime - Get a clock's reference time
  ! !INTERFACE:


  subroutine ESMF_ClockGetRefTime(clock, RefTime, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time), intent(out) :: RefTime
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get an {\tt ESMF\_Clock}'s reference time
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the reference time from
    !     \item[RefTime]
    !          The reference time
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.3
    !EOP

    !
    !
    refTime = clock%clockint%RefTime
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
  end subroutine ESMF_ClockGetRefTime
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetPrevTime - Get a clock's previous current time
  ! !INTERFACE:


  subroutine ESMF_ClockGetPrevTime(clock, PrevTime, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    type(ESMF_Time), intent(out) :: PrevTime
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get an {\tt ESMF\_Clock}'s previous current time
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the previous current time from
    !     \item[PrevTime]
    !          The previous current time
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG3.5.4
    !EOP

    !
    !

    !YSK prevTime = Clock%clockint%CurrTime - Clock%clockint%TimeStep
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS
  end subroutine ESMF_ClockGetPrevTime
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetCurrSimTime - Get a clock's current simulation time
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetPrevSimTime - Get a clock's previous simulation time
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockAddAlarm - Add an alarm to a clock's alarm list
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetAlarmList - Get a clock's alarm list
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list
  ! !INTERFACE:


  subroutine ESMF_ClockGetNumAlarms(clock, NumAlarms, rc)
    ! !ARGUMENTS:

    type(ESMF_Clock), intent(in) :: clock
    integer, intent(out) :: NumAlarms
    integer, intent(out), optional :: rc
    ! !DESCRIPTION:
    !     Get the number of {\tt ESMF\_Alarm}s in an {\tt ESMF\_Clock}'s
    !       {\tt ESMF\_Alarm} list
    !     The arguments are:
    !     \begin{description}
    !     \item[clock]
    !          The object instance to get the number of {\tt ESMF\_Alarm}s from
    !     \item[NumAlarms]
    !          The number of {\tt ESMF\_Alarm}s in the {\tt ESMF\_Clock}'s
    !            {\tt ESMF\_Alarm} list
    !     \item[{[rc]}]
    !          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
    !     \end{description}
    ! !REQUIREMENTS:
    !     TMG4.3
    !EOP

    !
    !

    NumAlarms = clock%clockint%NumAlarms
    IF ( PRESENT(rc) ) rc = ESMF_SUCCESS

  end subroutine ESMF_ClockGetNumAlarms
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockSyncToWallClock - Set clock's current time to wall clock time
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockAdvance - Advance a clock's current time by one time step
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockStopTimeDisable - NOOP for compatibility with ESMF 2.1.0+
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockIsStopTime - Has the clock reached its stop time ?
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  ! This section defines the overridden Read, Write, Validate and Print methods
  ! from the ESMF_Base class
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockRead - Restores a clock
  ! !INTERFACE:

  !
  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_ClockWrite - Saves a clock
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_ClockValidate - Validate a Clock's properties
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_ClockPrint - Print out a Clock's properties
  ! !INTERFACE:


  !------------------------------------------------------------------------------


  !read state subroutine for kr_esmf_clockmod_esmf_clockint 
  RECURSIVE SUBROUTINE kr_esmf_clockmod_esmf_clockint(var, kgen_unit, printname, printvar) 
      TYPE(esmf_clockint), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_timeintervalmod_esmf_timeinterval(var%timestep, kgen_unit, printname // "%timestep", .TRUE.) 
      ELSE 
          CALL kr_esmf_timeintervalmod_esmf_timeinterval(var%timestep, kgen_unit, printname // "%timestep", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%starttime, kgen_unit, printname // "%starttime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%starttime, kgen_unit, printname // "%starttime", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%stoptime, kgen_unit, printname // "%stoptime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%stoptime, kgen_unit, printname // "%stoptime", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%reftime, kgen_unit, printname // "%reftime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%reftime, kgen_unit, printname // "%reftime", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%currtime, kgen_unit, printname // "%currtime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%currtime, kgen_unit, printname // "%currtime", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_esmf_shrtimemod_esmf_time(var%prevtime, kgen_unit, printname // "%prevtime", .TRUE.) 
      ELSE 
          CALL kr_esmf_shrtimemod_esmf_time(var%prevtime, kgen_unit, printname // "%prevtime", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%advancecount 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%advancecount = ", var%advancecount 
      END IF   
        
      READ (UNIT = kgen_unit) var%clockmutex 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%clockmutex = ", var%clockmutex 
      END IF   
        
      READ (UNIT = kgen_unit) var%numalarms 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%numalarms = ", var%numalarms 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_esmf_clockint_subp15(var%alarmlist, kgen_unit, printname // "%alarmlist", .TRUE.) 
      ELSE 
          CALL kr_kgen_esmf_clockint_subp15(var%alarmlist, kgen_unit, printname // "%alarmlist", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_esmf_clockmod_esmf_clockint 
    
  !write state subroutine for kr_kgen_esmf_clockint_subp15 
  SUBROUTINE kr_kgen_esmf_clockint_subp15(var, kgen_unit, printname, printvar) 
      TYPE(esmf_alarm), INTENT(INOUT), POINTER, DIMENSION(:) :: var 
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
                  CALL kr_esmf_alarmmod_esmf_alarm(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
              ELSE 
                  CALL kr_esmf_alarmmod_esmf_alarm(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
              END IF   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_esmf_clockint_subp15 
    
  !read state subroutine for kr_esmf_clockmod_esmf_clock 
  RECURSIVE SUBROUTINE kr_esmf_clockmod_esmf_clock(var, kgen_unit, printname, printvar) 
      TYPE(esmf_clock), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_esmf_clock_subp16(var%clockint, kgen_unit, printname // "%clockint", .TRUE.) 
      ELSE 
          CALL kr_kgen_esmf_clock_subp16(var%clockint, kgen_unit, printname // "%clockint", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_esmf_clockmod_esmf_clock 
    
  !write state subroutine for kr_kgen_esmf_clock_subp16 
  SUBROUTINE kr_kgen_esmf_clock_subp16(var, kgen_unit, printname, printvar) 
      TYPE(esmf_clockint), INTENT(INOUT), POINTER :: var 
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
              CALL kr_esmf_clockmod_esmf_clockint(var, kgen_unit, printname, .TRUE.) 
          ELSE 
              CALL kr_esmf_clockmod_esmf_clockint(var, kgen_unit, printname, .FALSE.) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_kgen_esmf_clock_subp16 
    
  !verify state subroutine for kv_esmf_clockmod_esmf_clockint 
  RECURSIVE SUBROUTINE kv_esmf_clockmod_esmf_clockint(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_clockint), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer(KIND=esmf_kind_i8) :: diff_advancecount 
      integer :: diff_clockmutex 
      integer :: diff_numalarms 
      INTEGER :: idx1_alarmlist 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_timeintervalmod_esmf_timeinterval("timestep", comp_check_status, var%timestep, kgenref_var%timestep) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%timestep", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%timestep is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%timestep is NOT IDENTICAL(within tolerance)." 
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
      CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
      CALL kv_esmf_shrtimemod_esmf_time("reftime", comp_check_status, var%reftime, kgenref_var%reftime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%reftime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%reftime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%reftime is NOT IDENTICAL(within tolerance)." 
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
      CALL kv_esmf_shrtimemod_esmf_time("currtime", comp_check_status, var%currtime, kgenref_var%currtime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%currtime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%currtime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%currtime is NOT IDENTICAL(within tolerance)." 
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
      CALL kv_esmf_shrtimemod_esmf_time("prevtime", comp_check_status, var%prevtime, kgenref_var%prevtime) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%prevtime", " is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%prevtime is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%prevtime is NOT IDENTICAL(within tolerance)." 
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
      IF (var%advancecount == kgenref_var%advancecount) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%advancecount is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_advancecount = ABS(var%advancecount - kgenref_var%advancecount) 
          IF (diff_advancecount <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%advancecount is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%advancecount is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_advancecount 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_advancecount 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%clockmutex == kgenref_var%clockmutex) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%clockmutex is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_clockmutex = ABS(var%clockmutex - kgenref_var%clockmutex) 
          IF (diff_clockmutex <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%clockmutex is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%clockmutex is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_clockmutex 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_clockmutex 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%numalarms == kgenref_var%numalarms) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%numalarms is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_numalarms = ABS(var%numalarms - kgenref_var%numalarms) 
          IF (diff_numalarms <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%numalarms is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%numalarms is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_numalarms 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_numalarms 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%alarmlist)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
          DO   idx1_alarmlist = LBOUND(var%alarmlist,1), UBOUND(var%alarmlist,1) 
              CALL kv_esmf_alarmmod_esmf_alarm(trim(adjustl(varname))//"%alarmlist", comp_check_status, &
              &var%alarmlist(idx1_alarmlist), kgenref_var%alarmlist(idx1_alarmlist)) 
          END DO   
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname))//"%alarmlist", " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alarmlist is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alarmlist is NOT IDENTICAL(within tolerance)." 
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
  END SUBROUTINE kv_esmf_clockmod_esmf_clockint 
    
  !verify state subroutine for kv_esmf_clockmod_esmf_clock 
  RECURSIVE SUBROUTINE kv_esmf_clockmod_esmf_clock(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_clock), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      IF (ASSOCIATED(var%clockint)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
          CALL kv_esmf_clockmod_esmf_clockint("clockint", comp_check_status, var%clockint, kgenref_var%clockint) 
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname))//"%clockint", " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%clockint is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%clockint is NOT IDENTICAL(within tolerance)." 
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
  END SUBROUTINE kv_esmf_clockmod_esmf_clock 
    
end module ESMF_ClockMod
