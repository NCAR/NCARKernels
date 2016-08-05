!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:55
!KGEN version : 0.6.2

! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Alarm Module
      module ESMF_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
































! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  



!===============================================================================
!BOPI
!
! !MODULE: ESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class

      ! associated derived types
          USE esmf_timeintervalmod
          USE esmf_timemod

          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
          IMPLICIT NONE

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
          PRIVATE
!------------------------------------------------------------------------------
!     ! ESMF_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for ESMF_Alarm
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
      end type

! Actual public type:  this bit allows easy mimic of "deep" ESMF_AlarmCreate
! in ESMF 2.1.0+.  Note that ESMF_AlarmCreate is in a separate module to avoid 
! cyclic dependence.  
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates ESMF 
!        shallow-copy-masquerading-as-reference-copy insanity.  
      type ESMF_Alarm
        type(ESMF_AlarmInt), pointer :: alarmint => null()
      end type

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
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt ESMF\_Alarm} class
!
!EOP
!
!------------------------------------------------------------------------------

!==============================================================================

      PUBLIC kr_esmf_alarmmod_esmf_alarmint
      PUBLIC kr_esmf_alarmmod_esmf_alarm
      PUBLIC kv_esmf_alarmmod_esmf_alarmint
      PUBLIC kv_esmf_alarmmod_esmf_alarm

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSet - Initializes an alarm

! !INTERFACE:
      
      CONTAINS
      

! !ARGUMENTS:

! !DESCRIPTION:
!     Initializes an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP











! Deallocate memory for ESMF_Alarm

         ! TBH:  ignore deallocate errors, for now




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP



 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt ESMF\_Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP







!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt ESMF\_Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGet - Get an alarm's parameters -- compatibility with ESMF 2.0.1
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get
!     \item[ringTime]
!          The ring time for a one-shot alarm or the next repeating alarm.
!     \item[ringInterval]
!          The ring interval for repeating (interval) alarms.
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP










!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:

! !ARGUMENTS:
   
! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Get an {\tt ESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt ESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Set an {\tt ESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt ESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmEnable - Enables an alarm

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Enables an {\tt ESMF\_Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmDisable - Disables an alarm

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Disables an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmRingerOn - Turn on an alarm


! !INTERFACE:

! !ARGUMENTS:
    
! !DESCRIPTION:
!     Turn on an {\tt ESMF\_Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP





!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmRingerOff - Turn off an alarm

! !INTERFACE:

! !ARGUMENTS:
    
! !DESCRIPTION:
!     Turn off an {\tt ESMF\_Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off   
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Check if {\tt ESMF\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state  
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Main method used by a {\tt ESMF\_Clock} to check whether to trigger
!     the {\tt ESMF\_Alarm} 
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring   
!     \item[ClockCurrTime]
!          The {\tt ESMF\_Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmEQ - Compare two alarms for equality
!
! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt ESMF\_Alarm} to compare
!     \item[alarm2]
!          The second {\tt ESMF\_Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:  
!EOP

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the ESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmRead - restores an alarm

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Restores an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore
!     \item[RingInterval]
!          The ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt ESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt ESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt ESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_AlarmWrite - saves an alarm

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Saves an {\tt ESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save
!     \item[RingInterval]
!          Ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt ESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt ESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt ESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt ESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Perform a validation check on a {\tt ESMF\_Alarm}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt ESMF\_Alarm} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP





!------------------------------------------------------------------------------

      !read state subroutine for kr_esmf_alarmmod_esmf_alarmint
      RECURSIVE SUBROUTINE kr_esmf_alarmmod_esmf_alarmint(var, kgen_unit, printvar)
          TYPE(esmf_alarmint), INTENT(INOUT) :: var
          INTEGER, INTENT(IN) :: kgen_unit
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          READ (UNIT = kgen_unit) var%name
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%name **" // NEW_LINE("A"), var%name
          END IF 
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_timeintervalmod_esmf_timeinterval(var%ringinterval, kgen_unit, printvar // "%ringinterval")
          ELSE
              CALL kr_esmf_timeintervalmod_esmf_timeinterval(var%ringinterval, kgen_unit)
          END IF 
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_shrtimemod_esmf_time(var%ringtime, kgen_unit, printvar // "%ringtime")
          ELSE
              CALL kr_esmf_shrtimemod_esmf_time(var%ringtime, kgen_unit)
          END IF 
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_shrtimemod_esmf_time(var%prevringtime, kgen_unit, printvar // "%prevringtime")
          ELSE
              CALL kr_esmf_shrtimemod_esmf_time(var%prevringtime, kgen_unit)
          END IF 
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_shrtimemod_esmf_time(var%stoptime, kgen_unit, printvar // "%stoptime")
          ELSE
              CALL kr_esmf_shrtimemod_esmf_time(var%stoptime, kgen_unit)
          END IF 
          
          READ (UNIT = kgen_unit) var%id
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%id **" // NEW_LINE("A"), var%id
          END IF 
          
          READ (UNIT = kgen_unit) var%alarmmutex
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%alarmmutex **" // NEW_LINE("A"), var%alarmmutex
          END IF 
          
          READ (UNIT = kgen_unit) var%ringing
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ringing **" // NEW_LINE("A"), var%ringing
          END IF 
          
          READ (UNIT = kgen_unit) var%enabled
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%enabled **" // NEW_LINE("A"), var%enabled
          END IF 
          
          READ (UNIT = kgen_unit) var%ringtimeset
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ringtimeset **" // NEW_LINE("A"), var%ringtimeset
          END IF 
          
          READ (UNIT = kgen_unit) var%ringintervalset
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ringintervalset **" // NEW_LINE("A"), var%ringintervalset
          END IF 
          
          READ (UNIT = kgen_unit) var%stoptimeset
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stoptimeset **" // NEW_LINE("A"), var%stoptimeset
          END IF 
          
      END SUBROUTINE kr_esmf_alarmmod_esmf_alarmint
      
      !read state subroutine for kr_esmf_alarmmod_esmf_alarm
      RECURSIVE SUBROUTINE kr_esmf_alarmmod_esmf_alarm(var, kgen_unit, printvar)
          TYPE(esmf_alarm), INTENT(INOUT) :: var
          INTEGER, INTENT(IN) :: kgen_unit
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_alarm_esmf_alarmint__esmf_alarmint_ptr(var%alarmint, kgen_unit, printvar // "%alarmint")
          ELSE
              CALL kr_esmf_alarm_esmf_alarmint__esmf_alarmint_ptr(var%alarmint, kgen_unit)
          END IF 
          
      END SUBROUTINE kr_esmf_alarmmod_esmf_alarm
      
      !write state subroutine for kr_esmf_alarm_esmf_alarmint__esmf_alarmint_ptr
      SUBROUTINE kr_esmf_alarm_esmf_alarmint__esmf_alarmint_ptr(var, kgen_unit, printvar)
          TYPE(esmf_alarmint), INTENT(INOUT), POINTER :: var
          INTEGER, INTENT(IN) :: kgen_unit
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              IF (ASSOCIATED( var )) THEN
                  NULLIFY (var)
              END IF 
              ALLOCATE (var)
              IF (PRESENT( printvar )) THEN
                  CALL kr_esmf_alarmmod_esmf_alarmint(var, kgen_unit, printvar // " alarmint ")
              ELSE
                  CALL kr_esmf_alarmmod_esmf_alarmint(var, kgen_unit)
              END IF 
          END IF 
          
      END SUBROUTINE kr_esmf_alarm_esmf_alarmint__esmf_alarmint_ptr
      
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
          
          CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%name == kgenref_var%name) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%name is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%name is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%id == kgenref_var%id) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%id is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_id = ABS(var%id - kgenref_var%id)
              IF (diff_id <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%id is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%id is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_id
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_id
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%alarmmutex == kgenref_var%alarmmutex) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%alarmmutex is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_alarmmutex = ABS(var%alarmmutex - kgenref_var%alarmmutex)
              IF (diff_alarmmutex <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%alarmmutex is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%alarmmutex is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_alarmmutex
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_alarmmutex
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%ringing .EQV. kgenref_var%ringing) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ringing is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ringing is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%enabled .EQV. kgenref_var%enabled) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%enabled is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%enabled is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%ringtimeset .EQV. kgenref_var%ringtimeset) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ringtimeset is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ringtimeset is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%ringintervalset .EQV. kgenref_var%ringintervalset) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ringintervalset is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%ringintervalset is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%stoptimeset .EQV. kgenref_var%stoptimeset) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stoptimeset is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stoptimeset is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
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
          
          CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
          IF (ASSOCIATED(var%alarmint)) THEN
              dtype_check_status%numTotal = dtype_check_status%numTotal + 1
              IF (check_result == CHECK_IDENTICAL) THEN
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
          IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN
              check_status%numIdentical = check_status%numIdentical + 1
          ELSE IF (dtype_check_status%numOutTol > 0) THEN
              check_status%numOutTol = check_status%numOutTol + 1
          ELSE IF (dtype_check_status%numInTol > 0) THEN
              check_status%numInTol = check_status%numInTol + 1
          END IF 
      END SUBROUTINE kv_esmf_alarmmod_esmf_alarm
      
      end module ESMF_AlarmMod