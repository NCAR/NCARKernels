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
!     ESMF Calendar Module


!
!
!
      module ESMF_CalendarMod
!==============================================================================
! This file contains the Calendar class definition and all Calendar class
! methods.
!------------------------------------------------------------------------------
! INCLUDES
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
!==============================================================================
!BOPI
! !MODULE: ESMF_CalendarMod
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
! Defines F90 wrapper entry points for corresponding
! C++ class { \tt ESMC\_Calendar} implementation
! See {\tt ../include/ESMC\_Calendar.h} for complete description
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
      ! inherit from base time class

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

!------------------------------------------------------------------------------
!     ! ESMF_CalKind_Flag
!     ! F90 "enum" type to match C++ ESMC_CalKind_Flag enum


!

      type ESMF_CalKind_Flag
        integer :: caltype
      end type

!      type(ESMF_CalKind_Flag), parameter :: &
!                               ESMF_CALKIND_GREGORIAN =  ESMF_CalKind_Flag(1), &
!                               ESMF_CALKIND_JULIAN =     ESMF_CalKind_Flag(2), &
!                           ! like Gregorian, except Feb always has 28 days
!                               ESMF_CALKIND_NOLEAP =     ESMF_CalKind_Flag(3), &
!                           ! 12 months, 30 days each
!                               ESMF_CALKIND_360DAY =     ESMF_CalKind_Flag(4), &
!                           ! user defined
!                               ESMF_CALKIND_GENERIC =    ESMF_CalKind_Flag(5), &
!                           ! track base time seconds only
!                               ESMF_CALKIND_NOCALENDAR = ESMF_CalKind_Flag(6)
!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!     ! F90 class type to match C++ Calendar class in size only;
!     !  all dereferencing within class is performed by C++ implementation
!------------------------------------------------------------------------------
!     ! ESMF_DaysPerYear


!
!
!
!
      type ESMF_DaysPerYear
        integer :: D = 0    ! whole days per year
        integer :: Dn = 0   ! fractional days per year numerator
        integer :: Dd = 1   ! fractional days per year denominator
      end type              ! e.g. for Venus, D=0, Dn=926, Dd=1000
!------------------------------------------------------------------------------
!     ! ESMF_Calendar
!
!
!
      type ESMF_Calendar
        type(ESMF_CalKind_Flag) :: Type
        logical :: Set = .false.
        integer, dimension(12_ESMF_KIND_I8) :: DaysPerMonth = 0
        integer :: SecondsPerDay = 0
        integer :: SecondsPerYear = 0
        type(ESMF_DaysPerYear) :: DaysPerYear
      end type
!------------------------------------------------------------------------------
! !PUBLIC DATA: added by Juanxiong He, in order to breakthe cycle call between
! ESMF_Stubs and ESMF_Time
!------------------------------------------------------------------------------
! !PUBLIC TYPES:

!
!      public mday
!      public mdayleap
!      public monthbdys
!      public monthbdysleap
!      public monthedys
!      public monthedysleap
!      public daym
!      public daymleap
!      public mdaycum
!      public mdayleapcum
      PUBLIC esmf_calkind_flag 
!             ESMF_CALKIND_360DAY, ESMF_CALKIND_NOCALENDAR
!      public ESMF_CAL_JULIAN
!      public ESMF_CAL_GENERIC
      PUBLIC esmf_calendar 
      PUBLIC esmf_daysperyear 
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

!
! Required inherited and overridden ESMF_Base class methods


                                      ! to be private within ESMF methods
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.

!==============================================================================
      PUBLIC kr_esmf_calendarmod_esmf_calkind_flag 
      PUBLIC kr_esmf_calendarmod_esmf_daysperyear 
      PUBLIC kr_esmf_calendarmod_esmf_calendar 
      PUBLIC kv_esmf_calendarmod_esmf_calkind_flag 
      PUBLIC kv_esmf_calendarmod_esmf_daysperyear 
      PUBLIC kv_esmf_calendarmod_esmf_calendar 


!==============================================================================
!BOP
! !IROUTINE: ESMF_CalendarCreate - Create a new ESMF Calendar of built-in type
! !INTERFACE:
      ! Private name; call using ESMF_CalendarCreate()
        
      CONTAINS 
        


!==============================================================================
!BOP
! !IROUTINE: ESMF_CalendarInitialized - check if calendar was created
! !INTERFACE:


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================


!==============================================================================

      !read state subroutine for kr_esmf_calendarmod_esmf_calkind_flag 
      RECURSIVE SUBROUTINE kr_esmf_calendarmod_esmf_calkind_flag(var, kgen_unit, printname, printvar) 
          TYPE(esmf_calkind_flag), INTENT(INOUT) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN) :: printname 
          LOGICAL, INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          READ (UNIT = kgen_unit) var%caltype 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%caltype = ", var%caltype 
          END IF   
            
      END SUBROUTINE kr_esmf_calendarmod_esmf_calkind_flag 
        
      !read state subroutine for kr_esmf_calendarmod_esmf_daysperyear 
      RECURSIVE SUBROUTINE kr_esmf_calendarmod_esmf_daysperyear(var, kgen_unit, printname, printvar) 
          TYPE(esmf_daysperyear), INTENT(INOUT) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN) :: printname 
          LOGICAL, INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          READ (UNIT = kgen_unit) var%d 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%d = ", var%d 
          END IF   
            
          READ (UNIT = kgen_unit) var%dn 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%dn = ", var%dn 
          END IF   
            
          READ (UNIT = kgen_unit) var%dd 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%dd = ", var%dd 
          END IF   
            
      END SUBROUTINE kr_esmf_calendarmod_esmf_daysperyear 
        
      !read state subroutine for kr_esmf_calendarmod_esmf_calendar 
      RECURSIVE SUBROUTINE kr_esmf_calendarmod_esmf_calendar(var, kgen_unit, printname, printvar) 
          TYPE(esmf_calendar), INTENT(INOUT) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN) :: printname 
          LOGICAL, INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              CALL kr_esmf_calendarmod_esmf_calkind_flag(var%type, kgen_unit, printname // "%type", .TRUE.) 
          ELSE 
              CALL kr_esmf_calendarmod_esmf_calkind_flag(var%type, kgen_unit, printname // "%type", .FALSE.) 
          END IF   
            
          READ (UNIT = kgen_unit) var%set 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%set = ", var%set 
          END IF   
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              READ (UNIT = kgen_unit) kgen_array_sum 
              READ (UNIT = kgen_unit) var%dayspermonth 
              CALL kgen_array_sumcheck(printname // "%dayspermonth", kgen_array_sum, DBLE(SUM(var%dayspermonth, &
              &mask=(var%dayspermonth .eq. var%dayspermonth))), .TRUE.) 
              IF (PRESENT( printvar ) .AND. printvar) THEN 
                  WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dayspermonth)) = ", DBLE(SUM(var%dayspermonth, &
                  &mask=(var%dayspermonth .eq. var%dayspermonth))) 
              END IF   
          END IF   
            
          READ (UNIT = kgen_unit) var%secondsperday 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%secondsperday = ", var%secondsperday 
          END IF   
            
          READ (UNIT = kgen_unit) var%secondsperyear 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%secondsperyear = ", var%secondsperyear 
          END IF   
            
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              CALL kr_esmf_calendarmod_esmf_daysperyear(var%daysperyear, kgen_unit, printname // "%daysperyear", .TRUE.) 
          ELSE 
              CALL kr_esmf_calendarmod_esmf_daysperyear(var%daysperyear, kgen_unit, printname // "%daysperyear", .FALSE.) 
          END IF   
            
      END SUBROUTINE kr_esmf_calendarmod_esmf_calendar 
        
      !verify state subroutine for kv_esmf_calendarmod_esmf_calkind_flag 
      RECURSIVE SUBROUTINE kv_esmf_calendarmod_esmf_calkind_flag(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          TYPE(esmf_calkind_flag), INTENT(IN) :: var, kgenref_var 
          TYPE(check_t) :: dtype_check_status, comp_check_status 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          integer :: diff_caltype 
            
          check_status%numTotal = check_status%numTotal + 1 
            
          CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (var%caltype == kgenref_var%caltype) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%caltype is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff_caltype = ABS(var%caltype - kgenref_var%caltype) 
              IF (diff_caltype <= kgen_tolerance) THEN 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%caltype is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%caltype is NOT IDENTICAL(out of tolerance)." 
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
                      WRITE (*, *) "Difference is ", diff_caltype 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "Difference is ", diff_caltype 
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
      END SUBROUTINE kv_esmf_calendarmod_esmf_calkind_flag 
        
      !verify state subroutine for kv_esmf_calendarmod_esmf_daysperyear 
      RECURSIVE SUBROUTINE kv_esmf_calendarmod_esmf_daysperyear(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          TYPE(esmf_daysperyear), INTENT(IN) :: var, kgenref_var 
          TYPE(check_t) :: dtype_check_status, comp_check_status 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          integer :: diff_d 
          integer :: diff_dn 
          integer :: diff_dd 
            
          check_status%numTotal = check_status%numTotal + 1 
            
          CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (var%d == kgenref_var%d) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%d is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff_d = ABS(var%d - kgenref_var%d) 
              IF (diff_d <= kgen_tolerance) THEN 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%d is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%d is NOT IDENTICAL(out of tolerance)." 
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
                      WRITE (*, *) "Difference is ", diff_d 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "Difference is ", diff_d 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (var%dn == kgenref_var%dn) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dn is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff_dn = ABS(var%dn - kgenref_var%dn) 
              IF (diff_dn <= kgen_tolerance) THEN 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dn is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dn is NOT IDENTICAL(out of tolerance)." 
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
                      WRITE (*, *) "Difference is ", diff_dn 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "Difference is ", diff_dn 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (var%dd == kgenref_var%dd) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dd is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff_dd = ABS(var%dd - kgenref_var%dd) 
              IF (diff_dd <= kgen_tolerance) THEN 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dd is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dd is NOT IDENTICAL(out of tolerance)." 
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
                      WRITE (*, *) "Difference is ", diff_dd 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "Difference is ", diff_dd 
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
      END SUBROUTINE kv_esmf_calendarmod_esmf_daysperyear 
        
      !verify state subroutine for kv_esmf_calendarmod_esmf_calendar 
      RECURSIVE SUBROUTINE kv_esmf_calendarmod_esmf_calendar(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          TYPE(esmf_calendar), INTENT(IN) :: var, kgenref_var 
          TYPE(check_t) :: dtype_check_status, comp_check_status 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          logical :: diff_set 
          INTEGER :: n_dayspermonth 
          integer :: nrmsdiff_dayspermonth, rmsdiff_dayspermonth 
          integer, ALLOCATABLE :: buf1_dayspermonth(:), buf2_dayspermonth(:) 
          integer :: diff_secondsperday 
          integer :: diff_secondsperyear 
            
          check_status%numTotal = check_status%numTotal + 1 
            
          CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
          CALL kv_esmf_calendarmod_esmf_calkind_flag("type", comp_check_status, var%type, kgenref_var%type) 
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname))//"%type", " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%type is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%type is NOT IDENTICAL(within tolerance)." 
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
          IF (var%set .EQV. kgenref_var%set) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%set is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%set is NOT IDENTICAL." 
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
          IF (ALL(var%dayspermonth == kgenref_var%dayspermonth)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dayspermonth is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dayspermonth(SIZE(var%dayspermonth,dim=1))) 
              ALLOCATE (buf2_dayspermonth(SIZE(var%dayspermonth,dim=1))) 
              n_dayspermonth = COUNT(var%dayspermonth /= kgenref_var%dayspermonth) 
              WHERE ( ABS(kgenref_var%dayspermonth) > kgen_minvalue ) 
                  buf1_dayspermonth = ((var%dayspermonth-kgenref_var%dayspermonth)/kgenref_var%dayspermonth)**2 
                  buf2_dayspermonth = (var%dayspermonth-kgenref_var%dayspermonth)**2 
              ELSEWHERE 
                  buf1_dayspermonth = (var%dayspermonth-kgenref_var%dayspermonth)**2 
                  buf2_dayspermonth = buf1_dayspermonth 
              END WHERE   
              nrmsdiff_dayspermonth = SQRT(SUM(buf1_dayspermonth)/REAL(n_dayspermonth)) 
              rmsdiff_dayspermonth = SQRT(SUM(buf2_dayspermonth)/REAL(n_dayspermonth)) 
              IF (rmsdiff_dayspermonth > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dayspermonth is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dayspermonth is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%dayspermonth /= kgenref_var%dayspermonth), " of ", size( var%dayspermonth ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dayspermonth)/real(size(var%dayspermonth)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dayspermonth)/real(size(kgenref_var%dayspermonth)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dayspermonth 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dayspermonth 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dayspermonth /= kgenref_var%dayspermonth), " of ", size( var%dayspermonth ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dayspermonth)/real(size(var%dayspermonth)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dayspermonth)/real(size(kgenref_var%dayspermonth)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dayspermonth 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dayspermonth 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (var%secondsperday == kgenref_var%secondsperday) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%secondsperday is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff_secondsperday = ABS(var%secondsperday - kgenref_var%secondsperday) 
              IF (diff_secondsperday <= kgen_tolerance) THEN 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%secondsperday is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%secondsperday is NOT IDENTICAL(out of tolerance)." 
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
                      WRITE (*, *) "Difference is ", diff_secondsperday 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "Difference is ", diff_secondsperday 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (var%secondsperyear == kgenref_var%secondsperyear) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%secondsperyear is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff_secondsperyear = ABS(var%secondsperyear - kgenref_var%secondsperyear) 
              IF (diff_secondsperyear <= kgen_tolerance) THEN 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%secondsperyear is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%secondsperyear is NOT IDENTICAL(out of tolerance)." 
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
                      WRITE (*, *) "Difference is ", diff_secondsperyear 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) "Difference is ", diff_secondsperyear 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
          CALL kv_esmf_calendarmod_esmf_daysperyear("daysperyear", comp_check_status, var%daysperyear, kgenref_var%daysperyear) 
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname))//"%daysperyear", " is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%daysperyear is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%daysperyear is NOT IDENTICAL(within tolerance)." 
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
      END SUBROUTINE kv_esmf_calendarmod_esmf_calendar 
        
end module ESMF_CalendarMod