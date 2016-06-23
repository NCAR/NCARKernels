      module ESMF_ShrTimeMod
!
!==============================================================================
!
! This file contains types and methods that are shared in the hierarchy
!
!------------------------------------------------------------------------------
! INCLUDES

!==============================================================================
!BOPI
! !MODULE: ESMF_ShrTimeMod
!
! !DESCRIPTION:
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class
      use ESMF_BaseMod

      ! inherit from base time class
      use ESMF_BaseTimeMod
      use ESMF_CalendarMod

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Time
!
!     ! F90 class type to match C++ Time class in size only;
!     !  all dereferencing within class is performed by C++ implementation

     type ESMF_Time
       type(ESMF_BaseTime) :: basetime           ! inherit base class
       ! time instant is expressed as year + basetime
       integer :: YR
       type(ESMF_Calendar), pointer :: calendar => null() ! associated calendar
     end type

     public ESMF_Time
!==============================================================================
     PUBLIC kw_esmf_shrtimemod_esmf_time
     
     CONTAINS
     
     !read state subroutine for kw_esmf_shrtimemod_esmf_time
     RECURSIVE SUBROUTINE kw_esmf_shrtimemod_esmf_time(var, kgen_unit, printvar)
         TYPE(esmf_time), INTENT(IN) :: var
         INTEGER, INTENT(IN) :: kgen_unit
         CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
         LOGICAL :: kgen_istrue
         REAL(KIND=8) :: kgen_array_sum
         
         IF (PRESENT( printvar )) THEN
             CALL kw_esmf_basetimemod_esmf_basetime(var%basetime, kgen_unit, printvar // "%basetime")
         ELSE
             CALL kw_esmf_basetimemod_esmf_basetime(var%basetime, kgen_unit)
         END IF 
         
         kgen_istrue = .TRUE.
         WRITE (UNIT = kgen_unit) var%yr
         IF (PRESENT( printvar )) THEN
             WRITE (*, *) "** KGEN DEBUG: " // printvar // "%yr **" // NEW_LINE("A"), var%yr
         END IF 
         
         IF (PRESENT( printvar )) THEN
             CALL kw_esmf_time_esmf_calendar__esmf_calendar_ptr(var%calendar, kgen_unit, printvar // "%calendar")
         ELSE
             CALL kw_esmf_time_esmf_calendar__esmf_calendar_ptr(var%calendar, kgen_unit)
         END IF 
         
     END SUBROUTINE kw_esmf_shrtimemod_esmf_time
     
     !write state subroutine for kw_esmf_time_esmf_calendar__esmf_calendar_ptr
     SUBROUTINE kw_esmf_time_esmf_calendar__esmf_calendar_ptr(var, kgen_unit, printvar)
         TYPE(esmf_calendar), INTENT(IN), POINTER :: var
         INTEGER, INTENT(IN) :: kgen_unit
         CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
         LOGICAL :: kgen_istrue
         REAL(KIND=8) :: kgen_array_sum
         
         kgen_istrue = .TRUE.
         IF (.NOT. ASSOCIATED(var)) THEN
             kgen_istrue = .FALSE.
         END IF 
         WRITE (UNIT = kgen_unit) kgen_istrue
         IF (kgen_istrue) THEN
             IF (PRESENT( printvar )) THEN
                 CALL kw_esmf_calendarmod_esmf_calendar(var, kgen_unit, printvar // " calendar ")
             ELSE
                 CALL kw_esmf_calendarmod_esmf_calendar(var, kgen_unit)
             END IF 
         END IF 
     END SUBROUTINE kw_esmf_time_esmf_calendar__esmf_calendar_ptr
     
end module ESMF_ShrTimeMod