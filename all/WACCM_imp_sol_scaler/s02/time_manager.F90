!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:54
!KGEN version : 0.6.2

module time_manager

! Provide CAM specific time management.  This is a wrapper layer for the ESMF
! time manager utility.

    USE esmf

    USE cam_abortutils, ONLY: endrun
    USE cam_logfile, ONLY: iulog

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE
    PRIVATE
    SAVE

! Public methods

    PUBLIC get_nstep


! Private module data



! The target attribute for tm_cal is needed (at least by NAG) because there are
! pointers to this object inside ESMF_Time objects.
type(ESMF_Clock)            :: tm_clock      ! Model clock   

!=========================================================================================
PUBLIC kr_externs_in_time_manager
PUBLIC kr_externs_out_time_manager
contains
!=========================================================================================


   ! dtime is the clock step size.  It is the time interval of coupling the dynamics
   ! and physics packages.




!=========================================================================================


! Initialize the time manager.

   ! Arguments

   ! Local variables
!----------------------------------------------------------------------------------------

   ! Initalize calendar type.

   ! create ESMF time instant objects

   ! Initialize ESMF clock


      ! Advance the timestep.  Data from the restart file corresponds to the
      ! last timestep of the previous run.

   ! Initialize date used for perpetual calendar day calculation.


   ! Print configuration summary to log file (stdout).



!=========================================================================================


! Create an ESMF clock based on the stepsize, start_date, stop_date, and ref_date
! Then advance the clock to the curr_date.

   ! Input variables

   ! Local variables




   ! check for valid input



   ! Initialize the clock

   ! Advance clock to the current time (in case of a branch or restart)



!=========================================================================================

!
! Set the time by an integer as YYYYMMDD and integer seconds in the day
!





!=========================================================================================

!
! Set the time as a float given year, month, day, sec
!



  !
  ! If the subroutine returned error, check if it is Feb 29 of a non-leap year
  ! (legitimately used by the time-interpolation routines in tracer_data.F90)
  ! in which case, substitute Feb 28 for the day
  !






!=========================================================================================

!
! Set year, month, day, sec given the time as a float
!









!=========================================================================================

!
! Get the date and time of day in ymd from ESMF Time.
!





!=========================================================================================







! Advance clock to the new time


! Print configuration summary to log file (stdout).




!=========================================================================================

!
! Initialize calendar
!
! Local variables



!=========================================================================================


! Local variables
!-----------------------------------------------------------------------------------------









!=========================================================================================


! Increment the timestep number.

! Local variables
!-----------------------------------------------------------------------------------------


! Set first step flag off.


!=========================================================================================


! Return the step size in seconds.

! Local variables
!-----------------------------------------------------------------------------------------


!=========================================================================================

integer function get_nstep()

! Return the timestep number.

! Local variables
   character(len=*), parameter :: sub = 'get_nstep'
   integer :: rc
   integer(ESMF_KIND_I8) :: step_no
!-----------------------------------------------------------------------------------------

   call ESMF_ClockGet(tm_clock, advanceCount=step_no, rc=rc)
   call chkrc(rc, sub//': error return from ESMF_ClockGet')
   get_nstep = step_no

end function get_nstep
!=========================================================================================


! Return date components valid at end of current timestep with an optional
! offset (positive or negative) in seconds.

! Arguments

                                            ! Positive for future times, negative 
                                            ! for previous times.

! Local variables
!-----------------------------------------------------------------------------------------





!=========================================================================================


! Return time of day valid at end of current timestep and the components
! of the perpetual date (with an optional offset (positive or negative) in seconds.

! Arguments

                                            ! Positive for future times, negative 
                                            ! for previous times.

! Local variables
!-----------------------------------------------------------------------------------------

   ! Get time of day add it to perpetual date
   ! Get year, month, day so that seconds are time-of-day rather than since start time

   ! Get time of day from the result
   ! Get year, month, day so that seconds are time-of-day rather than since start time

   ! Get the date from the fixed perpetual date (in case it overflows to next day)


!=========================================================================================


! Return date components valid at beginning of current timestep.

! Arguments

! Local variables
!-----------------------------------------------------------------------------------------



!=========================================================================================


! Return date components valid at beginning of initial run.

! Arguments

! Local variables
!-----------------------------------------------------------------------------------------



!=========================================================================================


! Return date components of the reference date.

! Arguments

! Local variables
!-----------------------------------------------------------------------------------------



!=========================================================================================


! Return time components valid at end of current timestep.
! Current time is the time interval between the current date and the reference date.

! Arguments

! Local variables
!-----------------------------------------------------------------------------------------





!=========================================================================================


! Return time components valid at beg of current timestep.
! prev time is the time interval between the prev date and the reference date.

! Arguments

! Local variables
!-----------------------------------------------------------------------------------------


!=========================================================================================


! Return calendar day at end of current timestep with optional offset.
! Calendar day 1.0 = 0Z on Jan 1.

! Arguments
                                            ! Positive for future times, negative 
                                            ! for previous times.
! Return value

! Local variables
!-----------------------------------------------------------------------------------------




!     Get current time-of-day from clock
!     Get date from perpetual date add time-of-day to it
!!!!  write(iulog,*) ' tod = ', tod
!!!!  call ESMF_TimePrint( date, "string" )


!
!                  WARNING: Gregorian calendar fakes day 366
!
! The zenith angle calculation is only capable of using a 365-day calendar.
! If a Gregorian calendar is being used, the last day of a leap year (day 366)
! is sent to the model as a repetition of the previous day (day 365). 
! This is done by decrementing calday by 1 immediately below.
! bundy, July 2008
!




!=========================================================================================


! Return calendar day corresponding to specified time instant.
! Calendar day 1.0 = 0Z on Jan 1.

! Arguments

! Return value

! Local variables
!-----------------------------------------------------------------------------------------


!
!                  WARNING: Gregorian calendar fakes day 366
!
! The zenith angle calculation is only capable of using a 365-day calendar.
! If a Gregorian calendar is being used, the last day of a leap year (day 366)
! is sent to the model as a repetition of the previous day (day 365). 
! This is done by decrementing calday by 1 immediately below.
! bundy, July 2008
!




!=========================================================================================


! Return cf standard for calendar type

! Local variables
!-----------------------------------------------------------------------------------------



!=========================================================================================
 

! Return true if incoming calendar type string matches actual calendar type in use


! Return value

!-----------------------------------------------------------------------------------------


!=========================================================================================
 

! Return true if current timestep is last timestep in current day.

! Return value

! Local variables
!-----------------------------------------------------------------------------------------


!=========================================================================================


! Return true if current timestep is last timestep in current month.

! Local variables
!-----------------------------------------------------------------------------------------


!=========================================================================================


! Return true on first step of initial run only.

! Local variables
!-----------------------------------------------------------------------------------------


!=========================================================================================


! Return true on first step of restart run only.

!-----------------------------------------------------------------------------------------


!=========================================================================================


! Return true on last timestep.

! Local variables
!-----------------------------------------------------------------------------------------



!=========================================================================================


! Return true on last timestep.

!-----------------------------------------------------------------------------------------



!=========================================================================================


! Calculate the difference (ymd2,tod2) - (ymd1,tod1) and return the result in days.

! Arguments


! Local variables

!-----------------------------------------------------------------------------------------



!=========================================================================================


! time2_ge_time1 is set to true if (ymd2,tod2) is later than or equal to (ymd1,tod1)

! Arguments


! Local variables

!-----------------------------------------------------------------------------------------



!=========================================================================================


! Increment the time instant (ymd1,tod1) by an interval and return the resulting
! time instant (ymd2,tod2).

   ! Arguments



   ! Local variables

!-----------------------------------------------------------------------------------------

   ! set esmf time object

   ! set esmf time interval object


   ! increment the time instant

   ! extract the time components


!=========================================================================================

subroutine chkrc(rc, mes)
   integer, intent(in)          :: rc   ! return code from time management library
   character(len=*), intent(in) :: mes  ! error message
   if ( rc == ESMF_SUCCESS ) return
   write(iulog,*) mes
   call endrun ('CHKRC')
end subroutine chkrc

!read state subroutine for kr_externs_in_time_manager
SUBROUTINE kr_externs_in_time_manager(kgen_unit)
    INTEGER, INTENT(IN) :: kgen_unit
    LOGICAL :: kgen_istrue
    REAL(KIND=8) :: kgen_array_sum
    
    CALL kr_esmf_clockmod_esmf_clock(tm_clock, kgen_unit)
END SUBROUTINE kr_externs_in_time_manager

!read state subroutine for kr_externs_out_time_manager
SUBROUTINE kr_externs_out_time_manager(kgen_unit)
    INTEGER, INTENT(IN) :: kgen_unit
    
    LOGICAL :: kgen_istrue
    REAL(KIND=8) :: kgen_array_sum
END SUBROUTINE kr_externs_out_time_manager

end module time_manager