
! KGEN-generated Fortran source file
!
! Filename    : time_management.F90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11



    MODULE time_management
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: time_management
        ! !DESCRIPTION:
        !  This module contains a large number of routines for calendar, time
        !  flags and other functions related to model time.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: time_management.F90 26358 2011-01-11 18:10:13Z njn01 $
        !
        ! !USES:
        USE kinds_mod, only : int_kind
        IMPLICIT NONE
        PUBLIC
        ! !PUBLIC MEMBER FUNCTIONS:
        ! !PUBLIC DATA TYPES:
        ! !PUBLIC DATA MEMBERS:
        !-----------------------------------------------------------------------
        !
        !  variables for run control
        !
        !-----------------------------------------------------------------------
        ! specify how to determine stopping time
        ! an identifier for the run
        ! method to determine tracer timestep size
        INTEGER(KIND=int_kind) :: nsteps_run
        ! num of stop_option intervals before stop
        !   OR date (yyyymmdd) at which model stops
        ! integer value for stop_option
        ! steps (full&half) since beginning of run sequence
        ! steps taken since beginning of this run
        ! integer number of steps per day
        ! length of character runid
        ! variables used with avgfit
        ! num of intervals/day into which full &
        !  half timesteps must exactly "fit"
        ! num of full timesteps per fitting interval
        ! num of half timesteps per fitting interval
        ! num of full timesteps per day
        ! num of half timesteps per day
        ! number of steps in each 'fit' interval
        ! number of steps in current 'fit' interval
        ! time_flag id for stopping
        ! time_flag id for a coupled timestep
        ! this timestep is:
        !   step at which year values updated
        !   at the end of the day
        !   at the end of the month
        !   at the end of the year
        !   an averaging timestep
        !   the second of two avg timesteps in row
        !   a forward Euler timestep (first ts)
        !   first time step
        !   a leapfrog timestep
        !   an Euler-backward timestep
        !   at midnight
        !   an ice-formation timestep
        !   time to sample qflux for time avg
        ! the last timestep was:
        !   at the end of the day
        !   at the end of the month
        !   at the end of the year
        !   at midnight
        !   an averaging timestep
        ! the next timestep is:
        !   step at which year values updated
        !   at the end of the month
        !   at midnight
        !   an averaging ts?
        !   a second avg step in a row
        ! does model run end at midnight
        ! does restart have a new step size
        ! number of timesteps in one year
        ! number of timesteps in one day
        ! used to determine close enough
        ! used to determine if seconds_this_year
        !  is close enough to seconds_in_year
        !-----------------------------------------------------------------------
        !
        !     quantities related to date
        !
        !-----------------------------------------------------------------------
        ! character to separate year-month-day
        ! year    [0,inf)  for present timestep
        ! month   [1,12]          |
        ! day     [1,31]          |
        ! hour    [0,23]          |
        ! minute  [0,59]          |
        ! second  [0,59]          |
        ! day no. [1,365/6]       V
        ! month   [1,12]    for next timestep
        ! day     [1,31]          |
        ! hour    [0,23]          |
        ! minute  [0,59]          |
        ! second  [0,59]          |
        ! day no. [1,365/6]       V
        ! year    [0,inf)   from previous timestep
        ! month   [1,12]          |
        ! day     [1,31]          |
        ! hour    [0,23]          |
        ! second  [0,59]          |
        ! day no. [1,365/6]       V
        ! initial start date and time
        !   for complete run
        !
        !
        !
        !
        ! initial start date and time
        !   for this run
        !
        !
        !
        !
        !
        ! final date for this run
        !
        !
        ! number of:
        ! days in present year
        ! days in prior   year
        ! full days elapsed since    01-01-0000
        ! full days elapsed between  01-01-0000
        !      and initial start date
        ! full days elapsed prior to 01-01-iyear
        ! full days elapsed since beginning of
        !                   this segment of run
        ! full days elapsed since beginning of yr
        ! full days elapsed since initial time
        ! full days elapsed from 01-01-0000 to end
        !                   of this run
        ! maximum number of full days allowed
        ! full months elapsed since   01-01-0000
        ! full months elapsed between 01-01-0000
        !       and initial start date
        ! full months elapsed since beginning of
        !                     this segment of run
        ! full months elapsed since initial start date
        ! full years  elapsed since   01-01-0000
        ! full years  elapsed between 01-01-0000
        !      and initial start date
        ! full years  elapsed since beginning of
        !      this segment of run
        ! full years  elapsed since initial start date
        !   days in a leap year
        !   days in a non-leap year
        ! cumulative num days in preceeding months
        ! number of days in each calendar month
        !   J  F  M    A  M  J   J  A  S     O  N  D
        ! seconds elapsed since beginning of year
        ! seconds elapsed this day
        ! seconds elapsed this day  at next timestep
        ! seconds elapsed this year at next timestep
        ! seconds in one year -- this varies,
        !         if leap years are allowed
        ! hours   in one year
        ! fraction of the day elapsed today
        ! decimal elapsed time in years
        ! decimal elapsed time in months
        ! decimal elapsed time in days
        ! decimal elapsed time in hours
        ! decimal elapsed time in seconds
        ! tsecond from previous timestep
        !
        !
        !
        ! allow leap years?
        ! is this a leapyear?
        ! character version of year
        ! character version of month
        ! character version of day
        ! character version of hour
        ! character version of minute
        ! character version of second
        ! character month in 3-letter form
        !*** for forcing calendar
        !
        !
        !
        !
        ! num hours to middle of calendar month
        ! num hours to end of calendar month
        ! num hours to middle of equal-spaced month
        ! num hours to end of equal-spaced month
        !-----------------------------------------------------------------------
        !
        !  parameters for time frequency and start options
        !
        !-----------------------------------------------------------------------
        ! integer choices for freq option
        ! integer choices for start options
        !-----------------------------------------------------------------------
        !
        !  user defined time flags
        !
        !-----------------------------------------------------------------------
        ! max number of user-defined flags
        ! array containing user-defined flags
        !-----------------------------------------------------------------------
        !
        !  time-step related constants and variables
        !
        !-----------------------------------------------------------------------
        ! flag for acceleration
        ! input count to determine dtt
        ! tracer timestep (sec)
        ! tracer timestep (sec) as specified in namelist
        !   input; may be different from restart value
        ! momentum timestep (sec)
        ! barotropic timestep (sec)
        !
        !
        !
        ! factor to multiply MOMENTUM timestep
        ! size of present timestep (sec)
        ! size of next timestep (sec)
        ! array for depth-dependent acceleration
        ! time step at each level
        !-----------------------------------------------------------------------
        !
        !  time-centering and mixing variables
        !
        !-----------------------------------------------------------------------
        ! implicit treatment of Coriolis terms
        ! use matsuno step for time mixing
        ! use averaging step for time mixing
        ! use averaging step for time mixing, with
        !  back_to_back option to keep time boundaries
        ! use averaging step for time mixing,
        !  selecting the timestep size in such
        !  a way as to force the end of the day
        !  (or interval) to coincide with the end of
        !  a timestep
        ! option for which time mixing to use
        ! frequency of mixing
        ! number of passes to perform mixing
        ! = {alpha,theta} on {leapfrog,Matsuno} steps
        ! leapfrog grap(ps) time-centering param
        ! Matsuno grap(ps) time-centering param
        ! for geostrophic balance, otherwise
        ! coriolis and  surface-pressure gradient
        ! are not time centered
        !-----------------------------------------------------------------------
        !
        !  variables documenting avgfit progression
        !
        !-----------------------------------------------------------------------
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !
        !  a few private variables used only internally
        !
        !-----------------------------------------------------------------------
        !access via time-flags only
        ! rhour   for next timestep
        ! rminute for next timestep
        ! rsecond for next timestep
        ! hour value at end of each interval
        ! min  value at end of each interval
        ! sec  value at end of each interval
        !EOC
        !***********************************************************************
            PUBLIC kgen_read_externs_time_management
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_time_management(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) nsteps_run
        END SUBROUTINE kgen_read_externs_time_management

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_time1
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: test_timestep
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_time2
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: time_manager
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: reset_switches
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: set_switches
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: access_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: set_num_time_flags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: get_time_flag_id
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: search_time_flags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: document_time_flags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: document_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: override_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: reset_time_flags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: reset_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: check_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: check_time_flag_int
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: eval_time_flags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: eval_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: error_check_time_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: time_to_do
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: time_to_start
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: model_date
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: get_tday
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ymd_hms
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: hms
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: reduce_months
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: reduce_seconds
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: leap_adjust
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: date2ymd
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ymd2date
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: eday2ymd
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ymd2eday
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: date2eday
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: eday2date
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: prior_days
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: time_stamp
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ccsm_date_stamp
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: is_near
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: is_leapyear
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: valid_date
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: valid_ymd_hms()
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: write_time_manager_options
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: int_to_char
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ccsm_char_date_and_time
        ! !INTERFACE:

        !***********************************************************************
        !***********************************************************************
    END MODULE time_management
