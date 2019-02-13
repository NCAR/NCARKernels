!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: time_manager_mod.f90 11289 2017-03-10 21:56:06Z hendric@ucar.edu $


!

module time_manager_mod

    USE utilities_mod, ONLY: error_handler, e_err, register_module 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
!====================================================================
! This module works best when the real variables have more than 7 
! significant digits, so this module uses the 'digits12' parameter 
! defined in types_mod ... 
!====================================================================
! The time_manager provides a single defined type, time_type, which is 
! used to store time and date quantities. A time_type is a positive 
! definite quantity that represents an interval of time. It can be most 
! easily thought of as representing the number of seconds in some time 
! interval. A time interval can be mapped to a date under a given calendar 
! definition by using it to represent the time that has passed since some 
! base date. A number of interfaces are provided to operate on time_type 
! variables and their associated calendars. Time intervals can be as large 
! as n days where n is the largest number represented by the default integer 
! type on a compiler. This is typically considerably greater than 10 million 
! years which is likely to be adequate for most applications. The 
! description of the interfaces is separated into two sections. The first 
! deals with operations on time intervals while the second deals with 
! operations that convert time intervals to dates for a given calendar.
!====================================================================
! Module defines a single type

!

    PUBLIC time_type 
! Operators defined on time_type

! Subroutines and functions operating on time_type

    PUBLIC get_time 
! List of available calendar types

! Subroutines and functions involving relations between time and calendar


! Subroutines and functions for basic I/O

    PUBLIC time_manager_init 
! version controlled file description for error handling, do not edit

character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/time_manager_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 11289 $"
character(len=128), parameter :: revdate  = "$Date: 2017-03-10 14:56:06 -0700 (Fri, 10 Mar 2017) $"
! Global data to define calendar type

! FIXME: should be a namelist to select default calendar.  make no calendar the
! default for now
! Define number of days per month

! time_type is implemented as seconds and days to allow for larger intervals

type time_type
   private
   integer:: seconds
   integer:: days
end type time_type
!======================================================================


!======================================================================


logical, save :: module_initialized = .false.

character(len=129) :: errstring
!======================================================================
PUBLIC kr_externs_in_time_manager_mod 
PUBLIC kr_externs_out_time_manager_mod 
PUBLIC kv_externs_time_manager_mod 
PUBLIC kr_time_manager_mod_time_type 
LOGICAL :: kgenref_module_initialized = .false. 
PUBLIC kv_time_manager_mod_time_type 


contains
! First define all operations on time intervals independent of calendar


subroutine get_time(time, seconds, days)
!---------------------------------------------------------------------------
! Returns days and seconds ( < 86400 ) corresponding to a time.
! If the optional 'days' argument is not given, the days are converted
! to seconds and the total time is returned as seconds.
!

type(time_type), intent(in)            :: time
integer,         intent(out)           :: seconds
integer,         intent(out), optional :: days

if ( .not. module_initialized ) call time_manager_init

seconds = time%seconds
if (present(days)) then
  days = time%days
else

  write(errstring,*)'seconds is ',seconds,' overflowing conversion to days'

  if (time%days > (huge(seconds) - seconds)/(60*60*24)) &
     call error_handler(E_ERR,'get_time',errstring,source,revision,revdate)

  seconds = seconds + time%days * (60*60*24)

endif

end subroutine get_time


!=========================================================================
! CALENDAR OPERATIONS BEGIN HERE
!=========================================================================


!========================================================================
! START OF get_date BLOCK
!========================================================================


!========================================================================
! END OF get_date BLOCK
!========================================================================
! START OF set_date BLOCK
!========================================================================


!=========================================================================
! END OF set_date BLOCK
!=========================================================================
! START OF increment_date BLOCK
!=========================================================================


!=========================================================================
! END OF increment_date BLOCK
!=========================================================================
! START OF decrement_date BLOCK
!=========================================================================


!=========================================================================
! END OF decrement_date BLOCK
!=========================================================================
! START days_in_month BLOCK
!=========================================================================


!==========================================================================
! END OF days_in_month BLOCK
!==========================================================================
! START OF leap_year BLOCK
!==========================================================================


!==========================================================================
! END OF leap_year BLOCK
!==========================================================================
! START OF length_of_year BLOCK
!==========================================================================


!==========================================================================
! END OF length_of_year BLOCK
!==========================================================================
! START OF days_in_year BLOCK
!==========================================================================


!==========================================================================
! END OF days_in_year BLOCK
!==========================================================================


!==========================================================================


!==========================================================================


subroutine time_manager_init ( )
!------------------------------------------------------------------------
! initialization routine
! this routine should be called, even though all it does is write
! the version information to the log file
!

   if ( module_initialized ) return  ! silent return if already called

   call register_module (source, revision, revdate)
   module_initialized  = .true.

end subroutine time_manager_init


!-------------------------------------------------------------------------


!-------------------------------------------------------------------------


!-------------------------------------------------------------------------


!read state subroutine for kr_externs_in_time_manager_mod 
SUBROUTINE kr_externs_in_time_manager_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) module_initialized 
    READ (UNIT = kgen_unit) errstring 
END SUBROUTINE kr_externs_in_time_manager_mod 
  
!read state subroutine for kr_externs_out_time_manager_mod 
SUBROUTINE kr_externs_out_time_manager_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_module_initialized 
END SUBROUTINE kr_externs_out_time_manager_mod 
  
!verify state subroutine for kv_externs_time_manager_mod 
SUBROUTINE kv_externs_time_manager_mod(check_status) 
    TYPE(check_t), INTENT(INOUT) :: check_status 
      
END SUBROUTINE kv_externs_time_manager_mod 
  
!read state subroutine for kr_time_manager_mod_time_type 
RECURSIVE SUBROUTINE kr_time_manager_mod_time_type(var, kgen_unit, printname, printvar) 
    TYPE(time_type), INTENT(INOUT) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) var%seconds 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%seconds = ", var%seconds 
    END IF   
      
    READ (UNIT = kgen_unit) var%days 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%days = ", var%days 
    END IF   
      
END SUBROUTINE kr_time_manager_mod_time_type 
  
!verify state subroutine for kv_time_manager_mod_time_type 
RECURSIVE SUBROUTINE kv_time_manager_mod_time_type(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    TYPE(time_type), INTENT(IN) :: var, kgenref_var 
    TYPE(check_t) :: dtype_check_status, comp_check_status 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    integer :: diff_seconds 
    integer :: diff_days 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%seconds == kgenref_var%seconds) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%seconds is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_seconds = ABS(var%seconds - kgenref_var%seconds) 
        IF (diff_seconds <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%seconds is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%seconds is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_seconds 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_seconds 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%days == kgenref_var%days) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%days is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_days = ABS(var%days - kgenref_var%days) 
        IF (diff_days <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%days is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%days is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_days 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_days 
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
END SUBROUTINE kv_time_manager_mod_time_type 
  
end module time_manager_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/time_manager_mod.f90 $
! $Id: time_manager_mod.f90 11289 2017-03-10 21:56:06Z hendric@ucar.edu $
! $Revision: 11289 $
! $Date: 2017-03-10 14:56:06 -0700 (Fri, 10 Mar 2017) $
