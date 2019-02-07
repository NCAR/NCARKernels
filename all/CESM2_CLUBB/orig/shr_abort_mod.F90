!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:38 
!KGEN version : 0.8.1 
  


module shr_abort_mod
  ! This module defines procedures that can be used to abort the model cleanly in a
  ! system-specific manner
  ! The public routines here are only meant to be used directly by shr_sys_mod. Other code
  ! that wishes to use these routines should use the republished names from shr_sys_mod
  ! (shr_sys_abort, shr_sys_backtrace). (This is for consistency with older code, from
  ! when these routines were defined in shr_sys_mod.)
  !

    USE, INTRINSIC :: iso_fortran_env, ONLY: output_unit, error_unit 

    USE shr_kind_mod, ONLY: shr_kind_in, shr_kind_cx 
    USE shr_mpi_mod, ONLY: shr_mpi_initialized, shr_mpi_abort 
    USE shr_log_mod, ONLY: s_logunit => shr_log_unit 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


    IMPLICIT NONE 
  ! PUBLIC: Public interfaces


    PRIVATE 
  ! The public routines here are only meant to be used directly by shr_sys_mod. Other code
  ! that wishes to use these routines should use the republished names from shr_sys_mod
  ! (shr_sys_abort, shr_sys_backtrace). (This is for consistency with older code, from
  ! when these routines were defined in shr_sys_mod.)

    PUBLIC shr_abort_abort 
    PUBLIC shr_abort_backtrace 

contains
  !===============================================================================

  subroutine shr_abort_abort(string,rc)
    ! Consistent stopping mechanism
    !----- arguments -----

    character(len=*)    , intent(in), optional :: string  ! error message string
    integer(shr_kind_in), intent(in), optional :: rc      ! error code
    !----- local -----

    logical :: flag
    ! Local version of the string.
    ! (Gets a default value if string is not present.)

    character(len=shr_kind_cx) :: local_string
    !-------------------------------------------------------------------------------

    if (present(string)) then
       local_string = trim(string)
    else
       local_string = "Unknown error submitted to shr_abort_abort."
    end if

    call print_error_to_logs("ERROR", local_string)

    call shr_abort_backtrace()

    call shr_mpi_initialized(flag)

    if (flag) then
       if (present(rc)) then
          call shr_mpi_abort(trim(local_string),rc)
       else
          call shr_mpi_abort(trim(local_string))
       endif
    endif
    ! A compiler's abort method may print a backtrace or do other nice
    ! things, but in fact we can rarely leverage this, because MPI_Abort
    ! usually sends SIGTERM to the process, and we don't catch that signal.


  end subroutine shr_abort_abort
  !===============================================================================
  !===============================================================================

  subroutine shr_abort_backtrace()
    ! This routine uses compiler-specific facilities to print a backtrace to
    ! error_unit (standard error, usually unit 0).
    ! tracebackqq uses optional arguments, so *must* have an explicit
    ! interface.


    ! An exit code of -1 is a special value that prevents this subroutine
    ! from aborting the run.


    flush(error_unit)

  end subroutine shr_abort_backtrace
  !===============================================================================
  !===============================================================================

  subroutine print_error_to_logs(error_type, message)
    ! This routine prints error messages to s_logunit (which is standard output
    ! for most tasks in CESM) and also to standard error if s_logunit is a
    ! file.
    ! It also flushes these output units.
    !

    character(len=*), intent(in) :: error_type, message

    integer, allocatable :: log_units(:)

    integer :: i

    if (s_logunit == output_unit .or. s_logunit == error_unit) then
       ! If the log unit number is standard output or standard error, just
       ! print to that.
       allocate(log_units(1), source=[s_logunit])
    else
       ! Otherwise print the same message to both the log unit and standard
       ! error.
       allocate(log_units(2), source=[error_unit, s_logunit])
    end if

    do i = 1, size(log_units)
       write(log_units(i),*) trim(error_type), ": ", trim(message)
       flush(log_units(i))
    end do

  end subroutine print_error_to_logs
  !===============================================================================

end module shr_abort_mod