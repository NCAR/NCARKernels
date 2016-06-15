!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:55
!KGEN version : 0.6.2

!===============================================================================
! SVN $Id: shr_sys_mod.F90 66411 2014-12-19 22:40:08Z santos@ucar.edu $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/trunk_tags/share3_150116/shr/shr_sys_mod.F90 $
!===============================================================================

! Currently supported by all compilers



! Except this combination?








MODULE shr_sys_mod

    USE, INTRINSIC :: iso_fortran_env, ONLY: output_unit, error_unit

    USE shr_kind_mod
!    USE shr_mpi_mod
    USE shr_log_mod, ONLY: s_logunit => shr_log_unit








    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

! PUBLIC: Public interfaces

    PRIVATE

    PUBLIC shr_sys_abort
    PUBLIC shr_sys_backtrace

!===============================================================================
CONTAINS
!===============================================================================

!===============================================================================
!===============================================================================



   !----- arguments ---

   !----- functions -----



   !----- formats -----

!-------------------------------------------------------------------------------
! PURPOSE: an architecture independent system call
!-------------------------------------------------------------------------------










!===============================================================================
!===============================================================================



   !----- arguments -----

   !----- local -----



   !----- formats -----

!-------------------------------------------------------------------------------
! PURPOSE: an architecture independent system call
!-------------------------------------------------------------------------------











!===============================================================================
!===============================================================================



   !----- arguments -----

   !----- local -----

   !----- formats -----

!-------------------------------------------------------------------------------
! PURPOSE: an architecture independent system call
!-------------------------------------------------------------------------------


!===============================================================================
!===============================================================================

SUBROUTINE shr_sys_abort(string,rc)

   IMPLICIT none

   character(*)        ,optional :: string  ! error message string
   integer(SHR_KIND_IN),optional :: rc      ! error code

   !----- local -----
   logical              :: flag

   !----- formats -----
   character(*),parameter :: subName =   '(shr_sys_abort) '
   character(*),parameter :: F00     = "('(shr_sys_abort) ',4a)"

   ! Local version of the string.
   ! (Gets a default value if string is not present.)
   character(len=shr_kind_cx) :: local_string

!-------------------------------------------------------------------------------
! PURPOSE: consistent stopping mechanism
!-------------------------------------------------------------------------------

   if (present(string)) then
      local_string = trim(string)
   else
      local_string = "Unknown error submitted to shr_sys_abort."
   end if

   call print_error_to_logs("ERROR", local_string)

   call shr_sys_backtrace()

!   call shr_mpi_initialized(flag)

!   if (flag) then
!      if (present(rc)) then
!         call shr_mpi_abort(trim(local_string),rc)
!      else
!         call shr_mpi_abort(trim(local_string))
!      endif
!   endif

  ! A compiler's abort method may print a backtrace or do other nice
  ! things, but in fact we can rarely leverage this, because MPI_Abort
  ! usually sends SIGTERM to the process, and we don't catch that signal.
   call abort()

END SUBROUTINE shr_sys_abort

!===============================================================================
!===============================================================================



   !----- arguments -----

   !----- local -----

   !----- formats -----

!-------------------------------------------------------------------------------
! emulates Cray/SGI irtc function (returns clock tick since last reboot)
!-------------------------------------------------------------------------------



   !--- adjust for clock wrap-around ---



!===============================================================================
!===============================================================================



   !----- arguments -----

   !----- local -----

   !----- formats -----

!-------------------------------------------------------------------------------
! PURPOSE: Sleep for approximately sec seconds
!-------------------------------------------------------------------------------



      ! Don't consider this an error and don't call system sleep







!===============================================================================
!===============================================================================



   !----- arguments -----

   !----- local -----

   !----- formats -----

!-------------------------------------------------------------------------------
! PURPOSE: an architecture independent system call
!
! This is probably no longer needed; the "flush" statement is supported by
! all compilers that CESM supports for years now.
!
!-------------------------------------------------------------------------------
!
! The following code was originally present, but there's an obvious issue.
! Since shr_sys_flush is usually used to flush output to a log, when it
! returns an error, does it do any good to print that error to the log?
!
!   if (ierr > 0) then
!      write(s_logunit,*) subname,' Flush reports error: ',ierr
!   endif
!


!===============================================================================
!===============================================================================

subroutine shr_sys_backtrace()

  ! This routine uses compiler-specific facilities to print a backtrace to
  ! error_unit (standard error, usually unit 0).


  ! tracebackqq uses optional arguments, so *must* have an explicit
  ! interface.
!SAM  use ifcore, only: tracebackqq

  ! An exit code of -1 is a special value that prevents this subroutine
  ! from aborting the run.
!SAM  call tracebackqq(user_exit_code=-1)


  flush(error_unit)

end subroutine shr_sys_backtrace

!===============================================================================
!===============================================================================

!
! This routine prints error messages to s_logunit (which is standard output
! for most tasks in CESM) and also to standard error if s_logunit is a
! file.
!
! It also flushes these output units.
!
subroutine print_error_to_logs(error_type, message)
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
!===============================================================================

END MODULE shr_sys_mod
