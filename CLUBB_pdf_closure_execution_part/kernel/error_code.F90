!KGEN-generated Fortran source file

!Generated at : 2016-01-04 08:38:23
!KGEN version : 0.6.0

!-------------------------------------------------------------------------------
! $Id: error_code.F90 7184 2014-08-11 15:23:43Z betlej@uwm.edu $
!-------------------------------------------------------------------------------

module error_code

! Description:
!   Since f90/95 lacks enumeration, we're stuck numbering each
!   error code by hand like this.

!   We are "enumerating" error codes to be used with CLUBB. Adding
!   additional codes is as simple adding an additional integer
!   parameter. The error codes are ranked by severity, the higher
!   number being more servere. When two errors occur, assign the
!   most servere to the output.

!   This code also handles subroutines related to debug_level. See
!   the 'set_clubb_debug_level' description for more detail.

! References:
!   None
!-------------------------------------------------------------------------------

    IMPLICIT NONE

    PRIVATE

    PUBLIC fatal_error, clubb_at_least_debug_level

    PRIVATE clubb_debug_level

  ! Model-Wide Debug Level
  integer, save :: clubb_debug_level = 0

!$omp threadprivate(clubb_debug_level)

  ! Error Code Values
  integer, parameter, public :: & 
    clubb_no_error                 =  0, & 
    clubb_var_less_than_zero       =  1, & 
    clubb_var_equals_NaN           =  2, & 
    clubb_singular_matrix          =  3, & 
    clubb_bad_lapack_arg           =  4, & 
    clubb_rtm_level_not_found      =  5, & 
    clubb_var_out_of_bounds        =  6, &
    clubb_var_out_of_range         =  7

  PUBLIC kr_externs_in_error_code
  PUBLIC kr_externs_out_error_code
  contains

!-------------------------------------------------------------------------------
!
! Description: 
!   Reports meaning of error code to console.
!
!-------------------------------------------------------------------------------



    ! Input Variable

    ! ---- Begin Code ----












!-------------------------------------------------------------------------------
!
! Description: 
!   Checks to see if the err_code is equal to one
!   caused by an error encountered using LAPACK.
! Reference:
!   None
!-------------------------------------------------------------------------------

    ! Input variable

    ! Output variable

    ! ---- Begin Code ----



!-------------------------------------------------------------------------------
  elemental function fatal_error( err_code )
!
! Description: Checks to see if the err_code is one that usually
!   causes an exit in other parts of CLUBB.
! References:
!   None
!-------------------------------------------------------------------------------
    implicit none

    ! Input Variable
    integer, intent(in) :: err_code ! Error Code being examined

    ! Output variable
    logical :: fatal_error

    ! ---- Begin Code ----

    fatal_error = err_code /= clubb_no_error .and. & 
                  err_code /= clubb_var_less_than_zero
    return
  end function fatal_error

!------------------------------------------------------------------	
  logical function clubb_at_least_debug_level( level )
!
! Description:
!   Checks to see if clubb has been set to a specified debug level
!------------------------------------------------------------------
    implicit none

    ! Input variable
    integer, intent(in) :: level   ! The debug level being checked against the current setting

    ! ---- Begin Code ----

    clubb_at_least_debug_level = ( level <= clubb_debug_level )

    return
  end function clubb_at_least_debug_level

!-------------------------------------------------------------------------------
!
!  Description:
!    Accessor for clubb_debug_level
!
!   0 => Print no debug messages to the screen
!   1 => Print lightweight debug messages, e.g. print statements
!   2 => Print debug messages that require extra testing,
!        e.g. checks for NaNs and spurious negative values.
!  References:
!    None
!-------------------------------------------------------------------------------

    ! Input variable

    ! ---- Begin Code ----



!-------------------------------------------------------------------------------
!
! Description:
!   Prints a message to file unit fstderr if the level is greater
!   than or equal to the current debug level.
!-------------------------------------------------------------------------------


    ! Input Variable(s)


    ! The debug level being checked against the current setting

    ! ---- Begin Code ----




  !read state subroutine for kr_externs_in_error_code
  SUBROUTINE kr_externs_in_error_code(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      
      READ (UNIT = kgen_unit) clubb_debug_level
  END SUBROUTINE kr_externs_in_error_code
  
  !read state subroutine for kr_externs_out_error_code
  SUBROUTINE kr_externs_out_error_code(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      
      LOGICAL :: kgen_istrue
  END SUBROUTINE kr_externs_out_error_code
  
end module error_code
!-------------------------------------------------------------------------------