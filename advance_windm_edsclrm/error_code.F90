
! KGEN-generated Fortran source file
!
! Filename    : error_code.F90
! Generated at: 2015-10-21 08:59:09
! KGEN version: 0.5.3



    MODULE error_code
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
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
        PRIVATE ! Default Scope
        PUBLIC clubb_debug, clubb_at_least_debug_level, fatal_error
        PRIVATE clubb_debug_level
! Model-Wide Debug Level
        INTEGER, save :: clubb_debug_level = 0
!$omp threadprivate(clubb_debug_level)
! Error Code Values
        INTEGER, parameter, public :: clubb_no_error                 =  0
        INTEGER, parameter, public :: clubb_bad_lapack_arg           =  4
        INTEGER, parameter, public :: clubb_var_equals_nan           =  2
        INTEGER, parameter, public :: clubb_singular_matrix          =  3
        INTEGER, parameter, public :: clubb_var_less_than_zero       =  1
            PUBLIC kgen_read_externs_error_code
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_error_code(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) clubb_debug_level
        END SUBROUTINE kgen_read_externs_error_code

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

        elemental FUNCTION fatal_error(err_code)
!
! Description: Checks to see if the err_code is one that usually
!   causes an exit in other parts of CLUBB.
! References:
!   None
!-------------------------------------------------------------------------------
            IMPLICIT NONE
! Input Variable
            INTEGER, intent(in) :: err_code ! Error Code being examined
! Output variable
            LOGICAL :: fatal_error
! ---- Begin Code ----
    fatal_error = err_code /= clubb_no_error .and. & 
                  err_code /= clubb_var_less_than_zero
    return
        END FUNCTION fatal_error
!------------------------------------------------------------------

        logical FUNCTION clubb_at_least_debug_level(level)
!
! Description:
!   Checks to see if clubb has been set to a specified debug level
!------------------------------------------------------------------
            IMPLICIT NONE
! Input variable
            INTEGER, intent(in) :: level ! The debug level being checked against the current setting
! ---- Begin Code ----
    clubb_at_least_debug_level = ( level <= clubb_debug_level )
    return
        END FUNCTION clubb_at_least_debug_level
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------

        SUBROUTINE clubb_debug(level, str)
!
! Description:
!   Prints a message to file unit fstderr if the level is greater
!   than or equal to the current debug level.
!-------------------------------------------------------------------------------
            USE constants_clubb, ONLY: fstderr
! Variable(s)
            IMPLICIT NONE
! Input Variable(s)
            CHARACTER(LEN=*), intent(in) :: str ! The message being reported
! The debug level being checked against the current setting
            INTEGER, intent(in) :: level
! ---- Begin Code ----
    if ( level <= clubb_debug_level ) then
      write(fstderr,*) str
    end if
    return
        END SUBROUTINE clubb_debug
    END MODULE error_code
