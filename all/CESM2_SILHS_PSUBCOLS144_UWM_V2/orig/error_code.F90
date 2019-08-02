!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:55 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id$
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
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


    IMPLICIT NONE 

    PRIVATE 

    PUBLIC clubb_at_least_debug_level 

    PRIVATE clubb_debug_level 
    ! Model-Wide Debug Level

    integer, save :: clubb_debug_level = 0


    ! Error Code Values

    !$omp threadprivate(clubb_debug_level,err_code,err_header)

    PUBLIC kr_externs_in_error_code 
    PUBLIC kr_externs_out_error_code 

    contains
!-------------------------------------------------------------------------------
! Description:
!   Checks to see if clubb has been set to a specified debug level
!-------------------------------------------------------------------------------
    logical function clubb_at_least_debug_level( level )

        implicit none
        ! Input variable

        integer, intent(in) :: level   ! The debug level being checked against the current setting
        ! ---- Begin Code ----


        clubb_at_least_debug_level = ( level <= clubb_debug_level )

        return

    end function clubb_at_least_debug_level


!-------------------------------------------------------------------------------
!  Description:
!    Accessor for clubb_debug_level
!   0 => Print no debug messages to the screen
!   1 => Print lightweight debug messages, e.g. print statements
!   2 => Print debug messages that require extra testing,
!        e.g. checks for NaNs and spurious negative values.
!  References:
!    None
!-------------------------------------------------------------------------------


!


    !read state subroutine for kr_externs_in_error_code 
    SUBROUTINE kr_externs_in_error_code(kgen_unit) 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        READ (UNIT = kgen_unit) clubb_debug_level 
    END SUBROUTINE kr_externs_in_error_code 
      
    !read state subroutine for kr_externs_out_error_code 
    SUBROUTINE kr_externs_out_error_code(kgen_unit) 
        INTEGER, INTENT(IN) :: kgen_unit 
          
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
    END SUBROUTINE kr_externs_out_error_code 
      
    end module error_code