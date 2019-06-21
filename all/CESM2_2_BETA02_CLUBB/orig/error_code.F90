!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:35 
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
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 


    IMPLICIT NONE 

    PRIVATE 

    PUBLIC clubb_at_least_debug_level 

    PRIVATE clubb_debug_level 
    ! Model-Wide Debug Level

    integer, save :: clubb_debug_level = 0

    integer, public :: err_code = 0;

    character(len=35), public :: err_header
    ! Error Code Values

    !$omp threadprivate(clubb_debug_level,err_code,err_header)

    integer, parameter, public :: & 
        clubb_no_error                 = 0, & 
        clubb_fatal_error              = 99
    PUBLIC kr_externs_in_error_code 
    PUBLIC kr_externs_out_error_code 
    PUBLIC kv_externs_error_code 
    INTEGER :: kgenref_err_code = 0 

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
        READ (UNIT = kgen_unit) err_code 
        READ (UNIT = kgen_unit) err_header 
    END SUBROUTINE kr_externs_in_error_code 
      
    !read state subroutine for kr_externs_out_error_code 
    SUBROUTINE kr_externs_out_error_code(kgen_unit) 
        INTEGER, INTENT(IN) :: kgen_unit 
          
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_err_code 
    END SUBROUTINE kr_externs_out_error_code 
      
    !verify state subroutine for kv_externs_error_code 
    SUBROUTINE kv_externs_error_code(check_status) 
        TYPE(check_t), INTENT(INOUT) :: check_status 
          
        CALL kv_error_code_integer__("err_code", check_status, err_code, kgenref_err_code) 
    END SUBROUTINE kv_externs_error_code 
      
    !verify state subroutine for kv_error_code_integer__ 
    RECURSIVE SUBROUTINE kv_error_code_integer__(varname, check_status, var, kgenref_var) 
        CHARACTER(LEN=*), INTENT(IN) :: varname 
        TYPE(check_t), INTENT(INOUT) :: check_status 
        INTEGER, INTENT(IN) :: var, kgenref_var 
        INTEGER :: check_result 
        LOGICAL :: is_print = .FALSE. 
          
        integer :: diff 
          
        check_status%numTotal = check_status%numTotal + 1 
          
        IF (var == kgenref_var) THEN 
            check_status%numIdentical = check_status%numIdentical + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                END IF   
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            diff = ABS(var - kgenref_var) 
            IF (diff <= kgen_tolerance) THEN 
                check_status%numInTol = check_status%numInTol + 1 
                IF (kgen_verboseLevel > 1) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_IN_TOL 
            ELSE 
                check_status%numOutTol = check_status%numOutTol + 1 
                IF (kgen_verboseLevel > 0) THEN 
                    IF (check_status%rank == 0) THEN 
                        WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                    END IF   
                END IF   
                check_result = CHECK_OUT_TOL 
            END IF   
        END IF   
        IF (check_result == CHECK_IDENTICAL) THEN 
            IF (kgen_verboseLevel > 2) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) "Difference is ", 0 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_OUT_TOL) THEN 
            IF (kgen_verboseLevel > 0) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) "Difference is ", diff 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) "Difference is ", diff 
                    WRITE (*, *) "" 
                END IF   
            END IF   
        END IF   
          
    END SUBROUTINE kv_error_code_integer__ 
      
    end module error_code