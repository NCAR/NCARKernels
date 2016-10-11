
! KGEN-generated Fortran source file
!
! Filename    : kernel_driver.f90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11


PROGRAM kernel_driver
    USE kinds_mod, only : int_kind
    USE ecosys_mod, ONLY : ecosys_set_interior
    USE ecosys_mod, ONLY : kgen_read_externs_ecosys_mod
    USE domain, ONLY : kgen_read_externs_domain
    USE time_management, ONLY : kgen_read_externs_time_management
    USE co2calc, ONLY : kgen_read_externs_co2calc
    USE blocks, ONLY : kgen_read_externs_blocks
    USE grid, ONLY : kgen_read_externs_grid
    USE constants, ONLY : kgen_read_externs_constants
    USE state_mod, ONLY : kgen_read_externs_state_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check

    IMPLICIT NONE

    INTEGER :: kgen_mpi_rank
    CHARACTER(LEN=16) ::kgen_mpi_rank_conv
    INTEGER, DIMENSION(1), PARAMETER :: kgen_mpi_rank_at = (/ 30 /)
    INTEGER :: kgen_ierr, kgen_unit
    INTEGER :: kgen_repeat_counter
    INTEGER :: kgen_counter
    CHARACTER(LEN=16) :: kgen_counter_conv
    INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 10 /)
    CHARACTER(LEN=1024) :: kgen_filepath
    INTEGER(KIND=int_kind) :: k

    DO kgen_repeat_counter = 0, 0
        kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_counter_conv, * ) kgen_counter
        kgen_mpi_rank = kgen_mpi_rank_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank
        kgen_filepath = "../data/comp_co3terms." // trim(adjustl(kgen_counter_conv)) // "." // trim(adjustl(kgen_mpi_rank_conv))
        kgen_unit = kgen_get_newunit()
        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
        WRITE (*,*)
        IF ( kgen_ierr /= 0 ) THEN
            CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )
        END IF
        WRITE (*,*)
        WRITE (*,*) "** Verification against '" // trim(adjustl(kgen_filepath)) // "' **"

            CALL kgen_read_externs_ecosys_mod(kgen_unit)
            CALL kgen_read_externs_domain(kgen_unit)
            CALL kgen_read_externs_time_management(kgen_unit)
            CALL kgen_read_externs_co2calc(kgen_unit)
            CALL kgen_read_externs_blocks(kgen_unit)
            CALL kgen_read_externs_grid(kgen_unit)
            CALL kgen_read_externs_constants(kgen_unit)
            CALL kgen_read_externs_state_mod(kgen_unit)

            ! driver variables
            READ(UNIT=kgen_unit) k

            call ecosys_set_interior(k, kgen_unit)

            CLOSE (UNIT=kgen_unit)
        END DO

        WRITE (*, *) ""
        WRITE (*, "(A)") "****************************************************"
        WRITE (*, "(4X,A)") "kernel execution summary: comp_co3terms"
        WRITE (*, "(A)") "****************************************************"
        IF (kgen_repeat_counter == 0) THEN
            WRITE (*, *) "No data file is verified."
        ELSE
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_repeat_counter
        END IF
        WRITE (*, "(A)") "****************************************************"

    CONTAINS

        ! write subroutines
        ! No subroutines
        FUNCTION kgen_get_newunit() RESULT(new_unit)
           INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000
           LOGICAL :: is_opened
           INTEGER :: nunit, new_unit, counter
        
           new_unit = -1
           DO counter=UNIT_MIN, UNIT_MAX
               inquire(UNIT=counter, OPENED=is_opened)
               IF (.NOT. is_opened) THEN
                   new_unit = counter
                   EXIT
               END IF
           END DO
        END FUNCTION
        
        SUBROUTINE kgen_error_stop( msg )
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: msg
        
            WRITE (*,*) msg
            STOP 1
        END SUBROUTINE 


    END PROGRAM kernel_driver
