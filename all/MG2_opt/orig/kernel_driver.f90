
! KGEN-generated Fortran source file
!
! Filename    : kernel_driver.f90
! Generated at: 2015-10-08 11:52:39
! KGEN version: 0.5.2


PROGRAM kernel_driver
    USE micro_mg_cam, ONLY : micro_mg_cam_tend
    USE shr_kind_mod, ONLY: r8 => shr_kind_r8
    USE micro_mg_cam, ONLY : kgen_read_externs_micro_mg_cam
    USE micro_mg_utils, ONLY : kgen_read_externs_micro_mg_utils
    USE micro_mg2_0, ONLY : kgen_read_externs_micro_mg2_0
    USE wv_sat_methods, ONLY : kgen_read_externs_wv_sat_methods
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check

    IMPLICIT NONE

    INTEGER :: kgen_mpi_rank
    CHARACTER(LEN=16) ::kgen_mpi_rank_conv
    INTEGER, DIMENSION(1), PARAMETER :: kgen_mpi_rank_at = (/ 0 /)
    INTEGER :: kgen_ierr, kgen_unit
    INTEGER :: kgen_repeat_counter
    INTEGER :: kgen_counter
    CHARACTER(LEN=16) :: kgen_counter_conv
    INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 1 /)
    CHARACTER(LEN=1024) :: kgen_filepath
    REAL(KIND=r8) :: dtime

    DO kgen_repeat_counter = 0, 0
        kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_counter_conv, * ) kgen_counter
        kgen_mpi_rank = kgen_mpi_rank_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank
        kgen_filepath = "../data/micro_mg_tend2_0." // trim(adjustl(kgen_counter_conv)) // "." // trim(adjustl(kgen_mpi_rank_conv))
        kgen_unit = kgen_get_newunit()
        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
        WRITE (*,*)
        IF ( kgen_ierr /= 0 ) THEN
            CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )
        END IF
        WRITE (*,*)
        WRITE (*,*) "** Verification against '" // trim(adjustl(kgen_filepath)) // "' **"

            CALL kgen_read_externs_micro_mg_cam(kgen_unit)
            CALL kgen_read_externs_micro_mg_utils(kgen_unit)
            CALL kgen_read_externs_micro_mg2_0(kgen_unit)
            CALL kgen_read_externs_wv_sat_methods(kgen_unit)

            ! driver variables
            READ(UNIT=kgen_unit) dtime

            call micro_mg_cam_tend(dtime, kgen_unit)

            CLOSE (UNIT=kgen_unit)
        END DO


        WRITE (*, *) ""
        WRITE (*, "(A)") "****************************************************"
        WRITE (*, "(4X,A)") "kernel execution summary: micro_mg_tend2_0"
        WRITE (*, "(A)") "****************************************************"
        IF (kgen_repeat_counter == 0) THEN
            WRITE (*, *) "No data file is verified."
        ELSE
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", 1
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
