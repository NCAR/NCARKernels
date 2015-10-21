
! KGEN-generated Fortran source file
!
! Filename    : kernel_driver.f90
! Generated at: 2015-10-20 14:26:59
! KGEN version: 0.5.3


PROGRAM kernel_driver
    USE clubb_intr, ONLY : clubb_tend_cam
    USE clubb_intr, ONLY : kgen_read_externs_clubb_intr
    USE sponge_layer_damping, ONLY : kgen_read_externs_sponge_layer_damping
    USE parameters_tunable, ONLY : kgen_read_externs_parameters_tunable
    USE grid_class, ONLY : kgen_read_externs_grid_class
    USE array_index, ONLY : kgen_read_externs_array_index
    USE stats_variables, ONLY : kgen_read_externs_stats_variables
    USE saturation, ONLY : kgen_read_externs_saturation
    USE variables_diagnostic_module, ONLY : kgen_read_externs_variables_diagnostic_module
    USE parameters_model, ONLY : kgen_read_externs_parameters_model
    USE model_flags, ONLY : kgen_read_externs_model_flags
    USE stat_file_module, ONLY : kgen_read_externs_stat_file_module
    USE error_code, ONLY : kgen_read_externs_error_code
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb

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
    REAL(KIND=kgen_dp) :: total_time

    total_time = 0.0_kgen_dp

    DO kgen_repeat_counter = 0, 0
        kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_counter_conv, * ) kgen_counter
        kgen_mpi_rank = kgen_mpi_rank_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank
        kgen_filepath = "./advance_clubb_core." // trim(adjustl(kgen_counter_conv)) // "." // trim(adjustl(kgen_mpi_rank_conv))
        kgen_unit = kgen_get_newunit()
        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
        WRITE (*,*)
        IF ( kgen_ierr /= 0 ) THEN
            CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )
        END IF
        WRITE (*,*)
        WRITE (*,*) "** Verification against '" // trim(adjustl(kgen_filepath)) // "' **"

            CALL kgen_read_externs_clubb_intr(kgen_unit)
            CALL kgen_read_externs_sponge_layer_damping(kgen_unit)
            CALL kgen_read_externs_parameters_tunable(kgen_unit)
            CALL kgen_read_externs_grid_class(kgen_unit)
            CALL kgen_read_externs_array_index(kgen_unit)
            CALL kgen_read_externs_stats_variables(kgen_unit)
            CALL kgen_read_externs_saturation(kgen_unit)
            CALL kgen_read_externs_variables_diagnostic_module(kgen_unit)
            CALL kgen_read_externs_parameters_model(kgen_unit)
            CALL kgen_read_externs_model_flags(kgen_unit)
            CALL kgen_read_externs_stat_file_module(kgen_unit)
            CALL kgen_read_externs_error_code(kgen_unit)

            ! driver variables
            ! Not kernel driver input

            call clubb_tend_cam(kgen_unit, total_time)

            CLOSE (UNIT=kgen_unit)
        END DO

PRINT *, ""
print *, "                    Summary                                                   "
PRINT *, "******************************************************************************"
PRINT *, "advance_clubb_core : Total number of verification cases: 1"
PRINT *, "advance_clubb_core : Total time for all calls (usec): ", total_time
PRINT *, "******************************************************************************"
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
