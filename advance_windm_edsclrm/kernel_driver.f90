
! KGEN-generated Fortran source file
!
! Filename    : kernel_driver.f90
! Generated at: 2015-10-21 08:59:09
! KGEN version: 0.5.3


PROGRAM kernel_driver
    USE advance_clubb_core_module, ONLY : advance_clubb_core
    USE clubb_precision, ONLY: core_rknd
    USE grid_class, ONLY: gr
    USE parameters_model, ONLY: edsclr_dim
    USE stats_variables, ONLY : kgen_read_externs_stats_variables
    USE sponge_layer_damping, ONLY : kgen_read_externs_sponge_layer_damping
    USE grid_class, ONLY : kgen_read_externs_grid_class
    USE model_flags, ONLY : kgen_read_externs_model_flags
    USE variables_diagnostic_module, ONLY : kgen_read_externs_variables_diagnostic_module
    USE parameters_model, ONLY : kgen_read_externs_parameters_model
    USE stat_file_module, ONLY : kgen_read_externs_stat_file_module
    USE error_code, ONLY : kgen_read_externs_error_code
    USE parameters_tunable, ONLY : kgen_read_externs_parameters_tunable
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
    REAL(KIND=core_rknd), allocatable :: wp2(:)
    REAL(KIND=core_rknd) :: dt
    REAL(KIND=core_rknd), allocatable :: vpwp(:)
    REAL(KIND=core_rknd), allocatable :: edsclrm_forcing(:,:)
    REAL(KIND=core_rknd), allocatable :: upwp(:)
    REAL(KIND=core_rknd), allocatable :: up2(:)
    REAL(KIND=core_rknd), allocatable :: edsclrm(:,:)
    REAL(KIND=core_rknd), allocatable :: invrs_rho_ds_zt(:)
    REAL(KIND=core_rknd), allocatable :: rho_ds_zm(:)
    REAL(KIND=core_rknd), allocatable :: wm_zt(:)
    LOGICAL :: l_implemented
    REAL(KIND=core_rknd), allocatable :: vp2(:)
    INTEGER :: err_code
    REAL(KIND=core_rknd), allocatable :: um_forcing(:)
    REAL(KIND=core_rknd), allocatable :: vm(:)
    REAL(KIND=core_rknd), allocatable :: vm_forcing(:)
    REAL(KIND=core_rknd) :: fcor
    REAL(KIND=core_rknd), allocatable :: um(:)

    total_time = 0.0_kgen_dp

    DO kgen_repeat_counter = 0, 0
        kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_counter_conv, * ) kgen_counter
        kgen_mpi_rank = kgen_mpi_rank_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank
        kgen_filepath = "./advance_windm_edsclrm." // trim(adjustl(kgen_counter_conv)) // "." // trim(adjustl(kgen_mpi_rank_conv))
        kgen_unit = kgen_get_newunit()
        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
        WRITE (*,*)
        IF ( kgen_ierr /= 0 ) THEN
            CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )
        END IF
        WRITE (*,*)
        WRITE (*,*) "** Verification against '" // trim(adjustl(kgen_filepath)) // "' **"

            CALL kgen_read_externs_stats_variables(kgen_unit)
            CALL kgen_read_externs_sponge_layer_damping(kgen_unit)
            CALL kgen_read_externs_grid_class(kgen_unit)
            CALL kgen_read_externs_model_flags(kgen_unit)
            CALL kgen_read_externs_variables_diagnostic_module(kgen_unit)
            CALL kgen_read_externs_parameters_model(kgen_unit)
            CALL kgen_read_externs_stat_file_module(kgen_unit)
            CALL kgen_read_externs_error_code(kgen_unit)
            CALL kgen_read_externs_parameters_tunable(kgen_unit)

            ! driver variables
            READ(UNIT=kgen_unit) dt
            CALL kgen_read_real_core_rknd_dim1(wm_zt, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(wp2, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(up2, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(vp2, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(um_forcing, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(vm_forcing, kgen_unit)
            CALL kgen_read_real_core_rknd_dim2(edsclrm_forcing, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(rho_ds_zm, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(invrs_rho_ds_zt, kgen_unit)
            READ(UNIT=kgen_unit) fcor
            READ(UNIT=kgen_unit) l_implemented
            CALL kgen_read_real_core_rknd_dim1(um, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(vm, kgen_unit)
            CALL kgen_read_real_core_rknd_dim2(edsclrm, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(upwp, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1(vpwp, kgen_unit)
            READ(UNIT=kgen_unit) err_code

            call advance_clubb_core(dt, wm_zt, wp2, up2, vp2, um_forcing, vm_forcing, edsclrm_forcing, rho_ds_zm, invrs_rho_ds_zt, fcor, l_implemented, um, vm, edsclrm, upwp, vpwp, err_code, kgen_unit, total_time)

            CLOSE (UNIT=kgen_unit)
        END DO

PRINT *, ""
PRINT *, "******************************************************************************"
PRINT *, "advance_windm_edsclrm summary: Total number of verification cases: 1"
PRINT *, "advance_windm_edsclrm summary: Total time of all calls (usec): ", total_time
PRINT *, "******************************************************************************"
    CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_core_rknd_dim1(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1
                INTEGER, DIMENSION(2,1) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim1

            SUBROUTINE kgen_read_real_core_rknd_dim2(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2
                INTEGER, DIMENSION(2,2) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim2

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
