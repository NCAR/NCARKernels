    !KGEN-generated Fortran source file
    
    !Generated at : 2016-01-07 12:54:30
    !KGEN version : 0.6.1
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck
        USE advance_clubb_core_module, ONLY: advance_clubb_core
        
        USE clubb_precision, ONLY: core_rknd
        USE grid_class, ONLY: gr
        USE parameters_model, ONLY: edsclr_dim
        USE grid_class, ONLY: kr_externs_in_grid_class
        USE parameters_model, ONLY: kr_externs_in_parameters_model
        USE error_code, ONLY: kr_externs_in_error_code
        USE stats_variables, ONLY: kr_externs_in_stats_variables
        USE stat_file_module, ONLY: kr_externs_in_stat_file_module
        USE parameters_tunable, ONLY: kr_externs_in_parameters_tunable
        USE model_flags, ONLY: kr_externs_in_model_flags
        USE sponge_layer_damping, ONLY: kr_externs_in_sponge_layer_damping
        USE variables_diagnostic_module, ONLY: kr_externs_in_variables_diagnostic_module
        IMPLICIT NONE
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(4) :: kgen_mpi_rank_at = (/ 0, 100, 200, 300 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(1) :: kgen_counter_at = (/ 10 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time
        REAL(KIND=8) :: kgen_array_sum
        
        LOGICAL :: l_implemented
        REAL(KIND=core_rknd) :: dt
        REAL(KIND=core_rknd) :: fcor
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wm_zt
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: um_forcing
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: vm_forcing
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: rho_ds_zm
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: invrs_rho_ds_zt
        REAL(KIND=core_rknd), DIMENSION(:,:), ALLOCATABLE :: edsclrm_forcing
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wp2
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: up2
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: vp2
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: um
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: vm
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: upwp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: vpwp
        REAL(KIND=core_rknd), DIMENSION(:,:), ALLOCATABLE :: edsclrm
        INTEGER :: err_code
        kgen_total_time = 0.0_kgen_dp
        
        DO kgen_repeat_counter = 0, 11
            
            kgen_mpi_rank = kgen_mpi_rank_at(kgen_repeat_counter/3 + 1)
            WRITE (kgen_mpi_rank_conv, *) kgen_mpi_rank
            kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 1) + 1)
            WRITE (kgen_counter_conv, *) kgen_counter
            kgen_filepath = "../data/advance_windm_edsclrm." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))
            kgen_unit = kgen_get_newunit()
            
            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr)
            IF (kgen_ierr /= 0) THEN
                CALL kgen_error_stop("FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)))
            END IF 
            
            WRITE (*, *) ""
            WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' *****************"
            
            
            !driver read in arguments
            READ (UNIT = kgen_unit) l_implemented
            READ (UNIT = kgen_unit) dt
            READ (UNIT = kgen_unit) fcor
            CALL kr_advance_clubb_core_real__core_rknd_dim1(wm_zt, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(um_forcing, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(vm_forcing, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(rho_ds_zm, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(invrs_rho_ds_zt, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim2(edsclrm_forcing, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(wp2, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(up2, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(vp2, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(um, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(vm, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(upwp, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(vpwp, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim2(edsclrm, kgen_unit)
            READ (UNIT = kgen_unit) err_code
            
            !extern input variables
            CALL kr_externs_in_grid_class(kgen_unit)
            CALL kr_externs_in_parameters_model(kgen_unit)
            CALL kr_externs_in_error_code(kgen_unit)
            CALL kr_externs_in_stats_variables(kgen_unit)
            CALL kr_externs_in_stat_file_module(kgen_unit)
            CALL kr_externs_in_parameters_tunable(kgen_unit)
            CALL kr_externs_in_model_flags(kgen_unit)
            CALL kr_externs_in_sponge_layer_damping(kgen_unit)
            CALL kr_externs_in_variables_diagnostic_module(kgen_unit)
            
            !callsite part
            CALL advance_clubb_core(kgen_unit, kgen_total_time, l_implemented, dt, fcor, wm_zt, um_forcing, vm_forcing, rho_ds_zm, invrs_rho_ds_zt, edsclrm_forcing, wp2, up2, vp2, um, vm, upwp, vpwp, edsclrm, err_code)
            CLOSE (UNIT=kgen_unit)
            
        END DO 
       
        WRITE (*, *) ""
        WRITE (*, "(A)") "****************************************************"
        WRITE (*, "(4X,A)") "kernel execution summary: advance_windm_edsclrm"
        WRITE (*, "(A)") "****************************************************"
        IF (kgen_repeat_counter == 0) THEN
            WRITE (*, *) "No data file is verified."
        ELSE
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", 12
            WRITE (*, *) ""
            WRITE (*, "(4X, A, E10.3)") "Average call time (usec): ", kgen_total_time / REAL(12)
        END IF
        WRITE (*, "(A)") "****************************************************"

        CONTAINS
        
        !read state subroutine for kr_advance_clubb_core_real__core_rknd_dim1
        SUBROUTINE kr_advance_clubb_core_real__core_rknd_dim1(var, kgen_unit, printvar)
            REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
            LOGICAL :: kgen_istrue
            REAL(KIND=8) :: kgen_array_sum
            INTEGER :: idx1
            INTEGER, DIMENSION(2,1) :: kgen_bound
            
            READ (UNIT = kgen_unit) kgen_istrue
            IF (kgen_istrue) THEN
                IF (ALLOCATED( var )) THEN
                    DEALLOCATE (var)
                END IF 
                READ (UNIT = kgen_unit) kgen_array_sum
                READ (UNIT = kgen_unit) kgen_bound(1, 1)
                READ (UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
                READ (UNIT = kgen_unit) var
                CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
                IF (PRESENT( printvar )) THEN
                    WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var
                END IF 
            END IF 
        END SUBROUTINE kr_advance_clubb_core_real__core_rknd_dim1
        
        !read state subroutine for kr_advance_clubb_core_real__core_rknd_dim2
        SUBROUTINE kr_advance_clubb_core_real__core_rknd_dim2(var, kgen_unit, printvar)
            REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
            LOGICAL :: kgen_istrue
            REAL(KIND=8) :: kgen_array_sum
            INTEGER :: idx1, idx2
            INTEGER, DIMENSION(2,2) :: kgen_bound
            
            READ (UNIT = kgen_unit) kgen_istrue
            IF (kgen_istrue) THEN
                IF (ALLOCATED( var )) THEN
                    DEALLOCATE (var)
                END IF 
                READ (UNIT = kgen_unit) kgen_array_sum
                READ (UNIT = kgen_unit) kgen_bound(1, 1)
                READ (UNIT = kgen_unit) kgen_bound(2, 1)
                READ (UNIT = kgen_unit) kgen_bound(1, 2)
                READ (UNIT = kgen_unit) kgen_bound(2, 2)
                ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1))
                READ (UNIT = kgen_unit) var
                CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
                IF (PRESENT( printvar )) THEN
                    WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var
                END IF 
            END IF 
        END SUBROUTINE kr_advance_clubb_core_real__core_rknd_dim2
        
    END PROGRAM 
