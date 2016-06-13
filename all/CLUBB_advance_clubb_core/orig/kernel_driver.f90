    !KGEN-generated Fortran source file
    
    !Generated at : 2016-01-07 12:08:48
    !KGEN version : 0.6.1
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck
        USE clubb_intr, ONLY: clubb_tend_cam
        
        USE clubb_intr, ONLY: kr_externs_in_clubb_intr
        USE grid_class, ONLY: kr_externs_in_grid_class
        USE parameters_model, ONLY: kr_externs_in_parameters_model
        USE stats_variables, ONLY: kr_externs_in_stats_variables
        USE error_code, ONLY: kr_externs_in_error_code
        USE stat_file_module, ONLY: kr_externs_in_stat_file_module
        USE model_flags, ONLY: kr_externs_in_model_flags
        USE variables_diagnostic_module, ONLY: kr_externs_in_variables_diagnostic_module
        USE parameters_tunable, ONLY: kr_externs_in_parameters_tunable
        USE saturation, ONLY: kr_externs_in_saturation
        USE array_index, ONLY: kr_externs_in_array_index
        USE sponge_layer_damping, ONLY: kr_externs_in_sponge_layer_damping
        IMPLICIT NONE
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(3) :: kgen_mpi_rank_at = (/ 100, 300, 500 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(2) :: kgen_counter_at = (/ 10, 50 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time
        REAL(KIND=8) :: kgen_array_sum
        
        kgen_total_time = 0.0_kgen_dp
        
        DO kgen_repeat_counter = 0, 8
            
            kgen_mpi_rank = kgen_mpi_rank_at(kgen_repeat_counter/3+ 1)
            WRITE (kgen_mpi_rank_conv, *) kgen_mpi_rank
            kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 2) + 1)
            WRITE (kgen_counter_conv, *) kgen_counter
            kgen_filepath = "../data/advance_clubb_core." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))
            kgen_unit = kgen_get_newunit()
            
            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr)
            IF (kgen_ierr /= 0) THEN
                CALL kgen_error_stop("FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)))
            END IF 
            
            WRITE (*, *) ""
            WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' *****************"
            
            
            !driver read in arguments
            
            !extern input variables
            CALL kr_externs_in_clubb_intr(kgen_unit)
            CALL kr_externs_in_grid_class(kgen_unit)
            CALL kr_externs_in_parameters_model(kgen_unit)
            CALL kr_externs_in_stats_variables(kgen_unit)
            CALL kr_externs_in_error_code(kgen_unit)
            CALL kr_externs_in_stat_file_module(kgen_unit)
            CALL kr_externs_in_model_flags(kgen_unit)
            CALL kr_externs_in_variables_diagnostic_module(kgen_unit)
            CALL kr_externs_in_parameters_tunable(kgen_unit)
            CALL kr_externs_in_saturation(kgen_unit)
            CALL kr_externs_in_array_index(kgen_unit)
            CALL kr_externs_in_sponge_layer_damping(kgen_unit)
            
            !callsite part
            CALL clubb_tend_cam(kgen_unit, kgen_total_time)
            CLOSE (UNIT=kgen_unit)
            
        END DO 
        
        WRITE (*, *) ""
        WRITE (*, *) "******************************************************************************"
        WRITE (*, *) "advance_clubb_core summary: Total number of verification cases: 9"
        WRITE (*, *) "advance_clubb_core summary: Average call time of all calls (usec): ", kgen_total_time / 9
        WRITE (*, *) "******************************************************************************"
    END PROGRAM 
