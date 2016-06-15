    !KGEN-generated Fortran source file 
      
    !Generated at : 2016-06-15 08:50:04 
    !KGEN version : 0.7.0 
      
    PROGRAM kernel_driver 
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck 
        USE clubb_intr, ONLY: clubb_tend_cam 
          
        USE clubb_intr, ONLY: kr_externs_in_clubb_intr 
        USE camsrfexch, ONLY: cam_in_t 
        USE camsrfexch, ONLY: kr_camsrfexch_cam_in_t 
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8 
        USE physconst, ONLY: kr_externs_in_physconst 
        USE scammod, ONLY: kr_externs_in_scammod 
        USE stats_variables, ONLY: kr_externs_in_stats_variables 
        USE parameters_tunable, ONLY: kr_externs_in_parameters_tunable 
        USE grid_class, ONLY: kr_externs_in_grid_class 
        USE error_code, ONLY: kr_externs_in_error_code 
        USE model_flags, ONLY: kr_externs_in_model_flags 
        USE parameters_model, ONLY: kr_externs_in_parameters_model 
        USE stat_file_module, ONLY: kr_externs_in_stat_file_module 
        USE variables_diagnostic_module, ONLY: kr_externs_in_variables_diagnostic_module 
        USE saturation, ONLY: kr_externs_in_saturation 
        USE array_index, ONLY: kr_externs_in_array_index 
        USE sponge_layer_damping, ONLY: kr_externs_in_sponge_layer_damping 
        USE shr_log_mod, ONLY: kr_externs_in_shr_log_mod 
        IMPLICIT NONE 
          
        LOGICAL :: kgen_isverified 
        INTEGER :: kgen_ierr_list, kgen_unit_list 
        INTEGER :: kgen_ierr, kgen_unit, kgen_count, kgen_count_verified 
        CHARACTER(LEN=1024) :: kgen_filepath 
        REAL(KIND=kgen_dp) :: kgen_elapsed_time, kgen_total_time, kgen_min_time, kgen_max_time 
        REAL(KIND=8) :: kgen_array_sum 
          
        TYPE(cam_in_t) :: cam_in 
        REAL(KIND=r8) :: hdtime 
        INTEGER :: cld_macmic_num_steps 
        INTEGER :: macmic_it 
        kgen_total_time = 0.0_kgen_dp 
        kgen_min_time = HUGE(0.0_kgen_dp) 
        kgen_max_time = 0.0_kgen_dp 
        kgen_count = 0 
        kgen_count_verified = 0 
          
        kgen_unit_list = kgen_get_newunit() 
        OPEN (UNIT=kgen_unit_list, FILE="kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list) 
        IF (kgen_ierr_list /= 0) THEN 
            CALL SYSTEM("ls -1 clubb.*.*.* > kgen_statefile.lst") 
            CALL SLEEP(1) 
            kgen_unit_list = kgen_get_newunit() 
            OPEN (UNIT=kgen_unit_list, FILE="kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list) 
        END IF   
        IF (kgen_ierr_list /= 0) THEN 
            WRITE (*, *) "" 
            WRITE (*, *) "ERROR: ""kgen_statefile.lst"" is not found in current directory." 
            STOP 
        END IF   
        DO WHILE ( kgen_ierr_list == 0 ) 
            READ (UNIT = kgen_unit_list, FMT="(A)", IOSTAT=kgen_ierr_list) kgen_filepath 
            IF (kgen_ierr_list == 0) THEN 
                kgen_unit = kgen_get_newunit() 
                OPEN (UNIT=kgen_unit, FILE=TRIM(ADJUSTL(kgen_filepath)), STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", &
                &ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr) 
                IF (kgen_ierr == 0) THEN 
                    WRITE (*, *) "" 
                    WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' &
                    &*****************" 
                    kgen_count = kgen_count + 1 
                    kgen_isverified = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_camsrfexch_cam_in_t(cam_in, kgen_unit) 
                    READ (UNIT = kgen_unit) hdtime 
                    READ (UNIT = kgen_unit) cld_macmic_num_steps 
                    READ (UNIT = kgen_unit) macmic_it 
                      
                    !extern input variables 
                    CALL kr_externs_in_clubb_intr(kgen_unit) 
                    CALL kr_externs_in_physconst(kgen_unit) 
                    CALL kr_externs_in_scammod(kgen_unit) 
                    CALL kr_externs_in_stats_variables(kgen_unit) 
                    CALL kr_externs_in_parameters_tunable(kgen_unit) 
                    CALL kr_externs_in_grid_class(kgen_unit) 
                    CALL kr_externs_in_error_code(kgen_unit) 
                    CALL kr_externs_in_model_flags(kgen_unit) 
                    CALL kr_externs_in_parameters_model(kgen_unit) 
                    CALL kr_externs_in_stat_file_module(kgen_unit) 
                    CALL kr_externs_in_variables_diagnostic_module(kgen_unit) 
                    CALL kr_externs_in_saturation(kgen_unit) 
                    CALL kr_externs_in_array_index(kgen_unit) 
                    CALL kr_externs_in_sponge_layer_damping(kgen_unit) 
                    CALL kr_externs_in_shr_log_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL clubb_tend_cam(kgen_unit, kgen_elapsed_time, kgen_isverified, cam_in, hdtime, cld_macmic_num_steps, &
                    &macmic_it) 
                    kgen_total_time = kgen_total_time + kgen_elapsed_time 
                    kgen_min_time = MIN( kgen_min_time, kgen_elapsed_time ) 
                    kgen_max_time = MAX( kgen_max_time, kgen_elapsed_time ) 
                    IF (kgen_isverified) THEN 
                        kgen_count_verified = kgen_count_verified + 1 
                    END IF   
                    CLOSE (UNIT=kgen_unit) 
                END IF   
            END IF   
        END DO   
          
        CLOSE (UNIT=kgen_unit_list) 
          
        WRITE (*, *) "" 
        WRITE (*, "(A)") "****************************************************" 
        WRITE (*, "(4X,A)") "kernel execution summary: clubb" 
        WRITE (*, "(A)") "****************************************************" 
        IF (kgen_count == 0) THEN 
            WRITE (*, *) "No data file is verified." 
        ELSE 
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_count 
            WRITE (*, "(4X, A36, A1, I6)") "Number of verification-passed cases ", ":", kgen_count_verified 
            WRITE (*, *) "" 
            WRITE (*, "(4X, A, E10.3)") "Average call time (usec): ", kgen_total_time / REAL(kgen_count) 
            WRITE (*, "(4X, A, E10.3)") "Minimum call time (usec): ", kgen_min_time 
            WRITE (*, "(4X, A, E10.3)") "Maximum call time (usec): ", kgen_max_time 
        END IF   
        WRITE (*, "(A)") "****************************************************" 
    END PROGRAM   