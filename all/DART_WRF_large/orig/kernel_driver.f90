    !KGEN-generated Fortran source file 
      
    !Generated at : 2019-02-07 15:28:29 
    !KGEN version : 0.8.1 
      
    PROGRAM kernel_driver 
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck, kgen_rankthreadinvoke 
        USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
        USE assim_tools_mod, ONLY: filter_assim 
          
        USE assim_tools_mod, ONLY: kr_externs_in_assim_tools_mod 
        USE ensemble_manager_mod, ONLY: ensemble_type 
        USE ensemble_manager_mod, ONLY: kr_ensemble_manager_mod_ensemble_type 
        USE adaptive_inflate_mod, ONLY: adaptive_inflate_type 
        USE adaptive_inflate_mod, ONLY: kr_kgen_adaptive_inflate_mod_typesubp0 
        USE mpi_utilities_mod, ONLY: kr_externs_in_mpi_utilities_mod 
        USE utilities_mod, ONLY: kr_externs_in_utilities_mod 
        USE time_manager_mod, ONLY: kr_externs_in_time_manager_mod 
        USE cov_cutoff_mod, ONLY: kr_externs_in_cov_cutoff_mod 
        USE sampling_error_correction_mod, ONLY: kr_externs_in_sampling_error_correction_mod 
        USE reg_factor_mod, ONLY: kr_externs_in_reg_factor_mod 
        USE adaptive_inflate_mod, ONLY: kr_externs_in_adaptive_inflate_mod 
        IMPLICIT NONE 
          
#ifdef _MPI 
        include "mpif.h" 
#endif 
          
        LOGICAL :: kgen_isverified 
        INTEGER :: kgen_ierr_list, kgen_unit_list 
        INTEGER :: kgen_ierr, kgen_unit, kgen_case_count, kgen_count_verified 
        CHARACTER(LEN=1024) :: kgen_filepath 
        REAL(KIND=kgen_dp) :: kgen_measure, kgen_total_time, kgen_avg_time, kgen_avg_rate, kgen_min_time,kgen_max_time
        INTEGER :: kgen_measure_int, kgen_total_states
        REAL(KIND=8) :: kgen_array_sum 
        INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
        INTEGER :: myrank, mpisize 
        LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
        COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
          
        TYPE(ensemble_type) :: ens_handle 
        INTEGER :: ens_size 
        INTEGER :: num_groups 
        TYPE(adaptive_inflate_type) :: inflate 
        INTEGER :: ens_inf_copy 
        INTEGER :: ens_sd_copy 
        INTEGER :: ens_inf_sd_copy 
        LOGICAL :: inflate_only 
#ifdef _MPI 
        CALL MPI_INIT(kgen_ierr) 
        IF (kgen_ierr .NE. MPI_SUCCESS) THEN 
            PRINT *, "MPI Initialization is failed." 
            CALL MPI_ABORT(MPI_COMM_WORLD, -1, kgen_ierr) 
        END IF 
        call mpi_comm_rank(mpi_comm_world, myrank, kgen_ierr) 
        call mpi_comm_size(mpi_comm_world, mpisize, kgen_ierr) 
#else 
        myrank = 0 
        mpisize = 1 
#endif 
          
        kgen_total_time = 0.0_kgen_dp 
        kgen_total_states = 0
        kgen_min_time = HUGE(0.0_kgen_dp) 
        kgen_max_time = 0.0_kgen_dp 
        kgen_case_count = 0 
        kgen_count_verified = 0 
          
        kgen_unit_list = kgen_get_newunit() 
        !OPEN (UNIT=kgen_unit_list, FILE="kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list) 
        OPEN (UNIT=kgen_unit_list, FILE=TRIM(STATEFILE), STATUS="OLD", IOSTAT=kgen_ierr_list) 
        IF (kgen_ierr_list .NE. 0) THEN 
            CALL SYSTEM("ls -1 dart_wrf.*.*.* > kgen_statefile.lst") 
            CALL SLEEP(1) 
            kgen_unit_list = kgen_get_newunit() 
            OPEN (UNIT=kgen_unit_list, FILE="kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list) 
        END IF   
        IF (kgen_ierr_list .NE. 0) THEN 
            IF (myrank == 0) THEN 
                WRITE (*, *) "" 
                WRITE (*, *) "ERROR: ""kgen_statefile.lst"" is not found in current directory." 
            END IF   
            STOP 
        END IF   
        DO WHILE ( kgen_ierr_list .EQ. 0 ) 
            READ (UNIT = kgen_unit_list, FMT="(A)", IOSTAT=kgen_ierr_list) kgen_filepath 
            IF (kgen_ierr_list .EQ. 0) THEN 
                kgen_unit = kgen_get_newunit() 
                CALL kgen_rankthreadinvoke(TRIM(ADJUSTL(kgen_filepath)), kgen_mpirank, kgen_openmptid, kgen_kernelinvoke) 
                OPEN (UNIT=kgen_unit, FILE=TRIM(ADJUSTL(kgen_filepath)), STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", &
                &ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr) 
                IF (kgen_ierr == 0) THEN 
                    IF (myrank == 0) THEN 
                        WRITE (*, *) "" 
                        WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' &
                        &*****************" 
                    END IF   
                    kgen_evalstage = .TRUE. 
                    kgen_warmupstage = .FALSE. 
                    kgen_mainstage = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_ensemble_manager_mod_ensemble_type(ens_handle, kgen_unit, "ens_handle", .FALSE.) 
                    READ (UNIT = kgen_unit) ens_size 
                    READ (UNIT = kgen_unit) num_groups 
                    CALL kr_kgen_adaptive_inflate_mod_typesubp0(inflate, kgen_unit, "inflate", .FALSE.) 
                    READ (UNIT = kgen_unit) ens_inf_copy 
                    READ (UNIT = kgen_unit) ens_sd_copy 
                    READ (UNIT = kgen_unit) ens_inf_sd_copy 
                    READ (UNIT = kgen_unit) inflate_only 
                      
                    !extern input variables 
                    CALL kr_externs_in_assim_tools_mod(kgen_unit) 
                    CALL kr_externs_in_mpi_utilities_mod(kgen_unit) 
                    CALL kr_externs_in_utilities_mod(kgen_unit) 
                    CALL kr_externs_in_time_manager_mod(kgen_unit) 
                    CALL kr_externs_in_cov_cutoff_mod(kgen_unit) 
                    CALL kr_externs_in_sampling_error_correction_mod(kgen_unit) 
                    CALL kr_externs_in_reg_factor_mod(kgen_unit) 
                    CALL kr_externs_in_adaptive_inflate_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL filter_assim(kgen_unit, kgen_measure, kgen_measure_int, kgen_isverified, &
                    &kgen_filepath, ens_handle, ens_size, num_groups, &
                    &inflate, ens_inf_copy, ens_sd_copy, ens_inf_sd_copy, inflate_only) 
                    REWIND (UNIT=kgen_unit) 
                    kgen_evalstage = .FALSE. 
                    kgen_warmupstage = .TRUE. 
                    kgen_mainstage = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_ensemble_manager_mod_ensemble_type(ens_handle, kgen_unit, "ens_handle", .FALSE.) 
                    READ (UNIT = kgen_unit) ens_size 
                    READ (UNIT = kgen_unit) num_groups 
                    CALL kr_kgen_adaptive_inflate_mod_typesubp0(inflate, kgen_unit, "inflate", .FALSE.) 
                    READ (UNIT = kgen_unit) ens_inf_copy 
                    READ (UNIT = kgen_unit) ens_sd_copy 
                    READ (UNIT = kgen_unit) ens_inf_sd_copy 
                    READ (UNIT = kgen_unit) inflate_only 
                      
                    !extern input variables 
                    CALL kr_externs_in_assim_tools_mod(kgen_unit) 
                    CALL kr_externs_in_mpi_utilities_mod(kgen_unit) 
                    CALL kr_externs_in_utilities_mod(kgen_unit) 
                    CALL kr_externs_in_time_manager_mod(kgen_unit) 
                    CALL kr_externs_in_cov_cutoff_mod(kgen_unit) 
                    CALL kr_externs_in_sampling_error_correction_mod(kgen_unit) 
                    CALL kr_externs_in_reg_factor_mod(kgen_unit) 
                    CALL kr_externs_in_adaptive_inflate_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL filter_assim(kgen_unit, kgen_measure, kgen_measure_int, kgen_isverified, &
                    &kgen_filepath, ens_handle, ens_size, num_groups, &
                    &inflate, ens_inf_copy, ens_sd_copy, ens_inf_sd_copy, inflate_only) 
                    REWIND (UNIT=kgen_unit) 
                    kgen_evalstage = .FALSE. 
                    kgen_warmupstage = .FALSE. 
                    kgen_mainstage = .TRUE. 
                    kgen_case_count = kgen_case_count + 1 
                    kgen_isverified = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_ensemble_manager_mod_ensemble_type(ens_handle, kgen_unit, "ens_handle", .FALSE.) 
                    READ (UNIT = kgen_unit) ens_size 
                    READ (UNIT = kgen_unit) num_groups 
                    CALL kr_kgen_adaptive_inflate_mod_typesubp0(inflate, kgen_unit, "inflate", .FALSE.) 
                    READ (UNIT = kgen_unit) ens_inf_copy 
                    READ (UNIT = kgen_unit) ens_sd_copy 
                    READ (UNIT = kgen_unit) ens_inf_sd_copy 
                    READ (UNIT = kgen_unit) inflate_only 
                      
                    !extern input variables 
                    CALL kr_externs_in_assim_tools_mod(kgen_unit) 
                    CALL kr_externs_in_mpi_utilities_mod(kgen_unit) 
                    CALL kr_externs_in_utilities_mod(kgen_unit) 
                    CALL kr_externs_in_time_manager_mod(kgen_unit) 
                    CALL kr_externs_in_cov_cutoff_mod(kgen_unit) 
                    CALL kr_externs_in_sampling_error_correction_mod(kgen_unit) 
                    CALL kr_externs_in_reg_factor_mod(kgen_unit) 
                    CALL kr_externs_in_adaptive_inflate_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL filter_assim(kgen_unit, kgen_measure, kgen_measure_int, kgen_isverified, &
                    &kgen_filepath, ens_handle, ens_size, num_groups, &
                    &inflate, ens_inf_copy, ens_sd_copy, ens_inf_sd_copy, inflate_only) 
                    kgen_total_time = kgen_total_time + kgen_measure 
                    kgen_min_time = MIN( kgen_min_time, kgen_measure ) 
                    kgen_max_time = MAX( kgen_max_time, kgen_measure ) 
                    kgen_total_states = kgen_total_states + kgen_measure_int
                    IF (kgen_isverified) THEN 
                        kgen_count_verified = kgen_count_verified + 1 
                    END IF   
                END IF   
                CLOSE (UNIT=kgen_unit) 
            END IF   
        END DO   
          
        CLOSE (UNIT=kgen_unit_list) 
          
        IF (myrank == 0) THEN 
            WRITE (*, *) "" 
            WRITE (*, "(A)") "****************************************************" 
            WRITE (*, "(4X,A)") "kernel execution summary: dart_wrf" 
            WRITE (*, "(A)") "****************************************************" 
            kgen_avg_time = kgen_total_time 
            kgen_avg_rate = 1.0e6*real(mpisize,kind=kgen_dp)*real(kgen_total_states,kind=kgen_dp)/kgen_avg_time

            IF (kgen_case_count == 0) THEN 
                WRITE (*, *) "No data file is verified." 
            ELSE 
                WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_case_count 
                WRITE (*, "(4X, A36, A1, I6)") "Number of verification-passed cases ", ":", kgen_count_verified 
                WRITE (*, *) "" 
                if((real(kgen_count_verified,kind=kgen_dp)/real(kgen_case_count,kind=kgen_dp)) > 0.5_kgen_dp) then
                    WRITE (*, "(4X,A)") "kernel: dart_wrf: PASSED verification" 
                ELSE 
                    WRITE (*, "(4X,A)") "kernel: dart_wrf: FAILED verification" 
                END IF   
                WRITE (*, *) "" 
                WRITE (*, "(4X,A21,I3)")  "number of MPI ranks: ", mpisize 
                WRITE (*, "(4X,A12,A30)") "State file: ",STATEFILE
                WRITE (*, *) "" 
                WRITE (*, "(4X, A, E10.3)") "Average call time (usec): ", kgen_total_time / DBLE(kgen_case_count) 
                WRITE (*, "(4X, A, F12.2)") "Average state updates: (million/sec) : ", kgen_avg_rate/1.0e6

!                WRITE (*, "(4X, A, E10.3)") "Minimum call time (usec): ", kgen_min_time 
!                WRITE (*, "(4X, A, E10.3)") "Maximum call time (usec): ", kgen_max_time 
            END IF   
            WRITE (*, "(A)") "****************************************************" 
        END IF   
          
#ifdef _MPI 
        CALL mpi_finalize(kgen_ierr) 
#endif 
          
    END PROGRAM   
    BLOCK DATA KGEN 
        INTEGER :: kgen_mpirank = 0, kgen_openmptid = 0, kgen_kernelinvoke = 0 
        LOGICAL :: kgen_evalstage = .TRUE., kgen_warmupstage = .FALSE., kgen_mainstage = .FALSE. 
        COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    END BLOCK DATA KGEN 
