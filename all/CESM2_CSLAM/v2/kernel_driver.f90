    !KGEN-generated Fortran source file 
      
    !Generated at : 2018-08-31 16:08:44 
    !KGEN version : 0.7.3 
      
    PROGRAM kernel_driver 
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck, kgen_rankthreadinvoke 
        USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
        USE fvm_consistent_se_cslam, ONLY: run_consistent_se_cslam 
          
        USE fvm_consistent_se_cslam, ONLY: kr_externs_in_fvm_consistent_se_cslam 
        USE element_mod, ONLY: element_t 
        USE element_mod, ONLY: kr_element_mod_element_t 
        USE fvm_control_volume_mod, ONLY: fvm_struct 
        USE fvm_control_volume_mod, ONLY: kr_fvm_control_volume_mod_fvm_struct 
        USE fvm_control_volume_mod, ONLY: kr_externs_in_fvm_control_volume_mod 
        USE dimensions_mod, ONLY: kr_externs_in_dimensions_mod 
        USE perf_mod, ONLY: kr_externs_in_perf_mod 
        USE shr_log_mod, ONLY: kr_externs_in_shr_log_mod 
        USE tprof_mod, ONLY: tprnt

        IMPLICIT NONE 
          
        LOGICAL :: kgen_isverified 
        INTEGER :: kgen_ierr_list, kgen_unit_list 
        INTEGER :: kgen_ierr, kgen_unit, kgen_case_count, kgen_count_verified 
        CHARACTER(LEN=1024) :: kgen_filepath 
        REAL(KIND=kgen_dp) :: kgen_measure, kgen_total_time, kgen_min_time, kgen_max_time 
        REAL(KIND=8) :: kgen_array_sum 
        INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
        LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
        COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
          
        TYPE(element_t), DIMENSION(:), ALLOCATABLE :: elem 
        TYPE(fvm_struct), DIMENSION(:), ALLOCATABLE :: fvm 
        INTEGER :: nets 
        INTEGER :: nete 
        kgen_total_time = 0.0_kgen_dp 
        kgen_min_time = HUGE(0.0_kgen_dp) 
        kgen_max_time = 0.0_kgen_dp 
        kgen_case_count = 0 
        kgen_count_verified = 0 
          
        kgen_unit_list = kgen_get_newunit() 
        OPEN (UNIT=kgen_unit_list, FILE="../data/kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list) 
        IF (kgen_ierr_list .NE. 0) THEN 
            CALL SYSTEM("ls -1 ../data/cslam.*.*.* > ../data/kgen_statefile.lst") 
            CALL SLEEP(1) 
            kgen_unit_list = kgen_get_newunit() 
            OPEN (UNIT=kgen_unit_list, FILE="../data/kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list) 
        END IF   
        IF (kgen_ierr_list .NE. 0) THEN 
            WRITE (*, *) "" 
            WRITE (*, *) "ERROR: ""kgen_statefile.lst"" is not found in current directory." 
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
                    WRITE (*, *) "" 
                    WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' &
                    &*****************" 
                    kgen_evalstage = .TRUE. 
                    kgen_warmupstage = .FALSE. 
                    kgen_mainstage = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_kgen_run_consistent_se_cslam_subp0(elem, kgen_unit, "elem", .FALSE.) 
                    CALL kr_kgen_run_consistent_se_cslam_subp1(fvm, kgen_unit, "fvm", .FALSE.) 
                    READ (UNIT = kgen_unit) nets 
                    READ (UNIT = kgen_unit) nete 
                      
                    !extern input variables 
                    CALL kr_externs_in_fvm_consistent_se_cslam(kgen_unit) 
                    CALL kr_externs_in_fvm_control_volume_mod(kgen_unit) 
                    CALL kr_externs_in_dimensions_mod(kgen_unit) 
                    CALL kr_externs_in_perf_mod(kgen_unit) 
                    CALL kr_externs_in_shr_log_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL run_consistent_se_cslam(kgen_unit, kgen_measure, kgen_isverified, elem, fvm, nets, nete) 
                    REWIND (UNIT=kgen_unit) 
                    kgen_evalstage = .FALSE. 
                    kgen_warmupstage = .TRUE. 
                    kgen_mainstage = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_kgen_run_consistent_se_cslam_subp0(elem, kgen_unit, "elem", .FALSE.) 
                    CALL kr_kgen_run_consistent_se_cslam_subp1(fvm, kgen_unit, "fvm", .FALSE.) 
                    READ (UNIT = kgen_unit) nets 
                    READ (UNIT = kgen_unit) nete 
                      
                    !extern input variables 
                    CALL kr_externs_in_fvm_consistent_se_cslam(kgen_unit) 
                    CALL kr_externs_in_fvm_control_volume_mod(kgen_unit) 
                    CALL kr_externs_in_dimensions_mod(kgen_unit) 
                    CALL kr_externs_in_perf_mod(kgen_unit) 
                    CALL kr_externs_in_shr_log_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL run_consistent_se_cslam(kgen_unit, kgen_measure, kgen_isverified, elem, fvm, nets, nete) 
                    REWIND (UNIT=kgen_unit) 
                    kgen_evalstage = .FALSE. 
                    kgen_warmupstage = .FALSE. 
                    kgen_mainstage = .TRUE. 
                    kgen_case_count = kgen_case_count + 1 
                    kgen_isverified = .FALSE. 
                      
                      
                    !driver read in arguments 
                    CALL kr_kgen_run_consistent_se_cslam_subp0(elem, kgen_unit, "elem", .FALSE.) 
                    CALL kr_kgen_run_consistent_se_cslam_subp1(fvm, kgen_unit, "fvm", .FALSE.) 
                    READ (UNIT = kgen_unit) nets 
                    READ (UNIT = kgen_unit) nete 
                      
                    !extern input variables 
                    CALL kr_externs_in_fvm_consistent_se_cslam(kgen_unit) 
                    CALL kr_externs_in_fvm_control_volume_mod(kgen_unit) 
                    CALL kr_externs_in_dimensions_mod(kgen_unit) 
                    CALL kr_externs_in_perf_mod(kgen_unit) 
                    CALL kr_externs_in_shr_log_mod(kgen_unit) 
                      
                    !callsite part 
                    CALL run_consistent_se_cslam(kgen_unit, kgen_measure, kgen_isverified, elem, fvm, nets, nete) 
                    kgen_total_time = kgen_total_time + kgen_measure 
                    kgen_min_time = MIN( kgen_min_time, kgen_measure ) 
                    kgen_max_time = MAX( kgen_max_time, kgen_measure ) 
                    IF (kgen_isverified) THEN 
                        kgen_count_verified = kgen_count_verified + 1 
                    END IF   
                END IF   
                CLOSE (UNIT=kgen_unit) 
            END IF   
        END DO   
          
        CLOSE (UNIT=kgen_unit_list) 
          
        call tprnt

        WRITE (*, *) "" 
        WRITE (*, "(A)") "****************************************************" 
        WRITE (*, "(4X,A)") "kernel execution summary: cslam" 
        WRITE (*, "(A)") "****************************************************" 
        IF (kgen_case_count == 0) THEN 
            WRITE (*, *) "No data file is verified." 
        ELSE 
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_case_count 
            WRITE (*, "(4X, A36, A1, I6)") "Number of verification-passed cases ", ":", kgen_count_verified 
            WRITE (*, *) "" 
            WRITE (*, "(4X, A, E10.3)") "Average call time (usec): ", kgen_total_time / DBLE(kgen_case_count) 
            WRITE (*, "(4X, A, E10.3)") "Minimum call time (usec): ", kgen_min_time 
            WRITE (*, "(4X, A, E10.3)") "Maximum call time (usec): ", kgen_max_time 
        END IF   
        WRITE (*, "(A)") "****************************************************" 
          
        CONTAINS 
          
        !read state subroutine for kr_kgen_run_consistent_se_cslam_subp0 
        SUBROUTINE kr_kgen_run_consistent_se_cslam_subp0(var, kgen_unit, printname, printvar) 
            TYPE(element_t), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
            INTEGER, INTENT(IN) :: kgen_unit 
            CHARACTER(LEN=*), INTENT(IN) :: printname 
            LOGICAL, INTENT(IN), OPTIONAL :: printvar 
            LOGICAL :: kgen_istrue 
            REAL(KIND=8) :: kgen_array_sum 
            INTEGER :: idx1 
            INTEGER, DIMENSION(2,1) :: kgen_bound 
              
            READ (UNIT = kgen_unit) kgen_istrue 
            IF (kgen_istrue) THEN 
                IF (ALLOCATED( var )) THEN 
                    DEALLOCATE (var) 
                END IF   
                READ (UNIT = kgen_unit) kgen_bound(1, 1) 
                READ (UNIT = kgen_unit) kgen_bound(2, 1) 
                ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
                DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
                    IF (PRESENT( printvar ) .AND. printvar) THEN 
                        CALL kr_element_mod_element_t(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
                    ELSE 
                        CALL kr_element_mod_element_t(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
                    END IF   
                END DO   
            END IF   
        END SUBROUTINE kr_kgen_run_consistent_se_cslam_subp0 
          
        !read state subroutine for kr_kgen_run_consistent_se_cslam_subp1 
        SUBROUTINE kr_kgen_run_consistent_se_cslam_subp1(var, kgen_unit, printname, printvar) 
            TYPE(fvm_struct), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
            INTEGER, INTENT(IN) :: kgen_unit 
            CHARACTER(LEN=*), INTENT(IN) :: printname 
            LOGICAL, INTENT(IN), OPTIONAL :: printvar 
            LOGICAL :: kgen_istrue 
            REAL(KIND=8) :: kgen_array_sum 
            INTEGER :: idx1 
            INTEGER, DIMENSION(2,1) :: kgen_bound 
              
            READ (UNIT = kgen_unit) kgen_istrue 
            IF (kgen_istrue) THEN 
                IF (ALLOCATED( var )) THEN 
                    DEALLOCATE (var) 
                END IF   
                READ (UNIT = kgen_unit) kgen_bound(1, 1) 
                READ (UNIT = kgen_unit) kgen_bound(2, 1) 
                ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
                DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
                    IF (PRESENT( printvar ) .AND. printvar) THEN 
                        CALL kr_fvm_control_volume_mod_fvm_struct(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
                    ELSE 
                        CALL kr_fvm_control_volume_mod_fvm_struct(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
                    END IF   
                END DO   
            END IF   
        END SUBROUTINE kr_kgen_run_consistent_se_cslam_subp1 
          
    END PROGRAM   
    BLOCK DATA KGEN 
        INTEGER :: kgen_mpirank = 0, kgen_openmptid = 0, kgen_kernelinvoke = 0 
        LOGICAL :: kgen_evalstage = .TRUE., kgen_warmupstage = .FALSE., kgen_mainstage = .FALSE. 
        COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    END BLOCK DATA KGEN 
