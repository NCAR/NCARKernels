    !KGEN-generated Fortran source file
    
    !Generated at : 2016-03-01 11:27:41
    !KGEN version : 0.6.2
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck
        USE mo_gas_phase_chemdr, ONLY: gas_phase_chemdr
        
        USE shr_kind_mod, ONLY: rkind_comp
        USE shr_kind_mod, ONLY: rkind_io

        USE mo_imp_sol, ONLY: kr_externs_in_mo_imp_sol
        USE chem_mods, ONLY: kr_externs_in_chem_mods
        USE mo_tracname, ONLY: kr_externs_in_mo_tracname
        USE ppgrid, only: nSystems

        use kgen_extensions_mod, only: read_runtime_options
#ifdef _MPI 
        use mpi
#endif
        IMPLICIT NONE
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(2) :: kgen_mpi_rank_at = (/ 0, 60 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        INTEGER :: kgen_case_count
        LOGICAL :: kgen_isverified
        INTEGER :: kgen_count_verified
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(2) :: kgen_counter_at = (/ 1, 48 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time, kgen_avg_time, kgen_max_time, kgen_avg_rate
        REAL(KIND=8) :: kgen_array_sum
        
        INTEGER :: lchnk
        INTEGER :: ncol
        REAL(KIND=rkind_comp) :: delt
        REAL(KIND=rkind_io)   :: delt_io

        REAL :: avg_call_time, max_call_time
        integer myrank, mpisize, ierror

#ifdef _MPI
        call MPI_INIT(ierror)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, mpisize, ierror)
        call MPI_COMM_RANK(MPI_COMM_WORLD, myrank, ierror)
#else
        myrank=0
        mpisize=1
#endif
        kgen_count_verified = 0

        call read_runtime_options

        kgen_total_time = 0.0_kgen_dp
        kgen_case_count = 0
        
        DO kgen_repeat_counter = 0, 3
            
            kgen_mpi_rank = kgen_mpi_rank_at(kgen_repeat_counter/2 + 1)
            WRITE (kgen_mpi_rank_conv, *) kgen_mpi_rank
            kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 2) + 1)
            WRITE (kgen_counter_conv, *) kgen_counter
            kgen_filepath = "../data/imp_sol." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))
            kgen_unit = kgen_get_newunit()
            
            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr)
            IF (kgen_ierr /= 0) THEN
                CALL kgen_error_stop("FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)))
            END IF 
            
            if(myrank==0)then 
            WRITE (*, *) ""
            WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' *****************"
            endif
            
            
            !driver read in arguments
            READ (UNIT = kgen_unit) lchnk
            READ (UNIT = kgen_unit) ncol
            READ (UNIT = kgen_unit) delt_io
            delt = REAL(delt_io,kind=rkind_comp)
            
            !extern input variables
            CALL kr_externs_in_mo_imp_sol(kgen_unit)
            CALL kr_externs_in_chem_mods(kgen_unit)
            CALL kr_externs_in_mo_tracname(kgen_unit)

             
            kgen_case_count = kgen_case_count + 1
            
            !callsite part
            CALL gas_phase_chemdr(kgen_unit, kgen_total_time, kgen_isverified, lchnk, ncol, delt, kgen_filepath,myrank)
            CLOSE (UNIT=kgen_unit)
            IF (kgen_isverified) THEN
               kgen_count_verified = kgen_count_verified + 1
            END IF
            ! print *,'kgen_total_time: ',kgen_total_time

            
        END DO 

        kgen_avg_time = kgen_total_time / real(kgen_case_count,kind=kgen_dp)
#ifdef _MPI
        call MPI_ALLREDUCE(kgen_avg_time, kgen_max_time, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, ierror)
#else
        kgen_max_time=kgen_avg_time         
#endif
        
        kgen_avg_rate = 1.0e6*real(mpisize,kind=kgen_dp)*real(nSystems,kind=kgen_dp)/kgen_max_time
        if (myrank == 0) then
           WRITE (*, *) ""
           WRITE (*, "(A)") "****************************************************"
           WRITE (*, "(4X,A)") "kernel execution summary: WACCM_imp_sol_vector"
           WRITE (*, "(A)") "****************************************************"
           WRITE (*, *) ""
           WRITE (*, "(4X,A,i2)") "Total number of verification cases: ",kgen_case_count
           WRITE (*, "(4X,A,i2)") "Number of verification-passed cases: ",kgen_count_verified
           WRITE (*, *) ""
           if(kgen_count_verified == kgen_case_count) then 
              WRITE (*,"(4X,A)") "kernel: WACCM_imp_sol_vector: PASSED verification"
           else
              WRITE (*,"(4X,A)") "kernel: WACCM_imp_sol_vector: FAILED verification"
           endif
           WRITE (*, "(4X,A26,I3)") "number of linear systems: ",nSystems
           WRITE (*, "(4X,A26,I3)") "number of mpi ranks: ",mpisize
           WRITE (*, *) ""
           WRITE (*, "(4X, A, E12.4)") "Average call time (usec): ", kgen_max_time
           WRITE (*, "(4X, A, F12.2)") "Average System solves per sec: ", kgen_avg_rate
           WRITE (*, *) "******************************************************************************"
        end if
#ifdef _MPI
        call MPI_FINALIZE(ierror)
#endif
        
    END PROGRAM 
