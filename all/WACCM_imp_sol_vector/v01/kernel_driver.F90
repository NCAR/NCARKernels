    !KGEN-generated Fortran source file
    
    !Generated at : 2016-03-01 11:27:41
    !KGEN version : 0.6.2
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck
        USE mo_gas_phase_chemdr, ONLY: gas_phase_chemdr
        
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        USE mo_imp_sol, ONLY: kr_externs_in_mo_imp_sol
        USE chem_mods, ONLY: kr_externs_in_chem_mods
        USE mo_tracname, ONLY: kr_externs_in_mo_tracname
        IMPLICIT NONE
#ifdef _MPI 
        include 'mpif.h'
#endif
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(2) :: kgen_mpi_rank_at = (/ 0, 60 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(2) :: kgen_counter_at = (/ 1, 48 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time
        REAL(KIND=8) :: kgen_array_sum
        
        INTEGER :: lchnk
        INTEGER :: ncol
        REAL(KIND=r8) :: delt

#ifdef _MPI
        integer rank, size, ierror
        REAL :: avg_call_time, max_call_time

        call MPI_INIT(ierror)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
#endif

        kgen_total_time = 0.0_kgen_dp
        
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
            
            WRITE (*, *) ""
            WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' *****************"
            
            
            !driver read in arguments
            READ (UNIT = kgen_unit) lchnk
            READ (UNIT = kgen_unit) ncol
            READ (UNIT = kgen_unit) delt
            
            !extern input variables
            CALL kr_externs_in_mo_imp_sol(kgen_unit)
            CALL kr_externs_in_chem_mods(kgen_unit)
            CALL kr_externs_in_mo_tracname(kgen_unit)
            
            !callsite part
            CALL gas_phase_chemdr(kgen_unit, kgen_total_time, lchnk, ncol, delt)
            CLOSE (UNIT=kgen_unit)
            
        END DO 

        avg_call_time = kgen_total_time / (kgen_repeat_counter)
#ifdef _MPI
        call MPI_REDUCE(avg_call_time, max_call_time, 1, MPI_REAL, MPI_MAX, 0, MPI_COMM_WORLD, ierror)
        if (rank == 0) then
           WRITE (*, *) ""
           WRITE (*, *) "******************************************************************************"
           WRITE (*, *) "imp_sol summary: Total number of verification cases: ",kgen_repeat_counter
           WRITE (*, *) "imp_sol summary: # ranks: ",size," Avg call time (usec): ",avg_call_time
           WRITE (*, *) "******************************************************************************"
        end if
        call MPI_FINALIZE(ierror)
#else
        max_call_time = avg_call_time
        WRITE (*, *) ""
        WRITE (*, "(A)") "****************************************************"
        WRITE (*, "(4X,A)") "kernel execution summary: imp_sol"
        WRITE (*, "(A)") "****************************************************"
        IF (kgen_repeat_counter == 0) THEN
            WRITE (*, *) "No data file is verified."
        ELSE
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_repeat_counter
            WRITE (*, *) ""
            WRITE (*, "(4X, A, E10.3)") "Average call time (usec): ", max_call_time
        END IF
        WRITE (*, "(A)") "****************************************************"

#endif
        



    END PROGRAM 
