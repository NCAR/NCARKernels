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
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_NULL_CHAR, &
           C_PTR, C_LOC
        IMPLICIT NONE

!        include 'f90papi.h'
        
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

        !! FOR PAPI
!        integer(kind=4) :: ierr
!        integer(kind=4), dimension(4) :: events
!        integer(kind=8), dimension(4) :: values
!        integer(kind=4) :: numevents = 4

!        events(1) = PAPI_L1_TCM
!        events(2) = PAPI_L2_TCM
!        events(3) = PAPI_L3_TCM
!        events(4) = PAPI_TOT_INS

!        call PAPIF_start_counters( events, numevents, ierr ); 
!        write(*,*) 'ERROR: ',ierr

        !! For extrae
!        INTEGER*8, PARAMETER, DIMENSION(2) :: values = (/ 0, 1 /)
!        CHARACTER(KIND=C_CHAR,LEN=6), DIMENSION(2), TARGET :: & 
!           description_values
!        TYPE(C_PTR), DIMENSION(2) :: description_values_ptrs
!         CHARACTER(KIND=C_CHAR,LEN=20) :: evt_desc = &
!           "Kernel execution" // C_NULL_CHAR
!
!         description_values(1) = "End  " // C_NULL_CHAR
!         description_values_ptrs(1) = C_LOC(description_values(1))
!         description_values(2) = "Begin" // C_NULL_CHAR
!         description_values_ptrs(2) = C_LOC(description_values(2))
!
!        CALL extrae_define_event_type (1000, evt_desc, 2, values, &
!           description_values_ptrs)
!
!        CALL extrae_event(1000, 1_8)
   
        kgen_total_time = 0.0_kgen_dp
        
!        DO kgen_repeat_counter = 0, 3
            
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
            
            !call PAPIF_read_counters( values, numevents, ierr )
!            call PAPIF_stop_counters( values, numevents, ierr )
!            write(*,*) 'ERROR: ',ierr
!            WRITE(*,*) 'Total L1 Misses: ',values(1)
!            WRITE(*,*) 'Total L2 Misses: ',values(2)
!            WRITE(*,*) 'Total L3 Misses: ',values(3)
!            WRITE(*,*) 'Total number of instructions: ',values(4)
!            WRITE(*,*) '---------------------------'
!            WRITE(*,*) 'L1 Misses',(real(values(1))/real(values(4)))*100.0
!            WRITE(*,*) 'L2 Misses',(real(values(2))/real(values(4)))*100.0
!            WRITE(*,*) 'L3 Misses',(real(values(3))/real(values(4)))*100.0

!        END DO

!        CALL extrae_event(1000, 0_8)       

        WRITE (*, *) ""
        WRITE (*, *) "******************************************************************************"
        WRITE (*, *) "imp_sol summary: Total number of verification cases: 4"
        WRITE (*, *) "imp_sol summary: Average call time of all calls (usec): ", kgen_total_time / 4
        WRITE (*, *) "******************************************************************************"
    END PROGRAM 
