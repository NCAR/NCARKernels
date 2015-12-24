    !KGEN-generated Fortran source file
    
    !Generated at : 2015-12-24 11:55:35
    !KGEN version : 0.6.0
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp
        USE mpas_atmphys_driver_radiation_lw, ONLY: driver_radiation_lw
        
        IMPLICIT NONE
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(3) :: kgen_mpi_rank_at = (/ 1, 4, 8 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(3) :: kgen_counter_at = (/ 1, 10, 5 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time
        
        kgen_total_time = 0.0_kgen_dp
        
        DO kgen_repeat_counter = 0, 8
            
            kgen_mpi_rank = kgen_mpi_rank_at(kgen_repeat_counter/3 + 1)
            WRITE (kgen_mpi_rank_conv, *) kgen_mpi_rank
            kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 3) + 1)
            WRITE (kgen_counter_conv, *) kgen_counter
            kgen_filepath = "../data/rrtmg_lwrad." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))
            kgen_unit = kgen_get_newunit()
            
            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr)
            IF (kgen_ierr /= 0) THEN
                CALL kgen_error_stop("FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)))
            END IF 
            
            WRITE (*, *) ""
            WRITE (*, *) "** Verification against '" // trim(adjustl(kgen_filepath)) // "' **"
            
            
            !driver read in arguments
            
            !callsite part
            CALL driver_radiation_lw(kgen_unit, kgen_total_time)
            CLOSE (UNIT=kgen_unit)
            
        END DO 
        
        WRITE (*, *) ""
        WRITE (*, *) "******************************************************************************"
        WRITE (*, *) "rrtmg_lwrad summary: Total number of verification cases: 9"
        WRITE (*, *) "rrtmg_lwrad summary: Average call time of all calls (usec): ", kgen_total_time / 9
        WRITE (*, *) "******************************************************************************"
    END PROGRAM 
