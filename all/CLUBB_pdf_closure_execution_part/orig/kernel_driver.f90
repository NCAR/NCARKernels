    !KGEN-generated Fortran source file
    
    !Generated at : 2016-01-07 08:45:17
    !KGEN version : 0.6.1
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp
        USE pdf_closure_module, ONLY: pdf_closure
        
        USE clubb_precision, ONLY: core_rknd
        USE parameters_model, ONLY: sclr_dim
        USE pdf_parameter_module, ONLY: pdf_parameter
        USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter
        IMPLICIT NONE
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(4) :: kgen_mpi_rank_at = (/ 0, 100, 200, 300 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(3) :: kgen_counter_at = (/ 10, 100, 500 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time
        
        INTEGER :: hydromet_dim
        REAL(KIND=core_rknd) :: wp2
        REAL(KIND=core_rknd) :: wm
        REAL(KIND=core_rknd) :: rtm
        REAL(KIND=core_rknd) :: thlm
        REAL(KIND=core_rknd) :: skw
        REAL(KIND=core_rknd) :: sigma_sqd_w
        REAL(KIND=core_rknd) :: thlp2
        REAL(KIND=core_rknd) :: wpthlp
        REAL(KIND=core_rknd) :: rtp2
        REAL(KIND=core_rknd) :: wprtp
        REAL(KIND=core_rknd) :: rtpthlp
        REAL(KIND=core_rknd) :: exner
        REAL(KIND=core_rknd) :: p_in_pa
        REAL(KIND=core_rknd) :: thv_ds
        REAL(KIND=core_rknd) :: wp3
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: sclrm
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: sclrp2
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wpsclrp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: sclrpthlp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: sclrprtp
        INTEGER :: level
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wp2hmp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wphydrometp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: thlphmp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: rtphmp
        REAL(KIND=core_rknd) :: wp2thlp
        REAL(KIND=core_rknd) :: wp2rtp
        REAL(KIND=core_rknd) :: wp2rcp
        REAL(KIND=core_rknd) :: wprcp
        REAL(KIND=core_rknd) :: thlprcp
        REAL(KIND=core_rknd) :: rtprcp
        REAL(KIND=core_rknd) :: rcm
        REAL(KIND=core_rknd) :: rcp2
        REAL(KIND=core_rknd) :: wp4
        REAL(KIND=core_rknd) :: wprtp2
        REAL(KIND=core_rknd) :: wpthlp2
        REAL(KIND=core_rknd) :: cloud_frac
        REAL(KIND=core_rknd) :: wpthvp
        REAL(KIND=core_rknd) :: wp2thvp
        REAL(KIND=core_rknd) :: rtpthvp
        REAL(KIND=core_rknd) :: thlpthvp
        REAL(KIND=core_rknd) :: wprtpthlp
        REAL(KIND=core_rknd) :: ice_supersat_frac
        TYPE(pdf_parameter) :: pdf_params
        INTEGER :: err_code
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: sclrprcp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: sclrpthvp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wpsclrp2
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wpsclrprtp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wpsclrpthlp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wp2sclrp
        REAL(KIND=core_rknd) :: rc_coef
        kgen_total_time = 0.0_kgen_dp
        
        DO kgen_repeat_counter = 0, 11
            
            kgen_mpi_rank = kgen_mpi_rank_at(kgen_repeat_counter/3 + 1)
            WRITE (kgen_mpi_rank_conv, *) kgen_mpi_rank
            kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 3) + 1)
            WRITE (kgen_counter_conv, *) kgen_counter
            kgen_filepath = "../data/pdf_closure_part3." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))
            kgen_unit = kgen_get_newunit()
            
            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr)
            IF (kgen_ierr /= 0) THEN
                CALL kgen_error_stop("FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)))
            END IF 
            
            WRITE (*, *) ""
            WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' *****************"
            
            
            !driver read in arguments
            READ (UNIT = kgen_unit) hydromet_dim
            READ (UNIT = kgen_unit) wp2
            READ (UNIT = kgen_unit) wm
            READ (UNIT = kgen_unit) rtm
            READ (UNIT = kgen_unit) thlm
            READ (UNIT = kgen_unit) skw
            READ (UNIT = kgen_unit) sigma_sqd_w
            READ (UNIT = kgen_unit) thlp2
            READ (UNIT = kgen_unit) wpthlp
            READ (UNIT = kgen_unit) rtp2
            READ (UNIT = kgen_unit) wprtp
            READ (UNIT = kgen_unit) rtpthlp
            READ (UNIT = kgen_unit) exner
            READ (UNIT = kgen_unit) p_in_pa
            READ (UNIT = kgen_unit) thv_ds
            READ (UNIT = kgen_unit) wp3
            CALL kr_pdf_closure_real__core_rknd_dim1(sclrm, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(sclrp2, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(wpsclrp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(sclrpthlp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(sclrprtp, kgen_unit)
            READ (UNIT = kgen_unit) level
            CALL kr_pdf_closure_real__core_rknd_dim1(wp2hmp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(wphydrometp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(thlphmp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(rtphmp, kgen_unit)
            READ (UNIT = kgen_unit) wp2thlp
            READ (UNIT = kgen_unit) wp2rtp
            READ (UNIT = kgen_unit) wp2rcp
            READ (UNIT = kgen_unit) wprcp
            READ (UNIT = kgen_unit) thlprcp
            READ (UNIT = kgen_unit) rtprcp
            READ (UNIT = kgen_unit) rcm
            READ (UNIT = kgen_unit) rcp2
            READ (UNIT = kgen_unit) wp4
            READ (UNIT = kgen_unit) wprtp2
            READ (UNIT = kgen_unit) wpthlp2
            READ (UNIT = kgen_unit) cloud_frac
            READ (UNIT = kgen_unit) wpthvp
            READ (UNIT = kgen_unit) wp2thvp
            READ (UNIT = kgen_unit) rtpthvp
            READ (UNIT = kgen_unit) thlpthvp
            READ (UNIT = kgen_unit) wprtpthlp
            READ (UNIT = kgen_unit) ice_supersat_frac
            CALL kr_pdf_parameter_module_pdf_parameter(pdf_params, kgen_unit)
            READ (UNIT = kgen_unit) err_code
            CALL kr_pdf_closure_real__core_rknd_dim1(sclrprcp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(sclrpthvp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(wpsclrp2, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(wpsclrprtp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(wpsclrpthlp, kgen_unit)
            CALL kr_pdf_closure_real__core_rknd_dim1(wp2sclrp, kgen_unit)
            READ (UNIT = kgen_unit) rc_coef
            
            !callsite part
            CALL pdf_closure(kgen_unit, kgen_total_time, hydromet_dim, wp2, wm, rtm, thlm, skw, &
sigma_sqd_w, thlp2, wpthlp, rtp2, wprtp, rtpthlp, exner, p_in_pa, thv_ds, wp3, sclrm, sclrp2, &
wpsclrp, sclrpthlp, sclrprtp, level, wp2hmp, wphydrometp, thlphmp, rtphmp, wp2thlp, wp2rtp, &
wp2rcp, wprcp, thlprcp, rtprcp, rcm, rcp2, wp4, wprtp2, wpthlp2, cloud_frac, wpthvp, wp2thvp, &
rtpthvp, thlpthvp, wprtpthlp, ice_supersat_frac, pdf_params, err_code, sclrprcp, sclrpthvp, &
wpsclrp2, wpsclrprtp, wpsclrpthlp, wp2sclrp, rc_coef)
            CLOSE (UNIT=kgen_unit)
            
        END DO 
        
        WRITE (*, *) ""
        WRITE (*, "(A)") "****************************************************"
        WRITE (*, "(4X,A)") "kernel execution summary: pdf_closure_part3"
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
        
        !read state subroutine for kr_pdf_closure_real__core_rknd_dim1
        SUBROUTINE kr_pdf_closure_real__core_rknd_dim1(var, kgen_unit, printvar)
            REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
            LOGICAL :: kgen_istrue
            INTEGER :: idx1
            INTEGER, DIMENSION(2,1) :: kgen_bound
            
            READ (UNIT = kgen_unit) kgen_istrue
            IF (kgen_istrue) THEN
                IF (ALLOCATED( var )) THEN
                    DEALLOCATE (var)
                END IF 
                READ (UNIT = kgen_unit) kgen_bound(1, 1)
                READ (UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
                READ (UNIT = kgen_unit) var
                IF (PRESENT( printvar )) THEN
                    WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var
                END IF 
            END IF 
        END SUBROUTINE kr_pdf_closure_real__core_rknd_dim1
        
    END PROGRAM 
