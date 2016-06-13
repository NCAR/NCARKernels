    !KGEN-generated Fortran source file
    
    !Generated at : 2016-01-07 11:47:38
    !KGEN version : 0.6.1
    
    PROGRAM kernel_driver
        USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck
        USE advance_clubb_core_module, ONLY: advance_clubb_core
        
        USE clubb_precision, ONLY: core_rknd
        USE grid_class, ONLY: gr
        USE parameters_model, ONLY: sclr_dim
        USE pdf_parameter_module, ONLY: pdf_parameter
        USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter
        USE parameters_model, ONLY: kr_externs_in_parameters_model
        USE error_code, ONLY: kr_externs_in_error_code
        USE parameters_tunable, ONLY: kr_externs_in_parameters_tunable
        USE stats_variables, ONLY: kr_externs_in_stats_variables
        USE model_flags, ONLY: kr_externs_in_model_flags
        USE array_index, ONLY: kr_externs_in_array_index
        USE grid_class, ONLY: kr_externs_in_grid_class
        USE variables_diagnostic_module, ONLY: kr_externs_in_variables_diagnostic_module
        IMPLICIT NONE
        
        INTEGER :: kgen_mpi_rank
        CHARACTER(LEN=16) :: kgen_mpi_rank_conv
        INTEGER, PARAMETER, DIMENSION(3) :: kgen_mpi_rank_at = (/ 100, 300, 500 /)
        INTEGER :: kgen_ierr, kgen_unit, kgen_counter, kgen_repeat_counter
        CHARACTER(LEN=16) :: kgen_counter_conv
        INTEGER, PARAMETER, DIMENSION(3) :: kgen_counter_at = (/ 10, 100, 50 /)
        CHARACTER(LEN=1024) :: kgen_filepath
        REAL(KIND=kgen_dp) :: kgen_total_time
        REAL(KIND=8) :: kgen_array_sum
        
        INTEGER :: hydromet_dim
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: p_in_pa
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: exner
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: thv_ds_zt
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wm_zt
        REAL(KIND=core_rknd), DIMENSION(:,:), ALLOCATABLE :: wp2hmp
        REAL(KIND=core_rknd), DIMENSION(:,:), ALLOCATABLE :: rtphmp_zt
        REAL(KIND=core_rknd), DIMENSION(:,:), ALLOCATABLE :: thlphmp_zt
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wp3
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: rtm
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wprtp
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: thlm
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: wpthlp
        REAL(KIND=core_rknd), DIMENSION(:,:), ALLOCATABLE :: sclrm
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: rcm
        TYPE(pdf_parameter), DIMENSION(:), ALLOCATABLE :: pdf_params
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: cloud_frac
        REAL(KIND=core_rknd), DIMENSION(:), ALLOCATABLE :: ice_supersat_frac
        kgen_total_time = 0.0_kgen_dp
        
        DO kgen_repeat_counter = 0, 8
            
            kgen_mpi_rank = kgen_mpi_rank_at(kgen_repeat_counter/3 + 1)
            WRITE (kgen_mpi_rank_conv, *) kgen_mpi_rank
            kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 3) + 1)
            WRITE (kgen_counter_conv, *) kgen_counter
            kgen_filepath = "../data/pdf_closure." // TRIM(ADJUSTL(kgen_counter_conv)) // "." // TRIM(ADJUSTL(kgen_mpi_rank_conv))
            kgen_unit = kgen_get_newunit()
            
            OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr)
            IF (kgen_ierr /= 0) THEN
                CALL kgen_error_stop("FILE OPEN ERROR: " // TRIM(ADJUSTL(kgen_filepath)))
            END IF 
            
            WRITE (*, *) ""
            WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' *****************"
            
            
            !driver read in arguments
            READ (UNIT = kgen_unit) hydromet_dim
            CALL kr_advance_clubb_core_real__core_rknd_dim1(p_in_pa, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(exner, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(thv_ds_zt, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(wm_zt, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim2(wp2hmp, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim2(rtphmp_zt, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim2(thlphmp_zt, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(wp3, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(rtm, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(wprtp, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(thlm, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(wpthlp, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim2(sclrm, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(rcm, kgen_unit)
            CALL kr_kgen_subpname_0(pdf_params, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(cloud_frac, kgen_unit)
            CALL kr_advance_clubb_core_real__core_rknd_dim1(ice_supersat_frac, kgen_unit)
            
            !extern input variables
            CALL kr_externs_in_parameters_model(kgen_unit)
            CALL kr_externs_in_error_code(kgen_unit)
            CALL kr_externs_in_parameters_tunable(kgen_unit)
            CALL kr_externs_in_stats_variables(kgen_unit)
            CALL kr_externs_in_model_flags(kgen_unit)
            CALL kr_externs_in_array_index(kgen_unit)
            CALL kr_externs_in_grid_class(kgen_unit)
            CALL kr_externs_in_variables_diagnostic_module(kgen_unit)
            
            !callsite part
            CALL advance_clubb_core(kgen_unit, kgen_total_time, hydromet_dim, p_in_pa, exner, thv_ds_zt, wm_zt, wp2hmp, rtphmp_zt, thlphmp_zt, wp3, rtm, wprtp, thlm, wpthlp, sclrm, rcm, pdf_params, cloud_frac, ice_supersat_frac)
            CLOSE (UNIT=kgen_unit)
            
        END DO 
        
        WRITE (*, *) ""
        WRITE (*, *) "******************************************************************************"
        WRITE (*, *) "pdf_closure summary: Total number of verification cases: 9"
        WRITE (*, *) "pdf_closure summary: Average call time of all calls (usec): ", kgen_total_time / 9
        WRITE (*, *) "******************************************************************************"
        
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
        
        !read state subroutine for kr_kgen_subpname_0
        SUBROUTINE kr_kgen_subpname_0(var, kgen_unit, printvar)
            TYPE(pdf_parameter), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var
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
                READ (UNIT = kgen_unit) kgen_bound(1, 1)
                READ (UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
                DO idx1=kgen_bound(1,1), kgen_bound(2,1)
                    IF (PRESENT( printvar )) THEN
                        CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printvar // "(idx1)")
                    ELSE
                        CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit)
                    END IF 
                END DO 
            END IF 
        END SUBROUTINE kr_kgen_subpname_0
        
    END PROGRAM 
