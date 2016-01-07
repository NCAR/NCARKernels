!KGEN-generated Fortran source file

!Generated at : 2016-01-07 11:47:38
!KGEN version : 0.6.1

!-------------------------------------------------------------------------
! $Id: variables_diagnostic_module.F90 7376 2014-11-09 02:55:23Z bmg2@uwm.edu $
!===============================================================================
module variables_diagnostic_module

! Description:
!   This module contains definitions of all diagnostic
!   arrays used in the single column model, as well as subroutines
!   to allocate, deallocate and initialize them.

!   Note that while these are all same dimension, there is a
!   thermodynamic and momentum grid and they have different levels
!-----------------------------------------------------------------------


    USE clubb_precision, ONLY: core_rknd

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
    IMPLICIT NONE

    PRIVATE



  ! Diagnostic variables

    REAL(KIND=core_rknd), allocatable, dimension(:), public :: sigma_sqd_w_zt, skw_zt

!!! Important Note !!!
! Do not indent the omp comments, they need to be in the first 4 columns
!!! End Important Note !!!
!$omp threadprivate(sigma_sqd_w_zt, Skw_zm, Skw_zt, ug, vg, &
!$omp   um_ref, vm_ref, thlm_ref, rtm_ref, thvm )


!$omp threadprivate(rsat)


!$omp threadprivate(pdf_params_zm, pdf_params_zm_frz)


!$omp threadprivate(Frad, radht, Frad_SW_up, Frad_SW_down, Frad_LW_up, Frad_LW_down)

! Second order moments

!$omp threadprivate(thlprcp, rtprcp, rcp2)

! Third order moments
    REAL(KIND=core_rknd), allocatable, dimension(:), public :: wpthlp2, wp2thlp, wprtp2, wp2rtp, wprtpthlp, wp2rcp

!$omp threadprivate(wpthlp2, wp2thlp, wprtp2, wp2rtp, &
!$omp   wprtpthlp, wp2rcp, wp3_zm )

! Fourth order moments

!$omp threadprivate(wp4)

! Buoyancy related moments
    REAL(KIND=core_rknd), allocatable, dimension(:), public :: wp2thvp

!$omp threadprivate(rtpthvp, thlpthvp, wpthvp, wp2thvp)


!$omp threadprivate(Kh_zt, Kh_zm)


!$omp threadprivate(K_hm)

! Mixing lengths

!$omp threadprivate(Lscale, Lscale_up, Lscale_down)


!$omp threadprivate(em, tau_zm, tau_zt)

! hydrometeors variable arrays
!$omp threadprivate( hydromet, hydrometp2, wphydrometp )

! Cloud droplet concentration arrays
!$omp threadprivate(Ncm,wpNcp)

!$omp threadprivate(Nccnm)


! Surface data

!$omp threadprivate(ustar, soil_heat_flux)

! Passive scalar variables

!$omp threadprivate(wpedsclrp)

    REAL(KIND=core_rknd), allocatable, dimension(:,:), public :: wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp

!$omp threadprivate(sclrpthvp, sclrprcp, &
!$omp   wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp )

! Interpolated variables for tuning
!
    REAL(KIND=core_rknd), allocatable, dimension(:), public :: wp2_zt, thlp2_zt, rtp2_zt, rtpthlp_zt

!$omp threadprivate(wp2_zt, thlp2_zt, wpthlp_zt, wprtp_zt, &
!$omp   rtp2_zt, rtpthlp_zt, &
!$omp   up2_zt, vp2_zt, upwp_zt, vpwp_zt)


! Latin Hypercube arrays.  Vince Larson 22 May 2005

!$omp threadprivate(lh_AKm, AKm, AKstd, AKstd_cld, lh_rcm_avg, AKm_rcm, &
!$omp   AKm_rcc)


!$omp threadprivate(Skw_velocity, a3_coef, a3_coef_zt)


!$omp threadprivate(wp3_on_wp2, wp3_on_wp2_zt)

    PUBLIC kr_externs_in_variables_diagnostic_module
    PUBLIC kr_externs_out_variables_diagnostic_module
    REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wpthlp2, kgenref_wp2thlp, kgenref_wprtp2, kgenref_wp2rtp, kgenref_wprtpthlp, kgenref_wp2rcp
    REAL(KIND=core_rknd), allocatable, dimension(:) :: kgenref_wp2thvp
    REAL(KIND=core_rknd), allocatable, dimension(:,:) :: kgenref_wp2sclrp, kgenref_wpsclrp2, kgenref_wpsclrprtp, kgenref_wpsclrpthlp
    PUBLIC kv_externs_variables_diagnostic_module

!-----------------------------------------------------------------------
    
    CONTAINS
    
! Description:
!   Allocates and initializes prognostic scalar and array variables
!   for the CLUBB model code
!-----------------------------------------------------------------------





    ! Input Variables

    ! Local Variables

!   --- Allocation ---

    ! Diagnostic variables





    ! pdf_params on momentum levels

    ! Second order moments


    ! Third order moments



    ! Fourth order moments


    ! Buoyancy related moments






    ! Interpolated Variables


    ! Microphysics Variables

    ! Variables for Latin hypercube microphysics.  Vince Larson 22 May 2005
    ! End of variables for Latin hypercube.

    ! High-order passive scalars


    ! Eddy Diff. Scalars




    !   --- Initializaton ---

    ! Diagnostic variables





    ! pdf_params on momentum levels


    ! Second order moments

    ! Third order moments


    ! Fourth order moments

    ! Buoyancy related moments

    ! Eddy diffusivity



    ! TKE

    ! Length scale

    ! Dissipation time

    ! Hydrometer types



    ! Cloud droplet concentration


    ! Variables for Latin hypercube microphysics.  Vince Larson 22 May 2005

    ! Passive scalars









!------------------------------------------------------------------------

! Description:
!   Subroutine to deallocate variables defined in module global
!------------------------------------------------------------------------



    ! --- Deallocate ---







    ! Second order moments


    ! Third order moments



    ! Fourth order moments


    ! Buoyancy related moments




    ! Cloud water variables



    ! Interpolated variables for tuning

    ! Variables for Latin hypercube microphysics.  Vince Larson 22 May 2005

    ! Passive scalars







    !read state subroutine for kr_externs_in_variables_diagnostic_module
    SUBROUTINE kr_externs_in_variables_diagnostic_module(kgen_unit)
        INTEGER, INTENT(IN) :: kgen_unit
        LOGICAL :: kgen_istrue
        REAL(KIND=8) :: kgen_array_sum
        
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(skw_zt, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(sigma_sqd_w_zt, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2rcp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wpthlp2, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wprtp2, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wprtpthlp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2rtp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2thlp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2thvp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpsclrpthlp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wp2sclrp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpsclrp2, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(wpsclrprtp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtpthlp_zt, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(wp2_zt, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(rtp2_zt, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(thlp2_zt, kgen_unit)
    END SUBROUTINE kr_externs_in_variables_diagnostic_module
    
    !read state subroutine for kr_externs_out_variables_diagnostic_module
    SUBROUTINE kr_externs_out_variables_diagnostic_module(kgen_unit)
        INTEGER, INTENT(IN) :: kgen_unit
        
        LOGICAL :: kgen_istrue
        REAL(KIND=8) :: kgen_array_sum
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp2rcp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wpthlp2, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wprtp2, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wprtpthlp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp2rtp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp2thlp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim1(kgenref_wp2thvp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_wpsclrpthlp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_wp2sclrp, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_wpsclrp2, kgen_unit)
        CALL kr_variables_diagnostic_module_real__core_rknd_dim2(kgenref_wpsclrprtp, kgen_unit)
    END SUBROUTINE kr_externs_out_variables_diagnostic_module
    
    !read state subroutine for kr_variables_diagnostic_module_real__core_rknd_dim1
    SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim1(var, kgen_unit, printvar)
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
    END SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim1
    
    !read state subroutine for kr_variables_diagnostic_module_real__core_rknd_dim2
    SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim2(var, kgen_unit, printvar)
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
    END SUBROUTINE kr_variables_diagnostic_module_real__core_rknd_dim2
    
    !verify state subroutine for kv_externs_variables_diagnostic_module
    SUBROUTINE kv_externs_variables_diagnostic_module(check_status)
        TYPE(check_t), INTENT(INOUT) :: check_status
        
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp2rcp", check_status, wp2rcp, kgenref_wp2rcp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wpthlp2", check_status, wpthlp2, kgenref_wpthlp2)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wprtp2", check_status, wprtp2, kgenref_wprtp2)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wprtpthlp", check_status, wprtpthlp, kgenref_wprtpthlp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp2rtp", check_status, wp2rtp, kgenref_wp2rtp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp2thlp", check_status, wp2thlp, kgenref_wp2thlp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim1("wp2thvp", check_status, wp2thvp, kgenref_wp2thvp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim2("wpsclrpthlp", check_status, wpsclrpthlp, kgenref_wpsclrpthlp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim2("wp2sclrp", check_status, wp2sclrp, kgenref_wp2sclrp)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim2("wpsclrp2", check_status, wpsclrp2, kgenref_wpsclrp2)
        CALL kv_variables_diagnostic_module_real__core_rknd_dim2("wpsclrprtp", check_status, wpsclrprtp, kgenref_wpsclrprtp)
    END SUBROUTINE kv_externs_variables_diagnostic_module
    
    !verify state subroutine for kv_variables_diagnostic_module_real__core_rknd_dim1
    RECURSIVE SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim1(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        REAL(KIND=core_rknd), allocatable, INTENT(IN), DIMENSION(:) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        INTEGER :: idx1
        INTEGER :: n
        real(KIND=core_rknd) :: nrmsdiff, rmsdiff
        real(KIND=core_rknd), ALLOCATABLE :: buf1(:), buf2(:)
        
        IF (ALLOCATED(var)) THEN
            check_status%numTotal = check_status%numTotal + 1
            
            IF (ALL(var == kgenref_var)) THEN
                check_status%numIdentical = check_status%numIdentical + 1
                IF (check_status%verboseLevel > 1) THEN
                    WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
                END IF 
                check_result = CHECK_IDENTICAL
            ELSE
                ALLOCATE (buf1(SIZE(var,dim=1)))
                ALLOCATE (buf2(SIZE(var,dim=1)))
                n = COUNT(var /= kgenref_var)
                WHERE ( ABS(kgenref_var) > check_status%minvalue )
                    buf1 = ((var-kgenref_var)/kgenref_var)**2
                    buf2 = (var-kgenref_var)**2
                ELSEWHERE
                    buf1 = (var-kgenref_var)**2
                    buf2 = buf1
                END WHERE 
                nrmsdiff = SQRT(SUM(buf1)/REAL(n))
                rmsdiff = SQRT(SUM(buf2)/REAL(n))
                IF (nrmsdiff > check_status%tolerance) THEN
                    check_status%numOutTol = check_status%numOutTol + 1
                    IF (check_status%verboseLevel > 0) THEN
                        WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                    END IF 
                    check_result = CHECK_OUT_TOL
                ELSE
                    check_status%numInTol = check_status%numInTol + 1
                    IF (check_status%verboseLevel > 0) THEN
                        WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                    END IF 
                    check_result = CHECK_IN_TOL
                END IF 
            END IF 
            IF (check_result == CHECK_IDENTICAL) THEN
                IF (check_status%verboseLevel > 2) THEN
                    WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                    WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                    WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                    WRITE (*, *) "RMS of difference is ", 0
                    WRITE (*, *) "Normalized RMS of difference is ", 0
                    WRITE (*, *) ""
                END IF 
            ELSE IF (check_result == CHECK_OUT_TOL) THEN
                IF (check_status%verboseLevel > 0) THEN
                    WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                    WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                    WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                    WRITE (*, *) "RMS of difference is ", rmsdiff
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                    WRITE (*, *) ""
                END IF 
            ELSE IF (check_result == CHECK_IN_TOL) THEN
                IF (check_status%verboseLevel > 1) THEN
                    WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                    WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                    WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                    WRITE (*, *) "RMS of difference is ", rmsdiff
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                    WRITE (*, *) ""
                END IF 
            END IF 
            
        END IF 
    END SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim1
    
    !verify state subroutine for kv_variables_diagnostic_module_real__core_rknd_dim2
    RECURSIVE SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim2(varname, check_status, var, kgenref_var)
        CHARACTER(LEN=*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        REAL(KIND=core_rknd), allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var
        INTEGER :: check_result
        LOGICAL :: is_print = .FALSE.
        
        INTEGER :: idx1, idx2
        INTEGER :: n
        real(KIND=core_rknd) :: nrmsdiff, rmsdiff
        real(KIND=core_rknd), ALLOCATABLE :: buf1(:,:), buf2(:,:)
        
        IF (ALLOCATED(var)) THEN
            check_status%numTotal = check_status%numTotal + 1
            
            IF (ALL(var == kgenref_var)) THEN
                check_status%numIdentical = check_status%numIdentical + 1
                IF (check_status%verboseLevel > 1) THEN
                    WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
                END IF 
                check_result = CHECK_IDENTICAL
            ELSE
                ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2)))
                ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2)))
                n = COUNT(var /= kgenref_var)
                WHERE ( ABS(kgenref_var) > check_status%minvalue )
                    buf1 = ((var-kgenref_var)/kgenref_var)**2
                    buf2 = (var-kgenref_var)**2
                ELSEWHERE
                    buf1 = (var-kgenref_var)**2
                    buf2 = buf1
                END WHERE 
                nrmsdiff = SQRT(SUM(buf1)/REAL(n))
                rmsdiff = SQRT(SUM(buf2)/REAL(n))
                IF (nrmsdiff > check_status%tolerance) THEN
                    check_status%numOutTol = check_status%numOutTol + 1
                    IF (check_status%verboseLevel > 0) THEN
                        WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                    END IF 
                    check_result = CHECK_OUT_TOL
                ELSE
                    check_status%numInTol = check_status%numInTol + 1
                    IF (check_status%verboseLevel > 0) THEN
                        WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                    END IF 
                    check_result = CHECK_IN_TOL
                END IF 
            END IF 
            IF (check_result == CHECK_IDENTICAL) THEN
                IF (check_status%verboseLevel > 2) THEN
                    WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                    WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                    WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                    WRITE (*, *) "RMS of difference is ", 0
                    WRITE (*, *) "Normalized RMS of difference is ", 0
                    WRITE (*, *) ""
                END IF 
            ELSE IF (check_result == CHECK_OUT_TOL) THEN
                IF (check_status%verboseLevel > 0) THEN
                    WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                    WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                    WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                    WRITE (*, *) "RMS of difference is ", rmsdiff
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                    WRITE (*, *) ""
                END IF 
            ELSE IF (check_result == CHECK_IN_TOL) THEN
                IF (check_status%verboseLevel > 1) THEN
                    WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                    WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                    WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                    WRITE (*, *) "RMS of difference is ", rmsdiff
                    WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                    WRITE (*, *) ""
                END IF 
            END IF 
            
        END IF 
    END SUBROUTINE kv_variables_diagnostic_module_real__core_rknd_dim2
    
end module variables_diagnostic_module