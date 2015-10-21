
! KGEN-generated Fortran source file
!
! Filename    : pdf_parameter_module.F90
! Generated at: 2015-10-20 14:27:07
! KGEN version: 0.5.3



    MODULE pdf_parameter_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   This module defines the derived type pdf_parameter.
! References:
!   None
!-------------------------------------------------------------------------------
        USE clubb_precision, ONLY: core_rknd
        IMPLICIT NONE
        PRIVATE ! Default scope
        PUBLIC pdf_parameter
        TYPE pdf_parameter
            REAL(KIND=core_rknd) :: w_1, w_2, varnce_w_1, varnce_w_2, rt_1, rt_2, varnce_rt_1, varnce_rt_2, thl_1, thl_2, &
            varnce_thl_1, varnce_thl_2, rrtthl, alpha_thl, alpha_rt, crt_1, crt_2, cthl_1, cthl_2, chi_1, chi_2, stdev_chi_1, &
            stdev_chi_2, stdev_eta_1, stdev_eta_2, covar_chi_eta_1, covar_chi_eta_2, corr_chi_eta_1, corr_chi_eta_2, rsatl_1, &
            rsatl_2, rc_1, rc_2, cloud_frac_1, cloud_frac_2, mixt_frac
! Mean of w (1st PDF component)                   [m/s]
! Mean of w (2nd PDF component)                   [m/s]
! Variance of w (1st PDF component)           [m^2/s^2]
! Variance of w (2nd PDF component)           [m^2/s^2]
! Mean of r_t (1st PDF component)               [kg/kg]
! Mean of r_t (2nd PDF component)               [kg/kg]
! Variance of r_t (1st PDF component)       [kg^2/kg^2]
! Variance of r_t (2nd PDF component)       [kg^2/kg^2]
! Mean of th_l (1st PDF component)                  [K]
! Mean of th_l (2nd PDF component)                  [K]
! Variance of th_l (1st PDF component)            [K^2]
! Variance of th_l (2nd PDF component)            [K^2]
! Correlation of r_t and th_l (both components)     [-]
! Factor relating to normalized variance for th_l   [-]
! Factor relating to normalized variance for r_t    [-]
! r_t coef. in chi/eta eqns. (1st PDF comp.)        [-]
! r_t coef. in chi/eta eqns. (2nd PDF comp.)        [-]
! th_l coef.: chi/eta eqns. (1st PDF comp.) [(kg/kg)/K]
! th_l coef.: chi/eta eqns. (2nd PDF comp.) [(kg/kg)/K]
! Mean of chi (old s) (1st PDF component)       [kg/kg]
! Mean of chi (old s) (2nd PDF component)       [kg/kg]
! Standard deviation of chi (1st PDF component) [kg/kg]
! Standard deviation of chi (2nd PDF component) [kg/kg]
! Standard dev. of eta (old t) (1st PDF comp.)  [kg/kg]
! Standard dev. of eta (old t) (2nd PDF comp.)  [kg/kg]
! Covariance of chi and eta (1st PDF comp.) [kg^2/kg^2]
! Covariance of chi and eta (2nd PDF comp.) [kg^2/kg^2]
! Correlation of chi and eta (1st PDF component)    [-]
! Correlation of chi and eta (2nd PDF component)    [-]
! Saturation mixing ratio r_sat(mu_Tl_1,p)      [kg/kg]
! Saturation mixing ratio r_sat(mu_Tl_2,p)      [kg/kg]
! Mean of r_c (1st PDF component)               [kg/kg]
! Mean of r_c (2nd PDF component)               [kg/kg]
! Cloud fraction (1st PDF component)                [-]
! Cloud fraction (2nd PDF component)                [-]
! Weight of 1st PDF component (Sk_w dependent)      [-]
        END TYPE pdf_parameter
!-------

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_pdf_parameter
        END INTERFACE kgen_read

        PUBLIC kgen_verify
        INTERFACE kgen_verify
            MODULE PROCEDURE kgen_verify_pdf_parameter
        END INTERFACE kgen_verify

        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        SUBROUTINE kgen_read_pdf_parameter(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(pdf_parameter), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%w_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%w_1 **", var%w_1
            END IF
            READ(UNIT=kgen_unit) var%w_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%w_2 **", var%w_2
            END IF
            READ(UNIT=kgen_unit) var%varnce_w_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%varnce_w_1 **", var%varnce_w_1
            END IF
            READ(UNIT=kgen_unit) var%varnce_w_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%varnce_w_2 **", var%varnce_w_2
            END IF
            READ(UNIT=kgen_unit) var%rt_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rt_1 **", var%rt_1
            END IF
            READ(UNIT=kgen_unit) var%rt_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rt_2 **", var%rt_2
            END IF
            READ(UNIT=kgen_unit) var%varnce_rt_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%varnce_rt_1 **", var%varnce_rt_1
            END IF
            READ(UNIT=kgen_unit) var%varnce_rt_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%varnce_rt_2 **", var%varnce_rt_2
            END IF
            READ(UNIT=kgen_unit) var%thl_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%thl_1 **", var%thl_1
            END IF
            READ(UNIT=kgen_unit) var%thl_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%thl_2 **", var%thl_2
            END IF
            READ(UNIT=kgen_unit) var%varnce_thl_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%varnce_thl_1 **", var%varnce_thl_1
            END IF
            READ(UNIT=kgen_unit) var%varnce_thl_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%varnce_thl_2 **", var%varnce_thl_2
            END IF
            READ(UNIT=kgen_unit) var%rrtthl
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rrtthl **", var%rrtthl
            END IF
            READ(UNIT=kgen_unit) var%alpha_thl
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%alpha_thl **", var%alpha_thl
            END IF
            READ(UNIT=kgen_unit) var%alpha_rt
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%alpha_rt **", var%alpha_rt
            END IF
            READ(UNIT=kgen_unit) var%crt_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%crt_1 **", var%crt_1
            END IF
            READ(UNIT=kgen_unit) var%crt_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%crt_2 **", var%crt_2
            END IF
            READ(UNIT=kgen_unit) var%cthl_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%cthl_1 **", var%cthl_1
            END IF
            READ(UNIT=kgen_unit) var%cthl_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%cthl_2 **", var%cthl_2
            END IF
            READ(UNIT=kgen_unit) var%chi_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%chi_1 **", var%chi_1
            END IF
            READ(UNIT=kgen_unit) var%chi_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%chi_2 **", var%chi_2
            END IF
            READ(UNIT=kgen_unit) var%stdev_chi_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%stdev_chi_1 **", var%stdev_chi_1
            END IF
            READ(UNIT=kgen_unit) var%stdev_chi_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%stdev_chi_2 **", var%stdev_chi_2
            END IF
            READ(UNIT=kgen_unit) var%stdev_eta_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%stdev_eta_1 **", var%stdev_eta_1
            END IF
            READ(UNIT=kgen_unit) var%stdev_eta_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%stdev_eta_2 **", var%stdev_eta_2
            END IF
            READ(UNIT=kgen_unit) var%covar_chi_eta_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%covar_chi_eta_1 **", var%covar_chi_eta_1
            END IF
            READ(UNIT=kgen_unit) var%covar_chi_eta_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%covar_chi_eta_2 **", var%covar_chi_eta_2
            END IF
            READ(UNIT=kgen_unit) var%corr_chi_eta_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%corr_chi_eta_1 **", var%corr_chi_eta_1
            END IF
            READ(UNIT=kgen_unit) var%corr_chi_eta_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%corr_chi_eta_2 **", var%corr_chi_eta_2
            END IF
            READ(UNIT=kgen_unit) var%rsatl_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rsatl_1 **", var%rsatl_1
            END IF
            READ(UNIT=kgen_unit) var%rsatl_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rsatl_2 **", var%rsatl_2
            END IF
            READ(UNIT=kgen_unit) var%rc_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rc_1 **", var%rc_1
            END IF
            READ(UNIT=kgen_unit) var%rc_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rc_2 **", var%rc_2
            END IF
            READ(UNIT=kgen_unit) var%cloud_frac_1
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%cloud_frac_1 **", var%cloud_frac_1
            END IF
            READ(UNIT=kgen_unit) var%cloud_frac_2
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%cloud_frac_2 **", var%cloud_frac_2
            END IF
            READ(UNIT=kgen_unit) var%mixt_frac
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%mixt_frac **", var%mixt_frac
            END IF
        END SUBROUTINE
        RECURSIVE SUBROUTINE kgen_verify_pdf_parameter(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(pdf_parameter), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_real_core_rknd("w_1", dtype_check_status, var%w_1, ref_var%w_1)
            CALL kgen_verify_real_core_rknd("w_2", dtype_check_status, var%w_2, ref_var%w_2)
            CALL kgen_verify_real_core_rknd("varnce_w_1", dtype_check_status, var%varnce_w_1, ref_var%varnce_w_1)
            CALL kgen_verify_real_core_rknd("varnce_w_2", dtype_check_status, var%varnce_w_2, ref_var%varnce_w_2)
            CALL kgen_verify_real_core_rknd("rt_1", dtype_check_status, var%rt_1, ref_var%rt_1)
            CALL kgen_verify_real_core_rknd("rt_2", dtype_check_status, var%rt_2, ref_var%rt_2)
            CALL kgen_verify_real_core_rknd("varnce_rt_1", dtype_check_status, var%varnce_rt_1, ref_var%varnce_rt_1)
            CALL kgen_verify_real_core_rknd("varnce_rt_2", dtype_check_status, var%varnce_rt_2, ref_var%varnce_rt_2)
            CALL kgen_verify_real_core_rknd("thl_1", dtype_check_status, var%thl_1, ref_var%thl_1)
            CALL kgen_verify_real_core_rknd("thl_2", dtype_check_status, var%thl_2, ref_var%thl_2)
            CALL kgen_verify_real_core_rknd("varnce_thl_1", dtype_check_status, var%varnce_thl_1, ref_var%varnce_thl_1)
            CALL kgen_verify_real_core_rknd("varnce_thl_2", dtype_check_status, var%varnce_thl_2, ref_var%varnce_thl_2)
            CALL kgen_verify_real_core_rknd("rrtthl", dtype_check_status, var%rrtthl, ref_var%rrtthl)
            CALL kgen_verify_real_core_rknd("alpha_thl", dtype_check_status, var%alpha_thl, ref_var%alpha_thl)
            CALL kgen_verify_real_core_rknd("alpha_rt", dtype_check_status, var%alpha_rt, ref_var%alpha_rt)
            CALL kgen_verify_real_core_rknd("crt_1", dtype_check_status, var%crt_1, ref_var%crt_1)
            CALL kgen_verify_real_core_rknd("crt_2", dtype_check_status, var%crt_2, ref_var%crt_2)
            CALL kgen_verify_real_core_rknd("cthl_1", dtype_check_status, var%cthl_1, ref_var%cthl_1)
            CALL kgen_verify_real_core_rknd("cthl_2", dtype_check_status, var%cthl_2, ref_var%cthl_2)
            CALL kgen_verify_real_core_rknd("chi_1", dtype_check_status, var%chi_1, ref_var%chi_1)
            CALL kgen_verify_real_core_rknd("chi_2", dtype_check_status, var%chi_2, ref_var%chi_2)
            CALL kgen_verify_real_core_rknd("stdev_chi_1", dtype_check_status, var%stdev_chi_1, ref_var%stdev_chi_1)
            CALL kgen_verify_real_core_rknd("stdev_chi_2", dtype_check_status, var%stdev_chi_2, ref_var%stdev_chi_2)
            CALL kgen_verify_real_core_rknd("stdev_eta_1", dtype_check_status, var%stdev_eta_1, ref_var%stdev_eta_1)
            CALL kgen_verify_real_core_rknd("stdev_eta_2", dtype_check_status, var%stdev_eta_2, ref_var%stdev_eta_2)
            CALL kgen_verify_real_core_rknd("covar_chi_eta_1", dtype_check_status, var%covar_chi_eta_1, ref_var%covar_chi_eta_1)
            CALL kgen_verify_real_core_rknd("covar_chi_eta_2", dtype_check_status, var%covar_chi_eta_2, ref_var%covar_chi_eta_2)
            CALL kgen_verify_real_core_rknd("corr_chi_eta_1", dtype_check_status, var%corr_chi_eta_1, ref_var%corr_chi_eta_1)
            CALL kgen_verify_real_core_rknd("corr_chi_eta_2", dtype_check_status, var%corr_chi_eta_2, ref_var%corr_chi_eta_2)
            CALL kgen_verify_real_core_rknd("rsatl_1", dtype_check_status, var%rsatl_1, ref_var%rsatl_1)
            CALL kgen_verify_real_core_rknd("rsatl_2", dtype_check_status, var%rsatl_2, ref_var%rsatl_2)
            CALL kgen_verify_real_core_rknd("rc_1", dtype_check_status, var%rc_1, ref_var%rc_1)
            CALL kgen_verify_real_core_rknd("rc_2", dtype_check_status, var%rc_2, ref_var%rc_2)
            CALL kgen_verify_real_core_rknd("cloud_frac_1", dtype_check_status, var%cloud_frac_1, ref_var%cloud_frac_1)
            CALL kgen_verify_real_core_rknd("cloud_frac_2", dtype_check_status, var%cloud_frac_2, ref_var%cloud_frac_2)
            CALL kgen_verify_real_core_rknd("mixt_frac", dtype_check_status, var%mixt_frac, ref_var%mixt_frac)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
            SUBROUTINE kgen_verify_real_core_rknd( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=core_rknd), intent(in) :: var, ref_var
                check_status%numTotal = check_status%numTotal + 1
                IF ( var == ref_var ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                    endif
                ELSE
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "KERNEL: ", var
                            WRITE(*,*) "REF.  : ", ref_var
                        end if
                    end if
                    check_status%numFatal = check_status%numFatal + 1
                END IF
            END SUBROUTINE kgen_verify_real_core_rknd

!-------




    END MODULE pdf_parameter_module
