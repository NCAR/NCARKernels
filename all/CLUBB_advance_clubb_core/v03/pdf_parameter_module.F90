!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:44
!KGEN version : 0.6.1

!-----------------------------------------------------------------------
! $Id: pdf_parameter_module.F90 7309 2014-09-20 17:06:28Z betlej@uwm.edu $
!===============================================================================
module pdf_parameter_module
! Description:
!   This module defines the derived type pdf_parameter.
! References:
!   None
!-------------------------------------------------------------------------------

    USE clubb_precision, ONLY: core_rknd

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
    IMPLICIT NONE

    PRIVATE

    PUBLIC pdf_parameter

  type pdf_parameter
    real( kind = core_rknd ) :: &
      w_1,             & ! Mean of w (1st PDF component)                   [m/s]
      w_2,             & ! Mean of w (2nd PDF component)                   [m/s]
      varnce_w_1,      & ! Variance of w (1st PDF component)           [m^2/s^2]
      varnce_w_2,      & ! Variance of w (2nd PDF component)           [m^2/s^2]
      rt_1,            & ! Mean of r_t (1st PDF component)               [kg/kg]
      rt_2,            & ! Mean of r_t (2nd PDF component)               [kg/kg]
      varnce_rt_1,     & ! Variance of r_t (1st PDF component)       [kg^2/kg^2]
      varnce_rt_2,     & ! Variance of r_t (2nd PDF component)       [kg^2/kg^2]
      thl_1,           & ! Mean of th_l (1st PDF component)                  [K]
      thl_2,           & ! Mean of th_l (2nd PDF component)                  [K]
      varnce_thl_1,    & ! Variance of th_l (1st PDF component)            [K^2]
      varnce_thl_2,    & ! Variance of th_l (2nd PDF component)            [K^2]
      rrtthl,          & ! Correlation of r_t and th_l (both components)     [-]
      alpha_thl,       & ! Factor relating to normalized variance for th_l   [-]
      alpha_rt,        & ! Factor relating to normalized variance for r_t    [-]
      crt_1,           & ! r_t coef. in chi/eta eqns. (1st PDF comp.)        [-]
      crt_2,           & ! r_t coef. in chi/eta eqns. (2nd PDF comp.)        [-]
      cthl_1,          & ! th_l coef.: chi/eta eqns. (1st PDF comp.) [(kg/kg)/K]
      cthl_2,          & ! th_l coef.: chi/eta eqns. (2nd PDF comp.) [(kg/kg)/K]
      chi_1,           & ! Mean of chi (old s) (1st PDF component)       [kg/kg]
      chi_2,           & ! Mean of chi (old s) (2nd PDF component)       [kg/kg]
      stdev_chi_1,     & ! Standard deviation of chi (1st PDF component) [kg/kg]
      stdev_chi_2,     & ! Standard deviation of chi (2nd PDF component) [kg/kg]
      stdev_eta_1,     & ! Standard dev. of eta (old t) (1st PDF comp.)  [kg/kg]
      stdev_eta_2,     & ! Standard dev. of eta (old t) (2nd PDF comp.)  [kg/kg]
      covar_chi_eta_1, & ! Covariance of chi and eta (1st PDF comp.) [kg^2/kg^2]
      covar_chi_eta_2, & ! Covariance of chi and eta (2nd PDF comp.) [kg^2/kg^2]
      corr_chi_eta_1,  & ! Correlation of chi and eta (1st PDF component)    [-]
      corr_chi_eta_2,  & ! Correlation of chi and eta (2nd PDF component)    [-]
      rsatl_1,         & ! Saturation mixing ratio r_sat(mu_Tl_1,p)      [kg/kg]
      rsatl_2,         & ! Saturation mixing ratio r_sat(mu_Tl_2,p)      [kg/kg]
      rc_1,            & ! Mean of r_c (1st PDF component)               [kg/kg]
      rc_2,            & ! Mean of r_c (2nd PDF component)               [kg/kg]
      cloud_frac_1,    & ! Cloud fraction (1st PDF component)                [-]
      cloud_frac_2,    & ! Cloud fraction (2nd PDF component)                [-]
      mixt_frac          ! Weight of 1st PDF component (Sk_w dependent)      [-]
  end type pdf_parameter





  !-------
  PUBLIC kr_pdf_parameter_module_pdf_parameter
  PUBLIC kv_pdf_parameter_module_pdf_parameter
  !-------

  
  CONTAINS
  
    ! Input a pdf_parameter array with nz instances of pdf_parameter

    ! Output a two dimensional real array with all values

    ! Local Loop vars

 
       


    ! Input a two dimensional real array with pdf values

    ! Output a pdf_parameter array with nz instances of pdf_parameter

    ! Local Loop vars
    ! temp var
    





!          NAG compiler does not like divide by zero - commented out



      	   ! do nothing !




  !read state subroutine for kr_pdf_parameter_module_pdf_parameter
  RECURSIVE SUBROUTINE kr_pdf_parameter_module_pdf_parameter(var, kgen_unit, printvar)
      TYPE(pdf_parameter), INTENT(INOUT) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      READ (UNIT = kgen_unit) var%w_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%w_1 **" // NEW_LINE("A"), var%w_1
      END IF 
      READ (UNIT = kgen_unit) var%w_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%w_2 **" // NEW_LINE("A"), var%w_2
      END IF 
      READ (UNIT = kgen_unit) var%varnce_w_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_w_1 **" // NEW_LINE("A"), var%varnce_w_1
      END IF 
      READ (UNIT = kgen_unit) var%varnce_w_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_w_2 **" // NEW_LINE("A"), var%varnce_w_2
      END IF 
      READ (UNIT = kgen_unit) var%rt_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rt_1 **" // NEW_LINE("A"), var%rt_1
      END IF 
      READ (UNIT = kgen_unit) var%rt_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rt_2 **" // NEW_LINE("A"), var%rt_2
      END IF 
      READ (UNIT = kgen_unit) var%varnce_rt_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_rt_1 **" // NEW_LINE("A"), var%varnce_rt_1
      END IF 
      READ (UNIT = kgen_unit) var%varnce_rt_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_rt_2 **" // NEW_LINE("A"), var%varnce_rt_2
      END IF 
      READ (UNIT = kgen_unit) var%thl_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%thl_1 **" // NEW_LINE("A"), var%thl_1
      END IF 
      READ (UNIT = kgen_unit) var%thl_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%thl_2 **" // NEW_LINE("A"), var%thl_2
      END IF 
      READ (UNIT = kgen_unit) var%varnce_thl_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_thl_1 **" // NEW_LINE("A"), var%varnce_thl_1
      END IF 
      READ (UNIT = kgen_unit) var%varnce_thl_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_thl_2 **" // NEW_LINE("A"), var%varnce_thl_2
      END IF 
      READ (UNIT = kgen_unit) var%rrtthl
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rrtthl **" // NEW_LINE("A"), var%rrtthl
      END IF 
      READ (UNIT = kgen_unit) var%alpha_thl
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%alpha_thl **" // NEW_LINE("A"), var%alpha_thl
      END IF 
      READ (UNIT = kgen_unit) var%alpha_rt
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%alpha_rt **" // NEW_LINE("A"), var%alpha_rt
      END IF 
      READ (UNIT = kgen_unit) var%crt_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%crt_1 **" // NEW_LINE("A"), var%crt_1
      END IF 
      READ (UNIT = kgen_unit) var%crt_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%crt_2 **" // NEW_LINE("A"), var%crt_2
      END IF 
      READ (UNIT = kgen_unit) var%cthl_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cthl_1 **" // NEW_LINE("A"), var%cthl_1
      END IF 
      READ (UNIT = kgen_unit) var%cthl_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cthl_2 **" // NEW_LINE("A"), var%cthl_2
      END IF 
      READ (UNIT = kgen_unit) var%chi_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%chi_1 **" // NEW_LINE("A"), var%chi_1
      END IF 
      READ (UNIT = kgen_unit) var%chi_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%chi_2 **" // NEW_LINE("A"), var%chi_2
      END IF 
      READ (UNIT = kgen_unit) var%stdev_chi_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_chi_1 **" // NEW_LINE("A"), var%stdev_chi_1
      END IF 
      READ (UNIT = kgen_unit) var%stdev_chi_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_chi_2 **" // NEW_LINE("A"), var%stdev_chi_2
      END IF 
      READ (UNIT = kgen_unit) var%stdev_eta_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_eta_1 **" // NEW_LINE("A"), var%stdev_eta_1
      END IF 
      READ (UNIT = kgen_unit) var%stdev_eta_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_eta_2 **" // NEW_LINE("A"), var%stdev_eta_2
      END IF 
      READ (UNIT = kgen_unit) var%covar_chi_eta_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%covar_chi_eta_1 **" // NEW_LINE("A"), var%covar_chi_eta_1
      END IF 
      READ (UNIT = kgen_unit) var%covar_chi_eta_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%covar_chi_eta_2 **" // NEW_LINE("A"), var%covar_chi_eta_2
      END IF 
      READ (UNIT = kgen_unit) var%corr_chi_eta_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%corr_chi_eta_1 **" // NEW_LINE("A"), var%corr_chi_eta_1
      END IF 
      READ (UNIT = kgen_unit) var%corr_chi_eta_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%corr_chi_eta_2 **" // NEW_LINE("A"), var%corr_chi_eta_2
      END IF 
      READ (UNIT = kgen_unit) var%rsatl_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rsatl_1 **" // NEW_LINE("A"), var%rsatl_1
      END IF 
      READ (UNIT = kgen_unit) var%rsatl_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rsatl_2 **" // NEW_LINE("A"), var%rsatl_2
      END IF 
      READ (UNIT = kgen_unit) var%rc_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rc_1 **" // NEW_LINE("A"), var%rc_1
      END IF 
      READ (UNIT = kgen_unit) var%rc_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rc_2 **" // NEW_LINE("A"), var%rc_2
      END IF 
      READ (UNIT = kgen_unit) var%cloud_frac_1
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cloud_frac_1 **" // NEW_LINE("A"), var%cloud_frac_1
      END IF 
      READ (UNIT = kgen_unit) var%cloud_frac_2
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cloud_frac_2 **" // NEW_LINE("A"), var%cloud_frac_2
      END IF 
      READ (UNIT = kgen_unit) var%mixt_frac
      IF (PRESENT( printvar )) THEN
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%mixt_frac **" // NEW_LINE("A"), var%mixt_frac
      END IF 
      
  END SUBROUTINE kr_pdf_parameter_module_pdf_parameter
  
  !verify state subroutine for kv_pdf_parameter_module_pdf_parameter
  RECURSIVE SUBROUTINE kv_pdf_parameter_module_pdf_parameter(varname, check_status, var, kgenref_var)
      CHARACTER(LEN=*), INTENT(IN) :: varname
      TYPE(check_t), INTENT(INOUT) :: check_status
      TYPE(pdf_parameter), INTENT(IN) :: var, kgenref_var
      TYPE(check_t) :: dtype_check_status, comp_check_status
      INTEGER :: check_result
      LOGICAL :: is_print = .FALSE.
      
      real(KIND=core_rknd) :: diff_w_1
      real(KIND=core_rknd) :: diff_w_2
      real(KIND=core_rknd) :: diff_varnce_w_1
      real(KIND=core_rknd) :: diff_varnce_w_2
      real(KIND=core_rknd) :: diff_rt_1
      real(KIND=core_rknd) :: diff_rt_2
      real(KIND=core_rknd) :: diff_varnce_rt_1
      real(KIND=core_rknd) :: diff_varnce_rt_2
      real(KIND=core_rknd) :: diff_thl_1
      real(KIND=core_rknd) :: diff_thl_2
      real(KIND=core_rknd) :: diff_varnce_thl_1
      real(KIND=core_rknd) :: diff_varnce_thl_2
      real(KIND=core_rknd) :: diff_rrtthl
      real(KIND=core_rknd) :: diff_alpha_thl
      real(KIND=core_rknd) :: diff_alpha_rt
      real(KIND=core_rknd) :: diff_crt_1
      real(KIND=core_rknd) :: diff_crt_2
      real(KIND=core_rknd) :: diff_cthl_1
      real(KIND=core_rknd) :: diff_cthl_2
      real(KIND=core_rknd) :: diff_chi_1
      real(KIND=core_rknd) :: diff_chi_2
      real(KIND=core_rknd) :: diff_stdev_chi_1
      real(KIND=core_rknd) :: diff_stdev_chi_2
      real(KIND=core_rknd) :: diff_stdev_eta_1
      real(KIND=core_rknd) :: diff_stdev_eta_2
      real(KIND=core_rknd) :: diff_covar_chi_eta_1
      real(KIND=core_rknd) :: diff_covar_chi_eta_2
      real(KIND=core_rknd) :: diff_corr_chi_eta_1
      real(KIND=core_rknd) :: diff_corr_chi_eta_2
      real(KIND=core_rknd) :: diff_rsatl_1
      real(KIND=core_rknd) :: diff_rsatl_2
      real(KIND=core_rknd) :: diff_rc_1
      real(KIND=core_rknd) :: diff_rc_2
      real(KIND=core_rknd) :: diff_cloud_frac_1
      real(KIND=core_rknd) :: diff_cloud_frac_2
      real(KIND=core_rknd) :: diff_mixt_frac
      
      check_status%numTotal = check_status%numTotal + 1
      
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%w_1 == kgenref_var%w_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%w_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_w_1 = ABS(var%w_1 - kgenref_var%w_1)
          IF (diff_w_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%w_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%w_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_w_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_w_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%w_2 == kgenref_var%w_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%w_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_w_2 = ABS(var%w_2 - kgenref_var%w_2)
          IF (diff_w_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%w_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%w_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_w_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_w_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%varnce_w_1 == kgenref_var%varnce_w_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%varnce_w_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_varnce_w_1 = ABS(var%varnce_w_1 - kgenref_var%varnce_w_1)
          IF (diff_varnce_w_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_w_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_w_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_w_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_w_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%varnce_w_2 == kgenref_var%varnce_w_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%varnce_w_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_varnce_w_2 = ABS(var%varnce_w_2 - kgenref_var%varnce_w_2)
          IF (diff_varnce_w_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_w_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_w_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_w_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_w_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rt_1 == kgenref_var%rt_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rt_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rt_1 = ABS(var%rt_1 - kgenref_var%rt_1)
          IF (diff_rt_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rt_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rt_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rt_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rt_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rt_2 == kgenref_var%rt_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rt_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rt_2 = ABS(var%rt_2 - kgenref_var%rt_2)
          IF (diff_rt_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rt_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rt_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rt_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rt_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%varnce_rt_1 == kgenref_var%varnce_rt_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_varnce_rt_1 = ABS(var%varnce_rt_1 - kgenref_var%varnce_rt_1)
          IF (diff_varnce_rt_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_rt_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_rt_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%varnce_rt_2 == kgenref_var%varnce_rt_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_varnce_rt_2 = ABS(var%varnce_rt_2 - kgenref_var%varnce_rt_2)
          IF (diff_varnce_rt_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_rt_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_rt_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%thl_1 == kgenref_var%thl_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%thl_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_thl_1 = ABS(var%thl_1 - kgenref_var%thl_1)
          IF (diff_thl_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%thl_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%thl_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_thl_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_thl_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%thl_2 == kgenref_var%thl_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%thl_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_thl_2 = ABS(var%thl_2 - kgenref_var%thl_2)
          IF (diff_thl_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%thl_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%thl_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_thl_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_thl_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%varnce_thl_1 == kgenref_var%varnce_thl_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_varnce_thl_1 = ABS(var%varnce_thl_1 - kgenref_var%varnce_thl_1)
          IF (diff_varnce_thl_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_thl_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_thl_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%varnce_thl_2 == kgenref_var%varnce_thl_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_varnce_thl_2 = ABS(var%varnce_thl_2 - kgenref_var%varnce_thl_2)
          IF (diff_varnce_thl_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_thl_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_varnce_thl_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rrtthl == kgenref_var%rrtthl) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rrtthl is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rrtthl = ABS(var%rrtthl - kgenref_var%rrtthl)
          IF (diff_rrtthl <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rrtthl is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rrtthl is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rrtthl
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rrtthl
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%alpha_thl == kgenref_var%alpha_thl) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%alpha_thl is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_alpha_thl = ABS(var%alpha_thl - kgenref_var%alpha_thl)
          IF (diff_alpha_thl <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%alpha_thl is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%alpha_thl is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_alpha_thl
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_alpha_thl
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%alpha_rt == kgenref_var%alpha_rt) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%alpha_rt is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_alpha_rt = ABS(var%alpha_rt - kgenref_var%alpha_rt)
          IF (diff_alpha_rt <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%alpha_rt is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%alpha_rt is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_alpha_rt
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_alpha_rt
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%crt_1 == kgenref_var%crt_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%crt_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_crt_1 = ABS(var%crt_1 - kgenref_var%crt_1)
          IF (diff_crt_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%crt_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%crt_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_crt_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_crt_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%crt_2 == kgenref_var%crt_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%crt_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_crt_2 = ABS(var%crt_2 - kgenref_var%crt_2)
          IF (diff_crt_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%crt_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%crt_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_crt_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_crt_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%cthl_1 == kgenref_var%cthl_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%cthl_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_cthl_1 = ABS(var%cthl_1 - kgenref_var%cthl_1)
          IF (diff_cthl_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cthl_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cthl_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cthl_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cthl_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%cthl_2 == kgenref_var%cthl_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%cthl_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_cthl_2 = ABS(var%cthl_2 - kgenref_var%cthl_2)
          IF (diff_cthl_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cthl_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cthl_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cthl_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cthl_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%chi_1 == kgenref_var%chi_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%chi_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_chi_1 = ABS(var%chi_1 - kgenref_var%chi_1)
          IF (diff_chi_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%chi_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%chi_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_chi_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_chi_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%chi_2 == kgenref_var%chi_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%chi_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_chi_2 = ABS(var%chi_2 - kgenref_var%chi_2)
          IF (diff_chi_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%chi_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%chi_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_chi_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_chi_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%stdev_chi_1 == kgenref_var%stdev_chi_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_stdev_chi_1 = ABS(var%stdev_chi_1 - kgenref_var%stdev_chi_1)
          IF (diff_stdev_chi_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_chi_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_chi_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%stdev_chi_2 == kgenref_var%stdev_chi_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_stdev_chi_2 = ABS(var%stdev_chi_2 - kgenref_var%stdev_chi_2)
          IF (diff_stdev_chi_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_chi_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_chi_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%stdev_eta_1 == kgenref_var%stdev_eta_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_stdev_eta_1 = ABS(var%stdev_eta_1 - kgenref_var%stdev_eta_1)
          IF (diff_stdev_eta_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_eta_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_eta_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%stdev_eta_2 == kgenref_var%stdev_eta_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_stdev_eta_2 = ABS(var%stdev_eta_2 - kgenref_var%stdev_eta_2)
          IF (diff_stdev_eta_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_eta_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_stdev_eta_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%covar_chi_eta_1 == kgenref_var%covar_chi_eta_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_covar_chi_eta_1 = ABS(var%covar_chi_eta_1 - kgenref_var%covar_chi_eta_1)
          IF (diff_covar_chi_eta_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_covar_chi_eta_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_covar_chi_eta_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%covar_chi_eta_2 == kgenref_var%covar_chi_eta_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_covar_chi_eta_2 = ABS(var%covar_chi_eta_2 - kgenref_var%covar_chi_eta_2)
          IF (diff_covar_chi_eta_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_covar_chi_eta_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_covar_chi_eta_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%corr_chi_eta_1 == kgenref_var%corr_chi_eta_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_corr_chi_eta_1 = ABS(var%corr_chi_eta_1 - kgenref_var%corr_chi_eta_1)
          IF (diff_corr_chi_eta_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_corr_chi_eta_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_corr_chi_eta_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%corr_chi_eta_2 == kgenref_var%corr_chi_eta_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_corr_chi_eta_2 = ABS(var%corr_chi_eta_2 - kgenref_var%corr_chi_eta_2)
          IF (diff_corr_chi_eta_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_corr_chi_eta_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_corr_chi_eta_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rsatl_1 == kgenref_var%rsatl_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rsatl_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rsatl_1 = ABS(var%rsatl_1 - kgenref_var%rsatl_1)
          IF (diff_rsatl_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rsatl_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rsatl_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rsatl_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rsatl_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rsatl_2 == kgenref_var%rsatl_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rsatl_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rsatl_2 = ABS(var%rsatl_2 - kgenref_var%rsatl_2)
          IF (diff_rsatl_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rsatl_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rsatl_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rsatl_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rsatl_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rc_1 == kgenref_var%rc_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rc_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rc_1 = ABS(var%rc_1 - kgenref_var%rc_1)
          IF (diff_rc_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rc_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rc_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rc_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rc_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%rc_2 == kgenref_var%rc_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%rc_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_rc_2 = ABS(var%rc_2 - kgenref_var%rc_2)
          IF (diff_rc_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rc_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%rc_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rc_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_rc_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%cloud_frac_1 == kgenref_var%cloud_frac_1) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_1 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_cloud_frac_1 = ABS(var%cloud_frac_1 - kgenref_var%cloud_frac_1)
          IF (diff_cloud_frac_1 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_1 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_1 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cloud_frac_1
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cloud_frac_1
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%cloud_frac_2 == kgenref_var%cloud_frac_2) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_2 is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_cloud_frac_2 = ABS(var%cloud_frac_2 - kgenref_var%cloud_frac_2)
          IF (diff_cloud_frac_2 <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_2 is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_2 is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cloud_frac_2
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_cloud_frac_2
              WRITE (*, *) ""
          END IF 
      END IF 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1
      IF (var%mixt_frac == kgenref_var%mixt_frac) THEN
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) trim(adjustl(varname)), "%mixt_frac is IDENTICAL."
          END IF 
          check_result = CHECK_IDENTICAL
      ELSE
          diff_mixt_frac = ABS(var%mixt_frac - kgenref_var%mixt_frac)
          IF (diff_mixt_frac <= dtype_check_status%tolerance) THEN
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%mixt_frac is NOT IDENTICAL(within tolerance)."
              END IF 
              check_result = CHECK_IN_TOL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%mixt_frac is NOT IDENTICAL(out of tolerance)."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
      END IF 
      IF (check_result == CHECK_IDENTICAL) THEN
      ELSE IF (check_result == CHECK_OUT_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_mixt_frac
              WRITE (*, *) ""
          END IF 
      ELSE IF (check_result == CHECK_IN_TOL) THEN
          IF (check_status%verboseLevel > 2) THEN
              WRITE (*, *) "Difference is ", diff_mixt_frac
              WRITE (*, *) ""
          END IF 
      END IF 
      
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN
          check_status%numIdentical = check_status%numIdentical + 1
      ELSE IF (dtype_check_status%numOutTol > 0) THEN
          check_status%numOutTol = check_status%numOutTol + 1
      ELSE IF (dtype_check_status%numInTol > 0) THEN
          check_status%numInTol = check_status%numInTol + 1
      END IF 
  END SUBROUTINE kv_pdf_parameter_module_pdf_parameter
  
end module pdf_parameter_module