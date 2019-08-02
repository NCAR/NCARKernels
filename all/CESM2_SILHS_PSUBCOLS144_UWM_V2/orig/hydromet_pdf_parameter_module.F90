!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:54 
!KGEN version : 0.8.1 
  
!---------------------------------------------------------------------------
! $Id$
!===============================================================================


module hydromet_pdf_parameter_module
  ! Description:
  ! This module defines the derived type hydromet_pdf_parameter.
  ! References:
  !   None
  !-------------------------------------------------------------------------


    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC hydromet_pdf_parameter 

  integer, parameter, private :: &
    max_hydromet_dim = 8

  type hydromet_pdf_parameter

    real( kind = core_rknd ), dimension(max_hydromet_dim) :: &
      hm_1,          & ! Mean of hydrometeor, hm (1st PDF component)   [un vary]
      hm_2,          & ! Mean of hydrometeor, hm (2nd PDF component)   [un vary]
      mu_hm_1,       & ! Mean of hm (1st PDF component) in-precip (ip) [un vary]
      mu_hm_2,       & ! Mean of hm (2nd PDF component) ip             [un vary]
      sigma_hm_1,    & ! Standard deviation of hm (1st PDF comp.) ip   [un vary]
      sigma_hm_2,    & ! Standard deviation of hm (2nd PDF comp.) ip   [un vary]
      corr_w_hm_1,   & ! Correlation of w and hm (1st PDF component) ip      [-]
      corr_w_hm_2,   & ! Correlation of w and hm (2nd PDF component) ip      [-]
      corr_chi_hm_1, & ! Correlation of chi and hm (1st PDF component) ip    [-]
      corr_chi_hm_2, & ! Correlation of chi and hm (2nd PDF component) ip    [-]
      corr_eta_hm_1, & ! Correlation of eta and hm (1st PDF component) ip    [-]
      corr_eta_hm_2    ! Correlation of eta and hm (2nd PDF component) ip    [-]

    real( kind = core_rknd ), dimension(max_hydromet_dim,max_hydromet_dim) :: &
      corr_hmx_hmy_1, & ! Correlation of hmx and hmy (1st PDF component) ip  [-]
      corr_hmx_hmy_2    ! Correlation of hmx and hmy (2nd PDF component) ip  [-]

    real( kind = core_rknd ) :: &
      mu_Ncn_1,    & ! Mean of Ncn (1st PDF component)                  [num/kg]
      mu_Ncn_2,    & ! Mean of Ncn (2nd PDF component)                  [num/kg]
      sigma_Ncn_1, & ! Standard deviation of Ncn (1st PDF component)    [num/kg]
      sigma_Ncn_2    ! Standard deviation of Ncn (2nd PDF component)    [num/kg]

    real( kind = core_rknd ) :: &
      precip_frac,   & ! Precipitation fraction (overall)           [-]
      precip_frac_1, & ! Precipitation fraction (1st PDF component) [-]
      precip_frac_2    ! Precipitation fraction (2nd PDF component) [-]

  end type hydromet_pdf_parameter
  PUBLIC kr_kgen_hydromet_pdf_parameter_module_typesubp0 
  PUBLIC kv_kgen_hydromet_pdf_parameter_module_typesubp0 

  !=============================================================================
    
  CONTAINS 
    


!===============================================================================


  !read state subroutine for kr_kgen_hydromet_pdf_parameter_module_typesubp0 
  RECURSIVE SUBROUTINE kr_kgen_hydromet_pdf_parameter_module_typesubp0(var, kgen_unit, printname, printvar) 
      TYPE(hydromet_pdf_parameter), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%hm_1 
          CALL kgen_array_sumcheck(printname // "%hm_1", kgen_array_sum, DBLE(SUM(var%hm_1, mask=(var%hm_1 .eq. var%hm_1))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%hm_1)) = ", DBLE(SUM(var%hm_1, mask=(var%hm_1 .eq. &
              &var%hm_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%hm_2 
          CALL kgen_array_sumcheck(printname // "%hm_2", kgen_array_sum, DBLE(SUM(var%hm_2, mask=(var%hm_2 .eq. var%hm_2))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%hm_2)) = ", DBLE(SUM(var%hm_2, mask=(var%hm_2 .eq. &
              &var%hm_2))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%mu_hm_1 
          CALL kgen_array_sumcheck(printname // "%mu_hm_1", kgen_array_sum, DBLE(SUM(var%mu_hm_1, mask=(var%mu_hm_1 .eq. &
          &var%mu_hm_1))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%mu_hm_1)) = ", DBLE(SUM(var%mu_hm_1, mask=(var%mu_hm_1 .eq. &
              &var%mu_hm_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%mu_hm_2 
          CALL kgen_array_sumcheck(printname // "%mu_hm_2", kgen_array_sum, DBLE(SUM(var%mu_hm_2, mask=(var%mu_hm_2 .eq. &
          &var%mu_hm_2))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%mu_hm_2)) = ", DBLE(SUM(var%mu_hm_2, mask=(var%mu_hm_2 .eq. &
              &var%mu_hm_2))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%sigma_hm_1 
          CALL kgen_array_sumcheck(printname // "%sigma_hm_1", kgen_array_sum, DBLE(SUM(var%sigma_hm_1, mask=(var%sigma_hm_1 .eq. &
          &var%sigma_hm_1))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%sigma_hm_1)) = ", DBLE(SUM(var%sigma_hm_1, &
              &mask=(var%sigma_hm_1 .eq. var%sigma_hm_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%sigma_hm_2 
          CALL kgen_array_sumcheck(printname // "%sigma_hm_2", kgen_array_sum, DBLE(SUM(var%sigma_hm_2, mask=(var%sigma_hm_2 .eq. &
          &var%sigma_hm_2))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%sigma_hm_2)) = ", DBLE(SUM(var%sigma_hm_2, &
              &mask=(var%sigma_hm_2 .eq. var%sigma_hm_2))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_w_hm_1 
          CALL kgen_array_sumcheck(printname // "%corr_w_hm_1", kgen_array_sum, DBLE(SUM(var%corr_w_hm_1, mask=(var%corr_w_hm_1 &
          &.eq. var%corr_w_hm_1))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_w_hm_1)) = ", DBLE(SUM(var%corr_w_hm_1, &
              &mask=(var%corr_w_hm_1 .eq. var%corr_w_hm_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_w_hm_2 
          CALL kgen_array_sumcheck(printname // "%corr_w_hm_2", kgen_array_sum, DBLE(SUM(var%corr_w_hm_2, mask=(var%corr_w_hm_2 &
          &.eq. var%corr_w_hm_2))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_w_hm_2)) = ", DBLE(SUM(var%corr_w_hm_2, &
              &mask=(var%corr_w_hm_2 .eq. var%corr_w_hm_2))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_chi_hm_1 
          CALL kgen_array_sumcheck(printname // "%corr_chi_hm_1", kgen_array_sum, DBLE(SUM(var%corr_chi_hm_1, &
          &mask=(var%corr_chi_hm_1 .eq. var%corr_chi_hm_1))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_chi_hm_1)) = ", DBLE(SUM(var%corr_chi_hm_1, &
              &mask=(var%corr_chi_hm_1 .eq. var%corr_chi_hm_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_chi_hm_2 
          CALL kgen_array_sumcheck(printname // "%corr_chi_hm_2", kgen_array_sum, DBLE(SUM(var%corr_chi_hm_2, &
          &mask=(var%corr_chi_hm_2 .eq. var%corr_chi_hm_2))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_chi_hm_2)) = ", DBLE(SUM(var%corr_chi_hm_2, &
              &mask=(var%corr_chi_hm_2 .eq. var%corr_chi_hm_2))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_eta_hm_1 
          CALL kgen_array_sumcheck(printname // "%corr_eta_hm_1", kgen_array_sum, DBLE(SUM(var%corr_eta_hm_1, &
          &mask=(var%corr_eta_hm_1 .eq. var%corr_eta_hm_1))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_eta_hm_1)) = ", DBLE(SUM(var%corr_eta_hm_1, &
              &mask=(var%corr_eta_hm_1 .eq. var%corr_eta_hm_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_eta_hm_2 
          CALL kgen_array_sumcheck(printname // "%corr_eta_hm_2", kgen_array_sum, DBLE(SUM(var%corr_eta_hm_2, &
          &mask=(var%corr_eta_hm_2 .eq. var%corr_eta_hm_2))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_eta_hm_2)) = ", DBLE(SUM(var%corr_eta_hm_2, &
              &mask=(var%corr_eta_hm_2 .eq. var%corr_eta_hm_2))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_hmx_hmy_1 
          CALL kgen_array_sumcheck(printname // "%corr_hmx_hmy_1", kgen_array_sum, DBLE(SUM(var%corr_hmx_hmy_1, &
          &mask=(var%corr_hmx_hmy_1 .eq. var%corr_hmx_hmy_1))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_hmx_hmy_1)) = ", DBLE(SUM(var%corr_hmx_hmy_1, &
              &mask=(var%corr_hmx_hmy_1 .eq. var%corr_hmx_hmy_1))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%corr_hmx_hmy_2 
          CALL kgen_array_sumcheck(printname // "%corr_hmx_hmy_2", kgen_array_sum, DBLE(SUM(var%corr_hmx_hmy_2, &
          &mask=(var%corr_hmx_hmy_2 .eq. var%corr_hmx_hmy_2))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%corr_hmx_hmy_2)) = ", DBLE(SUM(var%corr_hmx_hmy_2, &
              &mask=(var%corr_hmx_hmy_2 .eq. var%corr_hmx_hmy_2))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%mu_ncn_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%mu_ncn_1 = ", var%mu_ncn_1 
      END IF   
      READ (UNIT = kgen_unit) var%mu_ncn_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%mu_ncn_2 = ", var%mu_ncn_2 
      END IF   
      READ (UNIT = kgen_unit) var%sigma_ncn_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%sigma_ncn_1 = ", var%sigma_ncn_1 
      END IF   
      READ (UNIT = kgen_unit) var%sigma_ncn_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%sigma_ncn_2 = ", var%sigma_ncn_2 
      END IF   
        
      READ (UNIT = kgen_unit) var%precip_frac 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%precip_frac = ", var%precip_frac 
      END IF   
      READ (UNIT = kgen_unit) var%precip_frac_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%precip_frac_1 = ", var%precip_frac_1 
      END IF   
      READ (UNIT = kgen_unit) var%precip_frac_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%precip_frac_2 = ", var%precip_frac_2 
      END IF   
        
  END SUBROUTINE kr_kgen_hydromet_pdf_parameter_module_typesubp0 
    
  !verify state subroutine for kv_kgen_hydromet_pdf_parameter_module_typesubp0 
  RECURSIVE SUBROUTINE kv_kgen_hydromet_pdf_parameter_module_typesubp0(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(hydromet_pdf_parameter), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_hm_1 
      real(KIND=core_rknd) :: nrmsdiff_hm_1, rmsdiff_hm_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_hm_1(:), buf2_hm_1(:) 
      INTEGER :: n_hm_2 
      real(KIND=core_rknd) :: nrmsdiff_hm_2, rmsdiff_hm_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_hm_2(:), buf2_hm_2(:) 
      INTEGER :: n_mu_hm_1 
      real(KIND=core_rknd) :: nrmsdiff_mu_hm_1, rmsdiff_mu_hm_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_mu_hm_1(:), buf2_mu_hm_1(:) 
      INTEGER :: n_mu_hm_2 
      real(KIND=core_rknd) :: nrmsdiff_mu_hm_2, rmsdiff_mu_hm_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_mu_hm_2(:), buf2_mu_hm_2(:) 
      INTEGER :: n_sigma_hm_1 
      real(KIND=core_rknd) :: nrmsdiff_sigma_hm_1, rmsdiff_sigma_hm_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_sigma_hm_1(:), buf2_sigma_hm_1(:) 
      INTEGER :: n_sigma_hm_2 
      real(KIND=core_rknd) :: nrmsdiff_sigma_hm_2, rmsdiff_sigma_hm_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_sigma_hm_2(:), buf2_sigma_hm_2(:) 
      INTEGER :: n_corr_w_hm_1 
      real(KIND=core_rknd) :: nrmsdiff_corr_w_hm_1, rmsdiff_corr_w_hm_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_w_hm_1(:), buf2_corr_w_hm_1(:) 
      INTEGER :: n_corr_w_hm_2 
      real(KIND=core_rknd) :: nrmsdiff_corr_w_hm_2, rmsdiff_corr_w_hm_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_w_hm_2(:), buf2_corr_w_hm_2(:) 
      INTEGER :: n_corr_chi_hm_1 
      real(KIND=core_rknd) :: nrmsdiff_corr_chi_hm_1, rmsdiff_corr_chi_hm_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_chi_hm_1(:), buf2_corr_chi_hm_1(:) 
      INTEGER :: n_corr_chi_hm_2 
      real(KIND=core_rknd) :: nrmsdiff_corr_chi_hm_2, rmsdiff_corr_chi_hm_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_chi_hm_2(:), buf2_corr_chi_hm_2(:) 
      INTEGER :: n_corr_eta_hm_1 
      real(KIND=core_rknd) :: nrmsdiff_corr_eta_hm_1, rmsdiff_corr_eta_hm_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_eta_hm_1(:), buf2_corr_eta_hm_1(:) 
      INTEGER :: n_corr_eta_hm_2 
      real(KIND=core_rknd) :: nrmsdiff_corr_eta_hm_2, rmsdiff_corr_eta_hm_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_eta_hm_2(:), buf2_corr_eta_hm_2(:) 
      INTEGER :: n_corr_hmx_hmy_1 
      real(KIND=core_rknd) :: nrmsdiff_corr_hmx_hmy_1, rmsdiff_corr_hmx_hmy_1 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_hmx_hmy_1(:,:), buf2_corr_hmx_hmy_1(:,:) 
      INTEGER :: n_corr_hmx_hmy_2 
      real(KIND=core_rknd) :: nrmsdiff_corr_hmx_hmy_2, rmsdiff_corr_hmx_hmy_2 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_corr_hmx_hmy_2(:,:), buf2_corr_hmx_hmy_2(:,:) 
      real(KIND=core_rknd) :: diff_mu_ncn_1 
      real(KIND=core_rknd) :: diff_mu_ncn_2 
      real(KIND=core_rknd) :: diff_sigma_ncn_1 
      real(KIND=core_rknd) :: diff_sigma_ncn_2 
      real(KIND=core_rknd) :: diff_precip_frac 
      real(KIND=core_rknd) :: diff_precip_frac_1 
      real(KIND=core_rknd) :: diff_precip_frac_2 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%hm_1 == kgenref_var%hm_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%hm_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_hm_1(SIZE(var%hm_1,dim=1))) 
          ALLOCATE (buf2_hm_1(SIZE(var%hm_1,dim=1))) 
          n_hm_1 = COUNT(var%hm_1 /= kgenref_var%hm_1) 
          WHERE ( ABS(kgenref_var%hm_1) > kgen_minvalue ) 
              buf1_hm_1 = ((var%hm_1-kgenref_var%hm_1)/kgenref_var%hm_1)**2 
              buf2_hm_1 = (var%hm_1-kgenref_var%hm_1)**2 
          ELSEWHERE 
              buf1_hm_1 = (var%hm_1-kgenref_var%hm_1)**2 
              buf2_hm_1 = buf1_hm_1 
          END WHERE   
          nrmsdiff_hm_1 = SQRT(SUM(buf1_hm_1)/REAL(n_hm_1)) 
          rmsdiff_hm_1 = SQRT(SUM(buf2_hm_1)/REAL(n_hm_1)) 
          IF (rmsdiff_hm_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%hm_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%hm_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%hm_1 /= kgenref_var%hm_1), " of ", size( var%hm_1 ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%hm_1)/real(size(var%hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%hm_1)/real(size(kgenref_var%hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%hm_1 /= kgenref_var%hm_1), " of ", size( var%hm_1 ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%hm_1)/real(size(var%hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%hm_1)/real(size(kgenref_var%hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%hm_2 == kgenref_var%hm_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%hm_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_hm_2(SIZE(var%hm_2,dim=1))) 
          ALLOCATE (buf2_hm_2(SIZE(var%hm_2,dim=1))) 
          n_hm_2 = COUNT(var%hm_2 /= kgenref_var%hm_2) 
          WHERE ( ABS(kgenref_var%hm_2) > kgen_minvalue ) 
              buf1_hm_2 = ((var%hm_2-kgenref_var%hm_2)/kgenref_var%hm_2)**2 
              buf2_hm_2 = (var%hm_2-kgenref_var%hm_2)**2 
          ELSEWHERE 
              buf1_hm_2 = (var%hm_2-kgenref_var%hm_2)**2 
              buf2_hm_2 = buf1_hm_2 
          END WHERE   
          nrmsdiff_hm_2 = SQRT(SUM(buf1_hm_2)/REAL(n_hm_2)) 
          rmsdiff_hm_2 = SQRT(SUM(buf2_hm_2)/REAL(n_hm_2)) 
          IF (rmsdiff_hm_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%hm_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%hm_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%hm_2 /= kgenref_var%hm_2), " of ", size( var%hm_2 ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%hm_2)/real(size(var%hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%hm_2)/real(size(kgenref_var%hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%hm_2 /= kgenref_var%hm_2), " of ", size( var%hm_2 ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%hm_2)/real(size(var%hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%hm_2)/real(size(kgenref_var%hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%mu_hm_1 == kgenref_var%mu_hm_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mu_hm_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_mu_hm_1(SIZE(var%mu_hm_1,dim=1))) 
          ALLOCATE (buf2_mu_hm_1(SIZE(var%mu_hm_1,dim=1))) 
          n_mu_hm_1 = COUNT(var%mu_hm_1 /= kgenref_var%mu_hm_1) 
          WHERE ( ABS(kgenref_var%mu_hm_1) > kgen_minvalue ) 
              buf1_mu_hm_1 = ((var%mu_hm_1-kgenref_var%mu_hm_1)/kgenref_var%mu_hm_1)**2 
              buf2_mu_hm_1 = (var%mu_hm_1-kgenref_var%mu_hm_1)**2 
          ELSEWHERE 
              buf1_mu_hm_1 = (var%mu_hm_1-kgenref_var%mu_hm_1)**2 
              buf2_mu_hm_1 = buf1_mu_hm_1 
          END WHERE   
          nrmsdiff_mu_hm_1 = SQRT(SUM(buf1_mu_hm_1)/REAL(n_mu_hm_1)) 
          rmsdiff_mu_hm_1 = SQRT(SUM(buf2_mu_hm_1)/REAL(n_mu_hm_1)) 
          IF (rmsdiff_mu_hm_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_hm_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_hm_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%mu_hm_1 /= kgenref_var%mu_hm_1), " of ", size( var%mu_hm_1 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%mu_hm_1)/real(size(var%mu_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%mu_hm_1)/real(size(kgenref_var%mu_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_mu_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mu_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%mu_hm_1 /= kgenref_var%mu_hm_1), " of ", size( var%mu_hm_1 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%mu_hm_1)/real(size(var%mu_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%mu_hm_1)/real(size(kgenref_var%mu_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_mu_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mu_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%mu_hm_2 == kgenref_var%mu_hm_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mu_hm_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_mu_hm_2(SIZE(var%mu_hm_2,dim=1))) 
          ALLOCATE (buf2_mu_hm_2(SIZE(var%mu_hm_2,dim=1))) 
          n_mu_hm_2 = COUNT(var%mu_hm_2 /= kgenref_var%mu_hm_2) 
          WHERE ( ABS(kgenref_var%mu_hm_2) > kgen_minvalue ) 
              buf1_mu_hm_2 = ((var%mu_hm_2-kgenref_var%mu_hm_2)/kgenref_var%mu_hm_2)**2 
              buf2_mu_hm_2 = (var%mu_hm_2-kgenref_var%mu_hm_2)**2 
          ELSEWHERE 
              buf1_mu_hm_2 = (var%mu_hm_2-kgenref_var%mu_hm_2)**2 
              buf2_mu_hm_2 = buf1_mu_hm_2 
          END WHERE   
          nrmsdiff_mu_hm_2 = SQRT(SUM(buf1_mu_hm_2)/REAL(n_mu_hm_2)) 
          rmsdiff_mu_hm_2 = SQRT(SUM(buf2_mu_hm_2)/REAL(n_mu_hm_2)) 
          IF (rmsdiff_mu_hm_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_hm_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_hm_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%mu_hm_2 /= kgenref_var%mu_hm_2), " of ", size( var%mu_hm_2 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%mu_hm_2)/real(size(var%mu_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%mu_hm_2)/real(size(kgenref_var%mu_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_mu_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mu_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%mu_hm_2 /= kgenref_var%mu_hm_2), " of ", size( var%mu_hm_2 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%mu_hm_2)/real(size(var%mu_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%mu_hm_2)/real(size(kgenref_var%mu_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_mu_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mu_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%sigma_hm_1 == kgenref_var%sigma_hm_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sigma_hm_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_sigma_hm_1(SIZE(var%sigma_hm_1,dim=1))) 
          ALLOCATE (buf2_sigma_hm_1(SIZE(var%sigma_hm_1,dim=1))) 
          n_sigma_hm_1 = COUNT(var%sigma_hm_1 /= kgenref_var%sigma_hm_1) 
          WHERE ( ABS(kgenref_var%sigma_hm_1) > kgen_minvalue ) 
              buf1_sigma_hm_1 = ((var%sigma_hm_1-kgenref_var%sigma_hm_1)/kgenref_var%sigma_hm_1)**2 
              buf2_sigma_hm_1 = (var%sigma_hm_1-kgenref_var%sigma_hm_1)**2 
          ELSEWHERE 
              buf1_sigma_hm_1 = (var%sigma_hm_1-kgenref_var%sigma_hm_1)**2 
              buf2_sigma_hm_1 = buf1_sigma_hm_1 
          END WHERE   
          nrmsdiff_sigma_hm_1 = SQRT(SUM(buf1_sigma_hm_1)/REAL(n_sigma_hm_1)) 
          rmsdiff_sigma_hm_1 = SQRT(SUM(buf2_sigma_hm_1)/REAL(n_sigma_hm_1)) 
          IF (rmsdiff_sigma_hm_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_hm_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_hm_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%sigma_hm_1 /= kgenref_var%sigma_hm_1), " of ", size( var%sigma_hm_1 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%sigma_hm_1)/real(size(var%sigma_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%sigma_hm_1)/real(size(kgenref_var%sigma_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_sigma_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sigma_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%sigma_hm_1 /= kgenref_var%sigma_hm_1), " of ", size( var%sigma_hm_1 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%sigma_hm_1)/real(size(var%sigma_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%sigma_hm_1)/real(size(kgenref_var%sigma_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_sigma_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sigma_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%sigma_hm_2 == kgenref_var%sigma_hm_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sigma_hm_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_sigma_hm_2(SIZE(var%sigma_hm_2,dim=1))) 
          ALLOCATE (buf2_sigma_hm_2(SIZE(var%sigma_hm_2,dim=1))) 
          n_sigma_hm_2 = COUNT(var%sigma_hm_2 /= kgenref_var%sigma_hm_2) 
          WHERE ( ABS(kgenref_var%sigma_hm_2) > kgen_minvalue ) 
              buf1_sigma_hm_2 = ((var%sigma_hm_2-kgenref_var%sigma_hm_2)/kgenref_var%sigma_hm_2)**2 
              buf2_sigma_hm_2 = (var%sigma_hm_2-kgenref_var%sigma_hm_2)**2 
          ELSEWHERE 
              buf1_sigma_hm_2 = (var%sigma_hm_2-kgenref_var%sigma_hm_2)**2 
              buf2_sigma_hm_2 = buf1_sigma_hm_2 
          END WHERE   
          nrmsdiff_sigma_hm_2 = SQRT(SUM(buf1_sigma_hm_2)/REAL(n_sigma_hm_2)) 
          rmsdiff_sigma_hm_2 = SQRT(SUM(buf2_sigma_hm_2)/REAL(n_sigma_hm_2)) 
          IF (rmsdiff_sigma_hm_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_hm_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_hm_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%sigma_hm_2 /= kgenref_var%sigma_hm_2), " of ", size( var%sigma_hm_2 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%sigma_hm_2)/real(size(var%sigma_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%sigma_hm_2)/real(size(kgenref_var%sigma_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_sigma_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sigma_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%sigma_hm_2 /= kgenref_var%sigma_hm_2), " of ", size( var%sigma_hm_2 ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%sigma_hm_2)/real(size(var%sigma_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%sigma_hm_2)/real(size(kgenref_var%sigma_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_sigma_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sigma_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_w_hm_1 == kgenref_var%corr_w_hm_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_hm_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_w_hm_1(SIZE(var%corr_w_hm_1,dim=1))) 
          ALLOCATE (buf2_corr_w_hm_1(SIZE(var%corr_w_hm_1,dim=1))) 
          n_corr_w_hm_1 = COUNT(var%corr_w_hm_1 /= kgenref_var%corr_w_hm_1) 
          WHERE ( ABS(kgenref_var%corr_w_hm_1) > kgen_minvalue ) 
              buf1_corr_w_hm_1 = ((var%corr_w_hm_1-kgenref_var%corr_w_hm_1)/kgenref_var%corr_w_hm_1)**2 
              buf2_corr_w_hm_1 = (var%corr_w_hm_1-kgenref_var%corr_w_hm_1)**2 
          ELSEWHERE 
              buf1_corr_w_hm_1 = (var%corr_w_hm_1-kgenref_var%corr_w_hm_1)**2 
              buf2_corr_w_hm_1 = buf1_corr_w_hm_1 
          END WHERE   
          nrmsdiff_corr_w_hm_1 = SQRT(SUM(buf1_corr_w_hm_1)/REAL(n_corr_w_hm_1)) 
          rmsdiff_corr_w_hm_1 = SQRT(SUM(buf2_corr_w_hm_1)/REAL(n_corr_w_hm_1)) 
          IF (rmsdiff_corr_w_hm_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_hm_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_hm_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_w_hm_1 /= kgenref_var%corr_w_hm_1), " of ", size( var%corr_w_hm_1 ), " elements &
                  &are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_w_hm_1)/real(size(var%corr_w_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_w_hm_1)/real(size(kgenref_var%corr_w_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_w_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_w_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_w_hm_1 /= kgenref_var%corr_w_hm_1), " of ", size( var%corr_w_hm_1 ), " elements &
                  &are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_w_hm_1)/real(size(var%corr_w_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_w_hm_1)/real(size(kgenref_var%corr_w_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_w_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_w_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_w_hm_2 == kgenref_var%corr_w_hm_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_hm_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_w_hm_2(SIZE(var%corr_w_hm_2,dim=1))) 
          ALLOCATE (buf2_corr_w_hm_2(SIZE(var%corr_w_hm_2,dim=1))) 
          n_corr_w_hm_2 = COUNT(var%corr_w_hm_2 /= kgenref_var%corr_w_hm_2) 
          WHERE ( ABS(kgenref_var%corr_w_hm_2) > kgen_minvalue ) 
              buf1_corr_w_hm_2 = ((var%corr_w_hm_2-kgenref_var%corr_w_hm_2)/kgenref_var%corr_w_hm_2)**2 
              buf2_corr_w_hm_2 = (var%corr_w_hm_2-kgenref_var%corr_w_hm_2)**2 
          ELSEWHERE 
              buf1_corr_w_hm_2 = (var%corr_w_hm_2-kgenref_var%corr_w_hm_2)**2 
              buf2_corr_w_hm_2 = buf1_corr_w_hm_2 
          END WHERE   
          nrmsdiff_corr_w_hm_2 = SQRT(SUM(buf1_corr_w_hm_2)/REAL(n_corr_w_hm_2)) 
          rmsdiff_corr_w_hm_2 = SQRT(SUM(buf2_corr_w_hm_2)/REAL(n_corr_w_hm_2)) 
          IF (rmsdiff_corr_w_hm_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_hm_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_hm_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_w_hm_2 /= kgenref_var%corr_w_hm_2), " of ", size( var%corr_w_hm_2 ), " elements &
                  &are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_w_hm_2)/real(size(var%corr_w_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_w_hm_2)/real(size(kgenref_var%corr_w_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_w_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_w_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_w_hm_2 /= kgenref_var%corr_w_hm_2), " of ", size( var%corr_w_hm_2 ), " elements &
                  &are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_w_hm_2)/real(size(var%corr_w_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_w_hm_2)/real(size(kgenref_var%corr_w_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_w_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_w_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_chi_hm_1 == kgenref_var%corr_chi_hm_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_hm_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_chi_hm_1(SIZE(var%corr_chi_hm_1,dim=1))) 
          ALLOCATE (buf2_corr_chi_hm_1(SIZE(var%corr_chi_hm_1,dim=1))) 
          n_corr_chi_hm_1 = COUNT(var%corr_chi_hm_1 /= kgenref_var%corr_chi_hm_1) 
          WHERE ( ABS(kgenref_var%corr_chi_hm_1) > kgen_minvalue ) 
              buf1_corr_chi_hm_1 = ((var%corr_chi_hm_1-kgenref_var%corr_chi_hm_1)/kgenref_var%corr_chi_hm_1)**2 
              buf2_corr_chi_hm_1 = (var%corr_chi_hm_1-kgenref_var%corr_chi_hm_1)**2 
          ELSEWHERE 
              buf1_corr_chi_hm_1 = (var%corr_chi_hm_1-kgenref_var%corr_chi_hm_1)**2 
              buf2_corr_chi_hm_1 = buf1_corr_chi_hm_1 
          END WHERE   
          nrmsdiff_corr_chi_hm_1 = SQRT(SUM(buf1_corr_chi_hm_1)/REAL(n_corr_chi_hm_1)) 
          rmsdiff_corr_chi_hm_1 = SQRT(SUM(buf2_corr_chi_hm_1)/REAL(n_corr_chi_hm_1)) 
          IF (rmsdiff_corr_chi_hm_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_hm_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_hm_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_chi_hm_1 /= kgenref_var%corr_chi_hm_1), " of ", size( var%corr_chi_hm_1 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_chi_hm_1)/real(size(var%corr_chi_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_chi_hm_1)/real(size(kgenref_var%corr_chi_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_chi_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_chi_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_chi_hm_1 /= kgenref_var%corr_chi_hm_1), " of ", size( var%corr_chi_hm_1 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_chi_hm_1)/real(size(var%corr_chi_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_chi_hm_1)/real(size(kgenref_var%corr_chi_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_chi_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_chi_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_chi_hm_2 == kgenref_var%corr_chi_hm_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_hm_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_chi_hm_2(SIZE(var%corr_chi_hm_2,dim=1))) 
          ALLOCATE (buf2_corr_chi_hm_2(SIZE(var%corr_chi_hm_2,dim=1))) 
          n_corr_chi_hm_2 = COUNT(var%corr_chi_hm_2 /= kgenref_var%corr_chi_hm_2) 
          WHERE ( ABS(kgenref_var%corr_chi_hm_2) > kgen_minvalue ) 
              buf1_corr_chi_hm_2 = ((var%corr_chi_hm_2-kgenref_var%corr_chi_hm_2)/kgenref_var%corr_chi_hm_2)**2 
              buf2_corr_chi_hm_2 = (var%corr_chi_hm_2-kgenref_var%corr_chi_hm_2)**2 
          ELSEWHERE 
              buf1_corr_chi_hm_2 = (var%corr_chi_hm_2-kgenref_var%corr_chi_hm_2)**2 
              buf2_corr_chi_hm_2 = buf1_corr_chi_hm_2 
          END WHERE   
          nrmsdiff_corr_chi_hm_2 = SQRT(SUM(buf1_corr_chi_hm_2)/REAL(n_corr_chi_hm_2)) 
          rmsdiff_corr_chi_hm_2 = SQRT(SUM(buf2_corr_chi_hm_2)/REAL(n_corr_chi_hm_2)) 
          IF (rmsdiff_corr_chi_hm_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_hm_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_hm_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_chi_hm_2 /= kgenref_var%corr_chi_hm_2), " of ", size( var%corr_chi_hm_2 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_chi_hm_2)/real(size(var%corr_chi_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_chi_hm_2)/real(size(kgenref_var%corr_chi_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_chi_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_chi_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_chi_hm_2 /= kgenref_var%corr_chi_hm_2), " of ", size( var%corr_chi_hm_2 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_chi_hm_2)/real(size(var%corr_chi_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_chi_hm_2)/real(size(kgenref_var%corr_chi_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_chi_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_chi_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_eta_hm_1 == kgenref_var%corr_eta_hm_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_eta_hm_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_eta_hm_1(SIZE(var%corr_eta_hm_1,dim=1))) 
          ALLOCATE (buf2_corr_eta_hm_1(SIZE(var%corr_eta_hm_1,dim=1))) 
          n_corr_eta_hm_1 = COUNT(var%corr_eta_hm_1 /= kgenref_var%corr_eta_hm_1) 
          WHERE ( ABS(kgenref_var%corr_eta_hm_1) > kgen_minvalue ) 
              buf1_corr_eta_hm_1 = ((var%corr_eta_hm_1-kgenref_var%corr_eta_hm_1)/kgenref_var%corr_eta_hm_1)**2 
              buf2_corr_eta_hm_1 = (var%corr_eta_hm_1-kgenref_var%corr_eta_hm_1)**2 
          ELSEWHERE 
              buf1_corr_eta_hm_1 = (var%corr_eta_hm_1-kgenref_var%corr_eta_hm_1)**2 
              buf2_corr_eta_hm_1 = buf1_corr_eta_hm_1 
          END WHERE   
          nrmsdiff_corr_eta_hm_1 = SQRT(SUM(buf1_corr_eta_hm_1)/REAL(n_corr_eta_hm_1)) 
          rmsdiff_corr_eta_hm_1 = SQRT(SUM(buf2_corr_eta_hm_1)/REAL(n_corr_eta_hm_1)) 
          IF (rmsdiff_corr_eta_hm_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_eta_hm_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_eta_hm_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_eta_hm_1 /= kgenref_var%corr_eta_hm_1), " of ", size( var%corr_eta_hm_1 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_eta_hm_1)/real(size(var%corr_eta_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_eta_hm_1)/real(size(kgenref_var%corr_eta_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_eta_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_eta_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_eta_hm_1 /= kgenref_var%corr_eta_hm_1), " of ", size( var%corr_eta_hm_1 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_eta_hm_1)/real(size(var%corr_eta_hm_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_eta_hm_1)/real(size(kgenref_var%corr_eta_hm_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_eta_hm_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_eta_hm_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_eta_hm_2 == kgenref_var%corr_eta_hm_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_eta_hm_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_eta_hm_2(SIZE(var%corr_eta_hm_2,dim=1))) 
          ALLOCATE (buf2_corr_eta_hm_2(SIZE(var%corr_eta_hm_2,dim=1))) 
          n_corr_eta_hm_2 = COUNT(var%corr_eta_hm_2 /= kgenref_var%corr_eta_hm_2) 
          WHERE ( ABS(kgenref_var%corr_eta_hm_2) > kgen_minvalue ) 
              buf1_corr_eta_hm_2 = ((var%corr_eta_hm_2-kgenref_var%corr_eta_hm_2)/kgenref_var%corr_eta_hm_2)**2 
              buf2_corr_eta_hm_2 = (var%corr_eta_hm_2-kgenref_var%corr_eta_hm_2)**2 
          ELSEWHERE 
              buf1_corr_eta_hm_2 = (var%corr_eta_hm_2-kgenref_var%corr_eta_hm_2)**2 
              buf2_corr_eta_hm_2 = buf1_corr_eta_hm_2 
          END WHERE   
          nrmsdiff_corr_eta_hm_2 = SQRT(SUM(buf1_corr_eta_hm_2)/REAL(n_corr_eta_hm_2)) 
          rmsdiff_corr_eta_hm_2 = SQRT(SUM(buf2_corr_eta_hm_2)/REAL(n_corr_eta_hm_2)) 
          IF (rmsdiff_corr_eta_hm_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_eta_hm_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_eta_hm_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_eta_hm_2 /= kgenref_var%corr_eta_hm_2), " of ", size( var%corr_eta_hm_2 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_eta_hm_2)/real(size(var%corr_eta_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_eta_hm_2)/real(size(kgenref_var%corr_eta_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_eta_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_eta_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_eta_hm_2 /= kgenref_var%corr_eta_hm_2), " of ", size( var%corr_eta_hm_2 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_eta_hm_2)/real(size(var%corr_eta_hm_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_eta_hm_2)/real(size(kgenref_var%corr_eta_hm_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_eta_hm_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_eta_hm_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_hmx_hmy_1 == kgenref_var%corr_hmx_hmy_1)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_hmx_hmy_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_hmx_hmy_1(SIZE(var%corr_hmx_hmy_1,dim=1),SIZE(var%corr_hmx_hmy_1,dim=2))) 
          ALLOCATE (buf2_corr_hmx_hmy_1(SIZE(var%corr_hmx_hmy_1,dim=1),SIZE(var%corr_hmx_hmy_1,dim=2))) 
          n_corr_hmx_hmy_1 = COUNT(var%corr_hmx_hmy_1 /= kgenref_var%corr_hmx_hmy_1) 
          WHERE ( ABS(kgenref_var%corr_hmx_hmy_1) > kgen_minvalue ) 
              buf1_corr_hmx_hmy_1 = ((var%corr_hmx_hmy_1-kgenref_var%corr_hmx_hmy_1)/kgenref_var%corr_hmx_hmy_1)**2 
              buf2_corr_hmx_hmy_1 = (var%corr_hmx_hmy_1-kgenref_var%corr_hmx_hmy_1)**2 
          ELSEWHERE 
              buf1_corr_hmx_hmy_1 = (var%corr_hmx_hmy_1-kgenref_var%corr_hmx_hmy_1)**2 
              buf2_corr_hmx_hmy_1 = buf1_corr_hmx_hmy_1 
          END WHERE   
          nrmsdiff_corr_hmx_hmy_1 = SQRT(SUM(buf1_corr_hmx_hmy_1)/REAL(n_corr_hmx_hmy_1)) 
          rmsdiff_corr_hmx_hmy_1 = SQRT(SUM(buf2_corr_hmx_hmy_1)/REAL(n_corr_hmx_hmy_1)) 
          IF (rmsdiff_corr_hmx_hmy_1 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_hmx_hmy_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_hmx_hmy_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_hmx_hmy_1 /= kgenref_var%corr_hmx_hmy_1), " of ", size( var%corr_hmx_hmy_1 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_hmx_hmy_1)/real(size(var%corr_hmx_hmy_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_hmx_hmy_1)/real(size(kgenref_var%corr_hmx_hmy_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_hmx_hmy_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_hmx_hmy_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_hmx_hmy_1 /= kgenref_var%corr_hmx_hmy_1), " of ", size( var%corr_hmx_hmy_1 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_hmx_hmy_1)/real(size(var%corr_hmx_hmy_1)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_hmx_hmy_1)/real(size(kgenref_var%corr_hmx_hmy_1)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_hmx_hmy_1 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_hmx_hmy_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%corr_hmx_hmy_2 == kgenref_var%corr_hmx_hmy_2)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_hmx_hmy_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_corr_hmx_hmy_2(SIZE(var%corr_hmx_hmy_2,dim=1),SIZE(var%corr_hmx_hmy_2,dim=2))) 
          ALLOCATE (buf2_corr_hmx_hmy_2(SIZE(var%corr_hmx_hmy_2,dim=1),SIZE(var%corr_hmx_hmy_2,dim=2))) 
          n_corr_hmx_hmy_2 = COUNT(var%corr_hmx_hmy_2 /= kgenref_var%corr_hmx_hmy_2) 
          WHERE ( ABS(kgenref_var%corr_hmx_hmy_2) > kgen_minvalue ) 
              buf1_corr_hmx_hmy_2 = ((var%corr_hmx_hmy_2-kgenref_var%corr_hmx_hmy_2)/kgenref_var%corr_hmx_hmy_2)**2 
              buf2_corr_hmx_hmy_2 = (var%corr_hmx_hmy_2-kgenref_var%corr_hmx_hmy_2)**2 
          ELSEWHERE 
              buf1_corr_hmx_hmy_2 = (var%corr_hmx_hmy_2-kgenref_var%corr_hmx_hmy_2)**2 
              buf2_corr_hmx_hmy_2 = buf1_corr_hmx_hmy_2 
          END WHERE   
          nrmsdiff_corr_hmx_hmy_2 = SQRT(SUM(buf1_corr_hmx_hmy_2)/REAL(n_corr_hmx_hmy_2)) 
          rmsdiff_corr_hmx_hmy_2 = SQRT(SUM(buf2_corr_hmx_hmy_2)/REAL(n_corr_hmx_hmy_2)) 
          IF (rmsdiff_corr_hmx_hmy_2 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_hmx_hmy_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_hmx_hmy_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_hmx_hmy_2 /= kgenref_var%corr_hmx_hmy_2), " of ", size( var%corr_hmx_hmy_2 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_hmx_hmy_2)/real(size(var%corr_hmx_hmy_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_hmx_hmy_2)/real(size(kgenref_var%corr_hmx_hmy_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_hmx_hmy_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_hmx_hmy_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%corr_hmx_hmy_2 /= kgenref_var%corr_hmx_hmy_2), " of ", size( var%corr_hmx_hmy_2 ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%corr_hmx_hmy_2)/real(size(var%corr_hmx_hmy_2)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%corr_hmx_hmy_2)/real(size(kgenref_var%corr_hmx_hmy_2)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_corr_hmx_hmy_2 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_corr_hmx_hmy_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%mu_ncn_1 == kgenref_var%mu_ncn_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mu_ncn_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_mu_ncn_1 = ABS(var%mu_ncn_1 - kgenref_var%mu_ncn_1) 
          IF (diff_mu_ncn_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_ncn_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_ncn_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_mu_ncn_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_mu_ncn_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%mu_ncn_2 == kgenref_var%mu_ncn_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mu_ncn_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_mu_ncn_2 = ABS(var%mu_ncn_2 - kgenref_var%mu_ncn_2) 
          IF (diff_mu_ncn_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_ncn_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mu_ncn_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_mu_ncn_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_mu_ncn_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%sigma_ncn_1 == kgenref_var%sigma_ncn_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sigma_ncn_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_sigma_ncn_1 = ABS(var%sigma_ncn_1 - kgenref_var%sigma_ncn_1) 
          IF (diff_sigma_ncn_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_ncn_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_ncn_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sigma_ncn_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sigma_ncn_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%sigma_ncn_2 == kgenref_var%sigma_ncn_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sigma_ncn_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_sigma_ncn_2 = ABS(var%sigma_ncn_2 - kgenref_var%sigma_ncn_2) 
          IF (diff_sigma_ncn_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_ncn_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sigma_ncn_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sigma_ncn_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sigma_ncn_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%precip_frac == kgenref_var%precip_frac) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%precip_frac is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_precip_frac = ABS(var%precip_frac - kgenref_var%precip_frac) 
          IF (diff_precip_frac <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%precip_frac is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%precip_frac is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_precip_frac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_precip_frac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%precip_frac_1 == kgenref_var%precip_frac_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%precip_frac_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_precip_frac_1 = ABS(var%precip_frac_1 - kgenref_var%precip_frac_1) 
          IF (diff_precip_frac_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%precip_frac_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%precip_frac_1 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_precip_frac_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_precip_frac_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%precip_frac_2 == kgenref_var%precip_frac_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%precip_frac_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_precip_frac_2 = ABS(var%precip_frac_2 - kgenref_var%precip_frac_2) 
          IF (diff_precip_frac_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%precip_frac_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%precip_frac_2 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_precip_frac_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_precip_frac_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_kgen_hydromet_pdf_parameter_module_typesubp0 
    
end module hydromet_pdf_parameter_module