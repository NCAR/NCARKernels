!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:30 
!KGEN version : 0.8.1 
  
!------------------------------------------------------------------------
! $Id$
!===============================================================================


module grid_class
  ! Description:
  ! Definition of a grid class and associated functions
  ! The grid specification is as follows:
  !     +                ================== zm(nz) =========GP=========
  !     |
  !     |
  ! 1/dzt(nz) +          ------------------ zt(nz) ---------GP---------
  !     |        |
  !     |        |
  !     + 1/dzm(nz-1)    ================== zm(nz-1) ==================
  !              |
  !              |
  !              +       ------------------ zt(nz-1) ------------------
  !                                           .
  !                                           .
  !                                           .
  !                                           .
  !                      ================== zm(k+1) ===================
  !              +       ------------------ zt(k+1) -------------------
  !              |
  !              |
  !     +    1/dzm(k)    ================== zm(k) =====================
  !     |        |
  !     |        |
  ! 1/dzt(k)     +       ------------------ zt(k) ---------------------
  !     |
  !     |
  !     +                ================== zm(k-1) ===================
  !                      ------------------ zt(k-1) -------------------
  !                                           .
  !                                           .
  !                                           .
  !                                           .
  !     +                ================== zm(2) =====================
  !     |
  !     |
  ! 1/dzt(2)     +       ------------------ zt(2) ---------------------
  !     |        |
  !     |        |
  !     +    1/dzm(1)    ================== zm(1) ============GP=======  zm_init
  !              |       //////////////////////////////////////////////  surface
  !              |
  !              +       ------------------ zt(1) ------------GP-------
  ! The variable zm(k) stands for the momentum level altitude at momentum
  ! level k; the variable zt(k) stands for the thermodynamic level altitude at
  ! thermodynamic level k; the variable invrs_dzt(k) is the inverse distance
  ! between momentum levels (over a central thermodynamic level k); and the
  ! variable invrs_dzm(k) is the inverse distance between thermodynamic levels
  ! (over a central momentum level k).  Please note that in the above diagram,
  ! "invrs_dzt" is denoted "dzt", and "invrs_dzm" is denoted "dzm", such that
  ! 1/dzt is the distance between successive momentum levels k-1 and k (over a
  ! central thermodynamic level k), and 1/dzm is the distance between successive
  ! thermodynamic levels k and k+1 (over a central momentum level k).
  ! The grid setup is compatible with a stretched (unevely-spaced) grid.  Thus,
  ! the distance between successive grid levels may not always be constant.
  ! The following diagram is an example of a stretched grid that is defined on
  ! momentum levels.  The thermodynamic levels are placed exactly halfway
  ! between the momentum levels.  However, the momentum levels do not fall
  ! halfway between the thermodynamic levels.
  !        =============== zm(k+1) ===============
  !        --------------- zt(k+1) ---------------
  !        ===============  zm(k)  ===============
  !        ---------------  zt(k)  ---------------
  !        =============== zm(k-1) ===============
  ! The following diagram is an example of a stretched grid that is defined on
  ! thermodynamic levels.  The momentum levels are placed exactly halfway
  ! between the thermodynamic levels.  However, the thermodynamic levels do not
  ! fall halfway between the momentum levels.
  !        --------------- zt(k+1) ---------------
  !        ===============  zm(k)  ===============
  !        ---------------  zt(k)  ---------------
  !        =============== zm(k-1) ===============
  !        --------------- zt(k-1) ---------------
  ! NOTE:  Any future code written for use in the CLUBB parameterization should
  !        use interpolation formulas consistent with a stretched grid.  The
  !        simplest way to do so is to call the appropriate interpolation
  !        function from this module.  Interpolations should *not* be handled in
  !        the form of:  ( var_zm(k) + var_zm(k-1) ) / 2; *nor* in the form of:
  !        0.5_core_rknd*( var_zt(k+1) + var_zt(k) ).  Rather, all explicit interpolations
  !        should call zt2zm or zm2zt; while interpolations for a variable being
  !        solved for implicitly in the code should use gr%weights_zt2zm (which
  !        refers to interp_weights_zt2zm_imp), or gr%weights_zm2zt (which
  !        refers to interp_weights_zm2zt_imp).
  ! Momentum level 1 is placed at altitude zm_init, which is usually at the
  ! surface.  However, in general, zm_init can be at any altitude defined by the
  ! user.
  ! GP indicates ghost points. Variables located at those levels are not
  ! prognosed, but only used for boundary conditions.
  ! Chris Golaz, 7/17/99
  ! modified 9/10/99
  ! schemena, modified 6/11/2014 - Restructered code to add cubic/linear flag
  !  References:
  !  https://arxiv.org/pdf/1711.03675v1.pdf#nameddest=url:clubb_grid
  !  Section 3c, p. 3548 /Numerical discretization/ of:
  !   ``A PDF-Based Model for Boundary Layer Clouds. Part I:
  !     Method and Model Description'' Golaz, et al. (2002)
  !     JAS, Vol. 59, pp. 3540--3551.
  !-----------------------------------------------------------------------

  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !
  !

  !
  !

    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PUBLIC gr, grid 


    PRIVATE 
  ! Constant parameters


  type grid

    integer :: nz ! Number of points in the grid
    !   Note: Fortran 90/95 prevents an allocatable array from appearing
    !   within a derived type.  However, Fortran 2003 does not!!!!!!!!
    real( kind = core_rknd ), allocatable, dimension(:) :: &
      zm, & ! Momentum grid
      zt    ! Thermo grid
    real( kind = core_rknd ), allocatable, dimension(:) :: &
      invrs_dzm, & ! The inverse spacing between thermodynamic grid
                   ! levels; centered over momentum grid levels.
      invrs_dzt    ! The inverse spacing between momentum grid levels;
                   ! levels; centered over momentum grid levels.
                   ! centered over thermodynamic grid levels.

    real( kind = core_rknd ), allocatable, dimension(:) :: &
      dzm, &  ! Spacing between thermodynamic grid levels; centered over
              ! momentum grid levels
      dzt     ! Spcaing between momentum grid levels; centered over
              ! momentum grid levels
              ! thermodynamic grid levels
    ! These weights are normally used in situations
    ! where a momentum level variable is being
    ! solved for implicitly in an equation and
    ! needs to be interpolated to the thermodynamic grid levels.

    real( kind = core_rknd ), allocatable, dimension(:,:) :: weights_zm2zt, & 
    ! These weights are normally used in situations where a
    ! thermodynamic level variable is being solved for implicitly in an equation
    ! and needs to be interpolated to the momentum grid levels.
                                     weights_zt2zm
    ! These weights are normally used in situations where a
    ! thermodynamic level variable is being solved for implicitly in an equation
    ! and needs to be interpolated to the momentum grid levels.

  end type grid
  !   The grid is defined here so that it is common throughout the module.
  !   The implication is that only one grid can be defined !


  type (grid), target :: gr
!   Modification for using CLUBB in a host model (i.e. one grid per column)
  ! Interfaces provided for function overloading

!$omp threadprivate(gr)


  ! Vertical derivative functions


  PUBLIC kr_externs_in_grid_class 
  PUBLIC kr_grid_class_grid 
  PUBLIC kv_grid_class_grid 

  !=============================================================================
    
  CONTAINS 
    


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


!===============================================================================


  !read state subroutine for kr_externs_in_grid_class 
  SUBROUTINE kr_externs_in_grid_class(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_grid_class_grid(gr, kgen_unit, "gr", .FALSE.) 
  END SUBROUTINE kr_externs_in_grid_class 
    
  !read state subroutine for kr_grid_class_grid 
  RECURSIVE SUBROUTINE kr_grid_class_grid(var, kgen_unit, printname, printvar) 
      TYPE(grid), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%nz 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nz = ", var%nz 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%zm, kgen_unit, printname // "%zm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%zm, kgen_unit, printname // "%zm", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%zt, kgen_unit, printname // "%zt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%zt, kgen_unit, printname // "%zt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzm, kgen_unit, printname // "%invrs_dzm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzm, kgen_unit, printname // "%invrs_dzm", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzt, kgen_unit, printname // "%invrs_dzt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzt, kgen_unit, printname // "%invrs_dzt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%dzm, kgen_unit, printname // "%dzm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%dzm, kgen_unit, printname // "%dzm", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%dzt, kgen_unit, printname // "%dzt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%dzt, kgen_unit, printname // "%dzt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zm2zt, kgen_unit, printname // "%weights_zm2zt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zm2zt, kgen_unit, printname // "%weights_zm2zt", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zt2zm, kgen_unit, printname // "%weights_zt2zm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zt2zm, kgen_unit, printname // "%weights_zt2zm", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_grid_class_grid 
    
  !write state subroutine for kr_grid_real__core_rknd_dim1 
  SUBROUTINE kr_grid_real__core_rknd_dim1(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
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
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_grid_real__core_rknd_dim1 
    
  !write state subroutine for kr_grid_real__core_rknd_dim2 
  SUBROUTINE kr_grid_real__core_rknd_dim2(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
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
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_grid_real__core_rknd_dim2 
    
  !verify state subroutine for kv_grid_class_grid 
  RECURSIVE SUBROUTINE kv_grid_class_grid(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(grid), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_nz 
      INTEGER :: n_zm 
      real(KIND=core_rknd) :: nrmsdiff_zm, rmsdiff_zm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_zm(:), buf2_zm(:) 
      INTEGER :: n_zt 
      real(KIND=core_rknd) :: nrmsdiff_zt, rmsdiff_zt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_zt(:), buf2_zt(:) 
      INTEGER :: n_invrs_dzm 
      real(KIND=core_rknd) :: nrmsdiff_invrs_dzm, rmsdiff_invrs_dzm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_invrs_dzm(:), buf2_invrs_dzm(:) 
      INTEGER :: n_invrs_dzt 
      real(KIND=core_rknd) :: nrmsdiff_invrs_dzt, rmsdiff_invrs_dzt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_invrs_dzt(:), buf2_invrs_dzt(:) 
      INTEGER :: n_dzm 
      real(KIND=core_rknd) :: nrmsdiff_dzm, rmsdiff_dzm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_dzm(:), buf2_dzm(:) 
      INTEGER :: n_dzt 
      real(KIND=core_rknd) :: nrmsdiff_dzt, rmsdiff_dzt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_dzt(:), buf2_dzt(:) 
      INTEGER :: n_weights_zm2zt 
      real(KIND=core_rknd) :: nrmsdiff_weights_zm2zt, rmsdiff_weights_zm2zt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_weights_zm2zt(:,:), buf2_weights_zm2zt(:,:) 
      INTEGER :: n_weights_zt2zm 
      real(KIND=core_rknd) :: nrmsdiff_weights_zt2zm, rmsdiff_weights_zt2zm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_weights_zt2zm(:,:), buf2_weights_zt2zm(:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nz == kgenref_var%nz) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nz is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nz = ABS(var%nz - kgenref_var%nz) 
          IF (diff_nz <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nz is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nz is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_nz 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nz 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ALLOCATED(var%zm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%zm == kgenref_var%zm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%zm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_zm(SIZE(var%zm,dim=1))) 
              ALLOCATE (buf2_zm(SIZE(var%zm,dim=1))) 
              n_zm = COUNT(var%zm /= kgenref_var%zm) 
              WHERE ( ABS(kgenref_var%zm) > kgen_minvalue ) 
                  buf1_zm = ((var%zm-kgenref_var%zm)/kgenref_var%zm)**2 
                  buf2_zm = (var%zm-kgenref_var%zm)**2 
              ELSEWHERE 
                  buf1_zm = (var%zm-kgenref_var%zm)**2 
                  buf2_zm = buf1_zm 
              END WHERE   
              nrmsdiff_zm = SQRT(SUM(buf1_zm)/REAL(n_zm)) 
              rmsdiff_zm = SQRT(SUM(buf2_zm)/REAL(n_zm)) 
              IF (rmsdiff_zm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zm is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%zm /= kgenref_var%zm), " of ", size( var%zm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zm)/real(size(var%zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zm)/real(size(kgenref_var%zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zm /= kgenref_var%zm), " of ", size( var%zm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zm)/real(size(var%zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zm)/real(size(kgenref_var%zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%zt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%zt == kgenref_var%zt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%zt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_zt(SIZE(var%zt,dim=1))) 
              ALLOCATE (buf2_zt(SIZE(var%zt,dim=1))) 
              n_zt = COUNT(var%zt /= kgenref_var%zt) 
              WHERE ( ABS(kgenref_var%zt) > kgen_minvalue ) 
                  buf1_zt = ((var%zt-kgenref_var%zt)/kgenref_var%zt)**2 
                  buf2_zt = (var%zt-kgenref_var%zt)**2 
              ELSEWHERE 
                  buf1_zt = (var%zt-kgenref_var%zt)**2 
                  buf2_zt = buf1_zt 
              END WHERE   
              nrmsdiff_zt = SQRT(SUM(buf1_zt)/REAL(n_zt)) 
              rmsdiff_zt = SQRT(SUM(buf2_zt)/REAL(n_zt)) 
              IF (rmsdiff_zt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zt is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%zt /= kgenref_var%zt), " of ", size( var%zt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zt)/real(size(var%zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zt)/real(size(kgenref_var%zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zt /= kgenref_var%zt), " of ", size( var%zt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zt)/real(size(var%zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zt)/real(size(kgenref_var%zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%invrs_dzm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%invrs_dzm == kgenref_var%invrs_dzm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%invrs_dzm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_invrs_dzm(SIZE(var%invrs_dzm,dim=1))) 
              ALLOCATE (buf2_invrs_dzm(SIZE(var%invrs_dzm,dim=1))) 
              n_invrs_dzm = COUNT(var%invrs_dzm /= kgenref_var%invrs_dzm) 
              WHERE ( ABS(kgenref_var%invrs_dzm) > kgen_minvalue ) 
                  buf1_invrs_dzm = ((var%invrs_dzm-kgenref_var%invrs_dzm)/kgenref_var%invrs_dzm)**2 
                  buf2_invrs_dzm = (var%invrs_dzm-kgenref_var%invrs_dzm)**2 
              ELSEWHERE 
                  buf1_invrs_dzm = (var%invrs_dzm-kgenref_var%invrs_dzm)**2 
                  buf2_invrs_dzm = buf1_invrs_dzm 
              END WHERE   
              nrmsdiff_invrs_dzm = SQRT(SUM(buf1_invrs_dzm)/REAL(n_invrs_dzm)) 
              rmsdiff_invrs_dzm = SQRT(SUM(buf2_invrs_dzm)/REAL(n_invrs_dzm)) 
              IF (rmsdiff_invrs_dzm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzm is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%invrs_dzm /= kgenref_var%invrs_dzm), " of ", size( var%invrs_dzm ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzm)/real(size(var%invrs_dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzm)/real(size(kgenref_var%invrs_dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%invrs_dzm /= kgenref_var%invrs_dzm), " of ", size( var%invrs_dzm ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzm)/real(size(var%invrs_dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzm)/real(size(kgenref_var%invrs_dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%invrs_dzt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%invrs_dzt == kgenref_var%invrs_dzt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%invrs_dzt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_invrs_dzt(SIZE(var%invrs_dzt,dim=1))) 
              ALLOCATE (buf2_invrs_dzt(SIZE(var%invrs_dzt,dim=1))) 
              n_invrs_dzt = COUNT(var%invrs_dzt /= kgenref_var%invrs_dzt) 
              WHERE ( ABS(kgenref_var%invrs_dzt) > kgen_minvalue ) 
                  buf1_invrs_dzt = ((var%invrs_dzt-kgenref_var%invrs_dzt)/kgenref_var%invrs_dzt)**2 
                  buf2_invrs_dzt = (var%invrs_dzt-kgenref_var%invrs_dzt)**2 
              ELSEWHERE 
                  buf1_invrs_dzt = (var%invrs_dzt-kgenref_var%invrs_dzt)**2 
                  buf2_invrs_dzt = buf1_invrs_dzt 
              END WHERE   
              nrmsdiff_invrs_dzt = SQRT(SUM(buf1_invrs_dzt)/REAL(n_invrs_dzt)) 
              rmsdiff_invrs_dzt = SQRT(SUM(buf2_invrs_dzt)/REAL(n_invrs_dzt)) 
              IF (rmsdiff_invrs_dzt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzt is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%invrs_dzt /= kgenref_var%invrs_dzt), " of ", size( var%invrs_dzt ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzt)/real(size(var%invrs_dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzt)/real(size(kgenref_var%invrs_dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%invrs_dzt /= kgenref_var%invrs_dzt), " of ", size( var%invrs_dzt ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzt)/real(size(var%invrs_dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzt)/real(size(kgenref_var%invrs_dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%dzm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dzm == kgenref_var%dzm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dzm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dzm(SIZE(var%dzm,dim=1))) 
              ALLOCATE (buf2_dzm(SIZE(var%dzm,dim=1))) 
              n_dzm = COUNT(var%dzm /= kgenref_var%dzm) 
              WHERE ( ABS(kgenref_var%dzm) > kgen_minvalue ) 
                  buf1_dzm = ((var%dzm-kgenref_var%dzm)/kgenref_var%dzm)**2 
                  buf2_dzm = (var%dzm-kgenref_var%dzm)**2 
              ELSEWHERE 
                  buf1_dzm = (var%dzm-kgenref_var%dzm)**2 
                  buf2_dzm = buf1_dzm 
              END WHERE   
              nrmsdiff_dzm = SQRT(SUM(buf1_dzm)/REAL(n_dzm)) 
              rmsdiff_dzm = SQRT(SUM(buf2_dzm)/REAL(n_dzm)) 
              IF (rmsdiff_dzm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzm is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%dzm /= kgenref_var%dzm), " of ", size( var%dzm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzm)/real(size(var%dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzm)/real(size(kgenref_var%dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dzm /= kgenref_var%dzm), " of ", size( var%dzm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzm)/real(size(var%dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzm)/real(size(kgenref_var%dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%dzt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dzt == kgenref_var%dzt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dzt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dzt(SIZE(var%dzt,dim=1))) 
              ALLOCATE (buf2_dzt(SIZE(var%dzt,dim=1))) 
              n_dzt = COUNT(var%dzt /= kgenref_var%dzt) 
              WHERE ( ABS(kgenref_var%dzt) > kgen_minvalue ) 
                  buf1_dzt = ((var%dzt-kgenref_var%dzt)/kgenref_var%dzt)**2 
                  buf2_dzt = (var%dzt-kgenref_var%dzt)**2 
              ELSEWHERE 
                  buf1_dzt = (var%dzt-kgenref_var%dzt)**2 
                  buf2_dzt = buf1_dzt 
              END WHERE   
              nrmsdiff_dzt = SQRT(SUM(buf1_dzt)/REAL(n_dzt)) 
              rmsdiff_dzt = SQRT(SUM(buf2_dzt)/REAL(n_dzt)) 
              IF (rmsdiff_dzt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzt is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%dzt /= kgenref_var%dzt), " of ", size( var%dzt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzt)/real(size(var%dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzt)/real(size(kgenref_var%dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dzt /= kgenref_var%dzt), " of ", size( var%dzt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzt)/real(size(var%dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzt)/real(size(kgenref_var%dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%weights_zm2zt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%weights_zm2zt == kgenref_var%weights_zm2zt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%weights_zm2zt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_weights_zm2zt(SIZE(var%weights_zm2zt,dim=1),SIZE(var%weights_zm2zt,dim=2))) 
              ALLOCATE (buf2_weights_zm2zt(SIZE(var%weights_zm2zt,dim=1),SIZE(var%weights_zm2zt,dim=2))) 
              n_weights_zm2zt = COUNT(var%weights_zm2zt /= kgenref_var%weights_zm2zt) 
              WHERE ( ABS(kgenref_var%weights_zm2zt) > kgen_minvalue ) 
                  buf1_weights_zm2zt = ((var%weights_zm2zt-kgenref_var%weights_zm2zt)/kgenref_var%weights_zm2zt)**2 
                  buf2_weights_zm2zt = (var%weights_zm2zt-kgenref_var%weights_zm2zt)**2 
              ELSEWHERE 
                  buf1_weights_zm2zt = (var%weights_zm2zt-kgenref_var%weights_zm2zt)**2 
                  buf2_weights_zm2zt = buf1_weights_zm2zt 
              END WHERE   
              nrmsdiff_weights_zm2zt = SQRT(SUM(buf1_weights_zm2zt)/REAL(n_weights_zm2zt)) 
              rmsdiff_weights_zm2zt = SQRT(SUM(buf2_weights_zm2zt)/REAL(n_weights_zm2zt)) 
              IF (rmsdiff_weights_zm2zt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zm2zt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zm2zt is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%weights_zm2zt /= kgenref_var%weights_zm2zt), " of ", size( var%weights_zm2zt ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zm2zt)/real(size(var%weights_zm2zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zm2zt)/real(size(kgenref_var%weights_zm2zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zm2zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zm2zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%weights_zm2zt /= kgenref_var%weights_zm2zt), " of ", size( var%weights_zm2zt ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zm2zt)/real(size(var%weights_zm2zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zm2zt)/real(size(kgenref_var%weights_zm2zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zm2zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zm2zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%weights_zt2zm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%weights_zt2zm == kgenref_var%weights_zt2zm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%weights_zt2zm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_weights_zt2zm(SIZE(var%weights_zt2zm,dim=1),SIZE(var%weights_zt2zm,dim=2))) 
              ALLOCATE (buf2_weights_zt2zm(SIZE(var%weights_zt2zm,dim=1),SIZE(var%weights_zt2zm,dim=2))) 
              n_weights_zt2zm = COUNT(var%weights_zt2zm /= kgenref_var%weights_zt2zm) 
              WHERE ( ABS(kgenref_var%weights_zt2zm) > kgen_minvalue ) 
                  buf1_weights_zt2zm = ((var%weights_zt2zm-kgenref_var%weights_zt2zm)/kgenref_var%weights_zt2zm)**2 
                  buf2_weights_zt2zm = (var%weights_zt2zm-kgenref_var%weights_zt2zm)**2 
              ELSEWHERE 
                  buf1_weights_zt2zm = (var%weights_zt2zm-kgenref_var%weights_zt2zm)**2 
                  buf2_weights_zt2zm = buf1_weights_zt2zm 
              END WHERE   
              nrmsdiff_weights_zt2zm = SQRT(SUM(buf1_weights_zt2zm)/REAL(n_weights_zt2zm)) 
              rmsdiff_weights_zt2zm = SQRT(SUM(buf2_weights_zt2zm)/REAL(n_weights_zt2zm)) 
              IF (rmsdiff_weights_zt2zm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zt2zm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zt2zm is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%weights_zt2zm /= kgenref_var%weights_zt2zm), " of ", size( var%weights_zt2zm ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zt2zm)/real(size(var%weights_zt2zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zt2zm)/real(size(kgenref_var%weights_zt2zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zt2zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zt2zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%weights_zt2zm /= kgenref_var%weights_zt2zm), " of ", size( var%weights_zt2zm ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zt2zm)/real(size(var%weights_zt2zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zt2zm)/real(size(kgenref_var%weights_zt2zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zt2zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zt2zm 
                      WRITE (*, *) "" 
                  END IF   
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
  END SUBROUTINE kv_grid_class_grid 
    
end module grid_class