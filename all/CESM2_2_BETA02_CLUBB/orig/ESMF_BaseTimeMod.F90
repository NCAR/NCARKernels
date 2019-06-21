!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
!     ESMF BaseTime Module


!
!
module ESMF_BaseTimeMod
  !==============================================================================
  ! This file contains the BaseTime class definition and all BaseTime class
  ! methods.
  !------------------------------------------------------------------------------
  ! INCLUDES
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
  !===============================================================================
  !BOPI
  ! !MODULE: ESMF_BaseTimeMod - Base ESMF time definition
  ! !DESCRIPTION:
  ! Part of Time Manager F90 API wrapper of C++ implemenation
  ! This module serves only as the common Time definition inherited
  ! by {\tt ESMF\_TimeInterval} and {\tt ESMF\_Time}
  ! See {\tt ../include/ESMC\_BaseTime.h} for complete description
  !------------------------------------------------------------------------------
  ! !USES:
  !
  !
  !


  !
  !
  !
  !
  !
    USE esmf_basemod 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
    IMPLICIT NONE 
  !------------------------------------------------------------------------------
  ! !PRIVATE TYPES:
  !
    PRIVATE 
  !------------------------------------------------------------------------------
  !     ! ESMF_BaseTime
  !     ! Base class type to match C++ BaseTime class in size only;
  !     !  all dereferencing within class is performed by C++ implementation
  !

  type ESMF_BaseTime
     integer(ESMF_KIND_I8) :: S   ! whole seconds
     integer(ESMF_KIND_I8) :: Sn  ! fractional seconds, numerator
     integer(ESMF_KIND_I8) :: Sd  ! fractional seconds, denominator
  end type ESMF_BaseTime
  !------------------------------------------------------------------------------
  ! !PUBLIC TYPES:

  PUBLIC esmf_basetime 
  !------------------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  ! overloaded operators
  !
  !
  !==============================================================================
  ! INTERFACE BLOCKS
  !==============================================================================

  !
  !


  !==============================================================================
  PUBLIC kr_esmf_basetimemod_esmf_basetime 
  PUBLIC kv_esmf_basetimemod_esmf_basetime 


  !==============================================================================
    
  CONTAINS 
    


  !==============================================================================
  ! Add two basetimes


  ! Subtract two basetimes


  ! Divide basetime by 8-byte integer


  ! Divide basetime by integer


  ! .EQ. for two basetimes


  ! .NE. for two basetimes


  ! .LT. for two basetimes


  ! .GT. for two basetimes


  ! .LE. for two basetimes


  ! .GE. for two basetimes


  !==============================================================================


        !==============================================================================


                 !==============================================================================
                 ! spaceship operator for seconds + Sn/Sd


                 !==============================================================================


  !read state subroutine for kr_esmf_basetimemod_esmf_basetime 
  RECURSIVE SUBROUTINE kr_esmf_basetimemod_esmf_basetime(var, kgen_unit, printname, printvar) 
      TYPE(esmf_basetime), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%s 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%s = ", var%s 
      END IF   
        
      READ (UNIT = kgen_unit) var%sn 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%sn = ", var%sn 
      END IF   
        
      READ (UNIT = kgen_unit) var%sd 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%sd = ", var%sd 
      END IF   
        
  END SUBROUTINE kr_esmf_basetimemod_esmf_basetime 
    
  !verify state subroutine for kv_esmf_basetimemod_esmf_basetime 
  RECURSIVE SUBROUTINE kv_esmf_basetimemod_esmf_basetime(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(esmf_basetime), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer(KIND=esmf_kind_i8) :: diff_s 
      integer(KIND=esmf_kind_i8) :: diff_sn 
      integer(KIND=esmf_kind_i8) :: diff_sd 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%s == kgenref_var%s) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%s is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_s = ABS(var%s - kgenref_var%s) 
          IF (diff_s <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_s 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_s 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%sn == kgenref_var%sn) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sn is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_sn = ABS(var%sn - kgenref_var%sn) 
          IF (diff_sn <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sn is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sn is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_sn 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sn 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%sd == kgenref_var%sd) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sd is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_sd = ABS(var%sd - kgenref_var%sd) 
          IF (diff_sd <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sd is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sd is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_sd 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sd 
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
  END SUBROUTINE kv_esmf_basetimemod_esmf_basetime 
    
               end module ESMF_BaseTimeMod