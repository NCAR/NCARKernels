!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:55
!KGEN version : 0.6.2

! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF BaseTime Module
      module ESMF_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES

































! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  


!
!===============================================================================
!BOPI
! !MODULE: ESMF_BaseTimeMod - Base ESMF time definition 
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt ESMF\_TimeInterval} and {\tt ESMF\_Time}
!
! See {\tt ../include/ESMC\_BaseTime.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
          USE esmf_basemod
          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
          IMPLICIT NONE
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
          PRIVATE
!------------------------------------------------------------------------------
!     ! ESMF_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type ESMF_BaseTime
        integer(ESMF_KIND_I8) :: S   ! whole seconds
        integer(ESMF_KIND_I8) :: Sn  ! fractional seconds, numerator
        integer(ESMF_KIND_I8) :: Sd  ! fractional seconds, denominator
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      PUBLIC esmf_basetime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! overloaded operators

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================











!==============================================================================

      PUBLIC kr_esmf_basetimemod_esmf_basetime
      PUBLIC kv_esmf_basetimemod_esmf_basetime

!==============================================================================

      
      CONTAINS
      
  ! Factor so abs(Sn) < Sd and ensure that signs of S and Sn match.
  ! Also, enforce consistency.
  ! YR and MM fields are ignored.

  !PRINT *,'DEBUG:  BEGIN normalize_basetime()'
  ! Consistency check...


  ! factor so abs(Sn) < Sd
      !PRINT *,'DEBUG:  normalize_basetime() A1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      !PRINT *,'DEBUG:  normalize_basetime() A2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
    ! change sign of Sn if it does not match S
      !PRINT *,'DEBUG:  normalize_basetime() B1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      !PRINT *,'DEBUG:  normalize_basetime() B2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      !PRINT *,'DEBUG:  normalize_basetime() C1:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
      !PRINT *,'DEBUG:  normalize_basetime() C2:  S,Sn,Sd = ',basetime%S,basetime%Sn,basetime%Sd
  !PRINT *,'DEBUG:  END normalize_basetime()'

!==============================================================================

! Add two basetimes
        ! locals
!  PRINT *,'DEBUG:  BEGIN ESMF_BaseTimeSum()'
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime1%S = ',basetime1%S
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime1%Sn = ',basetime1%Sn
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime1%Sd = ',basetime1%Sd
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime2%S = ',basetime2%S
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime2%Sn = ',basetime2%Sn
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  basetime2%Sd = ',basetime2%Sd
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sn1 = ',Sn1
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sd1 = ',Sd1
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sn2 = ',Sn2
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  Sd2 = ',Sd2
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  no fractions'
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  ESMF_BaseTimeSum%S = ',ESMF_BaseTimeSum%S
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  ESMF_BaseTimeSum%Sn = ',ESMF_BaseTimeSum%Sn
!  PRINT *,'DEBUG:  ESMF_BaseTimeSum():  ESMF_BaseTimeSum%Sd = ',ESMF_BaseTimeSum%Sd
!  PRINT *,'DEBUG:  END ESMF_BaseTimeSum()'


! Subtract two basetimes
        ! locals





! Divide basetime by 8-byte integer
        ! locals

!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() A:  S,Sn,Sd = ', &
!  basetime%S,basetime%Sn,basetime%Sd
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() A:  divisor = ', divisor


!$$$ move to default constructor

        ! convert to a fraction and divide by multipling the denonminator by 
        ! the divisor

!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() B:  n,d = ',n,d
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() C:  S,Sn,Sd = ', &
!  ESMF_BaseTimeQuotI8%S,ESMF_BaseTimeQuotI8%Sn,ESMF_BaseTimeQuotI8%Sd
!PRINT *,'DEBUG ESMF_BaseTimeQuotI8() D:  S,Sn,Sd = ', &
!  ESMF_BaseTimeQuotI8%S,ESMF_BaseTimeQuotI8%Sn,ESMF_BaseTimeQuotI8%Sd

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
!
! !ARGUMENTS:
! local










!==============================================================================

      !read state subroutine for kr_esmf_basetimemod_esmf_basetime
      RECURSIVE SUBROUTINE kr_esmf_basetimemod_esmf_basetime(var, kgen_unit, printvar)
          TYPE(esmf_basetime), INTENT(INOUT) :: var
          INTEGER, INTENT(IN) :: kgen_unit
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          READ (UNIT = kgen_unit) var%s
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%s **" // NEW_LINE("A"), var%s
          END IF 
          
          READ (UNIT = kgen_unit) var%sn
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%sn **" // NEW_LINE("A"), var%sn
          END IF 
          
          READ (UNIT = kgen_unit) var%sd
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%sd **" // NEW_LINE("A"), var%sd
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
          
          CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%s == kgenref_var%s) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%s is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_s = ABS(var%s - kgenref_var%s)
              IF (diff_s <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_s
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_s
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%sn == kgenref_var%sn) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%sn is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_sn = ABS(var%sn - kgenref_var%sn)
              IF (diff_sn <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%sn is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%sn is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_sn
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_sn
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%sd == kgenref_var%sd) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%sd is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_sd = ABS(var%sd - kgenref_var%sd)
              IF (diff_sd <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%sd is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%sd is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_sd
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_sd
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
      END SUBROUTINE kv_esmf_basetimemod_esmf_basetime
      
      end module ESMF_BaseTimeMod