!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:49:59 
!KGEN version : 0.7.0 
  
module scamMod
!----------------------------------------------------------------------- 
!BOP
!
! !MODULE: scamMod
! 
! !DESCRIPTION: 
! scam specific routines and data
!
! !USES:
!
!
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    IMPLICIT NONE 

    PRIVATE 
!
! !PUBLIC INTERFACES:
!

!
! !PUBLIC MODULE DATA:
!



  logical, public ::  single_column         ! Using IOP file or not





                                               ! mo_drydep algorithm
!


  character(len=200), public ::  scm_clubb_iop_name   ! IOP name for CLUBB

!=======================================================================
  PUBLIC kr_externs_in_scammod 
!=======================================================================

!
!-----------------------------------------------------------------------
!


    
  CONTAINS 
    






















!
!-----------------------------------------------------------------------
!



!
!-----------------------------------------------------------------------
!

!
!-----------------------------------------------------------------------
!

  !read state subroutine for kr_externs_in_scammod 
  SUBROUTINE kr_externs_in_scammod(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) single_column 
      READ (UNIT = kgen_unit) scm_clubb_iop_name 
  END SUBROUTINE kr_externs_in_scammod 
    
end module scamMod