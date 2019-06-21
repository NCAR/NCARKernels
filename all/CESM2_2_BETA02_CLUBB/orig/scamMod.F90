!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:34 
!KGEN version : 0.8.1 
  


module scamMod

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
! PUBLIC INTERFACES:


! PUBLIC MODULE DATA:


logical, public ::  single_column         ! Using IOP file or not


! note that scm_zadv_q is set to slt to be consistent with CAM BFB testing


                                             ! mo_drydep algorithm

! SCAM public data defaults


! SCAM namelist defaults
  

! +++BPM:
! modification... allow a linear ramp in relaxation time scale:

! note that scm_use_obs_uv is set to true to be consistent with CAM BFB testing

!
!


character(len=200), public ::  scm_clubb_iop_name   ! IOP name for CLUBB
!=======================================================================
PUBLIC kr_externs_in_scammod 

!=======================================================================
  
CONTAINS 
  


!===============================================================================


!read state subroutine for kr_externs_in_scammod 
SUBROUTINE kr_externs_in_scammod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) single_column 
    READ (UNIT = kgen_unit) scm_clubb_iop_name 
END SUBROUTINE kr_externs_in_scammod 
  
end module scamMod