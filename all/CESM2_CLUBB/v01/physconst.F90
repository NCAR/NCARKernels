!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:34 
!KGEN version : 0.8.1 
  


module physconst
! Physical constants.  Use csm_share values whenever available.


    USE shr_kind_mod, ONLY: r8 => shr_kind_r8 
    USE shr_const_mod, ONLY: shr_const_g, shr_const_latice, shr_const_latvap, shr_const_cpdair 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

! Constants based off share code or defined in physconst


real(r8), public, parameter :: latice      = shr_const_latice     ! Latent heat of fusion (J/kg)
real(r8), public, parameter :: latvap      = shr_const_latvap     ! Latent heat of vaporization (J/kg)

! Molecular weights

! modifiable physical constants for aquaplanet


real(r8), public, protected :: gravit  = shr_const_g      ! gravitational acceleration (m/s**2)
real(r8), public, protected :: cpair   = shr_const_cpdair ! specific heat of dry air (J/K/kg)
!---------------  Variables below here are derived from those above -----------------------


!---------------  Variables below here are for WACCM-X -----------------------


!================================================================================================
PUBLIC kr_externs_in_physconst 

!================================================================================================
! Read namelist variables.
  
CONTAINS 
  


!===============================================================================


!===============================================================================


!===============================================================================


!read state subroutine for kr_externs_in_physconst 
SUBROUTINE kr_externs_in_physconst(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) gravit 
    READ (UNIT = kgen_unit) cpair 
END SUBROUTINE kr_externs_in_physconst 
  
end module physconst