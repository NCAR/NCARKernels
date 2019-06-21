!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:34 
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

real(r8), public, dimension(:,:,:), pointer :: cpairv ! composition dependent specific heat at constant pressure

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
    CALL kr_physconst_real__r8_dim3_ptr(cpairv, kgen_unit, "cpairv", .FALSE.) 
END SUBROUTINE kr_externs_in_physconst 
  
!read state subroutine for kr_physconst_real__r8_dim3_ptr 
SUBROUTINE kr_physconst_real__r8_dim3_ptr(var, kgen_unit, printname, printvar) 
    REAL(KIND=r8), INTENT(INOUT), POINTER, DIMENSION(:,:,:) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: idx1, idx2, idx3 
    INTEGER, DIMENSION(2,3) :: kgen_bound 
      
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        IF (ASSOCIATED( var )) THEN 
            NULLIFY (var) 
        END IF   
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgen_bound(1, 1) 
        READ (UNIT = kgen_unit) kgen_bound(2, 1) 
        READ (UNIT = kgen_unit) kgen_bound(1, 2) 
        READ (UNIT = kgen_unit) kgen_bound(2, 2) 
        READ (UNIT = kgen_unit) kgen_bound(1, 3) 
        READ (UNIT = kgen_unit) kgen_bound(2, 3) 
        ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3))) 
        READ (UNIT = kgen_unit) var 
        CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
        END IF   
    END IF   
END SUBROUTINE kr_physconst_real__r8_dim3_ptr 
  
end module physconst