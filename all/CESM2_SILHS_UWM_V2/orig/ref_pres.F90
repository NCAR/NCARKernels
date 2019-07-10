!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:31 
!KGEN version : 0.8.1 
  


module ref_pres
!--------------------------------------------------------------------------
! Provides access to reference pressures for use by the physics
! parameterizations.  The pressures are provided by the dynamical core
! since it determines the grid used by the physics.
! Note that the init method for this module is called before the init
! method in physpkg; therefore, most physics modules can use these
! reference pressures during their init phases.
!--------------------------------------------------------------------------
! 
! 
! 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PUBLIC 
    SAVE 
! Reference pressures (Pa)

                                            ! surface pressure ('eta' coordinate)

! Number of top levels using pure pressure representation

! Pressure used to set troposphere cloud physics top (Pa)

! Top level for troposphere cloud physics
integer, protected :: trop_cloud_top_lev
! Pressure used to set MAM process top (Pa)

! Top level for MAM processes that impact climate
! Molecular diffusion is calculated only if the model top is below this
! pressure (Pa).

! Pressure used to set bottom of molecular diffusion region (Pa).
! Flag for molecular diffusion, and molecular diffusion level index.
!====================================================================================
PUBLIC kr_externs_in_ref_pres 

!====================================================================================
  
CONTAINS 
  


!====================================================================================


!====================================================================================
! Convert pressure limiters to the appropriate level.


!====================================================================================


!read state subroutine for kr_externs_in_ref_pres 
SUBROUTINE kr_externs_in_ref_pres(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) trop_cloud_top_lev 
END SUBROUTINE kr_externs_in_ref_pres 
  
end module ref_pres