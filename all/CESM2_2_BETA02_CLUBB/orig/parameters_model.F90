!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:35 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id$
!===============================================================================


module parameters_model
! Description:
!   Contains model parameters that are determined at run time rather than
!   compile time.
! References:
!   None
!-------------------------------------------------------------------------------

!

    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PRIVATE 

  integer, parameter :: &
    sp = selected_real_kind(6)  ! 32-bit floating point number
  ! Maximum magnitude of PDF parameter 'mixt_frac'. 

  real( kind = core_rknd ), public :: mixt_frac_max_mag
  ! Model parameters and constraints setup in the namelists

!$omp threadprivate(mixt_frac_max_mag)

  real( kind = core_rknd ), public ::  & 
    T0 = 300._core_rknd,       & ! Reference temperature (usually 300)  [K]
    ts_nudge = 0._core_rknd      ! Timescale of u/v nudging             [s]


!$omp threadprivate(T0, ts_nudge)

  real( kind = core_rknd), public :: &
    rtm_min = epsilon( rtm_min ), &             ! Value below which rtm will be nudged [kg/kg]
    rtm_nudge_max_altitude = 10000._core_rknd ! Highest altitude at which to nudge rtm [m]
!$omp threadprivate( rtm_min, rtm_nudge_max_altitude )

  integer, public :: & 
    sclr_dim = 0,        & ! Number of passive scalars
    edsclr_dim = 0,      & ! Number of eddy-diff. passive scalars
    hydromet_dim = 0       ! Number of hydrometeor species

!$omp threadprivate(sclr_dim, edsclr_dim, hydromet_dim)

  real( kind = core_rknd ), dimension(:), allocatable, public :: & 
    sclr_tol ! Threshold(s) on the passive scalars  [units vary]

!$omp threadprivate(sclr_tol)

  real( kind = sp ), public :: PosInf

!$omp threadprivate(PosInf)

  PUBLIC kr_externs_in_parameters_model 

!-------------------------------------------------------------------------------
    
  CONTAINS 
    


!-------------------------------------------------------------------------------

  !read state subroutine for kr_externs_in_parameters_model 
  SUBROUTINE kr_externs_in_parameters_model(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) mixt_frac_max_mag 
      READ (UNIT = kgen_unit) t0 
      READ (UNIT = kgen_unit) ts_nudge 
      READ (UNIT = kgen_unit) rtm_min 
      READ (UNIT = kgen_unit) rtm_nudge_max_altitude 
      READ (UNIT = kgen_unit) sclr_dim 
      READ (UNIT = kgen_unit) edsclr_dim 
      READ (UNIT = kgen_unit) hydromet_dim 
      CALL kr_parameters_model_real__core_rknd_dim1(sclr_tol, kgen_unit, "sclr_tol", .FALSE.) 
      READ (UNIT = kgen_unit) posinf 
  END SUBROUTINE kr_externs_in_parameters_model 
    
  !read state subroutine for kr_parameters_model_real__core_rknd_dim1 
  SUBROUTINE kr_parameters_model_real__core_rknd_dim1(var, kgen_unit, printname, printvar) 
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
  END SUBROUTINE kr_parameters_model_real__core_rknd_dim1 
    
end module parameters_model