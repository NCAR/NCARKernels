!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:44
!KGEN version : 0.6.1

!-------------------------------------------------------------------------------
! $Id: parameters_model.F90 7226 2014-08-19 15:52:41Z betlej@uwm.edu $
!===============================================================================
module parameters_model

! Description:
!   Contains model parameters that are determined at run time rather than
!   compile time.
!
! References:
!   None
!-------------------------------------------------------------------------------

    USE clubb_precision, ONLY: core_rknd

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PRIVATE

  ! Maximum magnitude of PDF parameter 'mixt_frac'. 
  real( kind = core_rknd ), public :: mixt_frac_max_mag

!$omp threadprivate(mixt_frac_max_mag)

  ! Model parameters and constraints setup in the namelists
  real( kind = core_rknd ), public ::  & 
    T0 = 300._core_rknd,       & ! Reference temperature (usually 300)  [K]
    ts_nudge = 0._core_rknd      ! Timescale of u/v nudging             [s]








!$omp threadprivate(T0, ts_nudge)

  real( kind = core_rknd), public :: &
    rtm_min = epsilon( rtm_min ), &             ! Value below which rtm will be nudged [kg/kg]
    rtm_nudge_max_altitude = 10000._core_rknd ! Highest altitude at which to nudge rtm [m]
!$omp threadprivate(rtm_min, rtm_nudge_max_altitude)

  INTEGER, public :: sclr_dim = 0, edsclr_dim = 0

!$omp threadprivate(sclr_dim, edsclr_dim, hydromet_dim)

  real( kind = core_rknd ), dimension(:), allocatable, public :: & 
    sclr_tol ! Threshold(s) on the passive scalars  [units vary]

!$omp threadprivate(sclr_tol)

  real( kind = 4 ), public :: PosInf

!$omp threadprivate(PosInf)


  PUBLIC kr_externs_in_parameters_model

!-------------------------------------------------------------------------------
  
  CONTAINS
  

! Description:
!   Sets parameters to their initial values
!
! References:
!   None
!-------------------------------------------------------------------------------



    ! External

    ! Constants

    ! Input Variables








    ! --- Begin Code --- 
     
    ! Formula from subroutine pdf_closure, where sigma_sqd_w = 0.4 and Skw =
    ! Skw_max_mag in this formula.  Note that this is constant, but can't appear
    ! with a Fortran parameter attribute, so we define it here. 



    ! In a tuning run, this array has the potential to be allocated already








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
      READ (UNIT = kgen_unit) edsclr_dim
      READ (UNIT = kgen_unit) sclr_dim
      CALL kr_parameters_model_real__core_rknd_dim1(sclr_tol, kgen_unit)
      READ (UNIT = kgen_unit) posinf
  END SUBROUTINE kr_externs_in_parameters_model
  
  !read state subroutine for kr_parameters_model_real__core_rknd_dim1
  SUBROUTINE kr_parameters_model_real__core_rknd_dim1(var, kgen_unit, printvar)
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var
      INTEGER, INTENT(IN) :: kgen_unit
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
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
          ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
          READ (UNIT = kgen_unit) var
          CALL kgen_array_sumcheck("var", kgen_array_sum, REAL(SUM(var), 8), .TRUE.)
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var
          END IF 
      END IF 
  END SUBROUTINE kr_parameters_model_real__core_rknd_dim1
  
end module parameters_model