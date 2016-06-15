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

  use clubb_precision, only: &
    core_rknd

  implicit none

  private ! Default scope

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

  integer, public :: & 
    sclr_dim = 0,        & ! Number of passive scalars
    edsclr_dim = 0,      & ! Number of eddy-diff. passive scalars
    hydromet_dim = 0       ! Number of hydrometeor species

!$omp threadprivate(sclr_dim, edsclr_dim, hydromet_dim)

  real( kind = core_rknd ), dimension(:), allocatable, public :: & 
    sclr_tol ! Threshold(s) on the passive scalars  [units vary]

!$omp threadprivate(sclr_tol)

  real( kind = selected_real_kind(6) ), public :: PosInf

!$omp threadprivate(PosInf)

  public :: setup_parameters_model 

  PUBLIC kw_externs_in_parameters_model 
  contains

!-------------------------------------------------------------------------------
  subroutine setup_parameters_model &
             ( T0_in, ts_nudge_in, &
               hydromet_dim_in, & 
               sclr_dim_in, sclr_tol_in, edsclr_dim_in &



    
              )

! Description:
!   Sets parameters to their initial values
!
! References:
!   None
!-------------------------------------------------------------------------------
    use constants_clubb, only: Skw_max_mag, Skw_max_mag_sqd

    use clubb_precision, only: &
      core_rknd ! Variable(s)

    implicit none

    ! External
    intrinsic :: sqrt, allocated, transfer

    ! Constants
    integer(kind=4), parameter :: nanbits = 2139095040

    ! Input Variables
    real( kind = core_rknd ), intent(in) ::  & 
      T0_in,        & ! Ref. temperature             [K]
      ts_nudge_in     ! Timescale for u/v nudging    [s]






    integer, intent(in) :: & 
      hydromet_dim_in,  & ! Number of hydrometeor species
      sclr_dim_in,      & ! Number of passive scalars
      edsclr_dim_in       ! Number of eddy-diff. passive scalars

    real( kind = core_rknd ), intent(in), dimension(sclr_dim_in) :: & 
      sclr_tol_in     ! Threshold on passive scalars

    ! --- Begin Code --- 
     
    ! Formula from subroutine pdf_closure, where sigma_sqd_w = 0.4 and Skw =
    ! Skw_max_mag in this formula.  Note that this is constant, but can't appear
    ! with a Fortran parameter attribute, so we define it here. 
    mixt_frac_max_mag = 1.0_core_rknd &
      - ( 0.5_core_rknd * ( 1.0_core_rknd - Skw_max_mag / &
      sqrt( 4.0_core_rknd * ( 1.0_core_rknd - 0.4_core_rknd )**3 &
      + Skw_max_mag_sqd ) ) ) ! Known magic number

    T0       = T0_in
    ts_nudge = ts_nudge_in

    hydromet_dim = hydromet_dim_in
    sclr_dim     = sclr_dim_in
    edsclr_dim   = edsclr_dim_in

    ! In a tuning run, this array has the potential to be allocated already
    if ( .not. allocated( sclr_tol ) ) then
      allocate( sclr_tol(1:sclr_dim) )
    else
      deallocate( sclr_tol )
      allocate( sclr_tol(1:sclr_dim) )
    end if

    sclr_tol(1:sclr_dim) = sclr_tol_in(1:sclr_dim)

    PosInf = transfer( nanbits, PosInf )





    return
  end subroutine setup_parameters_model
!-------------------------------------------------------------------------------

  !write in state subroutine for kw_externs_in_parameters_model 
  SUBROUTINE kw_externs_in_parameters_model(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) mixt_frac_max_mag 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) t0 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) ts_nudge 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) rtm_min 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) rtm_nudge_max_altitude 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) edsclr_dim 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) sclr_dim 
      CALL kw_parameters_model_real__core_rknd_dim1(sclr_tol, kgen_unit) 
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) posinf 
  END SUBROUTINE kw_externs_in_parameters_model 
    
  !write state subroutine for kw_parameters_model_real__core_rknd_dim1 
  SUBROUTINE kw_parameters_model_real__core_rknd_dim1(var, kgen_unit, printvar) 
      REAL(KIND=core_rknd), INTENT(IN), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
        
      IF (SIZE(var)==1) THEN 
          IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN 
              kgen_istrue = .FALSE. 
          ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN 
              kgen_istrue = .FALSE. 
          ELSE 
              kgen_istrue = .TRUE. 
          END IF   
      ELSE IF (SIZE(var)==0) THEN 
          kgen_istrue = .FALSE. 
      ELSE 
          kgen_istrue = .TRUE. 
      END IF   
      IF (.NOT. ALLOCATED(var)) THEN 
          kgen_istrue = .FALSE. 
      END IF   
      WRITE (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          kgen_array_sum = REAL(SUM(var, mask=(var .eq. var)), 8) 
          WRITE (UNIT = kgen_unit) kgen_array_sum 
          WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
          WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
          WRITE (UNIT = kgen_unit) var 
          IF (PRESENT( printvar )) THEN 
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
          END IF   
      END IF   
        
  END SUBROUTINE kw_parameters_model_real__core_rknd_dim1 
    
end module parameters_model