!---------------------------------------------------------------------------
! $Id: array_index.F90 7118 2014-07-25 00:12:15Z raut@uwm.edu $
!===============================================================================
module array_index

  ! Description:
  ! Contains indices to variables in larger arrays.
  ! Note that the 'ii' is necessary because 'i' is used in
  ! statistics to track locations in the zt/zm/sfc derived types.

  ! References:
  !   None
  !-------------------------------------------------------------------------

  use clubb_precision, only: &
    core_rknd      ! Precision

  implicit none

  ! Variables
  ! Microphysics mixing ratios
  integer, public :: &
    iirrm,    & ! Hydrometeor array index for rain water mixing ratio, rr
    iirsm,    & ! Hydrometeor array index for snow mixing ratio, rs
    iirim,     & ! Hydrometeor array index for ice mixing ratio, ri
    iirgm    ! Hydrometeor array index for graupel mixing ratio, rg
!$omp threadprivate(iirrm, iirsm, iirim, iirgm)

  ! Microphysics concentrations
  integer, public :: &
    iiNrm,       & ! Hydrometeor array index for rain drop concentration, Nr
    iiNsm,    & ! Hydrometeor array index for snow concentration, Ns
    iiNim,       & ! Hydrometeor array index for ice concentration, Ni
    iiNgm    ! Hydrometeor array index for graupel concentration, Ng
!$omp threadprivate(iiNrm, iiNsm, iiNim, iiNgm)

  ! Scalar quantities
  integer, public :: & 
    iisclr_rt, iisclr_thl, iisclr_CO2, & ! [kg/kg]/[K]/[1e6 mol/mol]
    iiedsclr_rt, iiedsclr_thl, iiedsclr_CO2 ! "    "
!$omp threadprivate(iisclr_rt, iisclr_thl, iisclr_CO2, &
!$omp   iiedsclr_rt, iiedsclr_thl, iiedsclr_CO2)

  ! Logical fields
  logical, dimension(:), allocatable, public :: &
    l_frozen_hm, & ! if true, then the hydrometeor is frozen; otherwise liquid
    l_mix_rat_hm   ! if true, then the quantity is a hydrometeor mixing ratio
!$omp threadprivate(l_frozen_hm, l_mix_rat_hm)

  character(len=10), dimension(:), allocatable, public :: & 
    hydromet_list

!$omp threadprivate( hydromet_list )

  real( kind = core_rknd ), dimension(:), allocatable, public :: &
    hydromet_tol    ! Tolerance values for all hydrometeors    [units vary]

!$omp threadprivate( hydromet_tol )   

  private ! Default Scope

!===============================================================================

  PUBLIC kw_externs_in_array_index
  
  CONTAINS
  
  !write in state subroutine for kw_externs_in_array_index
  SUBROUTINE kw_externs_in_array_index(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) iirrm
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) iisclr_thl
      kgen_istrue = .TRUE.
      WRITE (UNIT = kgen_unit) iisclr_rt
      CALL kw_array_index_logical___dim1(l_mix_rat_hm, kgen_unit)
  END SUBROUTINE kw_externs_in_array_index
  
  !write state subroutine for kw_array_index_logical___dim1
  SUBROUTINE kw_array_index_logical___dim1(var, kgen_unit, printvar)
      LOGICAL, INTENT(IN), ALLOCATABLE, DIMENSION(:) :: var
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
          WRITE (UNIT = kgen_unit) LBOUND(var, 1)
          WRITE (UNIT = kgen_unit) UBOUND(var, 1)
          WRITE (UNIT = kgen_unit) var
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var
          END IF 
      END IF 
      
  END SUBROUTINE kw_array_index_logical___dim1
  
end module array_index