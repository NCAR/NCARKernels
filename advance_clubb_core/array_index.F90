
! KGEN-generated Fortran source file
!
! Filename    : array_index.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE array_index
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
! Contains indices to variables in larger arrays.
! Note that the 'ii' is necessary because 'i' is used in
! statistics to track locations in the zt/zm/sfc derived types.
! References:
!   None
!-------------------------------------------------------------------------
! Precision
        IMPLICIT NONE
! Variables
! Microphysics mixing ratios
        INTEGER, public :: iirrm
! Hydrometeor array index for rain water mixing ratio, rr
! Hydrometeor array index for snow mixing ratio, rs
! Hydrometeor array index for ice mixing ratio, ri
! Hydrometeor array index for graupel mixing ratio, rg
!$omp threadprivate(iirrm, iirsm, iirim, iirgm)
! Microphysics concentrations
! Hydrometeor array index for rain drop concentration, Nr
! Hydrometeor array index for snow concentration, Ns
! Hydrometeor array index for ice concentration, Ni
! Hydrometeor array index for graupel concentration, Ng
!$omp threadprivate(iiNrm, iiNsm, iiNim, iiNgm)
! Scalar quantities
        INTEGER, public :: iisclr_rt
        INTEGER, public :: iisclr_thl
! [kg/kg]/[K]/[1e6 mol/mol]
! "    "
!$omp threadprivate(iisclr_rt, iisclr_thl, iisclr_CO2, &
!$omp   iiedsclr_rt, iiedsclr_thl, iiedsclr_CO2)
! Logical fields
        LOGICAL, dimension(:), allocatable, public :: l_mix_rat_hm
! if true, then the hydrometeor is frozen; otherwise liquid
! if true, then the quantity is a hydrometeor mixing ratio
!$omp threadprivate(l_frozen_hm, l_mix_rat_hm)
!$omp threadprivate( hydromet_list )
! Tolerance values for all hydrometeors    [units vary]
!$omp threadprivate( hydromet_tol )
        PRIVATE ! Default Scope
!===============================================================================
        PUBLIC kgen_read_externs_array_index
    CONTAINS

    ! write subroutines
        SUBROUTINE kgen_read_logical_4_dim1_alloc(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            logical(KIND=4), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1
            INTEGER, DIMENSION(2,1) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_logical_4_dim1_alloc


    ! module extern variables

    SUBROUTINE kgen_read_externs_array_index(kgen_unit)
        INTEGER, INTENT(IN) :: kgen_unit
        CALL kgen_read_logical_4_dim1_alloc(l_mix_rat_hm, kgen_unit)
        READ(UNIT=kgen_unit) iirrm
        READ(UNIT=kgen_unit) iisclr_rt
        READ(UNIT=kgen_unit) iisclr_thl
    END SUBROUTINE kgen_read_externs_array_index

    END MODULE array_index
