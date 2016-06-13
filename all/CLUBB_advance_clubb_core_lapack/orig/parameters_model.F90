
! KGEN-generated Fortran source file
!
! Filename    : parameters_model.F90
! Generated at: 2015-10-20 14:27:10
! KGEN version: 0.5.3



    MODULE parameters_model
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   Contains model parameters that are determined at run time rather than
!   compile time.
!
! References:
!   None
!-------------------------------------------------------------------------------
        USE clubb_precision, ONLY: core_rknd
        IMPLICIT NONE
        PRIVATE ! Default scope
! Maximum magnitude of PDF parameter 'mixt_frac'.
        REAL(KIND=core_rknd), public :: mixt_frac_max_mag
!$omp threadprivate(mixt_frac_max_mag)
! Model parameters and constraints setup in the namelists
        REAL(KIND=core_rknd), public :: ts_nudge = 0._core_rknd
        REAL(KIND=core_rknd), public :: t0 = 300._core_rknd
! Reference temperature (usually 300)  [K]
! Timescale of u/v nudging             [s]
!$omp threadprivate(T0, ts_nudge)
        REAL(KIND=core_rknd), public :: rtm_min = epsilon( rtm_min )
        REAL(KIND=core_rknd), public :: rtm_nudge_max_altitude = 10000._core_rknd
! Value below which rtm will be nudged [kg/kg]
! Highest altitude at which to nudge rtm [m]
!$omp threadprivate(rtm_min, rtm_nudge_max_altitude)
        INTEGER, public :: edsclr_dim = 0
        INTEGER, public :: sclr_dim = 0
! Number of passive scalars
! Number of eddy-diff. passive scalars
! Number of hydrometeor species
!$omp threadprivate(sclr_dim, edsclr_dim, hydromet_dim)
        REAL(KIND=core_rknd), dimension(:), allocatable, public :: sclr_tol
! Threshold(s) on the passive scalars  [units vary]
!$omp threadprivate(sclr_tol)
        REAL(KIND=selected_real_kind(6)), public :: posinf
!$omp threadprivate(PosInf)
            PUBLIC kgen_read_externs_parameters_model
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_core_rknd_dim1_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
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
            END SUBROUTINE kgen_read_real_core_rknd_dim1_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_parameters_model(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) edsclr_dim
            READ(UNIT=kgen_unit) sclr_dim
            READ(UNIT=kgen_unit) posinf
            READ(UNIT=kgen_unit) mixt_frac_max_mag
            CALL kgen_read_real_core_rknd_dim1_alloc(sclr_tol, kgen_unit)
            READ(UNIT=kgen_unit) rtm_min
            READ(UNIT=kgen_unit) rtm_nudge_max_altitude
            READ(UNIT=kgen_unit) ts_nudge
            READ(UNIT=kgen_unit) t0
        END SUBROUTINE kgen_read_externs_parameters_model

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
    END MODULE parameters_model
