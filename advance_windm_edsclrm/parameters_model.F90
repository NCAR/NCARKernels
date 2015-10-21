
! KGEN-generated Fortran source file
!
! Filename    : parameters_model.F90
! Generated at: 2015-10-21 08:59:10
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
!$omp threadprivate(mixt_frac_max_mag)
! Model parameters and constraints setup in the namelists
        REAL(KIND=core_rknd), public :: ts_nudge = 0._core_rknd
! Reference temperature (usually 300)  [K]
! Timescale of u/v nudging             [s]
!$omp threadprivate(T0, ts_nudge)
! Value below which rtm will be nudged [kg/kg]
! Highest altitude at which to nudge rtm [m]
!$omp threadprivate(rtm_min, rtm_nudge_max_altitude)
        INTEGER, public :: edsclr_dim = 0
! Number of passive scalars
! Number of eddy-diff. passive scalars
! Number of hydrometeor species
!$omp threadprivate(sclr_dim, edsclr_dim, hydromet_dim)
! Threshold(s) on the passive scalars  [units vary]
!$omp threadprivate(sclr_tol)
!$omp threadprivate(PosInf)
            PUBLIC kgen_read_externs_parameters_model
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_parameters_model(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) edsclr_dim
            READ(UNIT=kgen_unit) ts_nudge
        END SUBROUTINE kgen_read_externs_parameters_model

!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
    END MODULE parameters_model
