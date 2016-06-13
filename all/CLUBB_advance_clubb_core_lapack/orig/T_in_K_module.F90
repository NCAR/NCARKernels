
! KGEN-generated Fortran source file
!
! Filename    : T_in_K_module.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE t_in_k_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE ! Default scope
        PUBLIC thlm2t_in_k
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!-------------------------------------------------------------------------------

        elemental FUNCTION thlm2t_in_k(thlm, exner, rcm) RESULT ( t_in_k )
! Description:
!   Calculates absolute temperature from liquid water potential
!   temperature.  (Does not include ice.)
! References:
!   Cotton and Anthes (1989), "Storm and Cloud Dynamics", Eqn. (2.51).
!-------------------------------------------------------------------------------
            USE constants_clubb, ONLY: cp
            USE constants_clubb, ONLY: lv
! Variable(s)
! Dry air specific heat at constant p [J/kg/K]
! Latent heat of vaporization         [J/kg]
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input
            REAL(KIND=core_rknd), intent(in) :: thlm
            REAL(KIND=core_rknd), intent(in) :: exner
            REAL(KIND=core_rknd), intent(in) :: rcm
! Liquid potential temperature  [K]
! Exner function                [-]
! Liquid water mixing ratio     [kg/kg]
            REAL(KIND=core_rknd) :: t_in_k
! Result temperature [K]
! ---- Begin Code ----
    T_in_K = thlm * exner + Lv * rcm / Cp
    return
        END FUNCTION thlm2t_in_k
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
    END MODULE t_in_k_module
