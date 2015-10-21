
! KGEN-generated Fortran source file
!
! Filename    : variables_diagnostic_module.F90
! Generated at: 2015-10-21 08:59:09
! KGEN version: 0.5.3



    MODULE variables_diagnostic_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   This module contains definitions of all diagnostic
!   arrays used in the single column model, as well as subroutines
!   to allocate, deallocate and initialize them.
!   Note that while these are all same dimension, there is a
!   thermodynamic and momentum grid and they have different levels
!-----------------------------------------------------------------------
! derived type
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
        PRIVATE ! Set default scope
! Diagnostic variables
        REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: vg
        REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: um_ref
        REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: vm_ref
        REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: ug
! PDF width parameter interpolated to t-levs.  [-]
! Skewness of w on momentum levels             [-]
! Skewness of w on thermodynamic levels        [-]
! u geostrophic wind                           [m/s]
! v geostrophic wind                           [m/s]
! Initial u wind; Michael Falk                 [m/s]
! Initial v wind; Michael Falk                 [m/s]
! Initial liquid water potential temperature   [K]
! Initial total water mixing ratio             [kg/kg]
! Virtual potential temperature                [K]
!!! Important Note !!!
! Do not indent the omp comments, they need to be in the first 4 columns
!!! End Important Note !!!
!$omp threadprivate(sigma_sqd_w_zt, Skw_zm, Skw_zt, ug, vg, &
!$omp   um_ref, vm_ref, thlm_ref, rtm_ref, thvm )
! Saturation mixing ratio  ! Brian
!$omp threadprivate(rsat)
! pdf_params on momentum levels  [units vary]
!used when l_use_ice_latent = .true.
!$omp threadprivate(pdf_params_zm, pdf_params_zm_frz)
! Radiative flux (momentum point)   [W/m^2]
! SW + LW heating rate              [K/s]
! SW radiative upwelling flux       [W/m^2]
! LW radiative upwelling flux       [W/m^2]
! SW radiative downwelling flux     [W/m^2]
! LW radiative downwelling flux        [W/m^2]
!$omp threadprivate(Frad, radht, Frad_SW_up, Frad_SW_down, Frad_LW_up, Frad_LW_down)
! Second order moments
! thl'rc'              [K kg/kg]
! rt'rc'               [kg^2/kg^2]
! rc'^2                [kg^2/kg^2]
!$omp threadprivate(thlprcp, rtprcp, rcp2)
! Third order moments
! w'thl'^2    [m K^2/s]
! w'^2 thl'   [m^2 K/s^2]
! w'rt'^2     [m kg^2/kg^2]
! w'^2rt'     [m^2 kg/kg]
! w'rt'thl'   [m kg K/kg s]
! w'^2 rc'    [m^2 kg/kg s^2]
! w'^3        [m^3/s^3]
!$omp threadprivate(wpthlp2, wp2thlp, wprtp2, wp2rtp, &
!$omp   wprtpthlp, wp2rcp, wp3_zm )
! Fourth order moments
! w'^4      [m^4/s^4]
!$omp threadprivate(wp4)
! Buoyancy related moments
! rt'thv'     [K kg/kg]
! thl'thv'    [K^2]
! w'thv'      [K m/s]
! w'^2thv'    [K m^2/s^2]
!$omp threadprivate(rtpthvp, thlpthvp, wpthvp, wp2thvp)
! Eddy diffusivity coefficient on thermodynamic levels   [m^2/s]
! Eddy diffusivity coefficient on momentum levels        [m^2/s]
!$omp threadprivate(Kh_zt, Kh_zm)
! Eddy diffusivity coefficient for hydrometeors on momentum levels [m^2 s^-1]
!$omp threadprivate(K_hm)
! Mixing lengths
! [m]
!$omp threadprivate(Lscale, Lscale_up, Lscale_down)
! Turbulent Kinetic Energy (TKE)                        [m^2/s^2]
! Eddy dissipation time scale on momentum levels        [s]
! Eddy dissipation time scale on thermodynamic levels   [s]
!$omp threadprivate(em, tau_zm, tau_zt)
! hydrometeors variable arrays
! Mean hydrometeor (thermodynamic levels)           [units]
! Variance of a hydrometeor (overall) (m-levs.)     [units^2]
! Covariance of w and hydrometeor (momentum levels) [(m/s)un]
!$omp threadprivate( hydromet, hydrometp2, wphydrometp )
! Cloud droplet concentration arrays
! Mean cloud droplet concentration, <N_c> (thermo. levels) [num/kg]
! Covariance of w and N_c, <w'N_c'> (momentum levels) [(m/s)(#/kg)]
!$omp threadprivate(Ncm,wpNcp)
! Cloud condensation nuclei concentration (COAMPS/MG)   [num/kg]
!$omp threadprivate(Nccnm)
! Surface data
! Average value of friction velocity [m/s]
! Soil Heat Flux [W/m^2]
!$omp threadprivate(ustar, soil_heat_flux)
! Passive scalar variables
        REAL(KIND=core_rknd), target, allocatable, dimension(:,:), public :: wpedsclrp
! w'edsclr'
!$omp threadprivate(wpedsclrp)
! sclr'th_v'
! sclr'rc'
! w'^2 sclr'
! w'sclr'^2
! w'sclr'rt'
! w'sclr'thl'
!$omp threadprivate(sclrpthvp, sclrprcp, &
!$omp   wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp )
! Interpolated variables for tuning
!
! w'^2 on thermo. grid     [m^2/s^2]
! thl'^2 on thermo. grid   [K^2]
! w'thl' on thermo. grid   [m K/s]
! w'rt' on thermo. grid    [m kg/(kg s)]
! rt'^2 on therm. grid     [(kg/kg)^2]
! rt'thl' on thermo. grid  [kg K/kg]
! u'^2 on thermo. grid     [m^2/s^2]
! v'^2 on thermo. grid     [m^2/s^2]
! u'w' on thermo. grid     [m^2/s^2]
! v'w' on thermo. grid     [m^2/s^2]
!$omp threadprivate(wp2_zt, thlp2_zt, wpthlp_zt, wprtp_zt, &
!$omp   rtp2_zt, rtpthlp_zt, &
!$omp   up2_zt, vp2_zt, upwp_zt, vpwp_zt)
! Latin Hypercube arrays.  Vince Larson 22 May 2005
! Kessler ac estimate                 [kg/kg/s]
! Exact Kessler ac                    [kg/kg/s]
! St dev of exact Kessler ac          [kg/kg/s]
! Stdev of exact w/in cloud ac        [kg/kg/s]
! Monte Carlo rcm estimate            [kg/kg]
! Kessler ac based on rcm             [kg/kg/s]
! Kessler ac based on rcm/cloud_frac  [kg/kg/s]
!$omp threadprivate(lh_AKm, AKm, AKstd, AKstd_cld, lh_rcm_avg, AKm_rcm, &
!$omp   AKm_rcc)
! Skewness velocity    [m/s]
! The a3 coefficient from CLUBB eqns                [-]
! The a3 coefficient interpolated to the zt grid    [-]
!$omp threadprivate(Skw_velocity, a3_coef, a3_coef_zt)
! w'^3 / w'^2 on the zm grid [m/s]
! w'^3 / w'^2 on the zt grid [m/s]
!$omp threadprivate(wp3_on_wp2, wp3_on_wp2_zt)
            PUBLIC kgen_read_externs_variables_diagnostic_module
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

            SUBROUTINE kgen_read_real_core_rknd_dim2_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2
                INTEGER, DIMENSION(2,2) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim2_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_variables_diagnostic_module(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_real_core_rknd_dim1_alloc(ug, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1_alloc(vg, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1_alloc(um_ref, kgen_unit)
            CALL kgen_read_real_core_rknd_dim1_alloc(vm_ref, kgen_unit)
            CALL kgen_read_real_core_rknd_dim2_alloc(wpedsclrp, kgen_unit)
        END SUBROUTINE kgen_read_externs_variables_diagnostic_module

!-----------------------------------------------------------------------

!------------------------------------------------------------------------

    END MODULE variables_diagnostic_module
