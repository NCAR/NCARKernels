
! KGEN-generated Fortran source file
!
! Filename    : model_flags.F90
! Generated at: 2015-10-20 14:27:06
! KGEN version: 0.5.3



    MODULE model_flags
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   Various model options that can be toggled off and on as desired.
! References:
!   None
!-------------------------------------------------------------------------------
        IMPLICIT NONE
        PRIVATE ! Default Scope
        LOGICAL, parameter, public :: l_sat_mixrat_lookup           = .false.
        LOGICAL, parameter, public :: l_clip_semi_implicit          = .false.
        LOGICAL, parameter, public :: l_pos_def                     = .false.
        LOGICAL, parameter, public :: l_hole_fill                   = .true.
        LOGICAL, parameter, public :: l_clip_turb_adv               = .false.
        LOGICAL, parameter, public :: l_gmres                       = .false.
! Flux limiting positive definite scheme on rtm
! Hole filling pos def scheme on wp2,up2,rtp2,etc
! Semi-implicit clipping scheme on wpthlp and wprtp
! Corrects thlm/rtm when w'th_l'/w'r_t' is clipped
! Use GMRES iterative solver rather than LAPACK
! Use a lookup table for mixing length
! saturation vapor pressure calculations
        LOGICAL, parameter, public :: l_gamma_skw     = .true.
! Use the native byte ordering in GrADS output
! Use a Skw dependent gamma parameter
! Flag to use the Boussinesq form of the
! predictive equations.  The predictive
! equations are anelastic by default.
! Flag to use precipitation fraction in KK
! microphysics.  The precipitation fraction
! is automatically set to 1 when this flag
! is turned off.
!$omp threadprivate( l_use_precip_frac )
!Flag to include the effects of rain evaporation
!on rtp2 and thlp2.  The moister (rt_1 or rt_2)
!and colder (thl_1 or thl_2) will be fed into
!the morrison microphys, and rain evaporation will
!be allowed to increase variances
! Flag to evaporate cloud water at temperatures
! colder than -37C.  This is to be used for
! Morrison microphysics, to prevent excess ice
        LOGICAL, parameter, public :: l_cubic_interp = .false.
! Flag to convert grid points with cubic monotonic
! spline interpolation as opposed to linear interpolation.
! See clubb:ticket:632 for details
! Include the contribution of radiation to thlp2
!$omp threadprivate( l_calc_thlp2_rad )
! These are the integer constants that represent the various saturation
! formulas. To add a new formula, add an additional constant here,
! add the logic to check the strings for the new formula in clubb_core and
! this module, and add logic in saturation to call the proper function--
! the control logic will be based on these named constants.
        INTEGER, parameter, public :: saturation_flatau = 3
        INTEGER, parameter, public :: saturation_bolton = 1
        INTEGER, parameter, public :: saturation_gfdl   = 2
! Constant for Bolton approximations of saturation
! Constant for the GFDL approximation of saturation
! Constant for Flatau approximations of saturation
!-----------------------------------------------------------------------------
! Options that can be changed at runtime
! The default values are chosen below and overwritten if desired by the user
!-----------------------------------------------------------------------------
! These flags determine whether or not we want CLUBB to do diffusion
!   on thlm and rtm and if a stability correction is applied
        LOGICAL, public :: l_stability_correct_kh_n2_zm  = .false.
        LOGICAL, public :: l_diffuse_rtm_and_thlm        = .false.
! Diffuses rtm and thlm
! Divides Kh_N2_zm by a stability factor
!$omp threadprivate(l_diffuse_rtm_and_thlm, l_stability_correct_Kh_N2_zm)
! These flags determine whether we want to use an upwind differencing approximation
! rather than a centered differencing for turbulent or mean advection terms.
! wpxp_ta affects wprtp, wpthlp, & wpsclrp
! xpyp_ta affects rtp2, thlp2, up2, vp2, sclrp2, rtpthlp, sclrprtp, & sclrpthlp
! xm_ma affects rtm, thlm, sclrm, um and vm.
        LOGICAL, public :: l_upwind_xm_ma   = .true.
        LOGICAL, public :: l_upwind_wpxp_ta = .false.
        LOGICAL, public :: l_upwind_xpyp_ta = .true.
!$omp threadprivate(l_upwind_wpxp_ta, l_upwind_xpyp_ta, l_upwind_xm_ma)
        LOGICAL, public :: l_quintic_poly_interp = .false.
! Use a quintic polynomial in mono_cubic_interp
!$omp threadprivate(l_quintic_poly_interp)
        LOGICAL, public :: l_rtm_nudge = .false.
        LOGICAL, public :: l_tke_aniso = .true.
        LOGICAL, public :: l_uv_nudge = .false.
! For wind speed nudging. - Michael Falk
! For rtm nudging
! For anisotropic turbulent kinetic energy,
! i.e. TKE = 1/2 (u'^2 + v'^2 + w'^2)
!$omp threadprivate(l_uv_nudge, l_tke_aniso, l_rtm_nudge)
! Use 2 calls to pdf_closure and the trapezoidal rule to  compute the
! varibles that are output from high order closure
!$omp threadprivate(l_vert_avg_closure)
! These are currently set based on l_vert_avg_closure
        LOGICAL, public :: l_call_pdf_closure_twice = .true.
        LOGICAL, public :: l_trapezoidal_rule_zt = .true.
        LOGICAL, public :: l_trapezoidal_rule_zm = .true.
        LOGICAL, public :: l_single_c2_skw = .false.
! If true, the trapezoidal rule is called for
! the thermodynamic-level variables output
! from pdf_closure.
! If true, the trapezoidal rule is called for
! three momentum-level variables - wpthvp,
! thlpthvp, and rtpthvp - output from pdf_closure.
! This logical flag determines whether or not to
! call subroutine pdf_closure twice.  If true,
! pdf_closure is called first on thermodynamic levels
! and then on momentum levels so that each variable is
! computed on its native level.  If false, pdf_closure
! is only called on thermodynamic levels, and variables
! which belong on momentum levels are interpolated.
! Use a single Skewness dependent C2 for rtp2, thlp2, and rtpthlp
!$omp threadprivate(l_trapezoidal_rule_zt, l_trapezoidal_rule_zm, &
!$omp   l_call_pdf_closure_twice, l_single_C2_Skw)
        LOGICAL, public :: l_standard_term_ta = .false.
! Use the standard discretization for the
! turbulent advection terms.  Setting to
! .false. means that a_1 and a_3 are pulled
! outside of the derivative in advance_wp2_wp3_module.F90
! and in advance_xp2_xpyp_module.F90.
!$omp threadprivate(l_standard_term_ta)
! Use to determine whether a host model has already applied the surface flux,
! to avoid double counting.
        LOGICAL, public :: l_host_applies_sfc_fluxes = .false.
!$omp threadprivate(l_host_applies_sfc_fluxes)
! Use cloud_cover and rcm_in_layer to help boost cloud_frac and rcm to help increase cloudiness
! at coarser grid resolutions.
        LOGICAL, public :: l_use_cloud_cover = .true.
!$omp threadprivate(l_use_cloud_cover)
        INTEGER, public :: saturation_formula = saturation_flatau
! Integer that stores the saturation formula to be used
!$omp threadprivate(saturation_formula)
! See clubb:ticket:514 for details
! Diagnose correlations instead of using fixed ones
! Calculate the correlations between w and the hydrometeors
!$omp threadprivate(l_diagnose_correlations, l_calc_w_corr)
! Resolve radiation over subcolumns using SILHS
! Use a constant cloud droplet conc. within cloud (K&K)
! Use a fixed correlation for s and t Mellor(chi/eta)
!$omp threadprivate( l_const_Nc_in_cloud, l_fix_chi_eta_correlations )
            PUBLIC kgen_read_externs_model_flags
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_model_flags(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) l_host_applies_sfc_fluxes
            READ(UNIT=kgen_unit) l_quintic_poly_interp
            READ(UNIT=kgen_unit) l_rtm_nudge
            READ(UNIT=kgen_unit) saturation_formula
            READ(UNIT=kgen_unit) l_call_pdf_closure_twice
            READ(UNIT=kgen_unit) l_trapezoidal_rule_zt
            READ(UNIT=kgen_unit) l_trapezoidal_rule_zm
            READ(UNIT=kgen_unit) l_use_cloud_cover
            READ(UNIT=kgen_unit) l_tke_aniso
            READ(UNIT=kgen_unit) l_stability_correct_kh_n2_zm
            READ(UNIT=kgen_unit) l_diffuse_rtm_and_thlm
            READ(UNIT=kgen_unit) l_upwind_xm_ma
            READ(UNIT=kgen_unit) l_upwind_wpxp_ta
            READ(UNIT=kgen_unit) l_single_c2_skw
            READ(UNIT=kgen_unit) l_upwind_xpyp_ta
            READ(UNIT=kgen_unit) l_standard_term_ta
            READ(UNIT=kgen_unit) l_uv_nudge
        END SUBROUTINE kgen_read_externs_model_flags

!===============================================================================

!===============================================================================

!===============================================================================

!===============================================================================

!===============================================================================

    END MODULE model_flags
