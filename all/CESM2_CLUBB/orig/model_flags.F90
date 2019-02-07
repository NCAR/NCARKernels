!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:35 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id: model_flags.F90 7367 2014-11-06 18:29:49Z schemena@uwm.edu $
!===============================================================================


module model_flags
! Description:
!   Various model options that can be toggled off and on as desired.
! References:
!   None
!-------------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


    IMPLICIT NONE 


    PRIVATE 

  logical, parameter, public ::  & 
    l_pos_def                     = .false., & ! Flux limiting positive definite scheme on rtm
    l_hole_fill                   = .true.,  & ! Hole filling pos def scheme on wp2,up2,rtp2,etc
    l_clip_semi_implicit          = .false., & ! Semi-implicit clipping scheme on wpthlp and wprtp
    l_clip_turb_adv               = .false., & ! Corrects thlm/rtm when w'th_l'/w'r_t' is clipped
    l_gmres                       = .false., & ! Use GMRES iterative solver rather than LAPACK
    l_sat_mixrat_lookup           = .false.    ! Use a lookup table for mixing length
                                      ! saturation vapor pressure calculations

  logical, parameter, public :: &
    l_byteswap_io   = .false., & ! Use the native byte ordering in GrADS output
    l_gamma_Skw     = .true.      ! Use a Skw dependent gamma parameter

                                ! predictive equations.  The predictive
                                ! equations are anelastic by default.

                                 ! microphysics.  The precipitation fraction
                                 ! is automatically set to 1 when this flag
                                 ! is turned off.
  ! These flags determine whether or not we want CLUBB to do diffusion
  !   on thlm and rtm and if a stability correction is applied

!$omp threadprivate( l_use_precip_frac )

  logical, public :: &
    l_diffuse_rtm_and_thlm        = .false., & ! Diffuses rtm and thlm
    l_stability_correct_Kh_N2_zm  = .false.    ! Divides Kh_N2_zm by a stability factor

!$omp threadprivate( l_diffuse_rtm_and_thlm, l_stability_correct_Kh_N2_zm )

                                  !on rtp2 and thlp2.  The moister (rt_1 or rt_2)
                                  !and colder (thl_1 or thl_2) will be fed into
                                  !the morrison microphys, and rain evaporation will
                                  !be allowed to increase variances

                                   ! colder than -37C.  This is to be used for 
                                   ! Morrison microphysics, to prevent excess ice

  logical, parameter, public :: &
    l_cubic_interp = .false.      ! Flag to convert grid points with cubic monotonic
                                  ! spline interpolation as opposed to linear interpolation.
  ! See clubb:ticket:632 for details

  ! These are the integer constants that represent the various saturation
  ! formulas. To add a new formula, add an additional constant here,
  ! add the logic to check the strings for the new formula in clubb_core and
  ! this module, and add logic in saturation to call the proper function--
  ! the control logic will be based on these named constants.
!$omp threadprivate( l_calc_thlp2_rad )


  integer, parameter, public :: &
    saturation_bolton = 1, & ! Constant for Bolton approximations of saturation
    saturation_gfdl   = 2, & ! Constant for the GFDL approximation of saturation
    saturation_flatau = 3    ! Constant for Flatau approximations of saturation
  !-----------------------------------------------------------------------------
  ! Options that can be changed at runtime 
  ! The default values are chosen below and overwritten if desired by the user
  !-----------------------------------------------------------------------------
  ! These flags determine whether we want to use an upwind differencing approximation 
  ! rather than a centered differencing for turbulent or mean advection terms.
  ! wpxp_ta affects wprtp, wpthlp, & wpsclrp
  ! xpyp_ta affects rtp2, thlp2, up2, vp2, sclrp2, rtpthlp, sclrprtp, & sclrpthlp
  ! xm_ma affects rtm, thlm, sclrm, um and vm.


  logical, public :: & 
    l_upwind_wpxp_ta = .false., & 
    l_upwind_xpyp_ta = .true.,  &
    l_upwind_xm_ma   = .true.

!$omp threadprivate(l_upwind_wpxp_ta, l_upwind_xpyp_ta, l_upwind_xm_ma)

  logical, public :: & 
    l_quintic_poly_interp = .false. ! Use a quintic polynomial in mono_cubic_interp

!$omp threadprivate(l_quintic_poly_interp)


  logical, public :: & 
    l_uv_nudge = .false.,  & ! For wind speed nudging. - Michael Falk
    l_rtm_nudge = .false., & ! For rtm nudging
    l_tke_aniso = .true.     ! For anisotropic turbulent kinetic energy, 
                             ! i.e. TKE = 1/2 (u'^2 + v'^2 + w'^2)
  ! Use 2 calls to pdf_closure and the trapezoidal rule to  compute the 
  ! varibles that are output from high order closure
!$omp threadprivate(l_uv_nudge, l_tke_aniso, l_rtm_nudge)

  ! These are currently set based on l_vert_avg_closure
!$omp threadprivate(l_vert_avg_closure)

  logical, public :: &
    l_trapezoidal_rule_zt = .true., & ! If true, the trapezoidal rule is called for
                                         ! the thermodynamic-level variables output 
                                         ! from pdf_closure.  
    l_trapezoidal_rule_zm = .true., & ! If true, the trapezoidal rule is called for
                                         ! three momentum-level variables - wpthvp,
                                       ! thlpthvp, and rtpthvp - output from pdf_closure.
    l_call_pdf_closure_twice = .true., & ! This logical flag determines whether or not to
    ! call subroutine pdf_closure twice.  If true,
    ! pdf_closure is called first on thermodynamic levels
    ! and then on momentum levels so that each variable is
    ! computed on its native level.  If false, pdf_closure
    ! is only called on thermodynamic levels, and variables
    ! which belong on momentum levels are interpolated.
    l_single_C2_Skw = .false. ! Use a single Skewness dependent C2 for rtp2, thlp2, and rtpthlp
                                         ! the thermodynamic-level variables output 
                                         ! from pdf_closure.  
                                         ! three momentum-level variables - wpthvp,
                                       ! thlpthvp, and rtpthvp - output from pdf_closure.
    ! call subroutine pdf_closure twice.  If true,
    ! pdf_closure is called first on thermodynamic levels
    ! and then on momentum levels so that each variable is
    ! computed on its native level.  If false, pdf_closure
    ! is only called on thermodynamic levels, and variables
    ! which belong on momentum levels are interpolated.

!$omp threadprivate(l_trapezoidal_rule_zt, l_trapezoidal_rule_zm, &
!$omp   l_call_pdf_closure_twice, l_single_C2_Skw)

  logical, public :: &
    l_standard_term_ta = .false.    ! Use the standard discretization for the
  ! turbulent advection terms.  Setting to
  ! .false. means that a_1 and a_3 are pulled
  ! outside of the derivative in advance_wp2_wp3_module.F90
  ! and in advance_xp2_xpyp_module.F90.
  ! Use to determine whether a host model has already applied the surface flux,
  ! to avoid double counting.
!$omp threadprivate(l_standard_term_ta)

  logical, public :: &
    l_host_applies_sfc_fluxes = .false.
  ! Use cloud_cover and rcm_in_layer to help boost cloud_frac and rcm to help increase cloudiness
  ! at coarser grid resolutions.

!$omp threadprivate(l_host_applies_sfc_fluxes)

  logical, public :: &
    l_use_cloud_cover = .true.
!$omp threadprivate(l_use_cloud_cover)

  integer, public :: &
    saturation_formula = saturation_flatau ! Integer that stores the saturation formula to be used
  ! See clubb:ticket:514 for details

!$omp threadprivate(saturation_formula)

!$omp threadprivate(l_diagnose_correlations, l_calc_w_corr)


!$omp threadprivate( l_const_Nc_in_cloud, l_fix_chi_eta_correlations )

  logical, public :: &
    l_use_ADG2 = .false.    ! Use Luhar et al. (2002) to close the w Gaussians.
                            ! Allows for each w Gaussian to have a different
                            ! width
!$omp threadprivate(l_use_ADG2)

  logical, public :: &
    l_use_3D_closure = .false., & ! Use Luhar et al. (2002) to close the w, thl, and rt Gaussians.
    l_stability_correct_tau_zm = .true., & ! Use tau_N2_zm instead of tau_zm in wpxp_pr1
                                           !  stability correction
    l_damp_wp2_using_em = .false., & ! In wp2 equation, use a dissipation
                                     ! formula of -(2/3)*em/tau_zm, as in Bougeault (1981)
    l_do_expldiff_rtm_thlm = .false., & ! Diffuse rtm and thlm explicitly
    l_Lscale_plume_centered = .false., &   ! Alternate that uses the PDF to
                                           !  compute the perturbed values
    l_use_ice_latent = .false., &          ! Includes the effects of ice latent heating in
                                           !  turbulence terms
    l_use_C7_Richardson = .false., &       ! Parameterize C7 based on Richardson number
    l_brunt_vaisala_freq_moist = .false.   ! Use a different formula for the Brunt-Vaisala 
                                           !  stability correction
                                     ! formula of -(2/3)*em/tau_zm, as in Bougeault (1981)
                                           !  compute the perturbed values
                                           !  turbulence terms
                                           !  frequency in saturated atmospheres
                                           !  (from Durran and Klemp, 1982)

!$omp threadprivate( l_use_3D_closure, l_stability_correct_tau_zm, l_damp_wp2_using_em, &
!$omp                l_do_expldiff_rtm_thlm, &
!$omp                l_Lscale_plume_centered, l_use_ice_latent, l_use_C7_Richardson, &
!$omp                l_brunt_vaisala_freq_moist )


  PUBLIC kr_externs_in_model_flags 

!===============================================================================
    
  CONTAINS 
    


!===============================================================================


!===============================================================================


!===============================================================================

!===============================================================================


  !read state subroutine for kr_externs_in_model_flags 
  SUBROUTINE kr_externs_in_model_flags(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) l_diffuse_rtm_and_thlm 
      READ (UNIT = kgen_unit) l_stability_correct_kh_n2_zm 
      READ (UNIT = kgen_unit) l_upwind_wpxp_ta 
      READ (UNIT = kgen_unit) l_upwind_xpyp_ta 
      READ (UNIT = kgen_unit) l_upwind_xm_ma 
      READ (UNIT = kgen_unit) l_quintic_poly_interp 
      READ (UNIT = kgen_unit) l_tke_aniso 
      READ (UNIT = kgen_unit) l_uv_nudge 
      READ (UNIT = kgen_unit) l_rtm_nudge 
      READ (UNIT = kgen_unit) l_trapezoidal_rule_zm 
      READ (UNIT = kgen_unit) l_trapezoidal_rule_zt 
      READ (UNIT = kgen_unit) l_single_c2_skw 
      READ (UNIT = kgen_unit) l_call_pdf_closure_twice 
      READ (UNIT = kgen_unit) l_standard_term_ta 
      READ (UNIT = kgen_unit) l_host_applies_sfc_fluxes 
      READ (UNIT = kgen_unit) l_use_cloud_cover 
      READ (UNIT = kgen_unit) saturation_formula 
      READ (UNIT = kgen_unit) l_use_adg2 
      READ (UNIT = kgen_unit) l_lscale_plume_centered 
      READ (UNIT = kgen_unit) l_use_ice_latent 
      READ (UNIT = kgen_unit) l_use_3d_closure 
      READ (UNIT = kgen_unit) l_damp_wp2_using_em 
      READ (UNIT = kgen_unit) l_use_c7_richardson 
      READ (UNIT = kgen_unit) l_brunt_vaisala_freq_moist 
      READ (UNIT = kgen_unit) l_stability_correct_tau_zm 
      READ (UNIT = kgen_unit) l_do_expldiff_rtm_thlm 
  END SUBROUTINE kr_externs_in_model_flags 
    
end module model_flags