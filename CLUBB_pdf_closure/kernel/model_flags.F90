!KGEN-generated Fortran source file

!Generated at : 2016-01-07 11:47:38
!KGEN version : 0.6.1

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
    IMPLICIT NONE


    PRIVATE

                                      ! saturation vapor pressure calculations


                                ! predictive equations.  The predictive
                                ! equations are anelastic by default.

                                 ! microphysics.  The precipitation fraction
                                 ! is automatically set to 1 when this flag
                                 ! is turned off.

!$omp threadprivate( l_use_precip_frac )

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
!$omp threadprivate( l_calc_thlp2_rad )

  ! These are the integer constants that represent the various saturation
  ! formulas. To add a new formula, add an additional constant here,
  ! add the logic to check the strings for the new formula in clubb_core and
  ! this module, and add logic in saturation to call the proper function--
  ! the control logic will be based on these named constants.

  integer, parameter, public :: &
    saturation_bolton = 1, & ! Constant for Bolton approximations of saturation
    saturation_gfdl   = 2, & ! Constant for the GFDL approximation of saturation
    saturation_flatau = 3    ! Constant for Flatau approximations of saturation

  !-----------------------------------------------------------------------------
  ! Options that can be changed at runtime 
  ! The default values are chosen below and overwritten if desired by the user
  !-----------------------------------------------------------------------------

  ! These flags determine whether or not we want CLUBB to do diffusion
  !   on thlm and rtm and if a stability correction is applied

!$omp threadprivate(l_diffuse_rtm_and_thlm, l_stability_correct_Kh_N2_zm)

  ! These flags determine whether we want to use an upwind differencing approximation 
  ! rather than a centered differencing for turbulent or mean advection terms.
  ! wpxp_ta affects wprtp, wpthlp, & wpsclrp
  ! xpyp_ta affects rtp2, thlp2, up2, vp2, sclrp2, rtpthlp, sclrprtp, & sclrpthlp
  ! xm_ma affects rtm, thlm, sclrm, um and vm.

!$omp threadprivate(l_upwind_wpxp_ta, l_upwind_xpyp_ta, l_upwind_xm_ma)

  logical, public :: & 
    l_quintic_poly_interp = .false. ! Use a quintic polynomial in mono_cubic_interp

!$omp threadprivate(l_quintic_poly_interp)


                             ! i.e. TKE = 1/2 (u'^2 + v'^2 + w'^2)
!$omp threadprivate(l_uv_nudge, l_tke_aniso, l_rtm_nudge)

  ! Use 2 calls to pdf_closure and the trapezoidal rule to  compute the 
  ! varibles that are output from high order closure
!$omp threadprivate(l_vert_avg_closure)

  ! These are currently set based on l_vert_avg_closure
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

  ! turbulent advection terms.  Setting to
  ! .false. means that a_1 and a_3 are pulled
  ! outside of the derivative in advance_wp2_wp3_module.F90
  ! and in advance_xp2_xpyp_module.F90.
!$omp threadprivate(l_standard_term_ta)

  ! Use to determine whether a host model has already applied the surface flux,
  ! to avoid double counting.

!$omp threadprivate(l_host_applies_sfc_fluxes)

  ! Use cloud_cover and rcm_in_layer to help boost cloud_frac and rcm to help increase cloudiness
  ! at coarser grid resolutions.
!$omp threadprivate(l_use_cloud_cover)

  integer, public :: &
    saturation_formula = saturation_flatau ! Integer that stores the saturation formula to be used

!$omp threadprivate(saturation_formula)

  ! See clubb:ticket:514 for details
!$omp threadprivate(l_diagnose_correlations, l_calc_w_corr)



!$omp threadprivate( l_const_Nc_in_cloud, l_fix_chi_eta_correlations )








  PUBLIC kr_externs_in_model_flags

!===============================================================================
  
  CONTAINS
  

! Description:
!   Setup flags that influence the numerics, etc. of CLUBB core

! References:
!   None
!-------------------------------------------------------------------------------


    ! External

    ! Input Variables







    !---- Begin Code ----

    ! Logicals



    ! Integers

    ! Set up the saturation formula value



      ! Add new saturation formulas after this.





!===============================================================================

! Description:
!   Read in some of the model flags of interest from a namelist file. If the
!   variable isn't in the file it will just be the default value.
!
! References:
!   None
!-------------------------------------------------------------------------------




   ! Read the namelist






!===============================================================================

! Description:
!   Write a new namelist for the configurable model flags
!
! References:
!   None
!-------------------------------------------------------------------------------




   ! Read the namelist



!===============================================================================

! Description:
!   Set a model flag based on the input arguments for the purposes of trying
!   all possible combinations in the clubb_tuner.
!
! References:
!   None
!-------------------------------------------------------------------------------


    ! Input Variables

    ! ---- Begin Code ----





!===============================================================================

! Description:
!   Get the current model flags.
!
! References:
!   None
!-------------------------------------------------------------------------------


    ! Input Variables

    ! ---- Begin Code ----



  !read state subroutine for kr_externs_in_model_flags
  SUBROUTINE kr_externs_in_model_flags(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      READ (UNIT = kgen_unit) l_quintic_poly_interp
      READ (UNIT = kgen_unit) saturation_formula
  END SUBROUTINE kr_externs_in_model_flags
  
end module model_flags