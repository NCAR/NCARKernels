
! KGEN-generated Fortran source file
!
! Filename    : numerical_check.F90
! Generated at: 2015-10-20 14:27:09
! KGEN version: 0.5.3



    MODULE numerical_check
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
!       Made is_nan_2d public so it may be used
!       for finding code that cause NaNs
!       Joshua Fasching November 2007
!       *_check subroutines were added to ensure that the
!       subroutines they are checking perform correctly
!       Joshua Fasching February 2008
!       rad_clipping has been replaced by rad_check as the new
!       subroutine only reports if there are invalid values.
!       Joshua Fasching March 2008
        PRIVATE ! Default scope
        PUBLIC parameterization_check, is_nan_sclr, is_nan_2d, pdf_closure_check, length_check, surface_varnce_check, calculate_spurious_source
        PRIVATE check_nan, check_negative
! Abstraction of check_nan

        INTERFACE check_nan
            MODULE PROCEDURE check_nan_sclr, check_nan_2d
        END INTERFACE 
! Abstraction of check_negative

        INTERFACE check_negative
            MODULE PROCEDURE check_negative_total, check_negative_index
        END INTERFACE 
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!---------------------------------------------------------------------------------

        SUBROUTINE length_check(lscale, lscale_up, lscale_down, err_code)
!
!        Description: This subroutine determines if any of the output
!        variables for the length_new subroutine carry values that
!        are NaNs.
!
!        Joshua Fasching February 2008
!---------------------------------------------------------------------------------
            USE grid_class, ONLY: gr
! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Constant Parameters
            CHARACTER(LEN=*), parameter :: proc_name = "compute_length"
! Input Variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: lscale
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: lscale_up
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: lscale_down
! Mixing length                 [m]
! Upward mixing length          [m]
! Downward mixing length        [m]
! Output Variable
            INTEGER, intent(inout) :: err_code
!-----------------------------------------------------------------------------
    call check_nan( Lscale, "Lscale", proc_name, err_code )
    call check_nan( Lscale_up, "Lscale_up", proc_name, err_code )
    call check_nan( Lscale_down, "Lscale_down", proc_name, err_code )
    return
        END SUBROUTINE length_check
!---------------------------------------------------------------------------

        SUBROUTINE pdf_closure_check(wp4, wprtp2, wp2rtp, wpthlp2, wp2thlp, cloud_frac, rcm, wpthvp, wp2thvp, rtpthvp, thlpthvp, &
        wprcp, wp2rcp, rtprcp, thlprcp, rcp2, wprtpthlp, crt_1, crt_2, cthl_1, cthl_2, pdf_params, sclrpthvp, sclrprcp, wpsclrp2, &
        wpsclrprtp, wpsclrpthlp, wp2sclrp, err_code)
! Description: This subroutine determines if any of the output
!   variables for the pdf_closure subroutine carry values that
!   are NaNs.
!
! Joshua Fasching February 2008
!---------------------------------------------------------------------------
            USE parameters_model, ONLY: sclr_dim
! Variable
            USE pdf_parameter_module, ONLY: pdf_parameter
! type
            USE stats_variables, ONLY: iwp4
            USE stats_variables, ONLY: iwprtp2
            USE stats_variables, ONLY: iwpthlp2
            USE stats_variables, ONLY: ircp2
            USE stats_variables, ONLY: iwprtpthlp
! Variables
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Parameter Constants
            CHARACTER(LEN=*), parameter :: proc_name =       "pdf_closure"
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: wp2thvp
            REAL(KIND=core_rknd), intent(in) :: rtpthvp
            REAL(KIND=core_rknd), intent(in) :: thlpthvp
            REAL(KIND=core_rknd), intent(in) :: cthl_1
            REAL(KIND=core_rknd), intent(in) :: wprcp
            REAL(KIND=core_rknd), intent(in) :: wp2rcp
            REAL(KIND=core_rknd), intent(in) :: rtprcp
            REAL(KIND=core_rknd), intent(in) :: thlprcp
            REAL(KIND=core_rknd), intent(in) :: rcp2
            REAL(KIND=core_rknd), intent(in) :: wprtpthlp
            REAL(KIND=core_rknd), intent(in) :: crt_1
            REAL(KIND=core_rknd), intent(in) :: crt_2
            REAL(KIND=core_rknd), intent(in) :: cthl_2
            REAL(KIND=core_rknd), intent(in) :: wp4
            REAL(KIND=core_rknd), intent(in) :: wpthlp2
            REAL(KIND=core_rknd), intent(in) :: wp2rtp
            REAL(KIND=core_rknd), intent(in) :: wprtp2
            REAL(KIND=core_rknd), intent(in) :: wp2thlp
            REAL(KIND=core_rknd), intent(in) :: cloud_frac
            REAL(KIND=core_rknd), intent(in) :: rcm
            REAL(KIND=core_rknd), intent(in) :: wpthvp
! w'^4                  [m^4/s^4]
! w' r_t'               [(m kg)/(s kg)]
! w'^2 r_t'             [(m^2 kg)/(s^2 kg)]
! w' th_l'^2            [(m K^2)/s]
! w'^2 th_l'            [(m^2 K)/s^2]
! Cloud fraction        [-]
! Mean liquid water     [kg/kg]
! Buoyancy flux         [(K m)/s]
! w'^2 th_v'            [(m^2 K)/s^2]
! r_t' th_v'            [(kg K)/kg]
! th_l' th_v'           [K^2]
! w' r_c'               [(m kg)/(s kg)]
! w'^2 r_c'             [(m^2 kg)/(s^2 kg)]
! r_t' r_c'             [(kg^2)/(kg^2)]
! th_l' r_c'            [(K kg)/kg]
! r_c'^2                [(kg^2)/(kg^2)]
! w' r_t' th_l'         [(m kg K)/(s kg)]
            TYPE(pdf_parameter), intent(in) :: pdf_params
! PDF parameters          [units vary]
! Input (Optional passive scalar variables)
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: wpsclrprtp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: wpsclrpthlp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrprcp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrpthvp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: wp2sclrp
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: wpsclrp2
! Output Variable
            INTEGER, intent(inout) :: err_code
! Returns appropriate error code
!-------------------------------------------------------------------------------
! ---- Begin Code ----
    if ( iwp4 > 0 ) call check_nan( wp4,"wp4", proc_name, err_code )
    if ( iwprtp2 > 0 ) call check_nan( wprtp2,"wprtp2", proc_name, err_code )
    call check_nan( wp2rtp,"wp2rtp", proc_name, err_code )
    if ( iwpthlp2 > 0 ) call check_nan( wpthlp2,"wpthlp2", proc_name, err_code )
    call check_nan( wp2thlp,"wp2thlp", proc_name, err_code )
    call check_nan( cloud_frac,"cloud_frac", proc_name, err_code )
    call check_nan( rcm,"rcm", proc_name, err_code )
    call check_nan( wpthvp, "wpthvp", proc_name, err_code )
    call check_nan( wp2thvp, "wp2thvp", proc_name, err_code )
    call check_nan( rtpthvp, "rtpthvp", proc_name, err_code )
    call check_nan( thlpthvp, "thlpthvp", proc_name, err_code )
    call check_nan( wprcp, "wprcp", proc_name, err_code )
    call check_nan( wp2rcp, "wp2rcp", proc_name, err_code )
    call check_nan( rtprcp, "rtprcp", proc_name, err_code )
    call check_nan( thlprcp, "thlprcp", proc_name, err_code )
    if ( ircp2 >  0 ) call check_nan( rcp2, "rcp2", proc_name, err_code)
    if ( iwprtpthlp > 0 ) call check_nan( wprtpthlp, "wprtpthlp", proc_name, err_code )
    call check_nan( crt_1, "crt_1", proc_name, err_code )
    call check_nan( crt_2, "crt_2", proc_name, err_code )
    call check_nan( cthl_1, "cthl_1", proc_name, err_code )
    call check_nan( cthl_2, "cthl_2", proc_name, err_code )
! Check each PDF parameter at the grid level sent in.
    call check_nan( pdf_params%w_1, "pdf_params%w_1", proc_name, err_code )
    call check_nan( pdf_params%w_2, "pdf_params%w_2", proc_name, err_code )
    call check_nan( pdf_params%varnce_w_1, "pdf_params%varnce_w_1", proc_name, err_code )
    call check_nan( pdf_params%varnce_w_2, "pdf_params%varnce_w_2", proc_name, err_code )
    call check_nan( pdf_params%rt_1, "pdf_params%rt_1", proc_name, err_code )
    call check_nan( pdf_params%rt_2, "pdf_params%rt_2", proc_name, err_code )
    call check_nan( pdf_params%varnce_rt_1, "pdf_params%varnce_rt_1", proc_name, err_code )
    call check_nan( pdf_params%varnce_rt_2, "pdf_params%varnce_rt_2", proc_name, err_code )
    call check_nan( pdf_params%thl_1, "pdf_params%thl_1", proc_name, err_code )
    call check_nan( pdf_params%thl_2, "pdf_params%thl_2", proc_name, err_code )
    call check_nan( pdf_params%varnce_thl_1, "pdf_params%varnce_thl_1", proc_name, err_code )
    call check_nan( pdf_params%varnce_thl_2, "pdf_params%varnce_thl_2", proc_name, err_code )
    call check_nan( pdf_params%mixt_frac, "pdf_params%mixt_frac", proc_name, err_code )
    call check_nan( pdf_params%rrtthl, "pdf_params%rrtthl", proc_name, err_code )
    call check_nan( pdf_params%rc_1, "pdf_params%rc_1", proc_name, err_code )
    call check_nan( pdf_params%rc_2, "pdf_params%rc_2", proc_name, err_code )
    call check_nan( pdf_params%rsatl_1, "pdf_params%rsatl_1", proc_name, err_code )
    call check_nan( pdf_params%rsatl_2, "pdf_params%rsatl_2", proc_name, err_code )
    call check_nan( pdf_params%cloud_frac_1, "pdf_params%cloud_frac_1", proc_name, err_code )
    call check_nan( pdf_params%cloud_frac_2, "pdf_params%cloud_frac_2", proc_name, err_code )
    call check_nan( pdf_params%chi_1, "pdf_params%chi_1", proc_name, err_code )
    call check_nan( pdf_params%chi_2, "pdf_params%chi_2", proc_name, err_code )
    call check_nan( pdf_params%stdev_chi_1, "pdf_params%stdev_chi_1", proc_name, err_code )
    call check_nan( pdf_params%stdev_chi_2, "pdf_params%stdev_chi_2", proc_name, err_code )
    call check_nan( pdf_params%alpha_thl, "pdf_params%alpha_thl", proc_name, err_code )
    call check_nan( pdf_params%alpha_rt, "pdf_params%alpha_rt", proc_name, err_code )
    if ( sclr_dim > 0 ) then
      call check_nan( sclrpthvp,"sclrpthvp", & 
                      proc_name, err_code)
      call check_nan( sclrprcp, "sclrprcp", & 
                      proc_name, err_code )
      call check_nan( wpsclrprtp, "wpsclrprtp",  & 
                      proc_name, err_code )
      call check_nan( wpsclrp2, "wpsclrp2",  & 
                      proc_name, err_code )
      call check_nan( wpsclrpthlp, "wpsclrtlp",  & 
                      proc_name, err_code )
      call check_nan( wp2sclrp, "wp2sclrp",  & 
                      proc_name, err_code )
    end if
    return
        END SUBROUTINE pdf_closure_check
!-------------------------------------------------------------------------------

        SUBROUTINE parameterization_check(thlm_forcing, rtm_forcing, um_forcing, vm_forcing, wm_zm, wm_zt, p_in_pa, rho_zm, rho, &
        exner, rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, wpthlp_sfc, wprtp_sfc, upwp_sfc, &
        vpwp_sfc, um, upwp, vm, vpwp, up2, vp2, rtm, wprtp, thlm, wpthlp, wp2, wp3, rtp2, thlp2, rtpthlp, prefix, wpsclrp_sfc, &
        wpedsclrp_sfc, sclrm, wpsclrp, sclrp2, sclrprtp, sclrpthlp, sclrm_forcing, edsclrm, edsclrm_forcing, err_code)
!
! Description:
!   This subroutine determines what input variables may have NaN values.
!   In addition it checks to see if rho_zm, rho, exner, up2, vp2, rtm, thlm,
!   wp2, rtp2, thlp2, or tau_zm have negative values.
!-------------------------------------------------------------------------------
            USE grid_class, ONLY: gr
! Variable
            USE parameters_model, ONLY: edsclr_dim
            USE parameters_model, ONLY: sclr_dim
! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Constant Parameters
! Name of the procedure using parameterization_check
            CHARACTER(LEN=25), parameter :: proc_name = "parameterization_timestep"
! Input variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: p_in_pa
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: exner
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: invrs_rho_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rho_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: invrs_rho_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wm_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thv_ds_zm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thv_ds_zt
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: um_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlm_forcing
! theta_l forcing (thermodynamic levels)    [K/s]
! r_t forcing (thermodynamic levels)        [(kg/kg)/s]
! u wind forcing (thermodynamic levels)     [m/s/s]
! v wind forcing (thermodynamic levels)     [m/s/s]
! w mean wind component on momentum levels  [m/s]
! w mean wind component on thermo. levels   [m/s]
! Air pressure (thermodynamic levels)       [Pa]
! Air density on momentum levels            [kg/m^3]
! Air density on thermodynamic levels       [kg/m^3]
! Exner function (thermodynamic levels)     [-]
! Dry, static density on momentum levels    [kg/m^3]
! Dry, static density on thermo. levels     [kg/m^3]
! Inv. dry, static density @ momentum levs. [m^3/kg]
! Inv. dry, static density @ thermo. levs.  [m^3/kg]
! Dry, base-state theta_v on momentum levs. [K]
! Dry, base-state theta_v on thermo. levs.  [K]
            REAL(KIND=core_rknd), intent(in) :: wpthlp_sfc
            REAL(KIND=core_rknd), intent(in) :: wprtp_sfc
            REAL(KIND=core_rknd), intent(in) :: upwp_sfc
            REAL(KIND=core_rknd), intent(in) :: vpwp_sfc
! w' theta_l' at surface.   [(m K)/s]
! w' r_t' at surface.       [(kg m)/( kg s)]
! u'w' at surface.          [m^2/s^2]
! v'w' at surface.          [m^2/s^2]
! These are prognostic or are planned to be in the future
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: upwp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: up2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: thlp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wpthlp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: rtpthlp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wp3
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: vpwp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: wprtp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: um
! u mean wind component (thermodynamic levels)   [m/s]
! u'w' (momentum levels)                         [m^2/s^2]
! v mean wind component (thermodynamic levels)   [m/s]
! v'w' (momentum levels)                         [m^2/s^2]
! u'^2 (momentum levels)                         [m^2/s^2]
! v'^2 (momentum levels)                         [m^2/s^2]
! total water mixing ratio, r_t (thermo. levels) [kg/kg]
! w' r_t' (momentum levels)                      [(kg/kg) m/s]
! liq. water pot. temp., th_l (thermo. levels)   [K]
! w' th_l' (momentum levels)                     [(m/s) K]
! r_t'^2 (momentum levels)                       [(kg/kg)^2]
! th_l'^2 (momentum levels)                      [K^2]
! r_t' th_l' (momentum levels)                   [(kg/kg) K]
! w'^2 (momentum levels)                         [m^2/s^2]
! w'^3 (thermodynamic levels)                    [m^3/s^3]
            CHARACTER(LEN=*), intent(in) :: prefix ! Location where subroutine is called
            REAL(KIND=core_rknd), intent(in), dimension(sclr_dim) :: wpsclrp_sfc
! Scalar flux at surface [units m/s]
            REAL(KIND=core_rknd), intent(in), dimension(edsclr_dim) :: wpedsclrp_sfc
! Eddy-Scalar flux at surface      [units m/s]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrp2
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrm_forcing
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrprtp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrpthlp
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: sclrm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,sclr_dim) :: wpsclrp
! Passive scalar mean      [units vary]
! w'sclr'                  [units vary]
! sclr'^2                  [units vary]
! sclr'rt'                 [units vary]
! sclr'thl'                [units vary]
! Passive scalar forcing   [units / s]
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,edsclr_dim) :: edsclrm
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz,edsclr_dim) :: edsclrm_forcing
! Eddy passive scalar mean    [units vary]
! Eddy passive scalar forcing [units / s]
! In / Out Variables
            INTEGER, intent(inout) :: err_code
! Error code
! Local Variables
            INTEGER :: i ! Loop iterator for the scalars
!-------- Input Nan Check ----------------------------------------------
    call check_nan( thlm_forcing, "thlm_forcing", prefix//proc_name, err_code)
    call check_nan( rtm_forcing,"rtm_forcing", prefix//proc_name, err_code )
    call check_nan( um_forcing,"um_forcing", prefix//proc_name, err_code )
    call check_nan( vm_forcing,"vm_forcing", prefix//proc_name, err_code )
    call check_nan( wm_zm, "wm_zm", prefix//proc_name, err_code )
    call check_nan( wm_zt, "wm_zt", prefix//proc_name, err_code )
    call check_nan( p_in_Pa, "p_in_Pa", prefix//proc_name, err_code )
    call check_nan( rho_zm, "rho_zm", prefix//proc_name, err_code )
    call check_nan( rho, "rho", prefix//proc_name, err_code )
    call check_nan( exner, "exner", prefix//proc_name, err_code )
    call check_nan( rho_ds_zm, "rho_ds_zm", prefix//proc_name, err_code )
    call check_nan( rho_ds_zt, "rho_ds_zt", prefix//proc_name, err_code )
    call check_nan( invrs_rho_ds_zm, "invrs_rho_ds_zm", prefix//proc_name, err_code )
    call check_nan( invrs_rho_ds_zt, "invrs_rho_ds_zt", prefix//proc_name, err_code )
    call check_nan( thv_ds_zm, "thv_ds_zm", prefix//proc_name, err_code )
    call check_nan( thv_ds_zt, "thv_ds_zt", prefix//proc_name, err_code )
    call check_nan( um, "um", prefix//proc_name, err_code )
    call check_nan( upwp, "upwp", prefix//proc_name, err_code )
    call check_nan( vm, "vm", prefix//proc_name, err_code )
    call check_nan( vpwp, "vpwp", prefix//proc_name, err_code )
    call check_nan( up2, "up2", prefix//proc_name, err_code )
    call check_nan( vp2, "vp2", prefix//proc_name, err_code )
    call check_nan( rtm, "rtm", prefix//proc_name, err_code )
    call check_nan( wprtp, "wprtp", prefix//proc_name, err_code )
    call check_nan( thlm, "thlm", prefix//proc_name, err_code )
    call check_nan( wpthlp, "wpthlp", prefix//proc_name, err_code )
    call check_nan( wp2, "wp2", prefix//proc_name, err_code )
    call check_nan( wp3, "wp3", prefix//proc_name, err_code )
    call check_nan( rtp2, "rtp2", prefix//proc_name, err_code )
    call check_nan( thlp2, "thlp2", prefix//proc_name, err_code )
    call check_nan( rtpthlp, "rtpthlp", prefix//proc_name, err_code )
    call check_nan( wpthlp_sfc, "wpthlp_sfc", prefix//proc_name, err_code )
    call check_nan( wprtp_sfc, "wprtp_sfc", prefix//proc_name, err_code )
    call check_nan( upwp_sfc, "upwp_sfc", prefix//proc_name, err_code )
    call check_nan( vpwp_sfc, "vpwp_sfc", prefix//proc_name, err_code )
    do i = 1, sclr_dim
      call check_nan( sclrm_forcing(:,i),"sclrm_forcing",  & 
                      prefix//proc_name, err_code )
      call check_nan( wpsclrp_sfc(i),"wpsclrp_sfc",  & 
                      prefix//proc_name, err_code )
      call check_nan( sclrm(:,i),"sclrm", prefix//proc_name, err_code )
      call check_nan( wpsclrp(:,i),"wpsclrp", prefix//proc_name, err_code )
      call check_nan( sclrp2(:,i),"sclrp2", prefix//proc_name, err_code )
      call check_nan( sclrprtp(:,i),"sclrprtp", prefix//proc_name, err_code )
      call check_nan( sclrpthlp(:,i),"sclrpthlp", prefix//proc_name, err_code )
    end do
    do i = 1, edsclr_dim
      call check_nan( edsclrm_forcing(:,i),"edsclrm_forcing", prefix//proc_name, err_code )
      call check_nan( wpedsclrp_sfc(i),"wpedsclrp_sfc",  & 
                      prefix//proc_name, err_code )
      call check_nan( edsclrm(:,i),"edsclrm", prefix//proc_name, err_code )
    enddo
!---------------------------------------------------------------------
    call check_negative( rtm, gr%nz ,"rtm", prefix//proc_name, err_code )
    call check_negative( p_in_Pa, gr%nz ,"p_in_Pa", prefix//proc_name, err_code )
    call check_negative( rho, gr%nz ,"rho", prefix//proc_name, err_code )
    call check_negative( rho_zm, gr%nz ,"rho_zm", prefix//proc_name, err_code )
    call check_negative( exner, gr%nz ,"exner", prefix//proc_name, err_code )
    call check_negative( rho_ds_zm, gr%nz ,"rho_ds_zm", prefix//proc_name, err_code )
    call check_negative( rho_ds_zt, gr%nz ,"rho_ds_zt", prefix//proc_name, err_code )
    call check_negative( invrs_rho_ds_zm, gr%nz ,"invrs_rho_ds_zm", &
                         prefix//proc_name, err_code )
    call check_negative( invrs_rho_ds_zt, gr%nz ,"invrs_rho_ds_zt", &
                         prefix//proc_name, err_code )
    call check_negative( thv_ds_zm, gr%nz ,"thv_ds_zm", prefix//proc_name, err_code )
    call check_negative( thv_ds_zt, gr%nz ,"thv_ds_zt", prefix//proc_name, err_code )
    call check_negative( up2, gr%nz ,"up2", prefix//proc_name, err_code )
    call check_negative( vp2, gr%nz ,"vp2", prefix//proc_name, err_code )
    call check_negative( wp2, gr%nz ,"wp2", prefix//proc_name, err_code )
    call check_negative( rtm, gr%nz ,"rtm", prefix//proc_name, err_code )
    call check_negative( thlm, gr%nz ,"thlm", prefix//proc_name, err_code )
    call check_negative( rtp2, gr%nz ,"rtp2", prefix//proc_name, err_code )
    call check_negative( thlp2, gr%nz ,"thlp2", prefix//proc_name, err_code )
    return
        END SUBROUTINE parameterization_check
!-----------------------------------------------------------------------

        SUBROUTINE surface_varnce_check(wp2_sfc, up2_sfc, vp2_sfc, thlp2_sfc, rtp2_sfc, rtpthlp_sfc, sclrp2_sfc, sclrprtp_sfc, &
        sclrpthlp_sfc, err_code)
!
!       Description:This subroutine determines if any of the output
!       variables for the surface_varnce subroutine carry values that
!       are nans.
!
!       Joshua Fasching February 2008
!
!
!-----------------------------------------------------------------------
            USE parameters_model, ONLY: sclr_dim
! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Constant Parameters
! Name of the subroutine calling the check
            CHARACTER(LEN=*), parameter :: proc_name = "surface_varnce"
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: up2_sfc
            REAL(KIND=core_rknd), intent(in) :: vp2_sfc
            REAL(KIND=core_rknd), intent(in) :: wp2_sfc
            REAL(KIND=core_rknd), intent(in) :: thlp2_sfc
            REAL(KIND=core_rknd), intent(in) :: rtp2_sfc
            REAL(KIND=core_rknd), intent(in) :: rtpthlp_sfc
! Vertical velocity variance        [m^2/s^2]
! u'^2                              [m^2/s^2]
! u'^2                              [m^2/s^2]
! thetal variance                   [K^2]
! rt variance                       [(kg/kg)^2]
! thetal rt covariance              [kg K/kg]
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrp2_sfc
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrprtp_sfc
            REAL(KIND=core_rknd), dimension(sclr_dim), intent(in) :: sclrpthlp_sfc
! Passive scalar variance                 [units^2]
! Passive scalar r_t covariance           [units kg/kg]
! Passive scalar theta_l covariance       [units K]
! Input/Output Variable
            INTEGER, intent(inout) :: err_code ! Are these outputs valid?
!-----------------------------------------------------------------------
! ---- Begin Code ----
    call check_nan( wp2_sfc, "wp2_sfc", proc_name, err_code)
    call check_nan( up2_sfc, "up2_sfc", proc_name, err_code)
    call check_nan( vp2_sfc, "vp2_sfc", proc_name, err_code)
    call check_nan( thlp2_sfc, "thlp2_sfc", proc_name, err_code)
    call check_nan( rtp2_sfc, "rtp2_sfc", proc_name, err_code)
    call check_nan( rtpthlp_sfc, "rtpthlp_sfc",  & 
                    proc_name, err_code)
    if ( sclr_dim > 0 ) then
      call check_nan( sclrp2_sfc, "sclrp2_sfc", & 
                      proc_name, err_code )
      call check_nan( sclrprtp_sfc, "sclrprtp_sfc", & 
                      proc_name, err_code )
      call check_nan( sclrpthlp_sfc, "sclrpthlp_sfc",  & 
                      proc_name, err_code )
    end if
    return
        END SUBROUTINE surface_varnce_check
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!------------------------------------------------------------------------

        logical FUNCTION is_nan_sclr(xarg)
! Description:
!   Checks if a given scalar real is a NaN, +inf or -inf.
! Notes:
!   I was advised by Andy Vaught to use a data statement and the transfer( )
!   intrinsic rather than using a hex number in a parameter for portability.
!   Certain compiler optimizations may cause variables with invalid
!   results to flush to zero.  Avoid these!
!  -dschanen 16 Dec 2010
!------------------------------------------------------------------------
            USE parameters_model, ONLY: posinf
! Variable(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: xarg
! ---- Begin Code ---
! This works on compilers with standardized floating point,
! because the IEEE 754 spec defines that subnormals and nans
! should not equal themselves.
! However, all compilers do not seem to follow this.
    if (xarg /= xarg ) then
      is_nan_sclr = .true.
! This a second check, assuming the above does not work as
! expected.
    else if ( xarg == PosInf ) then
      is_nan_sclr = .true.
    else
      is_nan_sclr = .false. ! Our result should be a standard float ! Our result should be a standard float
    end if
    return
        END FUNCTION is_nan_sclr
!------------------------------------------------------------------------
!------------------------------------------------------------------------

        logical FUNCTION is_nan_2d(x2d)
! Description:
!   Checks if a given real vector is a NaN, +inf or -inf.
!------------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC any
! Input Variables
            REAL(KIND=core_rknd), dimension(:), intent(in) :: x2d
! Local Variables
            INTEGER :: k
! ---- Begin Code ----
    is_nan_2d = .false.
    do k = 1, size( x2d )
      if ( is_nan_sclr( x2d(k) ) ) then
        is_nan_2d = .true.
        exit
      end if
    end do
    return
        END FUNCTION is_nan_2d
!------------------------------------------------------------------------

        SUBROUTINE check_negative_total(var, varname, operation, err_code)
!
! Description:
!   Checks for negative values in the var array and reports them.
!
!-----------------------------------------------------------------------
            USE constants_clubb, ONLY: fstderr
! Variable(s)
            USE error_code, ONLY: clubb_var_less_than_zero
! Variable(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC any, present
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(:) :: var
            CHARACTER(LEN=*), intent(in) :: varname
            CHARACTER(LEN=*), intent(in) :: operation
! Varible being examined
! Procedure calling check_zero
! Optional In/Out Variable
            INTEGER, optional, intent(inout) :: err_code
    if ( any( var < 0.0_core_rknd ) ) then
      write(fstderr,*) varname, " < 0 in ", operation
      if ( present( err_code ) ) then
        if (err_code < clubb_var_less_than_zero ) then
          err_code = clubb_var_less_than_zero
        end if
      end if
    end if ! any ( var < 0 ) ! any ( var < 0 )
    return
        END SUBROUTINE check_negative_total
!------------------------------------------------------------------------

        SUBROUTINE check_negative_index(var, ndim, varname, operation, err_code)
!
! Description:
!   Checks for negative values in the var array and reports
!   the index in which the negative values occur.
!
!-----------------------------------------------------------------------
            USE constants_clubb, ONLY: fstderr
! Variable
            USE error_code, ONLY: clubb_var_less_than_zero
! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC any, present
! Input Variables
            INTEGER, intent(in) :: ndim
            REAL(KIND=core_rknd), intent(in), dimension(ndim) :: var
            CHARACTER(LEN=*), intent(in) :: operation
            CHARACTER(LEN=*), intent(in) :: varname
! Varible being examined
! Procedure calling check_zero
! Optional In/Out Variable
            INTEGER, optional, intent(inout) :: err_code
! Local Variable
            INTEGER :: k ! Loop iterator
    do k=1,ndim,1
      if ( var(k) < 0.0_core_rknd )  then
        write(fstderr,*) varname, " < 0 in ", operation,  & 
                         " at k = ", k
        if ( present( err_code ) ) then
          if (err_code < clubb_var_less_than_zero ) then
            err_code = clubb_var_less_than_zero
          end if
        end if
      end if
    end do ! 1..n ! 1..n
    return
        END SUBROUTINE check_negative_index
!------------------------------------------------------------------------

        SUBROUTINE check_nan_2d(var, varname, operation, err_code)
!
!  Description:
!    Checks for a NaN in the var array and reports it.
!
!
!------------------------------------------------------------------------
            USE constants_clubb, ONLY: fstderr
! Variable(s)
            USE error_code, ONLY: clubb_var_equals_nan
! Variable(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC present
! Input variables
            REAL(KIND=core_rknd), intent(in), dimension(:) :: var ! Variable being examined
            CHARACTER(LEN=*), intent(in) :: varname
            CHARACTER(LEN=*), intent(in) :: operation
! Name of variable
! Procedure calling check_nan
! Optional In/Out Variable
            INTEGER, optional, intent(inout) :: err_code
    if ( is_nan_2d( var ) ) then
      write(fstderr,*) varname, " is NaN in ",operation
      if ( present( err_code ) ) then
        if( err_code < clubb_var_equals_NaN ) then
          err_code = clubb_var_equals_NaN
        end if
      end if
    end if
    return
        END SUBROUTINE check_nan_2d
!-----------------------------------------------------------------------

        SUBROUTINE check_nan_sclr(var, varname, operation, err_code)
!
! Description:
!   Checks for a NaN in the scalar var then reports it.
!
!-----------------------------------------------------------------------
            USE constants_clubb, ONLY: fstderr
! Variable
            USE error_code, ONLY: clubb_var_equals_nan
! Variable
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC present
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: var ! Variable being examined
            CHARACTER(LEN=*), intent(in) :: varname
            CHARACTER(LEN=*), intent(in) :: operation
! Name of variable being examined
! Procedure calling check_nan
! Optional In/Out variable
            INTEGER, optional, intent(inout) :: err_code
!--------------------------------------------------------------------
    if ( is_nan_sclr( var ) ) then
      write(fstderr,*) varname, " is NaN in ",operation
      if ( present( err_code ) ) then
        if( err_code < clubb_var_equals_NaN ) then
          err_code = clubb_var_equals_NAN
        end if
      end if
    end if
    return
        END SUBROUTINE check_nan_sclr
!-------------------------------------------------------------------------
!-----------------------------------------------------------------------

        pure FUNCTION calculate_spurious_source(integral_after, integral_before, flux_top, flux_sfc, integral_forcing, dt) RESULT &
        ( spurious_source )
!
! Description:
!   Checks whether there is conservation within the column and returns any
!   imbalance as spurious_source where spurious_source is defined negative
!   for a spurious sink.
!
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: integral_before
            REAL(KIND=core_rknd), intent(in) :: flux_top
            REAL(KIND=core_rknd), intent(in) :: flux_sfc
            REAL(KIND=core_rknd), intent(in) :: integral_after
            REAL(KIND=core_rknd), intent(in) :: integral_forcing
            REAL(KIND=core_rknd), intent(in) :: dt
! Vertically-integrated quantity after dt time  [units vary]
! Vertically-integrated quantity before dt time [units vary]
! Total flux at the top of the domain           [units vary]
! Total flux at the bottom of the domain        [units vary]
! Vertically-integrated forcing                 [units vary]
! Timestep size                                 [s]
! Return Variable
            REAL(KIND=core_rknd) :: spurious_source ! [units vary]
!--------------------------------------------------------------------
! ---- Begin Code ----
    spurious_source = (integral_after - integral_before) / dt & 
                        + flux_top - flux_sfc - integral_forcing
    return
        END FUNCTION calculate_spurious_source
!-------------------------------------------------------------------------
    END MODULE numerical_check
