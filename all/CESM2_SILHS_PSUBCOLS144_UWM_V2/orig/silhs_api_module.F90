!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:54 
!KGEN version : 0.8.1 
  
!--------------------------------------------------------------------------------------------------
! $Id$
!==================================================================================================
!       ######## ########### ###        ###    ###  ########            ###     ######### #########
!     ###    ###    ###     ###        ###    ### ###    ###         ### ###   ###    ###   ###
!    ###           ###     ###        ###    ### ###               ###   ###  ###    ###   ###
!   ##########    ###     ###        ########## ##########       ########### #########    ###
!         ###    ###     ###        ###    ###        ###       ###     ### ###          ###
! ###    ###    ###     ###        ###    ### ###    ###       ###     ### ###          ###
! ######## ########### ########## ###    ###  ########        ###     ### ###       #########
! The None API serves as the doorway through which external models can interact with None.
!               PLEASE REMEMBER, IF ANY CODE IS CHANGED IN THIS DOCUMENT,
!                   THE CHANGES MUST BE PROPOGATED TO ALL HOST MODELS.
! Cloud Layers Unified By Binormals (CLUBB) user license 
! agreement.
! Thank you for your interest in CLUBB. We work hard to create a
! code that implements the best software engineering practices,
! is supported to the extent allowed by our limited resources,
! and is available without cost to non-commercial users. You may
! use CLUBB if, in return, you abide by these conditions:
! 1. Please cite CLUBB in presentations and publications that
!  contain results obtained using CLUBB.
! 2. You may not use any part of CLUBB to create or modify
!  another single-column (1D) model that is not called CLUBB.
!  However, you may modify or augment CLUBB or parts of CLUBB if
!  you include "CLUBB" in the name of the resulting single-column
!  model. For example, a user at MIT might modify CLUBB and call
!  the modified version "CLUBB-MIT." Or, for example, a user of
!  the CLM land-surface model might interface CLM to CLUBB and
!  call it "CLM-CLUBB." This naming convention recognizes the
!  contributions of both sets of developers.
! 3. You may implement CLUBB as a parameterization in a large-
!  scale host model that has 2 or 3 spatial dimensions without 
!  including "CLUBB" in the combined model name, but please 
!  acknowledge in presentations and publications that CLUBB has 
!  been included as a parameterization.
! 4. You may not provide all or part of CLUBB to anyone without 
!  prior permission from Vincent Larson (vlarson@uwm.edu). If 
!  you wish to share CLUBB with your collaborators without 
!  seeking permission, please ask your collaborators to register 
!  as CLUBB users at http://clubb.larson-group.com and to 
!  download CLUBB from there.
! 5. You may not use CLUBB for commercial purposes unless you 
!  receive permission from Vincent Larson.
! 6. You may not re-license all or any part of CLUBB.
! 7. CLUBB is provided "as is" and without warranty.
! We hope that CLUBB will develop into a community resource. We 
! encourage users to contribute their CLUBB modifications or 
! extensions to the CLUBB development group. We will then 
! consider them for inclusion in CLUBB. Such contributions will 
! benefit all CLUBB users. We would be pleased to acknowledge 
! contributors and list their CLUBB-related papers on our "About 
! CLUBB" webpage (http://clubb.larson-group.com/about.php) for 
! those contributors who so desire.
! Thanks so much and best wishes for your research!
! The CLUBB Development Group
! (Present and past contributors to the source code include 
! Vincent Larson, Chris Golaz, David Schanen, Brian Griffin, 
! Joshua Fasching, Adam Smith, and Michael Falk).
!------------------------------------------------------------------


!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!

module silhs_api_module


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


    IMPLICIT NONE 

    PRIVATE 


    PUBLIC generate_silhs_sample_api 

contains
  !================================================================================================
  ! generate_silhs_sample - Generates sample points of moisture, temperature, et cetera.
  !================================================================================================


  subroutine generate_silhs_sample_api( &
    iter, pdf_dim, num_samples, sequence_length, nz, & ! In
    l_calc_weights_all_levs_itime, &
    pdf_params, delta_zm, rcm, Lscale, & ! In
    rho_ds_zt, mu1, mu2, sigma1, sigma2, & ! In
    corr_cholesky_mtx_1, corr_cholesky_mtx_2, & ! In
    hydromet_pdf_params, & ! In
    X_nl_all_levs, X_mixt_comp_all_levs, & ! Out
    lh_sample_point_weights ) ! Out

      USE latin_hypercube_driver_module, ONLY: generate_silhs_sample 

      USE pdf_parameter_module, ONLY: pdf_parameter 

      USE hydromet_pdf_parameter_module, ONLY: hydromet_pdf_parameter 

      USE clubb_precision, ONLY: core_rknd 
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE hydromet_pdf_parameter_module, ONLY: kr_kgen_hydromet_pdf_parameter_module_typesubp0 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
      USE hydromet_pdf_parameter_module, ONLY: kv_kgen_hydromet_pdf_parameter_module_typesubp0 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      iter,            & ! Model iteration number
      pdf_dim,     & ! Number of variables to sample
      num_samples,     & ! Number of samples per variable
      sequence_length, & ! nt_repeat/num_samples; number of timesteps before sequence repeats.
      nz                 ! Number of vertical model levels

    type(pdf_parameter), dimension(nz), intent(in) :: &
      pdf_params ! PDF parameters       [units vary]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      delta_zm, &  ! Difference in moment. altitudes    [m]
      rcm          ! Liquid water mixing ratio          [kg/kg]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      rho_ds_zt    ! Dry, static density on thermo. levels    [kg/m^3]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      Lscale       ! Turbulent Mixing Length  [m]
    ! Output Variables

    real( kind = core_rknd ), intent(out), dimension(nz,num_samples,pdf_dim) :: &
      X_nl_all_levs ! Sample that is transformed ultimately to normal-lognormal

    integer, intent(out), dimension(nz,num_samples) :: &
      X_mixt_comp_all_levs ! Which mixture component we're in

    real( kind = core_rknd ), intent(out), dimension(nz,num_samples) :: &
      lh_sample_point_weights
    ! More Input Variables!

    real( kind = core_rknd ), dimension(pdf_dim,pdf_dim,nz), intent(in) :: &
      corr_cholesky_mtx_1, & ! Correlations Cholesky matrix (1st comp.)  [-]
      corr_cholesky_mtx_2    ! Correlations Cholesky matrix (2nd comp.)  [-]

    real( kind = core_rknd ), dimension(pdf_dim,nz), intent(in) :: &
      mu1,    & ! Means of the hydrometeors, 1st comp. (chi, eta, w, <hydrometeors>)  [units vary]
      mu2,    & ! Means of the hydrometeors, 2nd comp. (chi, eta, w, <hydrometeors>)  [units vary]
      sigma1, & ! Stdevs of the hydrometeors, 1st comp. (chi, eta, w, <hydrometeors>) [units vary]
      sigma2    ! Stdevs of the hydrometeors, 2nd comp. (chi, eta, w, <hydrometeors>) [units vary]

    logical, intent(in) :: &
      l_calc_weights_all_levs_itime ! determines if vertically correlated sample points are needed
      
    type(hydromet_pdf_parameter), dimension(nz), intent(in) :: &
      hydromet_pdf_params

    call generate_silhs_sample( &
      iter, pdf_dim, num_samples, sequence_length, nz, & ! In
      l_calc_weights_all_levs_itime, & ! In
      pdf_params, delta_zm, rcm, Lscale, & ! In
      rho_ds_zt, mu1, mu2, sigma1, sigma2, & ! In
      corr_cholesky_mtx_1, corr_cholesky_mtx_2, & ! In
      hydromet_pdf_params, & ! In
      X_nl_all_levs, X_mixt_comp_all_levs, & ! Out
      lh_sample_point_weights ) ! Out

  end subroutine generate_silhs_sample_api
  !================================================================================================
  ! stats_accumulate_lh - Clips subcolumns from latin hypercube and creates stats.
  !================================================================================================


  !================================================================================================
  ! est_kessler_microphys - Computes microphysical grid box means of Kessler autoconversion scheme.
  !================================================================================================


  !================================================================================================
  ! clip_transform_silhs_output - Computes extra None sample variables, such as rt and thl.
  !================================================================================================


  !-----------------------------------------------------------------
  ! lh_microphys_var_covar_driver: Computes the effect of microphysics on gridbox covariances
  !-----------------------------------------------------------------


end module silhs_api_module