!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:31 
!KGEN version : 0.8.1 
  
!--------------------------------------------------------------------------------------------------
! $Id$
!==================================================================================================
!       ########  ###       ###    ### #########  #########           ###     ######### ###########
!     ###    ### ###       ###    ### ###    ### ###    ###        ### ###   ###    ###    ###
!    ###        ###       ###    ### ###    ### ###    ###       ###   ###  ###    ###    ###
!   ###        ###       ###    ### #########  #########       ########### #########     ###
!  ###        ###       ###    ### ###    ### ###    ###      ###     ### ###           ###
! ###    ### ###       ###    ### ###    ### ###    ###      ###     ### ###           ###
! ########  ########## ########  #########  #########       ###     ### ###       ###########
! The CLUBB API serves as the doorway through which external models can interact with CLUBB.
!               PLEASE REMEMBER, IF ANY CODE IS CHANGED IN THIS DOCUMENT,
!                   THE CHANGES MUST BE PROPOGATED TO ALL HOST MODELS.


!
!
!
!
module clubb_api_module


    ! Tolerances

    USE corr_varnce_module, ONLY: pdf_dim 


    USE hydromet_pdf_parameter_module, ONLY: hydromet_pdf_parameter 


    USE pdf_parameter_module, ONLY: pdf_parameter 
! The None preprocessor directives are being commented out because this
! code is now also used for WRF-CLUBB.
!#ifdef None 
!#endif


    ! These are used in CAM only


    USE variables_diagnostic_module, ONLY: lscale 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE hydromet_pdf_parameter_module, ONLY: kr_kgen_hydromet_pdf_parameter_module_typesubp0 
    USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
    USE hydromet_pdf_parameter_module, ONLY: kv_kgen_hydromet_pdf_parameter_module_typesubp0 
    USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 

    IMPLICIT NONE 

    PRIVATE 

    ! To Implement CLUBB:
        ! CLUBB can be set more specifically using these flags:
        ! The parameters of CLUBB can be retrieved and tuned using these indices:


    PUBLIC pdf_parameter 
        ! A hydromet array is required, and these variables are required for a hydromet array:

    PUBLIC hydromet_pdf_parameter, pdf_dim 
    ! To Implement None:
    ! generate_silhs_sample - None API
    ! To use the results, you will need these variables:

    ! To Interact With CLUBB's Grid:
    ! For Varying Grids

    ! To Obtain More Output from CLUBB for Diagnostics:


    ! To Convert Between Common CLUBB-related quantities:

    ! To Check For and Handle CLUBB's Errors:

    ! Constants That May be Helpful:
    ! Tolerances

    PUBLIC lscale 
    ! Attempt to Not Use the Following:
! The None preprocessor directives are being commented out because this
! code is now also used for WRF-CLUBB.
!#ifdef None 
!#endif


    PUBLIC kr_kgen_hydromet_pdf_parameter_module_typesubp0 
    PUBLIC kr_pdf_parameter_module_pdf_parameter 
    PUBLIC kv_kgen_hydromet_pdf_parameter_module_typesubp0 
    PUBLIC kv_pdf_parameter_module_pdf_parameter 

  !================================================================================================
  ! advance_clubb_core - Advances the model one timestep.
  !================================================================================================


  !================================================================================================
  ! setup_clubb_core - Sets up the model for execution.
  !================================================================================================


  !================================================================================================
  ! cleanup_clubb_core_api - Frees memory used by the model.
  !================================================================================================


  !================================================================================================
  ! gregorian2julian_day - Computes the number of days since 1 January 4713 BC.
  !================================================================================================


  !================================================================================================
  ! compute_current_date - Computes the current date and the seconds since that date.
  !================================================================================================


  !================================================================================================
  ! leap_year - Determines if the given year is a leap year.
  !================================================================================================


  !================================================================================================
  ! setup_corr_varnce_array - Creates a correlation array with x'^2/xm^2 variables on the diagonal
  !================================================================================================


  !================================================================================================
  ! set_clubb_debug_level - Controls the importance of error messages sent to the console.
  !================================================================================================


  !================================================================================================
  ! clubb_at_least_debug_level - Checks to see if clubb has been set to a specified debug level.
  !================================================================================================


  !================================================================================================
  ! fill_holes_driver - Fills holes between same-phase hydrometeors(i.e. for frozen hydrometeors).
  !================================================================================================


  !================================================================================================
  ! fill_holes_vertical - clips values of 'field' that are below 'threshold' as much as possible.
  !================================================================================================


  !================================================================================================
  ! vertical_integral - Computes the vertical integral.
  !================================================================================================


  !================================================================================================
  ! setup_grid_heights - Sets the heights and interpolation weights of the column.
  !================================================================================================


  !================================================================================================
  ! lin_interpolate_two_points - Computes a linear interpolation of the value of a variable.
  !================================================================================================


  !================================================================================================
  ! lin_interpolate_on_grid - Linear interpolation for 25 June 1996 altocumulus case.
  !================================================================================================


  !================================================================================================
  ! read_parameters - Read a namelist containing the model parameters.
  !================================================================================================


  !================================================================================================
  ! setup_parameters - Sets up model parameters.
  !================================================================================================


  !================================================================================================
  ! adj_low_res_nu - Adjusts values of background eddy diffusivity based on vertical grid spacing.
  !================================================================================================


! The None preprocessor directives are being commented out because this
! code is now also used for WRF-CLUBB.
!#ifdef None 
  !================================================================================================
  ! pack_pdf_params - Returns a two dimensional real array with all values.
  !================================================================================================


  !================================================================================================
  ! unpack_pdf_params - Returns a pdf_parameter array with nz instances of pdf_parameter.
  !================================================================================================


!#endif
  !================================================================================================
  ! setup_pdf_parameters
  !================================================================================================


  !================================================================================================
  ! stats_init - Initializes the statistics saving functionality of the CLUBB model.
  !================================================================================================


  !================================================================================================
  ! stats_begin_timestep - Sets flags determining specific timestep info.
  !================================================================================================


  !================================================================================================
  ! stats_end_timestep - Calls statistics to be written to the output format.
  !================================================================================================


  !================================================================================================
  ! stats_accumulate_hydromet - Computes stats related the hydrometeors.
  !================================================================================================


  !================================================================================================
  ! stats_finalize - Close NetCDF files and deallocate scratch space and stats file structures.
  !================================================================================================


  !================================================================================================
  ! stats_init_rad_zm - Initializes array indices for rad_zm variables.
  !================================================================================================


  !================================================================================================
  ! stats_init_rad_zt - Initializes array indices for zt.
  !================================================================================================


  !================================================================================================
  ! stats_init_zm - Initializes array indices for zm.
  !================================================================================================


  !================================================================================================
  ! stats_init_zt - Initializes array indices for zt.
  !================================================================================================


  !================================================================================================
  ! stats_init_sfc - Initializes array indices for sfc.
  !================================================================================================


  !================================================================================================
  ! thlm2T_in_K - Calculates absolute temperature from liquid water potential temperature.
  !================================================================================================


  !================================================================================================
  ! T_in_K2thlm - Calculates liquid water potential temperature from absolute temperature
  !================================================================================================


  !================================================================================================
  ! calculate_spurious_source - Checks whether there is conservation within the column.
  !================================================================================================


  !================================================================================================
  ! zm2zt_scalar - Interpolates a variable from zm to zt grid at one height level
  !================================================================================================


  !================================================================================================
  ! zt2zm_scalar - Interpolates a variable from zt to zm grid at one height level
  !================================================================================================


  !================================================================================================
  ! zt2zm_prof - Interpolates a variable (profile) from zt to zm grid
  !================================================================================================


  !================================================================================================
  ! zm2zt_prof - Interpolates a variable (profile) from zm to zt grid
  !================================================================================================


  !================================================================================================
  ! calculate_thlp2_rad - Computes the contribution of radiative cooling to thlp2
  !================================================================================================


  !================================================================================================
  ! update_xp2_mc - Calculates the effects of rain evaporation on rtp2 and thlp2
  !================================================================================================


  !================================================================================================
  ! sat_mixrat_liq - computes the saturation mixing ratio of liquid water
  !================================================================================================


  !================================================================================================
  ! subroutine init_pdf_hydromet_arrays_api
  ! DESCRIPTION: 
  !     This subroutine intializes the hydromet arrays(iirr, iiNr, etc.) to the values specified by
  !     the input arguements, this determines which hyrometeors are to be used by the microphysics
  !     scheme. It also sets up the corresponding pdf and hydromet arrays, and calculates the 
  !     subgrid variance ratio for each hydrometeor.
  ! OPTIONAL FUNCTIONALITY:
  !     The subgrid variance ratio for each hydrometeor is calculated based on the grid spacing 
  !     defined by the host model. The calculation is a linear equation defined by a slope and
  !     intercept, each of which may or may not be passed in to this subroutine. If the slope
  !     and/or intercept are not passed in through the arguement list the default values, which 
  !     are set in the corresponding type definitions, will be used. Otherwise the values
  !     specified by the aruements will be used.
  ! NOTES: 
  !     'hmp2_ip_on_hmm2_ip_slope_in' is of type 'hmp2_ip_on_hmm2_ip_slope_type' and
  !     'hmp2_ip_on_hmm2_ip_intrcpt_in' is of type 'hmp2_ip_on_hmm2_ip_intrcpt_in', both of which 
  !     are deinfed in corr_vrnce_module.F90, and made public through this API.
  !     If full control over the hydrometeor variance ratios is desired, pass in slopes that are
  !     initialized to 0.0, this causes the ratios to no longer depend on the grid spacing. Then
  !     pass in the intercepts set to the values of the desired ratios.
  ! ARGUEMENTS:
  !     host_dx (real) - Horizontal grid spacings
  !     host_dy (real)
  !     hydromet_dim (integer) - Number of enabled hydrometeors
  !         Each of these is an index value corresponding to a hydrometeor,
  !         used to index the hydrometeor arrays. Each index has to be unqiue
  !         for each different hyrometeor that is enabled. Setting one of these
  !         indices to -1 disables that hydrometeor
  !     iirr_in (integer) - Index of rain water mixing ratio
  !     iiri_in (integer) - Index of rain drop concentration
  !     iirs_in (integer) - Index of ice mixing ratio
  !     iirg_in (integer) - Index of ice crystal concentration
  !     iiNr_in (integer) - Index of snow mixing ratio
  !     iiNi_in (integer) - Index of snow flake concentration
  !     iiNs_in (integer) - Index of graupel mixing ratio
  !     iiNg_in (integer) - Index of graupel concentration
  !     hmp2_ip_on_hmm2_ip_slope_in (hmp2_ip_on_hmm2_ip_slope_type) - Custom slope values
  !     hmp2_ip_on_hmm2_ip_intrcpt_in (hmp2_ip_on_hmm2_ip_intrcpt_type) - Custom intercept values
  !================================================================================================

    
  ! 
  ! 
  ! 
  ! 
  ! 
  ! 
  ! 
  ! 
  ! 


    
end module clubb_api_module