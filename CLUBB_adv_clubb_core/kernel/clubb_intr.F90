!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:43
!KGEN version : 0.6.1

module clubb_intr

  !----------------------------------------------------------------------------------------------------- !
  ! Module to interface 1 with Cloud Layers Unified by Bi-normals (CLUBB), developed                   !
  !    by the University of Wisconsin Milwaukee Group (UWM).                                             !
  !                                                                                                      !
  ! CLUBB replaces the exisiting turbulence, shallow convection, and macrophysics in CAM5                !  
  !                                                                                                      !  
  ! Lastly, a implicit diffusion solver is called, and tendencies retrieved by                           !
  ! differencing the diffused and initial states.                                                        !
  !                                                                                                      ! 
  ! Calling sequence:                                                                                    !
  !                                                                                                      !
  !---------------------------Code history-------------------------------------------------------------- !
  ! Authors:  P. Bogenschutz, C. Craig, A. Gettelman                                                     ! 
  !                                                                                                      ! 
  !----------------------------------------------------------------------------------------------------- !

    USE shr_kind_mod, ONLY: r8=>shr_kind_r8

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PRIVATE
    SAVE

  ! ----------------- !
  ! Public interfaces !
  ! ----------------- !

    PUBLIC clubb_tend_cam
            ! This utilizes CLUBB specific variables in its interface


  ! Both of these utilize CLUBB specific variables in their interface



  ! ------------ !
  ! Private data !
  ! ------------ !

  integer, parameter :: &
      grid_type    = 3, &               ! The 2 option specifies stretched thermodynamic levels
      hydromet_dim = 0                  ! The hydromet array in SAM-CLUBB is currently 0 elements
   


      
  real(r8), parameter :: &
      host_dx = 100000._r8, &           ! Host model deltax [m]
      host_dy = 100000._r8              ! Host model deltay [m]
      
  integer, parameter :: & 
    sclr_dim = 0                        ! Higher-order scalars, set to zero

    
    
    
    
    

!  Constant parameters
  logical, parameter, private :: &
    l_uv_nudge       = .false.,       &  ! Use u/v nudging (not used)
    l_implemented    = .true.,        &  ! Implemented in a host model (always true)
    l_host_applies_sfc_fluxes = .false.  ! Whether the host model applies the surface fluxes
    
  logical            :: do_expldiff

  integer            :: edsclr_dim       ! Number of scalars to transport in CLUBB
 
!  define physics buffer indicies here       
 


  !  Output arrays for CLUBB statistics    



  PUBLIC kr_externs_in_clubb_intr
  PUBLIC kr_externs_out_clubb_intr
  contains
  
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

!-------------------------------------------------------------------------------
! Description:
!   Register the constituents and fields in the physics buffer
! Author: P. Bogenschutz, C. Craig, A. Gettelman
!
!-------------------------------------------------------------------------------


    !------------------------------------------------ !
    ! Register physics buffer fields and constituents !
    !------------------------------------------------ !

    !  Add CLUBB fields to pbuf 
    

       !  If CLUBB moments are advected, do not output them automatically which is typically done.  Some moments
       !    need a constant added to them before they are advected, thus this would corrupt the output.  
       !    Users should refer to the "XXXX_CLUBB" (THLP2_CLUBB for instance) output variables for these moments

    !  put pbuf_add calls here (see macrop_driver.F90 for sample) use indicies defined at top
    




  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !


  !----------------------------------------------------------------------------- !
  !                                                                              !
  ! Return true if specified constituent is implemented by this package          !
  !                                                                              !
  !----------------------------------------------------------------------------- !


   !-----------------------------------------------------------------------



 
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !





   !----------------------------------------------------------------------- !
   !                                                                        !
   ! Initialize the state if clubb_do_adv                                   !
   !                                                                        !
   !----------------------------------------------------------------------- !

   !-----------------------------------------------------------------------














  
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !










    !----- Begin Code -----

    !  Determine if we want clubb_history to be output  
    

    !  Read namelist to determine if CLUBB history should be called







! Broadcast namelist variables


    !  Overwrite defaults if they are true





    



      


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

!-------------------------------------------------------------------------------
! Description:
!   Initialize UWM CLUBB.
! Author: Cheryl Craig March 2011
! Modifications: Pete Bogenschutz 2011 March and onward
! Origin: Based heavily on UWM clubb_init.F90
! References:
!   None
!-------------------------------------------------------------------------------





    !  From 1 libraries

    !  From the CLUBB libraries


    !  These are only needed if we're using a passive scalar



    !  Input Variables



    


    ! The similar name to clubb_history is unfortunate...





    !----- Begin Code -----

    ! ----------------------------------------------------------------- !
    ! Determine how many constituents CLUBB will transport.  Note that  
    ! CLUBB does not transport aerosol consituents.  Therefore, need to 
    ! determine how many aerosols constituents there are and subtract that
    ! off of pcnst (the total consituents) 
    ! ----------------------------------------------------------------- !


    !  Select variables to apply tendencies back to 1
 
    ! Initialize all consituents to true to start

       ! Turn off modal aerosols and decrement edsclr_dim accordingly
 
 

 
       !  In addition, if running with MAM, droplet number is transported
       !  in dropmixnuc, therefore we do NOT want CLUBB to apply transport
       !  tendencies to avoid double counted.  Else, we apply tendencies.

    ! ----------------------------------------------------------------- !
    ! Set the debug level.  Level 2 has additional computational expense since
    ! it checks the array variables in CLUBB for invalid values.
    ! ----------------------------------------------------------------- !

    ! ----------------------------------------------------------------- !
    ! use pbuf_get_fld_idx to get existing physics buffer fields from other
    ! physics packages (e.g. tke)
    ! ----------------------------------------------------------------- !


    !  Defaults

    !  Overwrite defaults if needbe     


    !  Define physics buffers indexes


    
    ! ----------------------------------------------------------------- !
    ! Define number of tracers for CLUBB to diffuse
    ! ----------------------------------------------------------------- !    
    

    
    ! ----------------------------------------------------------------- !
    ! Setup CLUBB core
    ! ----------------------------------------------------------------- !
    
    !  Read in parameters for CLUBB.  Just read in default values 
!$OMP PARALLEL
!$OMP END PARALLEL
      
    !  Fill in dummy arrays for height.  Note that these are overwrote
    !  at every CLUBB step to physical values.    

   
    !  Set up CLUBB core.  Note that some of these inputs are overwrote
    !  when clubb_tend_cam is called.  The reason is that heights can change
    !  at each time step, which is why dummy arrays are read in here for heights
    !  as they are immediately overwrote.     
!$OMP PARALLEL
!$OMP END PARALLEL

    ! ----------------------------------------------------------------- !
    ! Set-up HB diffusion.  Only initialized to diagnose PBL depth      !
    ! ----------------------------------------------------------------- !

    ! Initialize eddy diffusivity module
    
    
    
    ! ----------------------------------------------------------------- !
    ! Initialize turbulent mountain stress module                       !
    ! ------------------------------------------------------------------!





    ! ----------------------------------------------------------------- !
    ! Add output fields for the history files
    ! ----------------------------------------------------------------- !



    !  These are default CLUBB output.  Not the higher order history budgets



    !  Initialize statistics, below are dummy variables

      



  
    ! ----------------------------------------------------------------- !
    ! Make all of this output default, this is not CLUBB history
    ! ----------------------------------------------------------------- !








    

     

    ! --------------- !
    ! First step?     !
    ! Initialization  !
    ! --------------- !

    !  Is this the first time step?  If so then initialize CLUBB variables as follows

      

   
    ! --------------- !
    ! End             !
    ! Initialization  !
    ! --------------- !


    
    
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

  SUBROUTINE clubb_tend_cam(kgen_unit, kgen_total_time)

!-------------------------------------------------------------------------------
! Description: Provide tendencies of shallow convection, turbulence, and 
!              macrophysics from CLUBB to 1
!   
! Author: Cheryl Craig, March 2011
! Modifications: Pete Bogenschutz, March 2011 and onward
! Origin: Based heavily on UWM clubb_init.F90
! References:
!   None
!-------------------------------------------------------------------------------



      USE ppgrid, ONLY: pverp
      

      USE advance_clubb_core_module, ONLY: advance_clubb_core
      USE pdf_parameter_module, ONLY: pdf_parameter
    


      USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter
      USE grid_class, ONLY: kr_externs_out_grid_class
      USE stats_variables, ONLY: kr_externs_out_stats_variables
      USE error_code, ONLY: kr_externs_out_error_code
      USE variables_diagnostic_module, ONLY: kr_externs_out_variables_diagnostic_module
      USE saturation, ONLY: kr_externs_out_saturation
      USE sponge_layer_damping, ONLY: kr_externs_out_sponge_layer_damping
      USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter
      USE stats_variables, ONLY: kv_externs_stats_variables
      USE variables_diagnostic_module, ONLY: kv_externs_variables_diagnostic_module
      USE kgen_utils_mod, ONLY: kgen_perturb_real
      IMPLICIT NONE
   
   ! --------------- !
   ! Input Auguments !
   ! --------------- !

    
   ! ---------------------- !
   ! Input-Output Auguments !
   ! ---------------------- !
    

   ! ---------------------- !
   ! Output Auguments !
   ! ---------------------- !


   ! These two variables are needed for energy check    

        
   ! --------------- !
   ! Local Variables !
   ! --------------- !



   
      INTEGER :: err_code
   

      REAL(KIND=r8) :: dtime
      REAL(KIND=r8) :: edsclr_in(pverp,edsclr_dim)
      REAL(KIND=r8) :: wp2_in(pverp)
      REAL(KIND=r8) :: wp3_in(pverp)
      REAL(KIND=r8) :: wpthlp_in(pverp)
      REAL(KIND=r8) :: wprtp_in(pverp)
      REAL(KIND=r8) :: rtpthlp_in(pverp)
      REAL(KIND=r8) :: rtp2_in(pverp)
      REAL(KIND=r8) :: thlp2_in(pverp)
      REAL(KIND=r8) :: up2_in(pverp)
      REAL(KIND=r8) :: vp2_in(pverp)
      REAL(KIND=r8) :: upwp_in(pverp)
      REAL(KIND=r8) :: vpwp_in(pverp)
      REAL(KIND=r8) :: thlm_in(pverp)
      REAL(KIND=r8) :: rtm_in(pverp)
      REAL(KIND=r8) :: um_in(pverp)
      REAL(KIND=r8) :: vm_in(pverp)
      REAL(KIND=r8) :: rho_in(pverp)
      REAL(KIND=r8) :: rcm_out(pverp)
      REAL(KIND=r8) :: wprcp_out(pverp)
      REAL(KIND=r8) :: cloud_frac_out(pverp)
      REAL(KIND=r8) :: rcm_in_layer_out(pverp)
      REAL(KIND=r8) :: cloud_cover_out(pverp)
      REAL(KIND=r8) :: thlprcp_out(pverp)
      REAL(KIND=r8) :: rho_ds_zm(pverp)
      REAL(KIND=r8) :: rho_ds_zt(pverp)
      REAL(KIND=r8) :: invrs_rho_ds_zm(pverp)
      REAL(KIND=r8) :: invrs_rho_ds_zt(pverp)
      REAL(KIND=r8) :: thv_ds_zm(pverp)
      REAL(KIND=r8) :: thv_ds_zt(pverp)
      REAL(KIND=r8) :: rfrzm(pverp)
      REAL(KIND=r8) :: radf(pverp)
      REAL(KIND=r8) :: wprtp_forcing(pverp)
      REAL(KIND=r8) :: wpthlp_forcing(pverp)
      REAL(KIND=r8) :: rtp2_forcing(pverp)
      REAL(KIND=r8) :: thlp2_forcing(pverp)
      REAL(KIND=r8) :: rtpthlp_forcing(pverp)
      REAL(KIND=r8) :: ice_supersat_frac(pverp)
      REAL(KIND=r8) :: fcor
      REAL(KIND=r8) :: sfc_elevation
      REAL(KIND=r8) :: thlm_forcing(pverp)
      REAL(KIND=r8) :: rtm_forcing(pverp)
      REAL(KIND=r8) :: um_forcing(pverp)
      REAL(KIND=r8) :: vm_forcing(pverp)
      REAL(KIND=r8) :: wm_zm(pverp)
      REAL(KIND=r8) :: wm_zt(pverp)
      REAL(KIND=r8) :: p_in_pa(pverp)
      REAL(KIND=r8) :: rho_zm(pverp)
      REAL(KIND=r8) :: exner(pverp)
      REAL(KIND=r8) :: wpthlp_sfc
      REAL(KIND=r8) :: wprtp_sfc
      REAL(KIND=r8) :: upwp_sfc
      REAL(KIND=r8) :: vpwp_sfc
      REAL(KIND=r8) :: sclrm_forcing(pverp,sclr_dim)
      REAL(KIND=r8) :: wpsclrp_sfc(sclr_dim)
      REAL(KIND=r8) :: edsclrm_forcing(pverp,edsclr_dim)
      REAL(KIND=r8) :: wpedsclrp_sfc(edsclr_dim)
      REAL(KIND=r8) :: sclrm(pverp,sclr_dim)
      REAL(KIND=r8) :: wpsclrp(pverp,sclr_dim)
      REAL(KIND=r8) :: sclrp2(pverp,sclr_dim)
      REAL(KIND=r8) :: sclrprtp(pverp,sclr_dim)
      REAL(KIND=r8) :: sclrpthlp(pverp,sclr_dim)
      REAL(KIND=r8) :: hydromet(pverp,hydromet_dim)
      REAL(KIND=r8) :: wphydrometp(pverp,hydromet_dim)
      REAL(KIND=r8) :: wp2hmp(pverp,hydromet_dim)
      REAL(KIND=r8) :: rtphmp_zt(pverp,hydromet_dim)
      REAL(KIND=r8) :: thlphmp_zt (pverp,hydromet_dim)
      REAL(KIND=r8) :: khzm_out(pverp)
      REAL(KIND=r8) :: khzt_out(pverp)
      REAL(KIND=r8) :: qclvar_out(pverp)
   
   ! Variables below are needed to compute energy integrals for conservation


   
      TYPE(pdf_parameter), dimension(pverp) :: pdf_params


   ! --------------- !
   ! Pointers        !
   ! --------------- !




   



      INTEGER, INTENT(IN) :: kgen_unit
      REAL(KIND=kgen_dp), INTENT(INOUT) :: kgen_total_time
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      INTEGER :: kgenref_err_code
      REAL(KIND=r8) :: kgenref_edsclr_in(pverp,edsclr_dim)
      REAL(KIND=r8) :: kgenref_wp2_in(pverp)
      REAL(KIND=r8) :: kgenref_wp3_in(pverp)
      REAL(KIND=r8) :: kgenref_wpthlp_in(pverp)
      REAL(KIND=r8) :: kgenref_wprtp_in(pverp)
      REAL(KIND=r8) :: kgenref_rtpthlp_in(pverp)
      REAL(KIND=r8) :: kgenref_rtp2_in(pverp)
      REAL(KIND=r8) :: kgenref_thlp2_in(pverp)
      REAL(KIND=r8) :: kgenref_up2_in(pverp)
      REAL(KIND=r8) :: kgenref_vp2_in(pverp)
      REAL(KIND=r8) :: kgenref_upwp_in(pverp)
      REAL(KIND=r8) :: kgenref_vpwp_in(pverp)
      REAL(KIND=r8) :: kgenref_thlm_in(pverp)
      REAL(KIND=r8) :: kgenref_rtm_in(pverp)
      REAL(KIND=r8) :: kgenref_um_in(pverp)
      REAL(KIND=r8) :: kgenref_vm_in(pverp)
      REAL(KIND=r8) :: kgenref_rcm_out(pverp)
      REAL(KIND=r8) :: kgenref_wprcp_out(pverp)
      REAL(KIND=r8) :: kgenref_cloud_frac_out(pverp)
      REAL(KIND=r8) :: kgenref_rcm_in_layer_out(pverp)
      REAL(KIND=r8) :: kgenref_cloud_cover_out(pverp)
      REAL(KIND=r8) :: kgenref_thlprcp_out(pverp)
      REAL(KIND=r8) :: kgenref_ice_supersat_frac(pverp)
      REAL(KIND=r8) :: kgenref_sclrm(pverp,sclr_dim)
      REAL(KIND=r8) :: kgenref_wpsclrp(pverp,sclr_dim)
      REAL(KIND=r8) :: kgenref_sclrp2(pverp,sclr_dim)
      REAL(KIND=r8) :: kgenref_sclrprtp(pverp,sclr_dim)
      REAL(KIND=r8) :: kgenref_sclrpthlp(pverp,sclr_dim)
      REAL(KIND=r8) :: kgenref_khzm_out(pverp)
      REAL(KIND=r8) :: kgenref_khzt_out(pverp)
      REAL(KIND=r8) :: kgenref_qclvar_out(pverp)
      TYPE(pdf_parameter), dimension(pverp) :: kgenref_pdf_params
      TYPE(check_t) :: check_status
      INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock
      INTEGER, PARAMETER :: kgen_maxiter = 1
      REAL(KIND=kgen_dp) :: kgen_elapsed_time


   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!
   !       MAIN COMPUTATION BEGINS HERE                                                            !
   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!
   !-----------------------------------------------------------------------------------------------!




 !  Get indicees for cloud and ice mass and cloud and ice number


 !  Initialize physics tendency arrays, copy the state to state1 array to use in this routine






   !  Determine number of columns and which chunk computation is to be performed on


   !  Determine time step of physics buffer
   

   !  Establish associations between pointers and physics buffer fields   


   



   ! Intialize the apply_const variable (note special logic is due to eularian backstepping)
                           !  from moments since it has not been added yet 


     ! -------------------------------------- !
     ! Ice Saturation Adjustment Computation  !
     ! -------------------------------------- !

   
   
   


     ! update local copy of state with the tendencies

    ! Add the ice tendency to the output tendency

    ! ptend_loc is reset to zero by this call

    !Write output for tendencies:
    !        oufld: QVTENDICE,QITENDICE,NITENDICE
   

   !  Determine CLUBB time step and make it sub-step friendly
   !  For now we want CLUBB time step to be 5 min since that is 
   !  what has been scientifically validated.  However, there are certain
   !  instances when a 5 min time step will not be possible (based on 
   !  host model time step or on macro-micro sub-stepping
   
   
   !  Now check to see if dtime is greater than the host model 
   !    (or sub stepped) time step.  If it is, then simply 
   !    set it equal to the host (or sub step) time step.  
   !    This section is mostly to deal with small host model
   !    time steps (or small sub-steps)
   

   
   !  Now check to see if CLUBB time step divides evenly into
   !    the host model time step.  If not, force it to divide evenly.
   !    We also want it to be 5 minutes or less.  This section is
   !    mainly for host model time steps that are not evenly divisible
   !    by 5 minutes  
   


   !  If resulting host model time step and CLUBB time step do not divide evenly
   !    into each other, have model throw a fit.  


   
   !  determine number of timesteps CLUBB core should be advanced, 
   !  host time step divided by CLUBB time step  
  
   !  Initialize forcings for transported scalars to zero
   
   

   !  Compute exner function consistent with CLUBB's definition, which uses a constant
   !  surface pressure.  1's exner (in state does not).  Therefore, for consistent 
   !  treatment with CLUBB code, anytime exner is needed to treat CLUBB variables 
   !  (such as thlm), use "exner_clubb" other wise use the exner in state


   
   !  At each CLUBB call, initialize mean momentum  and thermo CLUBB state 
   !  from the 1 state 




            !  Note that some of the moments below can be positive or negative.  
            !    Remove a constant that was added to prevent dynamics from clipping 
            !    them to prevent dynamics from making them positive.  

   
     ! If not last step of macmic loop then set apply_const back to 
     !   zero to prevent output from being corrupted.  


  


   ! Compute integrals of static energy, kinetic energy, water vapor, and liquid water
   ! for the computation of total energy before CLUBB is called.  This is for an 
   ! effort to conserve energy since liquid water potential temperature (which CLUBB 
   ! conserves) and static energy (which 1 conserves) are not exactly equal.   


   !  Compute virtual potential temperature, which is needed for CLUBB  

   
   ! ------------------------------------------------- !
   ! Begin module to compute turbulent mountain stress !
   ! ------------------------------------------------- !
   
   


   ! ------------------------------------------------- !
   ! End module to compute turbulent mountain stress   !
   ! ------------------------------------------------- !

   !  Loop over all columns in lchnk to advance CLUBB core

      !  Set time_elapsed to host model time step, this is for 
      !  CLUBB's budget stats

      !  Determine Coriolis force at given latitude.  This is never used
      !  when CLUBB is implemented in a host model, therefore just set
      !  to zero.

      !  Define the CLUBB momentum grid (in height, units of m)


      !  Define the CLUBB thermodynamic grid (in units of m)

 
      !  Thermodynamic ghost point is below surface 

      !  Set the elevation of the surface

      !  Compute thermodynamic stuff needed for CLUBB on thermo levels.  
      !  Inputs for the momentum levels are set below setup_clubb core


      !  Below computes the same stuff for the ghost point.  May or may
      !  not be needed, just to be safe to avoid NaN's

      !  Compute mean w wind on thermo grid, convert from omega to w 

    
      ! ------------------------------------------------- !
      ! Begin case specific code for SCAM cases.          !
      ! This section of code block NOT called in          !
      ! global simulations                                !
      ! ------------------------------------------------- !


        !  Initialize zo if variable ustar is used



        !  Compute surface wind (ubar)

    
        !  Below denotes case specifics for surface momentum
        !  and thermodynamic fluxes, depending on the case

        !  Define ustar (based on case, if not variable)     
    

    

    


       
    
        !  Compute the surface momentum fluxes, if this is a SCAM simulation       
    

      !  Define surface sources for transported variables for diffusion, will 
      !  be zero as these tendencies are done in clubb_surface


      !  Define forcings from 1 to CLUBB as zero for momentum and thermo,
      !  forcings already applied through 1
 
 
 
      !  Set stats output and increment equal to CLUBB and host dt
 
      !  Heights need to be set at each timestep.  Therefore, recall 
      !  setup_grid and setup_parameters for this.  
     
      !  Read in parameters for CLUBB.  Just read in default values 
 
      !  Set-up CLUBB core at each CLUBB call because heights can change 
 
 
      !  Compute some inputs from the thermodynamic grid
      !  to the momentum grid
      
      !  Surface fluxes provided by host model
      
      ! ------------------------------------------------- !
      ! Apply TMS                                         !
      ! ------------------------------------------------- !    
      
  
      !  Need to flip arrays around for CLUBB core
 


         !  Initialize these to prevent crashing behavior

         !  higher order scalar stuff, put to zero
 
     
     
 

  
      !  Do the same for tracers 

      

        

     
      ! --------------------------------------------------------- !
      ! Compute cloud-top radiative cooling contribution to CLUBB !
      ! --------------------------------------------------------- !       
 
      ! Sandbox version of code to take into account meso organization
     
      

      

       
         ! Now compute new entrainment rate based on organization
     

      ! --------------------------------------------------------- !
      ! End cloud-top radiative cooling contribution to CLUBB     !
      ! --------------------------------------------------------- !  

    
         !  Increment the statistics then being stats timestep


         !  Advance CLUBB CORE one timestep in the future
      
      !local input variables
      READ (UNIT = kgen_unit) err_code
      READ (UNIT = kgen_unit) dtime
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) edsclr_in
          CALL kgen_array_sumcheck("edsclr_in", kgen_array_sum, REAL(SUM(edsclr_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wp2_in
          CALL kgen_array_sumcheck("wp2_in", kgen_array_sum, REAL(SUM(wp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wp3_in
          CALL kgen_array_sumcheck("wp3_in", kgen_array_sum, REAL(SUM(wp3_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wpthlp_in
          CALL kgen_array_sumcheck("wpthlp_in", kgen_array_sum, REAL(SUM(wpthlp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wprtp_in
          CALL kgen_array_sumcheck("wprtp_in", kgen_array_sum, REAL(SUM(wprtp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rtpthlp_in
          CALL kgen_array_sumcheck("rtpthlp_in", kgen_array_sum, REAL(SUM(rtpthlp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rtp2_in
          CALL kgen_array_sumcheck("rtp2_in", kgen_array_sum, REAL(SUM(rtp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thlp2_in
          CALL kgen_array_sumcheck("thlp2_in", kgen_array_sum, REAL(SUM(thlp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) up2_in
          CALL kgen_array_sumcheck("up2_in", kgen_array_sum, REAL(SUM(up2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) vp2_in
          CALL kgen_array_sumcheck("vp2_in", kgen_array_sum, REAL(SUM(vp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) upwp_in
          CALL kgen_array_sumcheck("upwp_in", kgen_array_sum, REAL(SUM(upwp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) vpwp_in
          CALL kgen_array_sumcheck("vpwp_in", kgen_array_sum, REAL(SUM(vpwp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thlm_in
          CALL kgen_array_sumcheck("thlm_in", kgen_array_sum, REAL(SUM(thlm_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rtm_in
          CALL kgen_array_sumcheck("rtm_in", kgen_array_sum, REAL(SUM(rtm_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) um_in
          CALL kgen_array_sumcheck("um_in", kgen_array_sum, REAL(SUM(um_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) vm_in
          CALL kgen_array_sumcheck("vm_in", kgen_array_sum, REAL(SUM(vm_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rho_in
          CALL kgen_array_sumcheck("rho_in", kgen_array_sum, REAL(SUM(rho_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rcm_out
          CALL kgen_array_sumcheck("rcm_out", kgen_array_sum, REAL(SUM(rcm_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wprcp_out
          CALL kgen_array_sumcheck("wprcp_out", kgen_array_sum, REAL(SUM(wprcp_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) cloud_frac_out
          CALL kgen_array_sumcheck("cloud_frac_out", kgen_array_sum, REAL(SUM(cloud_frac_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rcm_in_layer_out
          CALL kgen_array_sumcheck("rcm_in_layer_out", kgen_array_sum, REAL(SUM(rcm_in_layer_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) cloud_cover_out
          CALL kgen_array_sumcheck("cloud_cover_out", kgen_array_sum, REAL(SUM(cloud_cover_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thlprcp_out
          CALL kgen_array_sumcheck("thlprcp_out", kgen_array_sum, REAL(SUM(thlprcp_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rho_ds_zm
          CALL kgen_array_sumcheck("rho_ds_zm", kgen_array_sum, REAL(SUM(rho_ds_zm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rho_ds_zt
          CALL kgen_array_sumcheck("rho_ds_zt", kgen_array_sum, REAL(SUM(rho_ds_zt), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) invrs_rho_ds_zm
          CALL kgen_array_sumcheck("invrs_rho_ds_zm", kgen_array_sum, REAL(SUM(invrs_rho_ds_zm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) invrs_rho_ds_zt
          CALL kgen_array_sumcheck("invrs_rho_ds_zt", kgen_array_sum, REAL(SUM(invrs_rho_ds_zt), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thv_ds_zm
          CALL kgen_array_sumcheck("thv_ds_zm", kgen_array_sum, REAL(SUM(thv_ds_zm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thv_ds_zt
          CALL kgen_array_sumcheck("thv_ds_zt", kgen_array_sum, REAL(SUM(thv_ds_zt), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rfrzm
          CALL kgen_array_sumcheck("rfrzm", kgen_array_sum, REAL(SUM(rfrzm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) radf
          CALL kgen_array_sumcheck("radf", kgen_array_sum, REAL(SUM(radf), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wprtp_forcing
          CALL kgen_array_sumcheck("wprtp_forcing", kgen_array_sum, REAL(SUM(wprtp_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wpthlp_forcing
          CALL kgen_array_sumcheck("wpthlp_forcing", kgen_array_sum, REAL(SUM(wpthlp_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rtp2_forcing
          CALL kgen_array_sumcheck("rtp2_forcing", kgen_array_sum, REAL(SUM(rtp2_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thlp2_forcing
          CALL kgen_array_sumcheck("thlp2_forcing", kgen_array_sum, REAL(SUM(thlp2_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rtpthlp_forcing
          CALL kgen_array_sumcheck("rtpthlp_forcing", kgen_array_sum, REAL(SUM(rtpthlp_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) ice_supersat_frac
          CALL kgen_array_sumcheck("ice_supersat_frac", kgen_array_sum, REAL(SUM(ice_supersat_frac), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) fcor
      READ (UNIT = kgen_unit) sfc_elevation
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) thlm_forcing
          CALL kgen_array_sumcheck("thlm_forcing", kgen_array_sum, REAL(SUM(thlm_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rtm_forcing
          CALL kgen_array_sumcheck("rtm_forcing", kgen_array_sum, REAL(SUM(rtm_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) um_forcing
          CALL kgen_array_sumcheck("um_forcing", kgen_array_sum, REAL(SUM(um_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) vm_forcing
          CALL kgen_array_sumcheck("vm_forcing", kgen_array_sum, REAL(SUM(vm_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wm_zm
          CALL kgen_array_sumcheck("wm_zm", kgen_array_sum, REAL(SUM(wm_zm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wm_zt
          CALL kgen_array_sumcheck("wm_zt", kgen_array_sum, REAL(SUM(wm_zt), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) p_in_pa
          CALL kgen_array_sumcheck("p_in_pa", kgen_array_sum, REAL(SUM(p_in_pa), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) rho_zm
          CALL kgen_array_sumcheck("rho_zm", kgen_array_sum, REAL(SUM(rho_zm), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) exner
          CALL kgen_array_sumcheck("exner", kgen_array_sum, REAL(SUM(exner), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) wpthlp_sfc
      READ (UNIT = kgen_unit) wprtp_sfc
      READ (UNIT = kgen_unit) upwp_sfc
      READ (UNIT = kgen_unit) vpwp_sfc
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) edsclrm_forcing
          CALL kgen_array_sumcheck("edsclrm_forcing", kgen_array_sum, REAL(SUM(edsclrm_forcing), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) wpedsclrp_sfc
          CALL kgen_array_sumcheck("wpedsclrp_sfc", kgen_array_sum, REAL(SUM(wpedsclrp_sfc), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) khzm_out
          CALL kgen_array_sumcheck("khzm_out", kgen_array_sum, REAL(SUM(khzm_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) khzt_out
          CALL kgen_array_sumcheck("khzt_out", kgen_array_sum, REAL(SUM(khzt_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) qclvar_out
          CALL kgen_array_sumcheck("qclvar_out", kgen_array_sum, REAL(SUM(qclvar_out), 8), .TRUE.)
      END IF 
      CALL kr_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1(pdf_params, kgen_unit)
      
      !extern output variables
      CALL kr_externs_out_clubb_intr(kgen_unit)
      CALL kr_externs_out_grid_class(kgen_unit)
      CALL kr_externs_out_stats_variables(kgen_unit)
      CALL kr_externs_out_error_code(kgen_unit)
      CALL kr_externs_out_variables_diagnostic_module(kgen_unit)
      CALL kr_externs_out_saturation(kgen_unit)
      CALL kr_externs_out_sponge_layer_damping(kgen_unit)
      
      !local output variables
      READ (UNIT = kgen_unit) kgenref_err_code
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_edsclr_in
          CALL kgen_array_sumcheck("kgenref_edsclr_in", kgen_array_sum, REAL(SUM(kgenref_edsclr_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_wp2_in
          CALL kgen_array_sumcheck("kgenref_wp2_in", kgen_array_sum, REAL(SUM(kgenref_wp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_wp3_in
          CALL kgen_array_sumcheck("kgenref_wp3_in", kgen_array_sum, REAL(SUM(kgenref_wp3_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_wpthlp_in
          CALL kgen_array_sumcheck("kgenref_wpthlp_in", kgen_array_sum, REAL(SUM(kgenref_wpthlp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_wprtp_in
          CALL kgen_array_sumcheck("kgenref_wprtp_in", kgen_array_sum, REAL(SUM(kgenref_wprtp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_rtpthlp_in
          CALL kgen_array_sumcheck("kgenref_rtpthlp_in", kgen_array_sum, REAL(SUM(kgenref_rtpthlp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_rtp2_in
          CALL kgen_array_sumcheck("kgenref_rtp2_in", kgen_array_sum, REAL(SUM(kgenref_rtp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_thlp2_in
          CALL kgen_array_sumcheck("kgenref_thlp2_in", kgen_array_sum, REAL(SUM(kgenref_thlp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_up2_in
          CALL kgen_array_sumcheck("kgenref_up2_in", kgen_array_sum, REAL(SUM(kgenref_up2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_vp2_in
          CALL kgen_array_sumcheck("kgenref_vp2_in", kgen_array_sum, REAL(SUM(kgenref_vp2_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_upwp_in
          CALL kgen_array_sumcheck("kgenref_upwp_in", kgen_array_sum, REAL(SUM(kgenref_upwp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_vpwp_in
          CALL kgen_array_sumcheck("kgenref_vpwp_in", kgen_array_sum, REAL(SUM(kgenref_vpwp_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_thlm_in
          CALL kgen_array_sumcheck("kgenref_thlm_in", kgen_array_sum, REAL(SUM(kgenref_thlm_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_rtm_in
          CALL kgen_array_sumcheck("kgenref_rtm_in", kgen_array_sum, REAL(SUM(kgenref_rtm_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_um_in
          CALL kgen_array_sumcheck("kgenref_um_in", kgen_array_sum, REAL(SUM(kgenref_um_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_vm_in
          CALL kgen_array_sumcheck("kgenref_vm_in", kgen_array_sum, REAL(SUM(kgenref_vm_in), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_rcm_out
          CALL kgen_array_sumcheck("kgenref_rcm_out", kgen_array_sum, REAL(SUM(kgenref_rcm_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_wprcp_out
          CALL kgen_array_sumcheck("kgenref_wprcp_out", kgen_array_sum, REAL(SUM(kgenref_wprcp_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_cloud_frac_out
          CALL kgen_array_sumcheck("kgenref_cloud_frac_out", kgen_array_sum, REAL(SUM(kgenref_cloud_frac_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_rcm_in_layer_out
          CALL kgen_array_sumcheck("kgenref_rcm_in_layer_out", kgen_array_sum, REAL(SUM(kgenref_rcm_in_layer_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_cloud_cover_out
          CALL kgen_array_sumcheck("kgenref_cloud_cover_out", kgen_array_sum, REAL(SUM(kgenref_cloud_cover_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_thlprcp_out
          CALL kgen_array_sumcheck("kgenref_thlprcp_out", kgen_array_sum, REAL(SUM(kgenref_thlprcp_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_ice_supersat_frac
          CALL kgen_array_sumcheck("kgenref_ice_supersat_frac", kgen_array_sum, REAL(SUM(kgenref_ice_supersat_frac), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_khzm_out
          CALL kgen_array_sumcheck("kgenref_khzm_out", kgen_array_sum, REAL(SUM(kgenref_khzm_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_khzt_out
          CALL kgen_array_sumcheck("kgenref_khzt_out", kgen_array_sum, REAL(SUM(kgenref_khzt_out), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) kgenref_qclvar_out
          CALL kgen_array_sumcheck("kgenref_qclvar_out", kgen_array_sum, REAL(SUM(kgenref_qclvar_out), 8), .TRUE.)
      END IF 
      CALL kr_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1(kgenref_pdf_params, kgen_unit)
      
      !Uncomment following call statement to turn on perturbation experiment.
      !Adjust perturbation value and/or kind parameter if required.
      !CALL kgen_perturb_real( your_variable, 1.0E-15_8 )
      
      
      !call to kgen kernel
         call advance_clubb_core &
            ( l_implemented, dtime, fcor, sfc_elevation, hydromet_dim, &
            thlm_forcing, rtm_forcing, um_forcing, vm_forcing, &
            sclrm_forcing, edsclrm_forcing, wprtp_forcing, &  
            wpthlp_forcing, rtp2_forcing, thlp2_forcing, &
            rtpthlp_forcing, wm_zm, wm_zt, &      
            wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, &
            wpsclrp_sfc, wpedsclrp_sfc, &       
            p_in_Pa, rho_zm, rho_in, exner, &
            rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, &
            invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, &
            rfrzm, radf, do_expldiff, &



            wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, &
            host_dx, host_dy, &
            um_in, vm_in, upwp_in, &
            vpwp_in, up2_in, vp2_in, &
            thlm_in, rtm_in, wprtp_in, wpthlp_in, &
            wp2_in, wp3_in, rtp2_in, &
            thlp2_in, rtpthlp_in, &
            sclrm, sclrp2, sclrprtp, sclrpthlp, &        
            wpsclrp, edsclr_in, err_code, &
            rcm_out, wprcp_out, cloud_frac_out, ice_supersat_frac, &
            rcm_in_layer_out, cloud_cover_out, &
            khzm_out, khzt_out, qclvar_out, thlprcp_out, &
            pdf_params)




            ! update turbulent moments based on rain evaporation  

!                     rtpthlp_in = rtpthlp_in + rtpthlp_mc_out * dtime

         

          !  Check to see if stats should be output, here stats are read into
          !  output arrays to make them conformable to 1 output


     


   
 
      !  Arrays need to be "flipped" to 1 grid 
     
     


     
      !  Fill up arrays needed for McICA.  Note we do not want the ghost point,
      !   thus why the second loop is needed.
     
     
      ! Compute integrals for static energy, kinetic energy, water vapor, and liquid water
      ! after CLUBB is called.  This is for energy conservation purposes.

     
      ! Based on these integrals, compute the total energy before and after CLUBB call

     
      ! Take into account the surface fluxes of heat and moisture
     
      ! Compute the disbalance of total energy
     
      ! Fix the total energy coming out of CLUBB so it achieves enery conservation.
      ! Apply this fixer throughout the column evenly.


      !  Now compute the tendencies of CLUBB to 1, note that pverp is the ghost point
      !  for all variables and therefore is never called in this loop
  


               ! Here add a constant to moments which can be either positive or 
               !  negative.  This is to prevent clipping when dynamics tries to
               !  make all constituents positive 



         !  Apply tendencies to ice mixing ratio, liquid and ice number, and aerosol constituents.
         !  Loading up this array doesn't mean the tendencies are applied.  
         ! edsclr_out is compressed with just the constituents being used, ptend and state are not compressed




   
   ! Add constant to ghost point so that output is not corrupted 



   ! ------------------------------------------------- !
   ! End column computation of CLUBB, begin to apply   !
   ! and compute output, etc                           !
   ! ------------------------------------------------- !

   !  Output CLUBB tendencies 




   !  Update physics tendencies     


   ! ------------------------------------------------------------ !
   ! ------------------------------------------------------------ ! 
   ! ------------------------------------------------------------ !
   ! The rest of the code deals with diagnosing variables         !
   ! for microphysics/radiation computation and macrophysics      !
   ! ------------------------------------------------------------ !
   ! ------------------------------------------------------------ !
   ! ------------------------------------------------------------ !

   
   ! --------------------------------------------------------------------------------- !  
   !  COMPUTE THE ICE CLOUD DETRAINMENT                                                !
   !  Detrainment of convective condensate into the environment or stratiform cloud    !
   ! --------------------------------------------------------------------------------- ! 

   !  Initialize the shallow convective detrainment rate, will always be zero

    
   

        
 
         ! Only rliq is saved from deep convection, which is the reserved liquid.  We need to keep
         !   track of the integrals of ice and static energy that is effected from conversion to ice
         !   so that the energy checker doesn't complain.
 
   

  

   ! ------------------------------------------------- !
   ! Diagnose relative cloud water variance            !
   ! ------------------------------------------------- !


   


   
   ! ------------------------------------------------- !
   ! Optional Accretion enhancement factor             !
   ! ------------------------------------------------- !   

   
   ! ------------------------------------------------- !
   ! Diagnose some output variables                    !
   ! ------------------------------------------------- !

   !  density

         !  buoyancy flux

         !  total water mixing ratio
         !  liquid water potential temperature
         !  liquid water static energy
   

   
   ! --------------------------------------------------------------------------------- ! 
   !  Diagnose some quantities that are computed in macrop_tend here.                  !
   !  These are inputs required for the microphysics calculation.                      !
   !                                                                                   !
   !  FIRST PART COMPUTES THE STRATIFORM CLOUD FRACTION FROM CLUBB CLOUD FRACTION      !
   ! --------------------------------------------------------------------------------- ! 
 
   !  initialize variables 
 

 
   ! --------------------------------------------------------------------------------- !  
   !  THIS PART COMPUTES CONVECTIVE AND DEEP CONVECTIVE CLOUD FRACTION                 !
   ! --------------------------------------------------------------------------------- ! 
 
 
         !  diagnose the deep convective cloud fraction, as done in macrophysics based on the 
         !  deep convective mass flux, read in from pbuf.  Since shallow convection is never 
         !  called, the shallow convective mass flux will ALWAYS be zero, ensuring that this cloud
         !  fraction is purely from deep convection scheme.  
       

             
         !  using the deep convective cloud fraction, and CLUBB cloud fraction (variable 
         !  "cloud_frac"), compute the convective cloud fraction.  This follows the formulation
         !  found in macrophysics code.  Assumes that convective cloud is all nonstratiform cloud 
         !  from CLUBB plus the deep convective cloud fraction
   
       
       
   
   ! --------------------------------------------------------------------------------- !  
   !  COMPUTE THE ICE CLOUD FRACTION PORTION                                           !
   !  use the aist_vector function to compute the ice cloud fraction                   !
   ! --------------------------------------------------------------------------------- !  


  
   ! --------------------------------------------------------------------------------- !  
   !  THIS PART COMPUTES THE LIQUID STRATUS FRACTION                                   !
   !                                                                                   !
   !  For now leave the computation of ice stratus fraction from macrop_driver intact  !
   !  because CLUBB does nothing with ice.  Here I simply overwrite the liquid stratus ! 
   !  fraction that was coded in macrop_driver                                         !
   ! --------------------------------------------------------------------------------- !  
 
   !  Recompute net stratus fraction using maximum over-lapping assumption, as done
   !  in macrophysics code, using alst computed above and aist read in from physics buffer
   



   
   !  Probably need to add deepcu cloud fraction to the cloud fraction array, else would just 
   !  be outputting the shallow convective cloud fraction 


   
   ! --------------------------------------------------------------------------------- !  
   !  DIAGNOSE THE PBL DEPTH                                                           !
   !  this is needed for aerosol code                                                  !
   ! --------------------------------------------------------------------------------- !   
    

 
   ! diagnose surface friction and obukhov length (inputs to diagnose PBL depth)

   
   

   !  Compute PBL depth according to Holtslag-Boville Scheme

   !  Output the PBL depth
 
   ! Assign the first pver levels of cloud_frac back to cld

   ! --------------------------------------------------------------------------------- !   
   !  END CLOUD FRACTION DIAGNOSIS, begin to store variables back into buffer          !
   ! --------------------------------------------------------------------------------- !  
 
   !  Output calls of variables goes here

   !  Output CLUBB history here
      
   

   
   
   

      


   

   

   
   

         
         !verify init
         CALL kgen_init_check(check_status, verboseLevel=1)
         
         !extern verify variables
         CALL kv_externs_stats_variables(check_status)
         CALL kv_externs_variables_diagnostic_module(check_status)
         
         !local verify variables
         CALL kv_clubb_tend_cam_integer__("err_code", check_status, err_code, kgenref_err_code)
         CALL kv_clubb_tend_cam_real__r8_dim2("edsclr_in", check_status, edsclr_in, kgenref_edsclr_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("wp2_in", check_status, wp2_in, kgenref_wp2_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("wp3_in", check_status, wp3_in, kgenref_wp3_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("wpthlp_in", check_status, wpthlp_in, kgenref_wpthlp_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("wprtp_in", check_status, wprtp_in, kgenref_wprtp_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("rtpthlp_in", check_status, rtpthlp_in, kgenref_rtpthlp_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("rtp2_in", check_status, rtp2_in, kgenref_rtp2_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("thlp2_in", check_status, thlp2_in, kgenref_thlp2_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("up2_in", check_status, up2_in, kgenref_up2_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("vp2_in", check_status, vp2_in, kgenref_vp2_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("upwp_in", check_status, upwp_in, kgenref_upwp_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("vpwp_in", check_status, vpwp_in, kgenref_vpwp_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("thlm_in", check_status, thlm_in, kgenref_thlm_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("rtm_in", check_status, rtm_in, kgenref_rtm_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("um_in", check_status, um_in, kgenref_um_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("vm_in", check_status, vm_in, kgenref_vm_in)
         CALL kv_clubb_tend_cam_real__r8_dim1("rcm_out", check_status, rcm_out, kgenref_rcm_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("wprcp_out", check_status, wprcp_out, kgenref_wprcp_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("cloud_frac_out", check_status, cloud_frac_out, kgenref_cloud_frac_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("rcm_in_layer_out", check_status, rcm_in_layer_out, kgenref_rcm_in_layer_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("cloud_cover_out", check_status, cloud_cover_out, kgenref_cloud_cover_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("thlprcp_out", check_status, thlprcp_out, kgenref_thlprcp_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("ice_supersat_frac", check_status, ice_supersat_frac, kgenref_ice_supersat_frac)
         CALL kv_clubb_tend_cam_real__r8_dim1("khzm_out", check_status, khzm_out, kgenref_khzm_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("khzt_out", check_status, khzt_out, kgenref_khzt_out)
         CALL kv_clubb_tend_cam_real__r8_dim1("qclvar_out", check_status, qclvar_out, kgenref_qclvar_out)
         CALL kv_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1("pdf_params", check_status, pdf_params, kgenref_pdf_params)
         WRITE (*, *) ""
         IF (check_status%verboseLevel > 0) THEN
             WRITE (*, *) "Number of verified variables: ", check_status%numTotal
             WRITE (*, *) "Number of identical variables: ", check_status%numIdentical
             WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol
             WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol
             WRITE (*, *) "Tolerance: ", check_status%tolerance
         END IF 
         WRITE (*, *) ""
         IF (check_status%numOutTol > 0) THEN
             WRITE (*, *) "Verification FAILED"
             check_status%Passed = .FALSE.
         ELSE
             WRITE (*, *) "Verification PASSED"
             check_status%Passed = .TRUE.
         END IF 
         WRITE (*, *) ""
         
         !Measuring elapsed time. Please increase the value of kgen_maxiter to get improve timing measurment resolution.
         CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock)
         DO kgen_intvar = 1, kgen_maxiter
         call advance_clubb_core &
            ( l_implemented, dtime, fcor, sfc_elevation, hydromet_dim, &
            thlm_forcing, rtm_forcing, um_forcing, vm_forcing, &
            sclrm_forcing, edsclrm_forcing, wprtp_forcing, &  
            wpthlp_forcing, rtp2_forcing, thlp2_forcing, &
            rtpthlp_forcing, wm_zm, wm_zt, &      
            wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, &
            wpsclrp_sfc, wpedsclrp_sfc, &       
            p_in_Pa, rho_zm, rho_in, exner, &
            rho_ds_zm, rho_ds_zt, invrs_rho_ds_zm, &
            invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet, &
            rfrzm, radf, do_expldiff, &



            wphydrometp, wp2hmp, rtphmp_zt, thlphmp_zt, &
            host_dx, host_dy, &
            um_in, vm_in, upwp_in, &
            vpwp_in, up2_in, vp2_in, &
            thlm_in, rtm_in, wprtp_in, wpthlp_in, &
            wp2_in, wp3_in, rtp2_in, &
            thlp2_in, rtpthlp_in, &
            sclrm, sclrp2, sclrprtp, sclrpthlp, &        
            wpsclrp, edsclr_in, err_code, &
            rcm_out, wprcp_out, cloud_frac_out, ice_supersat_frac, &
            rcm_in_layer_out, cloud_cover_out, &
            khzm_out, khzt_out, qclvar_out, thlprcp_out, &
            pdf_params)
         END DO 
         CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock)
         kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)
         WRITE (*, *) "advance_clubb_core : Time per call (usec): ", kgen_elapsed_time
         kgen_total_time = kgen_total_time + kgen_elapsed_time
         
         CONTAINS
         
         !read state subroutine for kr_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1
         SUBROUTINE kr_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1(var, kgen_unit, printvar)
             TYPE(pdf_parameter), INTENT(INOUT), DIMENSION(:) :: var
             INTEGER, INTENT(IN) :: kgen_unit
             CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
             LOGICAL :: kgen_istrue
             REAL(KIND=8) :: kgen_array_sum
             INTEGER :: idx1
             INTEGER, DIMENSION(2,1) :: kgen_bound
             
             READ (UNIT = kgen_unit) kgen_istrue
             IF (kgen_istrue) THEN
                 READ (UNIT = kgen_unit) kgen_bound(1, 1)
                 READ (UNIT = kgen_unit) kgen_bound(2, 1)
                 DO idx1=kgen_bound(1,1), kgen_bound(2,1)
                     IF (PRESENT( printvar )) THEN
                         CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printvar // "(idx1)")
                     ELSE
                         CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit)
                     END IF 
                 END DO 
             END IF 
         END SUBROUTINE kr_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1
         
         !verify state subroutine for kv_clubb_tend_cam_integer__
         RECURSIVE SUBROUTINE kv_clubb_tend_cam_integer__(varname, check_status, var, kgenref_var)
             CHARACTER(LEN=*), INTENT(IN) :: varname
             TYPE(check_t), INTENT(INOUT) :: check_status
             INTEGER, INTENT(IN) :: var, kgenref_var
             INTEGER :: check_result
             LOGICAL :: is_print = .FALSE.
             
             integer :: diff
             
             check_status%numTotal = check_status%numTotal + 1
             
             IF (var == kgenref_var) THEN
                 check_status%numIdentical = check_status%numIdentical + 1
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
                 END IF 
                 check_result = CHECK_IDENTICAL
             ELSE
                 diff = ABS(var - kgenref_var)
                 IF (diff <= check_status%tolerance) THEN
                     check_status%numInTol = check_status%numInTol + 1
                     IF (check_status%verboseLevel > 0) THEN
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                     END IF 
                     check_result = CHECK_IN_TOL
                 ELSE
                     check_status%numOutTol = check_status%numOutTol + 1
                     IF (check_status%verboseLevel > 0) THEN
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                     END IF 
                     check_result = CHECK_OUT_TOL
                 END IF 
             END IF 
             IF (check_result == CHECK_IDENTICAL) THEN
                 IF (check_status%verboseLevel > 2) THEN
                     WRITE (*, *) "Difference is ", 0
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_OUT_TOL) THEN
                 IF (check_status%verboseLevel > 0) THEN
                     WRITE (*, *) "Difference is ", diff
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_IN_TOL) THEN
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) "Difference is ", diff
                     WRITE (*, *) ""
                 END IF 
             END IF 
             
         END SUBROUTINE kv_clubb_tend_cam_integer__
         
         !verify state subroutine for kv_clubb_tend_cam_real__r8_dim2
         RECURSIVE SUBROUTINE kv_clubb_tend_cam_real__r8_dim2(varname, check_status, var, kgenref_var)
             CHARACTER(LEN=*), INTENT(IN) :: varname
             TYPE(check_t), INTENT(INOUT) :: check_status
             REAL(KIND=r8), INTENT(IN), DIMENSION(:,:) :: var, kgenref_var
             INTEGER :: check_result
             LOGICAL :: is_print = .FALSE.
             
             INTEGER :: idx1, idx2
             INTEGER :: n
             real(KIND=r8) :: nrmsdiff, rmsdiff
             real(KIND=r8), ALLOCATABLE :: buf1(:,:), buf2(:,:)
             
             check_status%numTotal = check_status%numTotal + 1
             
             IF (ALL(var == kgenref_var)) THEN
                 check_status%numIdentical = check_status%numIdentical + 1
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
                 END IF 
                 check_result = CHECK_IDENTICAL
             ELSE
                 ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2)))
                 ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2)))
                 n = COUNT(var /= kgenref_var)
                 WHERE ( ABS(kgenref_var) > check_status%minvalue )
                     buf1 = ((var-kgenref_var)/kgenref_var)**2
                     buf2 = (var-kgenref_var)**2
                 ELSEWHERE
                     buf1 = (var-kgenref_var)**2
                     buf2 = buf1
                 END WHERE 
                 nrmsdiff = SQRT(SUM(buf1)/REAL(n))
                 rmsdiff = SQRT(SUM(buf2)/REAL(n))
                 IF (nrmsdiff > check_status%tolerance) THEN
                     check_status%numOutTol = check_status%numOutTol + 1
                     IF (check_status%verboseLevel > 0) THEN
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                     END IF 
                     check_result = CHECK_OUT_TOL
                 ELSE
                     check_status%numInTol = check_status%numInTol + 1
                     IF (check_status%verboseLevel > 0) THEN
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                     END IF 
                     check_result = CHECK_IN_TOL
                 END IF 
             END IF 
             IF (check_result == CHECK_IDENTICAL) THEN
                 IF (check_status%verboseLevel > 2) THEN
                     WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                     WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                     WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                     WRITE (*, *) "RMS of difference is ", 0
                     WRITE (*, *) "Normalized RMS of difference is ", 0
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_OUT_TOL) THEN
                 IF (check_status%verboseLevel > 0) THEN
                     WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                     WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                     WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                     WRITE (*, *) "RMS of difference is ", rmsdiff
                     WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_IN_TOL) THEN
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                     WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                     WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                     WRITE (*, *) "RMS of difference is ", rmsdiff
                     WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                     WRITE (*, *) ""
                 END IF 
             END IF 
             
         END SUBROUTINE kv_clubb_tend_cam_real__r8_dim2
         
         !verify state subroutine for kv_clubb_tend_cam_real__r8_dim1
         RECURSIVE SUBROUTINE kv_clubb_tend_cam_real__r8_dim1(varname, check_status, var, kgenref_var)
             CHARACTER(LEN=*), INTENT(IN) :: varname
             TYPE(check_t), INTENT(INOUT) :: check_status
             REAL(KIND=r8), INTENT(IN), DIMENSION(:) :: var, kgenref_var
             INTEGER :: check_result
             LOGICAL :: is_print = .FALSE.
             
             INTEGER :: idx1
             INTEGER :: n
             real(KIND=r8) :: nrmsdiff, rmsdiff
             real(KIND=r8), ALLOCATABLE :: buf1(:), buf2(:)
             
             check_status%numTotal = check_status%numTotal + 1
             
             IF (ALL(var == kgenref_var)) THEN
                 check_status%numIdentical = check_status%numIdentical + 1
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
                 END IF 
                 check_result = CHECK_IDENTICAL
             ELSE
                 ALLOCATE (buf1(SIZE(var,dim=1)))
                 ALLOCATE (buf2(SIZE(var,dim=1)))
                 n = COUNT(var /= kgenref_var)
                 WHERE ( ABS(kgenref_var) > check_status%minvalue )
                     buf1 = ((var-kgenref_var)/kgenref_var)**2
                     buf2 = (var-kgenref_var)**2
                 ELSEWHERE
                     buf1 = (var-kgenref_var)**2
                     buf2 = buf1
                 END WHERE 
                 nrmsdiff = SQRT(SUM(buf1)/REAL(n))
                 rmsdiff = SQRT(SUM(buf2)/REAL(n))
                 IF (nrmsdiff > check_status%tolerance) THEN
                     check_status%numOutTol = check_status%numOutTol + 1
                     IF (check_status%verboseLevel > 0) THEN
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                     END IF 
                     check_result = CHECK_OUT_TOL
                 ELSE
                     check_status%numInTol = check_status%numInTol + 1
                     IF (check_status%verboseLevel > 0) THEN
                         WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                     END IF 
                     check_result = CHECK_IN_TOL
                 END IF 
             END IF 
             IF (check_result == CHECK_IDENTICAL) THEN
                 IF (check_status%verboseLevel > 2) THEN
                     WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                     WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                     WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                     WRITE (*, *) "RMS of difference is ", 0
                     WRITE (*, *) "Normalized RMS of difference is ", 0
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_OUT_TOL) THEN
                 IF (check_status%verboseLevel > 0) THEN
                     WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                     WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                     WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                     WRITE (*, *) "RMS of difference is ", rmsdiff
                     WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_IN_TOL) THEN
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                     WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                     WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                     WRITE (*, *) "RMS of difference is ", rmsdiff
                     WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                     WRITE (*, *) ""
                 END IF 
             END IF 
             
         END SUBROUTINE kv_clubb_tend_cam_real__r8_dim1
         
         !verify state subroutine for kv_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1
         RECURSIVE SUBROUTINE kv_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1(varname, check_status, var, kgenref_var)
             CHARACTER(LEN=*), INTENT(IN) :: varname
             TYPE(check_t), INTENT(INOUT) :: check_status
             TYPE(check_t) :: comp_check_status
             TYPE(pdf_parameter), INTENT(IN), DIMENSION(:) :: var, kgenref_var
             INTEGER :: check_result
             LOGICAL :: is_print = .FALSE.
             
             INTEGER :: idx1
             
             check_status%numTotal = check_status%numTotal + 1
             
             CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel)
             DO  idx1=LBOUND(var,1), UBOUND(var,1)
                 CALL kv_pdf_parameter_module_pdf_parameter(trim(adjustl(varname)), comp_check_status, var(idx1), kgenref_var(idx1))
             END DO 
             IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN
                 check_status%numIdentical = check_status%numIdentical + 1
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
                 END IF 
                 check_result = CHECK_IDENTICAL
             ELSE IF (comp_check_status%numOutTol > 0) THEN
                 check_status%numOutTol = check_status%numOutTol + 1
                 IF (check_status%verboseLevel > 0) THEN
                     WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)."
                 END IF 
                 check_result = CHECK_OUT_TOL
             ELSE IF (comp_check_status%numInTol > 0) THEN
                 check_status%numInTol = check_status%numInTol + 1
                 IF (check_status%verboseLevel > 0) THEN
                     WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)."
                 END IF 
                 check_result = CHECK_IN_TOL
             END IF 
             IF (check_result == CHECK_IDENTICAL) THEN
                 IF (check_status%verboseLevel > 2) THEN
                     WRITE (*, *) "    number of elements         : ", comp_check_status%numtotal
                     WRITE (*, *) "    identical                  : ", comp_check_status%numidentical
                     WRITE (*, *) "    not identical - out of tol.: ", comp_check_status%numouttol
                     WRITE (*, *) "    not identical - within tol.: ", comp_check_status%numintol
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_OUT_TOL) THEN
                 IF (check_status%verboseLevel > 0) THEN
                     WRITE (*, *) "    number of elements         : ", comp_check_status%numtotal
                     WRITE (*, *) "    identical                  : ", comp_check_status%numidentical
                     WRITE (*, *) "    not identical - out of tol.: ", comp_check_status%numouttol
                     WRITE (*, *) "    not identical - within tol.: ", comp_check_status%numintol
                     WRITE (*, *) ""
                 END IF 
             ELSE IF (check_result == CHECK_IN_TOL) THEN
                 IF (check_status%verboseLevel > 1) THEN
                     WRITE (*, *) "    number of elements         : ", comp_check_status%numtotal
                     WRITE (*, *) "    identical                  : ", comp_check_status%numidentical
                     WRITE (*, *) "    not identical - out of tol.: ", comp_check_status%numouttol
                     WRITE (*, *) "    not identical - within tol.: ", comp_check_status%numintol
                     WRITE (*, *) ""
                 END IF 
             END IF 
             
         END SUBROUTINE kv_clubb_tend_cam_pdf_parameter__pdf_parameter_dim1
         
  END SUBROUTINE clubb_tend_cam
    
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !
  
    
!-------------------------------------------------------------------------------
! Description: Provide the obukov length and the surface friction velocity 
!              for the dry deposition code in routine tphysac.  Since University
!              of Washington Moist Turbulence (UWMT) scheme is not called when 
!              CLUBB is turned on the obukov length and ustar are never initialized
!              nor computed (sometimes never updated from NaN).  In addition, surface
!              fluxes are applied to the constituents.  
!   
! Author: Peter Bogenschutz, August 2011
! Origin: Based heavily on UWMT code (eddy_diff.F90)
! References:
!   None
!-------------------------------------------------------------------------------

    
    
    ! --------------- !
    ! Input Auguments !
    ! --------------- !

    

    ! ---------------- !
    ! Output Auguments !
    ! ---------------- !
    
    


    ! --------------- !
    ! Local Variables !
    ! --------------- !
    
    
    




    ! ----------------------- !
    ! Main Computation Begins !
    ! ----------------------- !


    
    
    
    ! Compute the surface friction velocity and obukov length
    

    




    
    





! ----------------------------------------------------------------------
!
! DISCLAIMER : this code appears to be correct but has not been
!              very thouroughly tested. If you do notice any
!              anomalous behaviour then please contact Andy and/or
!              Bjorn
!
! Function diag_ustar:  returns value of ustar using the below
! similarity functions and a specified buoyancy flux (bflx) given in
! kinematic units
!
! phi_m (zeta > 0) =  (1 + am * zeta)
! phi_m (zeta < 0) =  (1 - bm * zeta)^(-1/4)
!
! where zeta = z/lmo and lmo = (theta_rev/g*vonk) * (ustar^2/tstar)
!
! Ref: Businger, 1973, Turbulent Transfer in the Atmospheric Surface
! Layer, in Workshop on Micormeteorology, pages 67-100.
!
! Code writen March, 1999 by Bjorn Stevens
!





















  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !



    !
    ! Description: Initializes the statistics saving functionality of
    !   the CLUBB model.  This is for purpose of 1-CLUBB interface.  Here
    !   the traditional stats_init of CLUBB is not called, as it is not compatible
    !   with 1 output.   
    
    !-----------------------------------------------------------------------






    ! Input Variables






    !  Local Variables

    !  Namelist Variables



    !  Local Variables




    !  Initialize

    !  Set stats_variables variables with inputs from calling subroutine
    



    !  Initialize namelist variables


    !  Read variables to compute from the namelist    



    ! Broadcast namelist variables


    !  Hardcode these for use in 1-CLUBB, don't want either

    !  Check sampling and output frequencies

    !  The model time step length, delt (which is dtmain), should multiply
    !  evenly into the statistical sampling time step length, stats_tsamp.


    !  Initialize zt (mass points)








    !  Allocate scratch space



    !  Default initialization for array indices for zt


    !  Initialize zm (momentum points)








    !  Allocate scratch space




    !  Initialize rad_zt (radiation points)

    










       !  Initialize rad_zm (radiation points)
 







   
 


    !  Initialize sfc (surface point)











    ! Check for errors



!   Now call add fields
    

     
    
    

    


    

    




  


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

  
    !-----------------------------------------------------------------------

    !     Description: Called when the stats timestep has ended. This subroutine
    !     is responsible for calling statistics to be written to the output
    !     format.
    !-----------------------------------------------------------------------






    






    


    ! Local Variables


    !  Check if it is time to write to file



    !  Initialize

    !  Look for errors by checking the number of sampling points
    !  for each variable in the zt statistics at each vertical level.







    !  Look for errors by checking the number of sampling points
    !  for each variable in the zm statistics at each vertical level.







      !  Look for errors by checking the number of sampling points
      !  for each variable in the rad_zt statistics at each vertical level.






    
      !  Look for errors by checking the number of sampling points
      !  for each variable in the rad_zm statistics at each vertical level.







    !  Look for errors by checking the number of sampling points
    !  for each variable in the sfc statistics at each vertical level.







    !  Stop the run if errors are found.


    !  Compute averages


   !  Here we are not outputting the data, rather reading the stats into 
   !  arrays which are conformable to 1 output.  Also, the data is "flipped"
   !  in the vertical level to be the same as 1 output.    

    



    

    


    !  Reset sample fields





  
  
  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !


  
    !-----------------------------------------------------------------------

    !     Description:
    !     Initialize stats to zero
    !-----------------------------------------------------------------------




    !  Input

    !  Output

    !  Zero out arrays




  


  ! =============================================================================== !
  !                                                                                 !
  ! =============================================================================== !

  

    !-----------------------------------------------------------------------

    !     Description:
    !     Compute the average of stats fields
    !-----------------------------------------------------------------------


    !  Input

    !  Output

    !  Internal


    !  Compute averages








  
  !read state subroutine for kr_externs_in_clubb_intr
  SUBROUTINE kr_externs_in_clubb_intr(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      READ (UNIT = kgen_unit) do_expldiff
      READ (UNIT = kgen_unit) edsclr_dim
  END SUBROUTINE kr_externs_in_clubb_intr
  
  !read state subroutine for kr_externs_out_clubb_intr
  SUBROUTINE kr_externs_out_clubb_intr(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
  END SUBROUTINE kr_externs_out_clubb_intr
  
end module clubb_intr
