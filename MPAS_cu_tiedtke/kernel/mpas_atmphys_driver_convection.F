!KGEN-generated Fortran source file

!Generated at : 2015-12-24 11:50:58
!KGEN version : 0.6.0

! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://1-dev.github.com/license.html
!
!==================================================================================================
 module mpas_atmphys_driver_convection

     USE mpas_atmphys_vars

!wrf physics:
     USE module_cu_tiedtke

     IMPLICIT NONE
     PRIVATE
     PUBLIC driver_convection



!>\brief MPAS driver for parameterization of convection.
!>\author Laura D. Fowler (send comments to laura@ucar.edu).
!>\date 2013-05-01.
!>
!>\details
!>
!> subroutines in mpas_atmphys_driver_convection:
!> ----------------------------------------------
!> allocate_convection     : allocate local arrays for parameterization of convection.
!> deallocate_convection   : deallocate local arrays for parameterization of convection.
!> init_convection         : initialization of individual convection scheme.
!> driver_convection       : main driver (called from subroutine physics_driver).
!> convection_from_MPAS    : initialize local arrays.
!> convection_to_MPAS      : copy local arrays to MPAS arrays.
!> update_convection_step1 : updates lifetime of deep convective clouds in Kain-Fritsch scheme.
!> update_convection_step2 : updates accumulated precipitation output from convection schemes.
!>
!> WRF physics called from driver_convection:
!> ------------------------------------------
!>    * module_cu_kfeta      : Kain-Fritsch convection scheme.
!>    * module_cu_tiedtke    : Tiedtke convection scheme.

!> add-ons and modifications to sourcecode:
!> ----------------------------------------
!>    * removed the pre-processor option "do_hydrostatic_pressure" before call to the subroutines 
!>      kf_eta_cps and tiedtke.
!>    * removed call to the updated Kain-Fritsch convection scheme.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-05-29.
!>    * added the mean distance between cell centers in the call to the Kain-Fritsch convection
!>      parameterization of convection.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-08-22. 
!>    * in call to subroutine kf_eta_cps, replaced the variable g (that originally pointed to
!>      gravity) with gravity, for simplicity.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
!>    * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
!>    * Modified sourcecode to use pools.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
!>    * in ./physics_wrf, updated the Tiedtke convection scheme to that in WRF version 3.6.1. The
!>      call to subroutine cu_tiedtke has been updated accordingly to include the sensible heat
!>      flux.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-09-11. 


 contains


!==================================================================================================
!==================================================================================================










































!==================================================================================================
!==================================================================================================






























!==================================================================================================
!==================================================================================================

!input arguments:

!inout arguments:

!local pointers:

!local variables:

!--------------------------------------------------------------------------------------------------
! write(0,*)
! write(0,*) '--- enter subroutine init_convection:'



!       write(0,*) '    enter kain-fritsch initialization:'

!       write(0,*) '    end kain-kritsch initialization'

!       write(0,*) '    enter tiedtke initialization:'
!      write(mpas_err_message,'(A,A10)') &
!        'Tiedtke is being tested. Do not use right now. Thanks '
!      call physics_error_fatal(mpas_err_message)



! write(0,*) '--- end subroutine init_convection'


!==================================================================================================
 SUBROUTINE driver_convection(kgen_unit, kgen_total_time, itimestep)
!==================================================================================================

!input and output arguments:
!---------------------------
     USE kgen_utils_mod, ONLY: kgen_dp
     USE mpas_atmphys_vars, ONLY: kr_externs_in_mpas_atmphys_vars
     USE mpas_atmphys_vars, ONLY: kr_externs_out_mpas_atmphys_vars
     USE module_cu_tiedtke, ONLY: kr_externs_in_module_cu_tiedtke
     USE module_cu_tiedtke, ONLY: kr_externs_out_module_cu_tiedtke
     USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
     USE mpas_atmphys_vars, ONLY: kv_externs_mpas_atmphys_vars
     INTEGER, INTENT(INOUT) :: itimestep

!local variables and arrays:
!---------------------------

!variables specific to Kain_Fritsch parameterization:

!temp:

!==================================================================================================
! write(0,*)
! write(0,*) '--- enter convection_driver: dt_cu=',dt_cu

!initialize instantaneous precipitation, and copy convective tendencies from the dynamics to
!the physics grid:
     INTEGER, INTENT(IN) :: kgen_unit
     REAL(KIND=kgen_dp), INTENT(INOUT) :: kgen_total_time
     LOGICAL :: kgen_istrue
     
     TYPE(check_t) :: check_status
     INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock
     INTEGER, PARAMETER :: kgen_maxiter = 1
     REAL(KIND=kgen_dp) :: kgen_elapsed_time

!... convert the convection time-step to minutes:

!... call to convection schemes:






!       write(0,*) '--- enter subroutine cu_tiedtke:'
     
     !extern input variables
     CALL kr_externs_in_mpas_atmphys_vars(kgen_unit)
     CALL kr_externs_in_module_cu_tiedtke(kgen_unit)
     
     !local input variables
     
     !extern output variables
     CALL kr_externs_out_mpas_atmphys_vars(kgen_unit)
     CALL kr_externs_out_module_cu_tiedtke(kgen_unit)
     
     !local output variables
     
     !Uncomment following call statement to turn on perturbation experiment.
     !CALL kgen_perturb_real( your_variable, 1.0E-15 )
     
       call cu_tiedtke ( &
             pcps        = pres_hyd_p    , p8w             = pres2_hyd_p ,           &
             znu         = znu_hyd_p     , t3d             = t_p         ,           &              
             dt          = dt_dyn        , itimestep       = itimestep   ,           &
             stepcu      = n_cu          , raincv          = raincv_p    ,           &
             pratec      = pratec_p      , hfx             = hfx_p       ,           &
             qfx         = qfx_p         , u3d             = u_p         ,           &
             v3d         = v_p           , w               = w_p         ,           &
             qv3d        = qv_p          , qc3d            = qc_p        ,           &
             qi3d        = qi_p          , pi3d            = pi_p        ,           &
             rho3d       = rho_p         , qvften          = rqvdynten_p ,           &
             qvpblten    = rqvdynblten_p , dz8w            = dz_p        ,           &
             xland       = xland_p       , cu_act_flag     = cu_act_flag ,           &
             f_qv        = f_qv          , f_qc            = f_qc        ,           &
             f_qr        = f_qr          , f_qi            = f_qi        ,           &
             f_qs        = f_qs          , rthcuten        = rthcuten_p  ,           &
             rqvcuten    = rqvcuten_p    , rqccuten        = rqccuten_p  ,           &
             rqicuten    = rqicuten_p    , rucuten         = rucuten_p   ,           &
             rvcuten     = rvcuten_p     ,                                           &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde , &
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme , &
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte   &
                    )



!copy instantaneous and accumulated precipitation, convective tendencies, and "other" arrays
!specific to convection parameterization back to the dynamics grid:

! write(0,*) '--- end subroutine convection_driver'

       
       !verify init
       CALL kgen_init_check(check_status, verboseLevel=1)
       
       !extern verify variables
       CALL kv_externs_mpas_atmphys_vars(check_status)
       
       !local verify variables
       WRITE (*, *) ""
       WRITE (*, *) "Number of verified variables: ", check_status%numTotal
       IF (check_status%numOutTol > 0) THEN
           WRITE (*, *) "Verification FAILED"
       ELSE
           WRITE (*, *) "Verification PASSED"
       END IF 
       CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock)
       DO kgen_intvar = 1, kgen_maxiter
           
           !Uncomment following call statement to turn on perturbation experiment.
           !CALL kgen_perturb_real( your_variable, 1.0E-15 )
           
       call cu_tiedtke ( &
             pcps        = pres_hyd_p    , p8w             = pres2_hyd_p ,           &
             znu         = znu_hyd_p     , t3d             = t_p         ,           &              
             dt          = dt_dyn        , itimestep       = itimestep   ,           &
             stepcu      = n_cu          , raincv          = raincv_p    ,           &
             pratec      = pratec_p      , hfx             = hfx_p       ,           &
             qfx         = qfx_p         , u3d             = u_p         ,           &
             v3d         = v_p           , w               = w_p         ,           &
             qv3d        = qv_p          , qc3d            = qc_p        ,           &
             qi3d        = qi_p          , pi3d            = pi_p        ,           &
             rho3d       = rho_p         , qvften          = rqvdynten_p ,           &
             qvpblten    = rqvdynblten_p , dz8w            = dz_p        ,           &
             xland       = xland_p       , cu_act_flag     = cu_act_flag ,           &
             f_qv        = f_qv          , f_qc            = f_qc        ,           &
             f_qr        = f_qr          , f_qi            = f_qi        ,           &
             f_qs        = f_qs          , rthcuten        = rthcuten_p  ,           &
             rqvcuten    = rqvcuten_p    , rqccuten        = rqccuten_p  ,           &
             rqicuten    = rqicuten_p    , rucuten         = rucuten_p   ,           &
             rvcuten     = rvcuten_p     ,                                           &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde , &
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme , &
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte   &
                    )
       END DO 
       CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock)
       kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)
       WRITE (*, *) "cu_tiedtke : Time per call (usec): ", kgen_elapsed_time
       kgen_total_time = kgen_total_time + kgen_elapsed_time
 END SUBROUTINE driver_convection

!==================================================================================================
!==================================================================================================
!input arguments:

!local pointers:

!--------------------------------------------------------------------------------------------------
! write(0,*)
! write(0,*) '--- enter subroutine convection_from_MPAS:'









          !area of grid-cell:       



          !decreases the characteristic time period that convection remains active. When nca_p
          !becomes less than the convective timestep, convective tendencies and precipitation
          !are reset to zero (note that this is also done in subroutine kf_eta_cps).






 

 

!==================================================================================================
!==================================================================================================
!inout arguments:

!local pointers:

!--------------------------------------------------------------------------------------------------












!==================================================================================================
!==================================================================================================
!input arguments:

!inout arguments:

!local pointers:


!local variables and arrays:

!--------------------------------------------------------------------------------------------------






          !decreases the characteristic time period that convection remains active. When nca_p
          !becomes less than the convective timestep, convective tendencies and precipitation
          !are reset to zero (note that this is also done in subroutine kf_eta_cps).
             





!==================================================================================================
!==================================================================================================

!input arguments:

!inout arguments:

!local pointers:


!local variables and arrays:

!--------------------------------------------------------------------------------------------------




!update the accumulated precipitation at the end of each dynamic time-step:





!==================================================================================================
 end module mpas_atmphys_driver_convection
!==================================================================================================