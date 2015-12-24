!KGEN-generated Fortran source file

!Generated at : 2015-12-24 11:55:33
!KGEN version : 0.6.0

! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://1-dev.github.com/license.html
!
!==================================================================================================
 module mpas_atmphys_driver_radiation_lw

     USE mpas_atmphys_constants
     USE mpas_atmphys_vars

!wrf physics:
     USE module_ra_rrtmg_lw

     IMPLICIT NONE
     PRIVATE
     PUBLIC driver_radiation_lw



!>\brief MPAS driver for parameterization of longwave radiation codes.
!>\author Laura D. Fowler (send comments to laura@ucar.edu).
!>\date 2013-05-01.
!>
!>\details
!>
!> subroutines in mpas_atmphys_driver_radiation_lw:
!> ------------------------------------------------
!> allocate_radiation_lw     : allocate local arrays for parameterization of lw radiation codes.
!> deallocate_radiation_lw   : deallocate local arrays for parameterization of lw radiation codes.
!> init_radiation_lw         : initialization of individual lw radiation codes.
!> driver_radiation_lw       : main driver (called from subroutine physics_driver).
!> radiation_lw_from_MPAS    : initialize local arrays.
!> radiation_lw_to_MPAS      : copy local arrays to MPAS arrays.
!> radiation_camlw_to_MPAS   : save local arrays (absorption, emission) for CAM lw radiation code.
!>
!> WRF physics called from driver_radiation_lw:
!> --------------------------------------------
!>    * module_ra_cam        : CAM long wave radiation code.
!>    * module_ra_rrtmg_lw   : RRTMG long wave radiation code.
!>
!> add-ons and modifications to sourcecode:
!> ----------------------------------------
!>    * removed the pre-processor option "do_hydrostatic_pressure" before call to subroutines
!>      rrtmg_lw and camrad.
!>      Laura D. Fowler (birch.mmm,ucar.edu) / 2013-05-29.
!>    * added structure diag in the call to subroutine init_radiation_lw.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-07-01.
!>    * modified the call to subroutine rrtmg_lwrad to include the option of using the same ozone
!>      climatology as the one used in the CAM radiation codes.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-07-17.
!>    * in call to subroutine rrtmg_lwrad, replaced the variable g (that originally pointed to 
!>      gravity) with gravity, for simplicity.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
!>    * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
!>    * modified sourcecode to use pools.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-05-15.


 contains


!==================================================================================================
!==================================================================================================

!input arguments:

!--------------------------------------------------------------------------------------------------























































       






       !allocate these arrays on the first time step, only:







!==================================================================================================
!==================================================================================================



















 



































       








!==================================================================================================
!==================================================================================================

!input arguments:


!inout arguments:

!local pointers:


!local variables and arrays:

!--------------------------------------------------------------------------------------------------










!initialization:



 





          !ozone mixing ratio:














       !On the first time-step of each model run, the local arrays absnxt_p, absnst_p,
       !and emstot_p are filled with the MPAS arrays abstot, absnxt, and emstot. If it
       !is a new run, these three arrays will be initialized to zero;If a restart run,
       !these three arrays will be filled with the restart values.




       !ozone mixing ratio:


       !aerosol mixing ratio:






!format:


!==================================================================================================
!==================================================================================================

!input arguments:

!local pointers:

!local variables and arrays:

!--------------------------------------------------------------------------------------------------








!==================================================================================================
!==================================================================================================

!input arguments:

!local pointers:

!---------------------------------------------------------------------------------------------


!write(0,*) '--- writing absnxt,abstot,and emstot to restart =', l_camlw





!==================================================================================================
!==================================================================================================

!input arguments:


!inout arguments:

!--------------------------------------------------------------------------------------------------

! write(0,*)
! write(0,*) '--- enter radiation_lw initialization:'

!       write(0,*) '    enter subroutine rrtmg_lwinit:'
!       write(0,*) '    end subroutine rrtmg_lwinit'

!       write(0,*) '    enter subroutine camradinit:'
!       write(0,*) '    end subroutine camradinit'
    

! write(0,*) '--- end radiation_lw initialization'


!==================================================================================================
 SUBROUTINE driver_radiation_lw(kgen_unit, kgen_total_time)
!==================================================================================================

!input arguments:
     USE kgen_utils_mod, ONLY: kgen_dp
     USE mpas_atmphys_vars, ONLY: kr_externs_in_mpas_atmphys_vars
     USE mpas_atmphys_vars, ONLY: kr_externs_out_mpas_atmphys_vars
     USE rrlw_cld, ONLY: kr_externs_in_rrlw_cld
     USE rrlw_con, ONLY: kr_externs_in_rrlw_con
     USE rrlw_con, ONLY: kr_externs_out_rrlw_con
     USE rrlw_kg01, ONLY: kr_externs_in_rrlw_kg01
     USE rrlw_kg02, ONLY: kr_externs_in_rrlw_kg02
     USE rrlw_kg03, ONLY: kr_externs_in_rrlw_kg03
     USE rrlw_kg04, ONLY: kr_externs_in_rrlw_kg04
     USE rrlw_kg05, ONLY: kr_externs_in_rrlw_kg05
     USE rrlw_kg06, ONLY: kr_externs_in_rrlw_kg06
     USE rrlw_kg07, ONLY: kr_externs_in_rrlw_kg07
     USE rrlw_kg08, ONLY: kr_externs_in_rrlw_kg08
     USE rrlw_kg09, ONLY: kr_externs_in_rrlw_kg09
     USE rrlw_kg10, ONLY: kr_externs_in_rrlw_kg10
     USE rrlw_kg11, ONLY: kr_externs_in_rrlw_kg11
     USE rrlw_kg12, ONLY: kr_externs_in_rrlw_kg12
     USE rrlw_kg13, ONLY: kr_externs_in_rrlw_kg13
     USE rrlw_kg14, ONLY: kr_externs_in_rrlw_kg14
     USE rrlw_kg15, ONLY: kr_externs_in_rrlw_kg15
     USE rrlw_kg16, ONLY: kr_externs_in_rrlw_kg16
     USE rrlw_ref, ONLY: kr_externs_in_rrlw_ref
     USE rrlw_tbl, ONLY: kr_externs_in_rrlw_tbl
     USE rrlw_vsn, ONLY: kr_externs_in_rrlw_vsn
     USE rrlw_vsn, ONLY: kr_externs_out_rrlw_vsn
     USE rrlw_wvn, ONLY: kr_externs_in_rrlw_wvn
     USE rrtmg_lw_rtrnmc, ONLY: kr_externs_in_rrtmg_lw_rtrnmc
     USE rrtmg_lw_rtrnmc, ONLY: kr_externs_out_rrtmg_lw_rtrnmc
     USE module_ra_rrtmg_lw, ONLY: kr_externs_in_module_ra_rrtmg_lw
     USE module_ra_rrtmg_lw, ONLY: kr_externs_out_module_ra_rrtmg_lw
     USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
     USE mpas_atmphys_vars, ONLY: kv_externs_mpas_atmphys_vars
     USE rrlw_con, ONLY: kv_externs_rrlw_con
     USE rrlw_vsn, ONLY: kv_externs_rrlw_vsn
     USE module_ra_rrtmg_lw, ONLY: kv_externs_module_ra_rrtmg_lw


!inout arguments:

!local pointers:

!local variables:
     INTEGER :: o3input

!--------------------------------------------------------------------------------------------------
     INTEGER, INTENT(IN) :: kgen_unit
     REAL(KIND=kgen_dp), INTENT(INOUT) :: kgen_total_time
     LOGICAL :: kgen_istrue
     
     TYPE(check_t) :: check_status
     INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock
     INTEGER, PARAMETER :: kgen_maxiter = 1
     REAL(KIND=kgen_dp) :: kgen_elapsed_time
! write(0,100)

!formats:


!copy MPAS arrays to local arrays:

!call to longwave radiation scheme:

!       write(0,*) '--- enter subroutine rrtmg_lwrad:'


     
     !extern input variables
     CALL kr_externs_in_mpas_atmphys_vars(kgen_unit)
     CALL kr_externs_in_rrlw_cld(kgen_unit)
     CALL kr_externs_in_rrlw_con(kgen_unit)
     CALL kr_externs_in_rrlw_kg01(kgen_unit)
     CALL kr_externs_in_rrlw_kg02(kgen_unit)
     CALL kr_externs_in_rrlw_kg03(kgen_unit)
     CALL kr_externs_in_rrlw_kg04(kgen_unit)
     CALL kr_externs_in_rrlw_kg05(kgen_unit)
     CALL kr_externs_in_rrlw_kg06(kgen_unit)
     CALL kr_externs_in_rrlw_kg07(kgen_unit)
     CALL kr_externs_in_rrlw_kg08(kgen_unit)
     CALL kr_externs_in_rrlw_kg09(kgen_unit)
     CALL kr_externs_in_rrlw_kg10(kgen_unit)
     CALL kr_externs_in_rrlw_kg11(kgen_unit)
     CALL kr_externs_in_rrlw_kg12(kgen_unit)
     CALL kr_externs_in_rrlw_kg13(kgen_unit)
     CALL kr_externs_in_rrlw_kg14(kgen_unit)
     CALL kr_externs_in_rrlw_kg15(kgen_unit)
     CALL kr_externs_in_rrlw_kg16(kgen_unit)
     CALL kr_externs_in_rrlw_ref(kgen_unit)
     CALL kr_externs_in_rrlw_tbl(kgen_unit)
     CALL kr_externs_in_rrlw_vsn(kgen_unit)
     CALL kr_externs_in_rrlw_wvn(kgen_unit)
     CALL kr_externs_in_rrtmg_lw_rtrnmc(kgen_unit)
     CALL kr_externs_in_module_ra_rrtmg_lw(kgen_unit)
     
     !local input variables
     READ (UNIT = kgen_unit) o3input
     
     !extern output variables
     CALL kr_externs_out_mpas_atmphys_vars(kgen_unit)
     CALL kr_externs_out_rrlw_con(kgen_unit)
     CALL kr_externs_out_rrlw_vsn(kgen_unit)
     CALL kr_externs_out_rrtmg_lw_rtrnmc(kgen_unit)
     CALL kr_externs_out_module_ra_rrtmg_lw(kgen_unit)
     
     !local output variables
     
     !Uncomment following call statement to turn on perturbation experiment.
     !CALL kgen_perturb_real( your_variable, 1.0E-15 )
     
       call rrtmg_lwrad( &
             p3d        = pres_hyd_p   , p8w        = pres2_hyd_p , pi3d       = pi_p          ,&
             t3d        = t_p          , t8w        = t2_p        , rho3d      = rho_p         ,&
             rthratenlw = rthratenlw_p , lwupt      = lwupt_p     , lwuptc     = lwuptc_p      ,&
             lwdnt      = lwdnt_p      , lwdntc     = lwdntc_p    , lwupb      = lwupb_p       ,&
             lwupbc     = lwupbc_p     , lwdnb      = lwdnb_p     , lwdnbc     = lwdnbc_p      ,&
             lwcf       = lwcf_p       , glw        = glw_p       , olr        = olrtoa_p      ,&
             emiss      = sfc_emiss_p  , tsk        = tsk_p       , dz8w       = dz_p          ,&
             cldfra3d   = cldfrac_p    , r          = R_d         , g          = gravity       ,&
             icloud     = icloud       , warm_rain  = warm_rain   , f_ice_phy  = f_ice         ,&
             f_rain_phy = f_rain       , xland      = xland_p     , xice       = xice_p        ,&
             snow       = snow_p       , qv3d       = qv_p        , qc3d       = qc_p          ,&
             qr3d       = qr_p         , qi3d       = qi_p        , qs3d       = qs_p          ,&
             qg3d       = qg_p         , f_qv       = f_qv        , f_qc       = f_qc          ,& 
             f_qr       = f_qr         , f_qi       = f_qi        , f_qs       = f_qs          ,&
             f_qg       = f_qg         , o3input    = o3input     , noznlevels = num_oznlevels ,&
             pin        = pin_p        , o3clim     = o3clim_p    ,                             &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,&
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,&
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte  &
                       )
!       write(0,*) '--- exit subroutine rrtmg_lwrad'


       !... calculates solar declination:

!... convert the radiation time_step to minutes:

!       write(0,*) '--- enter subroutine camrad_lw: doabsems=',doabsems



!copy local arrays to MPAS grid:

! write(0,*) '--- end subroutine driver_radiation_lw'

!formats:

       
       !verify init
       CALL kgen_init_check(check_status, verboseLevel=1)
       
       !extern verify variables
       CALL kv_externs_mpas_atmphys_vars(check_status)
       CALL kv_externs_rrlw_con(check_status)
       CALL kv_externs_rrlw_vsn(check_status)
       CALL kv_externs_module_ra_rrtmg_lw(check_status)
       
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
           
       call rrtmg_lwrad( &
             p3d        = pres_hyd_p   , p8w        = pres2_hyd_p , pi3d       = pi_p          ,&
             t3d        = t_p          , t8w        = t2_p        , rho3d      = rho_p         ,&
             rthratenlw = rthratenlw_p , lwupt      = lwupt_p     , lwuptc     = lwuptc_p      ,&
             lwdnt      = lwdnt_p      , lwdntc     = lwdntc_p    , lwupb      = lwupb_p       ,&
             lwupbc     = lwupbc_p     , lwdnb      = lwdnb_p     , lwdnbc     = lwdnbc_p      ,&
             lwcf       = lwcf_p       , glw        = glw_p       , olr        = olrtoa_p      ,&
             emiss      = sfc_emiss_p  , tsk        = tsk_p       , dz8w       = dz_p          ,&
             cldfra3d   = cldfrac_p    , r          = R_d         , g          = gravity       ,&
             icloud     = icloud       , warm_rain  = warm_rain   , f_ice_phy  = f_ice         ,&
             f_rain_phy = f_rain       , xland      = xland_p     , xice       = xice_p        ,&
             snow       = snow_p       , qv3d       = qv_p        , qc3d       = qc_p          ,&
             qr3d       = qr_p         , qi3d       = qi_p        , qs3d       = qs_p          ,&
             qg3d       = qg_p         , f_qv       = f_qv        , f_qc       = f_qc          ,& 
             f_qr       = f_qr         , f_qi       = f_qi        , f_qs       = f_qs          ,&
             f_qg       = f_qg         , o3input    = o3input     , noznlevels = num_oznlevels ,&
             pin        = pin_p        , o3clim     = o3clim_p    ,                             &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,&
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,&
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte  &
                       )
       END DO 
       CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock)
       kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter)
       WRITE (*, *) "rrtmg_lwrad : Time per call (usec): ", kgen_elapsed_time
       kgen_total_time = kgen_total_time + kgen_elapsed_time
 END SUBROUTINE driver_radiation_lw

!==================================================================================================
 end module mpas_atmphys_driver_radiation_lw
!==================================================================================================