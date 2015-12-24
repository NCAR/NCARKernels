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
 module mpas_atmphys_vars
     USE mpas_kind_types
 
     USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
     IMPLICIT NONE
     PUBLIC
     SAVE


!>\brief contains all local variables and arrays used in the physics parameterizations.
!>\author Laura D. Fowler (send comments to laura@ucar.edu).
!>\date 2013-05-01.
!>
!>\details
!>
!> add-ons and modifications:
!> --------------------------
!>    * added the variables sf_surface_physics,alswvisdir_p,alswvisdif_p,alswnirdir_p,alswnirdif_p,
!>      swvisdir_p,swvisdif_p,swnirdir_p,and swnirdif_p to upgrade the RRTMG short wave radiation
!>      code to WRF version 3.4.1. see definition of each individual variables below.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-03-11.
!>    * removed call to the updated Kain-Fritsch convection scheme.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-05-29.
!>    * added the arrays o3clim_p for implementation of monthly-varying climatological ozone in the
!>      long wave and short wave RRTMG radiation codes.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-07-08.
!>    * corrected definition of local variable dx_p.
!>      Laura D. Fowler (laura@ucar.edu) / 2013-08-23.
!>    * renamed local variable conv_deep_scheme to convection_scheme.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-09-18.
!>    * added empty subroutine atmphys_vars_init that does not do anything, but needed for 
!>      compiling MPAS with some compilers.
!>      Laura D. Fowler (laura@ucar.edu) / 2015-01-12.


!==================================================================================================
!list of physics parameterizations:
!==================================================================================================


!==================================================================================================
!wrf-variables:these variables are needed to keep calls to different physics parameterizations
!as in wrf model.
!==================================================================================================


 integer,public:: ids,ide,jds,jde,kds,kde
 integer,public:: ims,ime,jms,jme,kms,kme
 integer,public:: its,ite,jts,jte,kts,kte


 real(kind=RKIND),public:: dt_dyn     !time-step for dynamics
 


!... arrays related to surface:


 real(kind=RKIND),dimension(:,:,:),allocatable:: &
!... arrays related to u- and v-velocities interpolated to theta points:
    u_p,              &!u-velocity interpolated to theta points                               [m/s]
    v_p                !v-velocity interpolated to theta points                               [m/s]
!... arrays related to u- and v-velocities interpolated to theta points:
    
!... arrays related to vertical sounding:
 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    zz_p,             &!
    pres_p,           &!pressure                                                               [Pa]
    pi_p,             &!(p_phy/p0)**(r_d/cp)                                                    [-]
    z_p,              &!height of layer                                                         [m]
    zmid_p,           &!height of middle of layer                                               [m]
    dz_p,             &!layer thickness                                                         [m]
    t_p,              &!temperature                                                             [K]
    th_p,             &!potential temperature                                                   [K]
    al_p,             &!inverse of air density                                              [m3/kg]
    rho_p,            &!air density                                                         [kg/m3]
    rh_p               !relative humidity                                                       [-]

 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    qv_p,             &!water vapor mixing ratio                                            [kg/kg]
    qc_p,             &!cloud water mixing ratio                                            [kg/kg]
    qr_p,             &!rain mixing ratio                                                   [kg/kg]
    qi_p,             &!cloud ice mixing ratio                                              [kg/kg]
    qs_p,             &!snow mixing ratio                                                   [kg/kg]
    qg_p               !graupel mixing ratio                                                [kg/kg]
 
!... arrays located at w (vertical velocity) points, or at interface between layers:
 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    w_p,              &!vertical velocity                                                     [m/s]
    pres2_p,          &!pressure                                                               [Pa]
    t2_p               !temperature                                                             [K]

!... arrays used for calculating the hydrostatic pressure and exner function:
 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    pres_hyd_p,       &!pressure located at theta levels                                       [Pa]
    pres_hydd_p,      &!"dry" pressure located at theta levels                                 [Pa]
    pres2_hyd_p,      &!pressure located at w-velocity levels                                  [Pa]
    pres2_hydd_p,     &!"dry" pressure located at w-velocity levels                            [Pa]
    znu_hyd_p          !(pres_hyd_p / P0) needed in the Tiedtke convection scheme              [Pa]

!=============================================================================================
!... variables related to ozone climatlogy:
!=============================================================================================


!==================================================================================================
!... variables and arrays related to parameterization of cloud microphysics:
!    warm_phase: logical that determines if we want to run warm-phase cloud microphysics only.
!          If set to false, cold-phase cloud microphysics is active. In MPAS, we always assume
!          that the ice phase is included (except for the Kessler scheme which includes water
!          clouds only.

!    f_qv,f_qc,f_qr,f_qi,f_qs,f_qg: These logicals were initially defined in WRF to determine
!          which kind of hydrometeors are present. Here, we assume that all six water species
!          are present, even if their mixing ratios and number concentrations are zero.

!==================================================================================================


 logical,parameter:: &
    f_qv = .true.,    &!
    f_qc = .true.,    &!
    f_qr = .true.,    &!
    f_qi = .true.,    &!
    f_qs = .true.,    &!
    f_qg = .true.      !



!==================================================================================================
!... variables and arrays related to parameterization of convection:
!==================================================================================================
 integer,public:: n_cu

 logical,dimension(:,:),allocatable:: &
    cu_act_flag
 real(kind=RKIND),dimension(:,:),allocatable::   &
    rainc_p,          &!
    raincv_p,         &!
    pratec_p           !

 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    rthcuten_p,       &!
    rqvcuten_p,       &!
    rqccuten_p,       &!
    rqicuten_p         !

!... kain fritsch specific arrays:


!... tiedtke specific arrays:
 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    znu_p,            &!
    rqvdynten_p,      &!
    rqvdynblten_p      !

 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    rucuten_p,        &!
    rvcuten_p          !

!==================================================================================================
!... variables and arrays related to parameterization of pbl:
!==================================================================================================







!==================================================================================================
!... variables and arrays related to parameterization of gravity wave drag over orography:
!==================================================================================================





!==================================================================================================
!... variables and arrays related to parameterization of surface layer:
!==================================================================================================
 real(kind=RKIND),dimension(:,:),allocatable:: &
    br_p,             &!bulk richardson number                                                  [-]
    cd_p,             &!momentum exchange coeff at 10 meters                                    [?]
    cda_p,            &!momentum exchange coeff at the lowest model level                       [?]
    cpm_p,            &!
    chs_p,            &!
    chs2_p,           &!
    ck_p,             &!enthalpy exchange coeff at 10 meters                                    [?]
    cka_p,            &!enthalpy exchange coeff at the lowest model level                       [?]
    cqs2_p,           &!
    gz1oz0_p,         &!log of z1 over z0                                                       [-]
    flhc_p,           &!exchange coefficient for heat                                           [-]
    flqc_p,           &!exchange coefficient for moisture                                       [-]
    hfx_p,            &!upward heat flux at the surface                                      [W/m2]
    fh_p,             &!integrated stability function for heat                                  [-]
    fm_p,             &!integrated stability function for momentum                              [-]             
    lh_p,             &!latent heat flux at the surface                                      [W/m2]
    mavail_p,         &!surface moisture availability                                           [-]
    mol_p,            &!T* in similarity theory                                                 [K]
    pblh_p,           &!PBL height                                                              [m]
    psih_p,           &!similarity theory for heat                                              [-]
    psim_p,           &!similarity theory for momentum                                          [-]
    q2_p,             &!specific humidity at 2m                                             [kg/kg]
    qfx_p,            &!upward moisture flux at the surface                               [kg/m2/s]
    qgh_p,            &!
    qsfc_p,           &!specific humidity at lower boundary                                 [kg/kg]
    regime_p,         &!flag indicating PBL regime (stable_p,unstable_p,etc...)                 [-]
    rmol_p,           &!1 / Monin Ob length                                                     [-]
    t2m_p,            &!temperature at 2m                                                       [K]
    th2m_p,           &!potential temperature at 2m                                             [K]
    u10_p,            &!u at 10 m                                                             [m/s]
    ust_p,            &!u* in similarity theory                                               [m/s]
    ustm_p,           &!u* in similarity theory       without vconv correction                [m/s]
    v10_p,            &!v at 10 m                                                             [m/s]
    wspd_p,           &!wind speed                                                            [m/s]
    znt_p,            &!time-varying roughness length                                           [m]
    zol_p              !


!==================================================================================================
!... variables and arrays related to parameterization of short-wave radiation:
!==================================================================================================






!==================================================================================================
!... variables and arrays related to parameterization of long-wave radiation:
!==================================================================================================





!==================================================================================================
!... variables and arrays related to parameterization of long- and short-wave radiation needed
!    only by the "CAM" radiation codes:
!==================================================================================================








!==================================================================================================
!.. variables and arrays related to cloudiness:
!==================================================================================================



!==================================================================================================
!.. variables and arrays related to land-surface parameterization:
!==================================================================================================

                       !is only needed in module_ra_rrtmg_sw.F to define the spectral surface
                       !albedos as functions of the land surface scheme.

    




!==================================================================================================
!.. variables and arrays related to surface characteristics:
!==================================================================================================


 real(kind=RKIND),dimension(:,:),allocatable:: &
    sfc_albedo_p,     &!surface albedo                                                          [-]
    sfc_albbck_p,     &!surface background albedo                                               [-]
    sfc_emibck_p,     &!land surface background emissivity                                      [-]
    sfc_emiss_p,      &!land surface emissivity                                                 [-]
    snoalb_p,         &!annual max snow albedo                                                  [-]
    snow_p,           &!snow water equivalent                                              [kg m-2]
    tsk_p,            &!surface-skin temperature                                                [K]
    xice_p,           &!ice mask                                                                [-]
    xland_p            !land mask    (1 for land; 2 for water)                                  [-]


 PUBLIC kr_externs_in_mpas_atmphys_vars
 PUBLIC kr_externs_out_mpas_atmphys_vars
 REAL(KIND=rkind), dimension(:,:,:), allocatable :: kgenref_znu_hyd_p
 LOGICAL, dimension(:,:), allocatable :: kgenref_cu_act_flag
 REAL(KIND=rkind), dimension(:,:), allocatable :: kgenref_raincv_p, kgenref_pratec_p
 REAL(KIND=rkind), dimension(:,:,:), allocatable :: kgenref_rthcuten_p, kgenref_rqvcuten_p, kgenref_rqccuten_p, kgenref_rqicuten_p
 REAL(KIND=rkind), dimension(:,:,:), allocatable :: kgenref_rucuten_p, kgenref_rvcuten_p
 REAL(KIND=rkind), dimension(:,:), allocatable :: kgenref_hfx_p, kgenref_qfx_p
 PUBLIC kv_externs_mpas_atmphys_vars

!==================================================================================================
 
 CONTAINS
 
!==================================================================================================
!dummy subroutine that does not do anything.


!==================================================================================================
 !read state subroutine for kr_externs_in_mpas_atmphys_vars
 SUBROUTINE kr_externs_in_mpas_atmphys_vars(kgen_unit)
     INTEGER, INTENT(IN) :: kgen_unit
     LOGICAL :: kgen_istrue
     
     READ (UNIT = kgen_unit) jde
     READ (UNIT = kgen_unit) kde
     READ (UNIT = kgen_unit) ids
     READ (UNIT = kgen_unit) ide
     READ (UNIT = kgen_unit) jds
     READ (UNIT = kgen_unit) kds
     READ (UNIT = kgen_unit) jms
     READ (UNIT = kgen_unit) ims
     READ (UNIT = kgen_unit) ime
     READ (UNIT = kgen_unit) kme
     READ (UNIT = kgen_unit) jme
     READ (UNIT = kgen_unit) ite
     READ (UNIT = kgen_unit) jts
     READ (UNIT = kgen_unit) kts
     READ (UNIT = kgen_unit) jte
     READ (UNIT = kgen_unit) kte
     READ (UNIT = kgen_unit) its
     READ (UNIT = kgen_unit) dt_dyn
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(u_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(v_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(pi_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(t_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(dz_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rho_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qv_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qi_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(w_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(pres2_hyd_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(pres_hyd_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(znu_hyd_p, kgen_unit)
     READ (UNIT = kgen_unit) n_cu
     CALL kr_mpas_atmphys_vars_logical___dim2(cu_act_flag, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(pratec_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(raincv_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rthcuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rqccuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rqicuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rqvcuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rqvdynblten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rqvdynten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rucuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rvcuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(qfx_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(hfx_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(xland_p, kgen_unit)
 END SUBROUTINE kr_externs_in_mpas_atmphys_vars
 
 !read state subroutine for kr_externs_out_mpas_atmphys_vars
 SUBROUTINE kr_externs_out_mpas_atmphys_vars(kgen_unit)
     INTEGER, INTENT(IN) :: kgen_unit
     
     LOGICAL :: kgen_istrue
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_znu_hyd_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_logical___dim2(kgenref_cu_act_flag, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_pratec_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_raincv_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rthcuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rqccuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rqicuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rqvcuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rucuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rvcuten_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_qfx_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_hfx_p, kgen_unit)
 END SUBROUTINE kr_externs_out_mpas_atmphys_vars
 
 !read state subroutine for kr_mpas_atmphys_vars_real__rkind_dim3
 SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim3(var, kgen_unit, printvar)
     REAL(KIND=rkind), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var
     INTEGER, INTENT(IN) :: kgen_unit
     CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
     LOGICAL :: kgen_istrue
     INTEGER :: idx1, idx2, idx3
     INTEGER, DIMENSION(2,3) :: kgen_bound
     
     READ (UNIT = kgen_unit) kgen_istrue
     IF (kgen_istrue) THEN
         IF (ALLOCATED( var )) THEN
             DEALLOCATE (var)
         END IF 
         READ (UNIT = kgen_unit) kgen_bound(1, 1)
         READ (UNIT = kgen_unit) kgen_bound(2, 1)
         READ (UNIT = kgen_unit) kgen_bound(1, 2)
         READ (UNIT = kgen_unit) kgen_bound(2, 2)
         READ (UNIT = kgen_unit) kgen_bound(1, 3)
         READ (UNIT = kgen_unit) kgen_bound(2, 3)
         ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1, kgen_bound(2,3)-kgen_bound(1,3)+1))
         READ (UNIT = kgen_unit) var
         IF (PRESENT( printvar )) THEN
             WRITE (*, *) "** KGEN DEBUG: " // printvar // " u_p **", SUM(var)
         END IF 
     END IF 
 END SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim3
 
 !read state subroutine for kr_mpas_atmphys_vars_logical___dim2
 SUBROUTINE kr_mpas_atmphys_vars_logical___dim2(var, kgen_unit, printvar)
     LOGICAL, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var
     INTEGER, INTENT(IN) :: kgen_unit
     CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
     LOGICAL :: kgen_istrue
     INTEGER :: idx1, idx2
     INTEGER, DIMENSION(2,2) :: kgen_bound
     
     READ (UNIT = kgen_unit) kgen_istrue
     IF (kgen_istrue) THEN
         IF (ALLOCATED( var )) THEN
             DEALLOCATE (var)
         END IF 
         READ (UNIT = kgen_unit) kgen_bound(1, 1)
         READ (UNIT = kgen_unit) kgen_bound(2, 1)
         READ (UNIT = kgen_unit) kgen_bound(1, 2)
         READ (UNIT = kgen_unit) kgen_bound(2, 2)
         ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1))
         READ (UNIT = kgen_unit) var
         IF (PRESENT( printvar )) THEN
             WRITE (*, *) "** KGEN DEBUG: " // printvar // " cu_act_flag **" // NEW_LINE("A"), var
         END IF 
     END IF 
 END SUBROUTINE kr_mpas_atmphys_vars_logical___dim2
 
 !read state subroutine for kr_mpas_atmphys_vars_real__rkind_dim2
 SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim2(var, kgen_unit, printvar)
     REAL(KIND=rkind), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var
     INTEGER, INTENT(IN) :: kgen_unit
     CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
     LOGICAL :: kgen_istrue
     INTEGER :: idx1, idx2
     INTEGER, DIMENSION(2,2) :: kgen_bound
     
     READ (UNIT = kgen_unit) kgen_istrue
     IF (kgen_istrue) THEN
         IF (ALLOCATED( var )) THEN
             DEALLOCATE (var)
         END IF 
         READ (UNIT = kgen_unit) kgen_bound(1, 1)
         READ (UNIT = kgen_unit) kgen_bound(2, 1)
         READ (UNIT = kgen_unit) kgen_bound(1, 2)
         READ (UNIT = kgen_unit) kgen_bound(2, 2)
         ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1, kgen_bound(2,2)-kgen_bound(1,2)+1))
         READ (UNIT = kgen_unit) var
         IF (PRESENT( printvar )) THEN
             WRITE (*, *) "** KGEN DEBUG: " // printvar // " pratec_p **", SUM(var)
         END IF 
     END IF 
 END SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim2
 
 !verify state subroutine for kv_externs_mpas_atmphys_vars
 SUBROUTINE kv_externs_mpas_atmphys_vars(check_status)
     TYPE(check_t), INTENT(INOUT) :: check_status
     
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("znu_hyd_p", check_status, znu_hyd_p, kgenref_znu_hyd_p)
     CALL kv_mpas_atmphys_vars_logical___dim2("cu_act_flag", check_status, cu_act_flag, kgenref_cu_act_flag)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("pratec_p", check_status, pratec_p, kgenref_pratec_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("raincv_p", check_status, raincv_p, kgenref_raincv_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rthcuten_p", check_status, rthcuten_p, kgenref_rthcuten_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rqccuten_p", check_status, rqccuten_p, kgenref_rqccuten_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rqicuten_p", check_status, rqicuten_p, kgenref_rqicuten_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rqvcuten_p", check_status, rqvcuten_p, kgenref_rqvcuten_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rucuten_p", check_status, rucuten_p, kgenref_rucuten_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rvcuten_p", check_status, rvcuten_p, kgenref_rvcuten_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("qfx_p", check_status, qfx_p, kgenref_qfx_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("hfx_p", check_status, hfx_p, kgenref_hfx_p)
 END SUBROUTINE kv_externs_mpas_atmphys_vars
 
 !verify state subroutine for kv_mpas_atmphys_vars_real__rkind_dim3
 RECURSIVE SUBROUTINE kv_mpas_atmphys_vars_real__rkind_dim3(varname, check_status, var, kgenref_var)
     CHARACTER(LEN=*), INTENT(IN) :: varname
     TYPE(check_t), INTENT(INOUT) :: check_status
     REAL(KIND=rkind), allocatable, INTENT(IN), DIMENSION(:,:,:) :: var, kgenref_var
     INTEGER :: check_result
     LOGICAL :: is_print = .FALSE.
     
     INTEGER :: idx1, idx2, idx3
     INTEGER :: n
     real(KIND=rkind) :: nrmsdiff, rmsdiff
     real(KIND=rkind), ALLOCATABLE :: buf1(:,:,:), buf2(:,:,:)
     
     check_status%numTotal = check_status%numTotal + 1
     
     IF (ALLOCATED(var)) THEN
         IF (ALL(var == kgenref_var)) THEN
             check_status%numIdentical = check_status%numIdentical + 1
             WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
             check_result = CHECK_IDENTICAL
         ELSE
             ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
             ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
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
                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL out of tolerance."
                 check_result = CHECK_OUT_TOL
             ELSE
                 check_status%numInTol = check_status%numInTol + 1
                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL within tolerance."
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
                 
             END IF 
         ELSE IF (check_result == CHECK_OUT_TOL) THEN
             IF (check_status%verboseLevel > 0) THEN
                 WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                 WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                 WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                 WRITE (*, *) "RMS of difference is ", rmsdiff
                 WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                 
             END IF 
         ELSE IF (check_result == CHECK_IN_TOL) THEN
             IF (check_status%verboseLevel > 1) THEN
                 WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                 WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                 WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                 WRITE (*, *) "RMS of difference is ", rmsdiff
                 WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                 
             END IF 
         END IF 
         
     END IF 
 END SUBROUTINE kv_mpas_atmphys_vars_real__rkind_dim3
 
 !verify state subroutine for kv_mpas_atmphys_vars_logical___dim2
 RECURSIVE SUBROUTINE kv_mpas_atmphys_vars_logical___dim2(varname, check_status, var, kgenref_var)
     CHARACTER(LEN=*), INTENT(IN) :: varname
     TYPE(check_t), INTENT(INOUT) :: check_status
     LOGICAL, allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var
     INTEGER :: check_result
     LOGICAL :: is_print = .FALSE.
     
     INTEGER :: idx1, idx2
     INTEGER :: n
     
     check_status%numTotal = check_status%numTotal + 1
     
     IF (ALLOCATED(var)) THEN
         IF (ALL(var .EQV. kgenref_var)) THEN
             check_status%numIdentical = check_status%numIdentical + 1
             WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
             check_result = CHECK_IDENTICAL
         ELSE
             n = COUNT(var /= kgenref_var)
             check_status%numOutTol = check_status%numOutTol + 1
             WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL out of tolerance."
             check_result = CHECK_OUT_TOL
         END IF 
         IF (check_result == CHECK_IDENTICAL) THEN
             IF (check_status%verboseLevel > 2) THEN
                 WRITE (*, *) "NOT IMPLEMENTED"
             END IF 
         ELSE IF (check_result == CHECK_OUT_TOL) THEN
             IF (check_status%verboseLevel > 0) THEN
                 WRITE (*, *) "NOT IMPLEMENTED"
             END IF 
         ELSE IF (check_result == CHECK_IN_TOL) THEN
             IF (check_status%verboseLevel > 1) THEN
                 WRITE (*, *) "NOT IMPLEMENTED"
             END IF 
         END IF 
         
     END IF 
 END SUBROUTINE kv_mpas_atmphys_vars_logical___dim2
 
 !verify state subroutine for kv_mpas_atmphys_vars_real__rkind_dim2
 RECURSIVE SUBROUTINE kv_mpas_atmphys_vars_real__rkind_dim2(varname, check_status, var, kgenref_var)
     CHARACTER(LEN=*), INTENT(IN) :: varname
     TYPE(check_t), INTENT(INOUT) :: check_status
     REAL(KIND=rkind), allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var
     INTEGER :: check_result
     LOGICAL :: is_print = .FALSE.
     
     INTEGER :: idx1, idx2
     INTEGER :: n
     real(KIND=rkind) :: nrmsdiff, rmsdiff
     real(KIND=rkind), ALLOCATABLE :: buf1(:,:), buf2(:,:)
     
     check_status%numTotal = check_status%numTotal + 1
     
     IF (ALLOCATED(var)) THEN
         IF (ALL(var == kgenref_var)) THEN
             check_status%numIdentical = check_status%numIdentical + 1
             WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
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
                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL out of tolerance."
                 check_result = CHECK_OUT_TOL
             ELSE
                 check_status%numInTol = check_status%numInTol + 1
                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL within tolerance."
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
                 
             END IF 
         ELSE IF (check_result == CHECK_OUT_TOL) THEN
             IF (check_status%verboseLevel > 0) THEN
                 WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                 WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                 WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                 WRITE (*, *) "RMS of difference is ", rmsdiff
                 WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                 
             END IF 
         ELSE IF (check_result == CHECK_IN_TOL) THEN
             IF (check_status%verboseLevel > 1) THEN
                 WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different."
                 WRITE (*, *) "Average - kernel ", sum(var)/real(size(var))
                 WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var))
                 WRITE (*, *) "RMS of difference is ", rmsdiff
                 WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff
                 
             END IF 
         END IF 
         
     END IF 
 END SUBROUTINE kv_mpas_atmphys_vars_real__rkind_dim2
 
 end module mpas_atmphys_vars
!==================================================================================================