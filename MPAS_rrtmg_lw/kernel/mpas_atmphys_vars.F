!KGEN-generated Fortran source file

!Generated at : 2015-12-29 15:06:49
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


 


!... arrays related to surface:


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

 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    o3clim_p           !climatological ozone volume mixing ratio                         [???]

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
    warm_rain=.false.  !warm-phase cloud microphysics only (used in WRF).

 logical,parameter:: &
    f_qv = .true.,    &!
    f_qc = .true.,    &!
    f_qr = .true.,    &!
    f_qi = .true.,    &!
    f_qs = .true.,    &!
    f_qg = .true.      !

 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    f_ice,            &!fraction of cloud ice (used in WRF only).
    f_rain             !fraction of rain (used in WRF only).


!==================================================================================================
!... variables and arrays related to parameterization of convection:
!==================================================================================================



!... kain fritsch specific arrays:


!... tiedtke specific arrays:


!==================================================================================================
!... variables and arrays related to parameterization of pbl:
!==================================================================================================







!==================================================================================================
!... variables and arrays related to parameterization of gravity wave drag over orography:
!==================================================================================================





!==================================================================================================
!... variables and arrays related to parameterization of surface layer:
!==================================================================================================


!==================================================================================================
!... variables and arrays related to parameterization of short-wave radiation:
!==================================================================================================






!==================================================================================================
!... variables and arrays related to parameterization of long-wave radiation:
!==================================================================================================


 real(kind=RKIND),dimension(:,:),allocatable:: &
    glw_p,            &!net longwave flux at surface                                        [W m-2]
    lwcf_p,           &!longwave cloud forcing at top-of-atmosphere                         [W m-2]
    lwdnb_p,          &!all-sky downwelling longwave flux at bottom-of-atmosphere           [J m-2]
    lwdnbc_p,         &!clear-sky downwelling longwave flux at bottom-of-atmosphere         [J m-2]
    lwdnt_p,          &!all-sky downwelling longwave flux at top-of-atmosphere              [J m-2]
    lwdntc_p,         &!clear-sky downwelling longwave flux at top-of-atmosphere            [J m-2]
    lwupb_p,          &!all-sky upwelling longwave flux at bottom-of-atmosphere             [J m-2]
    lwupbc_p,         &!clear-sky upwelling longwave flux at bottom-of-atmosphere           [J m-2]
    lwupt_p,          &!all-sky upwelling longwave flux at top-of-atmosphere                [J m-2]
    lwuptc_p,         &!clear-sky upwelling longwave flux at top-of-atmosphere              [J m-2]
    olrtoa_p           !outgoing longwave radiation at top-of-the-atmosphere                [W m-2]


 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    rthratenlw_p       !uncoupled theta tendency due to longwave radiation                  [K s-1]

!==================================================================================================
!... variables and arrays related to parameterization of long- and short-wave radiation needed
!    only by the "CAM" radiation codes:
!==================================================================================================


 integer:: num_oznlevels

 real(kind=RKIND),dimension(:),allocatable:: &
    pin_p,            &!pressure levels for ozone concentration                                [Pa]
    m_hybi_p           !hybrid levels for aerosols                                             [-]





!==================================================================================================
!.. variables and arrays related to cloudiness:
!==================================================================================================

 integer,parameter:: &
    icloud= 1          !used in WRF only.

 real(kind=RKIND),dimension(:,:,:),allocatable:: &
    cldfrac_p          !cloud fraction                                                          [-]

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
 REAL(KIND=rkind), dimension(:,:), allocatable :: kgenref_glw_p, kgenref_lwcf_p, kgenref_lwdnb_p, kgenref_lwdnbc_p, kgenref_lwdnt_p, kgenref_lwdntc_p, kgenref_lwupb_p, kgenref_lwupbc_p, kgenref_lwupt_p, kgenref_lwuptc_p, kgenref_olrtoa_p
 REAL(KIND=rkind), dimension(:,:,:), allocatable :: kgenref_rthratenlw_p
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
     READ (UNIT = kgen_unit) kme
     READ (UNIT = kgen_unit) ime
     READ (UNIT = kgen_unit) jms
     READ (UNIT = kgen_unit) ims
     READ (UNIT = kgen_unit) kms
     READ (UNIT = kgen_unit) jme
     READ (UNIT = kgen_unit) ite
     READ (UNIT = kgen_unit) jts
     READ (UNIT = kgen_unit) kts
     READ (UNIT = kgen_unit) jte
     READ (UNIT = kgen_unit) kte
     READ (UNIT = kgen_unit) its
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(pi_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(t_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(dz_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rho_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qr_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qv_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qi_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qs_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(qg_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(t2_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(pres2_hyd_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(pres_hyd_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(o3clim_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(f_rain, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(f_ice, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwupt_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwcf_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwupbc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(olrtoa_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwdnt_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwuptc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(glw_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwdntc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwupb_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwdnb_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(lwdnbc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(rthratenlw_p, kgen_unit)
     READ (UNIT = kgen_unit) num_oznlevels
     CALL kr_mpas_atmphys_vars_real__rkind_dim1(pin_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(cldfrac_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(sfc_emiss_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(xland_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(tsk_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(snow_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(xice_p, kgen_unit)
 END SUBROUTINE kr_externs_in_mpas_atmphys_vars
 
 !read state subroutine for kr_externs_out_mpas_atmphys_vars
 SUBROUTINE kr_externs_out_mpas_atmphys_vars(kgen_unit)
     INTEGER, INTENT(IN) :: kgen_unit
     
     LOGICAL :: kgen_istrue
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwupt_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwcf_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwupbc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_olrtoa_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwdnt_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwuptc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_glw_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwdntc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwupb_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwdnb_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim2(kgenref_lwdnbc_p, kgen_unit)
     CALL kr_mpas_atmphys_vars_real__rkind_dim3(kgenref_rthratenlw_p, kgen_unit)
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
             WRITE (*, *) "** KGEN DEBUG: " // printvar // " pi_p **", SUM(var)
         END IF 
     END IF 
 END SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim3
 
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
             WRITE (*, *) "** KGEN DEBUG: " // printvar // " lwupt_p **", SUM(var)
         END IF 
     END IF 
 END SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim2
 
 !read state subroutine for kr_mpas_atmphys_vars_real__rkind_dim1
 SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim1(var, kgen_unit, printvar)
     REAL(KIND=rkind), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var
     INTEGER, INTENT(IN) :: kgen_unit
     CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
     LOGICAL :: kgen_istrue
     INTEGER :: idx1
     INTEGER, DIMENSION(2,1) :: kgen_bound
     
     READ (UNIT = kgen_unit) kgen_istrue
     IF (kgen_istrue) THEN
         IF (ALLOCATED( var )) THEN
             DEALLOCATE (var)
         END IF 
         READ (UNIT = kgen_unit) kgen_bound(1, 1)
         READ (UNIT = kgen_unit) kgen_bound(2, 1)
         ALLOCATE (var(kgen_bound(2,1)-kgen_bound(1,1)+1))
         READ (UNIT = kgen_unit) var
         IF (PRESENT( printvar )) THEN
             WRITE (*, *) "** KGEN DEBUG: " // printvar // " pin_p **", SUM(var)
         END IF 
     END IF 
 END SUBROUTINE kr_mpas_atmphys_vars_real__rkind_dim1
 
 !verify state subroutine for kv_externs_mpas_atmphys_vars
 SUBROUTINE kv_externs_mpas_atmphys_vars(check_status)
     TYPE(check_t), INTENT(INOUT) :: check_status
     
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwupt_p", check_status, lwupt_p, kgenref_lwupt_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwcf_p", check_status, lwcf_p, kgenref_lwcf_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwupbc_p", check_status, lwupbc_p, kgenref_lwupbc_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("olrtoa_p", check_status, olrtoa_p, kgenref_olrtoa_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwdnt_p", check_status, lwdnt_p, kgenref_lwdnt_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwuptc_p", check_status, lwuptc_p, kgenref_lwuptc_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("glw_p", check_status, glw_p, kgenref_glw_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwdntc_p", check_status, lwdntc_p, kgenref_lwdntc_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwupb_p", check_status, lwupb_p, kgenref_lwupb_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwdnb_p", check_status, lwdnb_p, kgenref_lwdnb_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim2("lwdnbc_p", check_status, lwdnbc_p, kgenref_lwdnbc_p)
     CALL kv_mpas_atmphys_vars_real__rkind_dim3("rthratenlw_p", check_status, rthratenlw_p, kgenref_rthratenlw_p)
 END SUBROUTINE kv_externs_mpas_atmphys_vars
 
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
         
     END IF 
 END SUBROUTINE kv_mpas_atmphys_vars_real__rkind_dim2
 
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
             IF (check_status%verboseLevel > 1) THEN
                 WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL."
             END IF 
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
         
     END IF 
 END SUBROUTINE kv_mpas_atmphys_vars_real__rkind_dim3
 
 end module mpas_atmphys_vars
!==================================================================================================