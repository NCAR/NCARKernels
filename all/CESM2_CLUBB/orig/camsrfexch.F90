!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:34 
!KGEN version : 0.8.1 
  


module camsrfexch
!-----------------------------------------------------------------------
! Module to handle data that is exchanged between the CAM atmosphere
! model and the surface models (land, sea-ice, and ocean).
!-----------------------------------------------------------------------
! USES:
!
!
!
!
    USE shr_kind_mod, ONLY: r8 => shr_kind_r8 
    USE constituents, ONLY: pcnst 
    USE ppgrid, ONLY: pcols 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
!----------------------------------------------------------------------- 
! PRIVATE: Make default data and interfaces private
!----------------------------------------------------------------------- 

    PRIVATE 
! Public interfaces
!
!
! Public data types
!
!
    PUBLIC cam_in_t 
!---------------------------------------------------------------------------
! This is the data that is sent from the atmosphere to the surface models
!---------------------------------------------------------------------------


!---------------------------------------------------------------------------
! This is the merged state of sea-ice, land and ocean surface parameterizations
!---------------------------------------------------------------------------


  type cam_in_t    
     integer  :: lchnk                   ! chunk index
     integer  :: ncol                    ! number of active columns
     real(r8) :: asdir(pcols)            ! albedo: shortwave, direct
     real(r8) :: asdif(pcols)            ! albedo: shortwave, diffuse
     real(r8) :: aldir(pcols)            ! albedo: longwave, direct
     real(r8) :: aldif(pcols)            ! albedo: longwave, diffuse
     real(r8) :: lwup(pcols)             ! longwave up radiative flux
     real(r8) :: lhf(pcols)              ! latent heat flux
     real(r8) :: shf(pcols)              ! sensible heat flux
     real(r8) :: wsx(pcols)              ! surface u-stress (N)
     real(r8) :: wsy(pcols)              ! surface v-stress (N)
     real(r8) :: tref(pcols)             ! ref height surface air temp
     real(r8) :: qref(pcols)             ! ref height specific humidity 
     real(r8) :: u10(pcols)              ! 10m wind speed
     real(r8) :: ts(pcols)               ! merged surface temp 
     real(r8) :: sst(pcols)              ! sea surface temp
     real(r8) :: snowhland(pcols)        ! snow depth (liquid water equivalent) over land 
     real(r8) :: snowhice(pcols)         ! snow depth over ice
     real(r8) :: fco2_lnd(pcols)         ! co2 flux from lnd
     real(r8) :: fco2_ocn(pcols)         ! co2 flux from ocn
     real(r8) :: fdms(pcols)             ! dms flux
     real(r8) :: landfrac(pcols)         ! land area fraction
     real(r8) :: icefrac(pcols)          ! sea-ice areal fraction
     real(r8) :: ocnfrac(pcols)          ! ocean areal fraction
     real(r8), pointer, dimension(:) :: ram1  !aerodynamical resistance (s/m) (pcols)
     real(r8), pointer, dimension(:) :: fv    !friction velocity (m/s) (pcols)
     real(r8), pointer, dimension(:) :: soilw !volumetric soil water (m3/m3)
     real(r8) :: cflx(pcols,pcnst)       ! constituent flux (emissions)
     real(r8) :: ustar(pcols)            ! atm/ocn saved version of ustar
     real(r8) :: re(pcols)               ! atm/ocn saved version of re
     real(r8) :: ssq(pcols)              ! atm/ocn saved version of ssq
     real(r8), pointer, dimension(:,:) :: depvel ! deposition velocities
     real(r8), pointer, dimension(:,:) :: dstflx ! dust fluxes
     real(r8), pointer, dimension(:,:) :: meganflx ! MEGAN fluxes
     real(r8), pointer, dimension(:,:) :: fireflx ! wild fire emissions
     real(r8), pointer, dimension(:)   :: fireztop ! wild fire emissions vert distribution top
  end type cam_in_t    
!===============================================================================
  PUBLIC kr_camsrfexch_cam_in_t 
  PUBLIC kv_camsrfexch_cam_in_t 

!===============================================================================
!----------------------------------------------------------------------- 
! BOP
! !IROUTINE: hub2atm_alloc
! !DESCRIPTION:
!   Allocate space for the surface to atmosphere data type. And initialize
!   the values.
!-----------------------------------------------------------------------
! !INTERFACE
    
  CONTAINS 
    

! 
!
!
!
! 
!
!


!===============================================================================
!----------------------------------------------------------------------- 
! BOP
! !IROUTINE: atm2hub_alloc
! !DESCRIPTION:
!   Allocate space for the atmosphere to surface data type. And initialize
!   the values.
!-----------------------------------------------------------------------
! !INTERFACE

!
!

! 
!
!
!
! 
!
!


!======================================================================


  !read state subroutine for kr_camsrfexch_cam_in_t 
  RECURSIVE SUBROUTINE kr_camsrfexch_cam_in_t(var, kgen_unit, printname, printvar) 
      TYPE(cam_in_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%lchnk 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%lchnk = ", var%lchnk 
      END IF   
        
      READ (UNIT = kgen_unit) var%ncol 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ncol = ", var%ncol 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%asdir 
          CALL kgen_array_sumcheck(printname // "%asdir", kgen_array_sum, DBLE(SUM(var%asdir, mask=(var%asdir .eq. var%asdir))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%asdir)) = ", DBLE(SUM(var%asdir, mask=(var%asdir .eq. &
              &var%asdir))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%asdif 
          CALL kgen_array_sumcheck(printname // "%asdif", kgen_array_sum, DBLE(SUM(var%asdif, mask=(var%asdif .eq. var%asdif))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%asdif)) = ", DBLE(SUM(var%asdif, mask=(var%asdif .eq. &
              &var%asdif))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%aldir 
          CALL kgen_array_sumcheck(printname // "%aldir", kgen_array_sum, DBLE(SUM(var%aldir, mask=(var%aldir .eq. var%aldir))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%aldir)) = ", DBLE(SUM(var%aldir, mask=(var%aldir .eq. &
              &var%aldir))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%aldif 
          CALL kgen_array_sumcheck(printname // "%aldif", kgen_array_sum, DBLE(SUM(var%aldif, mask=(var%aldif .eq. var%aldif))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%aldif)) = ", DBLE(SUM(var%aldif, mask=(var%aldif .eq. &
              &var%aldif))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%lwup 
          CALL kgen_array_sumcheck(printname // "%lwup", kgen_array_sum, DBLE(SUM(var%lwup, mask=(var%lwup .eq. var%lwup))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%lwup)) = ", DBLE(SUM(var%lwup, mask=(var%lwup .eq. &
              &var%lwup))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%lhf 
          CALL kgen_array_sumcheck(printname // "%lhf", kgen_array_sum, DBLE(SUM(var%lhf, mask=(var%lhf .eq. var%lhf))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%lhf)) = ", DBLE(SUM(var%lhf, mask=(var%lhf .eq. var%lhf))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%shf 
          CALL kgen_array_sumcheck(printname // "%shf", kgen_array_sum, DBLE(SUM(var%shf, mask=(var%shf .eq. var%shf))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%shf)) = ", DBLE(SUM(var%shf, mask=(var%shf .eq. var%shf))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%wsx 
          CALL kgen_array_sumcheck(printname // "%wsx", kgen_array_sum, DBLE(SUM(var%wsx, mask=(var%wsx .eq. var%wsx))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%wsx)) = ", DBLE(SUM(var%wsx, mask=(var%wsx .eq. var%wsx))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%wsy 
          CALL kgen_array_sumcheck(printname // "%wsy", kgen_array_sum, DBLE(SUM(var%wsy, mask=(var%wsy .eq. var%wsy))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%wsy)) = ", DBLE(SUM(var%wsy, mask=(var%wsy .eq. var%wsy))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%tref 
          CALL kgen_array_sumcheck(printname // "%tref", kgen_array_sum, DBLE(SUM(var%tref, mask=(var%tref .eq. var%tref))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%tref)) = ", DBLE(SUM(var%tref, mask=(var%tref .eq. &
              &var%tref))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%qref 
          CALL kgen_array_sumcheck(printname // "%qref", kgen_array_sum, DBLE(SUM(var%qref, mask=(var%qref .eq. var%qref))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%qref)) = ", DBLE(SUM(var%qref, mask=(var%qref .eq. &
              &var%qref))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%u10 
          CALL kgen_array_sumcheck(printname // "%u10", kgen_array_sum, DBLE(SUM(var%u10, mask=(var%u10 .eq. var%u10))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%u10)) = ", DBLE(SUM(var%u10, mask=(var%u10 .eq. var%u10))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ts 
          CALL kgen_array_sumcheck(printname // "%ts", kgen_array_sum, DBLE(SUM(var%ts, mask=(var%ts .eq. var%ts))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ts)) = ", DBLE(SUM(var%ts, mask=(var%ts .eq. var%ts))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%sst 
          CALL kgen_array_sumcheck(printname // "%sst", kgen_array_sum, DBLE(SUM(var%sst, mask=(var%sst .eq. var%sst))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%sst)) = ", DBLE(SUM(var%sst, mask=(var%sst .eq. var%sst))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%snowhland 
          CALL kgen_array_sumcheck(printname // "%snowhland", kgen_array_sum, DBLE(SUM(var%snowhland, mask=(var%snowhland .eq. &
          &var%snowhland))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%snowhland)) = ", DBLE(SUM(var%snowhland, mask=(var%snowhland &
              &.eq. var%snowhland))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%snowhice 
          CALL kgen_array_sumcheck(printname // "%snowhice", kgen_array_sum, DBLE(SUM(var%snowhice, mask=(var%snowhice .eq. &
          &var%snowhice))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%snowhice)) = ", DBLE(SUM(var%snowhice, mask=(var%snowhice &
              &.eq. var%snowhice))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%fco2_lnd 
          CALL kgen_array_sumcheck(printname // "%fco2_lnd", kgen_array_sum, DBLE(SUM(var%fco2_lnd, mask=(var%fco2_lnd .eq. &
          &var%fco2_lnd))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%fco2_lnd)) = ", DBLE(SUM(var%fco2_lnd, mask=(var%fco2_lnd &
              &.eq. var%fco2_lnd))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%fco2_ocn 
          CALL kgen_array_sumcheck(printname // "%fco2_ocn", kgen_array_sum, DBLE(SUM(var%fco2_ocn, mask=(var%fco2_ocn .eq. &
          &var%fco2_ocn))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%fco2_ocn)) = ", DBLE(SUM(var%fco2_ocn, mask=(var%fco2_ocn &
              &.eq. var%fco2_ocn))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%fdms 
          CALL kgen_array_sumcheck(printname // "%fdms", kgen_array_sum, DBLE(SUM(var%fdms, mask=(var%fdms .eq. var%fdms))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%fdms)) = ", DBLE(SUM(var%fdms, mask=(var%fdms .eq. &
              &var%fdms))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%landfrac 
          CALL kgen_array_sumcheck(printname // "%landfrac", kgen_array_sum, DBLE(SUM(var%landfrac, mask=(var%landfrac .eq. &
          &var%landfrac))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%landfrac)) = ", DBLE(SUM(var%landfrac, mask=(var%landfrac &
              &.eq. var%landfrac))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%icefrac 
          CALL kgen_array_sumcheck(printname // "%icefrac", kgen_array_sum, DBLE(SUM(var%icefrac, mask=(var%icefrac .eq. &
          &var%icefrac))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%icefrac)) = ", DBLE(SUM(var%icefrac, mask=(var%icefrac .eq. &
              &var%icefrac))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ocnfrac 
          CALL kgen_array_sumcheck(printname // "%ocnfrac", kgen_array_sum, DBLE(SUM(var%ocnfrac, mask=(var%ocnfrac .eq. &
          &var%ocnfrac))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ocnfrac)) = ", DBLE(SUM(var%ocnfrac, mask=(var%ocnfrac .eq. &
              &var%ocnfrac))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%ram1, kgen_unit, printname // "%ram1", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%ram1, kgen_unit, printname // "%ram1", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%fv, kgen_unit, printname // "%fv", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%fv, kgen_unit, printname // "%fv", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%soilw, kgen_unit, printname // "%soilw", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%soilw, kgen_unit, printname // "%soilw", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%cflx 
          CALL kgen_array_sumcheck(printname // "%cflx", kgen_array_sum, DBLE(SUM(var%cflx, mask=(var%cflx .eq. var%cflx))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%cflx)) = ", DBLE(SUM(var%cflx, mask=(var%cflx .eq. &
              &var%cflx))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ustar 
          CALL kgen_array_sumcheck(printname // "%ustar", kgen_array_sum, DBLE(SUM(var%ustar, mask=(var%ustar .eq. var%ustar))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ustar)) = ", DBLE(SUM(var%ustar, mask=(var%ustar .eq. &
              &var%ustar))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%re 
          CALL kgen_array_sumcheck(printname // "%re", kgen_array_sum, DBLE(SUM(var%re, mask=(var%re .eq. var%re))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%re)) = ", DBLE(SUM(var%re, mask=(var%re .eq. var%re))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ssq 
          CALL kgen_array_sumcheck(printname // "%ssq", kgen_array_sum, DBLE(SUM(var%ssq, mask=(var%ssq .eq. var%ssq))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ssq)) = ", DBLE(SUM(var%ssq, mask=(var%ssq .eq. var%ssq))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%depvel, kgen_unit, printname // "%depvel", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%depvel, kgen_unit, printname // "%depvel", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%dstflx, kgen_unit, printname // "%dstflx", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%dstflx, kgen_unit, printname // "%dstflx", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%meganflx, kgen_unit, printname // "%meganflx", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%meganflx, kgen_unit, printname // "%meganflx", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%fireflx, kgen_unit, printname // "%fireflx", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim2_ptr(var%fireflx, kgen_unit, printname // "%fireflx", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%fireztop, kgen_unit, printname // "%fireztop", .TRUE.) 
      ELSE 
          CALL kr_cam_in_t_real__r8_dim1_ptr(var%fireztop, kgen_unit, printname // "%fireztop", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_camsrfexch_cam_in_t 
    
  !write state subroutine for kr_cam_in_t_real__r8_dim1_ptr 
  SUBROUTINE kr_cam_in_t_real__r8_dim1_ptr(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), POINTER, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_cam_in_t_real__r8_dim1_ptr 
    
  !write state subroutine for kr_cam_in_t_real__r8_dim2_ptr 
  SUBROUTINE kr_cam_in_t_real__r8_dim2_ptr(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), POINTER, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_cam_in_t_real__r8_dim2_ptr 
    
  !verify state subroutine for kv_camsrfexch_cam_in_t 
  RECURSIVE SUBROUTINE kv_camsrfexch_cam_in_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(cam_in_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_lchnk 
      integer :: diff_ncol 
      INTEGER :: n_asdir 
      real(KIND=r8) :: nrmsdiff_asdir, rmsdiff_asdir 
      real(KIND=r8), ALLOCATABLE :: buf1_asdir(:), buf2_asdir(:) 
      INTEGER :: n_asdif 
      real(KIND=r8) :: nrmsdiff_asdif, rmsdiff_asdif 
      real(KIND=r8), ALLOCATABLE :: buf1_asdif(:), buf2_asdif(:) 
      INTEGER :: n_aldir 
      real(KIND=r8) :: nrmsdiff_aldir, rmsdiff_aldir 
      real(KIND=r8), ALLOCATABLE :: buf1_aldir(:), buf2_aldir(:) 
      INTEGER :: n_aldif 
      real(KIND=r8) :: nrmsdiff_aldif, rmsdiff_aldif 
      real(KIND=r8), ALLOCATABLE :: buf1_aldif(:), buf2_aldif(:) 
      INTEGER :: n_lwup 
      real(KIND=r8) :: nrmsdiff_lwup, rmsdiff_lwup 
      real(KIND=r8), ALLOCATABLE :: buf1_lwup(:), buf2_lwup(:) 
      INTEGER :: n_lhf 
      real(KIND=r8) :: nrmsdiff_lhf, rmsdiff_lhf 
      real(KIND=r8), ALLOCATABLE :: buf1_lhf(:), buf2_lhf(:) 
      INTEGER :: n_shf 
      real(KIND=r8) :: nrmsdiff_shf, rmsdiff_shf 
      real(KIND=r8), ALLOCATABLE :: buf1_shf(:), buf2_shf(:) 
      INTEGER :: n_wsx 
      real(KIND=r8) :: nrmsdiff_wsx, rmsdiff_wsx 
      real(KIND=r8), ALLOCATABLE :: buf1_wsx(:), buf2_wsx(:) 
      INTEGER :: n_wsy 
      real(KIND=r8) :: nrmsdiff_wsy, rmsdiff_wsy 
      real(KIND=r8), ALLOCATABLE :: buf1_wsy(:), buf2_wsy(:) 
      INTEGER :: n_tref 
      real(KIND=r8) :: nrmsdiff_tref, rmsdiff_tref 
      real(KIND=r8), ALLOCATABLE :: buf1_tref(:), buf2_tref(:) 
      INTEGER :: n_qref 
      real(KIND=r8) :: nrmsdiff_qref, rmsdiff_qref 
      real(KIND=r8), ALLOCATABLE :: buf1_qref(:), buf2_qref(:) 
      INTEGER :: n_u10 
      real(KIND=r8) :: nrmsdiff_u10, rmsdiff_u10 
      real(KIND=r8), ALLOCATABLE :: buf1_u10(:), buf2_u10(:) 
      INTEGER :: n_ts 
      real(KIND=r8) :: nrmsdiff_ts, rmsdiff_ts 
      real(KIND=r8), ALLOCATABLE :: buf1_ts(:), buf2_ts(:) 
      INTEGER :: n_sst 
      real(KIND=r8) :: nrmsdiff_sst, rmsdiff_sst 
      real(KIND=r8), ALLOCATABLE :: buf1_sst(:), buf2_sst(:) 
      INTEGER :: n_snowhland 
      real(KIND=r8) :: nrmsdiff_snowhland, rmsdiff_snowhland 
      real(KIND=r8), ALLOCATABLE :: buf1_snowhland(:), buf2_snowhland(:) 
      INTEGER :: n_snowhice 
      real(KIND=r8) :: nrmsdiff_snowhice, rmsdiff_snowhice 
      real(KIND=r8), ALLOCATABLE :: buf1_snowhice(:), buf2_snowhice(:) 
      INTEGER :: n_fco2_lnd 
      real(KIND=r8) :: nrmsdiff_fco2_lnd, rmsdiff_fco2_lnd 
      real(KIND=r8), ALLOCATABLE :: buf1_fco2_lnd(:), buf2_fco2_lnd(:) 
      INTEGER :: n_fco2_ocn 
      real(KIND=r8) :: nrmsdiff_fco2_ocn, rmsdiff_fco2_ocn 
      real(KIND=r8), ALLOCATABLE :: buf1_fco2_ocn(:), buf2_fco2_ocn(:) 
      INTEGER :: n_fdms 
      real(KIND=r8) :: nrmsdiff_fdms, rmsdiff_fdms 
      real(KIND=r8), ALLOCATABLE :: buf1_fdms(:), buf2_fdms(:) 
      INTEGER :: n_landfrac 
      real(KIND=r8) :: nrmsdiff_landfrac, rmsdiff_landfrac 
      real(KIND=r8), ALLOCATABLE :: buf1_landfrac(:), buf2_landfrac(:) 
      INTEGER :: n_icefrac 
      real(KIND=r8) :: nrmsdiff_icefrac, rmsdiff_icefrac 
      real(KIND=r8), ALLOCATABLE :: buf1_icefrac(:), buf2_icefrac(:) 
      INTEGER :: n_ocnfrac 
      real(KIND=r8) :: nrmsdiff_ocnfrac, rmsdiff_ocnfrac 
      real(KIND=r8), ALLOCATABLE :: buf1_ocnfrac(:), buf2_ocnfrac(:) 
      INTEGER :: n_ram1 
      real(KIND=r8) :: nrmsdiff_ram1, rmsdiff_ram1 
      real(KIND=r8), ALLOCATABLE :: buf1_ram1(:), buf2_ram1(:) 
      INTEGER :: n_fv 
      real(KIND=r8) :: nrmsdiff_fv, rmsdiff_fv 
      real(KIND=r8), ALLOCATABLE :: buf1_fv(:), buf2_fv(:) 
      INTEGER :: n_soilw 
      real(KIND=r8) :: nrmsdiff_soilw, rmsdiff_soilw 
      real(KIND=r8), ALLOCATABLE :: buf1_soilw(:), buf2_soilw(:) 
      INTEGER :: n_cflx 
      real(KIND=r8) :: nrmsdiff_cflx, rmsdiff_cflx 
      real(KIND=r8), ALLOCATABLE :: buf1_cflx(:,:), buf2_cflx(:,:) 
      INTEGER :: n_ustar 
      real(KIND=r8) :: nrmsdiff_ustar, rmsdiff_ustar 
      real(KIND=r8), ALLOCATABLE :: buf1_ustar(:), buf2_ustar(:) 
      INTEGER :: n_re 
      real(KIND=r8) :: nrmsdiff_re, rmsdiff_re 
      real(KIND=r8), ALLOCATABLE :: buf1_re(:), buf2_re(:) 
      INTEGER :: n_ssq 
      real(KIND=r8) :: nrmsdiff_ssq, rmsdiff_ssq 
      real(KIND=r8), ALLOCATABLE :: buf1_ssq(:), buf2_ssq(:) 
      INTEGER :: n_depvel 
      real(KIND=r8) :: nrmsdiff_depvel, rmsdiff_depvel 
      real(KIND=r8), ALLOCATABLE :: buf1_depvel(:,:), buf2_depvel(:,:) 
      INTEGER :: n_dstflx 
      real(KIND=r8) :: nrmsdiff_dstflx, rmsdiff_dstflx 
      real(KIND=r8), ALLOCATABLE :: buf1_dstflx(:,:), buf2_dstflx(:,:) 
      INTEGER :: n_meganflx 
      real(KIND=r8) :: nrmsdiff_meganflx, rmsdiff_meganflx 
      real(KIND=r8), ALLOCATABLE :: buf1_meganflx(:,:), buf2_meganflx(:,:) 
      INTEGER :: n_fireflx 
      real(KIND=r8) :: nrmsdiff_fireflx, rmsdiff_fireflx 
      real(KIND=r8), ALLOCATABLE :: buf1_fireflx(:,:), buf2_fireflx(:,:) 
      INTEGER :: n_fireztop 
      real(KIND=r8) :: nrmsdiff_fireztop, rmsdiff_fireztop 
      real(KIND=r8), ALLOCATABLE :: buf1_fireztop(:), buf2_fireztop(:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%lchnk == kgenref_var%lchnk) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lchnk is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_lchnk = ABS(var%lchnk - kgenref_var%lchnk) 
          IF (diff_lchnk <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lchnk is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lchnk is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_lchnk 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_lchnk 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ncol == kgenref_var%ncol) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ncol is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ncol = ABS(var%ncol - kgenref_var%ncol) 
          IF (diff_ncol <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ncol is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ncol is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ncol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ncol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%asdir == kgenref_var%asdir)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%asdir is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_asdir(SIZE(var%asdir,dim=1))) 
          ALLOCATE (buf2_asdir(SIZE(var%asdir,dim=1))) 
          n_asdir = COUNT(var%asdir /= kgenref_var%asdir) 
          WHERE ( ABS(kgenref_var%asdir) > kgen_minvalue ) 
              buf1_asdir = ((var%asdir-kgenref_var%asdir)/kgenref_var%asdir)**2 
              buf2_asdir = (var%asdir-kgenref_var%asdir)**2 
          ELSEWHERE 
              buf1_asdir = (var%asdir-kgenref_var%asdir)**2 
              buf2_asdir = buf1_asdir 
          END WHERE   
          nrmsdiff_asdir = SQRT(SUM(buf1_asdir)/REAL(n_asdir)) 
          rmsdiff_asdir = SQRT(SUM(buf2_asdir)/REAL(n_asdir)) 
          IF (rmsdiff_asdir > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%asdir is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%asdir is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%asdir /= kgenref_var%asdir), " of ", size( var%asdir ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%asdir)/real(size(var%asdir)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%asdir)/real(size(kgenref_var%asdir)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_asdir 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_asdir 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%asdir /= kgenref_var%asdir), " of ", size( var%asdir ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%asdir)/real(size(var%asdir)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%asdir)/real(size(kgenref_var%asdir)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_asdir 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_asdir 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%asdif == kgenref_var%asdif)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%asdif is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_asdif(SIZE(var%asdif,dim=1))) 
          ALLOCATE (buf2_asdif(SIZE(var%asdif,dim=1))) 
          n_asdif = COUNT(var%asdif /= kgenref_var%asdif) 
          WHERE ( ABS(kgenref_var%asdif) > kgen_minvalue ) 
              buf1_asdif = ((var%asdif-kgenref_var%asdif)/kgenref_var%asdif)**2 
              buf2_asdif = (var%asdif-kgenref_var%asdif)**2 
          ELSEWHERE 
              buf1_asdif = (var%asdif-kgenref_var%asdif)**2 
              buf2_asdif = buf1_asdif 
          END WHERE   
          nrmsdiff_asdif = SQRT(SUM(buf1_asdif)/REAL(n_asdif)) 
          rmsdiff_asdif = SQRT(SUM(buf2_asdif)/REAL(n_asdif)) 
          IF (rmsdiff_asdif > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%asdif is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%asdif is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%asdif /= kgenref_var%asdif), " of ", size( var%asdif ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%asdif)/real(size(var%asdif)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%asdif)/real(size(kgenref_var%asdif)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_asdif 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_asdif 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%asdif /= kgenref_var%asdif), " of ", size( var%asdif ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%asdif)/real(size(var%asdif)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%asdif)/real(size(kgenref_var%asdif)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_asdif 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_asdif 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%aldir == kgenref_var%aldir)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%aldir is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_aldir(SIZE(var%aldir,dim=1))) 
          ALLOCATE (buf2_aldir(SIZE(var%aldir,dim=1))) 
          n_aldir = COUNT(var%aldir /= kgenref_var%aldir) 
          WHERE ( ABS(kgenref_var%aldir) > kgen_minvalue ) 
              buf1_aldir = ((var%aldir-kgenref_var%aldir)/kgenref_var%aldir)**2 
              buf2_aldir = (var%aldir-kgenref_var%aldir)**2 
          ELSEWHERE 
              buf1_aldir = (var%aldir-kgenref_var%aldir)**2 
              buf2_aldir = buf1_aldir 
          END WHERE   
          nrmsdiff_aldir = SQRT(SUM(buf1_aldir)/REAL(n_aldir)) 
          rmsdiff_aldir = SQRT(SUM(buf2_aldir)/REAL(n_aldir)) 
          IF (rmsdiff_aldir > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%aldir is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%aldir is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%aldir /= kgenref_var%aldir), " of ", size( var%aldir ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%aldir)/real(size(var%aldir)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%aldir)/real(size(kgenref_var%aldir)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_aldir 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_aldir 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%aldir /= kgenref_var%aldir), " of ", size( var%aldir ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%aldir)/real(size(var%aldir)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%aldir)/real(size(kgenref_var%aldir)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_aldir 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_aldir 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%aldif == kgenref_var%aldif)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%aldif is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_aldif(SIZE(var%aldif,dim=1))) 
          ALLOCATE (buf2_aldif(SIZE(var%aldif,dim=1))) 
          n_aldif = COUNT(var%aldif /= kgenref_var%aldif) 
          WHERE ( ABS(kgenref_var%aldif) > kgen_minvalue ) 
              buf1_aldif = ((var%aldif-kgenref_var%aldif)/kgenref_var%aldif)**2 
              buf2_aldif = (var%aldif-kgenref_var%aldif)**2 
          ELSEWHERE 
              buf1_aldif = (var%aldif-kgenref_var%aldif)**2 
              buf2_aldif = buf1_aldif 
          END WHERE   
          nrmsdiff_aldif = SQRT(SUM(buf1_aldif)/REAL(n_aldif)) 
          rmsdiff_aldif = SQRT(SUM(buf2_aldif)/REAL(n_aldif)) 
          IF (rmsdiff_aldif > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%aldif is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%aldif is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%aldif /= kgenref_var%aldif), " of ", size( var%aldif ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%aldif)/real(size(var%aldif)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%aldif)/real(size(kgenref_var%aldif)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_aldif 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_aldif 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%aldif /= kgenref_var%aldif), " of ", size( var%aldif ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%aldif)/real(size(var%aldif)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%aldif)/real(size(kgenref_var%aldif)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_aldif 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_aldif 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%lwup == kgenref_var%lwup)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lwup is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_lwup(SIZE(var%lwup,dim=1))) 
          ALLOCATE (buf2_lwup(SIZE(var%lwup,dim=1))) 
          n_lwup = COUNT(var%lwup /= kgenref_var%lwup) 
          WHERE ( ABS(kgenref_var%lwup) > kgen_minvalue ) 
              buf1_lwup = ((var%lwup-kgenref_var%lwup)/kgenref_var%lwup)**2 
              buf2_lwup = (var%lwup-kgenref_var%lwup)**2 
          ELSEWHERE 
              buf1_lwup = (var%lwup-kgenref_var%lwup)**2 
              buf2_lwup = buf1_lwup 
          END WHERE   
          nrmsdiff_lwup = SQRT(SUM(buf1_lwup)/REAL(n_lwup)) 
          rmsdiff_lwup = SQRT(SUM(buf2_lwup)/REAL(n_lwup)) 
          IF (rmsdiff_lwup > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lwup is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lwup is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%lwup /= kgenref_var%lwup), " of ", size( var%lwup ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%lwup)/real(size(var%lwup)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%lwup)/real(size(kgenref_var%lwup)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_lwup 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lwup 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%lwup /= kgenref_var%lwup), " of ", size( var%lwup ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%lwup)/real(size(var%lwup)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%lwup)/real(size(kgenref_var%lwup)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_lwup 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lwup 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%lhf == kgenref_var%lhf)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lhf is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_lhf(SIZE(var%lhf,dim=1))) 
          ALLOCATE (buf2_lhf(SIZE(var%lhf,dim=1))) 
          n_lhf = COUNT(var%lhf /= kgenref_var%lhf) 
          WHERE ( ABS(kgenref_var%lhf) > kgen_minvalue ) 
              buf1_lhf = ((var%lhf-kgenref_var%lhf)/kgenref_var%lhf)**2 
              buf2_lhf = (var%lhf-kgenref_var%lhf)**2 
          ELSEWHERE 
              buf1_lhf = (var%lhf-kgenref_var%lhf)**2 
              buf2_lhf = buf1_lhf 
          END WHERE   
          nrmsdiff_lhf = SQRT(SUM(buf1_lhf)/REAL(n_lhf)) 
          rmsdiff_lhf = SQRT(SUM(buf2_lhf)/REAL(n_lhf)) 
          IF (rmsdiff_lhf > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lhf is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lhf is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%lhf /= kgenref_var%lhf), " of ", size( var%lhf ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%lhf)/real(size(var%lhf)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%lhf)/real(size(kgenref_var%lhf)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_lhf 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lhf 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%lhf /= kgenref_var%lhf), " of ", size( var%lhf ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%lhf)/real(size(var%lhf)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%lhf)/real(size(kgenref_var%lhf)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_lhf 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lhf 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%shf == kgenref_var%shf)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%shf is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_shf(SIZE(var%shf,dim=1))) 
          ALLOCATE (buf2_shf(SIZE(var%shf,dim=1))) 
          n_shf = COUNT(var%shf /= kgenref_var%shf) 
          WHERE ( ABS(kgenref_var%shf) > kgen_minvalue ) 
              buf1_shf = ((var%shf-kgenref_var%shf)/kgenref_var%shf)**2 
              buf2_shf = (var%shf-kgenref_var%shf)**2 
          ELSEWHERE 
              buf1_shf = (var%shf-kgenref_var%shf)**2 
              buf2_shf = buf1_shf 
          END WHERE   
          nrmsdiff_shf = SQRT(SUM(buf1_shf)/REAL(n_shf)) 
          rmsdiff_shf = SQRT(SUM(buf2_shf)/REAL(n_shf)) 
          IF (rmsdiff_shf > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%shf is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%shf is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%shf /= kgenref_var%shf), " of ", size( var%shf ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%shf)/real(size(var%shf)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%shf)/real(size(kgenref_var%shf)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_shf 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_shf 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%shf /= kgenref_var%shf), " of ", size( var%shf ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%shf)/real(size(var%shf)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%shf)/real(size(kgenref_var%shf)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_shf 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_shf 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%wsx == kgenref_var%wsx)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%wsx is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_wsx(SIZE(var%wsx,dim=1))) 
          ALLOCATE (buf2_wsx(SIZE(var%wsx,dim=1))) 
          n_wsx = COUNT(var%wsx /= kgenref_var%wsx) 
          WHERE ( ABS(kgenref_var%wsx) > kgen_minvalue ) 
              buf1_wsx = ((var%wsx-kgenref_var%wsx)/kgenref_var%wsx)**2 
              buf2_wsx = (var%wsx-kgenref_var%wsx)**2 
          ELSEWHERE 
              buf1_wsx = (var%wsx-kgenref_var%wsx)**2 
              buf2_wsx = buf1_wsx 
          END WHERE   
          nrmsdiff_wsx = SQRT(SUM(buf1_wsx)/REAL(n_wsx)) 
          rmsdiff_wsx = SQRT(SUM(buf2_wsx)/REAL(n_wsx)) 
          IF (rmsdiff_wsx > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%wsx is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%wsx is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%wsx /= kgenref_var%wsx), " of ", size( var%wsx ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%wsx)/real(size(var%wsx)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%wsx)/real(size(kgenref_var%wsx)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_wsx 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_wsx 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%wsx /= kgenref_var%wsx), " of ", size( var%wsx ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%wsx)/real(size(var%wsx)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%wsx)/real(size(kgenref_var%wsx)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_wsx 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_wsx 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%wsy == kgenref_var%wsy)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%wsy is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_wsy(SIZE(var%wsy,dim=1))) 
          ALLOCATE (buf2_wsy(SIZE(var%wsy,dim=1))) 
          n_wsy = COUNT(var%wsy /= kgenref_var%wsy) 
          WHERE ( ABS(kgenref_var%wsy) > kgen_minvalue ) 
              buf1_wsy = ((var%wsy-kgenref_var%wsy)/kgenref_var%wsy)**2 
              buf2_wsy = (var%wsy-kgenref_var%wsy)**2 
          ELSEWHERE 
              buf1_wsy = (var%wsy-kgenref_var%wsy)**2 
              buf2_wsy = buf1_wsy 
          END WHERE   
          nrmsdiff_wsy = SQRT(SUM(buf1_wsy)/REAL(n_wsy)) 
          rmsdiff_wsy = SQRT(SUM(buf2_wsy)/REAL(n_wsy)) 
          IF (rmsdiff_wsy > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%wsy is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%wsy is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%wsy /= kgenref_var%wsy), " of ", size( var%wsy ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%wsy)/real(size(var%wsy)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%wsy)/real(size(kgenref_var%wsy)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_wsy 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_wsy 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%wsy /= kgenref_var%wsy), " of ", size( var%wsy ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%wsy)/real(size(var%wsy)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%wsy)/real(size(kgenref_var%wsy)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_wsy 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_wsy 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%tref == kgenref_var%tref)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%tref is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_tref(SIZE(var%tref,dim=1))) 
          ALLOCATE (buf2_tref(SIZE(var%tref,dim=1))) 
          n_tref = COUNT(var%tref /= kgenref_var%tref) 
          WHERE ( ABS(kgenref_var%tref) > kgen_minvalue ) 
              buf1_tref = ((var%tref-kgenref_var%tref)/kgenref_var%tref)**2 
              buf2_tref = (var%tref-kgenref_var%tref)**2 
          ELSEWHERE 
              buf1_tref = (var%tref-kgenref_var%tref)**2 
              buf2_tref = buf1_tref 
          END WHERE   
          nrmsdiff_tref = SQRT(SUM(buf1_tref)/REAL(n_tref)) 
          rmsdiff_tref = SQRT(SUM(buf2_tref)/REAL(n_tref)) 
          IF (rmsdiff_tref > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tref is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tref is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%tref /= kgenref_var%tref), " of ", size( var%tref ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%tref)/real(size(var%tref)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%tref)/real(size(kgenref_var%tref)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_tref 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tref 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%tref /= kgenref_var%tref), " of ", size( var%tref ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%tref)/real(size(var%tref)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%tref)/real(size(kgenref_var%tref)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_tref 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tref 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%qref == kgenref_var%qref)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%qref is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_qref(SIZE(var%qref,dim=1))) 
          ALLOCATE (buf2_qref(SIZE(var%qref,dim=1))) 
          n_qref = COUNT(var%qref /= kgenref_var%qref) 
          WHERE ( ABS(kgenref_var%qref) > kgen_minvalue ) 
              buf1_qref = ((var%qref-kgenref_var%qref)/kgenref_var%qref)**2 
              buf2_qref = (var%qref-kgenref_var%qref)**2 
          ELSEWHERE 
              buf1_qref = (var%qref-kgenref_var%qref)**2 
              buf2_qref = buf1_qref 
          END WHERE   
          nrmsdiff_qref = SQRT(SUM(buf1_qref)/REAL(n_qref)) 
          rmsdiff_qref = SQRT(SUM(buf2_qref)/REAL(n_qref)) 
          IF (rmsdiff_qref > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%qref is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%qref is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%qref /= kgenref_var%qref), " of ", size( var%qref ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%qref)/real(size(var%qref)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%qref)/real(size(kgenref_var%qref)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_qref 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_qref 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%qref /= kgenref_var%qref), " of ", size( var%qref ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%qref)/real(size(var%qref)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%qref)/real(size(kgenref_var%qref)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_qref 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_qref 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%u10 == kgenref_var%u10)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%u10 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_u10(SIZE(var%u10,dim=1))) 
          ALLOCATE (buf2_u10(SIZE(var%u10,dim=1))) 
          n_u10 = COUNT(var%u10 /= kgenref_var%u10) 
          WHERE ( ABS(kgenref_var%u10) > kgen_minvalue ) 
              buf1_u10 = ((var%u10-kgenref_var%u10)/kgenref_var%u10)**2 
              buf2_u10 = (var%u10-kgenref_var%u10)**2 
          ELSEWHERE 
              buf1_u10 = (var%u10-kgenref_var%u10)**2 
              buf2_u10 = buf1_u10 
          END WHERE   
          nrmsdiff_u10 = SQRT(SUM(buf1_u10)/REAL(n_u10)) 
          rmsdiff_u10 = SQRT(SUM(buf2_u10)/REAL(n_u10)) 
          IF (rmsdiff_u10 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%u10 is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%u10 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%u10 /= kgenref_var%u10), " of ", size( var%u10 ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%u10)/real(size(var%u10)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%u10)/real(size(kgenref_var%u10)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_u10 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u10 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%u10 /= kgenref_var%u10), " of ", size( var%u10 ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%u10)/real(size(var%u10)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%u10)/real(size(kgenref_var%u10)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_u10 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u10 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ts == kgenref_var%ts)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ts is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ts(SIZE(var%ts,dim=1))) 
          ALLOCATE (buf2_ts(SIZE(var%ts,dim=1))) 
          n_ts = COUNT(var%ts /= kgenref_var%ts) 
          WHERE ( ABS(kgenref_var%ts) > kgen_minvalue ) 
              buf1_ts = ((var%ts-kgenref_var%ts)/kgenref_var%ts)**2 
              buf2_ts = (var%ts-kgenref_var%ts)**2 
          ELSEWHERE 
              buf1_ts = (var%ts-kgenref_var%ts)**2 
              buf2_ts = buf1_ts 
          END WHERE   
          nrmsdiff_ts = SQRT(SUM(buf1_ts)/REAL(n_ts)) 
          rmsdiff_ts = SQRT(SUM(buf2_ts)/REAL(n_ts)) 
          IF (rmsdiff_ts > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ts is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ts is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ts /= kgenref_var%ts), " of ", size( var%ts ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ts)/real(size(var%ts)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ts)/real(size(kgenref_var%ts)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ts 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ts 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ts /= kgenref_var%ts), " of ", size( var%ts ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ts)/real(size(var%ts)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ts)/real(size(kgenref_var%ts)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ts 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ts 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%sst == kgenref_var%sst)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sst is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_sst(SIZE(var%sst,dim=1))) 
          ALLOCATE (buf2_sst(SIZE(var%sst,dim=1))) 
          n_sst = COUNT(var%sst /= kgenref_var%sst) 
          WHERE ( ABS(kgenref_var%sst) > kgen_minvalue ) 
              buf1_sst = ((var%sst-kgenref_var%sst)/kgenref_var%sst)**2 
              buf2_sst = (var%sst-kgenref_var%sst)**2 
          ELSEWHERE 
              buf1_sst = (var%sst-kgenref_var%sst)**2 
              buf2_sst = buf1_sst 
          END WHERE   
          nrmsdiff_sst = SQRT(SUM(buf1_sst)/REAL(n_sst)) 
          rmsdiff_sst = SQRT(SUM(buf2_sst)/REAL(n_sst)) 
          IF (rmsdiff_sst > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sst is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sst is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%sst /= kgenref_var%sst), " of ", size( var%sst ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%sst)/real(size(var%sst)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%sst)/real(size(kgenref_var%sst)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_sst 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sst 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%sst /= kgenref_var%sst), " of ", size( var%sst ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%sst)/real(size(var%sst)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%sst)/real(size(kgenref_var%sst)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_sst 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sst 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%snowhland == kgenref_var%snowhland)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%snowhland is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_snowhland(SIZE(var%snowhland,dim=1))) 
          ALLOCATE (buf2_snowhland(SIZE(var%snowhland,dim=1))) 
          n_snowhland = COUNT(var%snowhland /= kgenref_var%snowhland) 
          WHERE ( ABS(kgenref_var%snowhland) > kgen_minvalue ) 
              buf1_snowhland = ((var%snowhland-kgenref_var%snowhland)/kgenref_var%snowhland)**2 
              buf2_snowhland = (var%snowhland-kgenref_var%snowhland)**2 
          ELSEWHERE 
              buf1_snowhland = (var%snowhland-kgenref_var%snowhland)**2 
              buf2_snowhland = buf1_snowhland 
          END WHERE   
          nrmsdiff_snowhland = SQRT(SUM(buf1_snowhland)/REAL(n_snowhland)) 
          rmsdiff_snowhland = SQRT(SUM(buf2_snowhland)/REAL(n_snowhland)) 
          IF (rmsdiff_snowhland > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%snowhland is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%snowhland is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%snowhland /= kgenref_var%snowhland), " of ", size( var%snowhland ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%snowhland)/real(size(var%snowhland)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%snowhland)/real(size(kgenref_var%snowhland)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_snowhland 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_snowhland 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%snowhland /= kgenref_var%snowhland), " of ", size( var%snowhland ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%snowhland)/real(size(var%snowhland)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%snowhland)/real(size(kgenref_var%snowhland)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_snowhland 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_snowhland 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%snowhice == kgenref_var%snowhice)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%snowhice is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_snowhice(SIZE(var%snowhice,dim=1))) 
          ALLOCATE (buf2_snowhice(SIZE(var%snowhice,dim=1))) 
          n_snowhice = COUNT(var%snowhice /= kgenref_var%snowhice) 
          WHERE ( ABS(kgenref_var%snowhice) > kgen_minvalue ) 
              buf1_snowhice = ((var%snowhice-kgenref_var%snowhice)/kgenref_var%snowhice)**2 
              buf2_snowhice = (var%snowhice-kgenref_var%snowhice)**2 
          ELSEWHERE 
              buf1_snowhice = (var%snowhice-kgenref_var%snowhice)**2 
              buf2_snowhice = buf1_snowhice 
          END WHERE   
          nrmsdiff_snowhice = SQRT(SUM(buf1_snowhice)/REAL(n_snowhice)) 
          rmsdiff_snowhice = SQRT(SUM(buf2_snowhice)/REAL(n_snowhice)) 
          IF (rmsdiff_snowhice > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%snowhice is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%snowhice is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%snowhice /= kgenref_var%snowhice), " of ", size( var%snowhice ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%snowhice)/real(size(var%snowhice)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%snowhice)/real(size(kgenref_var%snowhice)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_snowhice 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_snowhice 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%snowhice /= kgenref_var%snowhice), " of ", size( var%snowhice ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%snowhice)/real(size(var%snowhice)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%snowhice)/real(size(kgenref_var%snowhice)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_snowhice 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_snowhice 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%fco2_lnd == kgenref_var%fco2_lnd)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fco2_lnd is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_fco2_lnd(SIZE(var%fco2_lnd,dim=1))) 
          ALLOCATE (buf2_fco2_lnd(SIZE(var%fco2_lnd,dim=1))) 
          n_fco2_lnd = COUNT(var%fco2_lnd /= kgenref_var%fco2_lnd) 
          WHERE ( ABS(kgenref_var%fco2_lnd) > kgen_minvalue ) 
              buf1_fco2_lnd = ((var%fco2_lnd-kgenref_var%fco2_lnd)/kgenref_var%fco2_lnd)**2 
              buf2_fco2_lnd = (var%fco2_lnd-kgenref_var%fco2_lnd)**2 
          ELSEWHERE 
              buf1_fco2_lnd = (var%fco2_lnd-kgenref_var%fco2_lnd)**2 
              buf2_fco2_lnd = buf1_fco2_lnd 
          END WHERE   
          nrmsdiff_fco2_lnd = SQRT(SUM(buf1_fco2_lnd)/REAL(n_fco2_lnd)) 
          rmsdiff_fco2_lnd = SQRT(SUM(buf2_fco2_lnd)/REAL(n_fco2_lnd)) 
          IF (rmsdiff_fco2_lnd > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fco2_lnd is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fco2_lnd is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%fco2_lnd /= kgenref_var%fco2_lnd), " of ", size( var%fco2_lnd ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fco2_lnd)/real(size(var%fco2_lnd)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fco2_lnd)/real(size(kgenref_var%fco2_lnd)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fco2_lnd 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fco2_lnd 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%fco2_lnd /= kgenref_var%fco2_lnd), " of ", size( var%fco2_lnd ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fco2_lnd)/real(size(var%fco2_lnd)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fco2_lnd)/real(size(kgenref_var%fco2_lnd)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fco2_lnd 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fco2_lnd 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%fco2_ocn == kgenref_var%fco2_ocn)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fco2_ocn is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_fco2_ocn(SIZE(var%fco2_ocn,dim=1))) 
          ALLOCATE (buf2_fco2_ocn(SIZE(var%fco2_ocn,dim=1))) 
          n_fco2_ocn = COUNT(var%fco2_ocn /= kgenref_var%fco2_ocn) 
          WHERE ( ABS(kgenref_var%fco2_ocn) > kgen_minvalue ) 
              buf1_fco2_ocn = ((var%fco2_ocn-kgenref_var%fco2_ocn)/kgenref_var%fco2_ocn)**2 
              buf2_fco2_ocn = (var%fco2_ocn-kgenref_var%fco2_ocn)**2 
          ELSEWHERE 
              buf1_fco2_ocn = (var%fco2_ocn-kgenref_var%fco2_ocn)**2 
              buf2_fco2_ocn = buf1_fco2_ocn 
          END WHERE   
          nrmsdiff_fco2_ocn = SQRT(SUM(buf1_fco2_ocn)/REAL(n_fco2_ocn)) 
          rmsdiff_fco2_ocn = SQRT(SUM(buf2_fco2_ocn)/REAL(n_fco2_ocn)) 
          IF (rmsdiff_fco2_ocn > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fco2_ocn is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fco2_ocn is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%fco2_ocn /= kgenref_var%fco2_ocn), " of ", size( var%fco2_ocn ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fco2_ocn)/real(size(var%fco2_ocn)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fco2_ocn)/real(size(kgenref_var%fco2_ocn)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fco2_ocn 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fco2_ocn 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%fco2_ocn /= kgenref_var%fco2_ocn), " of ", size( var%fco2_ocn ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fco2_ocn)/real(size(var%fco2_ocn)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fco2_ocn)/real(size(kgenref_var%fco2_ocn)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fco2_ocn 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fco2_ocn 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%fdms == kgenref_var%fdms)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fdms is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_fdms(SIZE(var%fdms,dim=1))) 
          ALLOCATE (buf2_fdms(SIZE(var%fdms,dim=1))) 
          n_fdms = COUNT(var%fdms /= kgenref_var%fdms) 
          WHERE ( ABS(kgenref_var%fdms) > kgen_minvalue ) 
              buf1_fdms = ((var%fdms-kgenref_var%fdms)/kgenref_var%fdms)**2 
              buf2_fdms = (var%fdms-kgenref_var%fdms)**2 
          ELSEWHERE 
              buf1_fdms = (var%fdms-kgenref_var%fdms)**2 
              buf2_fdms = buf1_fdms 
          END WHERE   
          nrmsdiff_fdms = SQRT(SUM(buf1_fdms)/REAL(n_fdms)) 
          rmsdiff_fdms = SQRT(SUM(buf2_fdms)/REAL(n_fdms)) 
          IF (rmsdiff_fdms > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fdms is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fdms is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%fdms /= kgenref_var%fdms), " of ", size( var%fdms ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fdms)/real(size(var%fdms)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fdms)/real(size(kgenref_var%fdms)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fdms 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fdms 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%fdms /= kgenref_var%fdms), " of ", size( var%fdms ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fdms)/real(size(var%fdms)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fdms)/real(size(kgenref_var%fdms)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fdms 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fdms 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%landfrac == kgenref_var%landfrac)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%landfrac is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_landfrac(SIZE(var%landfrac,dim=1))) 
          ALLOCATE (buf2_landfrac(SIZE(var%landfrac,dim=1))) 
          n_landfrac = COUNT(var%landfrac /= kgenref_var%landfrac) 
          WHERE ( ABS(kgenref_var%landfrac) > kgen_minvalue ) 
              buf1_landfrac = ((var%landfrac-kgenref_var%landfrac)/kgenref_var%landfrac)**2 
              buf2_landfrac = (var%landfrac-kgenref_var%landfrac)**2 
          ELSEWHERE 
              buf1_landfrac = (var%landfrac-kgenref_var%landfrac)**2 
              buf2_landfrac = buf1_landfrac 
          END WHERE   
          nrmsdiff_landfrac = SQRT(SUM(buf1_landfrac)/REAL(n_landfrac)) 
          rmsdiff_landfrac = SQRT(SUM(buf2_landfrac)/REAL(n_landfrac)) 
          IF (rmsdiff_landfrac > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%landfrac is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%landfrac is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%landfrac /= kgenref_var%landfrac), " of ", size( var%landfrac ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%landfrac)/real(size(var%landfrac)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%landfrac)/real(size(kgenref_var%landfrac)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_landfrac 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_landfrac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%landfrac /= kgenref_var%landfrac), " of ", size( var%landfrac ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%landfrac)/real(size(var%landfrac)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%landfrac)/real(size(kgenref_var%landfrac)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_landfrac 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_landfrac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%icefrac == kgenref_var%icefrac)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%icefrac is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_icefrac(SIZE(var%icefrac,dim=1))) 
          ALLOCATE (buf2_icefrac(SIZE(var%icefrac,dim=1))) 
          n_icefrac = COUNT(var%icefrac /= kgenref_var%icefrac) 
          WHERE ( ABS(kgenref_var%icefrac) > kgen_minvalue ) 
              buf1_icefrac = ((var%icefrac-kgenref_var%icefrac)/kgenref_var%icefrac)**2 
              buf2_icefrac = (var%icefrac-kgenref_var%icefrac)**2 
          ELSEWHERE 
              buf1_icefrac = (var%icefrac-kgenref_var%icefrac)**2 
              buf2_icefrac = buf1_icefrac 
          END WHERE   
          nrmsdiff_icefrac = SQRT(SUM(buf1_icefrac)/REAL(n_icefrac)) 
          rmsdiff_icefrac = SQRT(SUM(buf2_icefrac)/REAL(n_icefrac)) 
          IF (rmsdiff_icefrac > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%icefrac is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%icefrac is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%icefrac /= kgenref_var%icefrac), " of ", size( var%icefrac ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%icefrac)/real(size(var%icefrac)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%icefrac)/real(size(kgenref_var%icefrac)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_icefrac 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_icefrac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%icefrac /= kgenref_var%icefrac), " of ", size( var%icefrac ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%icefrac)/real(size(var%icefrac)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%icefrac)/real(size(kgenref_var%icefrac)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_icefrac 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_icefrac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ocnfrac == kgenref_var%ocnfrac)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ocnfrac is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ocnfrac(SIZE(var%ocnfrac,dim=1))) 
          ALLOCATE (buf2_ocnfrac(SIZE(var%ocnfrac,dim=1))) 
          n_ocnfrac = COUNT(var%ocnfrac /= kgenref_var%ocnfrac) 
          WHERE ( ABS(kgenref_var%ocnfrac) > kgen_minvalue ) 
              buf1_ocnfrac = ((var%ocnfrac-kgenref_var%ocnfrac)/kgenref_var%ocnfrac)**2 
              buf2_ocnfrac = (var%ocnfrac-kgenref_var%ocnfrac)**2 
          ELSEWHERE 
              buf1_ocnfrac = (var%ocnfrac-kgenref_var%ocnfrac)**2 
              buf2_ocnfrac = buf1_ocnfrac 
          END WHERE   
          nrmsdiff_ocnfrac = SQRT(SUM(buf1_ocnfrac)/REAL(n_ocnfrac)) 
          rmsdiff_ocnfrac = SQRT(SUM(buf2_ocnfrac)/REAL(n_ocnfrac)) 
          IF (rmsdiff_ocnfrac > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ocnfrac is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ocnfrac is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ocnfrac /= kgenref_var%ocnfrac), " of ", size( var%ocnfrac ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ocnfrac)/real(size(var%ocnfrac)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ocnfrac)/real(size(kgenref_var%ocnfrac)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ocnfrac 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ocnfrac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ocnfrac /= kgenref_var%ocnfrac), " of ", size( var%ocnfrac ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ocnfrac)/real(size(var%ocnfrac)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ocnfrac)/real(size(kgenref_var%ocnfrac)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ocnfrac 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ocnfrac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%ram1)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%ram1 == kgenref_var%ram1)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ram1 is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_ram1(SIZE(var%ram1,dim=1))) 
              ALLOCATE (buf2_ram1(SIZE(var%ram1,dim=1))) 
              n_ram1 = COUNT(var%ram1 /= kgenref_var%ram1) 
              WHERE ( ABS(kgenref_var%ram1) > kgen_minvalue ) 
                  buf1_ram1 = ((var%ram1-kgenref_var%ram1)/kgenref_var%ram1)**2 
                  buf2_ram1 = (var%ram1-kgenref_var%ram1)**2 
              ELSEWHERE 
                  buf1_ram1 = (var%ram1-kgenref_var%ram1)**2 
                  buf2_ram1 = buf1_ram1 
              END WHERE   
              nrmsdiff_ram1 = SQRT(SUM(buf1_ram1)/REAL(n_ram1)) 
              rmsdiff_ram1 = SQRT(SUM(buf2_ram1)/REAL(n_ram1)) 
              IF (rmsdiff_ram1 > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ram1 is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ram1 is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%ram1 /= kgenref_var%ram1), " of ", size( var%ram1 ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ram1)/real(size(var%ram1)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ram1)/real(size(kgenref_var%ram1)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ram1 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ram1 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%ram1 /= kgenref_var%ram1), " of ", size( var%ram1 ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ram1)/real(size(var%ram1)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ram1)/real(size(kgenref_var%ram1)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ram1 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ram1 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%fv)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%fv == kgenref_var%fv)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fv is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_fv(SIZE(var%fv,dim=1))) 
              ALLOCATE (buf2_fv(SIZE(var%fv,dim=1))) 
              n_fv = COUNT(var%fv /= kgenref_var%fv) 
              WHERE ( ABS(kgenref_var%fv) > kgen_minvalue ) 
                  buf1_fv = ((var%fv-kgenref_var%fv)/kgenref_var%fv)**2 
                  buf2_fv = (var%fv-kgenref_var%fv)**2 
              ELSEWHERE 
                  buf1_fv = (var%fv-kgenref_var%fv)**2 
                  buf2_fv = buf1_fv 
              END WHERE   
              nrmsdiff_fv = SQRT(SUM(buf1_fv)/REAL(n_fv)) 
              rmsdiff_fv = SQRT(SUM(buf2_fv)/REAL(n_fv)) 
              IF (rmsdiff_fv > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%fv is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%fv is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%fv /= kgenref_var%fv), " of ", size( var%fv ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%fv)/real(size(var%fv)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%fv)/real(size(kgenref_var%fv)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_fv 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fv 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%fv /= kgenref_var%fv), " of ", size( var%fv ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%fv)/real(size(var%fv)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%fv)/real(size(kgenref_var%fv)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_fv 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fv 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%soilw)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%soilw == kgenref_var%soilw)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%soilw is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_soilw(SIZE(var%soilw,dim=1))) 
              ALLOCATE (buf2_soilw(SIZE(var%soilw,dim=1))) 
              n_soilw = COUNT(var%soilw /= kgenref_var%soilw) 
              WHERE ( ABS(kgenref_var%soilw) > kgen_minvalue ) 
                  buf1_soilw = ((var%soilw-kgenref_var%soilw)/kgenref_var%soilw)**2 
                  buf2_soilw = (var%soilw-kgenref_var%soilw)**2 
              ELSEWHERE 
                  buf1_soilw = (var%soilw-kgenref_var%soilw)**2 
                  buf2_soilw = buf1_soilw 
              END WHERE   
              nrmsdiff_soilw = SQRT(SUM(buf1_soilw)/REAL(n_soilw)) 
              rmsdiff_soilw = SQRT(SUM(buf2_soilw)/REAL(n_soilw)) 
              IF (rmsdiff_soilw > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%soilw is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%soilw is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%soilw /= kgenref_var%soilw), " of ", size( var%soilw ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%soilw)/real(size(var%soilw)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%soilw)/real(size(kgenref_var%soilw)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_soilw 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_soilw 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%soilw /= kgenref_var%soilw), " of ", size( var%soilw ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%soilw)/real(size(var%soilw)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%soilw)/real(size(kgenref_var%soilw)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_soilw 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_soilw 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%cflx == kgenref_var%cflx)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cflx is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_cflx(SIZE(var%cflx,dim=1),SIZE(var%cflx,dim=2))) 
          ALLOCATE (buf2_cflx(SIZE(var%cflx,dim=1),SIZE(var%cflx,dim=2))) 
          n_cflx = COUNT(var%cflx /= kgenref_var%cflx) 
          WHERE ( ABS(kgenref_var%cflx) > kgen_minvalue ) 
              buf1_cflx = ((var%cflx-kgenref_var%cflx)/kgenref_var%cflx)**2 
              buf2_cflx = (var%cflx-kgenref_var%cflx)**2 
          ELSEWHERE 
              buf1_cflx = (var%cflx-kgenref_var%cflx)**2 
              buf2_cflx = buf1_cflx 
          END WHERE   
          nrmsdiff_cflx = SQRT(SUM(buf1_cflx)/REAL(n_cflx)) 
          rmsdiff_cflx = SQRT(SUM(buf2_cflx)/REAL(n_cflx)) 
          IF (rmsdiff_cflx > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cflx is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cflx is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%cflx /= kgenref_var%cflx), " of ", size( var%cflx ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%cflx)/real(size(var%cflx)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%cflx)/real(size(kgenref_var%cflx)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_cflx 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cflx 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%cflx /= kgenref_var%cflx), " of ", size( var%cflx ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%cflx)/real(size(var%cflx)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%cflx)/real(size(kgenref_var%cflx)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_cflx 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cflx 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ustar == kgenref_var%ustar)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ustar is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ustar(SIZE(var%ustar,dim=1))) 
          ALLOCATE (buf2_ustar(SIZE(var%ustar,dim=1))) 
          n_ustar = COUNT(var%ustar /= kgenref_var%ustar) 
          WHERE ( ABS(kgenref_var%ustar) > kgen_minvalue ) 
              buf1_ustar = ((var%ustar-kgenref_var%ustar)/kgenref_var%ustar)**2 
              buf2_ustar = (var%ustar-kgenref_var%ustar)**2 
          ELSEWHERE 
              buf1_ustar = (var%ustar-kgenref_var%ustar)**2 
              buf2_ustar = buf1_ustar 
          END WHERE   
          nrmsdiff_ustar = SQRT(SUM(buf1_ustar)/REAL(n_ustar)) 
          rmsdiff_ustar = SQRT(SUM(buf2_ustar)/REAL(n_ustar)) 
          IF (rmsdiff_ustar > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ustar is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ustar is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ustar /= kgenref_var%ustar), " of ", size( var%ustar ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ustar)/real(size(var%ustar)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ustar)/real(size(kgenref_var%ustar)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ustar 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ustar 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ustar /= kgenref_var%ustar), " of ", size( var%ustar ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ustar)/real(size(var%ustar)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ustar)/real(size(kgenref_var%ustar)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ustar 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ustar 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%re == kgenref_var%re)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%re is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_re(SIZE(var%re,dim=1))) 
          ALLOCATE (buf2_re(SIZE(var%re,dim=1))) 
          n_re = COUNT(var%re /= kgenref_var%re) 
          WHERE ( ABS(kgenref_var%re) > kgen_minvalue ) 
              buf1_re = ((var%re-kgenref_var%re)/kgenref_var%re)**2 
              buf2_re = (var%re-kgenref_var%re)**2 
          ELSEWHERE 
              buf1_re = (var%re-kgenref_var%re)**2 
              buf2_re = buf1_re 
          END WHERE   
          nrmsdiff_re = SQRT(SUM(buf1_re)/REAL(n_re)) 
          rmsdiff_re = SQRT(SUM(buf2_re)/REAL(n_re)) 
          IF (rmsdiff_re > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%re is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%re is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%re /= kgenref_var%re), " of ", size( var%re ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%re)/real(size(var%re)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%re)/real(size(kgenref_var%re)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_re 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_re 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%re /= kgenref_var%re), " of ", size( var%re ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%re)/real(size(var%re)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%re)/real(size(kgenref_var%re)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_re 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_re 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ssq == kgenref_var%ssq)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ssq is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ssq(SIZE(var%ssq,dim=1))) 
          ALLOCATE (buf2_ssq(SIZE(var%ssq,dim=1))) 
          n_ssq = COUNT(var%ssq /= kgenref_var%ssq) 
          WHERE ( ABS(kgenref_var%ssq) > kgen_minvalue ) 
              buf1_ssq = ((var%ssq-kgenref_var%ssq)/kgenref_var%ssq)**2 
              buf2_ssq = (var%ssq-kgenref_var%ssq)**2 
          ELSEWHERE 
              buf1_ssq = (var%ssq-kgenref_var%ssq)**2 
              buf2_ssq = buf1_ssq 
          END WHERE   
          nrmsdiff_ssq = SQRT(SUM(buf1_ssq)/REAL(n_ssq)) 
          rmsdiff_ssq = SQRT(SUM(buf2_ssq)/REAL(n_ssq)) 
          IF (rmsdiff_ssq > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ssq is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ssq is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ssq /= kgenref_var%ssq), " of ", size( var%ssq ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ssq)/real(size(var%ssq)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ssq)/real(size(kgenref_var%ssq)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ssq 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ssq 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) count( var%ssq /= kgenref_var%ssq), " of ", size( var%ssq ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ssq)/real(size(var%ssq)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ssq)/real(size(kgenref_var%ssq)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ssq 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ssq 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%depvel)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%depvel == kgenref_var%depvel)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%depvel is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_depvel(SIZE(var%depvel,dim=1),SIZE(var%depvel,dim=2))) 
              ALLOCATE (buf2_depvel(SIZE(var%depvel,dim=1),SIZE(var%depvel,dim=2))) 
              n_depvel = COUNT(var%depvel /= kgenref_var%depvel) 
              WHERE ( ABS(kgenref_var%depvel) > kgen_minvalue ) 
                  buf1_depvel = ((var%depvel-kgenref_var%depvel)/kgenref_var%depvel)**2 
                  buf2_depvel = (var%depvel-kgenref_var%depvel)**2 
              ELSEWHERE 
                  buf1_depvel = (var%depvel-kgenref_var%depvel)**2 
                  buf2_depvel = buf1_depvel 
              END WHERE   
              nrmsdiff_depvel = SQRT(SUM(buf1_depvel)/REAL(n_depvel)) 
              rmsdiff_depvel = SQRT(SUM(buf2_depvel)/REAL(n_depvel)) 
              IF (rmsdiff_depvel > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%depvel is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%depvel is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%depvel /= kgenref_var%depvel), " of ", size( var%depvel ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%depvel)/real(size(var%depvel)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%depvel)/real(size(kgenref_var%depvel)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_depvel 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_depvel 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%depvel /= kgenref_var%depvel), " of ", size( var%depvel ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%depvel)/real(size(var%depvel)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%depvel)/real(size(kgenref_var%depvel)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_depvel 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_depvel 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%dstflx)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dstflx == kgenref_var%dstflx)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dstflx is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dstflx(SIZE(var%dstflx,dim=1),SIZE(var%dstflx,dim=2))) 
              ALLOCATE (buf2_dstflx(SIZE(var%dstflx,dim=1),SIZE(var%dstflx,dim=2))) 
              n_dstflx = COUNT(var%dstflx /= kgenref_var%dstflx) 
              WHERE ( ABS(kgenref_var%dstflx) > kgen_minvalue ) 
                  buf1_dstflx = ((var%dstflx-kgenref_var%dstflx)/kgenref_var%dstflx)**2 
                  buf2_dstflx = (var%dstflx-kgenref_var%dstflx)**2 
              ELSEWHERE 
                  buf1_dstflx = (var%dstflx-kgenref_var%dstflx)**2 
                  buf2_dstflx = buf1_dstflx 
              END WHERE   
              nrmsdiff_dstflx = SQRT(SUM(buf1_dstflx)/REAL(n_dstflx)) 
              rmsdiff_dstflx = SQRT(SUM(buf2_dstflx)/REAL(n_dstflx)) 
              IF (rmsdiff_dstflx > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dstflx is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dstflx is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dstflx /= kgenref_var%dstflx), " of ", size( var%dstflx ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dstflx)/real(size(var%dstflx)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dstflx)/real(size(kgenref_var%dstflx)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dstflx 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dstflx 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dstflx /= kgenref_var%dstflx), " of ", size( var%dstflx ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dstflx)/real(size(var%dstflx)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dstflx)/real(size(kgenref_var%dstflx)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dstflx 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dstflx 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%meganflx)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%meganflx == kgenref_var%meganflx)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%meganflx is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_meganflx(SIZE(var%meganflx,dim=1),SIZE(var%meganflx,dim=2))) 
              ALLOCATE (buf2_meganflx(SIZE(var%meganflx,dim=1),SIZE(var%meganflx,dim=2))) 
              n_meganflx = COUNT(var%meganflx /= kgenref_var%meganflx) 
              WHERE ( ABS(kgenref_var%meganflx) > kgen_minvalue ) 
                  buf1_meganflx = ((var%meganflx-kgenref_var%meganflx)/kgenref_var%meganflx)**2 
                  buf2_meganflx = (var%meganflx-kgenref_var%meganflx)**2 
              ELSEWHERE 
                  buf1_meganflx = (var%meganflx-kgenref_var%meganflx)**2 
                  buf2_meganflx = buf1_meganflx 
              END WHERE   
              nrmsdiff_meganflx = SQRT(SUM(buf1_meganflx)/REAL(n_meganflx)) 
              rmsdiff_meganflx = SQRT(SUM(buf2_meganflx)/REAL(n_meganflx)) 
              IF (rmsdiff_meganflx > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%meganflx is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%meganflx is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%meganflx /= kgenref_var%meganflx), " of ", size( var%meganflx ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%meganflx)/real(size(var%meganflx)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%meganflx)/real(size(kgenref_var%meganflx)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_meganflx 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_meganflx 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%meganflx /= kgenref_var%meganflx), " of ", size( var%meganflx ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%meganflx)/real(size(var%meganflx)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%meganflx)/real(size(kgenref_var%meganflx)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_meganflx 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_meganflx 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%fireflx)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%fireflx == kgenref_var%fireflx)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fireflx is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_fireflx(SIZE(var%fireflx,dim=1),SIZE(var%fireflx,dim=2))) 
              ALLOCATE (buf2_fireflx(SIZE(var%fireflx,dim=1),SIZE(var%fireflx,dim=2))) 
              n_fireflx = COUNT(var%fireflx /= kgenref_var%fireflx) 
              WHERE ( ABS(kgenref_var%fireflx) > kgen_minvalue ) 
                  buf1_fireflx = ((var%fireflx-kgenref_var%fireflx)/kgenref_var%fireflx)**2 
                  buf2_fireflx = (var%fireflx-kgenref_var%fireflx)**2 
              ELSEWHERE 
                  buf1_fireflx = (var%fireflx-kgenref_var%fireflx)**2 
                  buf2_fireflx = buf1_fireflx 
              END WHERE   
              nrmsdiff_fireflx = SQRT(SUM(buf1_fireflx)/REAL(n_fireflx)) 
              rmsdiff_fireflx = SQRT(SUM(buf2_fireflx)/REAL(n_fireflx)) 
              IF (rmsdiff_fireflx > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%fireflx is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%fireflx is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%fireflx /= kgenref_var%fireflx), " of ", size( var%fireflx ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%fireflx)/real(size(var%fireflx)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%fireflx)/real(size(kgenref_var%fireflx)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_fireflx 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fireflx 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%fireflx /= kgenref_var%fireflx), " of ", size( var%fireflx ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%fireflx)/real(size(var%fireflx)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%fireflx)/real(size(kgenref_var%fireflx)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_fireflx 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fireflx 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ASSOCIATED(var%fireztop)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%fireztop == kgenref_var%fireztop)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fireztop is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_fireztop(SIZE(var%fireztop,dim=1))) 
              ALLOCATE (buf2_fireztop(SIZE(var%fireztop,dim=1))) 
              n_fireztop = COUNT(var%fireztop /= kgenref_var%fireztop) 
              WHERE ( ABS(kgenref_var%fireztop) > kgen_minvalue ) 
                  buf1_fireztop = ((var%fireztop-kgenref_var%fireztop)/kgenref_var%fireztop)**2 
                  buf2_fireztop = (var%fireztop-kgenref_var%fireztop)**2 
              ELSEWHERE 
                  buf1_fireztop = (var%fireztop-kgenref_var%fireztop)**2 
                  buf2_fireztop = buf1_fireztop 
              END WHERE   
              nrmsdiff_fireztop = SQRT(SUM(buf1_fireztop)/REAL(n_fireztop)) 
              rmsdiff_fireztop = SQRT(SUM(buf2_fireztop)/REAL(n_fireztop)) 
              IF (rmsdiff_fireztop > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%fireztop is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%fireztop is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%fireztop /= kgenref_var%fireztop), " of ", size( var%fireztop ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%fireztop)/real(size(var%fireztop)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%fireztop)/real(size(kgenref_var%fireztop)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_fireztop 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fireztop 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%fireztop /= kgenref_var%fireztop), " of ", size( var%fireztop ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%fireztop)/real(size(var%fireztop)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%fireztop)/real(size(kgenref_var%fireztop)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_fireztop 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fireztop 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_camsrfexch_cam_in_t 
    
end module camsrfexch