!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:34 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
!physics data types module
!-------------------------------------------------------------------------------


module physics_types

    USE shr_kind_mod, ONLY: r8 => shr_kind_r8 
    USE constituents, ONLY: pcnst 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 

! Public types:


    PUBLIC physics_state 
    PUBLIC physics_ptend 
! Public interfaces


                             ! cannot be applied to eul or sld dycores


!-------------------------------------------------------------------------------

  type physics_state
     integer                                     :: &
          lchnk,                &! chunk index
          ngrdcol,              &! -- Grid        -- number of active columns (on the grid)
          psetcols=0,           &! --             -- max number of columns set - if subcols = pcols*psubcols, else = pcols
          ncol=0                 ! --             -- sum of nsubcol for all ngrdcols - number of active columns
     real(r8), dimension(:), allocatable         :: &
          lat,     &! latitude (radians)
          lon,     &! longitude (radians)
          ps,      &! surface pressure
          psdry,   &! dry surface pressure
          phis,    &! surface geopotential
          ulat,    &! unique latitudes  (radians)
          ulon      ! unique longitudes (radians)
     real(r8), dimension(:,:),allocatable        :: &
          t,       &! temperature (K)
          u,       &! zonal wind (m/s)
          v,       &! meridional wind (m/s)
          s,       &! dry static energy
          omega,   &! vertical pressure velocity (Pa/s)
          pmid,    &! midpoint pressure (Pa)
          pmiddry, &! midpoint pressure dry (Pa)
          pdel,    &! layer thickness (Pa)
          pdeldry, &! layer thickness dry (Pa)
          rpdel,   &! reciprocal of layer thickness (Pa)
          rpdeldry,&! recipricol layer thickness dry (Pa)
          lnpmid,  &! ln(pmid)
          lnpmiddry,&! log midpoint pressure dry (Pa)
          exner,   &! inverse exner function w.r.t. surface pressure (ps/p)^(R/cp)
          zm        ! geopotential height above surface at midpoints (m)

     real(r8), dimension(:,:,:),allocatable      :: &
          q         ! constituent mixing ratio (kg/kg moist or dry air depending on type)

     real(r8), dimension(:,:),allocatable        :: &
          pint,    &! interface pressure (Pa)
          pintdry, &! interface pressure dry (Pa)
          lnpint,  &! ln(pint)
          lnpintdry,&! log interface pressure dry (Pa)
          zi        ! geopotential height above surface at interfaces (m)

     real(r8), dimension(:),allocatable          :: &
          te_ini,  &! vertically integrated total (kinetic + static) energy of initial state
          te_cur,  &! vertically integrated total (kinetic + static) energy of current state
          tw_ini,  &! vertically integrated total water of initial state
          tw_cur    ! vertically integrated total water of new state
     integer :: count ! count of values with significant energy or water imbalances
     integer, dimension(:),allocatable           :: &
          latmapback, &! map from column to unique lat for that column
          lonmapback, &! map from column to unique lon for that column
          cid        ! unique column id
     integer :: ulatcnt, &! number of unique lats in chunk
                uloncnt   ! number of unique lons in chunk

  end type physics_state
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
! This is for tendencies returned from individual parameterizations

  type physics_ptend

     integer   ::   psetcols=0 ! max number of columns set- if subcols = pcols*psubcols, else = pcols

     character*24 :: name    ! name of parameterization which produced tendencies.

     logical ::             &
          ls = .false.,               &! true if dsdt is returned
          lu = .false.,               &! true if dudt is returned
          lv = .false.                 ! true if dvdt is returned

     logical,dimension(pcnst) ::  lq = .false.  ! true if dqdt() is returned

     integer ::             &
          top_level,        &! top level index for which nonzero tendencies have been set
          bot_level          ! bottom level index for which nonzero tendencies have been set

     real(r8), dimension(:,:),allocatable   :: &
          s,                &! heating rate (J/kg/s)
          u,                &! u momentum tendency (m/s/s)
          v                  ! v momentum tendency (m/s/s)
     real(r8), dimension(:,:,:),allocatable :: &
          q                  ! consituent tendencies (kg/kg/s)
! boundary fluxes

     real(r8), dimension(:),allocatable     ::&
          hflux_srf,     &! net heat flux at surface (W/m2)
          hflux_top,     &! net heat flux at top of model (W/m2)
          taux_srf,      &! net zonal stress at surface (Pa)
          taux_top,      &! net zonal stress at top of model (Pa)
          tauy_srf,      &! net meridional stress at surface (Pa)
          tauy_top        ! net meridional stress at top of model (Pa)
     real(r8), dimension(:,:),allocatable   ::&
          cflx_srf,      &! constituent flux at surface (kg/m2/s)
          cflx_top        ! constituent flux top of model (kg/m2/s)

  end type physics_ptend
!===============================================================================
  PUBLIC kr_physics_types_physics_state 
  PUBLIC kr_physics_types_physics_ptend 
  PUBLIC kv_physics_types_physics_state 
  PUBLIC kv_physics_types_physics_ptend 


!===============================================================================
    
  CONTAINS 
    


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!-----------------------------------------------------------------------
!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


!===============================================================================


  !read state subroutine for kr_physics_types_physics_state 
  RECURSIVE SUBROUTINE kr_physics_types_physics_state(var, kgen_unit, printname, printvar) 
      TYPE(physics_state), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%lchnk 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%lchnk = ", var%lchnk 
      END IF   
      READ (UNIT = kgen_unit) var%ngrdcol 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ngrdcol = ", var%ngrdcol 
      END IF   
      READ (UNIT = kgen_unit) var%psetcols 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%psetcols = ", var%psetcols 
      END IF   
      READ (UNIT = kgen_unit) var%ncol 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ncol = ", var%ncol 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%lat, kgen_unit, printname // "%lat", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%lat, kgen_unit, printname // "%lat", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%lon, kgen_unit, printname // "%lon", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%lon, kgen_unit, printname // "%lon", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%ps, kgen_unit, printname // "%ps", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%ps, kgen_unit, printname // "%ps", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%psdry, kgen_unit, printname // "%psdry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%psdry, kgen_unit, printname // "%psdry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%phis, kgen_unit, printname // "%phis", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%phis, kgen_unit, printname // "%phis", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%ulat, kgen_unit, printname // "%ulat", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%ulat, kgen_unit, printname // "%ulat", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%ulon, kgen_unit, printname // "%ulon", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%ulon, kgen_unit, printname // "%ulon", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%t, kgen_unit, printname // "%t", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%t, kgen_unit, printname // "%t", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%u, kgen_unit, printname // "%u", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%u, kgen_unit, printname // "%u", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%v, kgen_unit, printname // "%v", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%v, kgen_unit, printname // "%v", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%s, kgen_unit, printname // "%s", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%s, kgen_unit, printname // "%s", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%omega, kgen_unit, printname // "%omega", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%omega, kgen_unit, printname // "%omega", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%pmid, kgen_unit, printname // "%pmid", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%pmid, kgen_unit, printname // "%pmid", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%pmiddry, kgen_unit, printname // "%pmiddry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%pmiddry, kgen_unit, printname // "%pmiddry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%pdel, kgen_unit, printname // "%pdel", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%pdel, kgen_unit, printname // "%pdel", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%pdeldry, kgen_unit, printname // "%pdeldry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%pdeldry, kgen_unit, printname // "%pdeldry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%rpdel, kgen_unit, printname // "%rpdel", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%rpdel, kgen_unit, printname // "%rpdel", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%rpdeldry, kgen_unit, printname // "%rpdeldry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%rpdeldry, kgen_unit, printname // "%rpdeldry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%lnpmid, kgen_unit, printname // "%lnpmid", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%lnpmid, kgen_unit, printname // "%lnpmid", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%lnpmiddry, kgen_unit, printname // "%lnpmiddry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%lnpmiddry, kgen_unit, printname // "%lnpmiddry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%exner, kgen_unit, printname // "%exner", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%exner, kgen_unit, printname // "%exner", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%zm, kgen_unit, printname // "%zm", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%zm, kgen_unit, printname // "%zm", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim3(var%q, kgen_unit, printname // "%q", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim3(var%q, kgen_unit, printname // "%q", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%pint, kgen_unit, printname // "%pint", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%pint, kgen_unit, printname // "%pint", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%pintdry, kgen_unit, printname // "%pintdry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%pintdry, kgen_unit, printname // "%pintdry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%lnpint, kgen_unit, printname // "%lnpint", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%lnpint, kgen_unit, printname // "%lnpint", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%lnpintdry, kgen_unit, printname // "%lnpintdry", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%lnpintdry, kgen_unit, printname // "%lnpintdry", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim2(var%zi, kgen_unit, printname // "%zi", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim2(var%zi, kgen_unit, printname // "%zi", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%te_ini, kgen_unit, printname // "%te_ini", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%te_ini, kgen_unit, printname // "%te_ini", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%te_cur, kgen_unit, printname // "%te_cur", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%te_cur, kgen_unit, printname // "%te_cur", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%tw_ini, kgen_unit, printname // "%tw_ini", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%tw_ini, kgen_unit, printname // "%tw_ini", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_real__r8_dim1(var%tw_cur, kgen_unit, printname // "%tw_cur", .TRUE.) 
      ELSE 
          CALL kr_physics_state_real__r8_dim1(var%tw_cur, kgen_unit, printname // "%tw_cur", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%count 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%count = ", var%count 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_integer___dim1(var%latmapback, kgen_unit, printname // "%latmapback", .TRUE.) 
      ELSE 
          CALL kr_physics_state_integer___dim1(var%latmapback, kgen_unit, printname // "%latmapback", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_integer___dim1(var%lonmapback, kgen_unit, printname // "%lonmapback", .TRUE.) 
      ELSE 
          CALL kr_physics_state_integer___dim1(var%lonmapback, kgen_unit, printname // "%lonmapback", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_state_integer___dim1(var%cid, kgen_unit, printname // "%cid", .TRUE.) 
      ELSE 
          CALL kr_physics_state_integer___dim1(var%cid, kgen_unit, printname // "%cid", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%ulatcnt 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ulatcnt = ", var%ulatcnt 
      END IF   
      READ (UNIT = kgen_unit) var%uloncnt 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%uloncnt = ", var%uloncnt 
      END IF   
        
  END SUBROUTINE kr_physics_types_physics_state 
    
  !write state subroutine for kr_physics_state_real__r8_dim1 
  SUBROUTINE kr_physics_state_real__r8_dim1(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
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
        
  END SUBROUTINE kr_physics_state_real__r8_dim1 
    
  !write state subroutine for kr_physics_state_real__r8_dim2 
  SUBROUTINE kr_physics_state_real__r8_dim2(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
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
        
  END SUBROUTINE kr_physics_state_real__r8_dim2 
    
  !write state subroutine for kr_physics_state_real__r8_dim3 
  SUBROUTINE kr_physics_state_real__r8_dim3(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3 
      INTEGER, DIMENSION(2,3) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          READ (UNIT = kgen_unit) kgen_bound(1, 3) 
          READ (UNIT = kgen_unit) kgen_bound(2, 3) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_physics_state_real__r8_dim3 
    
  !write state subroutine for kr_physics_state_integer___dim1 
  SUBROUTINE kr_physics_state_integer___dim1(var, kgen_unit, printname, printvar) 
      INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
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
        
  END SUBROUTINE kr_physics_state_integer___dim1 
    
  !read state subroutine for kr_physics_types_physics_ptend 
  RECURSIVE SUBROUTINE kr_physics_types_physics_ptend(var, kgen_unit, printname, printvar) 
      TYPE(physics_ptend), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%psetcols 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%psetcols = ", var%psetcols 
      END IF   
        
      READ (UNIT = kgen_unit) var%name 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%name = ", var%name 
      END IF   
        
      READ (UNIT = kgen_unit) var%ls 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ls = ", var%ls 
      END IF   
      READ (UNIT = kgen_unit) var%lu 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%lu = ", var%lu 
      END IF   
      READ (UNIT = kgen_unit) var%lv 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%lv = ", var%lv 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) var%lq 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: " // printname // "%lq = ", var%lq 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%top_level 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%top_level = ", var%top_level 
      END IF   
      READ (UNIT = kgen_unit) var%bot_level 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%bot_level = ", var%bot_level 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim2(var%s, kgen_unit, printname // "%s", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim2(var%s, kgen_unit, printname // "%s", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim2(var%u, kgen_unit, printname // "%u", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim2(var%u, kgen_unit, printname // "%u", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim2(var%v, kgen_unit, printname // "%v", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim2(var%v, kgen_unit, printname // "%v", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim3(var%q, kgen_unit, printname // "%q", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim3(var%q, kgen_unit, printname // "%q", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim1(var%hflux_srf, kgen_unit, printname // "%hflux_srf", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim1(var%hflux_srf, kgen_unit, printname // "%hflux_srf", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim1(var%hflux_top, kgen_unit, printname // "%hflux_top", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim1(var%hflux_top, kgen_unit, printname // "%hflux_top", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim1(var%taux_srf, kgen_unit, printname // "%taux_srf", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim1(var%taux_srf, kgen_unit, printname // "%taux_srf", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim1(var%taux_top, kgen_unit, printname // "%taux_top", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim1(var%taux_top, kgen_unit, printname // "%taux_top", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim1(var%tauy_srf, kgen_unit, printname // "%tauy_srf", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim1(var%tauy_srf, kgen_unit, printname // "%tauy_srf", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim1(var%tauy_top, kgen_unit, printname // "%tauy_top", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim1(var%tauy_top, kgen_unit, printname // "%tauy_top", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim2(var%cflx_srf, kgen_unit, printname // "%cflx_srf", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim2(var%cflx_srf, kgen_unit, printname // "%cflx_srf", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_physics_ptend_real__r8_dim2(var%cflx_top, kgen_unit, printname // "%cflx_top", .TRUE.) 
      ELSE 
          CALL kr_physics_ptend_real__r8_dim2(var%cflx_top, kgen_unit, printname // "%cflx_top", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_physics_types_physics_ptend 
    
  !write state subroutine for kr_physics_ptend_real__r8_dim2 
  SUBROUTINE kr_physics_ptend_real__r8_dim2(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
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
        
  END SUBROUTINE kr_physics_ptend_real__r8_dim2 
    
  !write state subroutine for kr_physics_ptend_real__r8_dim3 
  SUBROUTINE kr_physics_ptend_real__r8_dim3(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3 
      INTEGER, DIMENSION(2,3) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          READ (UNIT = kgen_unit) kgen_bound(1, 3) 
          READ (UNIT = kgen_unit) kgen_bound(2, 3) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_physics_ptend_real__r8_dim3 
    
  !write state subroutine for kr_physics_ptend_real__r8_dim1 
  SUBROUTINE kr_physics_ptend_real__r8_dim1(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
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
        
  END SUBROUTINE kr_physics_ptend_real__r8_dim1 
    
  !verify state subroutine for kv_physics_types_physics_state 
  RECURSIVE SUBROUTINE kv_physics_types_physics_state(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(physics_state), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_lchnk 
      integer :: diff_ngrdcol 
      integer :: diff_psetcols 
      integer :: diff_ncol 
      INTEGER :: n_lat 
      real(KIND=r8) :: nrmsdiff_lat, rmsdiff_lat 
      real(KIND=r8), ALLOCATABLE :: buf1_lat(:), buf2_lat(:) 
      INTEGER :: n_lon 
      real(KIND=r8) :: nrmsdiff_lon, rmsdiff_lon 
      real(KIND=r8), ALLOCATABLE :: buf1_lon(:), buf2_lon(:) 
      INTEGER :: n_ps 
      real(KIND=r8) :: nrmsdiff_ps, rmsdiff_ps 
      real(KIND=r8), ALLOCATABLE :: buf1_ps(:), buf2_ps(:) 
      INTEGER :: n_psdry 
      real(KIND=r8) :: nrmsdiff_psdry, rmsdiff_psdry 
      real(KIND=r8), ALLOCATABLE :: buf1_psdry(:), buf2_psdry(:) 
      INTEGER :: n_phis 
      real(KIND=r8) :: nrmsdiff_phis, rmsdiff_phis 
      real(KIND=r8), ALLOCATABLE :: buf1_phis(:), buf2_phis(:) 
      INTEGER :: n_ulat 
      real(KIND=r8) :: nrmsdiff_ulat, rmsdiff_ulat 
      real(KIND=r8), ALLOCATABLE :: buf1_ulat(:), buf2_ulat(:) 
      INTEGER :: n_ulon 
      real(KIND=r8) :: nrmsdiff_ulon, rmsdiff_ulon 
      real(KIND=r8), ALLOCATABLE :: buf1_ulon(:), buf2_ulon(:) 
      INTEGER :: n_t 
      real(KIND=r8) :: nrmsdiff_t, rmsdiff_t 
      real(KIND=r8), ALLOCATABLE :: buf1_t(:,:), buf2_t(:,:) 
      INTEGER :: n_u 
      real(KIND=r8) :: nrmsdiff_u, rmsdiff_u 
      real(KIND=r8), ALLOCATABLE :: buf1_u(:,:), buf2_u(:,:) 
      INTEGER :: n_v 
      real(KIND=r8) :: nrmsdiff_v, rmsdiff_v 
      real(KIND=r8), ALLOCATABLE :: buf1_v(:,:), buf2_v(:,:) 
      INTEGER :: n_s 
      real(KIND=r8) :: nrmsdiff_s, rmsdiff_s 
      real(KIND=r8), ALLOCATABLE :: buf1_s(:,:), buf2_s(:,:) 
      INTEGER :: n_omega 
      real(KIND=r8) :: nrmsdiff_omega, rmsdiff_omega 
      real(KIND=r8), ALLOCATABLE :: buf1_omega(:,:), buf2_omega(:,:) 
      INTEGER :: n_pmid 
      real(KIND=r8) :: nrmsdiff_pmid, rmsdiff_pmid 
      real(KIND=r8), ALLOCATABLE :: buf1_pmid(:,:), buf2_pmid(:,:) 
      INTEGER :: n_pmiddry 
      real(KIND=r8) :: nrmsdiff_pmiddry, rmsdiff_pmiddry 
      real(KIND=r8), ALLOCATABLE :: buf1_pmiddry(:,:), buf2_pmiddry(:,:) 
      INTEGER :: n_pdel 
      real(KIND=r8) :: nrmsdiff_pdel, rmsdiff_pdel 
      real(KIND=r8), ALLOCATABLE :: buf1_pdel(:,:), buf2_pdel(:,:) 
      INTEGER :: n_pdeldry 
      real(KIND=r8) :: nrmsdiff_pdeldry, rmsdiff_pdeldry 
      real(KIND=r8), ALLOCATABLE :: buf1_pdeldry(:,:), buf2_pdeldry(:,:) 
      INTEGER :: n_rpdel 
      real(KIND=r8) :: nrmsdiff_rpdel, rmsdiff_rpdel 
      real(KIND=r8), ALLOCATABLE :: buf1_rpdel(:,:), buf2_rpdel(:,:) 
      INTEGER :: n_rpdeldry 
      real(KIND=r8) :: nrmsdiff_rpdeldry, rmsdiff_rpdeldry 
      real(KIND=r8), ALLOCATABLE :: buf1_rpdeldry(:,:), buf2_rpdeldry(:,:) 
      INTEGER :: n_lnpmid 
      real(KIND=r8) :: nrmsdiff_lnpmid, rmsdiff_lnpmid 
      real(KIND=r8), ALLOCATABLE :: buf1_lnpmid(:,:), buf2_lnpmid(:,:) 
      INTEGER :: n_lnpmiddry 
      real(KIND=r8) :: nrmsdiff_lnpmiddry, rmsdiff_lnpmiddry 
      real(KIND=r8), ALLOCATABLE :: buf1_lnpmiddry(:,:), buf2_lnpmiddry(:,:) 
      INTEGER :: n_exner 
      real(KIND=r8) :: nrmsdiff_exner, rmsdiff_exner 
      real(KIND=r8), ALLOCATABLE :: buf1_exner(:,:), buf2_exner(:,:) 
      INTEGER :: n_zm 
      real(KIND=r8) :: nrmsdiff_zm, rmsdiff_zm 
      real(KIND=r8), ALLOCATABLE :: buf1_zm(:,:), buf2_zm(:,:) 
      INTEGER :: n_q 
      real(KIND=r8) :: nrmsdiff_q, rmsdiff_q 
      real(KIND=r8), ALLOCATABLE :: buf1_q(:,:,:), buf2_q(:,:,:) 
      INTEGER :: n_pint 
      real(KIND=r8) :: nrmsdiff_pint, rmsdiff_pint 
      real(KIND=r8), ALLOCATABLE :: buf1_pint(:,:), buf2_pint(:,:) 
      INTEGER :: n_pintdry 
      real(KIND=r8) :: nrmsdiff_pintdry, rmsdiff_pintdry 
      real(KIND=r8), ALLOCATABLE :: buf1_pintdry(:,:), buf2_pintdry(:,:) 
      INTEGER :: n_lnpint 
      real(KIND=r8) :: nrmsdiff_lnpint, rmsdiff_lnpint 
      real(KIND=r8), ALLOCATABLE :: buf1_lnpint(:,:), buf2_lnpint(:,:) 
      INTEGER :: n_lnpintdry 
      real(KIND=r8) :: nrmsdiff_lnpintdry, rmsdiff_lnpintdry 
      real(KIND=r8), ALLOCATABLE :: buf1_lnpintdry(:,:), buf2_lnpintdry(:,:) 
      INTEGER :: n_zi 
      real(KIND=r8) :: nrmsdiff_zi, rmsdiff_zi 
      real(KIND=r8), ALLOCATABLE :: buf1_zi(:,:), buf2_zi(:,:) 
      INTEGER :: n_te_ini 
      real(KIND=r8) :: nrmsdiff_te_ini, rmsdiff_te_ini 
      real(KIND=r8), ALLOCATABLE :: buf1_te_ini(:), buf2_te_ini(:) 
      INTEGER :: n_te_cur 
      real(KIND=r8) :: nrmsdiff_te_cur, rmsdiff_te_cur 
      real(KIND=r8), ALLOCATABLE :: buf1_te_cur(:), buf2_te_cur(:) 
      INTEGER :: n_tw_ini 
      real(KIND=r8) :: nrmsdiff_tw_ini, rmsdiff_tw_ini 
      real(KIND=r8), ALLOCATABLE :: buf1_tw_ini(:), buf2_tw_ini(:) 
      INTEGER :: n_tw_cur 
      real(KIND=r8) :: nrmsdiff_tw_cur, rmsdiff_tw_cur 
      real(KIND=r8), ALLOCATABLE :: buf1_tw_cur(:), buf2_tw_cur(:) 
      integer :: diff_count 
      INTEGER :: n_latmapback 
      integer :: nrmsdiff_latmapback, rmsdiff_latmapback 
      integer, ALLOCATABLE :: buf1_latmapback(:), buf2_latmapback(:) 
      INTEGER :: n_lonmapback 
      integer :: nrmsdiff_lonmapback, rmsdiff_lonmapback 
      integer, ALLOCATABLE :: buf1_lonmapback(:), buf2_lonmapback(:) 
      INTEGER :: n_cid 
      integer :: nrmsdiff_cid, rmsdiff_cid 
      integer, ALLOCATABLE :: buf1_cid(:), buf2_cid(:) 
      integer :: diff_ulatcnt 
      integer :: diff_uloncnt 
        
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
      IF (var%ngrdcol == kgenref_var%ngrdcol) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ngrdcol is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ngrdcol = ABS(var%ngrdcol - kgenref_var%ngrdcol) 
          IF (diff_ngrdcol <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ngrdcol is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ngrdcol is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_ngrdcol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ngrdcol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%psetcols == kgenref_var%psetcols) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%psetcols is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_psetcols = ABS(var%psetcols - kgenref_var%psetcols) 
          IF (diff_psetcols <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%psetcols is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%psetcols is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_psetcols 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_psetcols 
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
        
      IF (ALLOCATED(var%lat)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lat == kgenref_var%lat)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lat is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lat(SIZE(var%lat,dim=1))) 
              ALLOCATE (buf2_lat(SIZE(var%lat,dim=1))) 
              n_lat = COUNT(var%lat /= kgenref_var%lat) 
              WHERE ( ABS(kgenref_var%lat) > kgen_minvalue ) 
                  buf1_lat = ((var%lat-kgenref_var%lat)/kgenref_var%lat)**2 
                  buf2_lat = (var%lat-kgenref_var%lat)**2 
              ELSEWHERE 
                  buf1_lat = (var%lat-kgenref_var%lat)**2 
                  buf2_lat = buf1_lat 
              END WHERE   
              nrmsdiff_lat = SQRT(SUM(buf1_lat)/REAL(n_lat)) 
              rmsdiff_lat = SQRT(SUM(buf2_lat)/REAL(n_lat)) 
              IF (rmsdiff_lat > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lat is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lat is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lat /= kgenref_var%lat), " of ", size( var%lat ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lat)/real(size(var%lat)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lat)/real(size(kgenref_var%lat)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lat 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lat 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lat /= kgenref_var%lat), " of ", size( var%lat ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lat)/real(size(var%lat)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lat)/real(size(kgenref_var%lat)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lat 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lat 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%lon)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lon == kgenref_var%lon)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lon is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lon(SIZE(var%lon,dim=1))) 
              ALLOCATE (buf2_lon(SIZE(var%lon,dim=1))) 
              n_lon = COUNT(var%lon /= kgenref_var%lon) 
              WHERE ( ABS(kgenref_var%lon) > kgen_minvalue ) 
                  buf1_lon = ((var%lon-kgenref_var%lon)/kgenref_var%lon)**2 
                  buf2_lon = (var%lon-kgenref_var%lon)**2 
              ELSEWHERE 
                  buf1_lon = (var%lon-kgenref_var%lon)**2 
                  buf2_lon = buf1_lon 
              END WHERE   
              nrmsdiff_lon = SQRT(SUM(buf1_lon)/REAL(n_lon)) 
              rmsdiff_lon = SQRT(SUM(buf2_lon)/REAL(n_lon)) 
              IF (rmsdiff_lon > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lon is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lon is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lon /= kgenref_var%lon), " of ", size( var%lon ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lon)/real(size(var%lon)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lon)/real(size(kgenref_var%lon)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lon 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lon 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lon /= kgenref_var%lon), " of ", size( var%lon ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lon)/real(size(var%lon)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lon)/real(size(kgenref_var%lon)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lon 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lon 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%ps)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%ps == kgenref_var%ps)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ps is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_ps(SIZE(var%ps,dim=1))) 
              ALLOCATE (buf2_ps(SIZE(var%ps,dim=1))) 
              n_ps = COUNT(var%ps /= kgenref_var%ps) 
              WHERE ( ABS(kgenref_var%ps) > kgen_minvalue ) 
                  buf1_ps = ((var%ps-kgenref_var%ps)/kgenref_var%ps)**2 
                  buf2_ps = (var%ps-kgenref_var%ps)**2 
              ELSEWHERE 
                  buf1_ps = (var%ps-kgenref_var%ps)**2 
                  buf2_ps = buf1_ps 
              END WHERE   
              nrmsdiff_ps = SQRT(SUM(buf1_ps)/REAL(n_ps)) 
              rmsdiff_ps = SQRT(SUM(buf2_ps)/REAL(n_ps)) 
              IF (rmsdiff_ps > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ps is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ps is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%ps /= kgenref_var%ps), " of ", size( var%ps ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ps)/real(size(var%ps)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ps)/real(size(kgenref_var%ps)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ps 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ps 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%ps /= kgenref_var%ps), " of ", size( var%ps ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ps)/real(size(var%ps)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ps)/real(size(kgenref_var%ps)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ps 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ps 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%psdry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%psdry == kgenref_var%psdry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%psdry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_psdry(SIZE(var%psdry,dim=1))) 
              ALLOCATE (buf2_psdry(SIZE(var%psdry,dim=1))) 
              n_psdry = COUNT(var%psdry /= kgenref_var%psdry) 
              WHERE ( ABS(kgenref_var%psdry) > kgen_minvalue ) 
                  buf1_psdry = ((var%psdry-kgenref_var%psdry)/kgenref_var%psdry)**2 
                  buf2_psdry = (var%psdry-kgenref_var%psdry)**2 
              ELSEWHERE 
                  buf1_psdry = (var%psdry-kgenref_var%psdry)**2 
                  buf2_psdry = buf1_psdry 
              END WHERE   
              nrmsdiff_psdry = SQRT(SUM(buf1_psdry)/REAL(n_psdry)) 
              rmsdiff_psdry = SQRT(SUM(buf2_psdry)/REAL(n_psdry)) 
              IF (rmsdiff_psdry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%psdry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%psdry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%psdry /= kgenref_var%psdry), " of ", size( var%psdry ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%psdry)/real(size(var%psdry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%psdry)/real(size(kgenref_var%psdry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_psdry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_psdry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%psdry /= kgenref_var%psdry), " of ", size( var%psdry ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%psdry)/real(size(var%psdry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%psdry)/real(size(kgenref_var%psdry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_psdry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_psdry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%phis)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%phis == kgenref_var%phis)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%phis is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_phis(SIZE(var%phis,dim=1))) 
              ALLOCATE (buf2_phis(SIZE(var%phis,dim=1))) 
              n_phis = COUNT(var%phis /= kgenref_var%phis) 
              WHERE ( ABS(kgenref_var%phis) > kgen_minvalue ) 
                  buf1_phis = ((var%phis-kgenref_var%phis)/kgenref_var%phis)**2 
                  buf2_phis = (var%phis-kgenref_var%phis)**2 
              ELSEWHERE 
                  buf1_phis = (var%phis-kgenref_var%phis)**2 
                  buf2_phis = buf1_phis 
              END WHERE   
              nrmsdiff_phis = SQRT(SUM(buf1_phis)/REAL(n_phis)) 
              rmsdiff_phis = SQRT(SUM(buf2_phis)/REAL(n_phis)) 
              IF (rmsdiff_phis > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%phis is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%phis is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%phis /= kgenref_var%phis), " of ", size( var%phis ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%phis)/real(size(var%phis)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%phis)/real(size(kgenref_var%phis)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_phis 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phis 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%phis /= kgenref_var%phis), " of ", size( var%phis ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%phis)/real(size(var%phis)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%phis)/real(size(kgenref_var%phis)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_phis 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phis 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%ulat)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%ulat == kgenref_var%ulat)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ulat is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_ulat(SIZE(var%ulat,dim=1))) 
              ALLOCATE (buf2_ulat(SIZE(var%ulat,dim=1))) 
              n_ulat = COUNT(var%ulat /= kgenref_var%ulat) 
              WHERE ( ABS(kgenref_var%ulat) > kgen_minvalue ) 
                  buf1_ulat = ((var%ulat-kgenref_var%ulat)/kgenref_var%ulat)**2 
                  buf2_ulat = (var%ulat-kgenref_var%ulat)**2 
              ELSEWHERE 
                  buf1_ulat = (var%ulat-kgenref_var%ulat)**2 
                  buf2_ulat = buf1_ulat 
              END WHERE   
              nrmsdiff_ulat = SQRT(SUM(buf1_ulat)/REAL(n_ulat)) 
              rmsdiff_ulat = SQRT(SUM(buf2_ulat)/REAL(n_ulat)) 
              IF (rmsdiff_ulat > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ulat is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ulat is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%ulat /= kgenref_var%ulat), " of ", size( var%ulat ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ulat)/real(size(var%ulat)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ulat)/real(size(kgenref_var%ulat)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ulat 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ulat 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%ulat /= kgenref_var%ulat), " of ", size( var%ulat ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ulat)/real(size(var%ulat)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ulat)/real(size(kgenref_var%ulat)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ulat 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ulat 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%ulon)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%ulon == kgenref_var%ulon)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ulon is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_ulon(SIZE(var%ulon,dim=1))) 
              ALLOCATE (buf2_ulon(SIZE(var%ulon,dim=1))) 
              n_ulon = COUNT(var%ulon /= kgenref_var%ulon) 
              WHERE ( ABS(kgenref_var%ulon) > kgen_minvalue ) 
                  buf1_ulon = ((var%ulon-kgenref_var%ulon)/kgenref_var%ulon)**2 
                  buf2_ulon = (var%ulon-kgenref_var%ulon)**2 
              ELSEWHERE 
                  buf1_ulon = (var%ulon-kgenref_var%ulon)**2 
                  buf2_ulon = buf1_ulon 
              END WHERE   
              nrmsdiff_ulon = SQRT(SUM(buf1_ulon)/REAL(n_ulon)) 
              rmsdiff_ulon = SQRT(SUM(buf2_ulon)/REAL(n_ulon)) 
              IF (rmsdiff_ulon > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ulon is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%ulon is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%ulon /= kgenref_var%ulon), " of ", size( var%ulon ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ulon)/real(size(var%ulon)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ulon)/real(size(kgenref_var%ulon)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ulon 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ulon 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%ulon /= kgenref_var%ulon), " of ", size( var%ulon ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%ulon)/real(size(var%ulon)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%ulon)/real(size(kgenref_var%ulon)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_ulon 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ulon 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%t)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%t == kgenref_var%t)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%t is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_t(SIZE(var%t,dim=1),SIZE(var%t,dim=2))) 
              ALLOCATE (buf2_t(SIZE(var%t,dim=1),SIZE(var%t,dim=2))) 
              n_t = COUNT(var%t /= kgenref_var%t) 
              WHERE ( ABS(kgenref_var%t) > kgen_minvalue ) 
                  buf1_t = ((var%t-kgenref_var%t)/kgenref_var%t)**2 
                  buf2_t = (var%t-kgenref_var%t)**2 
              ELSEWHERE 
                  buf1_t = (var%t-kgenref_var%t)**2 
                  buf2_t = buf1_t 
              END WHERE   
              nrmsdiff_t = SQRT(SUM(buf1_t)/REAL(n_t)) 
              rmsdiff_t = SQRT(SUM(buf2_t)/REAL(n_t)) 
              IF (rmsdiff_t > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%t is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%t is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%t /= kgenref_var%t), " of ", size( var%t ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%t)/real(size(var%t)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%t)/real(size(kgenref_var%t)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_t 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_t 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%t /= kgenref_var%t), " of ", size( var%t ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%t)/real(size(var%t)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%t)/real(size(kgenref_var%t)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_t 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_t 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%u)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%u == kgenref_var%u)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%u is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_u(SIZE(var%u,dim=1),SIZE(var%u,dim=2))) 
              ALLOCATE (buf2_u(SIZE(var%u,dim=1),SIZE(var%u,dim=2))) 
              n_u = COUNT(var%u /= kgenref_var%u) 
              WHERE ( ABS(kgenref_var%u) > kgen_minvalue ) 
                  buf1_u = ((var%u-kgenref_var%u)/kgenref_var%u)**2 
                  buf2_u = (var%u-kgenref_var%u)**2 
              ELSEWHERE 
                  buf1_u = (var%u-kgenref_var%u)**2 
                  buf2_u = buf1_u 
              END WHERE   
              nrmsdiff_u = SQRT(SUM(buf1_u)/REAL(n_u)) 
              rmsdiff_u = SQRT(SUM(buf2_u)/REAL(n_u)) 
              IF (rmsdiff_u > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%u is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%u is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%u /= kgenref_var%u), " of ", size( var%u ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%u)/real(size(var%u)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%u)/real(size(kgenref_var%u)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_u 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%u /= kgenref_var%u), " of ", size( var%u ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%u)/real(size(var%u)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%u)/real(size(kgenref_var%u)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_u 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%v)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%v == kgenref_var%v)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%v is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_v(SIZE(var%v,dim=1),SIZE(var%v,dim=2))) 
              ALLOCATE (buf2_v(SIZE(var%v,dim=1),SIZE(var%v,dim=2))) 
              n_v = COUNT(var%v /= kgenref_var%v) 
              WHERE ( ABS(kgenref_var%v) > kgen_minvalue ) 
                  buf1_v = ((var%v-kgenref_var%v)/kgenref_var%v)**2 
                  buf2_v = (var%v-kgenref_var%v)**2 
              ELSEWHERE 
                  buf1_v = (var%v-kgenref_var%v)**2 
                  buf2_v = buf1_v 
              END WHERE   
              nrmsdiff_v = SQRT(SUM(buf1_v)/REAL(n_v)) 
              rmsdiff_v = SQRT(SUM(buf2_v)/REAL(n_v)) 
              IF (rmsdiff_v > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%v is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%v is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%v /= kgenref_var%v), " of ", size( var%v ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%v)/real(size(var%v)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%v)/real(size(kgenref_var%v)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_v 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%v /= kgenref_var%v), " of ", size( var%v ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%v)/real(size(var%v)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%v)/real(size(kgenref_var%v)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_v 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%s)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%s == kgenref_var%s)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%s is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_s(SIZE(var%s,dim=1),SIZE(var%s,dim=2))) 
              ALLOCATE (buf2_s(SIZE(var%s,dim=1),SIZE(var%s,dim=2))) 
              n_s = COUNT(var%s /= kgenref_var%s) 
              WHERE ( ABS(kgenref_var%s) > kgen_minvalue ) 
                  buf1_s = ((var%s-kgenref_var%s)/kgenref_var%s)**2 
                  buf2_s = (var%s-kgenref_var%s)**2 
              ELSEWHERE 
                  buf1_s = (var%s-kgenref_var%s)**2 
                  buf2_s = buf1_s 
              END WHERE   
              nrmsdiff_s = SQRT(SUM(buf1_s)/REAL(n_s)) 
              rmsdiff_s = SQRT(SUM(buf2_s)/REAL(n_s)) 
              IF (rmsdiff_s > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%s /= kgenref_var%s), " of ", size( var%s ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%s)/real(size(var%s)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%s)/real(size(kgenref_var%s)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_s 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_s 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%s /= kgenref_var%s), " of ", size( var%s ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%s)/real(size(var%s)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%s)/real(size(kgenref_var%s)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_s 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_s 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%omega)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%omega == kgenref_var%omega)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%omega is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_omega(SIZE(var%omega,dim=1),SIZE(var%omega,dim=2))) 
              ALLOCATE (buf2_omega(SIZE(var%omega,dim=1),SIZE(var%omega,dim=2))) 
              n_omega = COUNT(var%omega /= kgenref_var%omega) 
              WHERE ( ABS(kgenref_var%omega) > kgen_minvalue ) 
                  buf1_omega = ((var%omega-kgenref_var%omega)/kgenref_var%omega)**2 
                  buf2_omega = (var%omega-kgenref_var%omega)**2 
              ELSEWHERE 
                  buf1_omega = (var%omega-kgenref_var%omega)**2 
                  buf2_omega = buf1_omega 
              END WHERE   
              nrmsdiff_omega = SQRT(SUM(buf1_omega)/REAL(n_omega)) 
              rmsdiff_omega = SQRT(SUM(buf2_omega)/REAL(n_omega)) 
              IF (rmsdiff_omega > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%omega is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%omega is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%omega /= kgenref_var%omega), " of ", size( var%omega ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%omega)/real(size(var%omega)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%omega)/real(size(kgenref_var%omega)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_omega 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_omega 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%omega /= kgenref_var%omega), " of ", size( var%omega ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%omega)/real(size(var%omega)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%omega)/real(size(kgenref_var%omega)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_omega 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_omega 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%pmid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%pmid == kgenref_var%pmid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%pmid is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_pmid(SIZE(var%pmid,dim=1),SIZE(var%pmid,dim=2))) 
              ALLOCATE (buf2_pmid(SIZE(var%pmid,dim=1),SIZE(var%pmid,dim=2))) 
              n_pmid = COUNT(var%pmid /= kgenref_var%pmid) 
              WHERE ( ABS(kgenref_var%pmid) > kgen_minvalue ) 
                  buf1_pmid = ((var%pmid-kgenref_var%pmid)/kgenref_var%pmid)**2 
                  buf2_pmid = (var%pmid-kgenref_var%pmid)**2 
              ELSEWHERE 
                  buf1_pmid = (var%pmid-kgenref_var%pmid)**2 
                  buf2_pmid = buf1_pmid 
              END WHERE   
              nrmsdiff_pmid = SQRT(SUM(buf1_pmid)/REAL(n_pmid)) 
              rmsdiff_pmid = SQRT(SUM(buf2_pmid)/REAL(n_pmid)) 
              IF (rmsdiff_pmid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pmid is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pmid is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%pmid /= kgenref_var%pmid), " of ", size( var%pmid ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pmid)/real(size(var%pmid)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pmid)/real(size(kgenref_var%pmid)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pmid 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pmid 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%pmid /= kgenref_var%pmid), " of ", size( var%pmid ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pmid)/real(size(var%pmid)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pmid)/real(size(kgenref_var%pmid)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pmid 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pmid 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%pmiddry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%pmiddry == kgenref_var%pmiddry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%pmiddry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_pmiddry(SIZE(var%pmiddry,dim=1),SIZE(var%pmiddry,dim=2))) 
              ALLOCATE (buf2_pmiddry(SIZE(var%pmiddry,dim=1),SIZE(var%pmiddry,dim=2))) 
              n_pmiddry = COUNT(var%pmiddry /= kgenref_var%pmiddry) 
              WHERE ( ABS(kgenref_var%pmiddry) > kgen_minvalue ) 
                  buf1_pmiddry = ((var%pmiddry-kgenref_var%pmiddry)/kgenref_var%pmiddry)**2 
                  buf2_pmiddry = (var%pmiddry-kgenref_var%pmiddry)**2 
              ELSEWHERE 
                  buf1_pmiddry = (var%pmiddry-kgenref_var%pmiddry)**2 
                  buf2_pmiddry = buf1_pmiddry 
              END WHERE   
              nrmsdiff_pmiddry = SQRT(SUM(buf1_pmiddry)/REAL(n_pmiddry)) 
              rmsdiff_pmiddry = SQRT(SUM(buf2_pmiddry)/REAL(n_pmiddry)) 
              IF (rmsdiff_pmiddry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pmiddry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pmiddry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%pmiddry /= kgenref_var%pmiddry), " of ", size( var%pmiddry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pmiddry)/real(size(var%pmiddry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pmiddry)/real(size(kgenref_var%pmiddry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pmiddry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pmiddry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%pmiddry /= kgenref_var%pmiddry), " of ", size( var%pmiddry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pmiddry)/real(size(var%pmiddry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pmiddry)/real(size(kgenref_var%pmiddry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pmiddry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pmiddry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%pdel)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%pdel == kgenref_var%pdel)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%pdel is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_pdel(SIZE(var%pdel,dim=1),SIZE(var%pdel,dim=2))) 
              ALLOCATE (buf2_pdel(SIZE(var%pdel,dim=1),SIZE(var%pdel,dim=2))) 
              n_pdel = COUNT(var%pdel /= kgenref_var%pdel) 
              WHERE ( ABS(kgenref_var%pdel) > kgen_minvalue ) 
                  buf1_pdel = ((var%pdel-kgenref_var%pdel)/kgenref_var%pdel)**2 
                  buf2_pdel = (var%pdel-kgenref_var%pdel)**2 
              ELSEWHERE 
                  buf1_pdel = (var%pdel-kgenref_var%pdel)**2 
                  buf2_pdel = buf1_pdel 
              END WHERE   
              nrmsdiff_pdel = SQRT(SUM(buf1_pdel)/REAL(n_pdel)) 
              rmsdiff_pdel = SQRT(SUM(buf2_pdel)/REAL(n_pdel)) 
              IF (rmsdiff_pdel > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pdel is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pdel is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%pdel /= kgenref_var%pdel), " of ", size( var%pdel ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pdel)/real(size(var%pdel)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pdel)/real(size(kgenref_var%pdel)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pdel 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pdel 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%pdel /= kgenref_var%pdel), " of ", size( var%pdel ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pdel)/real(size(var%pdel)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pdel)/real(size(kgenref_var%pdel)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pdel 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pdel 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%pdeldry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%pdeldry == kgenref_var%pdeldry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%pdeldry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_pdeldry(SIZE(var%pdeldry,dim=1),SIZE(var%pdeldry,dim=2))) 
              ALLOCATE (buf2_pdeldry(SIZE(var%pdeldry,dim=1),SIZE(var%pdeldry,dim=2))) 
              n_pdeldry = COUNT(var%pdeldry /= kgenref_var%pdeldry) 
              WHERE ( ABS(kgenref_var%pdeldry) > kgen_minvalue ) 
                  buf1_pdeldry = ((var%pdeldry-kgenref_var%pdeldry)/kgenref_var%pdeldry)**2 
                  buf2_pdeldry = (var%pdeldry-kgenref_var%pdeldry)**2 
              ELSEWHERE 
                  buf1_pdeldry = (var%pdeldry-kgenref_var%pdeldry)**2 
                  buf2_pdeldry = buf1_pdeldry 
              END WHERE   
              nrmsdiff_pdeldry = SQRT(SUM(buf1_pdeldry)/REAL(n_pdeldry)) 
              rmsdiff_pdeldry = SQRT(SUM(buf2_pdeldry)/REAL(n_pdeldry)) 
              IF (rmsdiff_pdeldry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pdeldry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pdeldry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%pdeldry /= kgenref_var%pdeldry), " of ", size( var%pdeldry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pdeldry)/real(size(var%pdeldry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pdeldry)/real(size(kgenref_var%pdeldry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pdeldry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pdeldry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%pdeldry /= kgenref_var%pdeldry), " of ", size( var%pdeldry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pdeldry)/real(size(var%pdeldry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pdeldry)/real(size(kgenref_var%pdeldry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pdeldry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pdeldry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%rpdel)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%rpdel == kgenref_var%rpdel)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rpdel is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_rpdel(SIZE(var%rpdel,dim=1),SIZE(var%rpdel,dim=2))) 
              ALLOCATE (buf2_rpdel(SIZE(var%rpdel,dim=1),SIZE(var%rpdel,dim=2))) 
              n_rpdel = COUNT(var%rpdel /= kgenref_var%rpdel) 
              WHERE ( ABS(kgenref_var%rpdel) > kgen_minvalue ) 
                  buf1_rpdel = ((var%rpdel-kgenref_var%rpdel)/kgenref_var%rpdel)**2 
                  buf2_rpdel = (var%rpdel-kgenref_var%rpdel)**2 
              ELSEWHERE 
                  buf1_rpdel = (var%rpdel-kgenref_var%rpdel)**2 
                  buf2_rpdel = buf1_rpdel 
              END WHERE   
              nrmsdiff_rpdel = SQRT(SUM(buf1_rpdel)/REAL(n_rpdel)) 
              rmsdiff_rpdel = SQRT(SUM(buf2_rpdel)/REAL(n_rpdel)) 
              IF (rmsdiff_rpdel > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%rpdel is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%rpdel is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%rpdel /= kgenref_var%rpdel), " of ", size( var%rpdel ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%rpdel)/real(size(var%rpdel)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%rpdel)/real(size(kgenref_var%rpdel)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_rpdel 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rpdel 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%rpdel /= kgenref_var%rpdel), " of ", size( var%rpdel ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%rpdel)/real(size(var%rpdel)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%rpdel)/real(size(kgenref_var%rpdel)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_rpdel 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rpdel 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%rpdeldry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%rpdeldry == kgenref_var%rpdeldry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rpdeldry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_rpdeldry(SIZE(var%rpdeldry,dim=1),SIZE(var%rpdeldry,dim=2))) 
              ALLOCATE (buf2_rpdeldry(SIZE(var%rpdeldry,dim=1),SIZE(var%rpdeldry,dim=2))) 
              n_rpdeldry = COUNT(var%rpdeldry /= kgenref_var%rpdeldry) 
              WHERE ( ABS(kgenref_var%rpdeldry) > kgen_minvalue ) 
                  buf1_rpdeldry = ((var%rpdeldry-kgenref_var%rpdeldry)/kgenref_var%rpdeldry)**2 
                  buf2_rpdeldry = (var%rpdeldry-kgenref_var%rpdeldry)**2 
              ELSEWHERE 
                  buf1_rpdeldry = (var%rpdeldry-kgenref_var%rpdeldry)**2 
                  buf2_rpdeldry = buf1_rpdeldry 
              END WHERE   
              nrmsdiff_rpdeldry = SQRT(SUM(buf1_rpdeldry)/REAL(n_rpdeldry)) 
              rmsdiff_rpdeldry = SQRT(SUM(buf2_rpdeldry)/REAL(n_rpdeldry)) 
              IF (rmsdiff_rpdeldry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%rpdeldry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%rpdeldry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%rpdeldry /= kgenref_var%rpdeldry), " of ", size( var%rpdeldry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%rpdeldry)/real(size(var%rpdeldry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%rpdeldry)/real(size(kgenref_var%rpdeldry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_rpdeldry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rpdeldry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%rpdeldry /= kgenref_var%rpdeldry), " of ", size( var%rpdeldry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%rpdeldry)/real(size(var%rpdeldry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%rpdeldry)/real(size(kgenref_var%rpdeldry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_rpdeldry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rpdeldry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%lnpmid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lnpmid == kgenref_var%lnpmid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lnpmid is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lnpmid(SIZE(var%lnpmid,dim=1),SIZE(var%lnpmid,dim=2))) 
              ALLOCATE (buf2_lnpmid(SIZE(var%lnpmid,dim=1),SIZE(var%lnpmid,dim=2))) 
              n_lnpmid = COUNT(var%lnpmid /= kgenref_var%lnpmid) 
              WHERE ( ABS(kgenref_var%lnpmid) > kgen_minvalue ) 
                  buf1_lnpmid = ((var%lnpmid-kgenref_var%lnpmid)/kgenref_var%lnpmid)**2 
                  buf2_lnpmid = (var%lnpmid-kgenref_var%lnpmid)**2 
              ELSEWHERE 
                  buf1_lnpmid = (var%lnpmid-kgenref_var%lnpmid)**2 
                  buf2_lnpmid = buf1_lnpmid 
              END WHERE   
              nrmsdiff_lnpmid = SQRT(SUM(buf1_lnpmid)/REAL(n_lnpmid)) 
              rmsdiff_lnpmid = SQRT(SUM(buf2_lnpmid)/REAL(n_lnpmid)) 
              IF (rmsdiff_lnpmid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpmid is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpmid is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lnpmid /= kgenref_var%lnpmid), " of ", size( var%lnpmid ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpmid)/real(size(var%lnpmid)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpmid)/real(size(kgenref_var%lnpmid)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpmid 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpmid 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lnpmid /= kgenref_var%lnpmid), " of ", size( var%lnpmid ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpmid)/real(size(var%lnpmid)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpmid)/real(size(kgenref_var%lnpmid)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpmid 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpmid 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%lnpmiddry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lnpmiddry == kgenref_var%lnpmiddry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lnpmiddry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lnpmiddry(SIZE(var%lnpmiddry,dim=1),SIZE(var%lnpmiddry,dim=2))) 
              ALLOCATE (buf2_lnpmiddry(SIZE(var%lnpmiddry,dim=1),SIZE(var%lnpmiddry,dim=2))) 
              n_lnpmiddry = COUNT(var%lnpmiddry /= kgenref_var%lnpmiddry) 
              WHERE ( ABS(kgenref_var%lnpmiddry) > kgen_minvalue ) 
                  buf1_lnpmiddry = ((var%lnpmiddry-kgenref_var%lnpmiddry)/kgenref_var%lnpmiddry)**2 
                  buf2_lnpmiddry = (var%lnpmiddry-kgenref_var%lnpmiddry)**2 
              ELSEWHERE 
                  buf1_lnpmiddry = (var%lnpmiddry-kgenref_var%lnpmiddry)**2 
                  buf2_lnpmiddry = buf1_lnpmiddry 
              END WHERE   
              nrmsdiff_lnpmiddry = SQRT(SUM(buf1_lnpmiddry)/REAL(n_lnpmiddry)) 
              rmsdiff_lnpmiddry = SQRT(SUM(buf2_lnpmiddry)/REAL(n_lnpmiddry)) 
              IF (rmsdiff_lnpmiddry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpmiddry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpmiddry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lnpmiddry /= kgenref_var%lnpmiddry), " of ", size( var%lnpmiddry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpmiddry)/real(size(var%lnpmiddry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpmiddry)/real(size(kgenref_var%lnpmiddry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpmiddry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpmiddry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lnpmiddry /= kgenref_var%lnpmiddry), " of ", size( var%lnpmiddry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpmiddry)/real(size(var%lnpmiddry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpmiddry)/real(size(kgenref_var%lnpmiddry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpmiddry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpmiddry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%exner)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%exner == kgenref_var%exner)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%exner is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_exner(SIZE(var%exner,dim=1),SIZE(var%exner,dim=2))) 
              ALLOCATE (buf2_exner(SIZE(var%exner,dim=1),SIZE(var%exner,dim=2))) 
              n_exner = COUNT(var%exner /= kgenref_var%exner) 
              WHERE ( ABS(kgenref_var%exner) > kgen_minvalue ) 
                  buf1_exner = ((var%exner-kgenref_var%exner)/kgenref_var%exner)**2 
                  buf2_exner = (var%exner-kgenref_var%exner)**2 
              ELSEWHERE 
                  buf1_exner = (var%exner-kgenref_var%exner)**2 
                  buf2_exner = buf1_exner 
              END WHERE   
              nrmsdiff_exner = SQRT(SUM(buf1_exner)/REAL(n_exner)) 
              rmsdiff_exner = SQRT(SUM(buf2_exner)/REAL(n_exner)) 
              IF (rmsdiff_exner > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%exner is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%exner is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%exner /= kgenref_var%exner), " of ", size( var%exner ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%exner)/real(size(var%exner)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%exner)/real(size(kgenref_var%exner)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_exner 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_exner 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%exner /= kgenref_var%exner), " of ", size( var%exner ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%exner)/real(size(var%exner)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%exner)/real(size(kgenref_var%exner)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_exner 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_exner 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%zm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%zm == kgenref_var%zm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%zm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_zm(SIZE(var%zm,dim=1),SIZE(var%zm,dim=2))) 
              ALLOCATE (buf2_zm(SIZE(var%zm,dim=1),SIZE(var%zm,dim=2))) 
              n_zm = COUNT(var%zm /= kgenref_var%zm) 
              WHERE ( ABS(kgenref_var%zm) > kgen_minvalue ) 
                  buf1_zm = ((var%zm-kgenref_var%zm)/kgenref_var%zm)**2 
                  buf2_zm = (var%zm-kgenref_var%zm)**2 
              ELSEWHERE 
                  buf1_zm = (var%zm-kgenref_var%zm)**2 
                  buf2_zm = buf1_zm 
              END WHERE   
              nrmsdiff_zm = SQRT(SUM(buf1_zm)/REAL(n_zm)) 
              rmsdiff_zm = SQRT(SUM(buf2_zm)/REAL(n_zm)) 
              IF (rmsdiff_zm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zm is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%zm /= kgenref_var%zm), " of ", size( var%zm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zm)/real(size(var%zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zm)/real(size(kgenref_var%zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zm /= kgenref_var%zm), " of ", size( var%zm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zm)/real(size(var%zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zm)/real(size(kgenref_var%zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%q)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%q == kgenref_var%q)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%q is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_q(SIZE(var%q,dim=1),SIZE(var%q,dim=2),SIZE(var%q,dim=3))) 
              ALLOCATE (buf2_q(SIZE(var%q,dim=1),SIZE(var%q,dim=2),SIZE(var%q,dim=3))) 
              n_q = COUNT(var%q /= kgenref_var%q) 
              WHERE ( ABS(kgenref_var%q) > kgen_minvalue ) 
                  buf1_q = ((var%q-kgenref_var%q)/kgenref_var%q)**2 
                  buf2_q = (var%q-kgenref_var%q)**2 
              ELSEWHERE 
                  buf1_q = (var%q-kgenref_var%q)**2 
                  buf2_q = buf1_q 
              END WHERE   
              nrmsdiff_q = SQRT(SUM(buf1_q)/REAL(n_q)) 
              rmsdiff_q = SQRT(SUM(buf2_q)/REAL(n_q)) 
              IF (rmsdiff_q > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%q is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%q is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%q /= kgenref_var%q), " of ", size( var%q ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%q)/real(size(var%q)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%q)/real(size(kgenref_var%q)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_q 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_q 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%q /= kgenref_var%q), " of ", size( var%q ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%q)/real(size(var%q)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%q)/real(size(kgenref_var%q)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_q 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_q 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%pint)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%pint == kgenref_var%pint)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%pint is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_pint(SIZE(var%pint,dim=1),SIZE(var%pint,dim=2))) 
              ALLOCATE (buf2_pint(SIZE(var%pint,dim=1),SIZE(var%pint,dim=2))) 
              n_pint = COUNT(var%pint /= kgenref_var%pint) 
              WHERE ( ABS(kgenref_var%pint) > kgen_minvalue ) 
                  buf1_pint = ((var%pint-kgenref_var%pint)/kgenref_var%pint)**2 
                  buf2_pint = (var%pint-kgenref_var%pint)**2 
              ELSEWHERE 
                  buf1_pint = (var%pint-kgenref_var%pint)**2 
                  buf2_pint = buf1_pint 
              END WHERE   
              nrmsdiff_pint = SQRT(SUM(buf1_pint)/REAL(n_pint)) 
              rmsdiff_pint = SQRT(SUM(buf2_pint)/REAL(n_pint)) 
              IF (rmsdiff_pint > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pint is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pint is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%pint /= kgenref_var%pint), " of ", size( var%pint ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pint)/real(size(var%pint)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pint)/real(size(kgenref_var%pint)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pint 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pint 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%pint /= kgenref_var%pint), " of ", size( var%pint ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pint)/real(size(var%pint)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pint)/real(size(kgenref_var%pint)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pint 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pint 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%pintdry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%pintdry == kgenref_var%pintdry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%pintdry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_pintdry(SIZE(var%pintdry,dim=1),SIZE(var%pintdry,dim=2))) 
              ALLOCATE (buf2_pintdry(SIZE(var%pintdry,dim=1),SIZE(var%pintdry,dim=2))) 
              n_pintdry = COUNT(var%pintdry /= kgenref_var%pintdry) 
              WHERE ( ABS(kgenref_var%pintdry) > kgen_minvalue ) 
                  buf1_pintdry = ((var%pintdry-kgenref_var%pintdry)/kgenref_var%pintdry)**2 
                  buf2_pintdry = (var%pintdry-kgenref_var%pintdry)**2 
              ELSEWHERE 
                  buf1_pintdry = (var%pintdry-kgenref_var%pintdry)**2 
                  buf2_pintdry = buf1_pintdry 
              END WHERE   
              nrmsdiff_pintdry = SQRT(SUM(buf1_pintdry)/REAL(n_pintdry)) 
              rmsdiff_pintdry = SQRT(SUM(buf2_pintdry)/REAL(n_pintdry)) 
              IF (rmsdiff_pintdry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pintdry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%pintdry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%pintdry /= kgenref_var%pintdry), " of ", size( var%pintdry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pintdry)/real(size(var%pintdry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pintdry)/real(size(kgenref_var%pintdry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pintdry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pintdry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%pintdry /= kgenref_var%pintdry), " of ", size( var%pintdry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%pintdry)/real(size(var%pintdry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%pintdry)/real(size(kgenref_var%pintdry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_pintdry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pintdry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%lnpint)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lnpint == kgenref_var%lnpint)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lnpint is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lnpint(SIZE(var%lnpint,dim=1),SIZE(var%lnpint,dim=2))) 
              ALLOCATE (buf2_lnpint(SIZE(var%lnpint,dim=1),SIZE(var%lnpint,dim=2))) 
              n_lnpint = COUNT(var%lnpint /= kgenref_var%lnpint) 
              WHERE ( ABS(kgenref_var%lnpint) > kgen_minvalue ) 
                  buf1_lnpint = ((var%lnpint-kgenref_var%lnpint)/kgenref_var%lnpint)**2 
                  buf2_lnpint = (var%lnpint-kgenref_var%lnpint)**2 
              ELSEWHERE 
                  buf1_lnpint = (var%lnpint-kgenref_var%lnpint)**2 
                  buf2_lnpint = buf1_lnpint 
              END WHERE   
              nrmsdiff_lnpint = SQRT(SUM(buf1_lnpint)/REAL(n_lnpint)) 
              rmsdiff_lnpint = SQRT(SUM(buf2_lnpint)/REAL(n_lnpint)) 
              IF (rmsdiff_lnpint > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpint is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpint is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lnpint /= kgenref_var%lnpint), " of ", size( var%lnpint ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpint)/real(size(var%lnpint)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpint)/real(size(kgenref_var%lnpint)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpint 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpint 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lnpint /= kgenref_var%lnpint), " of ", size( var%lnpint ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpint)/real(size(var%lnpint)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpint)/real(size(kgenref_var%lnpint)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpint 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpint 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%lnpintdry)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lnpintdry == kgenref_var%lnpintdry)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lnpintdry is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lnpintdry(SIZE(var%lnpintdry,dim=1),SIZE(var%lnpintdry,dim=2))) 
              ALLOCATE (buf2_lnpintdry(SIZE(var%lnpintdry,dim=1),SIZE(var%lnpintdry,dim=2))) 
              n_lnpintdry = COUNT(var%lnpintdry /= kgenref_var%lnpintdry) 
              WHERE ( ABS(kgenref_var%lnpintdry) > kgen_minvalue ) 
                  buf1_lnpintdry = ((var%lnpintdry-kgenref_var%lnpintdry)/kgenref_var%lnpintdry)**2 
                  buf2_lnpintdry = (var%lnpintdry-kgenref_var%lnpintdry)**2 
              ELSEWHERE 
                  buf1_lnpintdry = (var%lnpintdry-kgenref_var%lnpintdry)**2 
                  buf2_lnpintdry = buf1_lnpintdry 
              END WHERE   
              nrmsdiff_lnpintdry = SQRT(SUM(buf1_lnpintdry)/REAL(n_lnpintdry)) 
              rmsdiff_lnpintdry = SQRT(SUM(buf2_lnpintdry)/REAL(n_lnpintdry)) 
              IF (rmsdiff_lnpintdry > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpintdry is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lnpintdry is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lnpintdry /= kgenref_var%lnpintdry), " of ", size( var%lnpintdry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpintdry)/real(size(var%lnpintdry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpintdry)/real(size(kgenref_var%lnpintdry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpintdry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpintdry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lnpintdry /= kgenref_var%lnpintdry), " of ", size( var%lnpintdry ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lnpintdry)/real(size(var%lnpintdry)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lnpintdry)/real(size(kgenref_var%lnpintdry)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lnpintdry 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lnpintdry 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%zi)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%zi == kgenref_var%zi)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%zi is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_zi(SIZE(var%zi,dim=1),SIZE(var%zi,dim=2))) 
              ALLOCATE (buf2_zi(SIZE(var%zi,dim=1),SIZE(var%zi,dim=2))) 
              n_zi = COUNT(var%zi /= kgenref_var%zi) 
              WHERE ( ABS(kgenref_var%zi) > kgen_minvalue ) 
                  buf1_zi = ((var%zi-kgenref_var%zi)/kgenref_var%zi)**2 
                  buf2_zi = (var%zi-kgenref_var%zi)**2 
              ELSEWHERE 
                  buf1_zi = (var%zi-kgenref_var%zi)**2 
                  buf2_zi = buf1_zi 
              END WHERE   
              nrmsdiff_zi = SQRT(SUM(buf1_zi)/REAL(n_zi)) 
              rmsdiff_zi = SQRT(SUM(buf2_zi)/REAL(n_zi)) 
              IF (rmsdiff_zi > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zi is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zi is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%zi /= kgenref_var%zi), " of ", size( var%zi ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zi)/real(size(var%zi)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zi)/real(size(kgenref_var%zi)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zi 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zi 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zi /= kgenref_var%zi), " of ", size( var%zi ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zi)/real(size(var%zi)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zi)/real(size(kgenref_var%zi)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zi 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zi 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%te_ini)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%te_ini == kgenref_var%te_ini)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%te_ini is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_te_ini(SIZE(var%te_ini,dim=1))) 
              ALLOCATE (buf2_te_ini(SIZE(var%te_ini,dim=1))) 
              n_te_ini = COUNT(var%te_ini /= kgenref_var%te_ini) 
              WHERE ( ABS(kgenref_var%te_ini) > kgen_minvalue ) 
                  buf1_te_ini = ((var%te_ini-kgenref_var%te_ini)/kgenref_var%te_ini)**2 
                  buf2_te_ini = (var%te_ini-kgenref_var%te_ini)**2 
              ELSEWHERE 
                  buf1_te_ini = (var%te_ini-kgenref_var%te_ini)**2 
                  buf2_te_ini = buf1_te_ini 
              END WHERE   
              nrmsdiff_te_ini = SQRT(SUM(buf1_te_ini)/REAL(n_te_ini)) 
              rmsdiff_te_ini = SQRT(SUM(buf2_te_ini)/REAL(n_te_ini)) 
              IF (rmsdiff_te_ini > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%te_ini is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%te_ini is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%te_ini /= kgenref_var%te_ini), " of ", size( var%te_ini ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%te_ini)/real(size(var%te_ini)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%te_ini)/real(size(kgenref_var%te_ini)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_te_ini 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_te_ini 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%te_ini /= kgenref_var%te_ini), " of ", size( var%te_ini ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%te_ini)/real(size(var%te_ini)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%te_ini)/real(size(kgenref_var%te_ini)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_te_ini 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_te_ini 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%te_cur)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%te_cur == kgenref_var%te_cur)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%te_cur is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_te_cur(SIZE(var%te_cur,dim=1))) 
              ALLOCATE (buf2_te_cur(SIZE(var%te_cur,dim=1))) 
              n_te_cur = COUNT(var%te_cur /= kgenref_var%te_cur) 
              WHERE ( ABS(kgenref_var%te_cur) > kgen_minvalue ) 
                  buf1_te_cur = ((var%te_cur-kgenref_var%te_cur)/kgenref_var%te_cur)**2 
                  buf2_te_cur = (var%te_cur-kgenref_var%te_cur)**2 
              ELSEWHERE 
                  buf1_te_cur = (var%te_cur-kgenref_var%te_cur)**2 
                  buf2_te_cur = buf1_te_cur 
              END WHERE   
              nrmsdiff_te_cur = SQRT(SUM(buf1_te_cur)/REAL(n_te_cur)) 
              rmsdiff_te_cur = SQRT(SUM(buf2_te_cur)/REAL(n_te_cur)) 
              IF (rmsdiff_te_cur > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%te_cur is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%te_cur is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%te_cur /= kgenref_var%te_cur), " of ", size( var%te_cur ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%te_cur)/real(size(var%te_cur)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%te_cur)/real(size(kgenref_var%te_cur)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_te_cur 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_te_cur 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%te_cur /= kgenref_var%te_cur), " of ", size( var%te_cur ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%te_cur)/real(size(var%te_cur)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%te_cur)/real(size(kgenref_var%te_cur)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_te_cur 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_te_cur 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%tw_ini)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%tw_ini == kgenref_var%tw_ini)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tw_ini is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_tw_ini(SIZE(var%tw_ini,dim=1))) 
              ALLOCATE (buf2_tw_ini(SIZE(var%tw_ini,dim=1))) 
              n_tw_ini = COUNT(var%tw_ini /= kgenref_var%tw_ini) 
              WHERE ( ABS(kgenref_var%tw_ini) > kgen_minvalue ) 
                  buf1_tw_ini = ((var%tw_ini-kgenref_var%tw_ini)/kgenref_var%tw_ini)**2 
                  buf2_tw_ini = (var%tw_ini-kgenref_var%tw_ini)**2 
              ELSEWHERE 
                  buf1_tw_ini = (var%tw_ini-kgenref_var%tw_ini)**2 
                  buf2_tw_ini = buf1_tw_ini 
              END WHERE   
              nrmsdiff_tw_ini = SQRT(SUM(buf1_tw_ini)/REAL(n_tw_ini)) 
              rmsdiff_tw_ini = SQRT(SUM(buf2_tw_ini)/REAL(n_tw_ini)) 
              IF (rmsdiff_tw_ini > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tw_ini is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tw_ini is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%tw_ini /= kgenref_var%tw_ini), " of ", size( var%tw_ini ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tw_ini)/real(size(var%tw_ini)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tw_ini)/real(size(kgenref_var%tw_ini)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tw_ini 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tw_ini 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%tw_ini /= kgenref_var%tw_ini), " of ", size( var%tw_ini ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tw_ini)/real(size(var%tw_ini)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tw_ini)/real(size(kgenref_var%tw_ini)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tw_ini 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tw_ini 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%tw_cur)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%tw_cur == kgenref_var%tw_cur)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tw_cur is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_tw_cur(SIZE(var%tw_cur,dim=1))) 
              ALLOCATE (buf2_tw_cur(SIZE(var%tw_cur,dim=1))) 
              n_tw_cur = COUNT(var%tw_cur /= kgenref_var%tw_cur) 
              WHERE ( ABS(kgenref_var%tw_cur) > kgen_minvalue ) 
                  buf1_tw_cur = ((var%tw_cur-kgenref_var%tw_cur)/kgenref_var%tw_cur)**2 
                  buf2_tw_cur = (var%tw_cur-kgenref_var%tw_cur)**2 
              ELSEWHERE 
                  buf1_tw_cur = (var%tw_cur-kgenref_var%tw_cur)**2 
                  buf2_tw_cur = buf1_tw_cur 
              END WHERE   
              nrmsdiff_tw_cur = SQRT(SUM(buf1_tw_cur)/REAL(n_tw_cur)) 
              rmsdiff_tw_cur = SQRT(SUM(buf2_tw_cur)/REAL(n_tw_cur)) 
              IF (rmsdiff_tw_cur > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tw_cur is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tw_cur is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%tw_cur /= kgenref_var%tw_cur), " of ", size( var%tw_cur ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tw_cur)/real(size(var%tw_cur)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tw_cur)/real(size(kgenref_var%tw_cur)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tw_cur 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tw_cur 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%tw_cur /= kgenref_var%tw_cur), " of ", size( var%tw_cur ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tw_cur)/real(size(var%tw_cur)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tw_cur)/real(size(kgenref_var%tw_cur)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tw_cur 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tw_cur 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%count == kgenref_var%count) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%count is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_count = ABS(var%count - kgenref_var%count) 
          IF (diff_count <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%count is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%count is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_count 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_count 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ALLOCATED(var%latmapback)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%latmapback == kgenref_var%latmapback)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%latmapback is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_latmapback(SIZE(var%latmapback,dim=1))) 
              ALLOCATE (buf2_latmapback(SIZE(var%latmapback,dim=1))) 
              n_latmapback = COUNT(var%latmapback /= kgenref_var%latmapback) 
              WHERE ( ABS(kgenref_var%latmapback) > kgen_minvalue ) 
                  buf1_latmapback = ((var%latmapback-kgenref_var%latmapback)/kgenref_var%latmapback)**2 
                  buf2_latmapback = (var%latmapback-kgenref_var%latmapback)**2 
              ELSEWHERE 
                  buf1_latmapback = (var%latmapback-kgenref_var%latmapback)**2 
                  buf2_latmapback = buf1_latmapback 
              END WHERE   
              nrmsdiff_latmapback = SQRT(SUM(buf1_latmapback)/REAL(n_latmapback)) 
              rmsdiff_latmapback = SQRT(SUM(buf2_latmapback)/REAL(n_latmapback)) 
              IF (rmsdiff_latmapback > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%latmapback is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%latmapback is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%latmapback /= kgenref_var%latmapback), " of ", size( var%latmapback ), " elements &
                      &are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%latmapback)/real(size(var%latmapback)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%latmapback)/real(size(kgenref_var%latmapback)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_latmapback 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_latmapback 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%latmapback /= kgenref_var%latmapback), " of ", size( var%latmapback ), " elements &
                      &are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%latmapback)/real(size(var%latmapback)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%latmapback)/real(size(kgenref_var%latmapback)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_latmapback 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_latmapback 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%lonmapback)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%lonmapback == kgenref_var%lonmapback)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%lonmapback is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_lonmapback(SIZE(var%lonmapback,dim=1))) 
              ALLOCATE (buf2_lonmapback(SIZE(var%lonmapback,dim=1))) 
              n_lonmapback = COUNT(var%lonmapback /= kgenref_var%lonmapback) 
              WHERE ( ABS(kgenref_var%lonmapback) > kgen_minvalue ) 
                  buf1_lonmapback = ((var%lonmapback-kgenref_var%lonmapback)/kgenref_var%lonmapback)**2 
                  buf2_lonmapback = (var%lonmapback-kgenref_var%lonmapback)**2 
              ELSEWHERE 
                  buf1_lonmapback = (var%lonmapback-kgenref_var%lonmapback)**2 
                  buf2_lonmapback = buf1_lonmapback 
              END WHERE   
              nrmsdiff_lonmapback = SQRT(SUM(buf1_lonmapback)/REAL(n_lonmapback)) 
              rmsdiff_lonmapback = SQRT(SUM(buf2_lonmapback)/REAL(n_lonmapback)) 
              IF (rmsdiff_lonmapback > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lonmapback is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%lonmapback is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%lonmapback /= kgenref_var%lonmapback), " of ", size( var%lonmapback ), " elements &
                      &are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lonmapback)/real(size(var%lonmapback)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lonmapback)/real(size(kgenref_var%lonmapback)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lonmapback 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lonmapback 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%lonmapback /= kgenref_var%lonmapback), " of ", size( var%lonmapback ), " elements &
                      &are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%lonmapback)/real(size(var%lonmapback)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%lonmapback)/real(size(kgenref_var%lonmapback)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_lonmapback 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lonmapback 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%cid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%cid == kgenref_var%cid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cid is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_cid(SIZE(var%cid,dim=1))) 
              ALLOCATE (buf2_cid(SIZE(var%cid,dim=1))) 
              n_cid = COUNT(var%cid /= kgenref_var%cid) 
              WHERE ( ABS(kgenref_var%cid) > kgen_minvalue ) 
                  buf1_cid = ((var%cid-kgenref_var%cid)/kgenref_var%cid)**2 
                  buf2_cid = (var%cid-kgenref_var%cid)**2 
              ELSEWHERE 
                  buf1_cid = (var%cid-kgenref_var%cid)**2 
                  buf2_cid = buf1_cid 
              END WHERE   
              nrmsdiff_cid = SQRT(SUM(buf1_cid)/REAL(n_cid)) 
              rmsdiff_cid = SQRT(SUM(buf2_cid)/REAL(n_cid)) 
              IF (rmsdiff_cid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%cid is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%cid is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%cid /= kgenref_var%cid), " of ", size( var%cid ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%cid)/real(size(var%cid)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%cid)/real(size(kgenref_var%cid)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_cid 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cid 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%cid /= kgenref_var%cid), " of ", size( var%cid ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%cid)/real(size(var%cid)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%cid)/real(size(kgenref_var%cid)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_cid 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cid 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ulatcnt == kgenref_var%ulatcnt) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ulatcnt is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ulatcnt = ABS(var%ulatcnt - kgenref_var%ulatcnt) 
          IF (diff_ulatcnt <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ulatcnt is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ulatcnt is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_ulatcnt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ulatcnt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%uloncnt == kgenref_var%uloncnt) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%uloncnt is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_uloncnt = ABS(var%uloncnt - kgenref_var%uloncnt) 
          IF (diff_uloncnt <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%uloncnt is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%uloncnt is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_uloncnt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_uloncnt 
                  WRITE (*, *) "" 
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
  END SUBROUTINE kv_physics_types_physics_state 
    
  !verify state subroutine for kv_physics_types_physics_ptend 
  RECURSIVE SUBROUTINE kv_physics_types_physics_ptend(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(physics_ptend), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_psetcols 
      character(LEN=24) :: diff_name 
      logical :: diff_ls 
      logical :: diff_lu 
      logical :: diff_lv 
      INTEGER :: n_lq 
      integer :: diff_top_level 
      integer :: diff_bot_level 
      INTEGER :: n_s 
      real(KIND=r8) :: nrmsdiff_s, rmsdiff_s 
      real(KIND=r8), ALLOCATABLE :: buf1_s(:,:), buf2_s(:,:) 
      INTEGER :: n_u 
      real(KIND=r8) :: nrmsdiff_u, rmsdiff_u 
      real(KIND=r8), ALLOCATABLE :: buf1_u(:,:), buf2_u(:,:) 
      INTEGER :: n_v 
      real(KIND=r8) :: nrmsdiff_v, rmsdiff_v 
      real(KIND=r8), ALLOCATABLE :: buf1_v(:,:), buf2_v(:,:) 
      INTEGER :: n_q 
      real(KIND=r8) :: nrmsdiff_q, rmsdiff_q 
      real(KIND=r8), ALLOCATABLE :: buf1_q(:,:,:), buf2_q(:,:,:) 
      INTEGER :: n_hflux_srf 
      real(KIND=r8) :: nrmsdiff_hflux_srf, rmsdiff_hflux_srf 
      real(KIND=r8), ALLOCATABLE :: buf1_hflux_srf(:), buf2_hflux_srf(:) 
      INTEGER :: n_hflux_top 
      real(KIND=r8) :: nrmsdiff_hflux_top, rmsdiff_hflux_top 
      real(KIND=r8), ALLOCATABLE :: buf1_hflux_top(:), buf2_hflux_top(:) 
      INTEGER :: n_taux_srf 
      real(KIND=r8) :: nrmsdiff_taux_srf, rmsdiff_taux_srf 
      real(KIND=r8), ALLOCATABLE :: buf1_taux_srf(:), buf2_taux_srf(:) 
      INTEGER :: n_taux_top 
      real(KIND=r8) :: nrmsdiff_taux_top, rmsdiff_taux_top 
      real(KIND=r8), ALLOCATABLE :: buf1_taux_top(:), buf2_taux_top(:) 
      INTEGER :: n_tauy_srf 
      real(KIND=r8) :: nrmsdiff_tauy_srf, rmsdiff_tauy_srf 
      real(KIND=r8), ALLOCATABLE :: buf1_tauy_srf(:), buf2_tauy_srf(:) 
      INTEGER :: n_tauy_top 
      real(KIND=r8) :: nrmsdiff_tauy_top, rmsdiff_tauy_top 
      real(KIND=r8), ALLOCATABLE :: buf1_tauy_top(:), buf2_tauy_top(:) 
      INTEGER :: n_cflx_srf 
      real(KIND=r8) :: nrmsdiff_cflx_srf, rmsdiff_cflx_srf 
      real(KIND=r8), ALLOCATABLE :: buf1_cflx_srf(:,:), buf2_cflx_srf(:,:) 
      INTEGER :: n_cflx_top 
      real(KIND=r8) :: nrmsdiff_cflx_top, rmsdiff_cflx_top 
      real(KIND=r8), ALLOCATABLE :: buf1_cflx_top(:,:), buf2_cflx_top(:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%psetcols == kgenref_var%psetcols) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%psetcols is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_psetcols = ABS(var%psetcols - kgenref_var%psetcols) 
          IF (diff_psetcols <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%psetcols is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%psetcols is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_psetcols 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_psetcols 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%name == kgenref_var%name) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%name is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%name is NOT IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ls .EQV. kgenref_var%ls) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ls is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ls is NOT IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%lu .EQV. kgenref_var%lu) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lu is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lu is NOT IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%lv .EQV. kgenref_var%lv) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lv is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lv is NOT IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%lq .EQV. kgenref_var%lq)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lq is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          n_lq = COUNT(var%lq .NEQV. kgenref_var%lq) 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lq is NOT IDENTICAL(out of tolerance)." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%top_level == kgenref_var%top_level) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%top_level is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_top_level = ABS(var%top_level - kgenref_var%top_level) 
          IF (diff_top_level <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%top_level is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%top_level is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_top_level 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_top_level 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%bot_level == kgenref_var%bot_level) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%bot_level is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_bot_level = ABS(var%bot_level - kgenref_var%bot_level) 
          IF (diff_bot_level <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%bot_level is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%bot_level is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_bot_level 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_bot_level 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ALLOCATED(var%s)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%s == kgenref_var%s)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%s is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_s(SIZE(var%s,dim=1),SIZE(var%s,dim=2))) 
              ALLOCATE (buf2_s(SIZE(var%s,dim=1),SIZE(var%s,dim=2))) 
              n_s = COUNT(var%s /= kgenref_var%s) 
              WHERE ( ABS(kgenref_var%s) > kgen_minvalue ) 
                  buf1_s = ((var%s-kgenref_var%s)/kgenref_var%s)**2 
                  buf2_s = (var%s-kgenref_var%s)**2 
              ELSEWHERE 
                  buf1_s = (var%s-kgenref_var%s)**2 
                  buf2_s = buf1_s 
              END WHERE   
              nrmsdiff_s = SQRT(SUM(buf1_s)/REAL(n_s)) 
              rmsdiff_s = SQRT(SUM(buf2_s)/REAL(n_s)) 
              IF (rmsdiff_s > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%s is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%s /= kgenref_var%s), " of ", size( var%s ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%s)/real(size(var%s)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%s)/real(size(kgenref_var%s)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_s 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_s 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%s /= kgenref_var%s), " of ", size( var%s ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%s)/real(size(var%s)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%s)/real(size(kgenref_var%s)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_s 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_s 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%u)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%u == kgenref_var%u)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%u is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_u(SIZE(var%u,dim=1),SIZE(var%u,dim=2))) 
              ALLOCATE (buf2_u(SIZE(var%u,dim=1),SIZE(var%u,dim=2))) 
              n_u = COUNT(var%u /= kgenref_var%u) 
              WHERE ( ABS(kgenref_var%u) > kgen_minvalue ) 
                  buf1_u = ((var%u-kgenref_var%u)/kgenref_var%u)**2 
                  buf2_u = (var%u-kgenref_var%u)**2 
              ELSEWHERE 
                  buf1_u = (var%u-kgenref_var%u)**2 
                  buf2_u = buf1_u 
              END WHERE   
              nrmsdiff_u = SQRT(SUM(buf1_u)/REAL(n_u)) 
              rmsdiff_u = SQRT(SUM(buf2_u)/REAL(n_u)) 
              IF (rmsdiff_u > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%u is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%u is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%u /= kgenref_var%u), " of ", size( var%u ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%u)/real(size(var%u)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%u)/real(size(kgenref_var%u)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_u 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%u /= kgenref_var%u), " of ", size( var%u ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%u)/real(size(var%u)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%u)/real(size(kgenref_var%u)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_u 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%v)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%v == kgenref_var%v)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%v is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_v(SIZE(var%v,dim=1),SIZE(var%v,dim=2))) 
              ALLOCATE (buf2_v(SIZE(var%v,dim=1),SIZE(var%v,dim=2))) 
              n_v = COUNT(var%v /= kgenref_var%v) 
              WHERE ( ABS(kgenref_var%v) > kgen_minvalue ) 
                  buf1_v = ((var%v-kgenref_var%v)/kgenref_var%v)**2 
                  buf2_v = (var%v-kgenref_var%v)**2 
              ELSEWHERE 
                  buf1_v = (var%v-kgenref_var%v)**2 
                  buf2_v = buf1_v 
              END WHERE   
              nrmsdiff_v = SQRT(SUM(buf1_v)/REAL(n_v)) 
              rmsdiff_v = SQRT(SUM(buf2_v)/REAL(n_v)) 
              IF (rmsdiff_v > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%v is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%v is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%v /= kgenref_var%v), " of ", size( var%v ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%v)/real(size(var%v)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%v)/real(size(kgenref_var%v)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_v 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%v /= kgenref_var%v), " of ", size( var%v ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%v)/real(size(var%v)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%v)/real(size(kgenref_var%v)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_v 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%q)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%q == kgenref_var%q)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%q is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_q(SIZE(var%q,dim=1),SIZE(var%q,dim=2),SIZE(var%q,dim=3))) 
              ALLOCATE (buf2_q(SIZE(var%q,dim=1),SIZE(var%q,dim=2),SIZE(var%q,dim=3))) 
              n_q = COUNT(var%q /= kgenref_var%q) 
              WHERE ( ABS(kgenref_var%q) > kgen_minvalue ) 
                  buf1_q = ((var%q-kgenref_var%q)/kgenref_var%q)**2 
                  buf2_q = (var%q-kgenref_var%q)**2 
              ELSEWHERE 
                  buf1_q = (var%q-kgenref_var%q)**2 
                  buf2_q = buf1_q 
              END WHERE   
              nrmsdiff_q = SQRT(SUM(buf1_q)/REAL(n_q)) 
              rmsdiff_q = SQRT(SUM(buf2_q)/REAL(n_q)) 
              IF (rmsdiff_q > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%q is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%q is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%q /= kgenref_var%q), " of ", size( var%q ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%q)/real(size(var%q)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%q)/real(size(kgenref_var%q)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_q 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_q 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%q /= kgenref_var%q), " of ", size( var%q ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%q)/real(size(var%q)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%q)/real(size(kgenref_var%q)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_q 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_q 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%hflux_srf)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%hflux_srf == kgenref_var%hflux_srf)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%hflux_srf is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_hflux_srf(SIZE(var%hflux_srf,dim=1))) 
              ALLOCATE (buf2_hflux_srf(SIZE(var%hflux_srf,dim=1))) 
              n_hflux_srf = COUNT(var%hflux_srf /= kgenref_var%hflux_srf) 
              WHERE ( ABS(kgenref_var%hflux_srf) > kgen_minvalue ) 
                  buf1_hflux_srf = ((var%hflux_srf-kgenref_var%hflux_srf)/kgenref_var%hflux_srf)**2 
                  buf2_hflux_srf = (var%hflux_srf-kgenref_var%hflux_srf)**2 
              ELSEWHERE 
                  buf1_hflux_srf = (var%hflux_srf-kgenref_var%hflux_srf)**2 
                  buf2_hflux_srf = buf1_hflux_srf 
              END WHERE   
              nrmsdiff_hflux_srf = SQRT(SUM(buf1_hflux_srf)/REAL(n_hflux_srf)) 
              rmsdiff_hflux_srf = SQRT(SUM(buf2_hflux_srf)/REAL(n_hflux_srf)) 
              IF (rmsdiff_hflux_srf > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%hflux_srf is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%hflux_srf is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%hflux_srf /= kgenref_var%hflux_srf), " of ", size( var%hflux_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%hflux_srf)/real(size(var%hflux_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%hflux_srf)/real(size(kgenref_var%hflux_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_hflux_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hflux_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%hflux_srf /= kgenref_var%hflux_srf), " of ", size( var%hflux_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%hflux_srf)/real(size(var%hflux_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%hflux_srf)/real(size(kgenref_var%hflux_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_hflux_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hflux_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%hflux_top)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%hflux_top == kgenref_var%hflux_top)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%hflux_top is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_hflux_top(SIZE(var%hflux_top,dim=1))) 
              ALLOCATE (buf2_hflux_top(SIZE(var%hflux_top,dim=1))) 
              n_hflux_top = COUNT(var%hflux_top /= kgenref_var%hflux_top) 
              WHERE ( ABS(kgenref_var%hflux_top) > kgen_minvalue ) 
                  buf1_hflux_top = ((var%hflux_top-kgenref_var%hflux_top)/kgenref_var%hflux_top)**2 
                  buf2_hflux_top = (var%hflux_top-kgenref_var%hflux_top)**2 
              ELSEWHERE 
                  buf1_hflux_top = (var%hflux_top-kgenref_var%hflux_top)**2 
                  buf2_hflux_top = buf1_hflux_top 
              END WHERE   
              nrmsdiff_hflux_top = SQRT(SUM(buf1_hflux_top)/REAL(n_hflux_top)) 
              rmsdiff_hflux_top = SQRT(SUM(buf2_hflux_top)/REAL(n_hflux_top)) 
              IF (rmsdiff_hflux_top > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%hflux_top is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%hflux_top is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%hflux_top /= kgenref_var%hflux_top), " of ", size( var%hflux_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%hflux_top)/real(size(var%hflux_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%hflux_top)/real(size(kgenref_var%hflux_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_hflux_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hflux_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%hflux_top /= kgenref_var%hflux_top), " of ", size( var%hflux_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%hflux_top)/real(size(var%hflux_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%hflux_top)/real(size(kgenref_var%hflux_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_hflux_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_hflux_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%taux_srf)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%taux_srf == kgenref_var%taux_srf)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%taux_srf is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_taux_srf(SIZE(var%taux_srf,dim=1))) 
              ALLOCATE (buf2_taux_srf(SIZE(var%taux_srf,dim=1))) 
              n_taux_srf = COUNT(var%taux_srf /= kgenref_var%taux_srf) 
              WHERE ( ABS(kgenref_var%taux_srf) > kgen_minvalue ) 
                  buf1_taux_srf = ((var%taux_srf-kgenref_var%taux_srf)/kgenref_var%taux_srf)**2 
                  buf2_taux_srf = (var%taux_srf-kgenref_var%taux_srf)**2 
              ELSEWHERE 
                  buf1_taux_srf = (var%taux_srf-kgenref_var%taux_srf)**2 
                  buf2_taux_srf = buf1_taux_srf 
              END WHERE   
              nrmsdiff_taux_srf = SQRT(SUM(buf1_taux_srf)/REAL(n_taux_srf)) 
              rmsdiff_taux_srf = SQRT(SUM(buf2_taux_srf)/REAL(n_taux_srf)) 
              IF (rmsdiff_taux_srf > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%taux_srf is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%taux_srf is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%taux_srf /= kgenref_var%taux_srf), " of ", size( var%taux_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%taux_srf)/real(size(var%taux_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%taux_srf)/real(size(kgenref_var%taux_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_taux_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_taux_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%taux_srf /= kgenref_var%taux_srf), " of ", size( var%taux_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%taux_srf)/real(size(var%taux_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%taux_srf)/real(size(kgenref_var%taux_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_taux_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_taux_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%taux_top)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%taux_top == kgenref_var%taux_top)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%taux_top is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_taux_top(SIZE(var%taux_top,dim=1))) 
              ALLOCATE (buf2_taux_top(SIZE(var%taux_top,dim=1))) 
              n_taux_top = COUNT(var%taux_top /= kgenref_var%taux_top) 
              WHERE ( ABS(kgenref_var%taux_top) > kgen_minvalue ) 
                  buf1_taux_top = ((var%taux_top-kgenref_var%taux_top)/kgenref_var%taux_top)**2 
                  buf2_taux_top = (var%taux_top-kgenref_var%taux_top)**2 
              ELSEWHERE 
                  buf1_taux_top = (var%taux_top-kgenref_var%taux_top)**2 
                  buf2_taux_top = buf1_taux_top 
              END WHERE   
              nrmsdiff_taux_top = SQRT(SUM(buf1_taux_top)/REAL(n_taux_top)) 
              rmsdiff_taux_top = SQRT(SUM(buf2_taux_top)/REAL(n_taux_top)) 
              IF (rmsdiff_taux_top > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%taux_top is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%taux_top is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%taux_top /= kgenref_var%taux_top), " of ", size( var%taux_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%taux_top)/real(size(var%taux_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%taux_top)/real(size(kgenref_var%taux_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_taux_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_taux_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%taux_top /= kgenref_var%taux_top), " of ", size( var%taux_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%taux_top)/real(size(var%taux_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%taux_top)/real(size(kgenref_var%taux_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_taux_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_taux_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%tauy_srf)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%tauy_srf == kgenref_var%tauy_srf)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tauy_srf is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_tauy_srf(SIZE(var%tauy_srf,dim=1))) 
              ALLOCATE (buf2_tauy_srf(SIZE(var%tauy_srf,dim=1))) 
              n_tauy_srf = COUNT(var%tauy_srf /= kgenref_var%tauy_srf) 
              WHERE ( ABS(kgenref_var%tauy_srf) > kgen_minvalue ) 
                  buf1_tauy_srf = ((var%tauy_srf-kgenref_var%tauy_srf)/kgenref_var%tauy_srf)**2 
                  buf2_tauy_srf = (var%tauy_srf-kgenref_var%tauy_srf)**2 
              ELSEWHERE 
                  buf1_tauy_srf = (var%tauy_srf-kgenref_var%tauy_srf)**2 
                  buf2_tauy_srf = buf1_tauy_srf 
              END WHERE   
              nrmsdiff_tauy_srf = SQRT(SUM(buf1_tauy_srf)/REAL(n_tauy_srf)) 
              rmsdiff_tauy_srf = SQRT(SUM(buf2_tauy_srf)/REAL(n_tauy_srf)) 
              IF (rmsdiff_tauy_srf > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tauy_srf is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tauy_srf is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%tauy_srf /= kgenref_var%tauy_srf), " of ", size( var%tauy_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tauy_srf)/real(size(var%tauy_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tauy_srf)/real(size(kgenref_var%tauy_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tauy_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tauy_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%tauy_srf /= kgenref_var%tauy_srf), " of ", size( var%tauy_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tauy_srf)/real(size(var%tauy_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tauy_srf)/real(size(kgenref_var%tauy_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tauy_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tauy_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%tauy_top)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%tauy_top == kgenref_var%tauy_top)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tauy_top is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_tauy_top(SIZE(var%tauy_top,dim=1))) 
              ALLOCATE (buf2_tauy_top(SIZE(var%tauy_top,dim=1))) 
              n_tauy_top = COUNT(var%tauy_top /= kgenref_var%tauy_top) 
              WHERE ( ABS(kgenref_var%tauy_top) > kgen_minvalue ) 
                  buf1_tauy_top = ((var%tauy_top-kgenref_var%tauy_top)/kgenref_var%tauy_top)**2 
                  buf2_tauy_top = (var%tauy_top-kgenref_var%tauy_top)**2 
              ELSEWHERE 
                  buf1_tauy_top = (var%tauy_top-kgenref_var%tauy_top)**2 
                  buf2_tauy_top = buf1_tauy_top 
              END WHERE   
              nrmsdiff_tauy_top = SQRT(SUM(buf1_tauy_top)/REAL(n_tauy_top)) 
              rmsdiff_tauy_top = SQRT(SUM(buf2_tauy_top)/REAL(n_tauy_top)) 
              IF (rmsdiff_tauy_top > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tauy_top is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tauy_top is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%tauy_top /= kgenref_var%tauy_top), " of ", size( var%tauy_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tauy_top)/real(size(var%tauy_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tauy_top)/real(size(kgenref_var%tauy_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tauy_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tauy_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%tauy_top /= kgenref_var%tauy_top), " of ", size( var%tauy_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tauy_top)/real(size(var%tauy_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%tauy_top)/real(size(kgenref_var%tauy_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tauy_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tauy_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%cflx_srf)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%cflx_srf == kgenref_var%cflx_srf)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cflx_srf is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_cflx_srf(SIZE(var%cflx_srf,dim=1),SIZE(var%cflx_srf,dim=2))) 
              ALLOCATE (buf2_cflx_srf(SIZE(var%cflx_srf,dim=1),SIZE(var%cflx_srf,dim=2))) 
              n_cflx_srf = COUNT(var%cflx_srf /= kgenref_var%cflx_srf) 
              WHERE ( ABS(kgenref_var%cflx_srf) > kgen_minvalue ) 
                  buf1_cflx_srf = ((var%cflx_srf-kgenref_var%cflx_srf)/kgenref_var%cflx_srf)**2 
                  buf2_cflx_srf = (var%cflx_srf-kgenref_var%cflx_srf)**2 
              ELSEWHERE 
                  buf1_cflx_srf = (var%cflx_srf-kgenref_var%cflx_srf)**2 
                  buf2_cflx_srf = buf1_cflx_srf 
              END WHERE   
              nrmsdiff_cflx_srf = SQRT(SUM(buf1_cflx_srf)/REAL(n_cflx_srf)) 
              rmsdiff_cflx_srf = SQRT(SUM(buf2_cflx_srf)/REAL(n_cflx_srf)) 
              IF (rmsdiff_cflx_srf > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%cflx_srf is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%cflx_srf is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%cflx_srf /= kgenref_var%cflx_srf), " of ", size( var%cflx_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%cflx_srf)/real(size(var%cflx_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%cflx_srf)/real(size(kgenref_var%cflx_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_cflx_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cflx_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%cflx_srf /= kgenref_var%cflx_srf), " of ", size( var%cflx_srf ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%cflx_srf)/real(size(var%cflx_srf)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%cflx_srf)/real(size(kgenref_var%cflx_srf)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_cflx_srf 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cflx_srf 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%cflx_top)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%cflx_top == kgenref_var%cflx_top)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cflx_top is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_cflx_top(SIZE(var%cflx_top,dim=1),SIZE(var%cflx_top,dim=2))) 
              ALLOCATE (buf2_cflx_top(SIZE(var%cflx_top,dim=1),SIZE(var%cflx_top,dim=2))) 
              n_cflx_top = COUNT(var%cflx_top /= kgenref_var%cflx_top) 
              WHERE ( ABS(kgenref_var%cflx_top) > kgen_minvalue ) 
                  buf1_cflx_top = ((var%cflx_top-kgenref_var%cflx_top)/kgenref_var%cflx_top)**2 
                  buf2_cflx_top = (var%cflx_top-kgenref_var%cflx_top)**2 
              ELSEWHERE 
                  buf1_cflx_top = (var%cflx_top-kgenref_var%cflx_top)**2 
                  buf2_cflx_top = buf1_cflx_top 
              END WHERE   
              nrmsdiff_cflx_top = SQRT(SUM(buf1_cflx_top)/REAL(n_cflx_top)) 
              rmsdiff_cflx_top = SQRT(SUM(buf2_cflx_top)/REAL(n_cflx_top)) 
              IF (rmsdiff_cflx_top > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%cflx_top is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%cflx_top is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%cflx_top /= kgenref_var%cflx_top), " of ", size( var%cflx_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%cflx_top)/real(size(var%cflx_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%cflx_top)/real(size(kgenref_var%cflx_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_cflx_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cflx_top 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%cflx_top /= kgenref_var%cflx_top), " of ", size( var%cflx_top ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%cflx_top)/real(size(var%cflx_top)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%cflx_top)/real(size(kgenref_var%cflx_top)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_cflx_top 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_cflx_top 
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
  END SUBROUTINE kv_physics_types_physics_ptend 
    
end module physics_types