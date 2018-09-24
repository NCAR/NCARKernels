!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:44 
!KGEN version : 0.7.3 
  


module element_mod

    USE shr_kind_mod, ONLY: r8=>shr_kind_r8, i8=>shr_kind_i8 
    USE coordinate_systems_mod, ONLY: spherical_polar_t, cartesian2d_t, cartesian3d_t 
    USE dimensions_mod, ONLY: np, nc, npsq, nlev, nlevp, qsize_d, ntrac_d 
    USE edgetype_mod, ONLY: edgedescriptor_t 
    USE gridgraph_mod, ONLY: gridvertex_t 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE coordinate_systems_mod, ONLY: kr_kgen_coordinate_systems_mod_typesubp0 
    USE coordinate_systems_mod, ONLY: kr_coordinate_systems_mod_cartesian2d_t 
    USE coordinate_systems_mod, ONLY: kr_coordinate_systems_mod_cartesian3d_t 
    USE edgetype_mod, ONLY: kr_edgetype_mod_edgedescriptor_t 
    USE gridgraph_mod, ONLY: kr_gridgraph_mod_gridvertex_t 
    USE coordinate_systems_mod, ONLY: kv_kgen_coordinate_systems_mod_typesubp0 
    USE coordinate_systems_mod, ONLY: kv_coordinate_systems_mod_cartesian2d_t 
    USE coordinate_systems_mod, ONLY: kv_coordinate_systems_mod_cartesian3d_t 
    USE edgetype_mod, ONLY: kv_edgetype_mod_edgedescriptor_t 
    USE gridgraph_mod, ONLY: kv_gridgraph_mod_gridvertex_t 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
  integer, public, parameter :: timelevels = 3
! =========== PRIMITIVE-EQUATION DATA-STRUCTURES =====================


  type, public :: elem_state_t
    ! prognostic variables for preqx solver
    ! prognostics must match those in prim_restart_mod.F90
    ! vertically-lagrangian code advects dp3d instead of ps
    ! tracers Q, Qdp always use 2 level time scheme


    real (kind=r8) :: v     (np,np,2,nlev,timelevels)            ! velocity                           
    real (kind=r8) :: T     (np,np,nlev,timelevels)              ! temperature                        
    real (kind=r8) :: dp3d  (np,np,nlev,timelevels)              ! dry delta p on levels              
    real (kind=r8) :: psdry (np,np)                              ! dry surface pressure               
    real (kind=r8) :: phis  (np,np)                              ! surface geopotential (prescribed)  
    real (kind=r8) :: Qdp   (np,np,nlev,qsize_d,2)               ! Tracer mass                        

  end type elem_state_t
  !___________________________________________________________________

  type, public :: derived_state_t
     ! storage for subcycling tracers/dynamics
     !
     !
    real (kind=r8) :: vn0  (np,np,2,nlev)                      ! velocity for SE tracer advection
    real (kind=r8) :: dpdiss_biharmonic(np,np,nlev)            ! mean dp dissipation tendency, if nu_p>0
    real (kind=r8) :: dpdiss_ave(np,np,nlev)                   ! mean dp used to compute psdiss_tens
    ! diagnostics for explicit timestep

    real (kind=r8) :: phi(np,np,nlev)                          ! geopotential
    real (kind=r8) :: omega(np,np,nlev)                        ! vertical velocity
    ! semi-implicit diagnostics: computed in explict-component, reused in Helmholtz-component.

    real (kind=r8) :: zeta(np,np,nlev)                         ! relative vorticity
    real (kind=r8) :: div(np,np,nlev,timelevels)               ! divergence
    ! tracer advection fields used for consistency and limiters

    real (kind=r8) :: dp(np,np,nlev)                           ! for dp_tracers at physics timestep
    real (kind=r8) :: divdp(np,np,nlev)                        ! divergence of dp
    real (kind=r8) :: divdp_proj(np,np,nlev)                   ! DSSed divdp
    real (kind=r8) :: mass(MAX(qsize_d,ntrac_d)+8)             ! total tracer mass for diagnostics
    ! forcing terms for 

    real (kind=r8) :: FQ(np,np,nlev,qsize_d)                   ! tracer forcing
    real (kind=r8) :: FM(np,np,2,nlev)                         ! momentum forcing
    real (kind=r8) :: FT(np,np,nlev)                           ! temperature forcing
    real (kind=r8) :: etadot_prescribed(np,np,nlevp)           ! prescribed vertical tendency
    real (kind=r8) :: u_met(np,np,nlev)                        ! zonal component of prescribed meteorology winds
    real (kind=r8) :: dudt_met(np,np,nlev)                     ! rate of change of zonal component of prescribed meteorology winds
    real (kind=r8) :: v_met(np,np,nlev)                        ! meridional component of prescribed meteorology winds
    real (kind=r8) :: dvdt_met(np,np,nlev)                     ! rate of change of meridional component of prescribed meteorology winds
    real (kind=r8) :: T_met(np,np,nlev)                        ! prescribed meteorology temperature
    real (kind=r8) :: dTdt_met(np,np,nlev)                     ! rate of change of prescribed meteorology temperature
    real (kind=r8) :: ps_met(np,np)                            ! surface pressure of prescribed meteorology
    real (kind=r8) :: dpsdt_met(np,np)                         ! rate of change of surface pressure of prescribed meteorology
    real (kind=r8) :: nudge_factor(np,np,nlev)                 ! nudging factor (prescribed)
    real (kind=r8) :: Utnd(npsq,nlev)                          ! accumulated U tendency due to nudging towards prescribed met
    real (kind=r8) :: Vtnd(npsq,nlev)                          ! accumulated V tendency due to nudging towards prescribed met
    real (kind=r8) :: Ttnd(npsq,nlev)                          ! accumulated T tendency due to nudging towards prescribed met

    real (kind=r8) :: pecnd(np,np,nlev)                        ! pressure perturbation from condensate

  end type derived_state_t
  !___________________________________________________________________


! ============= DATA-STRUCTURES COMMON TO ALL SOLVERS ================


  type, public :: index_t
     integer :: ia(npsq),ja(npsq)
     integer :: is,ie
     integer :: NumUniquePts
     integer :: UniquePtOffset
  end type index_t
  !___________________________________________________________________

  type, public :: element_t
     integer :: LocalId
     integer :: GlobalId
     ! Coordinate values of element points

     type (spherical_polar_t) :: spherep(np,np)                       ! Spherical coords of GLL points
     ! Equ-angular gnomonic projection coordinates

     type (cartesian2D_t)     :: cartp(np,np)                         ! gnomonic coords of GLL points
     type (cartesian2D_t)     :: corners(4)                           ! gnomonic coords of element corners
     real (kind=r8)    :: u2qmap(4,2)                          ! bilinear map from ref element to quad in cubedsphere coordinates
                                                                      ! SHOULD BE REMOVED
     ! 3D cartesian coordinates
     type (cartesian3D_t)     :: corners3D(4)
     ! Element diagnostics

     real (kind=r8)    :: area                                 ! Area of element
     real (kind=r8)    :: normDinv                             ! some type of norm of Dinv used for CFL
     real (kind=r8)    :: dx_short                             ! short length scale in km
     real (kind=r8)    :: dx_long                              ! long length scale in km

     real (kind=r8)    :: variable_hyperviscosity(np,np)       ! hyperviscosity based on above
     real (kind=r8)    :: hv_courant                           ! hyperviscosity courant number
     real (kind=r8)    :: tensorVisc(np,np,2,2)                !og, matrix V for tensor viscosity
     ! Edge connectivity information
!     integer :: node_numbers(4)
!     integer :: node_multiplicity(4)                 ! number of elements sharing corner node


     type (GridVertex_t)      :: vertex                               ! element grid vertex information
     type (EdgeDescriptor_t)  :: desc

     type (elem_state_t)      :: state

     type (derived_state_t)   :: derived
     ! Metric terms
     real (kind=r8)    :: met(np,np,2,2)                       ! metric tensor on velocity and pressure grid
     real (kind=r8)    :: metinv(np,np,2,2)                    ! metric tensor on velocity and pressure grid
     real (kind=r8)    :: metdet(np,np)                        ! g = SQRT(det(g_ij)) on velocity and pressure grid
     real (kind=r8)    :: rmetdet(np,np)                       ! 1/metdet on velocity pressure grid
     real (kind=r8)    :: D(np,np,2,2)                         ! Map covariant field on cube to vector field on the sphere
     real (kind=r8)    :: Dinv(np,np,2,2)                      ! Map vector field on the sphere to covariant v on cube
     ! Mass flux across the sides of each sub-element.
     ! The storage is redundent since the mass across shared sides
     ! must be equal in magnitude and opposite in sign.
     ! The layout is like:
     !   --------------------------------------------------------------
     ! ^|    (1,4,3)     |                |              |    (4,4,3) |
     ! ||                |                |              |            |
     ! ||(1,4,4)         |                |              |(4,4,4)     |
     ! ||         (1,4,2)|                |              |     (4,4,2)|
     ! ||                |                |              |            |
     ! ||   (1,4,1)      |                |              |  (4,4,1)   |
     ! |---------------------------------------------------------------
     ! S|                |                |              |            |
     ! e|                |                |              |            |
     ! c|                |                |              |            |
     ! o|                |                |              |            |
     ! n|                |                |              |            |
     ! d|                |                |              |            |
     !  ---------------------------------------------------------------
     ! C|                |                |              |            |
     ! o|                |                |              |            |
     ! o|                |                |              |            |
     ! r|                |                |              |            |
     ! d|                |                |              |            |
     ! i|                |                |              |            |
     ! n---------------------------------------------------------------
     ! a|    (1,1,3)     |                |              |    (4,1,3) |
     ! t|                |                |              |(4,1,4)     |
     ! e|(1,1,4)         |                |              |            |
     !  |         (1,1,2)|                |              |     (4,1,2)|
     !  |                |                |              |            |
     !  |    (1,1,1)     |                |              |  (4,1,1)   |
     !  ---------------------------------------------------------------
     !          First Coordinate ------->


     real (kind=r8) :: sub_elem_mass_flux(nc,nc,4,nlev)
     ! Convert vector fields from spherical to rectangular components
     ! The transpose of this operation is its pseudoinverse.

     real (kind=r8)    :: vec_sphere2cart(np,np,3,2)
     ! Mass matrix terms for an element on a cube face

     real (kind=r8)    :: mp(np,np)                            ! mass matrix on v and p grid
     real (kind=r8)    :: rmp(np,np)                           ! inverse mass matrix on v and p grid
     ! Mass matrix terms for an element on the sphere
     ! This mass matrix is used when solving the equations in weak form
     ! with the natural (surface area of the sphere) inner product

     real (kind=r8)    :: spheremp(np,np)                      ! mass matrix on v and p grid
     real (kind=r8)    :: rspheremp(np,np)                     ! inverse mass matrix on v and p grid

     integer(i8) :: gdofP(np,np)                     ! global degree of freedom (P-grid)

     real (kind=r8)    :: fcor(np,np)                          ! Coreolis term

     type (index_t) :: idxP
     type (index_t),pointer :: idxV
     integer :: FaceNum
     ! force element_t to be a multiple of 8 bytes.
     ! on BGP, code will crash (signal 7, or signal 15) if 8 byte alignment is off
     ! check core file for:
     ! core.63:Generated by interrupt..(Alignment Exception DEAR=0xa1ef671c ESR=0x01800000 CCR0=0x4800a002)

     integer :: dummy
  end type element_t
  !___________________________________________________________________

  PUBLIC kr_kgen_coordinate_systems_mod_typesubp0 
  PUBLIC kr_coordinate_systems_mod_cartesian2d_t 
  PUBLIC kr_coordinate_systems_mod_cartesian3d_t 
  PUBLIC kr_edgetype_mod_edgedescriptor_t 
  PUBLIC kr_gridgraph_mod_gridvertex_t 
  PUBLIC kr_element_mod_elem_state_t 
  PUBLIC kr_element_mod_derived_state_t 
  PUBLIC kr_element_mod_index_t 
  PUBLIC kr_element_mod_element_t 
  PUBLIC kv_kgen_coordinate_systems_mod_typesubp0 
  PUBLIC kv_coordinate_systems_mod_cartesian2d_t 
  PUBLIC kv_coordinate_systems_mod_cartesian3d_t 
  PUBLIC kv_edgetype_mod_edgedescriptor_t 
  PUBLIC kv_gridgraph_mod_gridvertex_t 
  PUBLIC kv_element_mod_elem_state_t 
  PUBLIC kv_element_mod_derived_state_t 
  PUBLIC kv_element_mod_index_t 
  PUBLIC kv_element_mod_element_t 

    
  CONTAINS 
    


! ===================== ELEMENT_MOD METHODS ==========================


  !___________________________________________________________________


  !___________________________________________________________________


  !___________________________________________________________________


  !___________________________________________________________________


  !___________________________________________________________________


  !read state subroutine for kr_element_mod_elem_state_t 
  RECURSIVE SUBROUTINE kr_element_mod_elem_state_t(var, kgen_unit, printname, printvar) 
      TYPE(elem_state_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%v 
          CALL kgen_array_sumcheck(printname // "%v", kgen_array_sum, DBLE(SUM(var%v, mask=(var%v .eq. var%v))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%v)) = ", DBLE(SUM(var%v, mask=(var%v .eq. var%v))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%t 
          CALL kgen_array_sumcheck(printname // "%t", kgen_array_sum, DBLE(SUM(var%t, mask=(var%t .eq. var%t))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%t)) = ", DBLE(SUM(var%t, mask=(var%t .eq. var%t))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dp3d 
          CALL kgen_array_sumcheck(printname // "%dp3d", kgen_array_sum, DBLE(SUM(var%dp3d, mask=(var%dp3d .eq. var%dp3d))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dp3d)) = ", DBLE(SUM(var%dp3d, mask=(var%dp3d .eq. &
              &var%dp3d))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%psdry 
          CALL kgen_array_sumcheck(printname // "%psdry", kgen_array_sum, DBLE(SUM(var%psdry, mask=(var%psdry .eq. var%psdry))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%psdry)) = ", DBLE(SUM(var%psdry, mask=(var%psdry .eq. &
              &var%psdry))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%phis 
          CALL kgen_array_sumcheck(printname // "%phis", kgen_array_sum, DBLE(SUM(var%phis, mask=(var%phis .eq. var%phis))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%phis)) = ", DBLE(SUM(var%phis, mask=(var%phis .eq. &
              &var%phis))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%qdp 
          CALL kgen_array_sumcheck(printname // "%qdp", kgen_array_sum, DBLE(SUM(var%qdp, mask=(var%qdp .eq. var%qdp))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%qdp)) = ", DBLE(SUM(var%qdp, mask=(var%qdp .eq. var%qdp))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_element_mod_elem_state_t 
    
  !read state subroutine for kr_element_mod_derived_state_t 
  RECURSIVE SUBROUTINE kr_element_mod_derived_state_t(var, kgen_unit, printname, printvar) 
      TYPE(derived_state_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%vn0 
          CALL kgen_array_sumcheck(printname // "%vn0", kgen_array_sum, DBLE(SUM(var%vn0, mask=(var%vn0 .eq. var%vn0))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%vn0)) = ", DBLE(SUM(var%vn0, mask=(var%vn0 .eq. var%vn0))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dpdiss_biharmonic 
          CALL kgen_array_sumcheck(printname // "%dpdiss_biharmonic", kgen_array_sum, DBLE(SUM(var%dpdiss_biharmonic, &
          &mask=(var%dpdiss_biharmonic .eq. var%dpdiss_biharmonic))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dpdiss_biharmonic)) = ", DBLE(SUM(var%dpdiss_biharmonic, &
              &mask=(var%dpdiss_biharmonic .eq. var%dpdiss_biharmonic))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dpdiss_ave 
          CALL kgen_array_sumcheck(printname // "%dpdiss_ave", kgen_array_sum, DBLE(SUM(var%dpdiss_ave, mask=(var%dpdiss_ave .eq. &
          &var%dpdiss_ave))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dpdiss_ave)) = ", DBLE(SUM(var%dpdiss_ave, &
              &mask=(var%dpdiss_ave .eq. var%dpdiss_ave))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%phi 
          CALL kgen_array_sumcheck(printname // "%phi", kgen_array_sum, DBLE(SUM(var%phi, mask=(var%phi .eq. var%phi))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%phi)) = ", DBLE(SUM(var%phi, mask=(var%phi .eq. var%phi))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%omega 
          CALL kgen_array_sumcheck(printname // "%omega", kgen_array_sum, DBLE(SUM(var%omega, mask=(var%omega .eq. var%omega))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%omega)) = ", DBLE(SUM(var%omega, mask=(var%omega .eq. &
              &var%omega))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%zeta 
          CALL kgen_array_sumcheck(printname // "%zeta", kgen_array_sum, DBLE(SUM(var%zeta, mask=(var%zeta .eq. var%zeta))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%zeta)) = ", DBLE(SUM(var%zeta, mask=(var%zeta .eq. &
              &var%zeta))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%div 
          CALL kgen_array_sumcheck(printname // "%div", kgen_array_sum, DBLE(SUM(var%div, mask=(var%div .eq. var%div))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%div)) = ", DBLE(SUM(var%div, mask=(var%div .eq. var%div))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dp 
          CALL kgen_array_sumcheck(printname // "%dp", kgen_array_sum, DBLE(SUM(var%dp, mask=(var%dp .eq. var%dp))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dp)) = ", DBLE(SUM(var%dp, mask=(var%dp .eq. var%dp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%divdp 
          CALL kgen_array_sumcheck(printname // "%divdp", kgen_array_sum, DBLE(SUM(var%divdp, mask=(var%divdp .eq. var%divdp))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%divdp)) = ", DBLE(SUM(var%divdp, mask=(var%divdp .eq. &
              &var%divdp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%divdp_proj 
          CALL kgen_array_sumcheck(printname // "%divdp_proj", kgen_array_sum, DBLE(SUM(var%divdp_proj, mask=(var%divdp_proj .eq. &
          &var%divdp_proj))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%divdp_proj)) = ", DBLE(SUM(var%divdp_proj, &
              &mask=(var%divdp_proj .eq. var%divdp_proj))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%mass 
          CALL kgen_array_sumcheck(printname // "%mass", kgen_array_sum, DBLE(SUM(var%mass, mask=(var%mass .eq. var%mass))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%mass)) = ", DBLE(SUM(var%mass, mask=(var%mass .eq. &
              &var%mass))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%fq 
          CALL kgen_array_sumcheck(printname // "%fq", kgen_array_sum, DBLE(SUM(var%fq, mask=(var%fq .eq. var%fq))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%fq)) = ", DBLE(SUM(var%fq, mask=(var%fq .eq. var%fq))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%fm 
          CALL kgen_array_sumcheck(printname // "%fm", kgen_array_sum, DBLE(SUM(var%fm, mask=(var%fm .eq. var%fm))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%fm)) = ", DBLE(SUM(var%fm, mask=(var%fm .eq. var%fm))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ft 
          CALL kgen_array_sumcheck(printname // "%ft", kgen_array_sum, DBLE(SUM(var%ft, mask=(var%ft .eq. var%ft))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ft)) = ", DBLE(SUM(var%ft, mask=(var%ft .eq. var%ft))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%etadot_prescribed 
          CALL kgen_array_sumcheck(printname // "%etadot_prescribed", kgen_array_sum, DBLE(SUM(var%etadot_prescribed, &
          &mask=(var%etadot_prescribed .eq. var%etadot_prescribed))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%etadot_prescribed)) = ", DBLE(SUM(var%etadot_prescribed, &
              &mask=(var%etadot_prescribed .eq. var%etadot_prescribed))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%u_met 
          CALL kgen_array_sumcheck(printname // "%u_met", kgen_array_sum, DBLE(SUM(var%u_met, mask=(var%u_met .eq. var%u_met))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%u_met)) = ", DBLE(SUM(var%u_met, mask=(var%u_met .eq. &
              &var%u_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dudt_met 
          CALL kgen_array_sumcheck(printname // "%dudt_met", kgen_array_sum, DBLE(SUM(var%dudt_met, mask=(var%dudt_met .eq. &
          &var%dudt_met))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dudt_met)) = ", DBLE(SUM(var%dudt_met, mask=(var%dudt_met &
              &.eq. var%dudt_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%v_met 
          CALL kgen_array_sumcheck(printname // "%v_met", kgen_array_sum, DBLE(SUM(var%v_met, mask=(var%v_met .eq. var%v_met))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%v_met)) = ", DBLE(SUM(var%v_met, mask=(var%v_met .eq. &
              &var%v_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dvdt_met 
          CALL kgen_array_sumcheck(printname // "%dvdt_met", kgen_array_sum, DBLE(SUM(var%dvdt_met, mask=(var%dvdt_met .eq. &
          &var%dvdt_met))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dvdt_met)) = ", DBLE(SUM(var%dvdt_met, mask=(var%dvdt_met &
              &.eq. var%dvdt_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%t_met 
          CALL kgen_array_sumcheck(printname // "%t_met", kgen_array_sum, DBLE(SUM(var%t_met, mask=(var%t_met .eq. var%t_met))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%t_met)) = ", DBLE(SUM(var%t_met, mask=(var%t_met .eq. &
              &var%t_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dtdt_met 
          CALL kgen_array_sumcheck(printname // "%dtdt_met", kgen_array_sum, DBLE(SUM(var%dtdt_met, mask=(var%dtdt_met .eq. &
          &var%dtdt_met))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dtdt_met)) = ", DBLE(SUM(var%dtdt_met, mask=(var%dtdt_met &
              &.eq. var%dtdt_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ps_met 
          CALL kgen_array_sumcheck(printname // "%ps_met", kgen_array_sum, DBLE(SUM(var%ps_met, mask=(var%ps_met .eq. &
          &var%ps_met))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ps_met)) = ", DBLE(SUM(var%ps_met, mask=(var%ps_met .eq. &
              &var%ps_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dpsdt_met 
          CALL kgen_array_sumcheck(printname // "%dpsdt_met", kgen_array_sum, DBLE(SUM(var%dpsdt_met, mask=(var%dpsdt_met .eq. &
          &var%dpsdt_met))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dpsdt_met)) = ", DBLE(SUM(var%dpsdt_met, mask=(var%dpsdt_met &
              &.eq. var%dpsdt_met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%nudge_factor 
          CALL kgen_array_sumcheck(printname // "%nudge_factor", kgen_array_sum, DBLE(SUM(var%nudge_factor, &
          &mask=(var%nudge_factor .eq. var%nudge_factor))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%nudge_factor)) = ", DBLE(SUM(var%nudge_factor, &
              &mask=(var%nudge_factor .eq. var%nudge_factor))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%utnd 
          CALL kgen_array_sumcheck(printname // "%utnd", kgen_array_sum, DBLE(SUM(var%utnd, mask=(var%utnd .eq. var%utnd))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%utnd)) = ", DBLE(SUM(var%utnd, mask=(var%utnd .eq. &
              &var%utnd))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%vtnd 
          CALL kgen_array_sumcheck(printname // "%vtnd", kgen_array_sum, DBLE(SUM(var%vtnd, mask=(var%vtnd .eq. var%vtnd))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%vtnd)) = ", DBLE(SUM(var%vtnd, mask=(var%vtnd .eq. &
              &var%vtnd))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ttnd 
          CALL kgen_array_sumcheck(printname // "%ttnd", kgen_array_sum, DBLE(SUM(var%ttnd, mask=(var%ttnd .eq. var%ttnd))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ttnd)) = ", DBLE(SUM(var%ttnd, mask=(var%ttnd .eq. &
              &var%ttnd))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%pecnd 
          CALL kgen_array_sumcheck(printname // "%pecnd", kgen_array_sum, DBLE(SUM(var%pecnd, mask=(var%pecnd .eq. var%pecnd))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%pecnd)) = ", DBLE(SUM(var%pecnd, mask=(var%pecnd .eq. &
              &var%pecnd))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_element_mod_derived_state_t 
    
  !read state subroutine for kr_element_mod_index_t 
  RECURSIVE SUBROUTINE kr_element_mod_index_t(var, kgen_unit, printname, printvar) 
      TYPE(index_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ia 
          CALL kgen_array_sumcheck(printname // "%ia", kgen_array_sum, DBLE(SUM(var%ia, mask=(var%ia .eq. var%ia))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ia)) = ", DBLE(SUM(var%ia, mask=(var%ia .eq. var%ia))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ja 
          CALL kgen_array_sumcheck(printname // "%ja", kgen_array_sum, DBLE(SUM(var%ja, mask=(var%ja .eq. var%ja))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ja)) = ", DBLE(SUM(var%ja, mask=(var%ja .eq. var%ja))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%is 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%is = ", var%is 
      END IF   
      READ (UNIT = kgen_unit) var%ie 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ie = ", var%ie 
      END IF   
        
      READ (UNIT = kgen_unit) var%numuniquepts 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%numuniquepts = ", var%numuniquepts 
      END IF   
        
      READ (UNIT = kgen_unit) var%uniqueptoffset 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%uniqueptoffset = ", var%uniqueptoffset 
      END IF   
        
  END SUBROUTINE kr_element_mod_index_t 
    
  !read state subroutine for kr_element_mod_element_t 
  RECURSIVE SUBROUTINE kr_element_mod_element_t(var, kgen_unit, printname, printvar) 
      TYPE(element_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%localid 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%localid = ", var%localid 
      END IF   
        
      READ (UNIT = kgen_unit) var%globalid 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%globalid = ", var%globalid 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_element_t_subp3(var%spherep, kgen_unit, printname // "%spherep", .TRUE.) 
      ELSE 
          CALL kr_kgen_element_t_subp3(var%spherep, kgen_unit, printname // "%spherep", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_element_t_subp4(var%cartp, kgen_unit, printname // "%cartp", .TRUE.) 
      ELSE 
          CALL kr_kgen_element_t_subp4(var%cartp, kgen_unit, printname // "%cartp", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_element_t_subp5(var%corners, kgen_unit, printname // "%corners", .TRUE.) 
      ELSE 
          CALL kr_kgen_element_t_subp5(var%corners, kgen_unit, printname // "%corners", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%u2qmap 
          CALL kgen_array_sumcheck(printname // "%u2qmap", kgen_array_sum, DBLE(SUM(var%u2qmap, mask=(var%u2qmap .eq. &
          &var%u2qmap))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%u2qmap)) = ", DBLE(SUM(var%u2qmap, mask=(var%u2qmap .eq. &
              &var%u2qmap))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_element_t_subp6(var%corners3d, kgen_unit, printname // "%corners3d", .TRUE.) 
      ELSE 
          CALL kr_kgen_element_t_subp6(var%corners3d, kgen_unit, printname // "%corners3d", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%area 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%area = ", var%area 
      END IF   
        
      READ (UNIT = kgen_unit) var%normdinv 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%normdinv = ", var%normdinv 
      END IF   
        
      READ (UNIT = kgen_unit) var%dx_short 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dx_short = ", var%dx_short 
      END IF   
        
      READ (UNIT = kgen_unit) var%dx_long 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dx_long = ", var%dx_long 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%variable_hyperviscosity 
          CALL kgen_array_sumcheck(printname // "%variable_hyperviscosity", kgen_array_sum, DBLE(SUM(var%variable_hyperviscosity, &
          &mask=(var%variable_hyperviscosity .eq. var%variable_hyperviscosity))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%variable_hyperviscosity)) = ", &
              &DBLE(SUM(var%variable_hyperviscosity, mask=(var%variable_hyperviscosity .eq. var%variable_hyperviscosity))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%hv_courant 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%hv_courant = ", var%hv_courant 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%tensorvisc 
          CALL kgen_array_sumcheck(printname // "%tensorvisc", kgen_array_sum, DBLE(SUM(var%tensorvisc, mask=(var%tensorvisc .eq. &
          &var%tensorvisc))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%tensorvisc)) = ", DBLE(SUM(var%tensorvisc, &
              &mask=(var%tensorvisc .eq. var%tensorvisc))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_gridgraph_mod_gridvertex_t(var%vertex, kgen_unit, printname // "%vertex", .TRUE.) 
      ELSE 
          CALL kr_gridgraph_mod_gridvertex_t(var%vertex, kgen_unit, printname // "%vertex", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_edgetype_mod_edgedescriptor_t(var%desc, kgen_unit, printname // "%desc", .TRUE.) 
      ELSE 
          CALL kr_edgetype_mod_edgedescriptor_t(var%desc, kgen_unit, printname // "%desc", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_element_mod_elem_state_t(var%state, kgen_unit, printname // "%state", .TRUE.) 
      ELSE 
          CALL kr_element_mod_elem_state_t(var%state, kgen_unit, printname // "%state", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_element_mod_derived_state_t(var%derived, kgen_unit, printname // "%derived", .TRUE.) 
      ELSE 
          CALL kr_element_mod_derived_state_t(var%derived, kgen_unit, printname // "%derived", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%met 
          CALL kgen_array_sumcheck(printname // "%met", kgen_array_sum, DBLE(SUM(var%met, mask=(var%met .eq. var%met))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%met)) = ", DBLE(SUM(var%met, mask=(var%met .eq. var%met))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%metinv 
          CALL kgen_array_sumcheck(printname // "%metinv", kgen_array_sum, DBLE(SUM(var%metinv, mask=(var%metinv .eq. &
          &var%metinv))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%metinv)) = ", DBLE(SUM(var%metinv, mask=(var%metinv .eq. &
              &var%metinv))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%metdet 
          CALL kgen_array_sumcheck(printname // "%metdet", kgen_array_sum, DBLE(SUM(var%metdet, mask=(var%metdet .eq. &
          &var%metdet))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%metdet)) = ", DBLE(SUM(var%metdet, mask=(var%metdet .eq. &
              &var%metdet))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%rmetdet 
          CALL kgen_array_sumcheck(printname // "%rmetdet", kgen_array_sum, DBLE(SUM(var%rmetdet, mask=(var%rmetdet .eq. &
          &var%rmetdet))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%rmetdet)) = ", DBLE(SUM(var%rmetdet, mask=(var%rmetdet .eq. &
              &var%rmetdet))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%d 
          CALL kgen_array_sumcheck(printname // "%d", kgen_array_sum, DBLE(SUM(var%d, mask=(var%d .eq. var%d))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%d)) = ", DBLE(SUM(var%d, mask=(var%d .eq. var%d))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dinv 
          CALL kgen_array_sumcheck(printname // "%dinv", kgen_array_sum, DBLE(SUM(var%dinv, mask=(var%dinv .eq. var%dinv))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dinv)) = ", DBLE(SUM(var%dinv, mask=(var%dinv .eq. &
              &var%dinv))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%sub_elem_mass_flux 
          CALL kgen_array_sumcheck(printname // "%sub_elem_mass_flux", kgen_array_sum, DBLE(SUM(var%sub_elem_mass_flux, &
          &mask=(var%sub_elem_mass_flux .eq. var%sub_elem_mass_flux))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%sub_elem_mass_flux)) = ", DBLE(SUM(var%sub_elem_mass_flux, &
              &mask=(var%sub_elem_mass_flux .eq. var%sub_elem_mass_flux))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%vec_sphere2cart 
          CALL kgen_array_sumcheck(printname // "%vec_sphere2cart", kgen_array_sum, DBLE(SUM(var%vec_sphere2cart, &
          &mask=(var%vec_sphere2cart .eq. var%vec_sphere2cart))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%vec_sphere2cart)) = ", DBLE(SUM(var%vec_sphere2cart, &
              &mask=(var%vec_sphere2cart .eq. var%vec_sphere2cart))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%mp 
          CALL kgen_array_sumcheck(printname // "%mp", kgen_array_sum, DBLE(SUM(var%mp, mask=(var%mp .eq. var%mp))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%mp)) = ", DBLE(SUM(var%mp, mask=(var%mp .eq. var%mp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%rmp 
          CALL kgen_array_sumcheck(printname // "%rmp", kgen_array_sum, DBLE(SUM(var%rmp, mask=(var%rmp .eq. var%rmp))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%rmp)) = ", DBLE(SUM(var%rmp, mask=(var%rmp .eq. var%rmp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%spheremp 
          CALL kgen_array_sumcheck(printname // "%spheremp", kgen_array_sum, DBLE(SUM(var%spheremp, mask=(var%spheremp .eq. &
          &var%spheremp))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%spheremp)) = ", DBLE(SUM(var%spheremp, mask=(var%spheremp &
              &.eq. var%spheremp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%rspheremp 
          CALL kgen_array_sumcheck(printname // "%rspheremp", kgen_array_sum, DBLE(SUM(var%rspheremp, mask=(var%rspheremp .eq. &
          &var%rspheremp))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%rspheremp)) = ", DBLE(SUM(var%rspheremp, mask=(var%rspheremp &
              &.eq. var%rspheremp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%gdofp 
          CALL kgen_array_sumcheck(printname // "%gdofp", kgen_array_sum, DBLE(SUM(var%gdofp, mask=(var%gdofp .eq. var%gdofp))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%gdofp)) = ", DBLE(SUM(var%gdofp, mask=(var%gdofp .eq. &
              &var%gdofp))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%fcor 
          CALL kgen_array_sumcheck(printname // "%fcor", kgen_array_sum, DBLE(SUM(var%fcor, mask=(var%fcor .eq. var%fcor))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%fcor)) = ", DBLE(SUM(var%fcor, mask=(var%fcor .eq. &
              &var%fcor))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_element_mod_index_t(var%idxp, kgen_unit, printname // "%idxp", .TRUE.) 
      ELSE 
          CALL kr_element_mod_index_t(var%idxp, kgen_unit, printname // "%idxp", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_element_t_index_t__index_t_ptr(var%idxv, kgen_unit, printname // "%idxv", .TRUE.) 
      ELSE 
          CALL kr_element_t_index_t__index_t_ptr(var%idxv, kgen_unit, printname // "%idxv", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%facenum 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%facenum = ", var%facenum 
      END IF   
        
      READ (UNIT = kgen_unit) var%dummy 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dummy = ", var%dummy 
      END IF   
        
  END SUBROUTINE kr_element_mod_element_t 
    
  !write state subroutine for kr_kgen_element_t_subp3 
  SUBROUTINE kr_kgen_element_t_subp3(var, kgen_unit, printname, printvar) 
      TYPE(spherical_polar_t), INTENT(INOUT), DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              DO idx2=kgen_bound(1,2), kgen_bound(2,2) 
                  IF (PRESENT( printvar ) .AND. printvar) THEN 
                      CALL kr_kgen_coordinate_systems_mod_typesubp0(var(idx1,idx2), kgen_unit, printname // "(idx1,idx2)", &
                      &.TRUE.) 
                  ELSE 
                      CALL kr_kgen_coordinate_systems_mod_typesubp0(var(idx1,idx2), kgen_unit, printname // "(idx1,idx2)", &
                      &.FALSE.) 
                  END IF   
              END DO   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_element_t_subp3 
    
  !write state subroutine for kr_kgen_element_t_subp4 
  SUBROUTINE kr_kgen_element_t_subp4(var, kgen_unit, printname, printvar) 
      TYPE(cartesian2d_t), INTENT(INOUT), DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              DO idx2=kgen_bound(1,2), kgen_bound(2,2) 
                  IF (PRESENT( printvar ) .AND. printvar) THEN 
                      CALL kr_coordinate_systems_mod_cartesian2d_t(var(idx1,idx2), kgen_unit, printname // "(idx1,idx2)", .TRUE.) 
                  ELSE 
                      CALL kr_coordinate_systems_mod_cartesian2d_t(var(idx1,idx2), kgen_unit, printname // "(idx1,idx2)", &
                      &.FALSE.) 
                  END IF   
              END DO   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_element_t_subp4 
    
  !write state subroutine for kr_kgen_element_t_subp5 
  SUBROUTINE kr_kgen_element_t_subp5(var, kgen_unit, printname, printvar) 
      TYPE(cartesian2d_t), INTENT(INOUT), DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              IF (PRESENT( printvar ) .AND. printvar) THEN 
                  CALL kr_coordinate_systems_mod_cartesian2d_t(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
              ELSE 
                  CALL kr_coordinate_systems_mod_cartesian2d_t(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
              END IF   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_element_t_subp5 
    
  !write state subroutine for kr_kgen_element_t_subp6 
  SUBROUTINE kr_kgen_element_t_subp6(var, kgen_unit, printname, printvar) 
      TYPE(cartesian3d_t), INTENT(INOUT), DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
              IF (PRESENT( printvar ) .AND. printvar) THEN 
                  CALL kr_coordinate_systems_mod_cartesian3d_t(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
              ELSE 
                  CALL kr_coordinate_systems_mod_cartesian3d_t(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
              END IF   
          END DO   
      END IF   
        
  END SUBROUTINE kr_kgen_element_t_subp6 
    
  !write state subroutine for kr_element_t_index_t__index_t_ptr 
  SUBROUTINE kr_element_t_index_t__index_t_ptr(var, kgen_unit, printname, printvar) 
      TYPE(index_t), INTENT(INOUT), POINTER :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ASSOCIATED( var )) THEN 
              NULLIFY (var) 
          END IF   
          ALLOCATE (var) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              CALL kr_element_mod_index_t(var, kgen_unit, printname, .TRUE.) 
          ELSE 
              CALL kr_element_mod_index_t(var, kgen_unit, printname, .FALSE.) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_element_t_index_t__index_t_ptr 
    
  !verify state subroutine for kv_element_mod_elem_state_t 
  RECURSIVE SUBROUTINE kv_element_mod_elem_state_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(elem_state_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_v 
      real(KIND=r8) :: nrmsdiff_v, rmsdiff_v 
      real(KIND=r8), ALLOCATABLE :: buf1_v(:,:,:,:,:), buf2_v(:,:,:,:,:) 
      INTEGER :: n_t 
      real(KIND=r8) :: nrmsdiff_t, rmsdiff_t 
      real(KIND=r8), ALLOCATABLE :: buf1_t(:,:,:,:), buf2_t(:,:,:,:) 
      INTEGER :: n_dp3d 
      real(KIND=r8) :: nrmsdiff_dp3d, rmsdiff_dp3d 
      real(KIND=r8), ALLOCATABLE :: buf1_dp3d(:,:,:,:), buf2_dp3d(:,:,:,:) 
      INTEGER :: n_psdry 
      real(KIND=r8) :: nrmsdiff_psdry, rmsdiff_psdry 
      real(KIND=r8), ALLOCATABLE :: buf1_psdry(:,:), buf2_psdry(:,:) 
      INTEGER :: n_phis 
      real(KIND=r8) :: nrmsdiff_phis, rmsdiff_phis 
      real(KIND=r8), ALLOCATABLE :: buf1_phis(:,:), buf2_phis(:,:) 
      INTEGER :: n_qdp 
      real(KIND=r8) :: nrmsdiff_qdp, rmsdiff_qdp 
      real(KIND=r8), ALLOCATABLE :: buf1_qdp(:,:,:,:,:), buf2_qdp(:,:,:,:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%v == kgenref_var%v)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%v is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_v(SIZE(var%v,dim=1),SIZE(var%v,dim=2),SIZE(var%v,dim=3),SIZE(var%v,dim=4),SIZE(var%v,dim=5))) 
          ALLOCATE (buf2_v(SIZE(var%v,dim=1),SIZE(var%v,dim=2),SIZE(var%v,dim=3),SIZE(var%v,dim=4),SIZE(var%v,dim=5))) 
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
          IF (nrmsdiff_v > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%v is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%v is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%v /= kgenref_var%v), " of ", size( var%v ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%v)/real(size(var%v)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%v)/real(size(kgenref_var%v)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_v 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%v /= kgenref_var%v), " of ", size( var%v ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%v)/real(size(var%v)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%v)/real(size(kgenref_var%v)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_v 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%t == kgenref_var%t)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%t is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_t(SIZE(var%t,dim=1),SIZE(var%t,dim=2),SIZE(var%t,dim=3),SIZE(var%t,dim=4))) 
          ALLOCATE (buf2_t(SIZE(var%t,dim=1),SIZE(var%t,dim=2),SIZE(var%t,dim=3),SIZE(var%t,dim=4))) 
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
          IF (nrmsdiff_t > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%t is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%t is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%t /= kgenref_var%t), " of ", size( var%t ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%t)/real(size(var%t)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%t)/real(size(kgenref_var%t)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_t 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_t 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%t /= kgenref_var%t), " of ", size( var%t ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%t)/real(size(var%t)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%t)/real(size(kgenref_var%t)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_t 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_t 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dp3d == kgenref_var%dp3d)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dp3d is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dp3d(SIZE(var%dp3d,dim=1),SIZE(var%dp3d,dim=2),SIZE(var%dp3d,dim=3),SIZE(var%dp3d,dim=4))) 
          ALLOCATE (buf2_dp3d(SIZE(var%dp3d,dim=1),SIZE(var%dp3d,dim=2),SIZE(var%dp3d,dim=3),SIZE(var%dp3d,dim=4))) 
          n_dp3d = COUNT(var%dp3d /= kgenref_var%dp3d) 
          WHERE ( ABS(kgenref_var%dp3d) > kgen_minvalue ) 
              buf1_dp3d = ((var%dp3d-kgenref_var%dp3d)/kgenref_var%dp3d)**2 
              buf2_dp3d = (var%dp3d-kgenref_var%dp3d)**2 
          ELSEWHERE 
              buf1_dp3d = (var%dp3d-kgenref_var%dp3d)**2 
              buf2_dp3d = buf1_dp3d 
          END WHERE   
          nrmsdiff_dp3d = SQRT(SUM(buf1_dp3d)/REAL(n_dp3d)) 
          rmsdiff_dp3d = SQRT(SUM(buf2_dp3d)/REAL(n_dp3d)) 
          IF (nrmsdiff_dp3d > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp3d is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp3d is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp3d /= kgenref_var%dp3d), " of ", size( var%dp3d ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp3d)/real(size(var%dp3d)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp3d)/real(size(kgenref_var%dp3d)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp3d 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp3d 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp3d /= kgenref_var%dp3d), " of ", size( var%dp3d ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp3d)/real(size(var%dp3d)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp3d)/real(size(kgenref_var%dp3d)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp3d 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp3d 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%psdry == kgenref_var%psdry)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%psdry is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_psdry(SIZE(var%psdry,dim=1),SIZE(var%psdry,dim=2))) 
          ALLOCATE (buf2_psdry(SIZE(var%psdry,dim=1),SIZE(var%psdry,dim=2))) 
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
          IF (nrmsdiff_psdry > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%psdry is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%psdry is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%psdry /= kgenref_var%psdry), " of ", size( var%psdry ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%psdry)/real(size(var%psdry)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%psdry)/real(size(kgenref_var%psdry)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_psdry 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_psdry 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%psdry /= kgenref_var%psdry), " of ", size( var%psdry ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%psdry)/real(size(var%psdry)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%psdry)/real(size(kgenref_var%psdry)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_psdry 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_psdry 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%phis == kgenref_var%phis)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%phis is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_phis(SIZE(var%phis,dim=1),SIZE(var%phis,dim=2))) 
          ALLOCATE (buf2_phis(SIZE(var%phis,dim=1),SIZE(var%phis,dim=2))) 
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
          IF (nrmsdiff_phis > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%phis is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%phis is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%phis /= kgenref_var%phis), " of ", size( var%phis ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%phis)/real(size(var%phis)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%phis)/real(size(kgenref_var%phis)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_phis 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phis 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%phis /= kgenref_var%phis), " of ", size( var%phis ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%phis)/real(size(var%phis)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%phis)/real(size(kgenref_var%phis)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_phis 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phis 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%qdp == kgenref_var%qdp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%qdp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_qdp(SIZE(var%qdp,dim=1),SIZE(var%qdp,dim=2),SIZE(var%qdp,dim=3),SIZE(var%qdp,dim=4),SIZE(var%qdp,dim=5))) 
          ALLOCATE &
          &(buf2_qdp(SIZE(var%qdp,dim=1),SIZE(var%qdp,dim=2),SIZE(var%qdp,dim=3),SIZE(var%qdp,dim=4),SIZE(var%qdp,dim=5))) 
          n_qdp = COUNT(var%qdp /= kgenref_var%qdp) 
          WHERE ( ABS(kgenref_var%qdp) > kgen_minvalue ) 
              buf1_qdp = ((var%qdp-kgenref_var%qdp)/kgenref_var%qdp)**2 
              buf2_qdp = (var%qdp-kgenref_var%qdp)**2 
          ELSEWHERE 
              buf1_qdp = (var%qdp-kgenref_var%qdp)**2 
              buf2_qdp = buf1_qdp 
          END WHERE   
          nrmsdiff_qdp = SQRT(SUM(buf1_qdp)/REAL(n_qdp)) 
          rmsdiff_qdp = SQRT(SUM(buf2_qdp)/REAL(n_qdp)) 
          IF (nrmsdiff_qdp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%qdp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%qdp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%qdp /= kgenref_var%qdp), " of ", size( var%qdp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%qdp)/real(size(var%qdp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%qdp)/real(size(kgenref_var%qdp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_qdp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_qdp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%qdp /= kgenref_var%qdp), " of ", size( var%qdp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%qdp)/real(size(var%qdp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%qdp)/real(size(kgenref_var%qdp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_qdp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_qdp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_element_mod_elem_state_t 
    
  !verify state subroutine for kv_element_mod_derived_state_t 
  RECURSIVE SUBROUTINE kv_element_mod_derived_state_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(derived_state_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_vn0 
      real(KIND=r8) :: nrmsdiff_vn0, rmsdiff_vn0 
      real(KIND=r8), ALLOCATABLE :: buf1_vn0(:,:,:,:), buf2_vn0(:,:,:,:) 
      INTEGER :: n_dpdiss_biharmonic 
      real(KIND=r8) :: nrmsdiff_dpdiss_biharmonic, rmsdiff_dpdiss_biharmonic 
      real(KIND=r8), ALLOCATABLE :: buf1_dpdiss_biharmonic(:,:,:), buf2_dpdiss_biharmonic(:,:,:) 
      INTEGER :: n_dpdiss_ave 
      real(KIND=r8) :: nrmsdiff_dpdiss_ave, rmsdiff_dpdiss_ave 
      real(KIND=r8), ALLOCATABLE :: buf1_dpdiss_ave(:,:,:), buf2_dpdiss_ave(:,:,:) 
      INTEGER :: n_phi 
      real(KIND=r8) :: nrmsdiff_phi, rmsdiff_phi 
      real(KIND=r8), ALLOCATABLE :: buf1_phi(:,:,:), buf2_phi(:,:,:) 
      INTEGER :: n_omega 
      real(KIND=r8) :: nrmsdiff_omega, rmsdiff_omega 
      real(KIND=r8), ALLOCATABLE :: buf1_omega(:,:,:), buf2_omega(:,:,:) 
      INTEGER :: n_zeta 
      real(KIND=r8) :: nrmsdiff_zeta, rmsdiff_zeta 
      real(KIND=r8), ALLOCATABLE :: buf1_zeta(:,:,:), buf2_zeta(:,:,:) 
      INTEGER :: n_div 
      real(KIND=r8) :: nrmsdiff_div, rmsdiff_div 
      real(KIND=r8), ALLOCATABLE :: buf1_div(:,:,:,:), buf2_div(:,:,:,:) 
      INTEGER :: n_dp 
      real(KIND=r8) :: nrmsdiff_dp, rmsdiff_dp 
      real(KIND=r8), ALLOCATABLE :: buf1_dp(:,:,:), buf2_dp(:,:,:) 
      INTEGER :: n_divdp 
      real(KIND=r8) :: nrmsdiff_divdp, rmsdiff_divdp 
      real(KIND=r8), ALLOCATABLE :: buf1_divdp(:,:,:), buf2_divdp(:,:,:) 
      INTEGER :: n_divdp_proj 
      real(KIND=r8) :: nrmsdiff_divdp_proj, rmsdiff_divdp_proj 
      real(KIND=r8), ALLOCATABLE :: buf1_divdp_proj(:,:,:), buf2_divdp_proj(:,:,:) 
      INTEGER :: n_mass 
      real(KIND=r8) :: nrmsdiff_mass, rmsdiff_mass 
      real(KIND=r8), ALLOCATABLE :: buf1_mass(:), buf2_mass(:) 
      INTEGER :: n_fq 
      real(KIND=r8) :: nrmsdiff_fq, rmsdiff_fq 
      real(KIND=r8), ALLOCATABLE :: buf1_fq(:,:,:,:), buf2_fq(:,:,:,:) 
      INTEGER :: n_fm 
      real(KIND=r8) :: nrmsdiff_fm, rmsdiff_fm 
      real(KIND=r8), ALLOCATABLE :: buf1_fm(:,:,:,:), buf2_fm(:,:,:,:) 
      INTEGER :: n_ft 
      real(KIND=r8) :: nrmsdiff_ft, rmsdiff_ft 
      real(KIND=r8), ALLOCATABLE :: buf1_ft(:,:,:), buf2_ft(:,:,:) 
      INTEGER :: n_etadot_prescribed 
      real(KIND=r8) :: nrmsdiff_etadot_prescribed, rmsdiff_etadot_prescribed 
      real(KIND=r8), ALLOCATABLE :: buf1_etadot_prescribed(:,:,:), buf2_etadot_prescribed(:,:,:) 
      INTEGER :: n_u_met 
      real(KIND=r8) :: nrmsdiff_u_met, rmsdiff_u_met 
      real(KIND=r8), ALLOCATABLE :: buf1_u_met(:,:,:), buf2_u_met(:,:,:) 
      INTEGER :: n_dudt_met 
      real(KIND=r8) :: nrmsdiff_dudt_met, rmsdiff_dudt_met 
      real(KIND=r8), ALLOCATABLE :: buf1_dudt_met(:,:,:), buf2_dudt_met(:,:,:) 
      INTEGER :: n_v_met 
      real(KIND=r8) :: nrmsdiff_v_met, rmsdiff_v_met 
      real(KIND=r8), ALLOCATABLE :: buf1_v_met(:,:,:), buf2_v_met(:,:,:) 
      INTEGER :: n_dvdt_met 
      real(KIND=r8) :: nrmsdiff_dvdt_met, rmsdiff_dvdt_met 
      real(KIND=r8), ALLOCATABLE :: buf1_dvdt_met(:,:,:), buf2_dvdt_met(:,:,:) 
      INTEGER :: n_t_met 
      real(KIND=r8) :: nrmsdiff_t_met, rmsdiff_t_met 
      real(KIND=r8), ALLOCATABLE :: buf1_t_met(:,:,:), buf2_t_met(:,:,:) 
      INTEGER :: n_dtdt_met 
      real(KIND=r8) :: nrmsdiff_dtdt_met, rmsdiff_dtdt_met 
      real(KIND=r8), ALLOCATABLE :: buf1_dtdt_met(:,:,:), buf2_dtdt_met(:,:,:) 
      INTEGER :: n_ps_met 
      real(KIND=r8) :: nrmsdiff_ps_met, rmsdiff_ps_met 
      real(KIND=r8), ALLOCATABLE :: buf1_ps_met(:,:), buf2_ps_met(:,:) 
      INTEGER :: n_dpsdt_met 
      real(KIND=r8) :: nrmsdiff_dpsdt_met, rmsdiff_dpsdt_met 
      real(KIND=r8), ALLOCATABLE :: buf1_dpsdt_met(:,:), buf2_dpsdt_met(:,:) 
      INTEGER :: n_nudge_factor 
      real(KIND=r8) :: nrmsdiff_nudge_factor, rmsdiff_nudge_factor 
      real(KIND=r8), ALLOCATABLE :: buf1_nudge_factor(:,:,:), buf2_nudge_factor(:,:,:) 
      INTEGER :: n_utnd 
      real(KIND=r8) :: nrmsdiff_utnd, rmsdiff_utnd 
      real(KIND=r8), ALLOCATABLE :: buf1_utnd(:,:), buf2_utnd(:,:) 
      INTEGER :: n_vtnd 
      real(KIND=r8) :: nrmsdiff_vtnd, rmsdiff_vtnd 
      real(KIND=r8), ALLOCATABLE :: buf1_vtnd(:,:), buf2_vtnd(:,:) 
      INTEGER :: n_ttnd 
      real(KIND=r8) :: nrmsdiff_ttnd, rmsdiff_ttnd 
      real(KIND=r8), ALLOCATABLE :: buf1_ttnd(:,:), buf2_ttnd(:,:) 
      INTEGER :: n_pecnd 
      real(KIND=r8) :: nrmsdiff_pecnd, rmsdiff_pecnd 
      real(KIND=r8), ALLOCATABLE :: buf1_pecnd(:,:,:), buf2_pecnd(:,:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%vn0 == kgenref_var%vn0)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vn0 is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_vn0(SIZE(var%vn0,dim=1),SIZE(var%vn0,dim=2),SIZE(var%vn0,dim=3),SIZE(var%vn0,dim=4))) 
          ALLOCATE (buf2_vn0(SIZE(var%vn0,dim=1),SIZE(var%vn0,dim=2),SIZE(var%vn0,dim=3),SIZE(var%vn0,dim=4))) 
          n_vn0 = COUNT(var%vn0 /= kgenref_var%vn0) 
          WHERE ( ABS(kgenref_var%vn0) > kgen_minvalue ) 
              buf1_vn0 = ((var%vn0-kgenref_var%vn0)/kgenref_var%vn0)**2 
              buf2_vn0 = (var%vn0-kgenref_var%vn0)**2 
          ELSEWHERE 
              buf1_vn0 = (var%vn0-kgenref_var%vn0)**2 
              buf2_vn0 = buf1_vn0 
          END WHERE   
          nrmsdiff_vn0 = SQRT(SUM(buf1_vn0)/REAL(n_vn0)) 
          rmsdiff_vn0 = SQRT(SUM(buf2_vn0)/REAL(n_vn0)) 
          IF (nrmsdiff_vn0 > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vn0 is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vn0 is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vn0 /= kgenref_var%vn0), " of ", size( var%vn0 ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vn0)/real(size(var%vn0)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vn0)/real(size(kgenref_var%vn0)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vn0 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vn0 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vn0 /= kgenref_var%vn0), " of ", size( var%vn0 ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vn0)/real(size(var%vn0)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vn0)/real(size(kgenref_var%vn0)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vn0 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vn0 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dpdiss_biharmonic == kgenref_var%dpdiss_biharmonic)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dpdiss_biharmonic is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_dpdiss_biharmonic(SIZE(var%dpdiss_biharmonic,dim=1),SIZE(var%dpdiss_biharmonic,dim=2),SIZE(var%dpdiss_biharmonic,&
          &dim=3))) 
          ALLOCATE &
          &(buf2_dpdiss_biharmonic(SIZE(var%dpdiss_biharmonic,dim=1),SIZE(var%dpdiss_biharmonic,dim=2),SIZE(var%dpdiss_biharmonic,&
          &dim=3))) 
          n_dpdiss_biharmonic = COUNT(var%dpdiss_biharmonic /= kgenref_var%dpdiss_biharmonic) 
          WHERE ( ABS(kgenref_var%dpdiss_biharmonic) > kgen_minvalue ) 
              buf1_dpdiss_biharmonic = ((var%dpdiss_biharmonic-kgenref_var%dpdiss_biharmonic)/kgenref_var%dpdiss_biharmonic)**2 
              buf2_dpdiss_biharmonic = (var%dpdiss_biharmonic-kgenref_var%dpdiss_biharmonic)**2 
          ELSEWHERE 
              buf1_dpdiss_biharmonic = (var%dpdiss_biharmonic-kgenref_var%dpdiss_biharmonic)**2 
              buf2_dpdiss_biharmonic = buf1_dpdiss_biharmonic 
          END WHERE   
          nrmsdiff_dpdiss_biharmonic = SQRT(SUM(buf1_dpdiss_biharmonic)/REAL(n_dpdiss_biharmonic)) 
          rmsdiff_dpdiss_biharmonic = SQRT(SUM(buf2_dpdiss_biharmonic)/REAL(n_dpdiss_biharmonic)) 
          IF (nrmsdiff_dpdiss_biharmonic > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dpdiss_biharmonic is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dpdiss_biharmonic is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dpdiss_biharmonic /= kgenref_var%dpdiss_biharmonic), " of ", size( var%dpdiss_biharmonic ), &
              &" elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dpdiss_biharmonic)/real(size(var%dpdiss_biharmonic)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dpdiss_biharmonic)/real(size(kgenref_var%dpdiss_biharmonic)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dpdiss_biharmonic 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dpdiss_biharmonic 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dpdiss_biharmonic /= kgenref_var%dpdiss_biharmonic), " of ", size( var%dpdiss_biharmonic ), &
              &" elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dpdiss_biharmonic)/real(size(var%dpdiss_biharmonic)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dpdiss_biharmonic)/real(size(kgenref_var%dpdiss_biharmonic)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dpdiss_biharmonic 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dpdiss_biharmonic 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dpdiss_ave == kgenref_var%dpdiss_ave)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dpdiss_ave is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dpdiss_ave(SIZE(var%dpdiss_ave,dim=1),SIZE(var%dpdiss_ave,dim=2),SIZE(var%dpdiss_ave,dim=3))) 
          ALLOCATE (buf2_dpdiss_ave(SIZE(var%dpdiss_ave,dim=1),SIZE(var%dpdiss_ave,dim=2),SIZE(var%dpdiss_ave,dim=3))) 
          n_dpdiss_ave = COUNT(var%dpdiss_ave /= kgenref_var%dpdiss_ave) 
          WHERE ( ABS(kgenref_var%dpdiss_ave) > kgen_minvalue ) 
              buf1_dpdiss_ave = ((var%dpdiss_ave-kgenref_var%dpdiss_ave)/kgenref_var%dpdiss_ave)**2 
              buf2_dpdiss_ave = (var%dpdiss_ave-kgenref_var%dpdiss_ave)**2 
          ELSEWHERE 
              buf1_dpdiss_ave = (var%dpdiss_ave-kgenref_var%dpdiss_ave)**2 
              buf2_dpdiss_ave = buf1_dpdiss_ave 
          END WHERE   
          nrmsdiff_dpdiss_ave = SQRT(SUM(buf1_dpdiss_ave)/REAL(n_dpdiss_ave)) 
          rmsdiff_dpdiss_ave = SQRT(SUM(buf2_dpdiss_ave)/REAL(n_dpdiss_ave)) 
          IF (nrmsdiff_dpdiss_ave > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dpdiss_ave is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dpdiss_ave is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dpdiss_ave /= kgenref_var%dpdiss_ave), " of ", size( var%dpdiss_ave ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%dpdiss_ave)/real(size(var%dpdiss_ave)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dpdiss_ave)/real(size(kgenref_var%dpdiss_ave)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dpdiss_ave 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dpdiss_ave 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dpdiss_ave /= kgenref_var%dpdiss_ave), " of ", size( var%dpdiss_ave ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%dpdiss_ave)/real(size(var%dpdiss_ave)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dpdiss_ave)/real(size(kgenref_var%dpdiss_ave)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dpdiss_ave 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dpdiss_ave 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%phi == kgenref_var%phi)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%phi is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_phi(SIZE(var%phi,dim=1),SIZE(var%phi,dim=2),SIZE(var%phi,dim=3))) 
          ALLOCATE (buf2_phi(SIZE(var%phi,dim=1),SIZE(var%phi,dim=2),SIZE(var%phi,dim=3))) 
          n_phi = COUNT(var%phi /= kgenref_var%phi) 
          WHERE ( ABS(kgenref_var%phi) > kgen_minvalue ) 
              buf1_phi = ((var%phi-kgenref_var%phi)/kgenref_var%phi)**2 
              buf2_phi = (var%phi-kgenref_var%phi)**2 
          ELSEWHERE 
              buf1_phi = (var%phi-kgenref_var%phi)**2 
              buf2_phi = buf1_phi 
          END WHERE   
          nrmsdiff_phi = SQRT(SUM(buf1_phi)/REAL(n_phi)) 
          rmsdiff_phi = SQRT(SUM(buf2_phi)/REAL(n_phi)) 
          IF (nrmsdiff_phi > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%phi is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%phi is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%phi /= kgenref_var%phi), " of ", size( var%phi ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%phi)/real(size(var%phi)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%phi)/real(size(kgenref_var%phi)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_phi 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phi 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%phi /= kgenref_var%phi), " of ", size( var%phi ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%phi)/real(size(var%phi)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%phi)/real(size(kgenref_var%phi)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_phi 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phi 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%omega == kgenref_var%omega)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%omega is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_omega(SIZE(var%omega,dim=1),SIZE(var%omega,dim=2),SIZE(var%omega,dim=3))) 
          ALLOCATE (buf2_omega(SIZE(var%omega,dim=1),SIZE(var%omega,dim=2),SIZE(var%omega,dim=3))) 
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
          IF (nrmsdiff_omega > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%omega is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%omega is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%omega /= kgenref_var%omega), " of ", size( var%omega ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%omega)/real(size(var%omega)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%omega)/real(size(kgenref_var%omega)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_omega 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_omega 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%omega /= kgenref_var%omega), " of ", size( var%omega ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%omega)/real(size(var%omega)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%omega)/real(size(kgenref_var%omega)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_omega 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_omega 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%zeta == kgenref_var%zeta)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%zeta is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_zeta(SIZE(var%zeta,dim=1),SIZE(var%zeta,dim=2),SIZE(var%zeta,dim=3))) 
          ALLOCATE (buf2_zeta(SIZE(var%zeta,dim=1),SIZE(var%zeta,dim=2),SIZE(var%zeta,dim=3))) 
          n_zeta = COUNT(var%zeta /= kgenref_var%zeta) 
          WHERE ( ABS(kgenref_var%zeta) > kgen_minvalue ) 
              buf1_zeta = ((var%zeta-kgenref_var%zeta)/kgenref_var%zeta)**2 
              buf2_zeta = (var%zeta-kgenref_var%zeta)**2 
          ELSEWHERE 
              buf1_zeta = (var%zeta-kgenref_var%zeta)**2 
              buf2_zeta = buf1_zeta 
          END WHERE   
          nrmsdiff_zeta = SQRT(SUM(buf1_zeta)/REAL(n_zeta)) 
          rmsdiff_zeta = SQRT(SUM(buf2_zeta)/REAL(n_zeta)) 
          IF (nrmsdiff_zeta > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%zeta is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%zeta is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%zeta /= kgenref_var%zeta), " of ", size( var%zeta ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%zeta)/real(size(var%zeta)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%zeta)/real(size(kgenref_var%zeta)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_zeta 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zeta 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%zeta /= kgenref_var%zeta), " of ", size( var%zeta ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%zeta)/real(size(var%zeta)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%zeta)/real(size(kgenref_var%zeta)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_zeta 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zeta 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%div == kgenref_var%div)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%div is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_div(SIZE(var%div,dim=1),SIZE(var%div,dim=2),SIZE(var%div,dim=3),SIZE(var%div,dim=4))) 
          ALLOCATE (buf2_div(SIZE(var%div,dim=1),SIZE(var%div,dim=2),SIZE(var%div,dim=3),SIZE(var%div,dim=4))) 
          n_div = COUNT(var%div /= kgenref_var%div) 
          WHERE ( ABS(kgenref_var%div) > kgen_minvalue ) 
              buf1_div = ((var%div-kgenref_var%div)/kgenref_var%div)**2 
              buf2_div = (var%div-kgenref_var%div)**2 
          ELSEWHERE 
              buf1_div = (var%div-kgenref_var%div)**2 
              buf2_div = buf1_div 
          END WHERE   
          nrmsdiff_div = SQRT(SUM(buf1_div)/REAL(n_div)) 
          rmsdiff_div = SQRT(SUM(buf2_div)/REAL(n_div)) 
          IF (nrmsdiff_div > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%div is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%div is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%div /= kgenref_var%div), " of ", size( var%div ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%div)/real(size(var%div)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%div)/real(size(kgenref_var%div)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_div 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_div 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%div /= kgenref_var%div), " of ", size( var%div ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%div)/real(size(var%div)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%div)/real(size(kgenref_var%div)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_div 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_div 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dp == kgenref_var%dp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dp(SIZE(var%dp,dim=1),SIZE(var%dp,dim=2),SIZE(var%dp,dim=3))) 
          ALLOCATE (buf2_dp(SIZE(var%dp,dim=1),SIZE(var%dp,dim=2),SIZE(var%dp,dim=3))) 
          n_dp = COUNT(var%dp /= kgenref_var%dp) 
          WHERE ( ABS(kgenref_var%dp) > kgen_minvalue ) 
              buf1_dp = ((var%dp-kgenref_var%dp)/kgenref_var%dp)**2 
              buf2_dp = (var%dp-kgenref_var%dp)**2 
          ELSEWHERE 
              buf1_dp = (var%dp-kgenref_var%dp)**2 
              buf2_dp = buf1_dp 
          END WHERE   
          nrmsdiff_dp = SQRT(SUM(buf1_dp)/REAL(n_dp)) 
          rmsdiff_dp = SQRT(SUM(buf2_dp)/REAL(n_dp)) 
          IF (nrmsdiff_dp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp /= kgenref_var%dp), " of ", size( var%dp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp)/real(size(var%dp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp)/real(size(kgenref_var%dp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp /= kgenref_var%dp), " of ", size( var%dp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp)/real(size(var%dp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp)/real(size(kgenref_var%dp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%divdp == kgenref_var%divdp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%divdp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_divdp(SIZE(var%divdp,dim=1),SIZE(var%divdp,dim=2),SIZE(var%divdp,dim=3))) 
          ALLOCATE (buf2_divdp(SIZE(var%divdp,dim=1),SIZE(var%divdp,dim=2),SIZE(var%divdp,dim=3))) 
          n_divdp = COUNT(var%divdp /= kgenref_var%divdp) 
          WHERE ( ABS(kgenref_var%divdp) > kgen_minvalue ) 
              buf1_divdp = ((var%divdp-kgenref_var%divdp)/kgenref_var%divdp)**2 
              buf2_divdp = (var%divdp-kgenref_var%divdp)**2 
          ELSEWHERE 
              buf1_divdp = (var%divdp-kgenref_var%divdp)**2 
              buf2_divdp = buf1_divdp 
          END WHERE   
          nrmsdiff_divdp = SQRT(SUM(buf1_divdp)/REAL(n_divdp)) 
          rmsdiff_divdp = SQRT(SUM(buf2_divdp)/REAL(n_divdp)) 
          IF (nrmsdiff_divdp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%divdp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%divdp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%divdp /= kgenref_var%divdp), " of ", size( var%divdp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%divdp)/real(size(var%divdp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%divdp)/real(size(kgenref_var%divdp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_divdp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_divdp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%divdp /= kgenref_var%divdp), " of ", size( var%divdp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%divdp)/real(size(var%divdp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%divdp)/real(size(kgenref_var%divdp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_divdp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_divdp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%divdp_proj == kgenref_var%divdp_proj)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%divdp_proj is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_divdp_proj(SIZE(var%divdp_proj,dim=1),SIZE(var%divdp_proj,dim=2),SIZE(var%divdp_proj,dim=3))) 
          ALLOCATE (buf2_divdp_proj(SIZE(var%divdp_proj,dim=1),SIZE(var%divdp_proj,dim=2),SIZE(var%divdp_proj,dim=3))) 
          n_divdp_proj = COUNT(var%divdp_proj /= kgenref_var%divdp_proj) 
          WHERE ( ABS(kgenref_var%divdp_proj) > kgen_minvalue ) 
              buf1_divdp_proj = ((var%divdp_proj-kgenref_var%divdp_proj)/kgenref_var%divdp_proj)**2 
              buf2_divdp_proj = (var%divdp_proj-kgenref_var%divdp_proj)**2 
          ELSEWHERE 
              buf1_divdp_proj = (var%divdp_proj-kgenref_var%divdp_proj)**2 
              buf2_divdp_proj = buf1_divdp_proj 
          END WHERE   
          nrmsdiff_divdp_proj = SQRT(SUM(buf1_divdp_proj)/REAL(n_divdp_proj)) 
          rmsdiff_divdp_proj = SQRT(SUM(buf2_divdp_proj)/REAL(n_divdp_proj)) 
          IF (nrmsdiff_divdp_proj > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%divdp_proj is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%divdp_proj is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%divdp_proj /= kgenref_var%divdp_proj), " of ", size( var%divdp_proj ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%divdp_proj)/real(size(var%divdp_proj)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%divdp_proj)/real(size(kgenref_var%divdp_proj)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_divdp_proj 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_divdp_proj 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%divdp_proj /= kgenref_var%divdp_proj), " of ", size( var%divdp_proj ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%divdp_proj)/real(size(var%divdp_proj)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%divdp_proj)/real(size(kgenref_var%divdp_proj)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_divdp_proj 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_divdp_proj 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%mass == kgenref_var%mass)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%mass is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_mass(SIZE(var%mass,dim=1))) 
          ALLOCATE (buf2_mass(SIZE(var%mass,dim=1))) 
          n_mass = COUNT(var%mass /= kgenref_var%mass) 
          WHERE ( ABS(kgenref_var%mass) > kgen_minvalue ) 
              buf1_mass = ((var%mass-kgenref_var%mass)/kgenref_var%mass)**2 
              buf2_mass = (var%mass-kgenref_var%mass)**2 
          ELSEWHERE 
              buf1_mass = (var%mass-kgenref_var%mass)**2 
              buf2_mass = buf1_mass 
          END WHERE   
          nrmsdiff_mass = SQRT(SUM(buf1_mass)/REAL(n_mass)) 
          rmsdiff_mass = SQRT(SUM(buf2_mass)/REAL(n_mass)) 
          IF (nrmsdiff_mass > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mass is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mass is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%mass /= kgenref_var%mass), " of ", size( var%mass ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%mass)/real(size(var%mass)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%mass)/real(size(kgenref_var%mass)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_mass 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mass 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%mass /= kgenref_var%mass), " of ", size( var%mass ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%mass)/real(size(var%mass)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%mass)/real(size(kgenref_var%mass)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_mass 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mass 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%fq == kgenref_var%fq)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%fq is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_fq(SIZE(var%fq,dim=1),SIZE(var%fq,dim=2),SIZE(var%fq,dim=3),SIZE(var%fq,dim=4))) 
          ALLOCATE (buf2_fq(SIZE(var%fq,dim=1),SIZE(var%fq,dim=2),SIZE(var%fq,dim=3),SIZE(var%fq,dim=4))) 
          n_fq = COUNT(var%fq /= kgenref_var%fq) 
          WHERE ( ABS(kgenref_var%fq) > kgen_minvalue ) 
              buf1_fq = ((var%fq-kgenref_var%fq)/kgenref_var%fq)**2 
              buf2_fq = (var%fq-kgenref_var%fq)**2 
          ELSEWHERE 
              buf1_fq = (var%fq-kgenref_var%fq)**2 
              buf2_fq = buf1_fq 
          END WHERE   
          nrmsdiff_fq = SQRT(SUM(buf1_fq)/REAL(n_fq)) 
          rmsdiff_fq = SQRT(SUM(buf2_fq)/REAL(n_fq)) 
          IF (nrmsdiff_fq > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fq is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fq is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%fq /= kgenref_var%fq), " of ", size( var%fq ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%fq)/real(size(var%fq)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%fq)/real(size(kgenref_var%fq)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_fq 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fq 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%fq /= kgenref_var%fq), " of ", size( var%fq ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%fq)/real(size(var%fq)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%fq)/real(size(kgenref_var%fq)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_fq 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fq 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%fm == kgenref_var%fm)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%fm is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_fm(SIZE(var%fm,dim=1),SIZE(var%fm,dim=2),SIZE(var%fm,dim=3),SIZE(var%fm,dim=4))) 
          ALLOCATE (buf2_fm(SIZE(var%fm,dim=1),SIZE(var%fm,dim=2),SIZE(var%fm,dim=3),SIZE(var%fm,dim=4))) 
          n_fm = COUNT(var%fm /= kgenref_var%fm) 
          WHERE ( ABS(kgenref_var%fm) > kgen_minvalue ) 
              buf1_fm = ((var%fm-kgenref_var%fm)/kgenref_var%fm)**2 
              buf2_fm = (var%fm-kgenref_var%fm)**2 
          ELSEWHERE 
              buf1_fm = (var%fm-kgenref_var%fm)**2 
              buf2_fm = buf1_fm 
          END WHERE   
          nrmsdiff_fm = SQRT(SUM(buf1_fm)/REAL(n_fm)) 
          rmsdiff_fm = SQRT(SUM(buf2_fm)/REAL(n_fm)) 
          IF (nrmsdiff_fm > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fm is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fm is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%fm /= kgenref_var%fm), " of ", size( var%fm ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%fm)/real(size(var%fm)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%fm)/real(size(kgenref_var%fm)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_fm 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fm 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%fm /= kgenref_var%fm), " of ", size( var%fm ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%fm)/real(size(var%fm)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%fm)/real(size(kgenref_var%fm)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_fm 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fm 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ft == kgenref_var%ft)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ft is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ft(SIZE(var%ft,dim=1),SIZE(var%ft,dim=2),SIZE(var%ft,dim=3))) 
          ALLOCATE (buf2_ft(SIZE(var%ft,dim=1),SIZE(var%ft,dim=2),SIZE(var%ft,dim=3))) 
          n_ft = COUNT(var%ft /= kgenref_var%ft) 
          WHERE ( ABS(kgenref_var%ft) > kgen_minvalue ) 
              buf1_ft = ((var%ft-kgenref_var%ft)/kgenref_var%ft)**2 
              buf2_ft = (var%ft-kgenref_var%ft)**2 
          ELSEWHERE 
              buf1_ft = (var%ft-kgenref_var%ft)**2 
              buf2_ft = buf1_ft 
          END WHERE   
          nrmsdiff_ft = SQRT(SUM(buf1_ft)/REAL(n_ft)) 
          rmsdiff_ft = SQRT(SUM(buf2_ft)/REAL(n_ft)) 
          IF (nrmsdiff_ft > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ft is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ft is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ft /= kgenref_var%ft), " of ", size( var%ft ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ft)/real(size(var%ft)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ft)/real(size(kgenref_var%ft)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ft 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ft 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ft /= kgenref_var%ft), " of ", size( var%ft ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ft)/real(size(var%ft)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ft)/real(size(kgenref_var%ft)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ft 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ft 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%etadot_prescribed == kgenref_var%etadot_prescribed)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%etadot_prescribed is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_etadot_prescribed(SIZE(var%etadot_prescribed,dim=1),SIZE(var%etadot_prescribed,dim=2),SIZE(var%etadot_prescribed,&
          &dim=3))) 
          ALLOCATE &
          &(buf2_etadot_prescribed(SIZE(var%etadot_prescribed,dim=1),SIZE(var%etadot_prescribed,dim=2),SIZE(var%etadot_prescribed,&
          &dim=3))) 
          n_etadot_prescribed = COUNT(var%etadot_prescribed /= kgenref_var%etadot_prescribed) 
          WHERE ( ABS(kgenref_var%etadot_prescribed) > kgen_minvalue ) 
              buf1_etadot_prescribed = ((var%etadot_prescribed-kgenref_var%etadot_prescribed)/kgenref_var%etadot_prescribed)**2 
              buf2_etadot_prescribed = (var%etadot_prescribed-kgenref_var%etadot_prescribed)**2 
          ELSEWHERE 
              buf1_etadot_prescribed = (var%etadot_prescribed-kgenref_var%etadot_prescribed)**2 
              buf2_etadot_prescribed = buf1_etadot_prescribed 
          END WHERE   
          nrmsdiff_etadot_prescribed = SQRT(SUM(buf1_etadot_prescribed)/REAL(n_etadot_prescribed)) 
          rmsdiff_etadot_prescribed = SQRT(SUM(buf2_etadot_prescribed)/REAL(n_etadot_prescribed)) 
          IF (nrmsdiff_etadot_prescribed > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%etadot_prescribed is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%etadot_prescribed is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%etadot_prescribed /= kgenref_var%etadot_prescribed), " of ", size( var%etadot_prescribed ), &
              &" elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%etadot_prescribed)/real(size(var%etadot_prescribed)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%etadot_prescribed)/real(size(kgenref_var%etadot_prescribed)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_etadot_prescribed 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_etadot_prescribed 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%etadot_prescribed /= kgenref_var%etadot_prescribed), " of ", size( var%etadot_prescribed ), &
              &" elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%etadot_prescribed)/real(size(var%etadot_prescribed)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%etadot_prescribed)/real(size(kgenref_var%etadot_prescribed)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_etadot_prescribed 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_etadot_prescribed 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%u_met == kgenref_var%u_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%u_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_u_met(SIZE(var%u_met,dim=1),SIZE(var%u_met,dim=2),SIZE(var%u_met,dim=3))) 
          ALLOCATE (buf2_u_met(SIZE(var%u_met,dim=1),SIZE(var%u_met,dim=2),SIZE(var%u_met,dim=3))) 
          n_u_met = COUNT(var%u_met /= kgenref_var%u_met) 
          WHERE ( ABS(kgenref_var%u_met) > kgen_minvalue ) 
              buf1_u_met = ((var%u_met-kgenref_var%u_met)/kgenref_var%u_met)**2 
              buf2_u_met = (var%u_met-kgenref_var%u_met)**2 
          ELSEWHERE 
              buf1_u_met = (var%u_met-kgenref_var%u_met)**2 
              buf2_u_met = buf1_u_met 
          END WHERE   
          nrmsdiff_u_met = SQRT(SUM(buf1_u_met)/REAL(n_u_met)) 
          rmsdiff_u_met = SQRT(SUM(buf2_u_met)/REAL(n_u_met)) 
          IF (nrmsdiff_u_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%u_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%u_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%u_met /= kgenref_var%u_met), " of ", size( var%u_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%u_met)/real(size(var%u_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%u_met)/real(size(kgenref_var%u_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_u_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%u_met /= kgenref_var%u_met), " of ", size( var%u_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%u_met)/real(size(var%u_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%u_met)/real(size(kgenref_var%u_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_u_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dudt_met == kgenref_var%dudt_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dudt_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dudt_met(SIZE(var%dudt_met,dim=1),SIZE(var%dudt_met,dim=2),SIZE(var%dudt_met,dim=3))) 
          ALLOCATE (buf2_dudt_met(SIZE(var%dudt_met,dim=1),SIZE(var%dudt_met,dim=2),SIZE(var%dudt_met,dim=3))) 
          n_dudt_met = COUNT(var%dudt_met /= kgenref_var%dudt_met) 
          WHERE ( ABS(kgenref_var%dudt_met) > kgen_minvalue ) 
              buf1_dudt_met = ((var%dudt_met-kgenref_var%dudt_met)/kgenref_var%dudt_met)**2 
              buf2_dudt_met = (var%dudt_met-kgenref_var%dudt_met)**2 
          ELSEWHERE 
              buf1_dudt_met = (var%dudt_met-kgenref_var%dudt_met)**2 
              buf2_dudt_met = buf1_dudt_met 
          END WHERE   
          nrmsdiff_dudt_met = SQRT(SUM(buf1_dudt_met)/REAL(n_dudt_met)) 
          rmsdiff_dudt_met = SQRT(SUM(buf2_dudt_met)/REAL(n_dudt_met)) 
          IF (nrmsdiff_dudt_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dudt_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dudt_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dudt_met /= kgenref_var%dudt_met), " of ", size( var%dudt_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dudt_met)/real(size(var%dudt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dudt_met)/real(size(kgenref_var%dudt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dudt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dudt_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dudt_met /= kgenref_var%dudt_met), " of ", size( var%dudt_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dudt_met)/real(size(var%dudt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dudt_met)/real(size(kgenref_var%dudt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dudt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dudt_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%v_met == kgenref_var%v_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%v_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_v_met(SIZE(var%v_met,dim=1),SIZE(var%v_met,dim=2),SIZE(var%v_met,dim=3))) 
          ALLOCATE (buf2_v_met(SIZE(var%v_met,dim=1),SIZE(var%v_met,dim=2),SIZE(var%v_met,dim=3))) 
          n_v_met = COUNT(var%v_met /= kgenref_var%v_met) 
          WHERE ( ABS(kgenref_var%v_met) > kgen_minvalue ) 
              buf1_v_met = ((var%v_met-kgenref_var%v_met)/kgenref_var%v_met)**2 
              buf2_v_met = (var%v_met-kgenref_var%v_met)**2 
          ELSEWHERE 
              buf1_v_met = (var%v_met-kgenref_var%v_met)**2 
              buf2_v_met = buf1_v_met 
          END WHERE   
          nrmsdiff_v_met = SQRT(SUM(buf1_v_met)/REAL(n_v_met)) 
          rmsdiff_v_met = SQRT(SUM(buf2_v_met)/REAL(n_v_met)) 
          IF (nrmsdiff_v_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%v_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%v_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%v_met /= kgenref_var%v_met), " of ", size( var%v_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%v_met)/real(size(var%v_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%v_met)/real(size(kgenref_var%v_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_v_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%v_met /= kgenref_var%v_met), " of ", size( var%v_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%v_met)/real(size(var%v_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%v_met)/real(size(kgenref_var%v_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_v_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_v_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dvdt_met == kgenref_var%dvdt_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dvdt_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dvdt_met(SIZE(var%dvdt_met,dim=1),SIZE(var%dvdt_met,dim=2),SIZE(var%dvdt_met,dim=3))) 
          ALLOCATE (buf2_dvdt_met(SIZE(var%dvdt_met,dim=1),SIZE(var%dvdt_met,dim=2),SIZE(var%dvdt_met,dim=3))) 
          n_dvdt_met = COUNT(var%dvdt_met /= kgenref_var%dvdt_met) 
          WHERE ( ABS(kgenref_var%dvdt_met) > kgen_minvalue ) 
              buf1_dvdt_met = ((var%dvdt_met-kgenref_var%dvdt_met)/kgenref_var%dvdt_met)**2 
              buf2_dvdt_met = (var%dvdt_met-kgenref_var%dvdt_met)**2 
          ELSEWHERE 
              buf1_dvdt_met = (var%dvdt_met-kgenref_var%dvdt_met)**2 
              buf2_dvdt_met = buf1_dvdt_met 
          END WHERE   
          nrmsdiff_dvdt_met = SQRT(SUM(buf1_dvdt_met)/REAL(n_dvdt_met)) 
          rmsdiff_dvdt_met = SQRT(SUM(buf2_dvdt_met)/REAL(n_dvdt_met)) 
          IF (nrmsdiff_dvdt_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dvdt_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dvdt_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dvdt_met /= kgenref_var%dvdt_met), " of ", size( var%dvdt_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dvdt_met)/real(size(var%dvdt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dvdt_met)/real(size(kgenref_var%dvdt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dvdt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dvdt_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dvdt_met /= kgenref_var%dvdt_met), " of ", size( var%dvdt_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dvdt_met)/real(size(var%dvdt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dvdt_met)/real(size(kgenref_var%dvdt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dvdt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dvdt_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%t_met == kgenref_var%t_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%t_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_t_met(SIZE(var%t_met,dim=1),SIZE(var%t_met,dim=2),SIZE(var%t_met,dim=3))) 
          ALLOCATE (buf2_t_met(SIZE(var%t_met,dim=1),SIZE(var%t_met,dim=2),SIZE(var%t_met,dim=3))) 
          n_t_met = COUNT(var%t_met /= kgenref_var%t_met) 
          WHERE ( ABS(kgenref_var%t_met) > kgen_minvalue ) 
              buf1_t_met = ((var%t_met-kgenref_var%t_met)/kgenref_var%t_met)**2 
              buf2_t_met = (var%t_met-kgenref_var%t_met)**2 
          ELSEWHERE 
              buf1_t_met = (var%t_met-kgenref_var%t_met)**2 
              buf2_t_met = buf1_t_met 
          END WHERE   
          nrmsdiff_t_met = SQRT(SUM(buf1_t_met)/REAL(n_t_met)) 
          rmsdiff_t_met = SQRT(SUM(buf2_t_met)/REAL(n_t_met)) 
          IF (nrmsdiff_t_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%t_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%t_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%t_met /= kgenref_var%t_met), " of ", size( var%t_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%t_met)/real(size(var%t_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%t_met)/real(size(kgenref_var%t_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_t_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_t_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%t_met /= kgenref_var%t_met), " of ", size( var%t_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%t_met)/real(size(var%t_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%t_met)/real(size(kgenref_var%t_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_t_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_t_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dtdt_met == kgenref_var%dtdt_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dtdt_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dtdt_met(SIZE(var%dtdt_met,dim=1),SIZE(var%dtdt_met,dim=2),SIZE(var%dtdt_met,dim=3))) 
          ALLOCATE (buf2_dtdt_met(SIZE(var%dtdt_met,dim=1),SIZE(var%dtdt_met,dim=2),SIZE(var%dtdt_met,dim=3))) 
          n_dtdt_met = COUNT(var%dtdt_met /= kgenref_var%dtdt_met) 
          WHERE ( ABS(kgenref_var%dtdt_met) > kgen_minvalue ) 
              buf1_dtdt_met = ((var%dtdt_met-kgenref_var%dtdt_met)/kgenref_var%dtdt_met)**2 
              buf2_dtdt_met = (var%dtdt_met-kgenref_var%dtdt_met)**2 
          ELSEWHERE 
              buf1_dtdt_met = (var%dtdt_met-kgenref_var%dtdt_met)**2 
              buf2_dtdt_met = buf1_dtdt_met 
          END WHERE   
          nrmsdiff_dtdt_met = SQRT(SUM(buf1_dtdt_met)/REAL(n_dtdt_met)) 
          rmsdiff_dtdt_met = SQRT(SUM(buf2_dtdt_met)/REAL(n_dtdt_met)) 
          IF (nrmsdiff_dtdt_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dtdt_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dtdt_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dtdt_met /= kgenref_var%dtdt_met), " of ", size( var%dtdt_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dtdt_met)/real(size(var%dtdt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dtdt_met)/real(size(kgenref_var%dtdt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dtdt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dtdt_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dtdt_met /= kgenref_var%dtdt_met), " of ", size( var%dtdt_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dtdt_met)/real(size(var%dtdt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dtdt_met)/real(size(kgenref_var%dtdt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dtdt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dtdt_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ps_met == kgenref_var%ps_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ps_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ps_met(SIZE(var%ps_met,dim=1),SIZE(var%ps_met,dim=2))) 
          ALLOCATE (buf2_ps_met(SIZE(var%ps_met,dim=1),SIZE(var%ps_met,dim=2))) 
          n_ps_met = COUNT(var%ps_met /= kgenref_var%ps_met) 
          WHERE ( ABS(kgenref_var%ps_met) > kgen_minvalue ) 
              buf1_ps_met = ((var%ps_met-kgenref_var%ps_met)/kgenref_var%ps_met)**2 
              buf2_ps_met = (var%ps_met-kgenref_var%ps_met)**2 
          ELSEWHERE 
              buf1_ps_met = (var%ps_met-kgenref_var%ps_met)**2 
              buf2_ps_met = buf1_ps_met 
          END WHERE   
          nrmsdiff_ps_met = SQRT(SUM(buf1_ps_met)/REAL(n_ps_met)) 
          rmsdiff_ps_met = SQRT(SUM(buf2_ps_met)/REAL(n_ps_met)) 
          IF (nrmsdiff_ps_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ps_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ps_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ps_met /= kgenref_var%ps_met), " of ", size( var%ps_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ps_met)/real(size(var%ps_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ps_met)/real(size(kgenref_var%ps_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ps_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ps_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ps_met /= kgenref_var%ps_met), " of ", size( var%ps_met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ps_met)/real(size(var%ps_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ps_met)/real(size(kgenref_var%ps_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ps_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ps_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dpsdt_met == kgenref_var%dpsdt_met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dpsdt_met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dpsdt_met(SIZE(var%dpsdt_met,dim=1),SIZE(var%dpsdt_met,dim=2))) 
          ALLOCATE (buf2_dpsdt_met(SIZE(var%dpsdt_met,dim=1),SIZE(var%dpsdt_met,dim=2))) 
          n_dpsdt_met = COUNT(var%dpsdt_met /= kgenref_var%dpsdt_met) 
          WHERE ( ABS(kgenref_var%dpsdt_met) > kgen_minvalue ) 
              buf1_dpsdt_met = ((var%dpsdt_met-kgenref_var%dpsdt_met)/kgenref_var%dpsdt_met)**2 
              buf2_dpsdt_met = (var%dpsdt_met-kgenref_var%dpsdt_met)**2 
          ELSEWHERE 
              buf1_dpsdt_met = (var%dpsdt_met-kgenref_var%dpsdt_met)**2 
              buf2_dpsdt_met = buf1_dpsdt_met 
          END WHERE   
          nrmsdiff_dpsdt_met = SQRT(SUM(buf1_dpsdt_met)/REAL(n_dpsdt_met)) 
          rmsdiff_dpsdt_met = SQRT(SUM(buf2_dpsdt_met)/REAL(n_dpsdt_met)) 
          IF (nrmsdiff_dpsdt_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dpsdt_met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dpsdt_met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dpsdt_met /= kgenref_var%dpsdt_met), " of ", size( var%dpsdt_met ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%dpsdt_met)/real(size(var%dpsdt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dpsdt_met)/real(size(kgenref_var%dpsdt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dpsdt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dpsdt_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dpsdt_met /= kgenref_var%dpsdt_met), " of ", size( var%dpsdt_met ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%dpsdt_met)/real(size(var%dpsdt_met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dpsdt_met)/real(size(kgenref_var%dpsdt_met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dpsdt_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dpsdt_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%nudge_factor == kgenref_var%nudge_factor)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%nudge_factor is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_nudge_factor(SIZE(var%nudge_factor,dim=1),SIZE(var%nudge_factor,dim=2),SIZE(var%nudge_factor,dim=3))) 
          ALLOCATE (buf2_nudge_factor(SIZE(var%nudge_factor,dim=1),SIZE(var%nudge_factor,dim=2),SIZE(var%nudge_factor,dim=3))) 
          n_nudge_factor = COUNT(var%nudge_factor /= kgenref_var%nudge_factor) 
          WHERE ( ABS(kgenref_var%nudge_factor) > kgen_minvalue ) 
              buf1_nudge_factor = ((var%nudge_factor-kgenref_var%nudge_factor)/kgenref_var%nudge_factor)**2 
              buf2_nudge_factor = (var%nudge_factor-kgenref_var%nudge_factor)**2 
          ELSEWHERE 
              buf1_nudge_factor = (var%nudge_factor-kgenref_var%nudge_factor)**2 
              buf2_nudge_factor = buf1_nudge_factor 
          END WHERE   
          nrmsdiff_nudge_factor = SQRT(SUM(buf1_nudge_factor)/REAL(n_nudge_factor)) 
          rmsdiff_nudge_factor = SQRT(SUM(buf2_nudge_factor)/REAL(n_nudge_factor)) 
          IF (nrmsdiff_nudge_factor > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nudge_factor is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nudge_factor is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%nudge_factor /= kgenref_var%nudge_factor), " of ", size( var%nudge_factor ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%nudge_factor)/real(size(var%nudge_factor)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%nudge_factor)/real(size(kgenref_var%nudge_factor)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_nudge_factor 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nudge_factor 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%nudge_factor /= kgenref_var%nudge_factor), " of ", size( var%nudge_factor ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%nudge_factor)/real(size(var%nudge_factor)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%nudge_factor)/real(size(kgenref_var%nudge_factor)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_nudge_factor 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_nudge_factor 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%utnd == kgenref_var%utnd)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%utnd is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_utnd(SIZE(var%utnd,dim=1),SIZE(var%utnd,dim=2))) 
          ALLOCATE (buf2_utnd(SIZE(var%utnd,dim=1),SIZE(var%utnd,dim=2))) 
          n_utnd = COUNT(var%utnd /= kgenref_var%utnd) 
          WHERE ( ABS(kgenref_var%utnd) > kgen_minvalue ) 
              buf1_utnd = ((var%utnd-kgenref_var%utnd)/kgenref_var%utnd)**2 
              buf2_utnd = (var%utnd-kgenref_var%utnd)**2 
          ELSEWHERE 
              buf1_utnd = (var%utnd-kgenref_var%utnd)**2 
              buf2_utnd = buf1_utnd 
          END WHERE   
          nrmsdiff_utnd = SQRT(SUM(buf1_utnd)/REAL(n_utnd)) 
          rmsdiff_utnd = SQRT(SUM(buf2_utnd)/REAL(n_utnd)) 
          IF (nrmsdiff_utnd > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%utnd is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%utnd is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%utnd /= kgenref_var%utnd), " of ", size( var%utnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%utnd)/real(size(var%utnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%utnd)/real(size(kgenref_var%utnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_utnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_utnd 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%utnd /= kgenref_var%utnd), " of ", size( var%utnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%utnd)/real(size(var%utnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%utnd)/real(size(kgenref_var%utnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_utnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_utnd 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%vtnd == kgenref_var%vtnd)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vtnd is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_vtnd(SIZE(var%vtnd,dim=1),SIZE(var%vtnd,dim=2))) 
          ALLOCATE (buf2_vtnd(SIZE(var%vtnd,dim=1),SIZE(var%vtnd,dim=2))) 
          n_vtnd = COUNT(var%vtnd /= kgenref_var%vtnd) 
          WHERE ( ABS(kgenref_var%vtnd) > kgen_minvalue ) 
              buf1_vtnd = ((var%vtnd-kgenref_var%vtnd)/kgenref_var%vtnd)**2 
              buf2_vtnd = (var%vtnd-kgenref_var%vtnd)**2 
          ELSEWHERE 
              buf1_vtnd = (var%vtnd-kgenref_var%vtnd)**2 
              buf2_vtnd = buf1_vtnd 
          END WHERE   
          nrmsdiff_vtnd = SQRT(SUM(buf1_vtnd)/REAL(n_vtnd)) 
          rmsdiff_vtnd = SQRT(SUM(buf2_vtnd)/REAL(n_vtnd)) 
          IF (nrmsdiff_vtnd > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vtnd is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vtnd is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vtnd /= kgenref_var%vtnd), " of ", size( var%vtnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vtnd)/real(size(var%vtnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vtnd)/real(size(kgenref_var%vtnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vtnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vtnd 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vtnd /= kgenref_var%vtnd), " of ", size( var%vtnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vtnd)/real(size(var%vtnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vtnd)/real(size(kgenref_var%vtnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vtnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vtnd 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ttnd == kgenref_var%ttnd)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ttnd is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ttnd(SIZE(var%ttnd,dim=1),SIZE(var%ttnd,dim=2))) 
          ALLOCATE (buf2_ttnd(SIZE(var%ttnd,dim=1),SIZE(var%ttnd,dim=2))) 
          n_ttnd = COUNT(var%ttnd /= kgenref_var%ttnd) 
          WHERE ( ABS(kgenref_var%ttnd) > kgen_minvalue ) 
              buf1_ttnd = ((var%ttnd-kgenref_var%ttnd)/kgenref_var%ttnd)**2 
              buf2_ttnd = (var%ttnd-kgenref_var%ttnd)**2 
          ELSEWHERE 
              buf1_ttnd = (var%ttnd-kgenref_var%ttnd)**2 
              buf2_ttnd = buf1_ttnd 
          END WHERE   
          nrmsdiff_ttnd = SQRT(SUM(buf1_ttnd)/REAL(n_ttnd)) 
          rmsdiff_ttnd = SQRT(SUM(buf2_ttnd)/REAL(n_ttnd)) 
          IF (nrmsdiff_ttnd > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ttnd is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ttnd is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ttnd /= kgenref_var%ttnd), " of ", size( var%ttnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ttnd)/real(size(var%ttnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ttnd)/real(size(kgenref_var%ttnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ttnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ttnd 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ttnd /= kgenref_var%ttnd), " of ", size( var%ttnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ttnd)/real(size(var%ttnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ttnd)/real(size(kgenref_var%ttnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ttnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ttnd 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%pecnd == kgenref_var%pecnd)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%pecnd is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_pecnd(SIZE(var%pecnd,dim=1),SIZE(var%pecnd,dim=2),SIZE(var%pecnd,dim=3))) 
          ALLOCATE (buf2_pecnd(SIZE(var%pecnd,dim=1),SIZE(var%pecnd,dim=2),SIZE(var%pecnd,dim=3))) 
          n_pecnd = COUNT(var%pecnd /= kgenref_var%pecnd) 
          WHERE ( ABS(kgenref_var%pecnd) > kgen_minvalue ) 
              buf1_pecnd = ((var%pecnd-kgenref_var%pecnd)/kgenref_var%pecnd)**2 
              buf2_pecnd = (var%pecnd-kgenref_var%pecnd)**2 
          ELSEWHERE 
              buf1_pecnd = (var%pecnd-kgenref_var%pecnd)**2 
              buf2_pecnd = buf1_pecnd 
          END WHERE   
          nrmsdiff_pecnd = SQRT(SUM(buf1_pecnd)/REAL(n_pecnd)) 
          rmsdiff_pecnd = SQRT(SUM(buf2_pecnd)/REAL(n_pecnd)) 
          IF (nrmsdiff_pecnd > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%pecnd is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%pecnd is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%pecnd /= kgenref_var%pecnd), " of ", size( var%pecnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%pecnd)/real(size(var%pecnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%pecnd)/real(size(kgenref_var%pecnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_pecnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pecnd 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%pecnd /= kgenref_var%pecnd), " of ", size( var%pecnd ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%pecnd)/real(size(var%pecnd)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%pecnd)/real(size(kgenref_var%pecnd)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_pecnd 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_pecnd 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_element_mod_derived_state_t 
    
  !verify state subroutine for kv_element_mod_index_t 
  RECURSIVE SUBROUTINE kv_element_mod_index_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(index_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_ia 
      integer :: nrmsdiff_ia, rmsdiff_ia 
      integer, ALLOCATABLE :: buf1_ia(:), buf2_ia(:) 
      INTEGER :: n_ja 
      integer :: nrmsdiff_ja, rmsdiff_ja 
      integer, ALLOCATABLE :: buf1_ja(:), buf2_ja(:) 
      integer :: diff_is 
      integer :: diff_ie 
      integer :: diff_numuniquepts 
      integer :: diff_uniqueptoffset 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ia == kgenref_var%ia)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ia is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ia(SIZE(var%ia,dim=1))) 
          ALLOCATE (buf2_ia(SIZE(var%ia,dim=1))) 
          n_ia = COUNT(var%ia /= kgenref_var%ia) 
          WHERE ( ABS(kgenref_var%ia) > kgen_minvalue ) 
              buf1_ia = ((var%ia-kgenref_var%ia)/kgenref_var%ia)**2 
              buf2_ia = (var%ia-kgenref_var%ia)**2 
          ELSEWHERE 
              buf1_ia = (var%ia-kgenref_var%ia)**2 
              buf2_ia = buf1_ia 
          END WHERE   
          nrmsdiff_ia = SQRT(SUM(buf1_ia)/REAL(n_ia)) 
          rmsdiff_ia = SQRT(SUM(buf2_ia)/REAL(n_ia)) 
          IF (nrmsdiff_ia > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ia is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ia is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ia /= kgenref_var%ia), " of ", size( var%ia ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ia)/real(size(var%ia)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ia)/real(size(kgenref_var%ia)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ia 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ia 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ia /= kgenref_var%ia), " of ", size( var%ia ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ia)/real(size(var%ia)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ia)/real(size(kgenref_var%ia)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ia 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ia 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ja == kgenref_var%ja)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ja is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ja(SIZE(var%ja,dim=1))) 
          ALLOCATE (buf2_ja(SIZE(var%ja,dim=1))) 
          n_ja = COUNT(var%ja /= kgenref_var%ja) 
          WHERE ( ABS(kgenref_var%ja) > kgen_minvalue ) 
              buf1_ja = ((var%ja-kgenref_var%ja)/kgenref_var%ja)**2 
              buf2_ja = (var%ja-kgenref_var%ja)**2 
          ELSEWHERE 
              buf1_ja = (var%ja-kgenref_var%ja)**2 
              buf2_ja = buf1_ja 
          END WHERE   
          nrmsdiff_ja = SQRT(SUM(buf1_ja)/REAL(n_ja)) 
          rmsdiff_ja = SQRT(SUM(buf2_ja)/REAL(n_ja)) 
          IF (nrmsdiff_ja > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ja is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ja is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ja /= kgenref_var%ja), " of ", size( var%ja ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ja)/real(size(var%ja)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ja)/real(size(kgenref_var%ja)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ja 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ja 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ja /= kgenref_var%ja), " of ", size( var%ja ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ja)/real(size(var%ja)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ja)/real(size(kgenref_var%ja)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ja 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ja 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%is == kgenref_var%is) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%is is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_is = ABS(var%is - kgenref_var%is) 
          IF (diff_is <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%is is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%is is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_is 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_is 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ie == kgenref_var%ie) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ie is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ie = ABS(var%ie - kgenref_var%ie) 
          IF (diff_ie <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ie is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ie is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_ie 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_ie 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%numuniquepts == kgenref_var%numuniquepts) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%numuniquepts is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_numuniquepts = ABS(var%numuniquepts - kgenref_var%numuniquepts) 
          IF (diff_numuniquepts <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%numuniquepts is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%numuniquepts is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_numuniquepts 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_numuniquepts 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%uniqueptoffset == kgenref_var%uniqueptoffset) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%uniqueptoffset is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_uniqueptoffset = ABS(var%uniqueptoffset - kgenref_var%uniqueptoffset) 
          IF (diff_uniqueptoffset <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%uniqueptoffset is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%uniqueptoffset is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_uniqueptoffset 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_uniqueptoffset 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_element_mod_index_t 
    
  !verify state subroutine for kv_element_mod_element_t 
  RECURSIVE SUBROUTINE kv_element_mod_element_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(element_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_localid 
      integer :: diff_globalid 
      INTEGER :: idx1_spherep, idx2_spherep 
      INTEGER :: idx1_cartp, idx2_cartp 
      INTEGER :: idx1_corners 
      INTEGER :: n_u2qmap 
      real(KIND=r8) :: nrmsdiff_u2qmap, rmsdiff_u2qmap 
      real(KIND=r8), ALLOCATABLE :: buf1_u2qmap(:,:), buf2_u2qmap(:,:) 
      INTEGER :: idx1_corners3d 
      real(KIND=r8) :: diff_area 
      real(KIND=r8) :: diff_normdinv 
      real(KIND=r8) :: diff_dx_short 
      real(KIND=r8) :: diff_dx_long 
      INTEGER :: n_variable_hyperviscosity 
      real(KIND=r8) :: nrmsdiff_variable_hyperviscosity, rmsdiff_variable_hyperviscosity 
      real(KIND=r8), ALLOCATABLE :: buf1_variable_hyperviscosity(:,:), buf2_variable_hyperviscosity(:,:) 
      real(KIND=r8) :: diff_hv_courant 
      INTEGER :: n_tensorvisc 
      real(KIND=r8) :: nrmsdiff_tensorvisc, rmsdiff_tensorvisc 
      real(KIND=r8), ALLOCATABLE :: buf1_tensorvisc(:,:,:,:), buf2_tensorvisc(:,:,:,:) 
      INTEGER :: n_met 
      real(KIND=r8) :: nrmsdiff_met, rmsdiff_met 
      real(KIND=r8), ALLOCATABLE :: buf1_met(:,:,:,:), buf2_met(:,:,:,:) 
      INTEGER :: n_metinv 
      real(KIND=r8) :: nrmsdiff_metinv, rmsdiff_metinv 
      real(KIND=r8), ALLOCATABLE :: buf1_metinv(:,:,:,:), buf2_metinv(:,:,:,:) 
      INTEGER :: n_metdet 
      real(KIND=r8) :: nrmsdiff_metdet, rmsdiff_metdet 
      real(KIND=r8), ALLOCATABLE :: buf1_metdet(:,:), buf2_metdet(:,:) 
      INTEGER :: n_rmetdet 
      real(KIND=r8) :: nrmsdiff_rmetdet, rmsdiff_rmetdet 
      real(KIND=r8), ALLOCATABLE :: buf1_rmetdet(:,:), buf2_rmetdet(:,:) 
      INTEGER :: n_d 
      real(KIND=r8) :: nrmsdiff_d, rmsdiff_d 
      real(KIND=r8), ALLOCATABLE :: buf1_d(:,:,:,:), buf2_d(:,:,:,:) 
      INTEGER :: n_dinv 
      real(KIND=r8) :: nrmsdiff_dinv, rmsdiff_dinv 
      real(KIND=r8), ALLOCATABLE :: buf1_dinv(:,:,:,:), buf2_dinv(:,:,:,:) 
      INTEGER :: n_sub_elem_mass_flux 
      real(KIND=r8) :: nrmsdiff_sub_elem_mass_flux, rmsdiff_sub_elem_mass_flux 
      real(KIND=r8), ALLOCATABLE :: buf1_sub_elem_mass_flux(:,:,:,:), buf2_sub_elem_mass_flux(:,:,:,:) 
      INTEGER :: n_vec_sphere2cart 
      real(KIND=r8) :: nrmsdiff_vec_sphere2cart, rmsdiff_vec_sphere2cart 
      real(KIND=r8), ALLOCATABLE :: buf1_vec_sphere2cart(:,:,:,:), buf2_vec_sphere2cart(:,:,:,:) 
      INTEGER :: n_mp 
      real(KIND=r8) :: nrmsdiff_mp, rmsdiff_mp 
      real(KIND=r8), ALLOCATABLE :: buf1_mp(:,:), buf2_mp(:,:) 
      INTEGER :: n_rmp 
      real(KIND=r8) :: nrmsdiff_rmp, rmsdiff_rmp 
      real(KIND=r8), ALLOCATABLE :: buf1_rmp(:,:), buf2_rmp(:,:) 
      INTEGER :: n_spheremp 
      real(KIND=r8) :: nrmsdiff_spheremp, rmsdiff_spheremp 
      real(KIND=r8), ALLOCATABLE :: buf1_spheremp(:,:), buf2_spheremp(:,:) 
      INTEGER :: n_rspheremp 
      real(KIND=r8) :: nrmsdiff_rspheremp, rmsdiff_rspheremp 
      real(KIND=r8), ALLOCATABLE :: buf1_rspheremp(:,:), buf2_rspheremp(:,:) 
      INTEGER :: n_gdofp 
      integer(KIND=i8) :: nrmsdiff_gdofp, rmsdiff_gdofp 
      integer(KIND=i8), ALLOCATABLE :: buf1_gdofp(:,:), buf2_gdofp(:,:) 
      INTEGER :: n_fcor 
      real(KIND=r8) :: nrmsdiff_fcor, rmsdiff_fcor 
      real(KIND=r8), ALLOCATABLE :: buf1_fcor(:,:), buf2_fcor(:,:) 
      integer :: diff_facenum 
      integer :: diff_dummy 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%localid == kgenref_var%localid) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%localid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_localid = ABS(var%localid - kgenref_var%localid) 
          IF (diff_localid <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%localid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%localid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_localid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_localid 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%globalid == kgenref_var%globalid) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%globalid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_globalid = ABS(var%globalid - kgenref_var%globalid) 
          IF (diff_globalid <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%globalid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%globalid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_globalid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_globalid 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      DO   idx1_spherep = LBOUND(var%spherep,1), UBOUND(var%spherep,1) 
          DO   idx2_spherep = LBOUND(var%spherep,2), UBOUND(var%spherep,2) 
              CALL kv_kgen_coordinate_systems_mod_typesubp0(trim(adjustl(varname))//"%spherep", comp_check_status, &
              &var%spherep(idx1_spherep,idx2_spherep), kgenref_var%spherep(idx1_spherep,idx2_spherep)) 
          END DO   
      END DO   
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%spherep", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%spherep is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%spherep is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      DO   idx1_cartp = LBOUND(var%cartp,1), UBOUND(var%cartp,1) 
          DO   idx2_cartp = LBOUND(var%cartp,2), UBOUND(var%cartp,2) 
              CALL kv_coordinate_systems_mod_cartesian2d_t(trim(adjustl(varname))//"%cartp", comp_check_status, &
              &var%cartp(idx1_cartp,idx2_cartp), kgenref_var%cartp(idx1_cartp,idx2_cartp)) 
          END DO   
      END DO   
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%cartp", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%cartp is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%cartp is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      DO   idx1_corners = LBOUND(var%corners,1), UBOUND(var%corners,1) 
          CALL kv_coordinate_systems_mod_cartesian2d_t(trim(adjustl(varname))//"%corners", comp_check_status, &
          &var%corners(idx1_corners), kgenref_var%corners(idx1_corners)) 
      END DO   
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%corners", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%corners is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%corners is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%u2qmap == kgenref_var%u2qmap)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%u2qmap is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_u2qmap(SIZE(var%u2qmap,dim=1),SIZE(var%u2qmap,dim=2))) 
          ALLOCATE (buf2_u2qmap(SIZE(var%u2qmap,dim=1),SIZE(var%u2qmap,dim=2))) 
          n_u2qmap = COUNT(var%u2qmap /= kgenref_var%u2qmap) 
          WHERE ( ABS(kgenref_var%u2qmap) > kgen_minvalue ) 
              buf1_u2qmap = ((var%u2qmap-kgenref_var%u2qmap)/kgenref_var%u2qmap)**2 
              buf2_u2qmap = (var%u2qmap-kgenref_var%u2qmap)**2 
          ELSEWHERE 
              buf1_u2qmap = (var%u2qmap-kgenref_var%u2qmap)**2 
              buf2_u2qmap = buf1_u2qmap 
          END WHERE   
          nrmsdiff_u2qmap = SQRT(SUM(buf1_u2qmap)/REAL(n_u2qmap)) 
          rmsdiff_u2qmap = SQRT(SUM(buf2_u2qmap)/REAL(n_u2qmap)) 
          IF (nrmsdiff_u2qmap > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%u2qmap is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%u2qmap is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%u2qmap /= kgenref_var%u2qmap), " of ", size( var%u2qmap ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%u2qmap)/real(size(var%u2qmap)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%u2qmap)/real(size(kgenref_var%u2qmap)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_u2qmap 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u2qmap 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%u2qmap /= kgenref_var%u2qmap), " of ", size( var%u2qmap ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%u2qmap)/real(size(var%u2qmap)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%u2qmap)/real(size(kgenref_var%u2qmap)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_u2qmap 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_u2qmap 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      DO   idx1_corners3d = LBOUND(var%corners3d,1), UBOUND(var%corners3d,1) 
          CALL kv_coordinate_systems_mod_cartesian3d_t(trim(adjustl(varname))//"%corners3d", comp_check_status, &
          &var%corners3d(idx1_corners3d), kgenref_var%corners3d(idx1_corners3d)) 
      END DO   
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%corners3d", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%corners3d is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%corners3d is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%area == kgenref_var%area) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%area is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_area = ABS(var%area - kgenref_var%area) 
          IF (diff_area <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%area is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%area is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_area 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_area 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%normdinv == kgenref_var%normdinv) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%normdinv is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_normdinv = ABS(var%normdinv - kgenref_var%normdinv) 
          IF (diff_normdinv <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%normdinv is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%normdinv is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_normdinv 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_normdinv 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dx_short == kgenref_var%dx_short) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dx_short is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dx_short = ABS(var%dx_short - kgenref_var%dx_short) 
          IF (diff_dx_short <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dx_short is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dx_short is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dx_short 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dx_short 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dx_long == kgenref_var%dx_long) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dx_long is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dx_long = ABS(var%dx_long - kgenref_var%dx_long) 
          IF (diff_dx_long <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dx_long is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dx_long is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dx_long 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dx_long 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%variable_hyperviscosity == kgenref_var%variable_hyperviscosity)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%variable_hyperviscosity is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_variable_hyperviscosity(SIZE(var%variable_hyperviscosity,dim=1),SIZE(var%variable_hyperviscosity,dim=2))) 
          ALLOCATE &
          &(buf2_variable_hyperviscosity(SIZE(var%variable_hyperviscosity,dim=1),SIZE(var%variable_hyperviscosity,dim=2))) 
          n_variable_hyperviscosity = COUNT(var%variable_hyperviscosity /= kgenref_var%variable_hyperviscosity) 
          WHERE ( ABS(kgenref_var%variable_hyperviscosity) > kgen_minvalue ) 
              buf1_variable_hyperviscosity = &
              &((var%variable_hyperviscosity-kgenref_var%variable_hyperviscosity)/kgenref_var%variable_hyperviscosity)**2 
              buf2_variable_hyperviscosity = (var%variable_hyperviscosity-kgenref_var%variable_hyperviscosity)**2 
          ELSEWHERE 
              buf1_variable_hyperviscosity = (var%variable_hyperviscosity-kgenref_var%variable_hyperviscosity)**2 
              buf2_variable_hyperviscosity = buf1_variable_hyperviscosity 
          END WHERE   
          nrmsdiff_variable_hyperviscosity = SQRT(SUM(buf1_variable_hyperviscosity)/REAL(n_variable_hyperviscosity)) 
          rmsdiff_variable_hyperviscosity = SQRT(SUM(buf2_variable_hyperviscosity)/REAL(n_variable_hyperviscosity)) 
          IF (nrmsdiff_variable_hyperviscosity > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%variable_hyperviscosity is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%variable_hyperviscosity is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%variable_hyperviscosity /= kgenref_var%variable_hyperviscosity), " of ", size( &
              &var%variable_hyperviscosity ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%variable_hyperviscosity)/real(size(var%variable_hyperviscosity)) 
              WRITE (*, *) "Average - reference ", &
              &sum(kgenref_var%variable_hyperviscosity)/real(size(kgenref_var%variable_hyperviscosity)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_variable_hyperviscosity 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_variable_hyperviscosity 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%variable_hyperviscosity /= kgenref_var%variable_hyperviscosity), " of ", size( &
              &var%variable_hyperviscosity ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%variable_hyperviscosity)/real(size(var%variable_hyperviscosity)) 
              WRITE (*, *) "Average - reference ", &
              &sum(kgenref_var%variable_hyperviscosity)/real(size(kgenref_var%variable_hyperviscosity)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_variable_hyperviscosity 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_variable_hyperviscosity 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%hv_courant == kgenref_var%hv_courant) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%hv_courant is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_hv_courant = ABS(var%hv_courant - kgenref_var%hv_courant) 
          IF (diff_hv_courant <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%hv_courant is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%hv_courant is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_hv_courant 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_hv_courant 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%tensorvisc == kgenref_var%tensorvisc)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%tensorvisc is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_tensorvisc(SIZE(var%tensorvisc,dim=1),SIZE(var%tensorvisc,dim=2),SIZE(var%tensorvisc,dim=3),SIZE(var%tensorvisc,d&
          &im=4))) 
          ALLOCATE &
          &(buf2_tensorvisc(SIZE(var%tensorvisc,dim=1),SIZE(var%tensorvisc,dim=2),SIZE(var%tensorvisc,dim=3),SIZE(var%tensorvisc,d&
          &im=4))) 
          n_tensorvisc = COUNT(var%tensorvisc /= kgenref_var%tensorvisc) 
          WHERE ( ABS(kgenref_var%tensorvisc) > kgen_minvalue ) 
              buf1_tensorvisc = ((var%tensorvisc-kgenref_var%tensorvisc)/kgenref_var%tensorvisc)**2 
              buf2_tensorvisc = (var%tensorvisc-kgenref_var%tensorvisc)**2 
          ELSEWHERE 
              buf1_tensorvisc = (var%tensorvisc-kgenref_var%tensorvisc)**2 
              buf2_tensorvisc = buf1_tensorvisc 
          END WHERE   
          nrmsdiff_tensorvisc = SQRT(SUM(buf1_tensorvisc)/REAL(n_tensorvisc)) 
          rmsdiff_tensorvisc = SQRT(SUM(buf2_tensorvisc)/REAL(n_tensorvisc)) 
          IF (nrmsdiff_tensorvisc > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%tensorvisc is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%tensorvisc is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%tensorvisc /= kgenref_var%tensorvisc), " of ", size( var%tensorvisc ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%tensorvisc)/real(size(var%tensorvisc)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%tensorvisc)/real(size(kgenref_var%tensorvisc)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_tensorvisc 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tensorvisc 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%tensorvisc /= kgenref_var%tensorvisc), " of ", size( var%tensorvisc ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%tensorvisc)/real(size(var%tensorvisc)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%tensorvisc)/real(size(kgenref_var%tensorvisc)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_tensorvisc 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tensorvisc 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      CALL kv_gridgraph_mod_gridvertex_t("vertex", comp_check_status, var%vertex, kgenref_var%vertex) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%vertex", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vertex is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vertex is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      CALL kv_edgetype_mod_edgedescriptor_t("desc", comp_check_status, var%desc, kgenref_var%desc) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%desc", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%desc is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%desc is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      CALL kv_element_mod_elem_state_t("state", comp_check_status, var%state, kgenref_var%state) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%state", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%state is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%state is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      CALL kv_element_mod_derived_state_t("derived", comp_check_status, var%derived, kgenref_var%derived) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%derived", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%derived is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%derived is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%met == kgenref_var%met)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%met is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_met(SIZE(var%met,dim=1),SIZE(var%met,dim=2),SIZE(var%met,dim=3),SIZE(var%met,dim=4))) 
          ALLOCATE (buf2_met(SIZE(var%met,dim=1),SIZE(var%met,dim=2),SIZE(var%met,dim=3),SIZE(var%met,dim=4))) 
          n_met = COUNT(var%met /= kgenref_var%met) 
          WHERE ( ABS(kgenref_var%met) > kgen_minvalue ) 
              buf1_met = ((var%met-kgenref_var%met)/kgenref_var%met)**2 
              buf2_met = (var%met-kgenref_var%met)**2 
          ELSEWHERE 
              buf1_met = (var%met-kgenref_var%met)**2 
              buf2_met = buf1_met 
          END WHERE   
          nrmsdiff_met = SQRT(SUM(buf1_met)/REAL(n_met)) 
          rmsdiff_met = SQRT(SUM(buf2_met)/REAL(n_met)) 
          IF (nrmsdiff_met > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%met is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%met is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%met /= kgenref_var%met), " of ", size( var%met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%met)/real(size(var%met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%met)/real(size(kgenref_var%met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_met 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%met /= kgenref_var%met), " of ", size( var%met ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%met)/real(size(var%met)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%met)/real(size(kgenref_var%met)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_met 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_met 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%metinv == kgenref_var%metinv)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%metinv is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_metinv(SIZE(var%metinv,dim=1),SIZE(var%metinv,dim=2),SIZE(var%metinv,dim=3),SIZE(var%metinv,dim=4))) 
          ALLOCATE (buf2_metinv(SIZE(var%metinv,dim=1),SIZE(var%metinv,dim=2),SIZE(var%metinv,dim=3),SIZE(var%metinv,dim=4))) 
          n_metinv = COUNT(var%metinv /= kgenref_var%metinv) 
          WHERE ( ABS(kgenref_var%metinv) > kgen_minvalue ) 
              buf1_metinv = ((var%metinv-kgenref_var%metinv)/kgenref_var%metinv)**2 
              buf2_metinv = (var%metinv-kgenref_var%metinv)**2 
          ELSEWHERE 
              buf1_metinv = (var%metinv-kgenref_var%metinv)**2 
              buf2_metinv = buf1_metinv 
          END WHERE   
          nrmsdiff_metinv = SQRT(SUM(buf1_metinv)/REAL(n_metinv)) 
          rmsdiff_metinv = SQRT(SUM(buf2_metinv)/REAL(n_metinv)) 
          IF (nrmsdiff_metinv > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%metinv is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%metinv is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%metinv /= kgenref_var%metinv), " of ", size( var%metinv ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%metinv)/real(size(var%metinv)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%metinv)/real(size(kgenref_var%metinv)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_metinv 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_metinv 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%metinv /= kgenref_var%metinv), " of ", size( var%metinv ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%metinv)/real(size(var%metinv)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%metinv)/real(size(kgenref_var%metinv)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_metinv 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_metinv 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%metdet == kgenref_var%metdet)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%metdet is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_metdet(SIZE(var%metdet,dim=1),SIZE(var%metdet,dim=2))) 
          ALLOCATE (buf2_metdet(SIZE(var%metdet,dim=1),SIZE(var%metdet,dim=2))) 
          n_metdet = COUNT(var%metdet /= kgenref_var%metdet) 
          WHERE ( ABS(kgenref_var%metdet) > kgen_minvalue ) 
              buf1_metdet = ((var%metdet-kgenref_var%metdet)/kgenref_var%metdet)**2 
              buf2_metdet = (var%metdet-kgenref_var%metdet)**2 
          ELSEWHERE 
              buf1_metdet = (var%metdet-kgenref_var%metdet)**2 
              buf2_metdet = buf1_metdet 
          END WHERE   
          nrmsdiff_metdet = SQRT(SUM(buf1_metdet)/REAL(n_metdet)) 
          rmsdiff_metdet = SQRT(SUM(buf2_metdet)/REAL(n_metdet)) 
          IF (nrmsdiff_metdet > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%metdet is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%metdet is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%metdet /= kgenref_var%metdet), " of ", size( var%metdet ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%metdet)/real(size(var%metdet)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%metdet)/real(size(kgenref_var%metdet)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_metdet 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_metdet 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%metdet /= kgenref_var%metdet), " of ", size( var%metdet ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%metdet)/real(size(var%metdet)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%metdet)/real(size(kgenref_var%metdet)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_metdet 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_metdet 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%rmetdet == kgenref_var%rmetdet)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%rmetdet is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_rmetdet(SIZE(var%rmetdet,dim=1),SIZE(var%rmetdet,dim=2))) 
          ALLOCATE (buf2_rmetdet(SIZE(var%rmetdet,dim=1),SIZE(var%rmetdet,dim=2))) 
          n_rmetdet = COUNT(var%rmetdet /= kgenref_var%rmetdet) 
          WHERE ( ABS(kgenref_var%rmetdet) > kgen_minvalue ) 
              buf1_rmetdet = ((var%rmetdet-kgenref_var%rmetdet)/kgenref_var%rmetdet)**2 
              buf2_rmetdet = (var%rmetdet-kgenref_var%rmetdet)**2 
          ELSEWHERE 
              buf1_rmetdet = (var%rmetdet-kgenref_var%rmetdet)**2 
              buf2_rmetdet = buf1_rmetdet 
          END WHERE   
          nrmsdiff_rmetdet = SQRT(SUM(buf1_rmetdet)/REAL(n_rmetdet)) 
          rmsdiff_rmetdet = SQRT(SUM(buf2_rmetdet)/REAL(n_rmetdet)) 
          IF (nrmsdiff_rmetdet > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rmetdet is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rmetdet is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rmetdet /= kgenref_var%rmetdet), " of ", size( var%rmetdet ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%rmetdet)/real(size(var%rmetdet)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rmetdet)/real(size(kgenref_var%rmetdet)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rmetdet 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rmetdet 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rmetdet /= kgenref_var%rmetdet), " of ", size( var%rmetdet ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%rmetdet)/real(size(var%rmetdet)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rmetdet)/real(size(kgenref_var%rmetdet)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rmetdet 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rmetdet 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%d == kgenref_var%d)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%d is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_d(SIZE(var%d,dim=1),SIZE(var%d,dim=2),SIZE(var%d,dim=3),SIZE(var%d,dim=4))) 
          ALLOCATE (buf2_d(SIZE(var%d,dim=1),SIZE(var%d,dim=2),SIZE(var%d,dim=3),SIZE(var%d,dim=4))) 
          n_d = COUNT(var%d /= kgenref_var%d) 
          WHERE ( ABS(kgenref_var%d) > kgen_minvalue ) 
              buf1_d = ((var%d-kgenref_var%d)/kgenref_var%d)**2 
              buf2_d = (var%d-kgenref_var%d)**2 
          ELSEWHERE 
              buf1_d = (var%d-kgenref_var%d)**2 
              buf2_d = buf1_d 
          END WHERE   
          nrmsdiff_d = SQRT(SUM(buf1_d)/REAL(n_d)) 
          rmsdiff_d = SQRT(SUM(buf2_d)/REAL(n_d)) 
          IF (nrmsdiff_d > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%d is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%d is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%d /= kgenref_var%d), " of ", size( var%d ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%d)/real(size(var%d)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%d)/real(size(kgenref_var%d)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_d 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_d 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%d /= kgenref_var%d), " of ", size( var%d ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%d)/real(size(var%d)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%d)/real(size(kgenref_var%d)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_d 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_d 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dinv == kgenref_var%dinv)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dinv is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dinv(SIZE(var%dinv,dim=1),SIZE(var%dinv,dim=2),SIZE(var%dinv,dim=3),SIZE(var%dinv,dim=4))) 
          ALLOCATE (buf2_dinv(SIZE(var%dinv,dim=1),SIZE(var%dinv,dim=2),SIZE(var%dinv,dim=3),SIZE(var%dinv,dim=4))) 
          n_dinv = COUNT(var%dinv /= kgenref_var%dinv) 
          WHERE ( ABS(kgenref_var%dinv) > kgen_minvalue ) 
              buf1_dinv = ((var%dinv-kgenref_var%dinv)/kgenref_var%dinv)**2 
              buf2_dinv = (var%dinv-kgenref_var%dinv)**2 
          ELSEWHERE 
              buf1_dinv = (var%dinv-kgenref_var%dinv)**2 
              buf2_dinv = buf1_dinv 
          END WHERE   
          nrmsdiff_dinv = SQRT(SUM(buf1_dinv)/REAL(n_dinv)) 
          rmsdiff_dinv = SQRT(SUM(buf2_dinv)/REAL(n_dinv)) 
          IF (nrmsdiff_dinv > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dinv is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dinv is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dinv /= kgenref_var%dinv), " of ", size( var%dinv ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dinv)/real(size(var%dinv)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dinv)/real(size(kgenref_var%dinv)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dinv 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dinv 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dinv /= kgenref_var%dinv), " of ", size( var%dinv ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dinv)/real(size(var%dinv)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dinv)/real(size(kgenref_var%dinv)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dinv 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dinv 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%sub_elem_mass_flux == kgenref_var%sub_elem_mass_flux)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%sub_elem_mass_flux is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_sub_elem_mass_flux(SIZE(var%sub_elem_mass_flux,dim=1),SIZE(var%sub_elem_mass_flux,dim=2),SIZE(var%sub_elem_mass_f&
          &lux,dim=3),SIZE(var%sub_elem_mass_flux,dim=4))) 
          ALLOCATE &
          &(buf2_sub_elem_mass_flux(SIZE(var%sub_elem_mass_flux,dim=1),SIZE(var%sub_elem_mass_flux,dim=2),SIZE(var%sub_elem_mass_f&
          &lux,dim=3),SIZE(var%sub_elem_mass_flux,dim=4))) 
          n_sub_elem_mass_flux = COUNT(var%sub_elem_mass_flux /= kgenref_var%sub_elem_mass_flux) 
          WHERE ( ABS(kgenref_var%sub_elem_mass_flux) > kgen_minvalue ) 
              buf1_sub_elem_mass_flux = &
              &((var%sub_elem_mass_flux-kgenref_var%sub_elem_mass_flux)/kgenref_var%sub_elem_mass_flux)**2 
              buf2_sub_elem_mass_flux = (var%sub_elem_mass_flux-kgenref_var%sub_elem_mass_flux)**2 
          ELSEWHERE 
              buf1_sub_elem_mass_flux = (var%sub_elem_mass_flux-kgenref_var%sub_elem_mass_flux)**2 
              buf2_sub_elem_mass_flux = buf1_sub_elem_mass_flux 
          END WHERE   
          nrmsdiff_sub_elem_mass_flux = SQRT(SUM(buf1_sub_elem_mass_flux)/REAL(n_sub_elem_mass_flux)) 
          rmsdiff_sub_elem_mass_flux = SQRT(SUM(buf2_sub_elem_mass_flux)/REAL(n_sub_elem_mass_flux)) 
          IF (nrmsdiff_sub_elem_mass_flux > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sub_elem_mass_flux is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sub_elem_mass_flux is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%sub_elem_mass_flux /= kgenref_var%sub_elem_mass_flux), " of ", size( var%sub_elem_mass_flux &
              &), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%sub_elem_mass_flux)/real(size(var%sub_elem_mass_flux)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%sub_elem_mass_flux)/real(size(kgenref_var%sub_elem_mass_flux)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_sub_elem_mass_flux 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sub_elem_mass_flux 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%sub_elem_mass_flux /= kgenref_var%sub_elem_mass_flux), " of ", size( var%sub_elem_mass_flux &
              &), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%sub_elem_mass_flux)/real(size(var%sub_elem_mass_flux)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%sub_elem_mass_flux)/real(size(kgenref_var%sub_elem_mass_flux)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_sub_elem_mass_flux 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_sub_elem_mass_flux 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%vec_sphere2cart == kgenref_var%vec_sphere2cart)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vec_sphere2cart is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_vec_sphere2cart(SIZE(var%vec_sphere2cart,dim=1),SIZE(var%vec_sphere2cart,dim=2),SIZE(var%vec_sphere2cart,dim=3),S&
          &IZE(var%vec_sphere2cart,dim=4))) 
          ALLOCATE &
          &(buf2_vec_sphere2cart(SIZE(var%vec_sphere2cart,dim=1),SIZE(var%vec_sphere2cart,dim=2),SIZE(var%vec_sphere2cart,dim=3),S&
          &IZE(var%vec_sphere2cart,dim=4))) 
          n_vec_sphere2cart = COUNT(var%vec_sphere2cart /= kgenref_var%vec_sphere2cart) 
          WHERE ( ABS(kgenref_var%vec_sphere2cart) > kgen_minvalue ) 
              buf1_vec_sphere2cart = ((var%vec_sphere2cart-kgenref_var%vec_sphere2cart)/kgenref_var%vec_sphere2cart)**2 
              buf2_vec_sphere2cart = (var%vec_sphere2cart-kgenref_var%vec_sphere2cart)**2 
          ELSEWHERE 
              buf1_vec_sphere2cart = (var%vec_sphere2cart-kgenref_var%vec_sphere2cart)**2 
              buf2_vec_sphere2cart = buf1_vec_sphere2cart 
          END WHERE   
          nrmsdiff_vec_sphere2cart = SQRT(SUM(buf1_vec_sphere2cart)/REAL(n_vec_sphere2cart)) 
          rmsdiff_vec_sphere2cart = SQRT(SUM(buf2_vec_sphere2cart)/REAL(n_vec_sphere2cart)) 
          IF (nrmsdiff_vec_sphere2cart > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vec_sphere2cart is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vec_sphere2cart is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vec_sphere2cart /= kgenref_var%vec_sphere2cart), " of ", size( var%vec_sphere2cart ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vec_sphere2cart)/real(size(var%vec_sphere2cart)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vec_sphere2cart)/real(size(kgenref_var%vec_sphere2cart)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vec_sphere2cart 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vec_sphere2cart 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vec_sphere2cart /= kgenref_var%vec_sphere2cart), " of ", size( var%vec_sphere2cart ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vec_sphere2cart)/real(size(var%vec_sphere2cart)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vec_sphere2cart)/real(size(kgenref_var%vec_sphere2cart)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vec_sphere2cart 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vec_sphere2cart 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%mp == kgenref_var%mp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%mp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_mp(SIZE(var%mp,dim=1),SIZE(var%mp,dim=2))) 
          ALLOCATE (buf2_mp(SIZE(var%mp,dim=1),SIZE(var%mp,dim=2))) 
          n_mp = COUNT(var%mp /= kgenref_var%mp) 
          WHERE ( ABS(kgenref_var%mp) > kgen_minvalue ) 
              buf1_mp = ((var%mp-kgenref_var%mp)/kgenref_var%mp)**2 
              buf2_mp = (var%mp-kgenref_var%mp)**2 
          ELSEWHERE 
              buf1_mp = (var%mp-kgenref_var%mp)**2 
              buf2_mp = buf1_mp 
          END WHERE   
          nrmsdiff_mp = SQRT(SUM(buf1_mp)/REAL(n_mp)) 
          rmsdiff_mp = SQRT(SUM(buf2_mp)/REAL(n_mp)) 
          IF (nrmsdiff_mp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%mp /= kgenref_var%mp), " of ", size( var%mp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%mp)/real(size(var%mp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%mp)/real(size(kgenref_var%mp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_mp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%mp /= kgenref_var%mp), " of ", size( var%mp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%mp)/real(size(var%mp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%mp)/real(size(kgenref_var%mp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_mp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_mp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%rmp == kgenref_var%rmp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%rmp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_rmp(SIZE(var%rmp,dim=1),SIZE(var%rmp,dim=2))) 
          ALLOCATE (buf2_rmp(SIZE(var%rmp,dim=1),SIZE(var%rmp,dim=2))) 
          n_rmp = COUNT(var%rmp /= kgenref_var%rmp) 
          WHERE ( ABS(kgenref_var%rmp) > kgen_minvalue ) 
              buf1_rmp = ((var%rmp-kgenref_var%rmp)/kgenref_var%rmp)**2 
              buf2_rmp = (var%rmp-kgenref_var%rmp)**2 
          ELSEWHERE 
              buf1_rmp = (var%rmp-kgenref_var%rmp)**2 
              buf2_rmp = buf1_rmp 
          END WHERE   
          nrmsdiff_rmp = SQRT(SUM(buf1_rmp)/REAL(n_rmp)) 
          rmsdiff_rmp = SQRT(SUM(buf2_rmp)/REAL(n_rmp)) 
          IF (nrmsdiff_rmp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rmp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rmp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rmp /= kgenref_var%rmp), " of ", size( var%rmp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%rmp)/real(size(var%rmp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rmp)/real(size(kgenref_var%rmp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rmp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rmp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rmp /= kgenref_var%rmp), " of ", size( var%rmp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%rmp)/real(size(var%rmp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rmp)/real(size(kgenref_var%rmp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rmp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rmp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%spheremp == kgenref_var%spheremp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%spheremp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_spheremp(SIZE(var%spheremp,dim=1),SIZE(var%spheremp,dim=2))) 
          ALLOCATE (buf2_spheremp(SIZE(var%spheremp,dim=1),SIZE(var%spheremp,dim=2))) 
          n_spheremp = COUNT(var%spheremp /= kgenref_var%spheremp) 
          WHERE ( ABS(kgenref_var%spheremp) > kgen_minvalue ) 
              buf1_spheremp = ((var%spheremp-kgenref_var%spheremp)/kgenref_var%spheremp)**2 
              buf2_spheremp = (var%spheremp-kgenref_var%spheremp)**2 
          ELSEWHERE 
              buf1_spheremp = (var%spheremp-kgenref_var%spheremp)**2 
              buf2_spheremp = buf1_spheremp 
          END WHERE   
          nrmsdiff_spheremp = SQRT(SUM(buf1_spheremp)/REAL(n_spheremp)) 
          rmsdiff_spheremp = SQRT(SUM(buf2_spheremp)/REAL(n_spheremp)) 
          IF (nrmsdiff_spheremp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spheremp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spheremp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%spheremp /= kgenref_var%spheremp), " of ", size( var%spheremp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%spheremp)/real(size(var%spheremp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%spheremp)/real(size(kgenref_var%spheremp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_spheremp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_spheremp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%spheremp /= kgenref_var%spheremp), " of ", size( var%spheremp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%spheremp)/real(size(var%spheremp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%spheremp)/real(size(kgenref_var%spheremp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_spheremp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_spheremp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%rspheremp == kgenref_var%rspheremp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%rspheremp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_rspheremp(SIZE(var%rspheremp,dim=1),SIZE(var%rspheremp,dim=2))) 
          ALLOCATE (buf2_rspheremp(SIZE(var%rspheremp,dim=1),SIZE(var%rspheremp,dim=2))) 
          n_rspheremp = COUNT(var%rspheremp /= kgenref_var%rspheremp) 
          WHERE ( ABS(kgenref_var%rspheremp) > kgen_minvalue ) 
              buf1_rspheremp = ((var%rspheremp-kgenref_var%rspheremp)/kgenref_var%rspheremp)**2 
              buf2_rspheremp = (var%rspheremp-kgenref_var%rspheremp)**2 
          ELSEWHERE 
              buf1_rspheremp = (var%rspheremp-kgenref_var%rspheremp)**2 
              buf2_rspheremp = buf1_rspheremp 
          END WHERE   
          nrmsdiff_rspheremp = SQRT(SUM(buf1_rspheremp)/REAL(n_rspheremp)) 
          rmsdiff_rspheremp = SQRT(SUM(buf2_rspheremp)/REAL(n_rspheremp)) 
          IF (nrmsdiff_rspheremp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rspheremp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rspheremp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rspheremp /= kgenref_var%rspheremp), " of ", size( var%rspheremp ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%rspheremp)/real(size(var%rspheremp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rspheremp)/real(size(kgenref_var%rspheremp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rspheremp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rspheremp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rspheremp /= kgenref_var%rspheremp), " of ", size( var%rspheremp ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%rspheremp)/real(size(var%rspheremp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rspheremp)/real(size(kgenref_var%rspheremp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rspheremp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rspheremp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%gdofp == kgenref_var%gdofp)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%gdofp is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_gdofp(SIZE(var%gdofp,dim=1),SIZE(var%gdofp,dim=2))) 
          ALLOCATE (buf2_gdofp(SIZE(var%gdofp,dim=1),SIZE(var%gdofp,dim=2))) 
          n_gdofp = COUNT(var%gdofp /= kgenref_var%gdofp) 
          WHERE ( ABS(kgenref_var%gdofp) > kgen_minvalue ) 
              buf1_gdofp = ((var%gdofp-kgenref_var%gdofp)/kgenref_var%gdofp)**2 
              buf2_gdofp = (var%gdofp-kgenref_var%gdofp)**2 
          ELSEWHERE 
              buf1_gdofp = (var%gdofp-kgenref_var%gdofp)**2 
              buf2_gdofp = buf1_gdofp 
          END WHERE   
          nrmsdiff_gdofp = SQRT(SUM(buf1_gdofp)/REAL(n_gdofp)) 
          rmsdiff_gdofp = SQRT(SUM(buf2_gdofp)/REAL(n_gdofp)) 
          IF (nrmsdiff_gdofp > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%gdofp is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%gdofp is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%gdofp /= kgenref_var%gdofp), " of ", size( var%gdofp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%gdofp)/real(size(var%gdofp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%gdofp)/real(size(kgenref_var%gdofp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_gdofp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_gdofp 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%gdofp /= kgenref_var%gdofp), " of ", size( var%gdofp ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%gdofp)/real(size(var%gdofp)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%gdofp)/real(size(kgenref_var%gdofp)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_gdofp 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_gdofp 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%fcor == kgenref_var%fcor)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%fcor is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_fcor(SIZE(var%fcor,dim=1),SIZE(var%fcor,dim=2))) 
          ALLOCATE (buf2_fcor(SIZE(var%fcor,dim=1),SIZE(var%fcor,dim=2))) 
          n_fcor = COUNT(var%fcor /= kgenref_var%fcor) 
          WHERE ( ABS(kgenref_var%fcor) > kgen_minvalue ) 
              buf1_fcor = ((var%fcor-kgenref_var%fcor)/kgenref_var%fcor)**2 
              buf2_fcor = (var%fcor-kgenref_var%fcor)**2 
          ELSEWHERE 
              buf1_fcor = (var%fcor-kgenref_var%fcor)**2 
              buf2_fcor = buf1_fcor 
          END WHERE   
          nrmsdiff_fcor = SQRT(SUM(buf1_fcor)/REAL(n_fcor)) 
          rmsdiff_fcor = SQRT(SUM(buf2_fcor)/REAL(n_fcor)) 
          IF (nrmsdiff_fcor > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fcor is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fcor is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%fcor /= kgenref_var%fcor), " of ", size( var%fcor ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%fcor)/real(size(var%fcor)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%fcor)/real(size(kgenref_var%fcor)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_fcor 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fcor 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%fcor /= kgenref_var%fcor), " of ", size( var%fcor ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%fcor)/real(size(var%fcor)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%fcor)/real(size(kgenref_var%fcor)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_fcor 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fcor 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      CALL kv_element_mod_index_t("idxp", comp_check_status, var%idxp, kgenref_var%idxp) 
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%idxp", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%idxp is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%idxp is NOT IDENTICAL(within tolerance)." 
          END IF   
          check_result = CHECK_IN_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
              WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
              WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
              WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ASSOCIATED(var%idxv)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
          CALL kv_element_mod_index_t("idxv", comp_check_status, var%idxv, kgenref_var%idxv) 
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%idxv", " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%idxv is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%idxv is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
                  WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                  WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                  WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%facenum == kgenref_var%facenum) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%facenum is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_facenum = ABS(var%facenum - kgenref_var%facenum) 
          IF (diff_facenum <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%facenum is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%facenum is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_facenum 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_facenum 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dummy == kgenref_var%dummy) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dummy is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dummy = ABS(var%dummy - kgenref_var%dummy) 
          IF (diff_dummy <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dummy is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dummy is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dummy 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dummy 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_element_mod_element_t 
    
end module element_mod