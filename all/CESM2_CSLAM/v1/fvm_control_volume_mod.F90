!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  
!MODULE FVM_CONTROL_VOLUME_MOD---------------------------------------------CE-for FVM
! AUTHOR: Christoph Erath, 11.June 2011                                             !
! This module contains everything to initialize the arrival. It also provides the   !
! interpolation points for the reconstruction (projection from one face to another  !
! when the element is on the cube edge)                                             !
! It also intialize the start values, see also fvm_analytic                         !
!-----------------------------------------------------------------------------------! 


module fvm_control_volume_mod
    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
    USE coordinate_systems_mod, ONLY: spherical_polar_t 
    USE dimensions_mod, ONLY: nc, nhe, nlev, ntrac_d, nhr, ns, nhc 
    USE dimensions_mod, ONLY: irecons_tracer 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    USE coordinate_systems_mod, ONLY: kr_kgen_coordinate_systems_mod_typesubp0 
    USE coordinate_systems_mod, ONLY: kv_kgen_coordinate_systems_mod_typesubp0 

    IMPLICIT NONE 
    PRIVATE 
  integer, parameter, private:: nh = nhr+(nhe-1) ! = 2 (nhr=2; nhe=1)
                                                 ! = 3 (nhr=2; nhe=2)

  type, public :: fvm_struct
    ! fvm tracer mixing ratio: (kg/kg)
    real (kind=r8) :: c(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev,ntrac_d,2)
    real (kind=r8) :: se_flux(1-nhe:nc+nhe,1-nhe:nc+nhe,4,nlev) 

    real (kind=r8) :: dp_fvm(1-nhc:nc+nhc,1-nhc:nc+nhc,nlev,2)
    real (kind=r8) :: dp_ref(nlev)
    real (kind=r8) :: dp_ref_inverse(nlev)
    real (kind=r8) :: psc(nc,nc)

    real (kind=r8) :: inv_area_sphere(nc,nc)    ! inverse area_sphere    
    real (kind=r8) :: inv_se_area_sphere(nc,nc) ! inverse area_sphere    

    integer                  :: faceno         !face number
    ! number of south,....,swest and 0 for interior element 
    integer                  :: cubeboundary                                                 


    real (kind=r8) :: displ_max(1-nhc:nc+nhc,1-nhc:nc+nhc,4)
    integer        :: flux_vec (2,1-nhc:nc+nhc,1-nhc:nc+nhc,4) 
    ! cartesian location of vertices for flux sides
    !  x-coordinate of vertex 1: vtx_cart(1,1i,j,1,1)  = fvm%acartx(i)
    !  y-coordinate of vertex 1: vtx_cart(1,2,i,j,2,1) = fvm%acarty(j)
    !  x-coordinate of vertex 2: vtx_cart(2,1,i,j) = fvm%acartx(i+1)
    !  y-coordinate of vertex 2: vtx_cart(2,2,i,j) = fvm%acarty(j  )
    !  x-coordinate of vertex 3: vtx_cart(3,1,i,j) = fvm%acartx(i+1)
    !  y-coordinate of vertex 3: vtx_cart(3,2,i,j) = fvm%acarty(j+1)
    !  x-coordinate of vertex 4: vtx_cart(4,1,i,j) = fvm%acartx(i  )
    !  y-coordinate of vertex 4: vtx_cart(4,2,i,j) = fvm%acarty(j+1)
    !
    !
    !
    !
    !
    !
    !
    real (kind=r8) :: vtx_cart (4,2,1-nhc:nc+nhc,1-nhc:nc+nhc)
    ! flux_orient(1,i,j) = panel on which control volume (i,j) is located
    ! flux_orient(2,i,j) = cshift value for vertex permutation
    !
    !
    real (kind=r8) :: flux_orient(2  ,1-nhc:nc+nhc,1-nhc:nc+nhc) 
    ! i,j: indicator function for non-existent cells (0 for corner halo and 1 elsewhere)
    !
    !
    integer                  :: ifct   (1-nhc:nc+nhc,1-nhc:nc+nhc) 
    integer                  :: rot_matrix(2,2,1-nhc:nc+nhc,1-nhc:nc+nhc)
    !    
    real (kind=r8)           :: dalpha, dbeta             ! central-angle for gnomonic coordinates
    type (spherical_polar_t) :: center_cart(nc,nc)        ! center of fvm cell in gnomonic coordinates
    real (kind=r8)           :: area_sphere(nc,nc)        ! spherical area of fvm cell
    real (kind=r8)           :: spherecentroid(irecons_tracer-1,1-nhc:nc+nhc,1-nhc:nc+nhc) ! centroids
    ! pre-computed metric terms (for efficiency)
    ! recons_metrics(1,:,:) = spherecentroid(1,:,:)**2 -spherecentroid(3,:,:)
    ! recons_metrics(2,:,:) = spherecentroid(2,:,:)**2 -spherecentroid(4,:,:)
    ! recons_metrics(3,:,:) = spherecentroid(1,:,:)*spherecentroid(2,:,:)-spherecentroid(5,:,:)
    !
    !
    !
    real (kind=r8)           :: recons_metrics(3,1-nhe:nc+nhe,1-nhe:nc+nhe)    
    ! recons_metrics_integral(1,:,:) = 2.0_r8*spherecentroid(1,:,:)**2 -spherecentroid(3,:,:)
    ! recons_metrics_integral(2,:,:) = 2.0_r8*spherecentroid(2,:,:)**2 -spherecentroid(4,:,:)
    ! recons_metrics_integral(3,:,:) = 2.0_r8*spherecentroid(1,:,:)*spherecentroid(2,:,:)-spherecentroid(5,:,:)
    !
    !
    real (kind=r8)           :: recons_metrics_integral(3,1-nhe:nc+nhe,1-nhe:nc+nhe)    
    !
    integer                  :: jx_min(3), jx_max(3), jy_min(3), jy_max(3) !bounds for computation
    ! provide fixed interpolation points with respect to the arrival grid for 
    ! reconstruction   

    integer                  :: ibase(1-nh:nc+nh,1:nhr,2)  
    real (kind=r8)           :: halo_interp_weight(1:ns,1-nh:nc+nh,1:nhr,2)
    real (kind=r8)           :: centroid_stretch(7,1-nhe:nc+nhe,1-nhe:nc+nhe) !for finite-difference reconstruction
    ! pre-compute weights for reconstruction at cell vertices
    !  ! Evaluate constant order terms
    !  value = fcube(a,b) + &
    !  ! Evaluate linear order terms
    !          recons(1,a,b) * (cartx - centroid(1,a,b)) + &
    !          recons(2,a,b) * (carty - centroid(2,a,b)) + &
    !  ! Evaluate second order terms
    !          recons(3,a,b) * (centroid(1,a,b)**2 - centroid(3,a,b)) + &
    !          recons(4,a,b) * (centroid(2,a,b)**2 - centroid(4,a,b)) + &
    !          recons(5,a,b) * (centroid(1,a,b) * centroid(2,a,b) - centroid(5,a,b)) + &
    !          recons(3,a,b) * (cartx - centroid(1,a,b))**2 + &
    !          recons(4,a,b) * (carty - centroid(2,a,b))**2 + &
    !          recons(5,a,b) * (cartx - centroid(1,a,b)) * (carty - centroid(2,a,b))
    !
    !
    !
    !   
    real (kind=r8)    :: vertex_recons_weights(1:irecons_tracer-1,4,1-nhe:nc+nhe,1-nhe:nc+nhe)
    ! for mapping fvm2dyn
    !
    !
    real (kind=r8)    :: norm_elem_coord(2,1-nhc:nc+nhc,1-nhc:nc+nhc)
    !******************************************
    ! separate physics grid variables
    !******************************************
    !
    !
    !
    !
    real (kind=r8)           , allocatable :: phis_physgrid(:,:)
    real (kind=r8)           , allocatable :: vtx_cart_physgrid(:,:,:,:)
    real (kind=r8)           , allocatable :: flux_orient_physgrid(:,:,:)
    integer                  , allocatable :: ifct_physgrid(:,:)
    integer                  , allocatable :: rot_matrix_physgrid(:,:,:,:)
    real (kind=r8)           , allocatable :: spherecentroid_physgrid(:,:,:)
    real (kind=r8)           , allocatable :: recons_metrics_physgrid(:,:,:)
    real (kind=r8)           , allocatable :: recons_metrics_integral_physgrid(:,:,:)
    ! centroid_stretch_physgrid for finite-difference reconstruction
    real (kind=r8)           , allocatable :: centroid_stretch_physgrid       (:,:,:)
    real (kind=r8)                         :: dalpha_physgrid, dbeta_physgrid             ! central-angle for gnomonic coordinates
    type (spherical_polar_t) , allocatable :: center_cart_physgrid(:,:)        ! center of fvm cell in gnomonic coordinates
    real (kind=r8)           , allocatable :: area_sphere_physgrid(:,:)        ! spherical area of fvm cell
    integer                                :: jx_min_physgrid(3), jx_max_physgrid(3) !bounds for computation
    integer                                :: jy_min_physgrid(3), jy_max_physgrid(3) !bounds for computation
    integer                  , allocatable :: ibase_physgrid(:,:,:)
    real (kind=r8)           , allocatable :: halo_interp_weight_physgrid(:,:,:,:)
    real (kind=r8)           , allocatable :: vertex_recons_weights_physgrid(:,:,:,:)

    real (kind=r8)           , allocatable :: norm_elem_coord_physgrid(:,:,:)
    real (kind=r8)           , allocatable :: Dinv_physgrid(:,:,:,:)

    real (kind=r8)           , allocatable :: fc(:,:,:,:)
    real (kind=r8)           , allocatable :: fc_phys(:,:,:,:)
    real (kind=r8)           , allocatable :: ft(:,:,:)
    real (kind=r8)           , allocatable :: fm(:,:,:,:)
    real (kind=r8)           , allocatable :: dp_phys(:,:,:)
  end type fvm_struct


  

  integer, public            :: n0_fvm, np1_fvm !fvm time-levels
  PUBLIC kr_externs_in_fvm_control_volume_mod 
  PUBLIC kr_externs_out_fvm_control_volume_mod 
  PUBLIC kv_externs_fvm_control_volume_mod 
  PUBLIC kr_kgen_coordinate_systems_mod_typesubp0 
  PUBLIC kr_fvm_control_volume_mod_fvm_struct 
  INTEGER :: kgenref_n0_fvm 
  PUBLIC kv_kgen_coordinate_systems_mod_typesubp0 
  PUBLIC kv_fvm_control_volume_mod_fvm_struct 

    
  CONTAINS 
    


  !read state subroutine for kr_externs_in_fvm_control_volume_mod 
  SUBROUTINE kr_externs_in_fvm_control_volume_mod(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) np1_fvm 
      READ (UNIT = kgen_unit) n0_fvm 
  END SUBROUTINE kr_externs_in_fvm_control_volume_mod 
    
  !read state subroutine for kr_externs_out_fvm_control_volume_mod 
  SUBROUTINE kr_externs_out_fvm_control_volume_mod(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      READ (UNIT = kgen_unit) kgenref_n0_fvm 
  END SUBROUTINE kr_externs_out_fvm_control_volume_mod 
    
  !verify state subroutine for kv_externs_fvm_control_volume_mod 
  SUBROUTINE kv_externs_fvm_control_volume_mod(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_fvm_control_volume_mod_integer__("n0_fvm", check_status, n0_fvm, kgenref_n0_fvm) 
  END SUBROUTINE kv_externs_fvm_control_volume_mod 
    
  !read state subroutine for kr_fvm_control_volume_mod_fvm_struct 
  RECURSIVE SUBROUTINE kr_fvm_control_volume_mod_fvm_struct(var, kgen_unit, printname, printvar) 
      TYPE(fvm_struct), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%c 
          CALL kgen_array_sumcheck(printname // "%c", kgen_array_sum, DBLE(SUM(var%c, mask=(var%c .eq. var%c))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%c)) = ", DBLE(SUM(var%c, mask=(var%c .eq. var%c))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%se_flux 
          CALL kgen_array_sumcheck(printname // "%se_flux", kgen_array_sum, DBLE(SUM(var%se_flux, mask=(var%se_flux .eq. &
          &var%se_flux))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%se_flux)) = ", DBLE(SUM(var%se_flux, mask=(var%se_flux .eq. &
              &var%se_flux))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dp_fvm 
          CALL kgen_array_sumcheck(printname // "%dp_fvm", kgen_array_sum, DBLE(SUM(var%dp_fvm, mask=(var%dp_fvm .eq. &
          &var%dp_fvm))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dp_fvm)) = ", DBLE(SUM(var%dp_fvm, mask=(var%dp_fvm .eq. &
              &var%dp_fvm))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dp_ref 
          CALL kgen_array_sumcheck(printname // "%dp_ref", kgen_array_sum, DBLE(SUM(var%dp_ref, mask=(var%dp_ref .eq. &
          &var%dp_ref))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dp_ref)) = ", DBLE(SUM(var%dp_ref, mask=(var%dp_ref .eq. &
              &var%dp_ref))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%dp_ref_inverse 
          CALL kgen_array_sumcheck(printname // "%dp_ref_inverse", kgen_array_sum, DBLE(SUM(var%dp_ref_inverse, &
          &mask=(var%dp_ref_inverse .eq. var%dp_ref_inverse))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%dp_ref_inverse)) = ", DBLE(SUM(var%dp_ref_inverse, &
              &mask=(var%dp_ref_inverse .eq. var%dp_ref_inverse))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%psc 
          CALL kgen_array_sumcheck(printname // "%psc", kgen_array_sum, DBLE(SUM(var%psc, mask=(var%psc .eq. var%psc))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%psc)) = ", DBLE(SUM(var%psc, mask=(var%psc .eq. var%psc))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%inv_area_sphere 
          CALL kgen_array_sumcheck(printname // "%inv_area_sphere", kgen_array_sum, DBLE(SUM(var%inv_area_sphere, &
          &mask=(var%inv_area_sphere .eq. var%inv_area_sphere))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%inv_area_sphere)) = ", DBLE(SUM(var%inv_area_sphere, &
              &mask=(var%inv_area_sphere .eq. var%inv_area_sphere))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%inv_se_area_sphere 
          CALL kgen_array_sumcheck(printname // "%inv_se_area_sphere", kgen_array_sum, DBLE(SUM(var%inv_se_area_sphere, &
          &mask=(var%inv_se_area_sphere .eq. var%inv_se_area_sphere))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%inv_se_area_sphere)) = ", DBLE(SUM(var%inv_se_area_sphere, &
              &mask=(var%inv_se_area_sphere .eq. var%inv_se_area_sphere))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%faceno 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%faceno = ", var%faceno 
      END IF   
        
      READ (UNIT = kgen_unit) var%cubeboundary 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cubeboundary = ", var%cubeboundary 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%displ_max 
          CALL kgen_array_sumcheck(printname // "%displ_max", kgen_array_sum, DBLE(SUM(var%displ_max, mask=(var%displ_max .eq. &
          &var%displ_max))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%displ_max)) = ", DBLE(SUM(var%displ_max, mask=(var%displ_max &
              &.eq. var%displ_max))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%flux_vec 
          CALL kgen_array_sumcheck(printname // "%flux_vec", kgen_array_sum, DBLE(SUM(var%flux_vec, mask=(var%flux_vec .eq. &
          &var%flux_vec))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%flux_vec)) = ", DBLE(SUM(var%flux_vec, mask=(var%flux_vec &
              &.eq. var%flux_vec))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%vtx_cart 
          CALL kgen_array_sumcheck(printname // "%vtx_cart", kgen_array_sum, DBLE(SUM(var%vtx_cart, mask=(var%vtx_cart .eq. &
          &var%vtx_cart))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%vtx_cart)) = ", DBLE(SUM(var%vtx_cart, mask=(var%vtx_cart &
              &.eq. var%vtx_cart))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%flux_orient 
          CALL kgen_array_sumcheck(printname // "%flux_orient", kgen_array_sum, DBLE(SUM(var%flux_orient, mask=(var%flux_orient &
          &.eq. var%flux_orient))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%flux_orient)) = ", DBLE(SUM(var%flux_orient, &
              &mask=(var%flux_orient .eq. var%flux_orient))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ifct 
          CALL kgen_array_sumcheck(printname // "%ifct", kgen_array_sum, DBLE(SUM(var%ifct, mask=(var%ifct .eq. var%ifct))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ifct)) = ", DBLE(SUM(var%ifct, mask=(var%ifct .eq. &
              &var%ifct))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%rot_matrix 
          CALL kgen_array_sumcheck(printname // "%rot_matrix", kgen_array_sum, DBLE(SUM(var%rot_matrix, mask=(var%rot_matrix .eq. &
          &var%rot_matrix))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%rot_matrix)) = ", DBLE(SUM(var%rot_matrix, &
              &mask=(var%rot_matrix .eq. var%rot_matrix))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) var%dalpha 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dalpha = ", var%dalpha 
      END IF   
      READ (UNIT = kgen_unit) var%dbeta 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dbeta = ", var%dbeta 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_fvm_struct_subp2(var%center_cart, kgen_unit, printname // "%center_cart", .TRUE.) 
      ELSE 
          CALL kr_kgen_fvm_struct_subp2(var%center_cart, kgen_unit, printname // "%center_cart", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%area_sphere 
          CALL kgen_array_sumcheck(printname // "%area_sphere", kgen_array_sum, DBLE(SUM(var%area_sphere, mask=(var%area_sphere &
          &.eq. var%area_sphere))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%area_sphere)) = ", DBLE(SUM(var%area_sphere, &
              &mask=(var%area_sphere .eq. var%area_sphere))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%spherecentroid 
          CALL kgen_array_sumcheck(printname // "%spherecentroid", kgen_array_sum, DBLE(SUM(var%spherecentroid, &
          &mask=(var%spherecentroid .eq. var%spherecentroid))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%spherecentroid)) = ", DBLE(SUM(var%spherecentroid, &
              &mask=(var%spherecentroid .eq. var%spherecentroid))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%recons_metrics 
          CALL kgen_array_sumcheck(printname // "%recons_metrics", kgen_array_sum, DBLE(SUM(var%recons_metrics, &
          &mask=(var%recons_metrics .eq. var%recons_metrics))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%recons_metrics)) = ", DBLE(SUM(var%recons_metrics, &
              &mask=(var%recons_metrics .eq. var%recons_metrics))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%recons_metrics_integral 
          CALL kgen_array_sumcheck(printname // "%recons_metrics_integral", kgen_array_sum, DBLE(SUM(var%recons_metrics_integral, &
          &mask=(var%recons_metrics_integral .eq. var%recons_metrics_integral))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%recons_metrics_integral)) = ", &
              &DBLE(SUM(var%recons_metrics_integral, mask=(var%recons_metrics_integral .eq. var%recons_metrics_integral))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jx_min 
          CALL kgen_array_sumcheck(printname // "%jx_min", kgen_array_sum, DBLE(SUM(var%jx_min, mask=(var%jx_min .eq. &
          &var%jx_min))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jx_min)) = ", DBLE(SUM(var%jx_min, mask=(var%jx_min .eq. &
              &var%jx_min))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jx_max 
          CALL kgen_array_sumcheck(printname // "%jx_max", kgen_array_sum, DBLE(SUM(var%jx_max, mask=(var%jx_max .eq. &
          &var%jx_max))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jx_max)) = ", DBLE(SUM(var%jx_max, mask=(var%jx_max .eq. &
              &var%jx_max))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jy_min 
          CALL kgen_array_sumcheck(printname // "%jy_min", kgen_array_sum, DBLE(SUM(var%jy_min, mask=(var%jy_min .eq. &
          &var%jy_min))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jy_min)) = ", DBLE(SUM(var%jy_min, mask=(var%jy_min .eq. &
              &var%jy_min))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jy_max 
          CALL kgen_array_sumcheck(printname // "%jy_max", kgen_array_sum, DBLE(SUM(var%jy_max, mask=(var%jy_max .eq. &
          &var%jy_max))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jy_max)) = ", DBLE(SUM(var%jy_max, mask=(var%jy_max .eq. &
              &var%jy_max))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%ibase 
          CALL kgen_array_sumcheck(printname // "%ibase", kgen_array_sum, DBLE(SUM(var%ibase, mask=(var%ibase .eq. var%ibase))), &
          &.TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%ibase)) = ", DBLE(SUM(var%ibase, mask=(var%ibase .eq. &
              &var%ibase))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%halo_interp_weight 
          CALL kgen_array_sumcheck(printname // "%halo_interp_weight", kgen_array_sum, DBLE(SUM(var%halo_interp_weight, &
          &mask=(var%halo_interp_weight .eq. var%halo_interp_weight))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%halo_interp_weight)) = ", DBLE(SUM(var%halo_interp_weight, &
              &mask=(var%halo_interp_weight .eq. var%halo_interp_weight))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%centroid_stretch 
          CALL kgen_array_sumcheck(printname // "%centroid_stretch", kgen_array_sum, DBLE(SUM(var%centroid_stretch, &
          &mask=(var%centroid_stretch .eq. var%centroid_stretch))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%centroid_stretch)) = ", DBLE(SUM(var%centroid_stretch, &
              &mask=(var%centroid_stretch .eq. var%centroid_stretch))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%vertex_recons_weights 
          CALL kgen_array_sumcheck(printname // "%vertex_recons_weights", kgen_array_sum, DBLE(SUM(var%vertex_recons_weights, &
          &mask=(var%vertex_recons_weights .eq. var%vertex_recons_weights))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%vertex_recons_weights)) = ", &
              &DBLE(SUM(var%vertex_recons_weights, mask=(var%vertex_recons_weights .eq. var%vertex_recons_weights))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%norm_elem_coord 
          CALL kgen_array_sumcheck(printname // "%norm_elem_coord", kgen_array_sum, DBLE(SUM(var%norm_elem_coord, &
          &mask=(var%norm_elem_coord .eq. var%norm_elem_coord))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%norm_elem_coord)) = ", DBLE(SUM(var%norm_elem_coord, &
              &mask=(var%norm_elem_coord .eq. var%norm_elem_coord))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim2(var%phis_physgrid, kgen_unit, printname // "%phis_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim2(var%phis_physgrid, kgen_unit, printname // "%phis_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%vtx_cart_physgrid, kgen_unit, printname // "%vtx_cart_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%vtx_cart_physgrid, kgen_unit, printname // "%vtx_cart_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%flux_orient_physgrid, kgen_unit, printname // "%flux_orient_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%flux_orient_physgrid, kgen_unit, printname // "%flux_orient_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_integer___dim2(var%ifct_physgrid, kgen_unit, printname // "%ifct_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_integer___dim2(var%ifct_physgrid, kgen_unit, printname // "%ifct_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_integer___dim4(var%rot_matrix_physgrid, kgen_unit, printname // "%rot_matrix_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_integer___dim4(var%rot_matrix_physgrid, kgen_unit, printname // "%rot_matrix_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%spherecentroid_physgrid, kgen_unit, printname // "%spherecentroid_physgrid", &
          &.TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%spherecentroid_physgrid, kgen_unit, printname // "%spherecentroid_physgrid", &
          &.FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%recons_metrics_physgrid, kgen_unit, printname // "%recons_metrics_physgrid", &
          &.TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%recons_metrics_physgrid, kgen_unit, printname // "%recons_metrics_physgrid", &
          &.FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%recons_metrics_integral_physgrid, kgen_unit, printname // &
          &"%recons_metrics_integral_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%recons_metrics_integral_physgrid, kgen_unit, printname // &
          &"%recons_metrics_integral_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%centroid_stretch_physgrid, kgen_unit, printname // "%centroid_stretch_physgrid", &
          &.TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%centroid_stretch_physgrid, kgen_unit, printname // "%centroid_stretch_physgrid", &
          &.FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%dalpha_physgrid 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dalpha_physgrid = ", var%dalpha_physgrid 
      END IF   
      READ (UNIT = kgen_unit) var%dbeta_physgrid 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%dbeta_physgrid = ", var%dbeta_physgrid 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_fvm_struct_subp2_alloc(var%center_cart_physgrid, kgen_unit, printname // "%center_cart_physgrid", .TRUE.) 
      ELSE 
          CALL kr_kgen_fvm_struct_subp2_alloc(var%center_cart_physgrid, kgen_unit, printname // "%center_cart_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim2(var%area_sphere_physgrid, kgen_unit, printname // "%area_sphere_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim2(var%area_sphere_physgrid, kgen_unit, printname // "%area_sphere_physgrid", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jx_min_physgrid 
          CALL kgen_array_sumcheck(printname // "%jx_min_physgrid", kgen_array_sum, DBLE(SUM(var%jx_min_physgrid, &
          &mask=(var%jx_min_physgrid .eq. var%jx_min_physgrid))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jx_min_physgrid)) = ", DBLE(SUM(var%jx_min_physgrid, &
              &mask=(var%jx_min_physgrid .eq. var%jx_min_physgrid))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jx_max_physgrid 
          CALL kgen_array_sumcheck(printname // "%jx_max_physgrid", kgen_array_sum, DBLE(SUM(var%jx_max_physgrid, &
          &mask=(var%jx_max_physgrid .eq. var%jx_max_physgrid))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jx_max_physgrid)) = ", DBLE(SUM(var%jx_max_physgrid, &
              &mask=(var%jx_max_physgrid .eq. var%jx_max_physgrid))) 
          END IF   
      END IF   
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jy_min_physgrid 
          CALL kgen_array_sumcheck(printname // "%jy_min_physgrid", kgen_array_sum, DBLE(SUM(var%jy_min_physgrid, &
          &mask=(var%jy_min_physgrid .eq. var%jy_min_physgrid))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jy_min_physgrid)) = ", DBLE(SUM(var%jy_min_physgrid, &
              &mask=(var%jy_min_physgrid .eq. var%jy_min_physgrid))) 
          END IF   
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) var%jy_max_physgrid 
          CALL kgen_array_sumcheck(printname // "%jy_max_physgrid", kgen_array_sum, DBLE(SUM(var%jy_max_physgrid, &
          &mask=(var%jy_max_physgrid .eq. var%jy_max_physgrid))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%jy_max_physgrid)) = ", DBLE(SUM(var%jy_max_physgrid, &
              &mask=(var%jy_max_physgrid .eq. var%jy_max_physgrid))) 
          END IF   
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_integer___dim3(var%ibase_physgrid, kgen_unit, printname // "%ibase_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_integer___dim3(var%ibase_physgrid, kgen_unit, printname // "%ibase_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%halo_interp_weight_physgrid, kgen_unit, printname // &
          &"%halo_interp_weight_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%halo_interp_weight_physgrid, kgen_unit, printname // &
          &"%halo_interp_weight_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%vertex_recons_weights_physgrid, kgen_unit, printname // &
          &"%vertex_recons_weights_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%vertex_recons_weights_physgrid, kgen_unit, printname // &
          &"%vertex_recons_weights_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%norm_elem_coord_physgrid, kgen_unit, printname // "%norm_elem_coord_physgrid", &
          &.TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%norm_elem_coord_physgrid, kgen_unit, printname // "%norm_elem_coord_physgrid", &
          &.FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%dinv_physgrid, kgen_unit, printname // "%dinv_physgrid", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%dinv_physgrid, kgen_unit, printname // "%dinv_physgrid", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%fc, kgen_unit, printname // "%fc", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%fc, kgen_unit, printname // "%fc", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%fc_phys, kgen_unit, printname // "%fc_phys", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%fc_phys, kgen_unit, printname // "%fc_phys", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%ft, kgen_unit, printname // "%ft", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%ft, kgen_unit, printname // "%ft", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim4(var%fm, kgen_unit, printname // "%fm", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim4(var%fm, kgen_unit, printname // "%fm", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_fvm_struct_real__r8_dim3(var%dp_phys, kgen_unit, printname // "%dp_phys", .TRUE.) 
      ELSE 
          CALL kr_fvm_struct_real__r8_dim3(var%dp_phys, kgen_unit, printname // "%dp_phys", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_fvm_control_volume_mod_fvm_struct 
     
  !write state subroutine for kr_kgen_fvm_struct_subp2 
  SUBROUTINE kr_kgen_fvm_struct_subp2(var, kgen_unit, printname, printvar) 
      TYPE(spherical_polar_t), INTENT(INOUT), DIMENSION(:,:) :: var  !!! YSK
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
        
  END SUBROUTINE kr_kgen_fvm_struct_subp2 
    

  !write state subroutine for kr_kgen_fvm_struct_subp2 
  SUBROUTINE kr_kgen_fvm_struct_subp2_alloc(var, kgen_unit, printname, printvar) 
      !TYPE(spherical_polar_t), INTENT(INOUT), DIMENSION(:,:) :: var  !!! YSK
      TYPE(spherical_polar_t), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
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

          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 

          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 

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
        
  END SUBROUTINE kr_kgen_fvm_struct_subp2_alloc
    
  !write state subroutine for kr_fvm_struct_real__r8_dim2 
  SUBROUTINE kr_fvm_struct_real__r8_dim2(var, kgen_unit, printname, printvar) 
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
        
  END SUBROUTINE kr_fvm_struct_real__r8_dim2 
    
  !write state subroutine for kr_fvm_struct_real__r8_dim4 
  SUBROUTINE kr_fvm_struct_real__r8_dim4(var, kgen_unit, printname, printvar) 
      REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3, idx4 
      INTEGER, DIMENSION(2,4) :: kgen_bound 
        
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
          READ (UNIT = kgen_unit) kgen_bound(1, 4) 
          READ (UNIT = kgen_unit) kgen_bound(2, 4) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3), &
          &kgen_bound(1,4):kgen_bound(2,4))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_fvm_struct_real__r8_dim4 
    
  !write state subroutine for kr_fvm_struct_real__r8_dim3 
  SUBROUTINE kr_fvm_struct_real__r8_dim3(var, kgen_unit, printname, printvar) 
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
        
  END SUBROUTINE kr_fvm_struct_real__r8_dim3 
    
  !write state subroutine for kr_fvm_struct_integer___dim2 
  SUBROUTINE kr_fvm_struct_integer___dim2(var, kgen_unit, printname, printvar) 
      INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
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
        
  END SUBROUTINE kr_fvm_struct_integer___dim2 
    
  !write state subroutine for kr_fvm_struct_integer___dim4 
  SUBROUTINE kr_fvm_struct_integer___dim4(var, kgen_unit, printname, printvar) 
      INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2, idx3, idx4 
      INTEGER, DIMENSION(2,4) :: kgen_bound 
        
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
          READ (UNIT = kgen_unit) kgen_bound(1, 4) 
          READ (UNIT = kgen_unit) kgen_bound(2, 4) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3), &
          &kgen_bound(1,4):kgen_bound(2,4))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_fvm_struct_integer___dim4 
    
  !write state subroutine for kr_fvm_struct_integer___dim3 
  SUBROUTINE kr_fvm_struct_integer___dim3(var, kgen_unit, printname, printvar) 
      INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:,:) :: var 
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
        
  END SUBROUTINE kr_fvm_struct_integer___dim3 
    
  !verify state subroutine for kv_fvm_control_volume_mod_fvm_struct 
  RECURSIVE SUBROUTINE kv_fvm_control_volume_mod_fvm_struct(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(fvm_struct), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_c 
      real(KIND=r8) :: nrmsdiff_c, rmsdiff_c 
      real(KIND=r8), ALLOCATABLE :: buf1_c(:,:,:,:,:), buf2_c(:,:,:,:,:) 
      INTEGER :: n_se_flux 
      real(KIND=r8) :: nrmsdiff_se_flux, rmsdiff_se_flux 
      real(KIND=r8), ALLOCATABLE :: buf1_se_flux(:,:,:,:), buf2_se_flux(:,:,:,:) 
      INTEGER :: n_dp_fvm 
      real(KIND=r8) :: nrmsdiff_dp_fvm, rmsdiff_dp_fvm 
      real(KIND=r8), ALLOCATABLE :: buf1_dp_fvm(:,:,:,:), buf2_dp_fvm(:,:,:,:) 
      INTEGER :: n_dp_ref 
      real(KIND=r8) :: nrmsdiff_dp_ref, rmsdiff_dp_ref 
      real(KIND=r8), ALLOCATABLE :: buf1_dp_ref(:), buf2_dp_ref(:) 
      INTEGER :: n_dp_ref_inverse 
      real(KIND=r8) :: nrmsdiff_dp_ref_inverse, rmsdiff_dp_ref_inverse 
      real(KIND=r8), ALLOCATABLE :: buf1_dp_ref_inverse(:), buf2_dp_ref_inverse(:) 
      INTEGER :: n_psc 
      real(KIND=r8) :: nrmsdiff_psc, rmsdiff_psc 
      real(KIND=r8), ALLOCATABLE :: buf1_psc(:,:), buf2_psc(:,:) 
      INTEGER :: n_inv_area_sphere 
      real(KIND=r8) :: nrmsdiff_inv_area_sphere, rmsdiff_inv_area_sphere 
      real(KIND=r8), ALLOCATABLE :: buf1_inv_area_sphere(:,:), buf2_inv_area_sphere(:,:) 
      INTEGER :: n_inv_se_area_sphere 
      real(KIND=r8) :: nrmsdiff_inv_se_area_sphere, rmsdiff_inv_se_area_sphere 
      real(KIND=r8), ALLOCATABLE :: buf1_inv_se_area_sphere(:,:), buf2_inv_se_area_sphere(:,:) 
      integer :: diff_faceno 
      integer :: diff_cubeboundary 
      INTEGER :: n_displ_max 
      real(KIND=r8) :: nrmsdiff_displ_max, rmsdiff_displ_max 
      real(KIND=r8), ALLOCATABLE :: buf1_displ_max(:,:,:), buf2_displ_max(:,:,:) 
      INTEGER :: n_flux_vec 
      integer :: nrmsdiff_flux_vec, rmsdiff_flux_vec 
      integer, ALLOCATABLE :: buf1_flux_vec(:,:,:,:), buf2_flux_vec(:,:,:,:) 
      INTEGER :: n_vtx_cart 
      real(KIND=r8) :: nrmsdiff_vtx_cart, rmsdiff_vtx_cart 
      real(KIND=r8), ALLOCATABLE :: buf1_vtx_cart(:,:,:,:), buf2_vtx_cart(:,:,:,:) 
      INTEGER :: n_flux_orient 
      real(KIND=r8) :: nrmsdiff_flux_orient, rmsdiff_flux_orient 
      real(KIND=r8), ALLOCATABLE :: buf1_flux_orient(:,:,:), buf2_flux_orient(:,:,:) 
      INTEGER :: n_ifct 
      integer :: nrmsdiff_ifct, rmsdiff_ifct 
      integer, ALLOCATABLE :: buf1_ifct(:,:), buf2_ifct(:,:) 
      INTEGER :: n_rot_matrix 
      integer :: nrmsdiff_rot_matrix, rmsdiff_rot_matrix 
      integer, ALLOCATABLE :: buf1_rot_matrix(:,:,:,:), buf2_rot_matrix(:,:,:,:) 
      real(KIND=r8) :: diff_dalpha 
      real(KIND=r8) :: diff_dbeta 
      INTEGER :: idx1_center_cart, idx2_center_cart 
      INTEGER :: n_area_sphere 
      real(KIND=r8) :: nrmsdiff_area_sphere, rmsdiff_area_sphere 
      real(KIND=r8), ALLOCATABLE :: buf1_area_sphere(:,:), buf2_area_sphere(:,:) 
      INTEGER :: n_spherecentroid 
      real(KIND=r8) :: nrmsdiff_spherecentroid, rmsdiff_spherecentroid 
      real(KIND=r8), ALLOCATABLE :: buf1_spherecentroid(:,:,:), buf2_spherecentroid(:,:,:) 
      INTEGER :: n_recons_metrics 
      real(KIND=r8) :: nrmsdiff_recons_metrics, rmsdiff_recons_metrics 
      real(KIND=r8), ALLOCATABLE :: buf1_recons_metrics(:,:,:), buf2_recons_metrics(:,:,:) 
      INTEGER :: n_recons_metrics_integral 
      real(KIND=r8) :: nrmsdiff_recons_metrics_integral, rmsdiff_recons_metrics_integral 
      real(KIND=r8), ALLOCATABLE :: buf1_recons_metrics_integral(:,:,:), buf2_recons_metrics_integral(:,:,:) 
      INTEGER :: n_jx_min 
      integer :: nrmsdiff_jx_min, rmsdiff_jx_min 
      integer, ALLOCATABLE :: buf1_jx_min(:), buf2_jx_min(:) 
      INTEGER :: n_jx_max 
      integer :: nrmsdiff_jx_max, rmsdiff_jx_max 
      integer, ALLOCATABLE :: buf1_jx_max(:), buf2_jx_max(:) 
      INTEGER :: n_jy_min 
      integer :: nrmsdiff_jy_min, rmsdiff_jy_min 
      integer, ALLOCATABLE :: buf1_jy_min(:), buf2_jy_min(:) 
      INTEGER :: n_jy_max 
      integer :: nrmsdiff_jy_max, rmsdiff_jy_max 
      integer, ALLOCATABLE :: buf1_jy_max(:), buf2_jy_max(:) 
      INTEGER :: n_ibase 
      integer :: nrmsdiff_ibase, rmsdiff_ibase 
      integer, ALLOCATABLE :: buf1_ibase(:,:,:), buf2_ibase(:,:,:) 
      INTEGER :: n_halo_interp_weight 
      real(KIND=r8) :: nrmsdiff_halo_interp_weight, rmsdiff_halo_interp_weight 
      real(KIND=r8), ALLOCATABLE :: buf1_halo_interp_weight(:,:,:,:), buf2_halo_interp_weight(:,:,:,:) 
      INTEGER :: n_centroid_stretch 
      real(KIND=r8) :: nrmsdiff_centroid_stretch, rmsdiff_centroid_stretch 
      real(KIND=r8), ALLOCATABLE :: buf1_centroid_stretch(:,:,:), buf2_centroid_stretch(:,:,:) 
      INTEGER :: n_vertex_recons_weights 
      real(KIND=r8) :: nrmsdiff_vertex_recons_weights, rmsdiff_vertex_recons_weights 
      real(KIND=r8), ALLOCATABLE :: buf1_vertex_recons_weights(:,:,:,:), buf2_vertex_recons_weights(:,:,:,:) 
      INTEGER :: n_norm_elem_coord 
      real(KIND=r8) :: nrmsdiff_norm_elem_coord, rmsdiff_norm_elem_coord 
      real(KIND=r8), ALLOCATABLE :: buf1_norm_elem_coord(:,:,:), buf2_norm_elem_coord(:,:,:) 
      INTEGER :: n_phis_physgrid 
      real(KIND=r8) :: nrmsdiff_phis_physgrid, rmsdiff_phis_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_phis_physgrid(:,:), buf2_phis_physgrid(:,:) 
      INTEGER :: n_vtx_cart_physgrid 
      real(KIND=r8) :: nrmsdiff_vtx_cart_physgrid, rmsdiff_vtx_cart_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_vtx_cart_physgrid(:,:,:,:), buf2_vtx_cart_physgrid(:,:,:,:) 
      INTEGER :: n_flux_orient_physgrid 
      real(KIND=r8) :: nrmsdiff_flux_orient_physgrid, rmsdiff_flux_orient_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_flux_orient_physgrid(:,:,:), buf2_flux_orient_physgrid(:,:,:) 
      INTEGER :: n_ifct_physgrid 
      integer :: nrmsdiff_ifct_physgrid, rmsdiff_ifct_physgrid 
      integer, ALLOCATABLE :: buf1_ifct_physgrid(:,:), buf2_ifct_physgrid(:,:) 
      INTEGER :: n_rot_matrix_physgrid 
      integer :: nrmsdiff_rot_matrix_physgrid, rmsdiff_rot_matrix_physgrid 
      integer, ALLOCATABLE :: buf1_rot_matrix_physgrid(:,:,:,:), buf2_rot_matrix_physgrid(:,:,:,:) 
      INTEGER :: n_spherecentroid_physgrid 
      real(KIND=r8) :: nrmsdiff_spherecentroid_physgrid, rmsdiff_spherecentroid_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_spherecentroid_physgrid(:,:,:), buf2_spherecentroid_physgrid(:,:,:) 
      INTEGER :: n_recons_metrics_physgrid 
      real(KIND=r8) :: nrmsdiff_recons_metrics_physgrid, rmsdiff_recons_metrics_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_recons_metrics_physgrid(:,:,:), buf2_recons_metrics_physgrid(:,:,:) 
      INTEGER :: n_recons_metrics_integral_physgrid 
      real(KIND=r8) :: nrmsdiff_recons_metrics_integral_physgrid, rmsdiff_recons_metrics_integral_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_recons_metrics_integral_physgrid(:,:,:), buf2_recons_metrics_integral_physgrid(:,:,:) 
      INTEGER :: n_centroid_stretch_physgrid 
      real(KIND=r8) :: nrmsdiff_centroid_stretch_physgrid, rmsdiff_centroid_stretch_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_centroid_stretch_physgrid(:,:,:), buf2_centroid_stretch_physgrid(:,:,:) 
      real(KIND=r8) :: diff_dalpha_physgrid 
      real(KIND=r8) :: diff_dbeta_physgrid 
      INTEGER :: idx1_center_cart_physgrid, idx2_center_cart_physgrid 
      INTEGER :: n_area_sphere_physgrid 
      real(KIND=r8) :: nrmsdiff_area_sphere_physgrid, rmsdiff_area_sphere_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_area_sphere_physgrid(:,:), buf2_area_sphere_physgrid(:,:) 
      INTEGER :: n_jx_min_physgrid 
      integer :: nrmsdiff_jx_min_physgrid, rmsdiff_jx_min_physgrid 
      integer, ALLOCATABLE :: buf1_jx_min_physgrid(:), buf2_jx_min_physgrid(:) 
      INTEGER :: n_jx_max_physgrid 
      integer :: nrmsdiff_jx_max_physgrid, rmsdiff_jx_max_physgrid 
      integer, ALLOCATABLE :: buf1_jx_max_physgrid(:), buf2_jx_max_physgrid(:) 
      INTEGER :: n_jy_min_physgrid 
      integer :: nrmsdiff_jy_min_physgrid, rmsdiff_jy_min_physgrid 
      integer, ALLOCATABLE :: buf1_jy_min_physgrid(:), buf2_jy_min_physgrid(:) 
      INTEGER :: n_jy_max_physgrid 
      integer :: nrmsdiff_jy_max_physgrid, rmsdiff_jy_max_physgrid 
      integer, ALLOCATABLE :: buf1_jy_max_physgrid(:), buf2_jy_max_physgrid(:) 
      INTEGER :: n_ibase_physgrid 
      integer :: nrmsdiff_ibase_physgrid, rmsdiff_ibase_physgrid 
      integer, ALLOCATABLE :: buf1_ibase_physgrid(:,:,:), buf2_ibase_physgrid(:,:,:) 
      INTEGER :: n_halo_interp_weight_physgrid 
      real(KIND=r8) :: nrmsdiff_halo_interp_weight_physgrid, rmsdiff_halo_interp_weight_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_halo_interp_weight_physgrid(:,:,:,:), buf2_halo_interp_weight_physgrid(:,:,:,:) 
      INTEGER :: n_vertex_recons_weights_physgrid 
      real(KIND=r8) :: nrmsdiff_vertex_recons_weights_physgrid, rmsdiff_vertex_recons_weights_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_vertex_recons_weights_physgrid(:,:,:,:), buf2_vertex_recons_weights_physgrid(:,:,:,:) 
      INTEGER :: n_norm_elem_coord_physgrid 
      real(KIND=r8) :: nrmsdiff_norm_elem_coord_physgrid, rmsdiff_norm_elem_coord_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_norm_elem_coord_physgrid(:,:,:), buf2_norm_elem_coord_physgrid(:,:,:) 
      INTEGER :: n_dinv_physgrid 
      real(KIND=r8) :: nrmsdiff_dinv_physgrid, rmsdiff_dinv_physgrid 
      real(KIND=r8), ALLOCATABLE :: buf1_dinv_physgrid(:,:,:,:), buf2_dinv_physgrid(:,:,:,:) 
      INTEGER :: n_fc 
      real(KIND=r8) :: nrmsdiff_fc, rmsdiff_fc 
      real(KIND=r8), ALLOCATABLE :: buf1_fc(:,:,:,:), buf2_fc(:,:,:,:) 
      INTEGER :: n_fc_phys 
      real(KIND=r8) :: nrmsdiff_fc_phys, rmsdiff_fc_phys 
      real(KIND=r8), ALLOCATABLE :: buf1_fc_phys(:,:,:,:), buf2_fc_phys(:,:,:,:) 
      INTEGER :: n_ft 
      real(KIND=r8) :: nrmsdiff_ft, rmsdiff_ft 
      real(KIND=r8), ALLOCATABLE :: buf1_ft(:,:,:), buf2_ft(:,:,:) 
      INTEGER :: n_fm 
      real(KIND=r8) :: nrmsdiff_fm, rmsdiff_fm 
      real(KIND=r8), ALLOCATABLE :: buf1_fm(:,:,:,:), buf2_fm(:,:,:,:) 
      INTEGER :: n_dp_phys 
      real(KIND=r8) :: nrmsdiff_dp_phys, rmsdiff_dp_phys 
      real(KIND=r8), ALLOCATABLE :: buf1_dp_phys(:,:,:), buf2_dp_phys(:,:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%c == kgenref_var%c)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%c is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_c(SIZE(var%c,dim=1),SIZE(var%c,dim=2),SIZE(var%c,dim=3),SIZE(var%c,dim=4),SIZE(var%c,dim=5))) 
          ALLOCATE (buf2_c(SIZE(var%c,dim=1),SIZE(var%c,dim=2),SIZE(var%c,dim=3),SIZE(var%c,dim=4),SIZE(var%c,dim=5))) 
          n_c = COUNT(var%c /= kgenref_var%c) 
          WHERE ( ABS(kgenref_var%c) > kgen_minvalue ) 
              buf1_c = ((var%c-kgenref_var%c)/kgenref_var%c)**2 
              buf2_c = (var%c-kgenref_var%c)**2 
          ELSEWHERE 
              buf1_c = (var%c-kgenref_var%c)**2 
              buf2_c = buf1_c 
          END WHERE   
          nrmsdiff_c = SQRT(SUM(buf1_c)/REAL(n_c)) 
          rmsdiff_c = SQRT(SUM(buf2_c)/REAL(n_c)) 
          IF (nrmsdiff_c > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%c is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%c is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%c /= kgenref_var%c), " of ", size( var%c ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%c)/real(size(var%c)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%c)/real(size(kgenref_var%c)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_c 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_c 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%c /= kgenref_var%c), " of ", size( var%c ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%c)/real(size(var%c)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%c)/real(size(kgenref_var%c)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_c 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_c 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%se_flux == kgenref_var%se_flux)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%se_flux is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_se_flux(SIZE(var%se_flux,dim=1),SIZE(var%se_flux,dim=2),SIZE(var%se_flux,dim=3),SIZE(var%se_flux,dim=4))) 
          ALLOCATE &
          &(buf2_se_flux(SIZE(var%se_flux,dim=1),SIZE(var%se_flux,dim=2),SIZE(var%se_flux,dim=3),SIZE(var%se_flux,dim=4))) 
          n_se_flux = COUNT(var%se_flux /= kgenref_var%se_flux) 
          WHERE ( ABS(kgenref_var%se_flux) > kgen_minvalue ) 
              buf1_se_flux = ((var%se_flux-kgenref_var%se_flux)/kgenref_var%se_flux)**2 
              buf2_se_flux = (var%se_flux-kgenref_var%se_flux)**2 
          ELSEWHERE 
              buf1_se_flux = (var%se_flux-kgenref_var%se_flux)**2 
              buf2_se_flux = buf1_se_flux 
          END WHERE   
          nrmsdiff_se_flux = SQRT(SUM(buf1_se_flux)/REAL(n_se_flux)) 
          rmsdiff_se_flux = SQRT(SUM(buf2_se_flux)/REAL(n_se_flux)) 
          IF (nrmsdiff_se_flux > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%se_flux is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%se_flux is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%se_flux /= kgenref_var%se_flux), " of ", size( var%se_flux ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%se_flux)/real(size(var%se_flux)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%se_flux)/real(size(kgenref_var%se_flux)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_se_flux 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_se_flux 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%se_flux /= kgenref_var%se_flux), " of ", size( var%se_flux ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%se_flux)/real(size(var%se_flux)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%se_flux)/real(size(kgenref_var%se_flux)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_se_flux 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_se_flux 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dp_fvm == kgenref_var%dp_fvm)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dp_fvm is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dp_fvm(SIZE(var%dp_fvm,dim=1),SIZE(var%dp_fvm,dim=2),SIZE(var%dp_fvm,dim=3),SIZE(var%dp_fvm,dim=4))) 
          ALLOCATE (buf2_dp_fvm(SIZE(var%dp_fvm,dim=1),SIZE(var%dp_fvm,dim=2),SIZE(var%dp_fvm,dim=3),SIZE(var%dp_fvm,dim=4))) 
          n_dp_fvm = COUNT(var%dp_fvm /= kgenref_var%dp_fvm) 
          WHERE ( ABS(kgenref_var%dp_fvm) > kgen_minvalue ) 
              buf1_dp_fvm = ((var%dp_fvm-kgenref_var%dp_fvm)/kgenref_var%dp_fvm)**2 
              buf2_dp_fvm = (var%dp_fvm-kgenref_var%dp_fvm)**2 
          ELSEWHERE 
              buf1_dp_fvm = (var%dp_fvm-kgenref_var%dp_fvm)**2 
              buf2_dp_fvm = buf1_dp_fvm 
          END WHERE   
          nrmsdiff_dp_fvm = SQRT(SUM(buf1_dp_fvm)/REAL(n_dp_fvm)) 
          rmsdiff_dp_fvm = SQRT(SUM(buf2_dp_fvm)/REAL(n_dp_fvm)) 
          IF (nrmsdiff_dp_fvm > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_fvm is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_fvm is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp_fvm /= kgenref_var%dp_fvm), " of ", size( var%dp_fvm ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp_fvm)/real(size(var%dp_fvm)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_fvm)/real(size(kgenref_var%dp_fvm)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp_fvm 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_fvm 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp_fvm /= kgenref_var%dp_fvm), " of ", size( var%dp_fvm ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp_fvm)/real(size(var%dp_fvm)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_fvm)/real(size(kgenref_var%dp_fvm)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp_fvm 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_fvm 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dp_ref == kgenref_var%dp_ref)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dp_ref is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dp_ref(SIZE(var%dp_ref,dim=1))) 
          ALLOCATE (buf2_dp_ref(SIZE(var%dp_ref,dim=1))) 
          n_dp_ref = COUNT(var%dp_ref /= kgenref_var%dp_ref) 
          WHERE ( ABS(kgenref_var%dp_ref) > kgen_minvalue ) 
              buf1_dp_ref = ((var%dp_ref-kgenref_var%dp_ref)/kgenref_var%dp_ref)**2 
              buf2_dp_ref = (var%dp_ref-kgenref_var%dp_ref)**2 
          ELSEWHERE 
              buf1_dp_ref = (var%dp_ref-kgenref_var%dp_ref)**2 
              buf2_dp_ref = buf1_dp_ref 
          END WHERE   
          nrmsdiff_dp_ref = SQRT(SUM(buf1_dp_ref)/REAL(n_dp_ref)) 
          rmsdiff_dp_ref = SQRT(SUM(buf2_dp_ref)/REAL(n_dp_ref)) 
          IF (nrmsdiff_dp_ref > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_ref is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_ref is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp_ref /= kgenref_var%dp_ref), " of ", size( var%dp_ref ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp_ref)/real(size(var%dp_ref)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_ref)/real(size(kgenref_var%dp_ref)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp_ref 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_ref 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp_ref /= kgenref_var%dp_ref), " of ", size( var%dp_ref ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp_ref)/real(size(var%dp_ref)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_ref)/real(size(kgenref_var%dp_ref)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp_ref 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_ref 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%dp_ref_inverse == kgenref_var%dp_ref_inverse)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dp_ref_inverse is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_dp_ref_inverse(SIZE(var%dp_ref_inverse,dim=1))) 
          ALLOCATE (buf2_dp_ref_inverse(SIZE(var%dp_ref_inverse,dim=1))) 
          n_dp_ref_inverse = COUNT(var%dp_ref_inverse /= kgenref_var%dp_ref_inverse) 
          WHERE ( ABS(kgenref_var%dp_ref_inverse) > kgen_minvalue ) 
              buf1_dp_ref_inverse = ((var%dp_ref_inverse-kgenref_var%dp_ref_inverse)/kgenref_var%dp_ref_inverse)**2 
              buf2_dp_ref_inverse = (var%dp_ref_inverse-kgenref_var%dp_ref_inverse)**2 
          ELSEWHERE 
              buf1_dp_ref_inverse = (var%dp_ref_inverse-kgenref_var%dp_ref_inverse)**2 
              buf2_dp_ref_inverse = buf1_dp_ref_inverse 
          END WHERE   
          nrmsdiff_dp_ref_inverse = SQRT(SUM(buf1_dp_ref_inverse)/REAL(n_dp_ref_inverse)) 
          rmsdiff_dp_ref_inverse = SQRT(SUM(buf2_dp_ref_inverse)/REAL(n_dp_ref_inverse)) 
          IF (nrmsdiff_dp_ref_inverse > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_ref_inverse is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_ref_inverse is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp_ref_inverse /= kgenref_var%dp_ref_inverse), " of ", size( var%dp_ref_inverse ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp_ref_inverse)/real(size(var%dp_ref_inverse)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_ref_inverse)/real(size(kgenref_var%dp_ref_inverse)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp_ref_inverse 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_ref_inverse 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%dp_ref_inverse /= kgenref_var%dp_ref_inverse), " of ", size( var%dp_ref_inverse ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%dp_ref_inverse)/real(size(var%dp_ref_inverse)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_ref_inverse)/real(size(kgenref_var%dp_ref_inverse)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_dp_ref_inverse 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_ref_inverse 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%psc == kgenref_var%psc)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%psc is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_psc(SIZE(var%psc,dim=1),SIZE(var%psc,dim=2))) 
          ALLOCATE (buf2_psc(SIZE(var%psc,dim=1),SIZE(var%psc,dim=2))) 
          n_psc = COUNT(var%psc /= kgenref_var%psc) 
          WHERE ( ABS(kgenref_var%psc) > kgen_minvalue ) 
              buf1_psc = ((var%psc-kgenref_var%psc)/kgenref_var%psc)**2 
              buf2_psc = (var%psc-kgenref_var%psc)**2 
          ELSEWHERE 
              buf1_psc = (var%psc-kgenref_var%psc)**2 
              buf2_psc = buf1_psc 
          END WHERE   
          nrmsdiff_psc = SQRT(SUM(buf1_psc)/REAL(n_psc)) 
          rmsdiff_psc = SQRT(SUM(buf2_psc)/REAL(n_psc)) 
          IF (nrmsdiff_psc > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%psc is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%psc is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%psc /= kgenref_var%psc), " of ", size( var%psc ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%psc)/real(size(var%psc)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%psc)/real(size(kgenref_var%psc)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_psc 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_psc 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%psc /= kgenref_var%psc), " of ", size( var%psc ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%psc)/real(size(var%psc)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%psc)/real(size(kgenref_var%psc)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_psc 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_psc 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%inv_area_sphere == kgenref_var%inv_area_sphere)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%inv_area_sphere is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_inv_area_sphere(SIZE(var%inv_area_sphere,dim=1),SIZE(var%inv_area_sphere,dim=2))) 
          ALLOCATE (buf2_inv_area_sphere(SIZE(var%inv_area_sphere,dim=1),SIZE(var%inv_area_sphere,dim=2))) 
          n_inv_area_sphere = COUNT(var%inv_area_sphere /= kgenref_var%inv_area_sphere) 
          WHERE ( ABS(kgenref_var%inv_area_sphere) > kgen_minvalue ) 
              buf1_inv_area_sphere = ((var%inv_area_sphere-kgenref_var%inv_area_sphere)/kgenref_var%inv_area_sphere)**2 
              buf2_inv_area_sphere = (var%inv_area_sphere-kgenref_var%inv_area_sphere)**2 
          ELSEWHERE 
              buf1_inv_area_sphere = (var%inv_area_sphere-kgenref_var%inv_area_sphere)**2 
              buf2_inv_area_sphere = buf1_inv_area_sphere 
          END WHERE   
          nrmsdiff_inv_area_sphere = SQRT(SUM(buf1_inv_area_sphere)/REAL(n_inv_area_sphere)) 
          rmsdiff_inv_area_sphere = SQRT(SUM(buf2_inv_area_sphere)/REAL(n_inv_area_sphere)) 
          IF (nrmsdiff_inv_area_sphere > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%inv_area_sphere is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%inv_area_sphere is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%inv_area_sphere /= kgenref_var%inv_area_sphere), " of ", size( var%inv_area_sphere ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%inv_area_sphere)/real(size(var%inv_area_sphere)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%inv_area_sphere)/real(size(kgenref_var%inv_area_sphere)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_inv_area_sphere 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_inv_area_sphere 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%inv_area_sphere /= kgenref_var%inv_area_sphere), " of ", size( var%inv_area_sphere ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%inv_area_sphere)/real(size(var%inv_area_sphere)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%inv_area_sphere)/real(size(kgenref_var%inv_area_sphere)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_inv_area_sphere 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_inv_area_sphere 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%inv_se_area_sphere == kgenref_var%inv_se_area_sphere)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%inv_se_area_sphere is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_inv_se_area_sphere(SIZE(var%inv_se_area_sphere,dim=1),SIZE(var%inv_se_area_sphere,dim=2))) 
          ALLOCATE (buf2_inv_se_area_sphere(SIZE(var%inv_se_area_sphere,dim=1),SIZE(var%inv_se_area_sphere,dim=2))) 
          n_inv_se_area_sphere = COUNT(var%inv_se_area_sphere /= kgenref_var%inv_se_area_sphere) 
          WHERE ( ABS(kgenref_var%inv_se_area_sphere) > kgen_minvalue ) 
              buf1_inv_se_area_sphere = &
              &((var%inv_se_area_sphere-kgenref_var%inv_se_area_sphere)/kgenref_var%inv_se_area_sphere)**2 
              buf2_inv_se_area_sphere = (var%inv_se_area_sphere-kgenref_var%inv_se_area_sphere)**2 
          ELSEWHERE 
              buf1_inv_se_area_sphere = (var%inv_se_area_sphere-kgenref_var%inv_se_area_sphere)**2 
              buf2_inv_se_area_sphere = buf1_inv_se_area_sphere 
          END WHERE   
          nrmsdiff_inv_se_area_sphere = SQRT(SUM(buf1_inv_se_area_sphere)/REAL(n_inv_se_area_sphere)) 
          rmsdiff_inv_se_area_sphere = SQRT(SUM(buf2_inv_se_area_sphere)/REAL(n_inv_se_area_sphere)) 
          IF (nrmsdiff_inv_se_area_sphere > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%inv_se_area_sphere is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%inv_se_area_sphere is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%inv_se_area_sphere /= kgenref_var%inv_se_area_sphere), " of ", size( var%inv_se_area_sphere &
              &), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%inv_se_area_sphere)/real(size(var%inv_se_area_sphere)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%inv_se_area_sphere)/real(size(kgenref_var%inv_se_area_sphere)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_inv_se_area_sphere 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_inv_se_area_sphere 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%inv_se_area_sphere /= kgenref_var%inv_se_area_sphere), " of ", size( var%inv_se_area_sphere &
              &), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%inv_se_area_sphere)/real(size(var%inv_se_area_sphere)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%inv_se_area_sphere)/real(size(kgenref_var%inv_se_area_sphere)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_inv_se_area_sphere 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_inv_se_area_sphere 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%faceno == kgenref_var%faceno) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%faceno is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_faceno = ABS(var%faceno - kgenref_var%faceno) 
          IF (diff_faceno <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%faceno is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%faceno is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_faceno 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_faceno 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cubeboundary == kgenref_var%cubeboundary) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%cubeboundary is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cubeboundary = ABS(var%cubeboundary - kgenref_var%cubeboundary) 
          IF (diff_cubeboundary <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cubeboundary is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cubeboundary is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_cubeboundary 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_cubeboundary 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%displ_max == kgenref_var%displ_max)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%displ_max is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_displ_max(SIZE(var%displ_max,dim=1),SIZE(var%displ_max,dim=2),SIZE(var%displ_max,dim=3))) 
          ALLOCATE (buf2_displ_max(SIZE(var%displ_max,dim=1),SIZE(var%displ_max,dim=2),SIZE(var%displ_max,dim=3))) 
          n_displ_max = COUNT(var%displ_max /= kgenref_var%displ_max) 
          WHERE ( ABS(kgenref_var%displ_max) > kgen_minvalue ) 
              buf1_displ_max = ((var%displ_max-kgenref_var%displ_max)/kgenref_var%displ_max)**2 
              buf2_displ_max = (var%displ_max-kgenref_var%displ_max)**2 
          ELSEWHERE 
              buf1_displ_max = (var%displ_max-kgenref_var%displ_max)**2 
              buf2_displ_max = buf1_displ_max 
          END WHERE   
          nrmsdiff_displ_max = SQRT(SUM(buf1_displ_max)/REAL(n_displ_max)) 
          rmsdiff_displ_max = SQRT(SUM(buf2_displ_max)/REAL(n_displ_max)) 
          IF (nrmsdiff_displ_max > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%displ_max is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%displ_max is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%displ_max /= kgenref_var%displ_max), " of ", size( var%displ_max ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%displ_max)/real(size(var%displ_max)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%displ_max)/real(size(kgenref_var%displ_max)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_displ_max 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_displ_max 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%displ_max /= kgenref_var%displ_max), " of ", size( var%displ_max ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%displ_max)/real(size(var%displ_max)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%displ_max)/real(size(kgenref_var%displ_max)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_displ_max 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_displ_max 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%flux_vec == kgenref_var%flux_vec)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%flux_vec is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_flux_vec(SIZE(var%flux_vec,dim=1),SIZE(var%flux_vec,dim=2),SIZE(var%flux_vec,dim=3),SIZE(var%flux_vec,dim=4))) 
          ALLOCATE &
          &(buf2_flux_vec(SIZE(var%flux_vec,dim=1),SIZE(var%flux_vec,dim=2),SIZE(var%flux_vec,dim=3),SIZE(var%flux_vec,dim=4))) 
          n_flux_vec = COUNT(var%flux_vec /= kgenref_var%flux_vec) 
          WHERE ( ABS(kgenref_var%flux_vec) > kgen_minvalue ) 
              buf1_flux_vec = ((var%flux_vec-kgenref_var%flux_vec)/kgenref_var%flux_vec)**2 
              buf2_flux_vec = (var%flux_vec-kgenref_var%flux_vec)**2 
          ELSEWHERE 
              buf1_flux_vec = (var%flux_vec-kgenref_var%flux_vec)**2 
              buf2_flux_vec = buf1_flux_vec 
          END WHERE   
          nrmsdiff_flux_vec = SQRT(SUM(buf1_flux_vec)/REAL(n_flux_vec)) 
          rmsdiff_flux_vec = SQRT(SUM(buf2_flux_vec)/REAL(n_flux_vec)) 
          IF (nrmsdiff_flux_vec > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%flux_vec is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%flux_vec is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%flux_vec /= kgenref_var%flux_vec), " of ", size( var%flux_vec ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%flux_vec)/real(size(var%flux_vec)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%flux_vec)/real(size(kgenref_var%flux_vec)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_flux_vec 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_flux_vec 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%flux_vec /= kgenref_var%flux_vec), " of ", size( var%flux_vec ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%flux_vec)/real(size(var%flux_vec)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%flux_vec)/real(size(kgenref_var%flux_vec)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_flux_vec 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_flux_vec 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%vtx_cart == kgenref_var%vtx_cart)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vtx_cart is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_vtx_cart(SIZE(var%vtx_cart,dim=1),SIZE(var%vtx_cart,dim=2),SIZE(var%vtx_cart,dim=3),SIZE(var%vtx_cart,dim=4))) 
          ALLOCATE &
          &(buf2_vtx_cart(SIZE(var%vtx_cart,dim=1),SIZE(var%vtx_cart,dim=2),SIZE(var%vtx_cart,dim=3),SIZE(var%vtx_cart,dim=4))) 
          n_vtx_cart = COUNT(var%vtx_cart /= kgenref_var%vtx_cart) 
          WHERE ( ABS(kgenref_var%vtx_cart) > kgen_minvalue ) 
              buf1_vtx_cart = ((var%vtx_cart-kgenref_var%vtx_cart)/kgenref_var%vtx_cart)**2 
              buf2_vtx_cart = (var%vtx_cart-kgenref_var%vtx_cart)**2 
          ELSEWHERE 
              buf1_vtx_cart = (var%vtx_cart-kgenref_var%vtx_cart)**2 
              buf2_vtx_cart = buf1_vtx_cart 
          END WHERE   
          nrmsdiff_vtx_cart = SQRT(SUM(buf1_vtx_cart)/REAL(n_vtx_cart)) 
          rmsdiff_vtx_cart = SQRT(SUM(buf2_vtx_cart)/REAL(n_vtx_cart)) 
          IF (nrmsdiff_vtx_cart > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vtx_cart is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vtx_cart is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vtx_cart /= kgenref_var%vtx_cart), " of ", size( var%vtx_cart ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vtx_cart)/real(size(var%vtx_cart)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vtx_cart)/real(size(kgenref_var%vtx_cart)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vtx_cart 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vtx_cart 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vtx_cart /= kgenref_var%vtx_cart), " of ", size( var%vtx_cart ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vtx_cart)/real(size(var%vtx_cart)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%vtx_cart)/real(size(kgenref_var%vtx_cart)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vtx_cart 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vtx_cart 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%flux_orient == kgenref_var%flux_orient)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%flux_orient is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_flux_orient(SIZE(var%flux_orient,dim=1),SIZE(var%flux_orient,dim=2),SIZE(var%flux_orient,dim=3))) 
          ALLOCATE (buf2_flux_orient(SIZE(var%flux_orient,dim=1),SIZE(var%flux_orient,dim=2),SIZE(var%flux_orient,dim=3))) 
          n_flux_orient = COUNT(var%flux_orient /= kgenref_var%flux_orient) 
          WHERE ( ABS(kgenref_var%flux_orient) > kgen_minvalue ) 
              buf1_flux_orient = ((var%flux_orient-kgenref_var%flux_orient)/kgenref_var%flux_orient)**2 
              buf2_flux_orient = (var%flux_orient-kgenref_var%flux_orient)**2 
          ELSEWHERE 
              buf1_flux_orient = (var%flux_orient-kgenref_var%flux_orient)**2 
              buf2_flux_orient = buf1_flux_orient 
          END WHERE   
          nrmsdiff_flux_orient = SQRT(SUM(buf1_flux_orient)/REAL(n_flux_orient)) 
          rmsdiff_flux_orient = SQRT(SUM(buf2_flux_orient)/REAL(n_flux_orient)) 
          IF (nrmsdiff_flux_orient > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%flux_orient is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%flux_orient is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%flux_orient /= kgenref_var%flux_orient), " of ", size( var%flux_orient ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%flux_orient)/real(size(var%flux_orient)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%flux_orient)/real(size(kgenref_var%flux_orient)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_flux_orient 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_flux_orient 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%flux_orient /= kgenref_var%flux_orient), " of ", size( var%flux_orient ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%flux_orient)/real(size(var%flux_orient)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%flux_orient)/real(size(kgenref_var%flux_orient)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_flux_orient 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_flux_orient 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ifct == kgenref_var%ifct)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ifct is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ifct(SIZE(var%ifct,dim=1),SIZE(var%ifct,dim=2))) 
          ALLOCATE (buf2_ifct(SIZE(var%ifct,dim=1),SIZE(var%ifct,dim=2))) 
          n_ifct = COUNT(var%ifct /= kgenref_var%ifct) 
          WHERE ( ABS(kgenref_var%ifct) > kgen_minvalue ) 
              buf1_ifct = ((var%ifct-kgenref_var%ifct)/kgenref_var%ifct)**2 
              buf2_ifct = (var%ifct-kgenref_var%ifct)**2 
          ELSEWHERE 
              buf1_ifct = (var%ifct-kgenref_var%ifct)**2 
              buf2_ifct = buf1_ifct 
          END WHERE   
          nrmsdiff_ifct = SQRT(SUM(buf1_ifct)/REAL(n_ifct)) 
          rmsdiff_ifct = SQRT(SUM(buf2_ifct)/REAL(n_ifct)) 
          IF (nrmsdiff_ifct > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ifct is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ifct is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ifct /= kgenref_var%ifct), " of ", size( var%ifct ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ifct)/real(size(var%ifct)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ifct)/real(size(kgenref_var%ifct)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ifct 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ifct 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ifct /= kgenref_var%ifct), " of ", size( var%ifct ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ifct)/real(size(var%ifct)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ifct)/real(size(kgenref_var%ifct)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ifct 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ifct 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%rot_matrix == kgenref_var%rot_matrix)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%rot_matrix is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_rot_matrix(SIZE(var%rot_matrix,dim=1),SIZE(var%rot_matrix,dim=2),SIZE(var%rot_matrix,dim=3),SIZE(var%rot_matrix,d&
          &im=4))) 
          ALLOCATE &
          &(buf2_rot_matrix(SIZE(var%rot_matrix,dim=1),SIZE(var%rot_matrix,dim=2),SIZE(var%rot_matrix,dim=3),SIZE(var%rot_matrix,d&
          &im=4))) 
          n_rot_matrix = COUNT(var%rot_matrix /= kgenref_var%rot_matrix) 
          WHERE ( ABS(kgenref_var%rot_matrix) > kgen_minvalue ) 
              buf1_rot_matrix = ((var%rot_matrix-kgenref_var%rot_matrix)/kgenref_var%rot_matrix)**2 
              buf2_rot_matrix = (var%rot_matrix-kgenref_var%rot_matrix)**2 
          ELSEWHERE 
              buf1_rot_matrix = (var%rot_matrix-kgenref_var%rot_matrix)**2 
              buf2_rot_matrix = buf1_rot_matrix 
          END WHERE   
          nrmsdiff_rot_matrix = SQRT(SUM(buf1_rot_matrix)/REAL(n_rot_matrix)) 
          rmsdiff_rot_matrix = SQRT(SUM(buf2_rot_matrix)/REAL(n_rot_matrix)) 
          IF (nrmsdiff_rot_matrix > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rot_matrix is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rot_matrix is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rot_matrix /= kgenref_var%rot_matrix), " of ", size( var%rot_matrix ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%rot_matrix)/real(size(var%rot_matrix)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rot_matrix)/real(size(kgenref_var%rot_matrix)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rot_matrix 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rot_matrix 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%rot_matrix /= kgenref_var%rot_matrix), " of ", size( var%rot_matrix ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%rot_matrix)/real(size(var%rot_matrix)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%rot_matrix)/real(size(kgenref_var%rot_matrix)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_rot_matrix 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rot_matrix 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dalpha == kgenref_var%dalpha) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dalpha is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dalpha = ABS(var%dalpha - kgenref_var%dalpha) 
          IF (diff_dalpha <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dalpha is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dalpha is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dalpha 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dalpha 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dbeta == kgenref_var%dbeta) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dbeta is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dbeta = ABS(var%dbeta - kgenref_var%dbeta) 
          IF (diff_dbeta <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dbeta is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dbeta is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dbeta 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dbeta 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
      DO   idx1_center_cart = LBOUND(var%center_cart,1), UBOUND(var%center_cart,1) 
          DO   idx2_center_cart = LBOUND(var%center_cart,2), UBOUND(var%center_cart,2) 
              CALL kv_kgen_coordinate_systems_mod_typesubp0(trim(adjustl(varname))//"%center_cart", comp_check_status, &
              &var%center_cart(idx1_center_cart,idx2_center_cart), kgenref_var%center_cart(idx1_center_cart,idx2_center_cart)) 
          END DO   
      END DO   
      IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname))//"%center_cart", " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE IF (comp_check_status%numOutTol > 0) THEN 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%center_cart is NOT IDENTICAL(out of tolerance)." 
          END IF   
          check_result = CHECK_OUT_TOL 
      ELSE IF (comp_check_status%numInTol > 0) THEN 
          dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%center_cart is NOT IDENTICAL(within tolerance)." 
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
      IF (ALL(var%area_sphere == kgenref_var%area_sphere)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%area_sphere is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_area_sphere(SIZE(var%area_sphere,dim=1),SIZE(var%area_sphere,dim=2))) 
          ALLOCATE (buf2_area_sphere(SIZE(var%area_sphere,dim=1),SIZE(var%area_sphere,dim=2))) 
          n_area_sphere = COUNT(var%area_sphere /= kgenref_var%area_sphere) 
          WHERE ( ABS(kgenref_var%area_sphere) > kgen_minvalue ) 
              buf1_area_sphere = ((var%area_sphere-kgenref_var%area_sphere)/kgenref_var%area_sphere)**2 
              buf2_area_sphere = (var%area_sphere-kgenref_var%area_sphere)**2 
          ELSEWHERE 
              buf1_area_sphere = (var%area_sphere-kgenref_var%area_sphere)**2 
              buf2_area_sphere = buf1_area_sphere 
          END WHERE   
          nrmsdiff_area_sphere = SQRT(SUM(buf1_area_sphere)/REAL(n_area_sphere)) 
          rmsdiff_area_sphere = SQRT(SUM(buf2_area_sphere)/REAL(n_area_sphere)) 
          IF (nrmsdiff_area_sphere > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%area_sphere is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%area_sphere is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%area_sphere /= kgenref_var%area_sphere), " of ", size( var%area_sphere ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%area_sphere)/real(size(var%area_sphere)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%area_sphere)/real(size(kgenref_var%area_sphere)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_area_sphere 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_area_sphere 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%area_sphere /= kgenref_var%area_sphere), " of ", size( var%area_sphere ), " elements are &
              &different." 
              WRITE (*, *) "Average - kernel ", sum(var%area_sphere)/real(size(var%area_sphere)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%area_sphere)/real(size(kgenref_var%area_sphere)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_area_sphere 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_area_sphere 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%spherecentroid == kgenref_var%spherecentroid)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%spherecentroid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_spherecentroid(SIZE(var%spherecentroid,dim=1),SIZE(var%spherecentroid,dim=2),SIZE(var%spherecentroid,dim=3))) 
          ALLOCATE &
          &(buf2_spherecentroid(SIZE(var%spherecentroid,dim=1),SIZE(var%spherecentroid,dim=2),SIZE(var%spherecentroid,dim=3))) 
          n_spherecentroid = COUNT(var%spherecentroid /= kgenref_var%spherecentroid) 
          WHERE ( ABS(kgenref_var%spherecentroid) > kgen_minvalue ) 
              buf1_spherecentroid = ((var%spherecentroid-kgenref_var%spherecentroid)/kgenref_var%spherecentroid)**2 
              buf2_spherecentroid = (var%spherecentroid-kgenref_var%spherecentroid)**2 
          ELSEWHERE 
              buf1_spherecentroid = (var%spherecentroid-kgenref_var%spherecentroid)**2 
              buf2_spherecentroid = buf1_spherecentroid 
          END WHERE   
          nrmsdiff_spherecentroid = SQRT(SUM(buf1_spherecentroid)/REAL(n_spherecentroid)) 
          rmsdiff_spherecentroid = SQRT(SUM(buf2_spherecentroid)/REAL(n_spherecentroid)) 
          IF (nrmsdiff_spherecentroid > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spherecentroid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spherecentroid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%spherecentroid /= kgenref_var%spherecentroid), " of ", size( var%spherecentroid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%spherecentroid)/real(size(var%spherecentroid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%spherecentroid)/real(size(kgenref_var%spherecentroid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_spherecentroid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_spherecentroid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%spherecentroid /= kgenref_var%spherecentroid), " of ", size( var%spherecentroid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%spherecentroid)/real(size(var%spherecentroid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%spherecentroid)/real(size(kgenref_var%spherecentroid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_spherecentroid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_spherecentroid 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%recons_metrics == kgenref_var%recons_metrics)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%recons_metrics is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_recons_metrics(SIZE(var%recons_metrics,dim=1),SIZE(var%recons_metrics,dim=2),SIZE(var%recons_metrics,dim=3))) 
          ALLOCATE &
          &(buf2_recons_metrics(SIZE(var%recons_metrics,dim=1),SIZE(var%recons_metrics,dim=2),SIZE(var%recons_metrics,dim=3))) 
          n_recons_metrics = COUNT(var%recons_metrics /= kgenref_var%recons_metrics) 
          WHERE ( ABS(kgenref_var%recons_metrics) > kgen_minvalue ) 
              buf1_recons_metrics = ((var%recons_metrics-kgenref_var%recons_metrics)/kgenref_var%recons_metrics)**2 
              buf2_recons_metrics = (var%recons_metrics-kgenref_var%recons_metrics)**2 
          ELSEWHERE 
              buf1_recons_metrics = (var%recons_metrics-kgenref_var%recons_metrics)**2 
              buf2_recons_metrics = buf1_recons_metrics 
          END WHERE   
          nrmsdiff_recons_metrics = SQRT(SUM(buf1_recons_metrics)/REAL(n_recons_metrics)) 
          rmsdiff_recons_metrics = SQRT(SUM(buf2_recons_metrics)/REAL(n_recons_metrics)) 
          IF (nrmsdiff_recons_metrics > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%recons_metrics is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%recons_metrics is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%recons_metrics /= kgenref_var%recons_metrics), " of ", size( var%recons_metrics ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%recons_metrics)/real(size(var%recons_metrics)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%recons_metrics)/real(size(kgenref_var%recons_metrics)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%recons_metrics /= kgenref_var%recons_metrics), " of ", size( var%recons_metrics ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%recons_metrics)/real(size(var%recons_metrics)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%recons_metrics)/real(size(kgenref_var%recons_metrics)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%recons_metrics_integral == kgenref_var%recons_metrics_integral)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_integral is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_recons_metrics_integral(SIZE(var%recons_metrics_integral,dim=1),SIZE(var%recons_metrics_integral,dim=2),SIZE(var%&
          &recons_metrics_integral,dim=3))) 
          ALLOCATE &
          &(buf2_recons_metrics_integral(SIZE(var%recons_metrics_integral,dim=1),SIZE(var%recons_metrics_integral,dim=2),SIZE(var%&
          &recons_metrics_integral,dim=3))) 
          n_recons_metrics_integral = COUNT(var%recons_metrics_integral /= kgenref_var%recons_metrics_integral) 
          WHERE ( ABS(kgenref_var%recons_metrics_integral) > kgen_minvalue ) 
              buf1_recons_metrics_integral = &
              &((var%recons_metrics_integral-kgenref_var%recons_metrics_integral)/kgenref_var%recons_metrics_integral)**2 
              buf2_recons_metrics_integral = (var%recons_metrics_integral-kgenref_var%recons_metrics_integral)**2 
          ELSEWHERE 
              buf1_recons_metrics_integral = (var%recons_metrics_integral-kgenref_var%recons_metrics_integral)**2 
              buf2_recons_metrics_integral = buf1_recons_metrics_integral 
          END WHERE   
          nrmsdiff_recons_metrics_integral = SQRT(SUM(buf1_recons_metrics_integral)/REAL(n_recons_metrics_integral)) 
          rmsdiff_recons_metrics_integral = SQRT(SUM(buf2_recons_metrics_integral)/REAL(n_recons_metrics_integral)) 
          IF (nrmsdiff_recons_metrics_integral > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_integral is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_integral is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%recons_metrics_integral /= kgenref_var%recons_metrics_integral), " of ", size( &
              &var%recons_metrics_integral ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%recons_metrics_integral)/real(size(var%recons_metrics_integral)) 
              WRITE (*, *) "Average - reference ", &
              &sum(kgenref_var%recons_metrics_integral)/real(size(kgenref_var%recons_metrics_integral)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics_integral 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics_integral 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%recons_metrics_integral /= kgenref_var%recons_metrics_integral), " of ", size( &
              &var%recons_metrics_integral ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%recons_metrics_integral)/real(size(var%recons_metrics_integral)) 
              WRITE (*, *) "Average - reference ", &
              &sum(kgenref_var%recons_metrics_integral)/real(size(kgenref_var%recons_metrics_integral)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics_integral 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics_integral 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jx_min == kgenref_var%jx_min)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jx_min is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jx_min(SIZE(var%jx_min,dim=1))) 
          ALLOCATE (buf2_jx_min(SIZE(var%jx_min,dim=1))) 
          n_jx_min = COUNT(var%jx_min /= kgenref_var%jx_min) 
          WHERE ( ABS(kgenref_var%jx_min) > kgen_minvalue ) 
              buf1_jx_min = ((var%jx_min-kgenref_var%jx_min)/kgenref_var%jx_min)**2 
              buf2_jx_min = (var%jx_min-kgenref_var%jx_min)**2 
          ELSEWHERE 
              buf1_jx_min = (var%jx_min-kgenref_var%jx_min)**2 
              buf2_jx_min = buf1_jx_min 
          END WHERE   
          nrmsdiff_jx_min = SQRT(SUM(buf1_jx_min)/REAL(n_jx_min)) 
          rmsdiff_jx_min = SQRT(SUM(buf2_jx_min)/REAL(n_jx_min)) 
          IF (nrmsdiff_jx_min > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_min is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_min is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_min /= kgenref_var%jx_min), " of ", size( var%jx_min ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_min)/real(size(var%jx_min)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_min)/real(size(kgenref_var%jx_min)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_min 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_min 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_min /= kgenref_var%jx_min), " of ", size( var%jx_min ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_min)/real(size(var%jx_min)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_min)/real(size(kgenref_var%jx_min)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_min 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_min 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jx_max == kgenref_var%jx_max)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jx_max is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jx_max(SIZE(var%jx_max,dim=1))) 
          ALLOCATE (buf2_jx_max(SIZE(var%jx_max,dim=1))) 
          n_jx_max = COUNT(var%jx_max /= kgenref_var%jx_max) 
          WHERE ( ABS(kgenref_var%jx_max) > kgen_minvalue ) 
              buf1_jx_max = ((var%jx_max-kgenref_var%jx_max)/kgenref_var%jx_max)**2 
              buf2_jx_max = (var%jx_max-kgenref_var%jx_max)**2 
          ELSEWHERE 
              buf1_jx_max = (var%jx_max-kgenref_var%jx_max)**2 
              buf2_jx_max = buf1_jx_max 
          END WHERE   
          nrmsdiff_jx_max = SQRT(SUM(buf1_jx_max)/REAL(n_jx_max)) 
          rmsdiff_jx_max = SQRT(SUM(buf2_jx_max)/REAL(n_jx_max)) 
          IF (nrmsdiff_jx_max > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_max is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_max is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_max /= kgenref_var%jx_max), " of ", size( var%jx_max ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_max)/real(size(var%jx_max)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_max)/real(size(kgenref_var%jx_max)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_max 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_max 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_max /= kgenref_var%jx_max), " of ", size( var%jx_max ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_max)/real(size(var%jx_max)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_max)/real(size(kgenref_var%jx_max)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_max 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_max 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jy_min == kgenref_var%jy_min)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jy_min is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jy_min(SIZE(var%jy_min,dim=1))) 
          ALLOCATE (buf2_jy_min(SIZE(var%jy_min,dim=1))) 
          n_jy_min = COUNT(var%jy_min /= kgenref_var%jy_min) 
          WHERE ( ABS(kgenref_var%jy_min) > kgen_minvalue ) 
              buf1_jy_min = ((var%jy_min-kgenref_var%jy_min)/kgenref_var%jy_min)**2 
              buf2_jy_min = (var%jy_min-kgenref_var%jy_min)**2 
          ELSEWHERE 
              buf1_jy_min = (var%jy_min-kgenref_var%jy_min)**2 
              buf2_jy_min = buf1_jy_min 
          END WHERE   
          nrmsdiff_jy_min = SQRT(SUM(buf1_jy_min)/REAL(n_jy_min)) 
          rmsdiff_jy_min = SQRT(SUM(buf2_jy_min)/REAL(n_jy_min)) 
          IF (nrmsdiff_jy_min > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_min is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_min is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_min /= kgenref_var%jy_min), " of ", size( var%jy_min ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_min)/real(size(var%jy_min)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_min)/real(size(kgenref_var%jy_min)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_min 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_min 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_min /= kgenref_var%jy_min), " of ", size( var%jy_min ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_min)/real(size(var%jy_min)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_min)/real(size(kgenref_var%jy_min)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_min 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_min 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jy_max == kgenref_var%jy_max)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jy_max is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jy_max(SIZE(var%jy_max,dim=1))) 
          ALLOCATE (buf2_jy_max(SIZE(var%jy_max,dim=1))) 
          n_jy_max = COUNT(var%jy_max /= kgenref_var%jy_max) 
          WHERE ( ABS(kgenref_var%jy_max) > kgen_minvalue ) 
              buf1_jy_max = ((var%jy_max-kgenref_var%jy_max)/kgenref_var%jy_max)**2 
              buf2_jy_max = (var%jy_max-kgenref_var%jy_max)**2 
          ELSEWHERE 
              buf1_jy_max = (var%jy_max-kgenref_var%jy_max)**2 
              buf2_jy_max = buf1_jy_max 
          END WHERE   
          nrmsdiff_jy_max = SQRT(SUM(buf1_jy_max)/REAL(n_jy_max)) 
          rmsdiff_jy_max = SQRT(SUM(buf2_jy_max)/REAL(n_jy_max)) 
          IF (nrmsdiff_jy_max > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_max is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_max is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_max /= kgenref_var%jy_max), " of ", size( var%jy_max ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_max)/real(size(var%jy_max)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_max)/real(size(kgenref_var%jy_max)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_max 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_max 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_max /= kgenref_var%jy_max), " of ", size( var%jy_max ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_max)/real(size(var%jy_max)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_max)/real(size(kgenref_var%jy_max)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_max 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_max 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%ibase == kgenref_var%ibase)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%ibase is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_ibase(SIZE(var%ibase,dim=1),SIZE(var%ibase,dim=2),SIZE(var%ibase,dim=3))) 
          ALLOCATE (buf2_ibase(SIZE(var%ibase,dim=1),SIZE(var%ibase,dim=2),SIZE(var%ibase,dim=3))) 
          n_ibase = COUNT(var%ibase /= kgenref_var%ibase) 
          WHERE ( ABS(kgenref_var%ibase) > kgen_minvalue ) 
              buf1_ibase = ((var%ibase-kgenref_var%ibase)/kgenref_var%ibase)**2 
              buf2_ibase = (var%ibase-kgenref_var%ibase)**2 
          ELSEWHERE 
              buf1_ibase = (var%ibase-kgenref_var%ibase)**2 
              buf2_ibase = buf1_ibase 
          END WHERE   
          nrmsdiff_ibase = SQRT(SUM(buf1_ibase)/REAL(n_ibase)) 
          rmsdiff_ibase = SQRT(SUM(buf2_ibase)/REAL(n_ibase)) 
          IF (nrmsdiff_ibase > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ibase is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ibase is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ibase /= kgenref_var%ibase), " of ", size( var%ibase ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ibase)/real(size(var%ibase)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ibase)/real(size(kgenref_var%ibase)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ibase 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ibase 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%ibase /= kgenref_var%ibase), " of ", size( var%ibase ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%ibase)/real(size(var%ibase)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%ibase)/real(size(kgenref_var%ibase)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_ibase 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ibase 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%halo_interp_weight == kgenref_var%halo_interp_weight)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%halo_interp_weight is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_halo_interp_weight(SIZE(var%halo_interp_weight,dim=1),SIZE(var%halo_interp_weight,dim=2),SIZE(var%halo_interp_wei&
          &ght,dim=3),SIZE(var%halo_interp_weight,dim=4))) 
          ALLOCATE &
          &(buf2_halo_interp_weight(SIZE(var%halo_interp_weight,dim=1),SIZE(var%halo_interp_weight,dim=2),SIZE(var%halo_interp_wei&
          &ght,dim=3),SIZE(var%halo_interp_weight,dim=4))) 
          n_halo_interp_weight = COUNT(var%halo_interp_weight /= kgenref_var%halo_interp_weight) 
          WHERE ( ABS(kgenref_var%halo_interp_weight) > kgen_minvalue ) 
              buf1_halo_interp_weight = &
              &((var%halo_interp_weight-kgenref_var%halo_interp_weight)/kgenref_var%halo_interp_weight)**2 
              buf2_halo_interp_weight = (var%halo_interp_weight-kgenref_var%halo_interp_weight)**2 
          ELSEWHERE 
              buf1_halo_interp_weight = (var%halo_interp_weight-kgenref_var%halo_interp_weight)**2 
              buf2_halo_interp_weight = buf1_halo_interp_weight 
          END WHERE   
          nrmsdiff_halo_interp_weight = SQRT(SUM(buf1_halo_interp_weight)/REAL(n_halo_interp_weight)) 
          rmsdiff_halo_interp_weight = SQRT(SUM(buf2_halo_interp_weight)/REAL(n_halo_interp_weight)) 
          IF (nrmsdiff_halo_interp_weight > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%halo_interp_weight is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%halo_interp_weight is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%halo_interp_weight /= kgenref_var%halo_interp_weight), " of ", size( var%halo_interp_weight &
              &), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%halo_interp_weight)/real(size(var%halo_interp_weight)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%halo_interp_weight)/real(size(kgenref_var%halo_interp_weight)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_halo_interp_weight 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_halo_interp_weight 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%halo_interp_weight /= kgenref_var%halo_interp_weight), " of ", size( var%halo_interp_weight &
              &), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%halo_interp_weight)/real(size(var%halo_interp_weight)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%halo_interp_weight)/real(size(kgenref_var%halo_interp_weight)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_halo_interp_weight 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_halo_interp_weight 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%centroid_stretch == kgenref_var%centroid_stretch)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%centroid_stretch is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_centroid_stretch(SIZE(var%centroid_stretch,dim=1),SIZE(var%centroid_stretch,dim=2),SIZE(var%centroid_stretch,dim=&
          &3))) 
          ALLOCATE &
          &(buf2_centroid_stretch(SIZE(var%centroid_stretch,dim=1),SIZE(var%centroid_stretch,dim=2),SIZE(var%centroid_stretch,dim=&
          &3))) 
          n_centroid_stretch = COUNT(var%centroid_stretch /= kgenref_var%centroid_stretch) 
          WHERE ( ABS(kgenref_var%centroid_stretch) > kgen_minvalue ) 
              buf1_centroid_stretch = ((var%centroid_stretch-kgenref_var%centroid_stretch)/kgenref_var%centroid_stretch)**2 
              buf2_centroid_stretch = (var%centroid_stretch-kgenref_var%centroid_stretch)**2 
          ELSEWHERE 
              buf1_centroid_stretch = (var%centroid_stretch-kgenref_var%centroid_stretch)**2 
              buf2_centroid_stretch = buf1_centroid_stretch 
          END WHERE   
          nrmsdiff_centroid_stretch = SQRT(SUM(buf1_centroid_stretch)/REAL(n_centroid_stretch)) 
          rmsdiff_centroid_stretch = SQRT(SUM(buf2_centroid_stretch)/REAL(n_centroid_stretch)) 
          IF (nrmsdiff_centroid_stretch > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%centroid_stretch is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%centroid_stretch is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%centroid_stretch /= kgenref_var%centroid_stretch), " of ", size( var%centroid_stretch ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%centroid_stretch)/real(size(var%centroid_stretch)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%centroid_stretch)/real(size(kgenref_var%centroid_stretch)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_centroid_stretch 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_centroid_stretch 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%centroid_stretch /= kgenref_var%centroid_stretch), " of ", size( var%centroid_stretch ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%centroid_stretch)/real(size(var%centroid_stretch)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%centroid_stretch)/real(size(kgenref_var%centroid_stretch)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_centroid_stretch 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_centroid_stretch 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%vertex_recons_weights == kgenref_var%vertex_recons_weights)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%vertex_recons_weights is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_vertex_recons_weights(SIZE(var%vertex_recons_weights,dim=1),SIZE(var%vertex_recons_weights,dim=2),SIZE(var%vertex&
          &_recons_weights,dim=3),SIZE(var%vertex_recons_weights,dim=4))) 
          ALLOCATE &
          &(buf2_vertex_recons_weights(SIZE(var%vertex_recons_weights,dim=1),SIZE(var%vertex_recons_weights,dim=2),SIZE(var%vertex&
          &_recons_weights,dim=3),SIZE(var%vertex_recons_weights,dim=4))) 
          n_vertex_recons_weights = COUNT(var%vertex_recons_weights /= kgenref_var%vertex_recons_weights) 
          WHERE ( ABS(kgenref_var%vertex_recons_weights) > kgen_minvalue ) 
              buf1_vertex_recons_weights = &
              &((var%vertex_recons_weights-kgenref_var%vertex_recons_weights)/kgenref_var%vertex_recons_weights)**2 
              buf2_vertex_recons_weights = (var%vertex_recons_weights-kgenref_var%vertex_recons_weights)**2 
          ELSEWHERE 
              buf1_vertex_recons_weights = (var%vertex_recons_weights-kgenref_var%vertex_recons_weights)**2 
              buf2_vertex_recons_weights = buf1_vertex_recons_weights 
          END WHERE   
          nrmsdiff_vertex_recons_weights = SQRT(SUM(buf1_vertex_recons_weights)/REAL(n_vertex_recons_weights)) 
          rmsdiff_vertex_recons_weights = SQRT(SUM(buf2_vertex_recons_weights)/REAL(n_vertex_recons_weights)) 
          IF (nrmsdiff_vertex_recons_weights > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vertex_recons_weights is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vertex_recons_weights is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vertex_recons_weights /= kgenref_var%vertex_recons_weights), " of ", size( &
              &var%vertex_recons_weights ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vertex_recons_weights)/real(size(var%vertex_recons_weights)) 
              WRITE (*, *) "Average - reference ", &
              &sum(kgenref_var%vertex_recons_weights)/real(size(kgenref_var%vertex_recons_weights)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vertex_recons_weights 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vertex_recons_weights 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%vertex_recons_weights /= kgenref_var%vertex_recons_weights), " of ", size( &
              &var%vertex_recons_weights ), " elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%vertex_recons_weights)/real(size(var%vertex_recons_weights)) 
              WRITE (*, *) "Average - reference ", &
              &sum(kgenref_var%vertex_recons_weights)/real(size(kgenref_var%vertex_recons_weights)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_vertex_recons_weights 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vertex_recons_weights 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%norm_elem_coord == kgenref_var%norm_elem_coord)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%norm_elem_coord is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE &
          &(buf1_norm_elem_coord(SIZE(var%norm_elem_coord,dim=1),SIZE(var%norm_elem_coord,dim=2),SIZE(var%norm_elem_coord,dim=3)))&
          & 
          ALLOCATE &
          &(buf2_norm_elem_coord(SIZE(var%norm_elem_coord,dim=1),SIZE(var%norm_elem_coord,dim=2),SIZE(var%norm_elem_coord,dim=3)))&
          & 
          n_norm_elem_coord = COUNT(var%norm_elem_coord /= kgenref_var%norm_elem_coord) 
          WHERE ( ABS(kgenref_var%norm_elem_coord) > kgen_minvalue ) 
              buf1_norm_elem_coord = ((var%norm_elem_coord-kgenref_var%norm_elem_coord)/kgenref_var%norm_elem_coord)**2 
              buf2_norm_elem_coord = (var%norm_elem_coord-kgenref_var%norm_elem_coord)**2 
          ELSEWHERE 
              buf1_norm_elem_coord = (var%norm_elem_coord-kgenref_var%norm_elem_coord)**2 
              buf2_norm_elem_coord = buf1_norm_elem_coord 
          END WHERE   
          nrmsdiff_norm_elem_coord = SQRT(SUM(buf1_norm_elem_coord)/REAL(n_norm_elem_coord)) 
          rmsdiff_norm_elem_coord = SQRT(SUM(buf2_norm_elem_coord)/REAL(n_norm_elem_coord)) 
          IF (nrmsdiff_norm_elem_coord > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%norm_elem_coord is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%norm_elem_coord is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%norm_elem_coord /= kgenref_var%norm_elem_coord), " of ", size( var%norm_elem_coord ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%norm_elem_coord)/real(size(var%norm_elem_coord)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%norm_elem_coord)/real(size(kgenref_var%norm_elem_coord)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_norm_elem_coord 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_norm_elem_coord 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%norm_elem_coord /= kgenref_var%norm_elem_coord), " of ", size( var%norm_elem_coord ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%norm_elem_coord)/real(size(var%norm_elem_coord)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%norm_elem_coord)/real(size(kgenref_var%norm_elem_coord)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_norm_elem_coord 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_norm_elem_coord 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ALLOCATED(var%phis_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%phis_physgrid == kgenref_var%phis_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%phis_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_phis_physgrid(SIZE(var%phis_physgrid,dim=1),SIZE(var%phis_physgrid,dim=2))) 
              ALLOCATE (buf2_phis_physgrid(SIZE(var%phis_physgrid,dim=1),SIZE(var%phis_physgrid,dim=2))) 
              n_phis_physgrid = COUNT(var%phis_physgrid /= kgenref_var%phis_physgrid) 
              WHERE ( ABS(kgenref_var%phis_physgrid) > kgen_minvalue ) 
                  buf1_phis_physgrid = ((var%phis_physgrid-kgenref_var%phis_physgrid)/kgenref_var%phis_physgrid)**2 
                  buf2_phis_physgrid = (var%phis_physgrid-kgenref_var%phis_physgrid)**2 
              ELSEWHERE 
                  buf1_phis_physgrid = (var%phis_physgrid-kgenref_var%phis_physgrid)**2 
                  buf2_phis_physgrid = buf1_phis_physgrid 
              END WHERE   
              nrmsdiff_phis_physgrid = SQRT(SUM(buf1_phis_physgrid)/REAL(n_phis_physgrid)) 
              rmsdiff_phis_physgrid = SQRT(SUM(buf2_phis_physgrid)/REAL(n_phis_physgrid)) 
              IF (nrmsdiff_phis_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%phis_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%phis_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%phis_physgrid /= kgenref_var%phis_physgrid), " of ", size( var%phis_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%phis_physgrid)/real(size(var%phis_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%phis_physgrid)/real(size(kgenref_var%phis_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_phis_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phis_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%phis_physgrid /= kgenref_var%phis_physgrid), " of ", size( var%phis_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%phis_physgrid)/real(size(var%phis_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%phis_physgrid)/real(size(kgenref_var%phis_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_phis_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_phis_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%vtx_cart_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%vtx_cart_physgrid == kgenref_var%vtx_cart_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vtx_cart_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_vtx_cart_physgrid(SIZE(var%vtx_cart_physgrid,dim=1),SIZE(var%vtx_cart_physgrid,dim=2),SIZE(var%vtx_cart_physg&
              &rid,dim=3),SIZE(var%vtx_cart_physgrid,dim=4))) 
              ALLOCATE &
              &(buf2_vtx_cart_physgrid(SIZE(var%vtx_cart_physgrid,dim=1),SIZE(var%vtx_cart_physgrid,dim=2),SIZE(var%vtx_cart_physg&
              &rid,dim=3),SIZE(var%vtx_cart_physgrid,dim=4))) 
              n_vtx_cart_physgrid = COUNT(var%vtx_cart_physgrid /= kgenref_var%vtx_cart_physgrid) 
              WHERE ( ABS(kgenref_var%vtx_cart_physgrid) > kgen_minvalue ) 
                  buf1_vtx_cart_physgrid = &
                  &((var%vtx_cart_physgrid-kgenref_var%vtx_cart_physgrid)/kgenref_var%vtx_cart_physgrid)**2 
                  buf2_vtx_cart_physgrid = (var%vtx_cart_physgrid-kgenref_var%vtx_cart_physgrid)**2 
              ELSEWHERE 
                  buf1_vtx_cart_physgrid = (var%vtx_cart_physgrid-kgenref_var%vtx_cart_physgrid)**2 
                  buf2_vtx_cart_physgrid = buf1_vtx_cart_physgrid 
              END WHERE   
              nrmsdiff_vtx_cart_physgrid = SQRT(SUM(buf1_vtx_cart_physgrid)/REAL(n_vtx_cart_physgrid)) 
              rmsdiff_vtx_cart_physgrid = SQRT(SUM(buf2_vtx_cart_physgrid)/REAL(n_vtx_cart_physgrid)) 
              IF (nrmsdiff_vtx_cart_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%vtx_cart_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%vtx_cart_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%vtx_cart_physgrid /= kgenref_var%vtx_cart_physgrid), " of ", size( &
                  &var%vtx_cart_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%vtx_cart_physgrid)/real(size(var%vtx_cart_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%vtx_cart_physgrid)/real(size(kgenref_var%vtx_cart_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_vtx_cart_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vtx_cart_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%vtx_cart_physgrid /= kgenref_var%vtx_cart_physgrid), " of ", size( &
                  &var%vtx_cart_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%vtx_cart_physgrid)/real(size(var%vtx_cart_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%vtx_cart_physgrid)/real(size(kgenref_var%vtx_cart_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_vtx_cart_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vtx_cart_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%flux_orient_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%flux_orient_physgrid == kgenref_var%flux_orient_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%flux_orient_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_flux_orient_physgrid(SIZE(var%flux_orient_physgrid,dim=1),SIZE(var%flux_orient_physgrid,dim=2),SIZE(var%flux_&
              &orient_physgrid,dim=3))) 
              ALLOCATE &
              &(buf2_flux_orient_physgrid(SIZE(var%flux_orient_physgrid,dim=1),SIZE(var%flux_orient_physgrid,dim=2),SIZE(var%flux_&
              &orient_physgrid,dim=3))) 
              n_flux_orient_physgrid = COUNT(var%flux_orient_physgrid /= kgenref_var%flux_orient_physgrid) 
              WHERE ( ABS(kgenref_var%flux_orient_physgrid) > kgen_minvalue ) 
                  buf1_flux_orient_physgrid = &
                  &((var%flux_orient_physgrid-kgenref_var%flux_orient_physgrid)/kgenref_var%flux_orient_physgrid)**2 
                  buf2_flux_orient_physgrid = (var%flux_orient_physgrid-kgenref_var%flux_orient_physgrid)**2 
              ELSEWHERE 
                  buf1_flux_orient_physgrid = (var%flux_orient_physgrid-kgenref_var%flux_orient_physgrid)**2 
                  buf2_flux_orient_physgrid = buf1_flux_orient_physgrid 
              END WHERE   
              nrmsdiff_flux_orient_physgrid = SQRT(SUM(buf1_flux_orient_physgrid)/REAL(n_flux_orient_physgrid)) 
              rmsdiff_flux_orient_physgrid = SQRT(SUM(buf2_flux_orient_physgrid)/REAL(n_flux_orient_physgrid)) 
              IF (nrmsdiff_flux_orient_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%flux_orient_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%flux_orient_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%flux_orient_physgrid /= kgenref_var%flux_orient_physgrid), " of ", size( &
                  &var%flux_orient_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%flux_orient_physgrid)/real(size(var%flux_orient_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%flux_orient_physgrid)/real(size(kgenref_var%flux_orient_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_flux_orient_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_flux_orient_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%flux_orient_physgrid /= kgenref_var%flux_orient_physgrid), " of ", size( &
                  &var%flux_orient_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%flux_orient_physgrid)/real(size(var%flux_orient_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%flux_orient_physgrid)/real(size(kgenref_var%flux_orient_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_flux_orient_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_flux_orient_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%ifct_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%ifct_physgrid == kgenref_var%ifct_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ifct_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_ifct_physgrid(SIZE(var%ifct_physgrid,dim=1),SIZE(var%ifct_physgrid,dim=2))) 
              ALLOCATE (buf2_ifct_physgrid(SIZE(var%ifct_physgrid,dim=1),SIZE(var%ifct_physgrid,dim=2))) 
              n_ifct_physgrid = COUNT(var%ifct_physgrid /= kgenref_var%ifct_physgrid) 
              WHERE ( ABS(kgenref_var%ifct_physgrid) > kgen_minvalue ) 
                  buf1_ifct_physgrid = ((var%ifct_physgrid-kgenref_var%ifct_physgrid)/kgenref_var%ifct_physgrid)**2 
                  buf2_ifct_physgrid = (var%ifct_physgrid-kgenref_var%ifct_physgrid)**2 
              ELSEWHERE 
                  buf1_ifct_physgrid = (var%ifct_physgrid-kgenref_var%ifct_physgrid)**2 
                  buf2_ifct_physgrid = buf1_ifct_physgrid 
              END WHERE   
              nrmsdiff_ifct_physgrid = SQRT(SUM(buf1_ifct_physgrid)/REAL(n_ifct_physgrid)) 
              rmsdiff_ifct_physgrid = SQRT(SUM(buf2_ifct_physgrid)/REAL(n_ifct_physgrid)) 
              IF (nrmsdiff_ifct_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ifct_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ifct_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%ifct_physgrid /= kgenref_var%ifct_physgrid), " of ", size( var%ifct_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ifct_physgrid)/real(size(var%ifct_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ifct_physgrid)/real(size(kgenref_var%ifct_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ifct_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ifct_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%ifct_physgrid /= kgenref_var%ifct_physgrid), " of ", size( var%ifct_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ifct_physgrid)/real(size(var%ifct_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ifct_physgrid)/real(size(kgenref_var%ifct_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ifct_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ifct_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%rot_matrix_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%rot_matrix_physgrid == kgenref_var%rot_matrix_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rot_matrix_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_rot_matrix_physgrid(SIZE(var%rot_matrix_physgrid,dim=1),SIZE(var%rot_matrix_physgrid,dim=2),SIZE(var%rot_matr&
              &ix_physgrid,dim=3),SIZE(var%rot_matrix_physgrid,dim=4))) 
              ALLOCATE &
              &(buf2_rot_matrix_physgrid(SIZE(var%rot_matrix_physgrid,dim=1),SIZE(var%rot_matrix_physgrid,dim=2),SIZE(var%rot_matr&
              &ix_physgrid,dim=3),SIZE(var%rot_matrix_physgrid,dim=4))) 
              n_rot_matrix_physgrid = COUNT(var%rot_matrix_physgrid /= kgenref_var%rot_matrix_physgrid) 
              WHERE ( ABS(kgenref_var%rot_matrix_physgrid) > kgen_minvalue ) 
                  buf1_rot_matrix_physgrid = &
                  &((var%rot_matrix_physgrid-kgenref_var%rot_matrix_physgrid)/kgenref_var%rot_matrix_physgrid)**2 
                  buf2_rot_matrix_physgrid = (var%rot_matrix_physgrid-kgenref_var%rot_matrix_physgrid)**2 
              ELSEWHERE 
                  buf1_rot_matrix_physgrid = (var%rot_matrix_physgrid-kgenref_var%rot_matrix_physgrid)**2 
                  buf2_rot_matrix_physgrid = buf1_rot_matrix_physgrid 
              END WHERE   
              nrmsdiff_rot_matrix_physgrid = SQRT(SUM(buf1_rot_matrix_physgrid)/REAL(n_rot_matrix_physgrid)) 
              rmsdiff_rot_matrix_physgrid = SQRT(SUM(buf2_rot_matrix_physgrid)/REAL(n_rot_matrix_physgrid)) 
              IF (nrmsdiff_rot_matrix_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rot_matrix_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rot_matrix_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%rot_matrix_physgrid /= kgenref_var%rot_matrix_physgrid), " of ", size( &
                  &var%rot_matrix_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%rot_matrix_physgrid)/real(size(var%rot_matrix_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%rot_matrix_physgrid)/real(size(kgenref_var%rot_matrix_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_rot_matrix_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rot_matrix_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%rot_matrix_physgrid /= kgenref_var%rot_matrix_physgrid), " of ", size( &
                  &var%rot_matrix_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%rot_matrix_physgrid)/real(size(var%rot_matrix_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%rot_matrix_physgrid)/real(size(kgenref_var%rot_matrix_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_rot_matrix_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_rot_matrix_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%spherecentroid_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%spherecentroid_physgrid == kgenref_var%spherecentroid_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%spherecentroid_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_spherecentroid_physgrid(SIZE(var%spherecentroid_physgrid,dim=1),SIZE(var%spherecentroid_physgrid,dim=2),SIZE(&
              &var%spherecentroid_physgrid,dim=3))) 
              ALLOCATE &
              &(buf2_spherecentroid_physgrid(SIZE(var%spherecentroid_physgrid,dim=1),SIZE(var%spherecentroid_physgrid,dim=2),SIZE(&
              &var%spherecentroid_physgrid,dim=3))) 
              n_spherecentroid_physgrid = COUNT(var%spherecentroid_physgrid /= kgenref_var%spherecentroid_physgrid) 
              WHERE ( ABS(kgenref_var%spherecentroid_physgrid) > kgen_minvalue ) 
                  buf1_spherecentroid_physgrid = &
                  &((var%spherecentroid_physgrid-kgenref_var%spherecentroid_physgrid)/kgenref_var%spherecentroid_physgrid)**2 
                  buf2_spherecentroid_physgrid = (var%spherecentroid_physgrid-kgenref_var%spherecentroid_physgrid)**2 
              ELSEWHERE 
                  buf1_spherecentroid_physgrid = (var%spherecentroid_physgrid-kgenref_var%spherecentroid_physgrid)**2 
                  buf2_spherecentroid_physgrid = buf1_spherecentroid_physgrid 
              END WHERE   
              nrmsdiff_spherecentroid_physgrid = SQRT(SUM(buf1_spherecentroid_physgrid)/REAL(n_spherecentroid_physgrid)) 
              rmsdiff_spherecentroid_physgrid = SQRT(SUM(buf2_spherecentroid_physgrid)/REAL(n_spherecentroid_physgrid)) 
              IF (nrmsdiff_spherecentroid_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%spherecentroid_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%spherecentroid_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%spherecentroid_physgrid /= kgenref_var%spherecentroid_physgrid), " of ", size( &
                  &var%spherecentroid_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%spherecentroid_physgrid)/real(size(var%spherecentroid_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%spherecentroid_physgrid)/real(size(kgenref_var%spherecentroid_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_spherecentroid_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_spherecentroid_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%spherecentroid_physgrid /= kgenref_var%spherecentroid_physgrid), " of ", size( &
                  &var%spherecentroid_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%spherecentroid_physgrid)/real(size(var%spherecentroid_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%spherecentroid_physgrid)/real(size(kgenref_var%spherecentroid_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_spherecentroid_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_spherecentroid_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%recons_metrics_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%recons_metrics_physgrid == kgenref_var%recons_metrics_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_recons_metrics_physgrid(SIZE(var%recons_metrics_physgrid,dim=1),SIZE(var%recons_metrics_physgrid,dim=2),SIZE(&
              &var%recons_metrics_physgrid,dim=3))) 
              ALLOCATE &
              &(buf2_recons_metrics_physgrid(SIZE(var%recons_metrics_physgrid,dim=1),SIZE(var%recons_metrics_physgrid,dim=2),SIZE(&
              &var%recons_metrics_physgrid,dim=3))) 
              n_recons_metrics_physgrid = COUNT(var%recons_metrics_physgrid /= kgenref_var%recons_metrics_physgrid) 
              WHERE ( ABS(kgenref_var%recons_metrics_physgrid) > kgen_minvalue ) 
                  buf1_recons_metrics_physgrid = &
                  &((var%recons_metrics_physgrid-kgenref_var%recons_metrics_physgrid)/kgenref_var%recons_metrics_physgrid)**2 
                  buf2_recons_metrics_physgrid = (var%recons_metrics_physgrid-kgenref_var%recons_metrics_physgrid)**2 
              ELSEWHERE 
                  buf1_recons_metrics_physgrid = (var%recons_metrics_physgrid-kgenref_var%recons_metrics_physgrid)**2 
                  buf2_recons_metrics_physgrid = buf1_recons_metrics_physgrid 
              END WHERE   
              nrmsdiff_recons_metrics_physgrid = SQRT(SUM(buf1_recons_metrics_physgrid)/REAL(n_recons_metrics_physgrid)) 
              rmsdiff_recons_metrics_physgrid = SQRT(SUM(buf2_recons_metrics_physgrid)/REAL(n_recons_metrics_physgrid)) 
              IF (nrmsdiff_recons_metrics_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%recons_metrics_physgrid /= kgenref_var%recons_metrics_physgrid), " of ", size( &
                  &var%recons_metrics_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%recons_metrics_physgrid)/real(size(var%recons_metrics_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%recons_metrics_physgrid)/real(size(kgenref_var%recons_metrics_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%recons_metrics_physgrid /= kgenref_var%recons_metrics_physgrid), " of ", size( &
                  &var%recons_metrics_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%recons_metrics_physgrid)/real(size(var%recons_metrics_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%recons_metrics_physgrid)/real(size(kgenref_var%recons_metrics_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%recons_metrics_integral_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%recons_metrics_integral_physgrid == kgenref_var%recons_metrics_integral_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_integral_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_recons_metrics_integral_physgrid(SIZE(var%recons_metrics_integral_physgrid,dim=1),SIZE(var%recons_metrics_int&
              &egral_physgrid,dim=2),SIZE(var%recons_metrics_integral_physgrid,dim=3))) 
              ALLOCATE &
              &(buf2_recons_metrics_integral_physgrid(SIZE(var%recons_metrics_integral_physgrid,dim=1),SIZE(var%recons_metrics_int&
              &egral_physgrid,dim=2),SIZE(var%recons_metrics_integral_physgrid,dim=3))) 
              n_recons_metrics_integral_physgrid = COUNT(var%recons_metrics_integral_physgrid /= &
              &kgenref_var%recons_metrics_integral_physgrid) 
              WHERE ( ABS(kgenref_var%recons_metrics_integral_physgrid) > kgen_minvalue ) 
                  buf1_recons_metrics_integral_physgrid = &
                  &((var%recons_metrics_integral_physgrid-kgenref_var%recons_metrics_integral_physgrid)/kgenref_var%recons_metrics&
                  &_integral_physgrid)**2 
                  buf2_recons_metrics_integral_physgrid = &
                  &(var%recons_metrics_integral_physgrid-kgenref_var%recons_metrics_integral_physgrid)**2 
              ELSEWHERE 
                  buf1_recons_metrics_integral_physgrid = &
                  &(var%recons_metrics_integral_physgrid-kgenref_var%recons_metrics_integral_physgrid)**2 
                  buf2_recons_metrics_integral_physgrid = buf1_recons_metrics_integral_physgrid 
              END WHERE   
              nrmsdiff_recons_metrics_integral_physgrid = &
              &SQRT(SUM(buf1_recons_metrics_integral_physgrid)/REAL(n_recons_metrics_integral_physgrid)) 
              rmsdiff_recons_metrics_integral_physgrid = &
              &SQRT(SUM(buf2_recons_metrics_integral_physgrid)/REAL(n_recons_metrics_integral_physgrid)) 
              IF (nrmsdiff_recons_metrics_integral_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_integral_physgrid is NOT IDENTICAL(out of &
                      &tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%recons_metrics_integral_physgrid is NOT IDENTICAL(within &
                      &tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%recons_metrics_integral_physgrid /= kgenref_var%recons_metrics_integral_physgrid), " of &
                  &", size( var%recons_metrics_integral_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", &
                  &sum(var%recons_metrics_integral_physgrid)/real(size(var%recons_metrics_integral_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%recons_metrics_integral_physgrid)/real(size(kgenref_var%recons_metrics_integral_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics_integral_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics_integral_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%recons_metrics_integral_physgrid /= kgenref_var%recons_metrics_integral_physgrid), " of &
                  &", size( var%recons_metrics_integral_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", &
                  &sum(var%recons_metrics_integral_physgrid)/real(size(var%recons_metrics_integral_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%recons_metrics_integral_physgrid)/real(size(kgenref_var%recons_metrics_integral_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_recons_metrics_integral_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_recons_metrics_integral_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%centroid_stretch_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%centroid_stretch_physgrid == kgenref_var%centroid_stretch_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%centroid_stretch_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_centroid_stretch_physgrid(SIZE(var%centroid_stretch_physgrid,dim=1),SIZE(var%centroid_stretch_physgrid,dim=2)&
              &,SIZE(var%centroid_stretch_physgrid,dim=3))) 
              ALLOCATE &
              &(buf2_centroid_stretch_physgrid(SIZE(var%centroid_stretch_physgrid,dim=1),SIZE(var%centroid_stretch_physgrid,dim=2)&
              &,SIZE(var%centroid_stretch_physgrid,dim=3))) 
              n_centroid_stretch_physgrid = COUNT(var%centroid_stretch_physgrid /= kgenref_var%centroid_stretch_physgrid) 
              WHERE ( ABS(kgenref_var%centroid_stretch_physgrid) > kgen_minvalue ) 
                  buf1_centroid_stretch_physgrid = &
                  &((var%centroid_stretch_physgrid-kgenref_var%centroid_stretch_physgrid)/kgenref_var%centroid_stretch_physgrid)**&
                  &2 
                  buf2_centroid_stretch_physgrid = (var%centroid_stretch_physgrid-kgenref_var%centroid_stretch_physgrid)**2 
              ELSEWHERE 
                  buf1_centroid_stretch_physgrid = (var%centroid_stretch_physgrid-kgenref_var%centroid_stretch_physgrid)**2 
                  buf2_centroid_stretch_physgrid = buf1_centroid_stretch_physgrid 
              END WHERE   
              nrmsdiff_centroid_stretch_physgrid = SQRT(SUM(buf1_centroid_stretch_physgrid)/REAL(n_centroid_stretch_physgrid)) 
              rmsdiff_centroid_stretch_physgrid = SQRT(SUM(buf2_centroid_stretch_physgrid)/REAL(n_centroid_stretch_physgrid)) 
              IF (nrmsdiff_centroid_stretch_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%centroid_stretch_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%centroid_stretch_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%centroid_stretch_physgrid /= kgenref_var%centroid_stretch_physgrid), " of ", size( &
                  &var%centroid_stretch_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%centroid_stretch_physgrid)/real(size(var%centroid_stretch_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%centroid_stretch_physgrid)/real(size(kgenref_var%centroid_stretch_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_centroid_stretch_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_centroid_stretch_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%centroid_stretch_physgrid /= kgenref_var%centroid_stretch_physgrid), " of ", size( &
                  &var%centroid_stretch_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%centroid_stretch_physgrid)/real(size(var%centroid_stretch_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%centroid_stretch_physgrid)/real(size(kgenref_var%centroid_stretch_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_centroid_stretch_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_centroid_stretch_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dalpha_physgrid == kgenref_var%dalpha_physgrid) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dalpha_physgrid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dalpha_physgrid = ABS(var%dalpha_physgrid - kgenref_var%dalpha_physgrid) 
          IF (diff_dalpha_physgrid <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dalpha_physgrid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dalpha_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dalpha_physgrid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dalpha_physgrid 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%dbeta_physgrid == kgenref_var%dbeta_physgrid) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%dbeta_physgrid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_dbeta_physgrid = ABS(var%dbeta_physgrid - kgenref_var%dbeta_physgrid) 
          IF (diff_dbeta_physgrid <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dbeta_physgrid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dbeta_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dbeta_physgrid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_dbeta_physgrid 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ALLOCATED(var%center_cart_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
          DO   idx1_center_cart_physgrid = LBOUND(var%center_cart_physgrid,1), UBOUND(var%center_cart_physgrid,1) 
              DO   idx2_center_cart_physgrid = LBOUND(var%center_cart_physgrid,2), UBOUND(var%center_cart_physgrid,2) 
                  CALL kv_kgen_coordinate_systems_mod_typesubp0(trim(adjustl(varname))//"%center_cart_physgrid", &
                  &comp_check_status, var%center_cart_physgrid(idx1_center_cart_physgrid,idx2_center_cart_physgrid), &
                  &kgenref_var%center_cart_physgrid(idx1_center_cart_physgrid,idx2_center_cart_physgrid)) 
              END DO   
          END DO   
          IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname))//"%center_cart_physgrid", " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE IF (comp_check_status%numOutTol > 0) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%center_cart_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE IF (comp_check_status%numInTol > 0) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%center_cart_physgrid is NOT IDENTICAL(within tolerance)." 
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
            
      END IF   
      IF (ALLOCATED(var%area_sphere_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%area_sphere_physgrid == kgenref_var%area_sphere_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%area_sphere_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_area_sphere_physgrid(SIZE(var%area_sphere_physgrid,dim=1),SIZE(var%area_sphere_physgrid,dim=2))) 
              ALLOCATE (buf2_area_sphere_physgrid(SIZE(var%area_sphere_physgrid,dim=1),SIZE(var%area_sphere_physgrid,dim=2))) 
              n_area_sphere_physgrid = COUNT(var%area_sphere_physgrid /= kgenref_var%area_sphere_physgrid) 
              WHERE ( ABS(kgenref_var%area_sphere_physgrid) > kgen_minvalue ) 
                  buf1_area_sphere_physgrid = &
                  &((var%area_sphere_physgrid-kgenref_var%area_sphere_physgrid)/kgenref_var%area_sphere_physgrid)**2 
                  buf2_area_sphere_physgrid = (var%area_sphere_physgrid-kgenref_var%area_sphere_physgrid)**2 
              ELSEWHERE 
                  buf1_area_sphere_physgrid = (var%area_sphere_physgrid-kgenref_var%area_sphere_physgrid)**2 
                  buf2_area_sphere_physgrid = buf1_area_sphere_physgrid 
              END WHERE   
              nrmsdiff_area_sphere_physgrid = SQRT(SUM(buf1_area_sphere_physgrid)/REAL(n_area_sphere_physgrid)) 
              rmsdiff_area_sphere_physgrid = SQRT(SUM(buf2_area_sphere_physgrid)/REAL(n_area_sphere_physgrid)) 
              IF (nrmsdiff_area_sphere_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%area_sphere_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%area_sphere_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%area_sphere_physgrid /= kgenref_var%area_sphere_physgrid), " of ", size( &
                  &var%area_sphere_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%area_sphere_physgrid)/real(size(var%area_sphere_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%area_sphere_physgrid)/real(size(kgenref_var%area_sphere_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_area_sphere_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_area_sphere_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%area_sphere_physgrid /= kgenref_var%area_sphere_physgrid), " of ", size( &
                  &var%area_sphere_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%area_sphere_physgrid)/real(size(var%area_sphere_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%area_sphere_physgrid)/real(size(kgenref_var%area_sphere_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_area_sphere_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_area_sphere_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jx_min_physgrid == kgenref_var%jx_min_physgrid)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jx_min_physgrid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jx_min_physgrid(SIZE(var%jx_min_physgrid,dim=1))) 
          ALLOCATE (buf2_jx_min_physgrid(SIZE(var%jx_min_physgrid,dim=1))) 
          n_jx_min_physgrid = COUNT(var%jx_min_physgrid /= kgenref_var%jx_min_physgrid) 
          WHERE ( ABS(kgenref_var%jx_min_physgrid) > kgen_minvalue ) 
              buf1_jx_min_physgrid = ((var%jx_min_physgrid-kgenref_var%jx_min_physgrid)/kgenref_var%jx_min_physgrid)**2 
              buf2_jx_min_physgrid = (var%jx_min_physgrid-kgenref_var%jx_min_physgrid)**2 
          ELSEWHERE 
              buf1_jx_min_physgrid = (var%jx_min_physgrid-kgenref_var%jx_min_physgrid)**2 
              buf2_jx_min_physgrid = buf1_jx_min_physgrid 
          END WHERE   
          nrmsdiff_jx_min_physgrid = SQRT(SUM(buf1_jx_min_physgrid)/REAL(n_jx_min_physgrid)) 
          rmsdiff_jx_min_physgrid = SQRT(SUM(buf2_jx_min_physgrid)/REAL(n_jx_min_physgrid)) 
          IF (nrmsdiff_jx_min_physgrid > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_min_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_min_physgrid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_min_physgrid /= kgenref_var%jx_min_physgrid), " of ", size( var%jx_min_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_min_physgrid)/real(size(var%jx_min_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_min_physgrid)/real(size(kgenref_var%jx_min_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_min_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_min_physgrid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_min_physgrid /= kgenref_var%jx_min_physgrid), " of ", size( var%jx_min_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_min_physgrid)/real(size(var%jx_min_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_min_physgrid)/real(size(kgenref_var%jx_min_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_min_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_min_physgrid 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jx_max_physgrid == kgenref_var%jx_max_physgrid)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jx_max_physgrid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jx_max_physgrid(SIZE(var%jx_max_physgrid,dim=1))) 
          ALLOCATE (buf2_jx_max_physgrid(SIZE(var%jx_max_physgrid,dim=1))) 
          n_jx_max_physgrid = COUNT(var%jx_max_physgrid /= kgenref_var%jx_max_physgrid) 
          WHERE ( ABS(kgenref_var%jx_max_physgrid) > kgen_minvalue ) 
              buf1_jx_max_physgrid = ((var%jx_max_physgrid-kgenref_var%jx_max_physgrid)/kgenref_var%jx_max_physgrid)**2 
              buf2_jx_max_physgrid = (var%jx_max_physgrid-kgenref_var%jx_max_physgrid)**2 
          ELSEWHERE 
              buf1_jx_max_physgrid = (var%jx_max_physgrid-kgenref_var%jx_max_physgrid)**2 
              buf2_jx_max_physgrid = buf1_jx_max_physgrid 
          END WHERE   
          nrmsdiff_jx_max_physgrid = SQRT(SUM(buf1_jx_max_physgrid)/REAL(n_jx_max_physgrid)) 
          rmsdiff_jx_max_physgrid = SQRT(SUM(buf2_jx_max_physgrid)/REAL(n_jx_max_physgrid)) 
          IF (nrmsdiff_jx_max_physgrid > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_max_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jx_max_physgrid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_max_physgrid /= kgenref_var%jx_max_physgrid), " of ", size( var%jx_max_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_max_physgrid)/real(size(var%jx_max_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_max_physgrid)/real(size(kgenref_var%jx_max_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_max_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_max_physgrid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jx_max_physgrid /= kgenref_var%jx_max_physgrid), " of ", size( var%jx_max_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jx_max_physgrid)/real(size(var%jx_max_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jx_max_physgrid)/real(size(kgenref_var%jx_max_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jx_max_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jx_max_physgrid 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jy_min_physgrid == kgenref_var%jy_min_physgrid)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jy_min_physgrid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jy_min_physgrid(SIZE(var%jy_min_physgrid,dim=1))) 
          ALLOCATE (buf2_jy_min_physgrid(SIZE(var%jy_min_physgrid,dim=1))) 
          n_jy_min_physgrid = COUNT(var%jy_min_physgrid /= kgenref_var%jy_min_physgrid) 
          WHERE ( ABS(kgenref_var%jy_min_physgrid) > kgen_minvalue ) 
              buf1_jy_min_physgrid = ((var%jy_min_physgrid-kgenref_var%jy_min_physgrid)/kgenref_var%jy_min_physgrid)**2 
              buf2_jy_min_physgrid = (var%jy_min_physgrid-kgenref_var%jy_min_physgrid)**2 
          ELSEWHERE 
              buf1_jy_min_physgrid = (var%jy_min_physgrid-kgenref_var%jy_min_physgrid)**2 
              buf2_jy_min_physgrid = buf1_jy_min_physgrid 
          END WHERE   
          nrmsdiff_jy_min_physgrid = SQRT(SUM(buf1_jy_min_physgrid)/REAL(n_jy_min_physgrid)) 
          rmsdiff_jy_min_physgrid = SQRT(SUM(buf2_jy_min_physgrid)/REAL(n_jy_min_physgrid)) 
          IF (nrmsdiff_jy_min_physgrid > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_min_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_min_physgrid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_min_physgrid /= kgenref_var%jy_min_physgrid), " of ", size( var%jy_min_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_min_physgrid)/real(size(var%jy_min_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_min_physgrid)/real(size(kgenref_var%jy_min_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_min_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_min_physgrid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_min_physgrid /= kgenref_var%jy_min_physgrid), " of ", size( var%jy_min_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_min_physgrid)/real(size(var%jy_min_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_min_physgrid)/real(size(kgenref_var%jy_min_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_min_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_min_physgrid 
              WRITE (*, *) "" 
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (ALL(var%jy_max_physgrid == kgenref_var%jy_max_physgrid)) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%jy_max_physgrid is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          ALLOCATE (buf1_jy_max_physgrid(SIZE(var%jy_max_physgrid,dim=1))) 
          ALLOCATE (buf2_jy_max_physgrid(SIZE(var%jy_max_physgrid,dim=1))) 
          n_jy_max_physgrid = COUNT(var%jy_max_physgrid /= kgenref_var%jy_max_physgrid) 
          WHERE ( ABS(kgenref_var%jy_max_physgrid) > kgen_minvalue ) 
              buf1_jy_max_physgrid = ((var%jy_max_physgrid-kgenref_var%jy_max_physgrid)/kgenref_var%jy_max_physgrid)**2 
              buf2_jy_max_physgrid = (var%jy_max_physgrid-kgenref_var%jy_max_physgrid)**2 
          ELSEWHERE 
              buf1_jy_max_physgrid = (var%jy_max_physgrid-kgenref_var%jy_max_physgrid)**2 
              buf2_jy_max_physgrid = buf1_jy_max_physgrid 
          END WHERE   
          nrmsdiff_jy_max_physgrid = SQRT(SUM(buf1_jy_max_physgrid)/REAL(n_jy_max_physgrid)) 
          rmsdiff_jy_max_physgrid = SQRT(SUM(buf2_jy_max_physgrid)/REAL(n_jy_max_physgrid)) 
          IF (nrmsdiff_jy_max_physgrid > kgen_tolerance) THEN 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_max_physgrid is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          ELSE 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%jy_max_physgrid is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_max_physgrid /= kgenref_var%jy_max_physgrid), " of ", size( var%jy_max_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_max_physgrid)/real(size(var%jy_max_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_max_physgrid)/real(size(kgenref_var%jy_max_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_max_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_max_physgrid 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) count( var%jy_max_physgrid /= kgenref_var%jy_max_physgrid), " of ", size( var%jy_max_physgrid ), " &
              &elements are different." 
              WRITE (*, *) "Average - kernel ", sum(var%jy_max_physgrid)/real(size(var%jy_max_physgrid)) 
              WRITE (*, *) "Average - reference ", sum(kgenref_var%jy_max_physgrid)/real(size(kgenref_var%jy_max_physgrid)) 
              WRITE (*, *) "RMS of difference is ", rmsdiff_jy_max_physgrid 
              WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_jy_max_physgrid 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (ALLOCATED(var%ibase_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%ibase_physgrid == kgenref_var%ibase_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ibase_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_ibase_physgrid(SIZE(var%ibase_physgrid,dim=1),SIZE(var%ibase_physgrid,dim=2),SIZE(var%ibase_physgrid,dim=3)))&
              & 
              ALLOCATE &
              &(buf2_ibase_physgrid(SIZE(var%ibase_physgrid,dim=1),SIZE(var%ibase_physgrid,dim=2),SIZE(var%ibase_physgrid,dim=3)))&
              & 
              n_ibase_physgrid = COUNT(var%ibase_physgrid /= kgenref_var%ibase_physgrid) 
              WHERE ( ABS(kgenref_var%ibase_physgrid) > kgen_minvalue ) 
                  buf1_ibase_physgrid = ((var%ibase_physgrid-kgenref_var%ibase_physgrid)/kgenref_var%ibase_physgrid)**2 
                  buf2_ibase_physgrid = (var%ibase_physgrid-kgenref_var%ibase_physgrid)**2 
              ELSEWHERE 
                  buf1_ibase_physgrid = (var%ibase_physgrid-kgenref_var%ibase_physgrid)**2 
                  buf2_ibase_physgrid = buf1_ibase_physgrid 
              END WHERE   
              nrmsdiff_ibase_physgrid = SQRT(SUM(buf1_ibase_physgrid)/REAL(n_ibase_physgrid)) 
              rmsdiff_ibase_physgrid = SQRT(SUM(buf2_ibase_physgrid)/REAL(n_ibase_physgrid)) 
              IF (nrmsdiff_ibase_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ibase_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ibase_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%ibase_physgrid /= kgenref_var%ibase_physgrid), " of ", size( var%ibase_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ibase_physgrid)/real(size(var%ibase_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ibase_physgrid)/real(size(kgenref_var%ibase_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ibase_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ibase_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%ibase_physgrid /= kgenref_var%ibase_physgrid), " of ", size( var%ibase_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%ibase_physgrid)/real(size(var%ibase_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%ibase_physgrid)/real(size(kgenref_var%ibase_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_ibase_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_ibase_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%halo_interp_weight_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%halo_interp_weight_physgrid == kgenref_var%halo_interp_weight_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%halo_interp_weight_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_halo_interp_weight_physgrid(SIZE(var%halo_interp_weight_physgrid,dim=1),SIZE(var%halo_interp_weight_physgrid,&
              &dim=2),SIZE(var%halo_interp_weight_physgrid,dim=3),SIZE(var%halo_interp_weight_physgrid,dim=4))) 
              ALLOCATE &
              &(buf2_halo_interp_weight_physgrid(SIZE(var%halo_interp_weight_physgrid,dim=1),SIZE(var%halo_interp_weight_physgrid,&
              &dim=2),SIZE(var%halo_interp_weight_physgrid,dim=3),SIZE(var%halo_interp_weight_physgrid,dim=4))) 
              n_halo_interp_weight_physgrid = COUNT(var%halo_interp_weight_physgrid /= kgenref_var%halo_interp_weight_physgrid) 
              WHERE ( ABS(kgenref_var%halo_interp_weight_physgrid) > kgen_minvalue ) 
                  buf1_halo_interp_weight_physgrid = &
                  &((var%halo_interp_weight_physgrid-kgenref_var%halo_interp_weight_physgrid)/kgenref_var%halo_interp_weight_physg&
                  &rid)**2 
                  buf2_halo_interp_weight_physgrid = (var%halo_interp_weight_physgrid-kgenref_var%halo_interp_weight_physgrid)**2 
              ELSEWHERE 
                  buf1_halo_interp_weight_physgrid = (var%halo_interp_weight_physgrid-kgenref_var%halo_interp_weight_physgrid)**2 
                  buf2_halo_interp_weight_physgrid = buf1_halo_interp_weight_physgrid 
              END WHERE   
              nrmsdiff_halo_interp_weight_physgrid = &
              &SQRT(SUM(buf1_halo_interp_weight_physgrid)/REAL(n_halo_interp_weight_physgrid)) 
              rmsdiff_halo_interp_weight_physgrid = &
              &SQRT(SUM(buf2_halo_interp_weight_physgrid)/REAL(n_halo_interp_weight_physgrid)) 
              IF (nrmsdiff_halo_interp_weight_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%halo_interp_weight_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%halo_interp_weight_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%halo_interp_weight_physgrid /= kgenref_var%halo_interp_weight_physgrid), " of ", size( &
                  &var%halo_interp_weight_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", &
                  &sum(var%halo_interp_weight_physgrid)/real(size(var%halo_interp_weight_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%halo_interp_weight_physgrid)/real(size(kgenref_var%halo_interp_weight_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_halo_interp_weight_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_halo_interp_weight_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%halo_interp_weight_physgrid /= kgenref_var%halo_interp_weight_physgrid), " of ", size( &
                  &var%halo_interp_weight_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", &
                  &sum(var%halo_interp_weight_physgrid)/real(size(var%halo_interp_weight_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%halo_interp_weight_physgrid)/real(size(kgenref_var%halo_interp_weight_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_halo_interp_weight_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_halo_interp_weight_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%vertex_recons_weights_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%vertex_recons_weights_physgrid == kgenref_var%vertex_recons_weights_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%vertex_recons_weights_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_vertex_recons_weights_physgrid(SIZE(var%vertex_recons_weights_physgrid,dim=1),SIZE(var%vertex_recons_weights_&
              &physgrid,dim=2),SIZE(var%vertex_recons_weights_physgrid,dim=3),SIZE(var%vertex_recons_weights_physgrid,dim=4))) 
              ALLOCATE &
              &(buf2_vertex_recons_weights_physgrid(SIZE(var%vertex_recons_weights_physgrid,dim=1),SIZE(var%vertex_recons_weights_&
              &physgrid,dim=2),SIZE(var%vertex_recons_weights_physgrid,dim=3),SIZE(var%vertex_recons_weights_physgrid,dim=4))) 
              n_vertex_recons_weights_physgrid = COUNT(var%vertex_recons_weights_physgrid /= &
              &kgenref_var%vertex_recons_weights_physgrid) 
              WHERE ( ABS(kgenref_var%vertex_recons_weights_physgrid) > kgen_minvalue ) 
                  buf1_vertex_recons_weights_physgrid = &
                  &((var%vertex_recons_weights_physgrid-kgenref_var%vertex_recons_weights_physgrid)/kgenref_var%vertex_recons_weig&
                  &hts_physgrid)**2 
                  buf2_vertex_recons_weights_physgrid = &
                  &(var%vertex_recons_weights_physgrid-kgenref_var%vertex_recons_weights_physgrid)**2 
              ELSEWHERE 
                  buf1_vertex_recons_weights_physgrid = &
                  &(var%vertex_recons_weights_physgrid-kgenref_var%vertex_recons_weights_physgrid)**2 
                  buf2_vertex_recons_weights_physgrid = buf1_vertex_recons_weights_physgrid 
              END WHERE   
              nrmsdiff_vertex_recons_weights_physgrid = &
              &SQRT(SUM(buf1_vertex_recons_weights_physgrid)/REAL(n_vertex_recons_weights_physgrid)) 
              rmsdiff_vertex_recons_weights_physgrid = &
              &SQRT(SUM(buf2_vertex_recons_weights_physgrid)/REAL(n_vertex_recons_weights_physgrid)) 
              IF (nrmsdiff_vertex_recons_weights_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%vertex_recons_weights_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%vertex_recons_weights_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%vertex_recons_weights_physgrid /= kgenref_var%vertex_recons_weights_physgrid), " of ", &
                  &size( var%vertex_recons_weights_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", &
                  &sum(var%vertex_recons_weights_physgrid)/real(size(var%vertex_recons_weights_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%vertex_recons_weights_physgrid)/real(size(kgenref_var%vertex_recons_weights_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_vertex_recons_weights_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vertex_recons_weights_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%vertex_recons_weights_physgrid /= kgenref_var%vertex_recons_weights_physgrid), " of ", &
                  &size( var%vertex_recons_weights_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", &
                  &sum(var%vertex_recons_weights_physgrid)/real(size(var%vertex_recons_weights_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%vertex_recons_weights_physgrid)/real(size(kgenref_var%vertex_recons_weights_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_vertex_recons_weights_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_vertex_recons_weights_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%norm_elem_coord_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%norm_elem_coord_physgrid == kgenref_var%norm_elem_coord_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%norm_elem_coord_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_norm_elem_coord_physgrid(SIZE(var%norm_elem_coord_physgrid,dim=1),SIZE(var%norm_elem_coord_physgrid,dim=2),SI&
              &ZE(var%norm_elem_coord_physgrid,dim=3))) 
              ALLOCATE &
              &(buf2_norm_elem_coord_physgrid(SIZE(var%norm_elem_coord_physgrid,dim=1),SIZE(var%norm_elem_coord_physgrid,dim=2),SI&
              &ZE(var%norm_elem_coord_physgrid,dim=3))) 
              n_norm_elem_coord_physgrid = COUNT(var%norm_elem_coord_physgrid /= kgenref_var%norm_elem_coord_physgrid) 
              WHERE ( ABS(kgenref_var%norm_elem_coord_physgrid) > kgen_minvalue ) 
                  buf1_norm_elem_coord_physgrid = &
                  &((var%norm_elem_coord_physgrid-kgenref_var%norm_elem_coord_physgrid)/kgenref_var%norm_elem_coord_physgrid)**2 
                  buf2_norm_elem_coord_physgrid = (var%norm_elem_coord_physgrid-kgenref_var%norm_elem_coord_physgrid)**2 
              ELSEWHERE 
                  buf1_norm_elem_coord_physgrid = (var%norm_elem_coord_physgrid-kgenref_var%norm_elem_coord_physgrid)**2 
                  buf2_norm_elem_coord_physgrid = buf1_norm_elem_coord_physgrid 
              END WHERE   
              nrmsdiff_norm_elem_coord_physgrid = SQRT(SUM(buf1_norm_elem_coord_physgrid)/REAL(n_norm_elem_coord_physgrid)) 
              rmsdiff_norm_elem_coord_physgrid = SQRT(SUM(buf2_norm_elem_coord_physgrid)/REAL(n_norm_elem_coord_physgrid)) 
              IF (nrmsdiff_norm_elem_coord_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%norm_elem_coord_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%norm_elem_coord_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%norm_elem_coord_physgrid /= kgenref_var%norm_elem_coord_physgrid), " of ", size( &
                  &var%norm_elem_coord_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%norm_elem_coord_physgrid)/real(size(var%norm_elem_coord_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%norm_elem_coord_physgrid)/real(size(kgenref_var%norm_elem_coord_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_norm_elem_coord_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_norm_elem_coord_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%norm_elem_coord_physgrid /= kgenref_var%norm_elem_coord_physgrid), " of ", size( &
                  &var%norm_elem_coord_physgrid ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%norm_elem_coord_physgrid)/real(size(var%norm_elem_coord_physgrid)) 
                  WRITE (*, *) "Average - reference ", &
                  &sum(kgenref_var%norm_elem_coord_physgrid)/real(size(kgenref_var%norm_elem_coord_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_norm_elem_coord_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_norm_elem_coord_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%dinv_physgrid)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dinv_physgrid == kgenref_var%dinv_physgrid)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dinv_physgrid is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_dinv_physgrid(SIZE(var%dinv_physgrid,dim=1),SIZE(var%dinv_physgrid,dim=2),SIZE(var%dinv_physgrid,dim=3),SIZE(&
              &var%dinv_physgrid,dim=4))) 
              ALLOCATE &
              &(buf2_dinv_physgrid(SIZE(var%dinv_physgrid,dim=1),SIZE(var%dinv_physgrid,dim=2),SIZE(var%dinv_physgrid,dim=3),SIZE(&
              &var%dinv_physgrid,dim=4))) 
              n_dinv_physgrid = COUNT(var%dinv_physgrid /= kgenref_var%dinv_physgrid) 
              WHERE ( ABS(kgenref_var%dinv_physgrid) > kgen_minvalue ) 
                  buf1_dinv_physgrid = ((var%dinv_physgrid-kgenref_var%dinv_physgrid)/kgenref_var%dinv_physgrid)**2 
                  buf2_dinv_physgrid = (var%dinv_physgrid-kgenref_var%dinv_physgrid)**2 
              ELSEWHERE 
                  buf1_dinv_physgrid = (var%dinv_physgrid-kgenref_var%dinv_physgrid)**2 
                  buf2_dinv_physgrid = buf1_dinv_physgrid 
              END WHERE   
              nrmsdiff_dinv_physgrid = SQRT(SUM(buf1_dinv_physgrid)/REAL(n_dinv_physgrid)) 
              rmsdiff_dinv_physgrid = SQRT(SUM(buf2_dinv_physgrid)/REAL(n_dinv_physgrid)) 
              IF (nrmsdiff_dinv_physgrid > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dinv_physgrid is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dinv_physgrid is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%dinv_physgrid /= kgenref_var%dinv_physgrid), " of ", size( var%dinv_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%dinv_physgrid)/real(size(var%dinv_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%dinv_physgrid)/real(size(kgenref_var%dinv_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_dinv_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dinv_physgrid 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%dinv_physgrid /= kgenref_var%dinv_physgrid), " of ", size( var%dinv_physgrid ), " &
                  &elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%dinv_physgrid)/real(size(var%dinv_physgrid)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%dinv_physgrid)/real(size(kgenref_var%dinv_physgrid)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_dinv_physgrid 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dinv_physgrid 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%fc)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%fc == kgenref_var%fc)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fc is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_fc(SIZE(var%fc,dim=1),SIZE(var%fc,dim=2),SIZE(var%fc,dim=3),SIZE(var%fc,dim=4))) 
              ALLOCATE (buf2_fc(SIZE(var%fc,dim=1),SIZE(var%fc,dim=2),SIZE(var%fc,dim=3),SIZE(var%fc,dim=4))) 
              n_fc = COUNT(var%fc /= kgenref_var%fc) 
              WHERE ( ABS(kgenref_var%fc) > kgen_minvalue ) 
                  buf1_fc = ((var%fc-kgenref_var%fc)/kgenref_var%fc)**2 
                  buf2_fc = (var%fc-kgenref_var%fc)**2 
              ELSEWHERE 
                  buf1_fc = (var%fc-kgenref_var%fc)**2 
                  buf2_fc = buf1_fc 
              END WHERE   
              nrmsdiff_fc = SQRT(SUM(buf1_fc)/REAL(n_fc)) 
              rmsdiff_fc = SQRT(SUM(buf2_fc)/REAL(n_fc)) 
              IF (nrmsdiff_fc > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fc is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fc is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%fc /= kgenref_var%fc), " of ", size( var%fc ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fc)/real(size(var%fc)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fc)/real(size(kgenref_var%fc)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fc 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fc 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%fc /= kgenref_var%fc), " of ", size( var%fc ), " elements are different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fc)/real(size(var%fc)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fc)/real(size(kgenref_var%fc)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fc 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fc 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%fc_phys)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%fc_phys == kgenref_var%fc_phys)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%fc_phys is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE &
              &(buf1_fc_phys(SIZE(var%fc_phys,dim=1),SIZE(var%fc_phys,dim=2),SIZE(var%fc_phys,dim=3),SIZE(var%fc_phys,dim=4))) 
              ALLOCATE &
              &(buf2_fc_phys(SIZE(var%fc_phys,dim=1),SIZE(var%fc_phys,dim=2),SIZE(var%fc_phys,dim=3),SIZE(var%fc_phys,dim=4))) 
              n_fc_phys = COUNT(var%fc_phys /= kgenref_var%fc_phys) 
              WHERE ( ABS(kgenref_var%fc_phys) > kgen_minvalue ) 
                  buf1_fc_phys = ((var%fc_phys-kgenref_var%fc_phys)/kgenref_var%fc_phys)**2 
                  buf2_fc_phys = (var%fc_phys-kgenref_var%fc_phys)**2 
              ELSEWHERE 
                  buf1_fc_phys = (var%fc_phys-kgenref_var%fc_phys)**2 
                  buf2_fc_phys = buf1_fc_phys 
              END WHERE   
              nrmsdiff_fc_phys = SQRT(SUM(buf1_fc_phys)/REAL(n_fc_phys)) 
              rmsdiff_fc_phys = SQRT(SUM(buf2_fc_phys)/REAL(n_fc_phys)) 
              IF (nrmsdiff_fc_phys > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fc_phys is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%fc_phys is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%fc_phys /= kgenref_var%fc_phys), " of ", size( var%fc_phys ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fc_phys)/real(size(var%fc_phys)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fc_phys)/real(size(kgenref_var%fc_phys)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fc_phys 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fc_phys 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%fc_phys /= kgenref_var%fc_phys), " of ", size( var%fc_phys ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%fc_phys)/real(size(var%fc_phys)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%fc_phys)/real(size(kgenref_var%fc_phys)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_fc_phys 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_fc_phys 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%ft)) THEN 
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
            
      END IF   
      IF (ALLOCATED(var%fm)) THEN 
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
            
      END IF   
      IF (ALLOCATED(var%dp_phys)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dp_phys == kgenref_var%dp_phys)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%dp_phys is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dp_phys(SIZE(var%dp_phys,dim=1),SIZE(var%dp_phys,dim=2),SIZE(var%dp_phys,dim=3))) 
              ALLOCATE (buf2_dp_phys(SIZE(var%dp_phys,dim=1),SIZE(var%dp_phys,dim=2),SIZE(var%dp_phys,dim=3))) 
              n_dp_phys = COUNT(var%dp_phys /= kgenref_var%dp_phys) 
              WHERE ( ABS(kgenref_var%dp_phys) > kgen_minvalue ) 
                  buf1_dp_phys = ((var%dp_phys-kgenref_var%dp_phys)/kgenref_var%dp_phys)**2 
                  buf2_dp_phys = (var%dp_phys-kgenref_var%dp_phys)**2 
              ELSEWHERE 
                  buf1_dp_phys = (var%dp_phys-kgenref_var%dp_phys)**2 
                  buf2_dp_phys = buf1_dp_phys 
              END WHERE   
              nrmsdiff_dp_phys = SQRT(SUM(buf1_dp_phys)/REAL(n_dp_phys)) 
              rmsdiff_dp_phys = SQRT(SUM(buf2_dp_phys)/REAL(n_dp_phys)) 
              IF (nrmsdiff_dp_phys > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dp_phys is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dp_phys is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%dp_phys /= kgenref_var%dp_phys), " of ", size( var%dp_phys ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%dp_phys)/real(size(var%dp_phys)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_phys)/real(size(kgenref_var%dp_phys)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_dp_phys 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_phys 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) count( var%dp_phys /= kgenref_var%dp_phys), " of ", size( var%dp_phys ), " elements are &
                  &different." 
                  WRITE (*, *) "Average - kernel ", sum(var%dp_phys)/real(size(var%dp_phys)) 
                  WRITE (*, *) "Average - reference ", sum(kgenref_var%dp_phys)/real(size(kgenref_var%dp_phys)) 
                  WRITE (*, *) "RMS of difference is ", rmsdiff_dp_phys 
                  WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dp_phys 
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
  END SUBROUTINE kv_fvm_control_volume_mod_fvm_struct 
    
  !verify state subroutine for kv_fvm_control_volume_mod_integer__ 
  RECURSIVE SUBROUTINE kv_fvm_control_volume_mod_integer__(varname, check_status, var, kgenref_var) 
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
          IF (diff <= kgen_tolerance) THEN 
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
        
  END SUBROUTINE kv_fvm_control_volume_mod_integer__ 
    
end module fvm_control_volume_mod
