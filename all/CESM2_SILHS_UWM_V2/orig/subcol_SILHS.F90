!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:29 
!KGEN version : 0.8.1 
  


module subcol_SILHS
   !---------------------------------------------------------------------------
   ! Purpose:
   ! Implement a subcolumn scheme based on the Subgrid Importance Latin Hypercube 
   ! Sampling (None) functionality of the CLUBB moist turbulence parameterization.
   !---------------------------------------------------------------------------
   !
   !

    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PRIVATE 
    SAVE 

    PUBLIC subcol_gen_silhs 
   ! Calc subcol mean ! Calc subcol variance
   !-----
   ! Private module vars
   !-----
   ! Pbuf indicies


   ! There may or may not be a better place to put this.

!   real(r8) :: subcol_SILHS_c6rt, subcol_SILHS_c7, subcol_SILHS_c8, subcol_SILHS_c11, &
!               subcol_SILHS_c11b, subcol_SILHS_gamma_coef, &
!               subcol_SILHS_mult_coef, subcol_SILHS_mu


#ifdef _MPI 
    include "mpif.h" 
#endif 
      

contains


   
SUBROUTINE subcol_gen_silhs(kgen_unit, kgen_measure, kgen_isverified, kgen_filepath) 
      !-------------------------------
      ! This is where the subcolumns are created, and the call to
      !      generate_silhs_sample_mod_api
      !    goes out. Variables needed to make this call are pulled from the 
      !    pbuf, from module data, and calculated based on the CAM state.
      !-------------------------------

    USE ppgrid, ONLY: pverp 
    USE ref_pres, ONLY: top_lev => trop_cloud_top_lev 

    USE clubb_api_module, ONLY: pdf_parameter, lscale, hydromet_pdf_parameter, pdf_dim 
   
    USE silhs_api_module, ONLY: generate_silhs_sample_api 
      ! CAM data structures
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE clubb_api_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
    USE clubb_api_module, ONLY: kr_kgen_hydromet_pdf_parameter_module_typesubp0 
    USE mt95, ONLY: kr_externs_out_mt95 
    USE generate_uniform_sample_module, ONLY: kr_externs_out_generate_uniform_sample_module 
    USE error_code, ONLY: kr_externs_out_error_code 
    USE output_2d_samples_module, ONLY: kr_externs_out_output_2d_samples_module 
    USE clubb_api_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
    USE clubb_api_module, ONLY: kv_kgen_hydromet_pdf_parameter_module_typesubp0 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_init_verify, kgen_tolerance, kgen_minvalue, kgen_verboselevel, &
    &CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
    USE mt95, ONLY: kv_externs_mt95 
    USE generate_uniform_sample_module, ONLY: kv_externs_generate_uniform_sample_module 
    USE output_2d_samples_module, ONLY: kv_externs_output_2d_samples_module 
      
      !----------------
      ! Local variables
      !----------------


      !----------------
      ! Required for set_up_pdf_params_incl_hydromet
      !----------------

    TYPE(hydromet_pdf_parameter), dimension(pverp-top_lev+1) :: hydromet_pdf_params 
    REAL(KIND=r8), dimension(:,:), allocatable :: mu_x_1, mu_x_2, sigma_x_1, sigma_x_2 
    REAL(KIND=r8), dimension(:,:,:), allocatable :: corr_cholesky_mtx_1, corr_cholesky_mtx_2 
      !----------------
      ! Input to generate_silhs_sample
      !----------------


    INTEGER :: iter 
    INTEGER :: num_subcols 
      integer, parameter :: sequence_length = 1  ! Number of timesteps btn subcol calls
      TYPE(pdf_parameter), dimension(pverp-top_lev+1) :: pdf_params 
      REAL(KIND=r8), dimension(pverp-top_lev+1) :: rho_ds_zt 
      REAL(KIND=r8), dimension(pverp-top_lev+1) :: delta_zm 
      REAL(KIND=r8), dimension(pverp-top_lev+1) :: rcm_in 
                                          !   weights at all vertical grid levels 
      LOGICAL :: l_calc_weights_all_levs_itime 
                                         !    grid levels at the current time step   
      !---------------
      !Output from generate_silhs_sample
      !--------------
      
      REAL(KIND=r8), allocatable, dimension(:,:,:) :: x_nl_all_levs_raw 
      REAL(KIND=r8), allocatable, dimension(:,:) :: lh_sample_point_weights 
      INTEGER, allocatable, dimension(:,:) :: x_mixt_comp_all_levs 

      ! real(r8), allocatable, dimension(:,:) :: RVM_lh_out    ! Vapor mixing ratio sent away
      !----------------
      ! Output from clip_transform_silhs_output_api
      !----------------


                                  ! Ncn_to_Nc might cause problems with the MG microphysics 
                                  ! since the changes made here (Nc-tendency) are not fed into 
                                  ! the microphysics
      !----------------
      ! Output to history
      !----------------
      ! V. Larson note: These variables are on the zt (full) levels: why do they
      ! have dimension pverp?  The pverp level corresponds to the CLUBB
      ! below-ground level.
      ! The variables in this paragraph are oriented like CAM variables (k=1 is
      ! the model top).
      ! They are flipped versions of CLUBB variables, for the entire chunk.
        


      !----------------
      ! Output from Est_Kessler_microphys
      !----------------

      !----------------
      ! Needed to update State
      !----------------

                                                       ! pcols for output to history
      !----------------
      ! Pointers
      !----------------
     
      INTEGER, INTENT(IN) :: kgen_unit 
      REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
      LOGICAL, INTENT(OUT) :: kgen_isverified 
      CHARACTER(LEN=*), INTENT(IN) :: kgen_filepath 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: kgen_intvar, kgen_ierr 
      INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
      LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      INTEGER, PARAMETER :: KGEN_MAXITER = 1 
        
      TYPE(check_t) :: check_status 
      INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
      REAL(KIND=kgen_dp) :: gkgen_measure 
      REAL(KIND=r8), allocatable, dimension(:,:,:) :: kgenref_x_nl_all_levs_raw 
      REAL(KIND=r8), allocatable, dimension(:,:) :: kgenref_lh_sample_point_weights 
      INTEGER, allocatable, dimension(:,:) :: kgenref_x_mixt_comp_all_levs 
        
      !parent block preprocessing 
        
#ifdef _MPI 
      call mpi_comm_rank(mpi_comm_world, kgen_mpirank, kgen_ierr) 
#else 
      kgen_mpirank = 0 
#endif 
        
        
      !local input variables 
      CALL kr_kgen_subcol_gen_silhs_subp0(hydromet_pdf_params, kgen_unit, "hydromet_pdf_params", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim2(mu_x_1, kgen_unit, "mu_x_1", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim2(mu_x_2, kgen_unit, "mu_x_2", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim2(sigma_x_1, kgen_unit, "sigma_x_1", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim2(sigma_x_2, kgen_unit, "sigma_x_2", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim3(corr_cholesky_mtx_1, kgen_unit, "corr_cholesky_mtx_1", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim3(corr_cholesky_mtx_2, kgen_unit, "corr_cholesky_mtx_2", .FALSE.) 
      READ (UNIT = kgen_unit) iter 
      READ (UNIT = kgen_unit) num_subcols 
      CALL kr_kgen_subcol_gen_silhs_subp1(pdf_params, kgen_unit, "pdf_params", .FALSE.) 
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rho_ds_zt 
          CALL kgen_array_sumcheck("rho_ds_zt", kgen_array_sum, DBLE(SUM(rho_ds_zt, mask=(rho_ds_zt .eq. rho_ds_zt))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) delta_zm 
          CALL kgen_array_sumcheck("delta_zm", kgen_array_sum, DBLE(SUM(delta_zm, mask=(delta_zm .eq. delta_zm))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) rcm_in 
          CALL kgen_array_sumcheck("rcm_in", kgen_array_sum, DBLE(SUM(rcm_in, mask=(rcm_in .eq. rcm_in))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) l_calc_weights_all_levs_itime 
      CALL kr_subcol_gen_silhs_real__r8_dim3(x_nl_all_levs_raw, kgen_unit, "x_nl_all_levs_raw", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim2(lh_sample_point_weights, kgen_unit, "lh_sample_point_weights", .FALSE.) 
      CALL kr_subcol_gen_silhs_integer___dim2(x_mixt_comp_all_levs, kgen_unit, "x_mixt_comp_all_levs", .FALSE.) 
        
      !extern output variables 
      CALL kr_externs_out_mt95(kgen_unit) 
      CALL kr_externs_out_generate_uniform_sample_module(kgen_unit) 
      CALL kr_externs_out_error_code(kgen_unit) 
      CALL kr_externs_out_output_2d_samples_module(kgen_unit) 
        
      !local output variables 
      CALL kr_subcol_gen_silhs_real__r8_dim3(kgenref_x_nl_all_levs_raw, kgen_unit, "kgenref_x_nl_all_levs_raw", .FALSE.) 
      CALL kr_subcol_gen_silhs_real__r8_dim2(kgenref_lh_sample_point_weights, kgen_unit, "kgenref_lh_sample_point_weights", &
      &.FALSE.) 
      CALL kr_subcol_gen_silhs_integer___dim2(kgenref_x_mixt_comp_all_levs, kgen_unit, "kgenref_x_mixt_comp_all_levs", .FALSE.) 


      ! Determine num of columns and which chunk we're working on and what timestep

                         !           substepping into account. I may need to change this in 
                         !           the future. Also, why does None need an iter, but CLUBB
                         !           does not?
                         ! #ERDBG:   The model iteration number is not used in None unless
                         !           sequence_length > 1, but nobody runs with that option.
      !----------------
      ! Establish associations between pointers and physics buffer fields
      !----------------
      !----------------
      ! Copy state and populate numbers and values of sub-columns
      !----------------

      !----------------
      ! Get indices for ice mass and number
      ! This is the same code from clubb_intr.F90
      !----------------

      ! The number of vertical grid levels used in CLUBB is pverp, which is originally
      ! set in the call to setup_clubb_core_api from subroutine clubb_ini_cam.  This
      ! is stored in CLUBB in the object gr%nz.  This isn't changed in CLUBB.
      ! However, when None is used, None only uses pverp - top_lev + 1 vertical grid
      ! levels and also uses the gr%nz object.  The value of gr%nz needs to be reset
      ! for None here and then set again for CLUBB in subroutine clubb_tend_cam.

      !----------------
      ! Loop over all the active grid columns in the chunk
      !----------------


      IF (kgen_evalstage) THEN 
      END IF   
      IF (kgen_warmupstage) THEN 
      END IF   
      IF (kgen_mainstage) THEN 
      END IF   
        
      !Uncomment following call statement to turn on perturbation experiment. 
      !Adjust perturbation value and/or kind parameter if required. 
      !CALL kgen_perturb_real( your_variable, 1.0E-15_8 ) 
        
        
      !call to kgen kernel 
         call generate_silhs_sample_api &
              ( iter, pdf_dim, num_subcols, sequence_length, pverp-top_lev+1, & ! In
                l_calc_weights_all_levs_itime, &                   ! In 
                pdf_params, delta_zm, rcm_in, Lscale(1:pverp-top_lev+1), &      ! In
                rho_ds_zt, mu_x_1, mu_x_2, sigma_x_1, sigma_x_2, & ! In 
                corr_cholesky_mtx_1, corr_cholesky_mtx_2, &        ! In
                hydromet_pdf_params, &                             ! In
                X_nl_all_levs_raw, X_mixt_comp_all_levs, &         ! Out
                lh_sample_point_weights)                           ! Out
         IF (kgen_mainstage) THEN 
               
             !verify init 
             CALL kgen_init_verify(tolerance=1.D-14, minvalue=1.D-14, verboseLevel=1) 
             CALL kgen_init_check(check_status, rank=kgen_mpirank) 
               
             !extern verify variables 
             CALL kv_externs_mt95(check_status) 
             CALL kv_externs_generate_uniform_sample_module(check_status) 
             CALL kv_externs_output_2d_samples_module(check_status) 
               
             !local verify variables 
             CALL kv_subcol_gen_silhs_real__r8_dim3("x_nl_all_levs_raw", check_status, x_nl_all_levs_raw, &
             &kgenref_x_nl_all_levs_raw) 
             CALL kv_subcol_gen_silhs_real__r8_dim2("lh_sample_point_weights", check_status, lh_sample_point_weights, &
             &kgenref_lh_sample_point_weights) 
             CALL kv_subcol_gen_silhs_integer___dim2("x_mixt_comp_all_levs", check_status, x_mixt_comp_all_levs, &
             &kgenref_x_mixt_comp_all_levs) 
             IF (check_status%rank == 0) THEN 
                 WRITE (*, *) "" 
             END IF   
             IF (kgen_verboseLevel > 0) THEN 
                 IF (check_status%rank == 0) THEN 
                     WRITE (*, *) "Number of output variables: ", check_status%numTotal 
                     WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
                     WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
                     WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
                     WRITE (*, *) "Tolerance: ", kgen_tolerance 
                 END IF   
             END IF   
             IF (check_status%rank == 0) THEN 
                 WRITE (*, *) "" 
             END IF   
             IF (check_status%numOutTol > 0) THEN 
                 IF (check_status%rank == 0) THEN 
                     WRITE (*, *) "Verification FAILED with" // TRIM(ADJUSTL(kgen_filepath)) 
                 END IF   
                 check_status%Passed = .FALSE. 
                 kgen_isverified = .FALSE. 
             ELSE 
                 IF (check_status%rank == 0) THEN 
                     WRITE (*, *) "Verification PASSED with " // TRIM(ADJUSTL(kgen_filepath)) 
                 END IF   
                 check_status%Passed = .TRUE. 
                 kgen_isverified = .TRUE. 
             END IF   
             IF (check_status%rank == 0) THEN 
                 WRITE (*, *) "" 
             END IF   
               
#ifdef _MPI 
             call mpi_barrier(mpi_comm_world, kgen_ierr) 
#endif 
               
             CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
             DO kgen_intvar = 1, KGEN_MAXITER 
         call generate_silhs_sample_api &
              ( iter, pdf_dim, num_subcols, sequence_length, pverp-top_lev+1, & ! In
                l_calc_weights_all_levs_itime, &                   ! In 
                pdf_params, delta_zm, rcm_in, Lscale(1:pverp-top_lev+1), &      ! In
                rho_ds_zt, mu_x_1, mu_x_2, sigma_x_1, sigma_x_2, & ! In 
                corr_cholesky_mtx_1, corr_cholesky_mtx_2, &        ! In
                hydromet_pdf_params, &                             ! In
                X_nl_all_levs_raw, X_mixt_comp_all_levs, &         ! Out
                lh_sample_point_weights)                           ! Out
             END DO   
             CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
             kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock*KGEN_MAXITER) 
#ifdef _MPI 
             CALL mpi_allreduce(kgen_measure, gkgen_measure, 1, mpi_real8, mpi_max, mpi_comm_world, kgen_ierr) 
             kgen_measure = gkgen_measure 
#endif 
             IF (check_status%rank==0) THEN 
                 WRITE (*, *) "gensilhs : Time per call (usec): ", kgen_measure 
             END IF   
         END IF   
         IF (kgen_warmupstage) THEN 
         END IF   
         IF (kgen_evalstage) THEN 
         END IF   


           
         CONTAINS 
           

         !read state subroutine for kr_kgen_subcol_gen_silhs_subp0 
         SUBROUTINE kr_kgen_subcol_gen_silhs_subp0(var, kgen_unit, printname, printvar) 
             TYPE(hydromet_pdf_parameter), INTENT(INOUT), DIMENSION(:) :: var 
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
                         CALL kr_kgen_hydromet_pdf_parameter_module_typesubp0(var(idx1), kgen_unit, printname // "(idx1)", &
                         &.TRUE.) 
                     ELSE 
                         CALL kr_kgen_hydromet_pdf_parameter_module_typesubp0(var(idx1), kgen_unit, printname // "(idx1)", &
                         &.FALSE.) 
                     END IF   
                 END DO   
             END IF   
         END SUBROUTINE kr_kgen_subcol_gen_silhs_subp0 
           
         !read state subroutine for kr_subcol_gen_silhs_real__r8_dim2 
         SUBROUTINE kr_subcol_gen_silhs_real__r8_dim2(var, kgen_unit, printname, printvar) 
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
         END SUBROUTINE kr_subcol_gen_silhs_real__r8_dim2 
           
         !read state subroutine for kr_subcol_gen_silhs_real__r8_dim3 
         SUBROUTINE kr_subcol_gen_silhs_real__r8_dim3(var, kgen_unit, printname, printvar) 
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
                 ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), &
                 &kgen_bound(1,3):kgen_bound(2,3))) 
                 READ (UNIT = kgen_unit) var 
                 CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
                 IF (PRESENT( printvar ) .AND. printvar) THEN 
                     WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
                 END IF   
             END IF   
         END SUBROUTINE kr_subcol_gen_silhs_real__r8_dim3 
           
         !read state subroutine for kr_kgen_subcol_gen_silhs_subp1 
         SUBROUTINE kr_kgen_subcol_gen_silhs_subp1(var, kgen_unit, printname, printvar) 
             TYPE(pdf_parameter), INTENT(INOUT), DIMENSION(:) :: var 
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
                         CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
                     ELSE 
                         CALL kr_pdf_parameter_module_pdf_parameter(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
                     END IF   
                 END DO   
             END IF   
         END SUBROUTINE kr_kgen_subcol_gen_silhs_subp1 
           
         !read state subroutine for kr_subcol_gen_silhs_integer___dim2 
         SUBROUTINE kr_subcol_gen_silhs_integer___dim2(var, kgen_unit, printname, printvar) 
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
         END SUBROUTINE kr_subcol_gen_silhs_integer___dim2 
           
         !verify state subroutine for kv_subcol_gen_silhs_real__r8_dim3 
         RECURSIVE SUBROUTINE kv_subcol_gen_silhs_real__r8_dim3(varname, check_status, var, kgenref_var) 
             CHARACTER(LEN=*), INTENT(IN) :: varname 
             TYPE(check_t), INTENT(INOUT) :: check_status 
             REAL(KIND=r8), allocatable, INTENT(IN), DIMENSION(:,:,:) :: var, kgenref_var 
             INTEGER :: check_result 
             LOGICAL :: is_print = .FALSE. 
               
             INTEGER :: idx1, idx2, idx3 
             INTEGER :: n 
             real(KIND=r8) :: nrmsdiff, rmsdiff 
             real(KIND=r8), ALLOCATABLE :: buf1(:,:,:), buf2(:,:,:) 
               
             IF (ALLOCATED(var)) THEN 
                 check_status%numTotal = check_status%numTotal + 1 
                   
                 IF (ALL(var == kgenref_var)) THEN 
                     check_status%numIdentical = check_status%numIdentical + 1 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                         END IF   
                     END IF   
                     check_result = CHECK_IDENTICAL 
                 ELSE 
                     ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3))) 
                     ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3))) 
                     n = SIZE(var) 
                     WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                         buf1 = ((var-kgenref_var)/kgenref_var)**2 
                         buf2 = (var-kgenref_var)**2 
                     ELSEWHERE 
                         buf1 = (var-kgenref_var)**2 
                         buf2 = buf1 
                     END WHERE   
                     nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
                     rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
                     IF (rmsdiff > kgen_tolerance) THEN 
                         check_status%numOutTol = check_status%numOutTol + 1 
                         IF (kgen_verboseLevel > 0) THEN 
                             IF (check_status%rank == 0) THEN 
                                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                             END IF   
                         END IF   
                         check_result = CHECK_OUT_TOL 
                     ELSE 
                         check_status%numInTol = check_status%numInTol + 1 
                         IF (kgen_verboseLevel > 1) THEN 
                             IF (check_status%rank == 0) THEN 
                                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                             END IF   
                         END IF   
                         check_result = CHECK_IN_TOL 
                     END IF   
                 END IF   
                 IF (check_result == CHECK_IDENTICAL) THEN 
                     IF (kgen_verboseLevel > 2) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", 0 
                             WRITE (*, *) "Normalized RMS of difference is ", 0 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                     IF (kgen_verboseLevel > 0) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", rmsdiff 
                             WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 ELSE IF (check_result == CHECK_IN_TOL) THEN 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", rmsdiff 
                             WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 END IF   
                   
             END IF   
         END SUBROUTINE kv_subcol_gen_silhs_real__r8_dim3 
           
         !verify state subroutine for kv_subcol_gen_silhs_real__r8_dim2 
         RECURSIVE SUBROUTINE kv_subcol_gen_silhs_real__r8_dim2(varname, check_status, var, kgenref_var) 
             CHARACTER(LEN=*), INTENT(IN) :: varname 
             TYPE(check_t), INTENT(INOUT) :: check_status 
             REAL(KIND=r8), allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
             INTEGER :: check_result 
             LOGICAL :: is_print = .FALSE. 
               
             INTEGER :: idx1, idx2 
             INTEGER :: n 
             real(KIND=r8) :: nrmsdiff, rmsdiff 
             real(KIND=r8), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
               
             IF (ALLOCATED(var)) THEN 
                 check_status%numTotal = check_status%numTotal + 1 
                   
                 IF (ALL(var == kgenref_var)) THEN 
                     check_status%numIdentical = check_status%numIdentical + 1 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                         END IF   
                     END IF   
                     check_result = CHECK_IDENTICAL 
                 ELSE 
                     ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                     ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                     n = SIZE(var) 
                     WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                         buf1 = ((var-kgenref_var)/kgenref_var)**2 
                         buf2 = (var-kgenref_var)**2 
                     ELSEWHERE 
                         buf1 = (var-kgenref_var)**2 
                         buf2 = buf1 
                     END WHERE   
                     nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
                     rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
                     IF (rmsdiff > kgen_tolerance) THEN 
                         check_status%numOutTol = check_status%numOutTol + 1 
                         IF (kgen_verboseLevel > 0) THEN 
                             IF (check_status%rank == 0) THEN 
                                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                             END IF   
                         END IF   
                         check_result = CHECK_OUT_TOL 
                     ELSE 
                         check_status%numInTol = check_status%numInTol + 1 
                         IF (kgen_verboseLevel > 1) THEN 
                             IF (check_status%rank == 0) THEN 
                                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                             END IF   
                         END IF   
                         check_result = CHECK_IN_TOL 
                     END IF   
                 END IF   
                 IF (check_result == CHECK_IDENTICAL) THEN 
                     IF (kgen_verboseLevel > 2) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", 0 
                             WRITE (*, *) "Normalized RMS of difference is ", 0 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                     IF (kgen_verboseLevel > 0) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", rmsdiff 
                             WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 ELSE IF (check_result == CHECK_IN_TOL) THEN 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", rmsdiff 
                             WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 END IF   
                   
             END IF   
         END SUBROUTINE kv_subcol_gen_silhs_real__r8_dim2 
           
         !verify state subroutine for kv_subcol_gen_silhs_integer___dim2 
         RECURSIVE SUBROUTINE kv_subcol_gen_silhs_integer___dim2(varname, check_status, var, kgenref_var) 
             CHARACTER(LEN=*), INTENT(IN) :: varname 
             TYPE(check_t), INTENT(INOUT) :: check_status 
             INTEGER, allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
             INTEGER :: check_result 
             LOGICAL :: is_print = .FALSE. 
               
             INTEGER :: idx1, idx2 
             INTEGER :: n 
             integer :: nrmsdiff, rmsdiff 
             integer, ALLOCATABLE :: buf1(:,:), buf2(:,:) 
               
             IF (ALLOCATED(var)) THEN 
                 check_status%numTotal = check_status%numTotal + 1 
                   
                 IF (ALL(var == kgenref_var)) THEN 
                     check_status%numIdentical = check_status%numIdentical + 1 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                         END IF   
                     END IF   
                     check_result = CHECK_IDENTICAL 
                 ELSE 
                     ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                     ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                     n = SIZE(var) 
                     WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                         buf1 = ((var-kgenref_var)/kgenref_var)**2 
                         buf2 = (var-kgenref_var)**2 
                     ELSEWHERE 
                         buf1 = (var-kgenref_var)**2 
                         buf2 = buf1 
                     END WHERE   
                     nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
                     rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
                     IF (rmsdiff > kgen_tolerance) THEN 
                         check_status%numOutTol = check_status%numOutTol + 1 
                         IF (kgen_verboseLevel > 0) THEN 
                             IF (check_status%rank == 0) THEN 
                                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                             END IF   
                         END IF   
                         check_result = CHECK_OUT_TOL 
                     ELSE 
                         check_status%numInTol = check_status%numInTol + 1 
                         IF (kgen_verboseLevel > 1) THEN 
                             IF (check_status%rank == 0) THEN 
                                 WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                             END IF   
                         END IF   
                         check_result = CHECK_IN_TOL 
                     END IF   
                 END IF   
                 IF (check_result == CHECK_IDENTICAL) THEN 
                     IF (kgen_verboseLevel > 2) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", 0 
                             WRITE (*, *) "Normalized RMS of difference is ", 0 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                     IF (kgen_verboseLevel > 0) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", rmsdiff 
                             WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 ELSE IF (check_result == CHECK_IN_TOL) THEN 
                     IF (kgen_verboseLevel > 1) THEN 
                         IF (check_status%rank == 0) THEN 
                             WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                             WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                             WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                             WRITE (*, *) "RMS of difference is ", rmsdiff 
                             WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                             WRITE (*, *) "" 
                         END IF   
                     END IF   
                 END IF   
                   
             END IF   
         END SUBROUTINE kv_subcol_gen_silhs_integer___dim2 
           
END SUBROUTINE subcol_gen_silhs 


   ! =============================================================================== !
   !                                                                                 !
   ! =============================================================================== !


   ! =============================================================================== !
   !                                                                                 !
   ! =============================================================================== !
   !============================================================================


   !============================================================================


   !============================================================================


   !============================================================================


   !============================================================================

   
end module subcol_SILHS 
