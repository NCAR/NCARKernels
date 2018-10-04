!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  

module EXTRAE_MODULE
    interface
        subroutine extrae_next_hwc_set
        end subroutine extrae_next_hwc_set
    end interface
end module EXTRAE_MODULE

module fvm_consistent_se_cslam
    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
    USE dimensions_mod, ONLY: nc, nhe, nlev, ntrac, nhr, nhc, ngpc, ns, nht 
    USE dimensions_mod, ONLY: irecons_tracer 
    USE cam_abortutils, ONLY: endrun 

    USE element_mod, ONLY: element_t 
    USE fvm_control_volume_mod, ONLY: fvm_struct 
    !USE perf_mod, ONLY: t_startf, t_stopf 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    USE element_mod, ONLY: kr_element_mod_element_t 
    USE fvm_control_volume_mod, ONLY: kr_fvm_control_volume_mod_fvm_struct 
    USE element_mod, ONLY: kv_element_mod_element_t 
    USE fvm_control_volume_mod, ONLY: kv_fvm_control_volume_mod_fvm_struct 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

  real (kind=r8), dimension(ngpc), private :: gsweights, gspts
  real (kind=r8),parameter       , private :: eps=1.0e-14_r8
  PUBLIC run_consistent_se_cslam 
  PUBLIC kr_externs_in_fvm_consistent_se_cslam 
  PUBLIC kr_externs_out_fvm_consistent_se_cslam 
  PUBLIC kr_element_mod_element_t 
  PUBLIC kr_fvm_control_volume_mod_fvm_struct 
  PUBLIC kv_element_mod_element_t 
  PUBLIC kv_fvm_control_volume_mod_fvm_struct 
contains
  !**************************************************************************************
  ! Consistent CSLAM-SE algorithm documented in
  ! Lauritzen et al. (2017): -SE-CSLAM: Consistent finite-volume transport with
  !                          spectral-element dynamics. Mon. Wea. Rev.
  !**************************************************************************************
  !
  !
  !
  !
  !
  !
SUBROUTINE run_consistent_se_cslam(kgen_unit, kgen_measure, kgen_isverified, elem, fvm, nets, nete) 
    ! ---------------------------------------------------------------------------------
    USE fvm_control_volume_mod, ONLY: n0_fvm, np1_fvm 
    USE fvm_reconstruction_mod, ONLY: reconstruction 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE fvm_control_volume_mod, ONLY: kr_externs_out_fvm_control_volume_mod 
    USE dimensions_mod, ONLY: kr_externs_out_dimensions_mod 
    USE perf_mod, ONLY: kr_externs_out_perf_mod 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 
    USE fvm_control_volume_mod, ONLY: kv_externs_fvm_control_volume_mod 
    USE dimensions_mod, ONLY: kv_externs_dimensions_mod 
    IMPLICIT NONE 
    TYPE(element_t), INTENT(INOUT) :: elem(:) 
    TYPE(fvm_struct), INTENT(INOUT) :: fvm(:) 
    INTEGER, INTENT(INOUT) :: nets 
    INTEGER, INTENT(INOUT) :: nete 
    !high-order air density reconstruction


    REAL(KIND=r8) :: ctracer(irecons_tracer,1-nhe:nc+nhe,1-nhe:nc+nhe,ntrac)
    real(KIND=r8) :: recons_weights(4,irecons_tracer-1,1-nhe:nc+nhe,1-nhe:nc+nhe)
   

    LOGICAL :: llimiter(ntrac) 
    INTEGER :: k, ie 
    INTEGER, INTENT(IN) :: kgen_unit 
    REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
    LOGICAL, INTENT(OUT) :: kgen_isverified 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
    LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
    COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
      
    TYPE(check_t) :: check_status 
    INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
    TYPE(fvm_struct), dimension(:), allocatable :: kgenref_fvm 
    REAL(KIND=r8), dimension(irecons_tracer,1-nhe:nc+nhe,1-nhe:nc+nhe,ntrac) :: kgenref_ctracer 
    INTEGER :: kgenref_ie 
    INTEGER :: kgenref_k 
    integer :: ir
      
    !local input variables 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) ctracer 
        CALL kgen_array_sumcheck("ctracer", kgen_array_sum, DBLE(SUM(ctracer, mask=(ctracer .eq. ctracer))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) llimiter 
    END IF   
    READ (UNIT = kgen_unit) ie 
    READ (UNIT = kgen_unit) k 
      
    !extern output variables 
    CALL kr_externs_out_fvm_consistent_se_cslam(kgen_unit) 
    CALL kr_externs_out_fvm_control_volume_mod(kgen_unit) 
    CALL kr_externs_out_dimensions_mod(kgen_unit) 
    CALL kr_externs_out_perf_mod(kgen_unit) 
      
    !local output variables 
    CALL kr_kgen_run_consistent_se_cslam_subp1(kgenref_fvm, kgen_unit, "kgenref_fvm", .FALSE.) 
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) kgenref_ctracer 
        CALL kgen_array_sumcheck("kgenref_ctracer", kgen_array_sum, DBLE(SUM(kgenref_ctracer, mask=(kgenref_ctracer .eq. &
        &kgenref_ctracer))), .TRUE.) 
    END IF   
    READ (UNIT = kgen_unit) kgenref_ie 
    READ (UNIT = kgen_unit) kgenref_k 


!$kgen begin_callsite cslam

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

    do ie=nets,nete
      do ir=1,irecons_tracer-1
         recons_weights(1,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,1,1-nhe:nc+nhe,1-nhe:nc+nhe)
         recons_weights(2,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,2,1-nhe:nc+nhe,1-nhe:nc+nhe)
         recons_weights(3,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,3,1-nhe:nc+nhe,1-nhe:nc+nhe)
         recons_weights(4,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,4,1-nhe:nc+nhe,1-nhe:nc+nhe)
      enddo
      fvm(ie)%c(:,:,:,:,np1_fvm)    = 0.0_r8!to avoid problems when uninitialized variables are set to NaN
      fvm(ie)%dp_fvm(:,:,:,np1_fvm) = 0.0_r8!to avoid problems when uninitialized variables are set to NaN      
      do k=1,nlev
        !call t_startf('fvm:tracers_reconstruct')
        !!YSK call reconstruction(fvm(ie)%c(1-nhc:nc+nhc,1-nhc:nc+nhc,k,1:ntrac,n0_fvm),&
        call reconstruction(fvm(ie)%c(:,:,:,:,n0_fvm),nlev, k,&
             ctracer(:,:,:,:),irecons_tracer,llimiter,ntrac,&
             nc,nhe,nhr,nhc,nht,ns,nhr+(nhe-1),&
             fvm(ie)%jx_min,fvm(ie)%jx_max,fvm(ie)%jy_min,fvm(ie)%jy_max,&
             fvm(ie)%cubeboundary,fvm(ie)%halo_interp_weight,fvm(ie)%ibase,&
             fvm(ie)%spherecentroid(:,1-nhe:nc+nhe,1-nhe:nc+nhe),&
             fvm(ie)%recons_metrics,fvm(ie)%recons_metrics_integral,&
             fvm(ie)%rot_matrix,fvm(ie)%centroid_stretch,&
             recons_weights,fvm(ie)%vtx_cart&
             )
        !call t_stopf('fvm:tracers_reconstruct')
        !call t_startf('fvm:swept_flux')
        call swept_flux(elem(ie),fvm(ie),k,ctracer)
        !call t_stopf('fvm:swept_flux')
        !call extrae_next_hwc_set    
      end do
    end do
    IF (kgen_mainstage) THEN 
          
        !verify init 
        CALL kgen_init_check(check_status, tolerance=1.D-14, verboseLevel=1) 
          
        !extern verify variables 
        CALL kv_externs_fvm_control_volume_mod(check_status) 
        CALL kv_externs_dimensions_mod(check_status) 
          
        !local verify variables 
        CALL kv_kgen_run_consistent_se_cslam_subp0("fvm", check_status, fvm, kgenref_fvm) 
        CALL kv_run_consistent_se_cslam_real__r8_dim4("ctracer", check_status, ctracer, kgenref_ctracer) 
        CALL kv_run_consistent_se_cslam_integer__("k", check_status, k, kgenref_k) 
        CALL kv_run_consistent_se_cslam_integer__("ie", check_status, ie, kgenref_ie) 
        WRITE (*, *) "" 
        IF (check_status%verboseLevel > 0) THEN 
            WRITE (*, *) "Number of output variables: ", check_status%numTotal 
            WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
            WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
            WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
            WRITE (*, *) "Tolerance: ", kgen_tolerance 
        END IF   
        WRITE (*, *) "" 
        IF (check_status%numOutTol > 0) THEN 
            WRITE (*, *) "Verification FAILED" 
            check_status%Passed = .FALSE. 
            kgen_isverified = .FALSE. 
        ELSE 
            WRITE (*, *) "Verification PASSED" 
            check_status%Passed = .TRUE. 
            kgen_isverified = .TRUE. 
        END IF   
        WRITE (*, *) "" 
        CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
    do ie=nets,nete
      do ir=1,irecons_tracer-1
         recons_weights(1,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,1,1-nhe:nc+nhe,1-nhe:nc+nhe)
         recons_weights(2,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,2,1-nhe:nc+nhe,1-nhe:nc+nhe)
         recons_weights(3,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,3,1-nhe:nc+nhe,1-nhe:nc+nhe)
         recons_weights(4,ir,1-nhe:nc+nhe,1-nhe:nc+nhe) = fvm(ie)%vertex_recons_weights(ir,4,1-nhe:nc+nhe,1-nhe:nc+nhe)
      enddo
      fvm(ie)%c(:,:,:,:,np1_fvm)    = 0.0_r8!to avoid problems when uninitialized variables are set to NaN
      fvm(ie)%dp_fvm(:,:,:,np1_fvm) = 0.0_r8!to avoid problems when uninitialized variables are set to NaN      
      do k=1,nlev
        !!!YSKcall reconstruction(fvm(ie)%c(1-nhc:nc+nhc,1-nhc:nc+nhc,k,1:ntrac,n0_fvm),&
             !!!YSK ctracer(:,:,:,:),irecons_tracer,llimiter,ntrac,&
        !call t_startf('fvm:tracers_reconstruct')
        call reconstruction(fvm(ie)%c(:,:,:,:,n0_fvm),nlev, k,&
             ctracer(:,:,:,:),irecons_tracer,llimiter,ntrac,& !YSK
             nc,nhe,nhr,nhc,nht,ns,nhr+(nhe-1),&
             fvm(ie)%jx_min,fvm(ie)%jx_max,fvm(ie)%jy_min,fvm(ie)%jy_max,&
             fvm(ie)%cubeboundary,fvm(ie)%halo_interp_weight,fvm(ie)%ibase,&
             fvm(ie)%spherecentroid(:,1-nhe:nc+nhe,1-nhe:nc+nhe),&
             fvm(ie)%recons_metrics,fvm(ie)%recons_metrics_integral,&
             fvm(ie)%rot_matrix,fvm(ie)%centroid_stretch,&
             recons_weights,fvm(ie)%vtx_cart&
             )
        !call t_stopf('fvm:tracers_reconstruct')
        !call t_startf('fvm:swept_flux')
        call swept_flux(elem(ie),fvm(ie),k,ctracer)
        !call t_stopf('fvm:swept_flux')
      end do
    end do
    CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
    kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock) 
    WRITE (*, *) "cslam : Time per call (usec): ", kgen_measure 
    END IF   
    IF (kgen_warmupstage) THEN 
    END IF   
    IF (kgen_evalstage) THEN 
    END IF   
!$kgen end_callsite
     !***************************************
     ! Large Courant number increment
     !***************************************
     ! In the jet region the effective Courant number
     ! in the cslam trajectory algorithm can be > 1
     ! (by up to 20%) in 
     ! We limit the trajectories to < 1 but in this step
     ! we do a piecewise constant update for the
     ! amount of mass for which the Courant number is >1

     !
     !
     !
     !
     !
     !
     !


    ! advance fvm time-levels
    !
    !
      
    CONTAINS 
      
    !read state subroutine for kr_kgen_run_consistent_se_cslam_subp1 
    SUBROUTINE kr_kgen_run_consistent_se_cslam_subp1(var, kgen_unit, printname, printvar) 
        TYPE(fvm_struct), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
            READ (UNIT = kgen_unit) kgen_bound(1, 1) 
            READ (UNIT = kgen_unit) kgen_bound(2, 1) 
            ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
            DO idx1=kgen_bound(1,1), kgen_bound(2,1) 
                IF (PRESENT( printvar ) .AND. printvar) THEN 
                    CALL kr_fvm_control_volume_mod_fvm_struct(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
                ELSE 
                    CALL kr_fvm_control_volume_mod_fvm_struct(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
                END IF   
            END DO   
        END IF   
    END SUBROUTINE kr_kgen_run_consistent_se_cslam_subp1 
      
    !verify state subroutine for kv_kgen_run_consistent_se_cslam_subp0 
    RECURSIVE SUBROUTINE kv_kgen_run_consistent_se_cslam_subp0(varname, check_status, var, kgenref_var) 
        CHARACTER(LEN=*), INTENT(IN) :: varname 
        TYPE(check_t), INTENT(INOUT) :: check_status 
        TYPE(check_t) :: comp_check_status 
        TYPE(fvm_struct), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
        INTEGER :: check_result 
        LOGICAL :: is_print = .FALSE. 
          
        INTEGER :: idx1 
          
        check_status%numTotal = check_status%numTotal + 1 
          
        CALL kgen_init_check(comp_check_status, verboseLevel=check_status%verboseLevel) 
        DO   idx1=LBOUND(var,1), UBOUND(var,1) 
            CALL kv_fvm_control_volume_mod_fvm_struct(trim(adjustl(varname)), comp_check_status, var(idx1), kgenref_var(idx1)) 
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
                WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                WRITE (*, *) "" 
            END IF   
        ELSE IF (check_result == CHECK_OUT_TOL) THEN 
            IF (check_status%verboseLevel > 0) THEN 
                WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                WRITE (*, *) "" 
            END IF   
        ELSE IF (check_result == CHECK_IN_TOL) THEN 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) "       number of elements                 : ", comp_check_status%numtotal 
                WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                WRITE (*, *) "" 
            END IF   
        END IF   
          
    END SUBROUTINE kv_kgen_run_consistent_se_cslam_subp0 
      
    !verify state subroutine for kv_run_consistent_se_cslam_real__r8_dim4 
    RECURSIVE SUBROUTINE kv_run_consistent_se_cslam_real__r8_dim4(varname, check_status, var, kgenref_var) 
        CHARACTER(LEN=*), INTENT(IN) :: varname 
        TYPE(check_t), INTENT(INOUT) :: check_status 
        REAL(KIND=r8), INTENT(IN), DIMENSION(:,:,:,:) :: var, kgenref_var 
        INTEGER :: check_result 
        LOGICAL :: is_print = .FALSE. 
          
        INTEGER :: idx1, idx2, idx3, idx4 
        INTEGER :: n 
        real(KIND=r8) :: nrmsdiff, rmsdiff 
        real(KIND=r8), ALLOCATABLE :: buf1(:,:,:,:), buf2(:,:,:,:) 
          
        check_status%numTotal = check_status%numTotal + 1 
          
        IF (ALL(var == kgenref_var)) THEN 
            check_status%numIdentical = check_status%numIdentical + 1 
            IF (check_status%verboseLevel > 1) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
            END IF   
            check_result = CHECK_IDENTICAL 
        ELSE 
            ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4))) 
            ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4))) 
            n = COUNT(var /= kgenref_var) 
            WHERE ( ABS(kgenref_var) > kgen_minvalue ) 
                buf1 = ((var-kgenref_var)/kgenref_var)**2 
                buf2 = (var-kgenref_var)**2 
            ELSEWHERE 
                buf1 = (var-kgenref_var)**2 
                buf2 = buf1 
            END WHERE   
            nrmsdiff = SQRT(SUM(buf1)/DBLE(n)) 
            rmsdiff = SQRT(SUM(buf2)/DBLE(n)) 
            IF (nrmsdiff > kgen_tolerance) THEN 
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
          
    END SUBROUTINE kv_run_consistent_se_cslam_real__r8_dim4 
      
    !verify state subroutine for kv_run_consistent_se_cslam_integer__ 
    RECURSIVE SUBROUTINE kv_run_consistent_se_cslam_integer__(varname, check_status, var, kgenref_var) 
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
          
    END SUBROUTINE kv_run_consistent_se_cslam_integer__ 
      
END SUBROUTINE run_consistent_se_cslam 

  subroutine swept_flux(elem,fvm,ilev,ctracer)
      USE fvm_control_volume_mod, ONLY: n0_fvm, np1_fvm 
      USE fvm_analytic_mod, ONLY: get_high_order_weights_over_areas 
      USE dimensions_mod, ONLY: kmin_jet, kmax_jet 
    implicit none
    type (element_t) , intent(in)   :: elem
    type (fvm_struct), intent(inout):: fvm
    integer          , intent(in) :: ilev
    real (kind=r8), intent(inout) :: ctracer(irecons_tracer,1-nhe:nc+nhe,1-nhe:nc+nhe,ntrac)

    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    real (kind=r8)    , dimension(0:7       , imin:imax,imin:imax,num_sides) :: displ
    integer (kind=r8) , dimension(1:2,11    , imin:imax,imin:imax,num_sides) :: base_vec
    real (kind=r8)    , dimension(1:2, 6    , imin:imax,imin:imax,num_sides) :: base_vtx
    integer                  , dimension(2,num_area, imin:imax,imin:imax,num_sides) :: idx
    real (kind=r8)    , dimension(imin:imax,imin:imax,num_sides)             :: mass_flux_se
    real (kind=r8)    , dimension(irecons_tracer,num_area) :: weights
    real (kind=r8)                     :: gamma
    integer :: i,j,iside,iarea,iw

    integer, parameter :: num_seg_max=5
    REAL(KIND=r8), dimension(2,num_seg_max,num_area) :: x, dx, x_static, dx_static
    integer             , dimension(num_area)               :: num_seg, num_seg_static
    REAL(KIND=r8), dimension(2,8) :: x_start, dgam_vec
    REAL(KIND=r8) :: gamma_max, displ_first_guess

    REAL(KIND=r8) :: flux,flux_tracer(ntrac)

    REAL(KIND=r8), dimension(num_area) :: dp_area

    logical :: tl1,tl2,tr1,tr2

    integer, dimension(4), parameter :: imin_side = (/1   ,0   ,1   ,1   /)
    integer, dimension(4), parameter :: imax_side = (/nc  ,nc  ,nc  ,nc+1/)
    integer, dimension(4), parameter :: jmin_side = (/1   ,1   ,0   ,1   /)
    integer, dimension(4), parameter :: jmax_side = (/nc+1,nc  ,nc  ,nc  /)

    integer :: iseg, iseg_tmp,flowcase,ii,jj,itr

    call define_swept_areas(fvm,ilev,displ,base_vec,base_vtx,idx)

    mass_flux_se(1:nc,1:nc,1:4)  = -elem%sub_elem_mass_flux(1:nc,1:nc,1:4,ilev)
    mass_flux_se(0   ,1:nc,2  )  =  elem%sub_elem_mass_flux(1   ,1:nc,4  ,ilev)
    mass_flux_se(nc+1,1:nc,4  )  =  elem%sub_elem_mass_flux(nc  ,1:nc,2  ,ilev)
    mass_flux_se(1:nc,0   ,3  )  =  elem%sub_elem_mass_flux(1:nc,1   ,1  ,ilev)
    mass_flux_se(1:nc,nc+1,1  )  =  elem%sub_elem_mass_flux(1:nc,nc  ,3  ,ilev)
    ! prepare for air/tracer update
    !
    !
    fvm%dp_fvm(1:nc,1:nc,ilev,np1_fvm) = fvm%dp_fvm(1:nc,1:nc,ilev,n0_fvm)*fvm%area_sphere
    do itr=1,ntrac
      fvm%c(1:nc,1:nc,ilev,itr,np1_fvm) = fvm%c(1:nc,1:nc,ilev,itr,n0_fvm)*fvm%dp_fvm(1:nc,1:nc,ilev,np1_fvm)
      do iw=1,irecons_tracer
        ctracer(iw,1-nhe:nc+nhe,1-nhe:nc+nhe,itr)=ctracer(iw,1-nhe:nc+nhe,1-nhe:nc+nhe,itr)*&
             fvm%dp_fvm(1-nhe:nc+nhe,1-nhe:nc+nhe,ilev,n0_fvm)
      end do
    end do

    do iside=1,4
      do j=jmin_side(iside),jmax_side(iside)
        do i=imin_side(iside),imax_side(iside)
           !DO NOT USE MASS_FLUX_SE AS THRESHOLD - THRESHOLD CONDITION MUST BE CONSISTENT WITH 
           !THE ONE USED IN DEFINE_SWEPT_AREAS
!          if (mass_flux_se(i,j,iside)>eps) then 
          if (fvm%se_flux(i,j,iside,ilev)>eps) then
            !        ||             ||
            !  tl1   ||             || tr1
            !        ||             ||
            !  =============================
            !        ||             ||
            !  tl2   ||             || tr2
            !        ||             ||
            !
            !
            tl1 = displ(3,i,j,iside)<0.0_r8.and.displ(6,i,j,iside).ge.0.0_r8 !departure point in tl1 quadrant
            tl2 = displ(6,i,j,iside)<0.0_r8.and.displ(7,i,j,iside)   >0.0_r8 !departure point in tl2 quadrant
            tr1 = displ(2,i,j,iside)<0.0_r8.and.displ(4,i,j,iside).ge.0.0_r8 !departure point in tr1 quadrant
            tr2 = displ(4,i,j,iside)<0.0_r8.and.displ(5,i,j,iside)   >0.0_r8 !departure point in tr2 quadrant
            ! pathological cases
            !        |  ||           ||                      ||           ||
            !        |  ||-----------||                      ||-----------||
            !        |  ||           ||                      ||           ||
            !  ================================     =================================
            !           ||           ||                   |  ||           ||
            !  ---------||           ||             ------|--||           ||
            !           ||           ||                   |  ||           ||
            !                tl1=tl1.or.tl2
            !                tr1=tr1.or.tr2
            !                tl1=displ(3,i,j,iside)<0.0_r8.and..not.(tl1.and.tl2)
            !                tr1=displ(2,i,j,iside)<0.0_r8.and..not.(tr1.and.tr2)

            !
            !
            !

            num_seg=-1; num_seg_static=-1 !initialization
            if (.not.tl1.and..not.tl2.and..not.tr1.and..not.tr2) then
              flowcase=0
              !        ||             ||                 ||             ||                ||             ||
              !        ||  *       *  ||                 ||  *----------*                 |*----------*  ||
              !        || /         \ ||                 || /           ||                ||           \ ||
              !        ||/           \||                 ||/            ||                ||            \||
              !  =============================     =============================     =============================
              !        ||             ||                 ||             ||                ||             ||
              !
              !
              !
              call define_area3_center (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                   num_seg_static,x_start, dgam_vec,fvm%se_flux(i,j,iside,ilev),displ_first_guess)

              gamma=1.0_r8!fvm%se_flux(i,j,iside,ilev)
              gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
            else
              if (tl1.and.tr1) then
                flowcase=1
                !  tl1   ||             || tr1             ||             ||                ||             ||
                !     *--||-------------||--*           *--||-------------||                ||-------------||--*
                !      \ ||             || /             \ ||             ||\              /||             || /
                !       \||             ||/               \||             || \            / ||             ||/
                !  =============================     =========================*===     ==*==========================
                !        ||             ||                 ||             ||                ||             ||
                !
                !
                !
                call define_area2           (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static,&
                     num_seg, num_seg_static,x_start, dgam_vec,displ_first_guess)
                call define_area3_left_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static,&
                     num_seg, num_seg_static,x_start, dgam_vec)
                call define_area4           (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static,&
                     num_seg, num_seg_static,x_start, dgam_vec)
                gamma=1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tl1.and..not.tr1.and..not.tr2) then
                flowcase=2
                !        ||             ||                 ||             ||                ||             ||
                !     *--||----------*  ||                /||----------*  ||             *--||-------------*
                !      \ ||           \ ||               / ||           \ ||              \ ||             ||
                !       \||            \||              /  ||            \||               \||             ||
                !  =============================     ==*==========================     =============================
                !        ||             ||                 ||             ||                ||             ||
                !
                !
                call define_area2     (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, num_seg_static,&
                     x_start, dgam_vec,displ_first_guess)
                call define_area3_left(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, num_seg_static,&
                     x_start, dgam_vec)
                gamma=1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tr1.and..not.tl1.and..not.tl2) then !displ(3).ge.0.0_r8) then
                flowcase=3
                !        ||  *----------||--*              ||  *----------||\                *-------------||--*
                !        || /           || /               || /           || \              ||             || /
                !        ||/            ||/                ||/            ||  \             ||             ||/
                !  =============================     ==========================*==     =============================
                !        ||             ||                 ||             ||                ||             ||
                !        ||             ||                 ||             ||                ||             ||
                !        ||             ||                 ||             ||                ||             ||
                !
                !
                call define_area3_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, &
                     num_seg_static, x_start, dgam_vec)
                call define_area4      (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, &
                     num_seg_static, x_start, dgam_vec,displ_first_guess)
                gamma=1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tl2.and..not.tr1.and..not.tr2) then !displ(2).ge.0.0_r8) then
                flowcase=4
                !        ||----------*  ||                 ||-------------*
                !       /||           \ ||                /||             ||
                !      / ||            \||               / ||             ||
                !  ===/=========================     ===/=========================
                !     | /||             ||              | /||             ||
                !     |/ ||             ||              |/ ||             ||
                !     *  ||             ||              *  ||             ||
                !
                !
                call define_area1_area2(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area3_left (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,&
                     x_start, dgam_vec,displ_first_guess)
                gamma = 1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tr2.and..not.tl1.and..not.tl2) then !displ(3).ge.0.0_r8) then
                flowcase=5
                !                case(5)
                !        ||  *-----2----||
                !        || /1         3||                !        ||/      4     ||                 !  =============================
                !        ||             ||\ |
                !        ||             || \|
                !        ||             ||  *
                !
                !
                !
                call define_area3_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area4_area5(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec,displ_first_guess)
                gamma=1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tl2.and.tr1.and..not.tr2) then
                flowcase=6
                !                case(6)
                !        ||-------------||--*
                !       /||             || /
                !      / ||             ||/
                !  ===/=========================
                !     | /||             ||
                !     |/ ||             ||
                !     *  ||             ||
                !
                !
                !
                !
                call define_area1_area2     (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area3_left_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area4           (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec,displ_first_guess)

                gamma=1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tr2.and.tl1.and..not.tl2) then
                flowcase=7
                !                case(7)
                !     *--||-------------||
                !      \ ||             ||                !       \||             ||                 !  =============================
                !        ||             ||\ |
                !        ||             || \|
                !        ||             ||  *
                !
                !
                !
                !
                call define_area2           (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec,displ_first_guess)
                call define_area3_left_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area4_area5     (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                gamma =  1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else if (tl2.and.tr2) then
                flowcase=8
                !                case(8)
                !        ||-------------||
                !       /||             ||                !      / ||             ||                 !  =============================
                !     | /||             ||\ |
                !     |/ ||             || \|
                !     *  ||             ||  *
                !
                !
                !
                !
                !
                !
                !
                call define_area1_area2     (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area3_left_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec)
                call define_area4_area5     (i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg,&
                     num_seg_static,x_start, dgam_vec,displ_first_guess)
                gamma =  1.0_r8
                gamma_max = fvm%displ_max(i,j,iside)/displ_first_guess
              else
                call endrun('ERROR - unknown flow case')
              end if
            end if
            ! iterate to get flux area
            !
            !
            !call t_startf('fvm:swept_area:get_gamma')
            do iarea=1,num_area
              dp_area(iarea) = fvm%dp_fvm(idx(1,iarea,i,j,iside),idx(2,iarea,i,j,iside),ilev,n0_fvm)
            end do
            call get_flux_segments_area_iterate(x,x_static,dx_static,dx,x_start,dgam_vec,num_seg,num_seg_static,&
                 num_seg_max,num_area,dp_area,flowcase,gamma,mass_flux_se(i,j,iside),0.0_r8,gamma_max)
            !call t_stopf('fvm:swept_area:get_gamma')
            ! pack segments for high-order weights computation
            !
            !
            do iarea=1,num_area
              do iseg=1,num_seg_static(iarea)
                iseg_tmp=num_seg(iarea)+iseg
                x (:,iseg_tmp,iarea)  = x_static (:,iseg,iarea)
                dx(:,iseg_tmp,iarea)  = dx_static(:,iseg,iarea)
              end do
              num_seg(iarea)=num_seg(iarea)+MAX(0,num_seg_static(iarea))
            end do
            ! compute higher-order weights
            !
            !
            !call t_startf('fvm:swept_area:get_high_order_w')
            call get_high_order_weights_over_areas(x,dx,num_seg,num_seg_max,num_area,weights,ngpc,gsweights, gspts,irecons_tracer)
            !call t_stopf('fvm:swept_area:get_high_order_w')
            !**************************************************
            ! remap air and tracers
            !**************************************************
            !
            !
            !
            !
            !call t_startf('fvm:swept_area:remap')
            flux=0.0_r8; flux_tracer=0.0_r8
            do iarea=1,num_area
              if (num_seg(iarea)>0) then
                ii=idx(1,iarea,i,j,iside); jj=idx(2,iarea,i,j,iside)
                flux=flux+weights(1,iarea)*fvm%dp_fvm(ii,jj,ilev,n0_fvm)
                do itr=1,ntrac
                  do iw=1,irecons_tracer
                    flux_tracer(itr) = flux_tracer(itr)+weights(iw,iarea)*ctracer(iw,ii,jj,itr)
                  end do
                end do
              end if
            end do
            fvm%se_flux(i,j,iside,ilev) = mass_flux_se(i,j,iside)-flux
            if (fvm%se_flux(i,j,iside,ilev)>1.0E-13_r8.and.(ilev<kmin_jet.or.ilev>kmax_jet)) then
              write(*,*) "CN excess flux outside of pre-scribed jet region"
              write(*,*) "Increase jet region with kmin_jet and kmax_jet ",&
                   ilev,fvm%se_flux(i,j,iside,ilev),mass_flux_se(i,j,iside),flux,flowcase,&
                   kmin_jet,kmax_jet
            end if

            fvm%dp_fvm(i  ,j  ,ilev        ,np1_fvm) = fvm%dp_fvm(i  ,j  ,ilev        ,np1_fvm)-flux
            fvm%     c(i  ,j  ,ilev,1:ntrac,np1_fvm) = fvm%     c(i  ,j  ,ilev,1:ntrac,np1_fvm)-flux_tracer(1:ntrac)
            ! update flux in nearest neighbor cells
            !
            !
            if (iside==1) then
              fvm%dp_fvm(i,j-1,ilev        ,np1_fvm) = fvm%dp_fvm(i,j-1,ilev        ,np1_fvm)+flux
              fvm%     c(i,j-1,ilev,1:ntrac,np1_fvm) = fvm%     c(i,j-1,ilev,1:ntrac,np1_fvm)+flux_tracer(1:ntrac)
            end if
            if (iside==2) then
              fvm%dp_fvm(i+1,j,ilev        ,np1_fvm) = fvm%dp_fvm(i+1,j,ilev        ,np1_fvm)+flux
              fvm%     c(i+1,j,ilev,1:ntrac,np1_fvm) = fvm%     c(i+1,j,ilev,1:ntrac,np1_fvm)+flux_tracer(1:ntrac)
            end if
            if (iside==3) then
              fvm%dp_fvm(i,j+1,ilev        ,np1_fvm) = fvm%dp_fvm(i,j+1,ilev        ,np1_fvm)+flux
              fvm%     c(i,j+1,ilev,1:ntrac,np1_fvm) = fvm%     c(i,j+1,ilev,1:ntrac,np1_fvm)+flux_tracer(1:ntrac)
            end if
            if (iside==4) then
              fvm%dp_fvm(i-1,j,ilev        ,np1_fvm) = fvm%dp_fvm(i-1,j,ilev        ,np1_fvm)+flux
              fvm%     c(i-1,j,ilev,1:ntrac,np1_fvm) = fvm%     c(i-1,j,ilev,1:ntrac,np1_fvm)+flux_tracer(1:ntrac)
            end if
            !call t_stopf('fvm:swept_area:remap')
          end if
        end do
      end do
    end do    
  end subroutine swept_flux


  subroutine get_flux_segments_area_iterate(x,x_static,dx_static,dx,x_start,dgam_vec,num_seg,num_seg_static,&
       num_seg_max,num_area,c,flow_case,gamma,flux,gamma_min,gamma_max)
    implicit none
    integer                                                , intent(in)    :: num_area, num_seg_max
    REAL(KIND=r8), dimension(2,num_seg_max,num_area), intent(in)    :: x_static, dx_static
    REAL(KIND=r8), dimension(2,num_seg_max,num_area), intent(inout) :: x, dx
    integer             , dimension(num_area              ), intent(in) :: num_seg, num_seg_static
    REAL(KIND=r8), dimension(2,8)                   , intent(in) :: x_start, dgam_vec
    REAL(KIND=r8)                                   , intent(inout) :: gamma
    REAL(KIND=r8)                                   , intent(in) :: flux,gamma_min,gamma_max
    integer                                                , intent(in) :: flow_case

    real (kind=r8), dimension(num_area)             , intent(in) :: c

    real (kind=r8)                                :: flux_static
    real (kind=r8)                                :: weight_area(num_area), xtmp(2), xtmp2(2)
    real (kind=r8)                                :: gamma1, gamma2, gamma3, dgamma, f1, f2

    real (kind=r8), dimension(  ngpc  ) :: xq,yq
    real (kind=r8), dimension(  ngpc,1) :: F !linear

    real (kind=r8) :: xq2,xq2i, rho, rhoi, yrh, w_static(num_area)

    integer :: iseg,iarea,iter,ipt
    integer, parameter :: iter_max=20
    logical :: lexit_after_one_more_iteration

    lexit_after_one_more_iteration = .false.
    ! compute static line-integrals (not necessary to recompute them for every iteration)
    !
    !
    flux_static = 0.0_r8
    w_static    = 0.0_r8
    weight_area = 0.0_r8
    do iarea=1,num_area
       do iseg=1,num_seg_static(iarea)
!rck vector directive needed here
!DIR$ SIMD

          do ipt=1,ngpc
             xq(ipt) = x_static(1,iseg,iarea)+dx_static(1,iseg,iarea)*gspts(ipt)! create quadrature point locations
             yq(ipt) = x_static(2,iseg,iarea)+dx_static(2,iseg,iarea)*gspts(ipt)
             F(ipt,1) = yq(ipt)/(SQRT(1.0_r8+xq(ipt)*xq(ipt) + yq(ipt)*yq(ipt))*(1.0_r8+xq(ipt)*xq(ipt)))! potential ! potential
          enddo
          weight_area(iarea) = weight_area(iarea)+sum(gsweights(:)*F(:,1))*0.5_r8*dx_static(1,iseg,iarea) !integral
       end do
       w_static(iarea)= weight_area(iarea)
       flux_static = flux_static+weight_area(iarea)*c(iarea)      !add to swept flux
    end do
    ! initilization
    !
    !
    gamma1=0.0_r8; f1=-flux   ! zero flux guess 1
    ! compute flux integrals of first guess passed to subroutine
    !
    !
    gamma2=gamma
    f2 = 0.0_r8
    weight_area=w_static
    do iarea=1,num_area
       do iseg=1,num_seg(iarea)
!rck vector directive needed here
!DIR$ SIMD
          do ipt=1,ngpc
             xq(ipt)  = x(1,iseg,iarea)+dx(1,iseg,iarea)*gspts(ipt)! create quadrature point locations
             yq(ipt)  = x(2,iseg,iarea)+dx(2,iseg,iarea)*gspts(ipt)
             xq2      =  xq(ipt)*xq(ipt)
             xq2i     =  1.0_r8/(1.0_r8+xq2)
             rho      =  SQRT(1.0_r8+xq2+yq(ipt)*yq(ipt))
             rhoi     =  1.0_r8/rho
             yrh      =  yq(ipt)*rhoi
             F(ipt,1) =  yrh*xq2i
          enddo
          weight_area(iarea) = weight_area(iarea)+sum(gsweights(:)*F(:,1))*0.5_r8*dx(1,iseg,iarea)! integral
       end do
       f2 = f2+weight_area(iarea)*c(iarea)
    end do
    f2 = f2-flux !integral error
    iter=0
    if (abs(f2-f1)<eps) then
      ! in case the first guess is converged
      !
      !
      return
    end if
    

    dgamma=(gamma2-gamma1)*f2/(f2-f1);
    gamma3 = gamma2-dgamma;                    ! Newton "guess" for gamma
    gamma1 = gamma2; f1 = f2; gamma2 = gamma3; ! prepare for iteration
    do iter=1,iter_max
       ! update vertex location: flow_case dependent to avoid many zero operations
       !
       !
       select case(flow_case)
       case(-4)
          iarea=1
          dx       (:,2,1) =  gamma3*dgam_vec (:,1)
          x        (:,1,1) =  x_start(:,1)+gamma3*dgam_vec(:,1)
          dx       (:,1,1) = -gamma3*dgam_vec (:,1)

       case(-2)
          iarea=1
          dx       (:,1,iarea) =  gamma3*dgam_vec (:,1)
          x        (:,2,iarea) =  x_start(:,2)+gamma3*dgam_vec(:,1)
          dx       (:,2,iarea) = -gamma3*dgam_vec (:,1)
       case(-1)
          ! to compute first-guess perpendicular displacements for iside=1
          !
          !
          iarea=1          
          x        (:,1,iarea) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx       (:,1,iarea) = -dx_static(:,1,iarea)
          x        (:,2,iarea) = x_start(:,2)+gamma3*dgam_vec(:,1)
          dx       (:,2,iarea) = x_start(:,2)-x(:,2,iarea)
       case(0)
          iarea=3
          xtmp = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx       (:,1,iarea) = xtmp(:  )-x(:,1,iarea)           !dynamic - line 2
          x        (:,2,iarea) = xtmp(:  )                        !dynamic - line 3
          dx       (:,2,iarea) = x_static(:,2,iarea)-x(:,2,iarea) !dynamic - line 3
       case(1)
          iarea=2
          xtmp(:        ) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)        !dynamic - line 2
          x   (:,2,iarea) = xtmp(:)                     !dynamic  - line 3
          dx  (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 3

          iarea            = 3
          xtmp (:  )       = x_start(:,4)+gamma3*dgam_vec(:,4)
          xtmp2(:  )       = x_start(:,5)+gamma3*dgam_vec(:,5)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x    (:,2,iarea) = xtmp (:)         !dynamic
          dx   (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic
          x    (:,3,iarea) = xtmp2(:)              !dynamic
          dx   (:,3,iarea) = x_start(:,5)-xtmp2(:) !dynamic

          iarea         = 4
          xtmp    (:  ) = x_start(:,6)+gamma3*dgam_vec(:,6)
          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic - line 2
          x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(2)
          iarea=2
          xtmp(:        ) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)        !dynamic - line 2
          x   (:,2,iarea) = xtmp(:)                     !dynamic  - line 3
          dx  (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 3

          iarea=3
          xtmp(:        ) = x_start(:,4)+gamma3*dgam_vec(:,4)!
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)        !dynamic - line 1
          x   (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx  (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(3)
          iarea         = 3
          xtmp    (:  ) = x_start(:,5)+gamma3*dgam_vec(:,5)
          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea) !dynamic - line 2
          x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx       (:,2,iarea) = x_static(:,2,iarea)-xtmp(:) !dynamic - line 2

          iarea         = 4
          xtmp    (:  ) = x_start(:,6)+gamma3*dgam_vec(:,6)
          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic - line 2
          x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(4)
          iarea           = 1
          xtmp(:        ) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x (:,2,iarea) = xtmp(:)                      !dynamic
          dx(:,2,iarea) = x_static(:,1,iarea)-xtmp(:)  !dynamic

          iarea         = 2
          xtmp    (:  ) = x_start(:,2)+gamma3*dgam_vec(:,2)
          xtmp2   (:  ) = x_start(:,3)+gamma3*dgam_vec(:,3)

          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic

          x (:,2,iarea) = xtmp (:)          !dynamic
          dx(:,2,iarea) = xtmp2(:)-xtmp(:)  !dynamic

          x (:,3,iarea) = xtmp2(:)                !dynamic
          dx(:,3,iarea) = x(:,1,iarea)-xtmp2(:)   !dynamic

          iarea            = 3
          xtmp (:        ) = x_start(:,4)+gamma3*dgam_vec(:,4)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic - line 1
          x    (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx   (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(5)
          iarea                = 3
          xtmp    (:  )        = x_start(:,5)+gamma3*dgam_vec(:,5)
          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea) !dynamic - line 2
          x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx       (:,2,iarea) = x_static(:,2,iarea)-xtmp(:) !dynamic - line 2

          iarea         = 4
          xtmp    (:  ) = x_start(:,6)+gamma3*dgam_vec(:,6)
          xtmp2   (:  ) = x_start(:,7)+gamma3*dgam_vec(:,7)

          dx(:,1,iarea) = xtmp(:)-x(:,1,iarea)   !dynamic - line 1
          x (:,2,iarea) = xtmp(:)          !dynamic -line 2
          dx       (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic - line 2
          x        (:,3,iarea) = xtmp2(:)               !dynamic  -line 1
          dx       (:,3,iarea) = x(:,1,iarea)-xtmp2(:)  !dynamic - line 1

          iarea             = 5
          xtmp  (:  )       = x_start(:,8)+gamma3*dgam_vec(:,8)

          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)   !dynamic - line 1
          x        (:,2,iarea) = xtmp(:)                     !dynamic -line 2
          dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(6)
          iarea = 1
          xtmp(:  ) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x (:,2,iarea) = xtmp(:)                      !dynamic
          dx(:,2,iarea) = x_static(:,1,iarea)-xtmp(:)  !dynamic

          iarea         = 2
          xtmp    (:  ) = x_start(:,2)+gamma3*dgam_vec(:,2)
          xtmp2   (:  ) = x_start(:,3)+gamma3*dgam_vec(:,3)

          dx(:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic
          x (:,2,iarea) = xtmp (:)          !dynamic
          dx(:,2,iarea) = xtmp2(:)-xtmp(:)  !dynamic
          x (:,3,iarea) = xtmp2(:)                !dynamic
          dx(:,3,iarea) = x(:,1,iarea)-xtmp2(:)   !dynamic

          iarea            = 3
          xtmp (:  )       = x_start(:,4)+gamma3*dgam_vec(:,4)
          xtmp2(:  )       = x_start(:,5)+gamma3*dgam_vec(:,5)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x    (:,2,iarea) = xtmp (:)         !dynamic
          dx   (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic
          x    (:,3,iarea) = xtmp2(:)              !dynamic
          dx   (:,3,iarea) = x_start(:,5)-xtmp2(:) !dynamic

          iarea         = 4
          xtmp    (:  ) = x_start(:,6)+gamma3*dgam_vec(:,6)
          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic - line 2
          x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
          dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(7)
          iarea=2
          xtmp(:        ) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)        !dynamic - line 2
          x   (:,2,iarea) = xtmp(:)                     !dynamic  - line 3
          dx  (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 3

          iarea            = 3
          xtmp (:  )       = x_start(:,4)+gamma3*dgam_vec(:,4)
          xtmp2(:  )       = x_start(:,5)+gamma3*dgam_vec(:,5)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x    (:,2,iarea) = xtmp (:)         !dynamic
          dx   (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic
          x    (:,3,iarea) = xtmp2(:)              !dynamic
          dx   (:,3,iarea) = x_start(:,5)-xtmp2(:) !dynamic

          iarea      = 4
          xtmp    (:  ) = x_start(:,6)+gamma3*dgam_vec(:,6)
          xtmp2   (:  ) = x_start(:,7)+gamma3*dgam_vec(:,7)

          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea) !dynamic
          x        (:,2,iarea) = xtmp(:)              !dynamic
          dx       (:,2,iarea) = xtmp2(:)-xtmp(:)     !dynamic
          x        (:,3,iarea) = xtmp2(:)               !dynamic
          dx       (:,3,iarea) = x(:,1,iarea)-xtmp2(:)  !dynamic

          iarea      = 5
          xtmp (:  ) = x_start(:,8)+gamma3*dgam_vec(:,8)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)   !dynamic - line 1
          x    (:,2,iarea) = xtmp(:)                     !dynamic -line 2
          dx   (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case(8)
          iarea = 1
          xtmp(:  ) = x_start(:,1)+gamma3*dgam_vec(:,1)
          dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x (:,2,iarea) = xtmp(:)                      !dynamic
          dx(:,2,iarea) = x_static(:,1,iarea)-xtmp(:)  !dynamic

          iarea         = 2
          xtmp    (:  ) = x_start(:,2)+gamma3*dgam_vec(:,2)
          xtmp2   (:  ) = x_start(:,3)+gamma3*dgam_vec(:,3)

          dx(:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic
          x (:,2,iarea) = xtmp (:)          !dynamic
          dx(:,2,iarea) = xtmp2(:)-xtmp(:)  !dynamic
          x (:,3,iarea) = xtmp2(:)                !dynamic
          dx(:,3,iarea) = x(:,1,iarea)-xtmp2(:)   !dynamic

          iarea            = 3
          xtmp (:  )       = x_start(:,4)+gamma3*dgam_vec(:,4)
          xtmp2(:  )       = x_start(:,5)+gamma3*dgam_vec(:,5)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic
          x    (:,2,iarea) = xtmp (:)         !dynamic
          dx   (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic
          x    (:,3,iarea) = xtmp2(:)              !dynamic
          dx   (:,3,iarea) = x_start(:,5)-xtmp2(:) !dynamic

          iarea      = 4
          xtmp    (:  ) = x_start(:,6)+gamma3*dgam_vec(:,6)
          xtmp2   (:  ) = x_start(:,7)+gamma3*dgam_vec(:,7)

          dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea) !dynamic
          x        (:,2,iarea) = xtmp(:)              !dynamic
          dx       (:,2,iarea) = xtmp2(:)-xtmp(:)     !dynamic
          x        (:,3,iarea) = xtmp2(:)               !dynamic
          dx       (:,3,iarea) = x(:,1,iarea)-xtmp2(:)  !dynamic

          iarea      = 5
          xtmp (:  ) = x_start(:,8)+gamma3*dgam_vec(:,8)
          dx   (:,1,iarea) = xtmp(:)-x(:,1,iarea)   !dynamic - line 1
          x    (:,2,iarea) = xtmp(:)                     !dynamic -line 2
          dx   (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
       case default
          call endrun('flow case not defined in get_flux_segments_area_iterate')
       end select
       ! compute flux integral
       !
       !
       f2 = 0.0_r8
       weight_area=w_static
       do iarea=1,num_area
         do iseg=1,num_seg(iarea)
!rck vector directive needed here
!DIR$ SIMD
           do ipt=1,ngpc

             xq(ipt) = x(1,iseg,iarea)+dx(1,iseg,iarea)*gspts(ipt)! create quadrature point locations
             yq(ipt) = x(2,iseg,iarea)+dx(2,iseg,iarea)*gspts(ipt)

             xq2      =  xq(ipt)*xq(ipt)
             xq2i     =  1.0_r8/(1.0_r8+xq2)
             rho      =  SQRT(1.0_r8+xq2+yq(ipt)*yq(ipt))
             rhoi     =  1.0_r8/rho
             yrh      =  yq(ipt)*rhoi
             F(ipt,1) =  yrh*xq2i
           end do
           weight_area(iarea) = weight_area(iarea)+sum(gsweights(:)*F(:,1))*0.5_r8*dx(1,iseg,iarea)! integral
         end do
         f2 = f2+weight_area(iarea)*c(iarea)
       end do
       f2 = f2-flux !integral error
       ! uncommented logic leads to noise in PS in FKESSLER at element boundary
       !       if (ABS(f2)<eps.or.ABS((gamma2-gamma1)*f2)<eps.or.lexit_after_one_more_iteration) then

       !
       !
       !
       if (ABS(f2)<eps.or.lexit_after_one_more_iteration) then
         gamma=gamma3
         if (gamma>gamma_max) then
           lexit_after_one_more_iteration=.true.
           gamma=gamma_max
           gamma3=gamma_max
         else
           exit
         end if
       else
          ! Newton increment
          !
          !
         if (abs(f2-f1)<eps) then
           ! if entering here abs(f2)>eps and abs(f1)>eps but abs(f2-f1)<eps
           !
           !
           dgamma=-0.5_r8*(gamma2-gamma1)
           lexit_after_one_more_iteration=.true.
         else
           dgamma=(gamma2-gamma1)*f2/(f2-f1)
         endif
         if (ABS(dgamma)>eps) then
           gamma3 = gamma2-dgamma;
         else
           ! dgamma set to minimum displacement to avoid f2-f1=0
           !
           !
           gamma3=gamma2-SIGN(1.0_r8,dgamma)*eps
           write(*,*) "WARNING: setting gamma to min",gamma3,iter
         end if
         gamma3=MAX(gamma3,gamma_min)
         ! prepare for next iteration
         !
         !
         gamma1 = gamma2; f1 = f2; gamma2 = gamma3;
       endif
     end do
     if (iter>iter_max) write(*,*) "WARNING: iteration not converged",&
          ABS(f2),flux,gamma1,gamma2,gamma3
  end subroutine get_flux_segments_area_iterate

  subroutine define_swept_areas(fvm,ilev,displ,base_vec,base_vtx,idx)
      USE control_mod, ONLY: neast, nwest, seast, swest 
    implicit none
    type (fvm_struct), intent(inout) :: fvm
    integer          , intent(in)    :: ilev


    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    real (kind=r8)    , dimension(0:7       , imin:imax,imin:imax,num_sides), intent(out) :: displ
    integer (kind=r8) , dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(out) :: base_vec
    real (kind=r8)    , dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(out) :: base_vtx
    integer                  , dimension(2,num_area, imin:imax,imin:imax,num_sides), intent(out) :: idx

    real (kind=r8) :: flux_sum     (0:nc+1,0:nc+1,2)
    integer               :: degenerate   (1:nc+1,1:nc+1  )
    integer               :: circular_flow(1:nc+1,1:nc+1  )
    integer               :: illcond      (1:nc+1,1:nc+1)
    integer               :: ib,i,j,sgn, iside, iarea
    ! set where reconstruction function is as a function of area and side

    !
    !
    integer, dimension(num_area*4), parameter :: idx_shift_tmp = (/-1,-1, 0, 1, 1,&  !iside=1
                                                                    1, 0, 0, 0, 1,&  !iside=2
                                                                    1, 1, 0,-1,-1,&  !iside=3
                                                                   -1, 0, 0, 0,-1/)  !iside=4

    integer, dimension(num_area*4), parameter :: idy_shift_tmp = (/-1, 0, 0, 0,-1,&  !iside=1
                                                                   -1,-1, 0, 1, 1,&  !iside=2
                                                                    1, 0, 0, 0, 1,&  !iside=3
                                                                    1, 1, 0,-1,-1/)  !iside=4

    integer, dimension(num_area,4), parameter :: idx_shift = RESHAPE(idx_shift_tmp,(/num_area,4/))
    integer, dimension(num_area,4), parameter :: idy_shift = RESHAPE(idy_shift_tmp,(/num_area,4/))

    integer, dimension(4), parameter :: iside_m1 = (/4,1,2,3/)
    integer, dimension(4), parameter :: iside_p1 = (/2,3,4,1/)
    integer, dimension(4), parameter :: iside_p2 = (/3,4,1,2/)
    integer, dimension(4), parameter :: iside_p3 = (/4,1,2,3/)

    integer, dimension(4), parameter :: imin_side = (/1   ,0   ,1   ,1   /)
    integer, dimension(4), parameter :: imax_side = (/nc  ,nc  ,nc  ,nc+1/)
    integer, dimension(4), parameter :: jmin_side = (/1   ,1   ,0   ,1   /)
    integer, dimension(4), parameter :: jmax_side = (/nc+1,nc  ,nc  ,nc  /)


    integer :: iur,jur,ilr,jlr,iul,jul,ill,jll

    ib = fvm%cubeboundary
    flux_sum(0:nc+1,1:nc+1,1) = fvm%se_flux(0:nc+1,0:nc  ,3,ilev)-fvm%se_flux(0:nc+1,1:nc+1,1,ilev)
    flux_sum(1:nc+1,0:nc+1,2) = fvm%se_flux(0:nc  ,0:nc+1,2,ilev)-fvm%se_flux(1:nc+1,0:nc+1,4,ilev)
    ! Degenerate case ("two departure points")
    !           ||  |                        || no change in this situation ||  no change in this situation
    !           ||  |                        ||                             ||
    !           ||--------                   ||----------                   ||----------
    !           ||  |                        ||                             ||
    ! =======================      =======================         =====================
    !       |   ||                       |   ||                             ||
    !  -----|---||                 ------|---||                    ---------||
    !       |   ||                       |   ||                             ||
    !       |   ||                       |   ||                             ||

    !
    !
    !
    !
    where (flux_sum(0:nc,1:nc+1,1)*flux_sum(1:nc+1,1:nc+1,1)<0.0_r8.and.flux_sum(1:nc+1,0:nc,2)*flux_sum(1:nc+1,1:nc+1,2)<0.0_r8)
       degenerate(:,:) = 0
    elsewhere
       degenerate(:,:) = 1
    end where

    if (ib>0) then
       if (ib==swest) degenerate(1   ,1   ) = 1
       if (ib==nwest) degenerate(1   ,nc+1) = 1
       if (ib==neast) degenerate(nc+1,nc+1) = 1
       if (ib==seast) degenerate(nc+1,1   ) = 1
    end if

    do j=1,nc+1
       do i=1,nc+1
          do sgn=-1,1,2
             if (&
                  sgn*flux_sum(i-1,j,1)<0.0_r8.and.sgn*flux_sum(i,j-1,2)>0.0_r8.and.&
                  sgn*flux_sum(i  ,j,1)>0.0_r8.and.sgn*flux_sum(i,j  ,2)<0.0_r8) then
                circular_flow(i,j) = 0
             else
                circular_flow(i,j) = 1
             end if
          end do
       end do
    end do
    ! wrap around corners
    !
    !
    if (ib==nwest) then
       flux_sum(0,nc+1,1) = fvm%se_flux(0,nc,3,ilev)-fvm%se_flux(1,nc+1,4,ilev)
       flux_sum(1,nc+1,2) = fvm%se_flux(0,nc,3,ilev)-fvm%se_flux(1,nc+1,4,ilev)

       i=1;j=nc+1;
       circular_flow(i,j) = 1
       do sgn=-1,1,2
          if (&
               sgn*flux_sum(i,j-1,2)>0.0_r8.and.&
               sgn*flux_sum(i  ,j,1)>0.0_r8.and.sgn*flux_sum(i,j  ,2)<0.0_r8) then
             circular_flow(i,j) = 0
          end if
       end do
    else if (ib==swest) then
       flux_sum(0,1,1) = fvm%se_flux(1,0,4,ilev)-fvm%se_flux(0,1,1,ilev)
       flux_sum(1,0,2) = fvm%se_flux(0,1,1,ilev)-fvm%se_flux(1,0,4,ilev)
       i=1;j=1;
       circular_flow(i,j) = 1
       do sgn=-1,1,2
          if (&
               sgn*flux_sum(i-1,j,1)<0.0_r8.and.&
               sgn*flux_sum(i  ,j,1)>0.0_r8.and.sgn*flux_sum(i,j  ,2)<0.0_r8) then
             circular_flow(i,j) = 0
          end if
       end do
    else if (ib==neast) then
       flux_sum(nc+1,nc+1,1) = fvm%se_flux(nc+1,nc,3,ilev)-fvm%se_flux(nc,nc+1,2,ilev)
       flux_sum(nc+1,nc+1,2) = fvm%se_flux(nc,nc+1,2,ilev)-fvm%se_flux(nc+1,nc,3,ilev)
       i=nc+1;j=nc+1;
       circular_flow(i,j) = 1
       do sgn=-1,1,2
          if (&
               sgn*flux_sum(i-1,j,1)<0.0_r8.and.sgn*flux_sum(i,j-1,2)>0.0_r8.and.&
               sgn*flux_sum(i,j  ,2)<0.0_r8) then
             circular_flow(i,j) = 0
          end if
       end do
    else if (ib==seast) then
       flux_sum(nc+1,1   ,1) = fvm%se_flux(nc,0,2,ilev)-fvm%se_flux(nc+1,1,1,ilev)
       flux_sum(nc+1,0   ,2) = fvm%se_flux(nc,0,2,ilev)-fvm%se_flux(nc+1,1,1,ilev)
       i=nc+1;j=1;
       circular_flow(i,j) = 1
       do sgn=-1,1,2
          if (&
               sgn*flux_sum(i-1,j,1)<0.0_r8.and.sgn*flux_sum(i,j-1,2)>0.0_r8.and.&
               sgn*flux_sum(i,j  ,2)<0.0_r8) then
             circular_flow(i,j) = 0
          end if
       end do
    end if
    illcond = circular_flow*degenerate
    !
    !
    !
    !
    do iside=1,4
       do j=jmin_side(iside),jmax_side(iside)
          do i=imin_side(iside),imax_side(iside)
             if (fvm%se_flux(i,j,iside,ilev)>eps) then
                iur = i+idx_shift(4,iside); jur = j+idy_shift(4,iside) !(i,j) index of upper right quadrant
                ilr = i+idx_shift(5,iside); jlr = j+idy_shift(5,iside) !(i,j) index of lower left  quadrant
                iul = i+idx_shift(2,iside); jul = j+idy_shift(2,iside) !(i,j) index of upper right quadrant
                ill = i+idx_shift(1,iside); jll = j+idy_shift(1,iside) !(i,j) index of lower left  quadrant
                !iside=1

                if (iside==1) then
                displ(0,i,j,iside) = -flux_sum   (i  ,j  ,1)*illcond(i,j)     !center left
                displ(1,i,j,iside) = -flux_sum   (i  ,j  ,1)*illcond(i+1,j)   !center right
                displ(2,i,j,iside) =  flux_sum   (i+1,j  ,2)*illcond(i+1,j)   !c2
                displ(3,i,j,iside) = -flux_sum   (i  ,j  ,2)*illcond(i  ,j)   !c3
                displ(4,i,j,iside) = -flux_sum   (i+1,j  ,1)*illcond(i+1,j)   !r1
                displ(5,i,j,iside) = -flux_sum   (i+1,j-1,2)*illcond(i+1,j)   !r2
                displ(6,i,j,iside) = -flux_sum   (i-1,j  ,1)*illcond(i  ,j)   !l1
                displ(7,i,j,iside) =  flux_sum   (i  ,j-1,2)*illcond(i  ,j)   !l2

                end if
                if (iside==2) then
                !iside=2
                displ(0,i,j,iside) =  flux_sum   (i+1,j  ,2)*illcond(i+1,j  )     !center left
                displ(1,i,j,iside) =  flux_sum   (i+1,j  ,2)*illcond(i+1,j+1)   !center right
                displ(2,i,j,iside) =  flux_sum   (i  ,j+1,1)*illcond(i+1,j+1)   !c2
                displ(3,i,j,iside) = -flux_sum   (i  ,j  ,1)*illcond(i+1,j  )   !c3
                displ(4,i,j,iside) =  flux_sum   (i+1,j+1,2)*illcond(i+1,j+1)   !r1
                displ(5,i,j,iside) = -flux_sum   (i+1,j+1,1)*illcond(i+1,j+1)   !r2
                displ(6,i,j,iside) =  flux_sum   (i+1,j-1,2)*illcond(i+1,j)   !l1
                displ(7,i,j,iside) =  flux_sum   (i+1,j  ,1)*illcond(i+1,j)   !l2
                end if
                !iside=3
                if (iside==3) then
                displ(0,i,j,iside) =  flux_sum   (i  ,j+1,1)*illcond(i+1,j+1)     !center left
                displ(1,i,j,iside) =  flux_sum   (i  ,j+1,1)*illcond(i  ,j+1)   !center right
                displ(2,i,j,iside) = -flux_sum   (i  ,j  ,2)*illcond(i  ,j+1)   !c2
                displ(3,i,j,iside) =  flux_sum   (i+1,j  ,2)*illcond(i+1,j+1)   !c3
                displ(4,i,j,iside) =  flux_sum   (i-1,j+1,1)*illcond(i  ,j+1)   !r1
                displ(5,i,j,iside) =  flux_sum   (i  ,j+1,2)*illcond(i  ,j+1)   !r2
                displ(6,i,j,iside) =  flux_sum   (i+1,j+1,1)*illcond(i+1,j+1)   !l1
                displ(7,i,j,iside) = -flux_sum   (i+1,j+1,2)*illcond(i+1,j+1)   !l2
                end if
                if (iside==4) then
                !iside=4
                displ(0,i,j,iside) = -flux_sum   (i  ,j  ,2)*illcond(i  ,j+1)     !center left
                displ(1,i,j,iside) = -flux_sum   (i  ,j  ,2)*illcond(i  ,j  )   !center right
                displ(2,i,j,iside) = -flux_sum   (i  ,j  ,1)*illcond(i  ,j  )   !c2
                displ(3,i,j,iside) =  flux_sum   (i  ,j+1,1)*illcond(i  ,j+1)   !c3
                displ(4,i,j,iside) = -flux_sum   (i  ,j-1,2)*illcond(i  ,j  )   !r1
                displ(5,i,j,iside) =  flux_sum   (i-1,j  ,1)*illcond(i  ,j  )   !r2
                displ(6,i,j,iside) = -flux_sum   (i  ,j+1,2)*illcond(i  ,j+1)   !l1
                displ(7,i,j,iside) = -flux_sum   (i-1,j+1,1)*illcond(i  ,j+1)   !l2
                end if

                base_vtx(:,1,i,j,iside) = fvm%vtx_cart(iside,:,i  ,j            )       !vertex center left
                base_vtx(:,2,i,j,iside) = fvm%vtx_cart(iside_p1(iside),:,i  ,j  )       !vertex center right
                base_vtx(:,3,i,j,iside) = fvm%vtx_cart(iside,:,iur,jur          )       !vertex upper right
                base_vtx(:,4,i,j,iside) = fvm%vtx_cart(iside_p3(iside),:,ilr,jlr)       !vertex lower right
                base_vtx(:,5,i,j,iside) = fvm%vtx_cart(iside_p1(iside),:,iul,jul)       !vertex upper left
                base_vtx(:,6,i,j,iside) = fvm%vtx_cart(iside_p2(iside),:,ill,jll)       !vertex lower left

                base_vec(:, 1,i,j,iside) = fvm%flux_vec    (:,i  ,j  ,iside          )      !vector center
                base_vec(:, 2,i,j,iside) = fvm%flux_vec    (:,i  ,j  ,iside_p1(iside))      !vector center right
                base_vec(:, 3,i,j,iside) = fvm%flux_vec    (:,i  ,j  ,iside_p3(iside))      !vector center left
                base_vec(:, 4,i,j,iside) = fvm%flux_vec    (:,iur,jur,iside          )      !vector upper right 1
                base_vec(:, 5,i,j,iside) = fvm%flux_vec    (:,iur,jur,iside_p3(iside))      !vector upper right 2
                base_vec(:, 6,i,j,iside) = fvm%flux_vec    (:,ilr,jlr,iside_p3(iside))      !vector lower right 1
                base_vec(:, 7,i,j,iside) = fvm%flux_vec    (:,ilr,jlr,iside_p2(iside))      !vector lower right 2
                base_vec(:, 8,i,j,iside) = fvm%flux_vec    (:,iul,jul,iside          )      !vector upper left 1
                base_vec(:, 9,i,j,iside) = fvm%flux_vec    (:,iul,jul,iside_p1(iside))      !vector upper left 2
                base_vec(:,10,i,j,iside) = fvm%flux_vec    (:,ill,jll,iside_p1(iside))      !vector lower left 1
                base_vec(:,11,i,j,iside) = fvm%flux_vec    (:,ill,jll,iside_p2(iside))      !vector lower left 2

                do iarea=1,5
                   idx(1,iarea,i,j,iside) = i+idx_shift(iarea,iside)
                   idx(2,iarea,i,j,iside) = j+idy_shift(iarea,iside)
                end do
             else
                displ(:,i,j,iside) = 9D99!for debugging
             end if
          end do
       end do
    end do
    ! wrap around corners here
    !
    !

  end subroutine define_swept_areas
  ! Notation conventions used in define_area subroutines
  !   ^    ||--->   ^   <---||    ^
  !  /|\   || 3    /|\    2 ||   /|  !   | 6  ||     1 |       ||    | 4
  !   |    ||       |       ||    |
  ! =================================
  !        ||               ||
  !        ||               ||
  !      7 ||               || 5
  !    <---||               ||--->


  !
  !
  !
  !
  !

  subroutine define_area1_area2(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, num_seg_static,&
       x_start, dgam_vec)
    implicit none
    integer, intent(in) :: i,j,iside
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    real (kind=r8)    , dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8) , dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8)    , dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx
    integer, parameter :: num_seg_max=5
    REAL(KIND=r8), dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer             , dimension(num_area)              , intent(inout) :: num_seg, num_seg_static
    REAL(KIND=r8), dimension(2,8)                   , intent(inout):: x_start, dgam_vec


    real (kind=r8)    , dimension(2,3) :: xdep !departure points
    real (kind=r8)                     :: gamma
    integer :: iarea


    REAL(KIND=r8) :: xtmp(2),xtmp2(2)
    !        ||-----        ||
    !       /||             ||
    !      / ||             ||
    !  ===X=========================
    !     | /||             ||
    !     |/ ||             ||
    !     *  ||             ||
    ! crossing X
    !
    !
    !
    !
    if (SUM(ABS(base_vec(:,9,i,j,iside))).NE.0) then
       gamma = displ(0,i,j,iside)*displ(7,i,j,iside)/(displ(0,i,j,iside)-displ(6,i,j,iside))
!       gamma = MAX(MIN(gamma,displ(7,i,j,iside),-displ(3,i,j,iside)),0.0_r8)!MWR manuscript
       gamma = MAX(MIN(gamma,displ(7,i,j,iside),-0.25_r8*displ(3,i,j,iside)),0.0_r8)!dbgxxx
    else
       ! corner case
       !
       !
       gamma=displ(0,i,j,iside)
    end if


    xdep    (:,1) = base_vtx(:, 6,i,j,iside)+displ(7,i,j,iside)*base_vec(:,10,i,j,iside)-displ(6,i,j,iside)*base_vec(:,11,i,j,iside)
    x_start (:,1) = base_vtx(:, 6,i,j,iside)
    dgam_vec(:,1) = base_vec(:,10,i,j,iside)*gamma

    xdep(:,2) = base_vtx(:,2,i,j,iside)+displ(1,i,j,iside)*base_vec(:, 1,i,j,iside)+displ(2,i,j,iside)*base_vec(:, 2,i,j,iside)

    iarea                  = 1
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 1

    x_static (:,1,iarea) = base_vtx(:,6,i,j,iside)       !static
    dx_static(:,1,iarea) = xdep(:,1)-x_static(:,1,iarea) !static

    xtmp(:        ) = x_start(:,1)+dgam_vec(:,1)
    x   (:,1,iarea) = xdep(:,1)                  !static
    dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic

    x (:,2,iarea) = xtmp(:)                      !dynamic
    dx(:,2,iarea) = x_static(:,1,iarea)-xtmp(:)  !dynamic
    !
    !
    !
    iarea                  = 2
    num_seg       (iarea)  = 3

    x_start (:,2) = base_vtx(:,5,i,j,iside)
    dgam_vec(:,2) = base_vec(:,9,i,j,iside)*gamma
    xtmp    (:  ) = x_start(:,2)+dgam_vec(:,2)

    x_start (:,3) = base_vtx(:,5,i,j,iside)
    dgam_vec(:,3) = base_vec(:,8,i,j,iside)*displ(0,i,j,iside)
    xtmp2   (:  ) = x_start(:,3)+dgam_vec(:,3)

    x   (:,1,iarea) = base_vtx(:,5,i,j,iside) !static
    dx  (:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic

    x (:,2,iarea) = xtmp (:)          !dynamic
    dx(:,2,iarea) = xtmp2(:)-xtmp(:)  !dynamic

    x (:,3,iarea) = xtmp2(:)                !dynamic
    dx(:,3,iarea) = x(:,1,iarea)-xtmp2(:)   !dynamic
  end subroutine define_area1_area2


  subroutine define_area2(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, num_seg_static,x_start, dgam_vec,&
       displ_first_guess)
    implicit none
    integer, intent(in) :: i,j,iside
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    real (kind=r8)    , dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8) , dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8)    , dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx
    integer, parameter :: num_seg_max=5
    REAL(KIND=r8), dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer             , dimension(num_area)              , intent(inout) :: num_seg, num_seg_static
    REAL(KIND=r8), dimension(2,8)                   , intent(inout):: x_start, dgam_vec


    real (kind=r8)    , dimension(2,3) :: xdep !departure points
    real (kind=r8), optional, intent(out)        :: displ_first_guess
    real (kind=r8) :: gamma
    integer :: iarea


    REAL(KIND=r8) :: xtmp(2)
    ! *: xdep(:,1)
    ! x: xtmp
    !      2 ||             ||
    !     *--x              ||
    !     1\3||1            ||
    !       \||             ||
    !  =============================
    !        ||             ||
    ! compute departure points (xdep(1) is left; xdep(3) is right and xdep(2) is midway
    !
    !
    !
    !
    xdep(:,1) = base_vtx(:,5,i,j,iside)+&
         MAX(0.0_r8,displ(6,i,j,iside))*base_vec(:,8,i,j,iside)-displ(3,i,j,iside)*base_vec(:,9,i,j,iside)
    x_start (:,1) = base_vtx(:,5,i,j,iside)
    gamma         = displ(0,i,j,iside)
    dgam_vec(:,1) = base_vec(:,8,i,j,iside)*gamma
    if (present(displ_first_guess)) displ_first_guess = gamma

    iarea                  = 2
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 1

    x_static (:,1,iarea) = base_vtx(:,5,i,j,iside)       !static  - line 1
    dx_static(:,1,iarea) = xdep(:,1)-x_static(:,1,iarea) !static  - line 1

    xtmp     (:        ) = x_start(:,1)+dgam_vec(:,1)
    x        (:,1,iarea) = xdep(:,1)                  !static  - line 2
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic - line 2

    x        (:,2,iarea) = xtmp(:)                     !dynamic  - line 3
    dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 3
  end subroutine define_area2


  subroutine define_area3_left(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, &
       num_seg, num_seg_static,x_start, dgam_vec,displ_first_guess)
    implicit none
    integer, intent(in) :: i,j,iside
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    real (kind=r8)    , dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8) , dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8)    , dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx
    integer, parameter :: num_seg_max=5
    REAL(KIND=r8), dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer             , dimension(num_area)              , intent(inout) :: num_seg, num_seg_static
    REAL(KIND=r8), dimension(2,8)                   , intent(inout):: x_start, dgam_vec
    real (kind=r8), optional, intent(out)        :: displ_first_guess

    real (kind=r8)    , dimension(2,3) :: xdep !departure points
    real (kind=r8)                     :: gamma
    integer :: iarea


    REAL(KIND=r8) :: xtmp(2)
    ! iarea = 3
    !-------------------------------------------------------------------------------------------
    !          xtmp         xdep(2)
    !           |x-----2------*   ||
    !           ||             \  ||
    !           |1              3 ||
    !           ||               \||
    !        ===========4==============

    !
    !
    !
    xdep(:,2) = base_vtx(:,2,i,j,iside)+displ(1,i,j,iside)*base_vec(:,1,i,j,iside)&
         +MAX(0.0_r8,displ(2,i,j,iside))*base_vec(:,2,i,j,iside)
    x_start (:,4) = base_vtx(:,1,i,j,iside)
    gamma         = displ(0,i,j,iside)
    dgam_vec(:,4) = base_vec(:,1,i,j,iside)*gamma
    xtmp    (:  ) = x_start(:,4)+dgam_vec(:,4)

    if (present(displ_first_guess)) displ_first_guess = gamma

    iarea                  = 3
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 2

    x_static (:,1,iarea) = xdep(:,2)                         !static  - line 3
    dx_static(:,1,iarea) = base_vtx(:,2,i,j,iside)-xdep(:,2) !static  - line 3

    x_static (:,2,iarea) = base_vtx(:,2,i,j,iside)                         !static  - line 4
    dx_static(:,2,iarea) = base_vtx(:,1,i,j,iside)-base_vtx(:,2,i,j,iside) !static  - line 4

    x        (:,1,iarea) = base_vtx(:,1,i,j,iside)    !static  - line 1
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic - line 1

    x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
    dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
  end subroutine define_area3_left

  subroutine define_area3_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, &
       num_seg_static,x_start, dgam_vec)
    implicit none
    integer, intent(in) :: i,j,iside
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    real (kind=r8)    , dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8) , dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8)    , dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx
    integer, parameter :: num_seg_max=5
    REAL(KIND=r8), dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer             , dimension(num_area)              , intent(inout) :: num_seg, num_seg_static
    REAL(KIND=r8), dimension(2,8)                   , intent(inout):: x_start, dgam_vec


    real (kind=r8)    , dimension(2,3) :: xdep !departure points
    real (kind=r8)                     :: gamma
    integer :: iarea

    REAL(KIND=r8) :: xtmp(2)
    !        ||  *-----2----||    !        || /1         3||     !        ||/      4     ||
    !  =============================
    !        ||             ||
    !        ||             ||
    !        ||             ||
    !
    !
    !
    xdep(:,1) = base_vtx(:,1,i,j,iside)+displ(0,i,j,iside)*base_vec(:,1,i,j,iside)&
         +MAX(0.0_r8,displ(3,i,j,iside))*base_vec(:,3,i,j,iside)
    x_start (:,5) = base_vtx(:,2,i,j,iside)
    gamma         = displ(1,i,j,iside)
    dgam_vec(:,5) = base_vec(:,1,i,j,iside)*gamma
    xtmp    (:  ) = x_start(:,5)+dgam_vec(:,5)

    iarea                  = 3
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 2

    x_static (:,1,iarea) = base_vtx(:,1,i,j,iside)           !static  - line 1
    dx_static(:,1,iarea) = xdep(:,1)-base_vtx(:,1,i,j,iside) !static  - line 1

    x_static (:,2,iarea) = base_vtx(:,2,i,j,iside)                         !static  - line 4
    dx_static(:,2,iarea) = base_vtx(:,1,i,j,iside)-base_vtx(:,2,i,j,iside) !static  - line 4

    x        (:,1,iarea) = xdep(:,1)            !static  - line 2
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea) !dynamic - line 2

    x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
    dx       (:,2,iarea) = x_static(:,2,iarea)-xtmp(:) !dynamic - line 2
  end subroutine define_area3_right


  subroutine define_area3_left_right(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, &
       num_seg_static,x_start, dgam_vec)
    implicit none
    integer, parameter  :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    integer, parameter  :: num_seg_max=5
    integer,                                                                 intent(in)   :: i,j,iside
    real (kind=r8),    dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout):: displ
    integer (kind=r8), dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout):: base_vec
    real (kind=r8),    dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout):: base_vtx
    real(KIND=r8),     dimension(2,num_seg_max,num_area),                    intent(inout):: x, dx, x_static, dx_static
    integer,           dimension(num_area),                                  intent(inout):: num_seg, num_seg_static
    real(KIND=r8),     dimension(2,8),                                       intent(inout):: x_start, dgam_vec

    real (kind=r8)      :: gamma
    integer             :: iarea
    real(KIND=r8)       :: xtmp(2),xtmp2(2)
    !        ||-------------||
    !       /||             ||    !        ||             ||
    !  =============================
    !        ||             ||
    !        ||             ||
    !        ||             ||
    !
    !
    x_start (:,4) = base_vtx(:,1,i,j,iside)
    x_start (:,5) = base_vtx(:,2,i,j,iside)
    gamma         = displ(0,i,j,iside)
    dgam_vec(:,4) = base_vec(:,1,i,j,iside)*gamma
    dgam_vec(:,5) = base_vec(:,1,i,j,iside)*gamma
    xtmp    (:  ) = x_start(:,4)+dgam_vec(:,4)
    xtmp2   (:  ) = x_start(:,5)+dgam_vec(:,5)

    iarea                  = 3
    num_seg       (iarea)  = 3
    num_seg_static(iarea)  = 1

    x_static (:,1,iarea) = base_vtx(:,2,i,j,iside)                         !static
    dx_static(:,1,iarea) = base_vtx(:,1,i,j,iside)-base_vtx(:,2,i,j,iside) !static

    x        (:,1,iarea) = base_vtx(:,1,i,j,iside)    !static
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)       !dynamic

    x        (:,2,iarea) = xtmp (:)         !dynamic
    dx       (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic

    x        (:,3,iarea) = xtmp2(:)              !dynamic
    dx       (:,3,iarea) = x_start(:,5)-xtmp2(:) !dynamic
  end subroutine define_area3_left_right

  subroutine define_area4_area5(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, &
       num_seg_static,x_start, dgam_vec,displ_first_guess)
    implicit none
    integer, intent(in) :: i,j,iside
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    integer, parameter :: num_seg_max=5
    real (kind=r8),    dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8), dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8),    dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx
    real(KIND=r8),     dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer,           dimension(num_area),               intent(inout) :: num_seg, num_seg_static
    real(KIND=r8),     dimension(2,8),                    intent(inout) :: x_start, dgam_vec
    real(KIND=r8),     optional,                          intent(out)   :: displ_first_guess


    real (kind=r8)    , dimension(2,3) :: xdep !departure points
    real (kind=r8)                     :: gamma
    integer :: iarea

    real(KIND=r8) :: xtmp(2),xtmp2(2)
    !        ||     --------||
    !        ||             ||    !        ||             ||     !  =============================
    !        ||             ||\ |
    !        ||             || \|
    !        ||             ||  *
    ! iarea  = 4
    !
    !
    !
    !
    iarea                  = 4
    num_seg       (iarea)  = 3

    if (SUM(ABS(base_vec(:,5,i,j,iside))).NE.0) then
       gamma = displ(1,i,j,iside)*displ(5,i,j,iside)/(displ(1,i,j,iside)-displ(4,i,j,iside))
!       gamma = MAX(MIN(gamma,displ(5,i,j,iside),-displ(2,i,j,iside)),0.0_r8)!MWR manuscript
       gamma = MAX(MIN(gamma,displ(5,i,j,iside),-0.25_r8*displ(2,i,j,iside)),0.0_r8)
    else
       ! corner case
       !
       !
       gamma = displ(1,i,j,iside)
    end if

    if (present(displ_first_guess)) displ_first_guess = displ(1,i,j,iside)

    x_start (:,6) = base_vtx(:,3,i,j,iside)
    dgam_vec(:,6) = base_vec(:,4,i,j,iside)*displ(1,i,j,iside)
    xtmp    (:  ) = x_start(:,6)+dgam_vec(:,6)
    x_start (:,7) = base_vtx(:,3,i,j,iside)
    dgam_vec(:,7) = base_vec(:,5,i,j,iside)*gamma
    xtmp2   (:  ) = x_start(:,7)+dgam_vec(:,7)

    x        (:,1,iarea) = base_vtx(:,3,i,j,iside)!static   -line 1
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)   !dynamic - line 1

    x        (:,2,iarea) = xtmp(:)          !dynamic -line 2
    dx       (:,2,iarea) = xtmp2(:)-xtmp(:) !dynamic - line 2

    x        (:,3,iarea) = xtmp2(:)               !static   -line 1
    dx       (:,3,iarea) = x(:,1,iarea)-xtmp2(:)  !dynamic - line 1
    !iarea = 5
    !
    !
    xdep(:,1) = base_vtx(:,4,i,j,iside)+displ(5,i,j,iside)*base_vec(:,6,i,j,iside)&
         -displ(4,i,j,iside)*base_vec(:,7,i,j,iside)
    x_start (:,8) = base_vtx(:,4,i,j,iside)
    dgam_vec(:,8) = base_vec(:,6,i,j,iside)*gamma
    xtmp    (:  ) = x_start(:,8)+dgam_vec(:,8)

    iarea                  = 5
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 1

    x        (:,1,iarea) = base_vtx(:,4,i,j,iside)!static   -line 1
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)   !dynamic - line 1

    x_static (:,1,iarea) = xdep(:,1)                        !static - line 1
    dx_static(:,1,iarea) = x(:,1,iarea)-x_static(:,1,iarea) !static - line 1

    x        (:,2,iarea) = xtmp(:)                     !dynamic -line 2
    dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
  end subroutine define_area4_area5


  subroutine define_area4(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, &
       num_seg_static,x_start, dgam_vec,displ_first_guess)
    implicit none
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    integer, parameter :: num_seg_max=5
    integer,                                                                 intent(in)    :: i,j,iside
    real (kind=r8),    dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8), dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8),    dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx

    real(KIND=r8),     dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer,           dimension(num_area)              , intent(inout) :: num_seg, num_seg_static
    real(KIND=r8),     dimension(2,8)                   , intent(inout) :: x_start, dgam_vec
    real(KIND=r8), optional,                              intent(out)   :: displ_first_guess


    real (kind=r8), dimension(2,3) :: xdep !departure points
    real (kind=r8)                 :: gamma
    integer                        :: iarea
    real(KIND=r8)                  :: xtmp(2)

    iarea                  = 4
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 1

    xdep(:,1) = base_vtx(:,3,i,j,iside)+MAX(0.0_r8,displ(4,i,j,iside))*base_vec(:,4,i,j,iside)&
         -displ(2,i,j,iside)*base_vec(:,5,i,j,iside)
    x_start (:,6) = base_vtx(:,3,i,j,iside)
    gamma         = displ(1,i,j,iside)
    dgam_vec(:,6) = base_vec(:,4,i,j,iside)*gamma
    xtmp    (:  ) = x_start(:,6)+dgam_vec(:,6)

    if (present(displ_first_guess)) displ_first_guess = gamma

    x_static (:,1,iarea) = xdep(:,1)                         !static
    dx_static(:,1,iarea) = base_vtx(:,3,i,j,iside)-xdep(:,1) !static

    x        (:,1,iarea) = base_vtx(:,3,i,j,iside) !static  - line 2
    dx       (:,1,iarea) = xtmp(:)-x(:,1,iarea)    !dynamic - line 2

    x        (:,2,iarea) = xtmp(:)                     !dynamic  -line 2
    dx       (:,2,iarea) = x_static(:,1,iarea)-xtmp(:) !dynamic - line 2
  end subroutine define_area4

  subroutine define_area3_center(i,j,iside,displ,base_vec,base_vtx,x, dx, x_static, dx_static, num_seg, num_seg_static,&
       x_start, dgam_vec,se_flux_center,displ_first_guess)
    implicit none
    integer, intent(in) :: i,j,iside
    integer, parameter :: num_area=5, num_sides=4, imin= 0, imax=nc+1
    integer, parameter :: num_seg_max=5
    real (kind=r8),    dimension(0:7       , imin:imax,imin:imax,num_sides), intent(inout) :: displ
    integer (kind=r8), dimension(1:2,11    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vec
    real (kind=r8),    dimension(1:2, 6    , imin:imax,imin:imax,num_sides), intent(inout) :: base_vtx

    real(KIND=r8),     dimension(2,num_seg_max,num_area), intent(inout) :: x, dx, x_static, dx_static
    integer,           dimension(num_area),               intent(inout) :: num_seg, num_seg_static
    real(KIND=r8),     dimension(2,8),                    intent(inout) :: x_start, dgam_vec
    real(KIND=r8) ,                                       intent(in   ) :: se_flux_center
    real(KIND=r8),     optional,                          intent(out)   :: displ_first_guess

    real (kind=r8)    , dimension(2,3) :: xdep !departure points
    real (kind=r8)                     :: gamma
    integer :: iarea
    !                 xdep(2)
    !                 ______X______
    !        ||      /             \      ||
    !        ||  *--/               \--*  ||
    !        || /xdep(1)         xdep(3)\ ||
    !        ||/                         \||
    !  ========================================
    !        ||                           ||
    ! compute departure points (xdep(1) is left; xdep(3) is right and xdep(2) is midway
    !
    !
    !
    !

    xdep(:,1) = base_vtx(:,1,i,j,iside)+&
         displ(0,i,j,iside)*base_vec(:,1,i,j,iside)+displ(3,i,j,iside)*base_vec(:,3,i,j,iside)
    xdep(:,3) = base_vtx(:,2,i,j,iside)+&
         displ(1,i,j,iside)*base_vec(:,1,i,j,iside)+displ(2,i,j,iside)*base_vec(:,2,i,j,iside)
    xdep(:,2) = 0.5_r8*(xdep(:,1)+xdep(:,3))

    gamma= se_flux_center
    x_start(:,1) = ABS(base_vec(:,3,i,j,iside))*((xdep(:,2)-base_vtx(:,1,i,j,iside)))+&
         base_vtx(:,1,i,j,iside) !xdep(2) - midway between departure points projected to side 1

    dgam_vec(:,1) = gamma*base_vec(:,1,i,j,iside)

    if (present(displ_first_guess)) displ_first_guess = gamma

    xdep(:,2)     = x_start(:,1)+dgam_vec(:,1)
    iarea                  = 3
    num_seg       (iarea)  = 2
    num_seg_static(iarea)  = 3
    !                 ______X______
    !        ||    2 /             \ 3    ||
    !        ||  *--/               \--*  ||
    !        || /                       \ ||
    !        ||/ 1          5           4\||
    !  ========================================
    !        ||                           ||

    !
    x_static (:,1,iarea) = base_vtx(:,1,i,j,iside)       !static  - line 1
    dx_static(:,1,iarea) = xdep(:,1)-x_static(:,1,iarea) !static  - line 1

    x        (:,1,iarea) = xdep(:,1)                     !static  - line 2
    dx       (:,1,iarea) = xdep(:,2)-x(:,1,iarea)        !dynamic - line 2

    x        (:,2,iarea) = xdep(:,2)                     !dynamic - line 3
    dx       (:,2,iarea) = xdep(:,3)-x(:,2,iarea)        !dynamic - line 3

    x_static (:,2,iarea) = xdep(:,3)                                  !static  - line 4
    dx_static(:,2,iarea) = base_vtx(:,2,i,j,iside)-x_static(:,2,iarea)!static  - line 4

    x_static (:,3,iarea) = base_vtx(:,2,i,j,iside)                         !static - line 5
    dx_static(:,3,iarea) = base_vtx(:,1,i,j,iside)-base_vtx(:,2,i,j,iside) !static - line 5

  end subroutine define_area3_center
  !read state subroutine for kr_externs_in_fvm_consistent_se_cslam 
  SUBROUTINE kr_externs_in_fvm_consistent_se_cslam(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) gspts 
          CALL kgen_array_sumcheck("gspts", kgen_array_sum, DBLE(SUM(gspts, mask=(gspts .eq. gspts))), .TRUE.) 
      END IF   
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) gsweights 
          CALL kgen_array_sumcheck("gsweights", kgen_array_sum, DBLE(SUM(gsweights, mask=(gsweights .eq. gsweights))), .TRUE.) 
      END IF   
  END SUBROUTINE kr_externs_in_fvm_consistent_se_cslam 
    
  !read state subroutine for kr_externs_out_fvm_consistent_se_cslam 
  SUBROUTINE kr_externs_out_fvm_consistent_se_cslam(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
  END SUBROUTINE kr_externs_out_fvm_consistent_se_cslam 
    
end module fvm_consistent_se_cslam
