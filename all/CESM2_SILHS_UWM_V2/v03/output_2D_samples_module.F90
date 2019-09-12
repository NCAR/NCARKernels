!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:31 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id$
!===============================================================================


module output_2D_samples_module

    USE stat_file_module, ONLY: stat_file 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
    USE stat_file_module, ONLY: kr_stat_file_module_stat_file 
    USE stat_file_module, ONLY: kv_stat_file_module_stat_file 

    IMPLICIT NONE 

    PUBLIC output_2d_uniform_dist_file, output_2d_lognormal_dist_file 

    PRIVATE 

  type(stat_file), public :: &
    lognormal_sample_file, &
    uniform_sample_file
  PUBLIC kr_externs_in_output_2d_samples_module 
  PUBLIC kr_externs_out_output_2d_samples_module 
  PUBLIC kv_externs_output_2d_samples_module 
  PUBLIC kr_stat_file_module_stat_file 
  TYPE(stat_file) :: kgenref_lognormal_sample_file, kgenref_uniform_sample_file 
  PUBLIC kv_stat_file_module_stat_file 

  !$omp threadprivate( lognormal_sample_file, uniform_sample_file )

  contains
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------

  subroutine output_2D_lognormal_dist_file &
             ( nz, num_samples, pdf_dim, X_nl_all_levs )
! Description:
!   Output a 2D snapshot of latin hypercube samples
! References:
!   None
!-------------------------------------------------------------------------------

      USE clubb_precision, ONLY: stat_rknd 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      nz,          & ! Number of vertical levels
      num_samples, & ! Number of samples per variable
      pdf_dim   ! Number variates being sampled

    real(kind=stat_rknd), intent(in), dimension(nz,num_samples,pdf_dim) :: &
      X_nl_all_levs ! Sample that is transformed ultimately to normal-lognormal

    integer :: sample, j
    ! ---- Begin Code ----


    do j = 1, pdf_dim
      allocate( lognormal_sample_file%var(j)%ptr(num_samples,1,nz) )
    end do

    do sample = 1, num_samples
      do j = 1, pdf_dim
        lognormal_sample_file%var(j)%ptr(sample,1,1:nz) = X_nl_all_levs(1:nz,sample,j)
      end do
    end do

    stop "This version of CLUBB was not compiled for netCDF output"

    do j = 1, pdf_dim
      deallocate( lognormal_sample_file%var(j)%ptr )
    end do

    return
  end subroutine output_2D_lognormal_dist_file
!-------------------------------------------------------------------------------

  subroutine output_2D_uniform_dist_file &
             ( nz, num_samples, dp2, X_u_all_levs, X_mixt_comp_all_levs, &
               lh_sample_point_weights )
! Description:
!   Output a 2D snapshot of latin hypercube uniform distribution, i.e. (0,1)
! References:
!   None
!-------------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd, stat_rknd 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      nz,          & ! Number of vertical levels
      num_samples, & ! Number of samples per variable
      dp2            ! Number of variates being sampled + 2

    real(kind=core_rknd), intent(in), dimension(nz,num_samples,dp2) :: &
      X_u_all_levs ! Uniformly distributed numbers between (0,1)

    integer, intent(in), dimension(nz,num_samples) :: &
      X_mixt_comp_all_levs ! Either 1 or 2

    real( kind = core_rknd ), dimension(nz,num_samples), intent(in) :: &
      lh_sample_point_weights ! Weight of each sample

    integer :: sample, j, k
    ! ---- Begin Code ----


    do j = 1, dp2+2
      allocate( uniform_sample_file%var(j)%ptr(num_samples,1,nz) )
    end do

    do sample = 1, num_samples
      do j = 1, dp2
        uniform_sample_file%var(j)%ptr(sample,1,1:nz) = &
          real( X_u_all_levs(1:nz,sample,j), kind = stat_rknd )
      end do
      uniform_sample_file%var(dp2+1)%ptr(sample,1,1:nz) = &
        real( X_mixt_comp_all_levs(1:nz,sample), kind=stat_rknd )
      do k = 1, nz 
        uniform_sample_file%var(dp2+2)%ptr(sample,1,k) = &
          real( lh_sample_point_weights(k,sample), kind=stat_rknd )
      end do
    end do

    stop "This version of CLUBB was not compiled for netCDF output"

    do j = 1, dp2+2
      deallocate( uniform_sample_file%var(j)%ptr )
    end do

    return
  end subroutine output_2D_uniform_dist_file
!-------------------------------------------------------------------------------


  !read state subroutine for kr_externs_in_output_2d_samples_module 
  SUBROUTINE kr_externs_in_output_2d_samples_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_stat_file_module_stat_file(uniform_sample_file, kgen_unit, "uniform_sample_file", .FALSE.) 
      CALL kr_stat_file_module_stat_file(lognormal_sample_file, kgen_unit, "lognormal_sample_file", .FALSE.) 
  END SUBROUTINE kr_externs_in_output_2d_samples_module 
    
  !read state subroutine for kr_externs_out_output_2d_samples_module 
  SUBROUTINE kr_externs_out_output_2d_samples_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      CALL kr_stat_file_module_stat_file(kgenref_uniform_sample_file, kgen_unit, "kgenref_uniform_sample_file", .FALSE.) 
      CALL kr_stat_file_module_stat_file(kgenref_lognormal_sample_file, kgen_unit, "kgenref_lognormal_sample_file", .FALSE.) 
  END SUBROUTINE kr_externs_out_output_2d_samples_module 
    
  !verify state subroutine for kv_externs_output_2d_samples_module 
  SUBROUTINE kv_externs_output_2d_samples_module(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_stat_file_module_stat_file("uniform_sample_file", check_status, uniform_sample_file, kgenref_uniform_sample_file) 
      ! YSK
      !CALL kv_kgen_output_2d_samples_module_subp4("lognormal_sample_file", check_status, lognormal_sample_file, &
      !&kgenref_lognormal_sample_file) 
  END SUBROUTINE kv_externs_output_2d_samples_module 
    
end module output_2D_samples_module
