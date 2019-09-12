!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:30 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id$
!===============================================================================


module latin_hypercube_driver_module

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
  ! Constant Parameters

  logical, parameter, private :: &
    l_output_2D_lognormal_dist   = .false., & ! Output a 2D netCDF file of the lognormal variates
    l_output_2D_uniform_dist     = .false.    ! Output a 2D netCDF file of the uniform distribution

  PRIVATE 


  PUBLIC generate_silhs_sample 

  PRIVATE stats_accumulate_uniform_lh 

  contains
!-------------------------------------------------------------------------------

  subroutine generate_silhs_sample &
             ( iter, pdf_dim, num_samples, sequence_length, nz, & ! intent(in)
               l_calc_weights_all_levs_itime, &                   ! intent(in)
               pdf_params, delta_zm, rcm, Lscale, &               ! intent(in)
               rho_ds_zt, mu1, mu2, sigma1, sigma2, &             ! intent(in)
               corr_cholesky_mtx_1, corr_cholesky_mtx_2, &        ! intent(in)
               hydromet_pdf_params, &                             ! intent(in)
               X_nl_all_levs, X_mixt_comp_all_levs, &             ! intent(out)
               lh_sample_point_weights )                          ! intent(out)
! Description:
!   Generate sample points of moisture, temperature, et cetera for the purpose
!   of computing tendencies with a microphysics or radiation scheme.
! References:
! https://arxiv.org/pdf/1711.03675v1.pdf#nameddest=url:overview_silhs
!-------------------------------------------------------------------------------

!

      USE array_index, ONLY: iipdf_chi 

      USE transform_to_pdf_module, ONLY: transform_uniform_sample_to_pdf

      USE output_2d_samples_module, ONLY: output_2d_lognormal_dist_file, output_2d_uniform_dist_file 

      USE pdf_parameter_module, ONLY: pdf_parameter 

      USE hydromet_pdf_parameter_module, ONLY: hydromet_pdf_parameter 

      USE constants_clubb, ONLY: fstderr, zero, one 

      USE clubb_precision, ONLY: core_rknd, stat_rknd 

      USE parameters_silhs, ONLY: l_lh_importance_sampling 

      USE error_code, ONLY: clubb_at_least_debug_level 
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE hydromet_pdf_parameter_module, ONLY: kr_kgen_hydromet_pdf_parameter_module_typesubp0 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
      USE hydromet_pdf_parameter_module, ONLY: kv_kgen_hydromet_pdf_parameter_module_typesubp0 

    implicit none
    ! External

    intrinsic :: allocated, mod, maxloc, epsilon, transpose
    ! Parameter Constants


    integer, parameter :: &
      d_uniform_extra = 2   ! Number of variables that are included in the uniform sample but not in
                            ! the lognormal sample. Currently:
                            ! pdf_dim+1: Mixture component, for choosing PDF component
                            ! pdf_dim+2: Precipitation fraction, for determining precipitation
    ! Input Variables
                            !

    integer, intent(in) :: &
      iter,            & ! Model iteration (time step) number
      pdf_dim,         & ! Number of variables to sample
      num_samples,     & ! Number of samples per variable
      sequence_length, & ! nt_repeat/num_samples; number of timesteps before sequence repeats
      nz                 ! Number of vertical model levels

    type(pdf_parameter), dimension(nz), intent(in) :: &
      pdf_params ! PDF parameters       [units vary]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      delta_zm, &  ! Difference in momentum altitudes    [m]
      rcm          ! Liquid water mixing ratio          [kg/kg]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      Lscale       ! Turbulent mixing length            [m]

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      rho_ds_zt    ! Dry, static density on thermo. levels    [kg/m^3]

    logical, intent(in) :: &
      l_calc_weights_all_levs_itime ! determines if vertically correlated sample points are needed
    ! Output Variables
    
    
    real( kind = core_rknd ), intent(out), dimension(nz,num_samples,pdf_dim) :: &
      X_nl_all_levs ! Sample that is transformed ultimately to normal-lognormal

    integer, intent(out), dimension(nz,num_samples) :: &
      X_mixt_comp_all_levs ! Which mixture component we're in

    real( kind = core_rknd ), intent(out), dimension(nz,num_samples) :: &
      lh_sample_point_weights ! Weight of each sample point
    ! More Input Variables!

    real( kind = core_rknd ), dimension(pdf_dim,pdf_dim,nz), intent(in) :: &
      corr_cholesky_mtx_1, & ! Correlations Cholesky matrix (1st comp.)  [-]
      corr_cholesky_mtx_2    ! Correlations Cholesky matrix (2nd comp.)  [-]

    real( kind = core_rknd ), dimension(pdf_dim,nz), intent(in) :: &
      mu1,    & ! Means of the hydrometeors, 1st comp. (chi, eta, w, <hydrometeors>)  [units vary]
      mu2,    & ! Means of the hydrometeors, 2nd comp. (chi, eta, w, <hydrometeors>)  [units vary]
      sigma1, & ! Stdevs of the hydrometeors, 1st comp. (chi, eta, w, <hydrometeors>) [units vary]
      sigma2    ! Stdevs of the hydrometeors, 2nd comp. (chi, eta, w, <hydrometeors>) [units vary]

    type(hydromet_pdf_parameter), dimension(nz), intent(in) :: &
      hydromet_pdf_params ! Hydrometeor PDF parameters  [units vary]
    ! Local variables

    real( kind = core_rknd ), dimension(nz,num_samples,(pdf_dim+d_uniform_extra)) :: &
      X_u_all_levs ! Sample drawn from uniform distribution

    real( kind = core_rknd ), dimension(pdf_dim+d_uniform_extra, nz,num_samples) :: &
      X_u_all_levs_tmp ! Sample drawn from uniform distribution

    real( kind = core_rknd ), dimension(pdf_dim, nz,num_samples) :: &
      X_nl_all_levs_tmp ! Sample drawn from uniform distribution
     

    integer :: &
      k_lh_start, & ! Height for preferentially sampling within cloud
      k, sample, i  ! Loop iterators

    logical, dimension(nz,num_samples) :: &
      l_in_precip   ! Whether sample is in precipitation

    logical :: l_error, l_error_in_sub
    ! Precipitation fraction in a component of the PDF, for each sample

    real( kind = core_rknd ), dimension(num_samples) :: precip_frac_i
    real( kind = core_rknd ) cloud_frac_1_tmp(nz),cloud_frac_2_tmp(nz)
    integer :: pdf_dim2
    ! ---- Begin Code ----


    l_error = .false.
    ! Sanity checks for l_lh_importance_sampling

    if ( l_lh_importance_sampling .and. sequence_length /= 1 ) then
      write(fstderr,*) "Cloud weighted sampling requires sequence length be equal to 1."
      stop "Fatal error."
    end if
    !--------------------------------------------------------------
    ! Latin hypercube sampling
    !--------------------------------------------------------------
    ! Compute k_lh_start, the starting vertical grid level 
    !   for None sampling


    k_lh_start = compute_k_lh_start( nz, rcm, pdf_params )
    if ( .not. l_calc_weights_all_levs_itime ) then
      ! Generate a uniformly distributed sample at k_lh_start
    
      call generate_uniform_sample_at_k_lh_start &
           ( iter, pdf_dim, d_uniform_extra, num_samples, sequence_length, & ! Intent(in)
             pdf_params(k_lh_start), hydromet_pdf_params(k_lh_start), &          ! Intent(in)
             X_u_all_levs(k_lh_start,:,:), lh_sample_point_weights(1,:)  )             ! Intent(out)
                          
      forall ( k = 2:nz )
        lh_sample_point_weights(k,:) = lh_sample_point_weights(1,:)
      end forall
      ! Generate uniform sample at other grid levels 
      !   by vertically correlating them
      
      call vertical_overlap_driver &
           ( nz, pdf_dim, d_uniform_extra, num_samples, &     ! Intent(in)
             k_lh_start, delta_zm, rcm, Lscale, rho_ds_zt, &      ! Intent(in)
             X_u_all_levs )                                       ! Intent(inout)
    
    end if
    
    do k = 1, nz
    
      ! print *,'generate_silhs_sample: point #5'
      if ( l_calc_weights_all_levs_itime ) then
        ! moved inside the loop to apply importance sampling for each layer
        ! 
        call generate_uniform_sample_at_k_lh_start &
          ( iter, pdf_dim, d_uniform_extra, num_samples, sequence_length, & ! Intent(in)
            pdf_params(k), hydromet_pdf_params(k), &          ! Intent(in)
            X_u_all_levs(k,:,:), lh_sample_point_weights(k,:) )             ! Intent(out)
      end if
      ! print *,'generate_silhs_sample: point #6'
      ! Determine mixture component for all levels
           
      where ( in_mixt_comp_1( X_u_all_levs(k,:,pdf_dim+1), pdf_params(k)%mixt_frac ) )
        X_mixt_comp_all_levs(k,:) = 1
      else where
        X_mixt_comp_all_levs(k,:) = 2
      end where
      ! Determine precipitation fraction

      where ( X_mixt_comp_all_levs(k,:) == 1 )
        precip_frac_i(:) = hydromet_pdf_params(k)%precip_frac_1
      else where
        precip_frac_i(:) = hydromet_pdf_params(k)%precip_frac_2
      end where
      ! Determine precipitation for all levels

      where ( in_precipitation( X_u_all_levs(k,:,pdf_dim+2), &
                  precip_frac_i(:) ) )
        l_in_precip(k,:) = .true.
      else where
        l_in_precip(k,:) = .false.
      end where

    end do ! k = 1 .. nz

    call stats_accumulate_uniform_lh( nz, num_samples, l_in_precip, X_mixt_comp_all_levs, &
                                      X_u_all_levs(:,:,iiPDF_chi), pdf_params, &
                                      lh_sample_point_weights, k_lh_start )
    ! Check to ensure uniform variates are in the appropriate range

    
    !!$acc parallel 
    !!$acc loop collapse(2)
    do sample=1, num_samples
      do k=1, nz
        do i=1, pdf_dim+d_uniform_extra
          if ( X_u_all_levs(k,sample,i) >= one ) then
            X_u_all_levs(k,sample,i) = one - epsilon( X_u_all_levs(k,sample,i) )
          else if ( X_u_all_levs(k,sample,i) <= zero ) then
            X_u_all_levs(k,sample,i) = epsilon( X_u_all_levs(k,sample,i) )
          end if
        end do
      end do
    end do
    !!$acc end parallel
    ! Sample loop

    cloud_frac_1_tmp(:) = pdf_params(:)%cloud_frac_1
    cloud_frac_2_tmp(:) = pdf_params(:)%cloud_frac_2
    pdf_dim2 = pdf_dim + d_uniform_extra
    do k=1,nz
    do sample = 1, num_samples
       X_u_all_levs_tmp(:,k,sample) = X_u_all_levs(k,sample,:)
       X_nl_all_levs_tmp(:,k,sample) = X_nl_all_levs(k,sample,:)
    enddo
    enddo
    !$acc parallel
    !$acc loop collapse(2)
    do k = 1, nz
      ! Generate LH sample, represented by X_u and X_nl, for level k
      do sample = 1, num_samples, 1
        ! Transform the uniformly distributed samples to
        !   ones distributed according to CLUBB's PDF.
        call transform_uniform_sample_to_pdf &
             ( pdf_dim, d_uniform_extra, & ! In
               mu1(:,k), mu2(:,k), sigma1(:,k), sigma2(:,k), & ! In
               corr_cholesky_mtx_1(:,:,k), & ! In
               corr_cholesky_mtx_2(:,:,k), & ! In
               !X_u_all_levs(k,sample,:), X_mixt_comp_all_levs(k,sample), & ! In
               X_u_all_levs_tmp(:,k,sample), X_mixt_comp_all_levs(k,sample), & ! In
               pdf_params(k)%cloud_frac_1, pdf_params(k)%cloud_frac_2, & ! In
               !cloud_frac_1_tmp(k), cloud_frac_2_tmp(k), & ! In
               l_in_precip(k,sample), & ! In
               !X_nl_all_levs(k,sample,:) ) ! Out
               X_nl_all_levs_tmp(:,k,sample) ) ! Out
      end do ! sample = 1, num_samples, 1
    end do ! k = 1, nz
    !$acc end parallel
    do k=1,nz
    do sample = 1, num_samples
       X_u_all_levs(k,sample,:)  = X_u_all_levs_tmp(:,k,sample)
       X_nl_all_levs(k,sample,:) = X_nl_all_levs_tmp(:,k,sample)
    enddo
    enddo
 !    stop 'after ACC parallel region'

    if ( l_output_2D_lognormal_dist ) then
      ! Eric Raut removed lh_rt and lh_thl from call to output_2D_lognormal_dist_file
      ! because they are no longer generated in generate_silhs_sample.
      call output_2D_lognormal_dist_file( nz, num_samples, pdf_dim, &
                                          real(X_nl_all_levs, kind = stat_rknd) )
    end if
    if ( l_output_2D_uniform_dist ) then
      call output_2D_uniform_dist_file( nz, num_samples, pdf_dim+2, &
                                        X_u_all_levs, &
                                        X_mixt_comp_all_levs, &
                                        lh_sample_point_weights )
    end if
    ! Various nefarious assertion checks

    if ( clubb_at_least_debug_level( 2 ) ) then
      ! Simple assertion check to ensure uniform variates are in the appropriate
      ! range

      if ( any( X_u_all_levs <= zero .or. X_u_all_levs >= one ) ) then
        write(fstderr,*) "A uniform variate was not in the correct range."
        l_error = .true.
      end if

      do k=2, nz

        call assert_consistent_cloud_frac( pdf_params(k), l_error_in_sub )
        l_error = l_error .or. l_error_in_sub
        ! Check for correct transformation in normal space

        call assert_correct_cloud_normal( num_samples, X_u_all_levs(k,:,iiPDF_chi), & ! In
                                          X_nl_all_levs(k,:,iiPDF_chi), & ! In
                                          X_mixt_comp_all_levs(k,:), & ! In
                                          pdf_params(k)%cloud_frac_1, & ! In
                                          pdf_params(k)%cloud_frac_2, & ! In
                                          l_error_in_sub ) ! Out
        l_error = l_error .or. l_error_in_sub

      end do ! k=2, nz

    end if ! clubb_at_least_debug_level( 2 )
    ! Stop the run if an error occurred

    if ( l_error ) then
      stop "Fatal error in generate_silhs_sample"
    end if

    return
  end subroutine generate_silhs_sample
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine generate_uniform_sample_at_k_lh_start &
             ( iter, pdf_dim, d_uniform_extra, num_samples, sequence_length, &
               pdf_params, hydromet_pdf_params, &
               X_u_k_lh_start, lh_sample_point_weights )
  ! Description:
  !   Generates a uniform sample, X_u, at a single height level, applying Latin
  !   hypercube and importance sampling where configured.
  ! References:
  !   V. E. Larson and D. P. Schanen, 2013. The Subgrid Importance Latin
  !   Hypercube Sampler (None): a multivariate subcolumn generator
  !----------------------------------------------------------------------
    ! Included Modules


      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: one, fstderr 

      USE parameters_silhs, ONLY: l_lh_straight_mc, l_lh_importance_sampling 

      USE pdf_parameter_module, ONLY: pdf_parameter 

      USE hydromet_pdf_parameter_module, ONLY: hydromet_pdf_parameter 

      USE generate_uniform_sample_module, ONLY: rand_uniform_real, generate_uniform_lh_sample 

      USE silhs_importance_sample_module, ONLY: importance_sampling_driver, cloud_weighted_sampling_driver 

      USE latin_hypercube_arrays, ONLY: one_height_time_matrix 

      USE array_index, ONLY: iipdf_chi 
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE hydromet_pdf_parameter_module, ONLY: kr_kgen_hydromet_pdf_parameter_module_typesubp0 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
      USE hydromet_pdf_parameter_module, ONLY: kv_kgen_hydromet_pdf_parameter_module_typesubp0 

    implicit none
    ! Local Constants

    logical, parameter :: &
      l_lh_old_cloud_weighted  = .false. ! Use the old method of importance sampling that
                                         ! places one point in cloud and one point out of
                                         ! cloud
    ! Input Variables

    integer, intent(in) :: &
      iter,              &        ! Model iteration number
      pdf_dim,       &        ! Number of variates in CLUBB's PDF
      d_uniform_extra,   &        ! Uniform variates included in uniform sample but not
                                  !  in normal/lognormal sample
      num_samples,       &        ! Number of None sample points
      sequence_length             ! Number of timesteps before new sample points are picked
                                  !  in normal/lognormal sample

    type(pdf_parameter), intent(in) :: &
      pdf_params                  ! The PDF parameters at k_lh_start

    type(hydromet_pdf_parameter), intent(in) :: &
      hydromet_pdf_params
    ! Output Variables

    real( kind = core_rknd ), dimension(num_samples,pdf_dim+d_uniform_extra), intent(out) :: &
      X_u_k_lh_start              ! Uniform sample at k_lh_start

    real( kind = core_rknd ), dimension(num_samples), intent(out) :: &
      lh_sample_point_weights     ! Weight of each sample point (all equal to one if importance
                                  ! sampling is not used)
    ! Local Variables

    integer :: &
      i, sample
  !----------------------------------------------------------------------
    !----- Begin Code -----
    ! Sanity check


    if ( l_lh_old_cloud_weighted .and. mod( num_samples, 2 ) /= 0 ) then
      write(fstderr,*) "Old cloud weighted sampling requires num_samples to be divisible by 2."
      stop "Fatal error."
    end if

    if ( l_lh_straight_mc ) then
      ! Do a straight Monte Carlo sample without LH or importance sampling.

      do i=1, pdf_dim+d_uniform_extra
        do sample=1, num_samples
          X_u_k_lh_start(sample,i) = rand_uniform_real( )
        end do
      end do
      ! Importance sampling is not performed, so all sample points have the same weight!!

      lh_sample_point_weights(1:num_samples)  =  one

    else ! .not. l_lh_straight_mc
      ! Generate a uniformly distributed Latin hypercube sample

      call generate_uniform_lh_sample( iter, num_samples, sequence_length, &  ! Intent(in)
                                       pdf_dim+d_uniform_extra, &         ! Intent(in)
                                       X_u_k_lh_start(:,:) )                  ! Intent(out)

      if ( l_lh_importance_sampling ) then

        if ( l_lh_old_cloud_weighted ) then

          call cloud_weighted_sampling_driver &
               ( num_samples, one_height_time_matrix(:,iiPDF_chi), & ! In
                 one_height_time_matrix(:,pdf_dim+1), & ! In
                 pdf_params%cloud_frac_1, pdf_params%cloud_frac_2, & ! In
                 pdf_params%mixt_frac, & ! In
                 X_u_k_lh_start(:,iiPDF_chi), & ! In/Out
                 X_u_k_lh_start(:,pdf_dim+1), & ! In/Out
                 lh_sample_point_weights ) ! Out

        else ! .not. l_lh_old_cloud_weighted

          call importance_sampling_driver &
               ( num_samples, pdf_params, hydromet_pdf_params, & ! In
                 X_u_k_lh_start(:,iiPDF_chi), & ! In/Out
                 X_u_k_lh_start(:,pdf_dim+1), & ! In/Out
                 X_u_k_lh_start(:,pdf_dim+2), & ! In/Out
                 lh_sample_point_weights ) ! Out

        end if ! l_lh_old_cloud_weighted

      else
        ! No importance sampling is performed, so all sample points have the same weight.

        lh_sample_point_weights(1:num_samples) = one

      end if ! l_lh_importance_sampling

    end if ! l_lh_straight_mc

    return
  end subroutine generate_uniform_sample_at_k_lh_start
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine vertical_overlap_driver &
             ( nz, pdf_dim, d_uniform_extra, num_samples, &
               k_lh_start, delta_zm, rcm, Lscale, rho_ds_zt, &
               X_u_all_levs )
  ! Description:
  !   Takes a uniform sample at k_lh_start and correlates it vertically
  !   to other height levels
  ! References:
  ! https://arxiv.org/pdf/1711.03675v1.pdf#nameddest=url:vert_corr
  !-----------------------------------------------------------------------
    ! Included Modules


      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: fstderr, one, zero 

      USE grid_class, ONLY: gr 

      USE array_index, ONLY: iipdf_chi 

      USE parameters_silhs, ONLY: l_lscale_vert_avg 

      USE fill_holes, ONLY: vertical_avg 

      USE error_code, ONLY: clubb_at_least_debug_level 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      nz,              &
      pdf_dim,     &
      d_uniform_extra, &
      num_samples,     &
      k_lh_start

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      delta_zm,        &  ! Difference in altitude between momentum levels    [m]
      rcm,             &  ! Liquid water mixing ratio                         [kg/kg]
      Lscale,          &  ! Turbulent mixing length                           [m]
      rho_ds_zt           ! Dry, static density on thermodynamic levels       [kg/m^3]
    ! Input/Output Variables

    real( kind = core_rknd ), dimension(nz,num_samples,pdf_dim+d_uniform_extra), &
        intent(inout) :: &
      X_u_all_levs        ! A full uniform sample
    ! Local Variables

    real( kind = core_rknd ), dimension(nz) :: &
      Lscale_vert_avg, &  ! 3pt vertical average of Lscale                    [m]
      X_vert_corr         ! Vertical correlations between height levels       [-]

    integer :: k, km1, kp1, sample, ivar
  !-----------------------------------------------------------------------
    !----- Begin Code -----

    if ( l_Lscale_vert_avg ) then
      ! Determine 3pt vertically averaged Lscale
      do k = 1, nz, 1
        kp1 = min( k+1, nz )
        km1 = max( k-1, 1 )
        Lscale_vert_avg(k) = vertical_avg &
                             ( (kp1-km1+1), rho_ds_zt(km1:kp1), &
                               Lscale(km1:kp1), gr%dzt(km1:kp1) )
      end do
    else
        Lscale_vert_avg = Lscale 
    end if

    X_vert_corr(1:nz) = compute_vert_corr( nz, delta_zm, Lscale_vert_avg, rcm )
    ! Assertion check for the vertical correlation

    if ( clubb_at_least_debug_level( 1 ) ) then
      if ( any( X_vert_corr > one ) .or. any( X_vert_corr < zero ) ) then
        write(fstderr,*) "The vertical correlation in latin_hypercube_driver"// &
          "is not in the correct range"
        do k = 1, nz
          write(fstderr,*) "k = ", k,  "Vert. correlation = ", X_vert_corr(k)
        end do
        stop "Fatal error in vertical_overlap_driver"
      end if ! Some correlation isn't between [0,1]
    end if ! clubb_at_least_debug_level 1

    do sample = 1, num_samples
      ! Correlate chi vertically
      call compute_arb_overlap &
           ( nz, k_lh_start, &  ! In
             X_vert_corr, & ! In
             X_u_all_levs(:,sample,iiPDF_chi) ) ! Inout
      ! Correlate the d+1 variate vertically (used to compute the mixture
      ! component later)
      call compute_arb_overlap &
           ( nz, k_lh_start, &  ! In
             X_vert_corr, & ! In
             X_u_all_levs(:,sample,pdf_dim+1) ) ! Inout
      ! Correlate the d+2 variate vertically (used to determine precipitation
      ! later)

      call compute_arb_overlap &
           ( nz, k_lh_start, &  ! In
             X_vert_corr, & ! In
             X_u_all_levs(:,sample,pdf_dim+2) ) ! Inout
      ! Use these lines to make all variates vertically correlated, using the
      ! same correlation we used above for chi and the d+1 variate

      do ivar = 1, pdf_dim
        if ( ivar /= iiPDF_chi ) then
          call compute_arb_overlap &
               ( nz, k_lh_start, &  ! In
                 X_vert_corr, & ! In
                 X_u_all_levs(:,sample,ivar) ) ! Inout
        end if
      end do ! 1..pdf_dim
    end do ! 1..num_samples

    return
  end subroutine vertical_overlap_driver
!-------------------------------------------------------------------------------
!-----------------------------------------------------------------------

  function compute_k_lh_start( nz, rcm, pdf_params ) result( k_lh_start )
  ! Description:
  !   Determines the starting None sample level
  ! References:
  !   None
  !-----------------------------------------------------------------------
    ! Included Modules


      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: cloud_frac_min 

      USE pdf_parameter_module, ONLY: pdf_parameter 

      USE pdf_utilities, ONLY: compute_mean_binormal 

      USE math_utilities, ONLY: rand_integer_in_range 

      USE parameters_silhs, ONLY: l_rcm_in_cloud_k_lh_start, l_random_k_lh_start 
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      nz          ! Number of vertical levels

    real( kind = core_rknd ), dimension(nz), intent(in) :: &
      rcm         ! Liquid water mixing ratio               [kg/kg]

    type(pdf_parameter), dimension(nz), intent(in) :: &
      pdf_params  ! PDF parameters       [units vary]
    ! Output Variable

    integer :: &
      k_lh_start  ! Starting None sample level
    ! Local Variables

    integer :: &
      k_lh_start_rcm_in_cloud, &
      k_lh_start_rcm

    real( kind = core_rknd ), dimension(nz) :: &
      rcm_pdf, cloud_frac_pdf
  !-----------------------------------------------------------------------
    !----- Begin Code -----


    if ( l_rcm_in_cloud_k_lh_start .or. l_random_k_lh_start ) then
      rcm_pdf = compute_mean_binormal( pdf_params%rc_1, pdf_params%rc_2, pdf_params%mixt_frac )
      cloud_frac_pdf = compute_mean_binormal( pdf_params%cloud_frac_1, pdf_params%cloud_frac_2, &
                                              pdf_params%mixt_frac )
      k_lh_start_rcm_in_cloud = maxloc( rcm_pdf / max( cloud_frac_pdf, cloud_frac_min ), 1 )
    end if

    if ( .not. l_rcm_in_cloud_k_lh_start .or. l_random_k_lh_start ) then
      k_lh_start_rcm    = maxloc( rcm, 1 )
    end if

    if ( l_random_k_lh_start ) then
      if ( k_lh_start_rcm_in_cloud == k_lh_start_rcm ) then
        k_lh_start = k_lh_start_rcm
      else
        ! Pick a random height level between k_lh_start_rcm and
        ! k_lh_start_rcm_in_cloud
        if ( k_lh_start_rcm_in_cloud > k_lh_start_rcm ) then
          k_lh_start = rand_integer_in_range( k_lh_start_rcm, k_lh_start_rcm_in_cloud )
        else if ( k_lh_start_rcm > k_lh_start_rcm_in_cloud ) then
          k_lh_start = rand_integer_in_range( k_lh_start_rcm_in_cloud, k_lh_start_rcm )
        end if
      end if
    else if ( l_rcm_in_cloud_k_lh_start ) then
      k_lh_start = k_lh_start_rcm_in_cloud
    else ! .not. l_random_k_lh_start .and. .not. l_rcm_in_cloud_k_lh_start
      k_lh_start = k_lh_start_rcm
    end if
    ! If there's no cloud k_lh_start appears to end up being 1. Check if
    ! k_lh_start is 1 or nz and set it to the middle of the domain in that
    ! case.

    if ( k_lh_start == nz .or. k_lh_start == 1 ) then
      k_lh_start = nz / 2
    end if

    return
  end function compute_k_lh_start
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
!-------------------------------------------------------------------------------


  subroutine assert_consistent_cloud_frac( pdf_params, l_error )
  ! Description:
  !   Performs an assertion check that cloud_frac_i is consistent with chi_i and
  !   stdev_chi_i in pdf_params for each PDF component.
  ! References:
  !   Eric Raut
  !-----------------------------------------------------------------------


      USE constants_clubb, ONLY: fstderr 

      USE pdf_parameter_module, ONLY: pdf_parameter 
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 

    implicit none
    ! Input Variables

    type(pdf_parameter), intent(in) :: &
      pdf_params       ! PDF parameters, containing distribution of chi     [units vary]
                       ! and cloud fraction
    ! Output Variables

    logical, intent(out) :: &
      l_error          ! True if the assertion check fails
    ! Local Variables

    logical :: &
      l_error_in_sub
  !-----------------------------------------------------------------------
    !----- Begin Code -----


    l_error = .false.
    ! Perform assertion check for PDF component 1

    call assert_consistent_cf_component &
         ( pdf_params%chi_1, pdf_params%stdev_chi_1, pdf_params%cloud_frac_1, & ! Intent(in)
           l_error_in_sub )                                                    ! Intent(out)

    l_error = l_error .or. l_error_in_sub
    if ( l_error_in_sub ) then
      write(fstderr,*) "Cloud fraction is inconsistent in PDF component 1"
    end if
    ! Perform assertion check for PDF component 2

    call assert_consistent_cf_component &
         ( pdf_params%chi_2, pdf_params%stdev_chi_2, pdf_params%cloud_frac_2, & ! Intent(in)
           l_error_in_sub )                                                    ! Intent(out)

    l_error = l_error .or. l_error_in_sub
    if ( l_error_in_sub ) then
      write(fstderr,*) "Cloud fraction is inconsistent in PDF component 2"
    end if

    return
  end subroutine assert_consistent_cloud_frac
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


  subroutine assert_consistent_cf_component( mu_chi_i, sigma_chi_i, cloud_frac_i, &
                                             l_error )
  ! Description:
  !   Performs an assertion check that cloud_frac_i is consistent with chi_i and
  !   stdev_chi_i for a PDF component.
  !   The None sample generation process relies on precisely a cloud_frac
  !   amount of mass in the cloudy portion of the PDF of chi, that is, where
  !   chi > 0. In other words, the probability that chi > 0 should be exactly
  !   cloud_frac.
  !   Stated even more mathematically, CDF_chi(0) = 1 - cloud_frac, where
  !   CDF_chi is the cumulative distribution function of chi. This can be
  !   expressed as invCDF_chi(1 - cloud_frac) = zero.
  !   This subroutine uses ltqnorm, which is apparently a fancy name for the
  !   inverse cumulative distribution function of the standard normal
  !   distribution.
  ! References:
  !   Eric Raut
  !-----------------------------------------------------------------------
    ! Included Modules

  !
  !
  !


      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: fstderr, zero, one, chi_tol, eps 


      USE transform_to_pdf_module, ONLY: ltqnorm 

    implicit none
    ! Local Constants

    real( kind = core_rknd ), parameter :: &
      ! Values below ltqnorm_min_arg (or above 1-ltqnorm_min_arg) will not be
      ! supplied as arguments to the ltqnorm function.
      ltqnorm_min_arg = 1.0e-5_core_rknd, &
      ! It will be verified that all values of the chi uniform variate will not
      ! trigger the in/out of cloud assertion error, except for those values
      ! within a box with the following half-width, centered around the cloud
      ! fraction in this component.
      box_half_width = 5.0e-6_core_rknd
      ! Values below ltqnorm_min_arg (or above 1-ltqnorm_min_arg) will not be
      ! supplied as arguments to the ltqnorm function.
      ! It will be verified that all values of the chi uniform variate will not
      ! trigger the in/out of cloud assertion error, except for those values
      ! within a box with the following half-width, centered around the cloud
      ! fraction in this component.
    ! Input Variables

    real( kind = core_rknd ), intent(in) :: &
      mu_chi_i,      & ! Mean of chi in a PDF component
      sigma_chi_i,   & ! Standard deviation of chi in a PDF component
      cloud_frac_i     ! Cloud fraction in a PDF component
    ! Output Variables

    logical, intent(out) :: &
      l_error          ! True if the assertion check fails
    ! Local Variables

    real( kind = core_rknd ) :: chi, chi_std_normal, one_minus_ltqnorm_arg
  !-----------------------------------------------------------------------
    !----- Begin Code -----


    l_error = .false.
    ! The calculation of PDF component cloud fraction (cloud_frac_i) in
    ! pdf_closure now sets cloud_frac_i to 0 when the special condition is met
    ! that both | mu_chi_i | <= eps and sigma_chi_i <= chi_tol.  This check
    ! should be omitted under such conditions.
    ! Note:  The conditions on | mu_chi_i | and sigma_chi_i need to match the
    !        conditions given in subroutine calc_cloud_frac_component in
    !        pdf_closure_module.F90 (a chi_at_sat of 0 can be used).  If those
    !        conditions are changed in that subroutine, they need to change here
    !        too.  Someday, a better solution might be to pass the value of
    !        pdf_description_i out of that part of the code and into this check.

    ! 
    if ( abs( mu_chi_i ) <= eps .and. sigma_chi_i <= chi_tol ) then
       ! Return without performing this check.
       return
    endif
    ! Check left end of box.

    one_minus_ltqnorm_arg = cloud_frac_i - box_half_width
    ! Do not bother to check this end of the box if it dips below the minimum
    ! ltqnorm argument.
    if ( one_minus_ltqnorm_arg >= ltqnorm_min_arg ) then

      if ( one_minus_ltqnorm_arg > (one - ltqnorm_min_arg) ) then
        one_minus_ltqnorm_arg = one - ltqnorm_min_arg
      end if

      chi_std_normal = ltqnorm( one - one_minus_ltqnorm_arg )
      chi = chi_std_normal * sigma_chi_i + mu_chi_i
      if ( chi <= zero ) then
        l_error = .true.
        write(fstderr,*) 'chi (left side of box) = ', chi
      end if
    end if ! one_minus_ltqnorm_arg >= ltqnorm_min_arg
    ! Check right end of box.

    one_minus_ltqnorm_arg = cloud_frac_i + box_half_width
    ! Do not bother to check this end of the box if it exceeds the maximum
    ! ltqnorm argument
    if ( one_minus_ltqnorm_arg <= one-ltqnorm_min_arg ) then

      if ( one_minus_ltqnorm_arg < ltqnorm_min_arg ) then
        one_minus_ltqnorm_arg = ltqnorm_min_arg
      end if

      chi_std_normal = ltqnorm( one - one_minus_ltqnorm_arg )
      chi = chi_std_normal * sigma_chi_i + mu_chi_i
      if ( chi > zero ) then
        l_error = .true.
        write(fstderr,*) 'chi (right side of box) = ', chi
      end if
    end if ! one_minus_ltqnorm_arg >= ltqnorm_min_arg

    if ( l_error ) then
      write(fstderr,*) "In assert_consistent_cf_component, cloud_frac_i is inconsistent with &
                       &mu_chi_i and stdev_chi_i."
      write(fstderr,*) "mu_chi_i = ", mu_chi_i
      write(fstderr,*) "sigma_chi_i = ", sigma_chi_i
      write(fstderr,*) "cloud_frac_i = ", cloud_frac_i
    end if

    return
  end subroutine assert_consistent_cf_component
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine assert_correct_cloud_normal( num_samples, X_u_chi, X_nl_chi, X_mixt_comp, &
                                          cloud_frac_1, cloud_frac_2, &
                                          l_error )
  ! Description:
  !   Asserts that all None sample points that are in cloud in uniform space
  !   are in cloud in normal space, and that all None sample points that are
  !   in clear air in uniform space are in clear air in normal space.
  ! References:
  !   None
  !-----------------------------------------------------------------------
    ! Included Modules

  
  
      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: one, fstderr 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      num_samples            ! Number of None sample points

    real( kind = core_rknd ), dimension(num_samples), intent(in) :: &
      X_u_chi,  &            ! Samples of chi in uniform space
      X_nl_chi               ! Samples of chi in normal space

    integer, dimension(num_samples), intent(in) :: &
      X_mixt_comp            ! PDF component of each sample

    real( kind = core_rknd ), intent(in) :: &
      cloud_frac_1,   &      ! Cloud fraction in PDF component 1
      cloud_frac_2           ! Cloud fraction in PDF component 2
    ! Output Variables

    logical, intent(out) :: &
      l_error                ! True if the assertion check fails
    ! Local Constants

    real( kind = core_rknd ), parameter :: &
      error_threshold = 1000._core_rknd * epsilon( X_nl_chi ) ! A threshold to determine whether a
                                                              ! rogue value triggers the assertion
                                                              ! check.
    ! Local Variables

    real( kind = core_rknd ) :: &
      cloud_frac_i

    integer :: sample
  !-----------------------------------------------------------------------
    !----- Begin Code -----


    l_error = .false.

    do sample = 1, num_samples, 1
      ! Determine the appropriate cloud fraction

      if ( X_mixt_comp(sample) == 1 ) then
        cloud_frac_i = cloud_frac_1
      else if ( X_mixt_comp(sample) == 2 ) then
        cloud_frac_i = cloud_frac_2
      end if

      if ( X_u_chi(sample) < (one - cloud_frac_i) ) then
        ! The uniform sample is in clear air

        if ( X_nl_chi(sample) > error_threshold ) then
          l_error = .true.
        end if

      else if ( X_u_chi(sample) >= (one - cloud_frac_i) .and. &
                X_u_chi(sample) < one ) then
        ! The uniform sample is in cloud

        if ( X_nl_chi(sample) <= -error_threshold ) then
          l_error = .true.
        end if

      else
        stop "X_u_chi not in correct range in assert_correct_cloud_normal"
      end if

    end do ! 1..num_samples

    if ( l_error ) then
      write(fstderr,*) "In assert_correct_cloud_normal:"
      write(fstderr,*) "The 'cloudiness' of points in uniform and normal space is not consistent"
      write(fstderr,'(4X,A,A)')  "X_u_chi         ", "X_nl_chi "
      do sample = 1, num_samples, 1
        write(fstderr,'(I4,2G20.4)') &
          sample, X_u_chi(sample), X_nl_chi(sample)
      end do
      ! This will hopefully stop the run at some unknown point in the future
      l_error = .true.
    end if  ! in_cloud_points /= out_of_cloud_points

    return
  end subroutine assert_correct_cloud_normal
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!----------------------------------------------------------------------

  elemental function in_mixt_comp_1( X_u_dp1_element, frac )
! Description:
!   Determine if we're in mixture component 1
! References:
!   None
!----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

    implicit none

    real(kind=core_rknd), intent(in) :: &
      X_u_dp1_element, & ! Element of X_u telling us which mixture component we're in
      frac               ! The mixture fraction

    logical :: in_mixt_comp_1
    ! ---- Begin Code ----


    if ( X_u_dp1_element < frac ) then
      in_mixt_comp_1 = .true.
    else
      in_mixt_comp_1 = .false.
    end if

    return
  end function in_mixt_comp_1
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  elemental function in_precipitation( rnd, precip_frac ) result( l_in_precip )
  ! Description:
  !   Determines if a sample is in precipitation
  ! References:
  !   None
  !-----------------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind=core_rknd ), intent(in) :: &
      rnd, &         ! Random number between 0 and 1
      precip_frac    ! Precipitation fraction
    ! Output Variable

    logical :: &
      l_in_precip    ! Whether the sample is in precipitation
  !-----------------------------------------------------------------------
    !----- Begin Code -----


    if ( rnd < precip_frac ) then
      l_in_precip = .true.
    else
      l_in_precip = .false.
    end if

    return
  end function in_precipitation
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine compute_arb_overlap( nz, k_lh_start, &
                                  vert_corr, &
                                  X_u_one_var_all_levs )
! Description:
!   Re-computes X_u (uniform sample) for a single variate (e.g. chi) using
!   an arbitrary correlation specified by the input vert_corr variable (which
!   can vary with height).
!   This is an improved algorithm that doesn't require us to convert from a
!   unifrom distribution to a Gaussian distribution and back again.
! References:
!   None
!-------------------------------------------------------------------------------


      USE generate_uniform_sample_module, ONLY: rand_uniform_real 

      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: zero, one, two, fstderr 

    implicit none
    ! Parameter Constants
    ! Input Variables


    integer, intent(in) :: &
      nz,      & ! Number of vertical levels [-]
      k_lh_start   ! Starting k level          [-]

    real(kind=core_rknd), dimension(nz), intent(in) :: &
      vert_corr ! Vertical correlation between k points in range [0,1]   [-]
    ! Output Variables

    real(kind=core_rknd), dimension(nz), intent(inout) :: &
      X_u_one_var_all_levs ! Uniform distribution of 1 variate at all levels [-]
                           ! The value of this variate at k_lh_start should already be populated
                           ! in this array and will be used to fill in the other levels.
    ! Local Variables

    real(kind=core_rknd) :: rand, min_val, half_width, offset, unbounded_point

    integer :: k, kp1, km1 ! Loop iterators
    ! ---- Begin Code ----
    ! Upwards loop


    do k = k_lh_start, nz-1

      kp1 = k+1 ! This is the level we're computing

      if ( vert_corr(kp1) < zero .or. vert_corr(kp1) > one ) then
        write(fstderr,*) "vert_corr(kp1) not between 0 and 1"
        write(fstderr,*) "vert_corr(",kp1,") = ", vert_corr(kp1)
        stop "Fatal error in compute_arb_overlap (SILHS)"
      end if

      half_width = one - vert_corr(kp1)
      min_val = X_u_one_var_all_levs(k) - half_width

      rand = rand_uniform_real( )
      offset = two * half_width * rand

      unbounded_point = min_val + offset
      ! If unbounded_point lies outside the range [0,1],
      ! fold it back so that it is between [0,1]

      if ( unbounded_point > one ) then
        X_u_one_var_all_levs(kp1) = two - unbounded_point
      else if ( unbounded_point < zero ) then
        X_u_one_var_all_levs(kp1) = - unbounded_point
      else
        X_u_one_var_all_levs(kp1) = unbounded_point
      end if

    end do ! k_lh_start..nz-1
    ! Downwards loop

    do k = k_lh_start, 2, -1

      km1 = k-1 ! This is the level we're computing

      if ( vert_corr(km1) < zero .or. vert_corr(km1) > one ) then
        stop "vert_corr(km1) not between 0 and 1"
      end if

      half_width = one - vert_corr(km1)
      min_val = X_u_one_var_all_levs(k) - half_width

      rand = rand_uniform_real( )
      offset = two * half_width * rand

      unbounded_point = min_val + offset
      ! If unbounded_point lies outside the range [0,1],
      ! fold it back so that it is between [0,1]

      if ( unbounded_point > one ) then
        X_u_one_var_all_levs(km1) = two - unbounded_point
      else if ( unbounded_point < zero ) then
        X_u_one_var_all_levs(km1) = - unbounded_point
      else
        X_u_one_var_all_levs(km1) = unbounded_point
      end if

    end do ! k_lh_start..2 decrementing

    return
  end subroutine compute_arb_overlap
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

  function compute_vert_corr( nz, delta_zm, Lscale_vert_avg, rcm ) result( vert_corr )
! Description:
!   This function computes the vertical correlation for arbitrary overlap, using
!   density weighted 3pt averaged Lscale and the difference in height levels
!   (delta_zm).
! References:
!   None
!-------------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: rc_tol, one 

      USE parameters_silhs, ONLY: l_max_overlap_in_cloud, vert_decorr_coef 

    implicit none
    ! External

    intrinsic :: exp
    ! Input Variables

    integer, intent(in) :: &
      nz ! Number of vertical levels  [-]

    real( kind = core_rknd ), intent(in), dimension(nz) :: &
      delta_zm, &        ! Difference between altitudes    [m]
      Lscale_vert_avg, & ! Vertically averaged Lscale      [m]
      rcm                ! Cloud water mixing ratio        [kg/kg]
    ! Output Variable

    real( kind = core_rknd ), dimension(nz) :: &
      vert_corr ! The vertical correlation      [-]
    ! ---- Begin Code ----

    vert_corr(1:nz) = exp( -vert_decorr_coef &
                            * ( delta_zm(1:nz) / Lscale_vert_avg(1:nz) ) )

    if ( l_max_overlap_in_cloud ) then
      where ( rcm > rc_tol )
        vert_corr = one
      end where
    end if

    return
  end function compute_vert_corr
!-------------------------------------------------------------------------------


  !-----------------------------------------------------------------------

  subroutine stats_accumulate_uniform_lh( nz, num_samples, l_in_precip_all_levs, &
                                          X_mixt_comp_all_levs, X_u_chi_all_levs, pdf_params, &
                                          lh_sample_point_weights, k_lh_start )
  ! Description:
  !   Samples statistics that cannot be deduced from the normal-lognormal
  !   None sample (X_nl_all_levs)
  ! References:
  !   None
  !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE stats_type_utilities, ONLY: stat_update_var, stat_update_var_pt 

      USE stats_variables, ONLY: l_stats_samp, ilh_precip_frac, ilh_mixt_frac, ilh_precip_frac_unweighted, &
      &ilh_mixt_frac_unweighted, ik_lh_start, ilh_samp_frac_category, stats_lh_zt, stats_lh_sfc 

      USE math_utilities, ONLY: compute_sample_mean 

      USE constants_clubb, ONLY: one, zero 

      USE pdf_parameter_module, ONLY: pdf_parameter 

      USE silhs_importance_sample_module, ONLY: importance_category_type, num_importance_categories, define_importance_categories 
      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter 
      USE silhs_importance_sample_module, ONLY: kr_kgen_silhs_importance_sample_module_typesubp1 
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter 
      USE silhs_importance_sample_module, ONLY: kv_kgen_silhs_importance_sample_module_typesubp1 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      nz, &         ! Number of vertical levels
      num_samples ! Number of None sample points

    logical, dimension(nz,num_samples), intent(in) :: &
      l_in_precip_all_levs ! Boolean variables indicating whether a sample is in
                           ! precipitation at a given height level

    integer, dimension(nz,num_samples), intent(in) :: &
      X_mixt_comp_all_levs ! Integers indicating which mixture component a
                           ! sample is in at a given height level

    real( kind = core_rknd ), dimension(nz,num_samples), intent(in) :: &
      X_u_chi_all_levs     ! Uniform value of chi

    type(pdf_parameter), dimension(nz), intent(in) :: &
      pdf_params           ! The official PDF parameters!

    real( kind = core_rknd ), dimension(nz,num_samples), intent(in) :: &
      lh_sample_point_weights ! The weight of each sample

    integer, intent(in) :: &
      k_lh_start           ! Vertical level for sampling preferentially within       [-]
                           ! cloud
    ! Local Variables

    type(importance_category_type), dimension(num_importance_categories) :: &
      importance_categories

    real( kind = core_rknd ), dimension(nz) :: &
      lh_precip_frac, &
      lh_mixt_frac

    real( kind = core_rknd ), dimension(nz,num_samples) :: &
      int_in_precip, & ! '1' for samples in precipitation, '0' otherwise
      int_mixt_comp    ! '1' for samples in the first PDF component, '0' otherwise

    real( kind = core_rknd ), dimension(num_samples) :: &
      one_weights

    real( kind = core_rknd ), dimension(nz,num_importance_categories) :: &
      lh_samp_frac

    real( kind = core_rknd ) :: &
      cloud_frac_i

    logical :: &
      l_in_cloud, &
      l_in_comp_1

    integer, dimension(num_importance_categories) :: &
      category_counts  ! Count of number of samples in each importance category

    integer :: k, isample, icategory
  !-----------------------------------------------------------------------
    !----- Begin Code -----

    if ( l_stats_samp ) then
      ! Estimate of lh_precip_frac
      if ( ilh_precip_frac > 0 ) then
        where ( l_in_precip_all_levs )
          int_in_precip = 1.0_core_rknd
        else where
          int_in_precip = 0.0_core_rknd
        end where
        lh_precip_frac(:) = compute_sample_mean( nz, num_samples, lh_sample_point_weights, &
                                                 int_in_precip )
        call stat_update_var( ilh_precip_frac, lh_precip_frac, stats_lh_zt )
      end if
      ! Unweighted estimate of lh_precip_frac

      if ( ilh_precip_frac_unweighted > 0 ) then
        where ( l_in_precip_all_levs )
          int_in_precip = 1.0_core_rknd
        else where
          int_in_precip = 0.0_core_rknd
        end where
        one_weights = one
        lh_precip_frac(:) = compute_sample_mean( nz, num_samples, one_weights, &
                                                 int_in_precip )
        call stat_update_var( ilh_precip_frac_unweighted, lh_precip_frac, stats_lh_zt )
      end if
      ! Estimate of lh_mixt_frac

      if ( ilh_mixt_frac > 0 ) then
        where ( X_mixt_comp_all_levs == 1 )
          int_mixt_comp = 1.0_core_rknd
        else where
          int_mixt_comp = 0.0_core_rknd
        end where
        lh_mixt_frac(:) = compute_sample_mean( nz, num_samples, lh_sample_point_weights, &
                                               int_mixt_comp )
        call stat_update_var( ilh_mixt_frac, lh_mixt_frac, stats_lh_zt )
      end if
      ! Unweighted estimate of lh_mixt_frac

      if ( ilh_mixt_frac_unweighted > 0 ) then
        where ( X_mixt_comp_all_levs == 1 )
          int_mixt_comp = 1.0_core_rknd
        else where
          int_mixt_comp = 0.0_core_rknd
        end where
        one_weights = one
        lh_mixt_frac(:) = compute_sample_mean( nz, num_samples, one_weights, &
                                               int_mixt_comp )
        call stat_update_var( ilh_mixt_frac_unweighted, lh_mixt_frac, stats_lh_zt )
      end if
      ! k_lh_start is an integer, so it would be more appropriate to sample it
      ! as an integer, but as far as I can tell our current sampling
      ! infrastructure mainly supports sampling real numbers.

      call stat_update_var_pt( ik_lh_start, 1, real( k_lh_start, kind=core_rknd ), stats_lh_sfc )

      if ( allocated( ilh_samp_frac_category ) ) then
        if ( ilh_samp_frac_category(1) > 0 ) then

          importance_categories = define_importance_categories( )

          do k=1, nz
            category_counts(:) = 0

            do isample=1, num_samples

              if ( X_mixt_comp_all_levs(k,isample) == 1 ) then
                l_in_comp_1 = .true.
                cloud_frac_i = pdf_params(k)%cloud_frac_1
              else
                l_in_comp_1 = .false.
                cloud_frac_i = pdf_params(k)%cloud_frac_2
              end if

              l_in_cloud = X_u_chi_all_levs(k,isample) > (one - cloud_frac_i)

              do icategory=1, num_importance_categories
                if ( (l_in_cloud .eqv. importance_categories(icategory)%l_in_cloud) .and. &
                     (l_in_precip_all_levs(k,isample) .eqv. importance_categories(icategory)%&
                                                           l_in_precip) .and. &
                     (l_in_comp_1 .eqv. importance_categories(icategory)%l_in_component_1) ) then

                  category_counts(icategory) = category_counts(icategory) + 1
                  exit

                end if
              end do

            end do ! isample=1, num_samples

            do icategory=1, num_importance_categories
              lh_samp_frac(k,icategory) = real( category_counts(icategory), kind=core_rknd ) / &
                                          real( num_samples, kind=core_rknd )
            end do

          end do ! k=2, nz
          ! Microphysics is not run at lower level

          lh_samp_frac(1,:) = zero

          do icategory=1, num_importance_categories
            call stat_update_var( ilh_samp_frac_category(icategory), lh_samp_frac(:,icategory), &
                                  stats_lh_zt )
          end do ! icategory=1, num_importance_categories

        end if ! ilh_samp_frac_category(1) > 0
      end if ! allocated( ilh_samp_frac_category )

    end if ! l_stats_samp

    return
  end subroutine stats_accumulate_uniform_lh
  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------


end module latin_hypercube_driver_module
