!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: assim_tools_mod.f90 12812 2018-09-05 16:25:02Z nancy@ucar.edu $
!>  A variety of operations required by assimilation.


!

module assim_tools_mod
!> \defgroup assim_tools assim_tools_mod
!> 
!> @{

    USE types_mod, ONLY: r8, i8, digits12, missing_r8 


   


    USE cov_cutoff_mod, ONLY: comp_cov_factor 

    USE reg_factor_mod, ONLY: comp_reg_factor 


    USE sampling_error_correction_mod, ONLY: read_sampling_error_correction 

    USE location_mod, ONLY: location_type 

    USE ensemble_manager_mod, ONLY: ensemble_type 

    USE mpi_utilities_mod, ONLY: my_task_id, start_mpi_timer, read_mpi_timer, task_sync 

    USE adaptive_inflate_mod, ONLY: update_inflation, adaptive_inflate_type 

    USE time_manager_mod, ONLY: time_type 


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE location_mod, ONLY: kr_location_mod_location_type 
    USE ensemble_manager_mod, ONLY: kr_ensemble_manager_mod_ensemble_type 
    USE adaptive_inflate_mod, ONLY: kr_kgen_adaptive_inflate_mod_typesubp0 
    USE time_manager_mod, ONLY: kr_time_manager_mod_time_type 
    USE location_mod, ONLY: kv_location_mod_location_type 
    USE ensemble_manager_mod, ONLY: kv_ensemble_manager_mod_ensemble_type 
    USE adaptive_inflate_mod, ONLY: kv_kgen_adaptive_inflate_mod_typesubp0 
    USE time_manager_mod, ONLY: kv_time_manager_mod_time_type 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC filter_assim 
! Indicates if module initialization subroutine has been called yet


! True if random sequence needs to be initialized


real(r8), parameter    :: small = epsilon(1.0_r8)   ! threshold for avoiding NaNs/Inf
! true if we have multiple vert choices and we're doing vertical localization
! (make it a local variable so we don't keep making subroutine calls)


! Need to read in table for off-line based sampling correction and store it

integer                :: sec_table_size
real(r8), allocatable  :: exp_true_correl(:), alpha(:)
! if adjust_obs_impact is true, read in triplets from the ascii file
! and fill this 2d impact table. 

real(r8), allocatable  :: obs_impact_table(:,:)
! version controlled file description for error handling, do not edit

!============================================================================
!---- namelist with default values
! Filter kind selects type of observation space filter
!      1 = EAKF filter
!      2 = ENKF
!      3 = Kernel filter
!      4 = particle filter
!      5 = random draw from posterior
!      6 = deterministic draw from posterior with fixed kurtosis
!      8 = Rank Histogram Filter (see Anderson 2011)
!  special_localization_obs_types -> Special treatment for the specified observation types
!  special_localization_cutoffs   -> Different cutoff value for each specified obs type


!
!
logical  :: spread_restoration              = .false.
logical  :: sampling_error_correction       = .false.
! since this is in the namelist, it has to have a fixed size.


! Following only relevant for filter_kind = 8

! False by default; if true, expect to read in an ascii table
! to adjust the impact of obs on other state vector and obs values.

logical            :: adjust_obs_impact  = .false.
! These next two only affect models with multiple options
! for vertical localization:
! "convert_state" is false by default; it depends on the model
! what is faster - do the entire state up front and possibly
! do unneeded work, or do the conversion during the assimilation
! loop. we think this depends heavily on how much of the state
! is going to be adjusted by the obs.  for a global model
! we think false may be better; for a regional model with
! a lot of obs and full coverage true may be better.
! "convert_obs" is true by default; in general it seems to
! be better for each task to convert the obs vertical before
! going into the loop but again this depends on how many
! obs per task and whether the mean is distributed or 
! replicated on each task.

!
!
! Not in the namelist; this var disables the experimental
! linear and spherical case code in the adaptive localization 
! sections.  to try out the alternatives, set this to .false.

! Option to distribute the mean.  If 'false' each task will have a full
! copy of the ensemble mean, which speeds models doing vertical conversion.
! If 'true' the mean will be spread across all tasks which reduces the
! memory needed per task but requires communication if the mean is used
! for vertical conversion.  We have changed the default to be .false.
! compared to previous versions of this namelist item.


!============================================================================
PUBLIC kr_externs_in_assim_tools_mod 
PUBLIC kr_externs_out_assim_tools_mod 
#ifdef _MPI 
include "mpif.h" 
#endif 
  
PUBLIC kr_location_mod_location_type 
PUBLIC kr_ensemble_manager_mod_ensemble_type 
PUBLIC kr_kgen_adaptive_inflate_mod_typesubp0 
PUBLIC kr_time_manager_mod_time_type 
PUBLIC kv_location_mod_location_type 
PUBLIC kv_ensemble_manager_mod_ensemble_type 
PUBLIC kv_kgen_adaptive_inflate_mod_typesubp0 
PUBLIC kv_time_manager_mod_time_type 


contains
!-------------------------------------------------------------


!-------------------------------------------------------------


SUBROUTINE filter_assim(kgen_unit, kgen_measure, kgen_measure_int, kgen_isverified, kgen_filepath, ens_handle, ens_size, num_groups, inflate, &
&ens_inf_copy, ens_sd_copy, ens_inf_sd_copy, inflate_only) 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE kgen_utils_mod, ONLY: kgen_perturb_real 
    USE mpi_utilities_mod, ONLY: kr_externs_out_mpi_utilities_mod 
    USE utilities_mod, ONLY: kr_externs_out_utilities_mod 
    USE time_manager_mod, ONLY: kr_externs_out_time_manager_mod 
    USE cov_cutoff_mod, ONLY: kr_externs_out_cov_cutoff_mod 
    USE sampling_error_correction_mod, ONLY: kr_externs_out_sampling_error_correction_mod 
    USE reg_factor_mod, ONLY: kr_externs_out_reg_factor_mod 
    USE adaptive_inflate_mod, ONLY: kr_externs_out_adaptive_inflate_mod 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_init_verify, kgen_tolerance, kgen_minvalue, kgen_verboselevel, &
    &CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
    USE utilities_mod, ONLY: kv_externs_utilities_mod 
    USE time_manager_mod, ONLY: kv_externs_time_manager_mod 
    USE cov_cutoff_mod, ONLY: kv_externs_cov_cutoff_mod 
    USE sampling_error_correction_mod, ONLY: kv_externs_sampling_error_correction_mod 
    USE reg_factor_mod, ONLY: kv_externs_reg_factor_mod 

    TYPE(ensemble_type), INTENT(INOUT) :: ens_handle 
    INTEGER, INTENT(INOUT) :: ens_size, num_groups 
    TYPE(adaptive_inflate_type), INTENT(INOUT) :: inflate 
    INTEGER, INTENT(INOUT) :: ens_sd_copy, ens_inf_copy 
    INTEGER, INTENT(INOUT) :: ens_inf_sd_copy 
    LOGICAL, INTENT(INOUT) :: inflate_only 
! changed the ensemble sized things here to allocatable


    REAL(KIND=r8) :: obs_prior(ens_size), obs_inc(ens_size), increment(ens_size) 
    REAL(KIND=r8) :: reg_factor, impact_factor 
    REAL(KIND=r8) :: net_a(num_groups), reg_coef(num_groups), correl(num_groups) 
    REAL(KIND=r8) :: cov_factor, obs(1), obs_err_var 
    REAL(KIND=r8) :: varying_ss_inflate, varying_ss_inflate_sd 
    REAL(KIND=r8) :: ss_inflate_base, cutoff_rev 
    REAL(KIND=r8) :: gamma, ens_obs_mean, ens_obs_var, ens_var_deflate 
    REAL(KIND=r8) :: r_mean, r_var 
    REAL(KIND=r8) :: orig_obs_prior_mean(num_groups), orig_obs_prior_var(num_groups) 
    REAL(KIND=r8) :: obs_prior_mean(num_groups), obs_prior_var(num_groups) 
    REAL(KIND=r8) :: diff_sd, outlier_ratio 
    REAL(KIND=r8), allocatable :: close_state_dist(:) 

    INTEGER(KIND=i8) :: state_index 
    INTEGER(KIND=i8), allocatable :: my_state_indx(:) 

    INTEGER :: i, j 
    INTEGER :: grp_beg(num_groups), grp_end(num_groups), grp_size, grp_bot, grp_top, group 
    INTEGER :: num_close_states 
    INTEGER :: base_obs_type 
    INTEGER, allocatable :: close_state_ind(:) 
    INTEGER, allocatable :: my_state_kind(:) 


    TYPE(location_type) :: base_obs_loc 
    TYPE(location_type), allocatable :: my_state_loc(:) 

    TYPE(time_type) :: obs_time 

    LOGICAL :: do_adapt_inf_update 
    LOGICAL :: allow_missing_in_state 
    LOGICAL :: local_varying_ss_inflate 
! timing related vars:
! set timing(N) true to collect and print timing info

integer, parameter :: Ntimers = 5
integer, parameter :: LG_GRN = 2  ! large section timings
integer, parameter :: SM_GRN = 3  ! inner loops - use carefully!
LOGICAL :: timing(ntimers) 
REAL(KIND=digits12) :: t_base(ntimers) 
INTEGER(KIND=i8) :: t_items(ntimers) 
INTEGER(KIND=i8) :: t_limit(ntimers) 

! timing disabled by default
INTEGER, INTENT(IN) :: kgen_unit 
REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_measure 
integer, intent(out) :: kgen_measure_int
LOGICAL, INTENT(OUT) :: kgen_isverified 
CHARACTER(LEN=*), INTENT(IN) :: kgen_filepath 
LOGICAL :: kgen_istrue 
REAL(KIND=8) :: kgen_array_sum 
INTEGER :: kgen_intvar, kgen_ierr 
INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke 
LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage 
COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage 
INTEGER, PARAMETER :: KGEN_MAXITER = 30
  
TYPE(check_t) :: check_status 
INTEGER*8 :: kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
REAL(KIND=kgen_dp) :: gkgen_measure 
TYPE(ensemble_type) :: kgenref_ens_handle 
REAL(KIND=r8), dimension(ens_size) :: kgenref_increment 
REAL(KIND=r8) :: kgenref_impact_factor 
REAL(KIND=r8) :: kgenref_reg_factor 
REAL(KIND=r8), dimension(num_groups) :: kgenref_reg_coef 
REAL(KIND=r8), dimension(num_groups) :: kgenref_net_a 
REAL(KIND=r8), dimension(num_groups) :: kgenref_correl 
REAL(KIND=r8) :: kgenref_cov_factor 
REAL(KIND=r8) :: kgenref_varying_ss_inflate 
REAL(KIND=r8) :: kgenref_varying_ss_inflate_sd 
REAL(KIND=r8) :: kgenref_ss_inflate_base 
REAL(KIND=r8) :: kgenref_gamma 
REAL(KIND=r8) :: kgenref_ens_obs_mean 
REAL(KIND=r8) :: kgenref_ens_obs_var 
REAL(KIND=r8) :: kgenref_ens_var_deflate 
REAL(KIND=r8) :: kgenref_r_var 
REAL(KIND=r8) :: kgenref_r_mean 
REAL(KIND=r8) :: kgenref_diff_sd 
REAL(KIND=r8) :: kgenref_outlier_ratio 
INTEGER(KIND=i8) :: kgenref_state_index 
INTEGER :: kgenref_j 
INTEGER :: kgenref_group 
INTEGER :: kgenref_grp_bot 
INTEGER :: kgenref_grp_top 
LOGICAL :: kgenref_do_adapt_inf_update 
REAL(KIND=digits12), dimension(ntimers) :: kgenref_t_base 
INTEGER(KIND=i8), dimension(ntimers) :: kgenref_t_items 
  
!parent block preprocessing 
  
#ifdef _MPI 
call mpi_comm_rank(mpi_comm_world, kgen_mpirank, kgen_ierr) 
#else 
kgen_mpirank = 0 
#endif 
  
  
!local input variables 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) obs_prior 
    CALL kgen_array_sumcheck("obs_prior", kgen_array_sum, DBLE(SUM(obs_prior, mask=(obs_prior .eq. obs_prior))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) obs_inc 
    CALL kgen_array_sumcheck("obs_inc", kgen_array_sum, DBLE(SUM(obs_inc, mask=(obs_inc .eq. obs_inc))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) increment 
    CALL kgen_array_sumcheck("increment", kgen_array_sum, DBLE(SUM(increment, mask=(increment .eq. increment))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) impact_factor 
READ (UNIT = kgen_unit) reg_factor 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) reg_coef 
    CALL kgen_array_sumcheck("reg_coef", kgen_array_sum, DBLE(SUM(reg_coef, mask=(reg_coef .eq. reg_coef))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) net_a 
    CALL kgen_array_sumcheck("net_a", kgen_array_sum, DBLE(SUM(net_a, mask=(net_a .eq. net_a))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) correl 
    CALL kgen_array_sumcheck("correl", kgen_array_sum, DBLE(SUM(correl, mask=(correl .eq. correl))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) cov_factor 
READ (UNIT = kgen_unit) obs_err_var 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) obs 
    CALL kgen_array_sumcheck("obs", kgen_array_sum, DBLE(SUM(obs, mask=(obs .eq. obs))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) varying_ss_inflate 
READ (UNIT = kgen_unit) varying_ss_inflate_sd 
READ (UNIT = kgen_unit) cutoff_rev 
READ (UNIT = kgen_unit) ss_inflate_base 
READ (UNIT = kgen_unit) gamma 
READ (UNIT = kgen_unit) ens_obs_var 
READ (UNIT = kgen_unit) ens_var_deflate 
READ (UNIT = kgen_unit) ens_obs_mean 
READ (UNIT = kgen_unit) r_var 
READ (UNIT = kgen_unit) r_mean 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) orig_obs_prior_mean 
    CALL kgen_array_sumcheck("orig_obs_prior_mean", kgen_array_sum, DBLE(SUM(orig_obs_prior_mean, mask=(orig_obs_prior_mean .eq. &
    &orig_obs_prior_mean))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) orig_obs_prior_var 
    CALL kgen_array_sumcheck("orig_obs_prior_var", kgen_array_sum, DBLE(SUM(orig_obs_prior_var, mask=(orig_obs_prior_var .eq. &
    &orig_obs_prior_var))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) obs_prior_mean 
    CALL kgen_array_sumcheck("obs_prior_mean", kgen_array_sum, DBLE(SUM(obs_prior_mean, mask=(obs_prior_mean .eq. &
    &obs_prior_mean))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) obs_prior_var 
    CALL kgen_array_sumcheck("obs_prior_var", kgen_array_sum, DBLE(SUM(obs_prior_var, mask=(obs_prior_var .eq. obs_prior_var))), &
    &.TRUE.) 
END IF   
READ (UNIT = kgen_unit) diff_sd 
READ (UNIT = kgen_unit) outlier_ratio 
CALL kr_filter_assim_real__r8_dim1(close_state_dist, kgen_unit, "close_state_dist", .FALSE.) 
READ (UNIT = kgen_unit) state_index 
CALL kr_filter_assim_integer__i8_dim1(my_state_indx, kgen_unit, "my_state_indx", .FALSE.) 
READ (UNIT = kgen_unit) j 
READ (UNIT = kgen_unit) i 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) grp_beg 
    CALL kgen_array_sumcheck("grp_beg", kgen_array_sum, DBLE(SUM(grp_beg, mask=(grp_beg .eq. grp_beg))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) group 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) grp_end 
    CALL kgen_array_sumcheck("grp_end", kgen_array_sum, DBLE(SUM(grp_end, mask=(grp_end .eq. grp_end))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) grp_bot 
READ (UNIT = kgen_unit) grp_top 
READ (UNIT = kgen_unit) grp_size 
READ (UNIT = kgen_unit) num_close_states 
READ (UNIT = kgen_unit) base_obs_type 
CALL kr_filter_assim_integer___dim1(close_state_ind, kgen_unit, "close_state_ind", .FALSE.) 
CALL kr_filter_assim_integer___dim1(my_state_kind, kgen_unit, "my_state_kind", .FALSE.) 
CALL kr_location_mod_location_type(base_obs_loc, kgen_unit, "base_obs_loc", .FALSE.) 
CALL kr_kgen_filter_assim_subp3(my_state_loc, kgen_unit, "my_state_loc", .FALSE.) 
CALL kr_time_manager_mod_time_type(obs_time, kgen_unit, "obs_time", .FALSE.) 
READ (UNIT = kgen_unit) do_adapt_inf_update 
READ (UNIT = kgen_unit) allow_missing_in_state 
READ (UNIT = kgen_unit) local_varying_ss_inflate 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) timing 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) t_base 
    CALL kgen_array_sumcheck("t_base", kgen_array_sum, DBLE(SUM(t_base, mask=(t_base .eq. t_base))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) t_items 
    CALL kgen_array_sumcheck("t_items", kgen_array_sum, DBLE(SUM(t_items, mask=(t_items .eq. t_items))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) t_limit 
    CALL kgen_array_sumcheck("t_limit", kgen_array_sum, DBLE(SUM(t_limit, mask=(t_limit .eq. t_limit))), .TRUE.) 
END IF   
  
!extern output variables 
CALL kr_externs_out_assim_tools_mod(kgen_unit) 
CALL kr_externs_out_mpi_utilities_mod(kgen_unit) 
CALL kr_externs_out_utilities_mod(kgen_unit) 
CALL kr_externs_out_time_manager_mod(kgen_unit) 
CALL kr_externs_out_cov_cutoff_mod(kgen_unit) 
CALL kr_externs_out_sampling_error_correction_mod(kgen_unit) 
CALL kr_externs_out_reg_factor_mod(kgen_unit) 
CALL kr_externs_out_adaptive_inflate_mod(kgen_unit) 
  
!local output variables 
CALL kr_ensemble_manager_mod_ensemble_type(kgenref_ens_handle, kgen_unit, "kgenref_ens_handle", .FALSE.) 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_increment 
    CALL kgen_array_sumcheck("kgenref_increment", kgen_array_sum, DBLE(SUM(kgenref_increment, mask=(kgenref_increment .eq. &
    &kgenref_increment))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgenref_impact_factor 
READ (UNIT = kgen_unit) kgenref_reg_factor 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_reg_coef 
    CALL kgen_array_sumcheck("kgenref_reg_coef", kgen_array_sum, DBLE(SUM(kgenref_reg_coef, mask=(kgenref_reg_coef .eq. &
    &kgenref_reg_coef))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_net_a 
    CALL kgen_array_sumcheck("kgenref_net_a", kgen_array_sum, DBLE(SUM(kgenref_net_a, mask=(kgenref_net_a .eq. kgenref_net_a))), &
    &.TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_correl 
    CALL kgen_array_sumcheck("kgenref_correl", kgen_array_sum, DBLE(SUM(kgenref_correl, mask=(kgenref_correl .eq. &
    &kgenref_correl))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgenref_cov_factor 
READ (UNIT = kgen_unit) kgenref_varying_ss_inflate 
READ (UNIT = kgen_unit) kgenref_varying_ss_inflate_sd 
READ (UNIT = kgen_unit) kgenref_ss_inflate_base 
READ (UNIT = kgen_unit) kgenref_gamma 
READ (UNIT = kgen_unit) kgenref_ens_obs_mean 
READ (UNIT = kgen_unit) kgenref_ens_obs_var 
READ (UNIT = kgen_unit) kgenref_ens_var_deflate 
READ (UNIT = kgen_unit) kgenref_r_var 
READ (UNIT = kgen_unit) kgenref_r_mean 
READ (UNIT = kgen_unit) kgenref_diff_sd 
READ (UNIT = kgen_unit) kgenref_outlier_ratio 
READ (UNIT = kgen_unit) kgenref_state_index 
READ (UNIT = kgen_unit) kgenref_j 
READ (UNIT = kgen_unit) kgenref_group 
READ (UNIT = kgen_unit) kgenref_grp_bot 
READ (UNIT = kgen_unit) kgenref_grp_top 
READ (UNIT = kgen_unit) kgenref_do_adapt_inf_update 
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_t_base 
    CALL kgen_array_sumcheck("kgenref_t_base", kgen_array_sum, DBLE(SUM(kgenref_t_base, mask=(kgenref_t_base .eq. &
    &kgenref_t_base))), .TRUE.) 
END IF   
READ (UNIT = kgen_unit) kgen_istrue 
IF (kgen_istrue) THEN 
    READ (UNIT = kgen_unit) kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_t_items 
    CALL kgen_array_sumcheck("kgenref_t_items", kgen_array_sum, DBLE(SUM(kgenref_t_items, mask=(kgenref_t_items .eq. &
    &kgenref_t_items))), .TRUE.) 
END IF   

! how about this?  look for imbalances in the tasks

! turn these on carefully - they can generate a lot of output!
! also, to be readable - at least with ifort:
!  setenv FORT_FMT_RECL 1024
! so output lines don't wrap.
!timing(MLOOP)  = .true.
!timing(LG_GRN) = .true.


! use maxitems limit here or drown in output.
!timing(SM_GRN) = .false.
!t_limit(SM_GRN) = 4_i8
!timing(GC) = .true.
!t_limit(GC) = 4_i8
! allocate rather than dump all this on the stack


! end alloc
! we are going to read/write the copies array

! Initialize assim_tools_module if needed


!HK make window for mpi one-sided communication
! used for vertical conversion in get_close_obs
! Need to give create_mean_window the mean copy

! filter kinds 1 and 8 return sorted increments, however non-deterministic
! inflation can scramble these. the sort is expensive, so help users get better 
! performance by rejecting namelist combinations that do unneeded work.


!GSR open the dignostics file


! For performance, make local copies of these settings which
! are really in the inflate derived type.

! Default to printing nothing

! Divide ensemble into num_groups groups.
! make sure the number of groups and ensemble size result in 
! at least 2 members in each group (to avoid divide by 0) and 
! that the groups all have the same number of members.


! Put initial value of state space inflation in copy normally used for SD
! This is to avoid weird storage footprint in filter

! For single state or obs space inflation, the inflation is like a token
! Gets passed from the processor with a given obs on to the next


! Get info on my number and indices for obs

! Construct an observation temporary

! Get the locations for all of my observations 
! HK I would like to move this to before the calculation of the forward operator so you could
! overwrite the vertical location with the required localization vertical coordinate when you 
! do the forward operator calculation


! Get info on my number and indices for state

! Get the location and kind of all my state variables


!call test_get_state_meta_data(my_state_loc, ens_handle%my_num_vars)
!> optionally convert all state location verticals


! PAR: MIGHT BE BETTER TO HAVE ONE PE DEDICATED TO COMPUTING 
! INCREMENTS. OWNING PE WOULD SHIP IT'S PRIOR TO THIS ONE
! BEFORE EACH INCREMENT.
! Get mean and variance of each group's observation priors for adaptive inflation
! Important that these be from before any observations have been used


! The computations in the two get_close_maxdist_init are redundant
! Initialize the method for getting state variables close to a given ob on my process


! Initialize the method for getting obs close to a given ob on my process


! use MLOOP for the overall outer loop times; LG_GRN is for
! sections inside the overall loop, including the total time
! for the state_update and obs_update loops.  use SM_GRN for
! sections inside those last 2 loops and be careful - they will
! be called nobs * nstate * ntasks.
! Loop through all the (global) observations sequentially


IF (kgen_evalstage) THEN 
  if (kgen_mpirank == 0) then
    write(*,*) "Number of close states : ", num_close_states
  endif
  kgen_measure_int = num_close_states
END IF   
IF (kgen_warmupstage) THEN 
END IF   
IF (kgen_mainstage) THEN 
END IF   
  
!Uncomment following call statement to turn on perturbation experiment. 
!Adjust perturbation value and/or kind parameter if required. 
!CALL kgen_perturb_real( obs_prior, 1.0E-7_r8 ) 
  
!call to kgen kernel 

   if (timing(LG_GRN)) call start_timer(t_base(LG_GRN))
   STATE_UPDATE: do j = 1, num_close_states
      state_index = close_state_ind(j)

      ! the "any" is an expensive test when you do it for every ob.  don't test
      ! if we know there aren't going to be missing values in the state.
      if ( allow_missing_in_state ) then
         ! Some models can take evasive action if one or more of the ensembles have
         ! a missing value. Generally means 'do nothing' (as opposed to DIE)
         if (any(ens_handle%copies(1:ens_size, state_index) == MISSING_R8)) cycle STATE_UPDATE
      endif

      ! Get the initial values of inflation for this variable if state varying inflation
      if(local_varying_ss_inflate) then
         varying_ss_inflate    = ens_handle%copies(ENS_INF_COPY,    state_index)
         varying_ss_inflate_sd = ens_handle%copies(ENS_INF_SD_COPY, state_index)
      else
         varying_ss_inflate    = 0.0_r8
         varying_ss_inflate_sd = 0.0_r8
      endif
     
      ! Compute the distance and covariance factor 
      cov_factor = comp_cov_factor(close_state_dist(j), cutoff_rev, &
         base_obs_loc, base_obs_type, my_state_loc(state_index), my_state_kind(state_index))

      ! if external impact factors supplied, factor them in here
      ! FIXME: this would execute faster for 0.0 impact factors if
      ! we check for that before calling comp_cov_factor.  but it makes
      ! the logic more complicated - this is simpler if we do it after.
      if (adjust_obs_impact) then
         impact_factor = obs_impact_table(base_obs_type, my_state_kind(state_index))
         cov_factor = cov_factor * impact_factor
      endif

      ! If no weight is indicated, no more to do with this state variable
      if(cov_factor <= 0.0_r8) cycle STATE_UPDATE

      if (timing(SM_GRN)) call start_timer(t_base(SM_GRN), t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)
      ! Loop through groups to update the state variable ensemble members
      do group = 1, num_groups
         grp_bot = grp_beg(group)
         grp_top = grp_end(group)
         ! Do update of state, correl only needed for varying ss inflate
         if(local_varying_ss_inflate .and. varying_ss_inflate > 0.0_r8 .and. &
            varying_ss_inflate_sd > 0.0_r8) then
            call update_from_obs_inc(obs_prior(grp_bot:grp_top), obs_prior_mean(group), &
               obs_prior_var(group), obs_inc(grp_bot:grp_top), &
               ens_handle%copies(grp_bot:grp_top, state_index), grp_size, &
               increment(grp_bot:grp_top), reg_coef(group), net_a(group), correl(group))
         else
            call update_from_obs_inc(obs_prior(grp_bot:grp_top), obs_prior_mean(group), &
               obs_prior_var(group), obs_inc(grp_bot:grp_top), &
               ens_handle%copies(grp_bot:grp_top, state_index), grp_size, &
               increment(grp_bot:grp_top), reg_coef(group), net_a(group))
         endif
      end do
      if (timing(SM_GRN)) call read_timer(t_base(SM_GRN), 'update_from_obs_inc_S', &
                                          t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)

      ! Compute an information factor for impact of this observation on this state
      if(num_groups == 1) then
          reg_factor = 1.0_r8
      else
         ! Pass the time along with the index for possible diagnostic output
         ! Compute regression factor for this obs-state pair
         reg_factor = comp_reg_factor(num_groups, reg_coef, obs_time, i, my_state_indx(state_index))
      endif

      ! The final factor is the minimum of group regression factor and localization cov_factor
      reg_factor = min(reg_factor, cov_factor)

!PAR NEED TO TURN STUFF OFF MORE EFFICEINTLY
      ! If doing full assimilation, update the state variable ensemble with weighted increments
      if(.not. inflate_only) then
         ens_handle%copies(1:ens_size, state_index) = &
            ens_handle%copies(1:ens_size, state_index) + reg_factor * increment
      endif

      ! Compute spatially-varying state space inflation
      if(local_varying_ss_inflate) then
         ! base is the initial inflate value for this state variable
         ss_inflate_base = ens_handle%copies(ENS_SD_COPY, state_index)
         ! Loop through each group to update inflation estimate
         GroupInflate_K: do group = 1, num_groups
            if(varying_ss_inflate > 0.0_r8 .and. varying_ss_inflate_sd > 0.0_r8) then
               ! Gamma is less than 1 for varying ss, see adaptive inflate module
               gamma = reg_factor * abs(correl(group))
               ! Deflate the inflated variance using the INITIAL state inflate
               ! value (before these obs started gumming it up).
               ens_obs_mean = orig_obs_prior_mean(group)
               ens_obs_var =  orig_obs_prior_var(group)

               ! Remove the impact of inflation to allow efficient single pass with assim.
               if ( abs(gamma) > small ) then
                  ens_var_deflate = ens_obs_var / &
                     (1.0_r8 + gamma*(sqrt(ss_inflate_base) - 1.0_r8))**2
               else
                  ens_var_deflate = ens_obs_var
               endif
                  
               ! If this is inflate only (i.e. posterior) remove impact of this obs.
               if(inflate_only .and. &
                     ens_var_deflate               > small .and. &
                     obs_err_var                   > small .and. & 
                     obs_err_var - ens_var_deflate > small ) then 
                  r_var  = 1.0_r8 / (1.0_r8 / ens_var_deflate - 1.0_r8 / obs_err_var)
                  r_mean = r_var *(ens_obs_mean / ens_var_deflate - obs(1) / obs_err_var)
               else
                  r_var = ens_var_deflate
                  r_mean = ens_obs_mean
               endif

               ! IS A TABLE LOOKUP POSSIBLE TO ACCELERATE THIS?
               ! Update the inflation values
               if (timing(SM_GRN)) call start_timer(t_base(SM_GRN), t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)
               call update_inflation(inflate, varying_ss_inflate, varying_ss_inflate_sd, &
                  r_mean, r_var, grp_size, obs(1), obs_err_var, gamma)
               if (timing(SM_GRN)) call read_timer(t_base(SM_GRN), 'update_inflation_V', &
                                                   t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)
            else
               ! if we don't go into the previous if block, make sure these
               ! have good values going out for the block below
               r_mean = orig_obs_prior_mean(group)
               r_var =  orig_obs_prior_var(group)
            endif

            ! Update adaptive values if posterior outlier_ratio test doesn't fail.
            ! Match code in obs_space_diags() in filter.f90
            do_adapt_inf_update = .true.
            if (inflate_only) then
               diff_sd = sqrt(obs_err_var + r_var) 
               if (diff_sd > 0.0_r8) then
                  outlier_ratio = abs(obs(1) - r_mean) / diff_sd
                  do_adapt_inf_update = (outlier_ratio <= 3.0_r8) 
               endif
            endif
            if (do_adapt_inf_update) then   
               ens_handle%copies(ENS_INF_COPY, state_index) = varying_ss_inflate
               ens_handle%copies(ENS_INF_SD_COPY, state_index) = varying_ss_inflate_sd
            endif
         end do GroupInflate_K
      endif

   end do STATE_UPDATE
   IF (kgen_mainstage) THEN 
         
       !verify init 
       CALL kgen_init_verify(tolerance=8.D-5, minvalue=1.D-14, verboseLevel=1) 
       CALL kgen_init_check(check_status, rank=kgen_mpirank) 
         
       !extern verify variables 
       CALL kv_externs_utilities_mod(check_status) 
       CALL kv_externs_time_manager_mod(check_status) 
       CALL kv_externs_cov_cutoff_mod(check_status) 
       CALL kv_externs_sampling_error_correction_mod(check_status) 
       CALL kv_externs_reg_factor_mod(check_status) 
         
       !local verify variables 
       !CALL kv_ensemble_manager_mod_ensemble_type("ens_handle", check_status, ens_handle, kgenref_ens_handle) 
       CALL kv_filter_assim_real__r8_dim1("increment", check_status, increment, kgenref_increment) 
       CALL kv_filter_assim_real__r8("reg_factor", check_status, reg_factor, kgenref_reg_factor) 
       CALL kv_filter_assim_real__r8("impact_factor", check_status, impact_factor, kgenref_impact_factor) 
       CALL kv_filter_assim_real__r8_dim1("net_a", check_status, net_a, kgenref_net_a) 
       CALL kv_filter_assim_real__r8_dim1("reg_coef", check_status, reg_coef, kgenref_reg_coef) 
       CALL kv_filter_assim_real__r8_dim1("correl", check_status, correl, kgenref_correl) 
       CALL kv_filter_assim_real__r8("cov_factor", check_status, cov_factor, kgenref_cov_factor) 
       CALL kv_filter_assim_real__r8("varying_ss_inflate", check_status, varying_ss_inflate, kgenref_varying_ss_inflate) 
       CALL kv_filter_assim_real__r8("varying_ss_inflate_sd", check_status, varying_ss_inflate_sd, kgenref_varying_ss_inflate_sd) 
       CALL kv_filter_assim_real__r8("ss_inflate_base", check_status, ss_inflate_base, kgenref_ss_inflate_base) 
       CALL kv_filter_assim_real__r8("ens_obs_var", check_status, ens_obs_var, kgenref_ens_obs_var) 
       CALL kv_filter_assim_real__r8("ens_obs_mean", check_status, ens_obs_mean, kgenref_ens_obs_mean) 
       CALL kv_filter_assim_real__r8("gamma", check_status, gamma, kgenref_gamma) 
       CALL kv_filter_assim_real__r8("ens_var_deflate", check_status, ens_var_deflate, kgenref_ens_var_deflate) 
       CALL kv_filter_assim_real__r8("r_mean", check_status, r_mean, kgenref_r_mean) 
       CALL kv_filter_assim_real__r8("r_var", check_status, r_var, kgenref_r_var) 
       CALL kv_filter_assim_real__r8("outlier_ratio", check_status, outlier_ratio, kgenref_outlier_ratio) 
       CALL kv_filter_assim_real__r8("diff_sd", check_status, diff_sd, kgenref_diff_sd) 
       CALL kv_filter_assim_integer__i8("state_index", check_status, state_index, kgenref_state_index) 
       CALL kv_filter_assim_integer__("j", check_status, j, kgenref_j) 
       CALL kv_filter_assim_integer__("grp_bot", check_status, grp_bot, kgenref_grp_bot) 
       CALL kv_filter_assim_integer__("group", check_status, group, kgenref_group) 
       CALL kv_filter_assim_integer__("grp_top", check_status, grp_top, kgenref_grp_top) 
       CALL kv_filter_assim_logical__("do_adapt_inf_update", check_status, do_adapt_inf_update, kgenref_do_adapt_inf_update) 
       CALL kv_filter_assim_real__digits12_dim1("t_base", check_status, t_base, kgenref_t_base) 
       CALL kv_filter_assim_integer__i8_dim1("t_items", check_status, t_items, kgenref_t_items) 
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
   if (timing(LG_GRN)) call start_timer(t_base(LG_GRN))
   STATE_UPDATE_K: do j = 1, num_close_states
      state_index = close_state_ind(j)

      ! the "any" is an expensive test when you do it for every ob.  don't test
      ! if we know there aren't going to be missing values in the state.
      if ( allow_missing_in_state ) then
         ! Some models can take evasive action if one or more of the ensembles have
         ! a missing value. Generally means 'do nothing' (as opposed to DIE)
         if (any(ens_handle%copies(1:ens_size, state_index) == MISSING_R8)) cycle STATE_UPDATE_K
      endif

      ! Get the initial values of inflation for this variable if state varying inflation
      if(local_varying_ss_inflate) then
         varying_ss_inflate    = ens_handle%copies(ENS_INF_COPY,    state_index)
         varying_ss_inflate_sd = ens_handle%copies(ENS_INF_SD_COPY, state_index)
      else
         varying_ss_inflate    = 0.0_r8
         varying_ss_inflate_sd = 0.0_r8
      endif
     
      ! Compute the distance and covariance factor 
      cov_factor = comp_cov_factor(close_state_dist(j), cutoff_rev, &
         base_obs_loc, base_obs_type, my_state_loc(state_index), my_state_kind(state_index))

      ! if external impact factors supplied, factor them in here
      ! FIXME: this would execute faster for 0.0 impact factors if
      ! we check for that before calling comp_cov_factor.  but it makes
      ! the logic more complicated - this is simpler if we do it after.
      if (adjust_obs_impact) then
         impact_factor = obs_impact_table(base_obs_type, my_state_kind(state_index))
         cov_factor = cov_factor * impact_factor
      endif

      ! If no weight is indicated, no more to do with this state variable
      if(cov_factor <= 0.0_r8) cycle STATE_UPDATE_K

      if (timing(SM_GRN)) call start_timer(t_base(SM_GRN), t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)
      ! Loop through groups to update the state variable ensemble members
      do group = 1, num_groups
         grp_bot = grp_beg(group)
         grp_top = grp_end(group)
         ! Do update of state, correl only needed for varying ss inflate
         if(local_varying_ss_inflate .and. varying_ss_inflate > 0.0_r8 .and. &
            varying_ss_inflate_sd > 0.0_r8) then
            call update_from_obs_inc(obs_prior(grp_bot:grp_top), obs_prior_mean(group), &
               obs_prior_var(group), obs_inc(grp_bot:grp_top), &
               ens_handle%copies(grp_bot:grp_top, state_index), grp_size, &
               increment(grp_bot:grp_top), reg_coef(group), net_a(group), correl(group))
         else
            call update_from_obs_inc(obs_prior(grp_bot:grp_top), obs_prior_mean(group), &
               obs_prior_var(group), obs_inc(grp_bot:grp_top), &
               ens_handle%copies(grp_bot:grp_top, state_index), grp_size, &
               increment(grp_bot:grp_top), reg_coef(group), net_a(group))
         endif
      end do
      if (timing(SM_GRN)) call read_timer(t_base(SM_GRN), 'update_from_obs_inc_S', &
                                          t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)

      ! Compute an information factor for impact of this observation on this state
      if(num_groups == 1) then
          reg_factor = 1.0_r8
      else
         ! Pass the time along with the index for possible diagnostic output
         ! Compute regression factor for this obs-state pair
         reg_factor = comp_reg_factor(num_groups, reg_coef, obs_time, i, my_state_indx(state_index))
      endif

      ! The final factor is the minimum of group regression factor and localization cov_factor
      reg_factor = min(reg_factor, cov_factor)

!PAR NEED TO TURN STUFF OFF MORE EFFICEINTLY
      ! If doing full assimilation, update the state variable ensemble with weighted increments
      if(.not. inflate_only) then
         ens_handle%copies(1:ens_size, state_index) = &
            ens_handle%copies(1:ens_size, state_index) + reg_factor * increment
      endif

      ! Compute spatially-varying state space inflation
      if(local_varying_ss_inflate) then
         ! base is the initial inflate value for this state variable
         ss_inflate_base = ens_handle%copies(ENS_SD_COPY, state_index)
         ! Loop through each group to update inflation estimate
         GroupInflate: do group = 1, num_groups
            if(varying_ss_inflate > 0.0_r8 .and. varying_ss_inflate_sd > 0.0_r8) then
               ! Gamma is less than 1 for varying ss, see adaptive inflate module
               gamma = reg_factor * abs(correl(group))
               ! Deflate the inflated variance using the INITIAL state inflate
               ! value (before these obs started gumming it up).
               ens_obs_mean = orig_obs_prior_mean(group)
               ens_obs_var =  orig_obs_prior_var(group)

               ! Remove the impact of inflation to allow efficient single pass with assim.
               if ( abs(gamma) > small ) then
                  ens_var_deflate = ens_obs_var / &
                     (1.0_r8 + gamma*(sqrt(ss_inflate_base) - 1.0_r8))**2
               else
                  ens_var_deflate = ens_obs_var
               endif
                  
               ! If this is inflate only (i.e. posterior) remove impact of this obs.
               if(inflate_only .and. &
                     ens_var_deflate               > small .and. &
                     obs_err_var                   > small .and. & 
                     obs_err_var - ens_var_deflate > small ) then 
                  r_var  = 1.0_r8 / (1.0_r8 / ens_var_deflate - 1.0_r8 / obs_err_var)
                  r_mean = r_var *(ens_obs_mean / ens_var_deflate - obs(1) / obs_err_var)
               else
                  r_var = ens_var_deflate
                  r_mean = ens_obs_mean
               endif

               ! IS A TABLE LOOKUP POSSIBLE TO ACCELERATE THIS?
               ! Update the inflation values
               if (timing(SM_GRN)) call start_timer(t_base(SM_GRN), t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)
               call update_inflation(inflate, varying_ss_inflate, varying_ss_inflate_sd, &
                  r_mean, r_var, grp_size, obs(1), obs_err_var, gamma)
               if (timing(SM_GRN)) call read_timer(t_base(SM_GRN), 'update_inflation_V', &
                                                   t_items(SM_GRN), t_limit(SM_GRN), do_sync=.false.)
            else
               ! if we don't go into the previous if block, make sure these
               ! have good values going out for the block below
               r_mean = orig_obs_prior_mean(group)
               r_var =  orig_obs_prior_var(group)
            endif

            ! Update adaptive values if posterior outlier_ratio test doesn't fail.
            ! Match code in obs_space_diags() in filter.f90
            do_adapt_inf_update = .true.
            if (inflate_only) then
               diff_sd = sqrt(obs_err_var + r_var) 
               if (diff_sd > 0.0_r8) then
                  outlier_ratio = abs(obs(1) - r_mean) / diff_sd
                  do_adapt_inf_update = (outlier_ratio <= 3.0_r8) 
               endif
            endif
            if (do_adapt_inf_update) then   
               ens_handle%copies(ENS_INF_COPY, state_index) = varying_ss_inflate
               ens_handle%copies(ENS_INF_SD_COPY, state_index) = varying_ss_inflate_sd
            endif
         end do GroupInflate
      endif

   end do STATE_UPDATE_K
       END DO   
       CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
       kgen_measure = 1.0D6*(kgen_stop_clock - kgen_start_clock)/DBLE(kgen_rate_clock*KGEN_MAXITER) 
#ifdef _MPI 
       CALL mpi_allreduce(kgen_measure, gkgen_measure, 1, mpi_real8, mpi_max, mpi_comm_world, kgen_ierr) 
       kgen_measure = gkgen_measure 
#endif 
       IF (check_status%rank==0) THEN 
           WRITE (*, *) "dart_wrf : Time per call (usec): ", kgen_measure 
       END IF   
   END IF   
   IF (kgen_warmupstage) THEN 
   END IF   
   IF (kgen_evalstage) THEN 
   END IF   


! Every pe needs to get the current my_inflate and my_inflate_sd back


! Free up the storage

! print some stats about the assimilation
! (if interesting, could print exactly which obs # was fastest and slowest)


! do some stats - being aware that unless we do a reduce() operation
! this is going to be per-task.  so only print if something interesting
! shows up in the stats?  maybe it would be worth a reduce() call here?
!>@todo FIXME:  
!  we have n_close_obs_items and n_close_state_items for each assimilated
!  observation.  what we really want to know is across the tasks is there
!  a big difference in counts?  so that means communication.  maybe just
!  the largest value?  and the number of 0 values?  and if the largest val
!  is way off compared to the other tasks, warn the user?
!  we don't have space or time to do all the obs * tasks but could we
!  send enough info to make a histogram?  compute N bin counts and then
!  reduce that across all the tasks and have task 0 print out?
! still thinking on this idea.
!   write(msgstring, *) 'max state items per observation: ', maxval(n_close_state_items)
!   call error_handler(E_MSG, 'filter_assim:', msgstring)
! if i come up with something i like, can we use the same idea
! for the threed_sphere locations boxes?
! Assure user we have done something


! diagnostics for stats on saving calls by remembering obs at the same location.
! change .true. to .false. in the line below to remove the output completely.


!call test_state_copies(ens_handle, 'end')
!GSR close the localization diagnostics file


! get rid of mpi window

! deallocate space


! end dealloc
     
   CONTAINS 
     

   !read state subroutine for kr_filter_assim_real__r8_dim1 
   SUBROUTINE kr_filter_assim_real__r8_dim1(var, kgen_unit, printname, printvar) 
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
   END SUBROUTINE kr_filter_assim_real__r8_dim1 
     
   !read state subroutine for kr_filter_assim_integer__i8_dim1 
   SUBROUTINE kr_filter_assim_integer__i8_dim1(var, kgen_unit, printname, printvar) 
       INTEGER(KIND=i8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
   END SUBROUTINE kr_filter_assim_integer__i8_dim1 
     
   !read state subroutine for kr_filter_assim_integer___dim1 
   SUBROUTINE kr_filter_assim_integer___dim1(var, kgen_unit, printname, printvar) 
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
   END SUBROUTINE kr_filter_assim_integer___dim1 
     
   !read state subroutine for kr_kgen_filter_assim_subp3 
   SUBROUTINE kr_kgen_filter_assim_subp3(var, kgen_unit, printname, printvar) 
       TYPE(location_type), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
                   CALL kr_location_mod_location_type(var(idx1), kgen_unit, printname // "(idx1)", .TRUE.) 
               ELSE 
                   CALL kr_location_mod_location_type(var(idx1), kgen_unit, printname // "(idx1)", .FALSE.) 
               END IF   
           END DO   
       END IF   
   END SUBROUTINE kr_kgen_filter_assim_subp3 
     
   !verify state subroutine for kv_filter_assim_real__r8_dim1 
   RECURSIVE SUBROUTINE kv_filter_assim_real__r8_dim1(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=r8), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1 
       INTEGER :: n 
       real(KIND=r8) :: nrmsdiff, rmsdiff 
       real(KIND=r8), ALLOCATABLE :: buf1(:), buf2(:) 
         
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
           ALLOCATE (buf1(SIZE(var,dim=1))) 
           ALLOCATE (buf2(SIZE(var,dim=1))) 
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
         
   END SUBROUTINE kv_filter_assim_real__r8_dim1 
     
   !verify state subroutine for kv_filter_assim_real__r8 
   RECURSIVE SUBROUTINE kv_filter_assim_real__r8(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=r8), INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       real(KIND=r8) :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var == kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff = ABS(var - kgenref_var) 
           IF (diff <= kgen_tolerance) THEN 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_filter_assim_real__r8 
     
   !verify state subroutine for kv_filter_assim_integer__i8 
   RECURSIVE SUBROUTINE kv_filter_assim_integer__i8(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       INTEGER(KIND=i8), INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       integer(KIND=i8) :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var == kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff = ABS(var - kgenref_var) 
           IF (diff <= kgen_tolerance) THEN 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_filter_assim_integer__i8 
     
   !verify state subroutine for kv_filter_assim_integer__ 
   RECURSIVE SUBROUTINE kv_filter_assim_integer__(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       INTEGER, INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       integer :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var == kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           diff = ABS(var - kgenref_var) 
           IF (diff <= kgen_tolerance) THEN 
               check_status%numInTol = check_status%numInTol + 1 
               IF (kgen_verboseLevel > 1) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_IN_TOL 
           ELSE 
               check_status%numOutTol = check_status%numOutTol + 1 
               IF (kgen_verboseLevel > 0) THEN 
                   IF (check_status%rank == 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
               END IF   
               check_result = CHECK_OUT_TOL 
           END IF   
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "Difference is ", diff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_filter_assim_integer__ 
     
   !verify state subroutine for kv_filter_assim_logical__ 
   RECURSIVE SUBROUTINE kv_filter_assim_logical__(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       LOGICAL, INTENT(IN) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       logical :: diff 
         
       check_status%numTotal = check_status%numTotal + 1 
         
       IF (var .EQV. kgenref_var) THEN 
           check_status%numIdentical = check_status%numIdentical + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_IDENTICAL 
       ELSE 
           check_status%numOutTol = check_status%numOutTol + 1 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
               END IF   
           END IF   
           check_result = CHECK_OUT_TOL 
       END IF   
       IF (check_result == CHECK_IDENTICAL) THEN 
           IF (kgen_verboseLevel > 2) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "NOT IMPLEMENTED" 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_OUT_TOL) THEN 
           IF (kgen_verboseLevel > 0) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "NOT IMPLEMENTED" 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       ELSE IF (check_result == CHECK_IN_TOL) THEN 
           IF (kgen_verboseLevel > 1) THEN 
               IF (check_status%rank == 0) THEN 
                   WRITE (*, *) "NOT IMPLEMENTED" 
                   WRITE (*, *) "" 
               END IF   
           END IF   
       END IF   
         
   END SUBROUTINE kv_filter_assim_logical__ 
     
   !verify state subroutine for kv_filter_assim_real__digits12_dim1 
   RECURSIVE SUBROUTINE kv_filter_assim_real__digits12_dim1(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=digits12), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1 
       INTEGER :: n 
       real(KIND=digits12) :: nrmsdiff, rmsdiff 
       real(KIND=digits12), ALLOCATABLE :: buf1(:), buf2(:) 
         
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
           ALLOCATE (buf1(SIZE(var,dim=1))) 
           ALLOCATE (buf2(SIZE(var,dim=1))) 
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
         
   END SUBROUTINE kv_filter_assim_real__digits12_dim1 
     
   !verify state subroutine for kv_filter_assim_integer__i8_dim1 
   RECURSIVE SUBROUTINE kv_filter_assim_integer__i8_dim1(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       INTEGER(KIND=i8), INTENT(IN), DIMENSION(:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1 
       INTEGER :: n 
       integer(KIND=i8) :: nrmsdiff, rmsdiff 
       integer(KIND=i8), ALLOCATABLE :: buf1(:), buf2(:) 
         
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
           ALLOCATE (buf1(SIZE(var,dim=1))) 
           ALLOCATE (buf2(SIZE(var,dim=1))) 
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
         
   END SUBROUTINE kv_filter_assim_integer__i8_dim1 
     
END SUBROUTINE filter_assim 
!-------------------------------------------------------------


subroutine update_from_obs_inc(obs, obs_prior_mean, obs_prior_var, obs_inc, &
               state, ens_size, state_inc, reg_coef, net_a, correl_out)
!========================================================================
! Does linear regression of a state variable onto an observation and
! computes state variable increments from observation increments


integer,            intent(in)    :: ens_size
real(r8),           intent(in)    :: obs(ens_size), obs_inc(ens_size)
real(r8),           intent(in)    :: obs_prior_mean, obs_prior_var
real(r8),           intent(in)    :: state(ens_size)
real(r8),           intent(out)   :: state_inc(ens_size), reg_coef
real(r8),           intent(inout) :: net_a
real(r8), optional, intent(inout) :: correl_out

real(r8) :: obs_state_cov, intermed
real(r8) :: restoration_inc(ens_size), state_mean, state_var, correl
real(r8) :: factor, exp_true_correl, mean_factor
! For efficiency, just compute regression coefficient here unless correl is needed
integer :: i
real(r8) :: tempsum

state_mean = sum(state) / ens_size
obs_state_cov = sum( (state - state_mean) * (obs - obs_prior_mean) ) / (ens_size - 1)

if (obs_prior_var > 0.0_r8) then
   reg_coef = obs_state_cov/obs_prior_var
else
   reg_coef = 0.0_r8
endif
! If correl_out is present, need correl for adaptive inflation
! Also needed for file correction below.
! WARNING: we have had several different numerical problems in this
! section, especially with users running in single precision floating point.
! Be very cautious if changing any code in this section, taking into
! account underflow and overflow for 32 bit floats.


if(present(correl_out) .or. sampling_error_correction) then
   if (obs_state_cov == 0.0_r8 .or. obs_prior_var <= 0.0_r8) then
      correl = 0.0_r8
   else
      state_var = sum((state - state_mean)**2) / (ens_size - 1)
      if (state_var <= 0.0_r8) then
         correl = 0.0_r8
      else
         intermed = sqrt(obs_prior_var) * sqrt(state_var)
         if (intermed <= 0.0_r8) then
            correl = 0.0_r8
         else
            correl = obs_state_cov / intermed
         endif
      endif
   endif
   if(correl >  1.0_r8) correl =  1.0_r8
   if(correl < -1.0_r8) correl = -1.0_r8
endif
if(present(correl_out)) correl_out = correl
! Get the expected actual correlation and the regression weight reduction factor


if(sampling_error_correction) then
   call get_correction_from_table(correl, mean_factor, exp_true_correl, ens_size)
   ! Watch out for division by zero; if correl is really small regression is safely 0
   if(abs(correl) > 0.001_r8) then
      reg_coef = reg_coef * (exp_true_correl / correl) * mean_factor
   else
      reg_coef = 0.0_r8
   endif
   correl = exp_true_correl
endif
! Then compute the increment as product of reg_coef and observation space increment


state_inc = reg_coef * obs_inc
! FIXME: craig schwartz has a degenerate case involving externally computed
! forward operators in which the obs prior variance is in fact exactly 0.
! adding this test allowed him to continue to  use spread restoration
! without numerical problems.  we don't know if this is sufficient;
! for now we'll leave the original code but it needs to be revisited.
! Spread restoration algorithm option.
!if(spread_restoration .and. obs_prior_var > 0.0_r8) then
! Spread restoration algorithm option.

!
!
!

if(spread_restoration) then
   ! Don't use this to reduce spread at present (should revisit this line)
   if(net_a > 1.0_r8) net_a = 1.0_r8
   ! Default restoration increment is 0.0

   restoration_inc = 0.0_r8
   ! Compute the factor by which to inflate
   ! These come from correl_error.f90 in system_simulation and the files ens??_pairs and
   ! ens_pairs_0.5 in work under system_simulation. Assume a linear reduction from 1
   ! as a function of the net_a. Assume that the slope of this reduction is a function of
   ! the reciprocal of the ensemble_size (slope = 0.80 / ens_size). These are empirical
   ! for now. See also README in spread_restoration_paper documentation.
   !!!factor = 1.0_r8 / (1.0_r8 + (net_a - 1.0_r8) * (0.8_r8 / ens_size)) - 1.0_r8

   factor = 1.0_r8 / (1.0_r8 + (net_a - 1.0_r8) / (-2.4711_r8 + 1.6386_r8 * ens_size)) - 1.0_r8
   !!!factor = 1.0_r8 / (1.0_r8 + (net_a**2 - 1.0_r8) * (-0.0111_r8 + .8585_r8 / ens_size)) - 1.0_r8
   ! Variance restoration

   state_mean = sum(state) / ens_size
   restoration_inc = factor * (state - state_mean)
   state_inc = state_inc + restoration_inc
endif
!! NOTE: if requested to be returned, correl_out is set further up in the
!! code, before the sampling error correction, if enabled, is applied.
!! this means it's returning a different larger value than the correl 
!! being returned here.  it's used by the adaptive inflation and so the
!! inflation will see a slightly different correlation value.  it isn't
!! clear that this is a bad thing; it means the inflation might be a bit
!! larger than it would otherwise.  before we move any code this would
!! need to be studied to see what the real impact would be.


end subroutine update_from_obs_inc
!------------------------------------------------------------------------


subroutine get_correction_from_table(scorrel, mean_factor, expected_true_correl, ens_size)

real(r8),  intent(in) :: scorrel
real(r8), intent(out) :: mean_factor, expected_true_correl
integer,  intent(in)  :: ens_size
! Uses interpolation to get correction factor into the table


integer             :: low_indx, high_indx
real(r8)            :: correl, fract, low_correl, low_exp_correl, low_alpha
real(r8)            :: high_correl, high_exp_correl, high_alpha

logical, save :: first_time = .true.

if (first_time) then
   call read_sampling_error_correction(ens_size, exp_true_correl, alpha)
   first_time = .false.
endif
! Interpolate to get values of expected correlation and mean_factor

if(scorrel < -1.0_r8) then
   correl = -1.0_r8
   mean_factor = 1.0_r8
else if(scorrel > 1.0_r8) then
   correl = 1.0_r8
   mean_factor = 1.0_r8
else if(scorrel <= -0.995_r8) then
   fract = (scorrel + 1.0_r8) / 0.005_r8
   correl = (exp_true_correl(1) + 1.0_r8) * fract - 1.0_r8
   mean_factor = (alpha(1) - 1.0_r8) * fract + 1.0_r8
else if(scorrel >= 0.995_r8) then
   fract = (scorrel - 0.995_r8) / 0.005_r8
   correl = (1.0_r8 - exp_true_correl(sec_table_size)) * fract + exp_true_correl(sec_table_size)
   mean_factor = (1.0_r8 - alpha(sec_table_size)) * fract + alpha(sec_table_size)
else
   ! given the ifs above, the floor() computation below for low_indx 
   ! should always result in a value in the range 1 to 199.  but if this
   ! code is compiled with r8=r4 (single precision reals) it turns out
   ! to be possible to get values a few bits below 0 which results in
   ! a very large negative integer.  the limit tests below ensure the
   ! index stays in a legal range.
   low_indx = floor((scorrel + 0.995_r8) / 0.01_r8 + 1.0_r8)
   if (low_indx <   1) low_indx =   1
   if (low_indx > 199) low_indx = 199
   low_correl = -0.995_r8 + (low_indx - 1) * 0.01_r8
   low_exp_correl = exp_true_correl(low_indx)
   low_alpha = alpha(low_indx)
   high_indx = low_indx + 1
   high_correl = low_correl + 0.01_r8
   high_exp_correl = exp_true_correl(high_indx)
   high_alpha = alpha(high_indx)
   fract = (scorrel - low_correl) / (high_correl - low_correl)
   correl = (high_exp_correl - low_exp_correl) * fract + low_exp_correl
   mean_factor = (high_alpha - low_alpha) * fract + low_alpha
endif

expected_true_correl = correl 
! Don't want Monte Carlo interpolation problems to put us outside of a
! ratio between 0 and 1 for expected_true_correl / sample_correl
! If they have different signs, expected should just be 0

if(expected_true_correl * scorrel <= 0.0_r8) then
   expected_true_correl = 0.0_r8
else if(abs(expected_true_correl) > abs(scorrel)) then
   ! If same sign, expected should not be bigger in absolute value
   expected_true_correl = scorrel
endif 

end subroutine get_correction_from_table


!------------------------------------------------------------------------


!------------------------------------------------------------------------


!------------------------------------------------------------------------


!------------------------------------------------------------------------


!--------------------------------------------------------------------


!--------------------------------------------------------------------


!----------------------------------------------------------------------
!> gets the location of of all my observations


!--------------------------------------------------------------------
!> wrappers for timers
!>
!> t_space is where we store the time information 
!> itemcount is the running count of how many times we've been called
!> maxitems is a limit on the number of times we want this timer to print.
!> do_sync overrides the default for whether we want to do a task sync
!> or not.  right now this code defaults to yes, sync before getting
!> the time.  for very large processor counts this increases overhead.


subroutine start_timer(t_space, itemcount, maxitems, do_sync)
 real(digits12), intent(out) :: t_space
 integer(i8),    intent(inout), optional :: itemcount
 integer(i8),    intent(in),    optional :: maxitems
 logical,        intent(in),    optional :: do_sync

logical :: sync_me

if (present(itemcount) .and. present(maxitems)) then
  itemcount = itemcount + 1
  if (itemcount > maxitems) then
     ! if called enough, this can roll over the integer limit.  
     ! set itemcount to maxitems+1 here to avoid this.
     ! also, go ahead and set the time because there is
     ! an option to print large time values even if over the
     ! number of calls limit in read_timer()

     itemcount = maxitems + 1
     call start_mpi_timer(t_space)
     return
  endif
endif

sync_me = .true.
if (present(do_sync)) sync_me = do_sync

if (sync_me) call task_sync()
call start_mpi_timer(t_space)

end subroutine start_timer
!--------------------------------------------------------------------
!>
!> t_space is where we store the time information 
!> label is the string to print out with the time.  limited to ~60 chars.
!> itemcount is the running count of how many times we've been called
!> maxitems is a limit on the number of times we want this timer to print.
!> do_sync overrides the default for whether we want to do a task sync
!> or not.  right now this code defaults to yes, sync before getting
!> the time.  for very large processor counts this increases overhead.
!> elapsed is an optional return of the value instead of only printing here


subroutine read_timer(t_space, label, itemcount, maxitems, do_sync, elapsed)
 real(digits12),   intent(in) :: t_space
 character(len=*), intent(in) :: label
 integer(i8),      intent(inout), optional :: itemcount
 integer(i8),      intent(in),    optional :: maxitems
 logical,          intent(in),    optional :: do_sync
 real(digits12),   intent(out),   optional :: elapsed

real(digits12) :: interval
logical :: sync_me
character(len=132) :: buffer
! if interval time (in seconds) is > this, go ahead and
! print even if item count is over limit.

integer(i8), parameter :: T_ALWAYS_PRINT = 1.0_r8
! get the time first and then figure out what we're doing


interval = read_mpi_timer(t_space)

sync_me = .true.
if (present(do_sync)) sync_me = do_sync
! if there's a limit on number of prints don't allow sync 
! because if we return early we could hang everyone else.

if (present(maxitems)) sync_me = .false.
! print out large values no matter what
! (large is defined above locally in this routine)

if (present(itemcount) .and. present(maxitems)) then
  if (interval < T_ALWAYS_PRINT .and. itemcount > maxitems) return
endif
! if syncing, wait and read the timer again

if (sync_me) then
   call task_sync()
   interval = read_mpi_timer(t_space)
endif

if (sync_me) then
   if (my_task_id() == 0) then
      write(buffer,'(A75,F15.8)') "timer: "//trim(label)//" time ", interval
      write(*,*) buffer
   endif
else
   write(buffer,'(A75,F15.8,A6,I7)') "timer: "//trim(label)//" time ", interval, " rank ", my_task_id()
   write(*,*) buffer
endif

if (present(elapsed)) elapsed = interval

end subroutine read_timer
!--------------------------------------------------------------------
!> log what the user has selected via the namelist choices


!===========================================================
! TEST FUNCTIONS BELOW THIS POINT
!-----------------------------------------------------------
!> test get_state_meta_data
!> Write out the resutls of get_state_meta_data for each task
!> They should be the same as the Trunk version


!--------------------------------------------------------
!> dump out the copies array for the state ens handle


!--------------------------------------------------------
!> dump out the distances calculated in get_close_obs


!> @}
!========================================================================
! end module assim_tools_mod
!========================================================================


!read state subroutine for kr_externs_in_assim_tools_mod 
SUBROUTINE kr_externs_in_assim_tools_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) sec_table_size 
    CALL kr_assim_tools_mod_real__r8_dim1(alpha, kgen_unit, "alpha", .FALSE.) 
    CALL kr_assim_tools_mod_real__r8_dim1(exp_true_correl, kgen_unit, "exp_true_correl", .FALSE.) 
    CALL kr_assim_tools_mod_real__r8_dim2(obs_impact_table, kgen_unit, "obs_impact_table", .FALSE.) 
    READ (UNIT = kgen_unit) spread_restoration 
    READ (UNIT = kgen_unit) sampling_error_correction 
    READ (UNIT = kgen_unit) adjust_obs_impact 
END SUBROUTINE kr_externs_in_assim_tools_mod 
  
!read state subroutine for kr_externs_out_assim_tools_mod 
SUBROUTINE kr_externs_out_assim_tools_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_assim_tools_mod 
  
!read state subroutine for kr_assim_tools_mod_real__r8_dim1 
SUBROUTINE kr_assim_tools_mod_real__r8_dim1(var, kgen_unit, printname, printvar) 
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
END SUBROUTINE kr_assim_tools_mod_real__r8_dim1 
  
!read state subroutine for kr_assim_tools_mod_real__r8_dim2 
SUBROUTINE kr_assim_tools_mod_real__r8_dim2(var, kgen_unit, printname, printvar) 
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
END SUBROUTINE kr_assim_tools_mod_real__r8_dim2 
  
end module assim_tools_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/assim_tools_mod.f90 $
! $Id: assim_tools_mod.f90 12812 2018-09-05 16:25:02Z nancy@ucar.edu $
! $Revision: 12812 $
! $Date: 2018-09-05 10:25:02 -0600 (Wed, 05 Sep 2018) $
