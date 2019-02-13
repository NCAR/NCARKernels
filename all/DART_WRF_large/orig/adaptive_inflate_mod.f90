!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:29 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: adaptive_inflate_mod.f90 12939 2018-11-27 15:34:34Z nancy@ucar.edu $ 
!> Operations and storage required for various adaptive inflation algorithms


!


module adaptive_inflate_mod
!> \defgroup adaptive_inflate adaptive_inflate_mod
!> @{


    USE types_mod, ONLY: r8, pi 
    USE utilities_mod, ONLY: error_handler, e_err 
    USE random_seq_mod, ONLY: random_seq_type 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE random_seq_mod, ONLY: kr_random_seq_mod_random_seq_type 
    USE random_seq_mod, ONLY: kv_random_seq_mod_random_seq_type 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC update_inflation, adaptive_inflate_type, solve_quadratic 
! version controlled file description for error handling, do not edit


character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 12939 $"
character(len=128), parameter :: revdate  = "$Date: 2018-11-27 08:34:34 -0700 (Tue, 27 Nov 2018) $"
! Manages both observation space and state space inflation
! Handles initial values and restarts, diagnostic output, and computations
! Algorithm options at present include a single fixed observation space,
! a single fixed state space adaptive inflation,
! and a spatially-varying state space inflation that carries
! a mean and variance for the state space inflation at each point. 
!>@todo the 'flavor' should be a string in the namelist and an integer
!>parameter with a more descriptive name instead of an arbitrary integer.
!>Same with 1 and 2 corresponding to Prior and Posterior inflation.
!> eventually these namelist options should move from filter into
!> this module and then possibly become two different namelists so
!> we don't have these arrays of length (2).
! Type to keep track of information for inflation


type adaptive_inflate_type
   private
   ! Flavor can be 0:none, 1:obs_inflate, 2:varying_ss_inflate, 3:single_ss_inflate
   !  4 = RTPS, 5 = enhanced ss, modification of 2
   ! 1:obs_inflate is currently deprecated.
   integer               :: inflation_flavor
   integer               :: inflation_sub_flavor
   logical               :: output_restart = .false.
   logical               :: deterministic
   real(r8)              :: inflate, sd, sd_lower_bound, inf_lower_bound, inf_upper_bound
   real(r8)              :: sd_max_change
   ! Include a random sequence type in case non-deterministic inflation is used
   type(random_seq_type) :: ran_seq
   logical               :: allow_missing_in_clm
   real(r8)              :: minmax_mean(2), minmax_sd(2)
   logical               :: mean_from_restart
   logical               :: sd_from_restart
   logical               :: prior = .false.
   logical               :: posterior = .false.
   integer               :: input_mean_copy = -1 !>todo NO_COPY_PRESENT
   integer               :: input_sd_copy   = -1
end type adaptive_inflate_type
! types for updating the inflation

integer, parameter :: GHA2017 = 1
integer, parameter :: AND2009 = 2
! Module storage for writing error messages

CHARACTER(LEN=512) :: msgstring 
! Flag indicating whether module has been initialized

!============================================================================
PUBLIC kr_externs_in_adaptive_inflate_mod 
PUBLIC kr_externs_out_adaptive_inflate_mod 
PUBLIC kr_random_seq_mod_random_seq_type 
PUBLIC kr_kgen_adaptive_inflate_mod_typesubp0 
PUBLIC kv_random_seq_mod_random_seq_type 
PUBLIC kv_kgen_adaptive_inflate_mod_typesubp0 


contains
!------------------------------------------------------------------
!> Accessor functions for adaptive inflate type


!------------------------------------------------------------------


!------------------------------------------------------------------


!------------------------------------------------------------------


!------------------------------------------------------------------


!------------------------------------------------------------------


!------------------------------------------------------------------


!------------------------------------------------------------------


!------------------------------------------------------------------
!> Initializes an adaptive_inflate_type 


!------------------------------------------------------------------
!> Returns true if this inflation type indicates observation space inflation


!------------------------------------------------------------------
!> Returns true if this inflation type indicates varying state space inflation


!------------------------------------------------------------------
!> Returns true if this inflation type indicates fixed state space inflation


!------------------------------------------------------------------
!> Returns true if this inflation type indicates posterior relaxion-to-prior-spread
!> (whitaker & Hamill, 2012)


!------------------------------------------------------------------
!> *private* accessor routine for the subtype
!>
!> Returns true if this inflation sub type indicates enhanced state space inflation
!> Moha Gharamti, 2017


function do_enhanced_ss_inflate(inflate_handle)

logical                                 :: do_enhanced_ss_inflate
type(adaptive_inflate_type), intent(in) :: inflate_handle

do_enhanced_ss_inflate = ((inflate_handle%inflation_flavor == 2) .and. &
                          (inflate_handle%inflation_sub_flavor == 5))

end function do_enhanced_ss_inflate
!------------------------------------------------------------------
!> Returns true if deterministic inflation is indicated


!------------------------------------------------------------------
!> Make sure the combination of inflation options are legal


!------------------------------------------------------------------
!> Inflates subset of ensemble members given mean and inflate
!> Selects between deterministic and stochastic inflation


!------------------------------------------------------------------
!> This routine is given information from an inflate type, scalar values for inflate 
!> and inflate_sd, the ensemble prior_mean and prior_var for an observation, the
!> ensemble (or group) size, the observed value, observational error variance, 
!> and gamma_corr (see below for description).  It computes updated values for the 
!> inflate and inflate_sd items using algorithms described in filter.html (including
!> references to papers).
!>
!> The gamma_corr parameter gives the localized prior correlation times the localization
!> which is computed in the assim_tools routine filter_assim. For single state
!> space inflation it is 1.0.


subroutine update_inflation(inflate_handle, inflate, inflate_sd, prior_mean, prior_var, &
   ens_size, obs, obs_var, gamma_corr)

type(adaptive_inflate_type), intent(in)    :: inflate_handle
real(r8),                    intent(inout) :: inflate, inflate_sd
real(r8),                    intent(in)    :: prior_mean, prior_var
integer,                     intent(in)    :: ens_size
real(r8),                    intent(in)    :: obs, obs_var, gamma_corr

real(r8) :: new_inflate, new_inflate_sd
integer :: inf_type
! If the inflate_sd not positive, keep everything the same

if(inflate_sd <= 0.0_r8) return
! A lower bound on the updated inflation sd and an upper bound
! on the inflation itself are provided in the inflate_handle. 
! select which method to update with


if (do_enhanced_ss_inflate(inflate_handle)) then
   inf_type = GHA2017
else
   inf_type = AND2009
endif
! Use bayes theorem to update

call bayes_cov_inflate(ens_size, inf_type, prior_mean, prior_var, obs, obs_var, inflate, &
   inflate_sd, gamma_corr, inflate_handle%sd_lower_bound, inflate_handle%sd_max_change, &
   new_inflate, new_inflate_sd)
! Make sure inflate satisfies constraints

inflate = new_inflate
if(inflate < inflate_handle%inf_lower_bound) inflate = inflate_handle%inf_lower_bound
if(inflate > inflate_handle%inf_upper_bound) inflate = inflate_handle%inf_upper_bound
! Make sure sd satisfies constraints

inflate_sd = new_inflate_sd
if(inflate_sd < inflate_handle%sd_lower_bound) inflate_sd = inflate_handle%sd_lower_bound

end subroutine update_inflation
!------------------------------------------------------------------
!> Uses one of 2 algorithms in references on DART web site to update the 
!> distribution of inflation:  Anderson 2007, 2009 or Gharamti 2017


subroutine bayes_cov_inflate(ens_size, inf_type, x_p, sigma_p_2, y_o, sigma_o_2, lambda_mean, lambda_sd, &
   gamma_corr, sd_lower_bound_in, sd_max_change_in, new_cov_inflate, new_cov_inflate_sd)

integer , intent(in)  :: ens_size, inf_type
real(r8), intent(in)  :: x_p, sigma_p_2, y_o, sigma_o_2, lambda_mean, lambda_sd
real(r8), intent(in)  :: gamma_corr, sd_lower_bound_in, sd_max_change_in
real(r8), intent(out) :: new_cov_inflate, new_cov_inflate_sd

integer  :: i, mlambda_index(1)
real(r8) :: dist_2, rate, shape_old, shape_new, rate_new
real(r8) :: lambda_sd_2, density_1, density_2, omega, ratio
real(r8) :: new_1_sd, new_max
real(r8) :: b, c, d, Q, R, disc, alpha, beta, cube_root_alpha, cube_root_beta, x
real(r8) :: rrr, cube_root_rrr, angle, mx(3), sep(3), mlambda(3)
! If gamma is 0, nothing changes

if(gamma_corr <= 0.0_r8) then
   new_cov_inflate = lambda_mean
   new_cov_inflate_sd = lambda_sd
   return
endif
! Inflation variance

lambda_sd_2 = lambda_sd**2
! Squared Innovation

dist_2 = (y_o - x_p)**2
! this block of code no longer being used.  it's here for historical purposes.
!   ! Use ONLY the linear approximation, cubic solution below can be numerically
!   ! unstable for extreme cases. Should look at this later.
!   if(gamma_corr > 0.99_r8) then
!   ! The solution of the cubic below only works if gamma is 1.0
!   ! Can analytically find the maximum of the product: d/dlambda is a
!   ! cubic polynomial in lambda**2; solve using cubic formula for real root
!   ! Can write so that coefficient of x**3 is 1, other coefficients are:
!      b = -1.0_r8 * (sigma_o_2 + sigma_p_2 * lambda_mean)
!      c = lambda_sd_2 * sigma_p_2**2 / 2.0_r8
!      d = -1.0_r8 * (lambda_sd_2 * sigma_p_2**2 * dist_2) / 2.0_r8
!      Q = c - b**2 / 3
!      R = d + (2 * b**3) / 27 - (b * c) / 3
!      ! Compute discriminant, if this is negative have 3 real roots, else 1 real root
!      disc = R**2 / 4 + Q**3 / 27
!      if(disc < 0.0_r8) then
!         rrr = sqrt(-1.0 * Q**3 / 27)
!         ! Note that rrr is positive so no problem for cube root
!         cube_root_rrr = rrr ** (1.0 / 3.0)
!         angle = acos(-0.5 * R / rrr)
!         do i = 0, 2
!            mx(i+1) = 2.0_r8 * cube_root_rrr * cos((angle + i * 2.0_r8 * PI) / 3.0_r8) - b / 3.0_r8
!            mlambda(i + 1) = (mx(i + 1) - sigma_o_2) / sigma_p_2
!            sep(i+1) = abs(mlambda(i + 1) - lambda_mean)
!         end do
!         ! Root closest to initial peak is appropriate
!         mlambda_index = minloc(sep)
!         new_cov_inflate = mlambda(mlambda_index(1))
!      else
!         ! Only one real root here, find it.
!         ! Compute the two primary terms
!         alpha = -R/2 + sqrt(disc)
!         beta = R/2 + sqrt(disc)
!         cube_root_alpha = abs(alpha) ** (1.0 / 3.0) * abs(alpha) / alpha
!         cube_root_beta = abs(beta) ** (1.0 / 3.0) * abs(beta) / beta
!         x = cube_root_alpha - cube_root_beta - b / 3.0
!         ! This root is the value of x = theta**2
!         new_cov_inflate = (x - sigma_o_2) / sigma_p_2
!      endif
!      ! Put in code to approximate the mode (new_cov_inflate)
!      !write(*, *) 'old, orig mode is ', lambda_mean, new_cov_inflate
!   endif
   

!   
!   
!   
!   
!   
!   
!   
!   
!   
!   
!   

if (inf_type == AND2009) then
   ! Approximate with Taylor series for likelihood term

   call linear_bayes(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd_2, gamma_corr, &
      new_cov_inflate)
! Bail out to save cost when lower bound is reached on lambda standard deviation

if(lambda_sd <= sd_lower_bound_in) then
   new_cov_inflate_sd = lambda_sd
else
   ! Compute by forcing a Gaussian fit at one positive SD
! First compute the new_max value for normalization purposes
   new_max = compute_new_density(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd, &
                                    gamma_corr, new_cov_inflate)
! Find value at a point one OLD sd above new mean value

      new_1_sd = compute_new_density(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd, gamma_corr, &
      new_cov_inflate + lambda_sd)
   ! If either the numerator or denominator of the following computation 
   ! of 'ratio' is going to be zero (or almost so), return the original incoming
   ! inflation value.  The computation would have resulted in either Inf or NaN.

   if (abs(new_max) <= TINY(0.0_r8) .or. abs(new_1_sd) <= TINY(0.0_r8)) then
      new_cov_inflate_sd = lambda_sd
      return
   endif

   ratio = new_1_sd / new_max 
   ! Another error for numerical issues; if ratio is larger than 0.99, bail out

   if(ratio > 0.99) then
      new_cov_inflate_sd = lambda_sd
      return
   endif
   ! Can now compute the standard deviation consistent with this as
      ! sigma = sqrt(-x^2 / (2 ln(r))  where r is ratio and x is lambda_sd (distance from mean)

   new_cov_inflate_sd = sqrt( -1.0_r8 * lambda_sd_2 / (2.0_r8 * log(ratio)))
   ! Prevent an increase in the sd of lambda???
   ! For now, this is mostly countering numerical errors in this computation

   if(new_cov_inflate_sd > lambda_sd) new_cov_inflate_sd = lambda_sd

endif

else if (inf_type == GHA2017) then
   ! Transform Gaussian prior to Inverse Gamma

   call change_GA_IG(lambda_mean, lambda_sd_2, rate)
   ! Approximate with Taylor series for likelihood term

   call enh_linear_bayes(dist_2, sigma_p_2, sigma_o_2,lambda_mean, &
                    gamma_corr, ens_size, rate, new_cov_inflate)
   ! Bail out to save cost when lower bound is reached on lambda standard deviation

   if(lambda_sd <= sd_lower_bound_in) then
      new_cov_inflate_sd = lambda_sd
   
   else
      ! Compute the shape parameter of the prior IG
      ! This comes from the assumption that the mode of the IG is the mean/mode of the input Gaussian
      shape_old = rate / lambda_mean - 1.0_r8
      if (shape_old <= 2.0_r8) then
         new_cov_inflate_sd = lambda_sd
         return
      endif
      ! Evaluate the exact IG posterior at p1: \lambda_u+\sigma_{\lambda_b} & p2: \lambda_u
   
      density_1 = enh_compute_new_density(dist_2, ens_size, sigma_p_2, sigma_o_2, shape_old, &
                                          rate, gamma_corr, new_cov_inflate+lambda_sd)
      density_2 = enh_compute_new_density(dist_2, ens_size, sigma_p_2, sigma_o_2, shape_old, &
                                          rate, gamma_corr, new_cov_inflate)
      ! Computational errors check (small numbers + NaNs)
   
      if (abs(density_1) <= TINY(0.0_r8) .OR. &
          abs(density_2) <= TINY(0.0_r8) .OR. &
          density_1 /= density_1 .OR. density_1 /= density_1 .OR. &
          density_2 /= density_2 .OR. density_2 /= density_2) then
         new_cov_inflate_sd = lambda_sd
         return
      endif
      ! Now, compute omega and the new distribution parameters
   
      ratio     = density_1 / density_2
      omega     = log(new_cov_inflate          )/new_cov_inflate + 1.0_r8/new_cov_inflate - &
                  log(new_cov_inflate+lambda_sd)/new_cov_inflate - 1.0_r8/(new_cov_inflate+lambda_sd)
      rate_new  = log(ratio) / omega
      shape_new = rate_new / new_cov_inflate - 1.0_r8
      ! Finally, get the sd of the IG posterior
   
      if (shape_new <= 2.0_r8) then
         new_cov_inflate_sd = lambda_sd
         return
      endif
      new_cov_inflate_sd = sqrt(rate_new**2 / ( (shape_new-1.0_r8)**2 * (shape_new-2.0_r8) ))
      ! If the updated variance is more than xx% the prior variance, keep the prior unchanged 
      ! for stability reasons. Also, if the updated variance is NaN (not sure why this
      ! can happen; never did when develping this code), keep the prior variance unchanged. 
   
      if ( new_cov_inflate_sd > sd_max_change_in*lambda_sd .OR. &
          new_cov_inflate_sd /= new_cov_inflate_sd) new_cov_inflate_sd = lambda_sd
   endif
   
else
   write(msgstring, *) 'Internal error, should not happen.  Illegal value for bayes type.'
   call error_handler(E_ERR, 'bayes_cov_inflate', msgstring, source, revision, revdate)
endif

end subroutine bayes_cov_inflate
!------------------------------------------------------------------
!> Used to update density by taking approximate gaussian product
!> original routine.


function compute_new_density(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd, gamma, lambda)

real(r8), intent(in) :: dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd, gamma, lambda
real(r8)             :: compute_new_density

real(r8) :: theta_2, theta
real(r8) :: exponent_prior, exponent_likelihood
! Compute probability of this lambda being correct


exponent_prior = (lambda - lambda_mean)**2 / (-2.0_r8 * lambda_sd**2)
! Compute probability that observation would have been observed given this lambda

theta_2 = (1.0_r8 + gamma * (sqrt(lambda) - 1.0_r8))**2 * sigma_p_2 + sigma_o_2
theta = sqrt(theta_2)

exponent_likelihood = dist_2 / ( -2.0_r8 * theta_2)
! Compute the updated probability density for lambda
! Have 1 / sqrt(2 PI) twice, so product is 1 / (2 PI)

compute_new_density = exp(exponent_likelihood + exponent_prior) / &
   (2.0_r8 * PI * lambda_sd * theta)

end function compute_new_density
!------------------------------------------------------------------
!> new version


function enh_compute_new_density(dist_2, ens_size, sigma_p_2, sigma_o_2, alpha, beta, gamma_corr, lambda)
! Used to update density by taking approximate gaussian product

real(r8), intent(in) :: dist_2
integer , intent(in) :: ens_size
real(r8), intent(in) :: sigma_p_2, sigma_o_2, gamma_corr, lambda
real(r8), intent(in) :: alpha, beta
real(r8)             :: enh_compute_new_density

real(r8) :: theta, fac1, fac2
real(r8) :: exp_prior, exp_like
! Compute probability of this lambda being correct

exp_prior = - beta / lambda
! Compute probability that observation would have been observed given this lambda

fac1 = (1.0_r8 + gamma_corr * (sqrt(lambda) - 1.0_r8))**2
fac2 = -1.0_r8 / ens_size
if ( fac1 < abs(fac2) ) fac2 = 0.0_r8

theta    = sqrt( (fac1+fac2) * sigma_p_2 + sigma_o_2 )
exp_like = - 0.5_r8 * dist_2 / theta**2
! Compute the updated probability density for lambda

enh_compute_new_density = beta**alpha / gamma(alpha)  * &
                         lambda**(- alpha - 1.0_r8)  / &
                         (sqrt(2.0_r8 * PI) * theta) * &
                         exp(exp_like + exp_prior)

end function enh_compute_new_density
!---------------------------------------------------------------------
!> original linear_bayes routine


subroutine linear_bayes(dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd_2, gamma, &
   new_cov_inflate)

real(r8), intent(in)    :: dist_2, sigma_p_2, sigma_o_2, lambda_mean, lambda_sd_2
real(r8), intent(in)    :: gamma
real(r8), intent(inout) :: new_cov_inflate

real(r8) :: theta_bar_2, u_bar, like_exp_bar, v_bar, like_bar, like_prime, theta_bar
real(r8) :: a, b, c, plus_root, minus_root, dtheta_dlambda
! Compute value of theta at current lambda_mean
   
theta_bar_2 = (1.0_r8 + gamma * (sqrt(lambda_mean) - 1.0_r8))**2 * sigma_p_2 + sigma_o_2
theta_bar = sqrt(theta_bar_2)
! Compute constant coefficient for likelihood at lambda_bar
u_bar = 1.0_r8 / (sqrt(2.0_r8 * PI) * theta_bar)
! Compute exponent of likelihood at lambda_bar
like_exp_bar = dist_2 / (-2.0_r8 * theta_bar_2)
! Compute exponential part of likelihood at lambda_bar
v_bar = exp(like_exp_bar)
! Compute value of likelihood at current lambda_bar value
like_bar = u_bar * v_bar
! If like_bar goes to 0, can't do anything, so just keep current values

if(like_bar <= 0.0_r8) then
   new_cov_inflate = lambda_mean
   return
endif
! Next compute derivative of likelihood at this point
! First compute d/dlambda of theta evaluated at lambda_mean
! Verified correct by finite difference, 1 January, 2006


dtheta_dlambda = 0.5_r8 * sigma_p_2 * gamma *(1.0_r8 - gamma + gamma*sqrt(lambda_mean)) / &
   (theta_bar * sqrt(lambda_mean))
like_prime = (u_bar * v_bar * dtheta_dlambda / theta_bar) * (dist_2 / theta_bar_2 - 1.0_r8)
! If like_prime goes to 0, can't do anything, so just keep current values

if(like_prime == 0.0_r8) then
   new_cov_inflate = lambda_mean
   return
endif

a = 1.0_r8
b = like_bar / like_prime - 2.0_r8 * lambda_mean
c = lambda_mean**2 -lambda_sd_2 - like_bar * lambda_mean / like_prime
! Use nice scaled quadratic solver to avoid precision issues

call solve_quadratic(a, b, c, plus_root, minus_root)
! Do a check to pick closest root

if(abs(minus_root - lambda_mean) < abs(plus_root - lambda_mean)) then
   new_cov_inflate = minus_root
else
   new_cov_inflate = plus_root
endif

end subroutine linear_bayes
!---------------------------------------------------------------------
!> enhanced linear bayes


subroutine enh_linear_bayes(dist_2, sigma_p_2, sigma_o_2, &
           lambda_mean, gamma_corr, ens_size, beta, new_cov_inflate)

real(r8), intent(in)    :: dist_2, sigma_p_2, sigma_o_2, lambda_mean
real(r8), intent(in)    :: gamma_corr, beta
real(r8), intent(inout) :: new_cov_inflate

integer  :: ens_size
real(r8) :: theta_bar_2, like_bar, like_prime, theta_bar
real(r8) :: a, b, c, plus_root, minus_root, deriv_theta
real(r8) :: fac1, fac2, like_ratio
! Scaling factors

fac1 = (1.0_r8 + gamma_corr * (sqrt(lambda_mean) - 1.0_r8))**2
fac2 = -1.0_r8 / ens_size
! Compute value of theta at current lambda_mean

if ( fac1 < abs(fac2) ) fac2 = 0.0_r8
theta_bar_2 = (fac1+fac2) * sigma_p_2 + sigma_o_2
theta_bar   = sqrt(theta_bar_2)
! Compute constant coefficient for likelihood at lambda_bar

like_bar = exp(- 0.5_r8 * dist_2 / theta_bar_2) / (sqrt(2.0_r8 * PI) * theta_bar)
! If like_bar goes to 0, can't do anything, so just keep current values
! Density at current inflation value must be positive

if(like_bar <= 0.0_r8) then
   new_cov_inflate = lambda_mean
   return
endif
! Next compute derivative of likelihood at this point

deriv_theta = 0.5_r8 * sigma_p_2 * gamma_corr * ( 1.0_r8 - gamma_corr + &
              gamma_corr * sqrt(lambda_mean) ) / ( theta_bar * sqrt(lambda_mean) )
like_prime  = like_bar * deriv_theta * (dist_2 / theta_bar_2 - 1.0_r8) / theta_bar
! If like_prime goes to 0, can't do anything, so just keep current values
! We're dividing by the derivative in the quadratic equation, so this
! term better non-zero!

if(like_prime == 0.0_r8 .OR. abs(like_bar) <= TINY(0.0_r8) .OR. abs(like_prime) <= TINY(0.0_r8) ) then
   new_cov_inflate = lambda_mean
   return
endif
like_ratio = like_bar / like_prime

a = 1.0_r8 - lambda_mean / beta
b = like_ratio - 2.0_r8 * lambda_mean
c = lambda_mean**2 - like_ratio * lambda_mean
! Use nice scaled quadratic solver to avoid precision issues

call solve_quadratic(a, b, c, plus_root, minus_root)
! Do a check to pick closest root

if(abs(minus_root - lambda_mean) < abs(plus_root - lambda_mean)) then
   new_cov_inflate = minus_root
else
   new_cov_inflate = plus_root
endif
! Do a final check on the sign of the updated factor
! Sometimes the factor can be very small (almost zero) 
! From the selection process above it can be negative
! if the positive root is far away from it. 
! As such, keep the current factor value

if(new_cov_inflate <= 0.0_r8 .OR. new_cov_inflate /= new_cov_inflate) new_cov_inflate = lambda_mean

end subroutine enh_linear_bayes
!------------------------------------------------------------------------


subroutine solve_quadratic(a, b, c, r1, r2)

real(r8), intent(in)  :: a, b, c
real(r8), intent(out) :: r1, r2

real(r8) :: scaling, as, bs, cs, disc
! Scale the coefficients to get better round-off tolerance

scaling = max(abs(a), abs(b), abs(c))
as = a / scaling
bs = b / scaling
cs = c / scaling
! Get discriminant of scaled equation

disc = sqrt(bs**2 - 4.0_r8 * as * cs)

if(bs > 0.0_r8) then
   r1 = (-bs - disc) / (2 * as)
else
   r1 = (-bs + disc) / (2 * as)
endif
! Compute the second root given the larger one

r2 = (cs / as) / r1

end subroutine solve_quadratic
!----------------------------------------------
!> Routine to change the Gaussian prior into an inverse gamma (IG).
!> The Gaussian prior is represented by a mode (:= mean) and a variance; var 

subroutine change_GA_IG(mode, var, beta)

real(r8), intent(in)  :: mode, var
real(r8), intent(out) :: beta

integer :: i
real(r8) :: var_p(3), mode_p(9)   ! var and mode to the Nth power
real(r8) :: AA, BB, CC, DD, EE
! Computation savers - powers are computationally expensive

var_p(1) = var
do i=2, 3
   var_p(i) = var_p(i-1)*var
enddo

mode_p(1) = mode
do i = 2, 9
  mode_p(i) = mode_p(i-1)*mode
enddo
! Calculate the rate parameter for IG distribution.
! It's a function of both the prior mean and variannce, 
! obtained as a "real" solution to a cubic polynomial.

AA = mode_p(4) * sqrt((var_p(2) + 47.0_r8*var*mode_p(2) + 3.0_r8*mode_p(4)) / var_p(3))
BB = 75.0_r8*var_p(2)*mode_p(5)
CC = 21.0_r8*var*mode_p(7)
DD = var_p(3)*mode_p(3)
EE = (CC + BB + DD + mode_p(9) + 6.0_r8*sqrt(3.0_r8)*AA*var_p(3)) / var_p(3)

beta = (7.0_r8*var*mode + mode_p(3))/(3.0_r8*var)                               + &
       EE**(1.0_r8/3.0_r8)/3.0_r8 + mode_p(2)*(var_p(2) + 14.0_r8*var*mode_p(2) + &
       mode_p(4)) / (3.0_r8*var_p(2)*EE**(1.0_r8/3.0_r8))

end subroutine change_GA_IG
!------------------------------------------------------------------------
!> Write to log file what kind of inflation is being used.  


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
! Collect the min and max of inflation on task 0
! this block handles communicating the min/max local values to PE 0
! if running with MPI, or just sets the min/max directly if reading
! from a namelist.


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------


!========================================================================
! end module adaptive_inflate_mod
!========================================================================
!> @}


!read state subroutine for kr_externs_in_adaptive_inflate_mod 
SUBROUTINE kr_externs_in_adaptive_inflate_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) msgstring 
END SUBROUTINE kr_externs_in_adaptive_inflate_mod 
  
!read state subroutine for kr_externs_out_adaptive_inflate_mod 
SUBROUTINE kr_externs_out_adaptive_inflate_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_adaptive_inflate_mod 
  
!read state subroutine for kr_kgen_adaptive_inflate_mod_typesubp0 
RECURSIVE SUBROUTINE kr_kgen_adaptive_inflate_mod_typesubp0(var, kgen_unit, printname, printvar) 
    TYPE(adaptive_inflate_type), INTENT(INOUT) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) var%inflation_flavor 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%inflation_flavor = ", var%inflation_flavor 
    END IF   
      
    READ (UNIT = kgen_unit) var%inflation_sub_flavor 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%inflation_sub_flavor = ", var%inflation_sub_flavor 
    END IF   
      
    READ (UNIT = kgen_unit) var%output_restart 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%output_restart = ", var%output_restart 
    END IF   
      
    READ (UNIT = kgen_unit) var%deterministic 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%deterministic = ", var%deterministic 
    END IF   
      
    READ (UNIT = kgen_unit) var%inflate 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%inflate = ", var%inflate 
    END IF   
    READ (UNIT = kgen_unit) var%sd 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%sd = ", var%sd 
    END IF   
    READ (UNIT = kgen_unit) var%sd_lower_bound 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%sd_lower_bound = ", var%sd_lower_bound 
    END IF   
    READ (UNIT = kgen_unit) var%inf_lower_bound 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%inf_lower_bound = ", var%inf_lower_bound 
    END IF   
    READ (UNIT = kgen_unit) var%inf_upper_bound 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%inf_upper_bound = ", var%inf_upper_bound 
    END IF   
      
    READ (UNIT = kgen_unit) var%sd_max_change 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%sd_max_change = ", var%sd_max_change 
    END IF   
      
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        CALL kr_random_seq_mod_random_seq_type(var%ran_seq, kgen_unit, printname // "%ran_seq", .TRUE.) 
    ELSE 
        CALL kr_random_seq_mod_random_seq_type(var%ran_seq, kgen_unit, printname // "%ran_seq", .FALSE.) 
    END IF   
      
    READ (UNIT = kgen_unit) var%allow_missing_in_clm 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%allow_missing_in_clm = ", var%allow_missing_in_clm 
    END IF   
      
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) var%minmax_mean 
        CALL kgen_array_sumcheck(printname // "%minmax_mean", kgen_array_sum, DBLE(SUM(var%minmax_mean, mask=(var%minmax_mean &
        &.eq. var%minmax_mean))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%minmax_mean)) = ", DBLE(SUM(var%minmax_mean, &
            &mask=(var%minmax_mean .eq. var%minmax_mean))) 
        END IF   
    END IF   
    READ (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        READ (UNIT = kgen_unit) kgen_array_sum 
        READ (UNIT = kgen_unit) var%minmax_sd 
        CALL kgen_array_sumcheck(printname // "%minmax_sd", kgen_array_sum, DBLE(SUM(var%minmax_sd, mask=(var%minmax_sd .eq. &
        &var%minmax_sd))), .TRUE.) 
        IF (PRESENT( printvar ) .AND. printvar) THEN 
            WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%minmax_sd)) = ", DBLE(SUM(var%minmax_sd, mask=(var%minmax_sd &
            &.eq. var%minmax_sd))) 
        END IF   
    END IF   
      
    READ (UNIT = kgen_unit) var%mean_from_restart 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%mean_from_restart = ", var%mean_from_restart 
    END IF   
      
    READ (UNIT = kgen_unit) var%sd_from_restart 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%sd_from_restart = ", var%sd_from_restart 
    END IF   
      
    READ (UNIT = kgen_unit) var%prior 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%prior = ", var%prior 
    END IF   
      
    READ (UNIT = kgen_unit) var%posterior 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%posterior = ", var%posterior 
    END IF   
      
    READ (UNIT = kgen_unit) var%input_mean_copy 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%input_mean_copy = ", var%input_mean_copy 
    END IF   
      
    READ (UNIT = kgen_unit) var%input_sd_copy 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%input_sd_copy = ", var%input_sd_copy 
    END IF   
      
END SUBROUTINE kr_kgen_adaptive_inflate_mod_typesubp0 
  
!verify state subroutine for kv_kgen_adaptive_inflate_mod_typesubp0 
RECURSIVE SUBROUTINE kv_kgen_adaptive_inflate_mod_typesubp0(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    TYPE(adaptive_inflate_type), INTENT(IN) :: var, kgenref_var 
    TYPE(check_t) :: dtype_check_status, comp_check_status 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    integer :: diff_inflation_flavor 
    integer :: diff_inflation_sub_flavor 
    logical :: diff_output_restart 
    logical :: diff_deterministic 
    real(KIND=r8) :: diff_inflate 
    real(KIND=r8) :: diff_sd 
    real(KIND=r8) :: diff_sd_lower_bound 
    real(KIND=r8) :: diff_inf_lower_bound 
    real(KIND=r8) :: diff_inf_upper_bound 
    real(KIND=r8) :: diff_sd_max_change 
    logical :: diff_allow_missing_in_clm 
    INTEGER :: n_minmax_mean 
    real(KIND=r8) :: nrmsdiff_minmax_mean, rmsdiff_minmax_mean 
    real(KIND=r8), ALLOCATABLE :: buf1_minmax_mean(:), buf2_minmax_mean(:) 
    INTEGER :: n_minmax_sd 
    real(KIND=r8) :: nrmsdiff_minmax_sd, rmsdiff_minmax_sd 
    real(KIND=r8), ALLOCATABLE :: buf1_minmax_sd(:), buf2_minmax_sd(:) 
    logical :: diff_mean_from_restart 
    logical :: diff_sd_from_restart 
    logical :: diff_prior 
    logical :: diff_posterior 
    integer :: diff_input_mean_copy 
    integer :: diff_input_sd_copy 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%inflation_flavor == kgenref_var%inflation_flavor) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%inflation_flavor is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_inflation_flavor = ABS(var%inflation_flavor - kgenref_var%inflation_flavor) 
        IF (diff_inflation_flavor <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inflation_flavor is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inflation_flavor is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_inflation_flavor 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_inflation_flavor 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%inflation_sub_flavor == kgenref_var%inflation_sub_flavor) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%inflation_sub_flavor is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_inflation_sub_flavor = ABS(var%inflation_sub_flavor - kgenref_var%inflation_sub_flavor) 
        IF (diff_inflation_sub_flavor <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inflation_sub_flavor is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inflation_sub_flavor is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_inflation_sub_flavor 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_inflation_sub_flavor 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%output_restart .EQV. kgenref_var%output_restart) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%output_restart is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%output_restart is NOT IDENTICAL." 
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
    IF (var%deterministic .EQV. kgenref_var%deterministic) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%deterministic is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%deterministic is NOT IDENTICAL." 
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
    IF (var%inflate == kgenref_var%inflate) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%inflate is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_inflate = ABS(var%inflate - kgenref_var%inflate) 
        IF (diff_inflate <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inflate is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inflate is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_inflate 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_inflate 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%sd == kgenref_var%sd) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%sd is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_sd = ABS(var%sd - kgenref_var%sd) 
        IF (diff_sd <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%sd is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%sd is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_sd 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_sd 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%sd_lower_bound == kgenref_var%sd_lower_bound) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%sd_lower_bound is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_sd_lower_bound = ABS(var%sd_lower_bound - kgenref_var%sd_lower_bound) 
        IF (diff_sd_lower_bound <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%sd_lower_bound is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%sd_lower_bound is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_sd_lower_bound 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_sd_lower_bound 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%inf_lower_bound == kgenref_var%inf_lower_bound) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%inf_lower_bound is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_inf_lower_bound = ABS(var%inf_lower_bound - kgenref_var%inf_lower_bound) 
        IF (diff_inf_lower_bound <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inf_lower_bound is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inf_lower_bound is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_inf_lower_bound 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_inf_lower_bound 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%inf_upper_bound == kgenref_var%inf_upper_bound) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%inf_upper_bound is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_inf_upper_bound = ABS(var%inf_upper_bound - kgenref_var%inf_upper_bound) 
        IF (diff_inf_upper_bound <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inf_upper_bound is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%inf_upper_bound is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_inf_upper_bound 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_inf_upper_bound 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%sd_max_change == kgenref_var%sd_max_change) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%sd_max_change is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_sd_max_change = ABS(var%sd_max_change - kgenref_var%sd_max_change) 
        IF (diff_sd_max_change <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%sd_max_change is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%sd_max_change is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_sd_max_change 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_sd_max_change 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    CALL kgen_init_check(comp_check_status, rank=check_status%rank) 
    CALL kv_random_seq_mod_random_seq_type("ran_seq", comp_check_status, var%ran_seq, kgenref_var%ran_seq) 
    IF (comp_check_status%numTotal == comp_check_status%numIdentical) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname))//"%ran_seq", " is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE IF (comp_check_status%numOutTol > 0) THEN 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%ran_seq is NOT IDENTICAL(out of tolerance)." 
            END IF   
        END IF   
        check_result = CHECK_OUT_TOL 
    ELSE IF (comp_check_status%numInTol > 0) THEN 
        dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%ran_seq is NOT IDENTICAL(within tolerance)." 
            END IF   
        END IF   
        check_result = CHECK_IN_TOL 
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
                WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "       number of components             : ", comp_check_status%numtotal 
                WRITE (*, *) "       identical                                   : ", comp_check_status%numidentical 
                WRITE (*, *) "       not identical - out of tol.: ", comp_check_status%numouttol 
                WRITE (*, *) "       not identical - within tol.: ", comp_check_status%numintol 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%allow_missing_in_clm .EQV. kgenref_var%allow_missing_in_clm) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%allow_missing_in_clm is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%allow_missing_in_clm is NOT IDENTICAL." 
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
    IF (ALL(var%minmax_mean == kgenref_var%minmax_mean)) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%minmax_mean is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        ALLOCATE (buf1_minmax_mean(SIZE(var%minmax_mean,dim=1))) 
        ALLOCATE (buf2_minmax_mean(SIZE(var%minmax_mean,dim=1))) 
        n_minmax_mean = COUNT(var%minmax_mean /= kgenref_var%minmax_mean) 
        WHERE ( ABS(kgenref_var%minmax_mean) > kgen_minvalue ) 
            buf1_minmax_mean = ((var%minmax_mean-kgenref_var%minmax_mean)/kgenref_var%minmax_mean)**2 
            buf2_minmax_mean = (var%minmax_mean-kgenref_var%minmax_mean)**2 
        ELSEWHERE 
            buf1_minmax_mean = (var%minmax_mean-kgenref_var%minmax_mean)**2 
            buf2_minmax_mean = buf1_minmax_mean 
        END WHERE   
        nrmsdiff_minmax_mean = SQRT(SUM(buf1_minmax_mean)/REAL(n_minmax_mean)) 
        rmsdiff_minmax_mean = SQRT(SUM(buf2_minmax_mean)/REAL(n_minmax_mean)) 
        IF (rmsdiff_minmax_mean > kgen_tolerance) THEN 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%minmax_mean is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        ELSE 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%minmax_mean is NOT IDENTICAL(within tolerance)." 
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
                WRITE (*, *) count( var%minmax_mean /= kgenref_var%minmax_mean), " of ", size( var%minmax_mean ), " elements are &
                &different." 
                WRITE (*, *) "Average - kernel ", sum(var%minmax_mean)/real(size(var%minmax_mean)) 
                WRITE (*, *) "Average - reference ", sum(kgenref_var%minmax_mean)/real(size(kgenref_var%minmax_mean)) 
                WRITE (*, *) "RMS of difference is ", rmsdiff_minmax_mean 
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_minmax_mean 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) count( var%minmax_mean /= kgenref_var%minmax_mean), " of ", size( var%minmax_mean ), " elements are &
                &different." 
                WRITE (*, *) "Average - kernel ", sum(var%minmax_mean)/real(size(var%minmax_mean)) 
                WRITE (*, *) "Average - reference ", sum(kgenref_var%minmax_mean)/real(size(kgenref_var%minmax_mean)) 
                WRITE (*, *) "RMS of difference is ", rmsdiff_minmax_mean 
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_minmax_mean 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (ALL(var%minmax_sd == kgenref_var%minmax_sd)) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%minmax_sd is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        ALLOCATE (buf1_minmax_sd(SIZE(var%minmax_sd,dim=1))) 
        ALLOCATE (buf2_minmax_sd(SIZE(var%minmax_sd,dim=1))) 
        n_minmax_sd = COUNT(var%minmax_sd /= kgenref_var%minmax_sd) 
        WHERE ( ABS(kgenref_var%minmax_sd) > kgen_minvalue ) 
            buf1_minmax_sd = ((var%minmax_sd-kgenref_var%minmax_sd)/kgenref_var%minmax_sd)**2 
            buf2_minmax_sd = (var%minmax_sd-kgenref_var%minmax_sd)**2 
        ELSEWHERE 
            buf1_minmax_sd = (var%minmax_sd-kgenref_var%minmax_sd)**2 
            buf2_minmax_sd = buf1_minmax_sd 
        END WHERE   
        nrmsdiff_minmax_sd = SQRT(SUM(buf1_minmax_sd)/REAL(n_minmax_sd)) 
        rmsdiff_minmax_sd = SQRT(SUM(buf2_minmax_sd)/REAL(n_minmax_sd)) 
        IF (rmsdiff_minmax_sd > kgen_tolerance) THEN 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%minmax_sd is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        ELSE 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%minmax_sd is NOT IDENTICAL(within tolerance)." 
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
                WRITE (*, *) count( var%minmax_sd /= kgenref_var%minmax_sd), " of ", size( var%minmax_sd ), " elements are &
                &different." 
                WRITE (*, *) "Average - kernel ", sum(var%minmax_sd)/real(size(var%minmax_sd)) 
                WRITE (*, *) "Average - reference ", sum(kgenref_var%minmax_sd)/real(size(kgenref_var%minmax_sd)) 
                WRITE (*, *) "RMS of difference is ", rmsdiff_minmax_sd 
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_minmax_sd 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) count( var%minmax_sd /= kgenref_var%minmax_sd), " of ", size( var%minmax_sd ), " elements are &
                &different." 
                WRITE (*, *) "Average - kernel ", sum(var%minmax_sd)/real(size(var%minmax_sd)) 
                WRITE (*, *) "Average - reference ", sum(kgenref_var%minmax_sd)/real(size(kgenref_var%minmax_sd)) 
                WRITE (*, *) "RMS of difference is ", rmsdiff_minmax_sd 
                WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_minmax_sd 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%mean_from_restart .EQV. kgenref_var%mean_from_restart) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%mean_from_restart is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%mean_from_restart is NOT IDENTICAL." 
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
    IF (var%sd_from_restart .EQV. kgenref_var%sd_from_restart) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%sd_from_restart is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%sd_from_restart is NOT IDENTICAL." 
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
    IF (var%prior .EQV. kgenref_var%prior) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%prior is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%prior is NOT IDENTICAL." 
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
    IF (var%posterior .EQV. kgenref_var%posterior) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%posterior is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%posterior is NOT IDENTICAL." 
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
    IF (var%input_mean_copy == kgenref_var%input_mean_copy) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%input_mean_copy is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_input_mean_copy = ABS(var%input_mean_copy - kgenref_var%input_mean_copy) 
        IF (diff_input_mean_copy <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%input_mean_copy is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%input_mean_copy is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_input_mean_copy 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_input_mean_copy 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%input_sd_copy == kgenref_var%input_sd_copy) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%input_sd_copy is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_input_sd_copy = ABS(var%input_sd_copy - kgenref_var%input_sd_copy) 
        IF (diff_input_sd_copy <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%input_sd_copy is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%input_sd_copy is NOT IDENTICAL(out of tolerance)." 
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
                WRITE (*, *) "Difference is ", diff_input_sd_copy 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_input_sd_copy 
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
END SUBROUTINE kv_kgen_adaptive_inflate_mod_typesubp0 
  
end module adaptive_inflate_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/adaptive_inflate_mod.f90 $ 
! $Id: adaptive_inflate_mod.f90 12939 2018-11-27 15:34:34Z nancy@ucar.edu $ 
! $Revision: 12939 $ 
! $Date: 2018-11-27 08:34:34 -0700 (Tue, 27 Nov 2018) $ 
