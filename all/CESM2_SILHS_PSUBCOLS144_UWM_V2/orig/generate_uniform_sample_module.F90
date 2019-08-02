!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:55 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
!$Id: generate_uniform_sample_module.F90 91096 2019-04-29 22:54:55Z cacraig@ucar.edu $
!===============================================================================


module generate_uniform_sample_module
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PUBLIC permute_height_time, rand_permute, rand_uniform_real, generate_uniform_lh_sample, choose_permuted_random 

    PRIVATE 

  integer, private :: &
    prior_iter ! Prior iteration number (for diagnostic purposes)
  PUBLIC kr_externs_in_generate_uniform_sample_module 
  PUBLIC kr_externs_out_generate_uniform_sample_module 
  PUBLIC kv_externs_generate_uniform_sample_module 
  INTEGER :: kgenref_prior_iter 
!$omp threadprivate( prior_iter )

  contains
!-----------------------------------------------------------------------

  function rand_uniform_real( )
  ! Description:
  !   Generates a uniformly distributed random real number in the range
  !   (0,1) using the Mersenne Twister random number generator
  ! References:
  !   None
  !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE mt95, ONLY: genrand_real, genrand_real3 

      USE constants_clubb, ONLY: one 

    implicit none
    ! Output Variable

    real( kind = core_rknd ) :: &
      rand_uniform_real      ! A randomly distributed real number in the range (0,1)
    ! Local Variable

    real( kind = genrand_real ) :: &
      rand_uniform_real_genrand_real
  !-----------------------------------------------------------------------
    !----- Begin Code -----

    call genrand_real3( rand_uniform_real_genrand_real )

    rand_uniform_real = real( rand_uniform_real_genrand_real, kind=core_rknd )
    ! It is theoretically possible that the resulting real number is equal to
    ! one if core_rknd is not the same as genrand_real. Here, we apply clipping
    ! to prevent the output real number from being exactly equal to one.

    if ( rand_uniform_real >= one ) then
      rand_uniform_real = one - epsilon( rand_uniform_real )
    end if

    return
  end function rand_uniform_real
!-----------------------------------------------------------------------
!-------------------------------------------------------------------------------

  subroutine generate_uniform_lh_sample( iter, num_samples, sequence_length, n_vars, &
                                         X_u_one_lev )
  ! Description:
  !   Generates a matrix X that contains a Latin Hypercube sample.
  !   The sample is uniformly distributed.
  ! References:
  ! https://arxiv.org/pdf/1711.03675v1.pdf#nameddest=url:lh_algorithm
  !   See Art B. Owen (2003), ``Quasi-Monte Carlo Sampling,"
  !      a chapter from SIGGRAPH 2003
  !-------------------------------------------------------------------------------

  !

      USE clubb_precision, ONLY: core_rknd 

      USE latin_hypercube_arrays, ONLY: one_height_time_matrix 

      USE constants_clubb, ONLY: fstderr 

    implicit none
    ! Local Constants

    logical, parameter :: &
      l_diagnostic_iter_check = .true.  ! Perform check to make sure None is being called
                                        ! correctly
    ! Input Variables

    integer, intent(in) :: &
      iter,            & ! Model iteration number
      num_samples,     & ! `n'  Number of samples generated
      sequence_length, & ! `n_t' Num. random samples before sequence repeats
      n_vars             ! Number of uniform variables to generate
    ! Output Variables


    real(kind=core_rknd), intent(out), dimension(num_samples,n_vars) :: &
      X_u_one_lev        ! num_samples by n_vars matrix, X
                         ! each row of which is a n_vars-dimensional sample
    ! Local Variables


    integer :: j, k, nt_repeat, i_rmd
    ! ---- Begin Code ----


    nt_repeat = num_samples * sequence_length

    if ( .not. allocated( one_height_time_matrix ) ) then
      ! If this is first time latin_hypercube_driver is called, then allocate
      ! the one_height_time_matrix and set the prior iteration number for debugging
      ! purposes.
      allocate( one_height_time_matrix(nt_repeat, n_vars) )

      prior_iter = iter

    else if ( l_diagnostic_iter_check .and. sequence_length > 1 ) then
      ! Check for a bug where the iteration number isn't incrementing correctly,
      ! which will lead to improper sampling.

      if ( prior_iter /= iter-1 ) then
        write(fstderr,*) "The iteration number in latin_hypercube_driver is"// &
        " not incrementing properly."
      else
        prior_iter = iter
      end if
    end if ! First call to the driver
    ! Latin hypercube sample generation
    ! Generate one_height_time_matrix, an nt_repeat x pdf_dimarray of random integers

    i_rmd = mod( iter-1, sequence_length )

    if ( i_rmd == 0 ) then
      call permute_height_time( nt_repeat, n_vars, &          ! intent(in)
                                one_height_time_matrix )      ! intent(out)
    end if
    ! End Latin hypercube sample generation
    ! Choose values of sample using permuted vector and random number generator

    do j = 1,num_samples
      do k = 1,n_vars
        X_u_one_lev(j,k) = choose_permuted_random( nt_repeat, one_height_time_matrix(j,k) )
      end do
    end do

    return
  end subroutine generate_uniform_lh_sample
!----------------------------------------------------------------------
!----------------------------------------------------------------------

  function choose_permuted_random( nt_repeat, p_matrix_element )
! Description:
!   Chooses a permuted random using the Mersenne Twister algorithm.
! References:
!   None
!----------------------------------------------------------------------

!

      USE clubb_precision, ONLY: core_rknd 

      USE constants_clubb, ONLY: one 

    implicit none
    ! Input Variables

    integer, intent(in) :: &
      nt_repeat,        & ! Number of samples before the sequence repeats
      p_matrix_element    ! Permuted integer
    ! Output Variable

    real(kind=core_rknd) :: choose_permuted_random
    ! Local Variable

    real(kind=core_rknd) :: &
      rand ! Random float with a range of (0,1)
    ! ---- Begin Code ----


    rand = rand_uniform_real( )

    choose_permuted_random = (one / real( nt_repeat, kind=core_rknd) ) &
       *( real( p_matrix_element, kind=core_rknd ) + rand )

    return
  end function choose_permuted_random
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

  subroutine permute_height_time( nt_repeat, n_vars, one_height_time_matrix )
  ! Description:
  !   Generates a matrix one_height_time_matrix, which is a nt_repeat x n_vars
  !   matrix whose 1st dimension is random permutations of the integer sequence 
  !   (0,...,nt_repeat-1).
  ! References:
  !   None
  !-----------------------------------------------------------------------


    implicit none
    ! Input Variables


    integer, intent(in) :: &
      nt_repeat,  & ! Total number of sample points before sequence repeats.
      n_vars        ! The number of variates in the uniform sample
    ! Output Variables


    integer, dimension(nt_repeat,n_vars), intent(out) :: &
      one_height_time_matrix ! nt_repeat x n_vars matrix of integers
    ! Local Variables


    integer :: i
    ! Choose elements of one_height_time_matrix, with a random integer LH sample
    ! for each variate

    do i = 1, n_vars
      call rand_permute( nt_repeat, one_height_time_matrix(1:nt_repeat,i) )
    end do

    return
  end subroutine permute_height_time
!------------------------------------------------------------------------
!------------------------------------------------------------------------

  subroutine rand_permute( n, pvect )
  ! Description:
  !   Generates a vector of length n
  !      containing the integers 0, ... , n-1 in random order.
  !   We do not use a new seed.
  ! References:
  !   Follow `Quasi-Monte Carlo sampling' by Art Owen, Section 1.3
  !   He follows, in turn, Luc Devroye 'Non-uniform random ...' (1986)
  !----------------------------------------------------------------------


      USE mt95, ONLY: genrand_real3 

      USE mt95, ONLY: genrand_real 

    implicit none
    ! External


    intrinsic :: int
    ! Input Variables


    integer, intent(in) :: n ! Number of elements to permute
    ! Output Variables


    integer, dimension(n), intent(out) :: &
      pvect ! Array of n numbers in random order
    ! Local Variables


    integer j, k, temp

    real(kind=genrand_real) :: rand ! Random float on interval (0,1)
    ! Start with an ordered vector, pvect

    do j=1,n
      pvect(j) = j
    end do
    ! Now re-arrange the elements

    do j=n,2,-1
      temp = pvect(j)
      call genrand_real3( rand ) ! real3 excludes 0 and 1.
      ! choose an element randomly between 1 and j
      k = int( real( j, kind=genrand_real )*rand+1.0_genrand_real )
      ! swap elements j and k
      pvect(j) = pvect(k)
      pvect(k) = temp
    end do
    ! Convert range of array from 1:n to 0:n-1

    do j=1,n
      pvect(j) = pvect(j) - 1
    end do

    return
  end subroutine rand_permute
!------------------------------------------------------------------------

  !read state subroutine for kr_externs_in_generate_uniform_sample_module 
  SUBROUTINE kr_externs_in_generate_uniform_sample_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) prior_iter 
  END SUBROUTINE kr_externs_in_generate_uniform_sample_module 
    
  !read state subroutine for kr_externs_out_generate_uniform_sample_module 
  SUBROUTINE kr_externs_out_generate_uniform_sample_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      READ (UNIT = kgen_unit) kgenref_prior_iter 
  END SUBROUTINE kr_externs_out_generate_uniform_sample_module 
    
  !verify state subroutine for kv_externs_generate_uniform_sample_module 
  SUBROUTINE kv_externs_generate_uniform_sample_module(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_kgen_generate_uniform_sample_module_subp3("prior_iter", check_status, prior_iter, kgenref_prior_iter) 
  END SUBROUTINE kv_externs_generate_uniform_sample_module 
    
  !verify state subroutine for kv_kgen_generate_uniform_sample_module_subp3 
  RECURSIVE SUBROUTINE kv_kgen_generate_uniform_sample_module_subp3(varname, check_status, var, kgenref_var) 
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
        
  END SUBROUTINE kv_kgen_generate_uniform_sample_module_subp3 
    
end module generate_uniform_sample_module