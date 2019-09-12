!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:30 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
!$Id$
!===============================================================================


module math_utilities
!-----------------------------------------------------------------------
! Various mathematical utilities
!-----------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    IMPLICIT NONE 

    PUBLIC compute_sample_mean, rand_integer_in_range 

    PRIVATE 

  contains
!-----------------------------------------------------------------------

  pure function compute_sample_mean( n_levels, n_samples, weight, x_sample ) &
    result( mean )
! Description:
!   Find the mean of a set of sample points
! References:
!   None
!-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! External

    intrinsic :: real, sum
    ! Input Varibles

    integer, intent(in) :: &
      n_levels, &
      n_samples

    real( kind = core_rknd ),dimension(n_levels,n_samples), intent(in) :: &
      weight   ! Weights for individual points of the vector

    real( kind = core_rknd ),dimension(n_levels,n_samples), intent(in) :: &
      x_sample ! Collection of sample points    [units vary]
    ! Return type

    real( kind = core_rknd ), dimension(n_levels) :: mean

    integer :: k
    ! ---- Begin Code ----
    ! Get rid of an annoying compiler warning.


    k = 1
    k = k

    forall( k = 1:n_levels )
      mean(k) = sum( weight(k,1:n_samples) * x_sample(k,1:n_samples) ) &
              / real( n_samples, kind=core_rknd )
    end forall


    return

  end function compute_sample_mean
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------


  !-----------------------------------------------------------------------

  function rand_integer_in_range(low, high)
  ! Description:
  !   Returns a uniformly distributed integer in the range [low,high]
  !   using the Mersenne Twister PRNG library.
  !   The integers returned from this function are actually not quite
  !   evenly distributed because of the use of MOD. Smaller numbers are
  !   slightly more likely than larger ones. This could be fixed someday.
  ! References:
  !   None
  !-----------------------------------------------------------------------
    ! Included Modules

  !


      USE mt95, ONLY: genrand_intg, genrand_int32 

    implicit none
    ! Local Constants
    ! Input Variables


    integer, intent(in) :: &
      low,   &      ! Lowest possible returned value
      high          ! Highest possible returned value
    ! Output Variable

    integer :: &
      rand_integer_in_range  ! Random integer in range [low,high]
    ! Local Variables

    integer( kind = genrand_intg ) :: &
      rand_32                ! Random integer in range[-2^31, +2^31-1]

    integer :: &
      range_width
  !-----------------------------------------------------------------------
    !----- Begin Code -----


    range_width = high - low + 1
    call genrand_int32( rand_32 )
    rand_integer_in_range = abs( mod( rand_32, range_width ) ) + low

    return
  end function rand_integer_in_range
  !-----------------------------------------------------------------------


end module math_utilities