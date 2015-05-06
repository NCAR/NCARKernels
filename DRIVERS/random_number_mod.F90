include 'mkl_vsl.f90'

module random_number_mod
! this module contains the driver for the available versions of random number generator
 
use kissvec_mod
use mersennetwister_mod
use dSFMT_interface

use mkl_vsl_type
use mkl_vsl

integer :: errcode
integer :: brng, method, seed, vseed = 7776578

real(KIND=8) :: a=0.0, b=1.0

type(VSL_STREAM_STATE)     :: stream
type(dSFMT_t)              :: rng
type(randomNumberSequence) :: randomNumbers

public RanGen

contains 

 function RanGen( length, name ) result( randvec )
  integer,                         intent(in)    :: length
  character(len=*),                intent(in)    :: name
  real(KIND=8), dimension(length)                :: randvec

  integer, dimension(length) :: seed1, seed2, seed3, seed4
  integer :: i

  select case (name)

! intel math kernel library SIMD fast merseene twister 19937
  case ("SFMT_MKL")
    method  = VSL_RNG_METHOD_UNIFORM_STD
    brng    = VSL_BRNG_SFMT19937
    errcode = vslnewstream( stream, brng,  seed=vseed )
    errcode = vdrnguniform( method, stream, length, randvec, a, b )
  
! keep it simple stupid
  case("KISSVEC")
    do i=1,length
      seed1(i) = seed*1+i; seed2(i) = seed*2+i 
      seed3(i) = seed*3+i; seed4(i) = seed*4+i
    enddo
    call kissvec( seed1, seed2, seed3, seed4, randvec)

! fortran-95 implementation of merseene twister 19937
  case("MT19937")
    randomNumbers = new_RandomNumberSequence( seed=vseed )
    do i=1,length
      randvec(i) = getRandomReal( randomNumbers )
    enddo

! pseudorandom number generator
  case("F90_INTRINSIC")
    call random_seed()
    call random_number( randvec )

! SIMD-oriented fast merseene twister 19937
  case("dSFMT_F03")
    call dSFMT_init(seed, length, rng )
    call get_rand_arr_close_open( rng, randvec, length )
  
end select

end function RanGen

end module random_number_mod
