include 'mkl_vsl.f90'

module random_number_mod
! this module contains the driver for the available versions of random number generator
 
use mersennetwister_mod
use dSFMT_interface

use mkl_vsl_type
use mkl_vsl

integer, parameter :: r8 = selected_real_kind(12)

integer :: errcode
integer :: brng, method, seed

real(r8) :: a=0.0, b=1.0

type(VSL_STREAM_STATE)     :: stream
type(dSFMT_t)              :: rng
type(randomNumberSequence) :: randomNumbers

public RanGen, Rand_init

contains 

 function RanGen( length, name ) result( randvec )
  integer,                         intent(in)    :: length
  character(len=*),                intent(in)    :: name
  real(r8),     dimension(length)                :: randvec

  integer, dimension(length) :: seed1, seed2, seed3, seed4
  integer :: i

  select case (name)

! intel math kernel library SIMD fast merseene twister 19937
  case ("SFMT_MKL")
    method  = VSL_RNG_METHOD_UNIFORM_STD
    errcode = vdrnguniform( method, stream, length, randvec, a, b )
  
! fortran-95 implementation of merseene twister 19937
  case("MT19937")

    do i=1,length
      randvec(i) = getRandomReal( randomNumbers )
    enddo

! pseudorandom number generator
  case("F90_INTRINSIC")

    call random_number( randvec )

! SIMD-oriented fast merseene twister 19937
  case("dSFMT_F03")
    
    call get_rand_arr_close_open( rng, randvec, length )
  
end select

end function RanGen

subroutine RanGen_init( iseed, name, length ) 

  integer,                         intent(in)    :: iseed 
  character(len=*),                intent(in)    :: name
  integer,                         intent(in)    :: length

  select case (name)

! intel math kernel library SIMD fast merseene twister 19937
  case ("SFMT_MKL")

    method  = VSL_RNG_METHOD_UNIFORM_STD
    brng    = VSL_BRNG_SFMT19937
    errcode = vslnewstream( stream, brng,  seed=iseed )

! keep it simple stupid
! case("KISSVEC")

! call kissvec_init( seed1, seed2, seed3, seed4, vseed )

! fortran-95 implementation of merseene twister 19937
  case("MT19937")

    randomNumbers = new_RandomNumberSequence( seed=iseed )

! pseudorandom number generator
  case("F90_INTRINSIC")

    call random_seed()

! SIMD-oriented fast merseene twister 19937
  case("dSFMT_F03")

    call dSFMT_init(seed, length, rng )

end select

end subroutine RanGen_init

end module random_number_mod
