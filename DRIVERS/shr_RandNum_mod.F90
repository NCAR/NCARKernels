#ifdef INTEL_MKL
include 'mkl_vsl.f90'
#endif

module shr_RandNum_mod

! this module contains the driver for the available versions of random number generator
 
use mersennetwister_mod, only : new_RandomNumberSequence, getRandomReal, &
                                randomNumberSequence, finalize_RandomNumberSequence
use dSFMT_interface,     only : dSFMT_init, dSFMT_end, get_rand_arr_close_open, dSFMT_t
use kissvec_mod,         only : kissvec

#ifdef INTEL_MKL
use mkl_vsl_type
use mkl_vsl
#endif
 implicit none

integer, parameter :: r8 = selected_real_kind(12)

integer :: errcode

real(r8) :: a=0.0_r8, b=1.0_r8
integer  :: brng, method

type shr_rand_t 
  character(len=32) :: type
  integer           :: nstream
  integer           :: length 
  integer, allocatable, dimension(:) :: iseed1, iseed2, iseed3, iseed4
  integer, allocatable, dimension(:) :: iseed
#ifdef INTEL_MKL
  type (VSL_STREAM_STATE), allocatable :: vsl_stream(:)
#endif
  type (dSFMT_t),              allocatable :: rng(:)
  type (randomNumberSequence), allocatable :: randomseq(:)
end type  

public shr_genRandNum, shr_RandNum_init, shr_RandNum_term, shr_rand_t

contains 

subroutine shr_genRandNum(randStream, array)

  type (shr_rand_t),        intent(inout) :: randStream
  real(r8), dimension(:,:), intent(inout) :: array

  integer :: i, n, nstream, length
  integer, dimension(1) :: seed

  nstream = randStream%nstream 
  length  = randStream%length 

  select case (randStream%type)

#ifdef INTEL_MKL
! intel math kernel library SIMD fast merseene twister 19937
    case ("SFMT_MKL")
    do n=1,nstream
      errcode = vdrnguniform( method, randStream%vsl_stream(n), length, array(n,:), a, b)
    enddo
#endif
 
! keep it simple stupid
    case("KISSVEC")
    do i=1,length
      call kissvec( randStream%iseed1, randStream%iseed2, randStream%iseed3, randStream%iseed4, array(:,i), nstream )
    enddo

! fortran-95 implementation of merseene twister 19937
    case("MT19937")
    do i=1,length
      do n=1,nstream
        array(n,i) = getRandomReal( randStream%randomseq(n) )
      enddo
    enddo

! fortran-90 intrinsic pseudorandom number generator
    case("F90_INTRINSIC")
    do n=1,nstream
      seed(1) = randStream%iseed(n)
      call random_seed( put=seed )
      call random_number( array(n,:) )
    enddo

! SIMD-oriented fast merseene twister 19937
  case("DSFMT_F03")
  do n=1,nstream
    call get_rand_arr_close_open( randStream%rng(n), array(n,:), length )
  enddo

end select

end subroutine shr_genRandNum

subroutine shr_RandNum_init( randStream, nstream, length, type, iseed1, iseed2, iseed3, &
                                                                          iseed4, iseed )
 
  type (shr_rand_t),                     intent(inout) :: randStream 
  integer, intent(in)                                  :: nstream  ! number of streams of random numbers
  integer, intent(in)                                  :: length   ! length of stream of random numbers
  character(len=*),                      intent(in)    :: type
  integer, dimension(nstream), optional, intent(in)    :: iseed1, iseed2, iseed3, iseed4
  integer, dimension(nstream), optional, intent(in)    :: iseed 

  integer :: i, n

  randStream%nstream = nstream
  randStream%length  = length 
  randStream%type    = to_upper(type)

  select case (randStream%type)

#ifdef INTEL_MKL
! intel math kernel library SIMD fast merseene twister 19937
    case ("SFMT_MKL")
    method = VSL_RNG_METHOD_UNIFORM_STD
    brng   = VSL_BRNG_SFMT19937
    ! brng   = VSL_BRNG_MT19937
    !  brng = 0
    ! print *,'brng: ',brng
  
    if( .NOT. allocated(randStream%vsl_stream) ) allocate(randStream%vsl_stream(nstream))

    do n=1,nstream
      errcode = vslnewstream( randStream%vsl_stream(n), brng, seed=iseed(n) )
    enddo
#endif

! keep it simple stupid
    case("KISSVEC")
    if( .NOT. allocated(randStream%iseed1) ) allocate(randStream%iseed1(nstream))
    if( .NOT. allocated(randStream%iseed2) ) allocate(randStream%iseed2(nstream))
    if( .NOT. allocated(randStream%iseed3) ) allocate(randStream%iseed3(nstream))
    if( .NOT. allocated(randStream%iseed4) ) allocate(randStream%iseed4(nstream))

! fortran-95 implementation of merseene twister 19937
    case("MT19937")
    if( .NOT. allocated(randStream%randomseq) ) allocate(randStream%randomseq(nstream))

    do n=1,nstream
      randStream%randomseq(n) = new_RandomNumberSequence( seed=iseed(n) )
    enddo

! fortran-90 intrinsic pseudorandom number generator
    case("F90_INTRINSIC")
    if( .NOT. allocated(randStream%iseed) ) allocate(randStream%iseed(nstream))
    
    do n=1,nstream
      randStream%iseed(n) = iseed(n) 
    enddo

! SIMD-oriented fast merseene twister 19937
    case("DSFMT_F03")
    if( .NOT. allocated(randStream%rng) ) allocate(randStream%rng(nstream))
    do n=1,nstream
      call dSFMT_init(iseed(n), length, randStream%rng(n) )
    enddo
  
  end select

end subroutine shr_RandNum_init

subroutine shr_RandNum_term( randStream )

type (shr_rand_t), intent(inout) :: randStream

integer :: n, nstream,ierr

  nstream = randStream%nstream

  select case (randStream%type)

#ifdef INTEL_MKL
! intel math kernel library SIMD fast merseene twister 19937
    case ("SFMT_MKL")
       if( allocated (randStream%vsl_stream) ) then 
           do n=1,nstream
             ierr = vslDeleteStream(randStream%vsl_stream(n))
           enddo
           deallocate (randStream%vsl_stream)
       endif
#endif

! keep it simple stupid
    case("KISSVEC")
    if ( allocated (randStream%iseed1) ) deallocate (randStream%iseed1)
    if ( allocated (randStream%iseed2) ) deallocate (randStream%iseed2)
    if ( allocated (randStream%iseed3) ) deallocate (randStream%iseed3)
    if ( allocated (randStream%iseed4) ) deallocate (randStream%iseed4)

! fortran-95 implementation of merseene twister 19937
    case("MT19937")
    do n=1,nstream
      call finalize_RandomNumberSequence(randStream%randomseq(n))
    enddo
    if ( allocated(randStream%randomseq) ) deallocate (randStream%randomseq)

! fortran-90 intrinsic pseudorandom number generator
    case("F90_INTRINSIC")
    if ( allocated(randStream%iseed) ) deallocate(randStream%iseed)

! SIMD-oriented fast merseene twister 19937
    case("DSFMT_F03")
    do n=1,nstream
      call dSFMT_end(randStream%rng(n))
    enddo
    if ( allocated (randStream%rng) ) deallocate (randStream%rng)

  end select

end subroutine shr_RandNum_term

function to_upper(strIn) result(strOut)

     character(len=*), intent(in) :: strIn
     character(len=len(strIn)) :: strOut
     integer :: i, j

     do i = 1,len(strIn)
          j = iachar(strIn(i:i))
          if (j>= iachar("a") .and. j<=iachar("z") ) then
               strOut(i:i) = achar(iachar(strIn(i:i))-32)
          else
               strOut(i:i) = strIn(i:i)
          end if
     end do

end function to_upper

end module shr_RandNum_mod
