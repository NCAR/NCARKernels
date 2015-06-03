module kissvec_mod
! public domain code
! made available from http://www.fortran.com/
! downloaded by pjr on 03/16/04
! converted to vector form, functions inlined by pjr,mvr on 05/10/2004

implicit none

INTEGER,parameter :: r8 = selected_real_kind(12)
INTEGER,parameter :: i8 = selected_int_kind(12)
LOGICAL :: BIG_ENDIAN

private
public :: kissvec

contains

  subroutine kissvec( seed1, seed2, seed3, seed4, ran_arr, length)

! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
!  Overall period>2^123;
!
    INTEGER,                     INTENT(IN)    :: length
    REAL(r8), DIMENSION(length), INTENT(INOUT) :: ran_arr
    INTEGER,  DIMENSION(length), INTENT(INOUT) :: seed1, seed2, seed3, seed4
    INTEGER :: kiss
    INTEGER :: sz, k, n, i

    big_endian = (transfer(1_i8, 1) == 0)

    if ( big_endian ) then
      sz = SIZE(seed1)
      do i=1,sz
        kiss = 69069*seed1(i) + 1327217885 
        seed1(i) = transfer(ishft(kiss,bit_size(1)),1)
        seed2(i) = m (m (m (seed2(i), 13), - 17), 5)
        seed3(i) = 18000 * iand (seed3(i), 65535) + ishft (seed3(i), - 16)
        seed4(i) = 30903 * iand (seed4(i), 65535) + ishft (seed4(i), - 16)
        kiss = seed1(i) + seed2(i) + ishft (seed3(i), 16) + seed4(i)
        ran_arr(i) = low_byte(kiss)*2.328306e-10_r8 + 0.5_r8
    enddo
  else
      sz = SIZE(seed1)
      do i=1,sz
        kiss = 69069*seed1(i) + 1327217885   
        seed1(i) = kiss
        seed2(i) = m (m (m (seed2(i), 13), - 17), 5)
        seed3(i) = 18000 * iand (seed3(i), 65535) + ishft (seed3(i), - 16)
        seed4(i) = 30903 * iand (seed4(i), 65535) + ishft (seed4(i), - 16)
        kiss = seed1(i) + seed2(i) + ishft (seed3(i), 16) + seed4(i)
        ran_arr(i) = kiss*2.328306e-10_r8 + 0.5_r8
    enddo
  endif

  end subroutine kissvec

  pure integer function m(k, n)
    integer, intent(in) :: k
    integer, intent(in) :: n

    m = ieor (k, ishft (k, n) )

  end function m

  pure integer function low_byte(i)
  integer, intent(in) :: i

  if (big_endian) then
    low_byte = transfer(ishft(i,bit_size(1)),1)
  else
    low_byte = transfer(i,1)
  end if

  end function low_byte


end module kissvec_mod
