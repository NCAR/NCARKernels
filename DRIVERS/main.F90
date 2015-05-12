program random

! this program calls the available versions of the random generators

 use random_number_mod, only : RanGen, RanGen_init
 use kissvec_mod,       only : kissvec

INTEGER,parameter :: r8 = selected_real_kind(12)

integer(r8), parameter     :: length =  30000 
integer(r8)                :: ntrials = 50000
integer                    :: iseed = 7776578
integer, dimension(length) :: seed1
integer, dimension(length) :: seed2
integer, dimension(length) :: seed3
integer, dimension(length) :: seed4
integer            :: i, n
integer(r8)        :: c1, c2, cr, cm
real   (r8)        :: dt
real   (r8), dimension(length) :: radvec

! intel math kernel library merseene twister

  call system_clock(c1, cr, cm)

  do n = 1,ntrials
    call RanGen_init( iseed, 'SFMT_MKL', length )
    radvec = RanGen( length, 'SFMT_MKL' ) 
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (SFMT_MKL):   ',dt
  print *, 'MegaRNumbers (SFMT_MKL): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! keep it simple stupid random number

  call system_clock(c1, cr, cm)

  do n = 1,ntrials
   do i = 1,length
     seed1(i) = seed*1+i; seed2(i) = seed*2+i
     seed3(i) = seed*3+i; seed4(i) = seed*4+i
   enddo
     call kissvec( seed1, seed2, seed3, seed4, radvec )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (KISSVEC):   ',dt
  print *, 'MegaRNumbers (KISSVEC): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! fortran-95 implementation of merseene twister

  call system_clock(c1, cr, cm)

  do n = 1,ntrials
    call RanGen_init( iseed, 'MT19937', length )
    radvec = RanGen( length, 'MT19937' )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (MT19937):   ',dt
  print *, 'MegaRNumbers (MT19937): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! pseudo-random number
  
  call system_clock(c1, cr, cm)

  do n = 1,ntrials
    call RanGen_init( iseed, 'F90_INTRINSIC', length )
    radvec =  RanGen( length, 'F90_INTRINSIC' ) 
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (F90_INTRINSIC):   ',dt
  print *, 'MegaRNumbers (F90_INTRINSIC): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! SIMD-orientated merseene twister

  call system_clock(c1, cr, cm)

  do n = 1,ntrials
    call RanGen_init( iseed, 'dSFMT_F03', length )
    radvec = RanGen( length, 'dSFMT_F03' ) 
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (dSFMT_F03):   ',dt
  print *, 'MegaRNumbers (dSFMT_F03): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

#ifdef KISSVEC_INLINE
contains
subroutine kissvec( seed1, seed2, seed3, seed4, ran_arr)

! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
!  Overall period>2^123; 
!
    REAL(r8), DIMENSION(:), INTENT(INOUT) :: ran_arr
    INTEGER,  DIMENSION(:), INTENT(INOUT) :: seed1, seed2, seed3, seed4
    INTEGER(r8) :: i, kiss
    INTEGER     :: sz, k, n
    integer     :: low_byte_in

    sz = SIZE(seed1)
    do i=1,sz
      seed1(i) = 69069 * seed1(i) + 1327217885
      seed2(i) = m (m (m (seed2(i), 13), - 17), 5)
      seed3(i) = 18000 * iand (seed3(i), 65535) + ishft (seed3(i), - 16)
      seed4(i) = 30903 * iand (seed4(i), 65535) + ishft (seed4(i), - 16)
      kiss       = seed1(i) + seed2(i) + ishft (seed3(i), 16) + seed4(i)
      ran_arr(i) = low_byte(kiss)*2.328306e-10_r8 + 0.5
    enddo

  end subroutine kissvec

  integer function m(k, n)
    integer, intent(in) :: k
    integer, intent(in) :: n

    m = ieor (k, ishft (k, n) )

  end function m

  integer function low_byte(i)
  integer(r8), intent(in) :: i

  integer(r8), parameter :: one = 1
  logical                :: big_endian

#ifdef BIG_ENDIAN
  low_byte = transfer(ishft(kiss,bit_size(1)),1)
#else
  low_byte = kiss
#endif

! big_endian = (transfer(one, 1) == 0)
! if (big_endian) then
!   low_byte = transfer(ishft(i,bit_size(1)),1)
! else
!   low_byte = transfer(i,1)
! end if

  end function low_byte
#endif

end program random
