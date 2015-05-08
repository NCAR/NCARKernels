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

end program random
