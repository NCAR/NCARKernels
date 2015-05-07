program random

! this program calls the available versions of the random generators

use random_number_mod, only : RanGen

integer(KIND=8), parameter :: length =  30000 
integer(KIND=8)            :: ntrials = 50000
integer            :: i
integer(KIND=8)    :: c1, c2, cr, cm
real   (KIND=8)    :: dt
real(KIND=8), dimension(length) :: radvec

! intel math kernel library merseene twister

  call system_clock(c1, cr, cm)
  do i = 1,ntrials
    radvec = RanGen( length, 'SFMT_MKL' ) ! , .true. ) 
  enddo
  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (SFMT_MKL):   ',dt
  print *, 'MegaRNumbers (SFMT_MKL): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! keep it simple stupid random number

  call system_clock(c1, cr, cm)
  do i = 1,ntrials
    radvec = RanGen( length, 'KISSVEC' ) ! , .true. )
  enddo
  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (KISSVEC):   ',dt
  print *, 'MegaRNumbers (KISSVEC): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! fortran-95 implementation of merseene twister

  call system_clock(c1, cr, cm)
  do i = 1,ntrials
    radvec = RanGen( length, 'MT19937' ) ! , .true. )
  enddo
  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (MT19937):   ',dt
  print *, 'MegaRNumbers (MT19937): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! pseudo-random number
  
  call system_clock(c1, cr, cm)
  do i = 1,ntrials
    radvec =  RanGen( length, 'F90_INTRINSIC' ) ! , .true. )
  enddo
  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (F90_INTRINSIC):   ',dt
  print *, 'MegaRNumbers (F90_INTRINSIC): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

! SIMD-orientated merseene twister

  call system_clock(c1, cr, cm)
  do i = 1,ntrials
    radvec = RanGen( length, 'dSFMT_F03' ) ! , .true. )
  enddo
  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time (dSFMT_F03):   ',dt
  print *, 'MegaRNumbers (dSFMT_F03): ', 1.0e-6*dble(length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(radvec)
  print *, '--------'; print *, ''

end program random
