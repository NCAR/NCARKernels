program random

! this program calls the available versions of the random generators

use shr_RandNum_mod, only : shr_genRandNum, shr_RandNum_init, shr_RandNum_term, shr_rand_t

INTEGER,parameter :: r8 = selected_real_kind(12)

type (shr_rand_t) :: randStream

integer(r8), parameter     :: nstream = 30000
integer(r8), parameter     :: length  = 1
integer(r8)                :: ntrials = 50000

integer, dimension(length)  :: iseed  = 7776578
integer, dimension(nstream) :: iseed1
integer, dimension(nstream) :: iseed2
integer, dimension(nstream) :: iseed3
integer, dimension(nstream) :: iseed4

real(r8), dimension(nstream,length) :: array

integer     :: i, n, m
integer(r8) :: c1, c2, cr, cm
real   (r8) :: dt

#ifdef INTEL_MKL
! intel math kernel library merseene twister

  call system_clock(c1, cr, cm)

  do m = 1,ntrials
    call shr_RandNum_init( randStream, nstream, length, 'SFMT_MKL', iseed=iseed )

    call shr_genRandNum  ( randStream, array )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time   (SFMT_MKL): ',dt
  print *, 'MegaRNumbers (SFMT_MKL): ', 1.0e-6*dble(nstream*length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(array)
  print *, '--------'; print *, ''
#endif

! keep it simple stupid random number

  call system_clock(c1, cr, cm)

  do m = 1,ntrials
    do n = 1,nstream
      iseed1(n) = iseed(n)*1+n; iseed2(n) = iseed(n)*2+n
      iseed3(n) = iseed(n)*3+n; iseed4(n) = iseed(n)*4+n
    enddo
    call shr_RandNum_init( randStream, nstream, length, 'KISSVEC', &
         iseed1=iseed1, iseed2=iseed2, iseed3=iseed3, iseed4=iseed4 )

    call shr_genRandNum  ( randStream, array )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time   (KISSVEC): ',dt
  print *, 'MegaRNumbers (KISSVEC): ', 1.0e-6*dble(nstream*length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(array)
  print *, '--------'; print *, ''

! fortran-95 implementation of merseene twister

  call system_clock(c1, cr, cm)

  do m = 1,ntrials
    call shr_RandNum_init( randStream, nstream, length, 'MT19937', iseed=iseed )

    call shr_genRandNum  ( randStream, array )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time   (MT19937): ',dt
  print *, 'MegaRNumbers (MT19937): ', 1.0e-6*dble(nstream*length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(array)
  print *, '--------'; print *, ''

! fortran-90 intrinsic pseudorandom number generator
  
  call system_clock(c1, cr, cm)

  do m = 1,ntrials
    call shr_RandNum_init( randStream, nstream, length, 'F90_INTRINSIC', iseed=iseed )

    call shr_genRandNum  ( randStream, array )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time   (F90_INTRINSIC): ',dt
  print *, 'MegaRNumbers (F90_INTRINSIC): ', 1.0e-6*dble(nstream*length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(array)
  print *, '--------'; print *, ''

! SIMD-orientated merseene twister

  call system_clock(c1, cr, cm)

  do m = 1,ntrials
    call shr_RandNum_init( randStream, nstream, length, 'dSFMT_F03', iseed=iseed )

    call shr_genRandNum  ( randStream, array )
  enddo

  call system_clock(c2, cr, cm); dt = dble(c2-c1)/dble(cr)

  print *, 'Total time   (dSFMT_F03): ',dt
  print *, 'MegaRNumbers (dSFMT_F03): ', 1.0e-6*dble(nstream*length*ntrials)/dt
  print *, 'Summation of Random Numbers: ', SUM(array)
  print *, '--------'; print *, ''

end program random
