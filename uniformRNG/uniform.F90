     include 'mkl_vsl.f90'
 
      program MKL_VSL_GAUSSIAN
 
      USE MKL_VSL_TYPE
      USE MKL_VSL

      use kissvec_mod, only : kissvec
 
      integer, parameter :: n = 100000
      integer, parameter :: ntrials = 10000
      real(kind=8) r(n),rk(n)  ! buffer for random numbers
      real(kind=8) s,sk        ! average
      real(kind=8) a, b     ! parameters of normal distribution
 
      TYPE (VSL_STREAM_STATE) :: stream
 
      integer(kind=4) errcode
      integer(kind=4) i,j
      integer brng,method,seed

      integer :: seed1(n),seed2(n),seed3(n), seed4(n)
      integer*8 c1,c2,ck1,ck2,cr,cm
      real*8 dt,dtk
!dir$ attributes align : 64 :: r,rk
!dir$ attributes align : 64 :: seed1,seed2,seed3,seed4
 
      s = 0.0
      a = 0.0
      b = 1.0
!      sigma  = 2.0
!      brng=VSL_BRNG_SMT19937
      brng=VSL_BRNG_SFMT19937
!      method=VSL_RNG_METHOD_GAUSSIAN_ICDF
      method=VSL_RNG_METHOD_UNIFORM_STD
!      method=VSL_RNG_METHOD_UNIFORM_STD_ACCURATE
      seed=7776578
      do i=1,n
         seed1(i) = seed*1+i
         seed2(i) = seed*2+i
         seed3(i) = seed*3+i
         seed4(i) = seed*4+i
      enddo
 
!     ***** Initializing *****
      errcode=vslnewstream( stream, brng,  seed )
 
!     ***** Generating *****
      call system_clock(c1,cr,cm)
      do i = 1,ntrials
          errcode=vdrnguniform( method, stream, n, r, a, b )
      end do
      call system_clock(c2,cr,cm)
      dt = dble(c2-c1)/dble(cr)


      call system_clock(ck1,cr,cm)
      do i = 1,ntrials
          call kissvec(seed1,seed2,seed3,seed4,rk)
      end do
      call system_clock(ck2,cr,cm)
      dtk = dble(ck2-ck1)/dble(cr)
 
!      s = s / real(n*ntrials,kind=8)
!      sk = sk / real(n*ntrials,kind=8)
 
!     ***** Deinitialize *****
      errcode=vsldeletestream( stream )
 
!     ***** Printing results *****
!      print *,"Sample mean of normal distribution = ", s
!      print *,"Sample mean of normal distribution = ", sk
       print *,'Total time (MKL): ',dt
       print *,' MegaRNumbers (MKL): ', 1.0e-6*dble(n*ntrials)/dt 
       print *,'------'
       print *,'Total time (kissvec): ',dtk
       print *,' MegaRNumbers (kissvec): ', 1.0e-6*dble(n*ntrials)/dtk 


 
      end
