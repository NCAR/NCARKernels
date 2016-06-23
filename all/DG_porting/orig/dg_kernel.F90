! User defined parameters
#ifndef SET_NX
    #define SET_NX      4
#endif
#ifndef SET_NELEM
    #define SET_NELEM   6*120*120
#endif
#ifndef SET_NIT
    #define SET_NIT     1000 
#endif

!!------------------------------------------------
 !R.Nair NCAR/scd 08/03
 !Qudrature of the RHS evaluation 
!!------------------------------------------------

      PROGRAM Grad_Term_GPU
      
      USE omp_lib

      IMPLICIT NONE

      INTEGER, PARAMETER :: DOUBLE=SELECTED_REAL_KIND(p=14,r=100)

      INTEGER, PARAMETER :: nx=SET_NX      ! element order
      INTEGER, PARAMETER :: npts=nx*nx
      INTEGER, PARAMETER :: nit=SET_NIT   ! iteration count
      INTEGER, PARAMETER :: nelem=SET_NELEM

      REAL(KIND=DOUBLE), PARAMETER :: dt=.005D0 ! fake timestep

      REAL(KIND=DOUBLE) :: der(nx,nx)   ! Derivative matrix
      REAL(KIND=DOUBLE) :: delta(nx,nx) ! Kronecker delta function
      REAL(KIND=DOUBLE) :: gw(nx)       ! Gaussian wts
      REAL(KIND=DOUBLE), DIMENSION(nx*nx,nelem) :: flx,fly
      REAL(KIND=DOUBLE), DIMENSION(nx*nx,nelem) :: grad     

      REAL(KIND=DOUBLE) :: s1, s2
      REAL(KIND=DOUBLE) :: start_time, stop_time, elapsed_time

      INTEGER :: i, j, k, l, ii, ie, it, dummy

      ! Init static matrices

      der(:,:)=1.0_8
      gw(:) = 0.5_8

      delta(:,:)=0.0_8
      delta(1,1)=1.0_8
      delta(2,2)=1.0_8

      ! Load up some initial values

      flx(:,:) = 1.0_8
      fly(:,:) = -1.0_8

      !$OMP PARALLEL
      dummy = omp_get_num_threads()
      !$OMP END PARALLEL  

      start_time = omp_get_wtime()

      !$OMP PARALLEL DEFAULT(NONE) SHARED(flx,fly,grad,delta,der,gw) PRIVATE(it,ie,ii,i,j,k,l,s2,s1)
      DO it=1,nit
      !$OMP DO
      DO ie=1,nelem
         DO ii=1,npts
            k=MODULO(ii-1,nx)+1
            l=(ii-1)/nx+1
            s2 = 0.0_8
            DO j = 1, nx
               s1 = 0.0_8
!JMD               DO i = 1, nx
                  i = 1
                  s1 = (delta(l,j)*flx(i+(j-1)*nx,ie)*der(i,k) + &
                             delta(i,k)*fly(i+(j-1)*nx,ie)*der(j,l))*gw(i)
                  i = i+1 
                  s1 = s1 + (delta(l,j)*flx(i+(j-1)*nx,ie)*der(i,k) + &
                             delta(i,k)*fly(i+(j-1)*nx,ie)*der(j,l))*gw(i)
                  i = i+1 
                  s1 = s1 + (delta(l,j)*flx(i+(j-1)*nx,ie)*der(i,k) + &
                             delta(i,k)*fly(i+(j-1)*nx,ie)*der(j,l))*gw(i)
                  i = i+1 
                  s1 = s1 + (delta(l,j)*flx(i+(j-1)*nx,ie)*der(i,k) + &
                             delta(i,k)*fly(i+(j-1)*nx,ie)*der(j,l))*gw(i)
!JMD               END DO  ! i loop
               s2 = s2 + s1*gw(j) 
            END DO ! j loop
            grad(ii,ie) = s2
         END DO ! i1 loop
!JMD      END DO ! ie
!JMD      !$OMP END DO

     !write(*,*) "DOne with gradient"

!JMD      !$OMP DO
!JMD      DO ie=1,nelem
!DEC$ vector always
         DO ii=1,npts
            flx(ii,ie) = flx(ii,ie)+ dt*grad(ii,ie)
            fly(ii,ie) = fly(ii,ie)+ dt*grad(ii,ie)
         END DO
      END DO
      !$OMP END DO
      
      END DO ! iteration count, it
      !$OMP END PARALLEL

      stop_time = omp_get_wtime()

      elapsed_time = stop_time - start_time

      WRITE(*, *) "****************** RESULT ********************"
      WRITE(*, *)
      WRITE(*, "(A,I2,A,I10,A,I8)") "NX = ",SET_NX,", NELEM = ",SET_NELEM,", NIT = ", nit
      WRITE(*, "(A,E15.7)") "MAX(flx) = ", MAXVAL(flx)
      WRITE(*, "(A,E15.7)") "MIN(fly) = ", MINVAL(fly)
      WRITE(*, "(A,F7.2)") "Analytic Gflops   = ",(1.0d-9*nit*nelem*npts*(nx*nx*7.D0+2.D0*nx+4.0D0))/elapsed_time
      WRITE(*, "(A,F10.3,A)") 'completed in ', elapsed_time, ' seconds'

      END PROGRAM Grad_Term_GPU

