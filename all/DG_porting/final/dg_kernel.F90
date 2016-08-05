! DG_KERNEL on CPU, GPU, and MIC

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

        MODULE mod_grad_dg

        CONTAINS

	! Note: Tgrad is not used. We don't need it passed in -- it's a temp

        SUBROUTINE grad_dg(nit,dt,der,delta,gw,flx,fly,delta_der2,delta_der3)

        INTEGER, INTENT(IN) :: nit
        REAL*8, INTENT(IN) :: dt
        REAL*8, INTENT(IN) :: der(SET_NX,SET_NX)
        REAL*8, INTENT(IN) :: delta(SET_NX,SET_NX)
        REAL*8, INTENT(IN) :: gw(SET_NX)
        REAL*8, DIMENSION(SET_NX*SET_NX,SET_NELEM), INTENT(INOUT) :: flx, fly
        REAL*8, DIMENSION(SET_NX*SET_NX,SET_NX*SET_NX), INTENT(IN) :: delta_der2, delta_der3


	! Temporary arrays created to accumulate (grad/grad2) and to access
	! data sequentially using ii index (see below)
	!
        REAL*8, DIMENSION(SET_NX*SET_NX) :: grad
        REAL*8, DIMENSION(SET_NX*SET_NX, SET_NX) :: grad2



!DEC$ ATTRIBUTES ALIGN: 64 :: grad, grad2

        INTEGER :: i, j, k, l, ii, ie, it, ji

!DEC$ ASSUME_ALIGNED flx:64, fly:64, delta_der2:64, delta_der3:64


            !$OMP DO
            DO ie=1, SET_NELEM
            ! initialize grad/grad2. We use grad2 as an accumulator,
            ! instead of s1 in the original code
	    DO j=1,SET_NX   
	       !DEC$ vector always aligned
               DO ii=1, SET_NX*SET_NX
                   grad2(ii, j) = 0.0_8   ! init grad2
		   grad(ii) = 0.0_8       ! init grad
               END DO
            END DO

            ! Main compuation, accumulated into grad2. Note that
	    ! flx, fly use the same indexing as before. The other
	    ! smaller arrays like delta, der have been precomputed
	    ! to be amenable to sequential accesses.
	    !
            DO j=1,SET_NX	  ! j is the 3rd level outer loop

               DO i=1, SET_NX      ! i is the 2nd level outer loop

	          ji = (j-1)*SET_NX + i     ! calculate index into flx,fly

		  !DEC$ vector always aligned
                  DO ii=1, SET_NX*SET_NX  ! inner loop over ii

		     grad2(ii,j) = grad2(ii,j)  + &
		     	       ( delta_der2(ii,ji)*flx(ji,ie) + &
			       delta_der3(ii,ji)*fly(ji,ie) ) * gw(i)
	
                  END DO ! ii-loop

               END DO ! i-loop		    

	       ! Accumulation into grad
	       ! 
	       !DEC$ vector always aligned
               DO ii=1, SET_NX*SET_NX 
		   grad(ii) =  grad(ii) + grad2(ii,j) * gw(j)
	       ENDDO

            END DO ! j


            !DEC$ vector always aligned
            DO ii=1, SET_NX*SET_NX
                flx(ii,ie) = flx(ii,ie)+ dt*grad(ii)
                fly(ii,ie) = fly(ii,ie)+ dt*grad(ii)
            END DO ! ii
         END DO ! ie
         !$OMP END DO nowait

        END SUBROUTINE

        END MODULE


        PROGRAM dg_kernel

        USE mod_grad_dg
        USE omp_lib

        IMPLICIT NONE

        INTEGER, PARAMETER :: nx    =   SET_NX      ! element order
        INTEGER, PARAMETER :: npts  =   nx*nx
        INTEGER, PARAMETER :: nelem =   SET_NELEM
        INTEGER :: nit   =   SET_NIT     ! iteration count

        REAL*8 :: dt     = .005D0        ! fake timestep 

        REAL*8 :: der(nx,nx)                        ! derivative matrix
        REAL*8 :: delta(nx,nx)                      ! Kronecker delta function
        REAL*8 :: gw(nx)                            ! Gaussian wts
        REAL*8, DIMENSION(:,:), ALLOCATABLE :: grad
        REAL*8, DIMENSION(:,:), ALLOCATABLE :: flx, fly

        REAL*8 :: zero, half, one
        REAL*8 :: start_time, stop_time, elapsed_time
        INTEGER :: start_clock, stop_clock, rate_clock, val
        CHARACTER(len=1024) :: arg
        LOGICAL :: file_exists


        REAL*8 :: delta_der2(SET_NX*SET_NX,SET_NX*SET_NX)
        REAL*8 :: delta_der3(SET_NX*SET_NX,SET_NX*SET_NX)
        INTEGER :: i, j, k, l, ii, ie, it, ji


        ALLOCATE( flx(nx*nx,nelem) )
        ALLOCATE( fly(nx*nx,nelem) )
        ALLOCATE( grad(nx*nx, nelem) )

!DEC$ ATTRIBUTES ALIGN: 64 :: flx, fly, delta_der2, delta_der3

        zero = 0.0_8
        half = 0.5_8
        one  = 1.0_8

        IF (COMMAND_ARGUMENT_COUNT() > 0) THEN
            CALL GET_COMMAND_ARGUMENT(1, arg)
            READ (arg,"(I10)", ERR=100) nit
        END IF

        ! Init static matrices
        der(:,:)    = one
        gw(:)       = half

        delta(:,:)  = zero
        delta(1,1)  = one
        delta(2,2)  = one

        ! Load up some initial values
        flx(:,:)    = one
        fly(:,:)    = -one

        !$OMP PARALLEL
        val = omp_get_num_threads()
        !$OMP END PARALLEL



        start_time = omp_get_wtime()

	! Intialize temporary arrays, so we can access them sequentially using ii
	!
	DO j=1,SET_NX            
           DO i=1,SET_NX           
	      ji = (j-1)*SET_NX + i             ! index into tmp arrays
	      !DEC$ vector always
              DO ii=1, SET_NX*SET_NX
                  k = MODULO(ii-1,SET_NX) + 1
                  l = (ii - 1)/SET_NX + 1
	          delta_der2(ii,ji) = delta(l,j)*der(i,k)
	          delta_der3(ii,ji) = delta(i,k)*der(j,l)
              END DO
	   END DO      ! i-loop    
        END DO       ! j-loop 

        !$OMP PARALLEL SHARED(flx,fly,nit) PRIVATE(it,ie,ii,i,j,k,l,ji) DEFAULT(shared)
        DO it=1, nit                    ! original code

	 !DEC$ noinline
          CALL grad_dg (nit,dt,der,delta,gw,flx,fly,delta_der2, delta_der3)

        END DO ! it
        !$OMP END PARALLEL

        stop_time = omp_get_wtime()

        elapsed_time = stop_time - start_time

        WRITE(*, *) "****************** RESULT ********************"
        WRITE(*, *)
        WRITE(*, "(A,I2,A,I2,A)") "DG_KERNEL VERSION (",0," ,",0," )"
        WRITE(*, *)
        WRITE(*, "(A, I1,A,I2,A,I10,A,I8)")  "TARGET = ",0,", NX = ",SET_NX,", NELEM = ",SET_NELEM,", NIT = ", nit
        WRITE(*, "(A,E15.7)") "MAX(flx) = ", MAXVAL(flx)
        WRITE(*, "(A,E15.7)") "MIN(fly) = ", MINVAL(fly)
        WRITE(*, "(A,F7.2)") "Gflops   = ",(1.0d-9*nit*nelem*npts*(nx*nx*7.D0+2.D0*nx+4.0D0))/elapsed_time
        WRITE(*, "(A,F10.3,A)") 'completed in ', elapsed_time, ' seconds'

        IF ( ALLOCATED(flx) ) DEALLOCATE(flx)
        IF ( ALLOCATED(fly) ) DEALLOCATE(fly)
        IF ( ALLOCATED(grad) ) DEALLOCATE(grad)

        CALL EXIT()

100     WRITE(*, *) "Usage: dg_kernel_[SNB|MIC|ACC|CUF|F2C].exe [# of iteration] [no_show_info]"

        END PROGRAM dg_kernel
