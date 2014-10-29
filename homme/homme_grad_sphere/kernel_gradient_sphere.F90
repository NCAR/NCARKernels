        program kgen_kernel_gradient_sphere

        INTEGER(KIND=4)  , PARAMETER :: real_kind = 8

        REAL(KIND=real_kind)  , PARAMETER :: rearth = 6.376d6

        REAL(KIND=real_kind)  , PARAMETER :: rrearth = 1.0_real_kind/rearth

        INTEGER , PARAMETER  :: np = 4

        INTEGER , Parameter  :: nelem = 64

        INTEGER , PARAMETER  :: nc = 4

        INTEGER , PARAMETER  :: nip = 3

        INTEGER , PARAMETER  :: nipm = nip-1

        INTEGER , PARAMETER  :: nep = nipm*nc+1

            TYPE  :: derivative_t
              REAL(KIND=real_kind) dvv(np,np)
              REAL(KIND=real_kind) dvv_diag(np,np)
              REAL(KIND=real_kind) dvv_twt(np,np)
              REAL(KIND=real_kind) mvv_twt(np,np)
              ! diagonal matrix of GLL weights
              REAL(KIND=real_kind) mfvm(np,nc+1)
              REAL(KIND=real_kind) cfvm(np,nc)
              REAL(KIND=real_kind) sfvm(np,nep)
              REAL(KIND=real_kind) legdg(np,np)
            END TYPE derivative_t


            REAL(KIND=real_kind) s(np, np,nelem)
            TYPE(derivative_t) deriv
            REAL(KIND=real_kind) , DIMENSION(2, 2, np, np) :: dinv
            REAL(KIND=real_kind) KGEN_RESULT_ds(np, np, 2,nelem)
            REAL(KIND=real_kind) KGEN_ds(np, np, 2)

        !JMD manual timer additions
        integer*8 c1,c2,cr,cm
        real*8 dt
        real*8 flops 
        integer :: itmax=100000
        character(len=80), parameter :: kname='[kernel_gradient_sphere]'
        integer :: it
        !JMD


            ! populate dummy initial values
            do j=1,np
                do i=1,np
                    Dinv(1,1,i,j) = 0.2_real_kind * j
                    Dinv(2,1,i,j) = 0.3_real_kind * i*j
                    Dinv(2,1,i,j) = 0.4_real_kind * i
                    Dinv(2,2,i,j) = 0.5_real_kind * j
                    s(i,j,:) = 0.6_real_kind * i*j
                    deriv%Dvv(i,j) = 0.8_real_kind * j
                end do
            end do

            ! reference result
            KGEN_ds = gradient_sphere_ref(s,deriv,dinv)

            call system_clock(c1,cr,cm)
            ! modified result
            do it=1,itmax
               do ie=1,nelem
                  KGEN_RESULT_ds(:,:,:,ie) = gradient_sphere(s(:,:,ie),deriv,dinv)
               enddo
            enddo
            call system_clock(c2,cr,cm)
            dt = dble(c2-c1)/dble(cr)
            flops = real(nelem,kind=real_kind)*real(4*np*np*np + 5*np*np,kind=real_kind)*real(itmax,kind=real_kind)
            print *, TRIM(kname), ' total time (sec): ',dt
            print *, TRIM(kname), ' Gflop rate: ', 1.0e-9*flops/dt
            print *, TRIM(kname), ' time per call (usec): ',1.e6*dt/dble(itmax)





            IF ( ALL( KGEN_ds == KGEN_RESULT_ds(:,:,:,1) ) ) THEN
                WRITE(*,*) "ds is identical."
                WRITE(*,*) "Modified: ", KGEN_ds
                WRITE(*,*) "Reference:  ", KGEN_RESULT_ds(:,:,:,1)
            ELSE
                WRITE(*,*) "ds is NOT identical."
                WRITE(*,*) COUNT( KGEN_ds /= KGEN_RESULT_ds(:,:,:,1)), " of ", SIZE( KGEN_RESULT_ds(:,:,:,1) ), " elements are different."
                WRITE(*,*) "RMS of difference is ", SQRT(SUM((KGEN_ds - KGEN_RESULT_ds(:,:,:,1))**2)/SIZE(KGEN_ds))
                WRITE(*,*) "Minimum difference is ", MINVAL(ABS(KGEN_ds - KGEN_RESULT_ds(:,:,:,1)))
                WRITE(*,*) "Maximum difference is ", MAXVAL(ABS(KGEN_ds - KGEN_RESULT_ds(:,:,:,1)))
                WRITE(*,*) "Mean value of kernel-generated ds is ", SUM(KGEN_RESULT_ds(:,:,:,1))/SIZE(KGEN_RESULT_ds(:,:,:,1))
                WRITE(*,*) "Mean value of original ds is ", SUM(KGEN_ds)/SIZE(KGEN_ds)
                WRITE(*,*) ""
                STOP
            END IF

        contains

        function gradient_sphere_ref(s,deriv,Dinv) result(ds)
        !
        !   input s:  scalar
        !   output  ds: spherical gradient of s, lat-lon coordinates
        !

              type (derivative_t), intent(in) :: deriv
              real(kind=real_kind), intent(in), dimension(2,2,np,np) :: Dinv
              real(kind=real_kind), intent(in) :: s(np,np)

              real(kind=real_kind) :: ds(np,np,2)

              integer i
              integer j
              integer l

              real(kind=real_kind) ::  dsdx00
              real(kind=real_kind) ::  dsdy00
              real(kind=real_kind) ::  v1(np,np),v2(np,np)

              do j=1,np
                    do l=1,np
                          dsdx00=0.0d0
                          dsdy00=0.0d0
                          do i=1,np
                                dsdx00 = dsdx00 + deriv%Dvv(i,l  )*s(i,j  )
                                dsdy00 = dsdy00 + deriv%Dvv(i,l  )*s(j  ,i)
                          end do
                          v1(l  ,j  ) = dsdx00*rrearth
                          v2(j  ,l  ) = dsdy00*rrearth
                    end do
              end do
        ! convert covarient to latlon
              do j=1,np
                    do i=1,np
                          ds(i,j,1)=Dinv(1,1,i,j)*v1(i,j) + Dinv(2,1,i,j)*v2(i,j)
                          ds(i,j,2)=Dinv(1,2,i,j)*v1(i,j) + Dinv(2,2,i,j)*v2(i,j)
                    enddo
              enddo

        end function gradient_sphere_ref


        function gradient_sphere(s,deriv,Dinv) result(ds)
        !
        !   input s:  scalar
        !   output  ds: spherical gradient of s, lat-lon coordinates
        !

              type (derivative_t), intent(in) :: deriv
              real(kind=real_kind), intent(in), dimension(2,2,np,np) :: Dinv
              real(kind=real_kind), intent(in) :: s(np,np)

              real(kind=real_kind) :: ds(np,np,2)

              integer i
              integer j
              integer l

              real(kind=real_kind) ::  dsdx00
              real(kind=real_kind) ::  dsdy00
              real(kind=real_kind) ::  v1(np,np),v2(np,np)

              do j=1,np
                    do l=1,np
                          dsdx00=0.0d0
                          dsdy00=0.0d0
                          do i=1,np
                                dsdx00 = dsdx00 + deriv%Dvv(i,l  )*s(i,j  )
                                dsdy00 = dsdy00 + deriv%Dvv(i,l  )*s(j  ,i)
                          end do
                          v1(l  ,j  ) = dsdx00*rrearth
                          v2(j  ,l  ) = dsdy00*rrearth
                    end do
              end do
        ! convert covarient to latlon
              do j=1,np
                    do i=1,np
                          ds(i,j,1)=Dinv(1,1,i,j)*v1(i,j) + Dinv(2,1,i,j)*v2(i,j)
                          ds(i,j,2)=Dinv(1,2,i,j)*v1(i,j) + Dinv(2,2,i,j)*v2(i,j)
                    enddo
              enddo

        end function gradient_sphere

        end program kgen_kernel_gradient_sphere
