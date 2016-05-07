        program kgen_kernel_laplace_sphere_wk

        INTEGER , PARAMETER  :: np = 4
        INTEGER(KIND=4)  , PARAMETER :: real_kind = 8
        REAL(KIND=real_kind)  , PARAMETER :: rearth = 6.376d6
        REAL(KIND=real_kind)  , PARAMETER :: rrearth = 1.0_real_kind/rearth
        REAL(KIND=real_kind)   :: hypervis_scaling = 0
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

        INTEGER(KIND=4)  , PARAMETER :: int_kind = 4
        INTEGER , PARAMETER  :: npsq = np*np

        TYPE  :: index_t
          INTEGER(KIND=int_kind) ia(npsq), ja(npsq)
          INTEGER(KIND=int_kind) is, ie
          INTEGER(KIND=int_kind) numuniquepts
          INTEGER(KIND=int_kind) uniqueptoffset
        END TYPE index_t

        INTEGER(KIND=4)  , PARAMETER :: long_kind = 8
        INTEGER , PARAMETER  :: nlev = 20

        TYPE  :: elem_accum_t
          REAL(KIND=real_kind) u(np,np,nlev)
          REAL(KIND=real_kind) t(np,np,nlev)
          REAL(KIND=real_kind) ke(np,np,nlev)
        END TYPE elem_accum_t

        TYPE  :: derived_state_t
          REAL(KIND=real_kind) dummmy
          REAL(KIND=real_kind) vstar(np,np,2,nlev)
        END TYPE derived_state_t

        INTEGER  , PARAMETER :: timelevels = 3

        TYPE  :: elem_state_t
          REAL(KIND=real_kind) p(np,np,nlev,timelevels)
          REAL(KIND=real_kind) phis(np,np)
          REAL(KIND=real_kind) gradps(np,np,2)
          REAL(KIND=real_kind) v(np,np,2,nlev,timelevels)
          REAL(KIND=real_kind) couv(np,np,2,nlev)
          REAL(KIND=real_kind) uv(np,np,2,nlev)
          REAL(KIND=real_kind) uv0(np,np,2,nlev)
          REAL(KIND=real_kind) pgrads(np,np,2,nlev)
          REAL(KIND=real_kind) psi(np,np,nlev)
          REAL(KIND=real_kind) phi(np,np,nlev)
          REAL(KIND=real_kind) ht(np,np,nlev)
          REAL(KIND=real_kind) t(np,np,nlev,timelevels)
          REAL(KIND=real_kind) q(np,np,nlev,timelevels)
          REAL(KIND=real_kind) pt3d(np,np,nlev)
          REAL(KIND=real_kind) qt3d(np,np,nlev)
          REAL(KIND=real_kind) peta(np,np,nlev)
          REAL(KIND=real_kind) dp3d(np,np,nlev)
          REAL(KIND=real_kind) zeta(np,np,nlev)
          REAL(KIND=real_kind) pr3d(np,np,nlev+1)
          REAL(KIND=real_kind) pr3d_ref(np,np,nlev+1)
          REAL(KIND=real_kind) gp3d(np,np,nlev+1)
          REAL(KIND=real_kind) ptop(np,np)
          REAL(KIND=real_kind) sgp(np,np)
          REAL(KIND=real_kind) tbar(nlev)
        END TYPE elem_state_t

        TYPE  :: rotation_t
          INTEGER nbr
          INTEGER reverse
          REAL(KIND=real_kind), dimension(:,:,:), pointer :: r => null()
        END TYPE rotation_t

        INTEGER(KIND=4)  , PARAMETER :: log_kind = 4

        TYPE  :: cartesian3d_t
          REAL(KIND=real_kind) x
          REAL(KIND=real_kind) y
          REAL(KIND=real_kind) z
        END TYPE cartesian3d_t

        TYPE  :: edgedescriptor_t
          INTEGER(KIND=int_kind) use_rotation
          INTEGER(KIND=int_kind) padding
          INTEGER(KIND=int_kind), pointer :: putmapp(:) => null()
          INTEGER(KIND=int_kind), pointer :: getmapp(:) => null()
          INTEGER(KIND=int_kind), pointer :: putmapp_ghost(:) => null()
          INTEGER(KIND=int_kind), pointer :: getmapp_ghost(:) => null()
          INTEGER(KIND=int_kind), pointer :: globalid(:) => null()
          INTEGER(KIND=int_kind), pointer :: loc2buf(:) => null()
          TYPE(cartesian3d_t), pointer :: neigh_corners(:,:) => null()
          INTEGER actual_neigh_edges
          LOGICAL(KIND=log_kind), pointer :: reverse(:) => null()
          TYPE(rotation_t), dimension(:), pointer :: rot => null()
        END TYPE edgedescriptor_t

        INTEGER  , PARAMETER :: num_neighbors = 8

        TYPE  :: gridvertex_t
          INTEGER, pointer :: nbrs(:) => null()
          INTEGER, pointer :: nbrs_face(:) => null()
          INTEGER, pointer :: nbrs_wgt(:) => null()
          INTEGER, pointer :: nbrs_wgt_ghost(:) => null()
          INTEGER nbrs_ptr(num_neighbors + 1)
          INTEGER face_number
          INTEGER number
          INTEGER processor_number
          INTEGER spacecurve
        END TYPE gridvertex_t

        TYPE  :: cartesian2d_t
          REAL(KIND=real_kind) x
          REAL(KIND=real_kind) y
        END TYPE cartesian2d_t

        TYPE  :: spherical_polar_t
          REAL(KIND=real_kind) r
          REAL(KIND=real_kind) lon
          REAL(KIND=real_kind) lat
        END TYPE spherical_polar_t

        TYPE  :: element_t
          INTEGER(KIND=int_kind) localid
          INTEGER(KIND=int_kind) globalid
          TYPE(spherical_polar_t) spherep(np,np)
          TYPE(cartesian2d_t) cartp(np,np)
          TYPE(cartesian2d_t) corners(4)
          REAL(KIND=real_kind) u2qmap(4,2)
          TYPE(cartesian3d_t) corners3d(4)
          REAL(KIND=real_kind) area
          REAL(KIND=real_kind) max_eig
          REAL(KIND=real_kind) min_eig
          REAL(KIND=real_kind) max_eig_ratio
          REAL(KIND=real_kind) dx_short
          REAL(KIND=real_kind) dx_long
          REAL(KIND=real_kind) variable_hyperviscosity(np,np)
          REAL(KIND=real_kind) hv_courant
          REAL(KIND=real_kind) tensorvisc(np,np,2,2)
          INTEGER(KIND=int_kind) node_numbers(4)
          INTEGER(KIND=int_kind) node_multiplicity(4)
          TYPE(gridvertex_t) vertex
          TYPE(edgedescriptor_t) desc
          TYPE(elem_state_t) state
          TYPE(derived_state_t) derived
          TYPE(elem_accum_t) accum
          REAL(KIND=real_kind) met(2,2,np,np)
          REAL(KIND=real_kind) metinv(2,2,np,np)
          REAL(KIND=real_kind) metdet(np,np)
          REAL(KIND=real_kind) rmetdet(np,np)
          REAL(KIND=real_kind) d(2,2,np,np)
          REAL(KIND=real_kind) dinv(2,2,np,np)
          REAL(KIND=real_kind) dinv2(np,np,2,2)
          REAL(KIND=real_kind) vec_sphere2cart(np,np,3,2)
          REAL(KIND=real_kind) mp(np,np)
          REAL(KIND=real_kind) rmp(np,np)
          REAL(KIND=real_kind) spheremp(np,np)
          REAL(KIND=real_kind) rspheremp(np,np)
          INTEGER(KIND=long_kind) gdofp(np,np)
          REAL(KIND=real_kind) fcor(np,np)
          TYPE(index_t) idxp
          TYPE(index_t), pointer :: idxv
          INTEGER facenum
          INTEGER dummy
        END TYPE element_t

        REAL(KIND=real_kind)   :: hypervis_power = 0


        REAL(KIND=real_kind) s(np, np)
        TYPE(derivative_t) deriv
        TYPE(element_t) elem
        LOGICAL var_coef
        REAL(KIND=real_kind) KGEN_RESULT_laplace(np, np)
        REAL(KIND=real_kind) KGEN_laplace(np, np)
        !JMD manual timer additions
        integer*8 c1,c2,cr,cm
        real*8 dt
        integer :: itmax=1000000
        character(len=80), parameter :: kname='[kernel_laplace_sphere_wk]'
        integer :: it
        !JMD

        ! populate dummy initial values
        var_coef = .TRUE.
        hypervis_scaling = 1
        do j=1,np
            do i=1,np
                elem%Dinv(1,1,i,j) = 0.2_real_kind * j
                elem%Dinv(2,1,i,j) = 0.3_real_kind * i*j
                elem%Dinv(2,1,i,j) = 0.4_real_kind * i
                elem%Dinv(2,2,i,j) = 0.5_real_kind * j
                s(i,j) = 0.6_real_kind * i*j
                deriv%Dvv(i,j) = 0.8_real_kind * j
                elem%variable_hyperviscosity(i,j) = 0.9_real_kind * i*j
                elem%tensorVisc(i,j,1,1) = 1.0_real_kind * i
                elem%tensorVisc(i,j,2,1) = 1.1_real_kind * j
                elem%tensorVisc(i,j,2,1) = 1.2_real_kind * i*j
                elem%tensorVisc(i,j,2,2) = 1.3_real_kind * i
                elem%spheremp(i,j) = 1.4_real_kind * j
                elem%Dinv2(i,j,1,1) = elem%Dinv(1,1,i,j)
                elem%Dinv2(i,j,1,2) = elem%Dinv(1,2,i,j)
                elem%Dinv2(i,j,2,1) = elem%Dinv(2,1,i,j)
                elem%Dinv2(i,j,2,2) = elem%Dinv(2,2,i,j)
            end do
        end do

        ! reference result
        KGEN_laplace = laplace_sphere_wk_ref(s,deriv,elem,var_coef)

        call system_clock(c1,cr,cm)
        ! modified result
        do it=1,itmax
            KGEN_RESULT_laplace = laplace_sphere_wk(s,deriv,elem,var_coef)
        enddo
        call system_clock(c2,cr,cm)
        dt = dble(c2-c1)/dble(cr)
        print *, TRIM(kname), ' time per call (usec): ',1.e6*dt/dble(itmax)

        IF ( ALL( KGEN_laplace == KGEN_RESULT_laplace ) ) THEN
            WRITE(*,*) "laplace is identical."
            WRITE(*,*) ""
            WRITE(*,*) "Mean value of kernel-generated laplace is ", SUM(KGEN_RESULT_laplace)/SIZE(KGEN_RESULT_laplace)
            WRITE(*,*) "Mean value of original laplace is ", SUM(KGEN_laplace)/SIZE(KGEN_laplace)
            WRITE(*,*) ""
            WRITE(*,*) "Reference: ", KGEN_laplace
            WRITE(*,*) "Modified:  ", KGEN_RESULT_laplace
        ELSE
            WRITE(*,*) "laplace is NOT identical."
            WRITE(*,*) COUNT( KGEN_laplace /= KGEN_RESULT_laplace), " of ", SIZE( KGEN_RESULT_laplace ), " elements are different."
            WRITE(*,*) "RMS of difference is ", SQRT(SUM((KGEN_laplace - KGEN_RESULT_laplace)**2)/SIZE(KGEN_laplace))
            WRITE(*,*) "Minimum difference is ", MINVAL(ABS(KGEN_laplace - KGEN_RESULT_laplace))
            WRITE(*,*) "Maximum difference is ", MAXVAL(ABS(KGEN_laplace - KGEN_RESULT_laplace))
            WRITE(*,*) "Mean value of kernel-generated laplace is ", SUM(KGEN_RESULT_laplace)/SIZE(KGEN_RESULT_laplace)
            WRITE(*,*) "Mean value of original laplace is ", SUM(KGEN_laplace)/SIZE(KGEN_laplace)
            WRITE(*,*) ""
            WRITE(*,*) "Reference: ", KGEN_laplace
            WRITE(*,*) "Modified:  ", KGEN_RESULT_laplace
            WRITE(*,*) ""
            STOP
        END IF

        contains

        function laplace_sphere_wk(s,deriv,elem,var_coef) result(laplace)
        !
        !   input:  s = scalar
        !   ouput:  -< grad(phi), grad(s) >   = weak divergence of grad(s)
        !     note: for this form of the operator, grad(s) does not need to be made c0
        !
              real(kind=real_kind), intent(in) :: s(np,np)
              logical, intent(in) :: var_coef
              type (derivative_t), intent(in) :: deriv
              type (element_t), intent(in) :: elem
              real(kind=real_kind)             :: laplace(np,np)
              real(kind=real_kind)             :: laplace2(np,np)
              integer i,j

        ! local
              real(kind=real_kind) :: grads(np,np,2), oldgrads(np,np,2)

              grads=gradient_sphere(s,deriv,elem%dinv2)

              if (var_coef) then
                    if (hypervis_power/=0 ) then
                ! scalar viscosity with variable coefficient
                          grads(:,:,1) = grads(:,:,1)*elem%variable_hyperviscosity(:,:)
                          grads(:,:,2) = grads(:,:,2)*elem%variable_hyperviscosity(:,:)
                     else if (hypervis_scaling /=0 ) then
                ! tensor hv, (3)
                          oldgrads=grads
                          do j=1,np
                                do i=1,np
                                      grads(i,j,1) = oldgrads(i,j,1)*elem%tensorvisc(i,j,1,1) &
						   + oldgrads(i,j,2)*elem%tensorvisc(i,j,1,2)  
                                      grads(i,j,2) = oldgrads(i,j,1)*elem%tensorvisc(i,j,2,1) &
						   + oldgrads(i,j,2)*elem%tensorvisc(i,j,2,2)  
                                end do
                          end do
                          else
                ! do nothing: constant coefficient viscsoity
                    endif
              endif

        ! note: divergnece_sphere and divergence_sphere_wk are identical *after* bndry_exchange
        ! if input is c_0.  here input is not c_0, so we should use divergence_sphere_wk().
              laplace=divergence_sphere_wk(grads,deriv,elem)

        end function laplace_sphere_wk


        function laplace_sphere_wk_ref(s,deriv,elem,var_coef) result(laplace)
        !
        !   input:  s = scalar
        !   ouput:  -< grad(phi), grad(s) >   = weak divergence of grad(s)
        !     note: for this form of the operator, grad(s) does not need to be made c0
        !
              real(kind=real_kind), intent(in) :: s(np,np)
              logical, intent(in) :: var_coef
              type (derivative_t), intent(in) :: deriv
              type (element_t), intent(in) :: elem
              real(kind=real_kind)             :: laplace(np,np)
              real(kind=real_kind)             :: laplace2(np,np)
              integer i,j

        ! local
              real(kind=real_kind) :: grads(np,np,2), oldgrads(np,np,2)

              grads=gradient_sphere(s,deriv,elem%dinv2)

              if (var_coef) then
                    if (hypervis_power/=0 ) then
                ! scalar viscosity with variable coefficient
                          grads(:,:,1) = grads(:,:,1)*elem%variable_hyperviscosity(:,:)
                          grads(:,:,2) = grads(:,:,2)*elem%variable_hyperviscosity(:,:)
                     else if (hypervis_scaling /=0 ) then
                ! tensor hv, (3)
                          oldgrads=grads
                          do j=1,np
                                do i=1,np
                                      grads(i,j,1) = sum(oldgrads(i,j,:)*elem%tensorvisc(i,j,1,:))
                                      grads(i,j,2) = sum(oldgrads(i,j,:)*elem%tensorvisc(i,j,2,:))
                                end do
                          end do
                          else
                ! do nothing: constant coefficient viscsoity
                    endif
              endif

        ! note: divergnece_sphere and divergence_sphere_wk are identical *after* bndry_exchange
        ! if input is c_0.  here input is not c_0, so we should use divergence_sphere_wk().
              laplace=divergence_sphere_wk(grads,deriv,elem)

        end function laplace_sphere_wk_ref

        FUNCTION gradient_sphere_ref(s, deriv, dinv) RESULT ( ds )
          !
          !   input s:  scalar
          !   output  ds: spherical gradient of s, lat-lon coordinates
          !

          TYPE(derivative_t), intent(in) :: deriv
          REAL(KIND=real_kind), intent(in), dimension(2,2,np,np) :: dinv
          REAL(KIND=real_kind), intent(in) :: s(np,np)

          REAL(KIND=real_kind) ds(np,np,2)

          INTEGER i
          INTEGER j
          INTEGER l

          REAL(KIND=real_kind) dsdx00
          REAL(KIND=real_kind) dsdy00
          REAL(KIND=real_kind) v1(np,np), v2(np,np)

          DO j=1,np
            DO l=1,np
              dsdx00 = 0.0d0
              dsdy00 = 0.0d0
              DO i=1,np
                dsdx00 = dsdx00 + deriv%dvv(i,l)*s(i,j)
                dsdy00 = dsdy00 + deriv%dvv(i,l)*s(j  ,i)
              END DO
              v1(l  ,j) = dsdx00*rrearth
              v2(j  ,l) = dsdy00*rrearth
            END DO
          END DO
          ! convert covarient to latlon
          DO j=1,np
            DO i=1,np
              ds(i,j,1) = dinv(1,1,i,j)*v1(i,j) + dinv(2,1,i,j)*v2(i,j)
              ds(i,j,2) = dinv(1,2,i,j)*v1(i,j) + dinv(2,2,i,j)*v2(i,j)
            END DO
          END DO

        END FUNCTION gradient_sphere_ref

        FUNCTION gradient_sphere(s, deriv, dinv) RESULT ( ds )
          !
          !   input s:  scalar
          !   output  ds: spherical gradient of s, lat-lon coordinates
          !

          TYPE(derivative_t), intent(in) :: deriv
!          REAL(KIND=real_kind), intent(in), dimension(2,2,np,np) :: dinv
          REAL(KIND=real_kind), intent(in), dimension(np,np,2,2) :: dinv
          REAL(KIND=real_kind), intent(in) :: s(np,np)

          REAL(KIND=real_kind) ds(np,np,2)

          INTEGER i
          INTEGER j
          INTEGER l

          REAL(KIND=real_kind) dsdx00
          REAL(KIND=real_kind) dsdy00
          REAL(KIND=real_kind) v1(np,np), v2(np,np)

          DO j=1,np
            DO l=1,np
              dsdx00 = 0.0d0
              dsdy00 = 0.0d0
              DO i=1,np
                dsdx00 = dsdx00 + deriv%dvv(i,l)*s(i,j)
                dsdy00 = dsdy00 + deriv%dvv(i,l)*s(j  ,i)
              END DO
              v1(l  ,j) = dsdx00*rrearth
              v2(j  ,l) = dsdy00*rrearth
            END DO
          END DO
          ! convert covarient to latlon
          DO j=1,np
            DO i=1,np
              ds(i,j,1) = dinv(i,j,1,1)*v1(i,j) + dinv(i,j,2,1)*v2(i,j)
              ds(i,j,2) = dinv(i,j,1,2)*v1(i,j) + dinv(i,j,2,2)*v2(i,j)
            END DO
          END DO

        END FUNCTION gradient_sphere
        FUNCTION divergence_sphere_wk_ref(v, deriv, elem) RESULT ( div )
          !
          !   input:  v = velocity in lat-lon coordinates
          !   ouput:  div(v)  spherical divergence of v, integrated by parts
          !
          !   Computes  -< grad(psi) dot v >
          !   (the integrated by parts version of < psi div(v) > )
          !
          !   note: after DSS, divergence_sphere() and divergence_sphere_wk()
          !   are identical to roundoff, as theory predicts.
          !
          REAL(KIND=real_kind), intent(in) :: v(np,np,2)
          ! in lat-lon coordinates
          TYPE(derivative_t), intent(in) :: deriv
          TYPE(element_t), intent(in) :: elem
          REAL(KIND=real_kind) div(np,np)

          ! Local

          INTEGER i, j, m, n

          REAL(KIND=real_kind) vtemp(np,np,2)
          REAL(KIND=real_kind) ggtemp(np,np,2)
          REAL(KIND=real_kind) gtemp(np,np,2)
          REAL(KIND=real_kind) psi(np,np)
          REAL(KIND=real_kind) xtmp

          ! latlon- > contra
          DO j=1,np
            DO i=1,np
              vtemp(i,j,1) = (elem%dinv(1,1,i,j)*v(i,j,1) + elem%dinv(1,2,i,j)*v(i,j,2))
              vtemp(i,j,2) = (elem%dinv(2,1,i,j)*v(i,j,1) + elem%dinv(2,2,i,j)*v(i,j,2))
            END DO
          END DO

          DO n=1,np
            DO m=1,np

              div(m,n) = 0
              DO j=1,np
                div(m,n) = div(m,n)-(elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))* rrearth
              END DO
            END DO
          END DO

        END FUNCTION divergence_sphere_wk_ref

!!DIR$ ATTRIBUTES FORCEINLINE :: divergence_sphere_wk
        FUNCTION divergence_sphere_wk(v, deriv, elem) RESULT ( div )
          !
          !   input:  v = velocity in lat-lon coordinates
          !   ouput:  div(v)  spherical divergence of v, integrated by parts
          !
          !   Computes  -< grad(psi) dot v >
          !   (the integrated by parts version of < psi div(v) > )
          !
          !   note: after DSS, divergence_sphere() and divergence_sphere_wk()
          !   are identical to roundoff, as theory predicts.
          !
          REAL(KIND=real_kind), intent(in) :: v(np,np,2)
          ! in lat-lon coordinates
          TYPE(derivative_t), intent(in) :: deriv
          TYPE(element_t), intent(in) :: elem
          REAL(KIND=real_kind) div(np,np)

          ! Local

          INTEGER i, j, m, n

          REAL(KIND=real_kind) vtemp(np,np,2)
          REAL(KIND=real_kind) ggtemp(np,np,2)
          REAL(KIND=real_kind) gtemp(np,np,2)
          REAL(KIND=real_kind) psi(np,np)
          REAL(KIND=real_kind) xtmp
          REAL(Kind=real_kind) temp00

          ! latlon- > contra
          DO j=1,np
            DO i=1,np
              vtemp(i,j,1) = (elem%dinv2(i,j,1,1)*v(i,j,1) + elem%dinv2(i,j,1,2)*v(i,j,2))
              vtemp(i,j,2) = (elem%dinv2(i,j,2,1)*v(i,j,1) + elem%dinv2(i,j,2,2)*v(i,j,2))
            END DO
          END DO

!       if(np == 4) then  
          DO n=1,np
            DO m=1,np
                j=1
                temp00 = -(elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))
       
                j=j+1
                temp00 = temp00 -(elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))

                j=j+1
                temp00 = temp00 -(elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))

                j=j+1
                temp00 = temp00 -(elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))
                div(m,n) = temp00*rrearth
            END DO
          END DO
!        else
!          DO n=1,np
!            DO m=1,np
!              div(m,n) = 0
!              DO j=1,np
!                div(m,n) = div(m,n) - (elem%spheremp(j,n)*vtemp(j,n,1)*deriv%dvv(m,j)+elem%spheremp(m,j)*vtemp(m,j,2)*deriv%dvv(n,j))
!	      ENDDO
!           ENDDO
!          ENDDO
!        endif

        END FUNCTION divergence_sphere_wk

        end program kgen_kernel_laplace_sphere_wk
