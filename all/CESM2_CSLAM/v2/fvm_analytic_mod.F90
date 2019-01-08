!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:44 
!KGEN version : 0.7.3 
  
!MODULE FVM_ANALYTIC_MOD--------------------------------------------CE-for FVM!
! AUTHOR: CHRISTOPH ERATH, 17.October 2011                                    !
! This module contains all analytical terms for fvm                           !
!-----------------------------------------------------------------------------!


module fvm_analytic_mod
    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC get_high_order_weights_over_areas 

CONTAINS


  ! ----------------------------------------------------------------------------------!
  !SUBROUTINE MOMENT_ONSPHERE-----------------------------------------------CE-for FVM!
  ! AUTHOR: CHRISTOPH ERATH, 20.July 2011                                             !
  ! DESCRIPTION: Compute area and centroids/moments via line integrals                !
  !                                                                                   !
  ! INPUT:  x  ...  x cartesian coordinats of the arrival grid on the cube            !
  !         y  ...  y cartesian coordinats of the arrival grid on the cube            !
  !            ... cell boundaries in x and y directions                              !
  ! INPUT/OUTPUT:                                                                     !
  !         area      ... area of cells on the sphere                                 !
  !         centroid  ... x,y,x^2,y^2,xy                                              !
  !-----------------------------------------------------------------------------------!


  ! ----------------------------------------------------------------------------------!
  !SUBROUTINES I_00, I_01, I_20, I_02, I11----------------------------------CE-for FVM!
  ! AUTHOR: CHRISTOPH ERATH, 17.October 2011                                          !
  ! DESCRIPTION: calculates the exact integrals                                       !
  !                                                                                   !
  ! CALLS: none                                                                       !
  ! INPUT: x    ... x coordinate of the evaluation point (Cartesian on the cube)      !
  !        y    ... y coordinate of the evaluation point (Cartesian on the cube)      !
  ! OUTPUT: I_00, I_01, I_20, I_02, I11                                               !
  !-----------------------------------------------------------------------------------!


  !END SUBROUTINES I_00, I_01, I_20, I_02, I11------------------------------CE-for FVM!


  ! matrix version of reconstruct_cubic_onface


  !
  !


  subroutine get_high_order_weights_over_areas(x,dx,num_seg,num_seg_max,num_area,weights,ngpc,gsweights, gspts,irecons)
    implicit none
    integer                                                 , intent(in)    :: num_area, num_seg_max, irecons
    REAL(KIND=r8), dimension(2,num_seg_max,num_area ), intent(inout) :: x, dx
    integer                                                 , intent(in)    :: ngpc
    integer             , dimension(num_area               ), intent(in)    :: num_seg
    REAL(KIND=r8), dimension(irecons,num_area), intent(out)   :: weights

    real (kind=r8), dimension(ngpc,num_seg_max               ) :: xq,yq        !quadrature points along line segments
    real (kind=r8), dimension(ngpc,num_seg_max,irecons) :: F            !potentials
    real (kind=r8), dimension(                 irecons) :: weights_area
    real (kind=r8), dimension(ngpc,num_seg_max) :: xq2, yrh, rho, tmp !intermediate variables for optimization
    REAL(KIND=r8) , dimension(ngpc,num_seg_max) :: xq2ir, xq2i, rhoi  !intermediate variables for optimization

    integer :: iseg,iarea,i,j,k

    real (kind=r8), dimension(ngpc) :: gsweights, gspts

    weights(1:irecons,1:num_area) = 0.0_r8 !may not be necessary dbgxxx
    do iarea=1,num_area
      do iseg=1,num_seg(iarea)
        xq(:,iseg) = x(1,iseg,iarea)+dx(1,iseg,iarea)*gspts(:)
        yq(:,iseg) = x(2,iseg,iarea)+dx(2,iseg,iarea)*gspts(:)
      end do
      ! potentials (equation's 23-28 in CSLAM paper; Lauritzen et al., 2010):
      ! (Rory Kelly optimization)
      !
      !
      !
      do j=1,num_seg(iarea)
!DIR$ SIMD
        do i=1,ngpc
          xq2(i,j)   =  xq(i,j)*xq(i,j)
          xq2i(i,j)  =  1.0_r8/(1.0_r8+xq2(i,j))
          xq2ir(i,j) =  SQRT(xq2i(i,j))
          rho(i,j)   =  SQRT(1.0_r8+xq2(i,j)+yq(i,j)*yq(i,j))
          rhoi(i,j)  =  1.0_r8/rho(i,j)
          yrh(i,j)   =  yq(i,j)*rhoi(i,j)
          tmp(i,j)   =  yq(i,j)*xq2ir(i,j)
          F(i,j,1)   =  yrh(i,j)*xq2i(i,j)                 !F_00 !F_00
          F(i,j,2)   =  xq(i,j)*yrh(i,j)*xq2i(i,j)         !F_10 !F_10
          F(i,j,3)   = -1.0_r8*rhoi(i,j)                    !F_01 !F_01
          F(i,j,4)   =  xq2(i,j)*yrh(i,j)*xq2i(i,j)        !F_20 !F_20
          F(i,j,6)   = -xq(i,j)*rhoi(i,j)                  !F_11 !F_11
        enddo
        ! take F(i,j,5) out of loop above since it prevents vectorization
        !
        !
        do i=1,ngpc
          F(i,j,5)   = -yq(i,j)*rhoi(i,j)+log(tmp(i,j)+rho(i,j)*xq2ir(i,j))  !F_02 !F_02
        end do
      enddo
      weights_area = 0.0_r8
      do k=1,irecons
        do iseg=1,num_seg(iarea)
          weights_area(k) = weights_area(k) + sum(gsweights(:)*F(:,iseg,k))*0.5_r8*dx(1,iseg,iarea)
        end do
      end do
      weights(1:irecons,iarea) = weights_area(1:irecons)
    end do
  end subroutine get_high_order_weights_over_areas
  !********************************************************************************
  ! Gauss-Legendre quadrature
  ! Tabulated values
  !********************************************************************************


  !
  !
  !


! ----------------------------------------------------------------------------------!
!SUBROUTINE CREATE_INTERPOLATIION_POINTS----------------------------------CE-for FVM!
! AUTHOR: CHRISTOPH ERATH, 17.October 2011                                          !
! DESCRIPTION: for elements, which share a cube edge, we have to do some            !
!        interpolation on different cubic faces, also in the halo region:           !
!        because we also need the reconstruction coefficients in the halo zone,     !
!        which is basically calculated twice, on the original cell of an element    !
!        on face A and on a cell in the halo region of an element of face B         !
!        The crux is, that the interpolation has to be the same to ensure           !
!        conservation of the scheme                                                 !
!        SYMMETRY of the CUBE is used for calucaltion the interpolation_point       !
!                                                                                   !
! CALLS: interpolation_point                                                        !
! INPUT/OUTPUT:                                                                     !
!        elem     ...  element structure from HOMME                                 !
!        fvm   ...  structure                                                       !
!-----------------------------------------------------------------------------------!

!
!
!


!END SUBROUTINE CREATE_INTERPOLATION_POINTS-------------------------------CE-for FVM!
! ----------------------------------------------------------------------------------!
!SUBROUTINE INTERPOLATION_POINT-------------------------------------------CE-for FVM!
! AUTHOR: CHRISTOPH ERATH, 14.November 2011                                         !
! DESCRIPTION: calculates the interpolation point on from face 1 in face 2 in       !
!        alpha/beta coordinates, only 1D                                            !
!                                                                                   !
! CALLS: cubedsphere2cart, cart2cubedsphere                                         !
! INPUT: gnom... 1D coordinates                                                     !
!        gnom1d... 1d coordinates                                                   !
!        face1... orginal face                                                      !
!        face2... target face (where the interpolation has to be done)              !
!        xy ... 0 for alpha coordinate, any other for beta                          !
!        except.which type, interior, left edge (-1), right edge (1)                !
!        point... interpolation point                                               !
!        ida  ... begin of interpval                                                !
!        ide  ... end of interpval                                                  !
! INPUT/OUTPUT/RETURN:                                                              !
!        iref ... where we start the search, is also an OUTPUT, so we know for the  !
!                 next point where to start                                         !
!-----------------------------------------------------------------------------------!
! DESCRIPTION: searchs where the interpolation point has to be (iref), two values   !
!        of interpval on the left and on the right, except if we are out of range   !
!        which is indicated through ia and ie, respectively                         !
!        It is a 1D interpolation, use alpha/beta coordinates!!!                    !
!                                                                                   !
! CALLS: cubic_equispace_interp                                                     !
! INPUT: iref ... where we start the search, is also an OUTPUT, so we know for the  !
!                 next point where to start                                         !
!        ibaseref ... startindex of the four tracer value for the reconstruction    !
!        point    ... provides the difference of the interpolation point to use it  !
!                     directly in CUBIC_EQUISPACE_INTERP                            !
!-----------------------------------------------------------------------------------!


                                      !


!END SUBROUTINE INTERPOLATION_POINT---------------------------------------CE-for FVM!
! ---------------------------------------------------------------------!
!                                                                      !
! Precompute weights for Lagrange interpolation                        !
! for equi-distant source grid values                                  !
!                                                                      !
!----------------------------------------------------------------------!


end module fvm_analytic_mod