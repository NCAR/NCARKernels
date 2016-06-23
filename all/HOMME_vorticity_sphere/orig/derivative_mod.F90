
! KGEN-generated Fortran source file
!
! Filename    : derivative_mod.F90
! Generated at: 2015-03-14 21:47:57
! KGEN version: 0.4.5



    MODULE derivative_mod
        USE kinds, ONLY: real_kind
        USE dimensions_mod, ONLY: np
        USE dimensions_mod, ONLY: nc
        USE dimensions_mod, ONLY: nep
        ! needed for spherical differential operators:
        USE physical_constants, ONLY: rrearth
        IMPLICIT NONE
        PRIVATE
        TYPE, public :: derivative_t
            REAL(KIND=real_kind) :: dvv(np,np)
            REAL(KIND=real_kind) :: dvv_diag(np,np)
            REAL(KIND=real_kind) :: dvv_twt(np,np)
            REAL(KIND=real_kind) :: mvv_twt(np,np) ! diagonal matrix of GLL weights
            REAL(KIND=real_kind) :: mfvm(np,nc+1)
            REAL(KIND=real_kind) :: cfvm(np,nc)
            REAL(KIND=real_kind) :: sfvm(np,nep)
            REAL(KIND=real_kind) :: legdg(np,np)
        END TYPE derivative_t
        ! ======================================
        ! Public Interfaces
        ! ======================================



        ! these routines compute spherical differential operators as opposed to
        ! the gnomonic coordinate operators above.  Vectors (input or output)
        ! are always expressed in lat-lon coordinates
        !
        ! note that weak derivatives (integrated by parts form) can be defined using
        ! contra or co-variant test functions, so
        !
        ! only used for debugging
        PUBLIC vorticity_sphere
        !  public  :: curl_sphere_wk_testcontra  ! not coded

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_derivative_t
        END INTERFACE kgen_read

        CONTAINS

        ! write subroutines
        ! No module extern variables
        SUBROUTINE kgen_read_derivative_t(var, kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            TYPE(derivative_t), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%dvv
            READ(UNIT=kgen_unit) var%dvv_diag
            READ(UNIT=kgen_unit) var%dvv_twt
            READ(UNIT=kgen_unit) var%mvv_twt
            READ(UNIT=kgen_unit) var%mfvm
            READ(UNIT=kgen_unit) var%cfvm
            READ(UNIT=kgen_unit) var%sfvm
            READ(UNIT=kgen_unit) var%legdg
        END SUBROUTINE
        ! ==========================================
        ! derivinit:
        !
        ! Initialize the matrices for taking
        ! derivatives and interpolating
        ! ==========================================


        ! =======================================
        ! dmatinit:
        !
        ! Compute rectangular v->p
        ! derivative matrix (dmat)
        ! =======================================

        ! =======================================
        ! dpvinit:
        !
        ! Compute rectangular p->v
        ! derivative matrix (dmat)
        ! for strong gradients
        ! =======================================

        ! =======================================
        ! v2pinit:
        ! Compute interpolation matrix from gll(1:n1) -> gs(1:n2)
        ! =======================================

        ! =======================================
        ! dvvinit:
        !
        ! Compute rectangular v->v
        ! derivative matrix (dvv)
        ! =======================================

        !  ================================================
        !  divergence_stag:
        !
        !  Compute divergence (maps v grid -> p grid)
        !  ================================================

        !  ================================================
        !  divergence_nonstag:
        !
        !  Compute divergence (maps v->v)
        !  ================================================

        !  ================================================
        !  gradient_wk_stag:
        !
        !  Compute the weak form gradient:
        !  maps scalar field on the pressure grid to the
        !  velocity grid
        !  ================================================

        !  ================================================
        !  gradient_wk_nonstag:
        !
        !  Compute the weak form gradient:
        !  maps scalar field on the Gauss-Lobatto grid to the
        !  weak gradient on the Gauss-Lobbatto grid
        !  ================================================

        !  ================================================
        !  gradient_str_stag:
        !
        !  Compute the *strong* form gradient:
        !  maps scalar field on the pressure grid to the
        !  velocity grid
        !  ================================================

        !  ================================================
        !  gradient_str_nonstag:
        !
        !  Compute the *strong* gradient on the velocity grid
        !  of a scalar field on the velocity grid
        !  ================================================

        !  ================================================
        !  vorticity:
        !
        !  Compute the vorticity of the velocity field on the
        !  velocity grid
        !  ================================================

        !  ================================================
        !  interpolate_gll2fvm_points:
        !
        !  shape funtion interpolation from data on GLL grid to cellcenters on physics grid
        !  Author: Christoph Erath
        !  ================================================

        !  ================================================
        !  interpolate_gll2spelt_points:
        !
        !  shape function interpolation from data on GLL grid the spelt grid
        !  Author: Christoph Erath
        !  ================================================

        !  ================================================
        !  interpolate_gll2fvm_corners:
        !
        !  shape funtion interpolation from data on GLL grid to physics grid
        !
        !  ================================================

        !  ================================================
        !  remap_phys2gll:
        !
        !  interpolate to an equally spaced (in reference element coordinate system)
        !  "physics" grid to the GLL grid
        !
        !  1st order, monotone, conservative
        !  MT initial version 2013
        !  ================================================

        !----------------------------------------------------------------






        !--------------------------------------------------------------------------





        FUNCTION vorticity_sphere(v, deriv, d, rmetdet) RESULT ( vort )
            !
            !   input:  v = velocity in lat-lon coordinates
            !   ouput:  spherical vorticity of v
            !
            TYPE(derivative_t), intent(in) :: deriv
            !type (element_t), intent(in) :: elem
            REAL(KIND=real_kind), intent(in) :: d(2,2,np,np)
            REAL(KIND=real_kind), intent(in) :: rmetdet(np,np)
            REAL(KIND=real_kind), intent(in) :: v(np,np,2)
            REAL(KIND=real_kind) :: vort(np,np)
            INTEGER :: i
            INTEGER :: j
            INTEGER :: l
            REAL(KIND=real_kind) :: dvdx00
            REAL(KIND=real_kind) :: dudy00
            REAL(KIND=real_kind) :: vco(np,np,2)
            REAL(KIND=real_kind) :: vtemp(np,np)
            ! convert to covariant form
            DO j=1,np
                DO i=1,np
                    vco(i,j,1) = (d(1,1,i,j)*v(i,j,1) + d(2,1,i,j)*v(i,j,2))
                    vco(i,j,2) = (d(1,2,i,j)*v(i,j,1) + d(2,2,i,j)*v(i,j,2))
                END DO 
            END DO 
            DO j=1,np
                DO l=1,np
                    dudy00 = 0.0d0
                    dvdx00 = 0.0d0
                    DO i=1,np
                        dvdx00 = dvdx00 + deriv%dvv(i,l)*vco(i,j  ,2)
                        dudy00 = dudy00 + deriv%dvv(i,l)*vco(j  ,i,1)
                    END DO 
                    vort(l  ,j) = dvdx00
                    vtemp(j  ,l) = dudy00
                END DO 
            END DO 
            DO j=1,np
                DO i=1,np
                    vort(i,j) = (vort(i,j)-vtemp(i,j))*(rmetdet(i,j)*rrearth)
                END DO 
            END DO 
        END FUNCTION vorticity_sphere











        ! Given a field defined on the unit element, [-1,1]x[-1,1]
        ! sample values, sampled_val, premultiplied by integration weights,
        ! and a number, np, of Gauss-Lobatto-Legendre points. Divide
        ! the square up into intervals by intervals sub-squares so that
        ! there are now intervals**2 sub-cells.  Integrate the
        ! function defined by sampled_val over each of these
        ! sub-cells and return the integrated values as an
        ! intervals by intervals matrix.
        !
        ! Efficiency is obtained by computing and caching the appropriate
        ! integration matrix the first time the function is called.

        ! Helper subroutine that will fill in a matrix needed to
        ! integrate a function defined on the GLL points of a unit
        ! square on sub-cells.  So np is the number of integration
        ! GLL points defined on the unit square (actually [-1,1]x[-1,1])
        ! and intervals is the number to cut it up into, say a 3 by 3
        ! set of uniform sub-cells.  This function will fill the
        ! subcell_integration matrix with the correct coefficients
        ! to integrate over each subcell.

    END MODULE derivative_mod
