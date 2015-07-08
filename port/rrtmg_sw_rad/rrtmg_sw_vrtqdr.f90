
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_sw_vrtqdr.f90
! Generated at: 2015-07-07 00:48:25
! KGEN version: 0.4.13



    MODULE rrtmg_sw_vrtqdr
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !  --------------------------------------------------------------------------
        ! |                                                                          |
        ! |  Copyright 2002-2007, Atmospheric & Environmental Research, Inc. (AER).  |
        ! |  This software may be used, copied, or redistributed as long as it is    |
        ! |  not sold and this copyright notice is reproduced on each copy made.     |
        ! |  This model is provided as is without any express or implied warranties. |
        ! |                       (http://www.rtweb.aer.com/)                        |
        ! |                                                                          |
        !  --------------------------------------------------------------------------
        ! ------- Modules -------
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        !      use parkind, only: jpim, jprb
        !      use parrrsw, only: ngptsw
        IMPLICIT NONE
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        ! --------------------------------------------------------------------------

        SUBROUTINE vrtqdr_sw(klev, kw, pref, prefd, ptra, ptrad, pdbt, prdnd, prup, prupd, ptdbt, pfd, pfu)
            ! --------------------------------------------------------------------------
            ! Purpose: This routine performs the vertical quadrature integration
            !
            ! Interface:  *vrtqdr_sw* is called from *spcvrt_sw* and *spcvmc_sw*
            !
            ! Modifications.
            !
            ! Original: H. Barker
            ! Revision: Integrated with rrtmg_sw, J.-J. Morcrette, ECMWF, Oct 2002
            ! Revision: Reformatted for consistency with rrtmg_lw: MJIacono, AER, Jul 2006
            !
            !-----------------------------------------------------------------------
            ! ------- Declarations -------
            ! Input
            INTEGER, intent (in) :: klev ! number of model layers
            INTEGER, intent (in) :: kw ! g-point index
            REAL(KIND=r8), intent(in) :: pref(:) ! direct beam reflectivity
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(in) :: prefd(:) ! diffuse beam reflectivity
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(in) :: ptra(:) ! direct beam transmissivity
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(in) :: ptrad(:) ! diffuse beam transmissivity
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(in) :: pdbt(:)
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(in) :: ptdbt(:)
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(inout) :: prdnd(:)
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(inout) :: prup(:)
            !   Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(inout) :: prupd(:)
            !   Dimensions: (nlayers+1)
            ! Output
            REAL(KIND=r8), intent(out) :: pfd(:,:) ! downwelling flux (W/m2)
            !   Dimensions: (nlayers+1,ngptsw)
            ! unadjusted for earth/sun distance or zenith angle
            REAL(KIND=r8), intent(out) :: pfu(:,:) ! upwelling flux (W/m2)
            !   Dimensions: (nlayers+1,ngptsw)
            ! unadjusted for earth/sun distance or zenith angle
            ! Local
            INTEGER :: jk
            INTEGER :: ikp
            INTEGER :: ikx
            REAL(KIND=r8) :: zreflect
            REAL(KIND=r8) :: ztdn(klev+1)
            ! Definitions
            !
            ! pref(jk)   direct reflectance
            ! prefd(jk)  diffuse reflectance
            ! ptra(jk)   direct transmittance
            ! ptrad(jk)  diffuse transmittance
            !
            ! pdbt(jk)   layer mean direct beam transmittance
            ! ptdbt(jk)  total direct beam transmittance at levels
            !
            !-----------------------------------------------------------------------------
            ! Link lowest layer with surface
      zreflect = 1._r8 / (1._r8 - prefd(klev+1) * prefd(klev))
      prup(klev) = pref(klev) + (ptrad(klev) * &
                 ((ptra(klev) - pdbt(klev)) * prefd(klev+1) + &
                   pdbt(klev) * pref(klev+1))) * zreflect
      prupd(klev) = prefd(klev) + ptrad(klev) * ptrad(klev) * &
                    prefd(klev+1) * zreflect
            ! Pass from bottom to top
      do jk = 1,klev-1
         ikp = klev+1-jk                       
         ikx = ikp-1
         zreflect = 1._r8 / (1._r8 -prupd(ikp) * prefd(ikx))
         prup(ikx) = pref(ikx) + (ptrad(ikx) * &
                   ((ptra(ikx) - pdbt(ikx)) * prupd(ikp) + &
                     pdbt(ikx) * prup(ikp))) * zreflect
         prupd(ikx) = prefd(ikx) + ptrad(ikx) * ptrad(ikx) * &
                      prupd(ikp) * zreflect
      enddo
            ! Upper boundary conditions
      ztdn(1) = 1._r8
      prdnd(1) = 0._r8
      ztdn(2) = ptra(1)
      prdnd(2) = prefd(1)
            ! Pass from top to bottom
      do jk = 2,klev
         ikp = jk+1
         zreflect = 1._r8 / (1._r8 - prefd(jk) * prdnd(jk))
         ztdn(ikp) = ptdbt(jk) * ptra(jk) + &
                    (ptrad(jk) * ((ztdn(jk) - ptdbt(jk)) + &
                     ptdbt(jk) * pref(jk) * prdnd(jk))) * zreflect
         prdnd(ikp) = prefd(jk) + ptrad(jk) * ptrad(jk) * &
                      prdnd(jk) * zreflect
      enddo
            ! Up and down-welling fluxes at levels
      do jk = 1,klev+1
         zreflect = 1._r8 / (1._r8 - prdnd(jk) * prupd(jk))
         pfu(jk,kw) = (ptdbt(jk) * prup(jk) + &
                      (ztdn(jk) - ptdbt(jk)) * prupd(jk)) * zreflect
         pfd(jk,kw) = ptdbt(jk) + (ztdn(jk) - ptdbt(jk)+ &
                      ptdbt(jk) * prup(jk) * prdnd(jk)) * zreflect
      enddo
        END SUBROUTINE vrtqdr_sw
    END MODULE rrtmg_sw_vrtqdr
