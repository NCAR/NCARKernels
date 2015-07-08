
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_sw_setcoef.f90
! Generated at: 2015-07-07 00:48:25
! KGEN version: 0.4.13



    MODULE rrtmg_sw_setcoef
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
        !      use parkind, only : jpim, jprb
        USE rrsw_ref, ONLY: preflog
        USE rrsw_ref, ONLY: tref
        IMPLICIT NONE
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !----------------------------------------------------------------------------

        SUBROUTINE setcoef_sw(nlayers, pavel, tavel, pz, tz, tbound, coldry, wkl, laytrop, layswtch, laylow, jp, jt, jt1, co2mult,&
         colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, fac00, fac01, fac10, fac11, selffac, selffrac, indself, forfac, &
        forfrac, indfor)
            !----------------------------------------------------------------------------
            !
            ! Purpose:  For a given atmosphere, calculate the indices and
            ! fractions related to the pressure and temperature interpolations.
            ! Modifications:
            ! Original: J. Delamere, AER, Inc. (version 2.5, 02/04/01)
            ! Revised: Rewritten and adapted to ECMWF F90, JJMorcrette 030224
            ! Revised: For uniform rrtmg formatting, MJIacono, Jul 2006
            ! ------ Declarations -------
            ! ----- Input -----
            INTEGER, intent(in) :: nlayers ! total number of layers
            REAL(KIND=r8), intent(in) :: pavel(:) ! layer pressures (mb)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: tavel(:) ! layer temperatures (K)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: pz(0:) ! level (interface) pressures (hPa, mb)
            !    Dimensions: (0:nlayers)
            REAL(KIND=r8), intent(in) :: tz(0:) ! level (interface) temperatures (K)
            !    Dimensions: (0:nlayers)
            REAL(KIND=r8), intent(in) :: tbound ! surface temperature (K)
            REAL(KIND=r8), intent(in) :: coldry(:) ! dry air column density (mol/cm2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: wkl(:,:) ! molecular amounts (mol/cm-2)
            !    Dimensions: (mxmol,nlayers)
            ! ----- Output -----
            INTEGER, intent(out) :: laytrop ! tropopause layer index
            INTEGER, intent(out) :: layswtch !
            INTEGER, intent(out) :: laylow !
            INTEGER, intent(out) :: jp(:) !
            !    Dimensions: (nlayers)
            INTEGER, intent(out) :: jt(:) !
            !    Dimensions: (nlayers)
            INTEGER, intent(out) :: jt1(:) !
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colh2o(:) ! column amount (h2o)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colco2(:) ! column amount (co2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colo3(:) ! column amount (o3)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: coln2o(:) ! column amount (n2o)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colch4(:) ! column amount (ch4)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colo2(:) ! column amount (o2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colmol(:) !
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: co2mult(:) !
            !    Dimensions: (nlayers)
            INTEGER, intent(out) :: indself(:)
            !    Dimensions: (nlayers)
            INTEGER, intent(out) :: indfor(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: selffac(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: selffrac(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: forfac(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: forfrac(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: fac10(:)
            REAL(KIND=r8), intent(out) :: fac00(:)
            REAL(KIND=r8), intent(out) :: fac01(:)
            REAL(KIND=r8), intent(out) :: fac11(:) !
            !    Dimensions: (nlayers)
            ! ----- Local -----
            INTEGER :: indbound
            INTEGER :: indlev0
            INTEGER :: lay
            INTEGER :: jp1
            REAL(KIND=r8) :: stpfac
            REAL(KIND=r8) :: tbndfrac
            REAL(KIND=r8) :: t0frac
            REAL(KIND=r8) :: plog
            REAL(KIND=r8) :: fp
            REAL(KIND=r8) :: ft
            REAL(KIND=r8) :: ft1
            REAL(KIND=r8) :: water
            REAL(KIND=r8) :: scalefac
            REAL(KIND=r8) :: factor
            REAL(KIND=r8) :: co2reg
            REAL(KIND=r8) :: compfp
            ! Initializations
      stpfac = 296._r8/1013._r8
      indbound = tbound - 159._r8
      tbndfrac = tbound - int(tbound)
      indlev0  = tz(0) - 159._r8
      t0frac   = tz(0) - int(tz(0))
      laytrop  = 0
      layswtch = 0
      laylow   = 0
            ! Begin layer loop
      do lay = 1, nlayers
                ! Find the two reference pressures on either side of the
                ! layer pressure.  Store them in JP and JP1.  Store in FP the
                ! fraction of the difference (in ln(pressure)) between these
                ! two values that the layer pressure lies.
         plog = log(pavel(lay))
         jp(lay) = int(36._r8 - 5*(plog+0.04_r8))
         if (jp(lay) .lt. 1) then
            jp(lay) = 1
         elseif (jp(lay) .gt. 58) then
            jp(lay) = 58
         endif
         jp1 = jp(lay) + 1
         fp = 5._r8 * (preflog(jp(lay)) - plog)
                ! Determine, for each reference pressure (JP and JP1), which
                ! reference temperature (these are different for each
                ! reference pressure) is nearest the layer temperature but does
                ! not exceed it.  Store these indices in JT and JT1, resp.
                ! Store in FT (resp. FT1) the fraction of the way between JT
                ! (JT1) and the next highest reference temperature that the
                ! layer temperature falls.
         jt(lay) = int(3._r8 + (tavel(lay)-tref(jp(lay)))/15._r8)
         if (jt(lay) .lt. 1) then
            jt(lay) = 1
         elseif (jt(lay) .gt. 4) then
            jt(lay) = 4
         endif
         ft = ((tavel(lay)-tref(jp(lay)))/15._r8) - float(jt(lay)-3)
         jt1(lay) = int(3._r8 + (tavel(lay)-tref(jp1))/15._r8)
         if (jt1(lay) .lt. 1) then
            jt1(lay) = 1
         elseif (jt1(lay) .gt. 4) then
            jt1(lay) = 4
         endif
         ft1 = ((tavel(lay)-tref(jp1))/15._r8) - float(jt1(lay)-3)
         water = wkl(1,lay)/coldry(lay)
         scalefac = pavel(lay) * stpfac / tavel(lay)
                ! If the pressure is less than ~100mb, perform a different
                ! set of species interpolations.
         if (plog .le. 4.56_r8) go to 5300
         laytrop =  laytrop + 1
         if (plog .ge. 6.62_r8) laylow = laylow + 1
                ! Set up factors needed to separately include the water vapor
                ! foreign-continuum in the calculation of absorption coefficient.
         forfac(lay) = scalefac / (1.+water)
         factor = (332.0_r8-tavel(lay))/36.0_r8
         indfor(lay) = min(2, max(1, int(factor)))
         forfrac(lay) = factor - float(indfor(lay))
                ! Set up factors needed to separately include the water vapor
                ! self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)
         factor = (tavel(lay)-188.0_r8)/7.2_r8
         indself(lay) = min(9, max(1, int(factor)-7))
         selffrac(lay) = factor - float(indself(lay) + 7)
                ! Calculate needed column amounts.
         colh2o(lay) = 1.e-20_r8 * wkl(1,lay)
         colco2(lay) = 1.e-20_r8 * wkl(2,lay)
         colo3(lay) = 1.e-20_r8 * wkl(3,lay)
                !           colo3(lay) = 0._r8
                !           colo3(lay) = colo3(lay)/1.16_r8
         coln2o(lay) = 1.e-20_r8 * wkl(4,lay)
         colch4(lay) = 1.e-20_r8 * wkl(6,lay)
         colo2(lay) = 1.e-20_r8 * wkl(7,lay)
         colmol(lay) = 1.e-20_r8 * coldry(lay) + colh2o(lay)
                !           colco2(lay) = 0._r8
                !           colo3(lay) = 0._r8
                !           coln2o(lay) = 0._r8
                !           colch4(lay) = 0._r8
                !           colo2(lay) = 0._r8
                !           colmol(lay) = 0._r8
         if (colco2(lay) .eq. 0._r8) colco2(lay) = 1.e-32_r8 * coldry(lay)
         if (coln2o(lay) .eq. 0._r8) coln2o(lay) = 1.e-32_r8 * coldry(lay)
         if (colch4(lay) .eq. 0._r8) colch4(lay) = 1.e-32_r8 * coldry(lay)
         if (colo2(lay) .eq. 0._r8) colo2(lay) = 1.e-32_r8 * coldry(lay)
                ! Using E = 1334.2 cm-1.
         co2reg = 3.55e-24_r8 * coldry(lay)
         co2mult(lay)= (colco2(lay) - co2reg) * &
               272.63_r8*exp(-1919.4_r8/tavel(lay))/(8.7604e-4_r8*tavel(lay))
         goto 5400
                ! Above laytrop.
 5300    continue
                ! Set up factors needed to separately include the water vapor
                ! foreign-continuum in the calculation of absorption coefficient.
         forfac(lay) = scalefac / (1.+water)
         factor = (tavel(lay)-188.0_r8)/36.0_r8
         indfor(lay) = 3
         forfrac(lay) = factor - 1.0_r8
                ! Calculate needed column amounts.
         colh2o(lay) = 1.e-20_r8 * wkl(1,lay)
         colco2(lay) = 1.e-20_r8 * wkl(2,lay)
         colo3(lay)  = 1.e-20_r8 * wkl(3,lay)
         coln2o(lay) = 1.e-20_r8 * wkl(4,lay)
         colch4(lay) = 1.e-20_r8 * wkl(6,lay)
         colo2(lay)  = 1.e-20_r8 * wkl(7,lay)
         colmol(lay) = 1.e-20_r8 * coldry(lay) + colh2o(lay)
         if (colco2(lay) .eq. 0._r8) colco2(lay) = 1.e-32_r8 * coldry(lay)
         if (coln2o(lay) .eq. 0._r8) coln2o(lay) = 1.e-32_r8 * coldry(lay)
         if (colch4(lay) .eq. 0._r8) colch4(lay) = 1.e-32_r8 * coldry(lay)
         if (colo2(lay)  .eq. 0._r8) colo2(lay)  = 1.e-32_r8 * coldry(lay)
         co2reg = 3.55e-24_r8 * coldry(lay)
         co2mult(lay)= (colco2(lay) - co2reg) * &
               272.63_r8*exp(-1919.4_r8/tavel(lay))/(8.7604e-4_r8*tavel(lay))
         selffac(lay) = 0._r8
         selffrac(lay)= 0._r8
         indself(lay) = 0
 5400    continue
                ! We have now isolated the layer ln pressure and temperature,
                ! between two reference pressures and two reference temperatures
                ! (for each reference pressure).  We multiply the pressure
                ! fraction FP with the appropriate temperature fractions to get
                ! the factors that will be needed for the interpolation that yields
                ! the optical depths (performed in routines TAUGBn for band n).
         compfp = 1._r8 - fp
         fac10(lay) = compfp * ft
         fac00(lay) = compfp * (1._r8 - ft)
         fac11(lay) = fp * ft1
         fac01(lay) = fp * (1._r8 - ft1)
                ! End layer loop
      enddo
        END SUBROUTINE setcoef_sw
        !***************************************************************************

    END MODULE rrtmg_sw_setcoef
