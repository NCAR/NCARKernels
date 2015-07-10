
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_lw_setcoef.f90
! Generated at: 2015-07-06 23:28:43
! KGEN version: 0.4.13



    MODULE rrtmg_lw_setcoef
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
        USE rrlw_wvn, ONLY: totplnk
        USE rrlw_wvn, ONLY: totplk16
        USE rrlw_ref, only : preflog
        USE rrlw_ref, only : tref
        USE rrlw_ref, only : chi_mls
        USE rrlw_vsn, ONLY: hvrset
        IMPLICIT NONE
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !----------------------------------------------------------------------------

        SUBROUTINE setcoef(nlayers, istart, pavel, tavel, tz, tbound, semiss, coldry, wkl, wbroad, laytrop, jp, jt, jt1, planklay,&
         planklev, plankbnd, colh2o, colco2, colo3, coln2o, colco, colch4, colo2, colbrd, fac00, fac01, fac10, fac11, rat_h2oco2, &
        rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, rat_n2oco2, rat_n2oco2_1, &
        rat_o3co2, rat_o3co2_1, selffac, selffrac, indself, forfac, forfrac, indfor, minorfrac, scaleminor, scaleminorn2, &
        indminor)
            !----------------------------------------------------------------------------
            !
            !  Purpose:  For a given atmosphere, calculate the indices and
            !  fractions related to the pressure and temperature interpolations.
            !  Also calculate the values of the integrated Planck functions
            !  for each band at the level and layer temperatures.
            ! ------- Declarations -------
            ! ----- Input -----
            INTEGER, intent(in) :: nlayers ! total number of layers
            INTEGER, intent(in) :: istart ! beginning band of calculation
            REAL(KIND=r8), intent(in) :: pavel(:) ! layer pressures (mb)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: tavel(:) ! layer temperatures (K)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: tz(0:) ! level (interface) temperatures (K)
            !    Dimensions: (0:nlayers)
            REAL(KIND=r8), intent(in) :: tbound ! surface temperature (K)
            REAL(KIND=r8), intent(in) :: coldry(:) ! dry air column density (mol/cm2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: wbroad(:) ! broadening gas column density (mol/cm2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(in) :: wkl(:,:) ! molecular amounts (mol/cm-2)
            !    Dimensions: (mxmol,nlayers)
            REAL(KIND=r8), intent(in) :: semiss(:) ! lw surface emissivity
            !    Dimensions: (nbndlw)
            ! ----- Output -----
            INTEGER, intent(out) :: laytrop ! tropopause layer index
            INTEGER, intent(out) :: jp(:) !
            !    Dimensions: (nlayers)
            INTEGER, intent(out) :: jt(:) !
            !    Dimensions: (nlayers)
            INTEGER, intent(out) :: jt1(:) !
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: planklay(:,:) !
            !    Dimensions: (nlayers,nbndlw)
            REAL(KIND=r8), intent(out) :: planklev(0:,:) !
            !    Dimensions: (0:nlayers,nbndlw)
            REAL(KIND=r8), intent(out) :: plankbnd(:) !
            !    Dimensions: (nbndlw)
            REAL(KIND=r8), intent(out) :: colh2o(:) ! column amount (h2o)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colco2(:) ! column amount (co2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colo3(:) ! column amount (o3)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: coln2o(:) ! column amount (n2o)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colco(:) ! column amount (co)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colch4(:) ! column amount (ch4)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colo2(:) ! column amount (o2)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: colbrd(:) ! column amount (broadening gases)
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
            INTEGER, intent(out) :: indminor(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: minorfrac(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: scaleminor(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: scaleminorn2(:)
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: fac00(:)
            REAL(KIND=r8), intent(out) :: fac01(:)
            REAL(KIND=r8), intent(out) :: fac10(:)
            REAL(KIND=r8), intent(out) :: fac11(:) !
            !    Dimensions: (nlayers)
            REAL(KIND=r8), intent(out) :: rat_h2och4(:)
            REAL(KIND=r8), intent(out) :: rat_h2on2o(:)
            REAL(KIND=r8), intent(out) :: rat_h2on2o_1(:)
            REAL(KIND=r8), intent(out) :: rat_o3co2_1(:)
            REAL(KIND=r8), intent(out) :: rat_h2och4_1(:)
            REAL(KIND=r8), intent(out) :: rat_n2oco2_1(:)
            REAL(KIND=r8), intent(out) :: rat_h2oo3_1(:)
            REAL(KIND=r8), intent(out) :: rat_n2oco2(:)
            REAL(KIND=r8), intent(out) :: rat_h2oco2(:)
            REAL(KIND=r8), intent(out) :: rat_h2oco2_1(:)
            REAL(KIND=r8), intent(out) :: rat_h2oo3(:)
            REAL(KIND=r8), intent(out) :: rat_o3co2(:) !
            !    Dimensions: (nlayers)
            ! ----- Local -----
            INTEGER :: indbound
            INTEGER :: indlev0
            INTEGER :: lay
            INTEGER :: indlay
            INTEGER :: indlev
            INTEGER :: iband
            INTEGER :: jp1
            REAL(KIND=r8) :: stpfac
            REAL(KIND=r8) :: tbndfrac
            REAL(KIND=r8) :: t0frac
            REAL(KIND=r8) :: tlayfrac
            REAL(KIND=r8) :: tlevfrac
            REAL(KIND=r8) :: dbdtlev
            REAL(KIND=r8) :: dbdtlay
            REAL(KIND=r8) :: plog
            REAL(KIND=r8) :: fp
            REAL(KIND=r8) :: ft
            REAL(KIND=r8) :: ft1
            REAL(KIND=r8) :: water
            REAL(KIND=r8) :: scalefac
            REAL(KIND=r8) :: factor
            REAL(KIND=r8) :: compfp
      hvrset = '$Revision: 1.2 $'
      stpfac = 296._r8/1013._r8
      indbound = tbound - 159._r8
      if (indbound .lt. 1) then
         indbound = 1
      elseif (indbound .gt. 180) then
         indbound = 180
      endif
      tbndfrac = tbound - 159._r8 - float(indbound)
      indlev0 = tz(0) - 159._r8
      if (indlev0 .lt. 1) then
         indlev0 = 1
      elseif (indlev0 .gt. 180) then
         indlev0 = 180
      endif
      t0frac = tz(0) - 159._r8 - float(indlev0)
      laytrop = 0
            ! Begin layer loop
            !  Calculate the integrated Planck functions for each band at the
            !  surface, level, and layer temperatures.
      do lay = 1, nlayers
         indlay = tavel(lay) - 159._r8
         if (indlay .lt. 1) then
            indlay = 1
         elseif (indlay .gt. 180) then
            indlay = 180
         endif
         tlayfrac = tavel(lay) - 159._r8 - float(indlay)
         indlev = tz(lay) - 159._r8
         if (indlev .lt. 1) then
            indlev = 1
         elseif (indlev .gt. 180) then
            indlev = 180
         endif
         tlevfrac = tz(lay) - 159._r8 - float(indlev)
                ! Begin spectral band loop
         do iband = 1, 15
            if (lay.eq.1) then
               dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
               plankbnd(iband) = semiss(iband) * &
                   (totplnk(indbound,iband) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
            endif
            dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
            dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
            planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
         enddo
                !  For band 16, if radiative transfer will be performed on just
                !  this band, use integrated Planck values up to 3250 cm-1.
                !  If radiative transfer will be performed across all 16 bands,
                !  then include in the integrated Planck values for this band
                !  contributions from 2600 cm-1 to infinity.
         iband = 16
         if (istart .eq. 16) then
            if (lay.eq.1) then
               dbdtlev = totplk16(indbound+1) - totplk16(indbound)
               plankbnd(iband) = semiss(iband) * &
                    (totplk16(indbound) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplk16(indlev0) + &
                    t0frac * dbdtlev
            endif
            dbdtlev = totplk16(indlev+1) - totplk16(indlev)
            dbdtlay = totplk16(indlay+1) - totplk16(indlay)
            planklay(lay,iband) = totplk16(indlay) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplk16(indlev) + tlevfrac * dbdtlev
         else
            if (lay.eq.1) then
               dbdtlev = totplnk(indbound+1,iband) - totplnk(indbound,iband)
               plankbnd(iband) = semiss(iband) * &
                    (totplnk(indbound,iband) + tbndfrac * dbdtlev)
               dbdtlev = totplnk(indlev0+1,iband)-totplnk(indlev0,iband)
               planklev(0,iband) = totplnk(indlev0,iband) + t0frac * dbdtlev
            endif
            dbdtlev = totplnk(indlev+1,iband) - totplnk(indlev,iband)
            dbdtlay = totplnk(indlay+1,iband) - totplnk(indlay,iband)
            planklay(lay,iband) = totplnk(indlay,iband) + tlayfrac * dbdtlay
            planklev(lay,iband) = totplnk(indlev,iband) + tlevfrac * dbdtlev
         endif
                !  Find the two reference pressures on either side of the
                !  layer pressure.  Store them in JP and JP1.  Store in FP the
                !  fraction of the difference (in ln(pressure)) between these
                !  two values that the layer pressure lies.
                !         plog = alog(pavel(lay))
         plog = dlog(pavel(lay))
         jp(lay) = int(36._r8 - 5*(plog+0.04_r8))
         if (jp(lay) .lt. 1) then
            jp(lay) = 1
         elseif (jp(lay) .gt. 58) then
            jp(lay) = 58
         endif
         jp1 = jp(lay) + 1
         fp = 5._r8 *(preflog(jp(lay)) - plog)
                !  Determine, for each reference pressure (JP and JP1), which
                !  reference temperature (these are different for each
                !  reference pressure) is nearest the layer temperature but does
                !  not exceed it.  Store these indices in JT and JT1, resp.
                !  Store in FT (resp. FT1) the fraction of the way between JT
                !  (JT1) and the next highest reference temperature that the
                !  layer temperature falls.
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
                !  If the pressure is less than ~100mb, perform a different
                !  set of species interpolations.
         if (plog .le. 4.56_r8) go to 5300
         laytrop =  laytrop + 1
         forfac(lay) = scalefac / (1.+water)
         factor = (332.0_r8-tavel(lay))/36.0_r8
         indfor(lay) = min(2, max(1, int(factor)))
         forfrac(lay) = factor - float(indfor(lay))
                !  Set up factors needed to separately include the water vapor
                !  self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)
         factor = (tavel(lay)-188.0_r8)/7.2_r8
         indself(lay) = min(9, max(1, int(factor)-7))
         selffrac(lay) = factor - float(indself(lay) + 7)
                !  Set up factors needed to separately include the minor gases
                !  in the calculation of absorption coefficient
         scaleminor(lay) = pavel(lay)/tavel(lay)
         scaleminorn2(lay) = (pavel(lay)/tavel(lay)) &
             *(wbroad(lay)/(coldry(lay)+wkl(1,lay)))
         factor = (tavel(lay)-180.8_r8)/7.2_r8
         indminor(lay) = min(18, max(1, int(factor)))
         minorfrac(lay) = factor - float(indminor(lay))
                !  Setup reference ratio to be used in calculation of binary
                !  species parameter in lower atmosphere.
         rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
         rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)
         rat_h2oo3(lay)=chi_mls(1,jp(lay))/chi_mls(3,jp(lay))
         rat_h2oo3_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(3,jp(lay)+1)
         rat_h2on2o(lay)=chi_mls(1,jp(lay))/chi_mls(4,jp(lay))
         rat_h2on2o_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(4,jp(lay)+1)
         rat_h2och4(lay)=chi_mls(1,jp(lay))/chi_mls(6,jp(lay))
         rat_h2och4_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(6,jp(lay)+1)
         rat_n2oco2(lay)=chi_mls(4,jp(lay))/chi_mls(2,jp(lay))
         rat_n2oco2_1(lay)=chi_mls(4,jp(lay)+1)/chi_mls(2,jp(lay)+1)
                !  Calculate needed column amounts.
         colh2o(lay) = 1.e-20_r8 * wkl(1,lay)
         colco2(lay) = 1.e-20_r8 * wkl(2,lay)
         colo3(lay) = 1.e-20_r8 * wkl(3,lay)
         coln2o(lay) = 1.e-20_r8 * wkl(4,lay)
         colco(lay) = 1.e-20_r8 * wkl(5,lay)
         colch4(lay) = 1.e-20_r8 * wkl(6,lay)
         colo2(lay) = 1.e-20_r8 * wkl(7,lay)
         if (colco2(lay) .eq. 0._r8) colco2(lay) = 1.e-32_r8 * coldry(lay)
         if (colo3(lay) .eq. 0._r8) colo3(lay) = 1.e-32_r8 * coldry(lay)
         if (coln2o(lay) .eq. 0._r8) coln2o(lay) = 1.e-32_r8 * coldry(lay)
         if (colco(lay) .eq. 0._r8) colco(lay) = 1.e-32_r8 * coldry(lay)
         if (colch4(lay) .eq. 0._r8) colch4(lay) = 1.e-32_r8 * coldry(lay)
         colbrd(lay) = 1.e-20_r8 * wbroad(lay)
         go to 5400
                !  Above laytrop.
 5300    continue
         forfac(lay) = scalefac / (1.+water)
         factor = (tavel(lay)-188.0_r8)/36.0_r8
         indfor(lay) = 3
         forfrac(lay) = factor - 1.0_r8
                !  Set up factors needed to separately include the water vapor
                !  self-continuum in the calculation of absorption coefficient.
         selffac(lay) = water * forfac(lay)
                !  Set up factors needed to separately include the minor gases
                !  in the calculation of absorption coefficient
         scaleminor(lay) = pavel(lay)/tavel(lay)         
         scaleminorn2(lay) = (pavel(lay)/tavel(lay)) &
             * (wbroad(lay)/(coldry(lay)+wkl(1,lay)))
         factor = (tavel(lay)-180.8_r8)/7.2_r8
         indminor(lay) = min(18, max(1, int(factor)))
         minorfrac(lay) = factor - float(indminor(lay))
                !  Setup reference ratio to be used in calculation of binary
                !  species parameter in upper atmosphere.
         rat_h2oco2(lay)=chi_mls(1,jp(lay))/chi_mls(2,jp(lay))
         rat_h2oco2_1(lay)=chi_mls(1,jp(lay)+1)/chi_mls(2,jp(lay)+1)         
         rat_o3co2(lay)=chi_mls(3,jp(lay))/chi_mls(2,jp(lay))
         rat_o3co2_1(lay)=chi_mls(3,jp(lay)+1)/chi_mls(2,jp(lay)+1)         
                !  Calculate needed column amounts.
         colh2o(lay) = 1.e-20_r8 * wkl(1,lay)
         colco2(lay) = 1.e-20_r8 * wkl(2,lay)
         colo3(lay) = 1.e-20_r8 * wkl(3,lay)
         coln2o(lay) = 1.e-20_r8 * wkl(4,lay)
         colco(lay) = 1.e-20_r8 * wkl(5,lay)
         colch4(lay) = 1.e-20_r8 * wkl(6,lay)
         colo2(lay) = 1.e-20_r8 * wkl(7,lay)
         if (colco2(lay) .eq. 0._r8) colco2(lay) = 1.e-32_r8 * coldry(lay)
         if (colo3(lay) .eq. 0._r8) colo3(lay) = 1.e-32_r8 * coldry(lay)
         if (coln2o(lay) .eq. 0._r8) coln2o(lay) = 1.e-32_r8 * coldry(lay)
         if (colco(lay)  .eq. 0._r8) colco(lay) = 1.e-32_r8 * coldry(lay)
         if (colch4(lay) .eq. 0._r8) colch4(lay) = 1.e-32_r8 * coldry(lay)
         colbrd(lay) = 1.e-20_r8 * wbroad(lay)
 5400    continue
                !  We have now isolated the layer ln pressure and temperature,
                !  between two reference pressures and two reference temperatures
                !  (for each reference pressure).  We multiply the pressure
                !  fraction FP with the appropriate temperature fractions to get
                !  the factors that will be needed for the interpolation that yields
                !  the optical depths (performed in routines TAUGBn for band n).`
         compfp = 1. - fp
         fac10(lay) = compfp * ft
         fac00(lay) = compfp * (1._r8 - ft)
         fac11(lay) = fp * ft1
         fac01(lay) = fp * (1._r8 - ft1)
                !  Rescale selffac and forfac for use in taumol
         selffac(lay) = colh2o(lay)*selffac(lay)
         forfac(lay) = colh2o(lay)*forfac(lay)
                ! End layer loop
      enddo
        END SUBROUTINE setcoef
        !***************************************************************************

        !***************************************************************************

    END MODULE rrtmg_lw_setcoef
