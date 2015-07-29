
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_sw_spcvmc.f90
! Generated at: 2015-07-07 00:48:25
! KGEN version: 0.4.13



    MODULE rrtmg_sw_spcvmc
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
        USE parrrsw, ONLY: ngptsw
        USE rrsw_tbl, ONLY: od_lo
        USE rrsw_tbl, ONLY: bpade
        USE rrsw_tbl, ONLY: tblint
        USE rrsw_tbl, ONLY: exp_tbl
        USE rrsw_wvn, ONLY: ngc
        USE rrsw_wvn, ONLY: ngs
        USE rrtmg_sw_reftra, ONLY: reftra_sw
        USE rrtmg_sw_taumol, ONLY: taumol_sw
        USE rrtmg_sw_vrtqdr, ONLY: vrtqdr_sw
        IMPLICIT NONE
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        ! ---------------------------------------------------------------------------

        SUBROUTINE spcvmc_sw(lchnk, ncol, nlayers, istart, iend, icpr, idelm, iout, pavel, tavel, pz, tz, tbound, palbd, palbp, &
        pcldfmc, ptaucmc, pasycmc, pomgcmc, ptaormc, ptaua, pasya, pomga, prmu0, coldry, wkl, adjflux, laytrop, layswtch, laylow, &
        jp, jt, jt1, co2mult, colch4, colco2, colh2o, colmol, coln2o, colo2, colo3, fac00, fac01, fac10, fac11, selffac, selffrac,&
         indself, forfac, forfrac, indfor, pbbfd, pbbfu, pbbcd, pbbcu, puvfd, puvcd, pnifd, pnicd, pnifu, pnicu, pbbfddir, &
        pbbcddir, puvfddir, puvcddir, pnifddir, pnicddir, pbbfsu, pbbfsd)
            ! ---------------------------------------------------------------------------
            !
            ! Purpose: Contains spectral loop to compute the shortwave radiative fluxes,
            !          using the two-stream method of H. Barker and McICA, the Monte-Carlo
            !          Independent Column Approximation, for the representation of
            !          sub-grid cloud variability (i.e. cloud overlap).
            !
            ! Interface:  *spcvmc_sw* is called from *rrtmg_sw.F90* or rrtmg_sw.1col.F90*
            !
            ! Method:
            !    Adapted from two-stream model of H. Barker;
            !    Two-stream model options (selected with kmodts in rrtmg_sw_reftra.F90):
            !        1: Eddington, 2: PIFM, Zdunkowski et al., 3: discret ordinates
            !
            ! Modifications:
            !
            ! Original: H. Barker
            ! Revision: Merge with RRTMG_SW: J.-J.Morcrette, ECMWF, Feb 2003
            ! Revision: Add adjustment for Earth/Sun distance : MJIacono, AER, Oct 2003
            ! Revision: Bug fix for use of PALBP and PALBD: MJIacono, AER, Nov 2003
            ! Revision: Bug fix to apply delta scaling to clear sky: AER, Dec 2004
            ! Revision: Code modified so that delta scaling is not done in cloudy profiles
            !           if routine cldprop is used; delta scaling can be applied by swithcing
            !           code below if cldprop is not used to get cloud properties.
            !           AER, Jan 2005
            ! Revision: Modified to use McICA: MJIacono, AER, Nov 2005
            ! Revision: Uniform formatting for RRTMG: MJIacono, AER, Jul 2006
            ! Revision: Use exponential lookup table for transmittance: MJIacono, AER,
            !           Aug 2007
            !
            ! ------------------------------------------------------------------
            ! ------- Declarations ------
            ! ------- Input -------
            INTEGER, intent(in) :: lchnk
            INTEGER, intent(in) :: nlayers
            INTEGER, intent(in) :: istart
            INTEGER, intent(in) :: iend
            INTEGER, intent(in) :: icpr
            INTEGER, intent(in) :: idelm ! delta-m scaling flag
            ! [0 = direct and diffuse fluxes are unscaled]
            ! [1 = direct and diffuse fluxes are scaled]
            INTEGER, intent(in) :: iout
            INTEGER, intent(in) :: ncol ! column loop index
            INTEGER, intent(in) :: laytrop(ncol)
            INTEGER, intent(in) :: layswtch(ncol)
            INTEGER, intent(in) :: laylow(ncol)
            INTEGER, intent(in) :: indfor(:,:)
            !   Dimensions: (ncol,nlayers)
            INTEGER, intent(in) :: indself(:,:)
            !   Dimensions: (ncol,nlayers)
            INTEGER, intent(in) :: jp(:,:)
            !   Dimensions: (ncol,nlayers)
            INTEGER, intent(in) :: jt(:,:)
            !   Dimensions: (ncol,nlayers)
            INTEGER, intent(in) :: jt1(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: pavel(:,:) ! layer pressure (hPa, mb)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: tavel(:,:) ! layer temperature (K)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: pz(:,0:) ! level (interface) pressure (hPa, mb)
            !   Dimensions: (ncol,0:nlayers)
            REAL(KIND=r8), intent(in) :: tz(:,0:) ! level temperatures (hPa, mb)
            !   Dimensions: (ncol,0:nlayers)
            REAL(KIND=r8), intent(in) :: tbound(ncol) ! surface temperature (K)
            REAL(KIND=r8), intent(in) :: wkl(:,:,:) ! molecular amounts (mol/cm2)
            !   Dimensions: (ncol,mxmol,nlayers)
            REAL(KIND=r8), intent(in) :: coldry(:,:) ! dry air column density (mol/cm2)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: colmol(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: adjflux(:,:) ! Earth/Sun distance adjustment
            !   Dimensions: (ncol,jpband)
            REAL(KIND=r8), intent(in) :: palbd(:,:) ! surface albedo (diffuse)
            !   Dimensions: (ncol,nbndsw)
            REAL(KIND=r8), intent(in) :: palbp(:,:) ! surface albedo (direct)
            !   Dimensions: (ncol, nbndsw)
            REAL(KIND=r8), intent(in) :: prmu0(ncol) ! cosine of solar zenith angle
            REAL(KIND=r8), intent(in) :: pcldfmc(:,:,:) ! cloud fraction [mcica]
            !   Dimensions: (ncol,nlayers,ngptsw)
            REAL(KIND=r8), intent(in) :: ptaucmc(:,:,:) ! cloud optical depth [mcica]
            !   Dimensions: (ncol,nlayers,ngptsw)
            REAL(KIND=r8), intent(in) :: pasycmc(:,:,:) ! cloud asymmetry parameter [mcica]
            !   Dimensions: (ncol,nlayers,ngptsw)
            REAL(KIND=r8), intent(in) :: pomgcmc(:,:,:) ! cloud single scattering albedo [mcica]
            !   Dimensions: (ncol,nlayers,ngptsw)
            REAL(KIND=r8), intent(in) :: ptaormc(:,:,:) ! cloud optical depth, non-delta scaled [mcica]
            !   Dimensions: (ncol,nlayers,ngptsw)
            REAL(KIND=r8), intent(in) :: ptaua(:,:,:) ! aerosol optical depth
            !   Dimensions: (ncol,nlayers,nbndsw)
            REAL(KIND=r8), intent(in) :: pasya(:,:,:) ! aerosol asymmetry parameter
            !   Dimensions: (ncol,nlayers,nbndsw)
            REAL(KIND=r8), intent(in) :: pomga(:,:,:) ! aerosol single scattering albedo
            !   Dimensions: (ncol,nlayers,nbndsw)
            REAL(KIND=r8), intent(in) :: colh2o(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: colco2(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: colch4(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: co2mult(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: colo3(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: colo2(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: coln2o(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: forfac(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: forfrac(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: selffac(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: selffrac(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: fac00(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: fac01(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: fac10(:,:)
            !   Dimensions: (ncol,nlayers)
            REAL(KIND=r8), intent(in) :: fac11(:,:)
            !   Dimensions: (ncol,nlayers)
            ! ------- Output -------
            !   All Dimensions: (nlayers+1)
            REAL(KIND=r8), intent(out) :: pbbcd(:,:)
            REAL(KIND=r8), intent(out) :: pbbcu(:,:)
            REAL(KIND=r8), intent(out) :: pbbfd(:,:)
            REAL(KIND=r8), intent(out) :: pbbfu(:,:)
            REAL(KIND=r8), intent(out) :: pbbfddir(:,:)
            REAL(KIND=r8), intent(out) :: pbbcddir(:,:)
            REAL(KIND=r8), intent(out) :: puvcd(:,:)
            REAL(KIND=r8), intent(out) :: puvfd(:,:)
            REAL(KIND=r8), intent(out) :: puvcddir(:,:)
            REAL(KIND=r8), intent(out) :: puvfddir(:,:)
            REAL(KIND=r8), intent(out) :: pnicd(:,:)
            REAL(KIND=r8), intent(out) :: pnifd(:,:)
            REAL(KIND=r8), intent(out) :: pnicddir(:,:)
            REAL(KIND=r8), intent(out) :: pnifddir(:,:)
            ! Added for net near-IR flux diagnostic
            REAL(KIND=r8), intent(out) :: pnicu(:,:)
            REAL(KIND=r8), intent(out) :: pnifu(:,:)
            ! Output - inactive                                              !   All Dimensions: (nlayers+1)
            !      real(kind=r8), intent(out) :: puvcu(:)
            !      real(kind=r8), intent(out) :: puvfu(:)
            !      real(kind=r8), intent(out) :: pvscd(:)
            !      real(kind=r8), intent(out) :: pvscu(:)
            !      real(kind=r8), intent(out) :: pvsfd(:)
            !      real(kind=r8), intent(out) :: pvsfu(:)
            REAL(KIND=r8), intent(out) :: pbbfsu(:,:,:) ! shortwave spectral flux up (nswbands,nlayers+1)
            REAL(KIND=r8), intent(out) :: pbbfsd(:,:,:) ! shortwave spectral flux down (nswbands,nlayers+1)
            ! ------- Local -------
            LOGICAL :: lrtchkclr(nlayers)
            LOGICAL :: lrtchkcld(nlayers)
            INTEGER :: klev
            INTEGER :: ib1
            INTEGER :: ib2
            INTEGER :: ibm
            INTEGER :: igt
            INTEGER :: ikl
            INTEGER :: iw
            INTEGER :: jk
            INTEGER :: jb
            INTEGER :: jg, iplon
            !      integer, parameter :: nuv = ??
            !      integer, parameter :: nvs = ??
            INTEGER :: itind
            REAL(KIND=r8) :: ze1
            REAL(KIND=r8) :: tblind
            REAL(KIND=r8) :: zclear
            REAL(KIND=r8) :: zcloud
            REAL(KIND=r8) :: zdbt(nlayers+1)
            REAL(KIND=r8) :: zdbt_nodel(nlayers+1)
            REAL(KIND=r8) :: zgcc(nlayers)
            REAL(KIND=r8) :: zgco(nlayers)
            REAL(KIND=r8) :: zomcc(nlayers)
            REAL(KIND=r8) :: zomco(nlayers)
            REAL(KIND=r8) :: zrdndc(nlayers+1)
            REAL(KIND=r8) :: zrdnd(nlayers+1)
            REAL(KIND=r8) :: zrefc(nlayers+1)
            REAL(KIND=r8) :: zrefo(nlayers+1)
            REAL(KIND=r8) :: zref(nlayers+1)
            REAL(KIND=r8) :: zrefdc(nlayers+1)
            REAL(KIND=r8) :: zrefdo(nlayers+1)
            REAL(KIND=r8) :: zrefd(nlayers+1)
            REAL(KIND=r8) :: zrup(nlayers+1)
            REAL(KIND=r8) :: zrupd(nlayers+1)
            REAL(KIND=r8) :: zrupc(nlayers+1)
            REAL(KIND=r8) :: zrupdc(nlayers+1)
            REAL(KIND=r8) :: ztauc(nlayers)
            REAL(KIND=r8) :: ztauo(nlayers)
            REAL(KIND=r8) :: ztdbt(nlayers+1)
            REAL(KIND=r8) :: ztrac(nlayers+1)
            REAL(KIND=r8) :: ztrao(nlayers+1)
            REAL(KIND=r8) :: ztra(nlayers+1)
            REAL(KIND=r8) :: ztradc(nlayers+1)
            REAL(KIND=r8) :: ztrado(nlayers+1)
            REAL(KIND=r8) :: ztrad(nlayers+1)
            REAL(KIND=r8) :: ztdbtc(nlayers+1)
            REAL(KIND=r8) :: zdbtc(nlayers+1)
            REAL(KIND=r8) :: zincflx(ngptsw)
            REAL(KIND=r8) :: zdbtc_nodel(nlayers+1)
            REAL(KIND=r8) :: ztdbtc_nodel(nlayers+1)
            REAL(KIND=r8) :: ztdbt_nodel(nlayers+1)
            REAL(KIND=r8) :: zdbtmc
            REAL(KIND=r8) :: zdbtmo
            REAL(KIND=r8) :: zf
            REAL(KIND=r8) :: repclc
            REAL(KIND=r8) :: tauorig
            REAL(KIND=r8) :: zwf
            !     real(kind=r8) :: zincflux                                   ! inactive
            ! Arrays from rrtmg_sw_taumoln routines
            !      real(kind=r8) :: ztaug(nlayers,16), ztaur(nlayers,16)
            !      real(kind=r8) :: zsflxzen(16)
            REAL(KIND=r8) :: ztaug(ncol,nlayers,ngptsw)
            REAL(KIND=r8) :: ztaur(ncol,nlayers,ngptsw)
            REAL(KIND=r8) :: zsflxzen(ncol,ngptsw)
            ! Arrays from rrtmg_sw_vrtqdr routine
            REAL(KIND=r8) :: zcd(nlayers+1,ngptsw)
            REAL(KIND=r8) :: zcu(nlayers+1,ngptsw)
            REAL(KIND=r8) :: zfd(nlayers+1,ngptsw)
            REAL(KIND=r8) :: zfu(nlayers+1,ngptsw)
            ! Inactive arrays
            !     real(kind=r8) :: zbbcd(nlayers+1), zbbcu(nlayers+1)
            !     real(kind=r8) :: zbbfd(nlayers+1), zbbfu(nlayers+1)
            !     real(kind=r8) :: zbbfddir(nlayers+1), zbbcddir(nlayers+1)
            ! ------------------------------------------------------------------
            ! Initializations
      ib1 = istart
      ib2 = iend
      klev = nlayers
      repclc = 1.e-12_r8
            !      zincflux = 0.0_r8
        do iplon=1,ncol
      do jk=1,klev+1
         pbbcd(iplon,jk)=0._r8
         pbbcu(iplon,jk)=0._r8
         pbbfd(iplon,jk)=0._r8
         pbbfu(iplon,jk)=0._r8
         pbbcddir(iplon,jk)=0._r8
         pbbfddir(iplon,jk)=0._r8
         puvcd(iplon,jk)=0._r8
         puvfd(iplon,jk)=0._r8
         puvcddir(iplon,jk)=0._r8
         puvfddir(iplon,jk)=0._r8
         pnicd(iplon,jk)=0._r8
         pnifd(iplon,jk)=0._r8
         pnicddir(iplon,jk)=0._r8
         pnifddir(iplon,jk)=0._r8
         pnicu(iplon,jk)=0._r8
         pnifu(iplon,jk)=0._r8
      enddo
        end do
        do iplon=1,ncol
            ! Calculate the optical depths for gaseous absorption and Rayleigh scattering
      call taumol_sw(klev, &
                     colh2o(iplon,:), colco2(iplon,:), colch4(iplon,:), colo2(iplon,:), colo3(iplon,:), colmol(iplon,:), &
                     laytrop(iplon), jp(iplon,:), jt(iplon,:), jt1(iplon,:), &
                     fac00(iplon,:), fac01(iplon,:), fac10(iplon,:), fac11(iplon,:), &
                     selffac(iplon,:), selffrac(iplon,:), indself(iplon,:), forfac(iplon,:), forfrac(iplon,:),indfor(iplon,:), &
                     zsflxzen(iplon,:), ztaug(iplon,:,:), ztaur(iplon,:,:))
            ! Top of shortwave spectral band loop, jb = 16 -> 29; ibm = 1 -> 14
        end do
        do iplon=1,ncol
      iw = 0
      jb = ib1-1                  ! ??? ! ???
      do jb = ib1, ib2
         ibm = jb-15
         igt = ngc(ibm)
                ! Reinitialize g-point counter for each band if output for each band is requested.
         if (iout.gt.0.and.ibm.ge.2) iw= ngs(ibm-1)
                !        do jk=1,klev+1
                !           zbbcd(jk)=0.0_r8
                !           zbbcu(jk)=0.0_r8
                !           zbbfd(jk)=0.0_r8
                !           zbbfu(jk)=0.0_r8
                !        enddo
                ! Top of g-point interval loop within each band (iw is cumulative counter)
         do jg = 1,igt
            iw = iw+1
                    ! Apply adjustment for correct Earth/Sun distance and zenith angle to incoming solar flux
            zincflx(iw) = adjflux(iplon,jb) * zsflxzen(iplon,iw) * prmu0(iplon)
                    !             zincflux = zincflux + adjflux(jb) * zsflxzen(iw) * prmu0           ! inactive
                    ! Compute layer reflectances and transmittances for direct and diffuse sources,
                    ! first clear then cloudy
                    ! zrefc(jk)  direct albedo for clear
                    ! zrefo(jk)  direct albedo for cloud
                    ! zrefdc(jk) diffuse albedo for clear
                    ! zrefdo(jk) diffuse albedo for cloud
                    ! ztrac(jk)  direct transmittance for clear
                    ! ztrao(jk)  direct transmittance for cloudy
                    ! ztradc(jk) diffuse transmittance for clear
                    ! ztrado(jk) diffuse transmittance for cloudy
                    !
                    ! zref(jk)   direct reflectance
                    ! zrefd(jk)  diffuse reflectance
                    ! ztra(jk)   direct transmittance
                    ! ztrad(jk)  diffuse transmittance
                    !
                    ! zdbtc(jk)  clear direct beam transmittance
                    ! zdbto(jk)  cloudy direct beam transmittance
                    ! zdbt(jk)   layer mean direct beam transmittance
                    ! ztdbt(jk)  total direct beam transmittance at levels
                    ! Clear-sky
                    !   TOA direct beam
            ztdbtc(1)=1.0_r8
            ztdbtc_nodel(1)=1.0_r8
                    !   Surface values
            zdbtc(klev+1) =0.0_r8
            ztrac(klev+1) =0.0_r8
            ztradc(klev+1)=0.0_r8
            zrefc(klev+1) =palbp(iplon,ibm)
            zrefdc(klev+1)=palbd(iplon,ibm)
            zrupc(klev+1) =palbp(iplon,ibm)
            zrupdc(klev+1)=palbd(iplon,ibm)
                    ! Cloudy-sky
                    !   Surface values
            ztrao(klev+1) =0.0_r8
            ztrado(klev+1)=0.0_r8
            zrefo(klev+1) =palbp(iplon,ibm)
            zrefdo(klev+1)=palbd(iplon,ibm)
                    ! Total sky
                    !   TOA direct beam
            ztdbt(1)=1.0_r8
            ztdbt_nodel(1)=1.0_r8
                    !   Surface values
            zdbt(klev+1) =0.0_r8
            ztra(klev+1) =0.0_r8
            ztrad(klev+1)=0.0_r8
            zref(klev+1) =palbp(iplon,ibm)
            zrefd(klev+1)=palbd(iplon,ibm)
            zrup(klev+1) =palbp(iplon,ibm)
            zrupd(klev+1)=palbd(iplon,ibm)
                    ! Top of layer loop
            do jk=1,klev
                        ! Note: two-stream calculations proceed from top to bottom;
                        !   RRTMG_SW quantities are given bottom to top and are reversed here
               ikl=klev+1-jk
                        ! Set logical flag to do REFTRA calculation
                        !   Do REFTRA for all clear layers
               lrtchkclr(jk)=.true.
                        !   Do REFTRA only for cloudy layers in profile, since already done for clear layers
               lrtchkcld(jk)=.false.
               lrtchkcld(jk)=(pcldfmc(iplon,ikl,iw) > repclc)
                        ! Clear-sky optical parameters - this section inactive
                        !   Original
                        !               ztauc(jk) = ztaur(ikl,iw) + ztaug(ikl,iw)
                        !               zomcc(jk) = ztaur(ikl,iw) / ztauc(jk)
                        !               zgcc(jk) = 0.0001_r8
                        !   Total sky optical parameters
                        !               ztauo(jk) = ztaur(ikl,iw) + ztaug(ikl,iw) + ptaucmc(ikl,iw)
                        !               zomco(jk) = ptaucmc(ikl,iw) * pomgcmc(ikl,iw) + ztaur(ikl,iw)
                        !               zgco (jk) = (ptaucmc(ikl,iw) * pomgcmc(ikl,iw) * pasycmc(ikl,iw) + &
                        !                           ztaur(ikl,iw) * 0.0001_r8) / zomco(jk)
                        !               zomco(jk) = zomco(jk) / ztauo(jk)
                        ! Clear-sky optical parameters including aerosols
               ztauc(jk) = ztaur(iplon,ikl,iw) + ztaug(iplon,ikl,iw) + ptaua(iplon,ikl,ibm)
               zomcc(jk) = ztaur(iplon,ikl,iw) * 1.0_r8 + ptaua(iplon,ikl,ibm) * pomga(iplon,ikl,ibm)
               zgcc(jk) = pasya(iplon,ikl,ibm) * pomga(iplon,ikl,ibm) * ptaua(iplon,ikl,ibm) / zomcc(jk)
               zomcc(jk) = zomcc(jk) / ztauc(jk)
                        ! Pre-delta-scaling clear and cloudy direct beam transmittance (must use 'orig', unscaled cloud OD)
                        !   \/\/\/ This block of code is only needed for unscaled direct beam calculation
               if (idelm .eq. 0) then
                            !
                  zclear = 1.0_r8 - pcldfmc(iplon,ikl,iw)
                  zcloud = pcldfmc(iplon,ikl,iw)
                            ! Clear
                            !                   zdbtmc = exp(-ztauc(jk) / prmu0)
                            ! Use exponential lookup table for transmittance, or expansion of exponential for low tau
                  ze1 = ztauc(jk) / prmu0(iplon)
                  if (ze1 .le. od_lo) then
                     zdbtmc = 1._r8 - ze1 + 0.5_r8 * ze1 * ze1
                  else 
                     tblind = ze1 / (bpade + ze1)
                     itind = tblint * tblind + 0.5_r8
                     zdbtmc = exp_tbl(itind)
                  endif
                  zdbtc_nodel(jk) = zdbtmc
                  ztdbtc_nodel(jk+1) = zdbtc_nodel(jk) * ztdbtc_nodel(jk)
                            ! Clear + Cloud
                  tauorig = ztauc(jk) + ptaormc(iplon,ikl,iw)
                            !                   zdbtmo = exp(-tauorig / prmu0)
                            ! Use exponential lookup table for transmittance, or expansion of exponential for low tau
                  ze1 = tauorig / prmu0(iplon)
                  if (ze1 .le. od_lo) then
                     zdbtmo = 1._r8 - ze1 + 0.5_r8 * ze1 * ze1
                  else
                     tblind = ze1 / (bpade + ze1)
                     itind = tblint * tblind + 0.5_r8
                     zdbtmo = exp_tbl(itind)
                  endif
                  zdbt_nodel(jk) = zclear*zdbtmc + zcloud*zdbtmo
                  ztdbt_nodel(jk+1) = zdbt_nodel(jk) * ztdbt_nodel(jk)
               endif
                        !   /\/\/\ Above code only needed for unscaled direct beam calculation
                        ! Delta scaling - clear
               zf = zgcc(jk) * zgcc(jk)
               zwf = zomcc(jk) * zf
               ztauc(jk) = (1.0_r8 - zwf) * ztauc(jk)
               zomcc(jk) = (zomcc(jk) - zwf) / (1.0_r8 - zwf)
               zgcc (jk) = (zgcc(jk) - zf) / (1.0_r8 - zf)
                        ! Total sky optical parameters (cloud properties already delta-scaled)
                        !   Use this code if cloud properties are derived in rrtmg_sw_cldprop
               if (icpr .ge. 1) then
                  ztauo(jk) = ztauc(jk) + ptaucmc(iplon,ikl,iw)
                  zomco(jk) = ztauc(jk) * zomcc(jk) + ptaucmc(iplon,ikl,iw) * pomgcmc(iplon,ikl,iw) 
                  zgco (jk) = (ptaucmc(iplon,ikl,iw) * pomgcmc(iplon,ikl,iw) * pasycmc(iplon,ikl,iw) + &
                              ztauc(jk) * zomcc(jk) * zgcc(jk)) / zomco(jk)
                  zomco(jk) = zomco(jk) / ztauo(jk)
                            ! Total sky optical parameters (if cloud properties not delta scaled)
                            !   Use this code if cloud properties are not derived in rrtmg_sw_cldprop
               elseif (icpr .eq. 0) then
                  ztauo(jk) = ztaur(iplon,ikl,iw) + ztaug(iplon,ikl,iw) + ptaua(iplon,ikl,ibm) + ptaucmc(iplon,ikl,iw)
                  zomco(jk) = ptaua(iplon,ikl,ibm) * pomga(iplon,ikl,ibm) + ptaucmc(iplon,ikl,iw) * pomgcmc(iplon,ikl,iw) + &
                              ztaur(iplon,ikl,iw) * 1.0_r8
                  zgco (jk) = (ptaucmc(iplon,ikl,iw) * pomgcmc(iplon,ikl,iw) * pasycmc(iplon,ikl,iw) + &
                              ptaua(iplon,ikl,ibm)*pomga(iplon,ikl,ibm)*pasya(iplon,ikl,ibm)) / zomco(jk)
                  zomco(jk) = zomco(jk) / ztauo(jk)
                            ! Delta scaling - clouds
                            !   Use only if subroutine rrtmg_sw_cldprop is not used to get cloud properties and to apply delta scaling
                  zf = zgco(jk) * zgco(jk)
                  zwf = zomco(jk) * zf
                  ztauo(jk) = (1._r8 - zwf) * ztauo(jk)
                  zomco(jk) = (zomco(jk) - zwf) / (1.0_r8 - zwf)
                  zgco (jk) = (zgco(jk) - zf) / (1.0_r8 - zf)
               endif 
                        ! End of layer loop
            enddo    
                    ! Clear sky reflectivities
            call reftra_sw (klev, &
                            lrtchkclr, zgcc, prmu0(iplon), ztauc, zomcc, &
                            zrefc, zrefdc, ztrac, ztradc)
                    ! Total sky reflectivities
            call reftra_sw (klev, &
                            lrtchkcld, zgco, prmu0(iplon), ztauo, zomco, &
                            zrefo, zrefdo, ztrao, ztrado)
            do jk=1,klev
                        ! Combine clear and cloudy contributions for total sky
               ikl = klev+1-jk 
               zclear = 1.0_r8 - pcldfmc(iplon,ikl,iw)
               zcloud = pcldfmc(iplon,ikl,iw)
               zref(jk) = zclear*zrefc(jk) + zcloud*zrefo(jk)
               zrefd(jk)= zclear*zrefdc(jk) + zcloud*zrefdo(jk)
               ztra(jk) = zclear*ztrac(jk) + zcloud*ztrao(jk)
               ztrad(jk)= zclear*ztradc(jk) + zcloud*ztrado(jk)
                        ! Direct beam transmittance
                        ! Clear
                        !                zdbtmc = exp(-ztauc(jk) / prmu0)
                        ! Use exponential lookup table for transmittance, or expansion of
                        ! exponential for low tau
               ze1 = ztauc(jk) / prmu0(iplon)
               if (ze1 .le. od_lo) then
                  zdbtmc = 1._r8 - ze1 + 0.5_r8 * ze1 * ze1
               else
                  tblind = ze1 / (bpade + ze1)
                  itind = tblint * tblind + 0.5_r8
                  zdbtmc = exp_tbl(itind)
               endif
               zdbtc(jk) = zdbtmc
               ztdbtc(jk+1) = zdbtc(jk)*ztdbtc(jk)
                        ! Clear + Cloud
                        !                zdbtmo = exp(-ztauo(jk) / prmu0)
                        ! Use exponential lookup table for transmittance, or expansion of
                        ! exponential for low tau
               ze1 = ztauo(jk) / prmu0(iplon)
               if (ze1 .le. od_lo) then
                  zdbtmo = 1._r8 - ze1 + 0.5_r8 * ze1 * ze1
               else
                  tblind = ze1 / (bpade + ze1)
                  itind = tblint * tblind + 0.5_r8
                  zdbtmo = exp_tbl(itind)
               endif
               zdbt(jk) = zclear*zdbtmc + zcloud*zdbtmo
               ztdbt(jk+1) = zdbt(jk)*ztdbt(jk)
            enddo           
                    ! Vertical quadrature for clear-sky fluxes
            call vrtqdr_sw(klev, iw, &
                           zrefc, zrefdc, ztrac, ztradc, &
                           zdbtc, zrdndc, zrupc, zrupdc, ztdbtc, &
                           zcd, zcu)
                    ! Vertical quadrature for cloudy fluxes
            call vrtqdr_sw(klev, iw, &
                           zref, zrefd, ztra, ztrad, &
                           zdbt, zrdnd, zrup, zrupd, ztdbt, &
                           zfd, zfu)
                    ! Upwelling and downwelling fluxes at levels
                    !   Two-stream calculations go from top to bottom;
                    !   layer indexing is reversed to go bottom to top for output arrays
            do jk=1,klev+1
               ikl=klev+2-jk
                        ! Accumulate spectral fluxes over bands - inactive
                        !               zbbfu(ikl) = zbbfu(ikl) + zincflx(iw)*zfu(jk,iw)
                        !               zbbfd(ikl) = zbbfd(ikl) + zincflx(iw)*zfd(jk,iw)
                        !               zbbcu(ikl) = zbbcu(ikl) + zincflx(iw)*zcu(jk,iw)
                        !               zbbcd(ikl) = zbbcd(ikl) + zincflx(iw)*zcd(jk,iw)
                        !               zbbfddir(ikl) = zbbfddir(ikl) + zincflx(iw)*ztdbt_nodel(jk)
                        !               zbbcddir(ikl) = zbbcddir(ikl) + zincflx(iw)*ztdbtc_nodel(jk)
               pbbfsu(iplon,ibm,ikl) = pbbfsu(iplon,ibm,ikl) + zincflx(iw)*zfu(jk,iw)
               pbbfsd(iplon,ibm,ikl) = pbbfsd(iplon,ibm,ikl) + zincflx(iw)*zfd(jk,iw)
                        ! Accumulate spectral fluxes over whole spectrum
               pbbfu(iplon,ikl) = pbbfu(iplon,ikl) + zincflx(iw)*zfu(jk,iw)
               pbbfd(iplon,ikl) = pbbfd(iplon,ikl) +zincflx(iw)*zfd(jk,iw)
               pbbcu(iplon,ikl) = pbbcu(iplon,ikl) + zincflx(iw)*zcu(jk,iw)
               pbbcd(iplon,ikl) = pbbcd(iplon,ikl) + zincflx(iw)*zcd(jk,iw)
               if (idelm .eq. 0) then
                  pbbfddir(iplon,ikl) = pbbfddir(iplon,ikl) + zincflx(iw)*ztdbt_nodel(jk)
                  pbbcddir(iplon,ikl) = pbbcddir(iplon,ikl) + zincflx(iw)*ztdbtc_nodel(jk)
               elseif (idelm .eq. 1) then
                  pbbfddir(iplon,ikl) = pbbfddir(iplon,ikl) + zincflx(iw)*ztdbt(jk)
                  pbbcddir(iplon,ikl) = pbbcddir(iplon,ikl) + zincflx(iw)*ztdbtc(jk)
               endif
                        ! Accumulate direct fluxes for UV/visible bands
               if (ibm >= 10 .and. ibm <= 13) then
                  puvcd(iplon,ikl) = puvcd(iplon,ikl) + zincflx(iw)*zcd(jk,iw)
                  puvfd(iplon,ikl) = puvfd(iplon,ikl) + zincflx(iw)*zfd(jk,iw)
                  if (idelm .eq. 0) then
                     puvfddir(iplon,ikl) = puvfddir(iplon,ikl) + zincflx(iw)*ztdbt_nodel(jk)
                     puvcddir(iplon,ikl) = puvcddir(iplon,ikl) + zincflx(iw)*ztdbtc_nodel(jk)
                  elseif (idelm .eq. 1) then
                     puvfddir(iplon,ikl) = puvfddir(iplon,ikl) + zincflx(iw)*ztdbt(jk)
                     puvcddir(iplon,ikl) = puvcddir(iplon,ikl) + zincflx(iw)*ztdbtc(jk)
                  endif
                            ! band 9 is half-NearIR and half-Visible
               else if (ibm == 9) then  
                  puvcd(iplon,ikl) = puvcd(iplon,ikl) + 0.5_r8*zincflx(iw)*zcd(jk,iw)
                  puvfd(iplon,ikl) = puvfd(iplon,ikl) + 0.5_r8*zincflx(iw)*zfd(jk,iw)
                  pnicd(iplon,ikl) = pnicd(iplon,ikl) + 0.5_r8*zincflx(iw)*zcd(jk,iw)
                  pnifd(iplon,ikl) = pnifd(iplon,ikl) + 0.5_r8*zincflx(iw)*zfd(jk,iw)
                  if (idelm .eq. 0) then
                     puvfddir(iplon,ikl) = puvfddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbt_nodel(jk)
                     puvcddir(iplon,ikl) = puvcddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbtc_nodel(jk)
                     pnifddir(iplon,ikl) = pnifddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbt_nodel(jk)
                     pnicddir(iplon,ikl) = pnicddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbtc_nodel(jk)
                  elseif (idelm .eq. 1) then
                     puvfddir(iplon,ikl) = puvfddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbt(jk)
                     puvcddir(iplon,ikl) = puvcddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbtc(jk)
                     pnifddir(iplon,ikl) = pnifddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbt(jk)
                     pnicddir(iplon,ikl) = pnicddir(iplon,ikl) + 0.5_r8*zincflx(iw)*ztdbtc(jk)
                  endif
                  pnicu(iplon,ikl) = pnicu(iplon,ikl) + 0.5_r8*zincflx(iw)*zcu(jk,iw)
                  pnifu(iplon,ikl) = pnifu(iplon,ikl) + 0.5_r8*zincflx(iw)*zfu(jk,iw)
                            ! Accumulate direct fluxes for near-IR bands
               else if (ibm == 14 .or. ibm <= 8) then  
                  pnicd(iplon,ikl) = pnicd(iplon,ikl) + zincflx(iw)*zcd(jk,iw)
                  pnifd(iplon,ikl) = pnifd(iplon,ikl) + zincflx(iw)*zfd(jk,iw)
                  if (idelm .eq. 0) then
                     pnifddir(iplon,ikl) = pnifddir(iplon,ikl) + zincflx(iw)*ztdbt_nodel(jk)
                     pnicddir(iplon,ikl) = pnicddir(iplon,ikl) + zincflx(iw)*ztdbtc_nodel(jk)
                  elseif (idelm .eq. 1) then
                     pnifddir(iplon,ikl) = pnifddir(iplon,ikl) + zincflx(iw)*ztdbt(jk)
                     pnicddir(iplon,ikl) = pnicddir(iplon,ikl) + zincflx(iw)*ztdbtc(jk)
                  endif
                            ! Added for net near-IR flux diagnostic
                  pnicu(iplon,ikl) = pnicu(iplon,ikl) + zincflx(iw)*zcu(jk,iw)
                  pnifu(iplon,ikl) = pnifu(iplon,ikl) + zincflx(iw)*zfu(jk,iw)
               endif
            enddo
                    ! End loop on jg, g-point interval
         enddo             
                ! End loop on jb, spectral band
      enddo                   
        end do 
        END SUBROUTINE spcvmc_sw
    END MODULE rrtmg_sw_spcvmc
