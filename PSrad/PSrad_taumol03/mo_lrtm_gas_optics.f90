    MODULE mo_lrtm_gas_optics
    USE kgen_utils, only : read_var, verify_var
    USE mo_kind, only : wp
    USE rrlw_planck,    ONLY : chi_mls
    USE mo_lrtm_setup,  ONLY : nspa, nspb

    REAL(wp), PARAMETER :: oneminus = 1.0_wp - 1.0e-06_wp

    CONTAINS

    SUBROUTINE gas_optics_lw(nlayers, coldry, laytrop, jp, jt, jt1, colh2o, colco2, coln2o, fac00, fac01, fac10, fac11, &
        rat_h2oco2, rat_h2oco2_1, selffac, selffrac, indself, forfac, forfrac, indfor, minorfrac, indminor, fracs, taug, kgen_unit)
    INTEGER, INTENT(in) :: nlayers         ! total number of layers
    REAL(wp), INTENT(in) :: coldry(:)          ! column amount (dry air)
    INTEGER, INTENT(in) :: laytrop         ! tropopause layer index
    INTEGER, INTENT(in) :: jp(:) 
    INTEGER, INTENT(in) :: jt(:)
    INTEGER, INTENT(in) :: jt1(:)
    REAL(wp), INTENT(in) :: colh2o(:)          ! column amount (h2o)
    REAL(wp), INTENT(in) :: colco2(:)          ! column amount (co2)
    REAL(wp), INTENT(in) :: coln2o(:)          ! column amount (n2o)
    REAL(wp), INTENT(in) :: fac00(:), fac01(:), fac10(:), fac11(:)
    REAL(wp), INTENT(in) :: rat_h2oco2(:),rat_h2oco2_1(:)
    REAL(wp), INTENT(in) :: selffac(:)
    REAL(wp), INTENT(in) :: selffrac(:)
    INTEGER, INTENT(in) :: indself(:)
    REAL(wp), INTENT(in) :: forfac(:)
    REAL(wp), INTENT(in) :: forfrac(:)
    INTEGER, INTENT(in) :: indfor(:)
    REAL(wp), INTENT(in) :: minorfrac(:)
    INTEGER, INTENT(in) :: indminor(:)

    ! ----- Output -----
    REAL(wp), INTENT(out) :: fracs(:)        ! planck fractions Dimensions: (nlayers)
    REAL(wp), INTENT(out) :: taug(:)         ! gaseous optical depth Dimensions: (nlayers)

    INTEGER, INTENT(IN)  :: kgen_unit

    REAL(wp), allocatable :: ref_fracs(:)        ! planck fractions Dimensions: (nlayers)
    REAL(wp), allocatable :: ref_taug(:)         ! gaseous optical depth Dimensions: (nlayers)
    INTEGER :: ig

    INTEGER i
    INTEGER, parameter :: ITER = 1000
    INTEGER*8 :: rdtsc, start_clock, end_clock

    ! READ local variables
    READ(UNIT=kgen_unit) ig

    ! READ out and inout states from call site
    call read_var(ref_fracs, kgen_unit)
    call read_var(ref_taug, kgen_unit)

    ! RUN KERNEL
    CALL taumol03

    ! verify out and inout states from call site
    call verify_var('fracs', fracs, ref_fracs)
    call verify_var('taug', taug, ref_taug)

    ! measure average runtime
    start_clock = rdtsc()
    do i=1,ITER 
        CALL taumol03
    end do
    end_clock = rdtsc()
    write(*,*) "Elapsed clocks: ", (end_clock - start_clock)/ITER

    CONTAINS

    SUBROUTINE taumol03
      !----------------------------------------------------------------------------
      !
      !     band 3:  500-630 cm-1 (low key - h2o,co2; low minor - n2o)
      !                           (high key - h2o,co2; high minor - n2o)
      !----------------------------------------------------------------------------

      ! ------- Modules -------

      USE rrlw_kg03, ONLY : fracrefa, fracrefb, absa, absb, ka_mn2o, kb_mn2o, selfref, forref

      ! ------- Declarations -------

      ! Local 
      INTEGER :: lay, ind0, ind1, inds, indf, indm
      INTEGER :: js, js1, jmn2o, jpl
      REAL(wp) :: speccomb, specparm, specmult, fs
      REAL(wp) :: speccomb1, specparm1, specmult1, fs1
      REAL(wp) :: speccomb_mn2o, specparm_mn2o, specmult_mn2o, &
           fmn2o, fmn2omf, chi_n2o, ratn2o, adjfac, adjcoln2o
      REAL(wp) :: speccomb_planck, specparm_planck, specmult_planck, fpl
      REAL(wp) :: p, p4, fk0, fk1, fk2
      REAL(wp) :: fac000, fac100, fac200, fac010, fac110, fac210
      REAL(wp) :: fac001, fac101, fac201, fac011, fac111, fac211
      REAL(wp) :: tauself, taufor, n2om1, n2om2, absn2o
      REAL(wp) :: refrat_planck_a, refrat_planck_b, refrat_m_a, refrat_m_b
      REAL(wp) :: tau_major, tau_major1


      ! Minor gas mapping levels:
      !     lower - n2o, p = 706.272 mbar, t = 278.94 k
      !     upper - n2o, p = 95.58 mbar, t = 215.7 k

      !  P = 212.725 mb
      refrat_planck_a = chi_mls(1,9)/chi_mls(2,9)

      !  P = 95.58 mb
      refrat_planck_b = chi_mls(1,13)/chi_mls(2,13)

      !  P = 706.270mb
      refrat_m_a = chi_mls(1,3)/chi_mls(2,3)

      !  P = 95.58 mb 
      refrat_m_b = chi_mls(1,13)/chi_mls(2,13)

      ! Compute the optical depth by interpolating in ln(pressure) and 
      ! temperature, and appropriate species.  Below laytrop, the water vapor 
      ! self-continuum and foreign continuum is interpolated (in temperature) 
      ! separately.

      ! Lower atmosphere loop
      DO lay = 1, laytrop

        speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
        specparm = colh2o(lay)/speccomb
        IF (specparm .GE. oneminus) specparm = oneminus
        specmult = 8._wp*(specparm)
        js = 1 + INT(specmult)
        fs = MOD(specmult,1.0_wp)

        speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
        specparm1 = colh2o(lay)/speccomb1
        IF (specparm1 .GE. oneminus) specparm1 = oneminus
        specmult1 = 8._wp*(specparm1)
        js1 = 1 + INT(specmult1)
        fs1 = MOD(specmult1,1.0_wp)

        speccomb_mn2o = colh2o(lay) + refrat_m_a*colco2(lay)
        specparm_mn2o = colh2o(lay)/speccomb_mn2o
        IF (specparm_mn2o .GE. oneminus) specparm_mn2o = oneminus
        specmult_mn2o = 8._wp*specparm_mn2o
        jmn2o = 1 + INT(specmult_mn2o)
        fmn2o = MOD(specmult_mn2o,1.0_wp)
        fmn2omf = minorfrac(lay)*fmn2o
        !  In atmospheres where the amount of N2O is too great to be considered
        !  a minor species, adjust the column amount of N2O by an empirical factor 
        !  to obtain the proper contribution.
        chi_n2o = coln2o(lay)/coldry(lay)
        ratn2o = 1.e20_wp*chi_n2o/chi_mls(4,jp(lay)+1)
        IF (ratn2o .GT. 1.5_wp) THEN
          adjfac = 0.5_wp+(ratn2o-0.5_wp)**0.65_wp
          adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_wp
        ELSE
          adjcoln2o = coln2o(lay)
        ENDIF

        speccomb_planck = colh2o(lay)+refrat_planck_a*colco2(lay)
        specparm_planck = colh2o(lay)/speccomb_planck
        IF (specparm_planck .GE. oneminus) specparm_planck=oneminus
        specmult_planck = 8._wp*specparm_planck
        jpl= 1 + INT(specmult_planck)
        fpl = MOD(specmult_planck,1.0_wp)

        ind0 = ((jp(lay)-1)*5+(jt(lay)-1))*nspa(3) + js
        ind1 = (jp(lay)*5+(jt1(lay)-1))*nspa(3) + js1
        inds = indself(lay)
        indf = indfor(lay)
        indm = indminor(lay)

        IF (specparm .LT. 0.125_wp) THEN
          p = fs - 1
          p4 = p**4
          fk0 = p4
          fk1 = 1 - p - 2.0_wp*p4
          fk2 = p + p4
          fac000 = fk0*fac00(lay)
          fac100 = fk1*fac00(lay)
          fac200 = fk2*fac00(lay)
          fac010 = fk0*fac10(lay)
          fac110 = fk1*fac10(lay)
          fac210 = fk2*fac10(lay)
        ELSE IF (specparm .GT. 0.875_wp) THEN
          p = -fs
          p4 = p**4
          fk0 = p4
          fk1 = 1 - p - 2.0_wp*p4
          fk2 = p + p4
          fac000 = fk0*fac00(lay)
          fac100 = fk1*fac00(lay)
          fac200 = fk2*fac00(lay)
          fac010 = fk0*fac10(lay)
          fac110 = fk1*fac10(lay)
          fac210 = fk2*fac10(lay)
        ELSE
          fac000 = (1._wp - fs) * fac00(lay)
          fac010 = (1._wp - fs) * fac10(lay)
          fac100 = fs * fac00(lay)
          fac110 = fs * fac10(lay)
        ENDIF
        IF (specparm1 .LT. 0.125_wp) THEN
          p = fs1 - 1
          p4 = p**4
          fk0 = p4
          fk1 = 1 - p - 2.0_wp*p4
          fk2 = p + p4
          fac001 = fk0*fac01(lay)
          fac101 = fk1*fac01(lay)
          fac201 = fk2*fac01(lay)
          fac011 = fk0*fac11(lay)
          fac111 = fk1*fac11(lay)
          fac211 = fk2*fac11(lay)
        ELSE IF (specparm1 .GT. 0.875_wp) THEN
          p = -fs1
          p4 = p**4
          fk0 = p4
          fk1 = 1 - p - 2.0_wp*p4
          fk2 = p + p4
          fac001 = fk0*fac01(lay)
          fac101 = fk1*fac01(lay)
          fac201 = fk2*fac01(lay)
          fac011 = fk0*fac11(lay)
          fac111 = fk1*fac11(lay)
          fac211 = fk2*fac11(lay)
        ELSE
          fac001 = (1._wp - fs1) * fac01(lay)
          fac011 = (1._wp - fs1) * fac11(lay)
          fac101 = fs1 * fac01(lay)
          fac111 = fs1 * fac11(lay)
        ENDIF

        tauself = selffac(lay)* (selfref(inds,ig) + selffrac(lay) * &
             (selfref(inds+1,ig) - selfref(inds,ig)))
        taufor = forfac(lay) * (forref(indf,ig) + forfrac(lay) * &
             (forref(indf+1,ig) - forref(indf,ig)))
        n2om1 = ka_mn2o(jmn2o,indm,ig) + fmn2o * &
             (ka_mn2o(jmn2o+1,indm,ig) - ka_mn2o(jmn2o,indm,ig))
        n2om2 = ka_mn2o(jmn2o,indm+1,ig) + fmn2o * &
             (ka_mn2o(jmn2o+1,indm+1,ig) - ka_mn2o(jmn2o,indm+1,ig))
        absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)

        IF (specparm .LT. 0.125_wp) THEN
          tau_major = speccomb * &
               (fac000 * absa(ind0,ig) + &
               fac100 * absa(ind0+1,ig) + &
               fac200 * absa(ind0+2,ig) + &
               fac010 * absa(ind0+9,ig) + &
               fac110 * absa(ind0+10,ig) + &
               fac210 * absa(ind0+11,ig))
        ELSE IF (specparm .GT. 0.875_wp) THEN
          tau_major = speccomb * &
               (fac200 * absa(ind0-1,ig) + &
               fac100 * absa(ind0,ig) + &
               fac000 * absa(ind0+1,ig) + &
               fac210 * absa(ind0+8,ig) + &
               fac110 * absa(ind0+9,ig) + &
               fac010 * absa(ind0+10,ig))
        ELSE
          tau_major = speccomb * &
               (fac000 * absa(ind0,ig) + &
               fac100 * absa(ind0+1,ig) + &
               fac010 * absa(ind0+9,ig) + &
               fac110 * absa(ind0+10,ig))
        ENDIF

        IF (specparm1 .LT. 0.125_wp) THEN
          tau_major1 = speccomb1 * &
               (fac001 * absa(ind1,ig) + &
               fac101 * absa(ind1+1,ig) + &
               fac201 * absa(ind1+2,ig) + &
               fac011 * absa(ind1+9,ig) + &
               fac111 * absa(ind1+10,ig) + &
               fac211 * absa(ind1+11,ig))
        ELSE IF (specparm1 .GT. 0.875_wp) THEN
          tau_major1 = speccomb1 * &
               (fac201 * absa(ind1-1,ig) + &
               fac101 * absa(ind1,ig) + &
               fac001 * absa(ind1+1,ig) + &
               fac211 * absa(ind1+8,ig) + &
               fac111 * absa(ind1+9,ig) + &
               fac011 * absa(ind1+10,ig))
        ELSE
          tau_major1 = speccomb1 * &
               (fac001 * absa(ind1,ig) +  &
               fac101 * absa(ind1+1,ig) + &
               fac011 * absa(ind1+9,ig) + &
               fac111 * absa(ind1+10,ig))
        ENDIF

        taug(lay) = tau_major + tau_major1 &
             + tauself + taufor &
             + adjcoln2o*absn2o
        fracs(lay) = fracrefa(ig,jpl) + fpl * &
             (fracrefa(ig,jpl+1)-fracrefa(ig,jpl))
      ENDDO

      ! Upper atmosphere loop
      DO lay = laytrop+1, nlayers

        speccomb = colh2o(lay) + rat_h2oco2(lay)*colco2(lay)
        specparm = colh2o(lay)/speccomb
        IF (specparm .GE. oneminus) specparm = oneminus
        specmult = 4._wp*(specparm)
        js = 1 + INT(specmult)
        fs = MOD(specmult,1.0_wp)

        speccomb1 = colh2o(lay) + rat_h2oco2_1(lay)*colco2(lay)
        specparm1 = colh2o(lay)/speccomb1
        IF (specparm1 .GE. oneminus) specparm1 = oneminus
        specmult1 = 4._wp*(specparm1)
        js1 = 1 + INT(specmult1)
        fs1 = MOD(specmult1,1.0_wp)

        fac000 = (1._wp - fs) * fac00(lay)
        fac010 = (1._wp - fs) * fac10(lay)
        fac100 = fs * fac00(lay)
        fac110 = fs * fac10(lay)
        fac001 = (1._wp - fs1) * fac01(lay)
        fac011 = (1._wp - fs1) * fac11(lay)
        fac101 = fs1 * fac01(lay)
        fac111 = fs1 * fac11(lay)

        speccomb_mn2o = colh2o(lay) + refrat_m_b*colco2(lay)
        specparm_mn2o = colh2o(lay)/speccomb_mn2o
        IF (specparm_mn2o .GE. oneminus) specparm_mn2o = oneminus
        specmult_mn2o = 4._wp*specparm_mn2o
        jmn2o = 1 + INT(specmult_mn2o)
        fmn2o = MOD(specmult_mn2o,1.0_wp)
        fmn2omf = minorfrac(lay)*fmn2o
        !  In atmospheres where the amount of N2O is too great to be considered
        !  a minor species, adjust the column amount of N2O by an empirical factor 
        !  to obtain the proper contribution.
        chi_n2o = coln2o(lay)/coldry(lay)
        ratn2o = 1.e20*chi_n2o/chi_mls(4,jp(lay)+1)
        IF (ratn2o .GT. 1.5_wp) THEN
          adjfac = 0.5_wp+(ratn2o-0.5_wp)**0.65_wp
          adjcoln2o = adjfac*chi_mls(4,jp(lay)+1)*coldry(lay)*1.e-20_wp
        ELSE
          adjcoln2o = coln2o(lay)
        ENDIF
        speccomb_planck = colh2o(lay)+refrat_planck_b*colco2(lay)
        specparm_planck = colh2o(lay)/speccomb_planck
        IF (specparm_planck .GE. oneminus) specparm_planck=oneminus
        specmult_planck = 4._wp*specparm_planck
        jpl= 1 + INT(specmult_planck)
        fpl = MOD(specmult_planck,1.0_wp)

        ind0 = ((jp(lay)-13)*5+(jt(lay)-1))*nspb(3) + js
        ind1 = ((jp(lay)-12)*5+(jt1(lay)-1))*nspb(3) + js1
        indf = indfor(lay)
        indm = indminor(lay)

        taufor = forfac(lay) * (forref(indf,ig) + &
             forfrac(lay) * (forref(indf+1,ig) - forref(indf,ig)))
        n2om1 = kb_mn2o(jmn2o,indm,ig) + fmn2o * &
             (kb_mn2o(jmn2o+1,indm,ig)-kb_mn2o(jmn2o,indm,ig))
        n2om2 = kb_mn2o(jmn2o,indm+1,ig) + fmn2o * &
             (kb_mn2o(jmn2o+1,indm+1,ig)-kb_mn2o(jmn2o,indm+1,ig))
        absn2o = n2om1 + minorfrac(lay) * (n2om2 - n2om1)
        taug(lay) = speccomb * &
             (fac000 * absb(ind0,ig) + &
             fac100 * absb(ind0+1,ig) + &
             fac010 * absb(ind0+5,ig) + &
             fac110 * absb(ind0+6,ig)) &
             + speccomb1 * &
             (fac001 * absb(ind1,ig) +  &
             fac101 * absb(ind1+1,ig) + &
             fac011 * absb(ind1+5,ig) + &
             fac111 * absb(ind1+6,ig))  &
             + taufor &
             + adjcoln2o*absn2o
        fracs(lay) = fracrefb(ig,jpl) + fpl * &
             (fracrefb(ig,jpl+1)-fracrefb(ig,jpl))
      ENDDO

    END SUBROUTINE taumol03

    END SUBROUTINE gas_optics_lw

    END MODULE mo_lrtm_gas_optics
