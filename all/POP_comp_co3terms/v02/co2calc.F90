
! KGEN-generated Fortran source file
!
! Filename    : co2calc.F90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11



    MODULE co2calc
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !-----------------------------------------------------------------------------
        !   based upon OCMIP2 co2calc
        !
        !   CVS:$Id: co2calc.F90 941 2006-05-12 21:36:48Z klindsay $
        !   CVS:$Name$
        !-----------------------------------------------------------------------------
        USE constants, only : c0
        USE constants, only : rho_sw
        USE constants, only : c1
        USE constants, only : t0_kelvin
        USE constants, only : c1000
        USE constants, only : c10
        USE constants, only : p001
        USE constants, only : p5
        USE constants, only : c3
        USE constants, only : c2
        USE blocks, ONLY: nx_block
        USE blocks, ONLY: block
        USE blocks, ONLY: get_block
        USE domain, ONLY: blocks_clinic
        USE domain_size, ONLY: max_blocks_clinic
        USE kinds_mod, only : r8
        USE kinds_mod, only : int_kind
        USE kinds_mod, only : log_kind
        USE state_mod, ONLY: ref_pressure
        USE time_management, ONLY: nsteps_run
        !*** ccsm
        USE shr_vmath_mod, only : shr_vmath_log
        USE shr_vmath_mod, only : shr_vmath_sqrt
        USE shr_vmath_mod, only : shr_vmath_exp
        IMPLICIT NONE
        !-----------------------------------------------------------------------------
        !   public/private declarations
        !-----------------------------------------------------------------------------
        PRIVATE
        PUBLIC comp_co3terms
        !-----------------------------------------------------------------------------
        !   module parameters
        !-----------------------------------------------------------------------------
        !-----------------------------------------------------------------------------
        !   The current setting of xacc, a tolerance critera, will result in co2star
        !   being accurate to 3 significant figures (xx.y). Making xacc bigger will
        !   result in faster convergence also, but this is not recommended (xacc of
        !   10**-9 drops precision to 2 significant figures).
        !-----------------------------------------------------------------------------
        REAL(KIND=r8), parameter :: xacc = 1e-10_r8
        INTEGER(KIND=int_kind), parameter :: max_bracket_grow_it = 3
        INTEGER(KIND=int_kind), parameter :: maxit = 100
        REAL(KIND=r8), parameter :: salt_min = 0.1_r8
        REAL(KIND=r8), parameter :: dic_min  = salt_min / 35.0_r8 * 1944.0_r8
        REAL(KIND=r8), parameter :: alk_min  = salt_min / 35.0_r8 * 2225.0_r8
        !-----------------------------------------------------------------------------
        !   declarations for function coefficients & species concentrations
        !-----------------------------------------------------------------------------
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: kb
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: k1p
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: k2p
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: k3p
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: ksi
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: kw
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: ks
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: kf
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: bt
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: st
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: ft
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: dic
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: ta
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: pt
        REAL(KIND=r8), dimension(nx_block,max_blocks_clinic) :: sit
        !*****************************************************************************
            PUBLIC kgen_read_externs_co2calc
        CONTAINS

        ! write subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_co2calc(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) kb
            READ(UNIT=kgen_unit) k1p
            READ(UNIT=kgen_unit) k2p
            READ(UNIT=kgen_unit) k3p
            READ(UNIT=kgen_unit) ksi
            READ(UNIT=kgen_unit) kw
            READ(UNIT=kgen_unit) ks
            READ(UNIT=kgen_unit) kf
            READ(UNIT=kgen_unit) bt
            READ(UNIT=kgen_unit) st
            READ(UNIT=kgen_unit) ft
            READ(UNIT=kgen_unit) dic
            READ(UNIT=kgen_unit) ta
            READ(UNIT=kgen_unit) pt
            READ(UNIT=kgen_unit) sit
        END SUBROUTINE kgen_read_externs_co2calc

        !*****************************************************************************

        !*****************************************************************************

        SUBROUTINE comp_co3terms(iblock, j, k, mask, lcomp_co3_coeffs, temp, salt, dic_in, ta_in, pt_in, sit_in, phlo, phhi, ph, &
        h2co3, hco3, co3)
            !---------------------------------------------------------------------------
            !   SUBROUTINE comp_CO3terms
            !
            !   PURPOSE : Calculate H2CO3, HCO3, CO3 from
            !             total alkalinity, total CO2, temp, salinity (s), etc.
            !---------------------------------------------------------------------------
            !---------------------------------------------------------------------------
            !   input arguments
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind), intent(in) :: iblock
            INTEGER(KIND=int_kind), intent(in) :: j
            INTEGER(KIND=int_kind), intent(in) :: k
            LOGICAL(KIND=log_kind), dimension(nx_block), intent(in) :: mask
            LOGICAL(KIND=log_kind), intent(in) :: lcomp_co3_coeffs
            REAL(KIND=r8), dimension(nx_block), intent(in) :: salt
            REAL(KIND=r8), dimension(nx_block), intent(in) :: temp
            REAL(KIND=r8), dimension(nx_block), intent(in) :: pt_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: sit_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: ta_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: dic_in
            ! temperature (degrees C)
            ! salinity (PSU)
            ! total inorganic carbon (nmol/cm^3)
            ! total alkalinity (neq/cm^3)
            ! inorganic phosphate (nmol/cm^3)
            ! inorganic silicate (nmol/cm^3)
            !---------------------------------------------------------------------------
            !   input/output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(inout) :: phhi
            REAL(KIND=r8), dimension(nx_block), intent(inout) :: phlo
            ! lower limit of pH range
            ! upper limit of pH range
            !---------------------------------------------------------------------------
            !   output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(out) :: h2co3
            REAL(KIND=r8), dimension(nx_block), intent(out) :: ph
            REAL(KIND=r8), dimension(nx_block), intent(out) :: co3
            REAL(KIND=r8), dimension(nx_block), intent(out) :: hco3
            ! computed ph values, for initial guess on next time step
            ! Carbonic Acid Concentration
            ! Bicarbonate Ion Concentration
            ! Carbonate Ion Concentration
            !---------------------------------------------------------------------------
            !   local variable declarations
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind) :: i
            REAL(KIND=r8) :: mass_to_vol
            REAL(KIND=r8) :: vol_to_mass
            REAL(KIND=r8) :: htotal2
            REAL(KIND=r8) :: denom
            ! (mol/kg) -> (mmol/m^3)
            ! (mmol/m^3) -> (mol/kg)
            REAL(KIND=r8), dimension(nx_block) :: k2
            REAL(KIND=r8), dimension(nx_block) :: ff
            REAL(KIND=r8), dimension(nx_block) :: k0
            REAL(KIND=r8), dimension(nx_block) :: k1
            REAL(KIND=r8), dimension(nx_block) :: htotal
            ! free concentration of H ion
            ! equilibrium constants for CO2 species
            ! fugacity of CO2
            !---------------------------------------------------------------------------
            !   check for existence of ocean points
            !---------------------------------------------------------------------------
    IF (COUNT(mask) == 0) THEN
       ph         = c0
       H2CO3      = c0
       HCO3       = c0
       CO3        = c0
       RETURN
    END IF
            !---------------------------------------------------------------------------
            !   set unit conversion factors
            !---------------------------------------------------------------------------
    mass_to_vol = 1e6_r8 * rho_sw
    vol_to_mass = c1 / mass_to_vol
            !------------------------------------------------------------------------
            !   compute thermodynamic CO3 coefficients
            !------------------------------------------------------------------------
    IF (lcomp_co3_coeffs) THEN
       CALL comp_co3_coeffs(iblock, k, mask, temp, salt, k0, k1, k2, ff, k1_k2_pH_tot=.true.)
    END IF
            !------------------------------------------------------------------------
            !   compute htotal
            !------------------------------------------------------------------------
    CALL comp_htotal(iblock, j, k, mask, temp, dic_in, &
                     ta_in, pt_in, sit_in, k1, k2, &
                     phlo, phhi, htotal)
            !------------------------------------------------------------------------
            !   Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
            !   ORNL/CDIAC-74, Dickson and Goyet, eds. (Ch 2 p 10, Eq A.49-51)
            !------------------------------------------------------------------------
    DO i = 1,nx_block
       IF (mask(i)) THEN
          htotal2  = htotal(i) ** 2
          denom    = c1 / (htotal2 + k1(i) * htotal(i) + k1(i) * k2(i))
          H2CO3(i) = dic(i,iblock) * htotal2 * denom
          HCO3(i)  = dic(i,iblock) * k1(i) * htotal(i) * denom
          CO3(i)   = dic(i,iblock) * k1(i) * k2(i) * denom
          ph(i)    = -LOG10(htotal(i))
                    !------------------------------------------------------------------
                    !   Convert units of output arguments
                    !------------------------------------------------------------------
          H2CO3(i) = H2CO3(i) * mass_to_vol
          HCO3(i)  = HCO3(i) * mass_to_vol
          CO3(i)   = CO3(i) * mass_to_vol
       ELSE ! if mask ! if mask
          ph(i)    = c0
          H2CO3(i) = c0
          HCO3(i)  = c0
          CO3(i)   = c0
       END IF ! if mask ! if mask
    END DO ! i loop ! i loop
        END SUBROUTINE comp_co3terms
        !*****************************************************************************

        SUBROUTINE comp_co3_coeffs(iblock, k, mask, temp, salt, k0, k1, k2, ff, k1_k2_ph_tot)
            !---------------------------------------------------------------------------
            !   input arguments
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind), intent(in) :: iblock
            INTEGER(KIND=int_kind), intent(in) :: k
            LOGICAL(KIND=log_kind), dimension(nx_block), intent(in) :: mask
            REAL(KIND=r8), dimension(nx_block), intent(in) :: temp
            REAL(KIND=r8), dimension(nx_block), intent(in) :: salt
            ! temperature (degrees C)
            ! salinity (PSU)
            LOGICAL(KIND=log_kind), intent(in) :: k1_k2_ph_tot
            !---------------------------------------------------------------------------
            !   output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(out) :: k2
            REAL(KIND=r8), dimension(nx_block), intent(out) :: ff
            REAL(KIND=r8), dimension(nx_block), intent(out) :: k0
            REAL(KIND=r8), dimension(nx_block), intent(out) :: k1
            ! equilibrium constants for CO2 species
            ! fugacity of CO2
            !---------------------------------------------------------------------------
            !   local variable declarations
            !---------------------------------------------------------------------------
            REAL(KIND=r8) :: press_bar
            ! pressure at level k [bars]
            REAL(KIND=r8), dimension(nx_block) :: salt_lim
            REAL(KIND=r8), dimension(nx_block) :: tk
            REAL(KIND=r8), dimension(nx_block) :: tk100
            REAL(KIND=r8), dimension(nx_block) :: tk1002
            REAL(KIND=r8), dimension(nx_block) :: invtk
            REAL(KIND=r8), dimension(nx_block) :: dlogtk
            REAL(KIND=r8), dimension(nx_block) :: invrtk
            REAL(KIND=r8), dimension(nx_block) :: is
            REAL(KIND=r8), dimension(nx_block) :: is2
            REAL(KIND=r8), dimension(nx_block) :: sqrtis
            REAL(KIND=r8), dimension(nx_block) :: sqrts
            REAL(KIND=r8), dimension(nx_block) :: s2
            REAL(KIND=r8), dimension(nx_block) :: scl
            REAL(KIND=r8), dimension(nx_block) :: arg
            REAL(KIND=r8), dimension(nx_block) :: log_1_m_1p005em3_s
            REAL(KIND=r8), dimension(nx_block) :: deltav
            REAL(KIND=r8), dimension(nx_block) :: kappa
            REAL(KIND=r8), dimension(nx_block) :: lnkfac
            REAL(KIND=r8), dimension(nx_block) :: kfac
            REAL(KIND=r8), dimension(nx_block) :: log_1_p_tot_sulfate_div_ks
            ! bounded salt
            ! temperature (K)
            ! ionic strength
            ! chlorinity
            ! pressure correction terms
            !---------------------------------------------------------------------------
    press_bar = ref_pressure(k)
            !---------------------------------------------------------------------------
            !   Calculate all constants needed to convert between various
            !   measured carbon species. References for each equation are
            !   noted in the code.  Once calculated, the constants are stored
            !   and passed in the common block "const". The original version
            !   of this code was based on the code by Dickson in Version 2 of
            !   "Handbook of Methods for the Analysis of the Various Parameters
            !   of the Carbon Dioxide System in Seawater", DOE, 1994 (SOP No. 3,
            !   p25-26).
            !   Derive simple terms used more than once
            !---------------------------------------------------------------------------
    salt_lim = max(salt,salt_min)
    tk       = T0_Kelvin + temp
    tk100    = tk * 1e-2_r8
    tk1002   = tk100 * tk100
    invtk    = c1 / tk
    CALL shr_vmath_log(tk, dlogtk, nx_block)
    invRtk   = (c1 / 83.1451_r8) * invtk
    is       = 19.924_r8 * salt_lim / (c1000 - 1.005_r8 * salt_lim)
    is2      = is * is
    CALL shr_vmath_sqrt(is, sqrtis, nx_block)
    CALL shr_vmath_sqrt(salt_lim, sqrts, nx_block)
    s2       = salt_lim * salt_lim
    scl      = salt_lim / 1.80655_r8
    arg = c1 - 0.001005_r8 * salt_lim
    CALL shr_vmath_log(arg, log_1_m_1p005em3_s, nx_block)
            !---------------------------------------------------------------------------
            !   f = k0(1-pH2O)*correction term for non-ideality
            !   Weiss & Price (1980, Mar. Chem., 8, 347-359;
            !                 Eq 13 with table 6 values)
            !---------------------------------------------------------------------------
    arg = -162.8301_r8 + 218.2968_r8 / tk100 + &
          90.9241_r8 * (dlogtk + LOG(1e-2_r8)) - 1.47696_r8 * tk1002 + &
          salt_lim * (.025695_r8 - .025225_r8 * tk100 + 0.0049867_r8 * tk1002)
    CALL shr_vmath_exp(arg, ff, nx_block)
            !---------------------------------------------------------------------------
            !   K0 from Weiss 1974
            !---------------------------------------------------------------------------
    arg = 93.4517_r8 / tk100 - 60.2409_r8 + 23.3585_r8 * (dlogtk + LOG(1e-2_r8)) + &
          salt_lim * (.023517_r8 - 0.023656_r8 * tk100 + 0.0047036_r8 * tk1002)
    CALL shr_vmath_exp(arg, k0, nx_block)
            !---------------------------------------------------------------------------
            !   k1 = [H][HCO3]/[H2CO3]
            !   k2 = [H][CO3]/[HCO3]
            !   if k1_k2_pH_tot == .true., then use
            !      Lueker, Dickson, Keeling (2000) using Mehrbach et al. data on total scale
            !   otherwise, use
            !      Millero p.664 (1995) using Mehrbach et al. data on seawater scale
            !      this is only present to be consistent w/ OCMIP2 code
            !      it should not be used for new runs
            !      the only reason to use it is to be compatible with prior
            !      long spun up runs that had used it
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !---------------------------------------------------------------------------
    IF (k1_k2_pH_tot) THEN
                ! total pH scale
       arg = 3633.86_r8 * invtk - 61.2172_r8 + &
             9.67770_r8 * dlogtk - 0.011555_r8 * salt_lim + &
             0.0001152_r8 * s2
    ELSE
                ! seawater pH scale, see comment above
       arg = 3670.7_r8 * invtk - 62.008_r8 + &
             9.7944_r8 * dlogtk - 0.0118_r8 * salt_lim + &
             0.000116_r8 * s2
    END IF
    arg = -LOG(c10) * arg
    CALL shr_vmath_exp(arg, k1, nx_block)
    IF (k > 1) THEN
       deltaV = -25.5_r8 + 0.1271_r8 * temp
       Kappa  = (-3.08_r8 + 0.0877_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       k1 = k1 * Kfac
    END IF
    IF (k1_k2_pH_tot) THEN
                ! total pH scale
       arg = 471.78_r8 * invtk + 25.9290_r8 - &
             3.16967_r8 * dlogtk - 0.01781_r8 * salt_lim + 0.0001122_r8 * s2
    ELSE
                ! seawater pH scale, see comment above
       arg = 1394.7_r8 * invtk + 4.777_r8 - &
             0.0184_r8 * salt_lim + 0.000118_r8 * s2
    END IF
    arg = -LOG(c10) * arg
    CALL shr_vmath_exp(arg, k2, nx_block)
    IF (k > 1) THEN
       deltaV = -15.82_r8 - 0.0219_r8 * temp
       Kappa  = (1.13_r8 - 0.1475_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       k2 = k2 * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   kb = [H][BO2]/[HBO2]
            !   Millero p.669 (1995) using data from Dickson (1990)
            !   CO2SYS states that this in on total pH scale
            !   pressure correction from Millero 1979, p. 1657
            !      omitting salinity contribution
            !---------------------------------------------------------------------------
    arg = (-8966.90_r8 - 2890.53_r8 * sqrts - &
           77.942_r8 * salt_lim + 1.728_r8 * salt_lim * sqrts - &
           0.0996_r8 * s2) * invtk + &
          (148.0248_r8 + 137.1942_r8 * sqrts + 1.62142_r8 * salt_lim) + &
          (-24.4344_r8 - 25.085_r8 * sqrts - 0.2474_r8 * salt_lim) * dlogtk + &
          0.053105_r8 * sqrts * tk
    CALL shr_vmath_exp(arg, kb(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -29.48_r8 + (0.1622_r8 - 0.002608_r8 * temp) * temp
       Kappa  = -2.84_r8 * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       kb(:,iblock) = kb(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   k1p = [H][H2PO4]/[H3PO4]
            !   DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !---------------------------------------------------------------------------
    arg = -4576.752_r8 * invtk + 115.525_r8 - &
          18.453_r8 * dlogtk + &
          (-106.736_r8 * invtk + 0.69171_r8) * sqrts + &
          (-0.65643_r8 * invtk - 0.01844_r8) * salt_lim
    CALL shr_vmath_exp(arg, k1p(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -14.51_r8 + (0.1211_r8 - 0.000321_r8 * temp) * temp
       Kappa  = (-2.67_r8 + 0.0427_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       k1p(:,iblock) = k1p(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   k2p = [H][HPO4]/[H2PO4]
            !   DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !---------------------------------------------------------------------------
    arg = -8814.715_r8 * invtk + 172.0883_r8 - &
          27.927_r8 * dlogtk + &
          (-160.340_r8 * invtk + 1.3566_r8) * sqrts + &
          (0.37335_r8 * invtk - 0.05778_r8) * salt_lim
    CALL shr_vmath_exp(arg, k2p(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -23.12_r8 + (0.1758_r8 - 0.002647_r8 * temp) * temp
       Kappa  = (-5.15_r8 + 0.09_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       k2p(:,iblock) = k2p(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   k3p = [H][PO4]/[HPO4]
            !   DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !---------------------------------------------------------------------------
    arg = -3070.75_r8 * invtk - 18.141_r8 + &
          (17.27039_r8 * invtk + 2.81197_r8) * sqrts + &
          (-44.99486_r8 * invtk - 0.09984_r8) * salt_lim
    CALL shr_vmath_exp(arg, k3p(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -26.57_r8 + (0.202_r8 - 0.003042_r8 * temp) * temp
       Kappa  = (-4.08_r8 + 0.0714_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       k3p(:,iblock) = k3p(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   ksi = [H][SiO(OH)3]/[Si(OH)4]
            !   Millero p.671 (1995) using data from Yao and Millero (1995)
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !      apply boric acid values
            !---------------------------------------------------------------------------
    arg = -8904.2_r8 * invtk + 117.385_r8 - &
          19.334_r8 * dlogtk + &
          (-458.79_r8 * invtk + 3.5913_r8) * sqrtis + &
          (188.74_r8 * invtk - 1.5998_r8) * is + &
          (-12.1652_r8 * invtk + 0.07871_r8) * is2 + &
          log_1_m_1p005em3_s
    CALL shr_vmath_exp(arg, ksi(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -29.48_r8 + (0.1622_r8 - 0.002608_r8 * temp) * temp
       Kappa  = -2.84_r8 * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       ksi(:,iblock) = ksi(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   kw = [H][OH]
            !   Millero p.670 (1995) using composite data
            !   following DOE Handbook, 0.015 substracted from constant to
            !   approximately convert from SWS pH scale to total pH scale
            !   pressure correction from Millero 1983
            !      note that deltaV coeffs in Millero 1995 are those actually
            !      freshwater deltaV coeffs from Millero 1983
            !---------------------------------------------------------------------------
    arg = -13847.26_r8 * invtk + 148.9652_r8 - 23.6521_r8 * dlogtk + &
          (118.67_r8 * invtk - 5.977_r8 + 1.0495_r8 * dlogtk) * sqrts - &
          0.01615_r8 * salt_lim
    CALL shr_vmath_exp(arg, kw(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -20.02_r8 + (0.1119_r8 - 0.001409_r8 * temp) * temp
       Kappa  = (-5.13_r8 + 0.0794_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       kw(:,iblock) = kw(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------------
            !   ks = [H][SO4]/[HSO4], free pH scale
            !   Dickson (1990, J. chem. Thermodynamics 22, 113)
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !---------------------------------------------------------------------------
    arg = -4276.1_r8 * invtk + 141.328_r8 - 23.093_r8 * dlogtk + &
          (-13856.0_r8 * invtk + 324.57_r8 - 47.986_r8 * dlogtk) * sqrtis + &
          (35474.0_r8 * invtk - 771.54_r8 + 114.723_r8 * dlogtk) * is - &
          2698.0_r8 * invtk * is * sqrtis + &
          1776.0_r8 * invtk * is2 + &
          log_1_m_1p005em3_s
    CALL shr_vmath_exp(arg, ks(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -18.03_r8 + (0.0466_r8 + 0.000316_r8 * temp) * temp
       Kappa  = (-4.53_r8 + 0.09_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       ks(:,iblock) = ks(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------
            !   kf = [H][F]/[HF]
            !   Dickson and Riley (1979) -- change pH scale to total
            !   pressure correction from Millero 1995, p. 675
            !      w/ typo corrections from CO2SYS
            !---------------------------------------------------------------------
    arg = c1 + (0.1400_r8 / 96.062_r8) * (scl) / ks(:,iblock)
       CALL shr_vmath_log(arg, log_1_p_tot_sulfate_div_ks, nx_block)
    arg = 1590.2_r8 * invtk - 12.641_r8 + 1.525_r8 * sqrtis + &
          log_1_m_1p005em3_s + log_1_p_tot_sulfate_div_ks
    CALL shr_vmath_exp(arg, kf(:,iblock), nx_block)
    IF (k > 1) THEN
       deltaV = -9.78_r8 - (0.009_r8 + 0.000942_r8 * temp) * temp
       Kappa  = (-3.91_r8 + 0.054_r8 * temp) * p001
       lnKfac = (-deltaV + p5 * Kappa * press_bar) * press_bar * invRtk
       CALL shr_vmath_exp(lnKfac, Kfac, nx_block)
       kf(:,iblock) = kf(:,iblock) * Kfac
    END IF
            !---------------------------------------------------------------------
            !   Calculate concentrations for borate, sulfate, and fluoride
            !   bt : Uppstrom (1974)
            !   st : Morris & Riley (1966)
            !   ft : Riley (1965)
            !---------------------------------------------------------------------
    bt(:,iblock) = 0.000232_r8 / 10.811_r8 * scl
    st(:,iblock) = 0.14_r8 / 96.062_r8 * scl
    ft(:,iblock) = 0.000067_r8 / 18.9984_r8 * scl
        END SUBROUTINE comp_co3_coeffs
        !*****************************************************************************

        SUBROUTINE comp_htotal(iblock, j, k, mask, temp, dic_in, ta_in, pt_in, sit_in, k1, k2, phlo, phhi, htotal)
            !---------------------------------------------------------------------------
            !   SUBROUTINE comp_htotal
            !
            !   PURPOSE : Calculate htotal from total alkalinity, total CO2,
            !             temp, salinity (s), etc.
            !---------------------------------------------------------------------------
            !---------------------------------------------------------------------------
            !   input arguments
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind), intent(in) :: j
            INTEGER(KIND=int_kind), intent(in) :: iblock
            INTEGER(KIND=int_kind), intent(in) :: k
            LOGICAL(KIND=log_kind), dimension(nx_block), intent(in) :: mask
            REAL(KIND=r8), dimension(nx_block), intent(in) :: k2
            REAL(KIND=r8), dimension(nx_block), intent(in) :: pt_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: temp
            REAL(KIND=r8), dimension(nx_block), intent(in) :: dic_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: sit_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: ta_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: k1
            ! temperature (degrees C)
            ! total inorganic carbon (nmol/cm^3)
            ! total alkalinity (neq/cm^3)
            ! inorganic phosphate (nmol/cm^3)
            ! inorganic silicate (nmol/cm^3)
            ! equilibrium constants for CO2 species
            !---------------------------------------------------------------------------
            !   input/output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(inout) :: phlo
            REAL(KIND=r8), dimension(nx_block), intent(inout) :: phhi
            ! lower limit of pH range
            ! upper limit of pH range
            !---------------------------------------------------------------------------
            !   output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(out) :: htotal
            ! free concentration of H ion
            !---------------------------------------------------------------------------
            !   local variable declarations
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind) :: i
            REAL(KIND=r8) :: mass_to_vol
            REAL(KIND=r8) :: vol_to_mass
            ! (mol/kg) -> (mmol/m^3)
            ! (mmol/m^3) -> (mol/kg)
            REAL(KIND=r8), dimension(nx_block) :: x1
            REAL(KIND=r8), dimension(nx_block) :: x2
            ! bounds on htotal for solver
            !---------------------------------------------------------------------------
            !   check for existence of ocean points
            !---------------------------------------------------------------------------
    IF (COUNT(mask) == 0) THEN
       htotal = c0
       RETURN
    END IF
            !---------------------------------------------------------------------------
            !   set unit conversion factors
            !---------------------------------------------------------------------------
    mass_to_vol = 1e6_r8 * rho_sw
    vol_to_mass = c1 / mass_to_vol
            !---------------------------------------------------------------------------
            !   convert tracer units to per mass
            !---------------------------------------------------------------------------
    DO i = 1,nx_block
       IF (mask(i)) THEN
          dic(i,iblock)  = max(dic_in(i),dic_min) * vol_to_mass
          ta(i,iblock)   = max(ta_in(i),alk_min)  * vol_to_mass
          pt(i,iblock)   = max(pt_in(i),c0)       * vol_to_mass
          sit(i,iblock)  = max(sit_in(i),c0)      * vol_to_mass
          x1(i) = c10 ** (-phhi(i))
          x2(i) = c10 ** (-phlo(i))
       END IF ! if mask ! if mask
    END DO ! i loop ! i loop
            !---------------------------------------------------------------------------
            !   If DIC and TA are known then either a root finding or iterative
            !   method must be used to calculate htotal. In this case we use
            !   the Newton-Raphson "safe" method taken from "Numerical Recipes"
            !   (function "rtsafe.f" with error trapping removed).
            !
            !   As currently set, this procedure iterates about 12 times. The
            !   x1 and x2 values set below will accomodate ANY oceanographic
            !   values. If an initial guess of the pH is known, then the
            !   number of iterations can be reduced to about 5 by narrowing
            !   the gap between x1 and x2. It is recommended that the first
            !   few time steps be run with x1 and x2 set as below. After that,
            !   set x1 and x2 to the previous value of the pH +/- ~0.5.
            !---------------------------------------------------------------------------
    CALL drtsafe_row(iblock, j, k, mask, k1, k2, x1, x2, xacc, htotal)
        END SUBROUTINE comp_htotal
        !*****************************************************************************

        SUBROUTINE drtsafe_row(iblock, j, k, mask_in, k1, k2, x1, x2, xacc, soln)
            !---------------------------------------------------------------------------
            !   Vectorized version of drtsafe, which was a modified version of
            !      Numerical Recipes algorithm.
            !   Keith Lindsay, Oct 1999
            !
            !   Algorithm comment :
            !      Iteration from Newtons method is used unless it leaves
            !      bracketing interval or the dx is > 0.5 the previous dx.
            !      In that case, bisection method is used.
            !---------------------------------------------------------------------------
            !---------------------------------------------------------------------------
            !   input arguments
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind), intent(in) :: j
            INTEGER(KIND=int_kind), intent(in) :: iblock
            INTEGER(KIND=int_kind), intent(in) :: k
            LOGICAL(KIND=log_kind), dimension(nx_block), intent(in) :: mask_in
            REAL(KIND=r8), dimension(nx_block), intent(in) :: k1
            REAL(KIND=r8), dimension(nx_block), intent(in) :: k2
            REAL(KIND=r8), intent(in) :: xacc
            !---------------------------------------------------------------------------
            !   input/output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(inout) :: x2
            REAL(KIND=r8), dimension(nx_block), intent(inout) :: x1
            !---------------------------------------------------------------------------
            !   output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(out) :: soln
            !---------------------------------------------------------------------------
            !   local variable declarations
            !---------------------------------------------------------------------------
            LOGICAL(KIND=log_kind) :: leave_bracket
            LOGICAL(KIND=log_kind) :: dx_decrease
            LOGICAL(KIND=log_kind), dimension(nx_block) :: mask
            INTEGER(KIND=int_kind) :: it
            INTEGER(KIND=int_kind) :: i
            REAL(KIND=r8) :: temp
            REAL(KIND=r8), dimension(nx_block) :: df
            REAL(KIND=r8), dimension(nx_block) :: flo
            REAL(KIND=r8), dimension(nx_block) :: fhi
            REAL(KIND=r8), dimension(nx_block) :: dx
            REAL(KIND=r8), dimension(nx_block) :: xlo
            REAL(KIND=r8), dimension(nx_block) :: xhi
            REAL(KIND=r8), dimension(nx_block) :: dxold
            REAL(KIND=r8), dimension(nx_block) :: f
            TYPE(block) :: this_block
            !---------------------------------------------------------------------------
            !   bracket root at each location and set up first iteration
            !---------------------------------------------------------------------------
    mask = mask_in
    it = 0
    DO
       CALL talk_row(iblock, mask, k1, k2, x1, flo, df)
       CALL talk_row(iblock, mask, k1, k2, x2, fhi, df)
       WHERE ( mask )
          mask = (flo > c0 .AND. fhi > c0) .OR. &
                 (flo < c0 .AND. fhi < c0)
                END WHERE 
       IF (.NOT. ANY(mask)) EXIT
       it = it + 1
       DO i = 1,nx_block
          IF (mask(i)) THEN
             this_block = get_block(blocks_clinic(iblock), iblock)
!kgen_excluded              WRITE(stdout,*) '(co2calc.F90:drtsafe_row) ', &
                !'i_glob = ', this_block%i_glob(i), &
                !', j_glob = ', this_block%j_glob(j), ', k = ', k, &
                !', nsteps_run = ', nsteps_run, ', it = ', it
!kgen_excluded              WRITE(stdout,*) '(co2calc.F90:drtsafe_row) ', &
               ! '   x1,f = ', x1(i), flo(i)
!kgen_excluded              WRITE(stdout,*) '(co2calc.F90:drtsafe_row) ', &
               ! '   x2,f = ', x2(i), fhi(i)
          END IF
       END DO
       IF (it > max_bracket_grow_it) THEN
!kgen_excluded           CALL shr_sys_abort('bounding bracket for pH solution not found')
       END IF
       WHERE ( mask )
          dx = sqrt(x2 / x1)
          x2 = x2 * dx
          x1 = x1 / dx
                END WHERE 
    END DO
    mask = mask_in
    DO i = 1,nx_block
       IF (mask(i)) THEN
          IF (flo(i) .LT. c0) THEN
             xlo(i) = x1(i)
             xhi(i) = x2(i)
          ELSE
             xlo(i) = x2(i)
             xhi(i) = x1(i)
             temp = flo(i)
             flo(i) = fhi(i)
             fhi(i) = temp
          END IF
          soln(i) = p5 * (xlo(i) + xhi(i))
          dxold(i) = ABS(xlo(i) - xhi(i))
          dx(i) = dxold(i)
       END IF
    END DO
    CALL talk_row(iblock, mask, k1, k2, soln, f, df)
            !---------------------------------------------------------------------------
            !   perform iterations, zeroing mask when a location has converged
            !---------------------------------------------------------------------------
    DO it = 1,maxit
       DO i = 1,nx_block
          IF (mask(i)) THEN
             leave_bracket = ((soln(i) - xhi(i)) * df(i) - f(i)) * &
                  ((soln(i) - xlo(i)) * df(i) - f(i)) .GE. 0
             dx_decrease = ABS(c2 * f(i)) .LE. ABS(dxold(i) * df(i))
             IF (leave_bracket .OR. .NOT. dx_decrease) THEN
                dxold(i) = dx(i)
                dx(i) = p5 * (xhi(i) - xlo(i))
                soln(i) = xlo(i) + dx(i)
                IF (xlo(i) .EQ. soln(i)) mask(i) = .FALSE.
             ELSE
                dxold(i) = dx(i)
                dx(i) = -f(i) / df(i)
                temp = soln(i)
                soln(i) = soln(i) + dx(i)
                IF (temp .EQ. soln(i)) mask(i) = .FALSE.
             END IF
             IF (ABS(dx(i)) .LT. xacc) mask(i) = .FALSE.
          END IF
       END DO
       IF (.NOT. ANY(mask)) RETURN
       CALL talk_row(iblock, mask, k1, k2, soln, f, df)
       DO i = 1,nx_block
          IF (mask(i)) THEN
             IF (f(i) .LT. c0) THEN
                xlo(i) = soln(i)
                flo(i) = f(i)
             ELSE
                xhi(i) = soln(i)
                fhi(i) = f(i)
             END IF
          END IF
       END DO
    END DO ! iteration loop ! iteration loop
!kgen_excluded     CALL shr_sys_abort('lack of convergence in drtsafe_row')
        END SUBROUTINE drtsafe_row
        !*****************************************************************************

        SUBROUTINE talk_row(iblock, mask, k1, k2, x, fn, df)
            !---------------------------------------------------------------------------
            !   This routine computes TA as a function of DIC, htotal and constants.
            !   It also calculates the derivative of this function with respect to
            !   htotal. It is used in the iterative solution for htotal. In the call
            !   "x" is the input value for htotal, "fn" is the calculated value for
            !   TA and "df" is the value for dTA/dhtotal.
            !---------------------------------------------------------------------------
            !---------------------------------------------------------------------------
            !   input arguments
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind), intent(in) :: iblock
            LOGICAL(KIND=log_kind), dimension(nx_block), intent(in) :: mask
            REAL(KIND=r8), dimension(nx_block), intent(in) :: k1
            REAL(KIND=r8), dimension(nx_block), intent(in) :: k2
            REAL(KIND=r8), dimension(nx_block), intent(in) :: x
            !---------------------------------------------------------------------------
            !   output arguments
            !---------------------------------------------------------------------------
            REAL(KIND=r8), dimension(nx_block), intent(out) :: fn
            REAL(KIND=r8), dimension(nx_block), intent(out) :: df
            !---------------------------------------------------------------------------
            !   local variable declarations
            !---------------------------------------------------------------------------
            INTEGER(KIND=int_kind) :: i
            REAL(KIND=r8) :: x1
            REAL(KIND=r8) :: x1_r
            REAL(KIND=r8) :: x2
            REAL(KIND=r8) :: x2_r
            REAL(KIND=r8) :: x3
            REAL(KIND=r8) :: k12
            REAL(KIND=r8) :: k12p
            REAL(KIND=r8) :: k123p
            REAL(KIND=r8) :: a
            REAL(KIND=r8) :: a_r
            REAL(KIND=r8) :: a2_r
            REAL(KIND=r8) :: da
            REAL(KIND=r8) :: b
            REAL(KIND=r8) :: b_r
            REAL(KIND=r8) :: b2_r
            REAL(KIND=r8) :: db
            REAL(KIND=r8) :: c
            REAL(KIND=r8) :: c_r
            REAL(KIND=r8) :: kb_p_x1_r
            REAL(KIND=r8) :: ksi_p_x1_r
            REAL(KIND=r8) :: c1_p_c_ks_x1_r_r
            REAL(KIND=r8) :: c1_p_kf_x1_r_r
            !---------------------------------------------------------------------------
    DO i = 1,nx_block
       IF (mask(i)) THEN
          x1 = x(i)
          x1_r = c1 / x1
          x2 = x1 * x1
          x2_r = x1_r * x1_r
          x3 = x2 * x1
          k12 = k1(i) * k2(i)
          k12p = k1p(i,iblock) * k2p(i,iblock)
          k123p = k12p * k3p(i,iblock)
          a = x3 + k1p(i,iblock) * x2 + k12p * x1 + k123p
          a_r = c1 / a
          a2_r = a_r * a_r
          da = c3 * x2 + c2 * k1p(i,iblock) * x1 + k12p
          b = x2 + k1(i) * x1 + k12
          b_r = c1 / b
          b2_r = b_r * b_r
          db = c2 * x1 + k1(i)
          c = c1 + st(i,iblock) / ks(i,iblock)
          c_r = c1 / c
          kb_p_x1_r = c1 / (kb(i,iblock) + x1)
          ksi_p_x1_r = c1 / (ksi(i,iblock) + x1)
          c1_p_c_ks_x1_r_r = c1 / (c1 + c * ks(i,iblock) * x1_r)
          c1_p_kf_x1_r_r = c1 / (c1 + kf(i,iblock) * x1_r)
                    !---------------------------------------------------------------------
                    !   fn = hco3+co3+borate+oh+hpo4+2*po4+silicate-hfree-hso4-hf-h3po4-ta
                    !---------------------------------------------------------------------
          fn(i) = k1(i) * dic(i,iblock) * x1 * b_r &
               + c2 * dic(i,iblock) * k12 * b_r &
               + bt(i,iblock) * kb(i,iblock) * kb_p_x1_r &
               + kw(i,iblock) * x1_r &
               + pt(i,iblock) * k12p * x1 * a_r &
               + c2 * pt(i,iblock) * k123p * a_r &
               + sit(i,iblock) * ksi(i,iblock) * ksi_p_x1_r &
               - x1 * c_r &
               - st(i,iblock) * c1_p_c_ks_x1_r_r &
               - ft(i,iblock) * c1_p_kf_x1_r_r &
               - pt(i,iblock) * x3 * a_r &
               - ta(i,iblock)
                    !---------------------------------------------------------------------
                    !   df = d(fn)/dx
                    !---------------------------------------------------------------------
          df(i) = k1(i) * dic(i,iblock) * (b - x1 * db) * b2_r &
               - c2 * dic(i,iblock) * k12 * db * b2_r &
               - bt(i,iblock) * kb(i,iblock) * kb_p_x1_r * kb_p_x1_r &
               - kw(i,iblock) * x2_r &
               + (pt(i,iblock) * k12p * (a - x1 * da)) * a2_r &
               - c2 * pt(i,iblock) * k123p * da * a2_r &
               - sit(i,iblock) * ksi(i,iblock) * ksi_p_x1_r * ksi_p_x1_r &
               - c1 * c_r &
               - st(i,iblock) * c1_p_c_ks_x1_r_r * c1_p_c_ks_x1_r_r * (c * ks(i,iblock) * x2_r) &
               - ft(i,iblock) * c1_p_kf_x1_r_r * c1_p_kf_x1_r_r * kf(i,iblock) * x2_r &
               - pt(i,iblock) * x2 * (c3 * a - x1 * da) * a2_r
       END IF ! if mask ! if mask
    END DO ! i loop ! i loop
        END SUBROUTINE talk_row
        !*****************************************************************************

        !*****************************************************************************
    END MODULE co2calc
