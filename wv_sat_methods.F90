module wv_sat_methods

! This portable module contains all CAM methods for estimating
! the saturation vapor pressure of water.
!
! wv_saturation provides CAM-specific interfaces and utilities
! based on these formulae.
!
! Typical usage of this module:
!
! Init:
! call wv_sat_methods_init(r8, <constants>, errstring)
!
! Get scheme index from a name string:
! scheme_idx = wv_sat_get_scheme_idx(scheme_name)
! if (.not. wv_sat_valid_idx(scheme_idx)) <throw some error>
!
! Get pressures:
! es = wv_sat_svp_water(t, scheme_idx)
! es = wv_sat_svp_ice(t, scheme_idx)
!
! Use ice/water transition range:
! es = wv_sat_svp_trice(t, ttrice, scheme_idx)
!
! Note that elemental functions cannot be pointed to, nor passed
! as arguments. If you need to do either, it is recommended to
! wrap the function so that it can be given an explicit (non-
! elemental) interface.

use kgen_utils, only : read_var

implicit none
private
save

integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real

real(r8) :: tmelt   ! Melting point of water at 1 atm (K)
real(r8) :: h2otrip ! Triple point temperature of water (K)
real(r8) :: tboil   ! Boiling point of water at 1 atm (K)

real(r8) :: epsilo  ! Ice-water transition range
real(r8) :: omeps   ! 1._r8 - epsilo

! Indices representing individual schemes
integer, parameter :: OldGoffGratch_idx = 0
integer, parameter :: GoffGratch_idx = 1
integer, parameter :: MurphyKoop_idx = 2
integer, parameter :: Bolton_idx = 3

!NOTE: need to resolve locally
! Index representing the current default scheme.
integer, parameter :: initial_default_idx = GoffGratch_idx
integer :: default_idx = initial_default_idx

public wv_sat_svp_water, wv_sat_svp_water_vl
public wv_sat_svp_ice, wv_sat_svp_ice_vl

! pressure -> humidity conversion
public wv_sat_svp_to_qsat, wv_sat_svp_to_qsat_vl

! Combined qsat operations
public wv_sat_qsat_water, wv_sat_qsat_water_vl
public wv_sat_qsat_ice, wv_sat_qsat_ice_vl

public read_extern_wv_sat_methods

contains

! read external states required for kernel in this module
subroutine read_extern_wv_sat_methods(kgen_unit)
integer, intent(in) :: kgen_unit

READ(UNIT=kgen_unit) default_idx
READ(UNIT=kgen_unit) tmelt
READ(UNIT=kgen_unit) h2otrip
READ(UNIT=kgen_unit) tboil
READ(UNIT=kgen_unit) epsilo
READ(UNIT=kgen_unit) omeps

end subroutine

! Old CAM implementation, also labelled Goff & Gratch (1946)

! The water formula differs only due to compiler-dependent order of
! operations, so differences are roundoff level, usually 0.

! The ice formula gives fairly close answers to the current
! implementation, but has been rearranged, and uses the
! 1 atm melting point of water as the triple point.
! Differences are thus small but above roundoff.

! A curious fact: although using the melting point of water was
! probably a mistake, it mildly improves accuracy for ice svp,
! since it compensates for a systematic error in Goff & Gratch.

elemental function OldGoffGratch_svp_water(t) result(es)
  real(r8), intent(in) :: t
  real(r8) :: es
  real(r8) :: ps, e1, e2, f1, f2, f3, f4, f5, f

  ps = 1013.246_r8
  e1 = 11.344_r8*(1.0_r8 - t/tboil)
  e2 = -3.49149_r8*(tboil/t - 1.0_r8)
  f1 = -7.90298_r8*(tboil/t - 1.0_r8)
  f2 = 5.02808_r8*log10(tboil/t)
  f3 = -1.3816_r8*(10.0_r8**e1 - 1.0_r8)/10000000.0_r8
  f4 = 8.1328_r8*(10.0_r8**e2 - 1.0_r8)/1000.0_r8
  f5 = log10(ps)
  f  = f1 + f2 + f3 + f4 + f5

  es = (10.0_r8**f)*100.0_r8

end function OldGoffGratch_svp_water

function OldGoffGratch_svp_water_vl(t) result(es)
  real(r8), intent(in) :: t
  real(r8) :: es
  real(r8) :: ps, e1, e2, f1, f2, f3, f4, f5, f

  ps = 1013.246_r8
  e1 = 11.344_r8*(1.0_r8 - t/tboil)
  e2 = -3.49149_r8*(tboil/t - 1.0_r8)
  f1 = -7.90298_r8*(tboil/t - 1.0_r8)
  f2 = 5.02808_r8*log10(tboil/t)
  f3 = -1.3816_r8*(10.0_r8**e1 - 1.0_r8)/10000000.0_r8
  f4 = 8.1328_r8*(10.0_r8**e2 - 1.0_r8)/1000.0_r8
  f5 = log10(ps)
  f  = f1 + f2 + f3 + f4 + f5

  es = (10.0_r8**f)*100.0_r8

end function OldGoffGratch_svp_water_vl

! Murphy & Koop (2005)

elemental function MurphyKoop_svp_water(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! (good for 123 < T < 332 K)
  es = exp(54.842763_r8 - (6763.22_r8 / t) - (4.210_r8 * log(t)) + &
       (0.000367_r8 * t) + (tanh(0.0415_r8 * (t - 218.8_r8)) * &
       (53.878_r8 - (1331.22_r8 / t) - (9.44523_r8 * log(t)) + &
       0.014025_r8 * t)))

end function MurphyKoop_svp_water

function MurphyKoop_svp_water_vl(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! (good for 123 < T < 332 K)
  es = exp(54.842763_r8 - (6763.22_r8 / t) - (4.210_r8 * log(t)) + &
       (0.000367_r8 * t) + (tanh(0.0415_r8 * (t - 218.8_r8)) * &
       (53.878_r8 - (1331.22_r8 / t) - (9.44523_r8 * log(t)) + &
       0.014025_r8 * t)))

end function MurphyKoop_svp_water_vl

elemental function GoffGratch_svp_water(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! uncertain below -70 C
  es = 10._r8**(-7.90298_r8*(tboil/t-1._r8)+ &
       5.02808_r8*log10(tboil/t)- &
       1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/tboil))-1._r8)+ &
       8.1328e-3_r8*(10._r8**(-3.49149_r8*(tboil/t-1._r8))-1._r8)+ &
       log10(1013.246_r8))*100._r8

end function GoffGratch_svp_water

function GoffGratch_svp_water_vl(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! uncertain below -70 C
  es = 10._r8**(-7.90298_r8*(tboil/t-1._r8)+ &
       5.02808_r8*log10(tboil/t)- &
       1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/tboil))-1._r8)+ &
       8.1328e-3_r8*(10._r8**(-3.49149_r8*(tboil/t-1._r8))-1._r8)+ &
       log10(1013.246_r8))*100._r8

end function GoffGratch_svp_water_vl

elemental function wv_sat_svp_water(t, idx) result(es)
  real(r8), intent(in) :: t
  integer,  intent(in), optional :: idx
  real(r8) :: es

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     es = GoffGratch_svp_water(t)
  case(MurphyKoop_idx)
     es = MurphyKoop_svp_water(t)
  case(OldGoffGratch_idx)
     es = OldGoffGratch_svp_water(t)
  case(Bolton_idx)
     es = Bolton_svp_water(t)
  end select

end function wv_sat_svp_water

function wv_sat_svp_water_vl(t, idx) result(es)
  real(r8), intent(in) :: t
  integer,  intent(in), optional :: idx
  real(r8) :: es

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     es = GoffGratch_svp_water_vl(t)
  case(MurphyKoop_idx)
     es = MurphyKoop_svp_water_vl(t)
  case(OldGoffGratch_idx)
     es = OldGoffGratch_svp_water_vl(t)
  case(Bolton_idx)
     es = Bolton_svp_water_vl(t)
  end select

end function wv_sat_svp_water_vl

function wv_sat_svp_water_vl2(t, mgncol, idx) result(es)
  integer, intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t
  integer,  dimension(mgncol), intent(in), optional :: idx
  real(r8), dimension(mgncol) :: es

  integer,dimension(mgncol) :: use_idx
  integer :: i


  if (present(idx)) then
     use_idx(:) = idx(:)
  else
     use_idx(:) = default_idx
  end if

  do i=1,mgncol
  select case (use_idx(i))
  case(GoffGratch_idx)
     es(i) = GoffGratch_svp_water_vl(t(i))
  case(MurphyKoop_idx)
     es(i) = MurphyKoop_svp_water_vl(t(i))
  case(OldGoffGratch_idx)
     es(i) = OldGoffGratch_svp_water_vl(t(i))
  case(Bolton_idx)
     es(i) = Bolton_svp_water_vl(t(i))
  end select
  enddo

end function wv_sat_svp_water_vl2

! Bolton (1980)
! zm_conv deep convection scheme contained this SVP calculation.
! It appears to be from D. Bolton, 1980, Monthly Weather Review.
! Unlike the other schemes, no distinct ice formula is associated
! with it. (However, a Bolton ice formula exists in CLUBB.)

! The original formula used degrees C, but this function
! takes Kelvin and internally converts.

elemental function Bolton_svp_water(t) result(es)
  real(r8),parameter :: c1 = 611.2_r8
  real(r8),parameter :: c2 = 17.67_r8
  real(r8),parameter :: c3 = 243.5_r8

  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  es = c1*exp( (c2*(t - tmelt))/((t - tmelt)+c3) )

end function Bolton_svp_water

function Bolton_svp_water_vl(t) result(es)
  real(r8),parameter :: c1 = 611.2_r8
  real(r8),parameter :: c2 = 17.67_r8
  real(r8),parameter :: c3 = 243.5_r8

  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  es = c1*exp( (c2*(t - tmelt))/((t - tmelt)+c3) )

end function Bolton_svp_water_vl

elemental function OldGoffGratch_svp_ice(t) result(es)
  real(r8), intent(in) :: t
  real(r8) :: es
  real(r8) :: term1, term2, term3

  term1 = 2.01889049_r8/(tmelt/t)
  term2 = 3.56654_r8*log(tmelt/t)
  term3 = 20.947031_r8*(tmelt/t)

  es = 575.185606e10_r8*exp(-(term1 + term2 + term3))

end function OldGoffGratch_svp_ice

function OldGoffGratch_svp_ice_vl(t) result(es)
  real(r8), intent(in) :: t
  real(r8) :: es
  real(r8) :: term1, term2, term3

  term1 = 2.01889049_r8/(tmelt/t)
  term2 = 3.56654_r8*log(tmelt/t)
  term3 = 20.947031_r8*(tmelt/t)

  es = 575.185606e10_r8*exp(-(term1 + term2 + term3))

end function OldGoffGratch_svp_ice_vl

elemental function MurphyKoop_svp_ice(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! (good down to 110 K)
  es = exp(9.550426_r8 - (5723.265_r8 / t) + (3.53068_r8 * log(t)) &
       - (0.00728332_r8 * t))

end function MurphyKoop_svp_ice

function MurphyKoop_svp_ice_vl(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! (good down to 110 K)
  es = exp(9.550426_r8 - (5723.265_r8 / t) + (3.53068_r8 * log(t)) &
       - (0.00728332_r8 * t))

end function MurphyKoop_svp_ice_vl

elemental function GoffGratch_svp_ice(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! good down to -100 C
  es = 10._r8**(-9.09718_r8*(h2otrip/t-1._r8)-3.56654_r8* &
       log10(h2otrip/t)+0.876793_r8*(1._r8-t/h2otrip)+ &
       log10(6.1071_r8))*100._r8

end function GoffGratch_svp_ice

function GoffGratch_svp_ice_vl(t) result(es)
  real(r8), intent(in) :: t  ! Temperature in Kelvin
  real(r8) :: es             ! SVP in Pa

  ! good down to -100 C
  es = 10._r8**(-9.09718_r8*(h2otrip/t-1._r8)-3.56654_r8* &
       log10(h2otrip/t)+0.876793_r8*(1._r8-t/h2otrip)+ &
       log10(6.1071_r8))*100._r8

end function GoffGratch_svp_ice_vl

! Get saturation specific humidity given pressure and SVP.
! Specific humidity is limited to range 0-1.
elemental function wv_sat_svp_to_qsat(es, p) result(qs)

  real(r8), intent(in) :: es  ! SVP
  real(r8), intent(in) :: p   ! Current pressure.
  real(r8) :: qs

  ! If pressure is less than SVP, set qs to maximum of 1.
  if ( (p - es) <= 0._r8 ) then
     qs = 1.0_r8
  else
     qs = epsilo*es / (p - omeps*es)
  end if

end function wv_sat_svp_to_qsat

function wv_sat_svp_to_qsat_vl(es, p) result(qs)

  real(r8), intent(in) :: es  ! SVP
  real(r8), intent(in) :: p   ! Current pressure.
  real(r8) :: qs

  ! If pressure is less than SVP, set qs to maximum of 1.
  if ( (p - es) <= 0._r8 ) then
     qs = 1.0_r8
  else
     qs = epsilo*es / (p - omeps*es)
  end if

end function wv_sat_svp_to_qsat_vl

elemental function wv_sat_svp_ice(t, idx) result(es)
  real(r8), intent(in) :: t
  integer,  intent(in), optional :: idx
  real(r8) :: es

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     es = GoffGratch_svp_ice(t)
  case(MurphyKoop_idx)
     es = MurphyKoop_svp_ice(t)
  case(OldGoffGratch_idx)
     es = OldGoffGratch_svp_ice(t)
  case(Bolton_idx)
     es = Bolton_svp_water(t)
  end select

end function wv_sat_svp_ice

function wv_sat_svp_ice_vl(t, idx) result(es)
  real(r8), intent(in) :: t
  integer,  intent(in), optional :: idx
  real(r8) :: es

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     es = GoffGratch_svp_ice_vl(t)
  case(MurphyKoop_idx)
     es = MurphyKoop_svp_ice_vl(t)
  case(OldGoffGratch_idx)
     es = OldGoffGratch_svp_ice_vl(t)
  case(Bolton_idx)
     es = Bolton_svp_water_vl(t)
  end select

end function wv_sat_svp_ice_vl

function wv_sat_svp_ice_vl2(t, mgncol, idx) result(es) 
  integer, intent(in) :: mgncol
  real(r8), dimension(mgncol), intent(in) :: t
  integer,  dimension(mgncol), intent(in), optional :: idx
  real(r8), dimension(mgncol) :: es

  integer, dimension(mgncol) :: use_idx
  integer :: i

  if (present(idx)) then
     use_idx(:)= idx(:)
  else
     use_idx(:) = default_idx
  end if

  do i=1,mgncol
  select case (use_idx(i))
  case(GoffGratch_idx)
     es(i) = GoffGratch_svp_ice_vl(t(i))
  case(MurphyKoop_idx)
     es(i) = MurphyKoop_svp_ice_vl(t(i))
  case(OldGoffGratch_idx)
     es(i) = OldGoffGratch_svp_ice_vl(t(i))
  case(Bolton_idx)
     es(i) = Bolton_svp_water_vl(t(i))
  end select
  enddo

end function wv_sat_svp_ice_vl2

elemental subroutine wv_sat_qsat_water(t, p, es, qs, idx)
  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over water at a given temperature, and then      !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!

  ! Inputs
  real(r8), intent(in) :: t    ! Temperature
  real(r8), intent(in) :: p    ! Pressure
  ! Outputs
  real(r8), intent(out) :: es  ! Saturation vapor pressure
  real(r8), intent(out) :: qs  ! Saturation specific humidity

  integer,  intent(in), optional :: idx ! Scheme index

  es = wv_sat_svp_water(t, idx)

  qs = wv_sat_svp_to_qsat(es, p)

  ! Ensures returned es is consistent with limiters on qs.
  es = min(es, p)

end subroutine wv_sat_qsat_water

subroutine wv_sat_qsat_water_vl(t, p, es, qs, mgncol, idx)
  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over water at a given temperature, and then      !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!

  ! Inputs
  integer,  intent(in) :: mgncol
  real(r8), intent(in) :: t(mgncol)    ! Temperature
  real(r8), intent(in) :: p(mgncol)    ! Pressure
  ! Outputs
  real(r8), intent(out) :: es(mgncol)  ! Saturation vapor pressure
  real(r8), intent(out) :: qs(mgncol)  ! Saturation specific humidity

  integer,  intent(in), optional :: idx(mgncol) ! Scheme index

  integer :: i

  es = wv_sat_svp_water_vl2(t, mgncol, idx)
  qs = wv_sat_svp_to_qsat(es, p)

  do i=1,mgncol
!   es(i) = wv_sat_svp_water_vl(t(i), idx(i))
!   qs(i) = wv_sat_svp_to_qsat_vl(es(i), p(i))
  ! Ensures returned es is consistent with limiters on qs.
    es(i) = min(es(i), p(i))
  enddo

end subroutine wv_sat_qsat_water_vl


elemental subroutine wv_sat_qsat_ice(t, p, es, qs, idx)
  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over ice at a given temperature, and then        !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!

  ! Inputs
  real(r8), intent(in) :: t    ! Temperature
  real(r8), intent(in) :: p    ! Pressure
  ! Outputs
  real(r8), intent(out) :: es  ! Saturation vapor pressure
  real(r8), intent(out) :: qs  ! Saturation specific humidity

  integer,  intent(in), optional :: idx ! Scheme index

  es = wv_sat_svp_ice(t, idx)

  qs = wv_sat_svp_to_qsat(es, p)

  ! Ensures returned es is consistent with limiters on qs.
  es = min(es, p)

end subroutine wv_sat_qsat_ice

subroutine wv_sat_qsat_ice_vl(t, p, es, qs, mgncol, idx)
  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over ice at a given temperature, and then        !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!

  ! Inputs
  integer,  intent(in) :: mgncol
  real(r8), intent(in) :: t(mgncol)    ! Temperature
  real(r8), intent(in) :: p(mgncol)    ! Pressure
  ! Outputs
  real(r8), intent(out) :: es(mgncol)  ! Saturation vapor pressure
  real(r8), intent(out) :: qs(mgncol)  ! Saturation specific humidity

  integer,  intent(in), optional :: idx(mgncol) ! Scheme index
  integer :: i

  es = wv_sat_svp_ice_vl2(t, mgncol, idx)
  qs = wv_sat_svp_to_qsat(es, p)

  do i=1,mgncol
!   es(i) = wv_sat_svp_ice_vl(t(i), idx(i))

!   qs(i) = wv_sat_svp_to_qsat_vl(es(i), p(i))

  ! Ensures returned es is consistent with limiters on qs.
    es(i) = min(es(i), p(i))
  enddo

end subroutine wv_sat_qsat_ice_vl

end module wv_sat_methods
