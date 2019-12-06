!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  


module wv_sat_methods
! This portable module contains all CAM methods for estimating
! the saturation vapor pressure of water.
! wv_saturation provides CAM-specific interfaces and utilities
! based on these formulae.
! Typical usage of this module:
! Init:
! call wv_sat_methods_init(rkind_comp, <constants>, errstring)
! Get scheme index from a name string:
! scheme_idx = wv_sat_get_scheme_idx(scheme_name)
! if (.not. wv_sat_valid_idx(scheme_idx)) <throw some error>
! Get pressures:
! es = wv_sat_svp_water(t, scheme_idx)
! es = wv_sat_svp_ice(t, scheme_idx)
! Use ice/water transition range:
! es = wv_sat_svp_trice(t, ttrice, scheme_idx)
! Note that elemental functions cannot be pointed to, nor passed
! as arguments. If you need to do either, it is recommended to
! wrap the function so that it can be given an explicit (non-
! elemental) interface.
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE shr_kind_mod, ONLY: rkind_comp, rkind_io

!
!
!
!
!
!
!

    IMPLICIT NONE 
    PRIVATE 
    SAVE 


real(rkind_comp) :: tmelt   ! Melting point of water at 1 atm (K)
real(rkind_comp) :: h2otrip ! Triple point temperature of water (K)
real(rkind_comp) :: tboil   ! Boiling point of water at 1 atm (K)


real(rkind_comp) :: epsilo  ! Ice-water transition range
real(rkind_comp) :: omeps   ! 1._rkind_comp - epsilo
! Indices representing individual schemes

integer, parameter :: OldGoffGratch_idx = 0
integer, parameter :: GoffGratch_idx = 1
integer, parameter :: MurphyKoop_idx = 2
integer, parameter :: Bolton_idx = 3
! Index representing the current default scheme.

integer, parameter :: initial_default_idx = GoffGratch_idx
integer :: default_idx = initial_default_idx

!!$acc declare create(epsilo, tmelt, tboil, default_idx, omeps, h2otrip)
!$acc declare copyin(epsilo, tmelt, tboil, default_idx, omeps, h2otrip)

INTERFACE wv_sat_svp_water
   MODULE PROCEDURE wv_sat_svp_water_rkind_comp
   MODULE PROCEDURE wv_sat_svp_water_v8
END INTERFACE wv_sat_svp_water
PUBLIC wv_sat_svp_water 

INTERFACE wv_sat_svp_ice
   MODULE PROCEDURE wv_sat_svp_ice_rkind_comp
   MODULE PROCEDURE wv_sat_svp_ice_v8
END INTERFACE wv_sat_svp_ice
PUBLIC wv_sat_svp_ice 
! pressure -> humidity conversion

INTERFACE wv_sat_svp_to_qsat
   MODULE PROCEDURE wv_sat_svp_to_qsat_rkind_comp
   MODULE PROCEDURE wv_sat_svp_to_qsat_v8
END INTERFACE wv_sat_svp_to_qsat
PUBLIC wv_sat_svp_to_qsat 
! Combined qsat operations

!INTERFACE wv_sat_qsat_water
!   MODULE PROCEDURE wv_sat_qsat_water_rkind_comp
!   MODULE PROCEDURE wv_sat_qsat_water_v8
!END INTERFACE wv_sat_qsat_water
PUBLIC wv_sat_qsat_water_scalar 
PUBLIC wv_sat_qsat_water_vector

!INTERFACE wv_sat_qsat_ice
!   MODULE PROCEDURE wv_sat_qsat_ice_rkind_comp
!   MODULE PROCEDURE wv_sat_qsat_ice_v8
!END INTERFACE wv_sat_qsat_ice
PUBLIC wv_sat_qsat_ice_scalar 
PUBLIC wv_sat_qsat_ice_vector

INTERFACE goffgratch_svp_water
   MODULE PROCEDURE goffgratch_svp_water_rkind_comp
   MODULE PROCEDURE goffgratch_svp_water_v8
END INTERFACE goffgratch_svp_water

INTERFACE goffgratch_svp_ice
   MODULE PROCEDURE goffgratch_svp_ice_rkind_comp
   MODULE PROCEDURE goffgratch_svp_ice_v8
END INTERFACE goffgratch_svp_ice

PUBLIC kr_externs_in_wv_sat_methods 
PUBLIC kr_externs_out_wv_sat_methods 

contains
!---------------------------------------------------------------------
! ADMINISTRATIVE FUNCTIONS
!---------------------------------------------------------------------
! Get physical constants


! Look up index by name.


! Check validity of an index from the above routine.


! Set default scheme (otherwise, Goff & Gratch is default)
! Returns a logical representing success (.true.) or
! failure (.false.).


! Reset default scheme to initial value.
! The same thing can be accomplished with wv_sat_set_default;
! the real reason to provide this routine is to reset the
! module for testing purposes.


!---------------------------------------------------------------------
! UTILITIES
!---------------------------------------------------------------------
! Get saturation specific humidity given pressure and SVP.
! Specific humidity is limited to range 0-1.


subroutine  wv_sat_svp_to_qsat_rkind_comp(es, p, qs)

  real(rkind_comp), intent(in)  :: es  ! SVP
  real(rkind_comp), intent(in)  :: p   ! Current pressure.
  real(rkind_comp), intent(out) :: qs
  ! If pressure is less than SVP, set qs to maximum of 1.

  if ( (p - es) <= 0._rkind_comp ) then
     qs = 1.0_rkind_comp
  else
     qs = epsilo*es / (p - omeps*es)
  end if

end subroutine wv_sat_svp_to_qsat_rkind_comp

subroutine  wv_sat_svp_to_qsat_v8(es, p, qs, vlen)

  integer,  intent(in) :: vlen
  real(rkind_comp), intent(in)  :: es(vlen)  ! SVP
  real(rkind_comp), intent(in)  :: p(vlen)   ! Current pressure.
  real(rkind_comp), intent(out) :: qs(vlen)
  integer :: i
  !$acc declare present(es,p,qs)
  ! If pressure is less than SVP, set qs to maximum of 1.
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen 
     if ( (p(i) - es(i)) <= 0._rkind_comp ) then
        qs(i) = 1.0_rkind_comp
     else
        qs(i) = epsilo*es(i) / (p(i) - omeps*es(i))
     end if
  enddo
  !$acc end parallel

end subroutine wv_sat_svp_to_qsat_v8

subroutine wv_sat_qsat_water_scalar(t, p, es, qs, idx)
  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over water at a given temperature, and then      !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!
  ! Inputs


  real(rkind_comp), intent(in) :: t    ! Temperature
  real(rkind_comp), intent(in) :: p    ! Pressure
  ! Outputs
  real(rkind_comp), intent(out) :: es  ! Saturation vapor pressure
  real(rkind_comp), intent(out) :: qs  ! Saturation specific humidity

  integer,  intent(in), optional :: idx ! Scheme index

  call wv_sat_svp_water(t, es, idx)

  call wv_sat_svp_to_qsat(es, p, qs)
  ! Ensures returned es is consistent with limiters on qs.

  es = min(es, p)

end subroutine wv_sat_qsat_water_scalar

subroutine wv_sat_qsat_water_vector(t, p, es, qs, vlen, idx)

  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over water at a given temperature, and then      !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!
  ! Inputs

  integer,  intent(in) :: vlen
  real(rkind_comp), intent(in) :: t(vlen)    ! Temperature
  real(rkind_comp), intent(in) :: p(vlen)    ! Pressure
  ! Outputs
  real(rkind_comp), intent(out) :: es(vlen)  ! Saturation vapor pressure
  real(rkind_comp), intent(out) :: qs(vlen)  ! Saturation specific humidity

  integer,  intent(in), optional :: idx ! Scheme index
  integer :: i
  !$acc declare present(es,p)

  call wv_sat_svp_water(t, es, vlen, idx)
  call wv_sat_svp_to_qsat(es, p, qs, vlen)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     ! Ensures returned es is consistent with limiters on qs.
     es(i) = min(es(i), p(i))
  enddo
  !$acc end parallel

end subroutine wv_sat_qsat_water_vector

subroutine wv_sat_qsat_ice_scalar(t, p, es, qs, idx)

  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over ice at a given temperature, and then        !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!
  ! Inputs

  real(rkind_comp), intent(in) :: t    ! Temperature
  real(rkind_comp), intent(in) :: p    ! Pressure
  ! Outputs
  real(rkind_comp), intent(out) :: es  ! Saturation vapor pressure
  real(rkind_comp), intent(out) :: qs  ! Saturation specific humidity

  integer,  intent(in), optional :: idx ! Scheme index

  call wv_sat_svp_ice(t, es, idx)

  call wv_sat_svp_to_qsat(es, p, qs)
  ! Ensures returned es is consistent with limiters on qs.

  es = min(es, p)

end subroutine wv_sat_qsat_ice_scalar

subroutine wv_sat_qsat_ice_vector(t, p, es, qs, vlen, idx)

  !------------------------------------------------------------------!
  ! Purpose:                                                         !
  !   Calculate SVP over ice at a given temperature, and then        !
  !   calculate and return saturation specific humidity.             !
  !------------------------------------------------------------------!
  ! Inputs

  integer,  intent(in) :: vlen
  real(rkind_comp), intent(in) :: t(vlen)    ! Temperature
  real(rkind_comp), intent(in) :: p(vlen)    ! Pressure
  ! Outputs
  real(rkind_comp), intent(out) :: es(vlen)  ! Saturation vapor pressure
  real(rkind_comp), intent(out) :: qs(vlen)  ! Saturation specific humidity

  integer,  intent(in), optional :: idx ! Scheme index
  integer :: i
  !$acc declare present(es,p)

  call wv_sat_svp_ice(t, es, vlen, idx)
  call wv_sat_svp_to_qsat(es, p, qs, vlen)

  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     ! Ensures returned es is consistent with limiters on qs.
     es(i) = min(es(i), p(i))
  enddo
  !$acc end parallel

end subroutine wv_sat_qsat_ice_vector


!---------------------------------------------------------------------
! SVP INTERFACE FUNCTIONS
!---------------------------------------------------------------------


subroutine  wv_sat_svp_water_rkind_comp(t, es, idx)

  real(rkind_comp), intent(in) :: t
  integer,  intent(in), optional :: idx
  real(rkind_comp), intent(out) :: es

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     call GoffGratch_svp_water(t,es)
  case(MurphyKoop_idx)
     call MurphyKoop_svp_water(t,es)
  case(OldGoffGratch_idx)
     call OldGoffGratch_svp_water(t,es)
  case(Bolton_idx)
     call Bolton_svp_water(t,es)
  end select

end subroutine wv_sat_svp_water_rkind_comp

subroutine  wv_sat_svp_water_v8(t, es, vlen, idx)

  integer,  intent(in) :: vlen
  real(rkind_comp), intent(in) :: t(vlen)
  integer,  intent(in), optional :: idx
  real(rkind_comp), intent(out) :: es(vlen)
  integer :: i
  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     call GoffGratch_svp_water(t,es,vlen)
  case(MurphyKoop_idx)
     !$acc loop vector
     do i=1,vlen
        call MurphyKoop_svp_water(t(i),es(i))
     enddo
  case(OldGoffGratch_idx)
     !$acc loop vector
     do i=1,vlen
        call OldGoffGratch_svp_water(t(i),es(i))
     enddo
  case(Bolton_idx)
     !$acc loop vector
     do i=1,vlen
        call Bolton_svp_water(t(i),es(i))
     enddo
  end select

end subroutine wv_sat_svp_water_v8

subroutine wv_sat_svp_ice_rkind_comp(t, es, idx)

  real(rkind_comp), intent(in) :: t
  integer,  intent(in), optional :: idx
  real(rkind_comp), intent(out) :: es

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     call GoffGratch_svp_ice(t,es)
  case(MurphyKoop_idx)
     call MurphyKoop_svp_ice(t,es)
  case(OldGoffGratch_idx)
     call OldGoffGratch_svp_ice(t,es)
  case(Bolton_idx)
     call Bolton_svp_water(t,es)
  end select

end subroutine wv_sat_svp_ice_rkind_comp

subroutine wv_sat_svp_ice_v8(t, es, vlen, idx)

  integer,  intent(in) :: vlen
  real(rkind_comp), intent(in) :: t(vlen)
  integer,  intent(in), optional :: idx
  real(rkind_comp), intent(out) :: es(vlen)
  integer :: i

  integer :: use_idx

  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if

  select case (use_idx)
  case(GoffGratch_idx)
     call GoffGratch_svp_ice(t,es,vlen)
  case(MurphyKoop_idx)
     !$acc loop vector
     do i=1,vlen
        call MurphyKoop_svp_ice(t(i),es(i))
     enddo
  case(OldGoffGratch_idx)
     !$acc loop vector
     do i=1,vlen
        call OldGoffGratch_svp_ice(t(i),es(i))
     enddo
  case(Bolton_idx)
     !$acc loop vector
     do i=1,vlen
        call Bolton_svp_water(t(i),es(i))
     enddo
  end select

end subroutine wv_sat_svp_ice_v8

!---------------------------------------------------------------------
! SVP METHODS
!---------------------------------------------------------------------
! Goff & Gratch (1946)


subroutine GoffGratch_svp_water_rkind_comp(t, es)
  real(rkind_comp), intent(in)  :: t  ! Temperature in Kelvin
  real(rkind_comp), intent(out) :: es             ! SVP in Pa
  ! uncertain below -70 C


  es = 10._rkind_comp**(-7.90298_rkind_comp*(tboil/t-1._rkind_comp)+ &
       5.02808_rkind_comp*log10(tboil/t)- &
       1.3816e-7_rkind_comp*(10._rkind_comp**(11.344_rkind_comp*(1._rkind_comp-t/tboil))-1._rkind_comp)+ &
       8.1328e-3_rkind_comp*(10._rkind_comp**(-3.49149_rkind_comp*(tboil/t-1._rkind_comp))-1._rkind_comp)+ &
       log10(1013.246_rkind_comp))*100._rkind_comp

end subroutine GoffGratch_svp_water_rkind_comp

subroutine GoffGratch_svp_water_v8(t, es, vlen)

  integer :: vlen
  real(rkind_comp), intent(in)  :: t(vlen)  ! Temperature in Kelvin
  real(rkind_comp), intent(out) :: es(vlen) ! SVP in Pa
  integer :: i
  ! uncertain below -70 C
  !$acc declare present(t,es)
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
     es(i) = 10._rkind_comp**(-7.90298_rkind_comp*(tboil/t(i)-1._rkind_comp)+ &
       5.02808_rkind_comp*log10(tboil/t(i))- &
       1.3816e-7_rkind_comp*(10._rkind_comp**(11.344_rkind_comp*(1._rkind_comp-t(i)/tboil))-1._rkind_comp)+ &
       8.1328e-3_rkind_comp*(10._rkind_comp**(-3.49149_rkind_comp*(tboil/t(i)-1._rkind_comp))-1._rkind_comp)+ &
       log10(1013.246_rkind_comp))*100._rkind_comp
  enddo
  !$acc end parallel

end subroutine GoffGratch_svp_water_v8

subroutine GoffGratch_svp_ice_rkind_comp(t, es)
  real(rkind_comp), intent(in)  :: t  ! Temperature in Kelvin
  real(rkind_comp), intent(out) :: es             ! SVP in Pa
  ! good down to -100 C

  es = 10._rkind_comp**(-9.09718_rkind_comp*(h2otrip/t-1._rkind_comp)-3.56654_rkind_comp* &
       log10(h2otrip/t)+0.876793_rkind_comp*(1._rkind_comp-t/h2otrip)+ &
       log10(6.1071_rkind_comp))*100._rkind_comp

end subroutine GoffGratch_svp_ice_rkind_comp

subroutine GoffGratch_svp_ice_v8(t, es, vlen)

  integer :: vlen
  real(rkind_comp), intent(in)  :: t(vlen)  ! Temperature in Kelvin
  real(rkind_comp), intent(out) :: es(vlen)             ! SVP in Pa
  integer :: i
  ! good down to -100 C
  !$acc declare present(es,t) 
  !$acc parallel num_gangs(32)
  !$acc loop vector
  do i=1,vlen
  es(i) = 10._rkind_comp**(-9.09718_rkind_comp*(h2otrip/t(i)-1._rkind_comp)-3.56654_rkind_comp* &
       log10(h2otrip/t(i))+0.876793_rkind_comp*(1._rkind_comp-t(i)/h2otrip)+ &
       log10(6.1071_rkind_comp))*100._rkind_comp
  enddo
  !$acc end parallel

end subroutine GoffGratch_svp_ice_v8

! Murphy & Koop (2005)


subroutine MurphyKoop_svp_water(t, es)
  !$acc routine vector

  real(rkind_comp), intent(in)  :: t  ! Temperature in Kelvin
  real(rkind_comp), intent(out) :: es             ! SVP in Pa
  ! (good for 123 < T < 332 K)

  es = exp(54.842763_rkind_comp - (6763.22_rkind_comp / t) - (4.210_rkind_comp * log(t)) + &
       (0.000367_rkind_comp * t) + (tanh(0.0415_rkind_comp * (t - 218.8_rkind_comp)) * &
       (53.878_rkind_comp - (1331.22_rkind_comp / t) - (9.44523_rkind_comp * log(t)) + &
       0.014025_rkind_comp * t)))

end subroutine MurphyKoop_svp_water

subroutine MurphyKoop_svp_ice(t, es)
  !$acc routine vector

  real(rkind_comp), intent(in) :: t  ! Temperature in Kelvin
  real(rkind_comp) :: es             ! SVP in Pa
  ! (good down to 110 K)

  es = exp(9.550426_rkind_comp - (5723.265_rkind_comp / t) + (3.53068_rkind_comp * log(t)) &
       - (0.00728332_rkind_comp * t))

end subroutine MurphyKoop_svp_ice
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


subroutine OldGoffGratch_svp_water(t,es)
  !$acc routine vector

  real(rkind_comp), intent(in)  :: t
  real(rkind_comp), intent(out) :: es
  real(rkind_comp) :: ps, e1, e2, f1, f2, f3, f4, f5, f

  ps = 1013.246_rkind_comp
  e1 = 11.344_rkind_comp*(1.0_rkind_comp - t/tboil)
  e2 = -3.49149_rkind_comp*(tboil/t - 1.0_rkind_comp)
  f1 = -7.90298_rkind_comp*(tboil/t - 1.0_rkind_comp)
  f2 = 5.02808_rkind_comp*log10(tboil/t)
  f3 = -1.3816_rkind_comp*(10.0_rkind_comp**e1 - 1.0_rkind_comp)/10000000.0_rkind_comp
  f4 = 8.1328_rkind_comp*(10.0_rkind_comp**e2 - 1.0_rkind_comp)/1000.0_rkind_comp
  f5 = log10(ps)
  f  = f1 + f2 + f3 + f4 + f5

  es = (10.0_rkind_comp**f)*100.0_rkind_comp
  
end subroutine OldGoffGratch_svp_water

subroutine OldGoffGratch_svp_ice(t,es)
  !$acc routine vector

  real(rkind_comp), intent(in) :: t
  real(rkind_comp), intent(out) :: es
  real(rkind_comp) :: term1, term2, term3

  term1 = 2.01889049_rkind_comp/(tmelt/t)
  term2 = 3.56654_rkind_comp*log(tmelt/t)
  term3 = 20.947031_rkind_comp*(tmelt/t)

  es = 575.185606e10_rkind_comp*exp(-(term1 + term2 + term3))
  
end subroutine OldGoffGratch_svp_ice
! Bolton (1980)
! zm_conv deep convection scheme contained this SVP calculation.
! It appears to be from D. Bolton, 1980, Monthly Weather Review.
! Unlike the other schemes, no distinct ice formula is associated
! with it. (However, a Bolton ice formula exists in CLUBB.)
! The original formula used degrees C, but this function
! takes Kelvin and internally converts.


subroutine Bolton_svp_water(t, es)
  !$acc routine vector

  real(rkind_comp),parameter :: c1 = 611.2_rkind_comp
  real(rkind_comp),parameter :: c2 = 17.67_rkind_comp
  real(rkind_comp),parameter :: c3 = 243.5_rkind_comp

  real(rkind_comp), intent(in)  :: t  ! Temperature in Kelvin
  real(rkind_comp), intent(out) :: es             ! SVP in Pa

  es = c1*exp( (c2*(t - tmelt))/((t - tmelt)+c3) )

end subroutine Bolton_svp_water

!read state subroutine for kr_externs_in_wv_sat_methods 
SUBROUTINE kr_externs_in_wv_sat_methods(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    REAL(kind=rkind_io) :: tmp
      
    READ (UNIT = kgen_unit) tmp; tmelt   = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_wv_sat_methods: tboil: ',tmelt
    READ (UNIT = kgen_unit) tmp; h2otrip = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_wv_sat_methods: h2otrip: ',h2otrip
    READ (UNIT = kgen_unit) tmp; tboil   = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_wv_sat_methods: tboil: ',tboil
    READ (UNIT = kgen_unit) tmp; epsilo  = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_wv_sat_methods: epsilo: ',epsilo
    READ (UNIT = kgen_unit) tmp; omeps   = real(tmp,kind=rkind_comp)
!    print *,'kr_externs_in_wv_sat_methods: omeps: ',omeps
    READ (UNIT = kgen_unit) default_idx 
!    print *,'kr_externs_in_wv_sat_methods: default_idx: ',default_idx
END SUBROUTINE kr_externs_in_wv_sat_methods 
  
!read state subroutine for kr_externs_out_wv_sat_methods 
SUBROUTINE kr_externs_out_wv_sat_methods(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_wv_sat_methods 
  
end module wv_sat_methods
