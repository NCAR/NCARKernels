
! KGEN-generated Fortran source file
!
! Filename    : wv_sat_methods.F90
! Generated at: 2015-10-08 11:52:41
! KGEN version: 0.5.2



    MODULE wv_sat_methods
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        ! This portable module contains all CAM methods for estimating
        ! the saturation vapor pressure of water.
        !
        ! wv_saturation provides CAM-specific interfaces and utilities
        ! based on these formulae.
        !
        ! Typical usage of this module:
        !
        ! Init:
<<<<<<< HEAD
        ! call wv_sat_methods_init(r8, <constants>, errstring)
=======
        ! call wv_sat_methods_init(rkind_comp, <constants>, errstring)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
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
<<<<<<< HEAD
        IMPLICIT NONE
        PRIVATE
        INTEGER, parameter :: r8 = selected_real_kind(12) ! 8 byte real
        REAL(KIND=r8) :: tmelt ! Melting point of water at 1 atm (K)
        REAL(KIND=r8) :: h2otrip ! Triple point temperature of water (K)
        REAL(KIND=r8) :: tboil ! Boiling point of water at 1 atm (K)
        ! Ice-water transition range
        REAL(KIND=r8) :: epsilo ! Ice-water transition range
        REAL(KIND=r8) :: omeps ! 1._r8 - epsilo
=======
        USE shr_kind_mod, ONLY: rkind_comp, rkind_io
        IMPLICIT NONE
        PRIVATE
        REAL(KIND=rkind_comp) :: tmelt ! Melting point of water at 1 atm (K)
        REAL(KIND=rkind_comp) :: h2otrip ! Triple point temperature of water (K)
        REAL(KIND=rkind_comp) :: tboil ! Boiling point of water at 1 atm (K)
        ! Ice-water transition range
        REAL(KIND=rkind_comp) :: epsilo ! Ice-water transition range
        REAL(KIND=rkind_comp) :: omeps ! 1._rkind_comp - epsilo
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        ! Indices representing individual schemes
        INTEGER, parameter :: oldgoffgratch_idx = 0
        INTEGER, parameter :: goffgratch_idx = 1
        INTEGER, parameter :: murphykoop_idx = 2
        INTEGER, parameter :: bolton_idx = 3
        ! Index representing the current default scheme.
        INTEGER, parameter :: initial_default_idx = goffgratch_idx
        INTEGER :: default_idx = initial_default_idx
        PUBLIC wv_sat_svp_water
        PUBLIC wv_sat_svp_ice
        ! pressure -> humidity conversion
        PUBLIC wv_sat_svp_to_qsat
<<<<<<< HEAD

        ! Combined qsat operations
        INTERFACE wv_sat_qsat_water
            MODULE PROCEDURE wv_sat_qsat_water_r8
            MODULE PROCEDURE wv_sat_qsat_water_v8
        END INTERFACE wv_sat_qsat_water
        PUBLIC wv_sat_qsat_water

        INTERFACE wv_sat_svp_water
            MODULE PROCEDURE wv_sat_svp_water_r8          
            MODULE PROCEDURE wv_sat_svp_water_v8          
        END INTERFACE wv_sat_svp_water

        INTERFACE wv_sat_qsat_ice
            MODULE PROCEDURE wv_sat_qsat_ice_r8
            MODULE PROCEDURE wv_sat_qsat_ice_v8
        END INTERFACE wv_sat_qsat_ice

        INTERFACE wv_sat_svp_ice
            MODULE PROCEDURE wv_sat_svp_ice_r8          
            MODULE PROCEDURE wv_sat_svp_ice_v8          
        END INTERFACE wv_sat_svp_ice

        INTERFACE wv_sat_svp_to_qsat
            MODULE PROCEDURE wv_sat_svp_to_qsat_r8
            MODULE PROCEDURE wv_sat_svp_to_qsat_v8
        END INTERFACE wv_sat_svp_to_qsat

        INTERFACE goffgratch_svp_water
            MODULE PROCEDURE goffgratch_svp_water_r8
            MODULE PROCEDURE goffgratch_svp_water_v8
        END INTERFACE goffgratch_svp_water

=======
        ! Combined qsat operations
        PUBLIC wv_sat_qsat_water
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        PUBLIC wv_sat_qsat_ice
            PUBLIC kgen_read_externs_wv_sat_methods
        CONTAINS

        ! write subroutines
        ! No subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_wv_sat_methods(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
<<<<<<< HEAD
            READ(UNIT=kgen_unit) default_idx
            READ(UNIT=kgen_unit) tboil
            READ(UNIT=kgen_unit) tmelt
            READ(UNIT=kgen_unit) epsilo
            READ(UNIT=kgen_unit) omeps
            READ(UNIT=kgen_unit) h2otrip
=======
            REAL(kind=rkind_io) :: rtmp
            READ(UNIT=kgen_unit) default_idx

            READ(UNIT=kgen_unit) rtmp; tboil  = REAL(rtmp,kind=rkind_comp)
            print *,'wv_sat_methods: tboil: ',tboil
            READ(UNIT=kgen_unit) rtmp; tmelt  = REAL(rtmp,kind=rkind_comp)
            READ(UNIT=kgen_unit) rtmp; epsilo = REAL(rtmp,kind=rkind_comp)
            READ(UNIT=kgen_unit) rtmp; omeps  = REAL(rtmp,kind=rkind_comp)
            READ(UNIT=kgen_unit) rtmp; h2otrip  = REAL(rtmp,kind=rkind_comp)
            print *,'wv_sat_methods: h2otrip: ',h2otrip
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        END SUBROUTINE kgen_read_externs_wv_sat_methods

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

<<<<<<< HEAD
        SUBROUTINE wv_sat_svp_to_qsat_r8(es, p, qs)
            REAL(KIND=r8), intent(in) :: es ! SVP
            REAL(KIND=r8), intent(in)  :: p ! Current pressure.
            REAL(KIND=r8), intent(out) :: qs
            ! If pressure is less than SVP, set qs to maximum of 1.
            if ( (p - es) <= 0._r8 ) then
               qs = 1.0_r8
            else
               qs = epsilo*es / (p - omeps*es)
            end if
        END SUBROUTINE wv_sat_svp_to_qsat_r8

        SUBROUTINE wv_sat_svp_to_qsat_v8(es, p, qs,vlen)
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in)  :: es(vlen) ! SVP
            REAL(KIND=r8), intent(in)  :: p(vlen) ! Current pressure.
            REAL(KIND=r8), intent(out) :: qs(vlen)
            INTEGER :: i
            ! If pressure is less than SVP, set qs to maximum of 1.
            do i=1,vlen
               if ( (p(i) - es(i)) <= 0._r8 ) then
                  qs(i) = 1.0_r8
               else
                  qs(i) = epsilo*es(i) / (p(i) - omeps*es(i))
               end if
            enddo
        END SUBROUTINE wv_sat_svp_to_qsat_v8


        SUBROUTINE wv_sat_qsat_water_r8(t, p, es, qs, idx)
            !------------------------------------------------------------------!
            ! Purpose:                                                         !
            !   Calculate SVP over water at a given temperature, and then      !
            !   calculate and return saturation specific humidity.             !
            !------------------------------------------------------------------!
            ! Inputs
            REAL(KIND=r8), intent(in) :: t ! Temperature
            REAL(KIND=r8), intent(in) :: p ! Pressure
            ! Outputs
            REAL(KIND=r8), intent(out) :: es ! Saturation vapor pressure
            REAL(KIND=r8), intent(out) :: qs ! Saturation specific humidity
            INTEGER, intent(in), optional :: idx ! Scheme index
            call wv_sat_svp_water(t, es, idx)
            call wv_sat_svp_to_qsat(es, p, qs)
            ! Ensures returned es is consistent with limiters on qs.
            es = min(es, p)
        END SUBROUTINE wv_sat_qsat_water_r8
        SUBROUTINE wv_sat_qsat_water_v8(t, p, es, qs, vlen,idx)
=======
        elemental FUNCTION wv_sat_svp_to_qsat(es, p) RESULT ( qs )
            REAL(KIND=rkind_comp), intent(in) :: es ! SVP
            REAL(KIND=rkind_comp), intent(in) :: p ! Current pressure.
            REAL(KIND=rkind_comp) :: qs
            ! If pressure is less than SVP, set qs to maximum of 1.
  if ( (p - es) <= 0._rkind_comp ) then
     qs = 1.0_rkind_comp
  else
     qs = epsilo*es / (p - omeps*es)
  end if
        END FUNCTION wv_sat_svp_to_qsat

        elemental SUBROUTINE wv_sat_qsat_water(t, p, es, qs, idx)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            !------------------------------------------------------------------!
            ! Purpose:                                                         !
            !   Calculate SVP over water at a given temperature, and then      !
            !   calculate and return saturation specific humidity.             !
            !------------------------------------------------------------------!
            ! Inputs
<<<<<<< HEAD
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in) :: t(vlen) ! Temperature
            REAL(KIND=r8), intent(in) :: p(vlen) ! Pressure
            ! Outputs
            REAL(KIND=r8), intent(out) :: es(vlen) ! Saturation vapor pressure
            REAL(KIND=r8), intent(out) :: qs(vlen) ! Saturation specific humidity
            INTEGER, intent(in), optional :: idx ! Scheme index
            integer :: i
            call wv_sat_svp_water(t, es, vlen, idx)
            call wv_sat_svp_to_qsat(es, p,qs,vlen)
            do i=1,vlen
               ! Ensures returned es is consistent with limiters on qs.
               es(i) = min(es(i), p(i))
            enddo
        END SUBROUTINE wv_sat_qsat_water_v8

        SUBROUTINE wv_sat_qsat_ice_r8(t, p, es, qs, idx)
            !------------------------------------------------------------------!
            ! Purpose:                                                         !
            !   Calculate SVP over ice at a given temperature, and then        !
            !   calculate and return saturation specific humidity.             !
            !------------------------------------------------------------------!
            ! Inputs
            REAL(KIND=r8), intent(in) :: t ! Temperature
            REAL(KIND=r8), intent(in) :: p ! Pressure
            ! Outputs
            REAL(KIND=r8), intent(out) :: es ! Saturation vapor pressure
            REAL(KIND=r8), intent(out) :: qs ! Saturation specific humidity
            INTEGER, intent(in), optional :: idx ! Scheme index
            call wv_sat_svp_ice(t, es, idx)
            call wv_sat_svp_to_qsat(es, p, qs)
            ! Ensures returned es is consistent with limiters on qs.
            es = min(es, p)
        END SUBROUTINE wv_sat_qsat_ice_r8

        SUBROUTINE wv_sat_qsat_ice_v8(t, p, es, qs, vlen, idx)
=======
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), intent(in) :: p ! Pressure
            ! Outputs
            REAL(KIND=rkind_comp), intent(out) :: es ! Saturation vapor pressure
            REAL(KIND=rkind_comp), intent(out) :: qs ! Saturation specific humidity
            INTEGER, intent(in), optional :: idx ! Scheme index
  es = wv_sat_svp_water(t, idx)
  qs = wv_sat_svp_to_qsat(es, p)
            ! Ensures returned es is consistent with limiters on qs.
  es = min(es, p)
        END SUBROUTINE wv_sat_qsat_water

        elemental SUBROUTINE wv_sat_qsat_ice(t, p, es, qs, idx)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            !------------------------------------------------------------------!
            ! Purpose:                                                         !
            !   Calculate SVP over ice at a given temperature, and then        !
            !   calculate and return saturation specific humidity.             !
            !------------------------------------------------------------------!
            ! Inputs
<<<<<<< HEAD
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in) :: t(vlen) ! Temperature
            REAL(KIND=r8), intent(in) :: p(vlen) ! Pressure
            ! Outputs
            REAL(KIND=r8), intent(out) :: es(vlen) ! Saturation vapor pressure
            REAL(KIND=r8), intent(out) :: qs(vlen) ! Saturation specific humidity
            INTEGER, intent(in), optional :: idx ! Scheme index
            INTEGER :: i
            call wv_sat_svp_ice(t, es, vlen, idx)
            call wv_sat_svp_to_qsat(es, p, qs,vlen)
            ! Ensures returned es is consistent with limiters on qs.
            do i=1,vlen
               es(i) = min(es(i), p(i))
            enddo
        END SUBROUTINE wv_sat_qsat_ice_v8
=======
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), intent(in) :: p ! Pressure
            ! Outputs
            REAL(KIND=rkind_comp), intent(out) :: es ! Saturation vapor pressure
            REAL(KIND=rkind_comp), intent(out) :: qs ! Saturation specific humidity
            INTEGER, intent(in), optional :: idx ! Scheme index
  es = wv_sat_svp_ice(t, idx)
  qs = wv_sat_svp_to_qsat(es, p)
            ! Ensures returned es is consistent with limiters on qs.
  es = min(es, p)
        END SUBROUTINE wv_sat_qsat_ice
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a

        !---------------------------------------------------------------------
        ! SVP INTERFACE FUNCTIONS
        !---------------------------------------------------------------------

<<<<<<< HEAD
        SUBROUTINE wv_sat_svp_water_r8(t, es, idx)
            REAL(KIND=r8), intent(in) :: t
            INTEGER, intent(in), optional :: idx
            REAL(KIND=r8) :: es
            INTEGER :: use_idx
            if (present(idx)) then
               use_idx = idx
            else
               use_idx = default_idx
            end if
            select case (use_idx)
                CASE ( goffgratch_idx )
                    call GoffGratch_svp_water(t,es)
                CASE ( murphykoop_idx )
                    es = MurphyKoop_svp_water(t)
                CASE ( oldgoffgratch_idx )
                    es = OldGoffGratch_svp_water(t)
                CASE ( bolton_idx )
                    es = Bolton_svp_water(t)
           end select
        END SUBROUTINE wv_sat_svp_water_r8

        SUBROUTINE wv_sat_svp_water_v8(t, es, vlen, idx)
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in)  :: t(vlen)
            REAL(KIND=r8), intent(out) :: es(vlen)
             
            INTEGER, intent(in), optional :: idx
            INTEGER :: use_idx
            INTEGER :: i
            if (present(idx)) then
               use_idx = idx
            else
               use_idx = default_idx
            end if
            select case (use_idx)
                CASE ( goffgratch_idx )
                    call GoffGratch_svp_water(t,es,vlen)
                CASE ( murphykoop_idx )
                    do i=1,vlen
                       es(i) = MurphyKoop_svp_water(t(i))
                    enddo
                CASE ( oldgoffgratch_idx )
                    do i=1,vlen
                       es(i) = OldGoffGratch_svp_water(t(i))
                    enddo
                CASE ( bolton_idx )
                    do i=1,vlen
                       es(i) = Bolton_svp_water(t(i))
                    enddo
           end select
        END SUBROUTINE wv_sat_svp_water_v8


        SUBROUTINE wv_sat_svp_ice_r8(t, es, idx)
            REAL(KIND=r8), intent(in)  :: t
            REAL(KIND=r8), intent(out) :: es
            INTEGER, intent(in), optional :: idx
            INTEGER :: use_idx
            if (present(idx)) then
               use_idx = idx
            else
               use_idx = default_idx
            end if
            select case (use_idx)
               CASE ( goffgratch_idx )
                  es = GoffGratch_svp_ice(t)
               CASE ( murphykoop_idx )
                  es = MurphyKoop_svp_ice(t)
               CASE ( oldgoffgratch_idx )
                  es = OldGoffGratch_svp_ice(t)
               CASE ( bolton_idx )
                  es = Bolton_svp_water(t)
            end select
        END SUBROUTINE wv_sat_svp_ice_r8

        SUBROUTINE wv_sat_svp_ice_v8(t, es, vlen, idx)
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in)  :: t(vlen)
            REAL(KIND=r8), intent(out) :: es(vlen)
            INTEGER, intent(in), optional :: idx
            INTEGER :: use_idx
            INTEGER :: i
            if (present(idx)) then
               use_idx = idx
            else
               use_idx = default_idx
            end if
            select case (use_idx)
               CASE ( goffgratch_idx )
                  do i=1,vlen
                    es(i) = GoffGratch_svp_ice(t(i))
                  enddo
               CASE ( murphykoop_idx )
                  do i=1,vlen
                    es(i) = MurphyKoop_svp_ice(t(i))
                  enddo
               CASE ( oldgoffgratch_idx )
                  do i=1,vlen
                    es(i) = OldGoffGratch_svp_ice(t(i))
                  enddo
               CASE ( bolton_idx )
                  do i=1,vlen
                    es(i) = Bolton_svp_water(t(i))
                  enddo
            end select
        END SUBROUTINE wv_sat_svp_ice_v8

=======
        elemental FUNCTION wv_sat_svp_water(t, idx) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t
            INTEGER, intent(in), optional :: idx
            REAL(KIND=rkind_comp) :: es
            INTEGER :: use_idx
  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if
  select case (use_idx)
                CASE ( goffgratch_idx )
     es = GoffGratch_svp_water(t)
                CASE ( murphykoop_idx )
     es = MurphyKoop_svp_water(t)
                CASE ( oldgoffgratch_idx )
     es = OldGoffGratch_svp_water(t)
                CASE ( bolton_idx )
     es = Bolton_svp_water(t)
  end select
        END FUNCTION wv_sat_svp_water

        elemental FUNCTION wv_sat_svp_ice(t, idx) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t
            INTEGER, intent(in), optional :: idx
            REAL(KIND=rkind_comp) :: es
            INTEGER :: use_idx
  if (present(idx)) then
     use_idx = idx
  else
     use_idx = default_idx
  end if
  select case (use_idx)
                CASE ( goffgratch_idx )
     es = GoffGratch_svp_ice(t)
                CASE ( murphykoop_idx )
     es = MurphyKoop_svp_ice(t)
                CASE ( oldgoffgratch_idx )
     es = OldGoffGratch_svp_ice(t)
                CASE ( bolton_idx )
     es = Bolton_svp_water(t)
  end select
        END FUNCTION wv_sat_svp_ice
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a

        !---------------------------------------------------------------------
        ! SVP METHODS
        !---------------------------------------------------------------------
        ! Goff & Gratch (1946)

<<<<<<< HEAD
        SUBROUTINE goffgratch_svp_water_r8(t,es)
            REAL(KIND=r8), intent(in)  :: t ! Temperature in Kelvin
            REAL(KIND=r8), intent(out) :: es ! SVP in Pa
            ! uncertain below -70 C
  es = 10._r8**(-7.90298_r8*(tboil/t-1._r8)+ &
       5.02808_r8*log10(tboil/t)- &
       1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/tboil))-1._r8)+ &
       8.1328e-3_r8*(10._r8**(-3.49149_r8*(tboil/t-1._r8))-1._r8)+ &
       log10(1013.246_r8))*100._r8
        END subroutine goffgratch_svp_water_r8
        

        SUBROUTINE goffgratch_svp_water_v8(t,es,vlen)
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in)  :: t(vlen)  ! Temperature in Kelvin
            REAL(KIND=r8), intent(out) :: es(vlen) ! SVP in Pa
            INTEGER :: i
            ! uncertain below -70 C

        do i=1,vlen
          es(i) = 10._r8**(-7.90298_r8*(tboil/t(i)-1._r8)+ &
               5.02808_r8*log10(tboil/t(i))- &
               1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t(i)/tboil))-1._r8)+ &
               8.1328e-3_r8*(10._r8**(-3.49149_r8*(tboil/t(i)-1._r8))-1._r8)+ &
               log10(1013.246_r8))*100._r8
        enddo
        END subroutine goffgratch_svp_water_v8
       

        FUNCTION goffgratch_svp_ice(t) RESULT ( es )
            REAL(KIND=r8), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=r8) :: es ! SVP in Pa
            ! good down to -100 C
  es = 10._r8**(-9.09718_r8*(h2otrip/t-1._r8)-3.56654_r8* &
       log10(h2otrip/t)+0.876793_r8*(1._r8-t/h2otrip)+ &
       log10(6.1071_r8))*100._r8
        END FUNCTION goffgratch_svp_ice
        ! Murphy & Koop (2005)

        FUNCTION murphykoop_svp_water(t) RESULT ( es )
            REAL(KIND=r8), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=r8) :: es ! SVP in Pa
            ! (good for 123 < T < 332 K)
  es = exp(54.842763_r8 - (6763.22_r8 / t) - (4.210_r8 * log(t)) + &
       (0.000367_r8 * t) + (tanh(0.0415_r8 * (t - 218.8_r8)) * &
       (53.878_r8 - (1331.22_r8 / t) - (9.44523_r8 * log(t)) + &
       0.014025_r8 * t)))
        END FUNCTION murphykoop_svp_water

        FUNCTION murphykoop_svp_ice(t) RESULT ( es )
            REAL(KIND=r8), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=r8) :: es ! SVP in Pa
            ! (good down to 110 K)
  es = exp(9.550426_r8 - (5723.265_r8 / t) + (3.53068_r8 * log(t)) &
       - (0.00728332_r8 * t))
=======
        elemental FUNCTION goffgratch_svp_water(t) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=rkind_comp) :: es ! SVP in Pa
            ! uncertain below -70 C
  es = 10._rkind_comp**(-7.90298_rkind_comp*(tboil/t-1._rkind_comp)+ &
       5.02808_rkind_comp*log10(tboil/t)- &
       1.3816e-7_rkind_comp*(10._rkind_comp**(11.344_rkind_comp*(1._rkind_comp-t/tboil))-1._rkind_comp)+ &
       8.1328e-3_rkind_comp*(10._rkind_comp**(-3.49149_rkind_comp*(tboil/t-1._rkind_comp))-1._rkind_comp)+ &
       log10(1013.246_rkind_comp))*100._rkind_comp
        END FUNCTION goffgratch_svp_water

        elemental FUNCTION goffgratch_svp_ice(t) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=rkind_comp) :: es ! SVP in Pa
            ! good down to -100 C
  es = 10._rkind_comp**(-9.09718_rkind_comp*(h2otrip/t-1._rkind_comp)-3.56654_rkind_comp* &
       log10(h2otrip/t)+0.876793_rkind_comp*(1._rkind_comp-t/h2otrip)+ &
       log10(6.1071_rkind_comp))*100._rkind_comp
        END FUNCTION goffgratch_svp_ice
        ! Murphy & Koop (2005)

        elemental FUNCTION murphykoop_svp_water(t) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=rkind_comp) :: es ! SVP in Pa
            ! (good for 123 < T < 332 K)
  es = exp(54.842763_rkind_comp - (6763.22_rkind_comp / t) - (4.210_rkind_comp * log(t)) + &
       (0.000367_rkind_comp * t) + (tanh(0.0415_rkind_comp * (t - 218.8_rkind_comp)) * &
       (53.878_rkind_comp - (1331.22_rkind_comp / t) - (9.44523_rkind_comp * log(t)) + &
       0.014025_rkind_comp * t)))
        END FUNCTION murphykoop_svp_water

        elemental FUNCTION murphykoop_svp_ice(t) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=rkind_comp) :: es ! SVP in Pa
            ! (good down to 110 K)
  es = exp(9.550426_rkind_comp - (5723.265_rkind_comp / t) + (3.53068_rkind_comp * log(t)) &
       - (0.00728332_rkind_comp * t))
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        END FUNCTION murphykoop_svp_ice
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

<<<<<<< HEAD
        FUNCTION oldgoffgratch_svp_water(t) RESULT ( es )
            REAL(KIND=r8), intent(in) :: t
            REAL(KIND=r8) :: es
            REAL(KIND=r8) :: ps
            REAL(KIND=r8) :: e1
            REAL(KIND=r8) :: e2
            REAL(KIND=r8) :: f1
            REAL(KIND=r8) :: f2
            REAL(KIND=r8) :: f3
            REAL(KIND=r8) :: f4
            REAL(KIND=r8) :: f5
            REAL(KIND=r8) :: f
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
        END FUNCTION oldgoffgratch_svp_water

        FUNCTION oldgoffgratch_svp_ice(t) RESULT ( es )
            REAL(KIND=r8), intent(in) :: t
            REAL(KIND=r8) :: es
            REAL(KIND=r8) :: term1
            REAL(KIND=r8) :: term2
            REAL(KIND=r8) :: term3
  term1 = 2.01889049_r8/(tmelt/t)
  term2 = 3.56654_r8*log(tmelt/t)
  term3 = 20.947031_r8*(tmelt/t)
  es = 575.185606e10_r8*exp(-(term1 + term2 + term3))
=======
        elemental FUNCTION oldgoffgratch_svp_water(t) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t
            REAL(KIND=rkind_comp) :: es
            REAL(KIND=rkind_comp) :: ps
            REAL(KIND=rkind_comp) :: e1
            REAL(KIND=rkind_comp) :: e2
            REAL(KIND=rkind_comp) :: f1
            REAL(KIND=rkind_comp) :: f2
            REAL(KIND=rkind_comp) :: f3
            REAL(KIND=rkind_comp) :: f4
            REAL(KIND=rkind_comp) :: f5
            REAL(KIND=rkind_comp) :: f
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
        END FUNCTION oldgoffgratch_svp_water

        elemental FUNCTION oldgoffgratch_svp_ice(t) RESULT ( es )
            REAL(KIND=rkind_comp), intent(in) :: t
            REAL(KIND=rkind_comp) :: es
            REAL(KIND=rkind_comp) :: term1
            REAL(KIND=rkind_comp) :: term2
            REAL(KIND=rkind_comp) :: term3
  term1 = 2.01889049_rkind_comp/(tmelt/t)
  term2 = 3.56654_rkind_comp*log(tmelt/t)
  term3 = 20.947031_rkind_comp*(tmelt/t)
  es = 575.185606e10_rkind_comp*exp(-(term1 + term2 + term3))
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        END FUNCTION oldgoffgratch_svp_ice
        ! Bolton (1980)
        ! zm_conv deep convection scheme contained this SVP calculation.
        ! It appears to be from D. Bolton, 1980, Monthly Weather Review.
        ! Unlike the other schemes, no distinct ice formula is associated
        ! with it. (However, a Bolton ice formula exists in CLUBB.)
        ! The original formula used degrees C, but this function
        ! takes Kelvin and internally converts.

<<<<<<< HEAD
        FUNCTION bolton_svp_water(t) RESULT ( es )
            REAL(KIND=r8), parameter :: c1 = 611.2_r8
            REAL(KIND=r8), parameter :: c2 = 17.67_r8
            REAL(KIND=r8), parameter :: c3 = 243.5_r8
            REAL(KIND=r8), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=r8) :: es ! SVP in Pa
=======
        elemental FUNCTION bolton_svp_water(t) RESULT ( es )
            REAL(KIND=rkind_comp), parameter :: c1 = 611.2_rkind_comp
            REAL(KIND=rkind_comp), parameter :: c2 = 17.67_rkind_comp
            REAL(KIND=rkind_comp), parameter :: c3 = 243.5_rkind_comp
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature in Kelvin
            REAL(KIND=rkind_comp) :: es ! SVP in Pa
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  es = c1*exp( (c2*(t - tmelt))/((t - tmelt)+c3) )
        END FUNCTION bolton_svp_water
    END MODULE wv_sat_methods
