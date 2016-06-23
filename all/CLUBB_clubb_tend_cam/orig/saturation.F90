!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:50:01 
!KGEN version : 0.7.0 
  
!-------------------------------------------------------------------------
!$Id: saturation.F90 6849 2014-04-22 21:52:30Z charlass@uwm.edu $
!===============================================================================
module saturation

! Description:
!   Contains functions that compute saturation with respect
!   to liquid or ice.
!-----------------------------------------------------------------------






    USE clubb_precision, ONLY: core_rknd 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    IMPLICIT NONE 

    PRIVATE 

    PUBLIC sat_mixrat_liq, sat_mixrat_liq_lookup, sat_mixrat_ice, sat_vapor_press_liq 

    PRIVATE sat_vapor_press_liq_flatau, sat_vapor_press_liq_bolton 
    PRIVATE sat_vapor_press_ice_flatau, sat_vapor_press_ice_bolton 

  ! Lookup table of values for saturation 
  real( kind = core_rknd ), private, dimension(188:343) :: &
    svp_liq_lookup_table


!$omp threadprivate(svp_liq_lookup_table)

  PUBLIC kr_externs_in_saturation 
  PUBLIC kr_externs_out_saturation 
  contains

!-------------------------------------------------------------------------
  elemental real( kind = core_rknd ) function sat_mixrat_liq( p_in_Pa, T_in_K )

! Description:
!   Used to compute the saturation mixing ratio of liquid water.

! References:
!   Formula from Emanuel 1994, 4.4.14
!-------------------------------------------------------------------------

      USE constants_clubb, ONLY: ep 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) ::  & 
      p_in_Pa,  & ! Pressure    [Pa]
      T_in_K      ! Temperature [K]

   ! Local Variables
    real( kind = core_rknd ) :: esatv

    ! --- Begin Code ---

    ! Calculate the SVP for water vapor.
    esatv = sat_vapor_press_liq( T_in_K )

    ! If esatv exceeds the air pressure, then assume esatv~=0.5*pressure 
    !   and set rsat = ep = 0.622
    if ( p_in_Pa-esatv < 1.0_core_rknd ) then
      sat_mixrat_liq = ep
    else

    ! Formula for Saturation Mixing Ratio:
    !
    ! rs = (epsilon) * [ esat / ( p - esat ) ];
    ! where epsilon = R_d / R_v
    sat_mixrat_liq = ep * ( esatv / ( p_in_Pa - esatv ) )


    end if

    return
  end function sat_mixrat_liq

!-------------------------------------------------------------------------
  elemental real( kind = core_rknd ) function sat_mixrat_liq_lookup( p_in_Pa, T_in_K )

! Description:
!   Used to compute the saturation mixing ratio of liquid water.
!   This function utilizes sat_vapor_press_liq_lookup; the SVP is found
!   using a lookup table rather than calculating it using various
!   approximations.

! References:
!   Formula from Emanuel 1994, 4.4.14
!-------------------------------------------------------------------------

      USE constants_clubb, ONLY: ep 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) ::  & 
      p_in_Pa,  & ! Pressure    [Pa]
      T_in_K      ! Temperature [K]

   ! Local Variables
    real( kind = core_rknd ) :: esatv

    ! --- Begin Code ---

    ! Calculate the SVP for water vapor using a lookup table.
    esatv = sat_vapor_press_liq_lookup( T_in_K )

    ! If esatv exceeds the air pressure, then assume esatv~=0.5*pressure 
    !   and set rsat = ep = 0.622
    if ( p_in_Pa-esatv < 1.0_core_rknd ) then
      sat_mixrat_liq_lookup = ep
    else

    ! Formula for Saturation Mixing Ratio:
    !
    ! rs = (epsilon) * [ esat / ( p - esat ) ];
    ! where epsilon = R_d / R_v
    sat_mixrat_liq_lookup = ep * ( esatv / ( p_in_Pa - esatv ) )


    end if

    return
  end function sat_mixrat_liq_lookup

!-----------------------------------------------------------------
  elemental function sat_vapor_press_liq( T_in_K ) result ( esat )

! Description:
!   Computes SVP for water vapor. Calls one of the other functions
!   that calculate an approximation to SVP.

! References:
!   None

      USE model_flags, ONLY: saturation_formula, saturation_bolton, saturation_gfdl, saturation_flatau 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K     ! Temperature                          [K]

    ! Output Variables
    real( kind = core_rknd ) :: esat      ! Saturation Vapor Pressure over Water [Pa]

    ! Undefined approximation
    esat = -99999.999_core_rknd

    ! Saturation Vapor Pressure, esat, can be found to be approximated
    ! in many different ways.
    select case ( saturation_formula )
    case ( saturation_bolton )
      ! Using the Bolton 1980 approximations for SVP over vapor
      esat = sat_vapor_press_liq_bolton( T_in_K )

    case ( saturation_flatau )
      ! Using the Flatau, et al. polynomial approximation for SVP over vapor
      esat = sat_vapor_press_liq_flatau( T_in_K )

! ---> h1g
    case ( saturation_gfdl )
      ! Using GFDL polynomial approximation for SVP with respect to liquid
      esat = sat_vapor_press_liq_gfdl( T_in_K )
! <--- h1g

      ! Add new cases after this

    end select

    return

  end function sat_vapor_press_liq

!------------------------------------------------------------------------
  elemental function sat_vapor_press_liq_lookup( T_in_K ) result ( esat )

! Description:
!   Computes SVP for water vapor, using a lookup table.
!
!   The lookup table was constructed using the Flatau approximation.

! References:
!   ``Polynomial Fits to Saturation Vapor Pressure'' Falatau, Walko,
!     and Cotton.  (1992)  Journal of Applied Meteorology, Vol. 31,
!     pp. 1507--1513
!------------------------------------------------------------------------

    implicit none

    ! External
    intrinsic :: max, min, int, anint

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [K]

    ! Output Variables
    real( kind = core_rknd ) :: esat  ! Saturation vapor pressure over water [Pa]

    ! Local Variables
    integer :: T_in_K_int

    ! ---- Begin Code ----

    T_in_K_int = int( anint( T_in_K ) )

    ! Since this approximation is only good out to -85 degrees Celsius we
    ! truncate the result here
    T_in_K_int = min( max( T_in_K_int, 188 ), 343 )

    ! Use the lookup table to determine the saturation vapor pressure.
    esat = svp_liq_lookup_table( T_in_K_int )

    return
  end function sat_vapor_press_liq_lookup

!------------------------------------------------------------------------
  elemental function sat_vapor_press_liq_flatau( T_in_K ) result ( esat )

! Description:
!   Computes SVP for water vapor.

! References:
!   ``Polynomial Fits to Saturation Vapor Pressure'' Falatau, Walko,
!     and Cotton.  (1992)  Journal of Applied Meteorology, Vol. 31,
!     pp. 1507--1513
!------------------------------------------------------------------------

      USE constants_clubb, ONLY: t_freeze_k 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Constant parameters

    ! Relative error norm expansion (-50 to 50 deg_C) from
    ! Table 3 of pp. 1510 of Flatau et al. 1992 (Water Vapor)
    ! (The 100 coefficient converts from mb to Pa)
!   real, dimension(7), parameter :: a = & 
!   100.* (/ 6.11176750,      0.443986062,     0.143053301E-01, & 
!            0.265027242E-03, 0.302246994E-05, 0.203886313E-07, & 
!            0.638780966E-10 /)

    ! Relative error norm expansion (-85 to 70 deg_C) from
    ! Table 4 of pp. 1511 of Flatau et al.
    real( kind = core_rknd ), dimension(9), parameter :: a = & 
    100._core_rknd * &
             (/ 6.11583699_core_rknd,      0.444606896_core_rknd,     0.143177157E-01_core_rknd, &
             0.264224321E-03_core_rknd, 0.299291081E-05_core_rknd, 0.203154182E-07_core_rknd, & 
             0.702620698E-10_core_rknd, 0.379534310E-13_core_rknd,-0.321582393E-15_core_rknd /)

    real( kind = core_rknd ), parameter :: min_T_in_C = -85._core_rknd ! [deg_C]

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [K]

    ! Output Variables
    real( kind = core_rknd ) :: esat  ! Saturation vapor pressure over water [Pa]

    ! Local Variables
    real( kind = core_rknd ) :: T_in_C
!   integer :: i ! Loop index

    ! ---- Begin Code ----

    ! Determine deg K - 273.15
    T_in_C = T_in_K - T_freeze_K

    ! Since this approximation is only good out to -85 degrees Celsius we
    ! truncate the result here (Flatau, et al. 1992)
    T_in_C = max( T_in_C, min_T_in_C )

    ! Polynomial approx. (Flatau, et al. 1992)

    ! This is the generalized formula but is not computationally efficient.
    ! Based on Wexler's expressions(2.1)-(2.4) (See Flatau et al. p 1508)
    ! e_{sat} = a_1 + a_2 ( T - T_0 ) + ... + a_{n+1} ( T - T_0 )^n

!   esat = a(1)

!   do i = 2, size( a ) , 1
!     esat = esat + a(i) * ( T_in_C )**(i-1)
!   end do

    ! The 8th order polynomial fit.  When running deep 
    ! convective cases I noticed that absolute temperature often dips below
    ! -50 deg_C at higher altitudes, where the 6th order approximation is
    ! not accurate.  -dschanen 20 Nov 2008
    esat = a(1) + T_in_C*( a(2) + T_in_C*( a(3) + T_in_C*( a(4) + T_in_C &
    *( a(5) + T_in_C*( a(6) + T_in_C*( a(7) + T_in_C*( a(8) + T_in_C*( a(9) ) ) ) ) ) ) ) )

    return
  end function sat_vapor_press_liq_flatau


!------------------------------------------------------------------------
  elemental function sat_vapor_press_liq_bolton( T_in_K ) result ( esat )
! Description:
!   Computes SVP for water vapor.
! References:
!   Bolton 1980
!------------------------------------------------------------------------

      USE constants_clubb, ONLY: t_freeze_k 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! External
    intrinsic :: exp

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [K]

    ! Output Variables
    real( kind = core_rknd ) :: esat  ! Saturation vapor pressure over water [Pa]

    ! (Bolton 1980) approx.
    ! Generally this more computationally expensive than the Flatau polnomial expansion
    esat = 611.2_core_rknd * exp( (17.67_core_rknd*(T_in_K-T_freeze_K)) / &
      (T_in_K-29.65_core_rknd) ) ! Known magic number

    return
  end function sat_vapor_press_liq_bolton


! ---> h1g, 2010-06-16
!------------------------------------------------------------------------
  elemental function sat_vapor_press_liq_gfdl( T_in_K ) result ( esat )
! Description:
! copy from "GFDL polysvp.F90" 
!  Compute saturation vapor pressure with respect to liquid  by using 
! function from Goff and Gatch (1946)

!  Polysvp returned in units of pa.
!  T_in_K  is input in units of K.
!------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [K]

    ! Output Variables
    real( kind = core_rknd ) :: esat  ! Saturation vapor pressure over water [Pa]

    ! Goff Gatch equation, uncertain below -70 C
      
         esat = 10._core_rknd**(-7.90298_core_rknd*(373.16_core_rknd/T_in_K-1._core_rknd)+ &
             5.02808_core_rknd*log10(373.16_core_rknd/T_in_K)- &
             1.3816e-7_core_rknd*(10._core_rknd**(11.344_core_rknd &
               *(1._core_rknd-T_in_K/373.16_core_rknd))-1._core_rknd)+ &
             8.1328e-3_core_rknd*(10._core_rknd**(-3.49149_core_rknd &
               *(373.16_core_rknd/T_in_K-1._core_rknd))-1._core_rknd)+ &
             log10(1013.246_core_rknd))*100._core_rknd ! Known magic number

    return
  end function sat_vapor_press_liq_gfdl
! <--- h1g, 2010-06-16

!------------------------------------------------------------------------
  elemental real( kind = core_rknd ) function sat_mixrat_ice( p_in_Pa, T_in_K )

! Description:
!   Used to compute the saturation mixing ratio of ice.

! References:
!   Formula from Emanuel 1994, 4.4.15
!-------------------------------------------------------------------------

      USE constants_clubb, ONLY: ep 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! External
    intrinsic :: trim

    ! Input Variables

    real( kind = core_rknd ), intent(in) :: &
      p_in_Pa, &          ! Pressure [Pa]
      T_in_K              ! Temperature [K]

    ! Local Variables

    real( kind = core_rknd ) :: esat_ice

    ! --- Begin Code ---

    ! Determine the SVP for the given temperature
    esat_ice = sat_vapor_press_ice( T_in_K )

    ! If esat_ice exceeds the air pressure, then assume esat_ice~=0.5*pressure 
    !   and set rsat = ep = 0.622
    if ( p_in_Pa-esat_ice < 1.0_core_rknd ) then
      sat_mixrat_ice = ep
    else

    ! Formula for Saturation Mixing Ratio:
    !
    ! rs = (epsilon) * [ esat / ( p - esat ) ];
    ! where epsilon = R_d / R_v

    sat_mixrat_ice = ep * ( esat_ice / ( p_in_Pa - esat_ice ) )


    end if

    return
  end function sat_mixrat_ice

!------------------------------------------------------------------------
  elemental function sat_vapor_press_ice( T_in_K ) result ( esat_ice )
!
! Description:
!   Computes SVP for ice, using one of the various approximations.
!
! References:
!   None
!------------------------------------------------------------------------
 
      USE model_flags, ONLY: saturation_formula, saturation_bolton, saturation_gfdl, saturation_flatau 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Input Variable
    real( kind = core_rknd ), intent(in) :: &
      T_in_K      ! Temperature     [K]

    ! Output Variable
    real( kind = core_rknd ) :: esat_ice    ! Saturation Vapor Pressure over Ice [Pa]

    ! Undefined approximation
    esat_ice = -99999.999_core_rknd

    select case ( saturation_formula )
    case ( saturation_bolton )
      ! Using the Bolton 1980 approximations for SVP over ice
      esat_ice = sat_vapor_press_ice_bolton( T_in_K )

    case ( saturation_flatau )
      ! Using the Flatau, et al. polynomial approximation for SVP over ice
      esat_ice = sat_vapor_press_ice_flatau( T_in_K )

! ---> h1g, 2010-06-16
    case ( saturation_gfdl )
      ! Using GFDL polynomial approximation for SVP with respect to ice
      esat_ice = sat_vapor_press_ice_gfdl( T_in_K )
! <--- h1g, 2010-06-16

      ! Add new cases after this

    end select

    return

  end function sat_vapor_press_ice

!------------------------------------------------------------------------
  elemental function sat_vapor_press_ice_flatau( T_in_K ) result ( esati )
!
! Description:
!   Computes SVP for ice.
!
! References:
!   ``Polynomial Fits to Saturation Vapor Pressure'' Falatau, Walko,
!     and Cotton.  (1992)  Journal of Applied Meteorology, Vol. 31,
!     pp. 1507--1513
!------------------------------------------------------------------------
      USE constants_clubb, ONLY: t_freeze_k 

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! External
    intrinsic :: max

    ! Relative error norm expansion (-90 to 0 deg_C) from
    ! Table 4 of pp. 1511 of Flatau et al. 1992 (Ice)
    real( kind = core_rknd ), dimension(9), parameter :: a = & 
    100._core_rknd * (/ 6.09868993_core_rknd, 0.499320233_core_rknd, 0.184672631E-01_core_rknd, &
              0.402737184E-03_core_rknd, 0.565392987E-05_core_rknd, 0.521693933E-07_core_rknd, &
              0.307839583E-09_core_rknd, 0.105785160E-11_core_rknd, 0.161444444E-14_core_rknd /)

    real( kind = core_rknd ), parameter :: min_T_in_C = -90._core_rknd ! [deg_C]


    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [deg_K]

    ! Output Variables
    real( kind = core_rknd ) :: esati  ! Saturation vapor pressure over ice [Pa]

    ! Local Variables
    real( kind = core_rknd ) :: T_in_C ! Temperature [deg_C]
!   integer :: i

    ! ---- Begin Code ----

    ! Determine deg K - 273.15
    T_in_C = T_in_K - T_freeze_K

    ! Since this approximation is only good out to -90 degrees Celsius we
    ! truncate the result here (Flatau, et al. 1992)
    T_in_C = max( T_in_C, min_T_in_C )

    ! Polynomial approx. (Flatau, et al. 1992)
!   esati = a(1)

!   do i = 2, size( a ), 1
!     esati = esati + a(i) * ( T_in_C )**(i-1)
!   end do

    esati = a(1) + T_in_C*( a(2) + T_in_C*( a(3) + T_in_C*( a(4) + T_in_C &
    *( a(5) + T_in_C*( a(6) + T_in_C*( a(7) + T_in_C*( a(8) + T_in_C*( a(9) ) ) ) ) ) ) ) )

    return

  end function sat_vapor_press_ice_flatau

!------------------------------------------------------------------------
  elemental function sat_vapor_press_ice_bolton( T_in_K ) result ( esati )
!
! Description:
!   Computes SVP for ice.
!
! References:
!   Bolton 1980
!------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! External
    intrinsic :: exp, log

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [K]

    ! Output Variables
    real( kind = core_rknd ) :: esati  ! Saturation vapor pressure over ice [Pa]

    ! Exponential approx.
    esati = 100.0_core_rknd * exp( 23.33086_core_rknd - &
      (6111.72784_core_rknd/T_in_K) + (0.15215_core_rknd*log( T_in_K )) )

    return

  end function sat_vapor_press_ice_bolton


! ---> h1g, 2010-06-16
!------------------------------------------------------------------------
  elemental function sat_vapor_press_ice_gfdl( T_in_K ) result ( esati )
! Description:
! copy from "GFDL polysvp.F90" 
!  Compute saturation vapor pressure with respect to liquid  by using 
! function from Goff and Gatch (1946)
! 
!  Polysvp returned in units of pa.
!  T_in_K is input in units of K.
!------------------------------------------------------------------------
 
      USE clubb_precision, ONLY: core_rknd 

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: T_in_K   ! Temperature   [K]

    ! Output Variables
    real( kind = core_rknd ) :: esati  ! Saturation vapor pressure over ice [Pa]

    ! Goff Gatch equation (good down to -100 C)

    esati = 10._core_rknd**(-9.09718_core_rknd* &
            (273.16_core_rknd/T_in_k-1._core_rknd)-3.56654_core_rknd* &
          log10(273.16_core_rknd/T_in_k)+0.876793_core_rknd* &
            (1._core_rknd-T_in_k/273.16_core_rknd)+ &
          log10(6.1071_core_rknd))*100._core_rknd ! Known magic number

    return

  end function sat_vapor_press_ice_gfdl
! <--- h1g, 2010-06-16

!-------------------------------------------------------------------------




  !read state subroutine for kr_externs_in_saturation 
  SUBROUTINE kr_externs_in_saturation(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) svp_liq_lookup_table 
          CALL kgen_array_sumcheck("svp_liq_lookup_table", kgen_array_sum, REAL(SUM(svp_liq_lookup_table, &
          &mask=(svp_liq_lookup_table .eq. svp_liq_lookup_table)), 8), .TRUE.) 
      END IF   
  END SUBROUTINE kr_externs_in_saturation 
    
  !read state subroutine for kr_externs_out_saturation 
  SUBROUTINE kr_externs_out_saturation(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
  END SUBROUTINE kr_externs_out_saturation 
    
end module saturation