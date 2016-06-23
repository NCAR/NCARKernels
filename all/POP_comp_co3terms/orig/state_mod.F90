
! KGEN-generated Fortran source file
!
! Filename    : state_mod.F90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11



    MODULE state_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: state_mod
        !
        ! !DESCRIPTION:
        !  This module contains routines necessary for computing the density
        !  from model temperature and salinity using an equation of state.
        !
        !  The module supports four forms of EOS:
        !  \begin{enumerate}
        !     \item The UNESCO equation of state computed using the
        !           potential-temperature-based bulk modulus from Jackett and
        !           McDougall, JTECH, Vol.12, pp 381-389, April, 1995.
        !     \item The faster and more accurate alternative to the UNESCO eos
        !           of McDougall, Wright, Jackett and Feistel (hereafter
        !           MWJF, 2001 submission to JTECH).
        !     \item a polynomial fit to the full UNESCO EOS
        !     \item a simple linear EOS based on constant expansion coeffs
        !  \end{enumerate}
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: state_mod.F90 26114 2010-12-17 20:29:34Z njn01 $
        ! !USES:
        USE kinds_mod, only : int_kind
        USE kinds_mod, only : r8
        USE domain_size, only : km
        !*** ccsm
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS:
        PUBLIC ref_pressure
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !
        !  valid ranges and pressure as function of depth
        !
        !-----------------------------------------------------------------------
        REAL(KIND=r8), dimension(km) :: pressz
        ! valid temperature range for level k
        ! valid salinity    range for level k
        ! ref pressure (bars) at each level
        !-----------------------------------------------------------------------
        !
        !  choices for eos type and valid range checks
        !
        !-----------------------------------------------------------------------
        ! integer ids for state choice
        ! input state type chosen
        ! option for checking valid T,S range
        ! freq (in steps) for checking T,S range
        ! do not check T,S range
        ! check T,S range and report invalid
        ! force polynomial eval within range
        !-----------------------------------------------------------------------
        !
        !  UNESCO EOS constants and JMcD bulk modulus constants
        !
        !-----------------------------------------------------------------------
        !*** for density of fresh water (standard UNESCO)
        !*** for dependence of surface density on salinity (UNESCO)
        !*** from Table A1 of Jackett and McDougall
        !-----------------------------------------------------------------------
        !
        !  MWJF EOS coefficients
        !
        !-----------------------------------------------------------------------
        !*** these constants will be used to construct the numerator
        !*** factor unit change (kg/m^3 -> g/cm^3) into numerator terms
        !*** these constants will be used to construct the denominator
        !-----------------------------------------------------------------------
        !
        !  coeffs and reference values for polynomial eos
        !
        !-----------------------------------------------------------------------
        ! reference temperature for level k
        ! reference salinity    for level k
        ! reference density     for level k
        ! coefficients for polynomial eos
        !-----------------------------------------------------------------------
        !
        !  parameters for linear eos
        !
        !-----------------------------------------------------------------------
        ! reference T for linear eos (deg C)
        ! reference S for linear eos (msu)
        ! ref dens (g/cm3) at ref T,S and 0 bar
        ! expansion coeff -(drho/dT) (gr/cm^3/K)
        ! expansion coeff (drho/dS) (gr/cm^3/msu)
        !-----------------------------------------------------------------------
        !
        !  logical control parameters
        !
        !-----------------------------------------------------------------------
        !EOC
        !***********************************************************************
            PUBLIC kgen_read_externs_state_mod
        CONTAINS

        ! write subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_state_mod(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) pressz
        END SUBROUTINE kgen_read_externs_state_mod

        !***********************************************************************
        !BOP
        ! !IROUTINE: state
        ! !INTERFACE:


        !***********************************************************************
        !BOP
        ! !IROUTINE: ref_pressure
        ! !INTERFACE:

        FUNCTION ref_pressure(k)
            ! !DESCRIPTION:
            !  This function returns a reference pressure at level k.
            !
            ! !REVISION HISTORY:
            !  same as module
            ! !INPUT PARAMETERS:
            INTEGER(KIND=int_kind), intent(in) :: k
            ! vertical level index
            ! !OUTPUT PARAMETERS:
            REAL(KIND=r8) :: ref_pressure
            ! reference pressure at level k
            !EOP
            !BOC
            !-----------------------------------------------------------------------
            !
            !  return pre-computed reference pressure at level k
            !
            !-----------------------------------------------------------------------
    ref_pressure = pressz(k)
            !-----------------------------------------------------------------------
            !EOC
        END FUNCTION ref_pressure
        !***********************************************************************
        !BOP
        ! !IROUTINE: init_state
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_state_coeffs
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: potem
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: unesco
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: pressure
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: lsqsl2
        ! !INTERFACE:

        !***********************************************************************
    END MODULE state_mod
