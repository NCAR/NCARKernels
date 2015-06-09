
! KGEN-generated Fortran source file
!
! Filename    : constants.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



    MODULE constants
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: constants
        !
        ! !DESCRIPTION:
        !  This module defines a variety of physical and numerical constants
        !  used throughout the Parallel Ocean Program.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: constants.F90 24379 2010-08-13 19:54:51Z njn01 $
        ! !USES:
        USE kinds_mod, only : r8
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS:
        ! !DEFINED PARAMETERS:
        ! numbers
        REAL(KIND=r8), parameter, public :: c0     =    0.0_r8
        REAL(KIND=r8), parameter, public :: c1     =    1.0_r8
        REAL(KIND=r8), parameter, public :: c3     =    3.0_r8
        REAL(KIND=r8), parameter, public :: c2     =    2.0_r8
        REAL(KIND=r8), parameter, public :: eps    = 1.0e-10_r8
        REAL(KIND=r8), parameter, public :: p25    = 0.250_r8
        ! pi, pi/2 and 2pi
        !*** location of fields for staggered grids
        !*** field type attribute - necessary for handling
        !*** changes of direction across tripole boundary
        !  common formats for formatted output
        !  !PUBLIC DATA MEMBERS:
        ! empty character string
        ! physical constants
        ! note that most internal ocean constants are in cgs units
        !  while atmosphere and surface flux constants are sometimes
        !  in MKS units
        ! these constants are defined in an init routine to allow
        !  CSM shared constants to over-ride
        ! gravit. accel. (cm/s^2)
        ! angular vel. of Earth 1/s
        ! radius of Earth (cm)
        ! ambient air density (kg/m^3)
        ! density of fresh water (g/cm^3)
        ! density of salt water (g/cm^3)
        ! specific heat salt water
        ! heat capacity of air (J/kg/K)
        ! speed of sound (cm/s)
        ! von Karman constant
        !
        ! W/m^2/K^4
        ! lat heat of vaporization (erg/g)
        ! lat heat of vaporization (J/kg)
        ! lat heat of fusion (erg/g)
        ! lat heat of fusion (J/kg)
        ! salinity of sea ice formed (psu)
        ! ocean reference salinity (psu)
        ! degree-radian conversion
        !  conversion factors
        ! zero point for Celsius
        ! meters per cm
        ! cm per meter
        ! salt (g/g) to ppt
        ! salt ppt to g/g
        ! mass flux to Sverdrups
        ! heat flux to Petawatts
        ! salt flux to Sv*ppt
        ! salt to water (mm/day)
        ! wind stress (N/m^2) to vel flux (cm^2/s^2)
        ! heat flux (W/m^2) to temp flux (C*cm/s)
        ! fw flux (kg/m^2/s) to salt((msu/psu)*cm/s)
        ! fw flux (kg/m^2/s) to salt flux (msu*cm/s)
        ! salt flux (kg/m^2/s) to salt flux (msu*cm/s)
        ! fw flux (kg/m^2/s) to fw flux (cm/s)
        !EOP
        !BOC
        !EOC
        !***********************************************************************
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !***********************************************************************
        !BOP
        ! !IROUTINE: init_constants
        ! !INTERFACE:

        !***********************************************************************
    END MODULE constants
