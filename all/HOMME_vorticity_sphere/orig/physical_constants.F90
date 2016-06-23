
! KGEN-generated Fortran source file
!
! Filename    : physical_constants.F90
! Generated at: 2015-03-14 21:47:57
! KGEN version: 0.4.5



    MODULE physical_constants
        ! ------------------------------
        USE kinds, ONLY: real_kind
        ! -----------------------------
        IMPLICIT NONE
        PRIVATE
        ! to run Held-Suarez in JUPITER mode:
        ! make rearth,g,omega namelist variables and set to:
        ! omega = 2e-4, rearth=7e7, g=23.0
        REAL(KIND=real_kind), public, parameter :: rearth       = 6.376d6 ! m
        ! m s^-2
        ! radians/s
        ! surface pressure (mbar)
        !ASC ANDII VERIFY PLS
        REAL(KIND=real_kind), public, parameter :: rrearth      = 1.0_real_kind/rearth ! m
        ! multicloud J/Kg

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE physical_constants
