
! KGEN-generated Fortran source file
!
! Filename    : kinds_mod.F90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11



    MODULE kinds_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: kinds_mod
        !
        ! !DESCRIPTION:
        !  This module defines default numerical data types for all common data
        !  types like integer, character, logical, real4 and real8.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: kinds_mod.F90 12674 2008-10-31 22:21:32Z njn01 $
        ! !USES:
        !  uses no other modules
        IMPLICIT NONE
        PRIVATE
        ! !DEFINED PARAMETERS:
        INTEGER, parameter, public :: r8             = selected_real_kind(13)
        INTEGER, parameter, public :: int_kind       = kind(1)
        INTEGER, parameter, public :: log_kind       = kind(.true.)
        ! standard, single-precision
        !EOP
        !BOC
        !EOC
        !***********************************************************************

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE kinds_mod
