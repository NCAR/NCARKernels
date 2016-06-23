
! KGEN-generated Fortran source file
!
! Filename    : POP_KindsMod.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



    MODULE pop_kindsmod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: POP_KindsMod
        !
        ! !DESCRIPTION:
        !  This module defines default numerical data types for all common data
        !  types like integer, character, logical, real4 and real8.
        !
        ! !USERDOC:
        !  Users should not need to adjust anything in this module.  If various
        !  character strings like long paths to files exceed the default
        !  character length, the default value may be increased.
        !
        ! !REFDOC:
        !  This module is supplied to provide consistent data representation
        !  across machine architectures.  It is meant to replace the old
        !  Fortran double precision and real *X declarations that were
        !  implementation-specific.
        !  Users should not need to adjust anything in this module.  If various
        !  character strings like long paths to files exceed the default
        !  character length, the default value may be increased.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: POP_KindsMod.F90 22881 2010-05-11 04:23:39Z njn01 $
        ! !USES:
        !  uses no other modules
        IMPLICIT NONE
        PRIVATE
        ! !DEFINED PARAMETERS:
        INTEGER, parameter, public :: pop_i4          = selected_int_kind(6)
        INTEGER, parameter, public :: pop_r8          = selected_real_kind(13)
        ! standard, single-precision
        !EOP
        !BOC
        !EOC
        !***********************************************************************

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE pop_kindsmod
