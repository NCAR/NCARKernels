
! KGEN-generated Fortran source file
!
! Filename    : clubb_precision.F90
! Generated at: 2015-10-20 14:27:07
! KGEN version: 0.5.3



    MODULE clubb_precision
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PUBLIC core_rknd, dp, stat_rknd, stat_nknd, time_precision
        PRIVATE ! Default scope
! This definition of double precision must use a real type that is 64 bits
! wide, because (at least) the LAPACK routines depend on this definition being
! accurate. Otherwise, LAPACK must be recompiled, or some other trickery must
! be done.
        INTEGER, parameter :: dp = selected_real_kind(p=12)
! double precision
! The precisions below are arbitrary, and could be adjusted as
! needed for long simulations or time averaging.  Note that on
! most machines 12 digits of precision will use a data type
! which is 8 bytes long.
        INTEGER, parameter :: core_rknd = dp
        INTEGER, parameter :: stat_rknd = selected_real_kind(p=12)
        INTEGER, parameter :: stat_nknd = selected_int_kind( 8 )
        INTEGER, parameter :: time_precision = selected_real_kind(p=12)
! Value from the preprocessor directive

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE clubb_precision
