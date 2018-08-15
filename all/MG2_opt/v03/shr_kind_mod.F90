
! KGEN-generated Fortran source file
!
! Filename    : shr_kind_mod.F90
! Generated at: 2015-10-08 11:52:40
! KGEN version: 0.5.2



    MODULE shr_kind_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !----------------------------------------------------------------------------
        ! precision/kind constants add data public
        !----------------------------------------------------------------------------
        PUBLIC
<<<<<<< HEAD
        INTEGER, parameter :: shr_kind_r8 = selected_real_kind(12) ! 8 byte real
=======
#if defined(USE_R4)
        INTEGER, parameter :: rkind_comp = selected_real_kind( 6) ! 4 byte real
        INTEGER, parameter :: rkind_io   = selected_real_kind(12) ! 8 byte real
#else
        INTEGER, parameter :: rkind_comp = selected_real_kind(12) ! 8 byte real
        INTEGER, parameter :: rkind_io   = selected_real_kind(12) ! 8 byte real
#endif
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        ! 4 byte real
        ! native real
        ! 8 byte integer
        ! 4 byte integer
        INTEGER, parameter :: shr_kind_in = kind(1) ! native integer
        ! short char
        ! mid-sized char
        ! long char
        ! extra-long char
        ! extra-extra-long char

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE shr_kind_mod
