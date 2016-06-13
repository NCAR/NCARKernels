
! KGEN-generated Fortran source file
!
! Filename    : ppgrid.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE ppgrid
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
!-----------------------------------------------------------------------
!
! Purpose:
! Initialize physics grid resolution parameters
!  for a chunked data structure
!
! Author:
!
!-----------------------------------------------------------------------
        IMPLICIT NONE
        PRIVATE
        PUBLIC pver
        PUBLIC pverp
! Grid point resolution parameters
! number of columns (max)
! number of sub-columns (max)
        INTEGER :: pver ! number of vertical levels
        INTEGER :: pverp ! pver + 1
        PARAMETER (pver      = 32)
        PARAMETER (pverp     = pver + 1)
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!
!
!

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE ppgrid
