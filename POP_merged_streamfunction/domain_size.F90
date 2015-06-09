
! KGEN-generated Fortran source file
!
! Filename    : domain_size.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



    MODULE domain_size
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: domain_size
        !
        ! !DESCRIPTION:
        !  This module contains parameters for the global model domain size
        !  decomposition block size.  It is used by the domain and block
        !  modules for decomposing the model domain across processors.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id$
        ! !USES:
        USE kinds_mod, only : int_kind
        IMPLICIT NONE
        PRIVATE
        ! !DEFINED PARAMETERS:
        INTEGER(KIND=int_kind), parameter, public :: km = 60 ! model size parameters
        ! extent of horizontal axis in i direction
        ! extent of horizontal axis in j direction
        ! number of vertical levels
        ! total number of tracers
        INTEGER(KIND=int_kind), parameter, public :: block_size_x = 64
        INTEGER(KIND=int_kind), parameter, public :: block_size_y = 64
        ! size of block in 1st horizontal dimension
        ! size of block in 2nd horizontal dimension
        !*** The model will inform the user of the correct
        !*** values for theparameters below.  A value higher than
        !*** necessary will not cause the code to fail, but will
        !*** allocate more memory than is necessary.  A value that
        !*** is too low will cause the code to exit.
        !*** A good initial guess is found using
        !*** max=(nx_global/block_size_x)*(ny_global/block_size_y)/
        !***         num_procs
        INTEGER(KIND=int_kind), parameter, public :: max_blocks_clinic = 1
        ! max number of blocks per processor
        !   in each distribution
        !EOP
        !BOC
        !EOC
        !***********************************************************************

    ! write subroutines
    ! No subroutines
    ! No module extern variables
    END MODULE domain_size
