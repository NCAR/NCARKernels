
! KGEN-generated Fortran source file
!
! Filename    : domain.F90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11



    MODULE domain
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: domain
        !
        ! !DESCRIPTION:
        !  This module contains the model domain and routines for initializing
        !  the domain.  It also initializes the decompositions and
        !  distributions across processors/threads by calling relevent
        !  routines in the block, distribution modules.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: domain.F90 43213 2013-01-11 18:24:48Z mlevy@ucar.edu $
        ! !USES:
        USE kinds_mod, only : int_kind
        USE kinds_mod, only : r8
        USE kinds_mod, only : log_kind
        USE blocks, only : nx_block
        USE blocks, only : ny_block
        USE domain_size, only : km
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS
        ! !PUBLIC DATA MEMBERS:
        ! actual number of blocks on this processor
        !   in each distribution
        INTEGER(KIND=int_kind), dimension(:), pointer, public :: blocks_clinic
        ! block ids for local blocks in baroclinic dist
        ! block ids for local blocks in barotropic dist
        !  block distribution info
        ! block distribution for baroclinic part
        ! block distribution for barotropic part
        !  block distribution info
        ! block distribution for baroclinic part
        ! block distribution for barotropic part
        !------------------------------------------------------------
        ! Lets keep track of the land blocks for parallel IO reasons
        !------------------------------------------------------------
        ! acount number of land blocks assigned to processor
        ! blocks ids for land block
        ! block distribution info for land
        !  ghost cell update info
        ! halo information for baroclinic part
        ! halo information for barotropic part
        !
        ! flag to signal use of tripole grid
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !
        !   module private variables - for the most part these appear as
        !   module variables to facilitate sharing info between init_domain1
        !   and init_domain2.
        !
        !-----------------------------------------------------------------------
        ! method for distributing blocks
        ! method for distributing blocks
        ! method to use for distributing
        !    blocks in each case
        ! type of domain bndy in each logical
        !    direction (ew is i, ns is j)
        ! decomposition info
        ! num of processors in baroclinic dist
        ! num of processors in barotropic dist
        !EOC
        !***********************************************************************
            PUBLIC kgen_read_externs_domain
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_integer_int_kind_dim1_ptr(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                integer(KIND=int_kind), INTENT(OUT), POINTER, DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1
                INTEGER, DIMENSION(2,1) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_integer_int_kind_dim1_ptr


        ! module extern variables

        SUBROUTINE kgen_read_externs_domain(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_integer_int_kind_dim1_ptr(blocks_clinic, kgen_unit)
        END SUBROUTINE kgen_read_externs_domain

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_domain_blocks
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_domain_distribution
        ! !INTERFACE:

        !***********************************************************************
    END MODULE domain
