
! KGEN-generated Fortran source file
!
! Filename    : domain.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



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
        USE blocks, only : block
        USE blocks, only : nx_block
        USE blocks, only : ny_block
        USE constants, only : c0
        USE constants, only : c2
        USE constants, only : c1
        USE constants, only : eps
        USE constants, only : p25
        USE domain_size, only : km
        USE domain_size, only : max_blocks_clinic
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS
        ! !PUBLIC DATA MEMBERS:
        ! actual number of blocks on this processor
        !   in each distribution
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
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
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
