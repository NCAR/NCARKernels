
! KGEN-generated Fortran source file
!
! Filename    : exit_mod.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



    MODULE exit_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: exit_mod
        !
        ! !DESCRIPTION:
        !  This module provides a means for a graceful exit from POP when
        !  encountering an error.  it contains only the routines exit\_POP
        !  and flushm
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: exit_mod.F90 35325 2012-03-09 00:48:12Z njn01 $
        ! !USES:
        USE constants, only : c0
        USE constants, only : c2
        USE constants, only : c1
        USE constants, only : eps
        USE constants, only : p25
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS:
        ! !DEFINED PARAMETERS:
        ! signal for normal exit
        ! signal for aborting (exit due to error)
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
        ! !IROUTINE: exit_POP
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: flushm (iunit)
        ! !INTERFACE:

        !***********************************************************************
    END MODULE exit_mod
