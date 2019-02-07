!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:38 
!KGEN version : 0.8.1 
  


Module shr_mpi_mod
  !-------------------------------------------------------------------------------
  ! PURPOSE: general layer on MPI functions
  !-------------------------------------------------------------------------------


    USE shr_kind_mod 
    USE shr_log_mod, ONLY: s_logunit => shr_log_unit 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
  ! PUBLIC: Public interfaces


    PUBLIC shr_mpi_chkerr 
    PUBLIC shr_mpi_initialized 
    PUBLIC shr_mpi_abort 


! Copyright (C) 2003-2016 Intel Corporation.  All Rights Reserved.
! The source code contained or described herein and all documents
! related to the source code ("Material") are owned by Intel Corporation
! or its suppliers or licensors.  Title to the Material remains with
! Intel Corporation or its suppliers and licensors.  The Material is
! protected by worldwide copyright and trade secret laws and treaty
! provisions.  No part of the Material may be used, copied, reproduced,
! modified, published, uploaded, posted, transmitted, distributed, or
! disclosed in any way without Intel's prior express written permission.
! No license under any patent, copyright, trade secret or other
! intellectual property right is granted to or conferred upon you by
! disclosure or delivery of the Materials, either expressly, by
! implication, inducement, estoppel or otherwise.  Any license under
! such intellectual property rights must be express and approved by
! Intel in writing.
!      (C) 2001 by Argonne National Laboratory.
! 				  MPICH2 COPYRIGHT
! The following is a notice of limited availability of the code, and disclaimer
! which must be included in the prologue of the code and in all source listings
! of the code.
! Copyright Notice
!  + 2002 University of Chicago
! Permission is hereby granted to use, reproduce, prepare derivative works, and
! to redistribute to others.  This software was authored by:
! Mathematics and Computer Science Division
! Argonne National Laboratory, Argonne IL 60439
! (and)
! Department of Computer Science
! University of Illinois at Urbana-Champaign
! 			      GOVERNMENT LICENSE
! Portions of this material resulted from work developed under a U.S.
! Government Contract and are subject to the following license: the Government
! is granted for itself and others acting on its behalf a paid-up, nonexclusive,
! irrevocable worldwide license in this computer software to reproduce, prepare
! derivative works, and perform publicly and display publicly.
! 				  DISCLAIMER
! This computer code material was prepared, in part, as an account of work
! sponsored by an agency of the United States Government.  Neither the United
! States, nor the University of Chicago, nor any of their employees, makes any
! warranty express or implied, or assumes any legal liability or responsibility
! for the accuracy, completeness, or usefulness of any information, apparatus,
! product, or process disclosed, or represents that its use would not infringe
! privately owned rights.
! Portions of this code were written by Microsoft. Those portions are
! Copyright (c) 2007 Microsoft Corporation. Microsoft grants permission to
! use, reproduce, prepare derivative works, and to redistribute to
! others. The code is licensed "as is." The User bears the risk of using
! it. Microsoft gives no express warranties, guarantees or
! conditions. To the extent permitted by law, Microsoft excludes the
! implied warranties of merchantability, fitness for a particular
! purpose and non-infringement.
!      DO NOT EDIT
!      This file created by buildiface 

!
! 
! 
!      
!      
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
! 
!
!      
!      
!      
       INTEGER MPI_SUCCESS
       PARAMETER (mpi_success=0) 
       INTEGER MPI_MAX_ERROR_STRING
       PARAMETER (mpi_max_error_string=512-1) 


  !===============================================================================

CONTAINS
  !===============================================================================

  SUBROUTINE shr_mpi_chkerr(rcode,string)

    IMPLICIT none
    !----- arguments ---

    integer(SHR_KIND_IN), intent(in) :: rcode  ! input MPI error code
    character(*),         intent(in) :: string ! message
    !----- local ---

    character(*),parameter           :: subName = '(shr_mpi_chkerr) '
    character(MPI_MAX_ERROR_STRING)  :: lstring
    integer(SHR_KIND_IN)             :: len
    integer(SHR_KIND_IN)             :: ierr
    !-------------------------------------------------------------------------------
    ! PURPOSE: layer on MPI error checking
    !-------------------------------------------------------------------------------


    if (rcode /= MPI_SUCCESS) then
       write(s_logunit,*) trim(subName),":",lstring(1:len)
       call shr_mpi_abort(string,rcode)
    endif

  END SUBROUTINE shr_mpi_chkerr
  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  SUBROUTINE shr_mpi_initialized(flag,string)

    IMPLICIT none
    !----- arguments ---

    logical,intent(out)                :: flag
    character(*),optional,intent(in)   :: string   ! message
    !----- local ---

    character(*),parameter             :: subName = '(shr_mpi_initialized) '
    integer(SHR_KIND_IN)               :: ierr
    !-------------------------------------------------------------------------------
    ! PURPOSE: MPI initialized
    !-------------------------------------------------------------------------------


    if (present(string)) then
       call shr_mpi_chkerr(ierr,subName//trim(string))
    else
       call shr_mpi_chkerr(ierr,subName)
    endif

  END SUBROUTINE shr_mpi_initialized
  !===============================================================================
  !===============================================================================


  SUBROUTINE shr_mpi_abort(string,rcode)

    IMPLICIT none
    !----- arguments ---

    character(*),optional,intent(in)   :: string   ! message
    integer,optional,intent(in)        :: rcode    ! optional code
    !----- local ---

    character(*),parameter             :: subName = '(shr_mpi_abort) '
    integer(SHR_KIND_IN)               :: ierr
    integer                            :: rc       ! return code
    !-------------------------------------------------------------------------------
    ! PURPOSE: MPI abort
    !-------------------------------------------------------------------------------


    if ( present(string) .and. present(rcode) ) then
       write(s_logunit,*) trim(subName),":",trim(string),rcode
    endif
    if ( present(rcode) )then
       rc = rcode
    else
       rc = 1001
    end if

  END SUBROUTINE shr_mpi_abort
  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


  !===============================================================================
  !===============================================================================


END MODULE shr_mpi_mod