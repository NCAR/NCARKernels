!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  


module perf_mod
!-----------------------------------------------------------------------
! Purpose: This module is responsible for controlling the performance
!          timer logic.
! Author:  P. Worley, January 2007
! $Id$
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!- Uses ----------------------------------------------------------------
!-----------------------------------------------------------------------

!
!
!
!


    USE perf_utils 
!-----------------------------------------------------------------------
!- module boilerplate --------------------------------------------------
!-----------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
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


    SAVE 
!-----------------------------------------------------------------------
! Public interfaces ----------------------------------------------------
!-----------------------------------------------------------------------

    PUBLIC t_startf 
    PUBLIC t_stopf 
!-----------------------------------------------------------------------
! Private interfaces (local) -------------------------------------------
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!- include statements --------------------------------------------------
!-----------------------------------------------------------------------
! $Id: gptl.inc,v 1.44 2011-03-28 20:55:19 rosinski Exp $
! Author: Jim Rosinski
! GPTL header file to be included in user code. Values match
! their counterparts in gptl.h. See that file or man pages
! or web-based documenation for descriptions of each value

!
!
!
!


! Externals


!-----------------------------------------------------------------------
! Private data ---------------------------------------------------------
!-----------------------------------------------------------------------
   !----------------------------------------------------------------------------
   ! perf_mod options
   !----------------------------------------------------------------------------


                         ! unit number for log output

   logical, parameter :: def_timing_initialized = .false.      ! default
   logical, private   :: timing_initialized = def_timing_initialized
                         ! flag indicating whether timing library has
                         ! been initialized

                         ! flag indicating whether timers are disabled

                         ! flag indicating whether the mpi_barrier in
                         ! t_barrierf should be called

                         ! integer indicating maximum number of levels of
                         ! timer nesting

                         ! integer indicating maximum detail level to
                         ! profile

   integer, parameter :: init_timing_disable_depth = 0         ! init
   integer, private   :: timing_disable_depth = init_timing_disable_depth
                         ! integer indicating depth of t_disablef calls

   integer, parameter :: init_timing_detail = 0                ! init
   integer, private   :: cur_timing_detail = init_timing_detail
                         ! current timing detail level

                         ! flag indicating whether the performance timer
                         ! output should be written to a single file
                         ! (per component communicator) or to a
                         ! separate file for each process

                         ! maximum number of processes writing out
                         ! timing data (for this component communicator)

                         ! separation between process ids for processes
                         ! that are writing out timing data
                         ! (for this component communicator)

                         ! collect and print out global performance statistics
                         ! (for this component communicator)

                         ! measure overhead of profiling directly

   logical, parameter :: def_perf_add_detail = .false.         ! default
   logical, private   :: perf_add_detail = def_perf_add_detail
                         ! flag indicating whether to prefix the
                         ! timer name with the current detail level.
                         ! This requires that even t_startf/t_stopf
                         ! calls do not cross detail level changes

                         ! current prefix for all event names.
                         ! Default defined to be blank via 
                         ! prefix_len_def
   integer, parameter :: prefix_len_def = 0                    ! default
   integer, private   :: prefix_len = prefix_len_def
                         ! For convenience, contains len_trim of 
                         ! event_prefix, if set.


                         ! integer indicating which timer to use
                         ! (as defined in gptl.inc)

                         ! flag indicating whether the PAPI namelist
                         ! should be read and HW performance counters
                         ! used in profiling
   ! PAPI counter ids


!=======================================================================
   PUBLIC kr_externs_in_perf_mod 
   PUBLIC kr_externs_out_perf_mod 

contains
!=======================================================================
!========================================================================

!
!

!========================================================================
!
!

!========================================================================
!
!


!========================================================================
!
!


!========================================================================

!
!


!========================================================================
!
!


!========================================================================
!
!

!========================================================================
!
!

!========================================================================
!
!

!========================================================================
!
!

!========================================================================
!
!

!========================================================================
!
!

!========================================================================
!
!
   subroutine t_startf(event, handle)
!-----------------------------------------------------------------------
! Purpose: Start an event timer
! Author: P. Worley
!-----------------------------------------------------------------------
!---------------------------Input arguments-----------------------------
   ! performance timer event name
!
   character(len=*), intent(in) :: event
!---------------------------Input/Output arguments----------------------
   ! GPTL event handle
!
!
   integer,  optional :: handle
!---------------------------Local workspace-----------------------------
!
!
   integer  ierr                          ! GPTL error return
   integer  str_length, i                 ! support for adding
                                          !  detail prefix
   character(len=2) cdetail               ! char variable for detail
!-----------------------------------------------------------------------
!
!
   if (.not. timing_initialized) return
   if (timing_disable_depth > 0) return

   if ((perf_add_detail) .AND. (cur_timing_detail < 100)) then

      write(cdetail,'(i2.2)') cur_timing_detail
      if (prefix_len > 0) then
         str_length = min(SHR_KIND_CM-prefix_len-5,len_trim(event))
      else
         str_length = min(SHR_KIND_CM-5,len_trim(event))
      endif

   else

      if (prefix_len > 0) then
         str_length = min(SHR_KIND_CM-prefix_len-2,len_trim(event))
      else
         str_length = min(SHR_KIND_CM-2,len_trim(event))
      endif
!pw   if ( present (handle) ) then
!pw      ierr = GPTLstart_handle(event, handle)
!pw   else
!pw      ierr = GPTLstart(event)
!pw   endif


   endif

   return
   end subroutine t_startf
!========================================================================
!
!
   subroutine t_stopf(event, handle)
!-----------------------------------------------------------------------
! Purpose: Stop an event timer
! Author: P. Worley
!-----------------------------------------------------------------------
!---------------------------Input arguments-----------------------------
   ! performance timer event name
!
   character(len=*), intent(in) :: event
!---------------------------Input/Output arguments----------------------
   ! GPTL event handle
!
!
   integer, optional :: handle
!---------------------------Local workspace-----------------------------
!
!
   integer  ierr                          ! GPTL error return
   integer  str_length, i                 ! support for adding
                                          !  detail prefix
   character(len=2) cdetail               ! char variable for detail
!-----------------------------------------------------------------------
!
!
   if (.not. timing_initialized) return
   if (timing_disable_depth > 0) return

   if ((perf_add_detail) .AND. (cur_timing_detail < 100)) then

      write(cdetail,'(i2.2)') cur_timing_detail
      if (prefix_len > 0) then
         str_length = min(SHR_KIND_CM-prefix_len-5,len_trim(event))
      else
         str_length = min(SHR_KIND_CM-5,len_trim(event))
      endif

   else

      if (prefix_len > 0) then
         str_length = min(SHR_KIND_CM-prefix_len-2,len_trim(event))
     else
         str_length = min(SHR_KIND_CM-2,len_trim(event))
     endif
!pw   if ( present (handle) ) then
!pw      ierr = GPTLstop_handle(event, handle)
!pw   else
!pw      ierr = GPTLstop(event)
!pw   endif


   endif

   return
   end subroutine t_stopf
!========================================================================
!
!


!========================================================================
!
!


!========================================================================
!
!


!========================================================================
!
!


!========================================================================
!
!


!========================================================================
!
!


!========================================================================
!
!


!========================================================================
!
!

!===============================================================================


   !read state subroutine for kr_externs_in_perf_mod 
   SUBROUTINE kr_externs_in_perf_mod(kgen_unit) 
       INTEGER, INTENT(IN) :: kgen_unit 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       READ (UNIT = kgen_unit) timing_initialized 
       READ (UNIT = kgen_unit) timing_disable_depth 
       READ (UNIT = kgen_unit) cur_timing_detail 
       READ (UNIT = kgen_unit) perf_add_detail 
       READ (UNIT = kgen_unit) prefix_len 
   END SUBROUTINE kr_externs_in_perf_mod 
     
   !read state subroutine for kr_externs_out_perf_mod 
   SUBROUTINE kr_externs_out_perf_mod(kgen_unit) 
       INTEGER, INTENT(IN) :: kgen_unit 
         
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
   END SUBROUTINE kr_externs_out_perf_mod 
     
end module perf_mod