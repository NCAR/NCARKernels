!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: mpi_utilities_mod.f90 12939 2018-11-27 15:34:34Z nancy@ucar.edu $
!> A collection of interfaces to the MPI (Message Passing Interface)
!> multi-processor communication library routines.
!>
!> All MPI routines are called from here.  There is a companion
!> file which has the same module name and entry points but all
!> routines are stubs.  This allows a single-task version of the
!> code to be compiled without the MPI libraries.
!>


!


module mpi_utilities_mod

    USE types_mod, ONLY: digits12 
    USE utilities_mod, ONLY: error_handler, e_err 

! BUILD TIP 1
! Many MPI installations have an MPI module; if one is present, use it.
! ('use mpi')
! If not, there will be an MPI include file which defines the parameters.
! ('include mpif.h')
! Use one but not both.   The 'use' line must be before the 'implicit none' 
! and 'private' lines, 'include' must come after.  Go figure.
! For more help on compiling a module which uses MPI see the 
! $DART/developer_tests/mpi_utilities/tests/README
!use mpi
! the NAG compiler needs these special definitions enabled
! but we don't preprocess this file (why?) so you have to
! edit this by hand for NAG.
!#ifdef __NAG__
 !use F90_unix_proc, only : sleep, system, exit
 !! block for NAG compiler
 !  PURE SUBROUTINE SLEEP(SECONDS,SECLEFT)
 !    INTEGER,INTENT(IN) :: SECONDS
 !    INTEGER,OPTIONAL,INTENT(OUT) :: SECLEFT
 !  SUBROUTINE SYSTEM(STRING,STATUS,ERRNO)
 !    CHARACTER*(*),INTENT(IN) :: STRING
 !    INTEGER,OPTIONAL,INTENT(OUT) :: STATUS,ERRNO
 !!also used in exit_all outside this module
 !  SUBROUTINE EXIT(STATUS)
 !    INTEGER,OPTIONAL :: STATUS
 !! end block
!#endif
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


 !
 !

    IMPLICIT NONE 
    PRIVATE 
!    Copyright (C) Silicon Graphics International Corp.
!    All rights reserved.
!    SGI RESERVES THE RIGHT TO WITHDRAW, MODIFY, OR REPLACE THIS SOFTWARE AT
!    ANY TIME, WITHOUT NOTICE. THE SOFTWARE IS "AS IS." IN CONNECTION WITH OR
!    ARISING IN RELATION TO THE SOFTWARE AND/OR THIS NOTICE, (1) IN NO EVENT
!    SHALL SGI OR ITS SUPPLIERS BE LIABLE FOR ANY SPECIAL, CONSEQUENTIAL,
!    INCIDENTAL OR INDIRECT DAMAGES, EVEN IF PRE-ADVISED OF THEIR PROSPECT,
!    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY; AND, (2) SGI AND
!    ITS SUPPLIERS DISCLAIM ANY AND ALL LIABILITY FOR: (a) WARRANTIES AND
!    CONDITIONS, WHETHER EXPRESSED, IMPLIED, OR STATUTORY,  ARISING IN RELATION
!    TO THE SOFTWARE AND/OR THIS NOTICE, INCLUDING WITHOUT LIMITATION ANY
!    WARRANTY AND/OR CONDITION OF ERROR-FREE AND/OR UNINTERRUPTED OPERATION,
!    MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A PARTICULAR PURPOSE,
!    AND NON-INFRINGEMENT; AND (b) INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
!    OR CONSEQUENTIAL DAMAGES RELATING TO THE SOFTWARE, ITS USE, MISUSE,
!    AND/OR FAILURE OF USE. ALL OF THE FOREGOING APPLY NOTWITHSTANDING THE
!    FAILURE OF ESSENTIAL PURPOSE OF ANY CONTRACTUAL REMEDY.
! Copyright Notice
!  + 1993 University of Chicago
!  + 1993 Mississippi State University
!    Copyright 1999-2016 (C) Silicon Graphics International Corp.
!    All rights reserved.
!    SGI RESERVES THE RIGHT TO WITHDRAW, MODIFY, OR REPLACE THIS SOFTWARE AT
!    ANY TIME, WITHOUT NOTICE. THE SOFTWARE IS "AS IS." IN CONNECTION WITH OR
!    ARISING IN RELATION TO THE SOFTWARE AND/OR THIS NOTICE, (1) IN NO EVENT
!    SHALL SGI OR ITS SUPPLIERS BE LIABLE FOR ANY SPECIAL, CONSEQUENTIAL,
!    INCIDENTAL OR INDIRECT DAMAGES, EVEN IF PRE-ADVISED OF THEIR PROSPECT,
!    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY; AND, (2) SGI AND
!    ITS SUPPLIERS DISCLAIM ANY AND ALL LIABILITY FOR: (a) WARRANTIES AND
!    CONDITIONS, WHETHER EXPRESSED, IMPLIED, OR STATUTORY,  ARISING IN RELATION
!    TO THE SOFTWARE AND/OR THIS NOTICE, INCLUDING WITHOUT LIMITATION ANY
!    WARRANTY AND/OR CONDITION OF ERROR-FREE AND/OR UNINTERRUPTED OPERATION,
!    MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A PARTICULAR PURPOSE,
!    AND NON-INFRINGEMENT; AND (b) INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
!    OR CONSEQUENTIAL DAMAGES RELATING TO THE SOFTWARE, ITS USE, MISUSE,
!    AND/OR FAILURE OF USE. ALL OF THE FOREGOING APPLY NOTWITHSTANDING THE
!    FAILURE OF ESSENTIAL PURPOSE OF ANY CONTRACTUAL REMEDY.


!
!
!

!
!
!


! MPI_Status


! Misc Fortran declarations


! MPI-2 Section 5.3


! MPI-1 error codes and classes


        integer MPI_SUCCESS

        PARAMETER (mpi_success                   = 0) 
! MPI-2 error codes and classes


! Permanent keyvals


! Results of the compare operations


! Topology types


! Misc constants


! The following 2 lines are included in the main mpif.h
!       double precision MPI_WTIME, MPI_WTICK
!       external MPI_WTIME, MPI_WTICK
! MPI-2 Section 4.10


! MPI-2 Section 5.4


! Kind values for MPI-2


! Section 6.4 bindings for one-sided communication


! Thread-safety support levels


! Datatype Decoding constants


! Permanent window keyvals


! Typeclasses


! Communicator types


! Window flavors


! MPI-2 I/O definitions
!    Copyright 1999-2016 (C) Silicon Graphics International Corp.
!    All rights reserved.
!    SGI RESERVES THE RIGHT TO WITHDRAW, MODIFY, OR REPLACE THIS SOFTWARE AT
!    ANY TIME, WITHOUT NOTICE. THE SOFTWARE IS "AS IS." IN CONNECTION WITH OR
!    ARISING IN RELATION TO THE SOFTWARE AND/OR THIS NOTICE, (1) IN NO EVENT
!    SHALL SGI OR ITS SUPPLIERS BE LIABLE FOR ANY SPECIAL, CONSEQUENTIAL,
!    INCIDENTAL OR INDIRECT DAMAGES, EVEN IF PRE-ADVISED OF THEIR PROSPECT,
!    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY; AND, (2) SGI AND
!    ITS SUPPLIERS DISCLAIM ANY AND ALL LIABILITY FOR: (a) WARRANTIES AND
!    CONDITIONS, WHETHER EXPRESSED, IMPLIED, OR STATUTORY,  ARISING IN RELATION
!    TO THE SOFTWARE AND/OR THIS NOTICE, INCLUDING WITHOUT LIMITATION ANY
!    WARRANTY AND/OR CONDITION OF ERROR-FREE AND/OR UNINTERRUPTED OPERATION,
!    MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A PARTICULAR PURPOSE,
!    AND NON-INFRINGEMENT; AND (b) INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
!    OR CONSEQUENTIAL DAMAGES RELATING TO THE SOFTWARE, ITS USE, MISUSE,
!    AND/OR FAILURE OF USE. ALL OF THE FOREGOING APPLY NOTWITHSTANDING THE
!    FAILURE OF ESSENTIAL PURPOSE OF ANY CONTRACTUAL REMEDY.
!    Fortran MPI-IO programs 
!    Copyright (C) 1997 University of Chicago. 


!
!
!
!
!
!
!
!
!      INTEGER MPI_OFFSET_KIND
!      PARAMETER (MPI_OFFSET_KIND=8)
!
!


!   End Fortran MPI-IO
!    Copyright 2014-2016 (C) Silicon Graphics International Corp.
!    All rights reserved.
!    SGI RESERVES THE RIGHT TO WITHDRAW, MODIFY, OR REPLACE THIS SOFTWARE AT
!    ANY TIME, WITHOUT NOTICE. THE SOFTWARE IS "AS IS." IN CONNECTION WITH OR
!    ARISING IN RELATION TO THE SOFTWARE AND/OR THIS NOTICE, (1) IN NO EVENT
!    SHALL SGI OR ITS SUPPLIERS BE LIABLE FOR ANY SPECIAL, CONSEQUENTIAL,
!    INCIDENTAL OR INDIRECT DAMAGES, EVEN IF PRE-ADVISED OF THEIR PROSPECT,
!    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY; AND, (2) SGI AND
!    ITS SUPPLIERS DISCLAIM ANY AND ALL LIABILITY FOR: (a) WARRANTIES AND
!    CONDITIONS, WHETHER EXPRESSED, IMPLIED, OR STATUTORY,  ARISING IN RELATION
!    TO THE SOFTWARE AND/OR THIS NOTICE, INCLUDING WITHOUT LIMITATION ANY
!    WARRANTY AND/OR CONDITION OF ERROR-FREE AND/OR UNINTERRUPTED OPERATION,
!    MERCHANTABILITY, SATISFACTORY QUALITY, FITNESS FOR A PARTICULAR PURPOSE,
!    AND NON-INFRINGEMENT; AND (b) INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
!    OR CONSEQUENTIAL DAMAGES RELATING TO THE SOFTWARE, ITS USE, MISUSE,
!    AND/OR FAILURE OF USE. ALL OF THE FOREGOING APPLY NOTWITHSTANDING THE
!    FAILURE OF ESSENTIAL PURPOSE OF ANY CONTRACTUAL REMEDY.
! MPI_Status
!

!
!
!


! Permanent window keyvals


!


! MPI_Op


! MPI_Datatype


! MPI_Comm


! MPI_Errhandler


! MPI_Group


! MPI_Message


! MPI_Request


! BUILD TIP 2
! Some compilers require an interface block for the system() function;
! some fail if you define one.  If you get an error at link time (something
! like 'undefined symbol _system_') try running the fixsystem script in
! this directory.  It is a sed script that comments in and out the interface
! block below.  Please leave the BLOCK comment lines unchanged.
 !!SYSTEM_BLOCK_EDIT START COMMENTED_IN
 ! interface block for getting return code back from system() routine


 ! end block
 !!SYSTEM_BLOCK_EDIT END COMMENTED_IN
! allow global sum to be computed for integers, r4, and r8s


!   ---- private data for mpi_utilities ----


integer :: myrank        = -1  ! my mpi number


PUBLIC my_task_id, task_sync, start_mpi_timer, read_mpi_timer 
! version controlled file description for error handling, do not edit

character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/mpi_utilities_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 12939 $"
character(len=128), parameter :: revdate  = "$Date: 2018-11-27 08:34:34 -0700 (Tue, 27 Nov 2018) $"

logical :: module_initialized   = .false.


CHARACTER(LEN=256) :: errstring 
! for broadcasts, pack small messages into larger ones.  remember that the
! byte size will be this count * 8 because we only communicate r8s.  (unless
! the code is compiled with r8 redefined as r4, in which case it's * 4).

! also for broadcasts, make sure message size is not too large.  if so,
! split a single request into two or more broadcasts.  i know 2G is really
! 2 * 1024 * 1024 * 1024, but err on the conservative side here.

! option for simple send/recvs to limit max single message size.
! split a single request into two or more broadcasts.  i know 2G is really
! 2 * 1024 * 1024 * 1024, but err on the conservative side here.

! this turns on trace messages for most MPI communications

logical :: verbose        = .false.   ! very very very verbose, use with care
! if your batch system does the task layout backwards, set this to true
! so the last task will communicate with the script in async 4 mode.
! as of now, mpich and mvapich do it forward, openmpi does it backwards.

! for large numbers of MPI tasks, you will get replicated messages, one
! per task, if this is set to true.  however, for debugging if you need
! messages from tasks which aren't 0, this will elicit them.  error messages
! from any task will print regardless of this setting.

! make local array copy for send/recv/bcast.  was needed on an old, buggy version
! of the mpi libs but seems unneeded now. 

! NAMELIST: change the following from .false. to .true. to enable
! the reading of this namelist.  This is the only place you need
! to make this change.


PUBLIC kr_externs_in_mpi_utilities_mod 
PUBLIC kr_externs_out_mpi_utilities_mod 

contains
!-----------------------------------------------------------------------------
! mpi cover routines
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


function my_task_id()
! Return my unique task id.  Values run from 0 to N-1 (where N is the
! total number of MPI tasks.


integer :: my_task_id

if ( .not. module_initialized ) then
   write(errstring, *) 'initialize_mpi_utilities() must be called first'
   call error_handler(E_ERR,'my_task_id', errstring, source, revision, revdate)
endif

my_task_id = myrank

end function my_task_id
!-----------------------------------------------------------------------------


subroutine task_sync()
! Synchronize all tasks.  This subroutine does not return until all tasks
! execute this line of code.


integer :: errcode

if ( .not. module_initialized ) then
   write(errstring, *) 'initialize_mpi_utilities() must be called first'
   call error_handler(E_ERR,'task_sync', errstring, source, revision, revdate)
endif

if (verbose) write(*,*) "PE", myrank, ": waiting at MPI Barrier"
if (errcode /= MPI_SUCCESS) then
   write(errstring, '(a,i8)') 'MPI_Barrier returned error code ', errcode
   call error_handler(E_ERR,'task_sync', errstring, source, revision, revdate)
endif

if (verbose) write(*,*) "PE", myrank, ": MPI Barrier released"

end subroutine task_sync
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! TODO: do i need to overload this for both integer and real?
!       do i need to handle 1D, 2D, 3D inputs?


!-----------------------------------------------------------------------------
! TODO: do i need to overload this for both integer and real?
!       do i need to handle 2D inputs?


!-----------------------------------------------------------------------------
! TODO: do i need to overload this for both integer and real?
!       do i need to handle 2D inputs?


!-----------------------------------------------------------------------------
! DART-specific cover utilities
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
   


!-----------------------------------------------------------------------------
! overloaded global reduce routines
! The external32 representations of the datatypes returned by MPI_TYPE_CREATE_F90_REAL/COMPLEX/INTEGER are given by the following rules.
! For MPI_TYPE_CREATE_F90_REAL:
!    if      (p > 33) or (r > 4931) then  external32 representation 
!                                         is undefined   
!    else if (p > 15) or (r >  307) then  external32_size = 16 
!    else if (p >  6) or (r >   37) then  external32_size =  8 
!    else                                 external32_size =  4 
! For MPI_TYPE_CREATE_F90_COMPLEX: twice the size as for MPI_TYPE_CREATE_F90_REAL.
! For MPI_TYPE_CREATE_F90_INTEGER:
!    if      (r > 38) then  external32 representation is undefined 
!    else if (r > 18) then  external32_size =  16  
!    else if (r >  9) then  external32_size =  8  
!    else if (r >  4) then  external32_size =  4 
!    else if (r >  2) then  external32_size =  2  
!    else                   external32_size =  1  
!-----------------------------------------------------------------------------
   

! 
! 
! 
!
!


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! pipe-related utilities
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! general system util wrappers.
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
!> start a time block.  call with different argument
!> to start multiple or nested timers.  same argument
!> must be supplied to read_timer function to get
!> elapsed time since that timer was set.  contrast this with
!> 'start_timer/read_timer' in the utils module which returns
!> elapsed seconds.  this returns whatever units the mpi wtime()
!> function returns.
!>
!> usage:
!>  real(digits12) :: base, time_elapsed
!>
!>  call start_mpi_timer(base)
!>  time_elapsed = read_mpi_timer(base)


subroutine start_mpi_timer(base)

real(digits12), intent(out) :: base


end subroutine start_mpi_timer
!-----------------------------------------------------------------------------
!> return the time since the last call to start_timer().
!> can call multiple times to get running times.
!> call with a different base for nested timers.


function read_mpi_timer(base)

real(digits12), intent(in) :: base
real(digits12) :: read_mpi_timer

real(digits12) :: now


read_mpi_timer = now - base

end function read_mpi_timer
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Collect sum across tasks for a given array.


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Collect min and max on task.


!-----------------------------------------------------------------------------
! cover routine which is deprecated.  when all user code replaces this
! with broadcast_minmax(), remove this.


!-----------------------------------------------------------------------------
! Find min and max of each element of an array, put the result on every task.
! Overwrites arrays min_var, max_var with the minimum and maximum for each 
! element across all tasks.


!-----------------------------------------------------------------------------
! One sided communication


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! Broadcast logical


!-----------------------------------------------------------------------------


!read state subroutine for kr_externs_in_mpi_utilities_mod 
SUBROUTINE kr_externs_in_mpi_utilities_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) myrank 
    READ (UNIT = kgen_unit) module_initialized 
    READ (UNIT = kgen_unit) errstring 
    READ (UNIT = kgen_unit) verbose 
END SUBROUTINE kr_externs_in_mpi_utilities_mod 
  
!read state subroutine for kr_externs_out_mpi_utilities_mod 
SUBROUTINE kr_externs_out_mpi_utilities_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
END SUBROUTINE kr_externs_out_mpi_utilities_mod 
  
end module mpi_utilities_mod
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! NOTE -- non-module code, so this subroutine can be called from the
!  utilities module, which this module uses (and cannot have circular refs)
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------


subroutine exit_all(exit_code)

 integer, intent(in) :: exit_code
! In case of error, call this instead of the fortran intrinsic exit().
! It will signal the other MPI tasks that something bad happened and they
! should also exit.


integer :: ierror
! if we made a local communicator, call abort on it.
! otherwise call abort on the world comm.
!print *, 'calling abort on comm ', get_dart_mpi_comm()


! execution should never get here


end subroutine exit_all
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/mpi_utilities_mod.f90 $
! $Id: mpi_utilities_mod.f90 12939 2018-11-27 15:34:34Z nancy@ucar.edu $
! $Revision: 12939 $
! $Date: 2018-11-27 08:34:34 -0700 (Tue, 27 Nov 2018) $
