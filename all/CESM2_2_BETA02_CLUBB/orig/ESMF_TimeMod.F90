!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
!     ESMF Time Module


!
!
module ESMF_TimeMod
  !==============================================================================
  ! This file contains the Time class definition and all Time class methods.
  !------------------------------------------------------------------------------
  ! INCLUDES
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
  !==============================================================================
  !BOPI
  ! !MODULE: ESMF_TimeMod
  ! !DESCRIPTION:
  ! Part of Time Manager F90 API wrapper of C++ implemenation
  ! Defines F90 wrapper entry points for corresponding
  ! C++ class {\tt ESMC\_Time} implementation
  ! See {\tt ../include/ESMC\_Time.h} for complete description
  !------------------------------------------------------------------------------
  ! !USES:
  ! inherit from ESMF base class
  !
  !
  !


  !
  !
  !
  !
  ! inherit from base time class

  ! associated derived types

    USE esmf_shrtimemod, ONLY: esmf_time 
  ! added by Jhe
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE esmf_shrtimemod, ONLY: kr_esmf_shrtimemod_esmf_time 
    USE esmf_shrtimemod, ONLY: kv_esmf_shrtimemod_esmf_time 

    IMPLICIT NONE 
  !------------------------------------------------------------------------------
  ! !PRIVATE TYPES:
  !
    PRIVATE 
  !------------------------------------------------------------------------------
  !     ! ESMF_Time
  !     ! F90 class type to match C++ Time class in size only;
  !     !  all dereferencing within class is performed by C++ implementation
  ! move to ESMF_ShrTimeMod
  !     type ESMF_Time
  !       type(ESMF_BaseTime) :: basetime           ! inherit base class
  !       ! time instant is expressed as year + basetime
  !       integer :: YR
  !       type(ESMF_Calendar), pointer :: calendar  ! associated calendar
  !     end type
  !------------------------------------------------------------------------------
  ! !PUBLIC DATA:
  !------------------------------------------------------------------------------
  ! !PUBLIC TYPES:
  !


    PUBLIC esmf_time 
  !------------------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !
  ! Required inherited and overridden ESMF_Base class methods


  ! !PRIVATE MEMBER FUNCTIONS:


  ! Inherited and overloaded from ESMF_BaseTime


  !EOPI
  !------------------------------------------------------------------------------
  ! The following line turns the CVS identifier string into a printable variable.


  !==============================================================================
  ! INTERFACE BLOCKS
  !==============================================================================
  !BOP
  ! !INTERFACE:

  !
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !INTERFACE:
  !

  !------------------------------------------------------------------------------
  !==============================================================================
    PUBLIC kr_esmf_shrtimemod_esmf_time 
    PUBLIC kv_esmf_shrtimemod_esmf_time 
  !


  !==============================================================================
  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeGet - Get value in user-specified units
  ! !INTERFACE:
  !      subroutine ESMF_TimeGet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, MS, &
  !                              US, NS, d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, &
  !                              dayOfYear, dayOfYear_r8, dayOfYear_intvl,      &
  !                              timeString, rc)


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeSet - Initialize via user-specified unit set
  ! !INTERFACE:
  !      subroutine ESMF_TimeSet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, &
  !                              MS, US, NS, d_, h_, m_, s_, ms_, us_, ns_, &
  !                              Sn, Sd, calendar, calkindflag, rc)


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMFold_TimeGetString - Get time instant value in string format
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeGetDayOfYearInteger - Get time instant's day of the year as an integer value
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeInc - Increment time instant with a time interval
  ! !INTERFACE:

  !


  ! this is added for certain compilers that don't deal with commutativity


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeDec - Decrement time instant with a time interval
  ! !INTERFACE:

  !

  ! this is added for certain compilers that don't deal with commutativity

  !
  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeDiff - Return the difference between two time instants
  ! !INTERFACE:
  !
  !


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeEQ - Compare two times for equality
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeNE - Compare two times for non-equality
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeLT - Time instant 1 less than time instant 2 ?
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeGT - Time instant 1 greater than time instant 2 ?
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeLE - Time instant 1 less than or equal to time instant 2 ?
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE: ESMF_TimeGE - Time instant 1 greater than or equal to time instant 2 ?
  ! !INTERFACE:

  !

  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimeCopy - Copy a time-instance
  ! !INTERFACE:


  !------------------------------------------------------------------------------
  !BOP
  ! !IROUTINE:  ESMF_TimePrint - Print out a time instant's properties
  ! !INTERFACE:


  !==============================================================================


  !==============================================================================


        !==============================================================================


        !==============================================================================


        !==============================================================================


        !==============================================================================
        ! Increment Time by number of seconds between start of year and start
        ! of month MM.
        ! 1 <= MM <= 12
        ! Time is NOT normalized.


        !==============================================================================
        ! Increment Time by number of seconds between start of year and start
        ! of month MM.
        ! 1 <= MM <= 12
        ! Time is NOT normalized.


        !==============================================================================
        !==============================================================================

      end module ESMF_TimeMod