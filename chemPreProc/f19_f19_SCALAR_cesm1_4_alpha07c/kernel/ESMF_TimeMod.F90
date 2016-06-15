!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:55
!KGEN version : 0.6.2

! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Time Module
      module ESMF_TimeMod
!
!==============================================================================
!
! This file contains the Time class definition and all Time class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
































! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  



!==============================================================================
!BOPI
! !MODULE: ESMF_TimeMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation
!
! See {\tt ../include/ESMC\_Time.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class

      ! inherit from base time class

      ! associated derived types
          USE esmf_shrtimemod, ONLY: esmf_time
! added by Jhe

          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE esmf_shrtimemod, ONLY: kr_esmf_shrtimemod_esmf_time
          USE esmf_shrtimemod, ONLY: kv_esmf_shrtimemod_esmf_time
          IMPLICIT NONE
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
          PRIVATE
!------------------------------------------------------------------------------
!     ! ESMF_Time
!
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
          PUBLIC esmf_time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! Required inherited and overridden ESMF_Base class methods


! !PRIVATE MEMBER FUNCTIONS:


! Inherited and overloaded from ESMF_BaseTime









!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.


!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the {\tt ESMF\_GetDayOfYear} method
!     for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the + operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the = operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the {\tt ESMF\_Time} class
!
!EOP
!
!------------------------------------------------------------------------------

!==============================================================================

          PUBLIC kr_esmf_shrtimemod_esmf_time
          PUBLIC kv_esmf_shrtimemod_esmf_time

!==============================================================================
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGet - Get value in user-specified units

! !INTERFACE:
!      subroutine ESMF_TimeGet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, MS, &
!                              US, NS, d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, &
!                              dayOfYear, dayOfYear_r8, dayOfYear_intvl,      &
!                              timeString, rc)


! !ARGUMENTS:
!      integer(ESMF_KIND_I8), intent(out), optional :: YRl
!      integer(ESMF_KIND_I8), intent(out), optional :: Sl
!      integer, intent(out), optional :: US
!      integer, intent(out), optional :: NS
!      double precision, intent(out), optional :: d_
!      double precision, intent(out), optional :: h_
!      double precision, intent(out), optional :: m_
!      double precision, intent(out), optional :: s_
!      double precision, intent(out), optional :: ms_
!      double precision, intent(out), optional :: us_
!      double precision, intent(out), optional :: ns_


! !DESCRIPTION:
!     Get the value of the {\tt ESMF\_Time} in units specified by the user
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally from integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{[YY]}]
!          Integer year CCYR (>= 32-bit)
!     \item[{[YRl]}]
!          Integer year CCYR (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer Julian days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6
!EOP











!
!$$$ push HMS down into ESMF_BaseTime








        ! This duplication for YMD is an optimization that avoids calling 
        ! timegetmonth() and timegetdayofmonth() when it is not needed.  
!$$$ push HMS down into ESMF_BaseTime








!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeSet - Initialize via user-specified unit set

! !INTERFACE:
!      subroutine ESMF_TimeSet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, &
!                              MS, US, NS, d_, h_, m_, s_, ms_, us_, ns_, &
!                              Sn, Sd, calendar, calkindflag, rc)


! !ARGUMENTS:
!      integer(ESMF_KIND_I8), intent(in), optional :: YRl
!      integer(ESMF_KIND_I8), intent(in), optional :: Sl
!      integer, intent(in), optional :: US
!      integer, intent(in), optional :: NS
!      double precision, intent(in), optional :: d_
!      double precision, intent(in), optional :: h_
!      double precision, intent(in), optional :: m_
!      double precision, intent(in), optional :: s_
!      double precision, intent(in), optional :: ms_
!      double precision, intent(in), optional :: us_
!      double precision, intent(in), optional :: ns_

      ! locals

! !DESCRIPTION:
!     Initializes a {\tt ESMF\_Time} with a set of user-specified units
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer year CCYR (>= 32-bit)
!     \item[{[YRl]}]
!          Integer year CCYR (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer Julian days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[cal]}]
!          Associated {\tt Calendar}
!     \item[{[tz]}]
!          Associated timezone (hours offset from GMT, e.g. EST = -5)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
!  PRINT *,'DEBUG:  BEGIN ESMF_TimeSet()'
!$$$ push this down into ESMF_BaseTime constructor




!  PRINT *,'DEBUG:  ESMF_TimeSet():  using passed-in calendar'

!        call flush(6)
!        write(6,*) 'tcx1 ESMF_TimeSet point to calendar'
!        call flush(6)
!  PRINT *,'DEBUG:  ESMF_TimeSet():  using default calendar'
! for the sake of WRF, check ESMF_IsInitialized, revised by Juanxiong He

!        IF ( .not. ESMF_CalendarInitialized( defaultCal ) )THEN
!           call wrf_error_fatal( "Error:: ESMF_Initialize not called")
!        END IF
!           write(6,*) 'tcx2 ESMF_TimeSet point to calendarkindflag',calkindflag%caltype
!           call flush(6)

!           write(6,*) 'tcx3 ESMF_TimeSet point to defaultcal'
!           call flush(6)
!      write(6,*) 'tcxn ESMF_TimeSet ',ESMF_CALKIND_NOLEAP%caltype
!      call flush(6)
!      write(6,*) 'tcxg ESMF_TimeSet ',ESMF_CALKIND_GREGORIAN%caltype
!      call flush(6)
!      write(6,*) 'tcxt ESMF_TimeSet ',time%calendar%type%caltype
!      call flush(6)



!  PRINT *,'DEBUG:  ESMF_TimeSet():  YY = ',YY


!  PRINT *,'DEBUG:  ESMF_TimeSet():  MM = ',MM

!  PRINT *,'DEBUG:  ESMF_TimeSet():  back from timeaddmonths'

!$$$ no check for DD in range of days of month MM yet
!$$$ Must separate D and DD for correct interface!
!  PRINT *,'DEBUG:  ESMF_TimeSet():  DD = ',DD
!$$$ push H,M,S,Sn,Sd,MS down into ESMF_BaseTime constructor
!  PRINT *,'DEBUG:  ESMF_TimeSet():  H = ',H
!  PRINT *,'DEBUG:  ESMF_TimeSet():  M = ',M
!  PRINT *,'DEBUG:  ESMF_TimeSet():  S = ',S


!  PRINT *,'DEBUG:  ESMF_TimeSet():  MS = ',MS
!  PRINT *,'DEBUG:  ESMF_TimeSet():  Sd = ',Sd
!  PRINT *,'DEBUG:  ESMF_TimeSet():  Sn = ',Sn

!  PRINT *,'DEBUG:  ESMF_TimeSet():  calling normalize_time()'
!$$$DEBUG
!IF ( time%basetime%Sd > 0 ) THEN
!  PRINT *,'DEBUG ESMF_TimeSet() before normalize:  S,Sn,Sd = ', &
!    time%basetime%S, time%basetime%Sn, time%basetime%Sd
!ENDIF
!$$$END DEBUG
!$$$DEBUG
!IF ( time%basetime%Sd > 0 ) THEN
!  PRINT *,'DEBUG ESMF_TimeSet() after normalize:  S,Sn,Sd = ', &
!    time%basetime%S, time%basetime%Sn, time%basetime%Sd
!ENDIF
!$$$END DEBUG

!  PRINT *,'DEBUG:  ESMF_TimeSet():  back from normalize_time()'



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFold_TimeGetString - Get time instant value in string format

! !INTERFACE:

! !ARGUMENTS:
! !DESCRIPTION:
!     Convert {\tt ESMF\_Time}'s value into ISO 8601 format YYYY-MM-DDThh:mm:ss
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.4.7
!EOP

!PRINT *,'DEBUG:  ESMF_TimePrint():  YR,S,Sn,Sd = ',time%YR,time%basetime%S,time%basetime%Sn,time%basetime%Sd
!PRINT *,'DEBUG:  ESMF_TimePrint():  year = ',year
!PRINT *,'DEBUG:  ESMF_TimePrint():  month, dayofmonth = ',month,dayofmonth
!PRINT *,'DEBUG:  ESMF_TimePrint():  hour = ',hour
!PRINT *,'DEBUG:  ESMF_TimePrint():  minute = ',minute
!PRINT *,'DEBUG:  ESMF_TimePrint():  second = ',second

!$$$here...  add negative sign for YR<0
!$$$here...  add Sn, Sd ??


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGetDayOfYearInteger - Get time instant's day of the year as an integer value
!
! !INTERFACE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt ESMF\_Time} instant falls on
!     (1-365).  Returned as an integer value
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfYear]
!          The {\tt ESMF\_Time} instant's day of the year (1-365)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      ! requires that time be normalized
!$$$ bug when Sn>0?  test
!$$$ add tests


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeInc - Increment time instant with a time interval
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
! !LOCAL:
!
! !DESCRIPTION:
!     Increment {\tt ESMF\_Time} instant with a {\tt ESMF\_TimeInterval},
!     return resulting {\tt ESMF\_Time} instant
!
!     Maps overloaded (+) operator interface function to
!     {\tt ESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt ESMF\_Time} to increment
!     \item[timeinterval]
!          The {\tt ESMF\_TimeInterval} to add to the given {\tt ESMF\_Time}
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP


      ! copy ESMF_Time specific properties (e.g. calendar, timezone) 

!      write(6,*) 'tcx timeinc1 ',ESMF_TimeInc%yr,ESMF_TimeInc%basetime%s

!      write(6,*) 'tcx timeint ',timeinterval%yr,timeinterval%mm,timeinterval%basetime%s

      ! add years and months by manually forcing incremental years then adjusting the day of
      ! the month at the end if it's greater than the number of days in the month
      ! esmf seems to do exactly this based on testing

!         write(6,*) 'tcx timeinc mon1 ',year,month,day,sec,nyr,nmon


!         write(6,*) 'tcx timeinc mon2 ',year,month,day,sec
!         write(6,*) 'tcx timeinc mon3 ',nmon,year,month,day,sec

      ! finally add seconds

!      write(6,*) 'tcx timeinc sec ',ESMF_TimeInc%basetime%s,timeinterval%basetime%s

      ! and normalize

!      write(6,*) 'tcx timeinc2p ',ESMF_TimeInc%yr,ESMF_TimeInc%basetime%s


!      write(6,*) 'tcx timeinc2 ',ESMF_TimeInc%yr,ESMF_TimeInc%basetime%s


! this is added for certain compilers that don't deal with commutativity



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeDec - Decrement time instant with a time interval
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
! !LOCAL:

! !DESCRIPTION:
!     Decrement {\tt ESMF\_Time} instant with a {\tt ESMF\_TimeInterval},
!     return resulting {\tt ESMF\_Time} instant
!
!     Maps overloaded (-) operator interface function to
!     {\tt ESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt ESMF\_Time} to decrement
!     \item[timeinterval]
!          The {\tt ESMF\_TimeInterval} to subtract from the given
!          {\tt ESMF\_Time}
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP


!$$$push this down into a unary negation operator on TimeInterval


!
! this is added for certain compilers that don't deal with commutativity
!

!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeDiff - Return the difference between two time instants
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
! !LOCAL:

! !DESCRIPTION:
!     Return the {\tt ESMF\_TimeInterval} difference between two
!     {\tt ESMF\_Time} instants, time1 - time2
!
!     Maps overloaded (-) operator interface function to
!     {\tt ESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt ESMF\_Time} instant
!     \item[time2]
!          The second {\tt ESMF\_Time} instant
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP



!      write(6,*) 'tcx timediff1 ',time2%yr,time2%basetime%s,time2%calendar%type%caltype
!      write(6,*) 'tcx timediff2 ',time1%yr,time1%basetime%s,time1%calendar%type%caltype


      ! Can either be yr/month based diff if diff is only in year and month
      ! or absolute seconds if diff in day/seconds as well

!         write(6,*) 'tcx timedifft ym'
!         write(6,*) 'tcx timedifft sec'
!              write(6,*) 'tcx timediff3 ',yr,nsecondsinyear(yr,time2%calendar%type)
!              write(6,*) 'tcx timediff4 ',yr,nsecondsinyear(yr,time2%calendar%type)

!      write(6,*) 'tcx timediff5 ',ESMF_TimeDiff%YR, ESMF_TimeDiff%MM, ESMF_TimeDiff%basetime%s


!      write(6,*) 'tcx timediff6 ',ESMF_TimeDiff%YR, ESMF_TimeDiff%MM, ESMF_TimeDiff%basetime%s


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeEQ - Compare two times for equality
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Return true if both given {\tt ESMF\_Time} instants are equal, false
!     otherwise.  Maps overloaded (==) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeNE - Compare two times for non-equality
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:

! !DESCRIPTION:
!     Return true if both given {\tt ESMF\_Time} instants are not equal, false
!     otherwise.  Maps overloaded (/=) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeLT - Time instant 1 less than time instant 2 ?
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Return true if first {\tt ESMF\_Time} instant is less than second
!     {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (<)
!     operator interface function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGT - Time instant 1 greater than time instant 2 ?
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Return true if first {\tt ESMF\_Time} instant is greater than second
!     {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (>) operator
!     interface function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeLE - Time instant 1 less than or equal to time instant 2 ?
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Return true if first {\tt ESMF\_Time} instant is less than or equal to
!     second {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (<=)
!     operator interface function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeGE - Time instant 1 greater than or equal to time instant 2 ?
!
! !INTERFACE:
!
! !RETURN VALUE:
!
! !ARGUMENTS:
!
! !DESCRIPTION:
!     Return true if first {\tt ESMF\_Time} instant is greater than or equal to
!     second {\tt ESMF\_Time} instant, false otherwise.  Maps overloaded (>=)
!     operator interface function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeCopy - Copy a time-instance

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Copy a time-instance to a new instance.
!
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
!tcx      timeout%Calendar = timein%Calendar
!      write(6,*) 'tcxa ESMF_TimeCopy'
!      call flush(6)
!      write(6,*) 'tcxb ESMF_TimeCopy',timein%calendar%type%caltype
!      call flush(6)



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimePrint - Print out a time instant's properties


! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt ESMF\_Time}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          {\tt ESMF\_Time} instant to print out
!     \item[{[options]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP

      ! Quick hack to mimic ESMF 2.0.1
      ! Really should check value of options...  



!==============================================================================



!==============================================================================

!
! !ARGUMENTS:



!==============================================================================

  ! A normalized time has time%basetime >= 0, time%basetime less than the current
  ! year expressed as a timeInterval, and time%YR can take any value
!  INTEGER(ESMF_KIND_I8) :: nsecondsinyear
  ! locals

  ! first, normalize basetime
  ! this will force abs(Sn) < Sd and ensure that signs of S and Sn match


  ! next, underflow negative seconds into YEARS
  ! time%basetime must end up non-negative



  ! next, overflow seconds into YEARS



!==============================================================================

  ! locals



!==============================================================================
  ! locals



!==============================================================================

! Increment Time by number of seconds between start of year and start 
! of month MM.  
! 1 <= MM <= 12
! Time is NOT normalized.  
  ! locals





!==============================================================================

! Increment Time by number of seconds between start of year and start 
! of month MM.  
! 1 <= MM <= 12
! Time is NOT normalized.  




!==============================================================================
!==============================================================================
end module ESMF_TimeMod