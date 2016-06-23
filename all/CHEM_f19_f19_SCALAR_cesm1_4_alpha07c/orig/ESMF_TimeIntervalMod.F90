!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:54
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
!     ESMF TimeInterval Module

module ESMF_TimeIntervalMod

!
!==============================================================================
!
! This file contains the TimeInterval class definition and all TimeInterval
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
































! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  


!
!===============================================================================
!BOPI
! !MODULE: ESMF_TimeIntervalMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ implementaion of class {\tt ESMC\_TimeInterval}
!
! See {\tt ../include/ESMC\_TimeInterval.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from ESMF base class

      ! inherit from base time class
    USE esmf_basetimemod

      ! associated derived types
    USE esmf_shrtimemod, ONLY: esmf_time

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    USE esmf_shrtimemod, ONLY: kr_esmf_shrtimemod_esmf_time
    USE esmf_shrtimemod, ONLY: kv_esmf_shrtimemod_esmf_time
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
    IMPLICIT NONE
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
    PRIVATE
!------------------------------------------------------------------------------
!     ! ESMF_TimeInterval
!
!     ! F90 class type to match C++ TimeInterval class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type ESMF_TimeInterval
        ! time interval is expressed as basetime
        type(ESMF_BaseTime) :: basetime  ! inherit base class
        ! Relative year and month fields support monthly or yearly time 
        ! intervals.  Many operations are undefined when these fields are 
        ! non-zero!  
        INTEGER :: YR                    ! relative year
        INTEGER :: MM                    ! relative month
        logical :: starttime_set           ! reference time set
        type(ESMF_Time) :: starttime       ! reference time
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      PUBLIC esmf_timeinterval
!------------------------------------------------------------------------------
!
! for running WRF, add three subroutines or functions (WRFADDITION_TimeIntervalGet,
! ESMF_TimeIntervalDIVQuot, ESMF_TimeIntervalIsPositive), by jhe
! !PUBLIC MEMBER FUNCTIONS:

! Required inherited and overridden ESMF_Base class methods

!!!!!!!!! added by jhe
!

! !PRIVATE MEMBER FUNCTIONS:
 
! overloaded operator functions
 


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
!     This interface overloads the * operator for the {\tt ESMF\_TimeInterval}
!     class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the / operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the + operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the - operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:

! !PRIVATE MEMBER FUNCTIONS:

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the
!     {\tt ESMF\_TimeInterval} class
!
!EOP
!
!------------------------------------------------------------------------------

!==============================================================================

      PUBLIC kr_esmf_shrtimemod_esmf_time
      PUBLIC kr_esmf_timeintervalmod_esmf_timeinterval
      PUBLIC kv_esmf_shrtimemod_esmf_time
      PUBLIC kv_esmf_timeintervalmod_esmf_timeinterval

!==============================================================================
!
! Generic Get/Set routines which use F90 optional arguments
!
!---------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalGet - Get value in user-specified units

! !INTERFACE:
      
      CONTAINS
      

! !ARGUMENTS:


! !DESCRIPTION:
!     Get the value of the {\tt ESMF\_TimeInterval} in units specified by the
!     user via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally from integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to query
!     \item[{[YY]}]
!          Integer years (>= 32-bit)
!     \item[{[YYl]}]
!          Integer years (large, >= 64-bit)
!     \item[{[MO]}]
!          Integer months (>= 32-bit)
!     \item[{[MOl]}]
!          Integer months (large, >= 64-bit)
!     \item[{[D]}]
!          Integer days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer days (large, >= 64-bit)
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
!     TMG1.1
!EOP







!        seconds = seconds - years * ( 365_ESMF_KIND_I8 * 86400_ESMF_KIND_I8 )

        ! convert years and months to days carefully

!        write(6,*) 'tcxti1 ',mmon,lstarttime%yr,mstart,lstarttime%basetime%s


!        do nmon = 1,abs(mmon)







!           write(6,*) 'tcxti2 ',nmon,iyr,imo,ndays

!        write(6,*) 'tcxti3 ',mmon,iyr,imo,secondsym











      ! If d_r8 present and sec present




      


      

    

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalSet - Initialize via user-specified unit set

! !INTERFACE:
!      subroutine ESMF_TimeIntervalSet(timeinterval, YY, YYl, MM, MOl, D, Dl, &
!                                      H, M, S, Sl, MS, US, NS, &
!                                      d_, d_r8, h_, m_, s_, ms_, us_, ns_, &
!                                      Sn, Sd, startTime, rc)

! !ARGUMENTS:
!      integer(ESMF_KIND_I8), intent(in), optional :: YYl
!      integer(ESMF_KIND_I8), intent(in), optional :: MOl
!      integer(ESMF_KIND_I8), intent(in), optional :: Dl
!      integer, intent(in), optional :: US
!      integer, intent(in), optional :: NS
!      double precision, intent(in), optional :: h_
!      double precision, intent(in), optional :: m_
!      double precision, intent(in), optional :: s_
!      double precision, intent(in), optional :: ms_
!      double precision, intent(in), optional :: us_
!      double precision, intent(in), optional :: ns_
      ! locals

! !DESCRIPTION:
!     Set the value of the {\tt ESMF\_TimeInterval} in units specified by
!     the user via F90 optional arguments
!
!     Time manager represents and manipulates time internally with integers 
!     to maintain precision.  Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h} and
!     {\tt ../include/ESMC\_TimeInterval.h} for complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer number of interval years (>= 32-bit)
!     \item[{[YYl]}]
!          Integer number of interval years (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer number of interval months (>= 32-bit)
!     \item[{[MOl]}]
!          Integer number of interval months (large, >= 64-bit)
!     \item[{[D]}]
!          Integer number of interval days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer number of interval days (large, >= 64-bit)
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
!     TMGn.n.n
!EOP





      ! note that YR and MM are relative









!$$$ push H,M,S,Sn,Sd,MS down into BaseTime constructor











!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMFold_TimeIntervalGetString - Get time interval value in string format

! !INTERFACE:

! !ARGUMENTS:
      ! locals
!      integer :: signnormtimeint

! !DESCRIPTION:
!     Convert {\tt ESMF\_TimeInterval}'s value into string format
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.9
!EOP

! NOTE:  Sn, and Sd are not yet included in the returned string...  
!PRINT *,'DEBUG ESMFold_TimeIntervalGetString():  YR,MM,S,Sn,Sd = ', &
!        timeinterval%YR, &
!        timeinterval%MM, &
!        timeinterval%basetime%S, &
!        timeinterval%basetime%Sn, &
!        timeinterval%basetime%Sd




!$$$here...  need to print Sn and Sd when they are used ???



!write(0,*)'TimeIntervalGetString Sn ',timeinterval%basetime%Sn,' Sd ',timeinterval%basetime%Sd



!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalAbsValue - Get the absolute value of a time interval

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:
! !LOCAL:

! !DESCRIPTION:
!     Return a {\tt ESMF\_TimeInterval}'s absolute value.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the absolute value of.
!          Absolute value returned as value of function.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
!$$$here...  move implementation into BaseTime
      !


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNegAbsValue - Get the negative absolute value of a time interval

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:
! !LOCAL:

! !DESCRIPTION:
!     Return a {\tt ESMF\_TimeInterval}'s negative absolute value.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The object instance to take the negative absolute value of.
!          Negative absolute value returned as value of function.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.8
!EOP
!$$$here...  move implementation into BaseTime
      !


!------------------------------------------------------------------------------
!
! This section includes overloaded operators defined only for TimeInterval
! (not inherited from BaseTime)
! Note:  these functions do not have a return code, since F90 forbids more
! than 2 arguments for arithmetic overloaded operators
!
!------------------------------------------------------------------------------

! new WRF-specific function, Divide two time intervals and return the whole integer, without remainder

! !RETURN VALUE:

! !ARGUMENTS:

! !LOCAL

! !DESCRIPTION:
!     Returns timeinterval1 divided by timeinterval2 as a fraction quotient.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The dividend
!     \item[timeinterval2]
!          The divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.5
!EOP




! repeated subtraction



! added by jhe
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   WRFADDITION_TimeIntervalProdI - Multiply a time interval by an
! integer

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:
! !LOCAL:

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by an integer, return product
!     as a
!     {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Integer multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

!$$$move this into overloaded operator(*) in BaseTime
      ! Don't multiply Sd


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalQuotI - Divide time interval by an integer, return time interval result 

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Divides a {\tt ESMF\_TimeInterval} by an integer divisor, returns
!     quotient as a {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The dividend
!     \item[divisor]
!          Integer divisor
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.6, TMG5.3, TMG7.2
!EOP

!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() A:  S,Sn,Sd = ', &
!  timeinterval%basetime%S,timeinterval%basetime%Sn,timeinterval%basetime%Sd
!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() A:  divisor = ', divisor



!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() B:  S,Sn,Sd = ', &
!  ESMF_TimeIntervalQuotI%basetime%S,ESMF_TimeIntervalQuotI%basetime%Sn,ESMF_TimeIntervalQuotI%basetime%Sd
!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() C:  S,Sn,Sd = ', &
!  ESMF_TimeIntervalQuotI%basetime%S,ESMF_TimeIntervalQuotI%basetime%Sn,ESMF_TimeIntervalQuotI%basetime%Sd

!PRINT *,'DEBUG ESMF_TimeIntervalQuotI() D:  S,Sn,Sd = ', &
!  ESMF_TimeIntervalQuotI%basetime%S,ESMF_TimeIntervalQuotI%basetime%Sn,ESMF_TimeIntervalQuotI%basetime%Sd


!------------------------------------------------------------------------------
!BOP
! !IROUTINE:   ESMF_TimeIntervalProdI - Multiply a time interval by an integer

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:
! !LOCAL:

! !DESCRIPTION:
!     Multiply a {\tt ESMF\_TimeInterval} by an integer, return product as a
!     {\tt ESMF\_TimeInterval}
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          The multiplicand
!     \item[mutliplier]
!          Integer multiplier
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.7, TMG7.2
!EOP

!$$$move this into overloaded operator(*) in BaseTime
      ! Don't multiply Sd


!------------------------------------------------------------------------------
!
! This section includes the inherited ESMF_BaseTime class overloaded operators
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalSum - Add two time intervals together

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:
! !LOCAL:
! !DESCRIPTION:
!     Add two {\tt ESMF\_TimeIntervals}, return sum as a
!     {\tt ESMF\_TimeInterval}.  Maps overloaded (+) operator interface
!     function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The augend 
!     \item[timeinterval2]
!          The addend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, 
!                 TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalDiff - Subtract one time interval from another
   
! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS: 
! !LOCAL:
! !DESCRIPTION:
!     Subtract timeinterval2 from timeinterval1, return remainder as a 
!     {\tt ESMF\_TimeInterval}.
!     Map overloaded (-) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          The minuend 
!     \item[timeinterval2]
!          The subtrahend
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_TimeIntervalEQ - Compare two time intervals for equality

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

!DESCRIPTION:
!     Return true if both given time intervals are equal, false otherwise.
!     Maps overloaded (==) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalNE - Compare two time intervals for inequality

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Return true if both given time intervals are not equal, false otherwise.
!     Maps overloaded (/=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLT - Time interval 1 less than time interval 2 ?

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Return true if first time interval is less than second time interval,
!     false otherwise. Maps overloaded (<) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGT - Time interval 1 greater than time interval 2?

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Return true if first time interval is greater than second time interval,
!     false otherwise.  Maps overloaded (>) operator interface function to
!     {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalLE - Time interval 1 less than or equal to time interval 2 ?

! !INTERFACE:

! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Return true if first time interval is less than or equal to second time
!     interval, false otherwise.
!     Maps overloaded (<=) operator interface function to {\tt ESMF\_BaseTime}
!     base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalGE - Time interval 1 greater than or equal to time interval 2 ?

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !DESCRIPTION:
!     Return true if first time interval is greater than or equal to second
!     time interval, false otherwise. Maps overloaded (>=) operator interface
!     function to {\tt ESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval1]
!          First time interval to compare
!     \item[timeinterval2]
!          Second time interval to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP




!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalIsPositive - Time interval greater than zero?

! !INTERFACE:
!
! !RETURN VALUE:

! !ARGUMENTS:

! !LOCALS:

! !DESCRIPTION:
!     Return true if time interval is greater than zero,  
!     false otherwise. 
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to compare
!     \end{description}
!EOP


! hack for bug in PGI 5.1-x
!      ESMF_TimeIntervalIsPositive = timeinterval > zerotimeint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  ESMF_TimeIntervalPrint - Print out a time interval's properties

! !INTERFACE:

! !ARGUMENTS:

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt ESMF\_TimeInterval}'s
!     properties.
!
!     The arguments are:
!     \begin{description}
!     \item[timeinterval]
!          Time interval to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP




!------------------------------------------------------------------------------

! Exits with error message if timeInt is not normalized.  
  ! locals



! tcraig, don't require normalize TimeInterval for relative diffs
!       outstr = 'un-normalized TimeInterval not allowed:  '//TRIM(msgstr)
!       CALL wrf_error_fatal( outstr )

!==============================================================================


!==============================================================================

!
! !ARGUMENTS:







!==============================================================================


  ! normalize basetime
  ! this will force abs(Sn) < Sd and ensure that signs of S and Sn match
  ! YR and MM are ignored


  ! Rollover months to years



  ! make sure yr and mm have same sign





!==============================================================================

  ! Compute the sign of a time interval.
  ! YR and MM fields are *IGNORED*.
  ! returns 1, 0, or -1 or exits if timeInt fields have inconsistent signs.

  ! Note that Sd is required to be non-negative.  This is enforced in
  ! normalize_timeint().
  ! Note that Sn is required to be zero when Sd is zero.  This is enforced
  ! in normalize_timeint().



!==============================================================================

      !read state subroutine for kr_esmf_timeintervalmod_esmf_timeinterval
      RECURSIVE SUBROUTINE kr_esmf_timeintervalmod_esmf_timeinterval(var, kgen_unit, printvar)
          TYPE(esmf_timeinterval), INTENT(INOUT) :: var
          INTEGER, INTENT(IN) :: kgen_unit
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_basetimemod_esmf_basetime(var%basetime, kgen_unit, printvar // "%basetime")
          ELSE
              CALL kr_esmf_basetimemod_esmf_basetime(var%basetime, kgen_unit)
          END IF 
          
          READ (UNIT = kgen_unit) var%yr
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%yr **" // NEW_LINE("A"), var%yr
          END IF 
          
          READ (UNIT = kgen_unit) var%mm
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%mm **" // NEW_LINE("A"), var%mm
          END IF 
          
          READ (UNIT = kgen_unit) var%starttime_set
          IF (PRESENT( printvar )) THEN
              WRITE (*, *) "** KGEN DEBUG: " // printvar // "%starttime_set **" // NEW_LINE("A"), var%starttime_set
          END IF 
          
          IF (PRESENT( printvar )) THEN
              CALL kr_esmf_shrtimemod_esmf_time(var%starttime, kgen_unit, printvar // "%starttime")
          ELSE
              CALL kr_esmf_shrtimemod_esmf_time(var%starttime, kgen_unit)
          END IF 
          
      END SUBROUTINE kr_esmf_timeintervalmod_esmf_timeinterval
      
      !verify state subroutine for kv_esmf_timeintervalmod_esmf_timeinterval
      RECURSIVE SUBROUTINE kv_esmf_timeintervalmod_esmf_timeinterval(varname, check_status, var, kgenref_var)
          CHARACTER(LEN=*), INTENT(IN) :: varname
          TYPE(check_t), INTENT(INOUT) :: check_status
          TYPE(esmf_timeinterval), INTENT(IN) :: var, kgenref_var
          TYPE(check_t) :: dtype_check_status, comp_check_status
          INTEGER :: check_result
          LOGICAL :: is_print = .FALSE.
          
          integer :: diff_yr
          integer :: diff_mm
          logical :: diff_starttime_set
          
          check_status%numTotal = check_status%numTotal + 1
          
          CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%yr == kgenref_var%yr) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%yr is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_yr = ABS(var%yr - kgenref_var%yr)
              IF (diff_yr <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%yr is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%yr is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_yr
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_yr
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%mm == kgenref_var%mm) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%mm is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              diff_mm = ABS(var%mm - kgenref_var%mm)
              IF (diff_mm <= dtype_check_status%tolerance) THEN
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%mm is NOT IDENTICAL(within tolerance)."
                  END IF 
                  check_result = CHECK_IN_TOL
              ELSE
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
                  IF (check_status%verboseLevel > 1) THEN
                      WRITE (*, *) trim(adjustl(varname)), "%mm is NOT IDENTICAL(out of tolerance)."
                  END IF 
                  check_result = CHECK_OUT_TOL
              END IF 
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_mm
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "Difference is ", diff_mm
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (var%starttime_set .EQV. kgenref_var%starttime_set) THEN
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%starttime_set is IDENTICAL."
              END IF 
              check_result = CHECK_IDENTICAL
          ELSE
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
              IF (check_status%verboseLevel > 1) THEN
                  WRITE (*, *) trim(adjustl(varname)), "%starttime_set is NOT IDENTICAL."
              END IF 
              check_result = CHECK_OUT_TOL
          END IF 
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1
          IF (check_result == CHECK_IDENTICAL) THEN
          ELSE IF (check_result == CHECK_OUT_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          ELSE IF (check_result == CHECK_IN_TOL) THEN
              IF (check_status%verboseLevel > 2) THEN
                  WRITE (*, *) "NOT IMPLEMENTED YET"
                  WRITE (*, *) ""
              END IF 
          END IF 
          
          IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN
              check_status%numIdentical = check_status%numIdentical + 1
          ELSE IF (dtype_check_status%numOutTol > 0) THEN
              check_status%numOutTol = check_status%numOutTol + 1
          ELSE IF (dtype_check_status%numInTol > 0) THEN
              check_status%numInTol = check_status%numInTol + 1
          END IF 
      END SUBROUTINE kv_esmf_timeintervalmod_esmf_timeinterval
      
end module ESMF_TimeIntervalMod
