c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                            Copyright (C) 1995   
c 
c              University Corporation for Atmospheric Research
c 
c                           All rights reserved.
c 
c      No part of this  code may  be reproduced,  stored in  a retrieval
c      system,  translated,  transcribed,  transmitted or distributed in
c      any form or by any means, manual, electric, electronic, chemical,
c      electo-magnetic, mechanical, optical, photocopying, recording, or
c      otherwise, without the prior  explicit written permission  of the
c      author(s)  or their  designated proxies.  In no event  shall this
c      copyright notice be removed or altered in any way.
c 
c      This code is provided "as is",  without any warranty of any kind,
c      either  expressed or implied,  including but not limited to,  any
c      implied warranty of  merchantibility or fitness  for any purpose.
c      In no event will any party who distributed the code be liable for
c      damages or for any claim(s) by any other party, including but not
c      limited to,  any lost  profits,  lost monies,  lost data  or data
c      rendered inaccurate,  losses sustained  by third parties,  or any
c      other special, incidental or consequential damages arising out of
c      the use or inability to use the program,  even if the possibility
c      of such damages has  been advised against.  The entire risk as to
c      the quality,  the performance, and the fitness of the program for
c      any particular purpose lies with the party using the code.
c 
c      *****************************************************************
c      ANY USE OF  THIS CODE CONSTITUES  ACCEPTANCE OF  THE TERMS OF THE
c                               ABOVE STATEMENTS
c      *****************************************************************
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c     TEST INTRINSIC FUNCTION PERFORMANCE
c
      program testfun
#if defined(DEC)
      use timer_module
#endif
      implicit none

#ifndef DEC
      external timer_time
      real*8 timer_time
#endif

      external my_pe
      integer my_pe

      real*8 tmp

      integer it
      integer nfun,          ! number of points to study
     $        nmax,          ! maximum number of elements in arrays
     $        ktries,        ! number of trials
     $        itmax

#if defined(SINGLE_PREC_ONLY)
      parameter(nfun=5,nmax=100000,ktries=3,itmax=1000)
#else
      parameter(nfun=14,nmax=100000,ktries=3,itmax=1000)
#endif

      real*8  mega           ! one million 
      parameter(mega=1000000.0)

      real sa(nmax),sb(nmax),sc(nmax)
      integer di(nmax)
#if !defined(SINGLE_PREC_ONLY)
      double precision da(nmax),db(nmax),dc(nmax) 
      double precision dd(nmax),de(nmax)
#endif

      real*8 mcalls
      real*8 perf(nfun),timex(nfun)
      character*5 fnname(nfun)

      integer i,k,n

c     Initialize the performance arrays
c
      do i=1,nfun
        perf(i)=0.0
        timex(i)=1.e30
      end do

      fnname(1)='ALOG '
      fnname(2)='EXP  '
      fnname(3)='PWR  '
      fnname(4)='SIN  '
      fnname(5)='SQRT '
      fnname(6)='NINT '
      fnname(7)='DLOG '
      fnname(8)='DEXP '
      fnname(9)='DPWR '
      fnname(10)='DSIN '
      fnname(11)='DSQRT'
      fnname(12)='DNINT'
      fnname(13)='DDIV'
      fnname(14)='DRECIP'
  
      do k=1,ktries

         if (my_pe().eq.0) print *,'trial no.',k

         mcalls=nmax*itmax
c
c        Time each function, store in following table:
c
c        slot/timer           function
c        -------------------------------
c         1                    ALOG
c         2                    EXP
c         3                    PWR
c         4                    SIN
c         5                    SQRT
c         6                    NINT
#if !defined(SINGLE_PREC_ONLY)
c         7                    DLOG
c         8                    DEXP
c         9                    DPWR
c        10                    DSIN
c        11                    DSQRT
c        12                    DNINT
c        13                    DDIV
c        14                    DRECIP

#endif
c
c
c        Initialize source/dest array with something harmless
c

         do i=1,nmax
            sa(i)=2
            sb(i)=0
	    sc(i)=sin(float(nmax/i))
         end do

         call timer_clear(1)
         call timer_start(1)
	 do it=1,itmax
           call alogit(sb,sa,nmax)
	 enddo
         call timer_stop(1)

         call timer_clear(2)
         call timer_start(2)
         do it=1,itmax
            call expit(sb,sa,nmax)
	 enddo
         call timer_stop(2)

         call timer_clear(3)
         call timer_start(3)
	 do it=1,itmax
            call powerit(sb,sa,nmax)
	 enddo
         call timer_stop(3)

         call timer_clear(4)
         call timer_start(4)
	 do it=1,itmax
            call sinit(sb,sa,nmax)
	 enddo
         call timer_stop(4)

         call timer_clear(5)
         call timer_start(5)
	 do it=1,itmax
            call sqrtit(sb,sa,nmax)
	 enddo
         call timer_stop(5)

         call timer_clear(6)
         call timer_start(6)
	 do it=1,itmax
            call nintit(di,sc,nmax)
	 enddo
         call timer_stop(6)

#if !defined(SINGLE_PREC_ONLY)
c
c
c        Initialize source/dest array with something harmless
c
         do i=1,nmax
            da(i)=2
            db(i)=0
	    dc(i)=sin(float(nmax/i))
            dd(i)=3
      de(i)=cos(float(nmax/i))
         end do

         call timer_clear(7)
         call timer_start(7)
	 do it=1,itmax
            call dlogit(db,da,nmax)
	 enddo
         call timer_stop(7)

         call timer_clear(8)
         call timer_start(8)
	 do it=1,itmax
            call dexpit(db,da,nmax)
	 enddo
         call timer_stop(8)

         call timer_clear(9)
         call timer_start(9)
	 do it=1,itmax
            call dpowerit(db,da,nmax)
	 enddo
         call timer_stop(9)

         call timer_clear(10)
         call timer_start(10)
	 do it=1,itmax
            call dsinit(db,da,nmax)
	 enddo
         call timer_stop(10)

         call timer_clear(11)
         call timer_start(11)
	 do it=1,itmax
            call dsqrtit(db,da,nmax)
	 enddo
         call timer_stop(11)

         call timer_clear(12)
         call timer_start(12)
	 do it=1,itmax
            call dnintit(di,dc,nmax)
	 enddo
         call timer_stop(12)

         call timer_clear(13)
         call timer_start(13)
	 do it=1,itmax
            call ddivit(dd,dc,nmax)
	 enddo
         call timer_stop(13)


         call timer_clear(14)
         call timer_start(14)
	 do it=1,itmax
            call drecipit(de,nmax)
	 enddo
         call timer_stop(14)

#endif

         if (my_pe().eq.0) then
             do n=1,nfun
                tmp=timer_time(n)
                timex(n)=MIN(timex(n),tmp)
                perf(n)=MAX(perf(n),(mcalls)/(tmp*mega))
             end do
         end if

      end do

      if (my_pe() .eq. 0) then
      
          OPEN(UNIT=8,FILE="FUN.DAT",
     $         STATUS="UNKNOWN",FORM="FORMATTED")
          do n=1,nfun
             write(8,20)perf(n),timex(n),fnname(n)
          end do
 20       format(f12.6,1x,f12.6,1x,a5)
          CLOSE(UNIT=8)

      end if

      end 




