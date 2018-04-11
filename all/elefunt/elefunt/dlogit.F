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
c     Kernel to test DLOG performance
c
      subroutine dlogit(b,a,n1)
      implicit none

      integer n1
      double precision a(n1),b(n1)
      
      integer i

      do i=1,n1
         b(i)=DLOG(a(i))
      end do

      return 
      end
  
