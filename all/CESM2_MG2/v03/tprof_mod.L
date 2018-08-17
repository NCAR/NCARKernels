NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TINIT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TINIT
FORMAT LIST

 LINE   LOOP      STATEMENT

    36:           subroutine tinit
    37:             integer :: n
    38:            
    39:             if ( tprof_init ) return
    40:           
    41: +------>    do n =1,max_clocks
    42: |             clock_details(n)%clock_time  = 0
    43: |             clock_details(n)%clock_total = 0
    44: |+=====>      clock_details(n)%clock_name  = ''
    45: +------     enddo
    46:           
    47:             tprof_init = .true.
    48:           
    49:           end subroutine tinit


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TNULL
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TNULL
FORMAT LIST

 LINE   LOOP      STATEMENT

    51:           subroutine tnull
    52:           
    53:             tprof_null = .true.
    54:           
    55:           end subroutine tnull


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TSTART
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

    63: inl(1222): Inlined: TPROF_MOD::TINIT
    65: inl(1222): Inlined: TPROF_MOD::TO_UPPER
    65: vec( 118): Unvectorizable data type.
    65: vec( 103): Unvectorized loop.
    65: vec( 108): Unvectorizable loop structure.
    65: vec( 103): Unvectorized loop.
    77: vec( 118): Unvectorizable data type.
    77: vec( 103): Unvectorized loop.
    81: opt(1084): Branch out of the loop inhibits optimization.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TSTART
FORMAT LIST

 LINE   LOOP      STATEMENT

    57:           subroutine tstart( block_name )
    58:             character(len=*),           intent(in) :: block_name
    59:             character(len=32) :: newStr
    60:             integer           :: n
    61:           
    62:             if ( tprof_null ) return
    63:         I   if ( .NOT. tprof_init ) call tinit( )
    64:           
    65: +======>I   newStr = TRIM( to_upper(block_name) )
    66:           
    67:           ! assign first clock
    68:             if ( num_clocks == 0 ) then
    69:               num_clocks = num_clocks + 1
    70: +======>      clock_details(num_clocks)%clock_name = newStr
    71:               call system_clock(start_clocks, rate_clock)
    72:               clock_details(num_clocks)%clock_time = start_clocks
    73:               return
    74:             endif
    75:           
    76:           ! assign if clock exists
    77: +------>    do n =1,num_clocks
    78: |             if ( newStr == clock_details(n)%clock_name ) then
    79: |               call system_clock(start_clocks, rate_clock)
    80: |               clock_details(n)%clock_time = start_clocks
    81: |               return
    82: |             endif
    83: +------     enddo
    84:           
    85:           ! assign new clock as one does not exist
    86:             num_clocks = num_clocks + 1
    87: +======>    clock_details(num_clocks)%clock_name = newStr
    88:             call system_clock(start_clocks, rate_clock)
    89:             clock_details(num_clocks)%clock_time = start_clocks
    90:           
    91:             if ( num_clocks+1 == max_clocks ) then
    92:               print *, 'Number of clocks used is greather than number of clocks available'
    93:               stop
    94:             endif
    95:           
    96:           end subroutine tstart


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TSTOP
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   105: inl(1222): Inlined: TPROF_MOD::TO_UPPER
   105: vec( 118): Unvectorizable data type.
   105: vec( 103): Unvectorized loop.
   105: vec( 108): Unvectorizable loop structure.
   105: vec( 103): Unvectorized loop.
   107: vec( 118): Unvectorizable data type.
   107: vec( 103): Unvectorized loop.
   112: opt(1084): Branch out of the loop inhibits optimization.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TSTOP
FORMAT LIST

 LINE   LOOP      STATEMENT

    98:           subroutine tstop( block_name )
    99:             character(len=*), intent(in) :: block_name
   100:             character(len=32) :: newStr
   101:             integer           :: n
   102:           
   103:             if ( tprof_null ) return
   104:           
   105: +======>I   newStr = TRIM( to_upper(block_name) )
   106:           
   107: +------>    do n =1,num_clocks
   108: |             if ( newStr == clock_details(n)%clock_name ) then
   109: |               call system_clock(stop_clocks, rate_clock)
   110: |               clock_details(n)%clock_time  = stop_clocks - clock_details(n)%clock_time
   111: |               clock_details(n)%clock_total = clock_details(n)%clock_total + clock_details(n)%clock_time
   112: |               return
   113: |             endif
   114: +------     enddo
   115:           
   116:             print *, block_name, ': Name not assigned for timing region'
   117:             stop
   118:           
   119:           end subroutine tstop


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TPRNT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   128: vec( 182): Deallocation obstructs vectorization.
   128: vec( 181): Allocation obstructs vectorization.
   128: vec( 180): I/O statement obstructs vectorization.
   129: opt(1118): This I/O statement inhibits optimization of loop.
   129: opt(1082): Backward transfers inhibit loop optimization.
   129: vec( 103): Unvectorized loop.
   129: vec( 108): Unvectorizable loop structure.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TPRNT
FORMAT LIST

 LINE   LOOP      STATEMENT

   121:           subroutine tprnt
   122:             integer :: n
   123:           
   124:             if( tprof_null ) then
   125:               write(6,1000)
   126:           1000 format( ' TPROF flag was set to null ')
   127:             else
   128: +------>      do n =1,num_clocks
   129: |               write(6,2000) TRIM(clock_details(n)%clock_name), clock_details(n)%clock_total/REAL(rate_clock)
   130: |           2000 format( ' Region Name = ', a32, ' : Time (secs) = ', f10.6)
   131: +------       enddo
   132:             endif
   133:           
   134:           end subroutine tprnt


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TO_UPPER
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   142: vec( 118): Unvectorizable data type.
   142: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:04 2018
FILE NAME: tprof_mod.f90

PROCEDURE NAME: TPROF_MOD::TO_UPPER
FORMAT LIST

 LINE   LOOP      STATEMENT

   136:           function to_upper(strIn) result(strOut)
   137:           
   138:                character(len=*), intent(in) :: strIn
   139:                character(len=len(strIn)) :: strOut
   140:                integer :: i, j
   141:           
   142: +------>       do i = 1,len(strIn)
   143: |                   j = iachar(strIn(i:i))
   144: |                   if (j>= iachar("a") .and. j<=iachar("z") ) then
   145: |                        strOut(i:i) = achar(iachar(strIn(i:i))-32)
   146: |                   else
   147: |                        strOut(i:i) = strIn(i:i)
   148: |                   end if
   149: +------        end do
   150:           
   151:           end function to_upper


