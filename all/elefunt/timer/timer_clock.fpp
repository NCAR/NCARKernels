      real*8 function timer_clock()
!
!     This is a private function which calculates
!     the number of seconds that the cpu devoted to the program
!
      integer current_count, clock_rate, maximum_count

      call system_clock(current_count, 
     $                  clock_rate,   
     $                  maximum_count)

      timer_clock=real(current_count)/real(clock_rate)

      return 
      end 





