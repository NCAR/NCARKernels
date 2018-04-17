module timer_module

  integer, parameter, private:: MAX_TIMER = 64

  logical, private :: timer_vrgn = .true.

  real*8, private, dimension(0:MAX_TIMER)  :: start_time, &
                                              stop_time, &
                                              accum_time
  real*8, private, dimension(0:MAX_TIMER)  :: is_assigned,&
                                              nstarts

  integer, private ::  clock_rate, maximum_count

  private timer_clock,check_timer_id

contains

  subroutine check_timer_id(timer_id)
  integer :: timer_id
  if (timer_id < 0        .or. &
      timer_id >MAX_TIMER) then
      print *, "ERROR: timer id ", timer_id," out of range"
      stop
  end if
  if (is_assigned(timer_id).ne.1) then
      print *, "ERROR: timer id ", timer_id," not assigned"
      stop
  endif
  end subroutine check_timer_id

  real*8 function timer_clock()

! This is a private function which calculates
! the number of seconds that the cpu devoted to the program

  integer :: current_count

  call system_clock(count=current_count,     &
                    count_rate=clock_rate,   &
                    count_max=maximum_count)

  timer_clock = real(current_count)
  return
  end function timer_clock

  subroutine timer_clear(timer_id)
  integer :: timer_id
  if (timer_id < 0        .or. &
      timer_id >MAX_TIMER) then
      print *, "ERROR: timer id ", timer_id," out of range"
      stop
  end if
  if (timer_vrgn) then
     timer_vrgn=.false.
     do i=0,MAX_TIMER
        is_assigned(i)=0
     end do
  endif
  is_assigned(timer_id)=1
  accum_time(timer_id)=0.0
  nstarts(timer_id)=0  
  end subroutine timer_clear

  subroutine timer_start(timer_id)
  integer :: timer_id
  call check_timer_id(timer_id)
  nstarts(timer_id) = nstarts(timer_id)+1
  start_time(timer_id) = timer_clock()
  return
  end subroutine timer_start

  subroutine timer_stop(timer_id)
  integer timer_id
  real*8  temp
  temp = timer_clock()
  call check_timer_id(timer_id)
  stop_time(timer_id)=temp
  if (stop_time(timer_id) > start_time(timer_id)) then
     accum_time(timer_id) = accum_time(timer_id) + &
         (stop_time(timer_id) - start_time(timer_id))  &
         /real(clock_rate)
  else
     accum_time(timer_id) = accum_time(timer_id) + &
         (real(maximum_count)- &
         (stop_time(timer_id) - start_time(timer_id))) &
         /real(clock_rate)
  end if
  return
  end subroutine timer_stop

  real*8 function timer_time(timer_id)
  integer timer_id
  call check_timer_id(timer_id)
  timer_time= accum_time(timer_id)
  return
  end function timer_time

  integer function timer_calls(timer_id)
  integer timer_id
  call check_timer_id(timer_id)
  timer_calls=nstarts(timer_id)
  return
  end function timer_calls

end module timer_module





