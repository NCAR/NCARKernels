! This module contains extensions to KGEN that aren't yet in the main code
!
! Currently, this is focused on having a simple, common way of specifying that
! input variables should be perturbed ('-perturb' on the cmd line) and does a
! one-line call to perform this action.
!
! Two functions right now:
!   1) 'read_runtime_options' checks the commandline for '-perturb' and sets a
!       flag to true if found, false otherwise
!   2) 'perturb_inputs' is called with up to 6 arguments (for now) and will
!       perturb each one with an epsilon per the CESM perturbation scheme.  


module kgen_extensions_mod
  use kernel_info
  implicit none

  logical :: perturb_flag = .false.
  logical :: stats_flag   = .false.

  interface variable_stats
      module procedure variable_stats_1DR8
      module procedure variable_stats_2DR8
      module procedure variable_stats_3DR8
  end interface

  contains

    subroutine read_runtime_options

      character(len=*), parameter :: version = '1.0'
      character(len=32) :: arg
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      integer :: i

      do i = 1, command_argument_count()
        call get_command_argument(i, arg)

        select case (arg)
        case ('-p', '-perturb', '--perturb')
           perturb_flag = .true.
        case ('-h', '--help')
           call print_help_message()
           stop
        case ('-i', '-info', '--info')
           call print_info_message()
           stop
        case default
           !print '(a,a,/)', 'Unrecognized command-line option: ', arg
           call print_help_message()
           stop
        end select
     end do

     ! Print the date and, optionally, the time
     !call date_and_time(DATE=date, TIME=time, ZONE=zone)
     !write (*, '(a,"-",a,"-",a)', advance='no') date(1:4), date(5:6), date(7:8)
     !if (perturb_flag) then
     !  write(*,*) "Performing perturbation of input variables..."
     !else
     !   write (*,*) " Standard run... "
     !end if

    end subroutine read_runtime_options

    subroutine variable_stats_1DR8(var, name)
        REAL(kind=8), dimension(:) :: var
        character(len=*) :: name

        write(*,'(A,A,ES16.8,A,ES16.8,A,ES16.8)') name,":   min = ",minval(var),",   max = ",maxval(var),",   mean = ", sum(var) / size(var)

    end subroutine variable_stats_1DR8

    subroutine variable_stats_2DR8(var, name)
        REAL(kind=8), dimension(:,:) :: var
        character(len=*) :: name

        write(*,'(A,A,ES16.8,A,ES16.8,A,ES16.8)') name,":   min = ",minval(var),",   max = ",maxval(var),",   mean = ", sum(var) / size(var)

    end subroutine variable_stats_2DR8

    subroutine variable_stats_3DR8(var, name)
        REAL(kind=8), dimension(:,:,:) :: var
        character(len=*) :: name

        write(*,'(A,A,ES16.8,A,ES16.8,A,ES16.8)') name,":   min = ",minval(var),",   max = ",maxval(var),",   mean = ", sum(var) / size(var)

    end subroutine variable_stats_3DR8

    !--------------------------------------------!

end module


