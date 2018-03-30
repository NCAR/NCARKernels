module kernel_info

  implicit none

  character(len=*), parameter :: name = "WACCM_imp_sol_vector"

  contains

    subroutine print_help_message
      write(*,*) ""
      write(*,*) "Use:  ./kernel.exe [options]"
      write(*,*) ""
      write(*,*) "  Options are:"
      write(*,*) "    -h | -help    : Print out this help message"
      write(*,*) "    -i | -info    : Give additional information about this kernel"
      write(*,*) "    -p | -perturb : Perform a perturbation test using machine epsilon"
      write(*,*) ""
    end subroutine print_help_message

    subroutine print_info_message
      write(*,'(A,A,A)') "Kernel Info: '",name, "' Validated on NCAR's Cheyenne (BDW) with:"

      !write(*,*) "    Run is identical on NCAR's Cheyenne (BDW) system with:"
      write(*,'(A)') "     - ifort 18.0.1"
      write(*,'(A)') "     - FC_FLAGS = -convert big_endian -assume byterecl -fp-model source -free -no-fma -xCORE-AVX2 -O3 -align array64byte -ftz -assume realloc_lhs"
      write(*,*) ""
    end subroutine print_info_message
end module kernel_info
