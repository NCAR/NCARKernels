!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  


MODULE shr_kind_mod
  !----------------------------------------------------------------------------
  ! precision/kind constants add data public
  !----------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    PUBLIC 
  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
  integer,parameter :: SHR_KIND_I8 = selected_int_kind (13) ! 8 byte integer
  integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer
  integer,parameter :: SHR_KIND_CX = 512                    ! extra-long char

END MODULE shr_kind_mod