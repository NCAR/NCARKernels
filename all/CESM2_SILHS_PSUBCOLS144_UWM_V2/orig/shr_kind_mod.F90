!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:56 
!KGEN version : 0.8.1 
  


MODULE shr_kind_mod
  !----------------------------------------------------------------------------
  ! precision/kind constants add data public
  !----------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    PUBLIC 
  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real

END MODULE shr_kind_mod