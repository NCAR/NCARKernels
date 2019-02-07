!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:38 
!KGEN version : 0.8.1 
  


module cam_abortutils

    USE shr_sys_mod, ONLY: endrun => shr_sys_abort 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

    PUBLIC endrun 

end module cam_abortutils