!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:44 
!KGEN version : 0.7.3 
  


module cam_abortutils

    USE shr_sys_mod, ONLY: endrun => shr_sys_abort 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 

    PUBLIC endrun 

end module cam_abortutils