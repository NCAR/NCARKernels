!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:50:04 
!KGEN version : 0.7.0 
  
module cam_abortutils

    USE shr_sys_mod, ONLY: endrun => shr_sys_abort 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    IMPLICIT NONE 
    PRIVATE 
    SAVE 

    PUBLIC endrun 

end module cam_abortutils