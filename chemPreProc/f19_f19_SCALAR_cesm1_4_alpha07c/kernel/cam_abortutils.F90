!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:55
!KGEN version : 0.6.2

module cam_abortutils

    USE shr_sys_mod, ONLY: endrun => shr_sys_abort

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE
    PRIVATE
    SAVE

    PUBLIC endrun

end module cam_abortutils