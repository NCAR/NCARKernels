!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:54
!KGEN version : 0.6.2

! TBH:  This version is for use with the ESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE ESMF
    USE esmf_basemod
    USE esmf_clockmod
































! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in 
! ../../frame/module_domain.F !!!  Eliminate this dependence with 
! grow-as-you-go AlarmList in ESMF_Clock...  


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
!
END MODULE ESMF