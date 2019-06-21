!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:40 
!KGEN version : 0.8.1 
  
! TBH:  This version is for use with the ESMF library embedded in the WRF
! TBH:  distribution.


MODULE ESMF
    USE esmf_basemod 
    USE esmf_clockmod 
! Note that MAX_ALARMS must match MAX_WRF_ALARMS defined in
! ../../frame/module_domain.F !!!  Eliminate this dependence with
! grow-as-you-go AlarmList in ESMF_Clock...
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


!
END MODULE ESMF