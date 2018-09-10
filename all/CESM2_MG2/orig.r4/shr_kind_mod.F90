!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-07 15:55:26 
!KGEN version : 0.7.3 
  


MODULE shr_kind_mod
  !----------------------------------------------------------------------------
  ! precision/kind constants add data public
  !----------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    PUBLIC 
#if defined(USE_R4)
   integer,parameter :: rkind_comp = selected_real_kind( 6, 37) ! 4 byte real
   integer,parameter :: rkind_io   = selected_real_kind(15,307) ! 8 byte real
#else
   integer,parameter :: rkind_comp = selected_real_kind(15,307) ! 8 byte real
   integer,parameter :: rkind_io   = selected_real_kind(15,307) ! 8 byte real
#endif

!  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
  integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer

END MODULE shr_kind_mod
