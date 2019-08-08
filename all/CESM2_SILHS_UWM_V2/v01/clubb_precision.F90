!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:29 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id$
!===============================================================================


module clubb_precision
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PUBLIC stat_nknd, stat_rknd, time_precision, dp, core_rknd 

    PRIVATE 
  ! This definition of double precision must use a real type that is 64 bits
  ! wide, because (at least) the LAPACK routines depend on this definition being
  ! accurate. Otherwise, LAPACK must be recompiled, or some other trickery must
  ! be done.

  integer, parameter :: &
    dp = selected_real_kind( p=12 )    ! double precision
  ! The precisions below are arbitrary, and could be adjusted as
  ! needed for long simulations or time averaging.  Note that on
  ! most machines 12 digits of precision will use a data type
  ! which is 8 bytes long.

  integer, parameter ::  & 
    stat_nknd = selected_int_kind( 8 ), & 
    stat_rknd = selected_real_kind( p=12 ), & 
    time_precision = selected_real_kind( p=12 ), &
    core_rknd = dp ! Value from the preprocessor directive

end module clubb_precision
!-------------------------------------------------------------------------------