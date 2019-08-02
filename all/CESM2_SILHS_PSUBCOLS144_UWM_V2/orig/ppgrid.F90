!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:56 
!KGEN version : 0.8.1 
  


module ppgrid
!----------------------------------------------------------------------- 
! Purpose: 
! Initialize physics grid resolution parameters
!  for a chunked data structure
! Author: 
!-----------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

! 
! 
! 

    IMPLICIT NONE 
    PRIVATE 
    SAVE 
 
    PUBLIC pver 
    PUBLIC pverp 
! Grid point resolution parameters


   integer pver       ! number of vertical levels
   integer pverp      ! pver + 1

   PARAMETER (pver           = 32) 
   PARAMETER (pverp         = pver + 1) 
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!
!

end module ppgrid