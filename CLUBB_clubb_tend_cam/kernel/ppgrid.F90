!KGEN-generated Fortran source file 
  
!Generated at : 2016-06-15 08:49:59 
!KGEN version : 0.7.0 
  

module ppgrid

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Initialize physics grid resolution parameters
!  for a chunked data structure
! 
! Author: 
! 
!-----------------------------------------------------------------------

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    IMPLICIT NONE 
    PRIVATE 
    SAVE 
 
    PUBLIC pcols 
    PUBLIC pver 
    PUBLIC pverp 


! Grid point resolution parameters

   integer pcols      ! number of columns (max)
   integer pver       ! number of vertical levels
   integer pverp      ! pver + 1

   PARAMETER (pcols         = 16) 
   PARAMETER (pver           = 30) 
   PARAMETER (pverp         = pver + 1) 
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!

end module ppgrid