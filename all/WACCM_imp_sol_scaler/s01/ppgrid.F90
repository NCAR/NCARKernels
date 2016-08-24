!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:53
!KGEN version : 0.6.2


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


! Grid point resolution parameters

   integer pcols      ! number of columns (max)
   integer pver       ! number of vertical levels

   PARAMETER (pcols     = 16)
   PARAMETER (pver      = 66)
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!

end module ppgrid