!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:48
!KGEN version : 0.6.1


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
 
    PUBLIC pver
    PUBLIC pverp


! Grid point resolution parameters

   integer pver       ! number of vertical levels
   integer pverp      ! pver + 1

   PARAMETER (pver      = 30)
   PARAMETER (pverp     = pver + 1)
!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!

end module ppgrid