!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:41
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
 
    PUBLIC pver
    PUBLIC veclen


! Grid point resolution parameters

   integer pver       ! number of vertical levels

   PARAMETER (pver      = 66)

  ! Changed VECLEN to be set in the compiler flags, since it differs from architecture to architecture.
  ! Default, when unspecified, is 16 (Intel BDW best)
#ifndef _VECLEN
   integer,parameter :: veclen=16
#else
   integer, parameter :: veclen=_VECLEN
#endif
   integer, parameter,public :: blklen=16
   integer,parameter,public  :: nSystems = 858 ! total number of linear systems

!
! start, end indices for chunks owned by a given MPI task
! (set in phys_grid_init).
!

end module ppgrid
