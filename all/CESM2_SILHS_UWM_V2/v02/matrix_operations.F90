!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:31 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id$
!===============================================================================


module matrix_operations
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 


    PUBLIC row_mult_lower_tri_matrix 


    PRIVATE 

  contains
!-----------------------------------------------------------------------
 

!-----------------------------------------------------------------------
  subroutine row_mult_lower_tri_matrix( ndim, xvector, tmatrix_in, tmatrix_out )
! Description:
!   Do a row-wise multiply of the elements of a lower triangular matrix.
! References:
!   None
!-----------------------------------------------------------------------
     !$acc routine vector


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables


    integer, intent(in) :: ndim

    real( kind = core_rknd ), dimension(ndim), intent(in) :: & 
      xvector ! Factors to be multiplied across a row [units vary]
    ! Input Variables

    real( kind = core_rknd ), dimension(ndim,ndim), intent(in) :: &
      tmatrix_in ! nxn matrix (usually a correlation matrix) [units vary]
    ! Output Variables

    real( kind = core_rknd ), dimension(ndim,ndim), intent(inout) :: &
      tmatrix_out ! nxn matrix (usually a covariance matrix) [units vary]
    ! Local Variables

    integer :: i, j
    ! ---- Begin Code ----


    do i = 1, ndim
      do j = 1, i
        tmatrix_out(i,j) = tmatrix_in(i,j) * xvector(i)
      end do
    end do

    return
  end subroutine row_mult_lower_tri_matrix
!-------------------------------------------------------------------------------


!----------------------------------------------------------------------


!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------


!-----------------------------------------------------------------------


  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------

end module matrix_operations
