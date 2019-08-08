!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:30 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id$
!===============================================================================


module stats_type_utilities
  ! Description:
  !   Contains subroutines for interfacing with type, stats
  !-----------------------------------------------------------------------


    USE stats_type, ONLY: stats 

    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE stats_type, ONLY: kr_stats_type_stats 
    USE stats_type, ONLY: kv_stats_type_stats 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC stat_update_var, stat_update_var_pt 
    PUBLIC kr_stats_type_stats 
    PUBLIC kv_stats_type_stats 
  contains
  !=============================================================================


  !=============================================================================

  subroutine stat_update_var( var_index, value, grid_kind )
    ! Description:
    ! This updates the value of a statistics variable located at var_index
    ! associated with grid type 'grid_kind' (zt, zm, or sfc).
    ! This subroutine is used when a statistical variable needs to be updated
    ! only once during a model timestep.
    ! In regards to budget terms, this subroutine is used for variables that
    ! are either completely implicit (e.g. wprtp_ma) or completely explicit
    ! (e.g. wp2_pr3).  For completely implicit terms, once the variable has been
    ! solved for, the implicit contribution can be finalized.  The finalized
    ! implicit contribution is sent into stat_update_var_pt.  For completely
    ! explicit terms, the explicit contribution is sent into stat_update_var_pt
    ! once it has been calculated.
    !---------------------------------------------------------------------

    !
    !

      USE clubb_precision, ONLY: stat_rknd 

      USE stat_file_module, ONLY: clubb_i, clubb_j 

    implicit none
    ! Input Variables(s)


    integer, intent(in) ::  &
      var_index ! The index at which the variable is stored  []
    ! Input/Output Variable(s)

    type(stats), intent(inout) ::  &
      grid_kind ! Which grid the variable is located on (zt, zm, rad, or sfc )
    ! Input Variable(s) NOTE: Due to the implicit none above, these must
    ! be declared below to allow the use of grid_kind


    real( kind = core_rknd ), dimension(grid_kind%kk), intent(in) :: &
      value ! Value of field being added to the statistic    [Units Vary]

    integer :: k

    if ( var_index > 0 ) then
      do k = 1, grid_kind%kk
        grid_kind%accum_field_values(clubb_i,clubb_j,k,var_index) =  &
             grid_kind%accum_field_values(clubb_i,clubb_j,k,var_index) + real( value(k), &
                kind=stat_rknd )
        grid_kind%accum_num_samples(clubb_i,clubb_j,k,var_index) =  &
             grid_kind%accum_num_samples(clubb_i,clubb_j,k,var_index) + 1
      end do
    endif

    return
  end subroutine stat_update_var
  !=============================================================================

  subroutine stat_update_var_pt( var_index, grid_level, value, grid_kind )
    ! Description:
    ! This updates the value of a statistics variable located at var_index
    ! associated with grid type 'grid_kind' at a specific grid_level.
    ! See the description of stat_update_var for more details.
    !---------------------------------------------------------------------

    !

      USE clubb_precision, ONLY: stat_rknd 

      USE stat_file_module, ONLY: clubb_i, clubb_j 

    implicit none
    ! Input Variables(s)


    integer, intent(in) ::  &
      var_index,    & ! The index at which the variable is stored           []
      grid_level      ! The level at which the variable is to be modified   []

    real( kind = core_rknd ), intent(in) :: &
      value ! Value of field being added to the statistic         [Units Vary]
    ! Input/Output Variable(s)

    type(stats), intent(inout) ::  &
      grid_kind ! Which grid the variable is located on (zt, zm, rad, or sfc).

    if ( var_index > 0 ) then

      grid_kind%accum_field_values(clubb_i,clubb_j,grid_level,var_index) = &
        grid_kind%accum_field_values(clubb_i,clubb_j,grid_level,var_index) + &
          real( value, kind=stat_rknd )

      grid_kind%accum_num_samples(clubb_i,clubb_j,grid_level,var_index) = &
        grid_kind%accum_num_samples(clubb_i,clubb_j,grid_level,var_index) + 1

    endif

    return
  end subroutine stat_update_var_pt
  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


  !=============================================================================


!===============================================================================


end module stats_type_utilities