
! KGEN-generated Fortran source file
!
! Filename    : stats_type_utilities.F90
! Generated at: 2015-10-20 14:26:59
! KGEN version: 0.5.3



    MODULE stats_type_utilities
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   Contains subroutines for interfacing with type, stats
!-----------------------------------------------------------------------
        USE stats_type, ONLY: stats
! type
        USE clubb_precision, ONLY: core_rknd
        IMPLICIT NONE
        PRIVATE ! Set Default Scope
        PUBLIC stat_update_var, stat_begin_update, stat_begin_update_pt, stat_update_var_pt, stat_end_update_pt, stat_end_update, &
        stat_modify, stat_modify_pt
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!=============================================================================

!=============================================================================

        SUBROUTINE stat_update_var(var_index, value, grid_kind)
! Description:
! This updates the value of a statistics variable located at var_index
! associated with grid type 'grid_kind' (zt, zm, or sfc).
!
! This subroutine is used when a statistical variable needs to be updated
! only once during a model timestep.
!
! In regards to budget terms, this subroutine is used for variables that
! are either completely implicit (e.g. wprtp_ma) or completely explicit
! (e.g. wp2_pr3).  For completely implicit terms, once the variable has been
! solved for, the implicit contribution can be finalized.  The finalized
! implicit contribution is sent into stat_update_var_pt.  For completely
! explicit terms, the explicit contribution is sent into stat_update_var_pt
! once it has been calculated.
!---------------------------------------------------------------------
            USE clubb_precision, ONLY: stat_rknd
! Constant
            USE stat_file_module, ONLY: clubb_i
            USE stat_file_module, ONLY: clubb_j
! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
! The index at which the variable is stored  []
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc )
! Input Variable(s) NOTE: Due to the implicit none above, these must
! be declared below to allow the use of grid_kind
            REAL(KIND=core_rknd), dimension(grid_kind%kk), intent(in) :: value
! Value of field being added to the statistic    [Units Vary]
            INTEGER :: k
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
        END SUBROUTINE stat_update_var
!=============================================================================

        SUBROUTINE stat_update_var_pt(var_index, grid_level, value, grid_kind)
! Description:
! This updates the value of a statistics variable located at var_index
! associated with grid type 'grid_kind' at a specific grid_level.
!
! See the description of stat_update_var for more details.
!---------------------------------------------------------------------
            USE clubb_precision, ONLY: stat_rknd
! Constant
            USE stat_file_module, ONLY: clubb_i
            USE stat_file_module, ONLY: clubb_j
! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
            INTEGER, intent(in) :: grid_level
! The index at which the variable is stored           []
! The level at which the variable is to be modified   []
            REAL(KIND=core_rknd), intent(in) :: value
! Value of field being added to the statistic         [Units Vary]
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
    if ( var_index > 0 ) then
      grid_kind%accum_field_values(clubb_i,clubb_j,grid_level,var_index) = &
        grid_kind%accum_field_values(clubb_i,clubb_j,grid_level,var_index) + &
          real( value, kind=stat_rknd )
      grid_kind%accum_num_samples(clubb_i,clubb_j,grid_level,var_index) = &
        grid_kind%accum_num_samples(clubb_i,clubb_j,grid_level,var_index) + 1
    endif
    return
        END SUBROUTINE stat_update_var_pt
!=============================================================================

        SUBROUTINE stat_begin_update(var_index, value, grid_kind)
! Description:
! This begins an update of the value of a statistics variable located at
! var_index on the (zt, zm, or sfc) grid.  It is used in conjunction with
! subroutine stat_end_update.
!
! This subroutine is used when a statistical variable needs to be updated
! more than one time during a model timestep.  Commonly, this is used for
! beginning a budget term calculation.
!
! In this type of stats calculation, we first subtract the field
! (e.g. rtm / dt ) from the statistic, then update rtm by a term
! (e.g. clip rtm), and then re-add the field (e.g. rtm / dt) to the
! statistic.
!
! Example:
!
!  call stat_begin_update( irtm_bt, real(rtm / dt), stats_zt )
!
!  !!! Perform clipping of rtm !!!
!
!  call stat_end_update( irtm_bt, real(rtm / dt), stats_zt )
!
! This subroutine is often used with stats budget terms for variables that
! have both implicit and explicit components (e.g. wp3_ta).  The explicit
! component is sent into stat_begin_update_pt (with the sign reversed
! because stat_begin_update_pt automatically subtracts the value sent into
! it).  Then, once the variable has been solved for, the implicit
! statistical contribution can be finalized.  The finalized implicit
! component is sent into stat_end_update_pt.
!---------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
! The index at which the variable is stored           []
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: value
! Value of field being added to the statistic         [Units Vary]
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
            INTEGER :: i
    do i = 1, gr%nz
      call stat_begin_update_pt &
            ( var_index, i, value(i), grid_kind )
    enddo
    return
        END SUBROUTINE stat_begin_update
!=============================================================================

        SUBROUTINE stat_begin_update_pt(var_index, grid_level, value, grid_kind)
! Description:
!   This begins an update of the value of a statistics variable located at
!   var_index associated with the grid type (grid_kind) at a specific
!   grid_level.  It is used in conjunction with subroutine stat_end_update_pt.
!
! Notes:
!   Commonly this is used for beginning a budget.  See the description of
!   stat_begin_update for more details.
!
! References:
!   None
!---------------------------------------------------------------------
            USE error_code, ONLY: clubb_debug ! Procedure(s)
            USE clubb_precision, ONLY: stat_rknd
! Constant
            USE stat_file_module, ONLY: clubb_i
            USE stat_file_module, ONLY: clubb_j
! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
            INTEGER, intent(in) :: grid_level
! The index at which the variable is stored           []
! The level at which the variable is to be modified   []
            REAL(KIND=core_rknd), intent(in) :: value
! Value of field being added to the statistic                [Units Vary]
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
! ---- Begin Code ----
    if ( var_index > 0 ) then  ! Are we storing this variable? ! Are we storing this variable?
! Can we begin an update?
      if ( .not. grid_kind%l_in_update(clubb_i,clubb_j,grid_level,var_index) ) then
        grid_kind%accum_field_values(clubb_i,clubb_j,grid_level, var_index) =  &
                grid_kind%accum_field_values(clubb_i,clubb_j,grid_level, var_index) - &
                  real( value, kind=stat_rknd )
        grid_kind%l_in_update(clubb_i,clubb_j,grid_level, var_index) = .true.  ! Start Record ! Start Record
      else
        call clubb_debug( 1, &
          "Beginning an update before finishing previous for variable: "// &
          trim( grid_kind%file%var(var_index)%name ) )
      endif
    endif
    return
        END SUBROUTINE stat_begin_update_pt
!=============================================================================

        SUBROUTINE stat_end_update(var_index, value, grid_kind)
! Description:
! This ends an update of the value of a statistics variable located at
! var_index on the (zt, zm, or sfc) grid.  It is used in conjunction with
! subroutine stat_begin_update.
!
! This subroutine is used when a statistical variable needs to be updated
! more than one time during a model timestep.  Commonly, this is used for
! finishing a budget term calculation.
!
! In this type of stats calculation, we first subtract the field
! (e.g. rtm / dt ) from the statistic, then update rtm by a term
! (e.g. clip rtm), and then re-add the field (e.g. rtm / dt) to the
! statistic.
!
! Example:
!
!  call stat_begin_update( irtm_bt, real(rtm / dt), stats_zt )
!
!  !!! Perform clipping of rtm !!!
!
!  call stat_end_update( irtm_bt, real(rtm / dt), stats_zt )
!
! This subroutine is often used with stats budget terms for variables that
! have both implicit and explicit components (e.g. wp3_ta).  The explicit
! component is sent into stat_begin_update_pt (with the sign reversed
! because stat_begin_update_pt automatically subtracts the value sent into
! it).  Then, once the variable has been solved for, the implicit
! statistical contribution can be finalized.  The finalized implicit
! component is sent into stat_end_update_pt.
!---------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
! The index at which the variable is stored           []
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: value
! Value of field being added to the statistic             [Units Vary]
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
            INTEGER :: k
! ---- Begin Code ----
    do k = 1,gr%nz
      call stat_end_update_pt &
               ( var_index, k, value(k), grid_kind )
    enddo
    return
        END SUBROUTINE stat_end_update
!=============================================================================

        SUBROUTINE stat_end_update_pt(var_index, grid_level, value, grid_kind)
! Description:
! This ends an update of the value of a statistics variable located at
! var_index associated with the grid type (grid_kind) at a specific
! grid_level.  It is used in conjunction with subroutine
! stat_begin_update_pt.
!
! Commonly this is used for finishing a budget.  See the description of
! stat_end_update for more details.
!---------------------------------------------------------------------
            USE error_code, ONLY: clubb_debug ! Procedure(s)
            USE stat_file_module, ONLY: clubb_i
            USE stat_file_module, ONLY: clubb_j
! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
            INTEGER, intent(in) :: grid_level
! The index at which the variable is stored           []
! The level at which the variable is to be modified   []
            REAL(KIND=core_rknd), intent(in) :: value
! Value of field being added to the statistic         [Units Vary]
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
! ---- Begin Code ----
    if ( var_index > 0 ) then ! Are we storing this variable? ! Are we storing this variable?
! Can we end an update?
      if ( grid_kind%l_in_update(clubb_i,clubb_j,grid_level,var_index) ) then
        call stat_update_var_pt &
                 ( var_index, grid_level, value, grid_kind )
        grid_kind%l_in_update(clubb_i,clubb_j,grid_level,var_index) = .false. ! End Record ! End Record
      else
        call clubb_debug( 1, "Ending before beginning update. For variable "// &
        grid_kind%file%var(var_index)%name )
      endif
    endif
    return
        END SUBROUTINE stat_end_update_pt
!=============================================================================

        SUBROUTINE stat_modify(var_index, value, grid_kind)
! Description:
! This modifies the value of a statistics variable located at var_index on
! the (zt, zm, or sfc) grid.  It does not increment the sampling count.
!
! This subroutine is normally used when a statistical variable needs to be
! updated more than twice during a model timestep.  Commonly, this is used
! if a budget term calculation needs an intermediate modification between
! stat_begin_update and stat_end_update.
!---------------------------------------------------------------------
            USE grid_class, ONLY: gr ! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
! The index at which the variable is stored           []
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: value
! Value of field being added to the statistic         [Units Vary]
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
            INTEGER :: k
! ---- Begin Code ----
    do k = 1, gr%nz
      call stat_modify_pt( var_index, k, value(k), grid_kind )
    enddo
    return
        END SUBROUTINE stat_modify
!=============================================================================

        SUBROUTINE stat_modify_pt(var_index, grid_level, value, grid_kind)
! Description:
! This modifies the value of a statistics variable located at var_index on
! the grid at a specific point. It does not increment the sampling count.
!
! Commonly this is used for intermediate updates to a budget.  See the
! description of stat_modify for more details.
!---------------------------------------------------------------------
            USE clubb_precision, ONLY: stat_rknd
! Constant
            USE stat_file_module, ONLY: clubb_j
            USE stat_file_module, ONLY: clubb_i
! Variable(s)
            IMPLICIT NONE
! Input Variables(s)
            INTEGER, intent(in) :: var_index
! The index at which the variable is stored            []
            REAL(KIND=core_rknd), intent(in) :: value
! Value of field being added to the statistic         [Units Vary]
            INTEGER, intent(in) :: grid_level
! The level at which the variable is to be modified   []
! Input/Output Variable(s)
            TYPE(stats), intent(inout) :: grid_kind
! Which grid the variable is located on (zt, zm, rad, or sfc).
! ---- Begin Code ----
    if ( var_index > 0 ) then
      grid_kind%accum_field_values(clubb_i,clubb_j,grid_level,var_index )  &
         = grid_kind%accum_field_values(clubb_i,clubb_j,grid_level,var_index ) + &
          real( value, kind=stat_rknd )
    end if
    return
        END SUBROUTINE stat_modify_pt
!===============================================================================
    END MODULE stats_type_utilities
