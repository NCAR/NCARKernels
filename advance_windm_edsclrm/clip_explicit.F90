
! KGEN-generated Fortran source file
!
! Filename    : clip_explicit.F90
! Generated at: 2015-10-21 08:59:09
! KGEN version: 0.5.3



    MODULE clip_explicit
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE
        PUBLIC clip_covar
! Named constants to avoid string comparisons
        INTEGER, parameter, public :: clip_wprtp = 8
        INTEGER, parameter, public :: clip_wpthlp = 9
        INTEGER, parameter, public :: clip_rtpthlp = 3
! Named constant for rtp2 clipping
! Named constant for thlp2 clipping
! Named constant for rtpthlp clipping
! Named constant for up2 clipping
! Named constant for vp2 clipping
!    clip_scalar = 7, &       ! Named constant for scalar clipping
! Named constant for wprtp clipping
! Named constant for wpthlp clipping
! Named constant for upwp clipping
! Named constant for vpwp clipping
! Named constant for wp2 clipping
! Named constant for wp scalar clipping
! Named constant for sclrp2 clipping
! Named constant for sclrprtp clipping
! Named constant for sclrpthlp clipping
! Named constant for wphydrometp clipping
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
!=============================================================================

!=============================================================================

        SUBROUTINE clip_covar(solve_type, l_first_clip_ts, l_last_clip_ts, dt, xp2, yp2, xpyp, xpyp_chnge)
! Description:
! Clipping the value of covariance x'y' based on the correlation between x
! and y.
!
! The correlation between variables x and y is:
!
! corr_(x,y) = x'y' / [ sqrt(x'^2) * sqrt(y'^2) ];
!
! where x'^2 is the variance of x, y'^2 is the variance of y, and x'y' is
! the covariance of x and y.
!
! The correlation of two variables must always have a value between -1
! and 1, such that:
!
! -1 <= corr_(x,y) <= 1.
!
! Therefore, there is an upper limit on x'y', such that:
!
! x'y' <=  [ sqrt(x'^2) * sqrt(y'^2) ];
!
! and a lower limit on x'y', such that:
!
! x'y' >= -[ sqrt(x'^2) * sqrt(y'^2) ].
!
! The values of x'y', x'^2, and y'^2 are all found on momentum levels.
!
! The value of x'y' may need to be clipped whenever x'y', x'^2, or y'^2 is
! updated.
!
! The following covariances are found in the code:
!
! w'r_t', w'th_l', w'sclr', (computed in advance_xm_wpxp);
! r_t'th_l', sclr'r_t', sclr'th_l', (computed in advance_xp2_xpyp);
! u'w', v'w', w'edsclr' (computed in advance_windm_edsclrm);
! and w'hm' (computed in setup_pdf_parameters).
! References:
! None
!-----------------------------------------------------------------------
            USE grid_class, ONLY: gr
! Variable(s)
            USE constants_clubb, ONLY: max_mag_correlation
! Constant(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE stats_type_utilities, ONLY: stat_begin_update
            USE stats_type_utilities, ONLY: stat_modify
            USE stats_type_utilities, ONLY: stat_end_update
! Procedure(s)
            USE stats_variables, ONLY: iwprtp_cl
            USE stats_variables, ONLY: iwpthlp_cl
            USE stats_variables, ONLY: irtpthlp_cl
            USE stats_variables, ONLY: l_stats_samp
            USE stats_variables, ONLY: stats_zm
! Variable(s)
            IMPLICIT NONE
! Input Variables
            INTEGER, intent(in) :: solve_type
! Variable being solved; used for STATS.
            LOGICAL, intent(in) :: l_first_clip_ts
            LOGICAL, intent(in) :: l_last_clip_ts
! First instance of clipping in a timestep.
! Last instance of clipping in a timestep.
            REAL(KIND=core_rknd), intent(in) :: dt
! Model timestep; used here for STATS           [s]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: xp2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: yp2
! Variance of x, x'^2 (momentum levels)         [{x units}^2]
! Variance of y, y'^2 (momentum levels)         [{y units}^2]
! Output Variable
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: xpyp
! Covariance of x and y, x'y' (momentum levels) [{x units}*{y units}]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(out) :: xpyp_chnge
! Net change in x'y' due to clipping [{x units}*{y units}]
! Local Variable
            INTEGER :: k ! Array index
            INTEGER :: ixpyp_cl
! ---- Begin Code ----
    select case ( solve_type )
                CASE ( clip_wprtp ) ! wprtp clipping budget term
      ixpyp_cl = iwprtp_cl
                CASE ( clip_wpthlp ) ! wpthlp clipping budget term
      ixpyp_cl = iwpthlp_cl
                CASE ( clip_rtpthlp ) ! rtpthlp clipping budget term
      ixpyp_cl = irtpthlp_cl
                CASE DEFAULT ! scalars (or upwp/vpwp) are involved
      ixpyp_cl = 0
    end select
    if ( l_stats_samp ) then
      if ( l_first_clip_ts ) then
        call stat_begin_update( ixpyp_cl, xpyp / dt, stats_zm )
      else
        call stat_modify( ixpyp_cl, -xpyp / dt, stats_zm )
      endif
    endif
! The value of x'y' at the surface (or lower boundary) is a set value that
! is either specified or determined elsewhere in a surface subroutine.  It
! is ensured elsewhere that the correlation between x and y at the surface
! (or lower boundary) is between -1 and 1.  Thus, the covariance clipping
! code does not need to be invoked at the lower boundary.  Likewise, the
! value of x'y' is set at the upper boundary, so the covariance clipping
! code does not need to be invoked at the upper boundary.
! Note that if clipping were applied at the lower boundary, momentum will
! not be conserved, therefore it should never be added.
    do k = 2, gr%nz-1, 1
! Clipping for xpyp at an upper limit corresponding with a correlation
! between x and y of max_mag_correlation.
      if ( xpyp(k) >  max_mag_correlation * sqrt( xp2(k) * yp2(k) ) ) then
        xpyp_chnge(k) =  max_mag_correlation * sqrt( xp2(k) * yp2(k) ) - xpyp(k)
        xpyp(k) =  max_mag_correlation * sqrt( xp2(k) * yp2(k) )
! Clipping for xpyp at a lower limit corresponding with a correlation
! between x and y of -max_mag_correlation.
      elseif ( xpyp(k) < -max_mag_correlation * sqrt( xp2(k) * yp2(k) ) ) then
        xpyp_chnge(k) = -max_mag_correlation * sqrt( xp2(k) * yp2(k) ) - xpyp(k)
        xpyp(k) = -max_mag_correlation * sqrt( xp2(k) * yp2(k) )
      else
        xpyp_chnge(k) = 0.0_core_rknd
      endif
    enddo ! k = 2..gr%nz ! k = 2..gr%nz
! Since there is no covariance clipping at the upper or lower boundaries,
! the change in x'y' due to covariance clipping at those levels is 0.
    xpyp_chnge(1)       = 0.0_core_rknd
    xpyp_chnge(gr%nz) = 0.0_core_rknd
    if ( l_stats_samp ) then
      if ( l_last_clip_ts ) then
        call stat_end_update( ixpyp_cl, xpyp / dt, stats_zm )
      else
        call stat_modify( ixpyp_cl, xpyp / dt, stats_zm )
      endif
    endif
    return
        END SUBROUTINE clip_covar
!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!===============================================================================
    END MODULE clip_explicit
