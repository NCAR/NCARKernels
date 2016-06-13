
! KGEN-generated Fortran source file
!
! Filename    : clip_explicit.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE clip_explicit
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
        IMPLICIT NONE
        PRIVATE
        PUBLIC clip_covar, clip_variance, clip_covars_denom, clip_skewness, clip_skewness_core
! Named constants to avoid string comparisons
        INTEGER, parameter, public :: clip_wprtp = 8
        INTEGER, parameter, public :: clip_wpthlp = 9
        INTEGER, parameter, public :: clip_wpsclrp = 13
        INTEGER, parameter, public :: clip_rtpthlp = 3
        INTEGER, parameter, public :: clip_wp2 = 12
        INTEGER, parameter, public :: clip_rtp2 = 1
        INTEGER, parameter, public :: clip_thlp2 = 2
        INTEGER, parameter, public :: clip_up2 = 5
        INTEGER, parameter, public :: clip_vp2 = 6
        INTEGER, parameter, public :: clip_sclrp2 = 14
        INTEGER, parameter, public :: clip_sclrprtp = 15
        INTEGER, parameter, public :: clip_sclrpthlp = 16
        INTEGER, parameter, public :: clip_upwp = 10
        INTEGER, parameter, public :: clip_vpwp = 11
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

        SUBROUTINE clip_covars_denom(dt, rtp2, thlp2, up2, vp2, wp2, sclrp2, wprtp_cl_num, wpthlp_cl_num, wpsclrp_cl_num, &
        upwp_cl_num, vpwp_cl_num, wprtp, wpthlp, upwp, vpwp, wpsclrp)
! Description:
! Some of the covariances found in the CLUBB model code need to be clipped
! multiple times during each timestep to ensure that the correlation between
! the two relevant variables stays between -1 and 1 at all times during the
! model run.  The covariances that need to be clipped multiple times are
! w'r_t', w'th_l', w'sclr', u'w', and v'w'.  One of the times that each one
! of these covariances is clipped is immediately after each one is set.
! However, each covariance still needs to be clipped two more times during
! each timestep (once after advance_xp2_xpyp is called and once after
! advance_wp2_wp3 is called).  This subroutine handles the times that the
! covariances are clipped away from the time that they are set.  In other
! words, this subroutine clips the covariances after the denominator terms
! in the relevant correlation equation have been altered, ensuring that
! all correlations will remain between -1 and 1 at all times.
! References:
! None
!-----------------------------------------------------------------------
            USE grid_class, ONLY: gr
! Variable(s)
            USE parameters_model, ONLY: sclr_dim
! Variable(s)
            USE model_flags, ONLY: l_tke_aniso
! Logical
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE stats_type_utilities, ONLY: stat_modify
! Procedure(s)
            USE stats_variables, ONLY: l_stats_samp
            USE stats_variables, ONLY: iwprtp_bt
            USE stats_variables, ONLY: stats_zm
            USE stats_variables, ONLY: iwpthlp_bt
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: dt
! Timestep [s]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: wp2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: rtp2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: thlp2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: up2
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: vp2
! r_t'^2         [(kg/kg)^2]
! theta_l'^2     [K^2]
! u'^2           [m^2/s^2]
! v'^2           [m^2/s^2]
! w'^2           [m^2/s^2]
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(in) :: sclrp2
! sclr'^2  [{units vary}^2]
            INTEGER, intent(in) :: upwp_cl_num
            INTEGER, intent(in) :: vpwp_cl_num
            INTEGER, intent(in) :: wprtp_cl_num
            INTEGER, intent(in) :: wpthlp_cl_num
            INTEGER, intent(in) :: wpsclrp_cl_num
! Input/Output Variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wprtp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wpthlp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: upwp
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: vpwp
! w'r_t'        [(kg/kg) m/s]
! w'theta_l'    [K m/s]
! u'w'          [m^2/s^2]
! v'w'          [m^2/s^2]
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim), intent(inout) :: wpsclrp
! w'sclr'         [units m/s]
! Local Variables
            LOGICAL :: l_first_clip_ts
            LOGICAL :: l_last_clip_ts
! First instance of clipping in a timestep.
! Last instance of clipping in a timestep.
            REAL(KIND=core_rknd), dimension(gr%nz) :: wprtp_chnge
            REAL(KIND=core_rknd), dimension(gr%nz) :: wpthlp_chnge
            REAL(KIND=core_rknd), dimension(gr%nz) :: upwp_chnge
            REAL(KIND=core_rknd), dimension(gr%nz) :: vpwp_chnge
! Net change in w'r_t' due to clipping  [(kg/kg) m/s]
! Net change in w'th_l' due to clipping [K m/s]
! Net change in u'w' due to clipping    [m^2/s^2]
! Net change in v'w' due to clipping    [m^2/s^2]
            REAL(KIND=core_rknd), dimension(gr%nz,sclr_dim) :: wpsclrp_chnge
! Net change in w'sclr' due to clipping [{units vary}]
            INTEGER :: i ! scalar array index.
! ---- Begin Code ----
!!! Clipping for w'r_t'
!
! Clipping w'r_t' at each vertical level, based on the
! correlation of w and r_t at each vertical level, such that:
! corr_(w,r_t) = w'r_t' / [ sqrt(w'^2) * sqrt(r_t'^2) ];
! -1 <= corr_(w,r_t) <= 1.
!
! Since w'^2, r_t'^2, and w'r_t' are each advanced in different
! subroutines from each other in advance_clubb_core, clipping for w'r_t'
! is done three times during each timestep (once after each variable has
! been updated).
!
! This subroutine handles the first and third instances of
! w'r_t' clipping.
! The first instance of w'r_t' clipping takes place after
! r_t'^2 is updated in advance_xp2_xpyp.
! The third instance of w'r_t' clipping takes place after
! w'^2 is updated in advance_wp2_wp3.
! Include effect of clipping in wprtp time tendency budget term.
    if ( l_stats_samp ) then
! if wprtp_cl_num == 1 do nothing since
! iwprtp_bt stat_begin_update is called outside of this method
      if ( wprtp_cl_num == 2 ) then
! wprtp total time tendency (effect of clipping)
        call stat_modify( iwprtp_bt,  -wprtp / dt,  & ! intent(in)
                          stats_zm )                               ! intent(inout) ! intent(in)
! intent(inout)
      elseif ( wprtp_cl_num == 3 ) then
! wprtp total time tendency (effect of clipping)
        call stat_modify( iwprtp_bt, -wprtp / dt,  & ! intent(in)
                          stats_zm )                               ! intent(inout) ! intent(in)
! intent(inout)
      endif
    endif
! Used within subroutine clip_covar.
    if ( wprtp_cl_num == 1 ) then
      l_first_clip_ts = .true.
      l_last_clip_ts  = .false.
    elseif ( wprtp_cl_num == 2 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .false.
    elseif ( wprtp_cl_num == 3 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .true.
    endif
! Clip w'r_t'
    call clip_covar( clip_wprtp, l_first_clip_ts,   & ! intent(in)
                     l_last_clip_ts, dt, wp2, rtp2, & ! intent(in)
                     wprtp, wprtp_chnge )             ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    if ( l_stats_samp ) then
      if ( wprtp_cl_num == 1 ) then
! wprtp total time tendency (effect of clipping)
        call stat_modify( iwprtp_bt,  wprtp / dt,  & ! intent(in)
                          stats_zm )                              ! intent(inout) ! intent(in)
! intent(inout)
      elseif ( wprtp_cl_num == 2 ) then
! wprtp total time tendency (effect of clipping)
        call stat_modify( iwprtp_bt, wprtp / dt,  & ! intent(in)
                          stats_zm )                              ! intent(inout) ! intent(in)
! intent(inout)
! if wprtp_cl_num == 3 do nothing since
! iwprtp_bt stat_end_update is called outside of this method
      endif
    endif
!!! Clipping for w'th_l'
!
! Clipping w'th_l' at each vertical level, based on the
! correlation of w and th_l at each vertical level, such that:
! corr_(w,th_l) = w'th_l' / [ sqrt(w'^2) * sqrt(th_l'^2) ];
! -1 <= corr_(w,th_l) <= 1.
!
! Since w'^2, th_l'^2, and w'th_l' are each advanced in different
! subroutines from each other in advance_clubb_core, clipping for w'th_l'
! is done three times during each timestep (once after each variable has
! been updated).
!
! This subroutine handles the first and third instances of
! w'th_l' clipping.
! The first instance of w'th_l' clipping takes place after
! th_l'^2 is updated in advance_xp2_xpyp.
! The third instance of w'th_l' clipping takes place after
! w'^2 is updated in advance_wp2_wp3.
! Include effect of clipping in wpthlp time tendency budget term.
    if ( l_stats_samp ) then
! if wpthlp_cl_num == 1 do nothing since
! iwpthlp_bt stat_begin_update is called outside of this method
      if ( wpthlp_cl_num == 2 ) then
! wpthlp total time tendency (effect of clipping)
        call stat_modify( iwpthlp_bt, -wpthlp / dt,  & ! intent(in)
                          stats_zm )                                 ! intent(inout) ! intent(in)
! intent(inout)
      elseif ( wpthlp_cl_num == 3 ) then
! wpthlp total time tendency (effect of clipping)
        call stat_modify( iwpthlp_bt, -wpthlp / dt,  & ! intent(in)
                          stats_zm )                                 ! intent(inout) ! intent(in)
! intent(inout)
      endif
    endif
! Used within subroutine clip_covar.
    if ( wpthlp_cl_num == 1 ) then
      l_first_clip_ts = .true.
      l_last_clip_ts  = .false.
    elseif ( wpthlp_cl_num == 2 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .false.
    elseif ( wpthlp_cl_num == 3 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .true.
    endif
! Clip w'th_l'
    call clip_covar( clip_wpthlp, l_first_clip_ts,   & ! intent(in)
                     l_last_clip_ts, dt, wp2, thlp2, & ! intent(in)
                     wpthlp, wpthlp_chnge )            ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    if ( l_stats_samp ) then
      if ( wpthlp_cl_num == 1 ) then
! wpthlp total time tendency (effect of clipping)
        call stat_modify( iwpthlp_bt, wpthlp / dt,  & ! intent(in)
                          stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
      elseif ( wpthlp_cl_num == 2 ) then
! wpthlp total time tendency (effect of clipping)
        call stat_modify( iwpthlp_bt, wpthlp / dt,  & ! intent(in)
                          stats_zm )                                ! intent(inout) ! intent(in)
! intent(inout)
! if wpthlp_cl_num == 3 do nothing since
! iwpthlp_bt stat_end_update is called outside of this method
      endif
    endif
!!! Clipping for w'sclr'
!
! Clipping w'sclr' at each vertical level, based on the
! correlation of w and sclr at each vertical level, such that:
! corr_(w,sclr) = w'sclr' / [ sqrt(w'^2) * sqrt(sclr'^2) ];
! -1 <= corr_(w,sclr) <= 1.
!
! Since w'^2, sclr'^2, and w'sclr' are each advanced in different
! subroutines from each other in advance_clubb_core, clipping for w'sclr'
! is done three times during each timestep (once after each variable has
! been updated).
!
! This subroutine handles the first and third instances of
! w'sclr' clipping.
! The first instance of w'sclr' clipping takes place after
! sclr'^2 is updated in advance_xp2_xpyp.
! The third instance of w'sclr' clipping takes place after
! w'^2 is updated in advance_wp2_wp3.
! Used within subroutine clip_covar.
    if ( wpsclrp_cl_num == 1 ) then
      l_first_clip_ts = .true.
      l_last_clip_ts  = .false.
    elseif ( wpsclrp_cl_num == 2 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .false.
    elseif ( wpsclrp_cl_num == 3 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .true.
    endif
! Clip w'sclr'
    do i = 1, sclr_dim, 1
      call clip_covar( clip_wpsclrp, l_first_clip_ts,           & ! intent(in)
                       l_last_clip_ts, dt, wp2(:), sclrp2(:,i), & ! intent(in)
                       wpsclrp(:,i), wpsclrp_chnge(:,i) )         ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    enddo
!!! Clipping for u'w'
!
! Clipping u'w' at each vertical level, based on the
! correlation of u and w at each vertical level, such that:
! corr_(u,w) = u'w' / [ sqrt(u'^2) * sqrt(w'^2) ];
! -1 <= corr_(u,w) <= 1.
!
! Since w'^2, u'^2, and u'w' are each advanced in different
! subroutines from each other in advance_clubb_core, clipping for u'w'
! is done three times during each timestep (once after each variable has
! been updated).
!
! This subroutine handles the first and second instances of
! u'w' clipping.
! The first instance of u'w' clipping takes place after
! u'^2 is updated in advance_xp2_xpyp.
! The second instance of u'w' clipping takes place after
! w'^2 is updated in advance_wp2_wp3.
! Used within subroutine clip_covar.
    if ( upwp_cl_num == 1 ) then
      l_first_clip_ts = .true.
      l_last_clip_ts  = .false.
    elseif ( upwp_cl_num == 2 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .false.
    elseif ( upwp_cl_num == 3 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .true.
    endif
! Clip u'w'
    if ( l_tke_aniso ) then
      call clip_covar( clip_upwp, l_first_clip_ts,   & ! intent(in)
                       l_last_clip_ts, dt, wp2, up2, & ! intent(in)
                       upwp, upwp_chnge )              ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    else
! In this case, up2 = wp2, and the variable `up2' does not interact
      call clip_covar( clip_upwp, l_first_clip_ts,   & ! intent(in)
                       l_last_clip_ts, dt, wp2, wp2, & ! intent(in)
                       upwp, upwp_chnge )              ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    end if
!!! Clipping for v'w'
!
! Clipping v'w' at each vertical level, based on the
! correlation of v and w at each vertical level, such that:
! corr_(v,w) = v'w' / [ sqrt(v'^2) * sqrt(w'^2) ];
! -1 <= corr_(v,w) <= 1.
!
! Since w'^2, v'^2, and v'w' are each advanced in different
! subroutines from each other in advance_clubb_core, clipping for v'w'
! is done three times during each timestep (once after each variable has
! been updated).
!
! This subroutine handles the first and second instances of
! v'w' clipping.
! The first instance of v'w' clipping takes place after
! v'^2 is updated in advance_xp2_xpyp.
! The second instance of v'w' clipping takes place after
! w'^2 is updated in advance_wp2_wp3.
! Used within subroutine clip_covar.
    if ( vpwp_cl_num == 1 ) then
      l_first_clip_ts = .true.
      l_last_clip_ts  = .false.
    elseif ( vpwp_cl_num == 2 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .false.
    elseif ( vpwp_cl_num == 3 ) then
      l_first_clip_ts = .false.
      l_last_clip_ts  = .true.
    endif
    if ( l_tke_aniso ) then
      call clip_covar( clip_vpwp, l_first_clip_ts,   & ! intent(in)
                       l_last_clip_ts, dt, wp2, vp2, & ! intent(in)
                       vpwp, vpwp_chnge )              ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    else
! In this case, vp2 = wp2, and the variable `vp2' does not interact
      call clip_covar( clip_vpwp, l_first_clip_ts,   & ! intent(in)
                       l_last_clip_ts, dt, wp2, wp2, & ! intent(in)
                       vpwp, vpwp_chnge )              ! intent(inout) ! intent(in)
! intent(in)
! intent(inout)
    end if
    return
        END SUBROUTINE clip_covars_denom
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

        SUBROUTINE clip_variance(solve_type, dt, threshold, xp2)
! Description:
! Clipping the value of variance x'^2 based on a minimum threshold value.
! The threshold value must be greater than or equal to 0.
!
! The values of x'^2 are found on the momentum levels.
!
! The following variances are found in the code:
!
! r_t'^2, th_l'^2, u'^2, v'^2, sclr'^2, (computed in advance_xp2_xpyp);
! w'^2 (computed in advance_wp2_wp3).
! References:
! None
!-----------------------------------------------------------------------
            USE grid_class, ONLY: gr
! Variable(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE stats_type_utilities, ONLY: stat_begin_update
            USE stats_type_utilities, ONLY: stat_end_update
! Procedure(s)
            USE stats_variables, ONLY: iwp2_cl
            USE stats_variables, ONLY: irtp2_cl
            USE stats_variables, ONLY: ithlp2_cl
            USE stats_variables, ONLY: iup2_cl
            USE stats_variables, ONLY: ivp2_cl
            USE stats_variables, ONLY: l_stats_samp
            USE stats_variables, ONLY: stats_zm
! Variable(s)
            IMPLICIT NONE
! Input Variables
            INTEGER, intent(in) :: solve_type
! Variable being solved; used for STATS.
            REAL(KIND=core_rknd), intent(in) :: dt
! Model timestep; used here for STATS     [s]
            REAL(KIND=core_rknd), intent(in) :: threshold
! Minimum value of x'^2                   [{x units}^2]
! Output Variable
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: xp2
! Variance of x, x'^2 (momentum levels)   [{x units}^2]
! Local Variables
            INTEGER :: k ! Array index
            INTEGER :: ixp2_cl
! ---- Begin Code ----
    select case ( solve_type )
                CASE ( clip_wp2 ) ! wp2 clipping budget term
      ixp2_cl = iwp2_cl
                CASE ( clip_rtp2 ) ! rtp2 clipping budget term
      ixp2_cl = irtp2_cl
                CASE ( clip_thlp2 ) ! thlp2 clipping budget term
      ixp2_cl = ithlp2_cl
                CASE ( clip_up2 ) ! up2 clipping budget term
      ixp2_cl = iup2_cl
                CASE ( clip_vp2 ) ! vp2 clipping budget term
      ixp2_cl = ivp2_cl
                CASE DEFAULT ! scalars are involved
      ixp2_cl = 0
    end select
    if ( l_stats_samp ) then
      call stat_begin_update( ixp2_cl, xp2 / dt, stats_zm )
    endif
! Limit the value of x'^2 at threshold.
! The value of x'^2 at the surface (or lower boundary) is a set value that
! is determined elsewhere in a surface subroutine.  Thus, the variance
! clipping code does not need to be invoked at the lower boundary.
! Likewise, the value of x'^2 is set at the upper boundary, so the variance
! clipping code does not need to be invoked at the upper boundary.
!
! charlass on 09/11/2013: I changed the clipping so that also the surface
! level is clipped. I did this because we discovered that there are slightly
! negative values in thlp2(1) and rtp2(1) when running quarter_ss case with
! WRF-CLUBB (see wrf:ticket:51#comment:33)
    do k = 1, gr%nz-1, 1
      if ( xp2(k) < threshold ) then
        xp2(k) = threshold
      endif
    enddo
    if ( l_stats_samp ) then
      call stat_end_update( ixp2_cl, xp2 / dt, stats_zm )
    endif
    return
        END SUBROUTINE clip_variance
!=============================================================================

        SUBROUTINE clip_skewness(dt, sfc_elevation, wp2_zt, wp3)
! Description:
! Clipping the value of w'^3 based on the skewness of w, Sk_w.
!
! Aditionally, to prevent possible crashes due to wp3 growing too large,
! abs(wp3) will be clipped to 100.
!
! The skewness of w is:
!
! Sk_w = w'^3 / (w'^2)^(3/2).
!
! The value of Sk_w is limited to a range between an upper limit and a lower
! limit.  The values of the limits depend on whether the level altitude is
! within 100 meters of the surface.
!
! For altitudes less than or equal to 100 meters above ground level (AGL):
!
! -0.2_core_rknd*sqrt(2) <= Sk_w <= 0.2_core_rknd*sqrt(2);
!
! while for all altitudes greater than 100 meters AGL:
!
! -4.5_core_rknd <= Sk_w <= 4.5_core_rknd.
!
! Therefore, there is an upper limit on w'^3, such that:
!
! w'^3  <=  threshold_magnitude * (w'^2)^(3/2);
!
! and a lower limit on w'^3, such that:
!
! w'^3  >= -threshold_magnitude * (w'^2)^(3/2).
!
! The values of w'^3 are found on the thermodynamic levels, while the values
! of w'^2 are found on the momentum levels.  Therefore, the values of w'^2
! are interpolated to the thermodynamic levels before being used to
! calculate the upper and lower limits for w'^3.
! References:
! None
!-----------------------------------------------------------------------
            USE grid_class, ONLY: gr
! Variable(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE stats_type_utilities, ONLY: stat_begin_update
            USE stats_type_utilities, ONLY: stat_end_update
! Procedure(s)
            USE stats_variables, ONLY: l_stats_samp
            USE stats_variables, ONLY: iwp3_cl
            USE stats_variables, ONLY: stats_zt
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC sign, sqrt, real
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: dt
! Model timestep; used here for STATS        [s]
            REAL(KIND=core_rknd), intent(in) :: sfc_elevation
! Elevation of ground level                  [m AMSL]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: wp2_zt
! w'^2 interpolated to thermodyamic levels   [m^2/s^2]
! Input/Output Variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wp3
! w'^3 (thermodynamic levels)                [m^3/s^3]
! ---- Begin Code ----
    if ( l_stats_samp ) then
      call stat_begin_update( iwp3_cl, wp3 / dt, stats_zt )
    endif
    call clip_skewness_core( sfc_elevation, wp2_zt, wp3 )
    if ( l_stats_samp ) then
      call stat_end_update( iwp3_cl, wp3 / dt, stats_zt )
    endif
    return
        END SUBROUTINE clip_skewness
!=============================================================================

        SUBROUTINE clip_skewness_core(sfc_elevation, wp2_zt, wp3)
!
            USE grid_class, ONLY: gr
! Variable(s)
            USE constants_clubb, ONLY: skw_max_mag_sqd
! [-]
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC sign, sqrt, real
! Input Variables
            REAL(KIND=core_rknd), intent(in) :: sfc_elevation
! Elevation of ground level                  [m AMSL]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: wp2_zt
! w'^2 interpolated to thermodyamic levels   [m^2/s^2]
! Input/Output Variables
            REAL(KIND=core_rknd), dimension(gr%nz), intent(inout) :: wp3
! w'^3 (thermodynamic levels)                [m^3/s^3]
! Local Variables
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp2_zt_cubed
            REAL(KIND=core_rknd), dimension(gr%nz) :: wp3_lim_sqd
! Variance of vertical velocity cubed (w^2_{zt}^3)   [m^6/s^6]
! Keeps absolute value of Sk_w from becoming > limit [m^6/s^6]
            INTEGER :: k ! Vertical array index.
            REAL(KIND=core_rknd), parameter :: wp3_max = 100._core_rknd
! Threshold for wp3 [m^3/s^3]
! ---- Begin Code ----
! Compute the upper and lower limits of w'^3 at every level,
! based on the skewness of w, Sk_w, such that:
! Sk_w = w'^3 / (w'^2)^(3/2);
! -4.5 <= Sk_w <= 4.5;
! or, if the level altitude is within 100 meters of the surface,
! -0.2*sqrt(2) <= Sk_w <= 0.2*sqrt(2).
! The normal magnitude limit of skewness of w in the CLUBB code is 4.5.
! However, according to Andre et al. (1976b & 1978), wp3 should not exceed
! [2*(wp2^3)]^(1/2) at any level.  However, this term should be multiplied
! by 0.2 close to the surface to include surface effects.  There already is
! a wp3 clipping term in place for all other altitudes, but this term will
! be included for the surface layer only.  Therefore, the lowest level wp3
! should not exceed 0.2 * sqrt(2) * wp2^(3/2).  Brian Griffin.  12/18/05.
! To lower compute time, we squared both sides of the equation and compute
! wp2^3 only once. -dschanen 9 Oct 2008
    wp2_zt_cubed(1:gr%nz) = wp2_zt(1:gr%nz)**3
    do k = 1, gr%nz, 1
      if ( gr%zt(k) - sfc_elevation <= 100.0_core_rknd ) then ! Clip for 100 m. AGL. ! Clip for 100 m. AGL.
!wp3_upper_lim(k) =  0.2_core_rknd * sqrt_2 * wp2_zt(k)**(3.0_core_rknd/2.0_core_rknd)
!wp3_lower_lim(k) = -0.2_core_rknd * sqrt_2 * wp2_zt(k)**(3.0_core_rknd/2.0_core_rknd)
        wp3_lim_sqd(k) = 0.08_core_rknd * wp2_zt_cubed(k) ! Where 0.08_core_rknd ! Where 0.08_core_rknd
! == (sqrt(2)*0.2_core_rknd)**2 known magic number
      else                          ! Clip skewness consistently with a. ! Clip skewness consistently with a.
!wp3_upper_lim(k) =  4.5_core_rknd * wp2_zt(k)**(3.0_core_rknd/2.0_core_rknd)
!wp3_lower_lim(k) = -4.5_core_rknd * wp2_zt(k)**(3.0_core_rknd/2.0_core_rknd)
        wp3_lim_sqd(k) = Skw_max_mag_sqd * wp2_zt_cubed(k) ! Skw_max_mag = 4.5_core_rknd^2 ! Skw_max_mag = 4.5_core_rknd^2
      endif
    enddo
! Clipping for w'^3 at an upper and lower limit corresponding with
! the appropriate value of Sk_w.
    where ( wp3**2 > wp3_lim_sqd ) &
! Set the magnitude to the wp3 limit and apply the sign of the current wp3
      wp3 = sign( sqrt( wp3_lim_sqd ), wp3 )
! Set the magnitude to the wp3 limit and apply the sign of the current wp3
! Clipping abs(wp3) to 100. This keeps wp3 from growing too large in some
! deep convective cases, which helps prevent these cases from blowing up.
    where ( abs(wp3) > wp3_max ) &
      wp3 = sign( wp3_max , wp3 ) ! Known magic number
! Known magic number
        END SUBROUTINE clip_skewness_core
!===============================================================================
    END MODULE clip_explicit
