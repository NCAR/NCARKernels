!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:38 
!KGEN version : 0.8.1 
  
!---------------------------------------------------------------------------
! $Id$
!===============================================================================


module turbulent_adv_pdf
  ! Description:
  ! Calculates the turbulent advection term in the predictive equation for a
  ! variance or covariance where turbulent advection is calculated by
  ! integrating over the PDF.  This includes the following predictive fields:
  ! <w'thl'>, <w'rt'>, <rt'^2>, <thl'^2>, and <rt'thl'>, as well as passive
  ! scalar fields <w'sclr'>, <sclr'^2>, <sclr'rt'>, and <sclr'thl'>.  CLUBB
  ! does not produce a PDF for horizontal wind components u and v.  However, the
  ! horizontal wind variances <u'^2> and <v'^2> still use this code, as well.
  ! References:
  !-------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 


    IMPLICIT NONE 

    PUBLIC xpyp_term_ta_pdf_lhs_all, xpyp_term_ta_pdf_rhs_all, sgn_turbulent_velocity 

    PRIVATE 

  contains
  !=============================================================================


    !=============================================================================================

    pure subroutine xpyp_term_ta_pdf_lhs_all( coef_wpxpyp_implicit, rho_ds_zt,  & ! Intent(in)
                                              invrs_rho_ds_zm, invrs_dzm,       & ! Intent(in)
                                              l_upwind_xpyp_turbulent_adv,      & ! Intent(in)
                                              sgn_turbulent_vel,                & ! Intent(in)
                                              coef_wpxpyp_implicit_zm,          & ! Intent(in)
                                              rho_ds_zm, invrs_dzt,             & ! Intent(in)
                                              lhs_ta                          ) ! Intent(out)
    ! Description:
    !   This subroutine is an optimized version of xpyp_term_ta_pdf_lhs. xpyp_term_ta_pdf_lhs
    !   returns a single 3 dimensional array for any specified grid level. This subroutine returns
    !   an array of 3 dimensional arrays, one for every grid level not including boundary values.
    ! Optional Arguements:
    !   The optional arguements can be used to override the default indices. 
    !   from_level - low index, default 2
    !   to level   - high index, default gr%nz-1
    ! Notes:
    !   This subroutine exists for performance concerns. It returns all lhs arrays at once
    !   so that it can be properly vectorized, see clubb:ticket:834 for detail.
    !   THIS SUBROUTINE DOES NOT HANDLE BOUNDARY CONDITIONS AND SETS THEM TO 0
    !---------------------------------------------------------------------------------------------
    ! 
    ! 
    !   

        USE grid_class, ONLY: gr 

        USE clubb_precision, ONLY: core_rknd 

        implicit none
        !------------------- Input Variables -------------------

        real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
            coef_wpxpyp_implicit,     & ! Coef. of <x'y'> in <w'x'y'>; t-lev    [m/s]
            rho_ds_zt,                & ! Dry, static density at t-level        [kg/m^3]
            invrs_rho_ds_zm,          & ! Inv dry, static density @ m-level     [m^3/kg]
            invrs_dzm,                & ! Inverse of grid spacing               [1/m]
            sgn_turbulent_vel,        & ! Sign of the turbulent velocity        [-]
            coef_wpxpyp_implicit_zm,  & ! coef_wpxpyp_implicit intrp m-lev      [m/s]
            rho_ds_zm,                & ! Dry, static density at m-lev          [kg/m^3]
            invrs_dzt                   ! Inverse of grid spacing               [1/m]

        logical, intent(in) :: &
            l_upwind_xpyp_turbulent_adv    ! Flag to use "upwind" discretization
        !------------------- Output Variables -------------------

        real( kind = core_rknd ), dimension(3,gr%nz), intent(out) :: &
            lhs_ta
        !---------------- Local Variables -------------------

        integer :: &
            k             ! Loop variable for current grid level
        !---------------- Begin Code -------------------
        ! Set lower boundary array to 0


        lhs_ta(:,1) = 0.0_core_rknd

        if ( .not. l_upwind_xpyp_turbulent_adv ) then

           do k = 2, gr%nz-1
               ! Momentum superdiagonal: [ x xpyp(k+1,<t+1>) ]

               lhs_ta(1,k) = invrs_rho_ds_zm(k) * invrs_dzm(k)          &
                             * rho_ds_zt(k+1) * coef_wpxpyp_implicit(k+1) &
                             * gr%weights_zm2zt(1,k+1)
               ! Momentum main diagonal: [ x xpyp(k,<t+1>) ]

               lhs_ta(2,k) = invrs_rho_ds_zm(k) * invrs_dzm(k)            &
                             * ( rho_ds_zt(k+1) * coef_wpxpyp_implicit(k+1) &
                               * gr%weights_zm2zt(2,k+1)                    &
                               - rho_ds_zt(k) * coef_wpxpyp_implicit(k)     &
                               * gr%weights_zm2zt(1,k) )
               ! Momentum subdiagonal: [ x xpyp(k-1,<t+1>) ]

               lhs_ta(3,k) = - invrs_rho_ds_zm(k) * invrs_dzm(k) * rho_ds_zt(k) &
                             * coef_wpxpyp_implicit(k) * gr%weights_zm2zt(2,k)

            end do

        else ! l_upwind_xpyp_turbulent_adv
           ! "Upwind" discretization

                
            do k = 2, gr%nz-1

                if ( sgn_turbulent_vel(k) > 0.0_core_rknd ) then
                    ! The "wind" is blowing upward.
                    ! Momentum superdiagonal: [ x xpyp(k+1,<t+1>) ]


                    lhs_ta(1,k) = 0.0_core_rknd
                    ! Momentum main diagonal: [ x xpyp(k,<t+1>) ]

                    lhs_ta(2,k) = invrs_rho_ds_zm(k) * invrs_dzt(k) &
                                  * rho_ds_zm(k) * coef_wpxpyp_implicit_zm(k)
                    ! Momentum subdiagonal: [ x xpyp(k-1,<t+1>) ]

                    lhs_ta(3,k) = - invrs_rho_ds_zm(k) * invrs_dzt(k) &
                                  * rho_ds_zm(k-1) * coef_wpxpyp_implicit_zm(k-1)

                else ! The "wind" is blowing downward.
                    ! Momentum superdiagonal: [ x xpyp(k+1,<t+1>) ]

                    lhs_ta(1,k) = invrs_rho_ds_zm(k) * invrs_dzt(k+1) &
                                  * rho_ds_zm(k+1)* coef_wpxpyp_implicit_zm(k+1)
                    ! Momentum main diagonal: [ x xpyp(k,<t+1>) ]

                    lhs_ta(2,k) = - invrs_rho_ds_zm(k) * invrs_dzt(k+1) &
                                  * rho_ds_zm(k) * coef_wpxpyp_implicit_zm(k)
                    ! Momentum subdiagonal: [ x xpyp(k-1,<t+1>) ]

                    lhs_ta(3,k) = 0.0_core_rknd

                endif ! sgn_turbulent_vel

            end do

        endif
        ! Set upper boundary array to 0

        lhs_ta(:,gr%nz) = 0.0_core_rknd

        return

    end subroutine xpyp_term_ta_pdf_lhs_all
  !=============================================================================


    !============================================================================================


    pure subroutine xpyp_term_ta_pdf_rhs_all( term_wpxpyp_explicit, rho_ds_zt,  & ! Intent(in)
                                              invrs_rho_ds_zm, invrs_dzm,       & ! Intent(in)
                                              l_upwind_xpyp_turbulent_adv,      & ! Intent(in)
                                              sgn_turbulent_vel,                & ! Intent(in)
                                              term_wpxpyp_explicit_zm,          & ! Intent(in)
                                              rho_ds_zm, invrs_dzt,             & ! Intent(in)
                                              rhs_ta                          ) ! Intent(out)
    ! Description:
    !   This subroutine is an optimized version of xpyp_term_ta_pdf_rhs. xpyp_term_ta_pdf_rhs
    !   returns a single value for any specified grid level. This subroutine returns an array
    !   of values for every grid level.
    ! Optional Arguements:
    !   The optional arguements can be used to override the default indices. 
    !   from_level - low index, default 2
    !   to level   - high index, default gr%nz-1
    ! Notes:
    !   This subroutine exists for performance concerns. It returns all rhs values at once
    !   so that it can be properly vectorized, see clubb:ticket:834 for detail.
    !   THIS SUBROUTINE DOES NOT HANDLE BOUNDARY CONDITIONS AND SETS THEM TO 0
    !--------------------------------------------------------------------------------------------
    ! 
    ! 
    !   
        USE clubb_precision, ONLY: core_rknd 

        USE grid_class, ONLY: gr 

        implicit none
        !------------------- Input Variables -------------------

        real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
          term_wpxpyp_explicit,         & ! RHS: <w'x'y'> eq; t-lev(k)   [m/s(x un)(y un)]
          rho_ds_zt,                    & ! Dry, static density at t-lev (k)      [kg/m^3]
          invrs_rho_ds_zm,              & ! Inv dry, static density at m-lev (k)  [m^3/kg]
          invrs_dzm,                    & ! Inverse of grid spacing (k)              [1/m]
          sgn_turbulent_vel,            & ! Sign of the turbulent velocity          [-]
          term_wpxpyp_explicit_zm,      & ! term_wpxpyp_expl. zm(k)   [m/s(x un)(y un)]
          rho_ds_zm,                    & ! Dry, static density at m-lev (k)   [kg/m^3]
          invrs_dzt                       ! Inverse of grid spacing (k)           [1/m]

        logical, intent(in) :: &
          l_upwind_xpyp_turbulent_adv    ! Flag to use "upwind" discretization
        !------------------- Output Variables -------------------

        real( kind = core_rknd ), dimension(gr%nz), intent(out) :: &
            rhs_ta
        !---------------- Local Variables -------------------

        integer :: &
            k             ! Loop variable for current grid level
        !---------------- Begin Code -------------------
        ! Set lower boundary value to 0


        rhs_ta(1) = 0.0_core_rknd

        if ( .not. l_upwind_xpyp_turbulent_adv ) then
           ! Centered discretization.


            do k = 2, gr%nz-1
                rhs_ta(k) = - invrs_rho_ds_zm(k) * invrs_dzm(k) &
                            * ( rho_ds_zt(k+1) * term_wpxpyp_explicit(k+1) &
                              - rho_ds_zt(k) * term_wpxpyp_explicit(k) )
            end do

        else ! l_upwind_xpyp_turbulent_adv
           ! "Upwind" discretization


            do k = 2, gr%nz-1

                if ( sgn_turbulent_vel(k) > 0.0_core_rknd ) then
                    ! The "wind" is blowing upward.

                    rhs_ta(k) = - invrs_rho_ds_zm(k) * invrs_dzt(k) &
                           * ( rho_ds_zm(k) * term_wpxpyp_explicit_zm(k) &
                             - rho_ds_zm(k-1) * term_wpxpyp_explicit_zm(k-1) )

                else ! sgn_turbulent_vel < 0
                    ! The "wind" is blowing downward.

                    rhs_ta(k) = - invrs_rho_ds_zm(k) * invrs_dzt(k+1) &
                                * ( rho_ds_zm(k+1) * term_wpxpyp_explicit_zm(k+1) &
                                  - rho_ds_zm(k) * term_wpxpyp_explicit_zm(k) )

                endif ! sgn_turbulent_vel

            end do

        endif
        ! Set upper boundary value to 0

        rhs_ta(gr%nz) = 0.0_core_rknd

        return

    end subroutine xpyp_term_ta_pdf_rhs_all
  !=============================================================================
  

  pure function sgn_turbulent_velocity( wpxpyp_zm, xpyp ) &
  result( sgn_turbulent_vel )
    ! Description:
    ! Calculates the sign of the turbulent velocity for a predictive variance
    ! or covariance, which is used for the "upwind" turbulent advection option.
    ! The turbulent velocity for any predictive variance or covariance <x'y'> is
    ! <w'x'y'> / <x'y'>, which has units of m/s.  The sign of the turbulent
    ! velocity is sgn( <w'x'y'> / <x'y'> ), where:
    ! sgn(x) = | 1; when x >= 0
    !          | -1; when x < 0.
    ! The sign of the turbulent velocity can also be rewritten as
    ! sgn( <w'x'y'> ) / sgn( <x'y'> ).  When a variance (<x'^2>) is being solved
    ! for, y = x, and sgn( <x'^2> ) is always 1.  The sign of the turbulent
    ! velocity reduces to simply sgn( <w'x'^2> ).
    ! The values of <x'y'> are found on momentum levels, while the values of
    ! <w'x'y'> are found on thermodynamic levels.  The values of <w'x'y'> are
    ! interpolated to momentum levels.  The sign of <x'y'> and the sign of
    ! the interpolated value of <w'x'y'> are calculated on the central momentum
    ! level, where sgn( <w'x'y'> ) is divided by sgn( <x'y'> ).
    ! References:
    !-----------------------------------------------------------------------

    !
    !
    !
    !


      USE grid_class, ONLY: gr 

      USE constants_clubb, ONLY: one, zero 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      wpxpyp_zm, & ! <w'x'y'> interpolated to momentum levels  [m/s(x un)(y un)]
      xpyp         ! Predictive (co)variance <x'y'> (m-levs.)  [(x un)(y un)]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      sgn_turbulent_vel    ! Sign of the turbulent velocity    [-]
    ! Local Variables

    real( kind = core_rknd ) :: &
      sgn_wpxpyp, & ! Sign of <w'x'y'>    [-]
      sgn_xpyp      ! Sign of <x'y'>      [-]

    integer :: k    ! Vertical level index
    ! Calculate the sign on the turbulent velocity at every momentum level.


    do k = 1, gr%nz, 1
       ! Calculate the sign of <w'x'y'>.

       if ( wpxpyp_zm(k) >= zero ) then
          sgn_wpxpyp = one
       else ! wpxpyp_zm(k) < 0
          sgn_wpxpyp = -one
       endif ! wpxpyp_zm(k) >= 0
       ! Calculate the sign of <x'y'>.

       if ( xpyp(k) >= zero ) then
          sgn_xpyp = one
       else ! xpyp(k) < 0
          sgn_xpyp = -one
       endif ! xpyp(k) >= 0
       ! Calculate the sign on the turbulent velocity.

       sgn_turbulent_vel(k) = sgn_wpxpyp / sgn_xpyp

    enddo ! k = 1, gr%nz, 1


    return

  end function sgn_turbulent_velocity
!===============================================================================


end module turbulent_adv_pdf