!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:30 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id: fill_holes.F90 8738 2018-07-19 19:58:53Z bmg2@uwm.edu $
!===============================================================================


module fill_holes
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PUBLIC vertical_avg 


    PRIVATE 

  contains
  !=============================================================================


  !=============================================================================


  !=============================================================================

  function vertical_avg( total_idx, rho_ds, field, dz )
    ! Description:
    ! Computes the density-weighted vertical average of a field.
    ! The average value of a function, f, over a set domain, [a,b], is
    ! calculated by the equation:
    ! f_avg = ( INT(a:b) f*g ) / ( INT(a:b) g );
    ! as long as f is continous and g is nonnegative and integrable.  Therefore,
    ! the density-weighted (by dry, static, base-static density) vertical
    ! average value of any model field, x, is calculated by the equation:
    ! x_avg|_z = ( INT(z_bot:z_top) x rho_ds dz )
    !            / ( INT(z_bot:z_top) rho_ds dz );
    ! where z_bot is the bottom of the vertical domain, and z_top is the top of
    ! the vertical domain.
    ! This calculation is done slightly differently depending on whether x is a
    ! thermodynamic-level or a momentum-level variable.
    ! Thermodynamic-level computation:
    ! For numerical purposes, INT(z_bot:z_top) x rho_ds dz, which is the
    ! numerator integral, is calculated as:
    ! SUM(k_bot:k_top) x(k) rho_ds(k) delta_z(k);
    ! where k is the index of the given thermodynamic level, x and rho_ds are
    ! both thermodynamic-level variables, and delta_z(k) = zm(k) - zm(k-1).  The
    ! indices k_bot and k_top are the indices of the respective lower and upper
    ! thermodynamic levels involved in the integration.
    ! Likewise, INT(z_bot:z_top) rho_ds dz, which is the denominator integral,
    ! is calculated as:
    ! SUM(k_bot:k_top) rho_ds(k) delta_z(k).
    ! The first (k=1) thermodynamic level is below ground (or below the
    ! official lower boundary at the first momentum level), so it should not
    ! count in a vertical average, whether that vertical average is used for
    ! the hole-filling scheme or for statistical purposes. Begin no lower
    ! than level k=2, which is the first thermodynamic level above ground (or
    ! above the model lower boundary).
    ! For cases where hole-filling over the entire (global) vertical domain
    ! is desired, or where statistics over the entire (global) vertical
    ! domain are desired, the lower (thermodynamic-level) index of k = 2 and
    ! the upper (thermodynamic-level) index of k = gr%nz, means that the
    ! overall vertical domain will be gr%zm(gr%nz) - gr%zm(1).
    ! Momentum-level computation:
    ! For numerical purposes, INT(z_bot:z_top) x rho_ds dz, which is the
    ! numerator integral, is calculated as:
    ! SUM(k_bot:k_top) x(k) rho_ds(k) delta_z(k);
    ! where k is the index of the given momentum level, x and rho_ds are both
    ! momentum-level variables, and delta_z(k) = zt(k+1) - zt(k).  The indices
    ! k_bot and k_top are the indices of the respective lower and upper momentum
    ! levels involved in the integration.
    ! Likewise, INT(z_bot:z_top) rho_ds dz, which is the denominator integral,
    ! is calculated as:
    ! SUM(k_bot:k_top) rho_ds(k) delta_z(k).
    ! The first (k=1) momentum level is right at ground level (or right at
    ! the official lower boundary).  The momentum level variables that call
    ! the hole-filling scheme have set values at the surface (or lower
    ! boundary), and those set values should not be changed.  Therefore, the
    ! vertical average (for purposes of hole-filling) should not include the
    ! surface level (or lower boundary level).  For hole-filling purposes,
    ! begin no lower than level k=2, which is the second momentum level above
    ! ground (or above the model lower boundary).  Likewise, the value at the
    ! model upper boundary (k=gr%nz) is also set for momentum level
    ! variables.  That value should also not be changed.
    ! However, this function is also used to keep track (for statistical
    ! purposes) of the vertical average of certain variables.  In that case,
    ! the vertical average needs to be taken over the entire vertical domain
    ! (level 1 to level gr%nz).
    ! In both the thermodynamic-level computation and the momentum-level
    ! computation, the numerator integral is divided by the denominator integral
    ! in order to find the average value (over the vertical domain) of x.
    ! References:
    ! None
    !-----------------------------------------------------------------------

    !
    !
    !
    !
    !
    !
    !
    
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !
    !


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input variables

    integer, intent(in) :: & 
      total_idx ! The total numer of indices within the range of averaging

    real( kind = core_rknd ), dimension(total_idx), intent(in) ::  &
      rho_ds, & ! Dry, static density on either thermodynamic or momentum levels    [kg/m^3]
      field,  & ! The field (e.g. wp2) to be vertically averaged                    [Units vary]
      dz  ! Reciprocal of thermodynamic or momentum level thickness           [1/m]
                ! depending on whether we're on zt or zm grid.
    ! Note:  The rho_ds and field points need to be arranged from
    !        lowest to highest in altitude, with rho_ds(1) and
    !        field(1) actually their respective values at level k = 1.
    ! Output variable

    real( kind = core_rknd ) :: & 
      vertical_avg  ! Vertical average of field    [Units of field]
    ! Local variables

    real( kind = core_rknd ) :: & 
      numer_integral, & ! Integral in the numerator (see description)
      denom_integral    ! Integral in the denominator (see description)
      

    integer :: k
    !-----------------------------------------------------------------------
    ! Initialize variable

    
    numer_integral = 0.0_core_rknd
    denom_integral = 0.0_core_rknd
    ! Compute the numerator and denominator integral.
    ! Multiply rho_ds at level k by the level thickness
    ! at level k.  Then, sum over all vertical levels.

    do k=1, total_idx

        numer_integral = numer_integral + rho_ds(k) * dz(k) * field(k)
        denom_integral = denom_integral + rho_ds(k) * dz(k)

    end do
    ! Find the vertical average of 'field'.

    vertical_avg = numer_integral / denom_integral

    return
  end function vertical_avg
  !=============================================================================


!===============================================================================


  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------


  !=============================================================================


  !-----------------------------------------------------------------------


  !-----------------------------------------------------------------------

end module fill_holes