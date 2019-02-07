!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:35 
!KGEN version : 0.8.1 
  
!------------------------------------------------------------------------
! $Id: grid_class.F90 7200 2014-08-13 15:15:12Z betlej@uwm.edu $
!===============================================================================


module grid_class
  ! Description:
  ! Definition of a grid class and associated functions
  ! The grid specification is as follows:
  !     +                ================== zm(nz) =========GP=========
  !     |
  !     |
  ! 1/dzt(nz) +          ------------------ zt(nz) ---------GP---------
  !     |        |
  !     |        |
  !     + 1/dzm(nz-1)    ================== zm(nz-1) ==================
  !              |
  !              |
  !              +       ------------------ zt(nz-1) ------------------
  !                                           .
  !                                           .
  !                                           .
  !                                           .
  !                      ================== zm(k+1) ===================
  !              +       ------------------ zt(k+1) -------------------
  !              |
  !              |
  !     +    1/dzm(k)    ================== zm(k) =====================
  !     |        |
  !     |        |
  ! 1/dzt(k)     +       ------------------ zt(k) ---------------------
  !     |
  !     |
  !     +                ================== zm(k-1) ===================
  !                      ------------------ zt(k-1) -------------------
  !                                           .
  !                                           .
  !                                           .
  !                                           .
  !     +                ================== zm(2) =====================
  !     |
  !     |
  ! 1/dzt(2)     +       ------------------ zt(2) ---------------------
  !     |        |
  !     |        |
  !     +    1/dzm(1)    ================== zm(1) ============GP=======  zm_init
  !              |       //////////////////////////////////////////////  surface
  !              |
  !              +       ------------------ zt(1) ------------GP-------
  ! The variable zm(k) stands for the momentum level altitude at momentum
  ! level k; the variable zt(k) stands for the thermodynamic level altitude at
  ! thermodynamic level k; the variable invrs_dzt(k) is the inverse distance
  ! between momentum levels (over a central thermodynamic level k); and the
  ! variable invrs_dzm(k) is the inverse distance between thermodynamic levels
  ! (over a central momentum level k).  Please note that in the above diagram,
  ! "invrs_dzt" is denoted "dzt", and "invrs_dzm" is denoted "dzm", such that
  ! 1/dzt is the distance between successive momentum levels k-1 and k (over a
  ! central thermodynamic level k), and 1/dzm is the distance between successive
  ! thermodynamic levels k and k+1 (over a central momentum level k).
  ! The grid setup is compatible with a stretched (unevely-spaced) grid.  Thus,
  ! the distance between successive grid levels may not always be constant.
  ! The following diagram is an example of a stretched grid that is defined on
  ! momentum levels.  The thermodynamic levels are placed exactly halfway
  ! between the momentum levels.  However, the momentum levels do not fall
  ! halfway between the thermodynamic levels.
  !        =============== zm(k+1) ===============
  !        --------------- zt(k+1) ---------------
  !        ===============  zm(k)  ===============
  !        ---------------  zt(k)  ---------------
  !        =============== zm(k-1) ===============
  ! The following diagram is an example of a stretched grid that is defined on
  ! thermodynamic levels.  The momentum levels are placed exactly halfway
  ! between the thermodynamic levels.  However, the thermodynamic levels do not
  ! fall halfway between the momentum levels.
  !        --------------- zt(k+1) ---------------
  !        ===============  zm(k)  ===============
  !        ---------------  zt(k)  ---------------
  !        =============== zm(k-1) ===============
  !        --------------- zt(k-1) ---------------
  ! NOTE:  Any future code written for use in the CLUBB parameterization should
  !        use interpolation formulas consistent with a stretched grid.  The
  !        simplest way to do so is to call the appropriate interpolation
  !        function from this module.  Interpolations should *not* be handled in
  !        the form of:  ( var_zm(k) + var_zm(k-1) ) / 2; *nor* in the form of:
  !        0.5_core_rknd*( var_zt(k+1) + var_zt(k) ).  Rather, all explicit interpolations
  !        should call zt2zm or zm2zt; while interpolations for a variable being
  !        solved for implicitly in the code should use gr%weights_zt2zm (which
  !        refers to interp_weights_zt2zm_imp), or gr%weights_zm2zt (which
  !        refers to interp_weights_zm2zt_imp).
  ! Momentum level 1 is placed at altitude zm_init, which is usually at the
  ! surface.  However, in general, zm_init can be at any altitude defined by the
  ! user.
  ! GP indicates ghost points. Variables located at those levels are not
  ! prognosed, but only used for boundary conditions.
  ! Chris Golaz, 7/17/99
  ! modified 9/10/99
  ! schemena, modified 6/11/2014 - Restructered code to add cubic/linear flag
  !  References:
  !  Section 3c, p. 3548 /Numerical discretization/ of:
  !   ``A PDF-Based Model for Boundary Layer Clouds. Part I:
  !     Method and Model Description'' Golaz, et al. (2002)
  !     JAS, Vol. 59, pp. 3540--3551.
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
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PUBLIC gr, grid, zt2zm, interp_weights_zt2zm_imp, zm2zt, interp_weights_zm2zt_imp, ddzm, ddzt, setup_grid_heights 

    PRIVATE linear_interpolated_azm, linear_interpolated_azmk, interpolated_azmk_imp, linear_interpolated_azt, &
    &linear_interpolated_aztk, interpolated_aztk_imp, gradzm, gradzt, t_above, t_below, m_above, m_below, &
    &cubic_interpolated_azmk, cubic_interpolated_aztk, cubic_interpolated_azm, cubic_interpolated_azt 

    PRIVATE 
  ! Constant parameters

  integer, parameter :: & 
    t_above = 1, & ! Upper thermodynamic level index (gr%weights_zt2zm).
    t_below = 2, & ! Lower thermodynamic level index (gr%weights_zt2zm).
    m_above = 1, & ! Upper momentum level index (gr%weights_zm2zt).
    m_below = 2    ! Lower momentum level index (gr%weights_zm2zt).

  type grid

    integer :: nz ! Number of points in the grid
    !   Note: Fortran 90/95 prevents an allocatable array from appearing
    !   within a derived type.  However, Fortran 2003 does not!!!!!!!!
    real( kind = core_rknd ), allocatable, dimension(:) :: &
      zm, & ! Momentum grid
      zt    ! Thermo grid
    real( kind = core_rknd ), allocatable, dimension(:) :: &
      invrs_dzm, & ! The inverse spacing between thermodynamic grid
                   ! levels; centered over momentum grid levels.
      invrs_dzt    ! The inverse spacing between momentum grid levels;
                   ! levels; centered over momentum grid levels.
                   ! centered over thermodynamic grid levels.

    real( kind = core_rknd ), allocatable, dimension(:) :: &
      dzm, &  ! Spacing between thermodynamic grid levels; centered over
              ! momentum grid levels
      dzt     ! Spcaing between momentum grid levels; centered over
              ! momentum grid levels
              ! thermodynamic grid levels
    ! These weights are normally used in situations
    ! where a momentum level variable is being
    ! solved for implicitly in an equation and
    ! needs to be interpolated to the thermodynamic grid levels.

    real( kind = core_rknd ), allocatable, dimension(:,:) :: weights_zm2zt, & 
    ! These weights are normally used in situations where a
    ! thermodynamic level variable is being solved for implicitly in an equation
    ! and needs to be interpolated to the momentum grid levels.
                                     weights_zt2zm
    ! These weights are normally used in situations where a
    ! thermodynamic level variable is being solved for implicitly in an equation
    ! and needs to be interpolated to the momentum grid levels.

  end type grid
  !   The grid is defined here so that it is common throughout the module.
  !   The implication is that only one grid can be defined !


  type (grid), target :: gr
!   Modification for using CLUBB in a host model (i.e. one grid per column)
  ! Interfaces provided for function overloading

!$omp threadprivate(gr)


  interface zt2zm
    ! For l_cubic_interp = .true.
    ! This version uses cublic spline interpolation of Stefen (1990).
    ! For l_cubic_interp = .false.
    ! This performs a linear extension at the highest grid level and therefore
    ! does not guarantee, for positive definite quantities (e.g. wp2), that the
    ! extended point is indeed positive definite.  Positive definiteness can be
    ! ensured with a max statement.
    ! In the future, we could add a flag (lposdef) and, when needed, apply the
    ! max statement directly within interpolated_azm and interpolated_azmk.
    !
    module procedure redirect_interpolated_azmk, redirect_interpolated_azm
  end interface

  interface zm2zt
    ! For l_cubic_interp = .true.
    ! This version uses cublic spline interpolation of Stefen (1990).
    ! For l_cubic_interp = .false.
    ! This performs a linear extension at the lowest grid level and therefore
    ! does not guarantee, for positive definite quantities (e.g. wp2), that the
    ! extended point is indeed positive definite.  Positive definiteness can be
    ! ensured with a max statement.
    ! In the future, we could add a flag (lposdef) and, when needed, apply the
    ! max statement directly within interpolated_azt and interpolated_aztk.
    !
    module procedure redirect_interpolated_aztk, redirect_interpolated_azt
  end interface

  interface interp_weights_zt2zm_imp
    module procedure interpolated_azmk_imp
  end interface


  interface interp_weights_zm2zt_imp
    module procedure interpolated_aztk_imp
  end interface
  ! Vertical derivative functions

  interface ddzm
    module procedure gradzm
  end interface

  interface ddzt
    module procedure gradzt
  end interface
  PUBLIC kr_externs_in_grid_class 
  PUBLIC kr_externs_out_grid_class 
  PUBLIC kv_externs_grid_class 
  PUBLIC kr_grid_class_grid 
  TYPE(grid) :: kgenref_gr 
  PUBLIC kv_grid_class_grid 

  contains
  !=============================================================================


  !=============================================================================


  !=============================================================================

  subroutine setup_grid_heights &
             ( l_implemented, grid_type,  & 
               deltaz, zm_init, momentum_heights,  & 
               thermodynamic_heights )
    ! Description:
    !   Sets the heights and interpolation weights of the column.
    !   This is seperated from setup_grid for those host models that have heights
    !   that vary with time.
    ! References:
    !   None
    !------------------------------------------------------------------------------


      USE constants_clubb, ONLY: fstderr 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables
    ! Flag to see if CLUBB is running on it's own,
    ! or if it's implemented as part of a host model.


    logical, intent(in) :: l_implemented
    ! If CLUBB is running on it's own, this option determines if it is using:
    ! 1) an evenly-spaced grid;
    ! 2) a stretched (unevenly-spaced) grid entered on the thermodynamic grid
    !    levels (with momentum levels set halfway between thermodynamic levels);
    !    or
    ! 3) a stretched (unevenly-spaced) grid entered on the momentum grid levels
    !    (with thermodynamic levels set halfway between momentum levels).

    integer, intent(in) :: grid_type
    ! If the CLUBB model is running by itself, and is using an evenly-spaced
    ! grid (grid_type = 1), it needs the vertical grid spacing and
    ! momentum-level starting altitude as input.

    real( kind = core_rknd ), intent(in) ::  & 
      deltaz,   & ! Vertical grid spacing                  [m]
      zm_init     ! Initial grid altitude (momentum level) [m]
    ! If the CLUBB parameterization is implemented in a host model, it needs to
    ! use the host model's momentum level altitudes and thermodynamic level
    ! altitudes.
    ! If the CLUBB model is running by itself, but is using a stretched grid
    ! entered on thermodynamic levels (grid_type = 2), it needs to use the
    ! thermodynamic level altitudes as input.
    ! If the CLUBB model is running by itself, but is using a stretched grid
    ! entered on momentum levels (grid_type = 3), it needs to use the momentum
    ! level altitudes as input.


    real( kind = core_rknd ), intent(in), dimension(gr%nz) ::  & 
      momentum_heights,   & ! Momentum level altitudes (input)      [m]
      thermodynamic_heights ! Thermodynamic level altitudes (input) [m]

    integer :: k
    ! ---- Begin Code ----


    if ( .not. l_implemented ) then


      if ( grid_type == 1 ) then
        ! Evenly-spaced grid.
        ! Momentum level altitudes are defined based on the grid starting
        ! altitude, zm_init, the constant grid-spacing, deltaz, and the number
        ! of grid levels, gr%nz.
        ! Define momentum level altitudes. The first momentum level is at
        ! altitude zm_init.


        do k = 1, gr%nz, 1
          gr%zm(k) = zm_init + real( k-1, kind = core_rknd ) * deltaz
        enddo
        ! Define thermodynamic level altitudes.  Thermodynamic level altitudes
        ! are located at the central altitude levels, exactly halfway between
        ! momentum level altitudes.  The lowermost thermodynamic level is
        ! found by taking 1/2 the altitude difference between the bottom two
        ! momentum levels and subtracting that value from the bottom momentum
        ! level.  The first thermodynamic level is below zm_init.

        gr%zt(1) = zm_init - ( 0.5_core_rknd * deltaz )
        do k = 2, gr%nz, 1
          gr%zt(k) = 0.5_core_rknd * ( gr%zm(k) + gr%zm(k-1) )
        enddo


      elseif ( grid_type == 2 ) then
        ! Stretched (unevenly-spaced) grid:  stretched thermodynamic levels.
        ! Thermodynamic levels are defined according to a stretched grid that
        ! is entered through the use of an input file.  This is similar to a
        ! SAM-style stretched grid.
        ! Define thermodynamic level altitudes.


        do k = 1, gr%nz, 1
          gr%zt(k) = thermodynamic_heights(k)
        enddo
        ! Define momentum level altitudes.  Momentum level altitudes are
        ! located at the central altitude levels, exactly halfway between
        ! thermodynamic level altitudes.  The uppermost momentum level
        ! altitude is found by taking 1/2 the altitude difference between the
        ! top two thermodynamic levels and adding that value to the top
        ! thermodynamic level.

        do k = 1, gr%nz-1, 1
          gr%zm(k) = 0.5_core_rknd * ( gr%zt(k+1) + gr%zt(k) )
        enddo
        gr%zm(gr%nz) = gr%zt(gr%nz) +  & 
             0.5_core_rknd * ( gr%zt(gr%nz) - gr%zt(gr%nz-1) )

      elseif ( grid_type == 3 ) then
        ! Stretched (unevenly-spaced) grid:  stretched momentum levels.
        ! Momentum levels are defined according to a stretched grid that is
        ! entered through the use of an input file.  This is similar to a
        ! WRF-style stretched grid.
        ! Define momentum level altitudes.


        do k = 1, gr%nz, 1
          gr%zm(k) = momentum_heights(k)
        enddo
        ! Define thermodynamic level altitudes.  Thermodynamic level altitudes
        ! are located at the central altitude levels, exactly halfway between
        ! momentum level altitudes.  The lowermost thermodynamic level
        ! altitude is found by taking 1/2 the altitude difference between the
        ! bottom two momentum levels and subtracting that value from the
        ! bottom momentum level.

        gr%zt(1) = gr%zm(1) - 0.5_core_rknd * ( gr%zm(2) - gr%zm(1) )
        do k = 2, gr%nz, 1
          gr%zt(k) = 0.5_core_rknd * ( gr%zm(k) + gr%zm(k-1) )
        enddo


      else
        ! Invalid grid type.

        write(fstderr,*) "Invalid grid type: ", grid_type, & 
                         ".  Valid options are 1, 2, or 3."
        stop "Fatal error."


      endif


    else
      ! The CLUBB parameterization is implemented in a host model.
      ! Use the host model's momentum level altitudes and thermodynamic level
      ! altitudes to set up the CLUBB grid.
      ! Momentum level altitudes from host model.


      do k = 1, gr%nz, 1
        gr%zm(k) = momentum_heights(k)
      enddo
      ! Thermodynamic level altitudes from host model after possible grid-index
      ! adjustment for CLUBB interface.

      do k = 1, gr%nz, 1
        gr%zt(k) = thermodynamic_heights(k)
      enddo


    endif ! not l_implemented
    ! Define dzm, the spacing between thermodynamic grid levels; centered over
    ! momentum grid levels


    do k=1,gr%nz-1
      gr%dzm(k) = gr%zt(k+1) - gr%zt(k)
    enddo
    gr%dzm(gr%nz) = gr%dzm(gr%nz-1)
    ! Define dzt, the spacing between momentum grid levels; centered over
    ! thermodynamic grid levels

    do k=2,gr%nz
      gr%dzt(k) = gr%zm(k) - gr%zm(k-1)
    enddo
    gr%dzt(1) = gr%dzt(2)
    ! Define invrs_dzm, which is the inverse spacing between thermodynamic grid
    ! levels; centered over momentum grid levels.

    do k=1,gr%nz-1
      gr%invrs_dzm(k) = 1._core_rknd / ( gr%zt(k+1) - gr%zt(k) )
    enddo
    gr%invrs_dzm(gr%nz) = gr%invrs_dzm(gr%nz-1)
    ! Define invrs_dzt, which is the inverse spacing between momentum grid
    ! levels; centered over thermodynamic grid levels.


    do k=2,gr%nz
      gr%invrs_dzt(k) = 1._core_rknd / ( gr%zm(k) - gr%zm(k-1) )
    enddo
    gr%invrs_dzt(1) = gr%invrs_dzt(2)
    ! Interpolation Weights: zm grid to zt grid.
    ! The grid index (k) is the index of the level on the thermodynamic (zt)
    ! grid.  The result is the weights of the upper and lower momentum levels
    ! (that sandwich the thermodynamic level) applied to that thermodynamic
    ! level.  These weights are normally used in situations where a momentum
    ! level variable is being solved for implicitly in an equation, and the
    ! aforementioned variable needs to be interpolated from three successive
    ! momentum levels (the central momentum level, as well as one momentum level
    ! above and below the central momentum level) to the intermediate
    ! thermodynamic grid levels that sandwich the central momentum level.
    ! For more information, see the comments in function interpolated_aztk_imp.


    do k = 1, gr%nz, 1
      gr%weights_zm2zt(m_above:m_below,k)  & 
             = interp_weights_zm2zt_imp( k )
    enddo
    ! Interpolation Weights: zt grid to zm grid.
    ! The grid index (k) is the index of the level on the momentum (zm) grid.
    ! The result is the weights of the upper and lower thermodynamic levels
    ! (that sandwich the momentum level) applied to that momentum level.  These
    ! weights are normally used in situations where a thermodynamic level
    ! variable is being solved for implicitly in an equation, and the
    ! aforementioned variable needs to be interpolated from three successive
    ! thermodynamic levels (the central thermodynamic level, as well as one
    ! thermodynamic level above and below the central thermodynamic level) to
    ! the intermediate momentum grid levels that sandwich the central
    ! thermodynamic level.
    ! For more information, see the comments in function interpolated_azmk_imp.


    do k = 1, gr%nz, 1
      gr%weights_zt2zm(t_above:t_below,k)  & 
             = interp_weights_zt2zm_imp( k )
    enddo

    return
  end subroutine setup_grid_heights
  !=============================================================================


  !=============================================================================

  function redirect_interpolated_azmk( azt, k )
    ! Description:
    ! Calls the appropriate corresponding function based on l_cubic_temp
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE model_flags, ONLY: l_cubic_interp, l_quintic_poly_interp 

      USE constants_clubb, ONLY: fstdout 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]

    integer, intent(in) :: &
      k    ! Vertical level index
    ! Return Variable

    real( kind = core_rknd ) :: &
      redirect_interpolated_azmk    ! Variable when interp. to momentum levels
    ! ---- Begin Code ----
    ! Sanity Check


    if (l_quintic_poly_interp) then
      if (.not. l_cubic_interp) then
        write (fstdout, *) "Error: Model flag l_quintic_poly_interp should not be true if "&
                         //"l_cubic_interp is false."
        stop
      end if
    end if
    ! Redirect

    if (l_cubic_interp) then
      redirect_interpolated_azmk = cubic_interpolated_azmk( azt, k )
    else
      redirect_interpolated_azmk = linear_interpolated_azmk( azt, k )
    end if

    return
  end function redirect_interpolated_azmk
  !=============================================================================

  function redirect_interpolated_azm( azt )
    ! Description:
    ! Calls the appropriate corresponding function based on l_cubic_temp
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE model_flags, ONLY: l_cubic_interp, l_quintic_poly_interp 

      USE constants_clubb, ONLY: fstdout 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      redirect_interpolated_azm    ! Variable when interp. to momentum levels
    ! ---- Begin Code ----
    ! Sanity Check


    if (l_quintic_poly_interp) then
      if (.not. l_cubic_interp) then
        write (fstdout, *) "Error: Model flag l_quintic_poly_interp should not be true if "&
                         //"l_cubic_interp is false."
        stop
      end if
    end if
    ! Redirect

    if (l_cubic_interp) then
      redirect_interpolated_azm = cubic_interpolated_azm( azt )
    else
      redirect_interpolated_azm = linear_interpolated_azm( azt )
    end if

    return
  end function redirect_interpolated_azm
  !=============================================================================

  function redirect_interpolated_aztk( azt, k )
    ! Description:
    ! Calls the appropriate corresponding function based on l_cubic_temp
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE model_flags, ONLY: l_cubic_interp, l_quintic_poly_interp 

      USE constants_clubb, ONLY: fstdout 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]

    integer, intent(in) :: &
      k    ! Vertical level index
    ! Return Variable

    real( kind = core_rknd ) :: &
      redirect_interpolated_aztk    ! Variable when interp. to momentum levels
    ! ---- Begin Code ----
    ! Sanity Check


    if (l_quintic_poly_interp) then
      if (.not. l_cubic_interp) then
        write (fstdout, *) "Error: Model flag l_quintic_poly_interp should not be true if "&
                         //"l_cubic_interp is false."
        stop
      end if
    end if
    ! Redirect

    if (l_cubic_interp) then
      redirect_interpolated_aztk = cubic_interpolated_aztk( azt, k )
    else
      redirect_interpolated_aztk = linear_interpolated_aztk( azt, k )
    end if

    return
  end function redirect_interpolated_aztk
  !=============================================================================

  function redirect_interpolated_azt( azt )
    ! Description:
    ! Calls the appropriate corresponding function based on l_cubic_temp
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE model_flags, ONLY: l_cubic_interp, l_quintic_poly_interp 

      USE constants_clubb, ONLY: fstdout 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      redirect_interpolated_azt    ! Variable when interp. to momentum levels
    ! ---- Begin Code ----
    ! Sanity Check


    if (l_quintic_poly_interp) then
      if (.not. l_cubic_interp) then
        write (fstdout, *) "Error: Model flag l_quintic_poly_interp should not be true if "&
                         //"l_cubic_interp is false."
        stop
      end if
    end if
    ! Redirect

    if (l_cubic_interp) then
      redirect_interpolated_azt = cubic_interpolated_azt( azt )
    else
      redirect_interpolated_azt = linear_interpolated_azt( azt )
    end if

    return
  end function redirect_interpolated_azt
  !=============================================================================


  pure function linear_interpolated_azm( azt )
    ! Description:
    ! Function to interpolate a variable located on the thermodynamic grid
    ! levels (azt) to the momentum grid levels (azm).  This function inputs the
    ! entire azt array and outputs the results as an azm array.  The
    ! formulation used is compatible with a stretched (unevenly-spaced) grid.
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE interpolation, ONLY: linear_interp_factor 

    implicit none
    ! Input Variable

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      linear_interpolated_azm    ! Variable when interp. to momentum levels
    ! Local Variable

    integer :: k  ! Grid level loop index
    ! Set the value of the thermodynamic-level variable, azt, at the uppermost
    ! level of the model, which is a momentum level.  The name of the variable
    ! when interpolated/extended to momentum levels is azm.


    k = gr%nz
!    ! Set the value of azm at level gr%nz (the uppermost level in the model)
!    ! to the value of azt at level gr%nz.
!    linear_interpolated_azm(k) = azt(k)
    ! Use a linear extension based on the values of azt at levels gr%nz and
    ! gr%nz-1 to find the value of azm at level gr%nz (the uppermost level
    ! in the model).
    linear_interpolated_azm(k) &
    = ( ( azt(k) - azt(k-1) ) / ( gr%zt(k) - gr%zt(k-1) ) ) & 
      * ( gr%zm(k) - gr%zt(k) ) + azt(k)
    ! Interpolate the value of a thermodynamic-level variable to the central
    ! momentum level, k, between two successive thermodynamic levels using
    ! linear interpolation.

    forall( k = 1 : gr%nz-1 : 1 )
       linear_interpolated_azm(k) &
       = linear_interp_factor( gr%weights_zt2zm(1, k), azt(k+1), azt(k) )
    end forall ! k = 1 : gr%nz-1 : 1


    return

  end function linear_interpolated_azm
  !=============================================================================

  pure function linear_interpolated_azmk( azt, k )
    ! Description:
    ! Function to interpolate a variable located on the thermodynamic grid
    ! levels (azt) to the momentum grid levels (azm).  This function outputs the
    ! value of azm at a single grid level (k) after interpolating using values
    ! of azt at two grid levels.  The formulation used is compatible with a
    ! stretched (unevenly-spaced) grid.
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE interpolation, ONLY: linear_interp_factor 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]

    integer, intent(in) :: &
      k    ! Vertical level index
    ! Return Variable

    real( kind = core_rknd ) :: &
      linear_interpolated_azmk    ! Variable when interp. to momentum levels
    ! Interpolate the value of a thermodynamic-level variable to the central
    ! momentum level, k, between two successive thermodynamic levels using
    ! linear interpolation.


    if ( k /= gr%nz ) then

       linear_interpolated_azmk &
       = linear_interp_factor( gr%weights_zt2zm(1, k), azt(k+1), azt(k) )

    else
!       ! Set the value of azm at level gr%nz (the uppermost level in the
!       ! model) to the value of azt at level gr%nz.
!       linear_interpolated_azmk = azt(gr%nz)
       ! Use a linear extension based on the values of azt at levels gr%nz and
       ! gr%nz-1 to find the value of azm at level gr%nz (the uppermost
       ! level in the model).

       linear_interpolated_azmk &
       = ( ( azt(gr%nz) - azt(gr%nz-1) ) / ( gr%zt(gr%nz) - gr%zt(gr%nz-1) ) ) & 
         * ( gr%zm(gr%nz) - gr%zt(gr%nz) ) + azt(gr%nz)

    endif


    return

  end function linear_interpolated_azmk
  !=============================================================================

  function cubic_interpolated_azm( azt )
    ! Description:
    ! Function to interpolate a variable located on the thermodynamic grid
    ! levels (azt) to the momentum grid levels (azm).  This function outputs the
    ! value of azt at a all grid levels using Steffen's monotonic cubic
    ! interpolation implemented by Tak Yamaguchi.
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      cubic_interpolated_azm
    ! Local Variable(s)

    real( kind = core_rknd ), dimension(gr%nz) :: &
      tmp ! This is needed for variables that self-reference
    integer :: &
      k
    ! ---- Begin Code ----


    do k = 1, gr%nz 
      tmp(k) = cubic_interpolated_azmk( azt, k )
    end do

    cubic_interpolated_azm = tmp

    return

  end function cubic_interpolated_azm
  !=============================================================================

  function cubic_interpolated_azmk( azt, k )
    ! Description:
    ! Function to interpolate a variable located on the thermodynamic grid
    ! levels (azt) to the momentum grid levels (azm).  This function outputs the
    ! value of azm at a single grid level (k) using Steffen's monotonic cubic
    ! interpolation implemented by Tak Yamaguchi.
    !-----------------------------------------------------------------------


      USE interpolation, ONLY: mono_cubic_interp 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: azt

    integer, intent(in) :: k
    ! Return Variable

    real( kind = core_rknd ) :: cubic_interpolated_azmk
    ! Local Variable(s)

    integer :: km1, k00, kp1, kp2
    ! ---- Begin Code ----
    ! Special case for a very small domain


    if ( gr%nz < 3 ) then
      cubic_interpolated_azmk = linear_interpolated_azmk( azt, k )
      return
    end if
    ! k levels are based on Tak's find_indices subroutine -dschanen 24 Oct 2011

    if ( k == gr%nz-1 ) then
      km1 = gr%nz-2
      kp1 = gr%nz
      kp2 = gr%nz
      k00 = gr%nz-1
    else if ( k == gr%nz ) then ! Extrapolation
      km1 = gr%nz
      kp1 = gr%nz
      kp2 = gr%nz
      k00 = gr%nz-1
    else if ( k == 1 ) then
      km1 = 1
      kp1 = 2
      kp2 = 3
      k00 = 1
    else
      km1 = k-1
      kp1 = k+1
      kp2 = k+2
      k00 = k
    end if
    ! Do the actual interpolation.
    ! Use a cubic monotonic spline interpolation.

    cubic_interpolated_azmk = &
      mono_cubic_interp( gr%zm(k), km1, k00, kp1, kp2, &
                         gr%zt(km1), gr%zt(k00), gr%zt(kp1), gr%zt(kp2), &
                         azt(km1), azt(k00), azt(kp1), azt(kp2) )

    return

  end function cubic_interpolated_azmk
  !=============================================================================

  pure function interpolated_azmk_imp( m_lev ) & 
    result( azt_weight )
    ! Description:
    ! Function used to help in an interpolation of a variable (var_zt) located
    ! on the thermodynamic grid levels (azt) to the momentum grid levels (azm).
    ! This function computes a weighting factor for both the upper thermodynamic
    ! level (k+1) and the lower thermodynamic level (k) applied to the central
    ! momentum level (k).  For the uppermost momentum grid level (k=gr%nz), a
    ! weighting factor for both the thermodynamic level at gr%nz and the
    ! thermodynamic level at gr%nz-1 are calculated based on the use of a
    ! linear extension.  This function outputs the weighting factors at a single
    ! momentum grid level (k).  The formulation used is compatible with a
    ! stretched (unevenly-spaced) grid.  The weights are defined as follows:
    ! ---var_zt(k+1)------------------------------------------- t(k+1)
    !                       azt_weight(t_above) = factor
    ! ===========var_zt(interp)================================ m(k)
    !                       azt_weight(t_below) = 1 - factor
    ! ---var_zt(k)--------------------------------------------- t(k)
    ! The vertical indices t(k+1), m(k), and t(k) correspond with altitudes
    ! zt(k+1), zm(k), and zt(k), respectively.  The letter "t" is used for
    ! thermodynamic levels and the letter "m" is used for momentum levels.
    ! For all levels k < gr%nz:
    ! The formula for a linear interpolation is given by:
    ! var_zt( interp to zm(k) )
    ! = [ ( var_zt(k+1) - var_zt(k) ) / ( zt(k+1) - zt(k) ) ]
    !     * ( zm(k) - zt(k) ) + var_zt(k);
    ! which can be rewritten as:
    ! var_zt( interp to zm(k) )
    ! = [ ( zm(k) - zt(k) ) / ( zt(k+1) - zt(k) ) ]
    !     * ( var_zt(k+1) - var_zt(k) ) + var_zt(k).
    ! Furthermore, the formula can be rewritten as:
    ! var_zt( interp to zm(k) )
    ! = factor * var_zt(k+1) + ( 1 - factor ) * var_zt(k);
    ! where:
    ! factor = ( zm(k) - zt(k) ) / ( zt(k+1) - zt(k) ).
    ! One of the important uses of this function is in situations where the
    ! variable to be interpolated is being treated IMPLICITLY in an equation.
    ! Usually, the variable to be interpolated is involved in a derivative (such
    ! as d(var_zt)/dz in the diagram below).  For the term of the equation
    ! containing the derivative, grid weights are needed for two interpolations,
    ! rather than just one interpolation.  Thus, four grid weights (labeled
    ! A(k), B(k), C(k), and D(k) in the diagram below) are needed.
    ! ---var_zt(k+1)------------------------------------------- t(k+1)
    !                                       A(k)
    ! ===========var_zt(interp)================================ m(k)
    !                                       B(k) = 1 - A(k)
    ! ---var_zt(k)-----------d(var_zt)/dz---------------------- t(k)
    !                                       C(k)
    ! ===========var_zt(interp)================================ m(k-1)
    !                                       D(k) = 1 - C(k)
    ! ---var_zt(k-1)------------------------------------------- t(k-1)
    ! The vertical indices t(k+1), m(k), t(k), m(k-1), and t(k-1) correspond
    ! with altitudes zt(k+1), zm(k), zt(k), zm(k-1), and zt(k-1), respectively.
    ! The letter "t" is used for thermodynamic levels and the letter "m" is used
    ! for momentum levels.
    ! The grid weights, indexed around the central thermodynamic level (k), are
    ! defined as follows:
    ! A(k) = ( zm(k) - zt(k) ) / ( zt(k+1) - zt(k) );
    ! which is the same as "factor" for the interpolation to momentum
    ! level (k).  In the code, this interpolation is referenced as
    ! gr%weights_zt2zm(t_above,mk), which can be read as "grid weight in a zt2zm
    ! interpolation of the thermodynamic level above momentum level (k) (applied
    ! to momentum level (k))".
    ! B(k) = 1 - [ ( zm(k) - zt(k) ) / ( zt(k+1) - zt(k) ) ]
    !      = 1 - A(k);
    ! which is the same as "1 - factor" for the interpolation to momentum
    ! level (k).  In the code, this interpolation is referenced as
    ! gr%weights_zt2zm(t_below,mk), which can be read as "grid weight in a zt2zm
    ! interpolation of the thermodynamic level below momentum level (k) (applied
    ! to momentum level (k))".
    ! C(k) = ( zm(k-1) - zt(k-1) ) / ( zt(k) - zt(k-1) );
    ! which is the same as "factor" for the interpolation to momentum
    ! level (k-1).  In the code, this interpolation is referenced as
    ! gr%weights_zt2zm(t_above,mkm1), which can be read as "grid weight in a
    ! zt2zm interpolation of the thermodynamic level above momentum level (k-1)
    ! (applied to momentum level (k-1))".
    ! D(k) = 1 - [ ( zm(k-1) - zt(k-1) ) / ( zt(k) - zt(k-1) ) ]
    !      = 1 - C(k);
    ! which is the same as "1 - factor" for the interpolation to momentum
    ! level (k-1).  In the code, this interpolation is referenced as
    ! gr%weights_zt2zm(t_below,mkm1), which can be read as "grid weight in a
    ! zt2zm interpolation of the thermodynamic level below momentum level (k-1)
    ! (applied to momentum level (k-1))".
    ! Additionally, as long as the central thermodynamic level (k) in the above
    ! scenario is not the uppermost thermodynamic level or the lowermost
    ! thermodynamic level (k /= gr%nz and k /= 1), the four weighting factors
    ! have the following relationships:  A(k) = C(k+1) and B(k) = D(k+1).
    ! Special condition for uppermost grid level, k = gr%nz:
    ! The uppermost momentum grid level is above the uppermost thermodynamic
    ! grid level.  Thus, a linear extension is used at this level.
    ! For level k = gr%nz:
    ! The formula for a linear extension is given by:
    ! var_zt( extend to zm(k) )
    ! = [ ( var_zt(k) - var_zt(k-1) ) / ( zt(k) - zt(k-1) ) ]
    !     * ( zm(k) - zt(k-1) ) + var_zt(k-1);
    ! which can be rewritten as:
    ! var_zt( extend to zm(k) )
    ! = [ ( zm(k) - zt(k-1) ) / ( zt(k) - zt(k-1) ) ]
    !     * ( var_zt(k) - var_zt(k-1) ) + var_zt(k-1).
    ! Furthermore, the formula can be rewritten as:
    ! var_zt( extend to zm(k) )
    ! = factor * var_zt(k) + ( 1 - factor ) * var_zt(k-1);
    ! where:
    ! factor = ( zm(k) - zt(k-1) ) / ( zt(k) - zt(k-1) ).
    ! Due to the fact that a linear extension is being used, the value of factor
    ! will be greater than 1.  The weight of thermodynamic level k = gr%nz on
    ! momentum level k = gr%nz equals the value of factor.  The weight of
    ! thermodynamic level k = gr%nz-1 on momentum level k = gr%nz equals
    ! 1 - factor, which is less than 0.  However, the sum of the two weights
    ! equals 1.
    ! Brian Griffin; September 12, 2008.
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

      USE constants_clubb, ONLY: one 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Constant parameters

    integer, parameter :: & 
      t_above = 1,  & ! Upper thermodynamic level.
      t_below = 2     ! Lower thermodynamic level.
    ! Input Variable

    integer, intent(in) :: m_lev  ! Momentum level index
    ! Output Variable

    real( kind = core_rknd ), dimension(2) :: &
      azt_weight  ! Weights of the thermodynamic levels.
    ! Local Variables

    real( kind = core_rknd ) :: factor

    integer :: k
    ! Compute the weighting factors at momentum level k.


    k = m_lev

    if ( k /= gr%nz ) then
       ! At most levels, the momentum level is found in-between two
       ! thermodynamic levels.  Linear interpolation is used.
       factor = ( gr%zm(k) - gr%zt(k) ) / ( gr%zt(k+1) - gr%zt(k) )
    else
       ! The top model level (gr%nz) is formulated differently because the top
       ! momentum level is above the top thermodynamic level.  A linear
       ! extension is required, rather than linear interpolation.
       ! Note:  Variable "factor" will be greater than 1 in this situation.
       factor &
       = ( gr%zm(gr%nz) - gr%zt(gr%nz-1) ) / ( gr%zt(gr%nz) - gr%zt(gr%nz-1) )
    endif
    ! Weight of upper thermodynamic level on momentum level.

    azt_weight(t_above) = factor
    ! Weight of lower thermodynamic level on momentum level.
    azt_weight(t_below) = one - factor


    return

  end function interpolated_azmk_imp
  !=============================================================================

  pure function linear_interpolated_azt( azm )
    ! Description:
    ! Function to interpolate a variable located on the momentum grid levels
    ! (azm) to the thermodynamic grid levels (azt).  This function inputs the
    ! entire azm array and outputs the results as an azt array.  The formulation
    ! used is compatible with a stretched (unevenly-spaced) grid.
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE interpolation, ONLY: linear_interp_factor 

    implicit none
    ! Input Variable

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azm    ! Variable on momentum grid levels    [units vary]
    ! Output Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      linear_interpolated_azt    ! Variable when interp. to thermodynamic levels
    ! Local Variable

    integer :: k  ! Grid level loop index
    ! Set the value of the momentum-level variable, azm, at the lowermost level
    ! of the model (below the model lower boundary), which is a thermodynamic
    ! level.  The name of the variable when interpolated/extended to
    ! thermodynamic levels is azt.


    k = 1
!    ! Set the value of azt at level 1 (the lowermost level in the model) to the
!    ! value of azm at level 1.
!    linear_interpolated_azt(k) = azm(k)
    ! Use a linear extension based on the values of azm at levels 1 and 2 to
    ! find the value of azt at level 1 (the lowermost level in the model).
    linear_interpolated_azt(k) &
    = ( ( azm(k+1) - azm(k) ) / ( gr%zm(k+1) - gr%zm(k) ) ) & 
      * ( gr%zt(k) - gr%zm(k) ) + azm(k)
    ! Interpolate the value of a momentum-level variable to the central
    ! thermodynamic level, k, between two successive momentum levels using
    ! linear interpolation.

    forall( k = gr%nz : 2 : -1 )
       linear_interpolated_azt(k) &
       = linear_interp_factor( gr%weights_zm2zt(1, k), azm(k), azm(k-1) )
    end forall ! k = gr%nz : 2 : -1


    return

  end function linear_interpolated_azt
  !=============================================================================

  pure function linear_interpolated_aztk( azm, k )
    ! Description:
    ! Function to interpolate a variable located on the momentum grid levels
    ! (azm) to the thermodynamic grid levels (azt).  This function outputs the
    ! value of azt at a single grid level (k) after interpolating using values
    ! of azm at two grid levels.  The formulation used is compatible with a
    ! stretched (unevenly-spaced) grid.
    !-----------------------------------------------------------------------


      USE clubb_precision, ONLY: core_rknd 

      USE interpolation, ONLY: linear_interp_factor 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azm    ! Variable on momentum grid levels    [units vary]

    integer, intent(in) :: &
      k    ! Vertical level index
    ! Return Variables

    real( kind = core_rknd ) :: &
      linear_interpolated_aztk    ! Variable when interp. to thermodynamic levs.
    ! Interpolate the value of a momentum-level variable to the central
    ! thermodynamic level, k, between two successive momentum levels using
    ! linear interpolation.


    if ( k /= 1 ) then

       linear_interpolated_aztk &
       = linear_interp_factor( gr%weights_zm2zt(1, k), azm(k), azm(k-1) )

    else
!       ! Set the value of azt at level 1 (the lowermost level in the model) to
!       ! the value of azm at level 1.
!       linear_interpolated_aztk = azm(1)
       ! Use a linear extension based on the values of azm at levels 1 and 2 to
       ! find the value of azt at level 1 (the lowermost level in the model).

       linear_interpolated_aztk &
       = ( ( azm(2) - azm(1) ) / ( gr%zm(2) - gr%zm(1) ) ) & 
         * ( gr%zt(1) - gr%zm(1) ) + azm(1)

    endif


    return

  end function linear_interpolated_aztk
  !=============================================================================

  function cubic_interpolated_azt( azm )
    ! Description:
    !   Function to interpolate a variable located on the momentum grid
    !   levels (azm) to the thermodynamic grid levels (azt).  This function outputs the
    !   value of azt at a all grid levels using Steffen's monotonic cubic
    !   interpolation implemented by Tak Yamaguchi.
    ! References:
    !   None
    !-----------------------------------------------------------------------

    ! 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azm
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      cubic_interpolated_azt
    ! Local Variable(s)

    real( kind = core_rknd ), dimension(gr%nz) :: &
      tmp ! This is needed for variables that self-reference
    integer :: &
      k
    ! ---- Begin Code ----


    do k = 1, gr%nz 
      tmp(k) = cubic_interpolated_aztk( azm, k )
    end do

    cubic_interpolated_azt = tmp

    return

  end function cubic_interpolated_azt
  !=============================================================================


  function cubic_interpolated_aztk( azm, k )
    ! Description:
    !   Function to interpolate a variable located on the momentum grid
    !   levels (azm) to the thermodynamic grid levels (azt).  This function outputs the
    !   value of azt at a single grid level (k) using Steffen's monotonic cubic
    !   interpolation implemented by Tak Yamaguchi.
    ! References:
    !   None
    !-----------------------------------------------------------------------

    !

      USE interpolation, ONLY: mono_cubic_interp 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variables

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: azm

    integer, intent(in) :: k
    ! Return Variable

    real( kind = core_rknd ) :: cubic_interpolated_aztk
    ! Local Variable(s)

    integer :: km1, k00, kp1, kp2
    ! ---- Begin Code ----
    ! Special case for a very small domain


    if ( gr%nz < 3 ) then
      cubic_interpolated_aztk = linear_interpolated_aztk( azm, k )
      return
    end if
    ! k levels are based on Tak's find_indices subroutine -dschanen 24 Oct 2011

    if ( k == gr%nz ) then
      km1 = gr%nz-2
      kp1 = gr%nz
      kp2 = gr%nz
      k00 = gr%nz-1
    else if ( k == 2 ) then
      km1 = 1
      kp1 = 2
      kp2 = 3
      k00 = 1
    else if ( k == 1 ) then ! Extrapolation for the ghost point
      km1 = gr%nz
      k00 = 1
      kp1 = 2
      kp2 = 3
    else
      km1 = k-2
      kp1 = k
      kp2 = k+1
      k00 = k-1
    end if
    ! Do the actual interpolation.
    ! Use a cubic monotonic spline interpolation.
    cubic_interpolated_aztk = &
      mono_cubic_interp( gr%zt(k), km1, k00, kp1, kp2, &
                         gr%zm(km1), gr%zm(k00), gr%zm(kp1), gr%zm(kp2), &
                         azm(km1), azm(k00), azm(kp1), azm(kp2) )

    return

  end function cubic_interpolated_aztk
  !=============================================================================

  pure function interpolated_aztk_imp( t_lev ) & 
    result( azm_weight )
    ! Description:
    ! Function used to help in an interpolation of a variable (var_zm) located
    ! on the momentum grid levels (azm) to the thermodynamic grid levels (azt).
    ! This function computes a weighting factor for both the upper momentum
    ! level (k) and the lower momentum level (k-1) applied to the central
    ! thermodynamic level (k).  For the lowermost thermodynamic grid
    ! level (k=1), a weighting factor for both the momentum level at 1 and the
    ! momentum level at 2 are calculated based on the use of a linear extension.
    ! This function outputs the weighting factors at a single thermodynamic grid
    ! level (k).   The formulation used is compatible with a stretched
    ! (unevenly-spaced) grid.  The weights are defined as follows:
    ! ===var_zm(k)============================================= m(k)
    !                       azm_weight(m_above) = factor
    ! -----------var_zm(interp)-------------------------------- t(k)
    !                       azm_weight(m_below) = 1 - factor
    ! ===var_zm(k-1)=========================================== m(k-1)
    ! The vertical indices m(k), t(k), and m(k-1) correspond with altitudes
    ! zm(k), zt(k), and zm(k-1), respectively.  The letter "t" is used for
    ! thermodynamic levels and the letter "m" is used for momentum levels.
    ! For all levels k > 1:
    ! The formula for a linear interpolation is given by:
    ! var_zm( interp to zt(k) )
    ! = [ ( var_zm(k) - var_zm(k-1) ) / ( zm(k) - zm(k-1) ) ]
    !     * ( zt(k) - zm(k-1) ) + var_zm(k-1);
    ! which can be rewritten as:
    ! var_zm( interp to zt(k) )
    ! = [ ( zt(k) - zm(k-1) ) / ( zm(k) - zm(k-1) ) ]
    !     * ( var_zm(k) - var_zm(k-1) ) + var_zm(k-1).
    ! Furthermore, the formula can be rewritten as:
    ! var_zm( interp to zt(k) )
    ! = factor * var_zm(k) + ( 1 - factor ) * var_zm(k-1);
    ! where:
    ! factor = ( zt(k) - zm(k-1) ) / ( zm(k) - zm(k-1) ).
    ! One of the important uses of this function is in situations where the
    ! variable to be interpolated is being treated IMPLICITLY in an equation.
    ! Usually, the variable to be interpolated is involved in a derivative (such
    ! as d(var_zm)/dz in the diagram below).  For the term of the equation
    ! containing the derivative, grid weights are needed for two interpolations,
    ! rather than just one interpolation.  Thus, four grid weights (labeled
    ! A(k), B(k), C(k), and D(k) in the diagram below) are needed.
    ! ===var_zm(k+1)=========================================== m(k+1)
    !                                       A(k)
    ! -----------var_zm(interp)-------------------------------- t(k+1)
    !                                       B(k) = 1 - A(k)
    ! ===var_zm(k)===========d(var_zm)/dz====================== m(k)
    !                                       C(k)
    ! -----------var_zm(interp)-------------------------------- t(k)
    !                                       D(k) = 1 - C(k)
    ! ===var_zm(k-1)=========================================== m(k-1)
    ! The vertical indices m(k+1), t(k+1), m(k), t(k), and m(k-1) correspond
    ! with altitudes zm(k+1), zt(k+1), zm(k), zt(k), and zm(k-1), respectively.
    ! The letter "t" is used for thermodynamic levels and the letter "m" is used
    ! for momentum levels.
    ! The grid weights, indexed around the central momentum level (k), are
    ! defined as follows:
    ! A(k) = ( zt(k+1) - zm(k) ) / ( zm(k+1) - zm(k) );
    ! which is the same as "factor" for the interpolation to thermodynamic
    ! level (k+1).  In the code, this interpolation is referenced as
    ! gr%weights_zm2zt(m_above,tkp1), which can be read as "grid weight in a
    ! zm2zt interpolation of the momentum level above thermodynamic
    ! level (k+1) (applied to thermodynamic level (k+1))".
    ! B(k) = 1 - [ ( zt(k+1) - zm(k) ) / ( zm(k+1) - zm(k) ) ]
    !      = 1 - A(k);
    ! which is the same as "1 - factor" for the interpolation to thermodynamic
    ! level (k+1).  In the code, this interpolation is referenced as
    ! gr%weights_zm2zt(m_below,tkp1), which can be read as "grid weight in a
    ! zm2zt interpolation of the momentum level below thermodynamic
    ! level (k+1) (applied to thermodynamic level (k+1))".
    ! C(k) = ( zt(k) - zm(k-1) ) / ( zm(k) - zm(k-1) );
    ! which is the same as "factor" for the interpolation to thermodynamic
    ! level (k).  In the code, this interpolation is referenced as
    ! gr%weights_zm2zt(m_above,tk), which can be read as "grid weight in a zm2zt
    ! interpolation of the momentum level above thermodynamic level (k) (applied
    ! to thermodynamic level (k))".
    ! D(k) = 1 - [ ( zt(k) - zm(k-1) ) / ( zm(k) - zm(k-1) ) ]
    !      = 1 - C(k);
    ! which is the same as "1 - factor" for the interpolation to thermodynamic
    ! level (k).  In the code, this interpolation is referenced as
    ! gr%weights_zm2zt(m_below,tk), which can be read as "grid weight in a zm2zt
    ! interpolation of the momentum level below thermodynamic level (k) (applied
    ! to thermodynamic level (k))".
    ! Additionally, as long as the central momentum level (k) in the above
    ! scenario is not the lowermost momentum level or the uppermost momentum
    ! level (k /= 1 and k /= gr%nz), the four weighting factors have the
    ! following relationships:  A(k) = C(k+1) and B(k) = D(k+1).
    ! Special condition for lowermost grid level, k = 1:
    ! The lowermost thermodynamic grid level is below the lowermost momentum
    ! grid level.  Thus, a linear extension is used at this level.  It should
    ! be noted that the thermodynamic level k = 1 is considered to be below the
    ! model lower boundary, which is defined to be at momentum level k = 1.
    ! Thus, the values of most variables at thermodynamic level k = 1 are not
    ! often needed or referenced.
    ! For level k = 1:
    ! The formula for a linear extension is given by:
    ! var_zm( extend to zt(k) )
    ! = [ ( var_zm(k+1) - var_zm(k) ) / ( zm(k+1) - zm(k) ) ]
    !     * ( zt(k) - zm(k) ) + var_zm(k);
    ! which can be rewritten as:
    ! var_zm( extend to zt(k) )
    ! = [ ( zt(k) - zm(k) ) / ( zm(k+1) - zm(k) ) ]
    !     * ( var_zm(k+1) - var_zm(k) ) + var_zm(k).
    ! Furthermore, the formula can be rewritten as:
    ! var_zm( extend to zt(k) )
    ! = factor * var_zm(k+1) + ( 1 - factor ) * var_zm(k);
    ! where:
    ! factor = ( zt(k) - zm(k) ) / ( zm(k+1) - zm(k) ).
    ! Due to the fact that a linear extension is being used, the value of factor
    ! will be less than 0.  The weight of the upper momentum level, which is
    ! momentum level k = 2, on thermodynamic level k = 1 equals the value of
    ! factor.  The weight of the lower momentum level, which is momentum level
    ! k = 1, on thermodynamic level k = 1 equals 1 - factor, which is greater
    ! than 1.  However, the sum of the weights equals 1.
    ! Brian Griffin; September 12, 2008.
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

      USE constants_clubb, ONLY: one 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Constant parameters

    integer, parameter :: & 
      m_above = 1,  & ! Upper momentum level.
      m_below = 2     ! Lower momentum level.
    ! Input Variable

    integer, intent(in) :: t_lev  ! Thermodynamic level index.
    ! Output Variable

    real( kind = core_rknd ), dimension(2) :: &
      azm_weight  ! Weights of the momentum levels.
    ! Local Variables

    real( kind = core_rknd ) :: factor

    integer :: k
    ! Compute the weighting factors at thermodynamic level k.


    k = t_lev

    if ( k /= 1 ) then
       ! At most levels, the thermodynamic level is found in-between two
       ! momentum levels.  Linear interpolation is used.
       factor = ( gr%zt(k) - gr%zm(k-1) ) / ( gr%zm(k) - gr%zm(k-1) )
    else
       ! The bottom model level (1) is formulated differently because the bottom
       ! thermodynamic level is below the bottom momentum level.  A linear
       ! extension is required, rather than linear interpolation.
       ! Note:  Variable "factor" will have a negative sign in this situation.
       factor = ( gr%zt(1) - gr%zm(1) ) / ( gr%zm(2) - gr%zm(1) )
    endif
    ! Weight of upper momentum level on thermodynamic level.

    azm_weight(m_above) = factor
    ! Weight of lower momentum level on thermodynamic level.
    azm_weight(m_below) = one - factor


    return

  end function interpolated_aztk_imp
  !=============================================================================

  pure function gradzm( azm )
    ! Description:
    ! Function to compute the vertical derivative of a variable (azm) located on
    ! the momentum grid.  The results are returned in an array defined on the
    ! thermodynamic grid.
    !-----------------------------------------------------------------------
!    use constants_clubb, only: &
!        zero  ! Constant(s)


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variable

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azm    ! Variable on momentum grid levels    [units vary]
    ! Return Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      gradzm    ! Vertical derivative of azm    [units vary / m]
    ! Local Variable

    integer :: k  ! Grid level loop index
    ! Set the value of the vertical derivative of a momentum-level variable over
    ! the thermodynamic grid level at the lowermost level of the model.


    k = 1
!    ! Thermodynamic level 1 is located below momentum level 1, so there is not
!    ! enough information to calculate the derivative over thermodynamic
!    ! level 1.  Thus, the value of the derivative at thermodynamic level 1 is
!    ! set equal to 0.  This formulation is consistent with setting the value of
!    ! the variable azm below the model grid to the value of the variable azm at
!    ! the lowest grid level.
!    gradzm(k) = zero
    ! Thermodynamic level 1 is located below momentum level 1, so there is not
    ! enough information to calculate the derivative over thermodynamic level 1.
    ! Thus, the value of the derivative at thermodynamic level 1 is set equal to
    ! the value of the derivative at thermodynamic level 2.  This formulation is
    ! consistent with using a linear extension to find the values of the
    ! variable azm below the model grid.
    gradzm(k) = ( azm(k+1) - azm(k) ) * gr%invrs_dzt(k+1)
    ! Calculate the vertical derivative of a momentum-level variable between two
    ! successive momentum grid levels.

    forall( k = gr%nz : 2 : -1 )
       ! Take derivative of momentum-level variable azm over the central
       ! thermodynamic level (k).
       gradzm(k) = ( azm(k) - azm(k-1) ) * gr%invrs_dzt(k)
    end forall ! k = gr%nz : 2 : -1


    return

  end function gradzm
  !=============================================================================

  pure function gradzt( azt )
    ! Description:
    ! Function to compute the vertical derivative of a variable (azt) located on
    ! the thermodynamic grid.  The results are returned in an array defined on
    ! the momentum grid.
    !-----------------------------------------------------------------------
!    use constants_clubb, only: &
!        zero  ! Constant(s)


      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variable

    real( kind = core_rknd ), intent(in), dimension(gr%nz) :: &
      azt    ! Variable on thermodynamic grid levels    [units vary]
    ! Output Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      gradzt    ! Vertical derivative of azt    [units vary / m]
    ! Local Variable

    integer :: k  ! Grid level loop index
    ! Set the value of the vertical derivative of a thermodynamic-level variable
    ! over the momentum grid level at the uppermost level of the model.


    k = gr%nz
!    ! Momentum level gr%nz is located above thermodynamic level gr%nz, so
!    ! there is not enough information to calculate the derivative over momentum
!    ! level gr%nz.  Thus, the value of the derivative at momentum level
!    ! gr%nz is set equal to 0.  This formulation is consistent with setting
!    ! the value of the variable azt above the model grid to the value of the
!    ! variable azt at the highest grid level.
!    gradzt(k) = zero
    ! Momentum level gr%nz is located above thermodynamic level gr%nz, so
    ! there is not enough information to calculate the derivative over momentum
    ! level gr%nz.  Thus, the value of the derivative at momentum level
    ! gr%nz is set equal to the value of the derivative at momentum level
    ! gr%nz-1.  This formulation is consistent with using a linear extension
    ! to find the values of the variable azt above the model grid.
    gradzt(k) = ( azt(k) - azt(k-1) ) * gr%invrs_dzm(k-1)
    ! Calculate the vertical derivative of a thermodynamic-level variable
    ! between two successive thermodynamic grid levels.

    forall( k = 1 : gr%nz-1 : 1 )
       ! Take derivative of thermodynamic-level variable azt over the central
       ! momentum level (k).
       gradzt(k) = ( azt(k+1) - azt(k) ) * gr%invrs_dzm(k)
    end forall ! k = 1 : gr%nz-1 : 1


    return

  end function gradzt
  !=============================================================================


!===============================================================================


  !read state subroutine for kr_externs_in_grid_class 
  SUBROUTINE kr_externs_in_grid_class(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_grid_class_grid(gr, kgen_unit, "gr", .FALSE.) 
  END SUBROUTINE kr_externs_in_grid_class 
    
  !read state subroutine for kr_externs_out_grid_class 
  SUBROUTINE kr_externs_out_grid_class(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      CALL kr_grid_class_grid(kgenref_gr, kgen_unit, "kgenref_gr", .FALSE.) 
  END SUBROUTINE kr_externs_out_grid_class 
    
  !verify state subroutine for kv_externs_grid_class 
  SUBROUTINE kv_externs_grid_class(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_grid_class_grid("gr", check_status, gr, kgenref_gr) 
  END SUBROUTINE kv_externs_grid_class 
    
  !read state subroutine for kr_grid_class_grid 
  RECURSIVE SUBROUTINE kr_grid_class_grid(var, kgen_unit, printname, printvar) 
      TYPE(grid), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%nz 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%nz = ", var%nz 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%zm, kgen_unit, printname // "%zm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%zm, kgen_unit, printname // "%zm", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%zt, kgen_unit, printname // "%zt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%zt, kgen_unit, printname // "%zt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzm, kgen_unit, printname // "%invrs_dzm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzm, kgen_unit, printname // "%invrs_dzm", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzt, kgen_unit, printname // "%invrs_dzt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%invrs_dzt, kgen_unit, printname // "%invrs_dzt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%dzm, kgen_unit, printname // "%dzm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%dzm, kgen_unit, printname // "%dzm", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim1(var%dzt, kgen_unit, printname // "%dzt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim1(var%dzt, kgen_unit, printname // "%dzt", .FALSE.) 
      END IF   
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zm2zt, kgen_unit, printname // "%weights_zm2zt", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zm2zt, kgen_unit, printname // "%weights_zm2zt", .FALSE.) 
      END IF   
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zt2zm, kgen_unit, printname // "%weights_zt2zm", .TRUE.) 
      ELSE 
          CALL kr_grid_real__core_rknd_dim2(var%weights_zt2zm, kgen_unit, printname // "%weights_zt2zm", .FALSE.) 
      END IF   
        
  END SUBROUTINE kr_grid_class_grid 
    
  !write state subroutine for kr_grid_real__core_rknd_dim1 
  SUBROUTINE kr_grid_real__core_rknd_dim1(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1 
      INTEGER, DIMENSION(2,1) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_grid_real__core_rknd_dim1 
    
  !write state subroutine for kr_grid_real__core_rknd_dim2 
  SUBROUTINE kr_grid_real__core_rknd_dim2(var, kgen_unit, printname, printvar) 
      REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
        
  END SUBROUTINE kr_grid_real__core_rknd_dim2 
    
  !verify state subroutine for kv_grid_class_grid 
  RECURSIVE SUBROUTINE kv_grid_class_grid(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(grid), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff_nz 
      INTEGER :: n_zm 
      real(KIND=core_rknd) :: nrmsdiff_zm, rmsdiff_zm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_zm(:), buf2_zm(:) 
      INTEGER :: n_zt 
      real(KIND=core_rknd) :: nrmsdiff_zt, rmsdiff_zt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_zt(:), buf2_zt(:) 
      INTEGER :: n_invrs_dzm 
      real(KIND=core_rknd) :: nrmsdiff_invrs_dzm, rmsdiff_invrs_dzm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_invrs_dzm(:), buf2_invrs_dzm(:) 
      INTEGER :: n_invrs_dzt 
      real(KIND=core_rknd) :: nrmsdiff_invrs_dzt, rmsdiff_invrs_dzt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_invrs_dzt(:), buf2_invrs_dzt(:) 
      INTEGER :: n_dzm 
      real(KIND=core_rknd) :: nrmsdiff_dzm, rmsdiff_dzm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_dzm(:), buf2_dzm(:) 
      INTEGER :: n_dzt 
      real(KIND=core_rknd) :: nrmsdiff_dzt, rmsdiff_dzt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_dzt(:), buf2_dzt(:) 
      INTEGER :: n_weights_zm2zt 
      real(KIND=core_rknd) :: nrmsdiff_weights_zm2zt, rmsdiff_weights_zm2zt 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_weights_zm2zt(:,:), buf2_weights_zm2zt(:,:) 
      INTEGER :: n_weights_zt2zm 
      real(KIND=core_rknd) :: nrmsdiff_weights_zt2zm, rmsdiff_weights_zt2zm 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_weights_zt2zm(:,:), buf2_weights_zt2zm(:,:) 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%nz == kgenref_var%nz) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%nz is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_nz = ABS(var%nz - kgenref_var%nz) 
          IF (diff_nz <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nz is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%nz is NOT IDENTICAL(out of tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nz 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_nz 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      IF (ALLOCATED(var%zm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%zm == kgenref_var%zm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%zm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_zm(SIZE(var%zm,dim=1))) 
              ALLOCATE (buf2_zm(SIZE(var%zm,dim=1))) 
              n_zm = COUNT(var%zm /= kgenref_var%zm) 
              WHERE ( ABS(kgenref_var%zm) > kgen_minvalue ) 
                  buf1_zm = ((var%zm-kgenref_var%zm)/kgenref_var%zm)**2 
                  buf2_zm = (var%zm-kgenref_var%zm)**2 
              ELSEWHERE 
                  buf1_zm = (var%zm-kgenref_var%zm)**2 
                  buf2_zm = buf1_zm 
              END WHERE   
              nrmsdiff_zm = SQRT(SUM(buf1_zm)/REAL(n_zm)) 
              rmsdiff_zm = SQRT(SUM(buf2_zm)/REAL(n_zm)) 
              IF (rmsdiff_zm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zm is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zm /= kgenref_var%zm), " of ", size( var%zm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zm)/real(size(var%zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zm)/real(size(kgenref_var%zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zm /= kgenref_var%zm), " of ", size( var%zm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zm)/real(size(var%zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zm)/real(size(kgenref_var%zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%zt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%zt == kgenref_var%zt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%zt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_zt(SIZE(var%zt,dim=1))) 
              ALLOCATE (buf2_zt(SIZE(var%zt,dim=1))) 
              n_zt = COUNT(var%zt /= kgenref_var%zt) 
              WHERE ( ABS(kgenref_var%zt) > kgen_minvalue ) 
                  buf1_zt = ((var%zt-kgenref_var%zt)/kgenref_var%zt)**2 
                  buf2_zt = (var%zt-kgenref_var%zt)**2 
              ELSEWHERE 
                  buf1_zt = (var%zt-kgenref_var%zt)**2 
                  buf2_zt = buf1_zt 
              END WHERE   
              nrmsdiff_zt = SQRT(SUM(buf1_zt)/REAL(n_zt)) 
              rmsdiff_zt = SQRT(SUM(buf2_zt)/REAL(n_zt)) 
              IF (rmsdiff_zt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%zt is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zt /= kgenref_var%zt), " of ", size( var%zt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zt)/real(size(var%zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zt)/real(size(kgenref_var%zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%zt /= kgenref_var%zt), " of ", size( var%zt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%zt)/real(size(var%zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%zt)/real(size(kgenref_var%zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%invrs_dzm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%invrs_dzm == kgenref_var%invrs_dzm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%invrs_dzm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_invrs_dzm(SIZE(var%invrs_dzm,dim=1))) 
              ALLOCATE (buf2_invrs_dzm(SIZE(var%invrs_dzm,dim=1))) 
              n_invrs_dzm = COUNT(var%invrs_dzm /= kgenref_var%invrs_dzm) 
              WHERE ( ABS(kgenref_var%invrs_dzm) > kgen_minvalue ) 
                  buf1_invrs_dzm = ((var%invrs_dzm-kgenref_var%invrs_dzm)/kgenref_var%invrs_dzm)**2 
                  buf2_invrs_dzm = (var%invrs_dzm-kgenref_var%invrs_dzm)**2 
              ELSEWHERE 
                  buf1_invrs_dzm = (var%invrs_dzm-kgenref_var%invrs_dzm)**2 
                  buf2_invrs_dzm = buf1_invrs_dzm 
              END WHERE   
              nrmsdiff_invrs_dzm = SQRT(SUM(buf1_invrs_dzm)/REAL(n_invrs_dzm)) 
              rmsdiff_invrs_dzm = SQRT(SUM(buf2_invrs_dzm)/REAL(n_invrs_dzm)) 
              IF (rmsdiff_invrs_dzm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzm is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%invrs_dzm /= kgenref_var%invrs_dzm), " of ", size( var%invrs_dzm ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzm)/real(size(var%invrs_dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzm)/real(size(kgenref_var%invrs_dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%invrs_dzm /= kgenref_var%invrs_dzm), " of ", size( var%invrs_dzm ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzm)/real(size(var%invrs_dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzm)/real(size(kgenref_var%invrs_dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%invrs_dzt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%invrs_dzt == kgenref_var%invrs_dzt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%invrs_dzt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_invrs_dzt(SIZE(var%invrs_dzt,dim=1))) 
              ALLOCATE (buf2_invrs_dzt(SIZE(var%invrs_dzt,dim=1))) 
              n_invrs_dzt = COUNT(var%invrs_dzt /= kgenref_var%invrs_dzt) 
              WHERE ( ABS(kgenref_var%invrs_dzt) > kgen_minvalue ) 
                  buf1_invrs_dzt = ((var%invrs_dzt-kgenref_var%invrs_dzt)/kgenref_var%invrs_dzt)**2 
                  buf2_invrs_dzt = (var%invrs_dzt-kgenref_var%invrs_dzt)**2 
              ELSEWHERE 
                  buf1_invrs_dzt = (var%invrs_dzt-kgenref_var%invrs_dzt)**2 
                  buf2_invrs_dzt = buf1_invrs_dzt 
              END WHERE   
              nrmsdiff_invrs_dzt = SQRT(SUM(buf1_invrs_dzt)/REAL(n_invrs_dzt)) 
              rmsdiff_invrs_dzt = SQRT(SUM(buf2_invrs_dzt)/REAL(n_invrs_dzt)) 
              IF (rmsdiff_invrs_dzt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%invrs_dzt is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%invrs_dzt /= kgenref_var%invrs_dzt), " of ", size( var%invrs_dzt ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzt)/real(size(var%invrs_dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzt)/real(size(kgenref_var%invrs_dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%invrs_dzt /= kgenref_var%invrs_dzt), " of ", size( var%invrs_dzt ), " elements are &
                      &different." 
                      WRITE (*, *) "Average - kernel ", sum(var%invrs_dzt)/real(size(var%invrs_dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%invrs_dzt)/real(size(kgenref_var%invrs_dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_invrs_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_invrs_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%dzm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dzm == kgenref_var%dzm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dzm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dzm(SIZE(var%dzm,dim=1))) 
              ALLOCATE (buf2_dzm(SIZE(var%dzm,dim=1))) 
              n_dzm = COUNT(var%dzm /= kgenref_var%dzm) 
              WHERE ( ABS(kgenref_var%dzm) > kgen_minvalue ) 
                  buf1_dzm = ((var%dzm-kgenref_var%dzm)/kgenref_var%dzm)**2 
                  buf2_dzm = (var%dzm-kgenref_var%dzm)**2 
              ELSEWHERE 
                  buf1_dzm = (var%dzm-kgenref_var%dzm)**2 
                  buf2_dzm = buf1_dzm 
              END WHERE   
              nrmsdiff_dzm = SQRT(SUM(buf1_dzm)/REAL(n_dzm)) 
              rmsdiff_dzm = SQRT(SUM(buf2_dzm)/REAL(n_dzm)) 
              IF (rmsdiff_dzm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzm is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dzm /= kgenref_var%dzm), " of ", size( var%dzm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzm)/real(size(var%dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzm)/real(size(kgenref_var%dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dzm /= kgenref_var%dzm), " of ", size( var%dzm ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzm)/real(size(var%dzm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzm)/real(size(kgenref_var%dzm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%dzt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%dzt == kgenref_var%dzt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%dzt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_dzt(SIZE(var%dzt,dim=1))) 
              ALLOCATE (buf2_dzt(SIZE(var%dzt,dim=1))) 
              n_dzt = COUNT(var%dzt /= kgenref_var%dzt) 
              WHERE ( ABS(kgenref_var%dzt) > kgen_minvalue ) 
                  buf1_dzt = ((var%dzt-kgenref_var%dzt)/kgenref_var%dzt)**2 
                  buf2_dzt = (var%dzt-kgenref_var%dzt)**2 
              ELSEWHERE 
                  buf1_dzt = (var%dzt-kgenref_var%dzt)**2 
                  buf2_dzt = buf1_dzt 
              END WHERE   
              nrmsdiff_dzt = SQRT(SUM(buf1_dzt)/REAL(n_dzt)) 
              rmsdiff_dzt = SQRT(SUM(buf2_dzt)/REAL(n_dzt)) 
              IF (rmsdiff_dzt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%dzt is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dzt /= kgenref_var%dzt), " of ", size( var%dzt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzt)/real(size(var%dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzt)/real(size(kgenref_var%dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%dzt /= kgenref_var%dzt), " of ", size( var%dzt ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%dzt)/real(size(var%dzt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%dzt)/real(size(kgenref_var%dzt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_dzt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_dzt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (ALLOCATED(var%weights_zm2zt)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%weights_zm2zt == kgenref_var%weights_zm2zt)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%weights_zm2zt is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_weights_zm2zt(SIZE(var%weights_zm2zt,dim=1),SIZE(var%weights_zm2zt,dim=2))) 
              ALLOCATE (buf2_weights_zm2zt(SIZE(var%weights_zm2zt,dim=1),SIZE(var%weights_zm2zt,dim=2))) 
              n_weights_zm2zt = COUNT(var%weights_zm2zt /= kgenref_var%weights_zm2zt) 
              WHERE ( ABS(kgenref_var%weights_zm2zt) > kgen_minvalue ) 
                  buf1_weights_zm2zt = ((var%weights_zm2zt-kgenref_var%weights_zm2zt)/kgenref_var%weights_zm2zt)**2 
                  buf2_weights_zm2zt = (var%weights_zm2zt-kgenref_var%weights_zm2zt)**2 
              ELSEWHERE 
                  buf1_weights_zm2zt = (var%weights_zm2zt-kgenref_var%weights_zm2zt)**2 
                  buf2_weights_zm2zt = buf1_weights_zm2zt 
              END WHERE   
              nrmsdiff_weights_zm2zt = SQRT(SUM(buf1_weights_zm2zt)/REAL(n_weights_zm2zt)) 
              rmsdiff_weights_zm2zt = SQRT(SUM(buf2_weights_zm2zt)/REAL(n_weights_zm2zt)) 
              IF (rmsdiff_weights_zm2zt > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zm2zt is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zm2zt is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%weights_zm2zt /= kgenref_var%weights_zm2zt), " of ", size( var%weights_zm2zt ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zm2zt)/real(size(var%weights_zm2zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zm2zt)/real(size(kgenref_var%weights_zm2zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zm2zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zm2zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%weights_zm2zt /= kgenref_var%weights_zm2zt), " of ", size( var%weights_zm2zt ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zm2zt)/real(size(var%weights_zm2zt)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zm2zt)/real(size(kgenref_var%weights_zm2zt)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zm2zt 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zm2zt 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
      END IF   
      IF (ALLOCATED(var%weights_zt2zm)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%weights_zt2zm == kgenref_var%weights_zt2zm)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%weights_zt2zm is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_weights_zt2zm(SIZE(var%weights_zt2zm,dim=1),SIZE(var%weights_zt2zm,dim=2))) 
              ALLOCATE (buf2_weights_zt2zm(SIZE(var%weights_zt2zm,dim=1),SIZE(var%weights_zt2zm,dim=2))) 
              n_weights_zt2zm = COUNT(var%weights_zt2zm /= kgenref_var%weights_zt2zm) 
              WHERE ( ABS(kgenref_var%weights_zt2zm) > kgen_minvalue ) 
                  buf1_weights_zt2zm = ((var%weights_zt2zm-kgenref_var%weights_zt2zm)/kgenref_var%weights_zt2zm)**2 
                  buf2_weights_zt2zm = (var%weights_zt2zm-kgenref_var%weights_zt2zm)**2 
              ELSEWHERE 
                  buf1_weights_zt2zm = (var%weights_zt2zm-kgenref_var%weights_zt2zm)**2 
                  buf2_weights_zt2zm = buf1_weights_zt2zm 
              END WHERE   
              nrmsdiff_weights_zt2zm = SQRT(SUM(buf1_weights_zt2zm)/REAL(n_weights_zt2zm)) 
              rmsdiff_weights_zt2zm = SQRT(SUM(buf2_weights_zt2zm)/REAL(n_weights_zt2zm)) 
              IF (rmsdiff_weights_zt2zm > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zt2zm is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%weights_zt2zm is NOT IDENTICAL(within tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_IN_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              CONTINUE 
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%weights_zt2zm /= kgenref_var%weights_zt2zm), " of ", size( var%weights_zt2zm ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zt2zm)/real(size(var%weights_zt2zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zt2zm)/real(size(kgenref_var%weights_zt2zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zt2zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zt2zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%weights_zt2zm /= kgenref_var%weights_zt2zm), " of ", size( var%weights_zt2zm ), " &
                      &elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%weights_zt2zm)/real(size(var%weights_zt2zm)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var%weights_zt2zm)/real(size(kgenref_var%weights_zt2zm)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_weights_zt2zm 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_weights_zt2zm 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_grid_class_grid 
    
end module grid_class