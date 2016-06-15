!-------------------------------------------------------------------------
!$Id: sponge_layer_damping.F90 7185 2014-08-11 17:45:21Z betlej@uwm.edu $
!===============================================================================
module sponge_layer_damping
! Description:
!   This module is used for damping variables in upper altitudes of the grid.
!
! References:
!   None
!---------------------------------------------------------------------------------------------------

  use clubb_precision, only: &
    core_rknd ! Variable(s)

  implicit none

  public :: sponge_damp_xm, initialize_tau_sponge_damp, finalize_tau_sponge_damp, &
            sponge_damp_settings, sponge_damp_profile


  type sponge_damp_settings

    real( kind = core_rknd ) :: &
      tau_sponge_damp_min, & ! Minimum damping time-scale (at the top) [s]
      tau_sponge_damp_max, & ! Maximum damping time-scale (base of damping layer) [s]
      sponge_damp_depth      ! damping depth as a fraction of domain height [-]

    logical :: &
      l_sponge_damping       ! True if damping is being used

  end type sponge_damp_settings

  type sponge_damp_profile
    real( kind = core_rknd ), pointer, dimension(:) :: &
      tau_sponge_damp ! Damping factor

    integer :: &
      n_sponge_damp ! Number of levels damped

  end type sponge_damp_profile


  type(sponge_damp_settings), public :: &
    thlm_sponge_damp_settings, &
    rtm_sponge_damp_settings, &
    uv_sponge_damp_settings

!$omp threadprivate(thlm_sponge_damp_settings, rtm_sponge_damp_settings, uv_sponge_damp_settings)

  type(sponge_damp_profile), public :: &
    thlm_sponge_damp_profile, &
    rtm_sponge_damp_profile, &
    uv_sponge_damp_profile

!$omp threadprivate(thlm_sponge_damp_profile, rtm_sponge_damp_profile,  uv_sponge_damp_profile)

  private

  PUBLIC kw_externs_in_sponge_layer_damping 
  PUBLIC kw_externs_out_sponge_layer_damping 
  PUBLIC kw_sponge_layer_damping_sponge_damp_settings 
  PUBLIC kw_sponge_layer_damping_sponge_damp_profile 
  contains

  !---------------------------------------------------------------------------------------------
  function sponge_damp_xm( dt, xm_ref, xm, damping_profile ) result( xm_p )
    !
    ! Description:
    !   Damps specified variable. The module must be initialized for
    !   this function to work. Otherwise a stop is issued.
    !
    ! References:
    !   None
    !-------------------------------------------------------------------------------------------

    !  "Sponge"-layer damping at the domain top region

    use grid_class, only: gr ! Variable(s)

    use clubb_precision, only: core_rknd ! Variable(s)

    implicit none

    ! External
    intrinsic :: associated

    ! Input Variable(s)
    real( kind = core_rknd ), intent(in) :: dt ! Model Timestep

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      xm_ref ! Reference to damp to [-]

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      xm ! Variable being damped [-]

    type(sponge_damp_profile), intent(in) :: &
        damping_profile

    ! Output Variable(s)
    real( kind = core_rknd ), dimension(gr%nz) :: xm_p ! Variable damped [-]

    real( kind = core_rknd ) :: dt_on_tau ! Ratio of timestep to damping timescale [-]

    integer :: k

    ! ---- Begin Code ----

    if ( associated( damping_profile%tau_sponge_damp ) ) then

      xm_p = xm

      do k = gr%nz, gr%nz-damping_profile%n_sponge_damp, -1

! Vince Larson used implicit discretization in order to 
! reduce noise in rtm in cloud_feedback_s12 (CGILS) 
!        xm_p(k) = xm(k) - real( ( ( xm(k) - xm_ref(k) ) / & 
!                        damping_profile%tau_sponge_damp(k) ) * dt )
        dt_on_tau = dt / damping_profile%tau_sponge_damp(k)

! Really, we should be using xm_ref at time n+1 rather than n.
! However, for steady profiles of xm_ref, it won't matter.        
        xm_p(k) = ( xm(k) + dt_on_tau * xm_ref(k) ) / &
                        ( 1.0_core_rknd + dt_on_tau )
! End Vince Larson's change
      end do ! k

    else

      stop "tau_sponge_damp in damping used before initialization"

    end if

    return
  end function sponge_damp_xm

  !---------------------------------------------------------------------------------------------
  subroutine initialize_tau_sponge_damp( dt, settings, damping_profile )
    !
    ! Description:
    !   Initialize tau_sponge_damp used for damping
    !
    ! References:
    !   None
    !-------------------------------------------------------------------------------------------
    use clubb_precision, only: core_rknd ! Variable(s)
    
    use constants_clubb, only: fstderr ! Constant(s)

    use grid_class, only: gr ! Variable(s)

    use interpolation, only: lin_interpolate_two_points ! function

    implicit none

    ! Input Variable(s)
    real( kind = core_rknd ), intent(in) :: dt ! Model Timestep [s]

    type(sponge_damp_settings), intent(in) :: &
        settings

    type(sponge_damp_profile), intent(out) :: &
      damping_profile

    integer :: k ! Loop iterator

    ! ---- Begin Code ----

    allocate( damping_profile%tau_sponge_damp(1:gr%nz))

    if( settings%tau_sponge_damp_min < 2._core_rknd * dt ) then
      write(fstderr,*) 'Error: in damping() tau_sponge_damp_min is too small!'
      stop
    end if

    do k=gr%nz,1,-1
      if(gr%zt(gr%nz)-gr%zt(k) < settings%sponge_damp_depth*gr%zt(gr%nz)) then
        damping_profile%n_sponge_damp=gr%nz-k+1
      endif
    end do

    do k=gr%nz,gr%nz-damping_profile%n_sponge_damp,-1
! Vince Larson added code to use standard linear interpolation.
!      damping_profile%tau_sponge_damp(k) = settings%tau_sponge_damp_min *&
!        (settings%tau_sponge_damp_max/settings%tau_sponge_damp_min)** &
!        ( ( gr%zt(gr%nz)-gr%zt(k) ) / &
!          (gr%zt(gr%nz) - gr%zt( gr%nz-damping_profile%n_sponge_damp ) ) )
      damping_profile%tau_sponge_damp(k) =                                     &
        lin_interpolate_two_points( gr%zt(k), gr%zt(gr%nz),                                     &
          gr%zt(gr%nz) - gr%zt( gr%nz-damping_profile%n_sponge_damp ) ,    &
          settings%tau_sponge_damp_min, settings%tau_sponge_damp_max )         
! End Vince Larson's change
    end do

    return
  end subroutine initialize_tau_sponge_damp

  !---------------------------------------------------------------------------------------------
  subroutine finalize_tau_sponge_damp( damping_profile )
    !
    ! Description:
    !   Frees memory allocated in initialize_tau_sponge_damp
    ! 
    ! References:
    !   None
    !-------------------------------------------------------------------------------------------
    implicit none

    ! Input/Output Variable(s)
    type(sponge_damp_profile), intent(inout) :: &
      damping_profile ! Information for damping the profile

    ! ---- Begin Code ----

    deallocate( damping_profile%tau_sponge_damp )

    return
  end subroutine finalize_tau_sponge_damp


  !write in state subroutine for kw_externs_in_sponge_layer_damping 
  SUBROUTINE kw_externs_in_sponge_layer_damping(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kw_sponge_layer_damping_sponge_damp_settings(thlm_sponge_damp_settings, kgen_unit) 
      CALL kw_sponge_layer_damping_sponge_damp_settings(uv_sponge_damp_settings, kgen_unit) 
      CALL kw_sponge_layer_damping_sponge_damp_settings(rtm_sponge_damp_settings, kgen_unit) 
      CALL kw_sponge_layer_damping_sponge_damp_profile(rtm_sponge_damp_profile, kgen_unit) 
      CALL kw_sponge_layer_damping_sponge_damp_profile(thlm_sponge_damp_profile, kgen_unit) 
      CALL kw_sponge_layer_damping_sponge_damp_profile(uv_sponge_damp_profile, kgen_unit) 
  END SUBROUTINE kw_externs_in_sponge_layer_damping 
    
  !write out state subroutine for kw_externs_out_sponge_layer_damping 
  SUBROUTINE kw_externs_out_sponge_layer_damping(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
  END SUBROUTINE kw_externs_out_sponge_layer_damping 
    
  !read state subroutine for kw_sponge_layer_damping_sponge_damp_settings 
  RECURSIVE SUBROUTINE kw_sponge_layer_damping_sponge_damp_settings(var, kgen_unit, printvar) 
      TYPE(sponge_damp_settings), INTENT(IN) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%tau_sponge_damp_min 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%tau_sponge_damp_min **" // NEW_LINE("A"), var%tau_sponge_damp_min 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%tau_sponge_damp_max 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%tau_sponge_damp_max **" // NEW_LINE("A"), var%tau_sponge_damp_max 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%sponge_damp_depth 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%sponge_damp_depth **" // NEW_LINE("A"), var%sponge_damp_depth 
      END IF   
        
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%l_sponge_damping 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%l_sponge_damping **" // NEW_LINE("A"), var%l_sponge_damping 
      END IF   
        
  END SUBROUTINE kw_sponge_layer_damping_sponge_damp_settings 
    
  !read state subroutine for kw_sponge_layer_damping_sponge_damp_profile 
  RECURSIVE SUBROUTINE kw_sponge_layer_damping_sponge_damp_profile(var, kgen_unit, printvar) 
      TYPE(sponge_damp_profile), INTENT(IN) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar )) THEN 
          CALL kw_sponge_damp_profile_real__core_rknd_dim1_ptr(var%tau_sponge_damp, kgen_unit, printvar // "%tau_sponge_damp") 
      ELSE 
          CALL kw_sponge_damp_profile_real__core_rknd_dim1_ptr(var%tau_sponge_damp, kgen_unit) 
      END IF   
        
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%n_sponge_damp 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%n_sponge_damp **" // NEW_LINE("A"), var%n_sponge_damp 
      END IF   
        
  END SUBROUTINE kw_sponge_layer_damping_sponge_damp_profile 
    
  !write state subroutine for kw_sponge_damp_profile_real__core_rknd_dim1_ptr 
  SUBROUTINE kw_sponge_damp_profile_real__core_rknd_dim1_ptr(var, kgen_unit, printvar) 
      REAL(KIND=core_rknd), INTENT(IN), POINTER, DIMENSION(:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      INTEGER :: idx1 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (SIZE(var)==1) THEN 
          IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN 
              kgen_istrue = .FALSE. 
          ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN 
              kgen_istrue = .FALSE. 
          ELSE 
              kgen_istrue = .TRUE. 
          END IF   
      ELSE IF (SIZE(var)==0) THEN 
          kgen_istrue = .FALSE. 
      ELSE 
          kgen_istrue = .TRUE. 
      END IF   
      IF (.NOT. ASSOCIATED(var)) THEN 
          kgen_istrue = .FALSE. 
      END IF   
      WRITE (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          kgen_array_sum = REAL(SUM(var, mask=(var .eq. var)), 8) 
          WRITE (UNIT = kgen_unit) kgen_array_sum 
          WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
          WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
          WRITE (UNIT = kgen_unit) var 
          IF (PRESENT( printvar )) THEN 
              WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8) 
          END IF   
      END IF   
  END SUBROUTINE kw_sponge_damp_profile_real__core_rknd_dim1_ptr 
    
end module sponge_layer_damping