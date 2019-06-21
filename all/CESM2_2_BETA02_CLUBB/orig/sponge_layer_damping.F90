!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:39 
!KGEN version : 0.8.1 
  
!---------------------------------------------------------------------------
! $Id$
!===============================================================================


module sponge_layer_damping
  ! Description:
  ! This module is used for damping variables in upper altitudes of the grid.
  ! References:
  !   None
  !-------------------------------------------------------------------------

  !

    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PUBLIC sponge_damp_xm, sponge_damp_xp2, sponge_damp_xp3, sponge_damp_settings, sponge_damp_profile 


  type sponge_damp_settings

    real( kind = core_rknd ) :: &
      tau_sponge_damp_min, & ! Minimum damping time scale (model top)        [s]
      tau_sponge_damp_max, & ! Maximum damping time scale (damp layer base)  [s]
      sponge_damp_depth      ! Damping depth as a fraction of domain height  [-]

    logical :: &
      l_sponge_damping       ! True if damping is being used

  end type sponge_damp_settings

  type sponge_damp_profile

    real( kind = core_rknd ), allocatable, dimension(:) :: &
      tau_sponge_damp    ! Damping time scale    [1/s]

    real( kind = core_rknd ) :: &
      sponge_layer_depth    ! Depth of sponge damping layer  [m]

  end type sponge_damp_profile


  type(sponge_damp_settings), public :: &
    thlm_sponge_damp_settings,    & ! Variable(s)
    rtm_sponge_damp_settings,     &
    uv_sponge_damp_settings,      &
    wp2_sponge_damp_settings,     &
    wp3_sponge_damp_settings,     &
    up2_vp2_sponge_damp_settings
!$omp threadprivate( thlm_sponge_damp_settings, rtm_sponge_damp_settings, &
!$omp                uv_sponge_damp_settings, wp2_sponge_damp_settings, &
!$omp                wp3_sponge_damp_settings, up2_vp2_sponge_damp_settings )

  type(sponge_damp_profile), public :: &
    thlm_sponge_damp_profile,    & ! Variable(s)
    rtm_sponge_damp_profile,     &
    uv_sponge_damp_profile,      &
    wp2_sponge_damp_profile,     &
    wp3_sponge_damp_profile,     &
    up2_vp2_sponge_damp_profile
!$omp threadprivate( thlm_sponge_damp_profile, rtm_sponge_damp_profile, &
!$omp                uv_sponge_damp_profile, wp2_sponge_damp_profile, &
!$omp                wp3_sponge_damp_profile, up2_vp2_sponge_damp_profile )


  PRIVATE 
  PUBLIC kr_externs_in_sponge_layer_damping 
  PUBLIC kr_externs_out_sponge_layer_damping 
  PUBLIC kr_kgen_sponge_layer_damping_typesubp1 
  PUBLIC kr_kgen_sponge_layer_damping_typesubp2 
  PUBLIC kv_kgen_sponge_layer_damping_typesubp1 
  PUBLIC kv_kgen_sponge_layer_damping_typesubp2 

  contains
  !=============================================================================

  function sponge_damp_xm( dt, z, xm_ref, xm, damping_profile ) result( xm_p )
    ! Description:
    ! Damps specified mean field toward a reference profile.  The module must be
    ! initialized for this function to work.  Otherwise a stop is issued.
    ! References:
    ! None
    !-----------------------------------------------------------------------
    !  "Sponge"-layer damping at the domain top region


      USE grid_class, ONLY: gr 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! External

    intrinsic :: allocated
    ! Input Variable(s)

    real( kind = core_rknd ), intent(in) :: &
      dt    ! Model Timestep  [s]

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      z,      & ! Height of model grid levels                [m]
      xm_ref, & ! Reference profile of x to damp xm towards  [units vary]
      xm        ! Mean field being damped                    [units vary]

    type(sponge_damp_profile), intent(in) :: &
      damping_profile
    ! Output Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      xm_p   ! Damped value of xm  [units_vary]
    ! Local Variable(s)

    real( kind = core_rknd ) :: &
      dt_on_tau    ! Ratio of timestep to damping timescale  [-]

    integer :: k
    ! ---- Begin Code ----


    if ( allocated( damping_profile%tau_sponge_damp ) ) then

       xm_p = xm
     
       do k = gr%nz, 1, -1
          ! The height of the model top is gr%zm(gr%nz).

          if ( gr%zm(gr%nz) - z(k) < damping_profile%sponge_layer_depth ) then
             ! Vince Larson used implicit discretization in order to 
             ! reduce noise in rtm in cloud_feedback_s12 (CGILS) 
             !xm_p(k) = xm(k) - real( ( ( xm(k) - xm_ref(k) ) / & 
             !                  damping_profile%tau_sponge_damp(k) ) * dt )

             dt_on_tau = dt / damping_profile%tau_sponge_damp(k)
             ! Really, we should be using xm_ref at time n+1 rather than n.
             ! However, for steady profiles of xm_ref, it won't matter.        

             xm_p(k) = ( xm(k) + dt_on_tau * xm_ref(k) ) / &
                             ( 1.0_core_rknd + dt_on_tau )
             ! End Vince Larson's change

          else ! gr%zm(gr%nz) - z(k) >= damping_profile%sponge_layer_depth
             ! Below sponge damping layer; exit loop.

             exit

          endif ! gr%zm(gr%nz) - z(k) < damping_profile%sponge_layer_depth


       enddo ! k = gr%nz, 1, -1

    else

       stop "tau_sponge_damp in sponge_damp_xm used before initialization"

    endif


    return

  end function sponge_damp_xm
  !=============================================================================

  function sponge_damp_xp2( dt, z, xp2, x_tol_sqd, damping_profile ) &
  result( xp2_damped )
    ! Description:
    ! Calculates the effects of "sponge"-layer damping on the variance of x,
    ! xp2.
    ! Sponge damping on a local value of x is given by the equation:
    ! x_d = x - ( delta_t / tau ) * ( x - <x> ),
    ! where x is the local value prior to damping, x_d is the damped local value
    ! of x, <x> is the grid-level mean value of x, delta_t is the model time
    ! step duration, and tau is the damping time scale.  Since delta_t / tau has
    ! the same value everywhere at a grid level, the grid-level mean of x does
    ! not change as a result of damping.
    ! Subtracting <x> from both sides:
    ! x_d - <x> = ( x - <x> ) - ( delta_t / tau ) * ( x - <x> ),
    ! which results in:
    ! x_d - <x> = ( 1 - delta_t / tau ) * ( x - <x> ).
    ! Squaring both sides:
    ! ( x_d - <x> )^2 = ( 1 - delta_t / tau )^2 * ( x - <x> )^2.
    ! After averaging both sides, the damped value of xp2 is:
    ! < x_d'^2 > = ( 1 - delta_t / tau )^2 * < x'^2 >.
    ! Any sponge damping is applied to (predictive) xp2 after the field has been
    ! advanced in time.  This allows sponge damping to be applied in an implicit
    ! manner.  The damped value of xp2 must also be limited at a minimum value
    ! of x_tol^2.
    ! References:
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


      USE grid_class, ONLY: gr 

      USE constants_clubb, ONLY: one 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variable(s)

    real( kind = core_rknd ), intent(in) :: &
      dt    ! Model Timestep  [s]

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      z,   & ! Height of model grid levels               [m]
      xp2    ! Variance of x, <x'^2>, prior to damping   [units vary]

    real( kind = core_rknd ), intent(in) :: &
      x_tol_sqd    ! Square of the tolerance value of x    [units vary]

    type(sponge_damp_profile), intent(in) :: &
      damping_profile
    ! Output Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      xp2_damped    ! Variance of x, <x'^2>, after damping   [units vary]
    ! Local Variable(s)

    real( kind = core_rknd ) :: &
      dt_on_tau    ! Ratio of model time step duration to damping timescale  [-]

    integer :: &
      k    ! Loop index


    if ( allocated( damping_profile%tau_sponge_damp ) ) then
       ! Set the entire profile of <x'^2> after damping to the profile of <x'^2>
       ! before damping.  The values of <x'^2> after damping will be overwritten
       ! at any levels where "sponge"-layer damping occurs.

       xp2_damped = xp2
     
       do k = gr%nz, 1, -1
          ! The height of the model top is gr%zm(gr%nz).

          if ( gr%zm(gr%nz) - z(k) < damping_profile%sponge_layer_depth ) then
             ! Calculate the value of delta_t / tau at the grid level.

             dt_on_tau = dt / damping_profile%tau_sponge_damp(k)
             ! Calculate the damped value of <x'^2>.

             xp2_damped(k) = ( one - dt_on_tau )**2 * xp2(k)
             ! The damped value of <x'^2> needs to be greater than or equal to
             ! x_tol^2.

             xp2_damped(k) = max( xp2_damped(k), x_tol_sqd )

          else ! gr%zm(gr%nz) - z(k) >= damping_profile%sponge_layer_depth
             ! Below sponge damping layer; exit loop.

             exit

          endif ! gr%zm(gr%nz) - z(k) < damping_profile%sponge_layer_depth


       enddo ! k = gr%nz, 1, -1

    else

       stop "tau_sponge_damp in sponge_damp_xp2 used before initialization"

    endif


    return

  end function sponge_damp_xp2
  !=============================================================================

  function sponge_damp_xp3( dt, z, xp3, damping_profile ) &
  result( xp3_damped )
    ! Description:
    ! Calculates the effects of "sponge"-layer damping on xp3.
    ! Sponge damping on a local value of x is given by the equation:
    ! x_d = x - ( delta_t / tau ) * ( x - <x> ),
    ! where x is the local value prior to damping, x_d is the damped local value
    ! of x, <x> is the grid-level mean value of x, delta_t is the model time
    ! step duration, and tau is the damping time scale.  Since delta_t / tau has
    ! the same value everywhere at a grid level, the grid-level mean of x does
    ! not change as a result of damping.
    ! Subtracting <x> from both sides:
    ! x_d - <x> = ( x - <x> ) - ( delta_t / tau ) * ( x - <x> ),
    ! which results in:
    ! x_d - <x> = ( 1 - delta_t / tau ) * ( x - <x> ).
    ! Taking both sides to the third power:
    ! ( x_d - <x> )^3 = ( 1 - delta_t / tau )^3 * ( x - <x> )^3.
    ! After averaging both sides, the damped value of xp3 is:
    ! < x_d'^3 > = ( 1 - delta_t / tau )^3 * < x'^3 >.
    ! Any sponge damping is applied to (predictive) xp3 after the field has been
    ! advanced in time.  This allows sponge damping to be applied in an implicit
    ! manner.
    ! References:
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


      USE grid_class, ONLY: gr 

      USE constants_clubb, ONLY: one 

      USE clubb_precision, ONLY: core_rknd 

    implicit none
    ! Input Variable(s)

    real( kind = core_rknd ), intent(in) :: &
      dt    ! Model Timestep  [s]

    real( kind = core_rknd ), dimension(gr%nz), intent(in) :: &
      z,   & ! Height of model grid levels     [m]
      xp3    ! <x'^3> prior to damping         [units vary]

    type(sponge_damp_profile), intent(in) :: &
      damping_profile
    ! Output Variable

    real( kind = core_rknd ), dimension(gr%nz) :: &
      xp3_damped    ! <x'^3> after damping   [units vary]
    ! Local Variable(s)

    real( kind = core_rknd ) :: &
      dt_on_tau    ! Ratio of model time step duration to damping timescale  [-]

    integer :: &
      k    ! Loop index


    if ( allocated( damping_profile%tau_sponge_damp ) ) then
       ! Set the entire profile of <x'^3> after damping to the profile of <x'^3>
       ! before damping.  The values of <x'^3> after damping will be overwritten
       ! at any levels where "sponge"-layer damping occurs.

       xp3_damped = xp3
     
       do k = gr%nz, 1, -1
          ! The height of the model top is gr%zm(gr%nz).

          if ( gr%zm(gr%nz) - z(k) < damping_profile%sponge_layer_depth ) then
             ! Calculate the value of delta_t / tau at the grid level.

             dt_on_tau = dt / damping_profile%tau_sponge_damp(k)
             ! Calculate the damped value of <x'^3>.

             xp3_damped(k) = ( one - dt_on_tau )**3 * xp3(k)

          else ! gr%zm(gr%nz) - z(k) >= damping_profile%sponge_layer_depth
             ! Below sponge damping layer; exit loop.

             exit

          endif ! gr%zm(gr%nz) - z(k) < damping_profile%sponge_layer_depth


       enddo ! k = gr%nz, 1, -1

    else

       stop "tau_sponge_damp in sponge_damp_xp3 used before initialization"

    endif


    return

  end function sponge_damp_xp3
  !=============================================================================


  !=============================================================================


!===============================================================================


  !read state subroutine for kr_externs_in_sponge_layer_damping 
  SUBROUTINE kr_externs_in_sponge_layer_damping(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_kgen_sponge_layer_damping_typesubp1(wp2_sponge_damp_settings, kgen_unit, "wp2_sponge_damp_settings", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp1(rtm_sponge_damp_settings, kgen_unit, "rtm_sponge_damp_settings", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp1(thlm_sponge_damp_settings, kgen_unit, "thlm_sponge_damp_settings", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp1(wp3_sponge_damp_settings, kgen_unit, "wp3_sponge_damp_settings", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp1(uv_sponge_damp_settings, kgen_unit, "uv_sponge_damp_settings", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp1(up2_vp2_sponge_damp_settings, kgen_unit, "up2_vp2_sponge_damp_settings", &
      &.FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp2(wp3_sponge_damp_profile, kgen_unit, "wp3_sponge_damp_profile", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp2(wp2_sponge_damp_profile, kgen_unit, "wp2_sponge_damp_profile", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp2(uv_sponge_damp_profile, kgen_unit, "uv_sponge_damp_profile", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp2(up2_vp2_sponge_damp_profile, kgen_unit, "up2_vp2_sponge_damp_profile", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp2(rtm_sponge_damp_profile, kgen_unit, "rtm_sponge_damp_profile", .FALSE.) 
      CALL kr_kgen_sponge_layer_damping_typesubp2(thlm_sponge_damp_profile, kgen_unit, "thlm_sponge_damp_profile", .FALSE.) 
  END SUBROUTINE kr_externs_in_sponge_layer_damping 
    
  !read state subroutine for kr_externs_out_sponge_layer_damping 
  SUBROUTINE kr_externs_out_sponge_layer_damping(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
  END SUBROUTINE kr_externs_out_sponge_layer_damping 
    
  !read state subroutine for kr_kgen_sponge_layer_damping_typesubp1 
  RECURSIVE SUBROUTINE kr_kgen_sponge_layer_damping_typesubp1(var, kgen_unit, printname, printvar) 
      TYPE(sponge_damp_settings), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%tau_sponge_damp_min 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%tau_sponge_damp_min = ", var%tau_sponge_damp_min 
      END IF   
      READ (UNIT = kgen_unit) var%tau_sponge_damp_max 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%tau_sponge_damp_max = ", var%tau_sponge_damp_max 
      END IF   
      READ (UNIT = kgen_unit) var%sponge_damp_depth 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%sponge_damp_depth = ", var%sponge_damp_depth 
      END IF   
        
      READ (UNIT = kgen_unit) var%l_sponge_damping 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%l_sponge_damping = ", var%l_sponge_damping 
      END IF   
        
  END SUBROUTINE kr_kgen_sponge_layer_damping_typesubp1 
    
  !read state subroutine for kr_kgen_sponge_layer_damping_typesubp2 
  RECURSIVE SUBROUTINE kr_kgen_sponge_layer_damping_typesubp2(var, kgen_unit, printname, printvar) 
      TYPE(sponge_damp_profile), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          CALL kr_kgen_sponge_damp_profile_subp5(var%tau_sponge_damp, kgen_unit, printname // "%tau_sponge_damp", .TRUE.) 
      ELSE 
          CALL kr_kgen_sponge_damp_profile_subp5(var%tau_sponge_damp, kgen_unit, printname // "%tau_sponge_damp", .FALSE.) 
      END IF   
        
      READ (UNIT = kgen_unit) var%sponge_layer_depth 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%sponge_layer_depth = ", var%sponge_layer_depth 
      END IF   
        
  END SUBROUTINE kr_kgen_sponge_layer_damping_typesubp2 
    
  !write state subroutine for kr_kgen_sponge_damp_profile_subp5 
  SUBROUTINE kr_kgen_sponge_damp_profile_subp5(var, kgen_unit, printname, printvar) 
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
        
  END SUBROUTINE kr_kgen_sponge_damp_profile_subp5 
    
  !verify state subroutine for kv_kgen_sponge_layer_damping_typesubp1 
  RECURSIVE SUBROUTINE kv_kgen_sponge_layer_damping_typesubp1(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(sponge_damp_settings), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=core_rknd) :: diff_tau_sponge_damp_min 
      real(KIND=core_rknd) :: diff_tau_sponge_damp_max 
      real(KIND=core_rknd) :: diff_sponge_damp_depth 
      logical :: diff_l_sponge_damping 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%tau_sponge_damp_min == kgenref_var%tau_sponge_damp_min) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp_min is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_tau_sponge_damp_min = ABS(var%tau_sponge_damp_min - kgenref_var%tau_sponge_damp_min) 
          IF (diff_tau_sponge_damp_min <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp_min is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp_min is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_tau_sponge_damp_min 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_tau_sponge_damp_min 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%tau_sponge_damp_max == kgenref_var%tau_sponge_damp_max) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp_max is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_tau_sponge_damp_max = ABS(var%tau_sponge_damp_max - kgenref_var%tau_sponge_damp_max) 
          IF (diff_tau_sponge_damp_max <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp_max is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp_max is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_tau_sponge_damp_max 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_tau_sponge_damp_max 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%sponge_damp_depth == kgenref_var%sponge_damp_depth) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sponge_damp_depth is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_sponge_damp_depth = ABS(var%sponge_damp_depth - kgenref_var%sponge_damp_depth) 
          IF (diff_sponge_damp_depth <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sponge_damp_depth is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sponge_damp_depth is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_sponge_damp_depth 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sponge_damp_depth 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%l_sponge_damping .EQV. kgenref_var%l_sponge_damping) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%l_sponge_damping is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
          IF (kgen_verboseLevel > 1) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%l_sponge_damping is NOT IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_OUT_TOL 
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "NOT IMPLEMENTED YET" 
                  WRITE (*, *) "" 
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
  END SUBROUTINE kv_kgen_sponge_layer_damping_typesubp1 
    
  !verify state subroutine for kv_kgen_sponge_layer_damping_typesubp2 
  RECURSIVE SUBROUTINE kv_kgen_sponge_layer_damping_typesubp2(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(sponge_damp_profile), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      INTEGER :: n_tau_sponge_damp 
      real(KIND=core_rknd) :: nrmsdiff_tau_sponge_damp, rmsdiff_tau_sponge_damp 
      real(KIND=core_rknd), ALLOCATABLE :: buf1_tau_sponge_damp(:), buf2_tau_sponge_damp(:) 
      real(KIND=core_rknd) :: diff_sponge_layer_depth 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      IF (ALLOCATED(var%tau_sponge_damp)) THEN 
          dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
          IF (ALL(var%tau_sponge_damp == kgenref_var%tau_sponge_damp)) THEN 
              dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp is IDENTICAL." 
                  END IF   
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              ALLOCATE (buf1_tau_sponge_damp(SIZE(var%tau_sponge_damp,dim=1))) 
              ALLOCATE (buf2_tau_sponge_damp(SIZE(var%tau_sponge_damp,dim=1))) 
              n_tau_sponge_damp = COUNT(var%tau_sponge_damp /= kgenref_var%tau_sponge_damp) 
              WHERE ( ABS(kgenref_var%tau_sponge_damp) > kgen_minvalue ) 
                  buf1_tau_sponge_damp = ((var%tau_sponge_damp-kgenref_var%tau_sponge_damp)/kgenref_var%tau_sponge_damp)**2 
                  buf2_tau_sponge_damp = (var%tau_sponge_damp-kgenref_var%tau_sponge_damp)**2 
              ELSEWHERE 
                  buf1_tau_sponge_damp = (var%tau_sponge_damp-kgenref_var%tau_sponge_damp)**2 
                  buf2_tau_sponge_damp = buf1_tau_sponge_damp 
              END WHERE   
              nrmsdiff_tau_sponge_damp = SQRT(SUM(buf1_tau_sponge_damp)/REAL(n_tau_sponge_damp)) 
              rmsdiff_tau_sponge_damp = SQRT(SUM(buf2_tau_sponge_damp)/REAL(n_tau_sponge_damp)) 
              IF (rmsdiff_tau_sponge_damp > kgen_tolerance) THEN 
                  dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                  END IF   
                  check_result = CHECK_OUT_TOL 
              ELSE 
                  dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
                  IF (kgen_verboseLevel > 1) THEN 
                      IF (check_status%rank == 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), "%tau_sponge_damp is NOT IDENTICAL(within tolerance)." 
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
                      WRITE (*, *) count( var%tau_sponge_damp /= kgenref_var%tau_sponge_damp), " of ", size( var%tau_sponge_damp &
                      &), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tau_sponge_damp)/real(size(var%tau_sponge_damp)) 
                      WRITE (*, *) "Average - reference ", &
                      &sum(kgenref_var%tau_sponge_damp)/real(size(kgenref_var%tau_sponge_damp)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tau_sponge_damp 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tau_sponge_damp 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (kgen_verboseLevel > 2) THEN 
                  IF (check_status%rank ==0) THEN 
                      WRITE (*, *) count( var%tau_sponge_damp /= kgenref_var%tau_sponge_damp), " of ", size( var%tau_sponge_damp &
                      &), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var%tau_sponge_damp)/real(size(var%tau_sponge_damp)) 
                      WRITE (*, *) "Average - reference ", &
                      &sum(kgenref_var%tau_sponge_damp)/real(size(kgenref_var%tau_sponge_damp)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff_tau_sponge_damp 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_tau_sponge_damp 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
          END IF   
            
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%sponge_layer_depth == kgenref_var%sponge_layer_depth) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%sponge_layer_depth is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_sponge_layer_depth = ABS(var%sponge_layer_depth - kgenref_var%sponge_layer_depth) 
          IF (diff_sponge_layer_depth <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sponge_layer_depth is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%sponge_layer_depth is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_sponge_layer_depth 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_sponge_layer_depth 
                  WRITE (*, *) "" 
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
  END SUBROUTINE kv_kgen_sponge_layer_damping_typesubp2 
    
end module sponge_layer_damping