
! KGEN-generated Fortran source file
!
! Filename    : sponge_layer_damping.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE sponge_layer_damping
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   This module is used for damping variables in upper altitudes of the grid.
!
! References:
!   None
!---------------------------------------------------------------------------------------------------
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
        PUBLIC sponge_damp_settings, sponge_damp_profile, sponge_damp_xm
        TYPE sponge_damp_settings
            REAL(KIND=core_rknd) :: tau_sponge_damp_min, tau_sponge_damp_max, sponge_damp_depth
! Minimum damping time-scale (at the top) [s]
! Maximum damping time-scale (base of damping layer) [s]
! damping depth as a fraction of domain height [-]
            LOGICAL :: l_sponge_damping
! True if damping is being used
        END TYPE sponge_damp_settings
        TYPE sponge_damp_profile
            REAL(KIND=core_rknd), pointer, dimension(:) :: tau_sponge_damp
! Damping factor
            INTEGER :: n_sponge_damp
! Number of levels damped
        END TYPE sponge_damp_profile
        TYPE(sponge_damp_settings), public :: rtm_sponge_damp_settings
        TYPE(sponge_damp_settings), public :: thlm_sponge_damp_settings
        TYPE(sponge_damp_settings), public :: uv_sponge_damp_settings
!$omp threadprivate(thlm_sponge_damp_settings, rtm_sponge_damp_settings, uv_sponge_damp_settings)
        TYPE(sponge_damp_profile), public :: rtm_sponge_damp_profile
        TYPE(sponge_damp_profile), public :: thlm_sponge_damp_profile
        TYPE(sponge_damp_profile), public :: uv_sponge_damp_profile
!$omp threadprivate(thlm_sponge_damp_profile, rtm_sponge_damp_profile,  uv_sponge_damp_profile)
        PRIVATE
            PUBLIC kgen_read_externs_sponge_layer_damping

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_sponge_damp_settings
            MODULE PROCEDURE kgen_read_sponge_damp_profile
        END INTERFACE kgen_read

        PUBLIC kgen_verify
        INTERFACE kgen_verify
            MODULE PROCEDURE kgen_verify_sponge_damp_settings
            MODULE PROCEDURE kgen_verify_sponge_damp_profile
        END INTERFACE kgen_verify

        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_core_rknd_dim1_ptr(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), POINTER, DIMENSION(:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1
                INTEGER, DIMENSION(2,1) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim1_ptr


        ! module extern variables

        SUBROUTINE kgen_read_externs_sponge_layer_damping(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_sponge_damp_settings(rtm_sponge_damp_settings, kgen_unit)
            CALL kgen_read_sponge_damp_settings(thlm_sponge_damp_settings, kgen_unit)
            CALL kgen_read_sponge_damp_profile(rtm_sponge_damp_profile, kgen_unit)
            CALL kgen_read_sponge_damp_profile(thlm_sponge_damp_profile, kgen_unit)
            CALL kgen_read_sponge_damp_settings(uv_sponge_damp_settings, kgen_unit)
            CALL kgen_read_sponge_damp_profile(uv_sponge_damp_profile, kgen_unit)
        END SUBROUTINE kgen_read_externs_sponge_layer_damping

        SUBROUTINE kgen_read_sponge_damp_settings(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(sponge_damp_settings), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%tau_sponge_damp_min
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%tau_sponge_damp_min **", var%tau_sponge_damp_min
            END IF
            READ(UNIT=kgen_unit) var%tau_sponge_damp_max
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%tau_sponge_damp_max **", var%tau_sponge_damp_max
            END IF
            READ(UNIT=kgen_unit) var%sponge_damp_depth
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%sponge_damp_depth **", var%sponge_damp_depth
            END IF
            READ(UNIT=kgen_unit) var%l_sponge_damping
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%l_sponge_damping **", var%l_sponge_damping
            END IF
        END SUBROUTINE
        SUBROUTINE kgen_read_sponge_damp_profile(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(sponge_damp_profile), INTENT(out) :: var
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%tau_sponge_damp, kgen_unit, printvar=printvar//"%tau_sponge_damp")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%tau_sponge_damp, kgen_unit)
            END IF
            READ(UNIT=kgen_unit) var%n_sponge_damp
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%n_sponge_damp **", var%n_sponge_damp
            END IF
        END SUBROUTINE
        RECURSIVE SUBROUTINE kgen_verify_sponge_damp_settings(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(sponge_damp_settings), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_real_core_rknd("tau_sponge_damp_min", dtype_check_status, var%tau_sponge_damp_min, ref_var%tau_sponge_damp_min)
            CALL kgen_verify_real_core_rknd("tau_sponge_damp_max", dtype_check_status, var%tau_sponge_damp_max, ref_var%tau_sponge_damp_max)
            CALL kgen_verify_real_core_rknd("sponge_damp_depth", dtype_check_status, var%sponge_damp_depth, ref_var%sponge_damp_depth)
            CALL kgen_verify_logical("l_sponge_damping", dtype_check_status, var%l_sponge_damping, ref_var%l_sponge_damping)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
        RECURSIVE SUBROUTINE kgen_verify_sponge_damp_profile(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(sponge_damp_profile), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_real_core_rknd_dim1_ptr("tau_sponge_damp", dtype_check_status, var%tau_sponge_damp, ref_var%tau_sponge_damp)
            CALL kgen_verify_integer("n_sponge_damp", dtype_check_status, var%n_sponge_damp, ref_var%n_sponge_damp)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
            SUBROUTINE kgen_verify_real_core_rknd( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=core_rknd), intent(in) :: var, ref_var
                check_status%numTotal = check_status%numTotal + 1
                IF ( var == ref_var ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                    endif
                ELSE
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "KERNEL: ", var
                            WRITE(*,*) "REF.  : ", ref_var
                        end if
                    end if
                    check_status%numFatal = check_status%numFatal + 1
                END IF
            END SUBROUTINE kgen_verify_real_core_rknd

            SUBROUTINE kgen_verify_logical( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                logical, intent(in) :: var, ref_var
                check_status%numTotal = check_status%numTotal + 1
                IF ( var .EQV. ref_var ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                    endif
                ELSE
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "KERNEL: ", var
                            WRITE(*,*) "REF.  : ", ref_var
                        end if
                    end if
                    check_status%numFatal = check_status%numFatal + 1
                END IF
            END SUBROUTINE kgen_verify_logical

            SUBROUTINE kgen_verify_real_core_rknd_dim1_ptr( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=core_rknd), intent(in), DIMENSION(:), POINTER :: var, ref_var
                real(KIND=core_rknd) :: nrmsdiff, rmsdiff
                real(KIND=core_rknd), allocatable, DIMENSION(:) :: temp, temp2
                integer :: n
                IF ( ASSOCIATED(var) ) THEN
                check_status%numTotal = check_status%numTotal + 1
                IF ( ALL( var == ref_var ) ) THEN
                
                    check_status%numIdentical = check_status%numIdentical + 1            
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                        !WRITE(*,*) "KERNEL: ", var
                        !WRITE(*,*) "REF.  : ", ref_var
                        IF ( ALL( var == 0 ) ) THEN
                            if(check_status%verboseLevel > 2) then
                                WRITE(*,*) "All values are zero."
                            end if
                        END IF
                    end if
                ELSE
                    allocate(temp(SIZE(var,dim=1)))
                    allocate(temp2(SIZE(var,dim=1)))
                
                    n = count(var/=ref_var)
                    where(abs(ref_var) > check_status%minvalue)
                        temp  = ((var-ref_var)/ref_var)**2
                        temp2 = (var-ref_var)**2
                    elsewhere
                        temp  = (var-ref_var)**2
                        temp2 = temp
                    endwhere
                    nrmsdiff = sqrt(sum(temp)/real(n))
                    rmsdiff = sqrt(sum(temp2)/real(n))
                
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                        if(check_status%verboseLevel > 1) then
                            WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                            WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                        endif
                        WRITE(*,*) "RMS of difference is ",rmsdiff
                        WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                    end if
                
                    if (nrmsdiff > check_status%tolerance) then
                        check_status%numFatal = check_status%numFatal+1
                    else
                        check_status%numWarning = check_status%numWarning+1
                    endif
                
                    deallocate(temp,temp2)
                END IF
                END IF
            END SUBROUTINE kgen_verify_real_core_rknd_dim1_ptr

            SUBROUTINE kgen_verify_integer( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer, intent(in) :: var, ref_var
                check_status%numTotal = check_status%numTotal + 1
                IF ( var == ref_var ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                    endif
                ELSE
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "KERNEL: ", var
                            WRITE(*,*) "REF.  : ", ref_var
                        end if
                    end if
                    check_status%numFatal = check_status%numFatal + 1
                END IF
            END SUBROUTINE kgen_verify_integer

!---------------------------------------------------------------------------------------------

        FUNCTION sponge_damp_xm(dt, xm_ref, xm, damping_profile) RESULT ( xm_p )
!
! Description:
!   Damps specified variable. The module must be initialized for
!   this function to work. Otherwise a stop is issued.
!
! References:
!   None
!-------------------------------------------------------------------------------------------
!  "Sponge"-layer damping at the domain top region
            USE grid_class, ONLY: gr ! Variable(s)
            USE clubb_precision, ONLY: core_rknd ! Variable(s)
            IMPLICIT NONE
! External
            INTRINSIC associated
! Input Variable(s)
            REAL(KIND=core_rknd), intent(in) :: dt ! Model Timestep
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: xm_ref
! Reference to damp to [-]
            REAL(KIND=core_rknd), dimension(gr%nz), intent(in) :: xm
! Variable being damped [-]
            TYPE(sponge_damp_profile), intent(in) :: damping_profile
! Output Variable(s)
            REAL(KIND=core_rknd), dimension(gr%nz) :: xm_p ! Variable damped [-]
            REAL(KIND=core_rknd) :: dt_on_tau ! Ratio of timestep to damping timescale [-]
            INTEGER :: k
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
      end do ! k ! k
    else
      stop "tau_sponge_damp in damping used before initialization"
    end if
    return
        END FUNCTION sponge_damp_xm
!---------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------

    END MODULE sponge_layer_damping
