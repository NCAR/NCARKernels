
! KGEN-generated Fortran source file
!
! Filename    : grid_class.F90
! Generated at: 2015-10-20 14:27:07
! KGEN version: 0.5.3



    MODULE grid_class
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!
! Definition of a grid class and associated functions
!
! The grid specification is as follows:
!
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
!
!                                           .
!                                           .
!                                           .
!                                           .
!
!                      ================== zm(k+1) ===================
!
!
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
!
!
!                      ------------------ zt(k-1) -------------------
!
!                                           .
!                                           .
!                                           .
!                                           .
!
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
!
!
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
!
! The grid setup is compatible with a stretched (unevely-spaced) grid.  Thus,
! the distance between successive grid levels may not always be constant.
!
! The following diagram is an example of a stretched grid that is defined on
! momentum levels.  The thermodynamic levels are placed exactly halfway
! between the momentum levels.  However, the momentum levels do not fall
! halfway between the thermodynamic levels.
!
!        =============== zm(k+1) ===============
!
!
!
!        --------------- zt(k+1) ---------------
!
!
!
!        ===============  zm(k)  ===============
!
!        ---------------  zt(k)  ---------------
!
!        =============== zm(k-1) ===============
!
! The following diagram is an example of a stretched grid that is defined on
! thermodynamic levels.  The momentum levels are placed exactly halfway
! between the thermodynamic levels.  However, the thermodynamic levels do not
! fall halfway between the momentum levels.
!
!        --------------- zt(k+1) ---------------
!
!
!
!        ===============  zm(k)  ===============
!
!
!
!        ---------------  zt(k)  ---------------
!
!        =============== zm(k-1) ===============
!
!        --------------- zt(k-1) ---------------
!
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
!
! Momentum level 1 is placed at altitude zm_init, which is usually at the
! surface.  However, in general, zm_init can be at any altitude defined by the
! user.
!
! GP indicates ghost points. Variables located at those levels are not
! prognosed, but only used for boundary conditions.
!
! Chris Golaz, 7/17/99
! modified 9/10/99
! schemena, modified 6/11/2014 - Restructered code to add cubic/linear flag
!  References:
!  Section 3c, p. 3548 /Numerical discretization/ of:
!   ``A PDF-Based Model for Boundary Layer Clouds. Part I:
!     Method and Model Description'' Golaz, et al. (2002)
!     JAS, Vol. 59, pp. 3540--3551.
!-----------------------------------------------------------------------
        USE clubb_precision, ONLY: core_rknd
! Variable(s)
        IMPLICIT NONE
        PUBLIC gr, grid, zm2zt, zt2zm, ddzt, ddzm
        PRIVATE cubic_interpolated_aztk, linear_interpolated_aztk, cubic_interpolated_azt, linear_interpolated_azt, &
        cubic_interpolated_azmk, linear_interpolated_azmk, cubic_interpolated_azm, linear_interpolated_azm, gradzt, gradzm
        PRIVATE ! Default Scoping
! Constant parameters
! Upper thermodynamic level index (gr%weights_zt2zm).
! Lower thermodynamic level index (gr%weights_zt2zm).
! Upper momentum level index (gr%weights_zm2zt).
! Lower momentum level index (gr%weights_zm2zt).
        TYPE grid
            INTEGER :: nz ! Number of points in the grid
!   Note: Fortran 90/95 prevents an allocatable array from appearing
!   within a derived type.  However, a pointer can be used in the same
!   manner as an allocatable array, as we have done here (the grid
!   pointers are always allocated rather than assigned and nullified
!   like real pointers).  Note that these must be de-allocated to prevent
!   memory leaks.
            REAL(KIND=core_rknd), pointer, dimension(:) :: zm, zt
! Momentum grid
! Thermo grid
            REAL(KIND=core_rknd), pointer, dimension(:) :: invrs_dzm, invrs_dzt
! The inverse spacing between thermodynamic grid
! levels; centered over momentum grid levels.
! The inverse spacing between momentum grid levels;
! centered over thermodynamic grid levels.
            REAL(KIND=core_rknd), pointer, dimension(:) :: dzm, dzt
! Spacing between thermodynamic grid levels; centered over
! momentum grid levels
! Spcaing between momentum grid levels; centered over
! thermodynamic grid levels
! These weights are normally used in situations
! where a momentum level variable is being
! solved for implicitly in an equation and
! needs to be interpolated to the thermodynamic grid levels.
            REAL(KIND=core_rknd), pointer, dimension(:,:) :: weights_zm2zt, weights_zt2zm
! These weights are normally used in situations where a
! thermodynamic level variable is being solved for implicitly in an equation
! and needs to be interpolated to the momentum grid levels.
        END TYPE grid
!   The grid is defined here so that it is common throughout the module.
!   The implication is that only one grid can be defined !
        TYPE(grid) :: gr
!   Modification for using CLUBB in a host model (i.e. one grid per column)
!$omp   threadprivate(gr)
! Interfaces provided for function overloading

        INTERFACE zt2zm
! For l_cubic_interp = .true.
! This version uses cublic spline interpolation of Stefen (1990).
!
! For l_cubic_interp = .false.
! This performs a linear extension at the highest grid level and therefore
! does not guarantee, for positive definite quantities (e.g. wp2), that the
! extended point is indeed positive definite.  Positive definiteness can be
! ensured with a max statement.
! In the future, we could add a flag (lposdef) and, when needed, apply the
! max statement directly within interpolated_azm and interpolated_azmk.
            MODULE PROCEDURE redirect_interpolated_azmk, redirect_interpolated_azm
        END INTERFACE 

        INTERFACE zm2zt
! For l_cubic_interp = .true.
! This version uses cublic spline interpolation of Stefen (1990).
!
! For l_cubic_interp = .false.
! This performs a linear extension at the lowest grid level and therefore
! does not guarantee, for positive definite quantities (e.g. wp2), that the
! extended point is indeed positive definite.  Positive definiteness can be
! ensured with a max statement.
! In the future, we could add a flag (lposdef) and, when needed, apply the
! max statement directly within interpolated_azt and interpolated_aztk.
            MODULE PROCEDURE redirect_interpolated_aztk, redirect_interpolated_azt
        END INTERFACE 


! Vertical derivative functions

        INTERFACE ddzm
            MODULE PROCEDURE gradzm
        END INTERFACE 

        INTERFACE ddzt
            MODULE PROCEDURE gradzt
        END INTERFACE 
            PUBLIC kgen_read_externs_grid_class

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_grid
        END INTERFACE kgen_read

        PUBLIC kgen_verify
        INTERFACE kgen_verify
            MODULE PROCEDURE kgen_verify_grid
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

            SUBROUTINE kgen_read_real_core_rknd_dim2_ptr(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=core_rknd), INTENT(OUT), POINTER, DIMENSION(:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2
                INTEGER, DIMENSION(2,2) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                    END IF
                ELSE
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_core_rknd_dim2_ptr


        ! module extern variables

        SUBROUTINE kgen_read_externs_grid_class(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_grid(gr, kgen_unit)
        END SUBROUTINE kgen_read_externs_grid_class

        SUBROUTINE kgen_read_grid(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(grid), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%nz
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%nz **", var%nz
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%zm, kgen_unit, printvar=printvar//"%zm")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%zm, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%zt, kgen_unit, printvar=printvar//"%zt")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%zt, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%invrs_dzm, kgen_unit, printvar=printvar//"%invrs_dzm")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%invrs_dzm, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%invrs_dzt, kgen_unit, printvar=printvar//"%invrs_dzt")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%invrs_dzt, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%dzm, kgen_unit, printvar=printvar//"%dzm")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%dzm, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim1_ptr(var%dzt, kgen_unit, printvar=printvar//"%dzt")
            ELSE
                CALL kgen_read_real_core_rknd_dim1_ptr(var%dzt, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim2_ptr(var%weights_zm2zt, kgen_unit, printvar=printvar//"%weights_zm2zt")
            ELSE
                CALL kgen_read_real_core_rknd_dim2_ptr(var%weights_zm2zt, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_real_core_rknd_dim2_ptr(var%weights_zt2zm, kgen_unit, printvar=printvar//"%weights_zt2zm")
            ELSE
                CALL kgen_read_real_core_rknd_dim2_ptr(var%weights_zt2zm, kgen_unit)
            END IF
        END SUBROUTINE
        RECURSIVE SUBROUTINE kgen_verify_grid(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(grid), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_integer("nz", dtype_check_status, var%nz, ref_var%nz)
            CALL kgen_verify_real_core_rknd_dim1_ptr("zm", dtype_check_status, var%zm, ref_var%zm)
            CALL kgen_verify_real_core_rknd_dim1_ptr("zt", dtype_check_status, var%zt, ref_var%zt)
            CALL kgen_verify_real_core_rknd_dim1_ptr("invrs_dzm", dtype_check_status, var%invrs_dzm, ref_var%invrs_dzm)
            CALL kgen_verify_real_core_rknd_dim1_ptr("invrs_dzt", dtype_check_status, var%invrs_dzt, ref_var%invrs_dzt)
            CALL kgen_verify_real_core_rknd_dim1_ptr("dzm", dtype_check_status, var%dzm, ref_var%dzm)
            CALL kgen_verify_real_core_rknd_dim1_ptr("dzt", dtype_check_status, var%dzt, ref_var%dzt)
            CALL kgen_verify_real_core_rknd_dim2_ptr("weights_zm2zt", dtype_check_status, var%weights_zm2zt, ref_var%weights_zm2zt)
            CALL kgen_verify_real_core_rknd_dim2_ptr("weights_zt2zm", dtype_check_status, var%weights_zt2zm, ref_var%weights_zt2zm)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
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

            SUBROUTINE kgen_verify_real_core_rknd_dim2_ptr( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=core_rknd), intent(in), DIMENSION(:,:), POINTER :: var, ref_var
                real(KIND=core_rknd) :: nrmsdiff, rmsdiff
                real(KIND=core_rknd), allocatable, DIMENSION(:,:) :: temp, temp2
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
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2)))
                
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
            END SUBROUTINE kgen_verify_real_core_rknd_dim2_ptr

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

        FUNCTION redirect_interpolated_azmk(azt, k)
! Description:
! Calls the appropriate corresponding function based on l_cubic_temp
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE model_flags, ONLY: l_quintic_poly_interp
            USE model_flags, ONLY: l_cubic_interp
! Variable(s)
            USE constants_clubb, ONLY: fstdout
! Variable
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
            INTEGER, intent(in) :: k
! Vertical level index
! Return Variable
            REAL(KIND=core_rknd) :: redirect_interpolated_azmk
! Variable when interp. to momentum levels
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
        END FUNCTION redirect_interpolated_azmk
!=============================================================================

        FUNCTION redirect_interpolated_azm(azt)
! Description:
! Calls the appropriate corresponding function based on l_cubic_temp
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE model_flags, ONLY: l_quintic_poly_interp
            USE model_flags, ONLY: l_cubic_interp
! Variable(s)
            USE constants_clubb, ONLY: fstdout
! Variable
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
! Return Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: redirect_interpolated_azm
! Variable when interp. to momentum levels
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
        END FUNCTION redirect_interpolated_azm
!=============================================================================

        FUNCTION redirect_interpolated_aztk(azt, k)
! Description:
! Calls the appropriate corresponding function based on l_cubic_temp
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE model_flags, ONLY: l_quintic_poly_interp
            USE model_flags, ONLY: l_cubic_interp
! Variable(s)
            USE constants_clubb, ONLY: fstdout
! Variable
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
            INTEGER, intent(in) :: k
! Vertical level index
! Return Variable
            REAL(KIND=core_rknd) :: redirect_interpolated_aztk
! Variable when interp. to momentum levels
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
        END FUNCTION redirect_interpolated_aztk
!=============================================================================

        FUNCTION redirect_interpolated_azt(azt)
! Description:
! Calls the appropriate corresponding function based on l_cubic_temp
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE model_flags, ONLY: l_quintic_poly_interp
            USE model_flags, ONLY: l_cubic_interp
! Variable(s)
            USE constants_clubb, ONLY: fstdout
! Variable
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
! Return Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: redirect_interpolated_azt
! Variable when interp. to momentum levels
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
        END FUNCTION redirect_interpolated_azt
!=============================================================================

        pure FUNCTION linear_interpolated_azm(azt)
! Description:
! Function to interpolate a variable located on the thermodynamic grid
! levels (azt) to the momentum grid levels (azm).  This function inputs the
! entire azt array and outputs the results as an azm array.  The
! formulation used is compatible with a stretched (unevenly-spaced) grid.
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE interpolation, ONLY: linear_interp_factor
! Procedure(s)
            IMPLICIT NONE
! Input Variable
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
! Return Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: linear_interpolated_azm
! Variable when interp. to momentum levels
! Local Variable
            INTEGER :: k ! Grid level loop index
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
    end forall ! k = 1 : gr%nz-1 : 1 ! k = 1 : gr%nz-1 : 1
    return
        END FUNCTION linear_interpolated_azm
!=============================================================================

        pure FUNCTION linear_interpolated_azmk(azt, k)
! Description:
! Function to interpolate a variable located on the thermodynamic grid
! levels (azt) to the momentum grid levels (azm).  This function outputs the
! value of azm at a single grid level (k) after interpolating using values
! of azt at two grid levels.  The formulation used is compatible with a
! stretched (unevenly-spaced) grid.
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE interpolation, ONLY: linear_interp_factor
! Procedure(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
            INTEGER, intent(in) :: k
! Vertical level index
! Return Variable
            REAL(KIND=core_rknd) :: linear_interpolated_azmk
! Variable when interp. to momentum levels
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
        END FUNCTION linear_interpolated_azmk
!=============================================================================

        FUNCTION cubic_interpolated_azm(azt)
! Description:
! Function to interpolate a variable located on the thermodynamic grid
! levels (azt) to the momentum grid levels (azm).  This function outputs the
! value of azt at a all grid levels using Steffen's monotonic cubic
! interpolation implemented by Tak Yamaguchi.
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Return Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: cubic_interpolated_azm
! Local Variable(s)
            REAL(KIND=core_rknd), dimension(gr%nz) :: tmp
! This is needed for variables that self-reference
            INTEGER :: k
! ---- Begin Code ----
    do k = 1, gr%nz 
      tmp(k) = cubic_interpolated_azmk( azt, k )
    end do
    cubic_interpolated_azm = tmp
    return
        END FUNCTION cubic_interpolated_azm
!=============================================================================

        FUNCTION cubic_interpolated_azmk(azt, k)
! Description:
! Function to interpolate a variable located on the thermodynamic grid
! levels (azt) to the momentum grid levels (azm).  This function outputs the
! value of azm at a single grid level (k) using Steffen's monotonic cubic
! interpolation implemented by Tak Yamaguchi.
!-----------------------------------------------------------------------
            USE interpolation, ONLY: mono_cubic_interp
! Procedure(s)
            USE clubb_precision, ONLY: core_rknd
! Constant(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
            INTEGER, intent(in) :: k
! Return Variable
            REAL(KIND=core_rknd) :: cubic_interpolated_azmk
! Local Variable(s)
            INTEGER :: km1
            INTEGER :: kp1
            INTEGER :: kp2
            INTEGER :: k00
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
    else if ( k == gr%nz ) then ! Extrapolation ! Extrapolation
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
        END FUNCTION cubic_interpolated_azmk
!=============================================================================

!=============================================================================

        pure FUNCTION linear_interpolated_azt(azm)
! Description:
! Function to interpolate a variable located on the momentum grid levels
! (azm) to the thermodynamic grid levels (azt).  This function inputs the
! entire azm array and outputs the results as an azt array.  The formulation
! used is compatible with a stretched (unevenly-spaced) grid.
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE interpolation, ONLY: linear_interp_factor
! Procedure(s)
            IMPLICIT NONE
! Input Variable
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azm
! Variable on momentum grid levels    [units vary]
! Output Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: linear_interpolated_azt
! Variable when interp. to thermodynamic levels
! Local Variable
            INTEGER :: k ! Grid level loop index
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
    end forall ! k = gr%nz : 2 : -1 ! k = gr%nz : 2 : -1
    return
        END FUNCTION linear_interpolated_azt
!=============================================================================

        pure FUNCTION linear_interpolated_aztk(azm, k)
! Description:
! Function to interpolate a variable located on the momentum grid levels
! (azm) to the thermodynamic grid levels (azt).  This function outputs the
! value of azt at a single grid level (k) after interpolating using values
! of azm at two grid levels.  The formulation used is compatible with a
! stretched (unevenly-spaced) grid.
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            USE interpolation, ONLY: linear_interp_factor
! Procedure(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azm
! Variable on momentum grid levels    [units vary]
            INTEGER, intent(in) :: k
! Vertical level index
! Return Variables
            REAL(KIND=core_rknd) :: linear_interpolated_aztk
! Variable when interp. to thermodynamic levs.
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
        END FUNCTION linear_interpolated_aztk
!=============================================================================

        FUNCTION cubic_interpolated_azt(azm)
! Description:
!   Function to interpolate a variable located on the momentum grid
!   levels (azm) to the thermodynamic grid levels (azt).  This function outputs the
!   value of azt at a all grid levels using Steffen's monotonic cubic
!   interpolation implemented by Tak Yamaguchi.
!
! References:
!   None
!-----------------------------------------------------------------------
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azm
! Return Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: cubic_interpolated_azt
! Local Variable(s)
            REAL(KIND=core_rknd), dimension(gr%nz) :: tmp
! This is needed for variables that self-reference
            INTEGER :: k
! ---- Begin Code ----
    do k = 1, gr%nz 
      tmp(k) = cubic_interpolated_aztk( azm, k )
    end do
    cubic_interpolated_azt = tmp
    return
        END FUNCTION cubic_interpolated_azt
!=============================================================================

        FUNCTION cubic_interpolated_aztk(azm, k)
! Description:
!   Function to interpolate a variable located on the momentum grid
!   levels (azm) to the thermodynamic grid levels (azt).  This function outputs the
!   value of azt at a single grid level (k) using Steffen's monotonic cubic
!   interpolation implemented by Tak Yamaguchi.
!
! References:
!   None
!-----------------------------------------------------------------------
            USE interpolation, ONLY: mono_cubic_interp
! Procedure(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variables
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azm
            INTEGER, intent(in) :: k
! Return Variable
            REAL(KIND=core_rknd) :: cubic_interpolated_aztk
! Local Variable(s)
            INTEGER :: km1
            INTEGER :: kp1
            INTEGER :: kp2
            INTEGER :: k00
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
    else if ( k == 1 ) then ! Extrapolation for the ghost point ! Extrapolation for the ghost point
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
        END FUNCTION cubic_interpolated_aztk
!=============================================================================

!=============================================================================

        pure FUNCTION gradzm(azm)
! Description:
! Function to compute the vertical derivative of a variable (azm) located on
! the momentum grid.  The results are returned in an array defined on the
! thermodynamic grid.
!-----------------------------------------------------------------------
!    use constants_clubb, only: &
!        zero  ! Constant(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variable
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azm
! Variable on momentum grid levels    [units vary]
! Return Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: gradzm
! Vertical derivative of azm    [units vary / m]
! Local Variable
            INTEGER :: k ! Grid level loop index
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
    end forall ! k = gr%nz : 2 : -1 ! k = gr%nz : 2 : -1
    return
        END FUNCTION gradzm
!=============================================================================

        pure FUNCTION gradzt(azt)
! Description:
! Function to compute the vertical derivative of a variable (azt) located on
! the thermodynamic grid.  The results are returned in an array defined on
! the momentum grid.
!-----------------------------------------------------------------------
!    use constants_clubb, only: &
!        zero  ! Constant(s)
            USE clubb_precision, ONLY: core_rknd
! Variable(s)
            IMPLICIT NONE
! Input Variable
            REAL(KIND=core_rknd), intent(in), dimension(gr%nz) :: azt
! Variable on thermodynamic grid levels    [units vary]
! Output Variable
            REAL(KIND=core_rknd), dimension(gr%nz) :: gradzt
! Vertical derivative of azt    [units vary / m]
! Local Variable
            INTEGER :: k ! Grid level loop index
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
    end forall ! k = 1 : gr%nz-1 : 1 ! k = 1 : gr%nz-1 : 1
    return
        END FUNCTION gradzt
!=============================================================================

!===============================================================================
    END MODULE grid_class
