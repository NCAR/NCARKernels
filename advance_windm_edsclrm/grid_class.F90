
! KGEN-generated Fortran source file
!
! Filename    : grid_class.F90
! Generated at: 2015-10-21 08:59:10
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
        PUBLIC gr, grid
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




! Vertical derivative functions


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

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!=============================================================================

!===============================================================================
    END MODULE grid_class
