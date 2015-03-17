
! KGEN-generated Fortran source file
!
! Filename    : prim_advance_mod.F90
! Generated at: 2015-03-14 21:47:57
! KGEN version: 0.4.5



    MODULE prim_advance_mod
    USE derivative_mod, ONLY : kgen_read_mod2 => kgen_read
        ! _EXTERNAL
        IMPLICIT NONE
        PRIVATE
        INTEGER, PARAMETER :: kgen_dp = selected_real_kind(15, 307)
        PUBLIC compute_and_apply_rhs
        type, public  ::  check_t
            logical :: Passed
            integer :: numFatal
            integer :: numTotal
            integer :: numIdentical
            integer :: numWarning
            integer :: VerboseLevel
            real(kind=kgen_dp) :: tolerance
        end type check_t
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        subroutine kgen_init_check(check,tolerance)
          type(check_t), intent(inout) :: check
          real(kind=kgen_dp), intent(in), optional :: tolerance
           check%Passed   = .TRUE.
           check%numFatal = 0
           check%numWarning = 0
           check%numTotal = 0
           check%numIdentical = 0
           check%VerboseLevel = 1
           if(present(tolerance)) then
             check%tolerance = tolerance
           else
              check%tolerance = 1.E-14
           endif
        end subroutine kgen_init_check
        subroutine kgen_print_check(kname, check)
           character(len=*) :: kname
           type(check_t), intent(in) ::  check
           write (*,*)
           write (*,*) TRIM(kname),' KGENPrtCheck: Tolerance for normalized RMS: ',check%tolerance
           write (*,*) TRIM(kname),' KGENPrtCheck: Number of variables checked: ',check%numTotal
           write (*,*) TRIM(kname),' KGENPrtCheck: Number of Identical results: ',check%numIdentical
           write (*,*) TRIM(kname),' KGENPrtCheck: Number of warnings detected: ',check%numWarning
           write (*,*) TRIM(kname),' KGENPrtCheck: Number of fatal errors detected: ', check%numFatal
           if (check%numFatal> 0) then
                write(*,*) TRIM(kname),' KGENPrtCheck: verification FAILED'
           else
                write(*,*) TRIM(kname),' KGENPrtCheck: verification PASSED'
           endif
        end subroutine kgen_print_check









        !
        ! phl notes: output is stored in first argument. Advances from 2nd argument using tendencies evaluated at 3rd rgument:
        ! phl: for offline winds use time at 3rd argument (same as rhs currently)
        !

        SUBROUTINE compute_and_apply_rhs(deriv, kgen_unit)
            ! ===================================
            ! compute the RHS, accumulate into u(np1) and apply DSS
            !
            !           u(np1) = u(nm1) + dt2*DSS[ RHS(u(n0)) ]
            !
            ! This subroutine is normally called to compute a leapfrog timestep
            ! but by adjusting np1,nm1,n0 and dt2, many other timesteps can be
            ! accomodated.  For example, setting nm1=np1=n0 this routine will
            ! take a forward euler step, overwriting the input with the output.
            !
            !    qn0 = timelevel used to access Qdp() in order to compute virtual Temperature
            !          qn0=-1 for the dry case
            !
            ! if  dt2<0, then the DSS'd RHS is returned in timelevel np1
            !
            ! Combining the RHS and DSS pack operation in one routine
            ! allows us to fuse these two loops for more cache reuse
            !
            ! Combining the dt advance and DSS unpack operation in one routine
            ! allows us to fuse these two loops for more cache reuse
            !
            ! note: for prescribed velocity case, velocity will be computed at
            ! "real_time", which should be the time of timelevel n0.
            !
            !
            ! ===================================
            USE kinds, ONLY: real_kind
            USE dimensions_mod, ONLY: np
            USE dimensions_mod, ONLY: nlev
            USE derivative_mod, ONLY: vorticity_sphere
            USE derivative_mod, ONLY: derivative_t
            IMPLICIT NONE
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            TYPE(derivative_t), intent(in) :: deriv
            ! weighting for eta_dot_dpdn mean flux
            ! local
            ! surface pressure for current tiime level
            ! half level vertical velocity on p-grid
            ! temporary field
            ! generic gradient storage
            !
            !
            ! v.grad(T)
            ! kinetic energy + PHI term
            ! lat-lon coord version
            ! gradient(p - p_met)
            REAL(KIND=real_kind), dimension(np,np,nlev) :: vort
            REAL(KIND=real_kind) :: ref_vort(np,np,nlev) ! vorticity
            ! pressure
            ! delta pressure
            ! inverse of delta pressure
            ! temperature vertical advection
            ! v.grad(p)
            ! half level pressures on p-grid
            ! velocity vertical advection
            REAL(KIND=real_kind), dimension(np,np,2,nlev,3) :: temp_elem_v !instead of elem%state%v
            REAL(KIND=real_kind), dimension(2,2,np,np) :: temp_elem_d !instead of elem%D
            REAL(KIND=real_kind), dimension(np,np) :: temp_elem_rmetdet !insetad of elem%rmetdet
            INTEGER :: k
            INTEGER :: ref_k
            !JMD  call t_barrierf('sync_compute_and_apply_rhs', hybrid%par%comm)
                    tolerance = 1.E-14
                    CALL kgen_init_check(check_status, tolerance)
                    READ(UNIT=kgen_unit) vort
                    READ(UNIT=kgen_unit) temp_elem_v
                    READ(UNIT=kgen_unit) temp_elem_d
                    READ(UNIT=kgen_unit) temp_elem_rmetdet
                    READ(UNIT=kgen_unit) k

                    READ(UNIT=kgen_unit) ref_vort
                    READ(UNIT=kgen_unit) ref_k

                    ! call to kernel
                    vort(:, :, k) = vorticity_sphere(temp_elem_v, deriv, temp_elem_d(:, :, :, :), temp_elem_rmetdet(:, :))
                    ! kernel verification for output variables
                    CALL kgen_verify_real_real_kind_dim3( "vort", check_status, vort, ref_vort)
                    CALL kgen_verify_integer( "k", check_status, k, ref_k)
                    CALL kgen_print_check("vorticity_sphere", check_status)
                    CALL system_clock(start_clock, rate_clock)
                    DO kgen_intvar=1,10
                        vort(:, :, k) = vorticity_sphere(temp_elem_v, deriv, temp_elem_d(:, :, :, :), temp_elem_rmetdet(:, :))
                    END DO
                    CALL system_clock(stop_clock, rate_clock)
                    WRITE(*,*)
                    PRINT *, "Elapsed time (sec): ", (stop_clock - start_clock)/REAL(rate_clock*10)
            ! =============================================================
            ! Insert communications here: for shared memory, just a single
            ! sync is required
            ! =============================================================
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_real_kind_dim3(var, kgen_unit)
                INTEGER, INTENT(IN) :: kgen_unit
                real(KIND=real_kind), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3
                INTEGER, DIMENSION(2,3) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1))
                    READ(UNIT = kgen_unit) var
                END IF
            END SUBROUTINE kgen_read_real_real_kind_dim3


        subroutine kgen_verify_logical(varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            logical, intent(in) :: var, ref_var
        
            check_status%numTotal = check_status%numTotal + 1
            IF ( var .eqv. ref_var ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                endif
            ELSE
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    if(check_status%verboseLevel > 2) then
                        WRITE(*,*) "KERNEL: ", var
                        WRITE(*,*) "REF.  : ", ref_var
                    endif
                endif
                check_status%numFatal = check_status%numFatal + 1
            END IF
        end subroutine
        
        subroutine kgen_verify_integer(varname, check_status, var, ref_var)
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
                    endif
                endif
                check_status%numFatal = check_status%numFatal + 1
            END IF
        end subroutine
        
        subroutine kgen_verify_real(varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real, intent(in) :: var, ref_var
        
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
                    endif
                endif
                check_status%numFatal = check_status%numFatal + 1
            END IF
        end subroutine
        
        subroutine kgen_verify_character(varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            character(*), intent(in) :: var, ref_var
        
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
        end subroutine

        subroutine kgen_verify_real_real_kind_dim3(varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(kind=real_kind), intent(in), dimension(:,:,:) :: var, ref_var
            !real(kind=real_kind), intent(in), dimension(:,:,:) :: ref_var
            real(kind=real_kind) :: nrmsdiff, rmsdiff
            real(kind=real_kind), allocatable :: temp(:,:,:), temp2(:,:,:)
            integer :: n
        
        
            IF ( .TRUE. ) THEN
                check_status%numTotal = check_status%numTotal + 1
                allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
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
                    n = count(var/=ref_var)
                    where(ref_var .NE. 0)
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
                END IF
                deallocate(temp,temp2)
            END IF
        end subroutine

        END SUBROUTINE compute_and_apply_rhs

        !TRILINOS


    END MODULE prim_advance_mod
