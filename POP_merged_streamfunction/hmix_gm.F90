
! KGEN-generated Fortran source file
!
! Filename    : hmix_gm.F90
! Generated at: 2015-06-09 10:04:05
! KGEN version: 0.4.12



    MODULE hmix_gm
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: hmix_gm
        ! !DESCRIPTION:
        !  This module contains routines for computing horizontal mixing
        !  using the Gent-McWilliams eddy transport parameterization
        !  and isopycnal diffusion.
        ! !REVISION HISTORY:
        !  SVN:$Id: hmix_gm.F90 48834 2013-07-09 19:37:42Z mlevy@ucar.edu $
        ! !USES:
        USE blocks, only : block
        USE kinds_mod, only : int_kind
        USE kinds_mod, only : r8
        USE blocks, only : nx_block
        USE blocks, only : ny_block
        USE kinds_mod, only : log_kind
        USE constants, only : c0
        USE domain_size, only : km
        USE domain_size, only : max_blocks_clinic
        USE grid, only : kmt
        USE grid, only : dz
        USE constants, only : c2
        USE grid, only : dzwr
        USE constants, only : c1
        USE constants, only : eps
        USE constants, only : p25
        USE grid, only : zt
        !use hmix_gm_submeso_share
        USE hmix_gm_submeso_share, ONLY: slx
        USE hmix_gm_submeso_share, ONLY: sly
        IMPLICIT NONE
        PRIVATE
        PUBLIC kgen_read_externs_hmix_gm
        PUBLIC hdifft_gm

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_tlt_info
        END INTERFACE kgen_read

        PUBLIC kgen_verify
        INTERFACE kgen_verify
            MODULE PROCEDURE kgen_verify_tlt_info
        END INTERFACE kgen_verify

        ! !PUBLIC MEMBER FUNCTIONS:
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !
        !     variables to save from one call to next
        !
        !-----------------------------------------------------------------------
        INTEGER(KIND=int_kind), parameter :: kbt = 2
        INTEGER(KIND=int_kind), parameter :: ktp = 1
        ! refer to the top and bottom halves of a
        !  grid cell, respectively
        ! depth dependence for KAPPA
        ! west and south-shifted values of above
        ! Rossby radius
        ! inverse of Rossby radius
        ! beta plane approximation
        ! boundary layer depth
        ! work arrays for isopycnal mixing velocities
        ! vertical component of isopycnal velocities
        REAL(KIND=r8), dimension(:,:,:,:,:,:), allocatable :: ref_sf_slx
        REAL(KIND=r8), dimension(:,:,:,:,:,:), allocatable :: sf_slx
        REAL(KIND=r8), dimension(:,:,:,:,:,:), allocatable :: ref_sf_sly
        REAL(KIND=r8), dimension(:,:,:,:,:,:), allocatable :: sf_sly
        ! components of the merged streamfunction
        ! isopycnal slopes
        ! vertical flux
        ! compute spatially varying coefficients
        !  this time step?
        ! different tapering for two diffusivities
        ! specified choices for the isopycnal and
        !  thickness diffusion coefficients result in
        !  cancellation of some tensor elements
        ! if .true., use climatological N^2 data
        !  instead of model dependent N^2
        !-----------------------------------------------------------------------
        !
        !     KAPPA_LATERAL and KAPPA_VERTICAL are 2D and 3D arrays, respectively,
        !     containing the spatial variations of the isopycnal (KAPPA_ISOP)
        !     and thickness (KAPPA_THIC) diffusion coefficients. Except in kappa_type_eg,
        !     KAPPA_LATERAL has the actual diffusion coefficient values in cm^2/s,
        !     whereas KAPPA_VERTICAL is unitless. So, the total coefficients are
        !     constructed using
        !
        !      KAPPA_ISOP or KAPPA_THIC (:,:,:,k,bid) ~ KAPPA_LATERAL(:,:,bid)
        !                                             * KAPPA_VERTICAL(:,:,k,bid)
        !
        !     When kappa_type_eg, KAPPA_VERTICAL contains the isopycnal diffusivity
        !     coefficients in cm^2/s and KAPPA_LATERAL is not used!
        !
        !-----------------------------------------------------------------------
        REAL(KIND=r8), dimension(:,:,:,:,:), allocatable :: ref_kappa_thic
        REAL(KIND=r8), dimension(:,:,:,:,:), allocatable :: kappa_thic
        ! 3D isopycnal diffusion coefficient
        !  for top and bottom half of a grid cell
        ! 3D thickness diffusion coefficient
        !  for top and bottom half of a grid cell
        ! 3D horizontal diffusion coefficient
        !  for top and bottom half of a grid cell
        ! horizontal variation of KAPPA in cm^2/s
        ! vertical variation of KAPPA (unitless),
        !  e.g. normalized buoyancy frequency dependent
        !  profiles at the tracer grid points
        !  ( = N^2 / N_ref^2 ) OR a time-independent
        !  user-specified function
        ! N^2 defined at level interfaces
        ! bottom topography mask used with kappa_type_eg
        !-----------------------------------------------------------------------
        !
        !     GM specific options
        !
        !     kappa_freq = how often spatial variations of the diffusion
        !                  coefficients are computed. Same frequency is
        !                  used for both coefficients.
        !     slope_control = tanh function (Danabasoglu and McWilliams 1995) or
        !                     DM95 with replacement function to tanh or
        !                     slope clipping or
        !                     method of Gerdes et al (1991)
        !     diag_gm_bolus = .true. for diagnostic bolus velocity computation.
        !
        !-----------------------------------------------------------------------
        ! choice of KAPPA_ISOP
        ! choice of KAPPA_THIC
        ! frequency of KAPPA computations
        ! choice for slope control
        ! true for diagnostic bolus velocity computation
        !-----------------------------------------------------------------------
        !
        !     if use_const_ah_bkg_srfbl = .true., then the specified constant
        !     value of ah_bkg_srfbl is used as the "maximum" background horizontal
        !     diffusivity within the surface boundary layer. Otherwise,
        !     KAPPA_ISOP is utilized as this "maximum".
        !
        !-----------------------------------------------------------------------
        ! see above
        ! control for transition layer parameterization
        ! isopycnal diffusivity
        ! thickness (GM bolus) diffusivity
        ! backgroud horizontal diffusivity at k = KMT
        ! backgroud horizontal diffusivity within the
        !  surface boundary layer
        ! max. slope allowed for redi diffusion
        ! max. slope allowed for bolus transport
        !-----------------------------------------------------------------------
        !
        !     the following set of variables are used in Eden and Greatbatch
        !     (2008) KAPPA formulation. They are in the input namelist.
        !
        !-----------------------------------------------------------------------
        ! tuning parameter (unitless)
        ! (> 0) effective upper limit for inverse eddy
        !  time scale (unitless)
        ! minimum KAPPA (cm^2/s)
        ! maximum KAPPA (cm^2/s)
        !-----------------------------------------------------------------------
        !
        !     transition layer type variables
        !
        !-----------------------------------------------------------------------
        TYPE tlt_info
            REAL(KIND=r8), dimension(nx_block,ny_block,max_blocks_clinic) :: diabatic_depth, thickness, interior_depth
            ! depth of the diabatic region at the
            !  surface, i.e. mean mixed or boundary layer
            !  depth
            ! transition layer thickness
            ! depth at which the interior, adiabatic
            !  region starts, i.e.
            !   = TLT%DIABATIC_DEPTH + TLT%THICKNESS
            INTEGER(KIND=int_kind), dimension(nx_block,ny_block,max_blocks_clinic) :: k_level, ztw
            ! k level at or below which the interior,
            !  adiabatic region starts
            ! designates if the interior region
            !  starts below depth zt or zw.
            !  ( = 1 for zt, = 2 for zw )
        END TYPE tlt_info
        TYPE(tlt_info) :: ref_tlt
        TYPE(tlt_info) :: tlt
        ! transition layer thickness related fields
        !-----------------------------------------------------------------------
        !
        !     tavg ids for tavg diagnostics related to diffusivities and
        !     isopycnal velocities. Zonal and meridional refer here to logical
        !     space only.
        !
        !-----------------------------------------------------------------------
        ! zonal      isopycnal velocity
        ! meridional isopycnal velocity
        ! vertical   isopycnal velocity
        ! isopycnal  diffusion coefficient
        ! thickness  diffusion coefficient
        ! horizontal diffusion coefficient
        ! depth of the diabatic region at the surface
        ! transition layer thickness
        ! depth at which the interior region starts
        ! vertically-integrated T eddy-induced
        !  advection tendency
        ! vertically-integrated S eddy-induced
        !  advection tendency
        ! heat flux tendency in grid-y direction
        !  due to eddy-induced velocity
        ! salt flux tendency in grid-y direction
        !  due to eddy-induced velocity
        !-----------------------------------------------------------------------
        !
        !  timers
        !
        !-----------------------------------------------------------------------
        ! main n loop
        !EOC
        !***********************************************************************
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_r8_dim6_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4,idx5,idx6
                INTEGER, DIMENSION(2,6) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    READ(UNIT = kgen_unit) kgen_bound(1, 4)
                    READ(UNIT = kgen_unit) kgen_bound(2, 4)
                    READ(UNIT = kgen_unit) kgen_bound(1, 5)
                    READ(UNIT = kgen_unit) kgen_bound(2, 5)
                    READ(UNIT = kgen_unit) kgen_bound(1, 6)
                    READ(UNIT = kgen_unit) kgen_bound(2, 6)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, &
kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1, &
kgen_bound(2, 5) - kgen_bound(1, 5) + 1, kgen_bound(2, 6) - kgen_bound(1, 6) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim6_alloc

            SUBROUTINE kgen_read_real_r8_dim5_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4,idx5
                INTEGER, DIMENSION(2,5) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    READ(UNIT = kgen_unit) kgen_bound(1, 4)
                    READ(UNIT = kgen_unit) kgen_bound(2, 4)
                    READ(UNIT = kgen_unit) kgen_bound(1, 5)
                    READ(UNIT = kgen_unit) kgen_bound(2, 5)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1, kgen_bound(2, 5) - kgen_bound(1, 5) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim5_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_hmix_gm(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_real_r8_dim6_alloc(sf_slx, kgen_unit)
            CALL kgen_read_real_r8_dim6_alloc(sf_sly, kgen_unit)
            CALL kgen_read_real_r8_dim5_alloc(kappa_thic, kgen_unit)
            CALL kgen_read_tlt_info(tlt, kgen_unit)
        END SUBROUTINE kgen_read_externs_hmix_gm

        SUBROUTINE kgen_read_tlt_info(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(tlt_info), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%diabatic_depth
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%diabatic_depth **", var%diabatic_depth
            END IF
            READ(UNIT=kgen_unit) var%thickness
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%thickness **", var%thickness
            END IF
            READ(UNIT=kgen_unit) var%interior_depth
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%interior_depth **", var%interior_depth
            END IF
            READ(UNIT=kgen_unit) var%k_level
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%k_level **", var%k_level
            END IF
            READ(UNIT=kgen_unit) var%ztw
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%ztw **", var%ztw
            END IF
        END SUBROUTINE
        SUBROUTINE kgen_verify_tlt_info(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(tlt_info), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_real_r8_dim3("diabatic_depth", dtype_check_status, var%diabatic_depth, ref_var%diabatic_depth)
            CALL kgen_verify_real_r8_dim3("thickness", dtype_check_status, var%thickness, ref_var%thickness)
            CALL kgen_verify_real_r8_dim3("interior_depth", dtype_check_status, var%interior_depth, ref_var%interior_depth)
            CALL kgen_verify_integer_int_kind_dim3("k_level", dtype_check_status, var%k_level, ref_var%k_level)
            CALL kgen_verify_integer_int_kind_dim3("ztw", dtype_check_status, var%ztw, ref_var%ztw)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
            SUBROUTINE kgen_verify_real_r8_dim3( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:,:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:,:) :: temp, temp2
                integer :: n
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
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                
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
            END SUBROUTINE kgen_verify_real_r8_dim3

            SUBROUTINE kgen_verify_integer_int_kind_dim3( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer(KIND=int_kind), intent(in), DIMENSION(:,:,:) :: var, ref_var
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
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                    end if
                
                    check_status%numFatal = check_status%numFatal+1
                END IF
            END SUBROUTINE kgen_verify_integer_int_kind_dim3

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_gm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: hdifft_gm
        ! !INTERFACE:

        SUBROUTINE hdifft_gm(this_block, kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            ! !DESCRIPTION:
            !  Gent-McWilliams eddy transport parameterization
            !  and isopycnal diffusion.
            !
            !  This routine must be called successively with k = 1,2,3,...
            !
            ! !REVISION HISTORY:
            !  same as module
            ! !INPUT PARAMETERS:
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            ! depth level index
            ! tracers at all vertical levels
            !   at mixing time level
            ! U,V  at all vertical levels
            !   at mixing time level
            ! tavg id for east face diffusive flux of tracer
            ! tavg id for north face diffusive flux of tracer
            ! tavg id for bottom face diffusive flux of tracer
            TYPE(block), intent(in) :: this_block
            ! block info for this sub block
            ! !OUTPUT PARAMETERS:
            ! diffusion+bolus advection for nth tracer at level k
            !EOP
            !BOC
            !-----------------------------------------------------------------------
            !
            !     local variables
            !
            !-----------------------------------------------------------------------
            ! dummy loop counters
            ! array indices
            ! local block address for this sub block
            ! Dz(rho)
            ! absolute value of slope
            ! local work space
            ! local work space
            ! ocean mask
            ! tapering factors
            ! work arrays for isopycnal mixing velocities
            ! horizontal components of isopycnal velocities
            ! fluxes across east, north faces
            !-----------------------------------------------------------------------
            !
            !     initialize various quantities
            !
            !-----------------------------------------------------------------------
                    tolerance = 1.E-14
                    CALL kgen_init_check(check_status, tolerance)
                    ! Not kernel driver input

                    CALL kgen_read_real_r8_dim6_alloc(ref_sf_slx, kgen_unit)
                    CALL kgen_read_real_r8_dim6_alloc(ref_sf_sly, kgen_unit)
                    CALL kgen_read_real_r8_dim5_alloc(ref_kappa_thic, kgen_unit)
                    CALL kgen_read_tlt_info(ref_tlt, kgen_unit)


                    ! call to kernel
          call merged_streamfunction ( this_block )
                    ! kernel verification for output variables
                    CALL kgen_verify_real_r8_dim6_alloc( "sf_slx", check_status, sf_slx, ref_sf_slx)
                    CALL kgen_verify_real_r8_dim6_alloc( "sf_sly", check_status, sf_sly, ref_sf_sly)
                    CALL kgen_verify_real_r8_dim5_alloc( "kappa_thic", check_status, kappa_thic, ref_kappa_thic)
                    CALL kgen_verify_tlt_info( "tlt", check_status, tlt, ref_tlt)
                    CALL kgen_print_check("merged_streamfunction", check_status)
                    CALL system_clock(start_clock, rate_clock)
                    DO kgen_intvar=1,10
                        CALL merged_streamfunction(this_block)
                    END DO
                    CALL system_clock(stop_clock, rate_clock)
                    WRITE(*,*)
                    PRINT *, "Elapsed time (sec): ", (stop_clock - start_clock)/REAL(rate_clock*10)
            ! end of k==1 if statement
            !-----------------------------------------------------------------------
            !
            !     calculate effective vertical diffusion coefficient
            !     NOTE: it is assumed that VDC has been set before this
            !           in vmix_coeffs or something similar.
            !
            !     Dz(VDC * Dz(T)) where D is derivative rather than difference
            !     VDC = (Az(dz*Ax(KAPPA*HYX*SLX**2)) + Az(dz*Ay(KAPPA*HXY*SLY**2)))*
            !           dzw/TAREA
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     check if some horizontal diffusion needs to be added to the
            !     bottom half of the bottom cell
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     combine isopycnal and horizontal diffusion coefficients
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     start loop over tracers
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     diagnostic computation of the bolus velocities
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     update remaining bottom-face fields to top-face fields for next
            !     pass
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     compute isopycnal diffusion cfl diagnostics if required
            !
            !-----------------------------------------------------------------------
            !-----------------------------------------------------------------------
            !
            !     accumulate time average if necessary; testing is internal to
            !       accumulate_tavg_field
            !
            !-----------------------------------------------------------------------
            ! mix_pass ne 1
            !-----------------------------------------------------------------------
            !EOC
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_r8_dim6_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4,idx5,idx6
                INTEGER, DIMENSION(2,6) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    READ(UNIT = kgen_unit) kgen_bound(1, 4)
                    READ(UNIT = kgen_unit) kgen_bound(2, 4)
                    READ(UNIT = kgen_unit) kgen_bound(1, 5)
                    READ(UNIT = kgen_unit) kgen_bound(2, 5)
                    READ(UNIT = kgen_unit) kgen_bound(1, 6)
                    READ(UNIT = kgen_unit) kgen_bound(2, 6)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, &
kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1, &
kgen_bound(2, 5) - kgen_bound(1, 5) + 1, kgen_bound(2, 6) - kgen_bound(1, 6) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim6_alloc

            SUBROUTINE kgen_read_real_r8_dim5_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4,idx5
                INTEGER, DIMENSION(2,5) :: kgen_bound

                READ(UNIT = kgen_unit) is_true

                IF ( is_true ) THEN
                    READ(UNIT = kgen_unit) kgen_bound(1, 1)
                    READ(UNIT = kgen_unit) kgen_bound(2, 1)
                    READ(UNIT = kgen_unit) kgen_bound(1, 2)
                    READ(UNIT = kgen_unit) kgen_bound(2, 2)
                    READ(UNIT = kgen_unit) kgen_bound(1, 3)
                    READ(UNIT = kgen_unit) kgen_bound(2, 3)
                    READ(UNIT = kgen_unit) kgen_bound(1, 4)
                    READ(UNIT = kgen_unit) kgen_bound(2, 4)
                    READ(UNIT = kgen_unit) kgen_bound(1, 5)
                    READ(UNIT = kgen_unit) kgen_bound(2, 5)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1, kgen_bound(2, 5) - kgen_bound(1, 5) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim5_alloc

            SUBROUTINE kgen_read_real_r8_dim3(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:) :: var
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
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim3

            SUBROUTINE kgen_read_integer_int_kind_dim3(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                integer(KIND=int_kind), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:) :: var
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
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_integer_int_kind_dim3


        ! verify subroutines
            SUBROUTINE kgen_verify_real_r8_dim6_alloc( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:,:,:,:,:) :: temp, temp2
                integer :: n
                IF ( ALLOCATED(var) ) THEN
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
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4),SIZE(var,dim=5),SIZE(var,dim=6)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4),SIZE(var,dim=5),SIZE(var,dim=6)))
                
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
            END SUBROUTINE kgen_verify_real_r8_dim6_alloc

            SUBROUTINE kgen_verify_real_r8_dim5_alloc( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:,:,:,:), ALLOCATABLE :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:,:,:,:) :: temp, temp2
                integer :: n
                IF ( ALLOCATED(var) ) THEN
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
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4),SIZE(var,dim=5)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4),SIZE(var,dim=5)))
                
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
            END SUBROUTINE kgen_verify_real_r8_dim5_alloc

            SUBROUTINE kgen_verify_tlt_info( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                type(check_t) :: dtype_check_status
                TYPE(tlt_info), intent(in) :: var, ref_var

                check_status%numTotal = check_status%numTotal + 1
                CALL kgen_init_check(dtype_check_status)
                CALL kgen_verify_real_r8_dim3("diabatic_depth", dtype_check_status, var%diabatic_depth, ref_var%diabatic_depth)
                CALL kgen_verify_real_r8_dim3("thickness", dtype_check_status, var%thickness, ref_var%thickness)
                CALL kgen_verify_real_r8_dim3("interior_depth", dtype_check_status, var%interior_depth, ref_var%interior_depth)
                CALL kgen_verify_integer_int_kind_dim3("k_level", dtype_check_status, var%k_level, ref_var%k_level)
                CALL kgen_verify_integer_int_kind_dim3("ztw", dtype_check_status, var%ztw, ref_var%ztw)
                IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                    check_status%numIdentical = check_status%numIdentical + 1
                ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                    check_status%numFatal = check_status%numFatal + 1
                ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                    check_status%numWarning = check_status%numWarning + 1
                END IF
            END SUBROUTINE kgen_verify_tlt_info

            SUBROUTINE kgen_verify_real_r8_dim3( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:,:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:,:) :: temp, temp2
                integer :: n
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
                    allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                    allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                
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
            END SUBROUTINE kgen_verify_real_r8_dim3

            SUBROUTINE kgen_verify_integer_int_kind_dim3( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer(KIND=int_kind), intent(in), DIMENSION(:,:,:) :: var, ref_var
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
                    if(check_status%verboseLevel > 0) then
                        WRITE(*,*)
                        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                    end if
                
                    check_status%numFatal = check_status%numFatal+1
                END IF
            END SUBROUTINE kgen_verify_integer_int_kind_dim3

        END SUBROUTINE hdifft_gm
        !***********************************************************************
        !BOP
        ! !IROUTINE: kappa_lon_lat_vmhs
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: kappa_eg
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: kappa_lon_lat_hdgr
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: kappa_lon_lat_dradius
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: buoyancy_frequency_dependent_profile
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: transition_layer
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: merged_streamfunction
        ! !INTERFACE:

        SUBROUTINE merged_streamfunction(this_block)
            ! !DESCRIPTION:
            !  Construct a merged streamfunction that has the appropriate
            !  behavior in the surface diabatic region, transition layer, and
            !  adiabatic interior
            !
            ! !REVISION HISTORY:
            !  same as module
            ! !INPUT PARAMETERS:
            TYPE(block), intent(in) :: this_block
            ! block info for this sub block
            !EOP
            !BOC
            !-----------------------------------------------------------------------
            !
            !     local variables
            !
            !-----------------------------------------------------------------------
            INTEGER(KIND=int_kind) :: k, kk, bid
            ! loop indices
            ! local block address for this sub block
            REAL(KIND=r8), dimension(nx_block,ny_block,2) :: work1, work2, work3, work4
            ! work arrays
            REAL(KIND=r8), dimension(nx_block,ny_block) :: work2_next, work4_next
            ! WORK2 or WORK4 at next level
            REAL(KIND=r8), dimension(nx_block,ny_block) :: work5, work6, work7
            ! more work arrays
            LOGICAL(KIND=log_kind), dimension(nx_block,ny_block) :: lmask
            ! flag
            REAL(KIND=r8), dimension(2) :: reference_depth
            ! zt or zw
            !-----------------------------------------------------------------------
            !
            !     initialize various quantities
            !
            !-----------------------------------------------------------------------
      bid = this_block%local_id
      SF_SLX(:,:,:,:,:,bid) = c0
      SF_SLY(:,:,:,:,:,bid) = c0
      WORK1 = c0
      WORK2 = c0
      WORK3 = c0
      WORK4 = c0
      WORK5 = c0
      WORK6 = c0
      WORK7 = c0
      WORK2_NEXT = c0
      WORK4_NEXT = c0
            !-----------------------------------------------------------------------
            !
            !     compute the interior streamfunction and its first derivative at the
            !     INTERIOR_DEPTH level. WORK1 and WORK2 contain the streamfunction
            !     and its first derivative, respectively, for the zonal component
            !     for the east and west sides of a grid cell. WORK3 and WORK4 are
            !     the corresponding fields for the meridional component for the
            !     north and south sides of a grid cell. Note that these definitions
            !     include a "dz". Also, the first derivative computations assume
            !     that the streamfunctions are located in the middle of the top or
            !     bottom half of a grid cell, hence a factor of two in WORK2 and
            !     WORK4 calculations.
            !
            !-----------------------------------------------------------------------
      do k=1,km-1
        do kk=1,2
          LMASK = TLT%K_LEVEL(:,:,bid) == k  .and.            &
                  TLT%K_LEVEL(:,:,bid) < KMT(:,:,bid)  .and.  &
                  TLT%ZTW(:,:,bid) == 1
          where ( LMASK ) 
            WORK1(:,:,kk) =  KAPPA_THIC(:,:,kbt,k,bid)  &
                           * SLX(:,:,kk,kbt,k,bid) * dz(k)
            WORK2(:,:,kk) = c2 * dzwr(k) * ( WORK1(:,:,kk)            &
              - KAPPA_THIC(:,:,ktp,k+1,bid) * SLX(:,:,kk,ktp,k+1,bid) &
                                            * dz(k+1) )
            WORK2_NEXT = c2 * ( &
              KAPPA_THIC(:,:,ktp,k+1,bid) * SLX(:,:,kk,ktp,k+1,bid) - &
              KAPPA_THIC(:,:,kbt,k+1,bid) * SLX(:,:,kk,kbt,k+1,bid) )
            WORK3(:,:,kk) =  KAPPA_THIC(:,:,kbt,k,bid)  &
                           * SLY(:,:,kk,kbt,k,bid) * dz(k)
            WORK4(:,:,kk) = c2 * dzwr(k) * ( WORK3(:,:,kk)            &
              - KAPPA_THIC(:,:,ktp,k+1,bid) * SLY(:,:,kk,ktp,k+1,bid) &
                                            * dz(k+1) )
            WORK4_NEXT = c2 * ( &
              KAPPA_THIC(:,:,ktp,k+1,bid) * SLY(:,:,kk,ktp,k+1,bid) - &
              KAPPA_THIC(:,:,kbt,k+1,bid) * SLY(:,:,kk,kbt,k+1,bid) )
                    END WHERE 
          where ( LMASK .and. abs(WORK2_NEXT) < abs(WORK2(:,:,kk)) ) &
            WORK2(:,:,kk) = WORK2_NEXT
          where ( LMASK .and. abs(WORK4_NEXT) < abs(WORK4(:,:,kk)) ) &
            WORK4(:,:,kk) = WORK4_NEXT
          LMASK = TLT%K_LEVEL(:,:,bid) == k  .and.           &
                  TLT%K_LEVEL(:,:,bid) < KMT(:,:,bid)  .and. &
                  TLT%ZTW(:,:,bid) == 2
          where ( LMASK )
            WORK1(:,:,kk) =  KAPPA_THIC(:,:,ktp,k+1,bid)     & 
                           * SLX(:,:,kk,ktp,k+1,bid)
            WORK2(:,:,kk) =  c2 * ( WORK1(:,:,kk)                 &
                           - ( KAPPA_THIC(:,:,kbt,k+1,bid)        &
                              * SLX(:,:,kk,kbt,k+1,bid) ) )
            WORK1(:,:,kk) = WORK1(:,:,kk) * dz(k+1)
            WORK3(:,:,kk) =  KAPPA_THIC(:,:,ktp,k+1,bid)     &
                           * SLY(:,:,kk,ktp,k+1,bid)
            WORK4(:,:,kk) =  c2 * ( WORK3(:,:,kk)                 &
                           - ( KAPPA_THIC(:,:,kbt,k+1,bid)        &
                              * SLY(:,:,kk,kbt,k+1,bid) ) )
            WORK3(:,:,kk) = WORK3(:,:,kk) * dz(k+1)
                    END WHERE 
          LMASK = LMASK .and. TLT%K_LEVEL(:,:,bid) + 1 < KMT(:,:,bid)
          if (k.lt.km-1) then ! added to avoid out of bounds access ! added to avoid out of bounds access
            where ( LMASK )
              WORK2_NEXT = c2 * dzwr(k+1) * ( &
                KAPPA_THIC(:,:,kbt,k+1,bid) * SLX(:,:,kk,kbt,k+1,bid) * dz(k+1) - &
                KAPPA_THIC(:,:,ktp,k+2,bid) * SLX(:,:,kk,ktp,k+2,bid) * dz(k+2))
              WORK4_NEXT = c2 * dzwr(k+1) * ( &
                KAPPA_THIC(:,:,kbt,k+1,bid) * SLY(:,:,kk,kbt,k+1,bid) * dz(k+1) - &
                KAPPA_THIC(:,:,ktp,k+2,bid) * SLY(:,:,kk,ktp,k+2,bid) * dz(k+2))
                        END WHERE 
          end if
          where ( LMASK .and. abs(WORK2_NEXT) < abs(WORK2(:,:,kk)) ) &
            WORK2(:,:,kk) = WORK2_NEXT
          where ( LMASK .and. abs(WORK4_NEXT) < abs(WORK4(:,:,kk)) ) &
            WORK4(:,:,kk) = WORK4_NEXT
        enddo
      enddo
            !-----------------------------------------------------------------------
            !
            !     compute the depth independent interpolation factors used in the
            !     linear and quadratic interpolations within the diabatic and
            !     transition regions, respectively.
            !
            !-----------------------------------------------------------------------
      WORK5 = c0
      where (KMT(:,:,bid) /= 0)
        WORK5(:,:) = c1 / ( c2 * TLT%DIABATIC_DEPTH(:,:,bid) &
                   + TLT%THICKNESS(:,:,bid) )
            END WHERE 
      WORK6 = c0
      where ((KMT(:,:,bid) /= 0) .AND. (TLT%THICKNESS(:,:,bid) > eps))
        WORK6(:,:) = WORK5(:,:) / TLT%THICKNESS(:,:,bid)
            END WHERE 
            !-----------------------------------------------------------------------
            !
            !     start of interpolation to construct the merged streamfunction
            !
            !-----------------------------------------------------------------------
      do k=1,km
        reference_depth(ktp) = zt(k) - p25 * dz(k)
        reference_depth(kbt) = zt(k) + p25 * dz(k)
        do kk=ktp,kbt
                    !-----------------------------------------------------------------------
                    !
                    !     diabatic region: use linear interpolation (in streamfunction)
                    !
                    !-----------------------------------------------------------------------
          where ( reference_depth(kk) <= TLT%DIABATIC_DEPTH(:,:,bid)  &
                  .and.  k <= KMT(:,:,bid) ) 
            SF_SLX(:,:,1,kk,k,bid) = reference_depth(kk) * WORK5  &
                  * ( c2 * WORK1(:,:,1) + TLT%THICKNESS(:,:,bid)  &
                     * WORK2(:,:,1) )
            SF_SLX(:,:,2,kk,k,bid) = reference_depth(kk) * WORK5  &
                  * ( c2 * WORK1(:,:,2) + TLT%THICKNESS(:,:,bid)  &
                     * WORK2(:,:,2) )
            SF_SLY(:,:,1,kk,k,bid) = reference_depth(kk) * WORK5  &
                  * ( c2 * WORK3(:,:,1) + TLT%THICKNESS(:,:,bid)  &
                     * WORK4(:,:,1) )
            SF_SLY(:,:,2,kk,k,bid) = reference_depth(kk) * WORK5  &
                  * ( c2 * WORK3(:,:,2) + TLT%THICKNESS(:,:,bid)  &
                     * WORK4(:,:,2) )
                    END WHERE 
                    !-----------------------------------------------------------------------
                    !
                    !     transition layer: use quadratic interpolation (in streamfunction)
                    !
                    !-----------------------------------------------------------------------
          where ( reference_depth(kk) > TLT%DIABATIC_DEPTH(:,:,bid)   &
            .and.  reference_depth(kk) <= TLT%INTERIOR_DEPTH(:,:,bid) &
            .and.  k <= KMT(:,:,bid) )
            WORK7 = (TLT%DIABATIC_DEPTH(:,:,bid)  &
                     - reference_depth(kk))**2
            SF_SLX(:,:,1,kk,k,bid) = - WORK7 * WORK6            &
                * ( WORK1(:,:,1) + TLT%INTERIOR_DEPTH(:,:,bid)  &
                   * WORK2(:,:,1) )                             &
               + reference_depth(kk) * WORK5                    &
                * ( c2 * WORK1(:,:,1) + TLT%THICKNESS(:,:,bid)  &
                   * WORK2(:,:,1) )
            SF_SLX(:,:,2,kk,k,bid) = - WORK7 * WORK6            &
                * ( WORK1(:,:,2) + TLT%INTERIOR_DEPTH(:,:,bid)  &
                   * WORK2(:,:,2) )                             &
               + reference_depth(kk) * WORK5                    &
                * ( c2 * WORK1(:,:,2) + TLT%THICKNESS(:,:,bid)  &
                   * WORK2(:,:,2) )
            SF_SLY(:,:,1,kk,k,bid) = - WORK7 * WORK6            &
                * ( WORK3(:,:,1) + TLT%INTERIOR_DEPTH(:,:,bid)  &
                   * WORK4(:,:,1) )                             &
               + reference_depth(kk) * WORK5                    &
                * ( c2 * WORK3(:,:,1) + TLT%THICKNESS(:,:,bid)  &
                   * WORK4(:,:,1) )
            SF_SLY(:,:,2,kk,k,bid) = - WORK7 * WORK6            &
                * ( WORK3(:,:,2) + TLT%INTERIOR_DEPTH(:,:,bid)  &
                   * WORK4(:,:,2) )                             &
               + reference_depth(kk) * WORK5                    &
                * ( c2 * WORK3(:,:,2) + TLT%THICKNESS(:,:,bid)  &
                   * WORK4(:,:,2) )
                    END WHERE 
                    !-----------------------------------------------------------------------
                    !
                    !     interior, adiabatic region: no interpolation is needed. note that
                    !     "dzw" is introduced here, too, for consistency.
                    !
                    !-----------------------------------------------------------------------
          where ( reference_depth(kk) > TLT%INTERIOR_DEPTH(:,:,bid)  & 
                  .and.  k <= KMT(:,:,bid) )
            SF_SLX(:,:,1,kk,k,bid) =  KAPPA_THIC(:,:,kk,k,bid)  &
                              * SLX(:,:,1,kk,k,bid) * dz(k)
            SF_SLX(:,:,2,kk,k,bid) =  KAPPA_THIC(:,:,kk,k,bid)  &
                              * SLX(:,:,2,kk,k,bid) * dz(k)
            SF_SLY(:,:,1,kk,k,bid) =  KAPPA_THIC(:,:,kk,k,bid)  &
                              * SLY(:,:,1,kk,k,bid) * dz(k)
            SF_SLY(:,:,2,kk,k,bid) =  KAPPA_THIC(:,:,kk,k,bid)  &
                              * SLY(:,:,2,kk,k,bid) * dz(k)
                    END WHERE 
        enddo  ! end of kk-loop ! end of kk-loop
      enddo    ! end of k-loop ! end of k-loop
            !-----------------------------------------------------------------------
            !EOC
        END SUBROUTINE merged_streamfunction
        !***********************************************************************
        !BOP
        ! !IROUTINE: apply_vertical_profile_to_isop_hor_diff
        ! !INTERFACE:

        !***********************************************************************
    END MODULE hmix_gm
