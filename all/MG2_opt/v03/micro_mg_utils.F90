
! KGEN-generated Fortran source file
!
! Filename    : micro_mg_utils.F90
! Generated at: 2015-10-08 11:52:39
! KGEN version: 0.5.2



    MODULE micro_mg_utils
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !--------------------------------------------------------------------------
        !
        ! This module contains process rates and utility functions used by the MG
        ! microphysics.
        !
        ! Original MG authors: Andrew Gettelman, Hugh Morrison
        ! Contributions from: Peter Caldwell, Xiaohong Liu and Steve Ghan
        !
        ! Separated from MG 1.5 by B. Eaton.
        ! Separated module switched to MG 2.0 and further changes by S. Santos.
        !
        ! for questions contact Hugh Morrison, Andrew Gettelman
        ! e-mail: morrison@ucar.edu, andrew@ucar.edu
        !
        !--------------------------------------------------------------------------
        !
        ! List of required external functions that must be supplied:
        !   gamma --> standard mathematical gamma function (if gamma is an
        !       intrinsic, define HAVE_GAMMA_INTRINSICS)
        !
        !--------------------------------------------------------------------------
        !
        ! Constants that must be specified in the "init" method (module variables):
        !
        ! kind            kind of reals (to verify correct linkage only) -
        ! gravit          acceleration due to gravity                    m s-2
        ! rair            dry air gas constant for air                   J kg-1 K-1
        ! rh2o            gas constant for water vapor                   J kg-1 K-1
        ! cpair           specific heat at constant pressure for dry air J kg-1 K-1
        ! tmelt           temperature of melting point for water         K
        ! latvap          latent heat of vaporization                    J kg-1
        ! latice          latent heat of fusion                          J kg-1
        !
        !--------------------------------------------------------------------------
<<<<<<< HEAD
!        USE shr_spfn_mod, ONLY: gamma => shr_spfn_gamma
=======
        USE shr_spfn_mod, ONLY: gamma => shr_spfn_gamma
        USE shr_kind_mod, ONLY: rkind_comp, rkind_io
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        IMPLICIT NONE
        PRIVATE
        PUBLIC size_dist_param_liq, rising_factorial, size_dist_param_basic, kk2000_liq_autoconversion, ice_autoconversion, &
        immersion_freezing, contact_freezing, snow_self_aggregation, accrete_cloud_water_snow, secondary_ice_production, &
        accrete_rain_snow, heterogeneous_rain_freezing, accrete_cloud_water_rain, self_collection_rain, accrete_cloud_ice_snow, &
        evaporate_sublimate_precip, bergeron_process_snow, ice_deposition_sublimation, avg_diameter
<<<<<<< HEAD
        PUBLIC avg_diameter_func
        ! 8 byte real and integer
        INTEGER, parameter, public :: r8 = selected_real_kind(12)
=======
        ! 8 byte real and integer
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        INTEGER, parameter, public :: i8 = selected_int_kind(18)
        PUBLIC mghydrometeorprops
        TYPE mghydrometeorprops
            ! Density (kg/m^3)
<<<<<<< HEAD
            REAL(KIND=r8) :: rho
=======
            REAL(KIND=rkind_comp) :: rho
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            ! Information for size calculations.
            ! Basic calculation of mean size is:
            !     lambda = (shape_coef*nic/qic)^(1/eff_dim)
            ! Then lambda is constrained by bounds.
<<<<<<< HEAD
            REAL(KIND=r8) :: eff_dim
            REAL(KIND=r8) :: shape_coef
            REAL(KIND=r8) :: lambda_bounds(2)
            ! Minimum average particle mass (kg).
            ! Limit is applied at the beginning of the size distribution calculations.
            REAL(KIND=r8) :: min_mean_mass
=======
            REAL(KIND=rkind_comp) :: eff_dim
            REAL(KIND=rkind_comp) :: shape_coef
            REAL(KIND=rkind_comp) :: lambda_bounds(2)
            ! Minimum average particle mass (kg).
            ! Limit is applied at the beginning of the size distribution calculations.
            REAL(KIND=rkind_comp) :: min_mean_mass
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        END TYPE mghydrometeorprops

        TYPE(mghydrometeorprops), public :: mg_liq_props
        TYPE(mghydrometeorprops), public :: mg_ice_props
        TYPE(mghydrometeorprops), public :: mg_rain_props
        TYPE(mghydrometeorprops), public :: mg_snow_props

        interface size_dist_param_liq
          module procedure size_dist_param_liq_vect
          module procedure size_dist_param_liq_line
        end interface
        interface size_dist_param_basic
<<<<<<< HEAD
          module procedure size_dist_param_basic_line
        end interface
        public :: size_dist_param_basic_vec
=======
          module procedure size_dist_param_basic_vect
          module procedure size_dist_param_basic_line
        end interface
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a

        !=================================================
        ! Public module parameters (mostly for MG itself)
        !=================================================
        ! Pi to 20 digits; more than enough to reach the limit of double precision.
<<<<<<< HEAD
        REAL(KIND=r8), parameter, public :: pi = 3.14159265358979323846_r8
        ! "One minus small number": number near unity for round-off issues.
        REAL(KIND=r8), parameter, public :: omsm   = 1._r8 - 1.e-5_r8
        ! Smallest mixing ratio considered in microphysics.
        REAL(KIND=r8), parameter, public :: qsmall = 1.e-18_r8
        ! minimum allowed cloud fraction
        REAL(KIND=r8), parameter, public :: mincld = 0.0001_r8
        REAL(KIND=r8), parameter, public :: rhosn = 250._r8 ! bulk density snow
        REAL(KIND=r8), parameter, public :: rhoi = 500._r8 ! bulk density ice
        REAL(KIND=r8), parameter, public :: rhow = 1000._r8 ! bulk density liquid
        REAL(KIND=r8), parameter, public :: rhows = 917._r8 ! bulk density water solid
        ! fall speed parameters, V = aD^b (V is in m/s)
        ! droplets
        REAL(KIND=r8), parameter, public :: bc = 2._r8
        ! snow
        REAL(KIND=r8), parameter, public :: as = 11.72_r8
        REAL(KIND=r8), parameter, public :: bs = 0.41_r8
        ! cloud ice
        REAL(KIND=r8), parameter, public :: ai = 700._r8
        REAL(KIND=r8), parameter, public :: bi = 1._r8
        ! rain
        REAL(KIND=r8), parameter, public :: ar = 841.99667_r8
        REAL(KIND=r8), parameter, public :: br = 0.8_r8
        ! mass of new crystal due to aerosol freezing and growth (kg)
        REAL(KIND=r8), parameter, public :: mi0 = 4._r8/3._r8*pi*rhoi*(10.e-6_r8)**3
=======
        REAL(KIND=rkind_comp), parameter, public :: pi = 3.14159265358979323846_rkind_comp
        ! "One minus small number": number near unity for round-off issues.
        REAL(KIND=rkind_comp), parameter, public :: omsm   = 1._rkind_comp - 1.e-5_rkind_comp
        ! Smallest mixing ratio considered in microphysics.
        REAL(KIND=rkind_comp), parameter, public :: qsmall = 1.e-18_rkind_comp
        ! minimum allowed cloud fraction
        REAL(KIND=rkind_comp), parameter, public :: mincld = 0.0001_rkind_comp
        REAL(KIND=rkind_comp), parameter, public :: rhosn = 250._rkind_comp ! bulk density snow
        REAL(KIND=rkind_comp), parameter, public :: rhoi = 500._rkind_comp ! bulk density ice
        REAL(KIND=rkind_comp), parameter, public :: rhow = 1000._rkind_comp ! bulk density liquid
        REAL(KIND=rkind_comp), parameter, public :: rhows = 917._rkind_comp ! bulk density water solid
        ! fall speed parameters, V = aD^b (V is in m/s)
        ! droplets
        REAL(KIND=rkind_comp), parameter, public :: bc = 2._rkind_comp
        ! snow
        REAL(KIND=rkind_comp), parameter, public :: as = 11.72_rkind_comp
        REAL(KIND=rkind_comp), parameter, public :: bs = 0.41_rkind_comp
        ! cloud ice
        REAL(KIND=rkind_comp), parameter, public :: ai = 700._rkind_comp
        REAL(KIND=rkind_comp), parameter, public :: bi = 1._rkind_comp
        ! rain
        REAL(KIND=rkind_comp), parameter, public :: ar = 841.99667_rkind_comp
        REAL(KIND=rkind_comp), parameter, public :: br = 0.8_rkind_comp
        ! mass of new crystal due to aerosol freezing and growth (kg)
        REAL(KIND=rkind_comp), parameter, public :: mi0 = 4._rkind_comp/3._rkind_comp*pi*rhoi*(10.e-6_rkind_comp)**3
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        !=================================================
        ! Private module parameters
        !=================================================
        ! Signaling NaN bit pattern that represents a limiter that's turned off.
        INTEGER(KIND=i8), parameter :: limiter_off = int(z'7FF1111111111111', i8)
        ! alternate threshold used for some in-cloud mmr
<<<<<<< HEAD
        REAL(KIND=r8), parameter :: icsmall = 1.e-8_r8
=======
        REAL(KIND=rkind_comp), parameter :: icsmall = 1.e-8_rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        ! particle mass-diameter relationship
        ! currently we assume spherical particles for cloud ice/snow
        ! m = cD^d
        ! exponent
        ! Bounds for mean diameter for different constituents.
        ! Minimum average mass of particles.
        ! ventilation parameters
        ! for snow
<<<<<<< HEAD
        REAL(KIND=r8), parameter :: f1s = 0.86_r8
        REAL(KIND=r8), parameter :: f2s = 0.28_r8
        ! for rain
        REAL(KIND=r8), parameter :: f1r = 0.78_r8
        REAL(KIND=r8), parameter :: f2r = 0.308_r8
        ! collection efficiencies
        ! aggregation of cloud ice and snow
        REAL(KIND=r8), parameter :: eii = 0.5_r8
        ! immersion freezing parameters, bigg 1953
        REAL(KIND=r8), parameter :: bimm = 100._r8
        REAL(KIND=r8), parameter :: aimm = 0.66_r8
        ! Mass of each raindrop created from autoconversion.
        REAL(KIND=r8), parameter :: droplet_mass_25um = 4._r8/3._r8*pi*rhow*(25.e-6_r8)**3
=======
        REAL(KIND=rkind_comp), parameter :: f1s = 0.86_rkind_comp
        REAL(KIND=rkind_comp), parameter :: f2s = 0.28_rkind_comp
        ! for rain
        REAL(KIND=rkind_comp), parameter :: f1r = 0.78_rkind_comp
        REAL(KIND=rkind_comp), parameter :: f2r = 0.308_rkind_comp
        ! collection efficiencies
        ! aggregation of cloud ice and snow
        REAL(KIND=rkind_comp), parameter :: eii = 0.5_rkind_comp
        ! immersion freezing parameters, bigg 1953
        REAL(KIND=rkind_comp), parameter :: bimm = 100._rkind_comp
        REAL(KIND=rkind_comp), parameter :: aimm = 0.66_rkind_comp
        ! Mass of each raindrop created from autoconversion.
        REAL(KIND=rkind_comp), parameter :: droplet_mass_25um = 4._rkind_comp/3._rkind_comp*pi*rhow*(25.e-6_rkind_comp)**3
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        !=========================================================
        ! Constants set in initialization
        !=========================================================
        ! Set using arguments to micro_mg_init
<<<<<<< HEAD
        REAL(KIND=r8) :: rv ! water vapor gas constant
        REAL(KIND=r8) :: cpp ! specific heat of dry air
        REAL(KIND=r8) :: tmelt ! freezing point of water (K)
        ! latent heats of:
        REAL(KIND=r8) :: xxlv ! vaporization
        ! freezing
        REAL(KIND=r8) :: xxls ! sublimation
        ! additional constants to help speed up code
        REAL(KIND=r8) :: gamma_bs_plus3
        REAL(KIND=r8) :: gamma_half_br_plus5
        REAL(KIND=r8) :: gamma_half_bs_plus5
=======
        REAL(KIND=rkind_comp) :: rv ! water vapor gas constant
        REAL(KIND=rkind_comp) :: cpp ! specific heat of dry air
        REAL(KIND=rkind_comp) :: tmelt ! freezing point of water (K)
        ! latent heats of:
        REAL(KIND=rkind_comp) :: xxlv ! vaporization
        ! freezing
        REAL(KIND=rkind_comp) :: xxls ! sublimation
        ! additional constants to help speed up code
        REAL(KIND=rkind_comp) :: gamma_bs_plus3
        REAL(KIND=rkind_comp) :: gamma_half_br_plus5
        REAL(KIND=rkind_comp) :: gamma_half_bs_plus5
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        !=========================================================
        ! Utilities that are cheaper if the compiler knows that
        ! some argument is an integer.
        !=========================================================

        INTERFACE rising_factorial
<<<<<<< HEAD
            MODULE PROCEDURE rising_factorial_v8
            MODULE PROCEDURE rising_factorial_r8
            MODULE PROCEDURE rising_factorial_integer
            MODULE PROCEDURE rising_factorial_vec_integer
        END INTERFACE rising_factorial

        INTERFACE var_coef
            MODULE PROCEDURE var_coef_r8
            MODULE PROCEDURE var_coef_v8
            MODULE PROCEDURE var_coef_integer
            MODULE PROCEDURE var_coef_vec_integer
=======
            MODULE PROCEDURE rising_factorial_real
            MODULE PROCEDURE rising_factorial_integer
        END INTERFACE rising_factorial

        INTERFACE var_coef
            MODULE PROCEDURE var_coef_rkind_comp
            MODULE PROCEDURE var_coef_integer
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        END INTERFACE var_coef
        !==========================================================================
            PUBLIC kgen_read_externs_micro_mg_utils

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_mghydrometeorprops
        END INTERFACE kgen_read

        PUBLIC kgen_verify
        INTERFACE kgen_verify
            MODULE PROCEDURE kgen_verify_mghydrometeorprops
        END INTERFACE kgen_verify

        CONTAINS

        ! write subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_micro_mg_utils(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
<<<<<<< HEAD
            READ(UNIT=kgen_unit) tmelt
            READ(UNIT=kgen_unit) gamma_bs_plus3
            READ(UNIT=kgen_unit) xxlv
            READ(UNIT=kgen_unit) rv
            READ(UNIT=kgen_unit) cpp
            READ(UNIT=kgen_unit) gamma_half_br_plus5
            READ(UNIT=kgen_unit) xxls
            READ(UNIT=kgen_unit) gamma_half_bs_plus5
=======
            real(kind=rkind_io) :: rtmp
            READ(UNIT=kgen_unit) rtmp; tmelt = REAL(rtmp,kind=rkind_comp) ! REAL
            READ(UNIT=kgen_unit) rtmp; gamma_bs_plus3  = REAL(rtmp,kind=rkind_comp)  ! REAL
            READ(UNIT=kgen_unit) rtmp; xxlv  = REAL(rtmp,kind=rkind_comp)             ! REAL
            READ(UNIT=kgen_unit) rtmp; rv  = REAL(rtmp,kind=rkind_comp)              ! REAL
            READ(UNIT=kgen_unit) rtmp; cpp  = REAL(rtmp,kind=rkind_comp)                 ! REAL
            READ(UNIT=kgen_unit) rtmp; gamma_half_br_plus5 = REAL(rtmp,kind=rkind_comp) ! REAL
            READ(UNIT=kgen_unit) rtmp; xxls  = REAL(rtmp,kind=rkind_comp)                ! REAL
            READ(UNIT=kgen_unit) rtmp; gamma_half_bs_plus5  = REAL(rtmp,kind=rkind_comp) ! REAL
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            CALL kgen_read_mghydrometeorprops(mg_liq_props, kgen_unit)
            CALL kgen_read_mghydrometeorprops(mg_ice_props, kgen_unit)
            CALL kgen_read_mghydrometeorprops(mg_rain_props, kgen_unit)
            CALL kgen_read_mghydrometeorprops(mg_snow_props, kgen_unit)
        END SUBROUTINE kgen_read_externs_micro_mg_utils

        SUBROUTINE kgen_read_mghydrometeorprops(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(mghydrometeorprops), INTENT(out) :: var
<<<<<<< HEAD
            READ(UNIT=kgen_unit) var%rho
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rho **", var%rho
            END IF
            READ(UNIT=kgen_unit) var%eff_dim
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%eff_dim **", var%eff_dim
            END IF
            READ(UNIT=kgen_unit) var%shape_coef
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%shape_coef **", var%shape_coef
            END IF
            READ(UNIT=kgen_unit) var%lambda_bounds
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%lambda_bounds **", var%lambda_bounds
            END IF
            READ(UNIT=kgen_unit) var%min_mean_mass
=======
            real(kind=rkind_io) :: rtmp,rtmp2(2)
            READ(UNIT=kgen_unit) rtmp; var%rho  = REAL(rtmp,kind=rkind_comp) ! REAL
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%rho **", var%rho
            END IF
            READ(UNIT=kgen_unit) rtmp; var%eff_dim  = REAL(rtmp,kind=rkind_comp)  ! REAL
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%eff_dim **", var%eff_dim
            END IF
            READ(UNIT=kgen_unit) rtmp; var%shape_coef  = REAL(rtmp,kind=rkind_comp)  ! REAL
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%shape_coef **", var%shape_coef
            END IF
            READ(UNIT=kgen_unit) rtmp2; var%lambda_bounds  = REAL(rtmp2,kind=rkind_comp)  ! REAL
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%lambda_bounds **", var%lambda_bounds
            END IF
            READ(UNIT=kgen_unit) rtmp; var%min_mean_mass  = REAL(rtmp,kind=rkind_comp)  ! REAL
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            IF ( PRESENT(printvar) ) THEN
                print *, "** KGEN DEBUG: " // printvar // "%min_mean_mass **", var%min_mean_mass
            END IF
        END SUBROUTINE
        RECURSIVE SUBROUTINE kgen_verify_mghydrometeorprops(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(mghydrometeorprops), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
<<<<<<< HEAD
            CALL kgen_verify_real_r8("rho", dtype_check_status, var%rho, ref_var%rho)
            CALL kgen_verify_real_r8("eff_dim", dtype_check_status, var%eff_dim, ref_var%eff_dim)
            CALL kgen_verify_real_r8("shape_coef", dtype_check_status, var%shape_coef, ref_var%shape_coef)
            CALL kgen_verify_real_r8_dim1("lambda_bounds", dtype_check_status, var%lambda_bounds, ref_var%lambda_bounds)
            CALL kgen_verify_real_r8("min_mean_mass", dtype_check_status, var%min_mean_mass, ref_var%min_mean_mass)
=======
            CALL kgen_verify_real_rkind_comp("rho", dtype_check_status, var%rho, ref_var%rho)
            CALL kgen_verify_real_rkind_comp("eff_dim", dtype_check_status, var%eff_dim, ref_var%eff_dim)
            CALL kgen_verify_real_rkind_comp("shape_coef", dtype_check_status, var%shape_coef, ref_var%shape_coef)
            CALL kgen_verify_real_rkind_comp_dim1("lambda_bounds", dtype_check_status, var%lambda_bounds, ref_var%lambda_bounds)
            CALL kgen_verify_real_rkind_comp("min_mean_mass", dtype_check_status, var%min_mean_mass, ref_var%min_mean_mass)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
<<<<<<< HEAD
            SUBROUTINE kgen_verify_real_r8( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in) :: var, ref_var
=======
            SUBROUTINE kgen_verify_real_rkind_comp( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=rkind_comp), intent(in) :: var, ref_var
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
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
<<<<<<< HEAD
            END SUBROUTINE kgen_verify_real_r8

            SUBROUTINE kgen_verify_real_r8_dim1( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:) :: temp, temp2
=======
            END SUBROUTINE kgen_verify_real_rkind_comp

            SUBROUTINE kgen_verify_real_rkind_comp_dim1( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=rkind_comp), intent(in), DIMENSION(:) :: var, ref_var
                real(KIND=rkind_comp) :: nrmsdiff, rmsdiff
                real(KIND=rkind_comp), allocatable, DIMENSION(:) :: temp, temp2
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
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
<<<<<<< HEAD
            END SUBROUTINE kgen_verify_real_r8_dim1
=======
            END SUBROUTINE kgen_verify_real_rkind_comp_dim1
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a

        !==========================================================================
        ! Initialize module variables.
        !
        ! "kind" serves no purpose here except to check for unlikely linking
        ! issues; always pass in the kind for a double precision real.
        !
        ! "errstring" is the only output; it is blank if there is no error, or set
        ! to a message if there is an error.
        !
        ! Check the list at the top of this module for descriptions of all other
        ! arguments.

        ! Constructor for a constituent property object.

        !========================================================================
        !FORMULAS
        !========================================================================
        ! Use gamma function to implement rising factorial extended to the reals.
<<<<<<< HEAD
        SUBROUTINE rising_factorial_r8(x, n, res)
            REAL(KIND=r8), intent(in)  :: x
            REAL(KIND=r8), intent(in)  :: n
            REAL(KIND=r8), intent(out) :: res
            res = gamma(x+n)/gamma(x)
        END SUBROUTINE rising_factorial_r8

        ! Use gamma function to implement rising factorial extended to the reals.
        SUBROUTINE rising_factorial_v8(x, n, res,vlen)
            INTEGER, intent(in)        :: vlen
            REAL(KIND=r8), intent(in)  :: x(vlen)
            REAL(KIND=r8), intent(in)  :: n
            REAL(KIND=r8), intent(out) :: res(vlen)
            INTEGER :: i
            do i=1,vlen
               res(i) = gamma(x(i)+n)/gamma(x(i))
            enddo
        END SUBROUTINE rising_factorial_v8

        ! Rising factorial can be performed much cheaper if n is a small integer.
        SUBROUTINE rising_factorial_integer(x, n, res)
            REAL(KIND=r8), intent(in) :: x
            INTEGER, intent(in) :: n
            REAL(KIND=r8), intent(out) :: res
            INTEGER :: i
            REAL(KIND=r8) :: factor
            res = 1._r8
            factor = x
            do i = 1, n
              res = res * factor
              factor = factor + 1._r8
            end do
        END SUBROUTINE rising_factorial_integer

        SUBROUTINE rising_factorial_vec_integer(x, n, res,vlen)
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in) :: x(vlen)
            INTEGER, intent(in) :: n
            REAL(KIND=r8), intent(out) :: res(vlen)
            INTEGER :: i,j
            REAL(KIND=r8) :: factor
            do j=1,vlen
              res(j) = 1._r8
              factor = x(j)
              do i = 1, n
                res(j) = res(j) * factor
                factor = factor + 1._r8
              enddo
            enddo
        END SUBROUTINE rising_factorial_vec_integer

        ! Calculate correction due to latent heat for evaporation/sublimation

        FUNCTION calc_ab(t, qv, xxl) RESULT ( ab )
            REAL(KIND=r8), intent(in) :: t ! Temperature
            REAL(KIND=r8), intent(in) :: qv ! Saturation vapor pressure
            REAL(KIND=r8), intent(in) :: xxl ! Latent heat
            REAL(KIND=r8) :: ab
            REAL(KIND=r8) :: dqsdt
  dqsdt = xxl*qv / (rv * t**2)
  ab = 1._r8 + dqsdt*xxl/cpp
        END FUNCTION calc_ab
        ! get cloud droplet size distribution parameters

        SUBROUTINE size_dist_param_liq_line(props, qcic, ncic, rho, pgam, lamc)
            TYPE(mghydrometeorprops), intent(in) :: props
            REAL(KIND=r8), intent(in) :: qcic
            REAL(KIND=r8), intent(inout) :: ncic
            REAL(KIND=r8), intent(in) :: rho
            REAL(KIND=r8), intent(out) :: pgam
            REAL(KIND=r8), intent(out) :: lamc
            REAL(KIND=r8) :: tmp
=======

        pure FUNCTION rising_factorial_real(x, n) RESULT ( res )
            REAL(KIND=rkind_comp), intent(in) :: x
            REAL(KIND=rkind_comp), intent(in) :: n
            REAL(KIND=rkind_comp) :: res
            res = gamma(x+n)/gamma(x)
        END FUNCTION rising_factorial_real
        ! Rising factorial can be performed much cheaper if n is a small integer.

        pure FUNCTION rising_factorial_integer(x, n) RESULT ( res )
            REAL(KIND=rkind_comp), intent(in) :: x
            INTEGER, intent(in) :: n
            REAL(KIND=rkind_comp) :: res
            INTEGER :: i
            REAL(KIND=rkind_comp) :: factor
            res = 1._rkind_comp
            factor = x
            do i = 1, n
              res = res * factor
              factor = factor + 1._rkind_comp
            end do
        END FUNCTION rising_factorial_integer
        ! Calculate correction due to latent heat for evaporation/sublimation

        elemental FUNCTION calc_ab(t, qv, xxl) RESULT ( ab )
            REAL(KIND=rkind_comp), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), intent(in) :: qv ! Saturation vapor pressure
            REAL(KIND=rkind_comp), intent(in) :: xxl ! Latent heat
            REAL(KIND=rkind_comp) :: ab
            REAL(KIND=rkind_comp) :: dqsdt
  dqsdt = xxl*qv / (rv * t**2)
  ab = 1._rkind_comp + dqsdt*xxl/cpp
        END FUNCTION calc_ab
        ! get cloud droplet size distribution parameters

        elemental SUBROUTINE size_dist_param_liq_line(props, qcic, ncic, rho, pgam, lamc)
            TYPE(mghydrometeorprops), intent(in) :: props
            REAL(KIND=rkind_comp), intent(in) :: qcic
            REAL(KIND=rkind_comp), intent(inout) :: ncic
            REAL(KIND=rkind_comp), intent(in) :: rho
            REAL(KIND=rkind_comp), intent(out) :: pgam
            REAL(KIND=rkind_comp), intent(out) :: lamc
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            TYPE(mghydrometeorprops) :: props_loc
  if (qcic > qsmall) then
                ! Local copy of properties that can be modified.
                ! (Elemental routines that operate on arrays can't modify scalar
                ! arguments.)
     props_loc = props
                ! Get pgam from fit to observations of martin et al. 1994
<<<<<<< HEAD
     pgam = 0.0005714_r8*1.e-6_r8*ncic*rho + 0.2714_r8
     pgam = 1._r8/(pgam**2) - 1._r8
     pgam = max(pgam, 2._r8)
                ! Set coefficient for use in size_dist_param_basic.
                ! The 3D case is so common and optimizable that we specialize it:
     if (props_loc%eff_dim == 3._r8) then
        ! props_loc%shape_coef = pi / 6._r8 * props_loc%rho * rising_factorial(pgam+1._r8, 3)
        call rising_factorial(pgam+1._r8,3,tmp)
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
     else
        ! props_loc%shape_coef = pi / 6._r8 * props_loc%rho * rising_factorial(pgam+1._r8, props_loc%eff_dim)
        call rising_factorial(pgam+1._r8,props_loc%eff_dim,tmp)
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
     end if
                ! Limit to between 2 and 50 microns mean size.
     props_loc%lambda_bounds = (pgam+1._r8)*1._r8/[50.e-6_r8, 2.e-6_r8]
=======
     pgam = 0.0005714_rkind_comp*1.e-6_rkind_comp*ncic*rho + 0.2714_rkind_comp
     pgam = 1._rkind_comp/(pgam**2) - 1._rkind_comp
     pgam = max(pgam, 2._rkind_comp)
                ! Set coefficient for use in size_dist_param_basic.
                ! The 3D case is so common and optimizable that we specialize it:
     if (props_loc%eff_dim == 3._rkind_comp) then
        props_loc%shape_coef = pi / 6._rkind_comp * props_loc%rho * &
             rising_factorial(pgam+1._rkind_comp, 3)
     else
        props_loc%shape_coef = pi / 6._rkind_comp * props_loc%rho * &
             rising_factorial(pgam+1._rkind_comp, props_loc%eff_dim)
     end if
                ! Limit to between 2 and 50 microns mean size.
     props_loc%lambda_bounds = (pgam+1._rkind_comp)*1._rkind_comp/[50.e-6_rkind_comp, 2.e-6_rkind_comp]
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
     call size_dist_param_basic(props_loc, qcic, ncic, lamc)
  else
                ! pgam not calculated in this case, so set it to a value likely to
                ! cause an error if it is accidentally used
                ! (gamma function undefined for negative integers)
<<<<<<< HEAD
     pgam = -100._r8
     lamc = 0._r8
=======
     pgam = -100._rkind_comp
     lamc = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
        END SUBROUTINE size_dist_param_liq_line
        ! get cloud droplet size distribution parameters

        SUBROUTINE size_dist_param_liq_vect(props, qcic, ncic, rho, pgam, lamc, mgncol)
            TYPE(mghydrometeorprops), intent(in) :: props
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=r8), dimension(mgncol), intent(inout) :: ncic
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho
            REAL(KIND=r8), dimension(mgncol), intent(out) :: pgam
            REAL(KIND=r8), dimension(mgncol), intent(out) :: lamc
            TYPE(mghydrometeorprops) :: props_loc
            INTEGER :: i
            REAL(KIND=r8) :: tmp
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(inout) :: ncic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: pgam
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: lamc
            TYPE(mghydrometeorprops) :: props_loc
            INTEGER :: i
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  props_loc = props
  do i=1,mgncol
  if (qcic(i) > qsmall) then
                ! Local copy of properties that can be modified.
                ! (Elemental routines that operate on arrays can't modify scalar
                ! arguments.)
                ! Get pgam from fit to observations of martin et al. 1994
<<<<<<< HEAD
     pgam(i) = 0.0005714_r8*1.e-6_r8*ncic(i)*rho(i) + 0.2714_r8
     pgam(i) = 1._r8/(pgam(i)**2) - 1._r8
     pgam(i) = max(pgam(i), 2._r8)
=======
     pgam(i) = 0.0005714_rkind_comp*1.e-6_rkind_comp*ncic(i)*rho(i) + 0.2714_rkind_comp
     pgam(i) = 1._rkind_comp/(pgam(i)**2) - 1._rkind_comp
     pgam(i) = max(pgam(i), 2._rkind_comp)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  endif
  enddo
  do i=1,mgncol
  if (qcic(i) > qsmall) then
                ! Set coefficient for use in size_dist_param_basic.
                ! The 3D case is so common and optimizable that we specialize it:
<<<<<<< HEAD
     if (props_loc%eff_dim == 3._r8) then
        ! props_loc%shape_coef = pi / 6._r8 * props_loc%rho * rising_factorial(pgam(i)+1._r8, 3)
        call rising_factorial(pgam(i)+1._r8,3,tmp)
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
     else
        ! props_loc%shape_coef = pi / 6._r8 * props_loc%rho * rising_factorial(pgam(i)+1._r8, props_loc%eff_dim)
        call rising_factorial(pgam(i)+1._r8, props_loc%eff_dim,tmp)
        props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
     end if
                ! Limit to between 2 and 50 microns mean size.
     props_loc%lambda_bounds(1) = (pgam(i)+1._r8)*1._r8/50.e-6_r8
     props_loc%lambda_bounds(2) = (pgam(i)+1._r8)*1._r8/2.e-6_r8
=======
     if (props_loc%eff_dim == 3._rkind_comp) then
        props_loc%shape_coef = pi / 6._rkind_comp * props_loc%rho * &
             rising_factorial(pgam(i)+1._rkind_comp, 3)
     else
        props_loc%shape_coef = pi / 6._rkind_comp * props_loc%rho * &
             rising_factorial(pgam(i)+1._rkind_comp, props_loc%eff_dim)
     end if
                ! Limit to between 2 and 50 microns mean size.
     props_loc%lambda_bounds(1) = (pgam(i)+1._rkind_comp)*1._rkind_comp/50.e-6_rkind_comp
     props_loc%lambda_bounds(2) = (pgam(i)+1._rkind_comp)*1._rkind_comp/2.e-6_rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
     call size_dist_param_basic(props_loc, qcic(i), ncic(i), lamc(i))
  endif
  enddo
  do i=1,mgncol
  if (qcic(i) <= qsmall) then
                ! pgam not calculated in this case, so set it to a value likely to
                ! cause an error if it is accidentally used
                ! (gamma function undefined for negative integers)
<<<<<<< HEAD
     pgam(i) = -100._r8
     lamc(i) = 0._r8
=======
     pgam(i) = -100._rkind_comp
     lamc(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE size_dist_param_liq_vect
        ! Basic routine for getting size distribution parameters.

<<<<<<< HEAD
        SUBROUTINE size_dist_param_basic_line(props, qic, nic, lam, n0)
            TYPE(mghydrometeorprops), intent(in) :: props
            REAL(KIND=r8), intent(in) :: qic
            REAL(KIND=r8), intent(inout) :: nic
            REAL(KIND=r8), intent(out) :: lam
            REAL(KIND=r8), intent(out), optional :: n0
=======
        elemental SUBROUTINE size_dist_param_basic_line(props, qic, nic, lam, n0)
            TYPE(mghydrometeorprops), intent(in) :: props
            REAL(KIND=rkind_comp), intent(in) :: qic
            REAL(KIND=rkind_comp), intent(inout) :: nic
            REAL(KIND=rkind_comp), intent(out) :: lam
            REAL(KIND=rkind_comp), intent(out), optional :: n0
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  if (qic > qsmall) then
                ! add upper limit to in-cloud number concentration to prevent
                ! numerical error
     if (limiter_is_on(props%min_mean_mass)) then
        nic = min(nic, qic / props%min_mean_mass)
     end if
                ! lambda = (c n/q)^(1/d)
<<<<<<< HEAD
     lam = (props%shape_coef * nic/qic)**(1._r8/props%eff_dim)
=======
     lam = (props%shape_coef * nic/qic)**(1._rkind_comp/props%eff_dim)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
                ! check for slope
                ! adjust vars
     if (lam < props%lambda_bounds(1)) then
        lam = props%lambda_bounds(1)
        nic = lam**(props%eff_dim) * qic/props%shape_coef
     else if (lam > props%lambda_bounds(2)) then
        lam = props%lambda_bounds(2)
        nic = lam**(props%eff_dim) * qic/props%shape_coef
     end if
  else
<<<<<<< HEAD
     lam = 0._r8
=======
     lam = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  if (present(n0)) n0 = nic * lam
        END SUBROUTINE size_dist_param_basic_line

<<<<<<< HEAD
        SUBROUTINE size_dist_param_basic_vec(props, qic, nic, lam, vlen, n0)
            TYPE(mghydrometeorprops), intent(in) :: props
            INTEGER,                          intent(in) :: vlen
            REAL(KIND=r8), dimension(vlen), intent(in) :: qic
            REAL(KIND=r8), dimension(vlen), intent(inout) :: nic
            REAL(KIND=r8), dimension(vlen), intent(out) :: lam
            REAL(KIND=r8), dimension(vlen), intent(out), optional :: n0
            INTEGER :: i
!NEC$ IVDEP
            do i=1,vlen
              if (qic(i) > qsmall) then
                ! add upper limit to in-cloud number concentration to prevent
                ! numerical error
                if (limiter_is_on(props%min_mean_mass)) then
                   nic(i) = min(nic(i), qic(i) / props%min_mean_mass)
                end if
                ! lambda = (c n/q)^(1/d)
                lam(i) = (props%shape_coef * nic(i)/qic(i))**(1._r8/props%eff_dim)
                ! check for slope
                ! adjust vars
                if (lam(i) < props%lambda_bounds(1)) then
                   lam(i) = props%lambda_bounds(1)
                   nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
                else if (lam(i) > props%lambda_bounds(2)) then
                   lam(i) = props%lambda_bounds(2)
                   nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
                end if
              else
                lam(i) = 0._r8
              end if
            enddo
            if (present(n0)) n0 = nic * lam
        END SUBROUTINE size_dist_param_basic_vec

        SUBROUTINE avg_diameter(q, n, rho_air, rho_sub, diam,vlen)
            ! Finds the average diameter of particles given their density, and
            ! mass/number concentrations in the air.
            ! Assumes that diameter follows an exponential distribution.
            INTEGER :: vlen
            REAL(KIND=r8), intent(in) :: q(vlen) ! mass mixing ratio
            REAL(KIND=r8), intent(in) :: n(vlen) ! number concentration (per volume)
            REAL(KIND=r8), intent(in) :: rho_air(vlen) ! local density of the air
            REAL(KIND=r8), intent(in) :: rho_sub       ! density of the particle substance
            REAL(KIND=r8), intent(out) :: diam(vlen)

            INTEGER :: i
            do i=1,vlen
               diam(i) = (pi * rho_sub * n(i)/(q(i)*rho_air(i)))**(-1._r8/3._r8)
            enddo
        END SUBROUTINE avg_diameter

        elemental real(r8) FUNCTION avg_diameter_func(q, n, rho_air, rho_sub)
            ! Finds the average diameter of particles given their density, and
            ! mass/number concentrations in the air.
            ! Assumes that diameter follows an exponential distribution.
            REAL(KIND=r8), intent(in) :: q ! mass mixing ratio
            REAL(KIND=r8), intent(in) :: n ! number concentration (per volume)
            REAL(KIND=r8), intent(in) :: rho_air ! local density of the air
            REAL(KIND=r8), intent(in) :: rho_sub ! density of the particle substance
  avg_diameter_func = (pi * rho_sub * n/(q*rho_air))**(-1._r8/3._r8)
        END FUNCTION avg_diameter_func


        SUBROUTINE var_coef_r8(relvar, a, res)
            ! Finds a coefficient for process rates based on the relative variance
            ! of cloud water.
            REAL(KIND=r8), intent(in)  :: relvar
            REAL(KIND=r8), intent(in)  :: a
            REAL(KIND=r8), intent(out) :: res
            REAL(KIND=r8) :: tmp
            call rising_factorial(relvar,a,tmp)
            res = tmp / relvar**a
        END SUBROUTINE var_coef_r8

        SUBROUTINE var_coef_v8(relvar, a, res,vlen)
            ! Finds a coefficient for process rates based on the relative variance
            ! of cloud water.
            INTEGER :: vlen 
            REAL(KIND=r8), intent(in)  :: relvar(vlen)
            REAL(KIND=r8), intent(in)  :: a
            REAL(KIND=r8), intent(out) :: res(vlen)
            INTEGER :: i
            REAL(KIND=r8) :: tmpA(vlen)
            call rising_factorial(relvar,a,tmpA,vlen)
            do i=1,vlen
               res(i) = tmpA(i)/relvar(i)**a
            enddo
        END SUBROUTINE var_coef_v8

        SUBROUTINE var_coef_integer(relvar, a, res)
            ! Finds a coefficient for process rates based on the relative variance
            ! of cloud water.
            REAL(KIND=r8), intent(in) :: relvar
            INTEGER, intent(in) :: a
            REAL(KIND=r8) :: res
            REAL(KIND=r8) :: tmp
            call rising_factorial(relvar,a,tmp)
            res = tmp / relvar**a
        END SUBROUTINE var_coef_integer


        SUBROUTINE var_coef_vec_integer(relvar, a, res,vlen)
            ! Finds a coefficient for process rates based on the relative variance
            ! of cloud water.
            INTEGER, intent(in) :: vlen
            REAL(KIND=r8), intent(in) :: relvar(vlen)
            INTEGER, intent(in) :: a
            REAL(KIND=r8), intent(out) :: res(vlen)
            INTEGER :: i
            REAL(KIND=r8) :: tmp(vlen)
            call rising_factorial(relvar, a,tmp,vlen)
            do i=1,vlen
               res(i) = tmp(i) / relvar(i)**a
            enddo
        END SUBROUTINE var_coef_vec_integer

=======
        SUBROUTINE size_dist_param_basic_vect(props, qic, nic, lam, mgncol, n0)
            TYPE(mghydrometeorprops), intent(in) :: props
            INTEGER,                          intent(in) :: mgncol
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(inout) :: nic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: lam
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out), optional :: n0
            INTEGER :: i
  do i=1,mgncol
  if (qic(i) > qsmall) then
                ! add upper limit to in-cloud number concentration to prevent
                ! numerical error
     if (limiter_is_on(props%min_mean_mass)) then
        nic(i) = min(nic(i), qic(i) / props%min_mean_mass)
     end if
                ! lambda = (c n/q)^(1/d)
     lam(i) = (props%shape_coef * nic(i)/qic(i))**(1._rkind_comp/props%eff_dim)
                ! check for slope
                ! adjust vars
     if (lam(i) < props%lambda_bounds(1)) then
        lam(i) = props%lambda_bounds(1)
        nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
     else if (lam(i) > props%lambda_bounds(2)) then
        lam(i) = props%lambda_bounds(2)
        nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
     end if
  else
     lam(i) = 0._rkind_comp
  end if
  enddo
  if (present(n0)) n0 = nic * lam
        END SUBROUTINE size_dist_param_basic_vect

        elemental real(rkind_comp) FUNCTION avg_diameter(q, n, rho_air, rho_sub)
            ! Finds the average diameter of particles given their density, and
            ! mass/number concentrations in the air.
            ! Assumes that diameter follows an exponential distribution.
            REAL(KIND=rkind_comp), intent(in) :: q ! mass mixing ratio
            REAL(KIND=rkind_comp), intent(in) :: n ! number concentration (per volume)
            REAL(KIND=rkind_comp), intent(in) :: rho_air ! local density of the air
            REAL(KIND=rkind_comp), intent(in) :: rho_sub ! density of the particle substance
  avg_diameter = (pi * rho_sub * n/(q*rho_air))**(-1._rkind_comp/3._rkind_comp)
        END FUNCTION avg_diameter

        elemental FUNCTION var_coef_rkind_comp(relvar, a) RESULT ( res )
            ! Finds a coefficient for process rates based on the relative variance
            ! of cloud water.
            REAL(KIND=rkind_comp), intent(in) :: relvar
            REAL(KIND=rkind_comp), intent(in) :: a
            REAL(KIND=rkind_comp) :: res
  res = rising_factorial(relvar, a) / relvar**a
        END FUNCTION var_coef_rkind_comp

        elemental FUNCTION var_coef_integer(relvar, a) RESULT ( res )
            ! Finds a coefficient for process rates based on the relative variance
            ! of cloud water.
            REAL(KIND=rkind_comp), intent(in) :: relvar
            INTEGER, intent(in) :: a
            REAL(KIND=rkind_comp) :: res
  res = rising_factorial(relvar, a) / relvar**a
        END FUNCTION var_coef_integer
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
        !========================================================================
        !MICROPHYSICAL PROCESS CALCULATIONS
        !========================================================================
        !========================================================================
        ! Initial ice deposition and sublimation loop.
        ! Run before the main loop
        ! This subroutine written by Peter Caldwell

        SUBROUTINE ice_deposition_sublimation(t, qv, qi, ni, icldm, rho, dv, qvl, qvi, berg, vap_dep, ice_sublim, mgncol) 
            !INPUT VARS:
            !===============================================
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qv
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qi
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ni
            REAL(KIND=r8), dimension(mgncol), intent(in) :: icldm
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho
            REAL(KIND=r8), dimension(mgncol), intent(in) :: dv
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qvl
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qvi
            !OUTPUT VARS:
            !===============================================
            REAL(KIND=r8), dimension(mgncol), intent(out) :: vap_dep !ice deposition (cell-ave value)
            REAL(KIND=r8), dimension(mgncol), intent(out) :: ice_sublim !ice sublimation (cell-ave value)
            REAL(KIND=r8), dimension(mgncol), intent(out) :: berg !bergeron enhancement (cell-ave value)
            !INTERNAL VARS:
            !===============================================
            REAL(KIND=r8) :: ab
            REAL(KIND=r8) :: epsi
            REAL(KIND=r8) :: qiic
            REAL(KIND=r8) :: niic
            REAL(KIND=r8) :: lami
            REAL(KIND=r8) :: n0i
            INTEGER :: i
!NEC$ IVDEP
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qv
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qi
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ni
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: icldm
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: dv
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qvl
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qvi
            !OUTPUT VARS:
            !===============================================
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: vap_dep !ice deposition (cell-ave value)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: ice_sublim !ice sublimation (cell-ave value)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: berg !bergeron enhancement (cell-ave value)
            !INTERNAL VARS:
            !===============================================
            REAL(KIND=rkind_comp) :: ab
            REAL(KIND=rkind_comp) :: epsi
            REAL(KIND=rkind_comp) :: qiic
            REAL(KIND=rkind_comp) :: niic
            REAL(KIND=rkind_comp) :: lami
            REAL(KIND=rkind_comp) :: n0i
            INTEGER :: i
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  do i=1,mgncol
  if (qi(i)>=qsmall) then
                !GET IN-CLOUD qi, ni
                !===============================================
     qiic = qi(i)/icldm(i)
     niic = ni(i)/icldm(i)
                !Compute linearized condensational heating correction
     ab=calc_ab(t(i), qvi(i), xxls)
                !Get slope and intercept of gamma distn for ice.
     call size_dist_param_basic(mg_ice_props, qiic, niic, lami, n0i)
                !Get depletion timescale=1/eps
<<<<<<< HEAD
     epsi = 2._r8*pi*n0i*rho(i)*Dv(i)/(lami*lami)
=======
     epsi = 2._rkind_comp*pi*n0i*rho(i)*Dv(i)/(lami*lami)
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
                !Compute deposition/sublimation
     vap_dep(i) = epsi/ab*(qv(i) - qvi(i))
                !Make this a grid-averaged quantity
     vap_dep(i)=vap_dep(i)*icldm(i)
                !Split into deposition or sublimation.
<<<<<<< HEAD
     if (t(i) < tmelt .and. vap_dep(i)>0._r8) then
        ice_sublim(i)=0._r8
     else
                    ! make ice_sublim negative for consistency with other evap/sub processes
        ice_sublim(i)=min(vap_dep(i),0._r8)
        vap_dep(i)=0._r8
=======
     if (t(i) < tmelt .and. vap_dep(i)>0._rkind_comp) then
        ice_sublim(i)=0._rkind_comp
     else
                    ! make ice_sublim negative for consistency with other evap/sub processes
        ice_sublim(i)=min(vap_dep(i),0._rkind_comp)
        vap_dep(i)=0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
     end if
                !sublimation occurs @ any T. Not so for berg.
     if (t(i) < tmelt) then
                    !Compute bergeron rate assuming cloud for whole step.
<<<<<<< HEAD
        berg(i) = max(epsi/ab*(qvl(i) - qvi(i)), 0._r8)
     else !T>frz !T>frz
        berg(i)=0._r8
     end if !T<frz !T<frz
  else !where qi<qsmall !where qi<qsmall
     berg(i)=0._r8
     vap_dep(i)=0._r8
     ice_sublim(i)=0._r8
=======
        berg(i) = max(epsi/ab*(qvl(i) - qvi(i)), 0._rkind_comp)
     else !T>frz !T>frz
        berg(i)=0._rkind_comp
     end if !T<frz !T<frz
  else !where qi<qsmall !where qi<qsmall
     berg(i)=0._rkind_comp
     vap_dep(i)=0._rkind_comp
     ice_sublim(i)=0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if !qi>qsmall !qi>qsmall
  enddo
        END SUBROUTINE ice_deposition_sublimation
        !========================================================================
        ! autoconversion of cloud liquid water to rain
        ! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
        ! minimum qc of 1 x 10^-8 prevents floating point error

        SUBROUTINE kk2000_liq_autoconversion(microp_uniform, qcic, ncic, rho, relvar, prc, nprc, nprc1, mgncol)
            LOGICAL, intent(in) :: microp_uniform
            INTEGER, intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ncic
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho
            REAL(KIND=r8), dimension(mgncol), intent(in) :: relvar
            REAL(KIND=r8), dimension(mgncol), intent(out) :: prc
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nprc
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nprc1
            REAL(KIND=r8) :: prc_coef
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ncic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: relvar
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: prc
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nprc
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nprc1
            REAL(KIND=rkind_comp) :: prc_coef
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            INTEGER :: i
            ! Take variance into account, or use uniform value.
  do i=1,mgncol
  if (.not. microp_uniform) then
<<<<<<< HEAD
!     prc_coef = var_coef(relvar(i), 2.47_r8)
      call var_coef(relvar(i), 2.47_r8, prc_coef)
  else
     prc_coef = 1._r8
=======
     prc_coef = var_coef(relvar(i), 2.47_rkind_comp)
  else
     prc_coef = 1._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  if (qcic(i) >= icsmall) then
                ! nprc is increase in rain number conc due to autoconversion
                ! nprc1 is decrease in cloud droplet conc due to autoconversion
                ! assume exponential sub-grid distribution of qc, resulting in additional
                ! factor related to qcvar below
                ! switch for sub-columns, don't include sub-grid qc
     prc(i) = prc_coef * &
<<<<<<< HEAD
          1350._r8 * qcic(i)**2.47_r8 * (ncic(i)*1.e-6_r8*rho(i))**(-1.79_r8)
     nprc(i) = prc(i) * (1._r8/droplet_mass_25um)
     nprc1(i) = prc(i)*ncic(i)/qcic(i)
  else
     prc(i)=0._r8
     nprc(i)=0._r8
     nprc1(i)=0._r8
=======
          1350._rkind_comp * qcic(i)**2.47_rkind_comp * (ncic(i)*1.e-6_rkind_comp*rho(i))**(-1.79_rkind_comp)
     nprc(i) = prc(i) * (1._rkind_comp/droplet_mass_25um)
     nprc1(i) = prc(i)*ncic(i)/qcic(i)
  else
     prc(i)=0._rkind_comp
     nprc(i)=0._rkind_comp
     nprc1(i)=0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE kk2000_liq_autoconversion

        !========================================================================
        ! Autoconversion of cloud ice to snow
        ! similar to Ferrier (1994)

        SUBROUTINE ice_autoconversion(t, qiic, lami, n0i, dcs, prci, nprci, mgncol)
            INTEGER, intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qiic
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lami
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0i
            REAL(KIND=r8),                    intent(in) :: dcs
            REAL(KIND=r8), dimension(mgncol), intent(out) :: prci
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nprci
            ! Assume autoconversion timescale of 180 seconds.
            REAL(KIND=r8), parameter :: ac_time = 180._r8
            ! Average mass of an ice particle.
            REAL(KIND=r8) :: m_ip
            ! Ratio of autoconversion diameter to average diameter.
            REAL(KIND=r8) :: d_rat
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qiic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lami
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0i
            REAL(KIND=rkind_comp),                    intent(in) :: dcs
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: prci
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nprci
            ! Assume autoconversion timescale of 180 seconds.
            REAL(KIND=rkind_comp), parameter :: ac_time = 180._rkind_comp
            ! Average mass of an ice particle.
            REAL(KIND=rkind_comp) :: m_ip
            ! Ratio of autoconversion diameter to average diameter.
            REAL(KIND=rkind_comp) :: d_rat
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (t(:) <= tmelt .and. qiic(:) >= qsmall)
  do i=1,mgncol
  if (loop_mask(i)) then
     d_rat = lami(i)*dcs
                ! Rate of ice particle conversion (number).
     nprci(i) = n0i(i)/(lami(i)*ac_time)*exp(-d_rat)
<<<<<<< HEAD
     m_ip = (rhoi*pi/6._r8) / lami(i)**3
=======
     m_ip = (rhoi*pi/6._rkind_comp) / lami(i)**3
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
                ! Rate of mass conversion.
                ! Note that this is:
                ! m n (d^3 + 3 d^2 + 6 d + 6)
     prci(i) = m_ip * nprci(i) * &
<<<<<<< HEAD
          (((d_rat + 3._r8)*d_rat + 6._r8)*d_rat + 6._r8)
  else
     prci(i) = 0._r8
     nprci(i) = 0._r8
=======
          (((d_rat + 3._rkind_comp)*d_rat + 6._rkind_comp)*d_rat + 6._rkind_comp)
  else
     prci(i) = 0._rkind_comp
     nprci(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE ice_autoconversion
        ! immersion freezing (Bigg, 1953)
        !===================================

        SUBROUTINE immersion_freezing(microp_uniform, t, pgam, lamc, qcic, ncic, relvar, mnuccc, nnuccc, mgncol)
            INTEGER, intent(in) :: mgncol
            LOGICAL, intent(in) :: microp_uniform
            ! Temperature
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t
            ! Cloud droplet size distribution parameters
            REAL(KIND=r8), dimension(mgncol), intent(in) :: pgam
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamc
            ! MMR and number concentration of in-cloud liquid water
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ncic
            ! Relative variance of cloud water
            REAL(KIND=r8), dimension(mgncol), intent(in) :: relvar
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: mnuccc ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nnuccc ! Number
            ! Coefficients that will be omitted for sub-columns
            REAL(KIND=r8), dimension(mgncol) :: dum
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
            REAL(KIND=r8) :: tmp
  if (.not. microp_uniform) then
!     dum(:) = var_coef(relvar, 2)
     call var_coef(relvar, 2,dum,mgncol)
  else
     dum(:) = 1._r8
  end if
  loop_mask(:) = (qcic(:) >= qsmall .and. t(:) < 269.15_r8)
  do i=1,mgncol
  if (loop_mask(i)) then
     call rising_factorial(pgam(i)+1._r8, 3,tmp)
     ! nnuccc(i) = &
     !     pi/6._r8*ncic(i)*rising_factorial(pgam(i)+1._r8, 3)* &
     !     bimm*(exp(aimm*(tmelt - t(i)))-1._r8)/lamc(i)**3
     ! mnuccc(i) = dum(i) * nnuccc(i) * &
     !     pi/6._r8*rhow* &
     !     rising_factorial(pgam(i)+4._r8, 3)/lamc(i)**3
     nnuccc(i) = &
          pi/6._r8*ncic(i)*tmp* &
          bimm*(exp(aimm*(tmelt - t(i)))-1._r8)/lamc(i)**3
     mnuccc(i) = dum(i) * nnuccc(i) * &
          pi/6._r8*rhow* &
          tmp/lamc(i)**3
  else
     mnuccc(i) = 0._r8
     nnuccc(i) = 0._r8
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t
            ! Cloud droplet size distribution parameters
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: pgam
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lamc
            ! MMR and number concentration of in-cloud liquid water
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ncic
            ! Relative variance of cloud water
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: relvar
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: mnuccc ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nnuccc ! Number
            ! Coefficients that will be omitted for sub-columns
            REAL(KIND=rkind_comp), dimension(mgncol) :: dum
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  if (.not. microp_uniform) then
     dum(:) = var_coef(relvar, 2)
  else
     dum(:) = 1._rkind_comp
  end if
  loop_mask(:) = (qcic(:) >= qsmall .and. t(:) < 269.15_rkind_comp)
  do i=1,mgncol
  if (loop_mask(i)) then
     nnuccc(i) = &
          pi/6._rkind_comp*ncic(i)*rising_factorial(pgam(i)+1._rkind_comp, 3)* &
          bimm*(exp(aimm*(tmelt - t(i)))-1._rkind_comp)/lamc(i)**3
     mnuccc(i) = dum(i) * nnuccc(i) * &
          pi/6._rkind_comp*rhow* &
          rising_factorial(pgam(i)+4._rkind_comp, 3)/lamc(i)**3
  else
     mnuccc(i) = 0._rkind_comp
     nnuccc(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if ! qcic > qsmall and t < 4 deg C ! qcic > qsmall and t < 4 deg C
  enddo
        END SUBROUTINE immersion_freezing
        ! contact freezing (-40<T<-3 C) (Young, 1974) with hooks into simulated dust
        !===================================================================
        ! dust size and number in multiple bins are read in from companion routine

        SUBROUTINE contact_freezing(microp_uniform, t, p, rndst, nacon, pgam, lamc, qcic, ncic, relvar, mnucct, nnucct, mgncol, mdust)
            LOGICAL, intent(in) :: microp_uniform
            INTEGER, intent(in) :: mgncol
            INTEGER, intent(in) :: mdust
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: p ! Pressure
            REAL(KIND=r8), dimension(mgncol, mdust), intent(in) :: rndst ! Radius (for multiple dust bins)
            REAL(KIND=r8), dimension(mgncol, mdust), intent(in) :: nacon ! Number (for multiple dust bins)
            ! Size distribution parameters for cloud droplets
            REAL(KIND=r8), dimension(mgncol), intent(in) :: pgam
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamc
            ! MMR and number concentration of in-cloud liquid water
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ncic
            ! Relative cloud water variance
            REAL(KIND=r8), dimension(mgncol), intent(in) :: relvar
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: mnucct ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nnucct ! Number
            REAL(KIND=r8) :: tcnt ! scaled relative temperature
            REAL(KIND=r8) :: viscosity ! temperature-specific viscosity (kg/m/s)
            REAL(KIND=r8) :: mfp ! temperature-specific mean free path (m)
            ! Dimension these according to number of dust bins, inferred from rndst size
            REAL(KIND=r8) :: nslip(size(rndst,2)) ! slip correction factors
            REAL(KIND=r8) :: ndfaer(size(rndst,2)) ! aerosol diffusivities (m^2/sec)
            ! Coefficients not used for subcolumns
            REAL(KIND=r8) :: dum
            REAL(KIND=r8) :: dum1
            ! Common factor between mass and number.
            REAL(KIND=r8) :: contact_factor
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
            REAL(KIND=r8) :: tmp
  loop_mask(:) = (qcic(:) >= qsmall .and. t(:) < 269.15_r8)
  do i = 1,mgncol
     if (loop_mask(i)) then
        if (.not. microp_uniform) then
!           dum = var_coef(relvar(i), 4._r8/3._r8)
           call var_coef(relvar(i), 4._r8/3._r8,dum)
!           dum1 = var_coef(relvar(i), 1._r8/3._r8)
           call var_coef(relvar(i), 1._r8/3._r8,dum1)
        else
           dum = 1._r8
           dum1 = 1._r8
        endif
        tcnt=(270.16_r8-t(i))**1.3_r8
        viscosity = 1.8e-5_r8*(t(i)/298.0_r8)**0.85_r8    ! Viscosity (kg/m/s) ! Viscosity (kg/m/s)
        mfp = 2.0_r8*viscosity/ &                         ! Mean free path (m)
                     (p(i)*sqrt( 8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i)) )) ! Mean free path (m)
                    ! Note that these two are vectors.
        nslip = 1.0_r8+(mfp/rndst(i,:))*(1.257_r8+(0.4_r8*exp(-(1.1_r8*rndst(i,:)/mfp))))! Slip correction factor ! Slip correction factor
        ndfaer = 1.381e-23_r8*t(i)*nslip/(6._r8*pi*viscosity*rndst(i,:))  ! aerosol diffusivity (m2/s) ! aerosol diffusivity (m2/s)
        contact_factor = dot_product(ndfaer,nacon(i,:)*tcnt) * pi * &
             ncic(i) * (pgam(i) + 1._r8) / lamc(i)
        ! mnucct(i) = dum * contact_factor * &
        !     pi/3._r8*rhow*rising_factorial(pgam(i)+2._r8, 3)/lamc(i)**3
        call rising_factorial(pgam(i)+2._r8, 3,tmp)
        mnucct(i) = dum * contact_factor * &
             pi/3._r8*rhow*tmp/lamc(i)**3
        nnucct(i) =  dum1 * 2._r8 * contact_factor
     else
        mnucct(i)=0._r8
        nnucct(i)=0._r8
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: p ! Pressure
            REAL(KIND=rkind_comp), dimension(mgncol, mdust), intent(in) :: rndst ! Radius (for multiple dust bins)
            REAL(KIND=rkind_comp), dimension(mgncol, mdust), intent(in) :: nacon ! Number (for multiple dust bins)
            ! Size distribution parameters for cloud droplets
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: pgam
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lamc
            ! MMR and number concentration of in-cloud liquid water
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ncic
            ! Relative cloud water variance
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: relvar
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: mnucct ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nnucct ! Number
            REAL(KIND=rkind_comp) :: tcnt ! scaled relative temperature
            REAL(KIND=rkind_comp) :: viscosity ! temperature-specific viscosity (kg/m/s)
            REAL(KIND=rkind_comp) :: mfp ! temperature-specific mean free path (m)
            ! Dimension these according to number of dust bins, inferred from rndst size
            REAL(KIND=rkind_comp) :: nslip(size(rndst,2)) ! slip correction factors
            REAL(KIND=rkind_comp) :: ndfaer(size(rndst,2)) ! aerosol diffusivities (m^2/sec)
            ! Coefficients not used for subcolumns
            REAL(KIND=rkind_comp) :: dum
            REAL(KIND=rkind_comp) :: dum1
            ! Common factor between mass and number.
            REAL(KIND=rkind_comp) :: contact_factor
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (qcic(:) >= qsmall .and. t(:) < 269.15_rkind_comp)
  do i = 1,mgncol
     if (loop_mask(i)) then
        if (.not. microp_uniform) then
           dum = var_coef(relvar(i), 4._rkind_comp/3._rkind_comp)
           dum1 = var_coef(relvar(i), 1._rkind_comp/3._rkind_comp)
        else
           dum = 1._rkind_comp
           dum1 = 1._rkind_comp
        endif
        tcnt=(270.16_rkind_comp-t(i))**1.3_rkind_comp
        viscosity = 1.8e-5_rkind_comp*(t(i)/298.0_rkind_comp)**0.85_rkind_comp    ! Viscosity (kg/m/s) ! Viscosity (kg/m/s)
        mfp = 2.0_rkind_comp*viscosity/ &                         ! Mean free path (m)
                     (p(i)*sqrt( 8.0_rkind_comp*28.96e-3_rkind_comp/(pi*8.314409_rkind_comp*t(i)) )) ! Mean free path (m)
                    ! Note that these two are vectors.
        nslip = 1.0_rkind_comp+(mfp/rndst(i,:))*(1.257_rkind_comp+(0.4_rkind_comp*exp(-(1.1_rkind_comp*rndst(i,:)/mfp))))! Slip correction factor ! Slip correction factor
        ndfaer = 1.381e-23_rkind_comp*t(i)*nslip/(6._rkind_comp*pi*viscosity*rndst(i,:))  ! aerosol diffusivity (m2/s) ! aerosol diffusivity (m2/s)
        contact_factor = dot_product(ndfaer,nacon(i,:)*tcnt) * pi * &
             ncic(i) * (pgam(i) + 1._rkind_comp) / lamc(i)
        mnucct(i) = dum * contact_factor * &
             pi/3._rkind_comp*rhow*rising_factorial(pgam(i)+2._rkind_comp, 3)/lamc(i)**3
        nnucct(i) =  dum1 * 2._rkind_comp * contact_factor
     else
        mnucct(i)=0._rkind_comp
        nnucct(i)=0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
     end if ! qcic > qsmall and t < 4 deg C ! qcic > qsmall and t < 4 deg C
  end do
        END SUBROUTINE contact_freezing
        ! snow self-aggregation from passarelli, 1978, used by reisner, 1998
        !===================================================================
        ! this is hard-wired for bs = 0.4 for now
        ! ignore self-collection of cloud ice

        ! accretion of cloud droplets onto snow/graupel
        SUBROUTINE snow_self_aggregation(t, rho, asn, rhosn, qsic, nsic, nsagg, mgncol)
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! Density
            REAL(KIND=r8), dimension(mgncol), intent(in) :: asn ! fall speed parameter for snow
            REAL(KIND=r8),                    intent(in) :: rhosn ! density of snow
            ! In-cloud snow
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qsic ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(in) :: nsic ! Number
            ! Output number tendency
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nsagg
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! Density
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: asn ! fall speed parameter for snow
            REAL(KIND=rkind_comp),                    intent(in) :: rhosn ! density of snow
            ! In-cloud snow
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qsic ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: nsic ! Number
            ! Output number tendency
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nsagg
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (qsic(:) >= qsmall .and. t(:) <= tmelt) 
  do i=1,mgncol
  if (loop_mask(i)) then
<<<<<<< HEAD
     nsagg(i) = -1108._r8*eii/(4._r8*720._r8*rhosn)*asn(i)*qsic(i)*nsic(i)*rho(i)*&
          ((qsic(i)/nsic(i))*(1._r8/(rhosn*pi)))**((bs-1._r8)/3._r8)
  else
     nsagg(i)=0._r8
=======
     nsagg(i) = -1108._rkind_comp*eii/(4._rkind_comp*720._rkind_comp*rhosn)*asn(i)*qsic(i)*nsic(i)*rho(i)*&
          ((qsic(i)/nsic(i))*(1._rkind_comp/(rhosn*pi)))**((bs-1._rkind_comp)/3._rkind_comp)
  else
     nsagg(i)=0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE snow_self_aggregation
        ! accretion of cloud droplets onto snow/graupel
        !===================================================================
        ! here use continuous collection equation with
        ! simple gravitational collection kernel
        ! ignore collisions between droplets/cloud ice
        ! since minimum size ice particle for accretion is 50 - 150 micron

        SUBROUTINE accrete_cloud_water_snow(t, rho, asn, uns, mu, qcic, ncic, qsic, pgam, lamc, lams, n0s, psacws, &
        npsacws, mgncol)
            INTEGER, intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! Density
            REAL(KIND=r8), dimension(mgncol), intent(in) :: asn ! Fallspeed parameter (snow)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: uns ! Current fallspeed   (snow)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: mu ! Viscosity
            ! In-cloud liquid water
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ncic ! Number
            ! In-cloud snow
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qsic ! MMR
            ! Cloud droplet size parameters
            REAL(KIND=r8), dimension(mgncol), intent(in) :: pgam
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamc
            ! Snow size parameters
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lams
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: psacws ! Mass mixing ratio
            REAL(KIND=r8), dimension(mgncol), intent(out) :: npsacws ! Number concentration
            REAL(KIND=r8) :: dc0 ! Provisional mean droplet size
            REAL(KIND=r8) :: dum
            REAL(KIND=r8) :: eci ! collection efficiency for riming of snow by droplets
            ! Fraction of cloud droplets accreted per second
            REAL(KIND=r8) :: accrete_rate
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! Density
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: asn ! Fallspeed parameter (snow)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: uns ! Current fallspeed   (snow)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: mu ! Viscosity
            ! In-cloud liquid water
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ncic ! Number
            ! In-cloud snow
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qsic ! MMR
            ! Cloud droplet size parameters
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: pgam
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lamc
            ! Snow size parameters
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lams
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: psacws ! Mass mixing ratio
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: npsacws ! Number concentration
            REAL(KIND=rkind_comp) :: dc0 ! Provisional mean droplet size
            REAL(KIND=rkind_comp) :: dum
            REAL(KIND=rkind_comp) :: eci ! collection efficiency for riming of snow by droplets
            ! Fraction of cloud droplets accreted per second
            REAL(KIND=rkind_comp) :: accrete_rate
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
            ! ignore collision of snow with droplets above freezing
  loop_mask(:) = (qsic(:) >= qsmall .and. t(:) <= tmelt .and. qcic(:) >= qsmall) 
  do i=1,mgncol
  if (loop_mask(i)) then
                ! put in size dependent collection efficiency
                ! mean diameter of snow is area-weighted, since
                ! accretion is function of crystal geometric area
                ! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)
<<<<<<< HEAD
     dc0 = (pgam(i)+1._r8)/lamc(i)
     dum = dc0*dc0*uns(i)*rhow*lams(i)/(9._r8*mu(i))
     eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))
     eci = max(eci,0._r8)
     eci = min(eci,1._r8)
                ! no impact of sub-grid distribution of qc since psacws
                ! is linear in qc
     accrete_rate = pi/4._r8*asn(i)*rho(i)*n0s(i)*eci*gamma_bs_plus3 / lams(i)**(bs+3._r8)
     psacws(i) = accrete_rate*qcic(i)
     npsacws(i) = accrete_rate*ncic(i)
  else
     psacws(i) = 0._r8
     npsacws(i) = 0._r8
=======
     dc0 = (pgam(i)+1._rkind_comp)/lamc(i)
     dum = dc0*dc0*uns(i)*rhow*lams(i)/(9._rkind_comp*mu(i))
     eci = dum*dum/((dum+0.4_rkind_comp)*(dum+0.4_rkind_comp))
     eci = max(eci,0._rkind_comp)
     eci = min(eci,1._rkind_comp)
                ! no impact of sub-grid distribution of qc since psacws
                ! is linear in qc
     accrete_rate = pi/4._rkind_comp*asn(i)*rho(i)*n0s(i)*eci*gamma_bs_plus3 / lams(i)**(bs+3._rkind_comp)
     psacws(i) = accrete_rate*qcic(i)
     npsacws(i) = accrete_rate*ncic(i)
  else
     psacws(i) = 0._rkind_comp
     npsacws(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE accrete_cloud_water_snow
        ! add secondary ice production due to accretion of droplets by snow
        !===================================================================
        ! (Hallet-Mossop process) (from Cotton et al., 1986)

        SUBROUTINE secondary_ice_production(t, psacws, msacwi, nsacwi, mgncol)
            INTEGER, intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            ! Accretion of cloud water to snow tendencies
            REAL(KIND=r8), dimension(mgncol), intent(inout) :: psacws ! MMR
            ! Output (ice) tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: msacwi ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nsacwi ! Number
            INTEGER :: i
  do i=1,mgncol
  if((t(i) < 270.16_r8) .and. (t(i) >= 268.16_r8)) then
     nsacwi(i) = 3.5e8_r8*(270.16_r8-t(i))/2.0_r8*psacws(i)
  else if((t(i) < 268.16_r8) .and. (t(i) >= 265.16_r8)) then
     nsacwi(i) = 3.5e8_r8*(t(i)-265.16_r8)/3.0_r8*psacws(i)
  else
     nsacwi(i) = 0.0_r8
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            ! Accretion of cloud water to snow tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(inout) :: psacws ! MMR
            ! Output (ice) tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: msacwi ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nsacwi ! Number
            INTEGER :: i
  do i=1,mgncol
  if((t(i) < 270.16_rkind_comp) .and. (t(i) >= 268.16_rkind_comp)) then
     nsacwi(i) = 3.5e8_rkind_comp*(270.16_rkind_comp-t(i))/2.0_rkind_comp*psacws(i)
  else if((t(i) < 268.16_rkind_comp) .and. (t(i) >= 265.16_rkind_comp)) then
     nsacwi(i) = 3.5e8_rkind_comp*(t(i)-265.16_rkind_comp)/3.0_rkind_comp*psacws(i)
  else
     nsacwi(i) = 0.0_rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  endif
  enddo

  do i=1,mgncol
  msacwi(i) = min(nsacwi(i)*mi0, psacws(i))
  psacws(i) = psacws(i) - msacwi(i)
  enddo
        END SUBROUTINE secondary_ice_production
        ! accretion of rain water by snow
        !===================================================================
        ! formula from ikawa and saito, 1991, used by reisner et al., 1998

        SUBROUTINE accrete_rain_snow(t, rho, umr, ums, unr, uns, qric, qsic, lamr, n0r, lams, n0s, pracs, npracs, mgncol)
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! Density
            ! Fallspeeds
            ! mass-weighted
            REAL(KIND=r8), dimension(mgncol), intent(in) :: umr ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ums ! snow
            ! number-weighted
            REAL(KIND=r8), dimension(mgncol), intent(in) :: unr ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: uns ! snow
            ! In cloud MMRs
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qric ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qsic ! snow
            ! Size distribution parameters
            ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamr
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0r
            ! snow
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lams
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: pracs ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: npracs ! Number
            ! Collection efficiency for accretion of rain by snow
            REAL(KIND=r8), parameter :: ecr = 1.0_r8
            ! Ratio of average snow diameter to average rain diameter.
            REAL(KIND=r8) :: d_rat
            ! Common factor between mass and number expressions
            REAL(KIND=r8) :: common_factor
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! Density
            ! Fallspeeds
            ! mass-weighted
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: umr ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ums ! snow
            ! number-weighted
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: unr ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: uns ! snow
            ! In cloud MMRs
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qric ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qsic ! snow
            ! Size distribution parameters
            ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lamr
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0r
            ! snow
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lams
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: pracs ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: npracs ! Number
            ! Collection efficiency for accretion of rain by snow
            REAL(KIND=rkind_comp), parameter :: ecr = 1.0_rkind_comp
            ! Ratio of average snow diameter to average rain diameter.
            REAL(KIND=rkind_comp) :: d_rat
            ! Common factor between mass and number expressions
            REAL(KIND=rkind_comp) :: common_factor
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (qric(:) >= icsmall .and. qsic(:) >= icsmall .and. t(:) <= tmelt)
  do i=1,mgncol
   if (loop_mask(i)) then
     common_factor = pi*ecr*rho(i)*n0r(i)*n0s(i)/(lamr(i)**3 * lams(i))
     d_rat = lamr(i)/lams(i)
     pracs(i) = common_factor*pi*rhow* &
<<<<<<< HEAD
          sqrt((1.2_r8*umr(i)-0.95_r8*ums(i))**2 + 0.08_r8*ums(i)*umr(i)) / lamr(i)**3 * &
          ((0.5_r8*d_rat + 2._r8)*d_rat + 5._r8)
     npracs(i) = common_factor*0.5_r8* &
          sqrt(1.7_r8*(unr(i)-uns(i))**2 + 0.3_r8*unr(i)*uns(i)) * &
          ((d_rat + 1._r8)*d_rat + 1._r8)
   else 
     pracs(i) = 0._r8
     npracs(i) = 0._r8
=======
          sqrt((1.2_rkind_comp*umr(i)-0.95_rkind_comp*ums(i))**2 + 0.08_rkind_comp*ums(i)*umr(i)) / lamr(i)**3 * &
          ((0.5_rkind_comp*d_rat + 2._rkind_comp)*d_rat + 5._rkind_comp)
     npracs(i) = common_factor*0.5_rkind_comp* &
          sqrt(1.7_rkind_comp*(unr(i)-uns(i))**2 + 0.3_rkind_comp*unr(i)*uns(i)) * &
          ((d_rat + 1._rkind_comp)*d_rat + 1._rkind_comp)
   else 
     pracs(i) = 0._rkind_comp
     npracs(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
   end if
  enddo
        END SUBROUTINE accrete_rain_snow
        ! heterogeneous freezing of rain drops
        !===================================================================
        ! follows from Bigg (1953)

        SUBROUTINE heterogeneous_rain_freezing(t, qric, nric, lamr, mnuccr, nnuccr, mgncol)
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            ! In-cloud rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qric ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(in) :: nric ! Number
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamr ! size parameter
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: mnuccr ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nnuccr ! Number
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (t(:) < 269.15_r8 .and. qric(:) >= qsmall)
  do i=1,mgncol
  if (loop_mask(i)) then
     nnuccr(i) = pi*nric(i)*bimm* &
          (exp(aimm*(tmelt - t(i)))-1._r8)/lamr(i)**3
     mnuccr(i) = nnuccr(i) * 20._r8*pi*rhow/lamr(i)**3
  else
     mnuccr(i) = 0._r8
     nnuccr(i) = 0._r8
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            ! In-cloud rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qric ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: nric ! Number
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lamr ! size parameter
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: mnuccr ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nnuccr ! Number
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (t(:) < 269.15_rkind_comp .and. qric(:) >= qsmall)
  do i=1,mgncol
  if (loop_mask(i)) then
     nnuccr(i) = pi*nric(i)*bimm* &
          (exp(aimm*(tmelt - t(i)))-1._rkind_comp)/lamr(i)**3
     mnuccr(i) = nnuccr(i) * 20._rkind_comp*pi*rhow/lamr(i)**3
  else
     mnuccr(i) = 0._rkind_comp
     nnuccr(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE heterogeneous_rain_freezing
        ! accretion of cloud liquid water by rain
        !===================================================================
        ! formula from Khrouditnov and Kogan (2000)
        ! gravitational collection kernel, droplet fall speed neglected

        SUBROUTINE accrete_cloud_water_rain(microp_uniform, qric, qcic, ncic, relvar, accre_enhan, pra, npra, mgncol)
            LOGICAL, intent(in) :: microp_uniform
            INTEGER, intent(in) :: mgncol
            ! In-cloud rain
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qric ! MMR
            ! Cloud droplets
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(in) :: ncic ! Number
            ! SGS variability
            REAL(KIND=r8), dimension(mgncol), intent(in) :: relvar
            REAL(KIND=r8), dimension(mgncol), intent(in) :: accre_enhan
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: pra ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: npra ! Number
            ! Coefficient that varies for subcolumns
            REAL(KIND=r8), dimension(mgncol) :: pra_coef
            REAL(KIND=r8), dimension(mgncol) :: tmpA
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  if (.not. microp_uniform) then
!     pra_coef(:) = accre_enhan * var_coef(relvar(:), 1.15_r8)
      call var_coef(relvar, 1.15_r8,tmpA,SIZE(tmpA))
      pra_coef(:) = accre_enhan * tmpA 
  else
     pra_coef(:) = 1._r8
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qric ! MMR
            ! Cloud droplets
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: ncic ! Number
            ! SGS variability
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: relvar
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: accre_enhan
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: pra ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: npra ! Number
            ! Coefficient that varies for subcolumns
            REAL(KIND=rkind_comp), dimension(mgncol) :: pra_coef
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  if (.not. microp_uniform) then
     pra_coef(:) = accre_enhan * var_coef(relvar(:), 1.15_rkind_comp)
  else
     pra_coef(:) = 1._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if

  loop_mask(:) = (qric(:) >= qsmall .and. qcic(:) >= qsmall) 
  do i=1,mgncol
  if (loop_mask(i)) then
                ! include sub-grid distribution of cloud water
<<<<<<< HEAD
     pra(i) = pra_coef(i) * 67._r8*(qcic(i)*qric(i))**1.15_r8
     npra(i) = pra(i)*ncic(i)/qcic(i)
  else
     pra(i) = 0._r8
     npra(i) = 0._r8
=======
     pra(i) = pra_coef(i) * 67._rkind_comp*(qcic(i)*qric(i))**1.15_rkind_comp
     npra(i) = pra(i)*ncic(i)/qcic(i)
  else
     pra(i) = 0._rkind_comp
     npra(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE accrete_cloud_water_rain
        ! Self-collection of rain drops
        !===================================================================
        ! from Beheng(1994)

        SUBROUTINE self_collection_rain(rho, qric, nric, nragg, mgncol)
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! Air density
            ! Rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qric ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(in) :: nric ! Number
            ! Output number tendency
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nragg
            INTEGER :: i
  do i=1,mgncol
  if (qric(i) >= qsmall) then
     nragg(i) = -8._r8*nric(i)*qric(i)*rho(i)
  else
     nragg(i) = 0._r8
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! Air density
            ! Rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qric ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: nric ! Number
            ! Output number tendency
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nragg
            INTEGER :: i
  do i=1,mgncol
  if (qric(i) >= qsmall) then
     nragg(i) = -8._rkind_comp*nric(i)*qric(i)*rho(i)
  else
     nragg(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE self_collection_rain
        ! Accretion of cloud ice by snow
        !===================================================================
        ! For this calculation, it is assumed that the Vs >> Vi
        ! and Ds >> Di for continuous collection

        SUBROUTINE accrete_cloud_ice_snow(t, rho, asn, qiic, niic, qsic, lams, n0s, prai, nprai, mgncol)
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! Density
            REAL(KIND=r8), dimension(mgncol), intent(in) :: asn ! Snow fallspeed parameter
            ! Cloud ice
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qiic ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(in) :: niic ! Number
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qsic ! Snow MMR
            ! Snow size parameters
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lams
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: prai ! MMR
            REAL(KIND=r8), dimension(mgncol), intent(out) :: nprai ! Number
            ! Fraction of cloud ice particles accreted per second
            REAL(KIND=r8) :: accrete_rate
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! Temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! Density
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: asn ! Snow fallspeed parameter
            ! Cloud ice
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qiic ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: niic ! Number
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qsic ! Snow MMR
            ! Snow size parameters
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lams
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: prai ! MMR
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: nprai ! Number
            ! Fraction of cloud ice particles accreted per second
            REAL(KIND=rkind_comp) :: accrete_rate
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (qsic(:) >= qsmall .and. qiic(:) >= qsmall .and. t(:) <= tmelt) 
  do i=1,mgncol
  if(loop_mask(i)) then
<<<<<<< HEAD
     accrete_rate = pi/4._r8 * eii * asn(i) * rho(i) * n0s(i) * gamma_bs_plus3/ &
          lams(i)**(bs+3._r8)
     prai(i) = accrete_rate * qiic(i)
     nprai(i) = accrete_rate * niic(i)
  else
     prai(i) = 0._r8
     nprai(i) = 0._r8
=======
     accrete_rate = pi/4._rkind_comp * eii * asn(i) * rho(i) * n0s(i) * gamma_bs_plus3/ &
          lams(i)**(bs+3._rkind_comp)
     prai(i) = accrete_rate * qiic(i)
     nprai(i) = accrete_rate * niic(i)
  else
     prai(i) = 0._rkind_comp
     nprai(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE accrete_cloud_ice_snow
        ! calculate evaporation/sublimation of rain and snow
        !===================================================================
        ! note: evaporation/sublimation occurs only in cloud-free portion of grid cell
        ! in-cloud condensation/deposition of rain and snow is neglected
        ! except for transfer of cloud water to snow through bergeron process

        SUBROUTINE evaporate_sublimate_precip(t, rho, dv, mu, sc, q, qvl, qvi, lcldm, precip_frac, arn, asn, qcic, qiic,&
         qric, qsic, lamr, n0r, lams, n0s, pre, prds, mgncol)
            INTEGER,                          intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! air density
            REAL(KIND=r8), dimension(mgncol), intent(in) :: dv ! water vapor diffusivity
            REAL(KIND=r8), dimension(mgncol), intent(in) :: mu ! viscosity
            REAL(KIND=r8), dimension(mgncol), intent(in) :: sc ! schmidt number
            REAL(KIND=r8), dimension(mgncol), intent(in) :: q ! humidity
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qvl ! saturation humidity (water)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qvi ! saturation humidity (ice)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lcldm ! liquid cloud fraction
            REAL(KIND=r8), dimension(mgncol), intent(in) :: precip_frac ! precipitation fraction (maximum overlap)
            ! fallspeed parameters
            REAL(KIND=r8), dimension(mgncol), intent(in) :: arn ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: asn ! snow
            ! In-cloud MMRs
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic ! cloud liquid
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qiic ! cloud ice
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qric ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qsic ! snow
            ! Size parameters
            ! rain
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lamr
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0r
            ! snow
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lams
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: pre
            REAL(KIND=r8), dimension(mgncol), intent(out) :: prds
            REAL(KIND=r8) :: qclr ! water vapor mixing ratio in clear air
            REAL(KIND=r8) :: ab ! correction to account for latent heat
            REAL(KIND=r8) :: eps ! 1/ sat relaxation timescale
            REAL(KIND=r8), dimension(mgncol) :: dum
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! air density
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: dv ! water vapor diffusivity
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: mu ! viscosity
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: sc ! schmidt number
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: q ! humidity
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qvl ! saturation humidity (water)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qvi ! saturation humidity (ice)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lcldm ! liquid cloud fraction
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: precip_frac ! precipitation fraction (maximum overlap)
            ! fallspeed parameters
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: arn ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: asn ! snow
            ! In-cloud MMRs
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic ! cloud liquid
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qiic ! cloud ice
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qric ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qsic ! snow
            ! Size parameters
            ! rain
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lamr
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0r
            ! snow
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lams
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: pre
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: prds
            REAL(KIND=rkind_comp) :: qclr ! water vapor mixing ratio in clear air
            REAL(KIND=rkind_comp) :: ab ! correction to account for latent heat
            REAL(KIND=rkind_comp) :: eps ! 1/ sat relaxation timescale
            REAL(KIND=rkind_comp), dimension(mgncol) :: dum
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            INTEGER :: i
            ! set temporary cloud fraction to zero if cloud water + ice is very small
            ! this will ensure that evaporation/sublimation of precip occurs over
            ! entire grid cell, since min cloud fraction is specified otherwise
  do i=1,mgncol
<<<<<<< HEAD
  if (qcic(i)+qiic(i) < 1.e-6_r8) then
     dum(i) = 0._r8
=======
  if (qcic(i)+qiic(i) < 1.e-6_rkind_comp) then
     dum(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  else
     dum(i) = lcldm(i)
  end if
  enddo
  do i=1,mgncol
            ! only calculate if there is some precip fraction > cloud fraction
  if (precip_frac(i) > dum(i)) then
                ! calculate q for out-of-cloud region
<<<<<<< HEAD
     qclr=(q(i)-dum(i)*qvl(i))/(1._r8-dum(i))
                ! evaporation of rain
     if (qric(i) >= qsmall) then
        ab = calc_ab(t(i), qvl(i), xxlv)
        eps = 2._r8*pi*n0r(i)*rho(i)*Dv(i)* &
             (f1r/(lamr(i)*lamr(i))+ &
             f2r*(arn(i)*rho(i)/mu(i))**0.5_r8* &
             sc(i)**(1._r8/3._r8)*gamma_half_br_plus5/ &
             (lamr(i)**(5._r8/2._r8+br/2._r8)))
        pre(i) = eps*(qclr-qvl(i))/ab
                    ! only evaporate in out-of-cloud region
                    ! and distribute across precip_frac
        pre(i)=min(pre(i)*(precip_frac(i)-dum(i)),0._r8)
        pre(i)=pre(i)/precip_frac(i)
     else
        pre(i) = 0._r8
=======
     qclr=(q(i)-dum(i)*qvl(i))/(1._rkind_comp-dum(i))
                ! evaporation of rain
     if (qric(i) >= qsmall) then
        ab = calc_ab(t(i), qvl(i), xxlv)
        eps = 2._rkind_comp*pi*n0r(i)*rho(i)*Dv(i)* &
             (f1r/(lamr(i)*lamr(i))+ &
             f2r*(arn(i)*rho(i)/mu(i))**0.5_rkind_comp* &
             sc(i)**(1._rkind_comp/3._rkind_comp)*gamma_half_br_plus5/ &
             (lamr(i)**(5._rkind_comp/2._rkind_comp+br/2._rkind_comp)))
        pre(i) = eps*(qclr-qvl(i))/ab
                    ! only evaporate in out-of-cloud region
                    ! and distribute across precip_frac
        pre(i)=min(pre(i)*(precip_frac(i)-dum(i)),0._rkind_comp)
        pre(i)=pre(i)/precip_frac(i)
     else
        pre(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
     end if
                ! sublimation of snow
     if (qsic(i) >= qsmall) then
        ab = calc_ab(t(i), qvi(i), xxls)
<<<<<<< HEAD
        eps = 2._r8*pi*n0s(i)*rho(i)*Dv(i)* &
             (f1s/(lams(i)*lams(i))+ &
             f2s*(asn(i)*rho(i)/mu(i))**0.5_r8* &
             sc(i)**(1._r8/3._r8)*gamma_half_bs_plus5/ &
             (lams(i)**(5._r8/2._r8+bs/2._r8)))
        prds(i) = eps*(qclr-qvi(i))/ab
                    ! only sublimate in out-of-cloud region and distribute over precip_frac
        prds(i)=min(prds(i)*(precip_frac(i)-dum(i)),0._r8)
        prds(i)=prds(i)/precip_frac(i)
     else
        prds(i) = 0._r8
     end if
  else
     prds(i) = 0._r8
     pre = 0._r8
=======
        eps = 2._rkind_comp*pi*n0s(i)*rho(i)*Dv(i)* &
             (f1s/(lams(i)*lams(i))+ &
             f2s*(asn(i)*rho(i)/mu(i))**0.5_rkind_comp* &
             sc(i)**(1._rkind_comp/3._rkind_comp)*gamma_half_bs_plus5/ &
             (lams(i)**(5._rkind_comp/2._rkind_comp+bs/2._rkind_comp)))
        prds(i) = eps*(qclr-qvi(i))/ab
                    ! only sublimate in out-of-cloud region and distribute over precip_frac
        prds(i)=min(prds(i)*(precip_frac(i)-dum(i)),0._rkind_comp)
        prds(i)=prds(i)/precip_frac(i)
     else
        prds(i) = 0._rkind_comp
     end if
  else
     prds(i) = 0._rkind_comp
     pre = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE evaporate_sublimate_precip
        ! bergeron process - evaporation of droplets and deposition onto snow
        !===================================================================

        SUBROUTINE bergeron_process_snow(t, rho, dv, mu, sc, qvl, qvi, asn, qcic, qsic, lams, n0s, bergs, mgncol)
            INTEGER, intent(in) :: mgncol
<<<<<<< HEAD
            REAL(KIND=r8), dimension(mgncol), intent(in) :: t ! temperature
            REAL(KIND=r8), dimension(mgncol), intent(in) :: rho ! air density
            REAL(KIND=r8), dimension(mgncol), intent(in) :: dv ! water vapor diffusivity
            REAL(KIND=r8), dimension(mgncol), intent(in) :: mu ! viscosity
            REAL(KIND=r8), dimension(mgncol), intent(in) :: sc ! schmidt number
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qvl ! saturation humidity (water)
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qvi ! saturation humidity (ice)
            ! fallspeed parameter for snow
            REAL(KIND=r8), dimension(mgncol), intent(in) :: asn
            ! In-cloud MMRs
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qcic ! cloud liquid
            REAL(KIND=r8), dimension(mgncol), intent(in) :: qsic ! snow
            ! Size parameters for snow
            REAL(KIND=r8), dimension(mgncol), intent(in) :: lams
            REAL(KIND=r8), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=r8), dimension(mgncol), intent(out) :: bergs
            REAL(KIND=r8) :: ab ! correction to account for latent heat
            REAL(KIND=r8) :: eps ! 1/ sat relaxation timescale
=======
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: t ! temperature
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: rho ! air density
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: dv ! water vapor diffusivity
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: mu ! viscosity
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: sc ! schmidt number
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qvl ! saturation humidity (water)
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qvi ! saturation humidity (ice)
            ! fallspeed parameter for snow
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: asn
            ! In-cloud MMRs
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qcic ! cloud liquid
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: qsic ! snow
            ! Size parameters for snow
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: lams
            REAL(KIND=rkind_comp), dimension(mgncol), intent(in) :: n0s
            ! Output tendencies
            REAL(KIND=rkind_comp), dimension(mgncol), intent(out) :: bergs
            REAL(KIND=rkind_comp) :: ab ! correction to account for latent heat
            REAL(KIND=rkind_comp) :: eps ! 1/ sat relaxation timescale
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL, dimension(mgncol) :: loop_mask
            INTEGER :: i
  loop_mask(:) = (qsic(:) >= qsmall.and. qcic(:) >= qsmall .and. t(:) < tmelt)
  do i=1,mgncol
  if (loop_mask(i)) then
     ab = calc_ab(t(i), qvi(i), xxls)
<<<<<<< HEAD
     eps = 2._r8*pi*n0s(i)*rho(i)*Dv(i)* &
          (f1s/(lams(i)*lams(i))+ &
          f2s*(asn(i)*rho(i)/mu(i))**0.5_r8* &
          sc(i)**(1._r8/3._r8)*gamma_half_bs_plus5/ &
          (lams(i)**(5._r8/2._r8+bs/2._r8)))
     bergs(i) = eps*(qvl(i)-qvi(i))/ab
  else
     bergs(i) = 0._r8
=======
     eps = 2._rkind_comp*pi*n0s(i)*rho(i)*Dv(i)* &
          (f1s/(lams(i)*lams(i))+ &
          f2s*(asn(i)*rho(i)/mu(i))**0.5_rkind_comp* &
          sc(i)**(1._rkind_comp/3._rkind_comp)*gamma_half_bs_plus5/ &
          (lams(i)**(5._rkind_comp/2._rkind_comp+bs/2._rkind_comp)))
     bergs(i) = eps*(qvl(i)-qvi(i))/ab
  else
     bergs(i) = 0._rkind_comp
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
  end if
  enddo
        END SUBROUTINE bergeron_process_snow
        !========================================================================
        !UTILITIES
        !========================================================================


        pure FUNCTION limiter_is_on(lim)
<<<<<<< HEAD
            REAL(KIND=r8), intent(in) :: lim
=======
            REAL(KIND=rkind_comp), intent(in) :: lim
>>>>>>> a7fffbad15d5ecf0e3551d46686784ceb706c78a
            LOGICAL :: limiter_is_on
  limiter_is_on = transfer(lim, limiter_off) /= limiter_off
        END FUNCTION limiter_is_on
    END MODULE micro_mg_utils
