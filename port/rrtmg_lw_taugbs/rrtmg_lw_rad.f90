
! KGEN-generated Fortran source file
!
! Filename    : rrtmg_lw_rad.f90
! Generated at: 2015-06-19 17:43:18
! KGEN version: 0.4.12



    MODULE rrtmg_lw_rad
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !  --------------------------------------------------------------------------
        ! |                                                                          |
        ! |  Copyright 2002-2007, Atmospheric & Environmental Research, Inc. (AER).  |
        ! |  This software may be used, copied, or redistributed as long as it is    |
        ! |  not sold and this copyright notice is reproduced on each copy made.     |
        ! |  This model is provided as is without any express or implied warranties. |
        ! |                       (http://www.rtweb.aer.com/)                        |
        ! |                                                                          |
        !  --------------------------------------------------------------------------
        !
        ! ****************************************************************************
        ! *                                                                          *
        ! *                              RRTMG_LW                                    *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                   a rapid radiative transfer model                       *
        ! *                       for the longwave region                            *
        ! *             for application to general circulation models                *
        ! *                                                                          *
        ! *                                                                          *
        ! *            Atmospheric and Environmental Research, Inc.                  *
        ! *                        131 Hartwell Avenue                               *
        ! *                        Lexington, MA 02421                               *
        ! *                                                                          *
        ! *                                                                          *
        ! *                           Eli J. Mlawer                                  *
        ! *                        Jennifer S. Delamere                              *
        ! *                         Michael J. Iacono                                *
        ! *                         Shepard A. Clough                                *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                                                                          *
        ! *                       email:  miacono@aer.com                            *
        ! *                       email:  emlawer@aer.com                            *
        ! *                       email:  jdelamer@aer.com                           *
        ! *                                                                          *
        ! *        The authors wish to acknowledge the contributions of the          *
        ! *        following people:  Steven J. Taubman, Karen Cady-Pereira,         *
        ! *        Patrick D. Brown, Ronald E. Farren, Luke Chen, Robert Bergstrom.  *
        ! *                                                                          *
        ! ****************************************************************************
        ! -------- Modules --------
        USE shr_kind_mod, ONLY: r8 => shr_kind_r8
        !      use parkind, only : jpim, jprb
        ! Move call to rrtmg_lw_ini and following use association to
        ! GCM initialization area
        !      use rrtmg_lw_init, only: rrtmg_lw_ini
        USE rrtmg_lw_taumol, ONLY: taumol
        IMPLICIT NONE
        ! public interfaces/functions/subroutines
        PUBLIC rrtmg_lw
        !------------------------------------------------------------------
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !------------------------------------------------------------------
        !------------------------------------------------------------------
        ! Public subroutines
        !------------------------------------------------------------------

        SUBROUTINE rrtmg_lw(nlay, kgen_unit)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
            ! -------- Description --------
            ! This program is the driver subroutine for RRTMG_LW, the AER LW radiation
            ! model for application to GCMs, that has been adapted from RRTM_LW for
            ! improved efficiency.
            !
            ! NOTE: The call to RRTMG_LW_INI should be moved to the GCM initialization
            !  area, since this has to be called only once.
            !
            ! This routine:
            !    a) calls INATM to read in the atmospheric profile from GCM;
            !       all layering in RRTMG is ordered from surface to toa.
            !    b) calls CLDPRMC to set cloud optical depth for McICA based
            !       on input cloud properties
            !    c) calls SETCOEF to calculate various quantities needed for
            !       the radiative transfer algorithm
            !    d) calls TAUMOL to calculate gaseous optical depths for each
            !       of the 16 spectral bands
            !    e) calls RTRNMC (for both clear and cloudy profiles) to perform the
            !       radiative transfer calculation using McICA, the Monte-Carlo
            !       Independent Column Approximation, to represent sub-grid scale
            !       cloud variability
            !    f) passes the necessary fluxes and cooling rates back to GCM
            !
            ! Two modes of operation are possible:
            !     The mode is chosen by using either rrtmg_lw.nomcica.f90 (to not use
            !     McICA) or rrtmg_lw.f90 (to use McICA) to interface with a GCM.
            !
            !    1) Standard, single forward model calculation (imca = 0)
            !    2) Monte Carlo Independent Column Approximation (McICA, Pincus et al.,
            !       JC, 2003) method is applied to the forward model calculation (imca = 1)
            !
            ! This call to RRTMG_LW must be preceeded by a call to the module
            !     mcica_subcol_gen_lw.f90 to run the McICA sub-column cloud generator,
            !     which will provide the cloud physical or cloud optical properties
            !     on the RRTMG quadrature point (ngpt) dimension.
            !
            ! Two methods of cloud property input are possible:
            !     Cloud properties can be input in one of two ways (controlled by input
            !     flags inflglw, iceflglw, and liqflglw; see text file rrtmg_lw_instructions
            !     and subroutine rrtmg_lw_cldprop.f90 for further details):
            !
            !    1) Input cloud fraction and cloud optical depth directly (inflglw = 0)
            !    2) Input cloud fraction and cloud physical properties (inflglw = 1 or 2);
            !       cloud optical properties are calculated by cldprop or cldprmc based
            !       on input settings of iceflglw and liqflglw
            !
            ! One method of aerosol property input is possible:
            !     Aerosol properties can be input in only one way (controlled by input
            !     flag iaer, see text file rrtmg_lw_instructions for further details):
            !
            !    1) Input aerosol optical depth directly by layer and spectral band (iaer=10);
            !       band average optical depth at the mid-point of each spectral band.
            !       RRTMG_LW currently treats only aerosol absorption;
            !       scattering capability is not presently available.
            !
            !
            ! ------- Modifications -------
            !
            ! This version of RRTMG_LW has been modified from RRTM_LW to use a reduced
            ! set of g-points for application to GCMs.
            !
            !-- Original version (derived from RRTM_LW), reduction of g-points, other
            !   revisions for use with GCMs.
            !     1999: M. J. Iacono, AER, Inc.
            !-- Adapted for use with NCAR/CAM.
            !     May 2004: M. J. Iacono, AER, Inc.
            !-- Revised to add McICA capability.
            !     Nov 2005: M. J. Iacono, AER, Inc.
            !-- Conversion to F90 formatting for consistency with rrtmg_sw.
            !     Feb 2007: M. J. Iacono, AER, Inc.
            !-- Modifications to formatting to use assumed-shape arrays.
            !     Aug 2007: M. J. Iacono, AER, Inc.
            !-- Modified to add longwave aerosol absorption.
            !     Apr 2008: M. J. Iacono, AER, Inc.
            ! --------- Modules ----------
            USE parrrtm, ONLY: ngptlw
            USE parrrtm, ONLY: maxxsec
            USE parrrtm, ONLY: nbndlw
            ! ------- Declarations -------
            ! ----- Input -----
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            ! chunk identifier
            ! Number of horizontal columns
            INTEGER, intent(in) :: nlay ! Number of model layers
            ! Cloud overlap method
            !    0: Clear only
            !    1: Random
            !    2: Maximum/random
            !    3: Maximum
            ! Layer pressures (hPa, mb)
            !    Dimensions: (ncol,nlay)
            ! Interface pressures (hPa, mb)
            !    Dimensions: (ncol,nlay+1)
            ! Layer temperatures (K)
            !    Dimensions: (ncol,nlay)
            ! Interface temperatures (K)
            !    Dimensions: (ncol,nlay+1)
            ! Surface temperature (K)
            !    Dimensions: (ncol)
            ! H2O volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! O3 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! CO2 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! Methane volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! O2 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! Nitrous oxide volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! CFC11 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! CFC12 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! CFC22 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! CCL4 volume mixing ratio
            !    Dimensions: (ncol,nlay)
            ! Surface emissivity
            !    Dimensions: (ncol,nbndlw)
            ! Flag for cloud optical properties
            ! Flag for ice particle specification
            ! Flag for liquid droplet specification
            ! Cloud fraction
            !    Dimensions: (ngptlw,ncol,nlay)
            ! Cloud ice water path (g/m2)
            !    Dimensions: (ngptlw,ncol,nlay)
            ! Cloud liquid water path (g/m2)
            !    Dimensions: (ngptlw,ncol,nlay)
            ! Cloud ice effective radius (microns)
            !    Dimensions: (ncol,nlay)
            ! Cloud water drop effective radius (microns)
            !    Dimensions: (ncol,nlay)
            ! Cloud optical depth
            !    Dimensions: (ngptlw,ncol,nlay)
            !      real(kind=r8), intent(in) :: ssacmcl(:,:,:)      ! Cloud single scattering albedo
            !    Dimensions: (ngptlw,ncol,nlay)
            !   for future expansion
            !   lw scattering not yet available
            !      real(kind=r8), intent(in) :: asmcmcl(:,:,:)      ! Cloud asymmetry parameter
            !    Dimensions: (ngptlw,ncol,nlay)
            !   for future expansion
            !   lw scattering not yet available
            ! aerosol optical depth
            !   at mid-point of LW spectral bands
            !    Dimensions: (ncol,nlay,nbndlw)
            !      real(kind=r8), intent(in) :: ssaaer(:,:,:)       ! aerosol single scattering albedo
            !    Dimensions: (ncol,nlay,nbndlw)
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            !      real(kind=r8), intent(in) :: asmaer(:,:,:)       ! aerosol asymmetry parameter
            !    Dimensions: (ncol,nlay,nbndlw)
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            ! ----- Output -----
            ! Total sky longwave upward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            ! Total sky longwave downward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            ! Total sky longwave radiative heating rate (K/d)
            !    Dimensions: (ncol,nlay)
            ! Clear sky longwave upward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            ! Clear sky longwave downward flux (W/m2)
            !    Dimensions: (ncol,nlay+1)
            ! Clear sky longwave radiative heating rate (K/d)
            !    Dimensions: (ncol,nlay)
            ! Total sky longwave upward flux spectral (W/m2)
            !    Dimensions: (nbndlw,ncol,nlay+1)
            ! Total sky longwave downward flux spectral (W/m2)
            !    Dimensions: (nbndlw,ncol,nlay+1)
            ! ----- Local -----
            ! Control
            ! beginning band of calculation
            ! ending band of calculation
            ! output option flag (inactive)
            ! aerosol option flag
            ! column loop index
            ! flag for mcica [0=off, 1=on]
            ! value for changing mcica permute seed
            ! layer loop index
            ! g-point loop index
            ! Atmosphere
            REAL(KIND=r8) :: pavel(nlay) ! layer pressures (mb)
            ! layer temperatures (K)
            ! level (interface) pressures (hPa, mb)
            ! level (interface) temperatures (K)
            ! surface temperature (K)
            REAL(KIND=r8) :: coldry(nlay) ! dry air column density (mol/cm2)
            ! broadening gas column density (mol/cm2)
            ! molecular amounts (mol/cm-2)
            REAL(KIND=r8) :: wx(maxxsec,nlay) ! cross-section amounts (mol/cm-2)
            ! precipitable water vapor (cm)
            ! lw surface emissivity
            REAL(KIND=r8) :: fracs(nlay,ngptlw)
            REAL(KIND=r8) :: ref_fracs(nlay,ngptlw) !
            REAL(KIND=r8) :: taug(nlay,ngptlw)
            REAL(KIND=r8) :: ref_taug(nlay,ngptlw) ! gaseous optical depths
            ! gaseous + aerosol optical depths
            ! aerosol optical depth
            !      real(kind=r8) :: ssaa(nlay,nbndlw)        ! aerosol single scattering albedo
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            !      real(kind=r8) :: asma(nlay+1,nbndlw)      ! aerosol asymmetry parameter
            !   for future expansion
            !   (lw aerosols/scattering not yet available)
            ! Atmosphere - setcoef
            INTEGER :: laytrop ! tropopause layer index
            INTEGER :: jp(nlay) ! lookup table index
            INTEGER :: jt(nlay) ! lookup table index
            INTEGER :: jt1(nlay) ! lookup table index
            REAL(KIND=r8) :: planklay(nlay,nbndlw) !
            REAL(KIND=r8) :: planklev(0:nlay,nbndlw) !
            REAL(KIND=r8) :: plankbnd(nbndlw) !
            REAL(KIND=r8) :: colh2o(nlay) ! column amount (h2o)
            REAL(KIND=r8) :: colco2(nlay) ! column amount (co2)
            REAL(KIND=r8) :: colo3(nlay) ! column amount (o3)
            REAL(KIND=r8) :: coln2o(nlay) ! column amount (n2o)
            REAL(KIND=r8) :: colco(nlay) ! column amount (co)
            REAL(KIND=r8) :: colch4(nlay) ! column amount (ch4)
            REAL(KIND=r8) :: colo2(nlay) ! column amount (o2)
            REAL(KIND=r8) :: colbrd(nlay) ! column amount (broadening gases)
            INTEGER :: indself(nlay)
            INTEGER :: indfor(nlay)
            REAL(KIND=r8) :: selffac(nlay)
            REAL(KIND=r8) :: selffrac(nlay)
            REAL(KIND=r8) :: forfac(nlay)
            REAL(KIND=r8) :: forfrac(nlay)
            INTEGER :: indminor(nlay)
            REAL(KIND=r8) :: minorfrac(nlay)
            REAL(KIND=r8) :: scaleminor(nlay)
            REAL(KIND=r8) :: scaleminorn2(nlay)
            REAL(KIND=r8) :: fac00(nlay)
            REAL(KIND=r8) :: fac01(nlay)
            REAL(KIND=r8) :: fac10(nlay)
            REAL(KIND=r8) :: fac11(nlay) !
            REAL(KIND=r8) :: rat_h2och4(nlay)
            REAL(KIND=r8) :: rat_o3co2(nlay)
            REAL(KIND=r8) :: rat_h2on2o(nlay)
            REAL(KIND=r8) :: rat_h2och4_1(nlay)
            REAL(KIND=r8) :: rat_h2on2o_1(nlay)
            REAL(KIND=r8) :: rat_o3co2_1(nlay)
            REAL(KIND=r8) :: rat_h2oco2_1(nlay)
            REAL(KIND=r8) :: rat_n2oco2(nlay)
            REAL(KIND=r8) :: rat_h2oco2(nlay)
            REAL(KIND=r8) :: rat_h2oo3(nlay)
            REAL(KIND=r8) :: rat_n2oco2_1(nlay)
            REAL(KIND=r8) :: rat_h2oo3_1(nlay) !
            ! Atmosphere/clouds - cldprop
            ! number of cloud spectral bands
            ! flag for cloud property method
            ! flag for ice cloud properties
            ! flag for liquid cloud properties
            ! Atmosphere/clouds - cldprmc [mcica]
            ! cloud fraction [mcica]
            ! cloud ice water path [mcica]
            ! cloud liquid water path [mcica]
            ! liquid particle size (microns)
            ! ice particle effective radius (microns)
            ! ice particle generalized effective size (microns)
            ! cloud optical depth [mcica]
            !      real(kind=r8) :: ssacmc(ngptlw,nlay)     ! cloud single scattering albedo [mcica]
            !   for future expansion
            !   (lw scattering not yet available)
            !      real(kind=r8) :: asmcmc(ngptlw,nlay)     ! cloud asymmetry parameter [mcica]
            !   for future expansion
            !   (lw scattering not yet available)
            ! Output
            ! upward longwave flux (w/m2)
            ! downward longwave flux (w/m2)
            ! upward longwave flux spectral (w/m2)
            ! downward longwave flux spectral (w/m2)
            ! net longwave flux (w/m2)
            ! longwave heating rate (k/day)
            ! clear sky upward longwave flux (w/m2)
            ! clear sky downward longwave flux (w/m2)
            ! clear sky net longwave flux (w/m2)
            ! clear sky longwave heating rate (k/day)
            ! Initializations
            ! orig:   fluxfac = pi * 2.d4
            ! Set imca to select calculation type:
            !  imca = 0, use standard forward model calculation
            !  imca = 1, use McICA for Monte Carlo treatment of sub-grid cloud variability
            ! *** This version uses McICA (imca = 1) ***
            ! Set icld to select of clear or cloud calculation and cloud overlap method
            ! icld = 0, clear only
            ! icld = 1, with clouds using random cloud overlap
            ! icld = 2, with clouds using maximum/random cloud overlap
            ! icld = 3, with clouds using maximum cloud overlap (McICA only)
            ! Set iaer to select aerosol option
            ! iaer = 0, no aerosols
            ! iaer = 10, input total aerosol optical depth (tauaer) directly
            ! Call model and data initialization, compute lookup tables, perform
            ! reduction of g-points from 256 to 140 for input absorption coefficient
            ! data and other arrays.
            !
            ! In a GCM this call should be placed in the model initialization
            ! area, since this has to be called only once.
            !      call rrtmg_lw_ini
            !  This is the main longitude/column loop within RRTMG.
                tolerance = 1.E-14
                CALL kgen_init_check(check_status, tolerance)
                READ(UNIT=kgen_unit) pavel
                READ(UNIT=kgen_unit) coldry
                READ(UNIT=kgen_unit) wx
                READ(UNIT=kgen_unit) fracs
                READ(UNIT=kgen_unit) taug
                READ(UNIT=kgen_unit) laytrop
                READ(UNIT=kgen_unit) jp
                READ(UNIT=kgen_unit) jt
                READ(UNIT=kgen_unit) jt1
                READ(UNIT=kgen_unit) planklay
                READ(UNIT=kgen_unit) planklev
                READ(UNIT=kgen_unit) plankbnd
                READ(UNIT=kgen_unit) colh2o
                READ(UNIT=kgen_unit) colco2
                READ(UNIT=kgen_unit) colo3
                READ(UNIT=kgen_unit) coln2o
                READ(UNIT=kgen_unit) colco
                READ(UNIT=kgen_unit) colch4
                READ(UNIT=kgen_unit) colo2
                READ(UNIT=kgen_unit) colbrd
                READ(UNIT=kgen_unit) indself
                READ(UNIT=kgen_unit) indfor
                READ(UNIT=kgen_unit) selffac
                READ(UNIT=kgen_unit) selffrac
                READ(UNIT=kgen_unit) forfac
                READ(UNIT=kgen_unit) forfrac
                READ(UNIT=kgen_unit) indminor
                READ(UNIT=kgen_unit) minorfrac
                READ(UNIT=kgen_unit) scaleminor
                READ(UNIT=kgen_unit) scaleminorn2
                READ(UNIT=kgen_unit) fac00
                READ(UNIT=kgen_unit) fac01
                READ(UNIT=kgen_unit) fac10
                READ(UNIT=kgen_unit) fac11
                READ(UNIT=kgen_unit) rat_h2och4
                READ(UNIT=kgen_unit) rat_o3co2
                READ(UNIT=kgen_unit) rat_h2on2o
                READ(UNIT=kgen_unit) rat_h2och4_1
                READ(UNIT=kgen_unit) rat_h2on2o_1
                READ(UNIT=kgen_unit) rat_o3co2_1
                READ(UNIT=kgen_unit) rat_h2oco2_1
                READ(UNIT=kgen_unit) rat_n2oco2
                READ(UNIT=kgen_unit) rat_h2oco2
                READ(UNIT=kgen_unit) rat_h2oo3
                READ(UNIT=kgen_unit) rat_n2oco2_1
                READ(UNIT=kgen_unit) rat_h2oo3_1

                READ(UNIT=kgen_unit) ref_fracs
                READ(UNIT=kgen_unit) ref_taug


                ! call to kernel
         call taumol(nlay, pavel, wx, coldry, &
                     laytrop, jp, jt, jt1, planklay, planklev, plankbnd, &
                     colh2o, colco2, colo3, coln2o, colco, colch4, colo2, &
                     colbrd, fac00, fac01, fac10, fac11, &
                     rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, &
                     rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, &
                     rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, &
                     selffac, selffrac, indself, forfac, forfrac, indfor, &
                     minorfrac, scaleminor, scaleminorn2, indminor, &
                     fracs, taug)
                ! kernel verification for output variables
                CALL kgen_verify_real_r8_dim2( "fracs", check_status, fracs, ref_fracs)
                CALL kgen_verify_real_r8_dim2( "taug", check_status, taug, ref_taug)
                CALL kgen_print_check("taumol", check_status)
                CALL system_clock(start_clock, rate_clock)
                DO kgen_intvar=1,10
                    CALL taumol(nlay, pavel, wx, coldry, laytrop, jp, jt, jt1, planklay, planklev, plankbnd, colh2o, colco2, colo3, coln2o, colco, colch4, colo2, colbrd, fac00, fac01, fac10, fac11, rat_h2oco2, rat_h2oco2_1, rat_h2oo3, rat_h2oo3_1, rat_h2on2o, rat_h2on2o_1, rat_h2och4, rat_h2och4_1, rat_n2oco2, rat_n2oco2_1, rat_o3co2, rat_o3co2_1, selffac, selffrac, indself, forfac, forfrac, indfor, minorfrac, scaleminor, scaleminorn2, indminor, fracs, taug)
                END DO
                CALL system_clock(stop_clock, rate_clock)
                WRITE(*,*)
                PRINT *, "Elapsed time (sec): ", (stop_clock - start_clock)/REAL(rate_clock*10)
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_r8_dim2(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:) :: var
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
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim2


        ! verify subroutines
            SUBROUTINE kgen_verify_real_r8_dim2( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                real(KIND=r8), intent(in), DIMENSION(:,:) :: var, ref_var
                real(KIND=r8) :: nrmsdiff, rmsdiff
                real(KIND=r8), allocatable, DIMENSION(:,:) :: temp, temp2
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
            END SUBROUTINE kgen_verify_real_r8_dim2

        END SUBROUTINE rrtmg_lw
        !***************************************************************************

    END MODULE rrtmg_lw_rad
