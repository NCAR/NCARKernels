
! KGEN-generated Fortran source file
!
! Filename    : ecosys_mod.F90
! Generated at: 2015-06-05 14:52:11
! KGEN version: 0.4.11



    MODULE ecosys_mod
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb_real
        !BOP
        ! !MODULE: ecosys_mod
        !
        ! !DESCRIPTION:
        !
        !  Multispecies ecosystem based on Doney et al. 1996, Moore et al., 2002
        !  Based on POP Global NCAR Nitrogen Ecosystem Model
        !  version 0.0 (June 15th, 1998) from S.C. Doney.
        !  Based on Doney et al., 1996 model.
        !  Climate and Global Dynamics, NCAR
        !  (doney@whoi.edu)
        !
        !  Version 1.0
        !  Multispecies, multiple limiting nutrient version of ecosystem
        !  based on mixed layer model of Moore et al.(2002).  Implemented here with
        !  fixed elemental ratios and including only the diatoms and small
        !  phytoplankton, with a parameterization of calcification,
        !  by Keith Lindsay and Keith Moore, Fall 2001 - Spring 2002.
        !  Calcification parameterization based on Moore et al. 2002.
        !
        !  Version 2.0, January 2003
        !    Adds diazotrophs as a phytoplankton group, (based on Moore et al., 2002a)
        !    Allows for variable fe/C for all phytoplankton groups
        !     Allows for variable si/C for the diatoms
        !     Adds explicit tracers for DON, DOP, DOFe
        !     variable remin length scale for detrital soft POM and bSi f(temperature)
        !     Extensive modifications to iron scavenging parameterization
        !     Addition of a sedimentary dissolved iron source,
        !        (implemented in ballast code as excess remin in bottom cell)
        !        coded by J.K. Moore, (jkmoore@uci.edu)
        !
        !   Version 2.01. March 2003
        !     corrected O2 bug
        !     corrected grazing parameter z_grz bug at depth
        !     dust dissolution at depth releases iron,
        !     increased length scale for dust diss., increased hard fraction dust
        !     no deep ocean reduction in scavenging rates,
        !     increase bSi OC/ballast ratio 0.3 -> 0.35,
        !     corrected bug in diazotroph photoadaptation, and diat and sp adapatation
        !
        !   Version 2.02.
        !     corrected bug in Fe_scavenge (units for dust), May 2003
        !     changed C/N/P ratios to 117/16/1 (Anderson & Sarmiento, 1994)
        !
        !   Version 2.03., July 2003
        !     Remin of DOM no longer temperature dependent,
        !     new iron scavenging parameterization added,
        !     some dissolution of hard fraction of ballast materials added
        !
        !   Version 2.1, September 2003
        !     modfied iron scavenging and dust dissolution at depth
        !
        !   Version 2.11, March 2004
        !     fixed bug in iron scavenging code, replace dust and POC flux_in w/ flux_out
        !
        !   Version 2.12, April 2004 - Final version for GBC paper revision,
        !     (Questions/comments, Keith Moore - jkmoore@uci.edu
        !
        !   References
        !   Doney, S.C., Glover, D.M., Najjar, R.G., 1996. A new coupled, one-dimensional
        !   biological-physical model for the upper ocean: applications to the JGOFS
        !   Bermuda Time-Series Study (BATS) site. Deep-Sea Res. II, 43: 591-624.
        !
        !   Moore, JK, Doney, SC, Kleypas, JA, Glover, DM, Fung, IY, 2002. An intermediate
        !   complexity marine ecosystem model for the global domain. Deep-Sea Res. II, 49:
        !   403-462.
        !
        !   Moore, JK, Doney, SC, Glover, DM, Fung, IY, 2002. Iron cycling and nutrient
        !   limitation patterns in surface waters of the world ocean. Deep-Sea Res. II,
        !   49: 463-507.
        ! !REVISION HISTORY:
        !  SVN:$Id:  $
        !-----------------------------------------------------------------------
        !  variables/subroutines/function used from other modules
        !  The following are used extensively in this ecosys, so are used at
        !  the module level. The use statements for variables that are only needed
        !  locally are located at the module subprogram level.
        !-----------------------------------------------------------------------
        ! !USES:
        USE kinds_mod, only : int_kind
        USE kinds_mod, only : r8
        USE blocks, only : nx_block
        USE blocks, only : ny_block
        USE kinds_mod, only : log_kind
        USE grid, ONLY: kmt
        USE co2calc, only : comp_co3terms
        ! !INPUT PARAMETERS:
        !-----------------------------------------------------------------------
        !  include ecosystem parameters
        !  all variables from this modules have a parm_ prefix
        !-----------------------------------------------------------------------
        IMPLICIT NONE
        PUBLIC kgen_read_externs_ecosys_mod
        PUBLIC ecosys_set_interior
        PRIVATE
        !-----------------------------------------------------------------------
        !  public/private declarations
        !-----------------------------------------------------------------------
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !  flags controlling which portion of code are executed
        !  usefull for debugging
        !-----------------------------------------------------------------------
        LOGICAL(KIND=log_kind), dimension(:,:,:), allocatable :: land_mask
        !-----------------------------------------------------------------------
        !  non-autotroph relative tracer indices
        !  autotroph relative tracer indices are in autotroph derived type and are determined at run time
        !-----------------------------------------------------------------------
        ! dissolved inorganic phosphate
        ! dissolved inorganic nitrate
        ! dissolved inorganic silicate
        ! dissolved ammonia
        ! dissolved inorganic iron
        ! dissolved oxygen
        ! dissolved inorganic carbon
        ! dissolved inorganic carbon with alternative CO2
        ! alkalinity
        ! dissolved organic carbon
        ! dissolved organic nitrogen
        ! dissolved organic iron
        ! dissolved organic phosphorus
        ! refractory DOP
        ! refractory DON
        !-----------------------------------------------------------------------
        !  derived type & parameter for tracer index lookup
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        !  options for forcing of gas fluxes
        !-----------------------------------------------------------------------
        ! value of atmospheric co2 (ppm, dry-air, 1 atm)
        ! value of atmospheric alternative co2 (ppm, dry-air, 1 atm)
        ! file containing gas flux forcing fields
        !-----------------------------------------------------------------------
        ! ice fraction for gas fluxes
        ! wind speed for gas fluxes
        ! atmospheric pressure for gas fluxes
        ! namelist input for iron_flux
        !-----------------------------------------------------------------------
        !  module variables related to ph computations
        !-----------------------------------------------------------------------
        ! computed ph from previous time step
        ! computed ph from previous time step, alternative CO2
        ! localized iron patch flux
        ! dust flux not stored in STF since dust is not prognostic
        ! computed pH_3D from previous time step
        ! computed pH_3D from previous time step, alternative CO2
        !-----------------------------------------------------------------------
        !  restoring climatologies for nutrients
        !-----------------------------------------------------------------------
        ! inverse restoring time scale for nutrients (1/secs)
        !  sedimentary Fe inputs
        ! file containing nutrient fields
        !maltrud variable restoring
        ! geographically varying nutrient restoring
        ! file containing variable restoring info
        ! format of file containing variable restoring info
        ! inverse restoring timescale for variable
        ! interior restoring
        ! maximum level for applying variable
        ! interior restoring
        ! temp array for interpolate_forcing output
        ! surface dust flux
        ! iron component of surface dust flux
        ! ice fraction, if read from file
        ! a * wind-speed ** 2, if read from file
        ! atmoshperic pressure, if read from file
        ! type of ndep forcing
        ! surface NOx species flux, added to nitrate pool
        ! surface NHy species flux, added to ammonium pool
        ! first year in stream to use
        ! last year in stream to use
        ! align ndep_shr_stream_year_first with this model year
        ! number of variables in ndep shr_stream
        ! index for NO forcing
        ! index for NH forcing
        ! file containing domain and input data
        ! unit conversion factor
        ! input data stream for ndep
        ! river DIN species flux, added to nitrate pool
        ! river DIP species flux, added to phosphate pool
        ! river DON flux, added to semi-lab don pool
        ! river DOP flux, added to semi-lab dop pool
        ! river DSI flux, added to dsi pool
        ! river dfe flux, added to dfe pool
        ! river dic flux, added to dic pool
        ! river alk flux, added to alk pool
        ! river doc flux, added to semi-labile DOC
        !-----------------------------------------------------------------------
        !  tavg ids and buffer indices (into ECO_SFLUX_TAVG) for 2d fields related to surface fluxes
        !  duplicates, which are used for placing fields into multiple tavg streams,
        !  do not need separate buffer indices
        !  fields that are recoverable from the STF field do not need separate buffer indices
        !-----------------------------------------------------------------------
        ! ice fraction
        ! ice fraction duplicate
        ! xkw
        ! xkw duplicate
        ! atmospheric pressure
        ! o2 piston velocity
        ! O2 schmidt number
        ! O2 saturation
        ! O2 flux duplicate
        ! co2star
        ! dco2star
        ! surface pco2
        ! delta pco2
        ! delta pco2 duplicate
        ! co2 piston velocity
        ! co2 schmidt number
        ! dic flux
        ! dic flux duplicate
        ! surface pH
        ! atmospheric CO2
        ! co2star alternative CO2
        ! dco2star alternative CO2
        ! surface pco2 alternative CO2
        ! delta pco2 alternative CO2
        ! dic flux alternative CO2
        ! surface pH alternative CO2
        ! atmospheric alternative CO2
        ! iron flux
        ! dust flux
        ! nox flux
        ! nhy flux
        ! din river flux
        ! dip river flux
        ! don river flux
        ! donr river flux
        ! dop river flux
        ! dopr river flux
        ! dsi river flux
        ! dfe river flux
        ! dic river flux
        ! alk river flux
        ! doc river flux
        !-----------------------------------------------------------------------
        !  define tavg id for nonstandard 2d fields
        !-----------------------------------------------------------------------
        ! tavg id for vertical minimum of O2
        ! tavg id for depth of vertical minimum of O2
        !-----------------------------------------------------------------------
        !  define tavg id for nonstandard 3d fields
        !-----------------------------------------------------------------------
        ! tavg id for o2 production
        ! tavg id for o2 consumption
        ! tavg id for AOU
        ! tavg id for available radiation avg over mixed layer
        ! tavg id for poc flux into cell
        ! tavg id for poc production
        ! tavg id for poc remineralization
        ! tavg id for poc accumulation
        ! tavg id for caco3 flux into cell
        ! tavg id for caco3 production
        ! tavg id for caco3 remineralization
        ! tavg id for sio2 flux into cell
        ! tavg id for sio2 production
        ! tavg id for sio2 remineralization
        ! tavg id for dust flux into cell
        ! tavg id for dust remineralization
        ! tavg id for p_iron flux into cell
        ! tavg id for p_iron production
        ! tavg id for p_iron remineralization
        ! tavg id for total autotroph grazing
        !-----------------------------------------------------------------------
        !  define tavg id for MORE nonstandard 3d fields
        !-----------------------------------------------------------------------
        ! tavg id for total C fixation
        ! tavg id for total C fixation vertical integral
        ! tavg id for total C fixation from NO3
        ! tavg id for total C fixation from NO3 vertical integral
        !-----------------------------------------------------------------------
        !  define tavg id for MORE nonstandard 3d fields
        !-----------------------------------------------------------------------
        ! tavg id for doc production
        ! tavg id for doc remineralization
        ! tavg id for don production
        ! tavg id for don remineralization
        ! tavg id for dofe production
        ! tavg id for dofe remineralization
        ! tavg id for dop production
        ! tavg id for dop remineralization
        ! tavg id for iron scavenging
        ! tavg id for iron scavenging rate
        ! tavg id for nitrification
        ! tavg id for denitrification
        ! tavg id for DONrefractory remin
        ! tavg id for DOPrefractory remin
        ! tavg id for zooplankton loss
        ! tavg id for zooplankton loss to poc
        ! tavg id for zooplankton loss to doc
        ! tavg id for zooplankton grazing
        ! tavg id for zooplankton grazing to poc
        ! tavg id for zooplankton grazing to doc
        ! tavg id for zooplankton grazing to zoo
        ! tavg id for zooplankton grazing assimilation
        ! tavg id for N limitation
        ! tavg id for P limitation
        ! tavg id for Fe limitation
        ! tavg id for SiO3 limitation
        ! tavg id for light limitation
        ! tavg id for C fixation
        ! tavg id for C fixation vertical integral
        ! tavg id for C fixation from NO3
        ! tavg id for C fixation from NO3 vertical integral
        ! tavg id for Fe uptake
        ! tavg id for NO3 uptake
        ! tavg id for NH4 uptake
        ! tavg id for DOP uptake
        ! tavg id for PO4 uptake
        ! tavg id for autotroph grazing
        ! tavg id for autotroph grazing to poc
        ! tavg id for autotroph grazing to doc
        ! tavg id for autotroph grazing to zoo
        ! tavg id for autotroph loss
        ! tavg id for autotroph loss to poc
        ! tavg id for autotroph loss to doc
        ! tavg id for autotroph aggregate
        ! tavg id for Si uptake
        ! tavg id for CaCO3 formation
        ! tavg id for CaCO3 formation vertical integral
        ! tavg id for N fixation
        ! tavg id for Si uptake
        ! tavg id for CaCO3 formation
        ! tavg id for CaCO3 formation vertical integral
        ! tavg id for N fixation
        ! tavg id for 3D carbonate ion
        ! tavg id for 3D bicarbonate ion
        ! tavg id for 3D carbonic acid
        ! tavg id for 3D pH
        ! tavg id for 3D carbonate ion, alternative CO2
        ! tavg id for 3D bicarbonate ion, alternative CO2
        ! tavg id for 3D carbonic acid, alternative CO2
        ! tavg id for 3D pH, alternative CO2
        ! tavg id for co3 concentration at calcite saturation
        ! tavg id for calcite saturation depth
        ! tavg id for co3 concentration at aragonite saturation
        ! tavg id for aragonite saturation depth
        ! tavg id for calcite flux sedimentary burial
        ! tavg id for poc burial flux to sediments
        ! tavg id for pon burial flux to sediments
        ! tavg id for pop burial flux to sediments
        ! tavg id for bsi burial flux to sediments
        ! tavg id for dust burial flux to sediments
        ! tavg id for pFe burial flux to sediments
        ! tavg id for sedimentary denitrification
        ! tavg id for non-oxic, non-denitr sed remin
        ! tavg id for vertical integral of conservative subterms of source sink term for Ctot
        ! tavg id for vertical integral of conservative subterms of source sink term for Ctot, 0-100m
        ! tavg id for vertical integral of conservative subterms of source sink term for Ntot
        ! tavg id for vertical integral of conservative subterms of source sink term for Ntot, 0-100m
        ! tavg id for vertical integral of conservative subterms of source sink term for Ptot
        ! tavg id for vertical integral of conservative subterms of source sink term for Ptot, 0-100m
        ! tavg id for vertical integral of conservative subterms of source sink term for Sitot
        ! tavg id for vertical integral of conservative subterms of source sink term for Sitot, 0-100m
        !-----------------------------------------------------------------------
        !  define array for holding flux-related quantities that need to be time-averaged
        !  this is necessary since the forcing routines are called before tavg flags
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        !  average surface tracer value related variables
        !  used as reference value for virtual flux computations
        !-----------------------------------------------------------------------
        ! which tracers get virtual fluxes applied
        ! time flag id for computing average
        ! surface tracer values
        ! average surface tracer values
        !-----------------------------------------------------------------------
        !  iron patch fertilization
        !-----------------------------------------------------------------------
        ! flag for iron patch fertilization
        ! file containing name of iron patch file
        !  integer month to add patch flux
        !-----------------------------------------------------------------------
        !  timers
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        !  named field indices
        !-----------------------------------------------------------------------
        ! total chlorophyll in surface layer
        ! air-sea co2 gas flux
        ! atmospheric co2
        !-----------------------------------------------------------------------
        ! photosynthetically available radiation (W/m^2)
        !-----------------------------------------------------------------------
        ! low bound for surface ph for no prev soln
        ! high bound for surface ph for no prev soln
        ! low bound for subsurface ph for no prev soln
        ! high bound for subsurface ph for no prev soln
        ! delta-ph for prev soln
        !-----------------------------------------------------------------------
        !EOC
        !*****************************************************************************
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_logical_log_kind_dim3_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                logical(KIND=log_kind), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:) :: var
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
            END SUBROUTINE kgen_read_logical_log_kind_dim3_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_ecosys_mod(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_logical_log_kind_dim3_alloc(land_mask, kgen_unit)
        END SUBROUTINE kgen_read_externs_ecosys_mod

        !*****************************************************************************
        !BOP
        ! !IROUTINE: ecosys_init
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ecosys_init_tavg
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ecosys_set_interior
        ! !INTERFACE:

        SUBROUTINE ecosys_set_interior(k, kgen_unit, usecs)
                USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
#ifdef _MPI
                use mpi
#endif
            ! !DESCRIPTION:
            !  Compute time derivatives for ecosystem state variables
            !
            ! !REVISION HISTORY:
            !  same as module
            ! !INPUT PARAMETERS:
            integer, intent(in) :: kgen_unit
            INTEGER*8 :: kgen_intvar, start_clock, stop_clock, rate_clock
            TYPE(check_t):: check_status
            REAL(KIND=kgen_dp) :: tolerance
            INTEGER(KIND=int_kind), intent(in) :: k

            real(kind=8), intent(out) :: usecs
            ! vertical level index
            ! old potential temperature (C)
            ! current potential temperature (C)
            ! old salinity (msu)
            ! current salinity (msu)
            ! old tracer values
            ! current tracer values
            ! flag to save shared_vars or not
            ! block info for the current block
            ! !OUTPUT PARAMETERS:
            ! computed source/sink terms
            !EOP
            !BOC
            !-----------------------------------------------------------------------
            !  local variables
            !-----------------------------------------------------------------------
            ! small C concentration (mmol C/m^3)
            ! small inverse time scale (1/year) (1/sec)
            ! small non-dimensional number (non-dim)
            !     POC               ! base units = nmol C -> Defined in ecosys_share
            !     P_CaCO3           ! base units = nmol CaCO3 -> Defined in ecosys_share
            ! base units = nmol SiO2
            ! base units = g
            ! base units = nmol Fe
            ! incoming deficit in the QA(dust) POC flux
            ! sedimentary denitrification (nmol N/cm^3/sec)
            ! organic C remin not due oxic or denitrif (nmolC/cm^3/sec)
            ! Calcite Saturation Depth
            ! Aragonite Saturation Depth
            ! CO3 concentration above calcite saturation at k-1
            ! CO3 concentration above aragonite saturation at k-1
            ! FIXME(bja, 2014-10) size should be (nx, ny, non_autotroph_ecosys_tracer_cnt)
            ! local copies of model tracer concentrations
            ! local restoring terms for nutrients (mmol ./m^3/sec)
            REAL(KIND=r8), dimension(nx_block,ny_block) :: sio3_loc
            REAL(KIND=r8), dimension(nx_block,ny_block) :: temp
            REAL(KIND=r8), dimension(nx_block,ny_block) :: salt
            REAL(KIND=r8), dimension(nx_block,ny_block) :: alk_loc
            REAL(KIND=r8), dimension(nx_block,ny_block) :: dic_loc
            REAL(KIND=r8), dimension(nx_block,ny_block) :: po4_loc
            ! local copy of model TEMP
            ! local copy of model SALT
            ! local copy of model DIC
            ! local copy of model DIC_ALT_CO2
            ! local copy of model ALK
            ! local copy of model PO4
            ! local copy of model NO3
            ! local copy of model SiO3
            ! local copy of model NH4
            ! local copy of model Fe
            ! local copy of model O2
            ! local copy of model DOC
            ! local copy of model DON
            ! local copy of model DOFe
            ! local copy of model DOP
            ! local copy of model DOPr
            ! local copy of model DONr
            ! local copy of model zooplankton C
            ! local copy of model autotroph Chl
            ! local copy of model autotroph C
            ! local copy of model autotroph Fe
            ! local copy of model autotroph Si
            ! local copy of model autotroph CaCO3
            REAL(KIND=r8), dimension(nx_block,ny_block) :: work1
            REAL(KIND=r8) :: ref_work1(nx_block,ny_block)
            REAL(KIND=r8), dimension(nx_block,ny_block) :: work3
            REAL(KIND=r8) :: ref_work3(nx_block,ny_block)
            REAL(KIND=r8), dimension(nx_block,ny_block) :: work2
            REAL(KIND=r8) :: ref_work2(nx_block,ny_block)
            ! temporaries
            ! fraction of grazing loss reduction at depth
            ! depth of top of cell
            ! photosynthetically available radiation (W/m^2)
            ! PAR adsorption coefficient (non-dim)
            ! average PAR over mixed layer depth (W/m^2)
            ! production of DOC (mmol C/m^3/sec)
            ! remineralization of DOC (mmol C/m^3/sec)
            ! portion of DON remineralized
            ! portion of DOFe remineralized
            ! portion of DOP remineralized
            ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
            ! WC nitrification (NO3 -> N2) (mmol N/m^3/sec)
            ! max. zoo growth rate at local T (1/sec)
            ! bio-C threshold at which losses go to zero (mmol C/m^3)
            ! temp response function GD98 (non-dim)
            ! nut limitation factor, modifies C fixation (non-dim)
            ! max value of PCphoto at temperature TEMP (1/sec)
            ! light limitation factor
            ! Chl synth. regulation term (mg Chl/mmol N)
            ! max of 39 continuation lines
            ! annual scavenging rate of iron as % of ambient
            ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
            ! C-specific N uptake rate (mmol N/mmol C/sec)
            ! C-specific PO4 uptake (non-dim)
            ! C-specific DOP uptake rate (non-dim)
            ! total P uptake rate (non-dim)
            ! C-specific Fe uptake (non-dim)
            ! C-specific SiO3 uptake (non-dim)
            ! C-specific rate of photosynth. (1/sec)
            ! local Chl/C ratio (mg Chl/mmol C)
            ! CaCO3/C ratio (mmol CaCO3/mmol C)
            ! NO3 uptake rate (non-dim)
            ! NH4 uptake rate (non-dim)
            ! total N uptake rate (non-dim)
            ! nitrate uptake (mmol NO3/m^3/sec)
            ! ammonium uptake (mmol NH4/m^3/sec)
            ! PO4 uptake (mmol PO4/m^3/sec)
            ! DOP uptake (mmol DOP/m^3/sec)
            ! init fe/C ratio (mmolFe/mmolC)
            ! fe/C for growth
            ! initial Si/C ratio (mmol Si/mmol C)
            ! diatom Si/C ratio for growth (new biomass)
            ! used to limit autotroph mort at low biomass (mmol C/m^3)
            ! autotroph grazing rate (mmol C/m^3/sec)
            ! auto_graze routed to zoo (mmol C/m^3/sec)
            ! auto_graze routed to poc (mmol C/m^3/sec)
            ! auto_graze routed to doc (mmol C/m^3/sec)
            ! auto_graze routed to dic (mmol C/m^3/sec)
            ! autotroph non-grazing mort (mmol C/m^3/sec)
            ! auto_loss routed to poc (mmol C/m^3/sec)
            ! auto_loss routed to doc (mmol C/m^3/sec)
            ! auto_loss routed to dic (mmol C/m^3/sec)
            ! autotroph aggregation (mmol C/m^3/sec)
            ! C-fixation (mmol C/m^3/sec)
            ! iron uptake
            ! silicon uptake (mmol Si/m^3/sec)
            ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
            ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
            ! total Nitrogen fixation (mmol N/m^3/sec)
            ! fixed N excretion
            ! used to limit zoo mort at low biomass (mmol C/m^3)
            ! frac of zoo losses into large detrital pool (non-dim)
            ! {auto,zoo}_graze routed to zoo (mmol C/m^3/sec)
            ! zooplankton losses due to grazing (mmol C/m^3/sec)
            ! grazing of zooplankton routed to zoo (mmol C/m^3/sec)
            ! grazing of zooplankton routed to poc (mmol C/m^3/sec)
            ! grazing of zooplankton routed to doc (mmol C/m^3/sec)
            ! grazing of zooplankton routed to dic (mmol C/m^3/sec)
            ! mortality & higher trophic grazing on zooplankton (mmol C/m^3/sec)
            ! zoo_loss routed to poc (mmol C/m^3/sec)
            ! zoo_loss routed to doc (mmol C/m^3/sec)
            ! zoo_loss routed to dic (mmol C/m^3/sec)
            ! used in routing P from autotrophs w/ Qp different from Qp_zoo_pom
            ! remaining_P from mort routed to DOP pool
            ! remaining_P from mort routed to remin
            ! production of dissolved organic N
            ! produciton of dissolved organic Fe
            ! production of dissolved organic P
            ! constant used in Fe quota modification
            ! constant used in Si quota modification
            ! O2 production
            ! O2 consumption
            ! portion of refractory DON remineralized
            ! portion of refractory DOP remineralized
            REAL(KIND=r8), dimension(nx_block,ny_block) :: co3
            REAL(KIND=r8) :: ref_co3(nx_block,ny_block)
            REAL(KIND=r8), dimension(nx_block,ny_block) :: hco3
            REAL(KIND=r8) :: ref_hco3(nx_block,ny_block)
            REAL(KIND=r8), dimension(nx_block,ny_block) :: h2co3
            REAL(KIND=r8) :: ref_h2co3(nx_block,ny_block)
            ! carbonate ion
            ! bicarbonate ion
            ! carbonic acid
            ! carbonate ion, alternative CO2
            ! bicarbonate ion, alternative CO2
            ! carbonic acid, alternative CO2
            ! solubility ratio for aragonite
            ! solubility ratio for calcite
            INTEGER(KIND=int_kind) :: j
            INTEGER(KIND=int_kind) :: bid
            ! local_block id
            ! tracer index
            ! autotroph functional group index
            ! autotroph functional group index
            ! zooplankton functional group index
            ! zooplankton functional group index
            ! grazee group index
            ! grazer group index
            ! index for looping over k levels
            ! index for looping over ny_block dimension
            ! are any alt_co2 terms being time averaged
            integer :: iError
                        tolerance = 2.E-14
                        CALL kgen_init_check(check_status, tolerance)
                        READ(UNIT=kgen_unit) sio3_loc
                        READ(UNIT=kgen_unit) temp
                        READ(UNIT=kgen_unit) salt
                        READ(UNIT=kgen_unit) alk_loc
                        READ(UNIT=kgen_unit) dic_loc
                        READ(UNIT=kgen_unit) po4_loc
                        READ(UNIT=kgen_unit) work1
                        READ(UNIT=kgen_unit) work3
                        READ(UNIT=kgen_unit) work2
                        READ(UNIT=kgen_unit) co3
                        READ(UNIT=kgen_unit) hco3
                        READ(UNIT=kgen_unit) h2co3
                        READ(UNIT=kgen_unit) j
                        READ(UNIT=kgen_unit) bid

                        READ(UNIT=kgen_unit) ref_work1
                        READ(UNIT=kgen_unit) ref_work3
                        READ(UNIT=kgen_unit) ref_work2
                        READ(UNIT=kgen_unit) ref_co3
                        READ(UNIT=kgen_unit) ref_hco3
                        READ(UNIT=kgen_unit) ref_h2co3

!bpd6
!write(*,*) "Shape temp: ", shape(temp), j
!call kgen_perturb_real(temp, epsilon(1.0_8))
!call kgen_perturb_real(temp, 1.3e-15_8)


                        ! call to kernel
         call comp_CO3terms(bid, j, k, LAND_MASK(:,j,bid) .and. k <= KMT(:,j,bid), .true., &
                            TEMP(:,j), SALT(:,j), DIC_loc(:,j), ALK_loc(:,j), PO4_loc(:,j), SiO3_loc(:,j), &
                            WORK1(:,j), WORK2(:,j), WORK3(:,j), H2CO3(:,j), HCO3(:,j), CO3(:,j))

                        ! kernel verification for output variables
                        CALL kgen_verify_real_r8_dim2( "work1", check_status, work1, ref_work1)
                        CALL kgen_verify_real_r8_dim2( "work3", check_status, work3, ref_work3)
                        CALL kgen_verify_real_r8_dim2( "work2", check_status, work2, ref_work2)
                        CALL kgen_verify_real_r8_dim2( "co3", check_status, co3, ref_co3)
                        CALL kgen_verify_real_r8_dim2( "hco3", check_status, hco3, ref_hco3)
                        CALL kgen_verify_real_r8_dim2( "h2co3", check_status, h2co3, ref_h2co3)
                        CALL kgen_print_check("comp_co3terms", check_status)
#ifdef _MPI
                        call MPI_Barrier(MPI_COMM_WORLD, iError)
#endif
                        CALL system_clock(start_clock, rate_clock)
                        DO kgen_intvar=1,10
                            CALL comp_co3terms(bid, j, k, land_mask(:, j, bid) .AND. k <= kmt(:, j, bid), .TRUE., &
temp(:, j), salt(:, j), dic_loc(:, j), alk_loc(:, j), po4_loc(:, j), sio3_loc(:, j), work1(:, j), work2(:, j), &
work3(:, j), h2co3(:, j), hco3(:, j), co3(:, j))
                        END DO
                        CALL system_clock(stop_clock, rate_clock)
                        usecs = (stop_clock - start_clock)/REAL(rate_clock*10)
            !-----------------------------------------------------------------------
            !EOC
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

        END SUBROUTINE ecosys_set_interior
        !***********************************************************************
        !BOP
        ! !IROUTINE: init_particulate_terms
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: compute_particulate_terms
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ecosys_init_sflux
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ecosys_init_interior_restore
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ecosys_set_sflux
        ! !INTERFACE:

        !*****************************************************************************
        !BOP
        ! !IROUTINE: SCHMIDT_O2
        ! !INTERFACE:

        !*****************************************************************************
        !BOP
        ! !IROUTINE: O2SAT
        ! !INTERFACE:

        !*****************************************************************************
        !BOP
        ! !IROUTINE: ecosys_tavg_forcing
        ! !INTERFACE:

        !*****************************************************************************
        !BOP
        ! !IROUTINE: ecosys_write_restart
        ! !INTERFACE:

        !*****************************************************************************
        !BOP
        ! !IROUTINE: ecosys_tracer_ref_val
        ! !INTERFACE:

        !***********************************************************************
    END MODULE ecosys_mod
