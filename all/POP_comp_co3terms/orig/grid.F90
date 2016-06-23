
! KGEN-generated Fortran source file
!
! Filename    : grid.F90
! Generated at: 2015-06-05 14:52:12
! KGEN version: 0.4.11



    MODULE grid
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: grid
        !
        ! !DESCRIPTION:
        !  This module contains grid info and routines for setting up the
        !  POP grid quantities.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: grid.F90 65485 2014-11-16 22:26:36Z mlevy@ucar.edu $
        ! !USES:
        USE pop_kindsmod, ONLY: pop_i4
        USE blocks, ONLY: nx_block
        USE blocks, ONLY: ny_block
        USE domain_size, ONLY: max_blocks_clinic
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS:
        ! !PUBLIC DATA MEMBERS:
        ! total ocean area of U,T cells
        ! total ocean volume of U,T cells
        ! volume of marginal seas (T cells)
        ! area of marginal seas (T cells)
        ! area of equatorial cell
        ! Default values used if l1Ddyn and lidentical_columns = .true.
        ! value to use for Coriolis (if lconst_Coriolis=.true.)
        ! tau_x in surface forcing
        ! coefficient of SHF in surface forcing
        ! total ocean area (T cells) at each dpth
        ! total ocean volume (T cells) at each dpth
        ! tot marginal seas vol (T cells) at each dpth
        ! choice for type of surface layer
        ! minimum allowed non-zero KMT value
        ! number of topo smoothing passes
        ! variable thickness surface layer
        ! rigid lid surface layer
        ! old free surface form
        ! flag for partial bottom cells
        ! flag to run with spatially-constant Coriolis
        ! flag to run POP in 1D mode (experimental)
        ! flag to specify min for FCOR
        ! flag to treat all columns the same (forcing, depth, etc)
        ! smallest magnitude of Coriolis
        ! (if lmin_Coriolis = .true.)
        ! Observed ocean bathymetry mapped to global T grid
        ! for use in computing KMT internally
        ! k index of deepest grid cell on global T grid
        ! for use in performing work distribution
        !-----------------------------------------------------------------------
        !
        !  grid information for all local blocks
        !  the local blocks are by default in baroclinic distribution
        !
        !-----------------------------------------------------------------------
        !*** dimension(1:km)
        ! thickness of layer k
        ! 2*dz
        ! reciprocals of dz, c2dz
        ! vert dist from sfc to midpoint of layer
        ! vert dist from sfc to bottom of layer
        !*** dimension(0:km)
        ! midpoint of k to midpoint of k+1
        !   and its reciprocal
        !*** geometric 2d arrays
        ! {x,y} spacing centered at U points
        ! {x,y} spacing centered at T points
        ! reciprocals of DXU, DYU
        ! reciprocals of DXT, DYT
        ! cell widths on {N,E} sides of T cell
        ! cell widths on {S,W} sides of U cell
        ! {latitude,longitude} of U points
        ! {latitude,longitude} of T points
        ! angle grid makes with latitude line
        ! coriolis parameter at U,T points
        ! area of U,T cells
        ! reciprocal of area of U,T cells
        ! ocean depth at T,U points
        ! {latitude,longitude} of T points in degrees
        !*** 3d depth fields for partial bottom cells
        ! thickness of U,T cell for pbc
        !*** 2d landmasks
        INTEGER(KIND=pop_i4), dimension(nx_block,ny_block,max_blocks_clinic), public :: kmt
        ! k index of deepest grid cell on T grid
        ! k index of deepest grid cell on U grid
        ! KMT field before smoothing
        ! flag=true if point is an ocean point
        !   at the surface
        ! real equiv of CALCT,U to use as more
        !   efficient multiplicative mask
        ! KMT field at neighbor points
        ! KMU field at neighbor points
        ! KMT field 2 cells away for upwind stencil
        ! allocated and computed in advection module
        ! mask defining regions, marginal seas
        !-----------------------------------------------------------------------
        !
        !     define types used with region masks and marginal seas balancing
        !
        !-----------------------------------------------------------------------
        ! max no. ocean regions
        ! max no. marginal seas
        ! num of ocean U,T points
        ! num of ocean U,T points at surface
        !#################### temporary kludge for overflows ####################
        ! copy of input topography filename
        !########################################################################
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !
        !  module private data
        !
        !-----------------------------------------------------------------------
        !*** geometric scalars
        ! j index of equatorial cell
        ! flag for flat-bottom topography
        ! flag for removing isolated points
        ! {latitude,longitude} of U points
        ! in global-sized array
        !-----------------------------------------------------------------------
        !
        !     area-weighted averaging coefficients
        !     AT{0,S,W,SW} = {central,s,w,sw} coefficients for area-weighted
        !       averaging of four U points surrounding a T point
        !     AU{0,N,E,NE} = {central,n,e,ne} coefficients for area-weighted
        !       averaging of four T points surrounding a U point
        !
        !-----------------------------------------------------------------------
        !-----------------------------------------------------------------------
        !
        !  variables which are shared between init_grid1,init_grid2
        !
        !-----------------------------------------------------------------------
        ! horizontal grid option
        ! vertical grid option
        ! choice for surface layer type
        ! topography (KMT) option
        ! input file for reading horiz grid info
        ! input file for reading horiz grid info
        ! input file for reading horiz grid info
        ! input file for reading horiz grid info
        ! input file for region mask
        ! input file with region identification
        ! input file for thickness of pbc
        ! output file for writing horiz grid info
        !EOC
        !***********************************************************************
            PUBLIC kgen_read_externs_grid
        CONTAINS

        ! write subroutines

        ! module extern variables

        SUBROUTINE kgen_read_externs_grid(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) kmt
        END SUBROUTINE kgen_read_externs_grid

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_grid1
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: init_grid2
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: horiz_grid_internal
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: read_horiz_grid
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: vert_grid_internal
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: compute_dz
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: read_vert_grid
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: topography_bathymetry
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: topography_internal
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: read_topography
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: read_bottom_cell
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: remove_isolated_points
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: remove_points
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: smooth_topography
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: landmasks
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: area_masks
        ! !INTERFACE:

        !***********************************************************************

        !***********************************************************************
        !BOP
        ! !IROUTINE: calc_tpoints
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: fill_points
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: ugrid_to_tgrid
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tgrid_to_ugrid
        ! !INTERFACE:

        !***********************************************************************
    END MODULE grid
