
! KGEN-generated Fortran source file
!
! Filename    : tavg.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



    MODULE tavg
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: tavg
        ! !DESCRIPTION:
        !  This module contains data types and routines for computing running
        !  time-averages of selected fields and writing this data to files.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: tavg.F90 56176 2013-12-20 18:35:46Z mlevy@ucar.edu $
        !
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
        USE constants, only : c2
        USE constants, only : c1
        USE constants, only : eps
        USE constants, only : p25
        USE time_management, only : kmt
        USE time_management, only : dz
        USE time_management, only : dzwr
        USE time_management, only : zt
        !*** ccsm
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS:
        !*** ccsm
        ! !PUBLIC DATA MEMBERS:
        ! run started from restart
        ! limit on number of streams; coding limitations restrict this to <= 9
        ! accumulated time (in seconds)
        !*** ccsm
        !-----------------------------------------------------------------------
        !
        !  tavg field descriptor data type and array of such types
        !
        !-----------------------------------------------------------------------
        ! !PUBLIC TYPES:
        !*** ccsm
        !EOP
        !BOC
        ! limit on available fields - can
        !   be pushed as high as necessary practical
        !   (total of all fields in all streams)
        !*** ccsm
        ! current number of defined fields
        ! number of fields requested
        ! number of lines in tavg_contents file
        !-----------------------------------------------------------------------
        !
        !  tavg stream information (support for separate tavg output "streams")
        !
        !-----------------------------------------------------------------------
        ! actual number of tavg output "streams" requested
        ! shorthand name for n_tavg_streams
        ! stream in which qflux is requested
        ! tavg file output wanted
        ! format (nc or bin) for writing
        ! output filename string (eg, h2.nday1)
        ! dimension descriptors for horiz dims
        ! dimension descriptor for vert levels (z_t, z_w_top, or z_w_bot grid)
        ! dimension descriptor for (unlimited) time dim
        !-----------------------------------------------------------------------
        !
        !  buffers for holding running tavg variables
        !
        !-----------------------------------------------------------------------
        ! size of buffer for 2d fields
        ! size of buffer for 3d fields
        ! buffer for holding accumulated sums
        ! buffer for holding accumulated sums
        ! method for each requested 2d field
        ! method for each requested 3d field
        ! work array in write_restart
        !-----------------------------------------------------------------------
        !
        !  variables for writing data
        !
        !-----------------------------------------------------------------------
        !*** field frequency support
        ! choice for frequency of tavg output
        ! choice for starting averaging
        ! format (nc or bin) for reading
        ! frequency option for writing tavg
        ! frequency of tavg output
        ! start after option
        ! start tavg after tavg_start
        !*** FILE frequency (timeseries) support
        ! choice for frequency of tavg FILE output
        ! frequency option for writing tavg FILE
        ! frequency of tavg output FILE creation
        ! filename for choosing fields for output
        ! filename for restart input
        ! root filename for tavg output
        ! root filename for tavg output (original)
        ! IO file descriptor
        !-----------------------------------------------------------------------
        !
        !  scalars
        !
        !-----------------------------------------------------------------------
        ! current time step
        ! date on which the current accumulated sum
        ! was started (not the tavg_start date)
        !-----------------------------------------------------------------------
        !
        !  coupled code -- ccsm-related
        !
        !-----------------------------------------------------------------------
        ! logical for diag_gm_bolus
        ! logical for submesoscale_mixing
        ! true if netCDF output format
        ! true if using new streams tavg_contents;
        ! false if using standard tavg_contents
        ! limit on available fields
        ! current number of defined nonstandard fields
        ! current number of ccsm labels
        ! single/double precision array
        !indices needed for tavg diagnostics
        ! k index of deepest grid cell on global U grid
        ! dimension descriptor for vert (z_t, z_w_top, or z_w_bot grid)
        ! dimension descriptor for vert (z_t grid)
        ! dimension descriptor for near-surf vert (z_t grid)
        ! dimension descriptor for vert (same as z_w_top; keep for backwards compatability)
        ! dimension descriptor for vert (z_w_top grid)
        ! dimension descriptor for vert (z_w_bot grid)
        ! dimension descriptor
        ! dimension descriptor for character arrays
        ! dimension descriptor
        ! dimension descriptor
        ! dimension descriptor
        ! dimension descriptor
        ! dimension descriptor
        ! dimension descriptor
        ! debug level [0,1]  1 ==> messages
        !-----------------------------------------------------------------------
        !
        !     variables for local spatial averaging of some time-averaged fields
        !
        !-----------------------------------------------------------------------
        ! true if ltavg_nino_diags_requested is true,
        ! TEMP is requested and is in the stream
        ! true if moc_requested is true, all necessary
        ! fields are requested and are in the same stream
        ! true if n_heat_trans_requested is true, all necessary
        ! fields are requested and are in the same stream
        ! true if n_salt_trans_requested is true, all necessary
        ! fields are requested and are in the same stream
        ! namelist
        ! number of regions
        ! local- and time-mean value
        ! area of the region
        ! mask for the region, i.e. 0 and 1 mean
        ! outside and inside the region, respectively
        ! name of the region
        !EOC
        !***********************************************************************
        CONTAINS

        ! write subroutines
        ! No subroutines
        ! No module extern variables
        !***********************************************************************
        !EOP
        ! !IROUTINE: init_tavg
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_set_flag
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: write_tavg
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: read_tavg
        ! !INTERFACE:

        !***********************************************************************
        ! !IROUTINE: tavg_global
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_global_sum_2D
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_increment_sum_qflux
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: accumulate_tavg_field
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: accumulate_tavg_now
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_reset_field_all
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_reset_field_stream
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_norm_field_all
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: define_tavg_field
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: request_tavg_field
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_requested
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_in_this_stream(id,stream_number)
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_in_which_stream(id)
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: set_in_tavg_contents
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_id
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_create_suffix
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_create_suffix_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_create_outfile_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_count_contents
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_add_attrib_file_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_add_attrib_io_field_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_get_cell_method_string
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_construct_ccsm_coordinates
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_construct_ccsm_time_invar
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_construct_ccsm_scalars
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_define_labels_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_construct_ccsm_time
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_define_time_bounds
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_write_vars_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_write_vars_nstd_ccsm
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_write_time_bounds
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE:  tavg_mask
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_bsf_diags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_init_moc_diags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_moc_diags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_init_transport_diags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_transport_diags
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_init_local_spatial_avg
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: tavg_local_spatial_avg
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: final_tavg
        ! !INTERFACE:

    END MODULE tavg
