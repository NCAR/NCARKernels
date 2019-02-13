!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: location_mod.f90 12417 2018-03-01 23:34:13Z nancy@ucar.edu $
!> Implements location interfaces for a three dimensional spherical shell 
!> with a choice of vertical coordinates.
!> Horizontal coordinates are always latitude and longitude.
!> Vertical coordinate choices include pressure, height, model level,
!> scale height, surface, and non-specific (column-integrated values, or 
!> with no logically defined vertical location, e.g. hurricane vortex center)
!> The internal representation of the location is stored as
!> radians from 0 to 2 PI for longitude and -PI/2 to PI/2 for latitude to
!> minimize computational cost for distances. However, the external 
!> representation is longitude in degrees from 0 to 360 and latitude 
!> from -90 to 90 for consistency with most applications in the field.
!>
!> This version supports multiple cutoff distances in an efficient manner.
!> Smaller cutoff values will do less searching than larger ones.  (This was
!> not true in earlier implementations of this code.)
!>


!

module location_mod

    USE types_mod, ONLY: r8 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC location_type 
! version controlled file description for error handling, do not edit


! The possible numeric values for the location_type%which_vert component.
! The numeric values are PRIVATE to this module. The parameter names are PUBLIC.


type location_type
   private
   real(r8) :: lon, lat        ! lon, lat are stored in radians
   real(r8) :: vloc            ! units vary based on value of which_vert
   integer  :: which_vert      ! determines if vert is level, height, pressure, ...
end type location_type
! Derived type to facilitate efficient computation of locations close to a given observation.


! Support more than a single cutoff distance.  nt is the count of
! distinct cutoffs, which are selected per specific observation type.
! The map associates the incoming location type with the 
! corresponding gtt index.  There are only as many close_types
! as there are distinct cutoff distances; if more than one specific
! type has the same cutoff distances they share the type.


! Horizontal localization/cutoff values are passed in by the caller.
! The Vertical normalization values are globals; are specified by namelist
! here, and apply to all specific types unless a 'special list' is also specified
! that overrides the default values.


! Some calls include information about the type or kind of the location. 
! If the location refers to an observation it is possible to have both
! a specific type and a generic kind.  If the location refers to a
! state vector item, it only has a generic kind.  Some routines have
! a specific type and a generic kind as arguments; look carefully at
! the argument names before using.


! Global storage for vertical distance normalization factors
! The 4 below is for the 4 vertical units (pressure, level, height,
! scale height).  undefined and surface don't need vert distances.
! NOTE: Code that uses VERT_TYPE_COUNT depends on pressure, level,
! height, and scale height having actual values between 1 and 4, or
! this code will break.

! Global storage for fast approximate sin and cosine lookups
! PAR For efficiency for small cases might want to fill tables as needed
! Also these could be larger for more accurate computations, if needed.
! 630 is 2 * PI rounded up, times 100.

! Tolerance for the top latitude boundary test.  All locations which
! are located on a box boundary are added to the bin on the larger
! side of the boundary (e.g. for north-south latitudes, it rounds
! towards the north).  But when a value falls exactly on the edge
! of the last box, technically it is inside the region but would be
! rounded up and outside the region unless handled specially.
! this tolerance below is used to determine if a value is within
! the range of the last box boundary and if so, the location is
! included in the last box.  In particular, for global grids this
! preserves locations which are at exactly 90.0 degrees latitude.

! Option for verification using exhaustive search

!-----------------------------------------------------------------
! Namelist with default values
! horiz_dist_only == .true.       -> Only the great circle horizontal distance is
!                                    computed in get_dist.
! horiz_dist_only == .false.      -> Square root of sum of squared horizontal and
!                                    normalized vertical dist computed in get_dist
! vert_normalization_pressure     -> Number pascals that give a distance equivalent
!                                    to one radian in horizontal
! vert_normalization_height       -> Number meters that give a distance equivalent 
!                                    to one radian in horizontal
! vert_normalization_level        -> Number levels that give a distance equivalent
!                                    to one radian in horizontal
! vert_normalization_scale_height -> Number scale heights that give a distance 
!                                    equivalent to one radian in horizontal
! approximate_distance            -> Use a faster table lookup for the trig math.
!                                    Works well for global models and large areas,
!                                    and improves performance.  For smaller regions
!                                    might introduce banding, so leave .false.
! nlon                            -> Number longitude boxes for get_close
!                                    nlon MUST BE ODD
! nlat                            -> Number latitude boxes for get_close
! output_box_info                 -> Useful for debugging performance problems.
! print_box_level                 -> How much data to print out.
! special_vert_normalization_obs_types -> Which obs types to modify the default vert
!                                    normalization values
! special_vert_normalization_pressure       -> must give all 4 values for each type listed
! special_vert_normalization_heights
! special_vert_normalization_levels
! special_vert_normalization_scale_heights


!>@todo make these allocatable instead of fixed length


!-----------------------------------------------------------------


PUBLIC kr_location_mod_location_type 
PUBLIC kv_location_mod_location_type 

!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! basic location routines
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! things which need doing exactly once.
  
CONTAINS 
  


!----------------------------------------------------------------------------


!---------------------------------------------------------------------------


!---------------------------------------------------------------------------


!---------------------------------------------------------------------------


!---------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!---------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!--------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!---------------------------------------------------------------------------


!----------------------------------------------------------------------------
!> use a string so caller doesn't have to have access to VERTISxxx values


!--------------------------------------------------------------------


!--------------------------------------------------------------------


!--------------------------------------------------------------------


!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
! get close routines
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!--------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
! end of location/threed_sphere/location_mod.f90
!----------------------------------------------------------------------------


!read state subroutine for kr_location_mod_location_type 
RECURSIVE SUBROUTINE kr_location_mod_location_type(var, kgen_unit, printname, printvar) 
    TYPE(location_type), INTENT(INOUT) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN) :: printname 
    LOGICAL, INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) var%lon 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%lon = ", var%lon 
    END IF   
    READ (UNIT = kgen_unit) var%lat 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%lat = ", var%lat 
    END IF   
      
    READ (UNIT = kgen_unit) var%vloc 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%vloc = ", var%vloc 
    END IF   
      
    READ (UNIT = kgen_unit) var%which_vert 
    IF (PRESENT( printvar ) .AND. printvar) THEN 
        WRITE (*, *) "KGEN DEBUG: " // printname // "%which_vert = ", var%which_vert 
    END IF   
      
END SUBROUTINE kr_location_mod_location_type 
  
!verify state subroutine for kv_location_mod_location_type 
RECURSIVE SUBROUTINE kv_location_mod_location_type(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    TYPE(location_type), INTENT(IN) :: var, kgenref_var 
    TYPE(check_t) :: dtype_check_status, comp_check_status 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    real(KIND=r8) :: diff_lon 
    real(KIND=r8) :: diff_lat 
    real(KIND=r8) :: diff_vloc 
    integer :: diff_which_vert 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%lon == kgenref_var%lon) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%lon is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_lon = ABS(var%lon - kgenref_var%lon) 
        IF (diff_lon <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%lon is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%lon is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_lon 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_lon 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%lat == kgenref_var%lat) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%lat is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_lat = ABS(var%lat - kgenref_var%lat) 
        IF (diff_lat <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%lat is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%lat is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_lat 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_lat 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%vloc == kgenref_var%vloc) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%vloc is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_vloc = ABS(var%vloc - kgenref_var%vloc) 
        IF (diff_vloc <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%vloc is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%vloc is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_vloc 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_vloc 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
    IF (var%which_vert == kgenref_var%which_vert) THEN 
        dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), "%which_vert is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff_which_vert = ABS(var%which_vert - kgenref_var%which_vert) 
        IF (diff_which_vert <= kgen_tolerance) THEN 
            dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%which_vert is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), "%which_vert is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        CONTINUE 
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_which_vert 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank ==0) THEN 
                WRITE (*, *) "Difference is ", diff_which_vert 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
    IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
    ELSE IF (dtype_check_status%numOutTol > 0) THEN 
        check_status%numOutTol = check_status%numOutTol + 1 
    ELSE IF (dtype_check_status%numInTol > 0) THEN 
        check_status%numInTol = check_status%numInTol + 1 
    END IF   
END SUBROUTINE kv_location_mod_location_type 
  
end module location_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/location/threed_sphere/location_mod.f90 $
! $Id: location_mod.f90 12417 2018-03-01 23:34:13Z nancy@ucar.edu $
! $Revision: 12417 $
! $Date: 2018-03-01 16:34:13 -0700 (Thu, 01 Mar 2018) $
