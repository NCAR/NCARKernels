!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  


module coordinate_systems_mod
    USE shr_kind_mod, ONLY: r8=>shr_kind_r8 
! WARNING:  When using this class be sure that you know if the
! cubic coordinates are on the unit cube or the [-\pi/4,\pi/4] cube
! and if the spherical longitude is in [0,2\pi] or [-\pi,\pi]
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 


  type, public :: cartesian2D_t
     real(r8) :: x             ! x coordinate
     real(r8) :: y             ! y coordinate
  end type cartesian2D_t

  type, public :: cartesian3D_t
     real(r8) :: x             ! x coordinate
     real(r8) :: y             ! y coordinate
     real(r8) :: z             ! z coordinate
  end type cartesian3D_t

  type, public :: spherical_polar_t
     real(r8) :: r             ! radius
     real(r8) :: lon           ! longitude
     real(r8) :: lat           ! latitude
  end type spherical_polar_t


  ! ==========================================
  ! Public Interfaces
  ! ==========================================


                                ! should be called cubedsphere2spherical
! CE


  !private :: spherical_to_cart
  PUBLIC kr_coordinate_systems_mod_cartesian2d_t 
  PUBLIC kr_coordinate_systems_mod_cartesian3d_t 
  PUBLIC kr_kgen_coordinate_systems_mod_typesubp0 
  PUBLIC kv_coordinate_systems_mod_cartesian2d_t 
  PUBLIC kv_coordinate_systems_mod_cartesian3d_t 
  PUBLIC kv_kgen_coordinate_systems_mod_typesubp0 

  ! ============================================
  ! copy_cart2d:
  ! Overload assignment operator for cartesian2D_t
  ! ============================================
    
  CONTAINS 
    

  !


  ! ============================================
  ! copy_spherical_polar:
  ! Overload assignment operator for spherical_polar_t
  ! ============================================

  !


  ! ============================================
  ! eq_cart2d:
  ! Overload == operator for cartesian2D_t
  ! ============================================

  !


  ! ===================================================
  ! distance_cart2D  : scalar version
  ! distance_cart2D_v: vector version
  ! computes distance between cartesian 2D coordinates
  ! ===================================================

  !


  ! ===================================================
  ! distance_cart3D  : scalar version
  ! distance_cart3D_v: vector version
  ! ===================================================


  ! ===================================================================
  ! spherical_to_cart:
  ! converts spherical polar {lon,lat}  to 3D cartesian {x,y,z}
  ! on unit sphere.  Note: spherical longitude is [0,2\pi]
  ! ===================================================================


  ! ===================================================================
  ! spherical_to_cart_v:
  ! converts spherical polar {lon,lat}  to 3D cartesian {x,y,z}
  ! on unit sphere.  Note: spherical longitude is [0,2\pi]
  ! ===================================================================


  ! ==========================================================================
  ! cart_to_spherical:
  ! converts 3D cartesian {x,y,z} to spherical polar {lon,lat}
  ! on unit sphere. Note: spherical longitude is [0,2\pi]
  ! ==========================================================================
  ! scalar version

  !


! Note: Output spherical longitude is [-pi,pi]


  ! takes a 2D point on a face of the cube of size [-\pi/4, \pi/4] and projects it
  ! onto a 3D point on a cube of size [-1,1] in R^3


  ! onto a cube of size [-\pi/2,\pi/2] in R^3
  ! the spherical longitude can be either in [0,2\pi] or [-\pi,\pi]


! Go from an arbitrary sized cube in 3D
! to a [-\pi/4,\pi/4] sized cube with (face,2d) coordinates.
!                        Z
!                        |
!                        |
!                        |
!                        |
!                        ---------------Y
!                       /
!                      /
!                     /
!                    /
!                   X
! NOTE: Face 1 =>  X positive constant face of cube
!       Face 2 =>  Y positive constant face of cube
!       Face 3 =>  X negative constant face of cube
!       Face 4 =>  Y negative constant face of cube
!       Face 5 =>  Z negative constant face of cube
!       Face 6 =>  Z positive constant face of cube

!
!


! This function divides three dimentional space up into
! six sectors.  These sectors are then considered as the
! faces of the cube.  It should work for any (x,y,z) coordinate
! if on a sphere or on a cube.


! This could be done directly by using the lon, lat coordinates,
! but call cube_face_number_from_cart just so that there is one place
! to do the conversions and they are all consistant.


! CE, need real (cartesian) xy coordinates on the cubed sphere


! CE END


!INPUT: Points in xy cubed sphere coordinates, counterclockwise
!OUTPUT: corresponding area on the sphere


  !read state subroutine for kr_coordinate_systems_mod_cartesian2d_t 
  RECURSIVE SUBROUTINE kr_coordinate_systems_mod_cartesian2d_t(var, kgen_unit, printname, printvar) 
      TYPE(cartesian2d_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%x 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%x = ", var%x 
      END IF   
        
      READ (UNIT = kgen_unit) var%y 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%y = ", var%y 
      END IF   
        
  END SUBROUTINE kr_coordinate_systems_mod_cartesian2d_t 
    
  !read state subroutine for kr_coordinate_systems_mod_cartesian3d_t 
  RECURSIVE SUBROUTINE kr_coordinate_systems_mod_cartesian3d_t(var, kgen_unit, printname, printvar) 
      TYPE(cartesian3d_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%x 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%x = ", var%x 
      END IF   
        
      READ (UNIT = kgen_unit) var%y 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%y = ", var%y 
      END IF   
        
      READ (UNIT = kgen_unit) var%z 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%z = ", var%z 
      END IF   
        
  END SUBROUTINE kr_coordinate_systems_mod_cartesian3d_t 
    
  !read state subroutine for kr_kgen_coordinate_systems_mod_typesubp0 
  RECURSIVE SUBROUTINE kr_kgen_coordinate_systems_mod_typesubp0(var, kgen_unit, printname, printvar) 
      TYPE(spherical_polar_t), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%r 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%r = ", var%r 
      END IF   
        
      READ (UNIT = kgen_unit) var%lon 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%lon = ", var%lon 
      END IF   
        
      READ (UNIT = kgen_unit) var%lat 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%lat = ", var%lat 
      END IF   
        
  END SUBROUTINE kr_kgen_coordinate_systems_mod_typesubp0 
    
  !verify state subroutine for kv_coordinate_systems_mod_cartesian2d_t 
  RECURSIVE SUBROUTINE kv_coordinate_systems_mod_cartesian2d_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(cartesian2d_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=r8) :: diff_x 
      real(KIND=r8) :: diff_y 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%x == kgenref_var%x) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%x is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_x = ABS(var%x - kgenref_var%x) 
          IF (diff_x <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%x is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%x is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_x 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_x 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%y == kgenref_var%y) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%y is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_y = ABS(var%y - kgenref_var%y) 
          IF (diff_y <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%y is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%y is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_y 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_y 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_coordinate_systems_mod_cartesian2d_t 
    
  !verify state subroutine for kv_coordinate_systems_mod_cartesian3d_t 
  RECURSIVE SUBROUTINE kv_coordinate_systems_mod_cartesian3d_t(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(cartesian3d_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=r8) :: diff_x 
      real(KIND=r8) :: diff_y 
      real(KIND=r8) :: diff_z 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%x == kgenref_var%x) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%x is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_x = ABS(var%x - kgenref_var%x) 
          IF (diff_x <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%x is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%x is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_x 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_x 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%y == kgenref_var%y) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%y is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_y = ABS(var%y - kgenref_var%y) 
          IF (diff_y <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%y is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%y is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_y 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_y 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%z == kgenref_var%z) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%z is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_z = ABS(var%z - kgenref_var%z) 
          IF (diff_z <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%z is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%z is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_z 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_z 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_coordinate_systems_mod_cartesian3d_t 
    
  !verify state subroutine for kv_kgen_coordinate_systems_mod_typesubp0 
  RECURSIVE SUBROUTINE kv_kgen_coordinate_systems_mod_typesubp0(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(spherical_polar_t), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=r8) :: diff_r 
      real(KIND=r8) :: diff_lon 
      real(KIND=r8) :: diff_lat 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%r == kgenref_var%r) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%r is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_r = ABS(var%r - kgenref_var%r) 
          IF (diff_r <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%r is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%r is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_r 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_r 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%lon == kgenref_var%lon) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%lon is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_lon = ABS(var%lon - kgenref_var%lon) 
          IF (diff_lon <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lon is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lon is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_lon 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_lon 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%lat == kgenref_var%lat) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) trim(adjustl(varname)), "%lat is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_lat = ABS(var%lat - kgenref_var%lat) 
          IF (diff_lat <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lat is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%lat is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          CONTINUE 
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_lat 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", diff_lat 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
      IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
      ELSE IF (dtype_check_status%numOutTol > 0) THEN 
          check_status%numOutTol = check_status%numOutTol + 1 
      ELSE IF (dtype_check_status%numInTol > 0) THEN 
          check_status%numInTol = check_status%numInTol + 1 
      END IF   
  END SUBROUTINE kv_kgen_coordinate_systems_mod_typesubp0 
    
end module coordinate_systems_mod
