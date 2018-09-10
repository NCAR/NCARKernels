!KGEN-generated Fortran source file 
  
!Generated at : 2018-08-31 16:08:43 
!KGEN version : 0.7.3 
  


module dimensions_mod
    USE constituents, ONLY: ntrac_d=>pcnst 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, CHECK_IN_TOL, &
    &CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
! set MAX number of tracers.  actual number of tracers is a run time argument  

  integer, parameter         :: qsize_d = 6 ! SE tracers (currently SE supports 6 condensate loading tracers)

  ! The variables below hold indices of water vapor and condensate loading tracers as well as
  ! associated heat capacities (initialized in dyn_init):
  !   qsize_condensate_loading_idx     = index of water tracers included in condensate loading according to  physics
  !   qsize_condensate_loading_idx_gll = index of water tracers included in condensate loading terms for SE tracers
  ! Note that when running without CSLAM then
  !   qsize_condensate_loading_idx_gll = qsize_condensate_loading_idx
  ! but when running with CSLAM then SE tracers are only the water tracers included in the condensate loading
  !
  !
  !
  !
  !
  !
  !moist cp in energy conversion term
  ! .false.: force dycore to use cpd (cp dry) instead of moist cp
  ! .true. : use moist cp in dycore
  !
  !
  !
 
  integer, parameter, public :: np = 4
  integer, parameter, public :: nc = 3       !cslam resolution

  integer         :: ntrac = 0 !ntrac is set in dyn_comp
  ! hyperviscosity is applied on approximate pressure levels
  ! Similar to -EUL; see CAM5 scietific documentation (Note TN-486), equation (3.09), page 58.
  !
  ! 
  ! fvm dimensions:
  integer, parameter, public :: ngpc=3          !number of Gausspoints for the fvm integral approximation   !phl change from 4
  integer, parameter, public :: irecons_tracer=6!=1 is PCoM, =3 is PLM, =6 is PPM for tracer reconstruction
  integer, parameter, public :: nhe=1           !Max. Courant number
  integer, parameter, public :: nhr=2           !halo width needed for reconstruction - phl
  integer, parameter, public :: nht=nhe+nhr     !total halo width where reconstruction is needed (nht<=nc) - phl
  integer, parameter, public :: ns=3!quadratic halo interpolation - recommended setting for nc=3
  !nhc determines width of halo exchanged with neighboring elements
  integer, parameter, public :: nhc = nhr+(nhe-1)+(ns-MOD(ns,2))/2
                                                !(different from halo needed for elements on edges and corners


  integer, parameter, public :: kmin_jet=1,kmax_jet=70 !min and max level index for the jet


  integer, parameter, public :: npsq = np*np
  integer, parameter, public :: nlev=70
  integer, parameter, public :: nlevp=nlev+1
!  params for a mesh 
!  integer, public, parameter :: max_elements_attached_to_node = 7
!  integer, public, parameter :: s_nv = 2*max_elements_attached_to_node 
  !default for non-refined mesh (note that these are *not* parameters now)


  PUBLIC qsize_d, ntrac_d, ntrac 


  PUBLIC kr_externs_in_dimensions_mod 
  PUBLIC kr_externs_out_dimensions_mod 
  PUBLIC kv_externs_dimensions_mod 
  INTEGER :: kgenref_ntrac = 0 

    
  CONTAINS 
    


  !read state subroutine for kr_externs_in_dimensions_mod 
  SUBROUTINE kr_externs_in_dimensions_mod(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) ntrac 
  END SUBROUTINE kr_externs_in_dimensions_mod 
    
  !read state subroutine for kr_externs_out_dimensions_mod 
  SUBROUTINE kr_externs_out_dimensions_mod(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
        
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      READ (UNIT = kgen_unit) kgenref_ntrac 
  END SUBROUTINE kr_externs_out_dimensions_mod 
    
  !verify state subroutine for kv_externs_dimensions_mod 
  SUBROUTINE kv_externs_dimensions_mod(check_status) 
      TYPE(check_t), INTENT(INOUT) :: check_status 
        
      CALL kv_dimensions_mod_integer__("ntrac", check_status, ntrac, kgenref_ntrac) 
  END SUBROUTINE kv_externs_dimensions_mod 
    
  !verify state subroutine for kv_dimensions_mod_integer__ 
  RECURSIVE SUBROUTINE kv_dimensions_mod_integer__(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      INTEGER, INTENT(IN) :: var, kgenref_var 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      integer :: diff 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      IF (var == kgenref_var) THEN 
          check_status%numIdentical = check_status%numIdentical + 1 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff = ABS(var - kgenref_var) 
          IF (diff <= kgen_tolerance) THEN 
              check_status%numInTol = check_status%numInTol + 1 
              IF (check_status%verboseLevel > 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              check_status%numOutTol = check_status%numOutTol + 1 
              IF (check_status%verboseLevel > 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
              END IF   
              check_result = CHECK_OUT_TOL 
          END IF   
      END IF   
      IF (check_result == CHECK_IDENTICAL) THEN 
          IF (check_status%verboseLevel > 2) THEN 
              WRITE (*, *) "Difference is ", 0 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_OUT_TOL) THEN 
          IF (check_status%verboseLevel > 0) THEN 
              WRITE (*, *) "Difference is ", diff 
              WRITE (*, *) "" 
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (check_status%verboseLevel > 1) THEN 
              WRITE (*, *) "Difference is ", diff 
              WRITE (*, *) "" 
          END IF   
      END IF   
        
  END SUBROUTINE kv_dimensions_mod_integer__ 
    
end module dimensions_mod
