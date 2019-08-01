!KGEN-generated Fortran source file 
  
!Generated at : 2019-08-01 13:32:55 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------------
! $Id$
!===============================================================================


module latin_hypercube_arrays

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 


    PRIVATE 

  integer, allocatable, dimension(:,:), public :: & 
    one_height_time_matrix ! matrix of rand ints
  PUBLIC kr_externs_in_latin_hypercube_arrays 

!$omp threadprivate(one_height_time_matrix)

  !-----------------------------------------------------------------------------
    
  CONTAINS 
    


  !read state subroutine for kr_externs_in_latin_hypercube_arrays 
  SUBROUTINE kr_externs_in_latin_hypercube_arrays(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      CALL kr_latin_hypercube_arrays_integer___dim2(one_height_time_matrix, kgen_unit, "one_height_time_matrix", .FALSE.) 
  END SUBROUTINE kr_externs_in_latin_hypercube_arrays 
    
  !read state subroutine for kr_latin_hypercube_arrays_integer___dim2 
  SUBROUTINE kr_latin_hypercube_arrays_integer___dim2(var, kgen_unit, printname, printvar) 
      INTEGER, INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
      INTEGER :: idx1, idx2 
      INTEGER, DIMENSION(2,2) :: kgen_bound 
        
      READ (UNIT = kgen_unit) kgen_istrue 
      IF (kgen_istrue) THEN 
          IF (ALLOCATED( var )) THEN 
              DEALLOCATE (var) 
          END IF   
          READ (UNIT = kgen_unit) kgen_array_sum 
          READ (UNIT = kgen_unit) kgen_bound(1, 1) 
          READ (UNIT = kgen_unit) kgen_bound(2, 1) 
          READ (UNIT = kgen_unit) kgen_bound(1, 2) 
          READ (UNIT = kgen_unit) kgen_bound(2, 2) 
          ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
          READ (UNIT = kgen_unit) var 
          CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
          IF (PRESENT( printvar ) .AND. printvar) THEN 
              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
          END IF   
      END IF   
  END SUBROUTINE kr_latin_hypercube_arrays_integer___dim2 
    
end module latin_hypercube_arrays