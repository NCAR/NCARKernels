!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-06 14:32:36 
!KGEN version : 0.8.1 
  
!---------------------------------------------------------------------------
! $Id: array_index.F90 7118 2014-07-25 00:12:15Z raut@uwm.edu $
!===============================================================================


module array_index
  ! Description:
  ! Contains indices to variables in larger arrays.
  ! Note that the 'ii' is necessary because 'i' is used in
  ! statistics to track locations in the zt/zm/sfc derived types.
  ! References:
  !   None
  !-------------------------------------------------------------------------


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 
  ! Variables
  ! Microphysics mixing ratios

    INTEGER, public :: iirrm 
  ! Microphysics concentrations
!$omp threadprivate(iirrm, iirsm, iirim, iirgm)

  ! Scalar quantities
!$omp threadprivate(iiNrm, iiNsm, iiNim, iiNgm)

    INTEGER, public :: iisclr_rt, iisclr_thl 
  ! Logical fields
!$omp threadprivate(iisclr_rt, iisclr_thl, iisclr_CO2, &
!$omp   iiedsclr_rt, iiedsclr_thl, iiedsclr_CO2)

    LOGICAL, dimension(:), allocatable, public :: l_mix_rat_hm 
!$omp threadprivate(l_frozen_hm, l_mix_rat_hm)


!$omp threadprivate( hydromet_list )


!$omp threadprivate( hydromet_tol )   

    PRIVATE 
!===============================================================================
    PUBLIC kr_externs_in_array_index 
      
    CONTAINS 
      


    !read state subroutine for kr_externs_in_array_index 
    SUBROUTINE kr_externs_in_array_index(kgen_unit) 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        READ (UNIT = kgen_unit) iirrm 
        READ (UNIT = kgen_unit) iisclr_thl 
        READ (UNIT = kgen_unit) iisclr_rt 
        CALL kr_array_index_logical___dim1(l_mix_rat_hm, kgen_unit, "l_mix_rat_hm", .FALSE.) 
    END SUBROUTINE kr_externs_in_array_index 
      
    !read state subroutine for kr_array_index_logical___dim1 
    SUBROUTINE kr_array_index_logical___dim1(var, kgen_unit, printname, printvar) 
        LOGICAL, INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
        INTEGER, INTENT(IN) :: kgen_unit 
        CHARACTER(LEN=*), INTENT(IN) :: printname 
        LOGICAL, INTENT(IN), OPTIONAL :: printvar 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
        INTEGER :: idx1 
        INTEGER, DIMENSION(2,1) :: kgen_bound 
          
        READ (UNIT = kgen_unit) kgen_istrue 
        IF (kgen_istrue) THEN 
            IF (ALLOCATED( var )) THEN 
                DEALLOCATE (var) 
            END IF   
            READ (UNIT = kgen_unit) kgen_bound(1, 1) 
            READ (UNIT = kgen_unit) kgen_bound(2, 1) 
            ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
            READ (UNIT = kgen_unit) var 
            IF (PRESENT( printvar ) .AND. printvar) THEN 
                WRITE (*, *) "KGEN DEBUG: " // printname // " = ", var 
            END IF   
        END IF   
    END SUBROUTINE kr_array_index_logical___dim1 
      
end module array_index