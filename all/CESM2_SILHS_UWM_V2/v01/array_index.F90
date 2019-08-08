!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:30 
!KGEN version : 0.8.1 
  
!---------------------------------------------------------------------------
! $Id$
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

  ! Microphysics concentrations
!$omp threadprivate(iirr, iirs, iiri, iirg)

  ! Scalar quantities
!$omp threadprivate(iiNr, iiNs, iiNi, iiNg)

  ! Logical fields
!$omp threadprivate(iisclr_rt, iisclr_thl, iisclr_CO2, &
!$omp   iiedsclr_rt, iiedsclr_thl, iiedsclr_CO2)

!$omp threadprivate(l_frozen_hm, l_mix_rat_hm)


!$omp threadprivate( hydromet_list )

  ! Latin hypercube indices / Correlation array indices

!$omp threadprivate( hydromet_tol )   

  integer, public :: &
    iiPDF_chi = -1, &
    iiPDF_eta = -1, &
    iiPDF_w   = -1
!$omp threadprivate(iiPDF_chi, iiPDF_eta, iiPDF_w)

!$omp threadprivate(iiPDF_rr, iiPDF_rs, iiPDF_ri, iiPDF_rg)

  INTEGER, public :: iipdf_ncn = -1 
!$omp threadprivate(iiPDF_Nr, iiPDF_Ns, iiPDF_Ni, iiPDF_Ng, iiPDF_Ncn)

  PRIVATE 
!===============================================================================
  PUBLIC kr_externs_in_array_index 
    
  CONTAINS 
    


  !read state subroutine for kr_externs_in_array_index 
  SUBROUTINE kr_externs_in_array_index(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) iipdf_eta 
      READ (UNIT = kgen_unit) iipdf_w 
      READ (UNIT = kgen_unit) iipdf_chi 
      READ (UNIT = kgen_unit) iipdf_ncn 
  END SUBROUTINE kr_externs_in_array_index 
    
end module array_index