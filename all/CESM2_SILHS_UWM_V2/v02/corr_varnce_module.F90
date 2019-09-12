!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:31 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
!$Id$
!-------------------------------------------------------------------------------


module corr_varnce_module


    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 


  ! These slopes and intercepts below are used to calculate the hmp2_ip_on_hmm2_ip_ratios_type
  ! values that are defined above. This functionality is described by equations 8, 10, & 11
  ! in ``Parameterization of the Spatial Variability of Rain for Large-Scale Models and 
  ! Remote Sensing`` Lebo, et al, October 2015
  ! https://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-15-0066.1
  ! see clubb:ticket:830 for more detail
  ! hmp2_ip_on_hmm2_ip(iirr) = hmp2_ip_on_hmm2_ip_intrcpt%rr + &
  !                            hmp2_ip_on_hmm2_ip_slope%rr * max( host_dx, host_dy )
  ! In Lebo et al. the suggested values were
  !     slope = 2.12e-5 [1/m]
  !     intercept = 0.54 [-]
  ! In CLUBB standalone, these parameters can be set based on the value for a
  ! given case in the CASE_model.in file.

  ! 
  ! 
  ! 


  ! Prescribed parameter for <N_cn'^2> / <N_cn>^2.
  ! NOTE: In the case that l_const_Nc_in_cloud is true, Ncn is constant
  !       throughout the entire grid box, so the parameter below should be
  !       ignored.


!$omp threadprivate(Ncnp2_on_Ncnm2)


  integer, public :: &
    pdf_dim
!$omp threadprivate(pdf_dim)


!$omp threadprivate(hmp2_ip_on_hmm2_ip)

!$omp threadprivate(corr_array_n_cloud, corr_array_n_below)

!$omp threadprivate( corr_array_n_cloud_def, corr_array_n_below_def )

  PRIVATE 


  PUBLIC kr_externs_in_corr_varnce_module 


  !-----------------------------------------------------------------------------
    
  CONTAINS 
    


  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------


  !--------------------------------------------------------------------------


  !===============================================================================


  !===============================================================================


!===============================================================================


  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------


  !-----------------------------------------------------------------------------

  !read state subroutine for kr_externs_in_corr_varnce_module 
  SUBROUTINE kr_externs_in_corr_varnce_module(kgen_unit) 
      INTEGER, INTENT(IN) :: kgen_unit 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) pdf_dim 
      print *,'kr_externs_in_corr_varnce_module: pdf_dim: ',pdf_dim
  END SUBROUTINE kr_externs_in_corr_varnce_module 
    
end module corr_varnce_module
