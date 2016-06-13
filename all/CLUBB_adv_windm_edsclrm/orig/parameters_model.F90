!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:54:30
!KGEN version : 0.6.1

!-------------------------------------------------------------------------------
! $Id: parameters_model.F90 7226 2014-08-19 15:52:41Z betlej@uwm.edu $
!===============================================================================
module parameters_model

! Description:
!   Contains model parameters that are determined at run time rather than
!   compile time.
!
! References:
!   None
!-------------------------------------------------------------------------------

    USE clubb_precision, ONLY: core_rknd

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE

    PRIVATE

  ! Maximum magnitude of PDF parameter 'mixt_frac'. 

!$omp threadprivate(mixt_frac_max_mag)

  ! Model parameters and constraints setup in the namelists
    REAL(KIND=core_rknd), public :: ts_nudge = 0._core_rknd








!$omp threadprivate(T0, ts_nudge)

!$omp threadprivate(rtm_min, rtm_nudge_max_altitude)

    INTEGER, public :: edsclr_dim = 0

!$omp threadprivate(sclr_dim, edsclr_dim, hydromet_dim)


!$omp threadprivate(sclr_tol)


!$omp threadprivate(PosInf)


    PUBLIC kr_externs_in_parameters_model

!-------------------------------------------------------------------------------
    
    CONTAINS
    

! Description:
!   Sets parameters to their initial values
!
! References:
!   None
!-------------------------------------------------------------------------------



    ! External

    ! Constants

    ! Input Variables








    ! --- Begin Code --- 
     
    ! Formula from subroutine pdf_closure, where sigma_sqd_w = 0.4 and Skw =
    ! Skw_max_mag in this formula.  Note that this is constant, but can't appear
    ! with a Fortran parameter attribute, so we define it here. 



    ! In a tuning run, this array has the potential to be allocated already








!-------------------------------------------------------------------------------

    !read state subroutine for kr_externs_in_parameters_model
    SUBROUTINE kr_externs_in_parameters_model(kgen_unit)
        INTEGER, INTENT(IN) :: kgen_unit
        LOGICAL :: kgen_istrue
        REAL(KIND=8) :: kgen_array_sum
        
        READ (UNIT = kgen_unit) ts_nudge
        READ (UNIT = kgen_unit) edsclr_dim
    END SUBROUTINE kr_externs_in_parameters_model
    
end module parameters_model