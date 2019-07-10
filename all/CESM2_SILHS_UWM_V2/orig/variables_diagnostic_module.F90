!KGEN-generated Fortran source file 
  
!Generated at : 2019-07-10 08:35:31 
!KGEN version : 0.8.1 
  
!-------------------------------------------------------------------------
! $Id$
!===============================================================================


module variables_diagnostic_module
! Description:
!   This module contains definitions of all diagnostic
!   arrays used in the single column model, as well as subroutines
!   to allocate, deallocate and initialize them.
!   Note that while these are all same dimension, there is a
!   thermodynamic and momentum grid and they have different levels
!-----------------------------------------------------------------------


    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 

    IMPLICIT NONE 

    PRIVATE 

  ! Diagnostic variables


!!! Important Note !!!
! Do not indent the omp comments, they need to be in the first 4 columns
!!! End Important Note !!!

!$omp threadprivate(sigma_sqd_w_zt, Skw_zm, Skw_zt, Skthl_zm, Skthl_zt, Skrt_zm,  &
!$omp Skrt_zt, ug, vg, um_ref, vm_ref, thlm_ref, rtm_ref, thvm )


!$omp threadprivate(rsat)


!$omp threadprivate(pdf_params_zm, pdf_params_zm_frz)

! Second order moments

!$omp threadprivate(Frad, radht, Frad_SW_up, Frad_SW_down, Frad_LW_up, Frad_LW_down)

! Third order moments

!$omp threadprivate(thlprcp, rtprcp, rcp2)

! Fourth order moments

!$omp threadprivate(wpthlp2, wp2thlp, wprtp2, wp2rtp, &
!$omp   wprtpthlp, wp2rcp, wp3_zm, thlp3, thlp3_zm, rtp3, rtp3_zm )

! Buoyancy related moments

!$omp threadprivate(wp4)


!$omp threadprivate(rtpthvp, thlpthvp, wpthvp, wp2thvp)


!$omp threadprivate(Kh_zt, Kh_zm)

! Mixing lengths

!$omp threadprivate(K_hm)

    REAL(KIND=core_rknd), target, allocatable, dimension(:), public :: lscale 

!$omp threadprivate(Lscale, Lscale_up, Lscale_down)

! hydrometeors variable arrays

!$omp threadprivate(em, tau_zm, tau_zt)

! Cloud droplet concentration arrays
!$omp threadprivate( hydromet, hydrometp2, wphydrometp )

!$omp threadprivate(Ncm,wpNcp)

! Surface data
!$omp threadprivate(Nccnm)


! Passive scalar variables
!$omp threadprivate(ustar, soil_heat_flux)


!$omp threadprivate(wpedsclrp)

! Interpolated variables for tuning

!$omp threadprivate(sclrpthvp, sclrprcp, &
!$omp   wp2sclrp, wpsclrp2, wpsclrprtp, wpsclrpthlp )

!
! Latin Hypercube arrays.  Vince Larson 22 May 2005

!$omp threadprivate(wp2_zt, thlp2_zt, wpthlp_zt, wprtp_zt, &
!$omp   rtp2_zt, rtpthlp_zt, &
!$omp   up2_zt, vp2_zt, upwp_zt, vpwp_zt)


!$omp threadprivate(lh_AKm, AKm, AKstd, AKstd_cld, lh_rcm_avg, AKm_rcm, &
!$omp   AKm_rcc)


!$omp threadprivate(Skw_velocity, a3_coef, a3_coef_zt)

    PUBLIC kr_externs_in_variables_diagnostic_module 

!$omp threadprivate(wp3_on_wp2, wp3_on_wp2_zt)

!-----------------------------------------------------------------------
      
    CONTAINS 
      


!------------------------------------------------------------------------


    !read state subroutine for kr_externs_in_variables_diagnostic_module 
    SUBROUTINE kr_externs_in_variables_diagnostic_module(kgen_unit) 
        INTEGER, INTENT(IN) :: kgen_unit 
        LOGICAL :: kgen_istrue 
        REAL(KIND=8) :: kgen_array_sum 
          
        CALL kr_kgen_variables_diagnostic_module_subp8(lscale, kgen_unit, "lscale", .FALSE.) 
    END SUBROUTINE kr_externs_in_variables_diagnostic_module 
      
    !read state subroutine for kr_kgen_variables_diagnostic_module_subp8 
    SUBROUTINE kr_kgen_variables_diagnostic_module_subp8(var, kgen_unit, printname, printvar) 
        REAL(KIND=core_rknd), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
            READ (UNIT = kgen_unit) kgen_array_sum 
            READ (UNIT = kgen_unit) kgen_bound(1, 1) 
            READ (UNIT = kgen_unit) kgen_bound(2, 1) 
            ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
            READ (UNIT = kgen_unit) var 
            CALL kgen_array_sumcheck(printname, kgen_array_sum, DBLE(SUM(var, mask=(var .eq. var))), .TRUE.) 
            IF (PRESENT( printvar ) .AND. printvar) THEN 
                WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // ")) = ", DBLE(SUM(var, mask=(var .eq. var))) 
            END IF   
        END IF   
    END SUBROUTINE kr_kgen_variables_diagnostic_module_subp8 
      
end module variables_diagnostic_module