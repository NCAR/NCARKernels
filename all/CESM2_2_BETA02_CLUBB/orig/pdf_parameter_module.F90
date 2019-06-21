!KGEN-generated Fortran source file 
  
!Generated at : 2019-06-20 14:46:36 
!KGEN version : 0.8.1 
  
!-----------------------------------------------------------------------
! $Id$
!===============================================================================


module pdf_parameter_module
  ! Description:
  ! This module defines the derived type pdf_parameter.
  ! References:
  !   None
  !-----------------------------------------------------------------------


    USE clubb_precision, ONLY: core_rknd 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_tolerance, kgen_minvalue, kgen_verboselevel, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 

    PRIVATE 

    PUBLIC pdf_parameter, implicit_coefs_terms, init_pdf_params 
  ! CLUBB's PDF parameters.

  type pdf_parameter

    real( kind = core_rknd ) :: &
      w_1,             & ! Mean of w (1st PDF component)                   [m/s]
      w_2,             & ! Mean of w (2nd PDF component)                   [m/s]
      varnce_w_1,      & ! Variance of w (1st PDF component)           [m^2/s^2]
      varnce_w_2,      & ! Variance of w (2nd PDF component)           [m^2/s^2]
      rt_1,            & ! Mean of r_t (1st PDF component)               [kg/kg]
      rt_2,            & ! Mean of r_t (2nd PDF component)               [kg/kg]
      varnce_rt_1,     & ! Variance of r_t (1st PDF component)       [kg^2/kg^2]
      varnce_rt_2,     & ! Variance of r_t (2nd PDF component)       [kg^2/kg^2]
      thl_1,           & ! Mean of th_l (1st PDF component)                  [K]
      thl_2,           & ! Mean of th_l (2nd PDF component)                  [K]
      varnce_thl_1,    & ! Variance of th_l (1st PDF component)            [K^2]
      varnce_thl_2,    & ! Variance of th_l (2nd PDF component)            [K^2]
      corr_w_rt_1,     & ! Correlation of w and r_t (1st PDF component)      [-]
      corr_w_rt_2,     & ! Correlation of w and r_t (2nd PDF component)      [-]
      corr_w_thl_1,    & ! Correlation of w and th_l (1st PDF component)     [-]
      corr_w_thl_2,    & ! Correlation of w and th_l (2nd PDF component)     [-]
      corr_rt_thl_1,   & ! Correlation of r_t and th_l (1st PDF component)   [-]
      corr_rt_thl_2,   & ! Correlation of r_t and th_l (2nd PDF component)   [-]
      alpha_thl,       & ! Factor relating to normalized variance for th_l   [-]
      alpha_rt,        & ! Factor relating to normalized variance for r_t    [-]
      crt_1,           & ! r_t coef. in chi/eta eqns. (1st PDF comp.)        [-]
      crt_2,           & ! r_t coef. in chi/eta eqns. (2nd PDF comp.)        [-]
      cthl_1,          & ! th_l coef.: chi/eta eqns. (1st PDF comp.) [(kg/kg)/K]
      cthl_2,          & ! th_l coef.: chi/eta eqns. (2nd PDF comp.) [(kg/kg)/K]
      chi_1,           & ! Mean of chi (old s) (1st PDF component)       [kg/kg]
      chi_2,           & ! Mean of chi (old s) (2nd PDF component)       [kg/kg]
      stdev_chi_1,     & ! Standard deviation of chi (1st PDF component) [kg/kg]
      stdev_chi_2,     & ! Standard deviation of chi (2nd PDF component) [kg/kg]
      stdev_eta_1,     & ! Standard dev. of eta (old t) (1st PDF comp.)  [kg/kg]
      stdev_eta_2,     & ! Standard dev. of eta (old t) (2nd PDF comp.)  [kg/kg]
      covar_chi_eta_1, & ! Covariance of chi and eta (1st PDF comp.) [kg^2/kg^2]
      covar_chi_eta_2, & ! Covariance of chi and eta (2nd PDF comp.) [kg^2/kg^2]
      corr_w_chi_1,    & ! Correlation of w and chi (1st PDF component)      [-]
      corr_w_chi_2,    & ! Correlation of w and chi (2nd PDF component)      [-]
      corr_w_eta_1,    & ! Correlation of w and eta (1st PDF component)      [-]
      corr_w_eta_2,    & ! Correlation of w and eta (2nd PDF component)      [-]
      corr_chi_eta_1,  & ! Correlation of chi and eta (1st PDF component)    [-]
      corr_chi_eta_2,  & ! Correlation of chi and eta (2nd PDF component)    [-]
      rsatl_1,         & ! Saturation mixing ratio r_sat(mu_Tl_1,p)      [kg/kg]
      rsatl_2,         & ! Saturation mixing ratio r_sat(mu_Tl_2,p)      [kg/kg]
      rc_1,            & ! Mean of r_c (1st PDF component)               [kg/kg]
      rc_2,            & ! Mean of r_c (2nd PDF component)               [kg/kg]
      cloud_frac_1,    & ! Cloud fraction (1st PDF component)                [-]
      cloud_frac_2,    & ! Cloud fraction (2nd PDF component)                [-]
      mixt_frac          ! Weight of 1st PDF component (Sk_w dependent)      [-]

    real( kind = core_rknd ) :: &
      ice_supersat_frac_1, & ! Ice supersaturation fraction (1st PDF comp.)  [-]
      ice_supersat_frac_2    ! Ice supersaturation fraction (2nd PDF comp.)  [-]

  end type pdf_parameter
  ! The implicit coefficients, semi-implicit coefficients and terms, and
  ! explicit terms for turbulent advection of turbulent fields are calculated
  ! from the PDF and the resulting PDF parameters.

  type implicit_coefs_terms

    real ( kind = core_rknd ) :: &
      coef_wp4_implicit,     & ! <w'^4> = coef_wp4_implicit * <w'^2>^2       [-]
      coef_wprtp2_implicit,  & ! <w'rt'^2> = coef_wprtp2_implicit*<rt'^2>  [m/s]
      coef_wpthlp2_implicit    ! <w'thl'^2>=coef_wpthlp2_implicit*<thl'^2> [m/s]
    ! <w'^2 rt'> = coef_wp2rtp_implicit * <w'rt'> + term_wp2rtp_explicit

    real ( kind = core_rknd ) :: &
      coef_wp2rtp_implicit, & ! Coefficient that is multiplied by <w'rt'>  [m/s]
      term_wp2rtp_explicit    ! Term that is on the RHS          [m^2/s^2 kg/kg]
    ! <w'^2 thl'> = coef_wp2thlp_implicit * <w'thl'> + term_wp2thlp_explicit

    real ( kind = core_rknd ) :: &
      coef_wp2thlp_implicit, & ! Coef. that is multiplied by <w'thl'>      [m/s]
      term_wp2thlp_explicit    ! Term that is on the RHS             [m^2/s^2 K]
    ! <w'rt'thl'> = coef_wprtpthlp_implicit*<rt'thl'> + term_wprtpthlp_explicit

    real ( kind = core_rknd ) :: &
      coef_wprtpthlp_implicit, & ! Coef. that is multiplied by <rt'thl'>   [m/s]
      term_wprtpthlp_explicit    ! Term that is on the RHS         [m/s(kg/kg)K]

  end type implicit_coefs_terms
! The None preprocessor directives are being commented out because this
! code is now also used for WRF-CLUBB.
!#ifdef None 


  PUBLIC pack_pdf_params, unpack_pdf_params 

  integer, public, parameter :: num_pdf_params = 47
!#endif 
  PUBLIC kr_pdf_parameter_module_pdf_parameter 
  PUBLIC kr_kgen_pdf_parameter_module_typesubp0 
  PUBLIC kv_pdf_parameter_module_pdf_parameter 
  PUBLIC kv_kgen_pdf_parameter_module_typesubp0 


  contains
  !=============================================================================

  subroutine init_pdf_params( nz, pdf_params )
    ! Description:
    ! Initializes all PDF parameters in the variable type pdf_parameter.
    ! References:
    !--------------------------------------------------------------------


      USE constants_clubb, ONLY: zero 

    implicit none
    ! Input Variable(s)

    integer, intent(in) :: &
      nz    ! Number of vertical grid levels    [-]
    ! Output Variable(s)

    type(pdf_parameter), dimension(nz), intent(out) :: &
      pdf_params    ! PDF parameters            [units vary]

    integer :: k
    ! Initialize all PDF parameters in variable type pdf_parameter.


    do k = 1, nz

        pdf_params(k)%w_1 = zero
        pdf_params(k)%w_2 = zero
        pdf_params(k)%varnce_w_1 = zero
        pdf_params(k)%varnce_w_2 = zero
        pdf_params(k)%rt_1 = zero
        pdf_params(k)%rt_2 = zero
        pdf_params(k)%varnce_rt_1 = zero
        pdf_params(k)%varnce_rt_2 = zero
        pdf_params(k)%thl_1 = zero
        pdf_params(k)%thl_2 = zero
        pdf_params(k)%varnce_thl_1 = zero
        pdf_params(k)%varnce_thl_2 = zero
        pdf_params(k)%corr_w_rt_1 = zero
        pdf_params(k)%corr_w_rt_2 = zero
        pdf_params(k)%corr_w_thl_1 = zero
        pdf_params(k)%corr_w_thl_2 = zero
        pdf_params(k)%corr_rt_thl_1 = zero
        pdf_params(k)%corr_rt_thl_2 = zero
        pdf_params(k)%alpha_thl = zero
        pdf_params(k)%alpha_rt = zero
        pdf_params(k)%crt_1 = zero
        pdf_params(k)%crt_2 = zero
        pdf_params(k)%cthl_1 = zero
        pdf_params(k)%cthl_2 = zero
        pdf_params(k)%chi_1 = zero
        pdf_params(k)%chi_2 = zero
        pdf_params(k)%stdev_chi_1 = zero
        pdf_params(k)%stdev_chi_2 = zero
        pdf_params(k)%stdev_eta_1 = zero
        pdf_params(k)%stdev_eta_2 = zero
        pdf_params(k)%covar_chi_eta_1 = zero
        pdf_params(k)%covar_chi_eta_2 = zero
        pdf_params(k)%corr_w_chi_1 = zero 
        pdf_params(k)%corr_w_chi_2 = zero 
        pdf_params(k)%corr_w_eta_1 = zero 
        pdf_params(k)%corr_w_eta_2 = zero 
        pdf_params(k)%corr_chi_eta_1 = zero 
        pdf_params(k)%corr_chi_eta_2 = zero 
        pdf_params(k)%rsatl_1 = zero
        pdf_params(k)%rsatl_2 = zero
        pdf_params(k)%rc_1 = zero
        pdf_params(k)%rc_2 = zero
        pdf_params(k)%cloud_frac_1 = zero
        pdf_params(k)%cloud_frac_2 = zero
        pdf_params(k)%mixt_frac = zero
        pdf_params(k)%ice_supersat_frac_1 = zero
        pdf_params(k)%ice_supersat_frac_2 = zero

    end do


    return

  end subroutine init_pdf_params
  !=============================================================================
! The None preprocessor directives are being commented out because this
! code is now also used for WRF-CLUBB.
!#ifdef None 


  subroutine pack_pdf_params(pdf_params, nz, r_param_array)
    implicit none
    ! Input a pdf_parameter array with nz instances of pdf_parameter
    integer, intent(in) :: nz ! Num Vert Model Levs
    type (pdf_parameter), dimension(nz), intent(in) :: pdf_params
    ! Output a two dimensional real array with all values

    real (kind = core_rknd), dimension(nz,num_pdf_params), intent(out) :: &
       r_param_array  
    ! Local Loop vars

    integer :: k, p    

    do k = 1,nz
       do p = 1,num_pdf_params
 
          r_param_array(k,p) = get_param_at_ind(pdf_params(k), p)
       
       end do ! p
    end do ! k

  end subroutine pack_pdf_params

  subroutine unpack_pdf_params(r_param_array, nz, pdf_params)
    implicit none
    ! Input a two dimensional real array with pdf values
    integer, intent(in) :: nz ! Num Vert Model Levs
    real (kind = core_rknd), dimension(nz,num_pdf_params), intent(in) :: &
       r_param_array 
    ! Output a pdf_parameter array with nz instances of pdf_parameter

    type (pdf_parameter), dimension(nz), intent(out) :: pdf_params
    ! Local Loop vars

    integer :: k, p
    ! temp var
    real (kind = core_rknd) :: value
    
    do k = 1,nz
       do p = 1,num_pdf_params

          value = r_param_array(k,p)
          call set_param_at_ind(pdf_params(k), p, value)       

       end do ! p
    end do ! k

  end subroutine unpack_pdf_params

  real( kind = core_rknd ) function get_param_at_ind(pp_struct, ind)

      USE constants_clubb, ONLY: fstderr 

      USE error_code, ONLY: err_code, clubb_fatal_error 

    implicit none
    type (pdf_parameter), intent(in) :: pp_struct
    integer, intent(in) :: ind

    SELECT CASE (ind)
      CASE (1)
          get_param_at_ind = pp_struct%w_1
      CASE (2)
          get_param_at_ind = pp_struct%w_2
      CASE (3)
          get_param_at_ind = pp_struct%varnce_w_1
      CASE (4)
          get_param_at_ind = pp_struct%varnce_w_2
      CASE (5)
          get_param_at_ind = pp_struct%rt_1
      CASE (6)
          get_param_at_ind = pp_struct%rt_2
      CASE (7)
          get_param_at_ind = pp_struct%varnce_rt_1
      CASE (8)
          get_param_at_ind = pp_struct%varnce_rt_2
      CASE (9)
          get_param_at_ind = pp_struct%thl_1
      CASE (10)
          get_param_at_ind = pp_struct%thl_2
      CASE (11)
          get_param_at_ind = pp_struct%varnce_thl_1
      CASE (12)
          get_param_at_ind = pp_struct%varnce_thl_2
      CASE (13)
          get_param_at_ind = pp_struct%corr_w_rt_1
      CASE (14)
          get_param_at_ind = pp_struct%corr_w_rt_2
      CASE (15)
          get_param_at_ind = pp_struct%corr_w_thl_1
      CASE (16)
          get_param_at_ind = pp_struct%corr_w_thl_2
      CASE (17)
          get_param_at_ind = pp_struct%corr_rt_thl_1
      CASE (18)
          get_param_at_ind = pp_struct%corr_rt_thl_2
      CASE (19)
          get_param_at_ind = pp_struct%alpha_thl
      CASE (20)
          get_param_at_ind = pp_struct%alpha_rt
      CASE (21)
          get_param_at_ind = pp_struct%crt_1
      CASE (22)
          get_param_at_ind = pp_struct%crt_2
      CASE (23)
          get_param_at_ind = pp_struct%cthl_1
      CASE (24)
          get_param_at_ind = pp_struct%cthl_2
      CASE (25)
          get_param_at_ind = pp_struct%chi_1
      CASE (26)
          get_param_at_ind = pp_struct%chi_2
      CASE (27)
          get_param_at_ind = pp_struct%stdev_chi_1
      CASE (28)
          get_param_at_ind = pp_struct%stdev_chi_2
      CASE (29)
          get_param_at_ind = pp_struct%stdev_eta_1
      CASE (30)
          get_param_at_ind = pp_struct%stdev_eta_2
      CASE (31)
          get_param_at_ind = pp_struct%covar_chi_eta_1
      CASE (32)
          get_param_at_ind = pp_struct%covar_chi_eta_2
      CASE (33)
          get_param_at_ind = pp_struct%corr_w_chi_1
      CASE (34)
          get_param_at_ind = pp_struct%corr_w_chi_2
      CASE (35)
          get_param_at_ind = pp_struct%corr_w_eta_1
      CASE (36)
          get_param_at_ind = pp_struct%corr_w_eta_2
      CASE (37)
          get_param_at_ind = pp_struct%corr_chi_eta_1
      CASE (38)
          get_param_at_ind = pp_struct%corr_chi_eta_2
      CASE (39)
          get_param_at_ind = pp_struct%rsatl_1
      CASE (40)
          get_param_at_ind = pp_struct%rsatl_2
      CASE (41)
          get_param_at_ind = pp_struct%rc_1
      CASE (42)
          get_param_at_ind = pp_struct%rc_2
      CASE (43)
          get_param_at_ind = pp_struct%cloud_frac_1
      CASE (44)
          get_param_at_ind = pp_struct%cloud_frac_2
      CASE (45)
          get_param_at_ind = pp_struct%mixt_frac
      CASE (46)
          get_param_at_ind = pp_struct%ice_supersat_frac_1
      CASE (47)
          get_param_at_ind = pp_struct%ice_supersat_frac_2
      CASE DEFAULT
          write(fstderr,*) "Invalid index in get_param_at_ind"
          err_code = clubb_fatal_error
          return
    END SELECT

    RETURN
  end function get_param_at_ind

  subroutine set_param_at_ind(pp_struct, ind, val)
    implicit none
    type (pdf_parameter), intent(inout) :: pp_struct
    integer, intent(in) :: ind
    real (kind = core_rknd), intent(in) :: val

    SELECT CASE (ind)
      CASE (1)
          pp_struct%w_1 = val
      CASE (2)
          pp_struct%w_2 = val
      CASE (3)
          pp_struct%varnce_w_1 = val
      CASE (4)
          pp_struct%varnce_w_2 = val
      CASE (5)
          pp_struct%rt_1 = val
      CASE (6)
          pp_struct%rt_2 = val
      CASE (7)
          pp_struct%varnce_rt_1 = val
      CASE (8)
          pp_struct%varnce_rt_2 = val
      CASE (9)
          pp_struct%thl_1 = val
      CASE (10)
          pp_struct%thl_2 = val
      CASE (11)
          pp_struct%varnce_thl_1 = val
      CASE (12)
          pp_struct%varnce_thl_2 = val
      CASE (13)
          pp_struct%corr_w_rt_1 = val
      CASE (14)
          pp_struct%corr_w_rt_2 = val
      CASE (15)
          pp_struct%corr_w_thl_1 = val
      CASE (16)
          pp_struct%corr_w_thl_2 = val
      CASE (17)
          pp_struct%corr_rt_thl_1 = val
      CASE (18)
          pp_struct%corr_rt_thl_2 = val
      CASE (19)
          pp_struct%alpha_thl = val
      CASE (20)
          pp_struct%alpha_rt = val
      CASE (21)
          pp_struct%crt_1 = val
      CASE (22)
          pp_struct%crt_2 = val
      CASE (23)
          pp_struct%cthl_1 = val
      CASE (24)
          pp_struct%cthl_2 = val
      CASE (25)
          pp_struct%chi_1 = val
      CASE (26)
          pp_struct%chi_2 = val
      CASE (27)
          pp_struct%stdev_chi_1 = val
      CASE (28)
          pp_struct%stdev_chi_2 = val
      CASE (29)
          pp_struct%stdev_eta_1 = val
      CASE (30)
          pp_struct%stdev_eta_2 = val
      CASE (31)
          pp_struct%covar_chi_eta_1 = val
      CASE (32)
          pp_struct%covar_chi_eta_2 = val
      CASE (33)
          pp_struct%corr_w_chi_1 = val
      CASE (34)
          pp_struct%corr_w_chi_2 = val
      CASE (35)
          pp_struct%corr_w_eta_1 = val
      CASE (36)
          pp_struct%corr_w_eta_2 = val
      CASE (37)
          pp_struct%corr_chi_eta_1 = val
      CASE (38)
          pp_struct%corr_chi_eta_2 = val
      CASE (39)
          pp_struct%rsatl_1 = val
      CASE (40)
          pp_struct%rsatl_2 = val
      CASE (41)
          pp_struct%rc_1 = val
      CASE (42)
          pp_struct%rc_2 = val
      CASE (43)
          pp_struct%cloud_frac_1 = val
      CASE (44)
          pp_struct%cloud_frac_2 = val
      CASE (45)
          pp_struct%mixt_frac = val
      CASE (46)
          pp_struct%ice_supersat_frac_1 = val
      CASE (47)
          pp_struct%ice_supersat_frac_2 = val
      CASE DEFAULT
          ! do nothing !
    END SELECT

  end subroutine set_param_at_ind
!#endif 


  !read state subroutine for kr_pdf_parameter_module_pdf_parameter 
  RECURSIVE SUBROUTINE kr_pdf_parameter_module_pdf_parameter(var, kgen_unit, printname, printvar) 
      TYPE(pdf_parameter), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%w_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%w_1 = ", var%w_1 
      END IF   
      READ (UNIT = kgen_unit) var%w_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%w_2 = ", var%w_2 
      END IF   
      READ (UNIT = kgen_unit) var%varnce_w_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%varnce_w_1 = ", var%varnce_w_1 
      END IF   
      READ (UNIT = kgen_unit) var%varnce_w_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%varnce_w_2 = ", var%varnce_w_2 
      END IF   
      READ (UNIT = kgen_unit) var%rt_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%rt_1 = ", var%rt_1 
      END IF   
      READ (UNIT = kgen_unit) var%rt_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%rt_2 = ", var%rt_2 
      END IF   
      READ (UNIT = kgen_unit) var%varnce_rt_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%varnce_rt_1 = ", var%varnce_rt_1 
      END IF   
      READ (UNIT = kgen_unit) var%varnce_rt_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%varnce_rt_2 = ", var%varnce_rt_2 
      END IF   
      READ (UNIT = kgen_unit) var%thl_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%thl_1 = ", var%thl_1 
      END IF   
      READ (UNIT = kgen_unit) var%thl_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%thl_2 = ", var%thl_2 
      END IF   
      READ (UNIT = kgen_unit) var%varnce_thl_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%varnce_thl_1 = ", var%varnce_thl_1 
      END IF   
      READ (UNIT = kgen_unit) var%varnce_thl_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%varnce_thl_2 = ", var%varnce_thl_2 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_rt_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_rt_1 = ", var%corr_w_rt_1 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_rt_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_rt_2 = ", var%corr_w_rt_2 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_thl_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_thl_1 = ", var%corr_w_thl_1 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_thl_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_thl_2 = ", var%corr_w_thl_2 
      END IF   
      READ (UNIT = kgen_unit) var%corr_rt_thl_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_rt_thl_1 = ", var%corr_rt_thl_1 
      END IF   
      READ (UNIT = kgen_unit) var%corr_rt_thl_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_rt_thl_2 = ", var%corr_rt_thl_2 
      END IF   
      READ (UNIT = kgen_unit) var%alpha_thl 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%alpha_thl = ", var%alpha_thl 
      END IF   
      READ (UNIT = kgen_unit) var%alpha_rt 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%alpha_rt = ", var%alpha_rt 
      END IF   
      READ (UNIT = kgen_unit) var%crt_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%crt_1 = ", var%crt_1 
      END IF   
      READ (UNIT = kgen_unit) var%crt_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%crt_2 = ", var%crt_2 
      END IF   
      READ (UNIT = kgen_unit) var%cthl_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cthl_1 = ", var%cthl_1 
      END IF   
      READ (UNIT = kgen_unit) var%cthl_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cthl_2 = ", var%cthl_2 
      END IF   
      READ (UNIT = kgen_unit) var%chi_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%chi_1 = ", var%chi_1 
      END IF   
      READ (UNIT = kgen_unit) var%chi_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%chi_2 = ", var%chi_2 
      END IF   
      READ (UNIT = kgen_unit) var%stdev_chi_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%stdev_chi_1 = ", var%stdev_chi_1 
      END IF   
      READ (UNIT = kgen_unit) var%stdev_chi_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%stdev_chi_2 = ", var%stdev_chi_2 
      END IF   
      READ (UNIT = kgen_unit) var%stdev_eta_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%stdev_eta_1 = ", var%stdev_eta_1 
      END IF   
      READ (UNIT = kgen_unit) var%stdev_eta_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%stdev_eta_2 = ", var%stdev_eta_2 
      END IF   
      READ (UNIT = kgen_unit) var%covar_chi_eta_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%covar_chi_eta_1 = ", var%covar_chi_eta_1 
      END IF   
      READ (UNIT = kgen_unit) var%covar_chi_eta_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%covar_chi_eta_2 = ", var%covar_chi_eta_2 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_chi_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_chi_1 = ", var%corr_w_chi_1 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_chi_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_chi_2 = ", var%corr_w_chi_2 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_eta_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_eta_1 = ", var%corr_w_eta_1 
      END IF   
      READ (UNIT = kgen_unit) var%corr_w_eta_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_w_eta_2 = ", var%corr_w_eta_2 
      END IF   
      READ (UNIT = kgen_unit) var%corr_chi_eta_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_chi_eta_1 = ", var%corr_chi_eta_1 
      END IF   
      READ (UNIT = kgen_unit) var%corr_chi_eta_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%corr_chi_eta_2 = ", var%corr_chi_eta_2 
      END IF   
      READ (UNIT = kgen_unit) var%rsatl_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%rsatl_1 = ", var%rsatl_1 
      END IF   
      READ (UNIT = kgen_unit) var%rsatl_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%rsatl_2 = ", var%rsatl_2 
      END IF   
      READ (UNIT = kgen_unit) var%rc_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%rc_1 = ", var%rc_1 
      END IF   
      READ (UNIT = kgen_unit) var%rc_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%rc_2 = ", var%rc_2 
      END IF   
      READ (UNIT = kgen_unit) var%cloud_frac_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cloud_frac_1 = ", var%cloud_frac_1 
      END IF   
      READ (UNIT = kgen_unit) var%cloud_frac_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%cloud_frac_2 = ", var%cloud_frac_2 
      END IF   
      READ (UNIT = kgen_unit) var%mixt_frac 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%mixt_frac = ", var%mixt_frac 
      END IF   
        
      READ (UNIT = kgen_unit) var%ice_supersat_frac_1 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ice_supersat_frac_1 = ", var%ice_supersat_frac_1 
      END IF   
      READ (UNIT = kgen_unit) var%ice_supersat_frac_2 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%ice_supersat_frac_2 = ", var%ice_supersat_frac_2 
      END IF   
        
  END SUBROUTINE kr_pdf_parameter_module_pdf_parameter 
    
  !read state subroutine for kr_kgen_pdf_parameter_module_typesubp0 
  RECURSIVE SUBROUTINE kr_kgen_pdf_parameter_module_typesubp0(var, kgen_unit, printname, printvar) 
      TYPE(implicit_coefs_terms), INTENT(INOUT) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN) :: printname 
      LOGICAL, INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      READ (UNIT = kgen_unit) var%coef_wp4_implicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%coef_wp4_implicit = ", var%coef_wp4_implicit 
      END IF   
      READ (UNIT = kgen_unit) var%coef_wprtp2_implicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%coef_wprtp2_implicit = ", var%coef_wprtp2_implicit 
      END IF   
      READ (UNIT = kgen_unit) var%coef_wpthlp2_implicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%coef_wpthlp2_implicit = ", var%coef_wpthlp2_implicit 
      END IF   
        
      READ (UNIT = kgen_unit) var%coef_wp2rtp_implicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%coef_wp2rtp_implicit = ", var%coef_wp2rtp_implicit 
      END IF   
      READ (UNIT = kgen_unit) var%term_wp2rtp_explicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%term_wp2rtp_explicit = ", var%term_wp2rtp_explicit 
      END IF   
        
      READ (UNIT = kgen_unit) var%coef_wp2thlp_implicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%coef_wp2thlp_implicit = ", var%coef_wp2thlp_implicit 
      END IF   
      READ (UNIT = kgen_unit) var%term_wp2thlp_explicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%term_wp2thlp_explicit = ", var%term_wp2thlp_explicit 
      END IF   
        
      READ (UNIT = kgen_unit) var%coef_wprtpthlp_implicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%coef_wprtpthlp_implicit = ", var%coef_wprtpthlp_implicit 
      END IF   
      READ (UNIT = kgen_unit) var%term_wprtpthlp_explicit 
      IF (PRESENT( printvar ) .AND. printvar) THEN 
          WRITE (*, *) "KGEN DEBUG: " // printname // "%term_wprtpthlp_explicit = ", var%term_wprtpthlp_explicit 
      END IF   
        
  END SUBROUTINE kr_kgen_pdf_parameter_module_typesubp0 
    
  !verify state subroutine for kv_pdf_parameter_module_pdf_parameter 
  RECURSIVE SUBROUTINE kv_pdf_parameter_module_pdf_parameter(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(pdf_parameter), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=core_rknd) :: diff_w_1 
      real(KIND=core_rknd) :: diff_w_2 
      real(KIND=core_rknd) :: diff_varnce_w_1 
      real(KIND=core_rknd) :: diff_varnce_w_2 
      real(KIND=core_rknd) :: diff_rt_1 
      real(KIND=core_rknd) :: diff_rt_2 
      real(KIND=core_rknd) :: diff_varnce_rt_1 
      real(KIND=core_rknd) :: diff_varnce_rt_2 
      real(KIND=core_rknd) :: diff_thl_1 
      real(KIND=core_rknd) :: diff_thl_2 
      real(KIND=core_rknd) :: diff_varnce_thl_1 
      real(KIND=core_rknd) :: diff_varnce_thl_2 
      real(KIND=core_rknd) :: diff_corr_w_rt_1 
      real(KIND=core_rknd) :: diff_corr_w_rt_2 
      real(KIND=core_rknd) :: diff_corr_w_thl_1 
      real(KIND=core_rknd) :: diff_corr_w_thl_2 
      real(KIND=core_rknd) :: diff_corr_rt_thl_1 
      real(KIND=core_rknd) :: diff_corr_rt_thl_2 
      real(KIND=core_rknd) :: diff_alpha_thl 
      real(KIND=core_rknd) :: diff_alpha_rt 
      real(KIND=core_rknd) :: diff_crt_1 
      real(KIND=core_rknd) :: diff_crt_2 
      real(KIND=core_rknd) :: diff_cthl_1 
      real(KIND=core_rknd) :: diff_cthl_2 
      real(KIND=core_rknd) :: diff_chi_1 
      real(KIND=core_rknd) :: diff_chi_2 
      real(KIND=core_rknd) :: diff_stdev_chi_1 
      real(KIND=core_rknd) :: diff_stdev_chi_2 
      real(KIND=core_rknd) :: diff_stdev_eta_1 
      real(KIND=core_rknd) :: diff_stdev_eta_2 
      real(KIND=core_rknd) :: diff_covar_chi_eta_1 
      real(KIND=core_rknd) :: diff_covar_chi_eta_2 
      real(KIND=core_rknd) :: diff_corr_w_chi_1 
      real(KIND=core_rknd) :: diff_corr_w_chi_2 
      real(KIND=core_rknd) :: diff_corr_w_eta_1 
      real(KIND=core_rknd) :: diff_corr_w_eta_2 
      real(KIND=core_rknd) :: diff_corr_chi_eta_1 
      real(KIND=core_rknd) :: diff_corr_chi_eta_2 
      real(KIND=core_rknd) :: diff_rsatl_1 
      real(KIND=core_rknd) :: diff_rsatl_2 
      real(KIND=core_rknd) :: diff_rc_1 
      real(KIND=core_rknd) :: diff_rc_2 
      real(KIND=core_rknd) :: diff_cloud_frac_1 
      real(KIND=core_rknd) :: diff_cloud_frac_2 
      real(KIND=core_rknd) :: diff_mixt_frac 
      real(KIND=core_rknd) :: diff_ice_supersat_frac_1 
      real(KIND=core_rknd) :: diff_ice_supersat_frac_2 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%w_1 == kgenref_var%w_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%w_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_w_1 = ABS(var%w_1 - kgenref_var%w_1) 
          IF (diff_w_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%w_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%w_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_w_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_w_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%w_2 == kgenref_var%w_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%w_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_w_2 = ABS(var%w_2 - kgenref_var%w_2) 
          IF (diff_w_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%w_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%w_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_w_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_w_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%varnce_w_1 == kgenref_var%varnce_w_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_w_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_varnce_w_1 = ABS(var%varnce_w_1 - kgenref_var%varnce_w_1) 
          IF (diff_varnce_w_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_w_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_w_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_varnce_w_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_varnce_w_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%varnce_w_2 == kgenref_var%varnce_w_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_w_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_varnce_w_2 = ABS(var%varnce_w_2 - kgenref_var%varnce_w_2) 
          IF (diff_varnce_w_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_w_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_w_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_varnce_w_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_varnce_w_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%rt_1 == kgenref_var%rt_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rt_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_rt_1 = ABS(var%rt_1 - kgenref_var%rt_1) 
          IF (diff_rt_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rt_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rt_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_rt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_rt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%rt_2 == kgenref_var%rt_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rt_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_rt_2 = ABS(var%rt_2 - kgenref_var%rt_2) 
          IF (diff_rt_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rt_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rt_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_rt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_rt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%varnce_rt_1 == kgenref_var%varnce_rt_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_varnce_rt_1 = ABS(var%varnce_rt_1 - kgenref_var%varnce_rt_1) 
          IF (diff_varnce_rt_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_varnce_rt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_varnce_rt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%varnce_rt_2 == kgenref_var%varnce_rt_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_varnce_rt_2 = ABS(var%varnce_rt_2 - kgenref_var%varnce_rt_2) 
          IF (diff_varnce_rt_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_rt_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_varnce_rt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_varnce_rt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%thl_1 == kgenref_var%thl_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%thl_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_thl_1 = ABS(var%thl_1 - kgenref_var%thl_1) 
          IF (diff_thl_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%thl_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%thl_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%thl_2 == kgenref_var%thl_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%thl_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_thl_2 = ABS(var%thl_2 - kgenref_var%thl_2) 
          IF (diff_thl_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%thl_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%thl_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%varnce_thl_1 == kgenref_var%varnce_thl_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_varnce_thl_1 = ABS(var%varnce_thl_1 - kgenref_var%varnce_thl_1) 
          IF (diff_varnce_thl_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_varnce_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_varnce_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%varnce_thl_2 == kgenref_var%varnce_thl_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_varnce_thl_2 = ABS(var%varnce_thl_2 - kgenref_var%varnce_thl_2) 
          IF (diff_varnce_thl_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%varnce_thl_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_varnce_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_varnce_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_rt_1 == kgenref_var%corr_w_rt_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_rt_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_rt_1 = ABS(var%corr_w_rt_1 - kgenref_var%corr_w_rt_1) 
          IF (diff_corr_w_rt_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_rt_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_rt_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_rt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_rt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_rt_2 == kgenref_var%corr_w_rt_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_rt_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_rt_2 = ABS(var%corr_w_rt_2 - kgenref_var%corr_w_rt_2) 
          IF (diff_corr_w_rt_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_rt_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_rt_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_rt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_rt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_thl_1 == kgenref_var%corr_w_thl_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_thl_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_thl_1 = ABS(var%corr_w_thl_1 - kgenref_var%corr_w_thl_1) 
          IF (diff_corr_w_thl_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_thl_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_thl_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_thl_2 == kgenref_var%corr_w_thl_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_thl_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_thl_2 = ABS(var%corr_w_thl_2 - kgenref_var%corr_w_thl_2) 
          IF (diff_corr_w_thl_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_thl_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_thl_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_rt_thl_1 == kgenref_var%corr_rt_thl_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_rt_thl_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_rt_thl_1 = ABS(var%corr_rt_thl_1 - kgenref_var%corr_rt_thl_1) 
          IF (diff_corr_rt_thl_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_rt_thl_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_rt_thl_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_rt_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_rt_thl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_rt_thl_2 == kgenref_var%corr_rt_thl_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_rt_thl_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_rt_thl_2 = ABS(var%corr_rt_thl_2 - kgenref_var%corr_rt_thl_2) 
          IF (diff_corr_rt_thl_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_rt_thl_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_rt_thl_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_rt_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_rt_thl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%alpha_thl == kgenref_var%alpha_thl) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%alpha_thl is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_alpha_thl = ABS(var%alpha_thl - kgenref_var%alpha_thl) 
          IF (diff_alpha_thl <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alpha_thl is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alpha_thl is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_alpha_thl 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_alpha_thl 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%alpha_rt == kgenref_var%alpha_rt) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%alpha_rt is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_alpha_rt = ABS(var%alpha_rt - kgenref_var%alpha_rt) 
          IF (diff_alpha_rt <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alpha_rt is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%alpha_rt is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_alpha_rt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_alpha_rt 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%crt_1 == kgenref_var%crt_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%crt_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_crt_1 = ABS(var%crt_1 - kgenref_var%crt_1) 
          IF (diff_crt_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%crt_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%crt_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_crt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_crt_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%crt_2 == kgenref_var%crt_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%crt_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_crt_2 = ABS(var%crt_2 - kgenref_var%crt_2) 
          IF (diff_crt_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%crt_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%crt_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_crt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_crt_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cthl_1 == kgenref_var%cthl_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cthl_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cthl_1 = ABS(var%cthl_1 - kgenref_var%cthl_1) 
          IF (diff_cthl_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cthl_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cthl_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_cthl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cthl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cthl_2 == kgenref_var%cthl_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cthl_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cthl_2 = ABS(var%cthl_2 - kgenref_var%cthl_2) 
          IF (diff_cthl_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cthl_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cthl_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_cthl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cthl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%chi_1 == kgenref_var%chi_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%chi_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_chi_1 = ABS(var%chi_1 - kgenref_var%chi_1) 
          IF (diff_chi_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%chi_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%chi_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_chi_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_chi_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%chi_2 == kgenref_var%chi_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%chi_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_chi_2 = ABS(var%chi_2 - kgenref_var%chi_2) 
          IF (diff_chi_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%chi_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%chi_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_chi_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_chi_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%stdev_chi_1 == kgenref_var%stdev_chi_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_stdev_chi_1 = ABS(var%stdev_chi_1 - kgenref_var%stdev_chi_1) 
          IF (diff_stdev_chi_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_stdev_chi_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_stdev_chi_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%stdev_chi_2 == kgenref_var%stdev_chi_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_stdev_chi_2 = ABS(var%stdev_chi_2 - kgenref_var%stdev_chi_2) 
          IF (diff_stdev_chi_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_chi_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_stdev_chi_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_stdev_chi_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%stdev_eta_1 == kgenref_var%stdev_eta_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_stdev_eta_1 = ABS(var%stdev_eta_1 - kgenref_var%stdev_eta_1) 
          IF (diff_stdev_eta_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_stdev_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_stdev_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%stdev_eta_2 == kgenref_var%stdev_eta_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_stdev_eta_2 = ABS(var%stdev_eta_2 - kgenref_var%stdev_eta_2) 
          IF (diff_stdev_eta_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%stdev_eta_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_stdev_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_stdev_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%covar_chi_eta_1 == kgenref_var%covar_chi_eta_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_covar_chi_eta_1 = ABS(var%covar_chi_eta_1 - kgenref_var%covar_chi_eta_1) 
          IF (diff_covar_chi_eta_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_covar_chi_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_covar_chi_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%covar_chi_eta_2 == kgenref_var%covar_chi_eta_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_covar_chi_eta_2 = ABS(var%covar_chi_eta_2 - kgenref_var%covar_chi_eta_2) 
          IF (diff_covar_chi_eta_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%covar_chi_eta_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_covar_chi_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_covar_chi_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_chi_1 == kgenref_var%corr_w_chi_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_chi_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_chi_1 = ABS(var%corr_w_chi_1 - kgenref_var%corr_w_chi_1) 
          IF (diff_corr_w_chi_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_chi_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_chi_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_chi_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_chi_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_chi_2 == kgenref_var%corr_w_chi_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_chi_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_chi_2 = ABS(var%corr_w_chi_2 - kgenref_var%corr_w_chi_2) 
          IF (diff_corr_w_chi_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_chi_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_chi_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_chi_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_chi_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_eta_1 == kgenref_var%corr_w_eta_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_eta_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_eta_1 = ABS(var%corr_w_eta_1 - kgenref_var%corr_w_eta_1) 
          IF (diff_corr_w_eta_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_eta_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_eta_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_w_eta_2 == kgenref_var%corr_w_eta_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_w_eta_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_w_eta_2 = ABS(var%corr_w_eta_2 - kgenref_var%corr_w_eta_2) 
          IF (diff_corr_w_eta_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_eta_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_w_eta_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_w_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_w_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_chi_eta_1 == kgenref_var%corr_chi_eta_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_chi_eta_1 = ABS(var%corr_chi_eta_1 - kgenref_var%corr_chi_eta_1) 
          IF (diff_corr_chi_eta_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_chi_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_chi_eta_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%corr_chi_eta_2 == kgenref_var%corr_chi_eta_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_corr_chi_eta_2 = ABS(var%corr_chi_eta_2 - kgenref_var%corr_chi_eta_2) 
          IF (diff_corr_chi_eta_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%corr_chi_eta_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_corr_chi_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_corr_chi_eta_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%rsatl_1 == kgenref_var%rsatl_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rsatl_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_rsatl_1 = ABS(var%rsatl_1 - kgenref_var%rsatl_1) 
          IF (diff_rsatl_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rsatl_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rsatl_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_rsatl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_rsatl_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%rsatl_2 == kgenref_var%rsatl_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rsatl_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_rsatl_2 = ABS(var%rsatl_2 - kgenref_var%rsatl_2) 
          IF (diff_rsatl_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rsatl_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rsatl_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_rsatl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_rsatl_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%rc_1 == kgenref_var%rc_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rc_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_rc_1 = ABS(var%rc_1 - kgenref_var%rc_1) 
          IF (diff_rc_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rc_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rc_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_rc_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_rc_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%rc_2 == kgenref_var%rc_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%rc_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_rc_2 = ABS(var%rc_2 - kgenref_var%rc_2) 
          IF (diff_rc_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rc_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%rc_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_rc_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_rc_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cloud_frac_1 == kgenref_var%cloud_frac_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cloud_frac_1 = ABS(var%cloud_frac_1 - kgenref_var%cloud_frac_1) 
          IF (diff_cloud_frac_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_cloud_frac_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_frac_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%cloud_frac_2 == kgenref_var%cloud_frac_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_cloud_frac_2 = ABS(var%cloud_frac_2 - kgenref_var%cloud_frac_2) 
          IF (diff_cloud_frac_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%cloud_frac_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_cloud_frac_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_cloud_frac_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%mixt_frac == kgenref_var%mixt_frac) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%mixt_frac is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_mixt_frac = ABS(var%mixt_frac - kgenref_var%mixt_frac) 
          IF (diff_mixt_frac <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mixt_frac is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%mixt_frac is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_mixt_frac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_mixt_frac 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ice_supersat_frac_1 == kgenref_var%ice_supersat_frac_1) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ice_supersat_frac_1 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ice_supersat_frac_1 = ABS(var%ice_supersat_frac_1 - kgenref_var%ice_supersat_frac_1) 
          IF (diff_ice_supersat_frac_1 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ice_supersat_frac_1 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ice_supersat_frac_1 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_ice_supersat_frac_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ice_supersat_frac_1 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%ice_supersat_frac_2 == kgenref_var%ice_supersat_frac_2) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%ice_supersat_frac_2 is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_ice_supersat_frac_2 = ABS(var%ice_supersat_frac_2 - kgenref_var%ice_supersat_frac_2) 
          IF (diff_ice_supersat_frac_2 <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ice_supersat_frac_2 is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%ice_supersat_frac_2 is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_ice_supersat_frac_2 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_ice_supersat_frac_2 
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
  END SUBROUTINE kv_pdf_parameter_module_pdf_parameter 
    
  !verify state subroutine for kv_kgen_pdf_parameter_module_typesubp0 
  RECURSIVE SUBROUTINE kv_kgen_pdf_parameter_module_typesubp0(varname, check_status, var, kgenref_var) 
      CHARACTER(LEN=*), INTENT(IN) :: varname 
      TYPE(check_t), INTENT(INOUT) :: check_status 
      TYPE(implicit_coefs_terms), INTENT(IN) :: var, kgenref_var 
      TYPE(check_t) :: dtype_check_status, comp_check_status 
      INTEGER :: check_result 
      LOGICAL :: is_print = .FALSE. 
        
      real(KIND=core_rknd) :: diff_coef_wp4_implicit 
      real(KIND=core_rknd) :: diff_coef_wprtp2_implicit 
      real(KIND=core_rknd) :: diff_coef_wpthlp2_implicit 
      real(KIND=core_rknd) :: diff_coef_wp2rtp_implicit 
      real(KIND=core_rknd) :: diff_term_wp2rtp_explicit 
      real(KIND=core_rknd) :: diff_coef_wp2thlp_implicit 
      real(KIND=core_rknd) :: diff_term_wp2thlp_explicit 
      real(KIND=core_rknd) :: diff_coef_wprtpthlp_implicit 
      real(KIND=core_rknd) :: diff_term_wprtpthlp_explicit 
        
      check_status%numTotal = check_status%numTotal + 1 
        
      CALL kgen_init_check(dtype_check_status, rank=check_status%rank) 
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%coef_wp4_implicit == kgenref_var%coef_wp4_implicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%coef_wp4_implicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_coef_wp4_implicit = ABS(var%coef_wp4_implicit - kgenref_var%coef_wp4_implicit) 
          IF (diff_coef_wp4_implicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wp4_implicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wp4_implicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_coef_wp4_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_coef_wp4_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%coef_wprtp2_implicit == kgenref_var%coef_wprtp2_implicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%coef_wprtp2_implicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_coef_wprtp2_implicit = ABS(var%coef_wprtp2_implicit - kgenref_var%coef_wprtp2_implicit) 
          IF (diff_coef_wprtp2_implicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wprtp2_implicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wprtp2_implicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_coef_wprtp2_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_coef_wprtp2_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%coef_wpthlp2_implicit == kgenref_var%coef_wpthlp2_implicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%coef_wpthlp2_implicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_coef_wpthlp2_implicit = ABS(var%coef_wpthlp2_implicit - kgenref_var%coef_wpthlp2_implicit) 
          IF (diff_coef_wpthlp2_implicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wpthlp2_implicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wpthlp2_implicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_coef_wpthlp2_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_coef_wpthlp2_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%coef_wp2rtp_implicit == kgenref_var%coef_wp2rtp_implicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%coef_wp2rtp_implicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_coef_wp2rtp_implicit = ABS(var%coef_wp2rtp_implicit - kgenref_var%coef_wp2rtp_implicit) 
          IF (diff_coef_wp2rtp_implicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wp2rtp_implicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wp2rtp_implicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_coef_wp2rtp_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_coef_wp2rtp_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%term_wp2rtp_explicit == kgenref_var%term_wp2rtp_explicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%term_wp2rtp_explicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_term_wp2rtp_explicit = ABS(var%term_wp2rtp_explicit - kgenref_var%term_wp2rtp_explicit) 
          IF (diff_term_wp2rtp_explicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%term_wp2rtp_explicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%term_wp2rtp_explicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_term_wp2rtp_explicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_term_wp2rtp_explicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%coef_wp2thlp_implicit == kgenref_var%coef_wp2thlp_implicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%coef_wp2thlp_implicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_coef_wp2thlp_implicit = ABS(var%coef_wp2thlp_implicit - kgenref_var%coef_wp2thlp_implicit) 
          IF (diff_coef_wp2thlp_implicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wp2thlp_implicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wp2thlp_implicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_coef_wp2thlp_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_coef_wp2thlp_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%term_wp2thlp_explicit == kgenref_var%term_wp2thlp_explicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%term_wp2thlp_explicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_term_wp2thlp_explicit = ABS(var%term_wp2thlp_explicit - kgenref_var%term_wp2thlp_explicit) 
          IF (diff_term_wp2thlp_explicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%term_wp2thlp_explicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%term_wp2thlp_explicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_term_wp2thlp_explicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_term_wp2thlp_explicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
        
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%coef_wprtpthlp_implicit == kgenref_var%coef_wprtpthlp_implicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%coef_wprtpthlp_implicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_coef_wprtpthlp_implicit = ABS(var%coef_wprtpthlp_implicit - kgenref_var%coef_wprtpthlp_implicit) 
          IF (diff_coef_wprtpthlp_implicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wprtpthlp_implicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%coef_wprtpthlp_implicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_coef_wprtpthlp_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_coef_wprtpthlp_implicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      END IF   
      dtype_check_status%numTotal = dtype_check_status%numTotal + 1 
      IF (var%term_wprtpthlp_explicit == kgenref_var%term_wprtpthlp_explicit) THEN 
          dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank == 0) THEN 
                  WRITE (*, *) trim(adjustl(varname)), "%term_wprtpthlp_explicit is IDENTICAL." 
              END IF   
          END IF   
          check_result = CHECK_IDENTICAL 
      ELSE 
          diff_term_wprtpthlp_explicit = ABS(var%term_wprtpthlp_explicit - kgenref_var%term_wprtpthlp_explicit) 
          IF (diff_term_wprtpthlp_explicit <= kgen_tolerance) THEN 
              dtype_check_status%numInTol = dtype_check_status%numInTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%term_wprtpthlp_explicit is NOT IDENTICAL(within tolerance)." 
                  END IF   
              END IF   
              check_result = CHECK_IN_TOL 
          ELSE 
              dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1 
              IF (kgen_verboseLevel > 1) THEN 
                  IF (check_status%rank == 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), "%term_wprtpthlp_explicit is NOT IDENTICAL(out of tolerance)." 
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
                  WRITE (*, *) "Difference is ", diff_term_wprtpthlp_explicit 
                  WRITE (*, *) "" 
              END IF   
          END IF   
      ELSE IF (check_result == CHECK_IN_TOL) THEN 
          IF (kgen_verboseLevel > 2) THEN 
              IF (check_status%rank ==0) THEN 
                  WRITE (*, *) "Difference is ", diff_term_wprtpthlp_explicit 
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
  END SUBROUTINE kv_kgen_pdf_parameter_module_typesubp0 
    
end module pdf_parameter_module