!-----------------------------------------------------------------------
! $Id: pdf_parameter_module.F90 7309 2014-09-20 17:06:28Z betlej@uwm.edu $
!===============================================================================
module pdf_parameter_module
! Description:
!   This module defines the derived type pdf_parameter.
! References:
!   None
!-------------------------------------------------------------------------------

  use clubb_precision, only: &
    core_rknd

  implicit none

  private ! Default scope

  public :: pdf_parameter

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
      rrtthl,          & ! Correlation of r_t and th_l (both components)     [-]
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
      corr_chi_eta_1,  & ! Correlation of chi and eta (1st PDF component)    [-]
      corr_chi_eta_2,  & ! Correlation of chi and eta (2nd PDF component)    [-]
      rsatl_1,         & ! Saturation mixing ratio r_sat(mu_Tl_1,p)      [kg/kg]
      rsatl_2,         & ! Saturation mixing ratio r_sat(mu_Tl_2,p)      [kg/kg]
      rc_1,            & ! Mean of r_c (1st PDF component)               [kg/kg]
      rc_2,            & ! Mean of r_c (2nd PDF component)               [kg/kg]
      cloud_frac_1,    & ! Cloud fraction (1st PDF component)                [-]
      cloud_frac_2,    & ! Cloud fraction (2nd PDF component)                [-]
      mixt_frac          ! Weight of 1st PDF component (Sk_w dependent)      [-]
  end type pdf_parameter



  public :: pack_pdf_params, unpack_pdf_params

  integer, public, parameter :: num_pdf_params = 36

  !-------
  PUBLIC kw_pdf_parameter_module_pdf_parameter 
  contains
  !-------

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
      	   get_param_at_ind = pp_struct%rrtthl
      CASE (14)
      	   get_param_at_ind = pp_struct%alpha_thl
      CASE (15)
      	   get_param_at_ind = pp_struct%alpha_rt
      CASE (16)
      	   get_param_at_ind = pp_struct%crt_1
      CASE (17)
      	   get_param_at_ind = pp_struct%crt_2
      CASE (18)
      	   get_param_at_ind = pp_struct%cthl_1
      CASE (19)
      	   get_param_at_ind = pp_struct%cthl_2
      CASE (20)
      	   get_param_at_ind = pp_struct%chi_1
      CASE (21)
      	   get_param_at_ind = pp_struct%chi_2
      CASE (22)
      	   get_param_at_ind = pp_struct%stdev_chi_1
      CASE (23)
      	   get_param_at_ind = pp_struct%stdev_chi_2
      CASE (24)
      	   get_param_at_ind = pp_struct%stdev_eta_1
      CASE (25)
      	   get_param_at_ind = pp_struct%stdev_eta_2
      CASE (26)
      	   get_param_at_ind = pp_struct%covar_chi_eta_1
      CASE (27)
      	   get_param_at_ind = pp_struct%covar_chi_eta_2
      CASE (28)
      	   get_param_at_ind = pp_struct%corr_chi_eta_1
      CASE (29)
      	   get_param_at_ind = pp_struct%corr_chi_eta_2
      CASE (30)
      	   get_param_at_ind = pp_struct%rsatl_1
      CASE (31)
      	   get_param_at_ind = pp_struct%rsatl_2
      CASE (32)
      	   get_param_at_ind = pp_struct%rc_1
      CASE (33)
      	   get_param_at_ind = pp_struct%rc_2
      CASE (34)
      	   get_param_at_ind = pp_struct%cloud_frac_1
      CASE (35)
      	   get_param_at_ind = pp_struct%cloud_frac_2
      CASE (36)
      	   get_param_at_ind = pp_struct%mixt_frac
      CASE DEFAULT
!          NAG compiler does not like divide by zero - commented out
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
      	   pp_struct%rrtthl = val
      CASE (14)
      	   pp_struct%alpha_thl = val
      CASE (15)
      	   pp_struct%alpha_rt = val
      CASE (16)
      	   pp_struct%crt_1 = val
      CASE (17)
      	   pp_struct%crt_2 = val
      CASE (18)
      	   pp_struct%cthl_1 = val
      CASE (19)
      	   pp_struct%cthl_2 = val
      CASE (20)
      	   pp_struct%chi_1 = val
      CASE (21)
      	   pp_struct%chi_2 = val
      CASE (22)
      	   pp_struct%stdev_chi_1 = val
      CASE (23)
      	   pp_struct%stdev_chi_2 = val
      CASE (24)
      	   pp_struct%stdev_eta_1 = val
      CASE (25)
      	   pp_struct%stdev_eta_2 = val
      CASE (26)
      	   pp_struct%covar_chi_eta_1 = val
      CASE (27)
      	   pp_struct%covar_chi_eta_2 = val
      CASE (28)
      	   pp_struct%corr_chi_eta_1 = val
      CASE (29)
      	   pp_struct%corr_chi_eta_2 = val
      CASE (30)
      	   pp_struct%rsatl_1 = val
      CASE (31)
      	   pp_struct%rsatl_2 = val
      CASE (32)
      	   pp_struct%rc_1 = val
      CASE (33)
      	   pp_struct%rc_2 = val
      CASE (34)
      	   pp_struct%cloud_frac_1 = val
      CASE (35)
      	   pp_struct%cloud_frac_2 = val
      CASE (36)
      	   pp_struct%mixt_frac = val
      CASE DEFAULT
      	   ! do nothing !
    END SELECT

  end subroutine set_param_at_ind



  !read state subroutine for kw_pdf_parameter_module_pdf_parameter 
  RECURSIVE SUBROUTINE kw_pdf_parameter_module_pdf_parameter(var, kgen_unit, printvar) 
      TYPE(pdf_parameter), INTENT(IN) :: var 
      INTEGER, INTENT(IN) :: kgen_unit 
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
      LOGICAL :: kgen_istrue 
      REAL(KIND=8) :: kgen_array_sum 
        
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%w_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%w_1 **" // NEW_LINE("A"), var%w_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%w_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%w_2 **" // NEW_LINE("A"), var%w_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%varnce_w_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_w_1 **" // NEW_LINE("A"), var%varnce_w_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%varnce_w_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_w_2 **" // NEW_LINE("A"), var%varnce_w_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rt_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rt_1 **" // NEW_LINE("A"), var%rt_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rt_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rt_2 **" // NEW_LINE("A"), var%rt_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%varnce_rt_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_rt_1 **" // NEW_LINE("A"), var%varnce_rt_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%varnce_rt_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_rt_2 **" // NEW_LINE("A"), var%varnce_rt_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%thl_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%thl_1 **" // NEW_LINE("A"), var%thl_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%thl_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%thl_2 **" // NEW_LINE("A"), var%thl_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%varnce_thl_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_thl_1 **" // NEW_LINE("A"), var%varnce_thl_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%varnce_thl_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%varnce_thl_2 **" // NEW_LINE("A"), var%varnce_thl_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rrtthl 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rrtthl **" // NEW_LINE("A"), var%rrtthl 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%alpha_thl 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%alpha_thl **" // NEW_LINE("A"), var%alpha_thl 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%alpha_rt 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%alpha_rt **" // NEW_LINE("A"), var%alpha_rt 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%crt_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%crt_1 **" // NEW_LINE("A"), var%crt_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%crt_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%crt_2 **" // NEW_LINE("A"), var%crt_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%cthl_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cthl_1 **" // NEW_LINE("A"), var%cthl_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%cthl_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cthl_2 **" // NEW_LINE("A"), var%cthl_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%chi_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%chi_1 **" // NEW_LINE("A"), var%chi_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%chi_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%chi_2 **" // NEW_LINE("A"), var%chi_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%stdev_chi_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_chi_1 **" // NEW_LINE("A"), var%stdev_chi_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%stdev_chi_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_chi_2 **" // NEW_LINE("A"), var%stdev_chi_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%stdev_eta_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_eta_1 **" // NEW_LINE("A"), var%stdev_eta_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%stdev_eta_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%stdev_eta_2 **" // NEW_LINE("A"), var%stdev_eta_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%covar_chi_eta_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%covar_chi_eta_1 **" // NEW_LINE("A"), var%covar_chi_eta_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%covar_chi_eta_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%covar_chi_eta_2 **" // NEW_LINE("A"), var%covar_chi_eta_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%corr_chi_eta_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%corr_chi_eta_1 **" // NEW_LINE("A"), var%corr_chi_eta_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%corr_chi_eta_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%corr_chi_eta_2 **" // NEW_LINE("A"), var%corr_chi_eta_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rsatl_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rsatl_1 **" // NEW_LINE("A"), var%rsatl_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rsatl_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rsatl_2 **" // NEW_LINE("A"), var%rsatl_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rc_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rc_1 **" // NEW_LINE("A"), var%rc_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%rc_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%rc_2 **" // NEW_LINE("A"), var%rc_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%cloud_frac_1 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cloud_frac_1 **" // NEW_LINE("A"), var%cloud_frac_1 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%cloud_frac_2 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%cloud_frac_2 **" // NEW_LINE("A"), var%cloud_frac_2 
      END IF   
      kgen_istrue = .TRUE. 
      WRITE (UNIT = kgen_unit) var%mixt_frac 
      IF (PRESENT( printvar )) THEN 
          WRITE (*, *) "** KGEN DEBUG: " // printvar // "%mixt_frac **" // NEW_LINE("A"), var%mixt_frac 
      END IF   
        
  END SUBROUTINE kw_pdf_parameter_module_pdf_parameter 
    
end module pdf_parameter_module