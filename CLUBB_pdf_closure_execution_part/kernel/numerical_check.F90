!KGEN-generated Fortran source file

!Generated at : 2016-01-04 08:38:23
!KGEN version : 0.6.0

!------------------------------------------------------------------------
! $Id: numerical_check.F90 7309 2014-09-20 17:06:28Z betlej@uwm.edu $
!===============================================================================
module numerical_check

    IMPLICIT NONE

!       Made is_nan_2d public so it may be used
!       for finding code that cause NaNs
!       Joshua Fasching November 2007

!       *_check subroutines were added to ensure that the
!       subroutines they are checking perform correctly
!       Joshua Fasching February 2008

!       rad_clipping has been replaced by rad_check as the new
!       subroutine only reports if there are invalid values.
!       Joshua Fasching March 2008

    PRIVATE

    PUBLIC is_nan_2d, pdf_closure_check, is_nan_sclr

    PRIVATE check_nan


  ! Abstraction of check_nan
  interface check_nan
    module procedure check_nan_sclr, check_nan_2d
  end interface

  ! Abstraction of check_negative



  contains
!---------------------------------------------------------------------------------
!
!        Description: This subroutine determines if any of the output
!        variables for the length_new subroutine carry values that
!        are NaNs.
!
!        Joshua Fasching February 2008
!---------------------------------------------------------------------------------



    ! Constant Parameters

    ! Input Variables

    ! Output Variable

!-----------------------------------------------------------------------------



!---------------------------------------------------------------------------
  subroutine pdf_closure_check( wp4, wprtp2, wp2rtp, wpthlp2, & 
                          wp2thlp, cloud_frac, rcm, wpthvp, wp2thvp, & 
                          rtpthvp, thlpthvp, wprcp, wp2rcp, & 
                          rtprcp, thlprcp, rcp2, wprtpthlp, & 
                          crt_1, crt_2, cthl_1, cthl_2, pdf_params, &
                          sclrpthvp, sclrprcp, wpsclrp2, & 
                          wpsclrprtp, wpsclrpthlp, wp2sclrp, &
                          err_code )

! Description: This subroutine determines if any of the output
!   variables for the pdf_closure subroutine carry values that
!   are NaNs.
!
! Joshua Fasching February 2008
!---------------------------------------------------------------------------

      USE parameters_model, ONLY: sclr_dim

      USE pdf_parameter_module, ONLY: pdf_parameter

      USE stats_variables, ONLY: iwp4, ircp2, iwprtp2, iwprtpthlp, iwpthlp2

      USE clubb_precision, ONLY: core_rknd

      USE pdf_parameter_module, ONLY: kr_pdf_parameter_module_pdf_parameter
      USE pdf_parameter_module, ONLY: kv_pdf_parameter_module_pdf_parameter
    implicit none

    ! Parameter Constants
    character(len=*), parameter :: proc_name = &
      "pdf_closure"

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: & 
      wp4,             & ! w'^4                  [m^4/s^4]
      wprtp2,          & ! w' r_t'               [(m kg)/(s kg)]
      wp2rtp,          & ! w'^2 r_t'             [(m^2 kg)/(s^2 kg)]
      wpthlp2,         & ! w' th_l'^2            [(m K^2)/s]
      wp2thlp,         & ! w'^2 th_l'            [(m^2 K)/s^2]
      cloud_frac,      & ! Cloud fraction        [-]
      rcm,             & ! Mean liquid water     [kg/kg]
      wpthvp,          & ! Buoyancy flux         [(K m)/s] 
      wp2thvp,         & ! w'^2 th_v'            [(m^2 K)/s^2]
      rtpthvp,         & ! r_t' th_v'            [(kg K)/kg]
      thlpthvp,        & ! th_l' th_v'           [K^2]
      wprcp,           & ! w' r_c'               [(m kg)/(s kg)]
      wp2rcp,          & ! w'^2 r_c'             [(m^2 kg)/(s^2 kg)]
      rtprcp,          & ! r_t' r_c'             [(kg^2)/(kg^2)]
      thlprcp,         & ! th_l' r_c'            [(K kg)/kg]
      rcp2,            & ! r_c'^2                [(kg^2)/(kg^2)]
      wprtpthlp,       & ! w' r_t' th_l'         [(m kg K)/(s kg)]
      crt_1, crt_2,  & 
      cthl_1, cthl_2

    type(pdf_parameter), intent(in) ::  & 
      pdf_params        ! PDF parameters          [units vary]

    ! Input (Optional passive scalar variables)
    real( kind = core_rknd ), dimension(sclr_dim), intent(in) ::  & 
      sclrpthvp,  & 
      sclrprcp,  & 
      wpsclrp2, & 
      wpsclrprtp, & 
      wpsclrpthlp, & 
      wp2sclrp

    ! Output Variable
    integer, intent(inout) ::  & 
      err_code          ! Returns appropriate error code

!-------------------------------------------------------------------------------

    ! ---- Begin Code ----

    if ( iwp4 > 0 ) call check_nan( wp4,"wp4", proc_name, err_code )
    if ( iwprtp2 > 0 ) call check_nan( wprtp2,"wprtp2", proc_name, err_code )
    call check_nan( wp2rtp,"wp2rtp", proc_name, err_code )
    if ( iwpthlp2 > 0 ) call check_nan( wpthlp2,"wpthlp2", proc_name, err_code )
    call check_nan( wp2thlp,"wp2thlp", proc_name, err_code )
    call check_nan( cloud_frac,"cloud_frac", proc_name, err_code )
    call check_nan( rcm,"rcm", proc_name, err_code )
    call check_nan( wpthvp, "wpthvp", proc_name, err_code )
    call check_nan( wp2thvp, "wp2thvp", proc_name, err_code )
    call check_nan( rtpthvp, "rtpthvp", proc_name, err_code )
    call check_nan( thlpthvp, "thlpthvp", proc_name, err_code )
    call check_nan( wprcp, "wprcp", proc_name, err_code )
    call check_nan( wp2rcp, "wp2rcp", proc_name, err_code )
    call check_nan( rtprcp, "rtprcp", proc_name, err_code )
    call check_nan( thlprcp, "thlprcp", proc_name, err_code )
    if ( ircp2 >  0 ) call check_nan( rcp2, "rcp2", proc_name, err_code)
    if ( iwprtpthlp > 0 ) call check_nan( wprtpthlp, "wprtpthlp", proc_name, err_code )
    call check_nan( crt_1, "crt_1", proc_name, err_code )
    call check_nan( crt_2, "crt_2", proc_name, err_code )
    call check_nan( cthl_1, "cthl_1", proc_name, err_code )
    call check_nan( cthl_2, "cthl_2", proc_name, err_code )
    ! Check each PDF parameter at the grid level sent in.
    call check_nan( pdf_params%w_1, "pdf_params%w_1", proc_name, err_code )
    call check_nan( pdf_params%w_2, "pdf_params%w_2", proc_name, err_code )
    call check_nan( pdf_params%varnce_w_1, "pdf_params%varnce_w_1", proc_name, err_code )
    call check_nan( pdf_params%varnce_w_2, "pdf_params%varnce_w_2", proc_name, err_code )
    call check_nan( pdf_params%rt_1, "pdf_params%rt_1", proc_name, err_code )
    call check_nan( pdf_params%rt_2, "pdf_params%rt_2", proc_name, err_code )
    call check_nan( pdf_params%varnce_rt_1, "pdf_params%varnce_rt_1", proc_name, err_code )
    call check_nan( pdf_params%varnce_rt_2, "pdf_params%varnce_rt_2", proc_name, err_code )
    call check_nan( pdf_params%thl_1, "pdf_params%thl_1", proc_name, err_code )
    call check_nan( pdf_params%thl_2, "pdf_params%thl_2", proc_name, err_code )
    call check_nan( pdf_params%varnce_thl_1, "pdf_params%varnce_thl_1", proc_name, err_code )
    call check_nan( pdf_params%varnce_thl_2, "pdf_params%varnce_thl_2", proc_name, err_code )
    call check_nan( pdf_params%mixt_frac, "pdf_params%mixt_frac", proc_name, err_code )
    call check_nan( pdf_params%rrtthl, "pdf_params%rrtthl", proc_name, err_code )
    call check_nan( pdf_params%rc_1, "pdf_params%rc_1", proc_name, err_code )
    call check_nan( pdf_params%rc_2, "pdf_params%rc_2", proc_name, err_code )
    call check_nan( pdf_params%rsatl_1, "pdf_params%rsatl_1", proc_name, err_code )
    call check_nan( pdf_params%rsatl_2, "pdf_params%rsatl_2", proc_name, err_code )
    call check_nan( pdf_params%cloud_frac_1, "pdf_params%cloud_frac_1", proc_name, err_code )
    call check_nan( pdf_params%cloud_frac_2, "pdf_params%cloud_frac_2", proc_name, err_code )
    call check_nan( pdf_params%chi_1, "pdf_params%chi_1", proc_name, err_code )
    call check_nan( pdf_params%chi_2, "pdf_params%chi_2", proc_name, err_code )
    call check_nan( pdf_params%stdev_chi_1, "pdf_params%stdev_chi_1", proc_name, err_code )
    call check_nan( pdf_params%stdev_chi_2, "pdf_params%stdev_chi_2", proc_name, err_code )
    call check_nan( pdf_params%alpha_thl, "pdf_params%alpha_thl", proc_name, err_code )
    call check_nan( pdf_params%alpha_rt, "pdf_params%alpha_rt", proc_name, err_code )

    if ( sclr_dim > 0 ) then
      call check_nan( sclrpthvp,"sclrpthvp", & 
                      proc_name, err_code)
      call check_nan( sclrprcp, "sclrprcp", & 
                      proc_name, err_code )
      call check_nan( wpsclrprtp, "wpsclrprtp",  & 
                      proc_name, err_code )
      call check_nan( wpsclrp2, "wpsclrp2",  & 
                      proc_name, err_code )
      call check_nan( wpsclrpthlp, "wpsclrtlp",  & 
                      proc_name, err_code )
      call check_nan( wp2sclrp, "wp2sclrp",  & 
                      proc_name, err_code )
    end if

    return
  end subroutine pdf_closure_check

!-------------------------------------------------------------------------------
!
! Description:
!   This subroutine determines what input variables may have NaN values.
!   In addition it checks to see if rho_zm, rho, exner, up2, vp2, rtm, thlm,
!   wp2, rtp2, thlp2, or tau_zm have negative values.
!-------------------------------------------------------------------------------





    ! Constant Parameters
    ! Name of the procedure using parameterization_check

    ! Input variables


    ! These are prognostic or are planned to be in the future






    ! In / Out Variables

    ! Local Variables

!-------- Input Nan Check ----------------------------------------------
















!---------------------------------------------------------------------




!-----------------------------------------------------------------------
!
!       Description:This subroutine determines if any of the output
!       variables for the surface_varnce subroutine carry values that
!       are nans.
!
!       Joshua Fasching February 2008
!
!
!-----------------------------------------------------------------------



    ! Constant Parameters
    ! Name of the subroutine calling the check

    ! Input Variables



    ! Input/Output Variable

!-----------------------------------------------------------------------

    ! ---- Begin Code ----






!-----------------------------------------------------------------------
! Description:
!   Checks radiation input variables. If they are < 0 it reports
!   to the console.
!------------------------------------------------------------------------




    ! Constant Parameters

    ! Input/Output variables

    ! Local variables

!-------------------------------------------------------------------------





!-----------------------------------------------------------------------

!       Description:
!       Checks for invalid floating point values in select model arrays.

!       References:
!       None
!------------------------------------------------------------------------







    ! Local Variables


    ! Check whether any variable array contains a NaN for
    ! um, vm, thlm, rtm, rtp2, thlp2, wprtp, wpthlp, rtpthlp,
    ! wp2, & wp3.
!         write(fstderr,*) "um= ", um
!         return

!         write(fstderr,*) "vm= ", vm
!         return

!         write(fstderr,*) "wp2= ", wp2
!         return

!         write(fstderr,*) "wp3= ", wp3
!         return

!         write(fstderr,*) "rtm= ", rtm
!         return

!         write(fstderr,*) "thlm= ", thlm
!         return

!         write(fstderr,*) "rtp2= ", rtp2
!         return

!         write(fstderr,*) "thlp2= ", thlp2
!         return

!         write(fstderr,*) "wprtp= ", wprtp
!         return

!         write(fstderr,*) "wpthlp= ", wpthlp
!         return

!         write(fstderr,*) "rtpthlp= ", rtpthlp
!         return

!             write(fstderr,*) "hydromet= ", hydromet
!             return

!       if ( is_nan_2d( wm_zt ) ) then
!         write(fstderr,*) "NaN in wm_zt model array"
!         write(fstderr,*) "wm_zt= ", wm_zt
!         invalid_model_arrays = .true.
!         return
!       end if

!         write(fstderr,*) "wp2thvp = ", wp2thvp
!         return

!         write(fstderr,*) "rtpthvp = ", rtpthvp

!         write(fstderr,*) "thlpthvp = ", thlpthvp

!           write(fstderr,'(a6,i2,a1)') "sclrm(", i, ")"
!           write(fstderr,*) sclrm(:,i)

!           write(fstderr,'(a8,i2,a1)') "edsclrm(", i, ")"
!           write(fstderr,*) edsclrm(:,i)


!------------------------------------------------------------------------
  logical function is_nan_sclr( xarg )

! Description:
!   Checks if a given scalar real is a NaN, +inf or -inf.

! Notes:
!   I was advised by Andy Vaught to use a data statement and the transfer( )
!   intrinsic rather than using a hex number in a parameter for portability.

!   Certain compiler optimizations may cause variables with invalid
!   results to flush to zero.  Avoid these!
!  -dschanen 16 Dec 2010

!------------------------------------------------------------------------


      USE parameters_model, ONLY: posinf


      USE clubb_precision, ONLY: core_rknd

    implicit none

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: xarg




    ! ---- Begin Code ---

    ! This works on compilers with standardized floating point,
    ! because the IEEE 754 spec defines that subnormals and nans
    ! should not equal themselves.
    ! However, all compilers do not seem to follow this.
    if (xarg /= xarg ) then
      is_nan_sclr = .true.

      ! This a second check, assuming the above does not work as
      ! expected.
    else if ( xarg == PosInf ) then
      is_nan_sclr = .true.

    else
      is_nan_sclr = .false. ! Our result should be a standard float

    end if


    return
  end function is_nan_sclr
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  logical function is_nan_2d( x2d )

! Description:
!   Checks if a given real vector is a NaN, +inf or -inf.

!------------------------------------------------------------------------

      USE clubb_precision, ONLY: core_rknd

    implicit none

    ! External
    intrinsic :: any

    ! Input Variables
    real( kind = core_rknd ), dimension(:), intent(in) :: x2d

    ! Local Variables
    integer :: k

    ! ---- Begin Code ----

    is_nan_2d = .false.

    do k = 1, size( x2d )
      if ( is_nan_sclr( x2d(k) ) ) then
        is_nan_2d = .true.
        exit
      end if
    end do

    return

  end function is_nan_2d

!------------------------------------------------------------------------
!
! Description:
!   Checks for negative values in the var array and reports them.
!
!-----------------------------------------------------------------------




    ! External

    ! Input Variables


    ! Optional In/Out Variable








!------------------------------------------------------------------------
!
! Description:
!   Checks for negative values in the var array and reports
!   the index in which the negative values occur.
!
!-----------------------------------------------------------------------




    ! External

    ! Input Variables



    ! Optional In/Out Variable

    ! Local Variable











!------------------------------------------------------------------------
  subroutine check_nan_2d( var, varname, operation, err_code )
!
!  Description:
!    Checks for a NaN in the var array and reports it.
!
!
!------------------------------------------------------------------------
      USE constants_clubb, ONLY: fstderr
      USE error_code, ONLY: clubb_var_equals_nan
      USE clubb_precision, ONLY: core_rknd

    implicit none

    ! External
    intrinsic :: present

    ! Input variables
    real( kind = core_rknd ), intent(in), dimension(:) :: var ! Variable being examined

    character(len=*), intent(in)::  & 
      varname,  & ! Name of variable
      operation   ! Procedure calling check_nan

    ! Optional In/Out Variable
    integer, optional, intent(inout) :: err_code

    if ( is_nan_2d( var ) ) then
      write(fstderr,*) varname, " is NaN in ",operation
      if ( present( err_code ) ) then
        if( err_code < clubb_var_equals_NaN ) then
          err_code = clubb_var_equals_NaN
        end if
      end if
    end if

    return
  end subroutine check_nan_2d

!-----------------------------------------------------------------------
  subroutine check_nan_sclr( var, varname, operation, err_code )
!
! Description:
!   Checks for a NaN in the scalar var then reports it.
!
!-----------------------------------------------------------------------
      USE constants_clubb, ONLY: fstderr
      USE error_code, ONLY: clubb_var_equals_nan
      USE clubb_precision, ONLY: core_rknd

    implicit none

    ! External
    intrinsic :: present

    ! Input Variables
    real( kind = core_rknd ), intent(in) :: var        ! Variable being examined

    character(len=*), intent(in)::  & 
      varname,    & ! Name of variable being examined
      operation  ! Procedure calling check_nan

    ! Optional In/Out variable
    integer, optional, intent(inout) :: err_code
!--------------------------------------------------------------------
    if ( is_nan_sclr( var ) ) then
      write(fstderr,*) varname, " is NaN in ",operation
      if ( present( err_code ) ) then
        if( err_code < clubb_var_equals_NaN ) then
          err_code = clubb_var_equals_NAN
        end if
      end if
    end if

    return

  end subroutine check_nan_sclr
!-------------------------------------------------------------------------

!-----------------------------------------------------------------------
!
! Description:
!   Checks whether there is conservation within the column and returns any
!   imbalance as spurious_source where spurious_source is defined negative
!   for a spurious sink.
!
!-----------------------------------------------------------------------



    ! Input Variables

    ! Return Variable

!--------------------------------------------------------------------

    ! ---- Begin Code ----



!-------------------------------------------------------------------------
end module numerical_check