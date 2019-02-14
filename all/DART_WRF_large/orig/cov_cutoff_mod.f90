!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: cov_cutoff_mod.f90 11289 2017-03-10 21:56:06Z hendric@ucar.edu $


!

module cov_cutoff_mod


    USE types_mod, ONLY: r8 
    USE utilities_mod, ONLY: register_module, error_handler, e_err, e_msg, do_output, do_nml_file, do_nml_term, nmlfileunit, &
    &find_namelist_in_file, check_namelist_read 
    USE location_mod, ONLY: location_type 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
    USE location_mod, ONLY: kr_location_mod_location_type 
    USE location_mod, ONLY: kv_location_mod_location_type 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC comp_cov_factor 
! version controlled file description for error handling, do not edit

character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/cov_cutoff_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 11289 $"
character(len=128), parameter :: revdate  = "$Date: 2017-03-10 14:56:06 -0700 (Fri, 10 Mar 2017) $"
!============================================================================
!---- namelist with default values


logical :: namelist_initialized = .false.

integer :: select_localization = 1
! Value 1 selects default Gaspari-Cohn cutoff
! Value 2 selects boxcar
! Value 3 selects ramped boxcar

!============================================================================
PUBLIC kr_externs_in_cov_cutoff_mod 
PUBLIC kr_externs_out_cov_cutoff_mod 
PUBLIC kv_externs_cov_cutoff_mod 
PUBLIC kr_location_mod_location_type 
LOGICAL :: kgenref_namelist_initialized = .false. 
PUBLIC kv_location_mod_location_type 


contains
!======================================================================


function comp_cov_factor(z_in, c, obs_loc, obs_type, target_loc, target_kind, &
   localization_override)
!----------------------------------------------------------------------
! function comp_cov_factor(z_in, c)
! Computes a covariance cutoff function from Gaspari and Cohn
! QJRMS, 125, 723-757.  (their eqn. 4.10)
! z_in is the distance while c is the cutoff distance. 
! For distances greater than 2c, the cov_factor returned goes to 0.
! Other ramping shapes are also available and can be selected by a namelist
! parameter. At present, these include a boxcar with the given halfwidth
! and a ramp in which the weight is set at 1.0 up to the half-width 
! distance and then decreases linearly to 0 at twice the half-width 
! distance.
! Additional information is passed in about the location and specific type of the
! observation and the location and generic kind of the variable being targeted for
! increments. These can be used for more refined algorithms that want to 
! make the cutoff a function of these additional arguments. 
!
!


implicit none

real(r8),                      intent(in) :: z_in, c
type(location_type), optional, intent(in) :: obs_loc, target_loc
integer,             optional, intent(in) :: obs_type, target_kind
integer,             optional, intent(in) :: localization_override
real(r8)                                  :: comp_cov_factor

real(r8) :: z, r
integer  :: iunit, io
integer  :: localization_type
!--------------------------------------------------------
! Initialize namelist if not already done

if(.not. namelist_initialized) then

   call register_module(source, revision, revdate)

   namelist_initialized = .true.
   ! Read the namelist entry

   call find_namelist_in_file("input.nml", "cov_cutoff_nml", iunit)
   !call check_namelist_read(iunit, io, "cov_cutoff_nml")


   if (do_output()) then
      select case (select_localization)
         case (1)
            call error_handler(E_MSG,'comp_cov_factor:', &
               'Standard Gaspari Cohn localization selected')
         case (2)
            call error_handler(E_MSG,'comp_cov_factor:', &
               'Boxcar localization selected')
         case (3)
            call error_handler(E_MSG,'comp_cov_factor:', &
               'Ramped localization selected')
         case default
            call error_handler(E_ERR,'comp_cov_factor', &
               'Illegal value of "select_localization" in cov_cutoff_mod namelist', &
                source, revision, revdate )
      end select
   endif

endif
!---------------------------------------------------------

if(present(localization_override)) then
   localization_type = localization_override
else
   localization_type = select_localization
endif

z = abs(z_in)
!----------------------------------------------------------


if(localization_type == 1) then ! Standard Gaspari Cohn localization

   if( z >= c*2.0_r8 ) then

      comp_cov_factor = 0.0_r8

   else if( z <= c ) then
      r = z / c
      comp_cov_factor = &
           ( ( ( -0.25_r8*r +0.5_r8 )*r +0.625_r8 )*r -5.0_r8/3.0_r8 )*r**2 + 1.0_r8
!!$           r**5 * (-0.25_r8 ) + &
!!$           r**4 / 2.0_r8 +              &
!!$           r**3 * 5.0_r8/8.0_r8 -       &
!!$           r**2 * 5.0_r8/3.0_r8 + 1.0_r8
   else

      r = z / c
      comp_cov_factor = &
           ( ( ( ( r/12.0_r8 -0.5_r8 )*r +0.625_r8 )*r +5.0_r8/3.0_r8 )*r -5.0_r8 )*r &
!!$           r**5 / 12.0_r8  -  &
!!$           r**4 / 2.0_r8   +  &
!!$           r**3 * 5.0_r8 / 8.0_r8 + &
!!$           r**2 * 5.0_r8 / 3.0_r8 - 5.0_r8*r &
           + 4.0_r8 - 2.0_r8 / (3.0_r8 * r) 
!!$           r**5 / 12.0_r8  -  &
!!$           r**4 / 2.0_r8   +  &
!!$           r**3 * 5.0_r8 / 8.0_r8 + &
!!$           r**2 * 5.0_r8 / 3.0_r8 - 5.0_r8*r &
   endif

else if(localization_type == 2) then ! BOXCAR localization

   if(z < 2.0_r8 * c) then
      comp_cov_factor = 1.0_r8
   else
      comp_cov_factor = 0.0_r8
   endif

else if(localization_type == 3) then ! Ramped localization

   if(z >= 2.0_r8 * c) then
      comp_cov_factor = 0.0_r8
   else if(z >= c .and. z < 2.0_r8 * c) then
      comp_cov_factor = (2.0_r8 * c - z) / c
   else
      comp_cov_factor = 1.0_r8
   endif

else ! Otherwise namelist parameter is illegal; this is an error

     call error_handler(E_ERR,'comp_cov_factor', &
              'Illegal value of "localization" in cov_cutoff_mod namelist', &
               source, revision, revdate )

endif

end function comp_cov_factor

!read state subroutine for kr_externs_in_cov_cutoff_mod 
SUBROUTINE kr_externs_in_cov_cutoff_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) namelist_initialized 
    READ (UNIT = kgen_unit) select_localization 
END SUBROUTINE kr_externs_in_cov_cutoff_mod 
  
!read state subroutine for kr_externs_out_cov_cutoff_mod 
SUBROUTINE kr_externs_out_cov_cutoff_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_namelist_initialized 
END SUBROUTINE kr_externs_out_cov_cutoff_mod 
  
!verify state subroutine for kv_externs_cov_cutoff_mod 
SUBROUTINE kv_externs_cov_cutoff_mod(check_status) 
    TYPE(check_t), INTENT(INOUT) :: check_status 
      
    !CALL kv_cov_cutoff_mod_logical__("namelist_initialized", check_status, namelist_initialized, kgenref_namelist_initialized) 
END SUBROUTINE kv_externs_cov_cutoff_mod 
  
!verify state subroutine for kv_cov_cutoff_mod_logical__ 
RECURSIVE SUBROUTINE kv_cov_cutoff_mod_logical__(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    LOGICAL, INTENT(IN) :: var, kgenref_var 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    logical :: diff 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    IF (var .EQV. kgenref_var) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        check_status%numOutTol = check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_OUT_TOL 
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 0) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
END SUBROUTINE kv_cov_cutoff_mod_logical__ 
  
end module cov_cutoff_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/cov_cutoff_mod.f90 $
! $Id: cov_cutoff_mod.f90 11289 2017-03-10 21:56:06Z hendric@ucar.edu $
! $Revision: 11289 $
! $Date: 2017-03-10 14:56:06 -0700 (Fri, 10 Mar 2017) $
