!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:29 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: reg_factor_mod.f90 11289 2017-03-10 21:56:06Z hendric@ucar.edu $


!

module reg_factor_mod

    USE types_mod, ONLY: r8, i8 
    USE utilities_mod, ONLY: get_unit, open_file, register_module, error_handler, e_err, nmlfileunit, find_namelist_in_file, &
    &check_namelist_read, do_nml_file, do_nml_term 

    USE time_manager_mod, ONLY: time_type, get_time 
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 
    USE time_manager_mod, ONLY: kr_time_manager_mod_time_type 
    USE time_manager_mod, ONLY: kv_time_manager_mod_time_type 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC comp_reg_factor 
! version controlled file description for error handling, do not edit

character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/reg_factor_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 11289 $"
character(len=128), parameter :: revdate  = "$Date: 2017-03-10 14:56:06 -0700 (Fri, 10 Mar 2017) $"
!============================================================================


logical :: namelist_initialized = .false.
!---- namelist with default values


integer :: select_regression = 1
! Value 1 selects default: Compute using sampling theory for any ensemble size
! Value 2 selects L96 file format: Works for archived 40 observation L96 files
! Value 3 selects bgrid archive default: Reads in file from bgrid experiments
character(len = 129) :: input_reg_file = "time_mean_reg"
character(len = 129) :: reg_diagnostics_file = "reg_diagnostics"
logical              :: save_reg_diagnostics = .false.

namelist / reg_factor_nml / select_regression, input_reg_file, &
                            save_reg_diagnostics, reg_diagnostics_file

!============================================================================
! Flags for loading startup


logical :: first_call = .true.
! Unit for output diagnostics
integer :: diag_unit
! Size of regression input files
integer :: num_obs, model_size
! Global storage for time mean regression factors from file

real(r8), allocatable :: time_mean_reg(:, :)
! Global storage for bgrid mean regression factor file

real(r8), allocatable :: obs_state_reg(:)
PUBLIC kr_externs_in_reg_factor_mod 
PUBLIC kr_externs_out_reg_factor_mod 
PUBLIC kv_externs_reg_factor_mod 
PUBLIC kr_time_manager_mod_time_type 
LOGICAL :: kgenref_namelist_initialized = .false. 
LOGICAL :: kgenref_first_call = .true. 
INTEGER :: kgenref_diag_unit 
PUBLIC kv_time_manager_mod_time_type 


CONTAINS


function comp_reg_factor(num_groups, regress, obs_time, &
   obs_index, state_index, obs_state_ind, obs_state_max)
! Computes factor by which to multiply regression coefficients
! for a given distribution of sample regressions OR computes
! factor for a single sample of a regression using some other
! methodology (for instance time mean from previous runs). Could
! also implement the standard distance dependence method, too.


integer,         intent(in) :: num_groups
real(r8),        intent(in) :: regress(num_groups)
type(time_type), intent(in) :: obs_time
integer,         intent(in) :: obs_index
integer(i8),     intent(in) :: state_index
integer,         intent(in), optional :: obs_state_ind, obs_state_max

real(r8) :: comp_reg_factor

real(r8) :: sum_reg2, sum_reg_reg

integer :: i, j, ii, jj, iunit, io, secs, days
!--------------------------------------------------------
! Initialize namelist if not already done

if(.not. namelist_initialized) then

   call register_module(source,revision,revdate)

   namelist_initialized = .true.
   ! Read the namelist entry

   call find_namelist_in_file("input.nml", "reg_factor_nml", iunit)
   read(iunit, nml = reg_factor_nml, iostat = io)
   call check_namelist_read(iunit, io, "reg_factor_nml")
   ! Record the namelist values used for the run ...

   if (do_nml_file()) write(nmlfileunit, nml=reg_factor_nml)
   if (do_nml_term()) write(     *     , nml=reg_factor_nml)
   ! See if diagnostic output is requested, if so, open file

   if(save_reg_diagnostics) then
      diag_unit = open_file(reg_diagnostics_file, action = 'write')
   endif

endif
!---------------------------------------------------------
!_____________________________________________________________________

if(select_regression == 1) then
! Get regression directly from sampling theory
! If only one group, don't know what else to do

   if(num_groups == 1) then
      comp_reg_factor = 1.0_r8
   else

      sum_reg_reg = 0.0_r8
      sum_reg2 = sum(regress * regress)
      do i = 1, num_groups
         do j = i + 1, num_groups
            sum_reg_reg = sum_reg_reg + regress(i) * regress(j)
         end do                                               
      end do
      if (sum_reg2 /= 0.0_r8) then
         comp_reg_factor = 2.0_r8 * sum_reg_reg / (sum_reg2 * (num_groups - 1))
      else
         comp_reg_factor = 0.0_r8
      endif

      if(comp_reg_factor < 0.0_r8) comp_reg_factor = 0.0_r8
      ! Write out diagnostic information

      if(save_reg_diagnostics) then
! DATA REDUCTION FOR WORKSHOP PURPOSES
       
         if(obs_index <= 4 .and. state_index > 0) then

         call get_time(obs_time, secs, days)
         write(diag_unit, 22) days, secs, obs_index, state_index, comp_reg_factor
         22 format(4(i7, 1x), e10.4)
         endif
      endif

   endif
!___________________________________________________________________


else if(select_regression == 2) then
! Table lookup version for time mean, temporary implementation
! This only works for a model with a time invariant observation set

   if(first_call) then
      first_call = .false.
! Read in the regression statistics file
      iunit = get_unit()
      open(unit = iunit, file = input_reg_file)
      read(iunit, *) num_obs, model_size
      allocate(time_mean_reg(num_obs, model_size))
      do j = 1, num_obs
         do i = 1, model_size
            read(iunit, *) jj, ii, time_mean_reg(j, i)
         end do
      end do
      close(iunit)
   endif

   comp_reg_factor = time_mean_reg(obs_index, state_index)

   if(comp_reg_factor < 0.0_r8) comp_reg_factor = 0.0_r8
!_____________________________________________________________________


else if(select_regression == 3) then

   if(first_call) then
      first_call = .false.
      iunit = get_unit()
      open(unit = iunit, file = 'obs_state_reg_file')
      allocate(obs_state_reg(obs_state_max))
      do i = 1, obs_state_max
         read(iunit, 11) obs_state_reg(i)
         close(unit = iunit)
11    format(f5.3)
      end do 
   end if

   comp_reg_factor = obs_state_reg(obs_state_ind)
!_____________________________________________________________________


else
   call error_handler(E_ERR,'comp_reg_factor', &
      'Illegal value for namelist parameter select_regression',source, revision, revdate)
endif

end function comp_reg_factor

!read state subroutine for kr_externs_in_reg_factor_mod 
SUBROUTINE kr_externs_in_reg_factor_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) namelist_initialized 
    READ (UNIT = kgen_unit) select_regression 
    READ (UNIT = kgen_unit) input_reg_file 
    READ (UNIT = kgen_unit) reg_diagnostics_file 
    READ (UNIT = kgen_unit) save_reg_diagnostics 
    READ (UNIT = kgen_unit) first_call 
    READ (UNIT = kgen_unit) diag_unit 
    READ (UNIT = kgen_unit) num_obs 
    READ (UNIT = kgen_unit) model_size 
    CALL kr_reg_factor_mod_real__r8_dim2(time_mean_reg, kgen_unit, "time_mean_reg", .FALSE.) 
    CALL kr_reg_factor_mod_real__r8_dim1(obs_state_reg, kgen_unit, "obs_state_reg", .FALSE.) 
END SUBROUTINE kr_externs_in_reg_factor_mod 
  
!read state subroutine for kr_externs_out_reg_factor_mod 
SUBROUTINE kr_externs_out_reg_factor_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_namelist_initialized 
    READ (UNIT = kgen_unit) kgenref_first_call 
    READ (UNIT = kgen_unit) kgenref_diag_unit 
END SUBROUTINE kr_externs_out_reg_factor_mod 
  
!verify state subroutine for kv_externs_reg_factor_mod 
SUBROUTINE kv_externs_reg_factor_mod(check_status) 
    TYPE(check_t), INTENT(INOUT) :: check_status 
      
    CALL kv_reg_factor_mod_logical__("first_call", check_status, first_call, kgenref_first_call) 
    CALL kv_reg_factor_mod_integer__("diag_unit", check_status, diag_unit, kgenref_diag_unit) 
END SUBROUTINE kv_externs_reg_factor_mod 
  
!read state subroutine for kr_reg_factor_mod_real__r8_dim2 
SUBROUTINE kr_reg_factor_mod_real__r8_dim2(var, kgen_unit, printname, printvar) 
    REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
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
END SUBROUTINE kr_reg_factor_mod_real__r8_dim2 
  
!read state subroutine for kr_reg_factor_mod_real__r8_dim1 
SUBROUTINE kr_reg_factor_mod_real__r8_dim1(var, kgen_unit, printname, printvar) 
    REAL(KIND=r8), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: var 
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
END SUBROUTINE kr_reg_factor_mod_real__r8_dim1 
  
!verify state subroutine for kv_reg_factor_mod_logical__ 
RECURSIVE SUBROUTINE kv_reg_factor_mod_logical__(varname, check_status, var, kgenref_var) 
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
      
END SUBROUTINE kv_reg_factor_mod_logical__ 
  
!verify state subroutine for kv_reg_factor_mod_integer__ 
RECURSIVE SUBROUTINE kv_reg_factor_mod_integer__(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    INTEGER, INTENT(IN) :: var, kgenref_var 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    integer :: diff 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    IF (var == kgenref_var) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff = ABS(var - kgenref_var) 
        IF (diff <= kgen_tolerance) THEN 
            check_status%numInTol = check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            check_status%numOutTol = check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 0) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "Difference is ", 0 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 0) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "Difference is ", diff 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "Difference is ", diff 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
END SUBROUTINE kv_reg_factor_mod_integer__ 
  
end module reg_factor_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/reg_factor_mod.f90 $
! $Id: reg_factor_mod.f90 11289 2017-03-10 21:56:06Z hendric@ucar.edu $
! $Revision: 11289 $
! $Date: 2017-03-10 14:56:06 -0700 (Fri, 10 Mar 2017) $
