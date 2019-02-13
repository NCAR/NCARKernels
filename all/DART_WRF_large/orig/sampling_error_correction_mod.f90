!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:29 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: sampling_error_correction_mod.f90 11626 2017-05-11 17:27:50Z nancy@ucar.edu $
!> Correct covariances for fixed ensemble sizes.
!> Ref: Anderson, J., 2012: 
!> Localization and Sampling Error Correction in Ensemble Kalman Filter Data Assimilation.
!> Mon. Wea. Rev., 140, 2359-2371, doi: 10.1175/MWR-D-11-00013.1. 
!> query the needed table sizes, and read in the values for any
!> given ensemble size.  the two arrays of values returned are
!> the true_correl_mean and alpha. 


!


module sampling_error_correction_mod

    USE types_mod, ONLY: r8 
    USE utilities_mod, ONLY: error_handler, e_err, nc_check 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 

    PUBLIC read_sampling_error_correction 
! version controlled file description for error handling, do not edit


character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 11626 $"
character(len=128), parameter :: revdate  = "$Date: 2017-05-11 11:27:50 -0600 (Thu, 11 May 2017) $"
! Using hardcoded filename for ease of scripting.
! and for now, say where the default location in the dart distribution tree is
! since it's so obscure.

character(len=128) :: input_filename = 'sampling_error_correction_table.nc'
character(len=128) :: default_path = ' "assimilation_code/programs/gen_sampling_err_table/work"'
! module globals - nentries is the number of values per ensemble size,
! nens is how many different ensemble sizes this file contains.


logical :: module_initialized = .false.
integer :: nentries = -1
integer :: nens = -1

character(len=512) :: msgstring, msgstring1
PUBLIC kr_externs_in_sampling_error_correction_mod 
PUBLIC kr_externs_out_sampling_error_correction_mod 
PUBLIC kv_externs_sampling_error_correction_mod 
LOGICAL :: kgenref_module_initialized = .false. 
CHARACTER(LEN=512) :: kgenref_msgstring, kgenref_msgstring1 

contains
!----------------------------------------------------------------
!>


subroutine init_sampling_error_correction()

integer :: ncid

if (module_initialized) return

ncid = open_input_file(input_filename)

call read_input_info(ncid, nentries, nens)

call close_input_file(ncid, input_filename)

module_initialized = .true.

end subroutine init_sampling_error_correction
!----------------------------------------------------------------
!>


!----------------------------------------------------------------
!>


subroutine read_sampling_error_correction(requested_ens_size, true_correl_mean, alpha)

integer,  intent(in) :: requested_ens_size
real(r8), intent(out) :: true_correl_mean(:), alpha(:)

integer :: ncid, indx

if (.not. module_initialized) call init_sampling_error_correction()

ncid = open_input_file(input_filename)

indx = lookup_ens_index(ncid, nens, requested_ens_size)

if (indx < 0) then
   write(msgstring, *) 'file "'//trim(input_filename)//'" does not contain a entry for ensemble size ', &
                        requested_ens_size
   write(msgstring1, *) 'You can add one to the existing file with the "gen_sampling_err_table" program'
   call error_handler(E_ERR, 'read_sampling_error_correction:', 'unsupported ensemble size requested', &
                      source, revision, revdate, text2=msgstring, text3=msgstring1)
endif

if (size(true_correl_mean(:)) /= nentries .or. &
    size(alpha(:)) /= nentries) then
   write(msgstring, *) 'one or both arrays "true_correl_mean" and "alpha" are not allocated correctly'
   write(msgstring1, *) 'they must be size ', nentries, ' but are ', size(true_correl_mean), ' and ', size(alpha)
   call error_handler(E_ERR, 'read_sampling_error_correction:', 'error in output array size', &
                      source, revision, revdate, text2=msgstring, text3=msgstring1)
endif

call read_input_file(ncid, indx, true_correl_mean, alpha)

call close_input_file(ncid, input_filename)

end subroutine read_sampling_error_correction
!----------------------------------------------------------------
! support routines below
!----------------------------------------------------------------


function open_input_file(input_filename)

character(len=*), intent(in) :: input_filename
integer :: open_input_file

integer :: rc, ncid

   msgstring  = 'File "'//trim(input_filename)//'" not found in the current directory.'
   msgstring1 = 'This file can be copied from this location in the DART distribution: '
   call error_handler(E_ERR, 'read_sampling_error_correction:', msgstring, &
                      source, revision, revdate, text2=msgstring1, text3=default_path)

open_input_file = ncid

end function open_input_file
!----------------------------------------------------------------
!> get the 2 dims - number of entries for any given ensemble size,
!> and number of supported ensemble sizes.


subroutine read_input_info(ncid, nbins, nens)

integer, intent(in)  :: ncid
integer, intent(out) :: nbins
integer, intent(out) :: nens

call get_sec_dim(ncid, 'bins', nbins)
call get_sec_dim(ncid, 'ens_sizes',  nens)

end subroutine read_input_info
!----------------------------------------------------------------
!>


function lookup_ens_index(ncid, num_ens, requested_ens_size)

integer, intent(in) :: ncid
integer, intent(in) :: num_ens
integer, intent(in) :: requested_ens_size

integer :: lookup_ens_index
integer :: i, indx, id
integer, allocatable :: index_array(:)

allocate(index_array(num_ens))

call query_sec_data(ncid, 'ens_sizes', id)
call read_sec_data_int(ncid, 1, 'ens_sizes', id, index_array)

indx = -1
do i=1, num_ens
   if (index_array(i) == requested_ens_size) then
      indx = i
      exit
   endif
enddo

lookup_ens_index = indx
deallocate(index_array)

end function lookup_ens_index
!----------------------------------------------------------------
!> read the true correlation and correction arrays 


subroutine read_input_file(ncid, col, a1, a2)

integer,          intent(in)  :: ncid
integer,          intent(in)  :: col
real(r8),         intent(out) :: a1(:)
real(r8),         intent(out) :: a2(:)

integer :: id1, id2
character(len=64) :: c1, c2

c1 = 'true_corr_mean'
c2 = 'alpha'

call query_sec_data(ncid, c1, id1)
call query_sec_data(ncid, c2, id2)

call read_sec_data_real(ncid, col, c1, id1, a1)
call read_sec_data_real(ncid, col, c2, id2, a2)

end subroutine read_input_file
!----------------------------------------------------------------
!>


subroutine close_input_file(ncid, input_filename)

integer,          intent(in) :: ncid
character(len=*), intent(in) :: input_filename

integer :: rc

call nc_check(rc, 'close_input_file', 'closing "'//trim(input_filename)//'"')

end subroutine close_input_file
!----------------------------------------------------------------
!> retrieve dimension for sampling error correction


subroutine get_sec_dim(ncid, c1, n1)

integer,          intent(in)  :: ncid
character(len=*), intent(in)  :: c1
integer,          intent(out) :: n1

integer :: rc, id1

call nc_check(rc, 'get_sec_dim', 'inq_dimid "'//trim(c1)//'"')

call nc_check(rc, 'get_sec_dim', 'inquire_dimension "'//trim(c1)//'"')

end subroutine get_sec_dim
!----------------------------------------------------------------
!> given a variable name, return variable id


subroutine query_sec_data(ncid, c1, id1)

integer,          intent(in)  :: ncid
character(len=*), intent(in)  :: c1
integer,          intent(out) :: id1

integer :: rc

call nc_check(rc, 'query_sec_data', 'querying variable "'//trim(c1)//'"')

end subroutine query_sec_data
!----------------------------------------------------------------
!>


subroutine read_sec_data_int(ncid, col, c1, id1, a1)

integer,          intent(in)  :: ncid
integer,          intent(in)  :: col
character(len=*), intent(in)  :: c1
integer,          intent(in)  :: id1
integer,          intent(out) :: a1(:)

integer :: rc

call nc_check(rc, 'read_sec_data_int', 'reading variable "'//trim(c1)//'"')

end subroutine read_sec_data_int
!----------------------------------------------------------------
!>


subroutine read_sec_data_real(ncid, col, c1, id1, a1) 

integer,          intent(in)  :: ncid
integer,          intent(in)  :: col
character(len=*), intent(in)  :: c1
integer,          intent(in)  :: id1
real(r8),         intent(out) :: a1(:)

integer :: rc

call nc_check(rc, 'read_sec_data_real', 'reading variable "'//trim(c1)//'"')

end subroutine read_sec_data_real
!----------------------------------------------------------------


!read state subroutine for kr_externs_in_sampling_error_correction_mod 
SUBROUTINE kr_externs_in_sampling_error_correction_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) input_filename 
    READ (UNIT = kgen_unit) default_path 
    READ (UNIT = kgen_unit) module_initialized 
    READ (UNIT = kgen_unit) nentries 
    READ (UNIT = kgen_unit) nens 
    READ (UNIT = kgen_unit) msgstring 
    READ (UNIT = kgen_unit) msgstring1 
END SUBROUTINE kr_externs_in_sampling_error_correction_mod 
  
!read state subroutine for kr_externs_out_sampling_error_correction_mod 
SUBROUTINE kr_externs_out_sampling_error_correction_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_module_initialized 
    READ (UNIT = kgen_unit) kgenref_msgstring 
    READ (UNIT = kgen_unit) kgenref_msgstring1 
END SUBROUTINE kr_externs_out_sampling_error_correction_mod 
  
!verify state subroutine for kv_externs_sampling_error_correction_mod 
SUBROUTINE kv_externs_sampling_error_correction_mod(check_status) 
    TYPE(check_t), INTENT(INOUT) :: check_status 
      
    CALL kv_kgen_sampling_error_correction_mod_subp1("msgstring", check_status, msgstring, kgenref_msgstring) 
    CALL kv_kgen_sampling_error_correction_mod_subp1("msgstring1", check_status, msgstring1, kgenref_msgstring1) 
END SUBROUTINE kv_externs_sampling_error_correction_mod 
  
!verify state subroutine for kv_kgen_sampling_error_correction_mod_subp1 
RECURSIVE SUBROUTINE kv_kgen_sampling_error_correction_mod_subp1(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    CHARACTER(LEN=512), INTENT(IN) :: var, kgenref_var 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    character(LEN=512) :: diff 
      
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
      
END SUBROUTINE kv_kgen_sampling_error_correction_mod_subp1 
  
end module sampling_error_correction_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/assimilation/sampling_error_correction_mod.f90 $
! $Id: sampling_error_correction_mod.f90 11626 2017-05-11 17:27:50Z nancy@ucar.edu $
! $Revision: 11626 $
! $Date: 2017-05-11 11:27:50 -0600 (Thu, 11 May 2017) $

