
! KGEN-generated Fortran source file
!
! Filename    : stats_type.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE stats_type
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
    USE stat_file_module, ONLY : kgen_read_mod14 => kgen_read
    USE stat_file_module, ONLY : kgen_verify_mod14 => kgen_verify
! Description:
!   Contains derived data type 'stats'.
!   Used for storing output statistics to disk.
!-----------------------------------------------------------------------
        USE stat_file_module, ONLY: stat_file
! Type
        USE clubb_precision, ONLY: core_rknd
        USE clubb_precision, ONLY: stat_rknd
        USE clubb_precision, ONLY: stat_nknd
! Variable(s)
        IMPLICIT NONE
        PRIVATE ! Set Default Scope
        PUBLIC stats
! Derived data types to store GrADS/netCDF statistics
        TYPE stats
! Number of fields to sample
            INTEGER :: num_output_fields ! Number of variables being output to disk (e.g.
! cloud_frac, rain rate, etc.)
            INTEGER :: ii, jj, kk
! Horizontal extent of the variables (Usually 1 for the single-column model)
! Horizontal extent of the variables (Usually 1 for the single-column model)
! Vertical extent of the variables (Usually gr%nz from grid_class)
! Vertical levels
            REAL(KIND=core_rknd), pointer, dimension(:) :: z ! altitude [m]
! Array to store sampled fields
            REAL(KIND=stat_rknd), pointer, dimension(:,:,:,:) :: accum_field_values
! The variable accum_field_values contains the cumulative sums
! of accum_num_samples sample values of each
! of the num_output_fields (e.g. the sum of the sampled rain rate values)
            INTEGER(KIND=stat_nknd), pointer, dimension(:,:,:,:) :: accum_num_samples
! accum_num_samples is the number of samples for each of the num_output_fields fields
! and each of the kk vertical levels
! Tracks if a field is in the process of an update
            LOGICAL, pointer, dimension(:,:,:,:) :: l_in_update
! Data for GrADS / netCDF output
            TYPE(stat_file) :: file
        END TYPE stats

    ! read interface
    PUBLIC kgen_read
    INTERFACE kgen_read
        MODULE PROCEDURE kgen_read_stats
    END INTERFACE kgen_read

    PUBLIC kgen_verify
    INTERFACE kgen_verify
        MODULE PROCEDURE kgen_verify_stats
    END INTERFACE kgen_verify

    CONTAINS

    ! write subroutines
        SUBROUTINE kgen_read_integer_stat_nknd_dim4_ptr(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            integer(KIND=stat_nknd), INTENT(OUT), POINTER, DIMENSION(:,:,:,:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1,idx2,idx3,idx4
            INTEGER, DIMENSION(2,4) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                READ(UNIT = kgen_unit) kgen_bound(1, 2)
                READ(UNIT = kgen_unit) kgen_bound(2, 2)
                READ(UNIT = kgen_unit) kgen_bound(1, 3)
                READ(UNIT = kgen_unit) kgen_bound(2, 3)
                READ(UNIT = kgen_unit) kgen_bound(1, 4)
                READ(UNIT = kgen_unit) kgen_bound(2, 4)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_integer_stat_nknd_dim4_ptr

        SUBROUTINE kgen_read_real_stat_rknd_dim4_ptr(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            real(KIND=stat_rknd), INTENT(OUT), POINTER, DIMENSION(:,:,:,:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1,idx2,idx3,idx4
            INTEGER, DIMENSION(2,4) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                READ(UNIT = kgen_unit) kgen_bound(1, 2)
                READ(UNIT = kgen_unit) kgen_bound(2, 2)
                READ(UNIT = kgen_unit) kgen_bound(1, 3)
                READ(UNIT = kgen_unit) kgen_bound(2, 3)
                READ(UNIT = kgen_unit) kgen_bound(1, 4)
                READ(UNIT = kgen_unit) kgen_bound(2, 4)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_real_stat_rknd_dim4_ptr

        SUBROUTINE kgen_read_logical_4_dim4_ptr(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            logical(KIND=4), INTENT(OUT), POINTER, DIMENSION(:,:,:,:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1,idx2,idx3,idx4
            INTEGER, DIMENSION(2,4) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                READ(UNIT = kgen_unit) kgen_bound(1, 2)
                READ(UNIT = kgen_unit) kgen_bound(2, 2)
                READ(UNIT = kgen_unit) kgen_bound(1, 3)
                READ(UNIT = kgen_unit) kgen_bound(2, 3)
                READ(UNIT = kgen_unit) kgen_bound(1, 4)
                READ(UNIT = kgen_unit) kgen_bound(2, 4)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_logical_4_dim4_ptr

        SUBROUTINE kgen_read_real_core_rknd_dim1_ptr(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            real(KIND=core_rknd), INTENT(OUT), POINTER, DIMENSION(:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1
            INTEGER, DIMENSION(2,1) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_real_core_rknd_dim1_ptr

    ! No module extern variables
    SUBROUTINE kgen_read_stats(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(stats), INTENT(out) :: var
        READ(UNIT=kgen_unit) var%num_output_fields
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%num_output_fields **", var%num_output_fields
        END IF
        READ(UNIT=kgen_unit) var%ii
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%ii **", var%ii
        END IF
        READ(UNIT=kgen_unit) var%jj
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%jj **", var%jj
        END IF
        READ(UNIT=kgen_unit) var%kk
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%kk **", var%kk
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_real_core_rknd_dim1_ptr(var%z, kgen_unit, printvar=printvar//"%z")
        ELSE
            CALL kgen_read_real_core_rknd_dim1_ptr(var%z, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_real_stat_rknd_dim4_ptr(var%accum_field_values, kgen_unit, printvar=printvar//"%accum_field_values")
        ELSE
            CALL kgen_read_real_stat_rknd_dim4_ptr(var%accum_field_values, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_integer_stat_nknd_dim4_ptr(var%accum_num_samples, kgen_unit, printvar=printvar//"%accum_num_samples")
        ELSE
            CALL kgen_read_integer_stat_nknd_dim4_ptr(var%accum_num_samples, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_logical_4_dim4_ptr(var%l_in_update, kgen_unit, printvar=printvar//"%l_in_update")
        ELSE
            CALL kgen_read_logical_4_dim4_ptr(var%l_in_update, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_mod14(var%file, kgen_unit, printvar=printvar//"%file")
        ELSE
            CALL kgen_read_mod14(var%file, kgen_unit)
        END IF
    END SUBROUTINE
    RECURSIVE SUBROUTINE kgen_verify_stats(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(stats), INTENT(IN) :: var, ref_var

        check_status%numTotal = check_status%numTotal + 1
        CALL kgen_init_check(dtype_check_status)
        CALL kgen_verify_integer("num_output_fields", dtype_check_status, var%num_output_fields, ref_var%num_output_fields)
        CALL kgen_verify_integer("ii", dtype_check_status, var%ii, ref_var%ii)
        CALL kgen_verify_integer("jj", dtype_check_status, var%jj, ref_var%jj)
        CALL kgen_verify_integer("kk", dtype_check_status, var%kk, ref_var%kk)
        CALL kgen_verify_real_core_rknd_dim1_ptr("z", dtype_check_status, var%z, ref_var%z)
        CALL kgen_verify_real_stat_rknd_dim4_ptr("accum_field_values", dtype_check_status, var%accum_field_values, ref_var%accum_field_values)
        CALL kgen_verify_integer_stat_nknd_dim4_ptr("accum_num_samples", dtype_check_status, var%accum_num_samples, ref_var%accum_num_samples)
        CALL kgen_verify_logical_4_dim4_ptr("l_in_update", dtype_check_status, var%l_in_update, ref_var%l_in_update)
        CALL kgen_verify_mod14("file", dtype_check_status, var%file, ref_var%file)
        IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
            check_status%numIdentical = check_status%numIdentical + 1
        ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
            check_status%numFatal = check_status%numFatal + 1
        ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
            check_status%numWarning = check_status%numWarning + 1
        END IF
    END SUBROUTINE
        SUBROUTINE kgen_verify_integer( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            integer, intent(in) :: var, ref_var
            check_status%numTotal = check_status%numTotal + 1
            IF ( var == ref_var ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
                endif
            ELSE
                if(check_status%verboseLevel > 0) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    if(check_status%verboseLevel > 2) then
                        WRITE(*,*) "KERNEL: ", var
                        WRITE(*,*) "REF.  : ", ref_var
                    end if
                end if
                check_status%numFatal = check_status%numFatal + 1
            END IF
        END SUBROUTINE kgen_verify_integer

        SUBROUTINE kgen_verify_real_core_rknd_dim1_ptr( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(KIND=core_rknd), intent(in), DIMENSION(:), POINTER :: var, ref_var
            real(KIND=core_rknd) :: nrmsdiff, rmsdiff
            real(KIND=core_rknd), allocatable, DIMENSION(:) :: temp, temp2
            integer :: n
            IF ( ASSOCIATED(var) ) THEN
            check_status%numTotal = check_status%numTotal + 1
            IF ( ALL( var == ref_var ) ) THEN
            
                check_status%numIdentical = check_status%numIdentical + 1            
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                    !WRITE(*,*) "KERNEL: ", var
                    !WRITE(*,*) "REF.  : ", ref_var
                    IF ( ALL( var == 0 ) ) THEN
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "All values are zero."
                        end if
                    END IF
                end if
            ELSE
                allocate(temp(SIZE(var,dim=1)))
                allocate(temp2(SIZE(var,dim=1)))
            
                n = count(var/=ref_var)
                where(abs(ref_var) > check_status%minvalue)
                    temp  = ((var-ref_var)/ref_var)**2
                    temp2 = (var-ref_var)**2
                elsewhere
                    temp  = (var-ref_var)**2
                    temp2 = temp
                endwhere
                nrmsdiff = sqrt(sum(temp)/real(n))
                rmsdiff = sqrt(sum(temp2)/real(n))
            
                if(check_status%verboseLevel > 0) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                        WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                    endif
                    WRITE(*,*) "RMS of difference is ",rmsdiff
                    WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                end if
            
                if (nrmsdiff > check_status%tolerance) then
                    check_status%numFatal = check_status%numFatal+1
                else
                    check_status%numWarning = check_status%numWarning+1
                endif
            
                deallocate(temp,temp2)
            END IF
            END IF
        END SUBROUTINE kgen_verify_real_core_rknd_dim1_ptr

        SUBROUTINE kgen_verify_real_stat_rknd_dim4_ptr( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(KIND=stat_rknd), intent(in), DIMENSION(:,:,:,:), POINTER :: var, ref_var
            real(KIND=stat_rknd) :: nrmsdiff, rmsdiff
            real(KIND=stat_rknd), allocatable, DIMENSION(:,:,:,:) :: temp, temp2
            integer :: n
            IF ( ASSOCIATED(var) ) THEN
            check_status%numTotal = check_status%numTotal + 1
            IF ( ALL( var == ref_var ) ) THEN
            
                check_status%numIdentical = check_status%numIdentical + 1            
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                    !WRITE(*,*) "KERNEL: ", var
                    !WRITE(*,*) "REF.  : ", ref_var
                    IF ( ALL( var == 0 ) ) THEN
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "All values are zero."
                        end if
                    END IF
                end if
            ELSE
                allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4)))
                allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3),SIZE(var,dim=4)))
            
                n = count(var/=ref_var)
                where(abs(ref_var) > check_status%minvalue)
                    temp  = ((var-ref_var)/ref_var)**2
                    temp2 = (var-ref_var)**2
                elsewhere
                    temp  = (var-ref_var)**2
                    temp2 = temp
                endwhere
                nrmsdiff = sqrt(sum(temp)/real(n))
                rmsdiff = sqrt(sum(temp2)/real(n))
            
                if(check_status%verboseLevel > 0) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                    if(check_status%verboseLevel > 1) then
                        WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
                        WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
                    endif
                    WRITE(*,*) "RMS of difference is ",rmsdiff
                    WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
                end if
            
                if (nrmsdiff > check_status%tolerance) then
                    check_status%numFatal = check_status%numFatal+1
                else
                    check_status%numWarning = check_status%numWarning+1
                endif
            
                deallocate(temp,temp2)
            END IF
            END IF
        END SUBROUTINE kgen_verify_real_stat_rknd_dim4_ptr

        SUBROUTINE kgen_verify_integer_stat_nknd_dim4_ptr( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            integer(KIND=stat_nknd), intent(in), DIMENSION(:,:,:,:), POINTER :: var, ref_var
            IF ( ASSOCIATED(var) ) THEN
            check_status%numTotal = check_status%numTotal + 1
            IF ( ALL( var == ref_var ) ) THEN
            
                check_status%numIdentical = check_status%numIdentical + 1            
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                    !WRITE(*,*) "KERNEL: ", var
                    !WRITE(*,*) "REF.  : ", ref_var
                    IF ( ALL( var == 0 ) ) THEN
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "All values are zero."
                        end if
                    END IF
                end if
            ELSE
                if(check_status%verboseLevel > 0) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                end if
            
                check_status%numFatal = check_status%numFatal+1
            END IF
            END IF
        END SUBROUTINE kgen_verify_integer_stat_nknd_dim4_ptr

        SUBROUTINE kgen_verify_logical_4_dim4_ptr( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            logical, intent(in), DIMENSION(:,:,:,:), POINTER :: var, ref_var
            IF ( ASSOCIATED(var) ) THEN
            check_status%numTotal = check_status%numTotal + 1
            IF ( ALL( var .EQV. ref_var ) ) THEN
            
                check_status%numIdentical = check_status%numIdentical + 1            
                if(check_status%verboseLevel > 1) then
                    WRITE(*,*)
                    WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
                    !WRITE(*,*) "KERNEL: ", var
                    !WRITE(*,*) "REF.  : ", ref_var
                    IF ( ALL( var == 0 ) ) THEN
                        if(check_status%verboseLevel > 2) then
                            WRITE(*,*) "All values are zero."
                        end if
                    END IF
                end if
            ELSE
                if(check_status%verboseLevel > 0) then
                    WRITE(*,*)
                    WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
                    WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
                end if
            
                check_status%numFatal = check_status%numFatal+1
            END IF
            END IF
        END SUBROUTINE kgen_verify_logical_4_dim4_ptr

    END MODULE stats_type
