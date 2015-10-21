
! KGEN-generated Fortran source file
!
! Filename    : stat_file_module.F90
! Generated at: 2015-10-20 14:27:08
! KGEN version: 0.5.3



    MODULE stat_file_module
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
! Description:
!   Contains two derived types for describing the contents and location of
!   either NetCDF or GrADS files.
!-------------------------------------------------------------------------------
        USE clubb_precision, ONLY: core_rknd
        USE clubb_precision, ONLY: time_precision
        USE clubb_precision, ONLY: stat_rknd
! Variable
        IMPLICIT NONE
        PUBLIC stat_file, variable
! These are used in a 2D or 3D host model to output multiple columns
! Set clubb_i and clubb_j according to the column within the host model;
! The indices must not exceed nlon (for i) or nlat (for j).
        INTEGER, save, public :: clubb_i = 1
        INTEGER, save, public :: clubb_j = 1
!$omp threadprivate(clubb_i, clubb_j)
        PRIVATE ! Default scope
! Structure to hold the description of a variable
        TYPE variable
! Pointer to the array
            REAL(KIND=stat_rknd), dimension(:,:,:), pointer :: ptr
            CHARACTER(LEN=30) :: name ! Variable name
            CHARACTER(LEN=100) :: description ! Variable description
            CHARACTER(LEN=20) :: units ! Variable units
            INTEGER :: indx ! NetCDF module Id for var / GrADS index
            LOGICAL :: l_silhs ! If true, we sample this variable once for each SILHS
! sample point per timestep, rather than just once per
! timestep.
        END TYPE variable
! Structure to hold the description of a NetCDF output file
! This makes the new code as compatible as possible with the
! GrADS output code
        TYPE stat_file
! File information
            CHARACTER(LEN=200) :: fname, fdir
! File name without suffix
! Path where fname resides
            INTEGER :: iounit ! This number is used internally by the
! NetCDF module to track the data set, or by
! GrADS to track the actual file unit.
            INTEGER :: nrecord, ntimes
! Number of records written
! Number of times written
            LOGICAL :: l_defined, l_byte_swapped
! Whether nf90_enddef() has been called
! Is this a file in the opposite byte ordering?
! NetCDF datafile dimensions indices
            INTEGER :: latdimid, longdimid, altdimid, timedimid, latvarid, longvarid, altvarid, timevarid
! Grid information
            INTEGER :: ia, iz ! Vertical extent
            INTEGER :: nlat, nlon ! The number of points in the X and Y
            REAL(KIND=core_rknd), dimension(:), pointer :: z
! Height of vertical levels [m]
! Time information
            INTEGER :: day, month, year ! Date of starting time
            REAL(KIND=core_rknd), dimension(:), pointer :: rlat, rlon
! Latitude                   [Degrees N]
! Longitude                  [Degrees E]
            REAL(KIND=core_rknd) :: dtwrite
! Interval between output    [Seconds]
            REAL(KIND=time_precision) :: time
! Start time                 [Seconds]
! Statistical Variables
            INTEGER :: nvar ! Number of variables for this file
            TYPE(variable), dimension(:), pointer :: var
! List and variable description
        END TYPE stat_file
        PUBLIC kgen_read_externs_stat_file_module

    ! read interface
    PUBLIC kgen_read
    INTERFACE kgen_read
        MODULE PROCEDURE kgen_read_variable
        MODULE PROCEDURE kgen_read_stat_file
    END INTERFACE kgen_read

    PUBLIC kgen_verify
    INTERFACE kgen_verify
        MODULE PROCEDURE kgen_verify_variable
        MODULE PROCEDURE kgen_verify_stat_file
    END INTERFACE kgen_verify

    CONTAINS

    ! write subroutines
        SUBROUTINE kgen_read_real_stat_rknd_dim3_ptr(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            real(KIND=stat_rknd), INTENT(OUT), POINTER, DIMENSION(:,:,:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1,idx2,idx3
            INTEGER, DIMENSION(2,3) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                READ(UNIT = kgen_unit) kgen_bound(1, 2)
                READ(UNIT = kgen_unit) kgen_bound(2, 2)
                READ(UNIT = kgen_unit) kgen_bound(1, 3)
                READ(UNIT = kgen_unit) kgen_bound(2, 3)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1))
                READ(UNIT = kgen_unit) var
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " **", var
                END IF
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_real_stat_rknd_dim3_ptr

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

        SUBROUTINE kgen_read_variable_dim1_ptr(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(variable), INTENT(OUT), POINTER, DIMENSION(:) :: var
            LOGICAL :: is_true
            INTEGER :: idx1
            INTEGER, DIMENSION(2,1) :: kgen_bound

            READ(UNIT = kgen_unit) is_true

            IF ( is_true ) THEN
                READ(UNIT = kgen_unit) kgen_bound(1, 1)
                READ(UNIT = kgen_unit) kgen_bound(2, 1)
                ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
                DO idx1=kgen_bound(1,1), kgen_bound(2, 1)
                IF ( PRESENT(printvar) ) THEN
                        CALL kgen_read_variable(var(idx1), kgen_unit, printvar=printvar)
                ELSE
                        CALL kgen_read_variable(var(idx1), kgen_unit)
                END IF
                END DO
            ELSE
                IF ( PRESENT(printvar) ) THEN
                    PRINT *, "** KGEN DEBUG: " // printvar // " ** is NOT present"
                END IF
            END IF
        END SUBROUTINE kgen_read_variable_dim1_ptr


    ! module extern variables

    SUBROUTINE kgen_read_externs_stat_file_module(kgen_unit)
        INTEGER, INTENT(IN) :: kgen_unit
        READ(UNIT=kgen_unit) clubb_i
        READ(UNIT=kgen_unit) clubb_j
    END SUBROUTINE kgen_read_externs_stat_file_module

    SUBROUTINE kgen_read_variable(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(variable), INTENT(out) :: var
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_real_stat_rknd_dim3_ptr(var%ptr, kgen_unit, printvar=printvar//"%ptr")
        ELSE
            CALL kgen_read_real_stat_rknd_dim3_ptr(var%ptr, kgen_unit)
        END IF
        READ(UNIT=kgen_unit) var%name
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%name **", var%name
        END IF
        READ(UNIT=kgen_unit) var%description
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%description **", var%description
        END IF
        READ(UNIT=kgen_unit) var%units
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%units **", var%units
        END IF
        READ(UNIT=kgen_unit) var%indx
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%indx **", var%indx
        END IF
        READ(UNIT=kgen_unit) var%l_silhs
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%l_silhs **", var%l_silhs
        END IF
    END SUBROUTINE
    SUBROUTINE kgen_read_stat_file(var, kgen_unit, printvar)
        INTEGER, INTENT(IN) :: kgen_unit
        CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
        TYPE(stat_file), INTENT(out) :: var
        READ(UNIT=kgen_unit) var%fname
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%fname **", var%fname
        END IF
        READ(UNIT=kgen_unit) var%fdir
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%fdir **", var%fdir
        END IF
        READ(UNIT=kgen_unit) var%iounit
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%iounit **", var%iounit
        END IF
        READ(UNIT=kgen_unit) var%nrecord
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%nrecord **", var%nrecord
        END IF
        READ(UNIT=kgen_unit) var%ntimes
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%ntimes **", var%ntimes
        END IF
        READ(UNIT=kgen_unit) var%l_defined
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%l_defined **", var%l_defined
        END IF
        READ(UNIT=kgen_unit) var%l_byte_swapped
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%l_byte_swapped **", var%l_byte_swapped
        END IF
        READ(UNIT=kgen_unit) var%latdimid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%latdimid **", var%latdimid
        END IF
        READ(UNIT=kgen_unit) var%longdimid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%longdimid **", var%longdimid
        END IF
        READ(UNIT=kgen_unit) var%altdimid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%altdimid **", var%altdimid
        END IF
        READ(UNIT=kgen_unit) var%timedimid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%timedimid **", var%timedimid
        END IF
        READ(UNIT=kgen_unit) var%latvarid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%latvarid **", var%latvarid
        END IF
        READ(UNIT=kgen_unit) var%longvarid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%longvarid **", var%longvarid
        END IF
        READ(UNIT=kgen_unit) var%altvarid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%altvarid **", var%altvarid
        END IF
        READ(UNIT=kgen_unit) var%timevarid
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%timevarid **", var%timevarid
        END IF
        READ(UNIT=kgen_unit) var%ia
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%ia **", var%ia
        END IF
        READ(UNIT=kgen_unit) var%iz
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%iz **", var%iz
        END IF
        READ(UNIT=kgen_unit) var%nlat
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%nlat **", var%nlat
        END IF
        READ(UNIT=kgen_unit) var%nlon
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%nlon **", var%nlon
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_real_core_rknd_dim1_ptr(var%z, kgen_unit, printvar=printvar//"%z")
        ELSE
            CALL kgen_read_real_core_rknd_dim1_ptr(var%z, kgen_unit)
        END IF
        READ(UNIT=kgen_unit) var%day
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%day **", var%day
        END IF
        READ(UNIT=kgen_unit) var%month
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%month **", var%month
        END IF
        READ(UNIT=kgen_unit) var%year
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%year **", var%year
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_real_core_rknd_dim1_ptr(var%rlat, kgen_unit, printvar=printvar//"%rlat")
        ELSE
            CALL kgen_read_real_core_rknd_dim1_ptr(var%rlat, kgen_unit)
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_real_core_rknd_dim1_ptr(var%rlon, kgen_unit, printvar=printvar//"%rlon")
        ELSE
            CALL kgen_read_real_core_rknd_dim1_ptr(var%rlon, kgen_unit)
        END IF
        READ(UNIT=kgen_unit) var%dtwrite
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%dtwrite **", var%dtwrite
        END IF
        READ(UNIT=kgen_unit) var%time
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%time **", var%time
        END IF
        READ(UNIT=kgen_unit) var%nvar
        IF ( PRESENT(printvar) ) THEN
            print *, "** KGEN DEBUG: " // printvar // "%nvar **", var%nvar
        END IF
        IF ( PRESENT(printvar) ) THEN
            CALL kgen_read_variable_dim1_ptr(var%var, kgen_unit, printvar=printvar//"%var")
        ELSE
            CALL kgen_read_variable_dim1_ptr(var%var, kgen_unit)
        END IF
    END SUBROUTINE
    RECURSIVE SUBROUTINE kgen_verify_variable(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(variable), INTENT(IN) :: var, ref_var

        check_status%numTotal = check_status%numTotal + 1
        CALL kgen_init_check(dtype_check_status)
        CALL kgen_verify_real_stat_rknd_dim3_ptr("ptr", dtype_check_status, var%ptr, ref_var%ptr)
        CALL kgen_verify_character("name", dtype_check_status, var%name, ref_var%name)
        CALL kgen_verify_character("description", dtype_check_status, var%description, ref_var%description)
        CALL kgen_verify_character("units", dtype_check_status, var%units, ref_var%units)
        CALL kgen_verify_integer("indx", dtype_check_status, var%indx, ref_var%indx)
        CALL kgen_verify_logical("l_silhs", dtype_check_status, var%l_silhs, ref_var%l_silhs)
        IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
            check_status%numIdentical = check_status%numIdentical + 1
        ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
            check_status%numFatal = check_status%numFatal + 1
        ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
            check_status%numWarning = check_status%numWarning + 1
        END IF
    END SUBROUTINE
    RECURSIVE SUBROUTINE kgen_verify_stat_file(varname, check_status, var, ref_var)
        CHARACTER(*), INTENT(IN) :: varname
        TYPE(check_t), INTENT(INOUT) :: check_status
        TYPE(check_t) :: dtype_check_status
        TYPE(stat_file), INTENT(IN) :: var, ref_var

        check_status%numTotal = check_status%numTotal + 1
        CALL kgen_init_check(dtype_check_status)
        CALL kgen_verify_character("fname", dtype_check_status, var%fname, ref_var%fname)
        CALL kgen_verify_character("fdir", dtype_check_status, var%fdir, ref_var%fdir)
        CALL kgen_verify_integer("iounit", dtype_check_status, var%iounit, ref_var%iounit)
        CALL kgen_verify_integer("nrecord", dtype_check_status, var%nrecord, ref_var%nrecord)
        CALL kgen_verify_integer("ntimes", dtype_check_status, var%ntimes, ref_var%ntimes)
        CALL kgen_verify_logical("l_defined", dtype_check_status, var%l_defined, ref_var%l_defined)
        CALL kgen_verify_logical("l_byte_swapped", dtype_check_status, var%l_byte_swapped, ref_var%l_byte_swapped)
        CALL kgen_verify_integer("latdimid", dtype_check_status, var%latdimid, ref_var%latdimid)
        CALL kgen_verify_integer("longdimid", dtype_check_status, var%longdimid, ref_var%longdimid)
        CALL kgen_verify_integer("altdimid", dtype_check_status, var%altdimid, ref_var%altdimid)
        CALL kgen_verify_integer("timedimid", dtype_check_status, var%timedimid, ref_var%timedimid)
        CALL kgen_verify_integer("latvarid", dtype_check_status, var%latvarid, ref_var%latvarid)
        CALL kgen_verify_integer("longvarid", dtype_check_status, var%longvarid, ref_var%longvarid)
        CALL kgen_verify_integer("altvarid", dtype_check_status, var%altvarid, ref_var%altvarid)
        CALL kgen_verify_integer("timevarid", dtype_check_status, var%timevarid, ref_var%timevarid)
        CALL kgen_verify_integer("ia", dtype_check_status, var%ia, ref_var%ia)
        CALL kgen_verify_integer("iz", dtype_check_status, var%iz, ref_var%iz)
        CALL kgen_verify_integer("nlat", dtype_check_status, var%nlat, ref_var%nlat)
        CALL kgen_verify_integer("nlon", dtype_check_status, var%nlon, ref_var%nlon)
        CALL kgen_verify_real_core_rknd_dim1_ptr("z", dtype_check_status, var%z, ref_var%z)
        CALL kgen_verify_integer("day", dtype_check_status, var%day, ref_var%day)
        CALL kgen_verify_integer("month", dtype_check_status, var%month, ref_var%month)
        CALL kgen_verify_integer("year", dtype_check_status, var%year, ref_var%year)
        CALL kgen_verify_real_core_rknd_dim1_ptr("rlat", dtype_check_status, var%rlat, ref_var%rlat)
        CALL kgen_verify_real_core_rknd_dim1_ptr("rlon", dtype_check_status, var%rlon, ref_var%rlon)
        CALL kgen_verify_real_core_rknd("dtwrite", dtype_check_status, var%dtwrite, ref_var%dtwrite)
        CALL kgen_verify_real_time_precision("time", dtype_check_status, var%time, ref_var%time)
        CALL kgen_verify_integer("nvar", dtype_check_status, var%nvar, ref_var%nvar)
        CALL kgen_verify_variable_dim1_ptr("var", dtype_check_status, var%var, ref_var%var)
        IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
            check_status%numIdentical = check_status%numIdentical + 1
        ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
            check_status%numFatal = check_status%numFatal + 1
        ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
            check_status%numWarning = check_status%numWarning + 1
        END IF
    END SUBROUTINE
        SUBROUTINE kgen_verify_real_stat_rknd_dim3_ptr( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(KIND=stat_rknd), intent(in), DIMENSION(:,:,:), POINTER :: var, ref_var
            real(KIND=stat_rknd) :: nrmsdiff, rmsdiff
            real(KIND=stat_rknd), allocatable, DIMENSION(:,:,:) :: temp, temp2
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
                allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
                allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3)))
            
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
        END SUBROUTINE kgen_verify_real_stat_rknd_dim3_ptr

        SUBROUTINE kgen_verify_character( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            character(LEN=*), intent(in) :: var, ref_var
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
        END SUBROUTINE kgen_verify_character

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

        SUBROUTINE kgen_verify_logical( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            logical, intent(in) :: var, ref_var
            check_status%numTotal = check_status%numTotal + 1
            IF ( var .EQV. ref_var ) THEN
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
        END SUBROUTINE kgen_verify_logical

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

        SUBROUTINE kgen_verify_real_core_rknd( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(KIND=core_rknd), intent(in) :: var, ref_var
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
        END SUBROUTINE kgen_verify_real_core_rknd

        SUBROUTINE kgen_verify_real_time_precision( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            real(KIND=time_precision), intent(in) :: var, ref_var
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
        END SUBROUTINE kgen_verify_real_time_precision

        RECURSIVE SUBROUTINE kgen_verify_variable_dim1_ptr( varname, check_status, var, ref_var)
            character(*), intent(in) :: varname
            type(check_t), intent(inout) :: check_status
            type(check_t) :: dtype_check_status
            TYPE(variable), intent(in), DIMENSION(:), POINTER :: var, ref_var
            integer :: idx1
            IF ( ASSOCIATED(var) ) THEN
            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            DO idx1=LBOUND(var,1), UBOUND(var,1)
                CALL kgen_verify_variable("variable", dtype_check_status, var(idx1), ref_var(idx1))
            END DO
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
            END IF
        END SUBROUTINE kgen_verify_variable_dim1_ptr

    END MODULE stat_file_module
