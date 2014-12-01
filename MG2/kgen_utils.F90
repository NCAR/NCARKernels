    module kgen_utils

    use shr_kind_mod,   only: r8=>shr_kind_r8

    implicit none
    private
    save

    interface read_var
        module procedure read_var_real_r8_dim1
        module procedure read_var_real_r8_dim2
        module procedure read_var_real_r8_dim2_pointer
        module procedure read_var_real_r8_dim3
    end interface read_var


    interface verify_var
        module procedure verify_var_logical
        module procedure verify_var_integer
        module procedure verify_var_character
        module procedure verify_var_real_r8_dim1
        module procedure verify_var_real_r8_dim2
        module procedure verify_var_real_r8_dim3
    end interface verify_var

    INTEGER, DIMENSION(2,10) :: kgen_bound

    public read_var, verify_var, kgen_get_newunit, kgen_error_stop
    contains

    subroutine read_var_real_r8_dim1(var, kgen_unit)
    integer, intent(in) :: kgen_unit
    real(kind=r8), intent(out), dimension(:), allocatable :: var
    logical is_save

    READ(UNIT = kgen_unit) is_save
    if ( is_save ) then
        READ(UNIT = kgen_unit) kgen_bound(1, 1)
        READ(UNIT = kgen_unit) kgen_bound(2, 1)
        ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1))
        READ(UNIT = kgen_unit) var
    end if

    end subroutine

    subroutine read_var_real_r8_dim2(var, kgen_unit)
    integer, intent(in) :: kgen_unit
    real(kind=r8), intent(out), dimension(:,:), allocatable :: var
    logical is_save

    READ(UNIT = kgen_unit) is_save
    if ( is_save ) then
        READ(UNIT = kgen_unit) kgen_bound(1, 1)
        READ(UNIT = kgen_unit) kgen_bound(2, 1)
        READ(UNIT = kgen_unit) kgen_bound(1, 2)
        READ(UNIT = kgen_unit) kgen_bound(2, 2)
        ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
        READ(UNIT = kgen_unit) var
    end if

    end subroutine

    subroutine read_var_real_r8_dim2_pointer(var, kgen_unit, is_pointer)
    integer, intent(in) :: kgen_unit
    real(kind=r8), intent(out), dimension(:,:), pointer :: var
    logical, intent(in) :: is_pointer
    logical is_save

    READ(UNIT = kgen_unit) is_save
    if ( is_save ) then
        READ(UNIT = kgen_unit) kgen_bound(1, 1)
        READ(UNIT = kgen_unit) kgen_bound(2, 1)
        READ(UNIT = kgen_unit) kgen_bound(1, 2)
        READ(UNIT = kgen_unit) kgen_bound(2, 2)
        ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1))
        READ(UNIT = kgen_unit) var
    end if

    end subroutine

    subroutine read_var_real_r8_dim3(var, kgen_unit)
    integer, intent(in) :: kgen_unit
    real(kind=r8), intent(out), dimension(:,:,:), allocatable :: var
    logical is_save

    READ(UNIT = kgen_unit) is_save
    if ( is_save ) then
        READ(UNIT = kgen_unit) kgen_bound(1, 1)
        READ(UNIT = kgen_unit) kgen_bound(2, 1)
        READ(UNIT = kgen_unit) kgen_bound(1, 2)
        READ(UNIT = kgen_unit) kgen_bound(2, 2)
        READ(UNIT = kgen_unit) kgen_bound(1, 3)
        READ(UNIT = kgen_unit) kgen_bound(2, 3)
        ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, &
            kgen_bound(2, 3) - kgen_bound(1, 3) + 1))
        READ(UNIT = kgen_unit) var
    end if

    end subroutine

    subroutine verify_var_logical(varname, var, ref_var)
    character(*), intent(in) :: varname
    logical, intent(in) :: var, ref_var

    IF ( var == ref_var ) THEN
        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) "KERNEL: ", var 
        WRITE(*,*) "REF.  : ", ref_var
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_integer(varname, var, ref_var)
    character(*), intent(in) :: varname
    integer, intent(in) :: var, ref_var

    IF ( var == ref_var ) THEN
        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) "KERNEL: ", var 
        WRITE(*,*) "REF.  : ", ref_var
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_character(varname, var, ref_var)
    character(*), intent(in) :: varname
    character(*), intent(in) :: var, ref_var

    IF ( var == ref_var ) THEN
        WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) "KERNEL: ", var 
        WRITE(*,*) "REF.  : ", ref_var
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_real_r8_dim1(varname, var, ref_var)
    character(*), intent(in) :: varname
    real(kind=r8), intent(in), dimension(:) :: var, ref_var

    IF ( ALL( var == ref_var ) ) THEN
        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
        !WRITE(*,*) "KERNEL: ", var
        !WRITE(*,*) "REF.  : ", ref_var
        IF ( ALL( var == 0 ) ) THEN
            WRITE(*,*) "All values are zero."
        END IF
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
        WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
        WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
        IF ( count(var /= ref_var) == 0 ) THEN
            WRITE(*,*) "RMS of difference is 0."
            WRITE(*,*) "Normalized RMS of difference is 0."
        ELSE
            IF ( ANY( ref_var == 0 ) ) THEN
                WRITE(*,*) "RMS of difference is ", sqrt(sum((var - ref_var)**2)/real(count( var /= ref_var)))
                WRITE(*,*) "Normalized RMS of difference is NOT available as one or more elements of ", trim(adjustl(varname)), " is(are) 0."
            ELSE
                WRITE(*,*) "RMS of difference is ", sqrt(sum((var - ref_var)**2)/real(count( var /= ref_var)))
                WRITE(*,*) "Normalized RMS of difference is ", sqrt(sum(((var - ref_var)/ref_var)**2)/real(count( var /= ref_var)))
            END IF
        END IF
        WRITE(*,*) "Minimum difference is ", minval(abs(var - ref_var))
        WRITE(*,*) "Maximum difference is ", maxval(abs(var - ref_var))
        IF ( sum(ref_var) == 0 ) THEN
            WRITE(*,*) "Minimum difference normalized to the reference average is NOT available as the sum of ", trim(adjustl(varname)), " is 0."
            WRITE(*,*) "Maximum difference normalized to the reference average is NOT available as the sum of ", trim(adjustl(varname)), " is 0."
        ELSE
            WRITE(*,*) "Minimum difference normalized to the reference average is ", minval(abs(var - ref_var))/(sum(ref_var)/real(size(ref_var)))
            WRITE(*,*) "Maximum difference normalized to the reference average is ", maxval(abs(var - ref_var))/(sum(ref_var)/real(size(ref_var)))
        END IF
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_real_r8_dim2(varname, var, ref_var)
    character(*), intent(in) :: varname
    real(kind=r8), intent(in), dimension(:,:) :: var, ref_var

    IF ( ALL( var == ref_var ) ) THEN
        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
        !WRITE(*,*) "KERNEL: ", var
        !WRITE(*,*) "REF.  : ", ref_var
        IF ( ALL( var == 0 ) ) THEN
            WRITE(*,*) "All values are zero."
        END IF
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
        WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
        WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
        IF ( count(var /= ref_var) == 0 ) THEN
            WRITE(*,*) "RMS of difference is 0."
            WRITE(*,*) "Normalized RMS of difference is 0."
        ELSE
            IF ( ANY( ref_var == 0 ) ) THEN
                WRITE(*,*) "RMS of difference is ", sqrt(sum((var - ref_var)**2)/real(count( var /= ref_var)))
                WRITE(*,*) "Normalized RMS of difference is NOT available as one or more elements of ", trim(adjustl(varname)), " is(are) 0."
            ELSE
                WRITE(*,*) "RMS of difference is ", sqrt(sum((var - ref_var)**2)/real(count( var /= ref_var)))
                WRITE(*,*) "Normalized RMS of difference is ", sqrt(sum(((var - ref_var)/ref_var)**2)/real(count( var /= ref_var)))
            END IF
        END IF
        WRITE(*,*) "Minimum difference is ", minval(abs(var - ref_var))
        WRITE(*,*) "Maximum difference is ", maxval(abs(var - ref_var))
        IF ( sum(ref_var) == 0 ) THEN
            WRITE(*,*) "Minimum difference normalized to the reference average is NOT available as the sum of ", trim(adjustl(varname)), " is 0."
            WRITE(*,*) "Maximum difference normalized to the reference average is NOT available as the sum of ", trim(adjustl(varname)), " is 0."
        ELSE
            WRITE(*,*) "Minimum difference normalized to the reference average is ", minval(abs(var - ref_var))/(sum(ref_var)/real(size(ref_var)))
            WRITE(*,*) "Maximum difference normalized to the reference average is ", maxval(abs(var - ref_var))/(sum(ref_var)/real(size(ref_var)))
        END IF
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_real_r8_dim3(varname, var, ref_var)
    character(*), intent(in) :: varname
    real(kind=r8), intent(in), dimension(:,:,:) :: var, ref_var

    IF ( ALL( var == ref_var ) ) THEN
        WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
        !WRITE(*,*) "KERNEL: ", var
        !WRITE(*,*) "REF.  : ", ref_var
        IF ( ALL( var == 0 ) ) THEN
            WRITE(*,*) "All values are zero."
        END IF
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
        WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
        WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
        IF ( count(var /= ref_var) == 0 ) THEN
            WRITE(*,*) "RMS of difference is 0."
            WRITE(*,*) "Normalized RMS of difference is 0."
        ELSE
            IF ( ANY( ref_var == 0 ) ) THEN
                WRITE(*,*) "RMS of difference is ", sqrt(sum((var - ref_var)**2)/real(count( var /= ref_var)))
                WRITE(*,*) "Normalized RMS of difference is NOT available as one or more elements of ", trim(adjustl(varname)), " is(are) 0."
            ELSE
                WRITE(*,*) "RMS of difference is ", sqrt(sum((var - ref_var)**2)/real(count( var /= ref_var)))
                WRITE(*,*) "Normalized RMS of difference is ", sqrt(sum(((var - ref_var)/ref_var)**2)/real(count( var /= ref_var)))
            END IF
        END IF
        WRITE(*,*) "Minimum difference is ", minval(abs(var - ref_var))
        WRITE(*,*) "Maximum difference is ", maxval(abs(var - ref_var))
        IF ( sum(ref_var) == 0 ) THEN
            WRITE(*,*) "Minimum difference normalized to the reference average is NOT available as the sum of ", trim(adjustl(varname)), " is 0."
            WRITE(*,*) "Maximum difference normalized to the reference average is NOT available as the sum of ", trim(adjustl(varname)), " is 0."
        ELSE
            WRITE(*,*) "Minimum difference normalized to the reference average is ", minval(abs(var - ref_var))/(sum(ref_var)/real(size(ref_var)))
            WRITE(*,*) "Maximum difference normalized to the reference average is ", maxval(abs(var - ref_var))/(sum(ref_var)/real(size(ref_var)))
        END IF
    END IF
    WRITE(*,*)

    end subroutine

    FUNCTION kgen_get_newunit(seed) RESULT(new_unit)
       INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000
       LOGICAL :: is_opened
       INTEGER :: nunit, new_unit, counter
       INTEGER, INTENT(IN) :: seed

       new_unit = -1

       DO counter=UNIT_MIN, UNIT_MAX
           inquire(UNIT=counter, OPENED=is_opened)
           IF (.NOT. is_opened) THEN
               new_unit = counter
               EXIT
           END IF
       END DO
    END FUNCTION

    SUBROUTINE kgen_error_stop( msg )
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: msg

        WRITE (*,*) msg
        STOP 1
    END SUBROUTINE
    end module
