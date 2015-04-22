    module kgen_utils

    use shr_kind_mod,   only: r8=>shr_kind_r8

    implicit none
    private
    save

    type, public  ::  check_t
         logical :: Passed 
         integer :: numFatal
         integer :: numTotal
         integer :: numIdentical
         integer :: numWarning
         integer :: VerboseLevel
         real(kind=r8) :: tolerance
    end type check_t 

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

    public :: read_var, verify_var, kgen_get_newunit, kgen_error_stop
    public :: KGENInitCheck, KGENPrtCheck, KGENApplyPert

     
    contains

   subroutine KGENApplyPert(var)

     real(kind=r8), intent(inout) :: var(:,:) 
     integer, allocatable :: rndm_seed(:)
     integer :: rndm_seed_sz
     real(kind=r8) :: pertval
     real(kind=r8) :: pertlim = 1.0e-15
     integer :: j,i,n1,n2


     n1=size(var,dim=1)
     n2=size(var,dim=2)

     call random_seed(size=rndm_seed_sz)
     allocate(rndm_seed(rndm_seed_sz))
     rndm_seed = 121869
     call random_seed(put=rndm_seed)

     do j=1,n2
     do i=1,n1
       call random_number(pertval)
       pertval = 2.0_r8*pertlim*(0.5_r8 - pertval)
       var(i,j) = var(i,j)*(1.0_r8 + pertval)
     enddo
     enddo
     deallocate(rndm_seed)
    end subroutine KGENApplyPert


    subroutine KGENinitCheck(check,tolerance)
      type(check_t), intent(inout) :: check
      real(kind=r8), intent(in), optional :: tolerance
       check%Passed   = .TRUE.
       check%numFatal = 0
       check%numWarning = 0
       check%numTotal = 0
       check%numIdentical = 0
       check%VerboseLevel = 0
       if(present(tolerance)) then 
         check%tolerance = tolerance
       else
          check%tolerance = 1.e-14
       endif

    end subroutine KGENinitCheck
    subroutine KGENPrtCheck(kname, check)
       character(len=*) :: kname
       type(check_t), intent(in) ::  check
       write (*,*)
       write (*,*) TRIM(kname),' KGENPrtCheck: Tolerance for normalized RMS: ',check%tolerance
       write (*,*) TRIM(kname),' KGENPrtCheck: Number of variables checked: ',check%numTotal
       write (*,*) TRIM(kname),' KGENPrtCheck: Number of Identical results: ',check%numIdentical 
       write (*,*) TRIM(kname),' KGENPrtCheck: Number of warnings detected: ',check%numWarning
       write (*,*) TRIM(kname),' KGENPrtCheck: Number of fatal errors detected: ', check%numFatal
       if (check%numFatal> 0) then 
            write(*,*) TRIM(kname),' KGENPrtCheck: verification FAILED'
       else
            write(*,*) TRIM(kname),' KGENPrtCheck: verification PASSED'
       endif
    
    end subroutine KgenPrtCheck

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

    subroutine verify_var_logical(varname, status, var, ref_var)
    character(*), intent(in) :: varname
    type(check_t), intent(inout) :: status
    logical, intent(in) :: var, ref_var

    status%numTotal = status%numTotal + 1
    IF ( var .eqv. ref_var ) THEN
        status%numIdentical = status%numIdentical + 1
        if(status%verboseLevel > 3) then 
            WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
        endif
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        if(status%verboseLevel > 2) then 
            WRITE(*,*) "KERNEL: ", var 
            WRITE(*,*) "REF.  : ", ref_var
        endif
        status%numFatal = status%numFatal + 1
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_integer(varname, status, var, ref_var)
    character(*), intent(in) :: varname
    type(check_t), intent(inout) :: status
    integer, intent(in) :: var, ref_var

    status%numTotal = status%numTotal + 1
    IF ( var == ref_var ) THEN
        status%numIdentical = status%numIdentical + 1
        if(status%verboseLevel > 3) then 
           WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
        endif
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        if(status%verboseLevel > 2) then 
           WRITE(*,*) "KERNEL: ", var 
           WRITE(*,*) "REF.  : ", ref_var
        endif
        status%numFatal = status%numFatal + 1
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_character(varname, status, var, ref_var)
    character(*), intent(in) :: varname
    type(check_t), intent(inout) :: status
    character(*), intent(in) :: var, ref_var

    status%numTotal = status%numTotal + 1
    IF ( var == ref_var ) THEN
        status%numIdentical = status%numIdentical + 1
        if(status%verboseLevel > 3) then 
           WRITE(*,*) trim(adjustl(varname)), " is IDENTICAL( ", var, " )."
        endif
    ELSE
        WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
        if(status%verboseLevel > 2) then 
           WRITE(*,*) "KERNEL: ", var 
           WRITE(*,*) "REF.  : ", ref_var
        endif
        status%numFatal = status%numFatal + 1
    END IF
    WRITE(*,*)

    end subroutine

    subroutine verify_var_real_r8_dim1(varname, status, var, ref_var)
    character(*), intent(in) :: varname
    type(check_t), intent(inout) :: status
    real(kind=r8), intent(in), dimension(:) :: var, ref_var
    ! local
    real(kind=r8) :: nrmsdiff, rmsdiff
    real(kind=r8), allocatable :: temp(:), temp2(:) 
    
    integer :: n
    

    status%numTotal = status%numTotal + 1
    IF ( ALL( var == ref_var ) ) THEN
        status%numIdentical = status%numIdentical + 1
        if(status%verboseLevel > 3) then 
           WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
           !WRITE(*,*) "KERNEL: ", var
           !WRITE(*,*) "REF.  : ", ref_var
           IF ( ALL( var == 0 ) ) THEN
              if(status%verboseLevel > 2) then 
                 WRITE(*,*) "All values are zero."
              endif
           END IF
        endif
    ELSE
         allocate(temp(SIZE(var,dim=1)))
         allocate(temp2(SIZE(var,dim=1)))
         n = count(var/=ref_var)
         where(ref_var > 0)
            temp  = ((var-ref_var)/ref_var)**2
            temp2 = (var-ref_var)**2
         elsewhere
            temp  = (var-ref_var)**2
            temp2 = temp
         endwhere
         nrmsdiff = sqrt(sum(temp)/real(n))
         rmsdiff = sqrt(sum(temp2)/real(n))
         if (nrmsdiff > status%tolerance) then
            WRITE(*,*)
            WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
            WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
            if(status%verboseLevel > 2) then
               WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
               WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
            endif
            WRITE(*,*) "RMS of difference is ",rmsdiff
            WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
            status%numFatal = status%numFatal+1
         endif
         deallocate(temp,temp2)
    END IF

    end subroutine

    subroutine verify_var_real_r8_dim2(varname, status, var, ref_var)
    character(*), intent(in) :: varname
    type(check_t), intent(inout) :: status
    real(kind=r8), intent(in), dimension(:,:) :: var, ref_var
    ! local
    integer :: n
    real (kind=r8) :: nrmsdiff,rmsdiff
    real (kind=r8), allocatable :: temp(:,:), temp2(:,:)

    status%numTotal = status%numTotal + 1
    IF ( ALL( var == ref_var ) ) THEN
        status%numIdentical = status%numIdentical + 1
        if(status%verboseLevel > 3) then 
           WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
           !WRITE(*,*) "KERNEL: ", var
           !WRITE(*,*) "REF.  : ", ref_var
           IF ( ALL( var == 0 ) ) THEN
              WRITE(*,*) "All values are zero."
           END IF
        endif
    ELSE
         allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2)))
         allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2)))
!         n = count(var/=ref_var)
         n = size(var,dim=1)*size(var,dim=2)
         where(ref_var > 0) 
            temp  = ((var-ref_var)/ref_var)**2
            temp2 = (var-ref_var)**2
         elsewhere
            temp  = (var-ref_var)**2
            temp2 = temp
         endwhere
         nrmsdiff = sqrt(sum(temp)/real(n))
         rmsdiff = sqrt(sum(temp2)/real(n))
         if (nrmsdiff > status%tolerance) then 
            WRITE(*,*)
            WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
            WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
            if(status%verboseLevel > 2) then 
               WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
               WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
            endif
            WRITE(*,*) "RMS of difference is ",rmsdiff
            WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
            status%numFatal = status%numFatal+1
         endif
         deallocate(temp,temp2)
    END IF

    end subroutine

    subroutine verify_var_real_r8_dim3(varname, status, var, ref_var)
    character(*), intent(in) :: varname
    type(check_t), intent(inout) :: status
    real(kind=r8), intent(in), dimension(:,:,:) :: var, ref_var
    ! local
    real (kind=r8) :: nrmsdiff, rmsdiff
    real (kind=r8), allocatable :: temp(:,:,:), temp2(:,:,:)
    integer :: n
  

    status%numTotal = status%numTotal + 1
    IF ( ALL( var == ref_var ) ) THEN
        status%numIdentical = status%numIdentical + 1
        if(status%verboseLevel > 3) then 
           WRITE(*,*) "All elements of ", trim(adjustl(varname)), " are IDENTICAL."
           !WRITE(*,*) "KERNEL: ", var
           !WRITE(*,*) "REF.  : ", ref_var
           IF ( ALL( var == 0 ) ) THEN
             WRITE(*,*) "All values are zero."
           END IF
        endif
    ELSE
         allocate(temp(SIZE(var,dim=1),SIZE(var,dim=2),size(var,dim=3)))
         allocate(temp2(SIZE(var,dim=1),SIZE(var,dim=2),size(var,dim=3)))
!         n = count(var/=ref_var)
         n = size(var,dim=1)*size(var,dim=2)*size(var,dim=3)
         where(ref_var > 0) 
            temp  = ((var-ref_var)/ref_var)**2
            temp2 = (var-ref_var)**2
         elsewhere
            temp  = (var-ref_var)**2
            temp2 = temp
         endwhere
         nrmsdiff = sqrt(sum(temp)/real(n))
         rmsdiff = sqrt(sum(temp2)/real(n))
         if (nrmsdiff > status%tolerance) then
            WRITE(*,*)
            WRITE(*,*) trim(adjustl(varname)), " is NOT IDENTICAL."
            WRITE(*,*) count( var /= ref_var), " of ", size( var ), " elements are different."
            if(status%verboseLevel > 2) then
               WRITE(*,*) "Average - kernel ", sum(var)/real(size(var))
               WRITE(*,*) "Average - reference ", sum(ref_var)/real(size(ref_var))
            endif
            WRITE(*,*) "RMS of difference is ",rmsdiff
            WRITE(*,*) "Normalized RMS of difference is ",nrmsdiff
            status%numFatal = status%numFatal+1
         endif
         deallocate(temp,temp2)

    END IF

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
