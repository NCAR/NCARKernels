MODULE kgen_utils_mod
INTEGER, PARAMETER :: kgen_dp = selected_real_kind(15, 307)
INTEGER, PARAMETER :: CHECK_IDENTICAL = 1
INTEGER, PARAMETER :: CHECK_IN_TOL = 2
INTEGER, PARAMETER :: CHECK_OUT_TOL = 3

interface kgen_tostr
    module procedure kgen_tostr_args1
    module procedure kgen_tostr_args2
    module procedure kgen_tostr_args3
    module procedure kgen_tostr_args4
    module procedure kgen_tostr_args5
    module procedure kgen_tostr_args6
end interface

! PERTURB: add following interface
interface kgen_perturb_real
    module procedure kgen_perturb_real4_dim1
    module procedure kgen_perturb_real4_dim2
    module procedure kgen_perturb_real4_dim3
    module procedure kgen_perturb_real8_dim1
    module procedure kgen_perturb_real8_dim2
    module procedure kgen_perturb_real8_dim3
end interface

type check_t
    logical :: Passed
    integer :: numOutTol
    integer :: numTotal
    integer :: numIdentical
    integer :: numInTol
    integer :: VerboseLevel
    real(kind=kgen_dp) :: tolerance
    real(kind=kgen_dp) :: minvalue
end type check_t

public kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb_real
public CHECK_NOT_CHECKED, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL
public kgen_get_newunit, kgen_error_stop
CONTAINS

function kgen_tostr_args1(idx1) result(tostr)
    integer, intent(in) :: idx1
    character(len=64) :: str_idx1
    character(len=64) :: tostr

    write(str_idx1, *) idx1
    tostr = trim(adjustl(str_idx1))
end function

function kgen_tostr_args2(idx1, idx2) result(tostr)
    integer, intent(in) :: idx1, idx2
    character(len=64) :: str_idx1, str_idx2
    character(len=128) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2))
end function

function kgen_tostr_args3(idx1, idx2, idx3) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3
    character(len=64) :: str_idx1, str_idx2, str_idx3
    character(len=192) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3))
end function

function kgen_tostr_args4(idx1, idx2, idx3, idx4) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3, idx4
    character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4
    character(len=256) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    write(str_idx4, *) idx4
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4))
end function

function kgen_tostr_args5(idx1, idx2, idx3, idx4, idx5) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3, idx4, idx5
    character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4, str_idx5
    character(len=320) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    write(str_idx4, *) idx4
    write(str_idx5, *) idx5
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4)) &
        // ", " // trim(adjustl(str_idx5))
end function

function kgen_tostr_args6(idx1, idx2, idx3, idx4, idx5, idx6) result(tostr)
    integer, intent(in) :: idx1, idx2, idx3, idx4, idx5, idx6
    character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4, str_idx5, str_idx6
    character(len=384) :: tostr

    write(str_idx1, *) idx1
    write(str_idx2, *) idx2
    write(str_idx3, *) idx3
    write(str_idx4, *) idx4
    write(str_idx5, *) idx5
    write(str_idx6, *) idx6
    tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
        // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4)) &
        // ", " // trim(adjustl(str_idx5)) // ", " // trim(adjustl(str_idx6))
end function

subroutine kgen_perturb_real4_dim1(var, pertlim)
    real*4, intent(inout), dimension(:) :: var
    real*4, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*4 :: pertval
    integer :: idx1

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        call random_number(pertval)
        pertval = 2.0_4*pertlim*(0.5_4 - pertval)
        var(idx1) = var(idx1)*(1.0_4 + pertval)
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real4_dim2(var, pertlim)
    real*4, intent(inout), dimension(:,:) :: var
    real*4, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*4 :: pertval
    integer :: idx1,idx2

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            call random_number(pertval)
            pertval = 2.0_4*pertlim*(0.5_4 - pertval)
            var(idx1,idx2) = var(idx1,idx2)*(1.0_4 + pertval)
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real4_dim3(var, pertlim)
    real*4, intent(inout), dimension(:,:,:) :: var
    real*4, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*4 :: pertval
    integer :: idx1,idx2,idx3

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            do idx3=1,size(var, dim=3)
                call random_number(pertval)
                pertval = 2.0_4*pertlim*(0.5_4 - pertval)
                var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_4 + pertval)
            end do
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real8_dim1(var, pertlim)
    real*8, intent(inout), dimension(:) :: var
    real*8, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*8 :: pertval
    integer :: idx1

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        call random_number(pertval)
        pertval = 2.0_8*pertlim*(0.5_8 - pertval)
        var(idx1) = var(idx1)*(1.0_8 + pertval)
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real8_dim2(var, pertlim)
    real*8, intent(inout), dimension(:,:) :: var
    real*8, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*8 :: pertval
    integer :: idx1,idx2

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            call random_number(pertval)
            pertval = 2.0_8*pertlim*(0.5_8 - pertval)
            var(idx1,idx2) = var(idx1,idx2)*(1.0_8 + pertval)
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_perturb_real8_dim3(var, pertlim)
    real*8, intent(inout), dimension(:,:,:) :: var
    real*8, intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real*8 :: pertval
    integer :: idx1,idx2,idx3

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            do idx3=1,size(var, dim=3)
                call random_number(pertval)
                pertval = 2.0_8*pertlim*(0.5_8 - pertval)
                var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_8 + pertval)
            end do
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_init_check(check, verboseLevel, tolerance, minValue)
  type(check_t), intent(inout) :: check
  integer, intent(in), optional :: verboseLevel
  real(kind=kgen_dp), intent(in), optional :: tolerance
  real(kind=kgen_dp), intent(in), optional :: minValue

  check%Passed   = .TRUE.
  check%numOutTol = 0
  check%numInTol = 0
  check%numTotal = 0
  check%numIdentical = 0

  if(present(verboseLevel)) then
     check%verboseLevel = verboseLevel
  else
      check%verboseLevel = 1
  end if
  if(present(tolerance)) then
     check%tolerance = tolerance
  else
      check%tolerance = 1.0D-15
  end if
  if(present(minvalue)) then
     check%minvalue = minvalue
  else
      check%minvalue = 1.0D-15
  end if
end subroutine kgen_init_check

subroutine kgen_print_check(kname, check)
   character(len=*) :: kname
   type(check_t), intent(in) ::  check

   write (*,*) TRIM(kname),': Tolerance for normalized RMS: ',check%tolerance
   !write (*,*) TRIM(kname),':',check%numFatal,'fatal errors,',check%numWarning,'warnings detected, and',check%numIdentical,'identical out of',check%numTotal,'variables checked'
   write (*,*) TRIM(kname),': Number of variables checked: ',check%numTotal
   write (*,*) TRIM(kname),': Number of Identical results: ',check%numIdentical
   write (*,*) TRIM(kname),': Number of variables within tolerance(not identical): ',check%numInTol
   write (*,*) TRIM(kname),': Number of variables out of tolerance: ', check%numOutTol

   if (check%numOutTol> 0) then
        write(*,*) TRIM(kname),': Verification FAILED'
   else
        write(*,*) TRIM(kname),': Verification PASSED'
   endif
end subroutine kgen_print_check

FUNCTION kgen_get_newunit() RESULT(new_unit)
   INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000
   LOGICAL :: is_opened
   INTEGER :: nunit, new_unit, counter

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
END MODULE kgen_utils_mod
