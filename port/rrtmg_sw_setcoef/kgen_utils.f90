module kgen_utils_mod

INTEGER, PARAMETER :: kgen_dp = selected_real_kind(15, 307)

! PERTURB: Add following kind parameter to match with the kind attribute of the perturbed variable
INTEGER, parameter :: r8 = selected_real_kind(12)

! PERTURB: add following interface
interface kgen_perturb
    module procedure kgen_perturb_real_dim1
    module procedure kgen_perturb_real_dim2
    module procedure kgen_perturb_real_dim3
end interface

type check_t
    logical :: Passed
    integer :: numFatal
    integer :: numTotal
    integer :: numIdentical
    integer :: numWarning
    integer :: VerboseLevel
    real(kind=kgen_dp) :: tolerance
    real(kind=kgen_dp) :: minvalue
end type check_t

! PERTURB: add kgen_perturb
public kgen_dp, check_t, kgen_init_check, kgen_print_check, kgen_perturb
!public kgen_dp, check_t, kgen_init_check, kgen_print_check

contains

! PERTURB: add following perturbation subroutine
subroutine kgen_perturb_real_dim1(var, pertlim)
    real(kind=r8), intent(inout), dimension(:) :: var
    real(kind=r8), intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real(kind=r8) :: pertval
    integer :: idx1

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        call random_number(pertval)
        pertval = 2.0_r8*pertlim*(0.5_r8 - pertval)
        var(idx1) = var(idx1)*(1.0_r8 + pertval)
    end do
    deallocate(rndm_seed)
end subroutine

! PERTURB: add following perturbation subroutine
subroutine kgen_perturb_real_dim2(var, pertlim)
    real(kind=r8), intent(inout), dimension(:,:) :: var
    real(kind=r8), intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real(kind=r8) :: pertval
    integer :: idx1,idx2

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            call random_number(pertval)
            pertval = 2.0_r8*pertlim*(0.5_r8 - pertval)
            var(idx1,idx2) = var(idx1,idx2)*(1.0_r8 + pertval)
        end do
    end do
    deallocate(rndm_seed)
end subroutine

! PERTURB: add following perturbation subroutine
subroutine kgen_perturb_real_dim3(var, pertlim)
    real(kind=r8), intent(inout), dimension(:,:,:) :: var
    real(kind=r8), intent(in) :: pertlim
    integer, allocatable :: rndm_seed(:)
    integer :: rndm_seed_sz
    real(kind=r8) :: pertval
    integer :: idx1,idx2,idx3

    call random_seed(size=rndm_seed_sz)
    allocate(rndm_seed(rndm_seed_sz))
    rndm_seed = 121869
    call random_seed(put=rndm_seed)
    do idx1=1,size(var, dim=1)
        do idx2=1,size(var, dim=2)
            do idx3=1,size(var, dim=3)
                call random_number(pertval)
                pertval = 2.0_r8*pertlim*(0.5_r8 - pertval)
                var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_r8 + pertval)
            end do
        end do
    end do
    deallocate(rndm_seed)
end subroutine

subroutine kgen_init_check(check, tolerance, minvalue)
  type(check_t), intent(inout) :: check
  real(kind=kgen_dp), intent(in), optional :: tolerance
  real(kind=kgen_dp), intent(in), optional :: minvalue

  check%Passed   = .TRUE.
  check%numFatal = 0
  check%numWarning = 0
  check%numTotal = 0
  check%numIdentical = 0
  check%VerboseLevel = 1
  if(present(tolerance)) then
     check%tolerance = tolerance
  else
      check%tolerance = 1.0D-15
  endif
  if(present(minvalue)) then
     check%minvalue = minvalue
  else
      check%minvalue = 1.0D-15
  endif
end subroutine kgen_init_check

subroutine kgen_print_check(kname, check)
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
end subroutine kgen_print_check

end module
