! TODO:
! make a pack of original src file, preprocessed source file, original parsed objects, modified parsed object, ...
! A class for a file
! NOTE: add one more step from origninal source file to a file to be parsed by f2py
! org src -> fpp preprocessed src -> f2py preprocessed src => f2py parsing

!main-program
!call call-site subprogram if any
!read input states from all used files

    PROGRAM kernel_micro_mg_tend

    use shr_kind_mod,   only: r8=>shr_kind_r8
    USE kgen_utils, only : kgen_get_newunit, kgen_error_stop
    USE micro_mg_cam, only : read_extern_micro_mg_cam, micro_mg_cam_tend
    use micro_mg2_0, only : read_extern_micro_mg2_0
    use micro_mg_utils, only : read_extern_micro_mg_utils
    use wv_sat_methods, only : read_extern_wv_sat_methods

    IMPLICIT NONE

    INTEGER :: kgen_mpi_rank
    CHARACTER(LEN=16) ::kgen_mpi_rank_conv
    INTEGER, DIMENSION(1), PARAMETER :: kgen_mpi_rank_at = (/ 0 /)
    INTEGER :: kgen_ierr, kgen_unit
    INTEGER :: kgen_repeat_counter
    INTEGER :: kgen_counter
    CHARACTER(LEN=16) :: kgen_counter_conv
    INTEGER, DIMENSION(1), PARAMETER :: kgen_counter_at = (/ 1 /)
    CHARACTER(LEN=1024) :: kgen_filepath

    ! call-site intent in or inout variables
    real(r8) :: dtime

    DO kgen_repeat_counter = 1, 1
        kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_counter_conv, * ) kgen_counter
        kgen_mpi_rank = kgen_mpi_rank_at(mod(kgen_repeat_counter, 1)+1)
        WRITE( kgen_mpi_rank_conv, * ) kgen_mpi_rank


        kgen_filepath = "../data/micro_mg_tend." // trim(adjustl(kgen_counter_conv)) // "." // trim(adjustl(kgen_mpi_rank_conv))
        kgen_unit = kgen_get_newunit(kgen_mpi_rank+kgen_counter)
        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", &
		IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
        WRITE (*,*)
        IF ( kgen_ierr /= 0 ) THEN
            CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )
        END IF

        WRITE (*,*)
        WRITE (*,*) "************ Verification against '" // trim(adjustl(kgen_filepath)) // "' ************"

        ! READ extern states
        CALL read_extern_micro_mg_cam(kgen_unit)
        CALL read_extern_micro_mg2_0(kgen_unit)
        CALL read_extern_micro_mg_utils(kgen_unit)
        CALL read_extern_wv_sat_methods(kgen_unit)

        ! NOTE:
        ! * if kernel is called within subprogram.
        ! * put read statements of intent(in) argument here

        ! READ in and inout states to call site
        ! NOTE: need to generalize handling variable such as allocatble, derived type, pointers,...
        READ(UNIT=kgen_unit) dtime

        ! call call-site
        CALL micro_mg_cam_tend(dtime, kgen_unit)

        CLOSE (UNIT=kgen_unit)

    END DO

    END PROGRAM kernel_micro_mg_tend
