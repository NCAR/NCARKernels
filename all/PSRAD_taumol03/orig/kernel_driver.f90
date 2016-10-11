    PROGRAM kernel_driver

    USE kgen_utils, only : read_var, kgen_get_newunit, kgen_error_stop
    USE mo_kind, only : wp
    USE mo_lrtm_gas_optics, only : gas_optics_lw
    USE rrlw_kg03, only : read_extern_rrlw_kg03
    USE rrlw_planck, only : read_extern_rrlw_planck

    IMPLICIT NONE

    INTEGER :: kgen_ierr, kgen_unit
    INTEGER :: kgen_repeat_counter
    INTEGER :: kgen_counter
    CHARACTER(LEN=16) :: kgen_counter_conv
    INTEGER, DIMENSION(3), PARAMETER :: kgen_counter_at = (/ 1, 10, 100 /)
    CHARACTER(LEN=1024) :: kgen_filepath

    ! NOTE: change all arrays to allocatable

    INTEGER :: nlayers         ! total number of layers
    REAL(wp), allocatable :: coldry(:)          ! column amount (dry air)
    INTEGER :: laytrop         ! tropopause layer index
    INTEGER, allocatable :: jp(:)
    INTEGER, allocatable :: jt(:)
    INTEGER, allocatable :: jt1(:)
    REAL(wp), allocatable :: colh2o(:)          ! column amount (h2o)
    REAL(wp), allocatable :: colco2(:)          ! column amount (co2)
    REAL(wp), allocatable :: coln2o(:)          ! column amount (n2o)
    REAL(wp), allocatable :: fac00(:), fac01(:), fac10(:), fac11(:)
    REAL(wp), allocatable :: rat_h2oco2(:),rat_h2oco2_1(:)
    REAL(wp), allocatable :: selffac(:)
    REAL(wp), allocatable :: selffrac(:)
    INTEGER, allocatable :: indself(:)
    REAL(wp), allocatable :: forfac(:)
    REAL(wp), allocatable :: forfrac(:)
    INTEGER, allocatable :: indfor(:)
    REAL(wp), allocatable :: minorfrac(:)
    INTEGER, allocatable :: indminor(:)
    REAL(wp), allocatable :: fracs(:)        ! planck fractions Dimensions: (nlayers)
    REAL(wp), allocatable :: taug(:)         ! gaseous optical depth Dimensions: (nlayers)

    DO kgen_repeat_counter = 0, 2
        kgen_counter = kgen_counter_at(mod(kgen_repeat_counter, 3)+1)
        WRITE( kgen_counter_conv, * ) kgen_counter

        kgen_filepath = "../data/taumol03." // trim(adjustl(kgen_counter_conv))
        kgen_unit = kgen_get_newunit(kgen_counter)
        OPEN (UNIT=kgen_unit, FILE=kgen_filepath, STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", ACTION="READ", IOSTAT=kgen_ierr, CONVERT="BIG_ENDIAN")
        WRITE (*,*)
        IF ( kgen_ierr /= 0 ) THEN
            CALL kgen_error_stop( "FILE OPEN ERROR: " // trim(adjustl(kgen_filepath)) )
        END IF

        WRITE (*,*)
        WRITE (*,*) "************ Verification against '" // trim(adjustl(kgen_filepath)) // "' ************"

        ! READ extern states per each modules
        CALL read_extern_rrlw_planck(kgen_unit)
        CALL read_extern_rrlw_kg03(kgen_unit)

        READ(UNIT=kgen_unit) nlayers
        call read_var(coldry, kgen_unit)
        READ(UNIT=kgen_unit) laytrop
        call read_var(jp, kgen_unit)
        call read_var(jt, kgen_unit)
        call read_var(jt1, kgen_unit)
        call read_var(colh2o, kgen_unit)
        call read_var(colco2, kgen_unit)
        call read_var(coln2o, kgen_unit)
        call read_var(fac00, kgen_unit)
        call read_var(fac01, kgen_unit)
        call read_var(fac10, kgen_unit)
        call read_var(fac11, kgen_unit)
        call read_var(rat_h2oco2, kgen_unit)
        call read_var(rat_h2oco2_1, kgen_unit)
        call read_var(selffac, kgen_unit)
        call read_var(selffrac, kgen_unit)
        call read_var(indself, kgen_unit)
        call read_var(forfac, kgen_unit)
        call read_var(forfrac, kgen_unit)
        call read_var(indfor, kgen_unit)
        call read_var(minorfrac, kgen_unit)
        call read_var(indminor, kgen_unit)
        call read_var(fracs, kgen_unit)
        call read_var(taug, kgen_unit)

        ! call call-site

        CALL gas_optics_lw(nlayers, coldry, laytrop, jp, jt, jt1, colh2o, colco2, coln2o, fac00, fac01, fac10, fac11, &
            rat_h2oco2, rat_h2oco2_1, selffac, selffrac, indself, forfac, forfrac, indfor, minorfrac, indminor, fracs, taug, kgen_unit)

        CLOSE (UNIT=kgen_unit)

    END DO

        WRITE (*, *) ""
        WRITE (*, "(A)") "****************************************************"
        WRITE (*, "(4X,A)") "kernel execution summary: taumol03"
        WRITE (*, "(A)") "****************************************************"
        IF (kgen_repeat_counter == 0) THEN
            WRITE (*, *) "No data file is verified."
        ELSE
            WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_repeat_counter
        END IF
        WRITE (*, "(A)") "****************************************************"
    END PROGRAM
