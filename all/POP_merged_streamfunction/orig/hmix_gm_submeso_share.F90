
! KGEN-generated Fortran source file
!
! Filename    : hmix_gm_submeso_share.F90
! Generated at: 2015-06-09 10:04:06
! KGEN version: 0.4.12



    MODULE hmix_gm_submeso_share
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: hmix_gm_submeso_share
        ! !DESCRIPTION:
        !  This module contains routines for computing tracer and density differences for
        !  use in the hmix_gm and mix_submeso routines. In addition, isopycnal slopes are
        !  computed if necessary.
        ! !REVISION HISTORY:
        !  SVN:$Id: hmix_gm_submeso_share.F90
        ! !USES:
        USE kinds_mod, only : r8
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC MEMBER FUNCTIONS:
        !-----------------------------------------------------------------------
        !
        !  variables to save from one call to next
        !
        !-----------------------------------------------------------------------
        ! Dx(rho), Dy(rho)
        ! tracer differences in each direction
        REAL(KIND=r8), dimension(:,:,:,:,:,:), allocatable, public :: slx
        REAL(KIND=r8), dimension(:,:,:,:,:,:), allocatable, public :: sly
        ! slope of isopycnal sfcs in x,y-direction
        ! Dz(rho)
        ! dx/dy for y-z plane
        ! dy/dx for x-z plane
        !***********************************************************************
            PUBLIC kgen_read_externs_hmix_gm_submeso_share
        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_real_r8_dim6_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                real(KIND=r8), INTENT(OUT), ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: var
                LOGICAL :: is_true
                INTEGER :: idx1,idx2,idx3,idx4,idx5,idx6
                INTEGER, DIMENSION(2,6) :: kgen_bound

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
                    READ(UNIT = kgen_unit) kgen_bound(1, 5)
                    READ(UNIT = kgen_unit) kgen_bound(2, 5)
                    READ(UNIT = kgen_unit) kgen_bound(1, 6)
                    READ(UNIT = kgen_unit) kgen_bound(2, 6)
                    ALLOCATE(var(kgen_bound(2, 1) - kgen_bound(1, 1) + 1, kgen_bound(2, 2) - kgen_bound(1, 2) + 1, kgen_bound(2, 3) - kgen_bound(1, 3) + 1, kgen_bound(2, 4) - kgen_bound(1, 4) + 1, kgen_bound(2, 5) - kgen_bound(1, 5) + 1, kgen_bound(2, 6) - kgen_bound(1, 6) + 1))
                    READ(UNIT = kgen_unit) var
                    IF ( PRESENT(printvar) ) THEN
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_real_r8_dim6_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_hmix_gm_submeso_share(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            CALL kgen_read_real_r8_dim6_alloc(slx, kgen_unit)
            CALL kgen_read_real_r8_dim6_alloc(sly, kgen_unit)
        END SUBROUTINE kgen_read_externs_hmix_gm_submeso_share

        !***********************************************************************
        ! !IROUTINE: init_meso_mixing
        ! !INTERFACE:

        !-----------------------------------------------------------------------
        ! !IROUTINE: tracer_diffs_and_isopyc_slopes
        ! !INTERFACE:

        !
        !***********************************************************************
    END MODULE hmix_gm_submeso_share
