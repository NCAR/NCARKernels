
! KGEN-generated Fortran source file
!
! Filename    : blocks.F90
! Generated at: 2015-06-05 14:52:13
! KGEN version: 0.4.11



    MODULE blocks
        USE kgen_utils_mod, ONLY : kgen_dp, check_t, kgen_init_check, kgen_print_check
        !BOP
        ! !MODULE: blocks
        !
        ! !DESCRIPTION:
        !  This module contains data types and tools for decomposing a global
        !  horizontal domain into a set of blocks.  It contains a data type
        !  for describing each block and contains routines for creating and
        !  querying the block decomposition for a global domain.
        !
        ! !REVISION HISTORY:
        !  SVN:$Id: blocks.F90 44198 2013-02-25 22:43:22Z mlevy@ucar.edu $
        !
        ! !USES:
        USE kinds_mod, only : int_kind
        USE domain_size, ONLY: block_size_x
        USE domain_size, ONLY: block_size_y
        IMPLICIT NONE
        PRIVATE
        ! !PUBLIC TYPES:
        TYPE, public :: block ! block data type
            INTEGER(KIND=int_kind) :: block_id, local_id, ib, ie, jb, je, iblock, jblock
            ! global block number
            ! local address of block in current distrib
            ! begin,end indices for physical domain
            ! cartesian i,j position for bloc
            INTEGER(KIND=int_kind), dimension(:), pointer :: i_glob, j_glob
            ! global domain location for each point
        END TYPE 
        ! !PUBLIC MEMBER FUNCTIONS:
        PUBLIC get_block
        ! !DEFINED PARAMETERS:
        INTEGER(KIND=int_kind), parameter, public :: nghost = 2
        ! number of ghost cells around each block
        INTEGER(KIND=int_kind), parameter, public :: nx_block = block_size_x + 2*nghost
        INTEGER(KIND=int_kind), parameter, public :: ny_block = block_size_y + 2*nghost ! size of block domain in
        !  x,y dir including ghost
        !  cells
        ! !PUBLIC DATA MEMBERS:
        INTEGER(KIND=int_kind), public :: nblocks_tot
        ! total number of blocks in decomposition
        ! tot num blocks in i direction
        ! tot num blocks in j direction
        !EOP
        !BOC
        !-----------------------------------------------------------------------
        !
        !  module private data
        !
        !-----------------------------------------------------------------------
        TYPE(block), dimension(:), allocatable :: all_blocks
        ! block information for all blocks in domain
        ! global i index for each point in each block
        ! global j index for each point in each block
        !EOC
        !***********************************************************************
            PUBLIC kgen_read_externs_blocks

        ! read interface
        PUBLIC kgen_read
        INTERFACE kgen_read
            MODULE PROCEDURE kgen_read_block
        END INTERFACE kgen_read

        PUBLIC kgen_verify
        INTERFACE kgen_verify
            MODULE PROCEDURE kgen_verify_block
        END INTERFACE kgen_verify

        CONTAINS

        ! write subroutines
            SUBROUTINE kgen_read_integer_int_kind_dim1_ptr(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                integer(KIND=int_kind), INTENT(OUT), POINTER, DIMENSION(:) :: var
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
                        PRINT *, "** " // printvar // " **", var
                    END IF
                END IF
            END SUBROUTINE kgen_read_integer_int_kind_dim1_ptr

            SUBROUTINE kgen_read_block_dim1_alloc(var, kgen_unit, printvar)
                INTEGER, INTENT(IN) :: kgen_unit
                CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
                TYPE(block), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: var
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
                            CALL kgen_read_block(var(idx1), kgen_unit, printvar=printvar)
                    ELSE
                            CALL kgen_read_block(var(idx1), kgen_unit)
                    END IF
                    END DO
                END IF
            END SUBROUTINE kgen_read_block_dim1_alloc


        ! module extern variables

        SUBROUTINE kgen_read_externs_blocks(kgen_unit)
            INTEGER, INTENT(IN) :: kgen_unit
            READ(UNIT=kgen_unit) nblocks_tot
            CALL kgen_read_block_dim1_alloc(all_blocks, kgen_unit)
        END SUBROUTINE kgen_read_externs_blocks

        SUBROUTINE kgen_read_block(var, kgen_unit, printvar)
            INTEGER, INTENT(IN) :: kgen_unit
            CHARACTER(*), INTENT(IN), OPTIONAL :: printvar
            TYPE(block), INTENT(out) :: var
            READ(UNIT=kgen_unit) var%block_id
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%block_id **", var%block_id
            END IF
            READ(UNIT=kgen_unit) var%local_id
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%local_id **", var%local_id
            END IF
            READ(UNIT=kgen_unit) var%ib
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%ib **", var%ib
            END IF
            READ(UNIT=kgen_unit) var%ie
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%ie **", var%ie
            END IF
            READ(UNIT=kgen_unit) var%jb
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%jb **", var%jb
            END IF
            READ(UNIT=kgen_unit) var%je
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%je **", var%je
            END IF
            READ(UNIT=kgen_unit) var%iblock
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%iblock **", var%iblock
            END IF
            READ(UNIT=kgen_unit) var%jblock
            IF ( PRESENT(printvar) ) THEN
                print *, "** " // printvar // "%jblock **", var%jblock
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_integer_int_kind_dim1_ptr(var%i_glob, kgen_unit, printvar=printvar//"%i_glob")
            ELSE
                CALL kgen_read_integer_int_kind_dim1_ptr(var%i_glob, kgen_unit)
            END IF
            IF ( PRESENT(printvar) ) THEN
                CALL kgen_read_integer_int_kind_dim1_ptr(var%j_glob, kgen_unit, printvar=printvar//"%j_glob")
            ELSE
                CALL kgen_read_integer_int_kind_dim1_ptr(var%j_glob, kgen_unit)
            END IF
        END SUBROUTINE
        SUBROUTINE kgen_verify_block(varname, check_status, var, ref_var)
            CHARACTER(*), INTENT(IN) :: varname
            TYPE(check_t), INTENT(INOUT) :: check_status
            TYPE(check_t) :: dtype_check_status
            TYPE(block), INTENT(IN) :: var, ref_var

            check_status%numTotal = check_status%numTotal + 1
            CALL kgen_init_check(dtype_check_status)
            CALL kgen_verify_integer_int_kind("block_id", dtype_check_status, var%block_id, ref_var%block_id)
            CALL kgen_verify_integer_int_kind("local_id", dtype_check_status, var%local_id, ref_var%local_id)
            CALL kgen_verify_integer_int_kind("ib", dtype_check_status, var%ib, ref_var%ib)
            CALL kgen_verify_integer_int_kind("ie", dtype_check_status, var%ie, ref_var%ie)
            CALL kgen_verify_integer_int_kind("jb", dtype_check_status, var%jb, ref_var%jb)
            CALL kgen_verify_integer_int_kind("je", dtype_check_status, var%je, ref_var%je)
            CALL kgen_verify_integer_int_kind("iblock", dtype_check_status, var%iblock, ref_var%iblock)
            CALL kgen_verify_integer_int_kind("jblock", dtype_check_status, var%jblock, ref_var%jblock)
            CALL kgen_verify_integer_int_kind_dim1_ptr("i_glob", dtype_check_status, var%i_glob, ref_var%i_glob)
            CALL kgen_verify_integer_int_kind_dim1_ptr("j_glob", dtype_check_status, var%j_glob, ref_var%j_glob)
            IF ( dtype_check_status%numTotal == dtype_check_status%numIdentical ) THEN
                check_status%numIdentical = check_status%numIdentical + 1
            ELSE IF ( dtype_check_status%numFatal > 0 ) THEN
                check_status%numFatal = check_status%numFatal + 1
            ELSE IF ( dtype_check_status%numWarning > 0 ) THEN
                check_status%numWarning = check_status%numWarning + 1
            END IF
        END SUBROUTINE
            SUBROUTINE kgen_verify_integer_int_kind( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer(KIND=int_kind), intent(in) :: var, ref_var
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
            END SUBROUTINE kgen_verify_integer_int_kind

            SUBROUTINE kgen_verify_integer_int_kind_dim1_ptr( varname, check_status, var, ref_var)
                character(*), intent(in) :: varname
                type(check_t), intent(inout) :: check_status
                integer(KIND=int_kind), intent(in), DIMENSION(:), POINTER :: var, ref_var
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
            END SUBROUTINE kgen_verify_integer_int_kind_dim1_ptr

        !***********************************************************************
        !BOP
        ! !IROUTINE: create_blocks
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: get_block
        ! !INTERFACE:

        FUNCTION get_block(block_id, local_id)
            ! !DESCRIPTION:
            !  This function returns the block data structure for the block
            !  associated with the input block id.
            !
            ! !REVISION HISTORY:
            !  same as module
            !
            ! !INPUT PARAMETERS:
            INTEGER(KIND=int_kind), intent(in) :: block_id
            INTEGER(KIND=int_kind), intent(in) :: local_id
            ! global block id for requested block info
            ! local  block id to assign to this block
            ! !OUTPUT PARAMETERS:
            TYPE(block) :: get_block
            ! block information returned for requested block
            !EOP
            !BOC
            !----------------------------------------------------------------------
            !
            !  check for valid id.  if valid, return block info for requested block
            !
            !----------------------------------------------------------------------
   if (block_id < 1 .or. block_id > nblocks_tot) then
!kgen_excluded       call exit_POP(sigAbort,'get_block: invalid block_id')
   endif
   get_block = all_blocks(block_id)
   get_block%local_id = local_id
            !----------------------------------------------------------------------
            !EOC
        END FUNCTION get_block
        !**********************************************************************
        !BOP
        ! !IROUTINE: get_block_parameter
        ! !INTERFACE:

        !**********************************************************************
        !BOP
        ! !IROUTINE: destroy_blocks
        ! !INTERFACE:

        !***********************************************************************
        !BOP
        ! !IROUTINE: get_block_ids_from_coords
        ! !INTERFACE:

        !***********************************************************************
    END MODULE blocks
