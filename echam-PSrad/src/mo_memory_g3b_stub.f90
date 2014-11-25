MODULE mo_memory_g3b

  USE mo_kind,          ONLY: wp
  USE mo_decomposition, ONLY: ldc=>local_decomposition
  IMPLICIT NONE

  PRIVATE

  PUBLIC :: construct_g3b ! routine to construct the g3b table
  PUBLIC :: destruct_g3b  ! routine to destruct  the g3b table

  ! declaration of predefined fields within this module

  REAL(wp), POINTER, PUBLIC :: geosp(:,:,:)

CONTAINS

  SUBROUTINE construct_g3b

    ALLOCATE(geosp(ldc%nlon,ldc%nlat,ldc%ntime))

  END SUBROUTINE construct_g3b

  SUBROUTINE destruct_g3b

    IF(ASSOCIATED(geosp)) DEALLOCATE(geosp)

  END SUBROUTINE destruct_g3b

END MODULE mo_memory_g3b
