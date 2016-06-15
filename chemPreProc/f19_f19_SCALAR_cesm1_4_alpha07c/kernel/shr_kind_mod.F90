!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:53
!KGEN version : 0.6.2

!===============================================================================
! SVN $Id: shr_kind_mod.F90 65994 2014-12-03 22:44:59Z cacraig@ucar.edu $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/trunk_tags/share3_150116/shr/shr_kind_mod.F90 $
!===============================================================================

MODULE shr_kind_mod

   !----------------------------------------------------------------------------
   ! precision/kind constants add data public
   !----------------------------------------------------------------------------
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    PUBLIC
   integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real
   integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer
   integer,parameter :: SHR_KIND_CX = 512                    ! extra-long char

END MODULE shr_kind_mod