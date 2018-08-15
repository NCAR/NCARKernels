!KGEN-generated Fortran source file

!Generated at : 2016-01-07 12:08:46
!KGEN version : 0.6.1

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
#if defined(USE_R4)
    INTEGER, parameter :: rkind_comp = selected_real_kind( 6) ! 4 byte real
    INTEGER, parameter :: rkind_io   = selected_real_kind(12) ! 8 byte real
#else
    INTEGER, parameter :: rkind_comp = selected_real_kind(12) ! 8 byte real
    INTEGER, parameter :: rkind_io   = selected_real_kind(12) ! 8 byte real
#endif


   integer,parameter :: SHR_KIND_IN = kind(1)                ! native integer

END MODULE shr_kind_mod
