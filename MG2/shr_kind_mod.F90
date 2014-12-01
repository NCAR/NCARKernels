!===============================================================================
! SVN $Id: shr_kind_mod.F90 41285 2012-10-26 01:46:45Z sacks $
! SVN $URL: https://svn-ccsm-models.cgd.ucar.edu/csm_share/branch_tags/share_ece_tags/share_ece_01_140723/shr/shr_kind_mod.F90 $
!===============================================================================

MODULE shr_kind_mod

   !----------------------------------------------------------------------------
   ! precision/kind constants add data public
   !----------------------------------------------------------------------------
   public
   integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) ! 8 byte real

END MODULE shr_kind_mod
