!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:41
!KGEN version : 0.6.2


      module mo_tracname
!-----------------------------------------------------------
!           surface fluxes for the advected species.
!-----------------------------------------------------------

          USE chem_mods, ONLY: gas_pcnst

          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          IMPLICIT NONE

      character(len=16) :: solsym(gas_pcnst)   ! species names

      PUBLIC kr_externs_in_mo_tracname
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_mo_tracname
      SUBROUTINE kr_externs_in_mo_tracname(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              READ (UNIT = kgen_unit) solsym
          END IF 
      END SUBROUTINE kr_externs_in_mo_tracname
      
      end module mo_tracname