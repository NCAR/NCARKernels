!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2






      module chem_mods
!--------------------------------------------------------------
! ... Basic chemistry parameters and arrays
!--------------------------------------------------------------


          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          IMPLICIT NONE

          SAVE

      integer, parameter :: phtcnt = 114, & ! number of photolysis reactions
                            rxntot = 472, & ! number of total reactions
                            gascnt = 358, & ! number of gas phase reactions
                            nabscol = 2, & ! number of absorbing column densities
                            gas_pcnst = 183, & ! number of "gas phase" species
                            nfs = 2, & ! number of "fixed" species
                            relcnt = 0, & ! number of relationship species
                            grpcnt = 0, & ! number of group members
                            nzcnt = 1471, & ! number of non-zero matrix entries
                            extcnt = 15, & ! number of species with external forcing
                            clscnt1 = 37, & ! number of species in explicit class
                            clscnt2 = 0, & ! number of species in hov class
                            clscnt3 = 0, & ! number of species in ebi class
                            clscnt4 = 146, & ! number of species in implicit class
                            clscnt5 = 0, & ! number of species in rodas class
                            indexm = 1, & ! index of total atm density in invariant array
                            indexh2o = 0, & ! index of water vapor density
                            clsze = 0, & ! loop length for implicit chemistry
                            rxt_tag_cnt = 472, &
                            enthalpy_cnt = 41, &
                            nslvd = 0

      integer :: cls_rxt_cnt(4,5) = 0
      integer :: clsmap(gas_pcnst,5) = 0
      integer :: permute(gas_pcnst,5) = 0









      PUBLIC kr_externs_in_chem_mods
      
      CONTAINS
      
      !read state subroutine for kr_externs_in_chem_mods
      SUBROUTINE kr_externs_in_chem_mods(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              READ (UNIT = kgen_unit) kgen_array_sum
              READ (UNIT = kgen_unit) cls_rxt_cnt
              CALL kgen_array_sumcheck("cls_rxt_cnt", kgen_array_sum, REAL(SUM(cls_rxt_cnt), 8), .TRUE.)
          END IF 
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              READ (UNIT = kgen_unit) kgen_array_sum
              READ (UNIT = kgen_unit) clsmap
              CALL kgen_array_sumcheck("clsmap", kgen_array_sum, REAL(SUM(clsmap), 8), .TRUE.)
          END IF 
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              READ (UNIT = kgen_unit) kgen_array_sum
              READ (UNIT = kgen_unit) permute
              CALL kgen_array_sumcheck("permute", kgen_array_sum, REAL(SUM(permute), 8), .TRUE.)
          END IF 
      END SUBROUTINE kr_externs_in_chem_mods
      
      end module chem_mods