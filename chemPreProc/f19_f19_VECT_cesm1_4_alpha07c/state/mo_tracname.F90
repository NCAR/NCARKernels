
      module mo_tracname
!-----------------------------------------------------------
!           surface fluxes for the advected species.
!-----------------------------------------------------------

      use chem_mods, only : grpcnt, gas_pcnst

      implicit none

      character(len=16) :: solsym(gas_pcnst)   ! species names

      PUBLIC kw_externs_in_mo_tracname
      
      CONTAINS
      
      !write in state subroutine for kw_externs_in_mo_tracname
      SUBROUTINE kw_externs_in_mo_tracname(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          REAL(KIND=8) :: kgen_array_sum
          
          IF (SIZE(solsym)==1) THEN
              IF (UBOUND(solsym, 1)<LBOUND(solsym, 1)) THEN
                  kgen_istrue = .FALSE.
              ELSE IF (UBOUND(solsym, 1)==0 .AND. LBOUND(solsym, 1)==0) THEN
                  kgen_istrue = .FALSE.
              ELSE
                  kgen_istrue = .TRUE.
              END IF 
          ELSE IF (SIZE(solsym)==0) THEN
              kgen_istrue = .FALSE.
          ELSE
              kgen_istrue = .TRUE.
          END IF 
          WRITE (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              WRITE (UNIT = kgen_unit) solsym
          END IF 
      END SUBROUTINE kw_externs_in_mo_tracname
      
      end module mo_tracname