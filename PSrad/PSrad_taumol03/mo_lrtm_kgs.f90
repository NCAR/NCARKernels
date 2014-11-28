    MODULE rrlw_planck
    USE mo_kind,        ONLY : wp

    REAL(wp) :: chi_mls(7,59)

    CONTAINS

    SUBROUTINE read_extern_rrlw_planck(kgen_unit)
    INTEGER, INTENT(IN)  :: kgen_unit

    READ(UNIT=kgen_unit) chi_mls

    END SUBROUTINE read_extern_rrlw_planck

    END MODULE rrlw_planck


    MODULE rrlw_kg03
    USE mo_kind ,ONLY : wp

    IMPLICIT NONE

    INTEGER, PARAMETER :: ng3  = 16

    REAL(wp) :: fracrefa(ng3,9) ,fracrefb(ng3,5)
    REAL(wp) :: ka(9,5,13,ng3)  ,absa(585,ng3)
    REAL(wp) :: kb(5,5,13:59,ng3),absb(1175,ng3)
    REAL(wp) :: ka_mn2o(9,19,ng3), kb_mn2o(5,19,ng3)
    REAL(wp) :: selfref(10,ng3)
    REAL(wp) :: forref(4,ng3)

    EQUIVALENCE (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))

    CONTAINS

    SUBROUTINE read_extern_rrlw_kg03(kgen_unit)
    INTEGER, INTENT(IN)  :: kgen_unit

    READ(UNIT=kgen_unit) fracrefa
    READ(UNIT=kgen_unit) fracrefb
    READ(UNIT=kgen_unit) ka
    READ(UNIT=kgen_unit) absa
    READ(UNIT=kgen_unit) kb
    READ(UNIT=kgen_unit) absb
    READ(UNIT=kgen_unit) ka_mn2o
    READ(UNIT=kgen_unit) kb_mn2o
    READ(UNIT=kgen_unit) selfref
    READ(UNIT=kgen_unit) forref

    END SUBROUTINE read_extern_rrlw_kg03

    END MODULE rrlw_kg03
