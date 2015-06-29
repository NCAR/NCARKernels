module mo_taugb01

    USE parrrtm, ONLY: ng1
    USE rrlw_kg01, ONLY: selfref
    USE rrlw_kg01, ONLY: forref
    USE rrlw_kg01, ONLY: ka_mn2
    USE rrlw_kg01, ONLY: absa
    USE rrlw_kg01, ONLY: fracrefa
    USE rrlw_kg01, ONLY: kb_mn2
    USE rrlw_kg01, ONLY: absb
    USE rrlw_kg01, ONLY: fracrefb



    IMPLICIT NONE
    PRIVATE
    PUBLIC taugb01_lwr,taugb01_upr
    CONTAINS
    SUBROUTINE taugb01_lwr(ncol, laytrop, nlayers, &
        pavel, scaleminor, scaleminorn2,colbrd &
        colh2o, fac00, fac01,fac10, fac11, minorfrac,&
        selffac,selffrac,forfac,forfrac,            &
        jp, jt, ig, indself, &
        indfor, indminor, &
        taug, fracs)
        IMPLICIT NONE

        real(kind=wp), PARAMETER :: oneminus = 1.0_wp - 1.0e-06_wp
        integer, intent(in) :: ncol                  ! number of simd columns
        integer, intent(in) :: laytrop               ! number of layers forwer atmosphere kernel
        integer, intent(in) :: nlayers               ! total number of layers
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: fac00, fac01, fac10, fac11    ! not sure of ncol depend
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: colh2o ! these appear to be gas concentrations

        real(kind=wp), intent(in), dimension(ncol,nlayers) :: selffac,selffrac
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: forfac,forfrac
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: pavel,scaleminor, scaleminorn2, colbrd
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: minorfrac

        ! Look up tables and related lookup indices
        ! I assume all lookup indices depend on 3D position
        ! =================================================

        integer, intent(in) :: jp(ncol,nlayers)      ! I assume jp depends on ncol
        integer, intent(in) :: jt(ncol,0:1,nlayers)  ! likewise for jt
        integer, intent(in) :: ig                    ! ig indexes into lookup tables
        integer, intent(in) :: indself(ncol,nlayers) ! self index array
        integer, intent(in) :: indfor(ncol,nlayers)  ! for index array
        integer, intent(in) :: indminor(ncol,nlayers) ! ka_mn2o index array
        real(kind=wp), intent(out), dimension(ncol,nlayers) :: taug  ! kernel result
        real(kind=wp), intent(out), dimension(ncol,nlayers) :: fracs ! kernel result

        ! Local variable
        ! ==============

        integer :: lay   ! layer index
        integer :: i     ! specparm types index
        integer :: icol  ! column index

        ! vector temporaries
        ! ====================

        integer, dimension(1:3,1:3) :: caseTypeOperations
        integer, dimension(ncol)    :: caseType
        real(kind=wp), dimension(ncol,0:1) :: tau_major
        real(kind=wp), dimension(ncol) :: taufor,tauself,corradj,scalen2
        integer, dimension(ncol) :: js, ind0, ind1, jpl

        ! Register temporaries
        ! ====================


        !dir$ assume_aligned jp:64
        !dir$ assume_aligned jt:64
        !dir$ assume_aligned colh2o:64
        !dir$ assume_aligned fac0:64
        !dir$ assume_aligned fac1:64
        !dir$ assume_aligned taug:64

        !dir$ assume_aligned indself:64
        !dir$ assume_aligned indfor:64
        !dir$ assume_aligned indminor:64
        !dir$ assume_aligned fs:64
        !dir$ assume_aligned tau_major:64

        !dir$ assume_aligned js:64
        !dir$ assume_aligned ind0:64
        !dir$ assume_aligned ind1:64


        !dir$ assume_aligned caseTypeOperations:64
        !dir$ assume_aligned caseType:64
        ! Lower atmosphere loop
        ! =====================

        DO lay = 1,laytrop  ! loop over layers

        ! Compute tau_major term
        ! ======================
            !dir$ SIMD
            DO icol=1,ncol  ! Vectorizes with dir 14.0.2
                ind0(icol) = ((jp(icol,lay)-1)*5+(jt(icol,lay)-1))*nspa(1) + 1
                ind1(icol) = (jp(icol,lay)*5+(jt1(icol,lay)-1))*nspa(1) + 1
                scalen2(icol) = colbrd(icol,lay) * scaleminorn2(icol,lay)
            END DO

            DO icol=1,ncol
                corradj(icol) =  1.
                if (pavel(icol,lay) .lt. 250._r8) then
                   corradj(icol) = 1._r8 - 0.15_r8 * (250._r8-pavel(icol,lay)) / 154.4_r8
                endif
            END DO

            DO ig =1,ng1
                !dir$ SIMD
                DO icol=1,ncol  ! Vectorizes with dir 14.0.2
                    tauself(icol) = selffac(icol,lay)*(selfref(indself(icol,lay),ig) +&
                        selffrac(icol,lay)*(selfref(indself(icol,lay)+1,ig)- selfref(indself(icol,lay),ig)))
                    taufor(icol) =  forfac(icol,lay)*( forref(indfor(icol,lay),ig) +&
                        forfrac(icol,lay)*( forref(indfor(icol,lay)+1,ig) -forref(indfor(icol,lay),ig)))
                    taun2(icol) = scalen2(icol)*(ka_mn2(icol,indminor(icol,lay),ig) + &
                        minorfrac(icol,lay) * (ka_mn2(indminor(icol,lay)+1,ig) - ka_mn2(indminor(icol,lay),ig)))
                    taug(icol,lay,ig) = corradj(icol) * (colh2o(icol,lay) * &
                        (fac00(icol,lay) * absa(ind0,ig) + &
                        fac10(icol,lay) * absa(ind0+1,ig) + &
                        fac01(icol,lay) * absa(ind1,ig) + &
                        fac11(icol,lay) * absa(ind1+1,ig)) &
                        + tauself(icol) + taufor(icol) + taun2(icol))
                    fracs(icol,lay,ig) = fracrefa(ig)
                END DO
            END DO


        END DO ! end lower atmosphere loop
    END SUBROUTINE taugb01_lwr

    CONTAINS
    SUBROUTINE taugb01_upr(ncol, laytrop, nlayers, &
        pavel, scaleminor, scaleminorn2,colbrd &
        colh2o, fac00, fac01,fac10, fac11, minorfrac,&
        selffac,selffrac,forfac,forfrac,            &
        jp, jt, ig, indself, &
        indfor, indminor, &
        taug, fracs)
        IMPLICIT NONE

        real(kind=wp), PARAMETER :: oneminus = 1.0_wp - 1.0e-06_wp
        integer, intent(in) :: ncol                  ! number of simd columns
        integer, intent(in) :: laytrop               ! number of layers forwer atmosphere kernel
        integer, intent(in) :: nlayers               ! total number of layers
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: fac00, fac01, fac10, fac11    ! not sure of ncol depend
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: colh2o ! these appear to be gas concentrations

        real(kind=wp), intent(in), dimension(ncol,nlayers) :: selffac,selffrac
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: forfac,forfrac
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: pavel,scaleminor, scaleminorn2, colbrd
        real(kind=wp), intent(in), dimension(ncol,nlayers) :: minorfrac

        ! Look up tables and related lookup indices
        ! I assume all lookup indices depend on 3D position
        ! =================================================

        integer, intent(in) :: jp(ncol,nlayers)      ! I assume jp depends on ncol
        integer, intent(in) :: jt(ncol,0:1,nlayers)  ! likewise for jt
        integer, intent(in) :: ig                    ! ig indexes into lookup tables
        integer, intent(in) :: indself(ncol,nlayers) ! self index array
        integer, intent(in) :: indfor(ncol,nlayers)  ! for index array
        integer, intent(in) :: indminor(ncol,nlayers) ! ka_mn2o index array
        real(kind=wp), intent(out), dimension(ncol,nlayers) :: taug  ! kernel result
        real(kind=wp), intent(out), dimension(ncol,nlayers) :: fracs ! kernel result

        ! Local variable
        ! ==============

        integer :: lay   ! layer index
        integer :: i     ! specparm types index
        integer :: icol  ! column index

        ! vector temporaries
        ! ====================

        integer, dimension(1:3,1:3) :: caseTypeOperations
        integer, dimension(ncol)    :: caseType
        real(kind=wp), dimension(ncol,0:1) :: tau_major
        real(kind=wp), dimension(ncol) :: taufor,tauself,corradj,scalen2
        integer, dimension(ncol) :: js, ind0, ind1, jpl

        ! Register temporaries
        ! ====================


        !dir$ assume_aligned jp:64
        !dir$ assume_aligned jt:64
        !dir$ assume_aligned colh2o:64
        !dir$ assume_aligned fac0:64
        !dir$ assume_aligned fac1:64
        !dir$ assume_aligned taug:64

        !dir$ assume_aligned indself:64
        !dir$ assume_aligned indfor:64
        !dir$ assume_aligned indminor:64
        !dir$ assume_aligned fs:64
        !dir$ assume_aligned tau_major:64

        !dir$ assume_aligned js:64
        !dir$ assume_aligned ind0:64
        !dir$ assume_aligned ind1:64


        !dir$ assume_aligned caseTypeOperations:64
        !dir$ assume_aligned caseType:64
        ! Lower atmosphere loop
        ! =====================
        DO lay = laytrop+1, nlayers
            !dir$ SIMD
            DO icol=1,ncol  ! Vectorizes with dir 14.0.2
                ind0(icol) = ((jp(icol,lay)-13)*5+(jt(icol,lay)-1))*nspb(1) + 1
                ind1(icol) = ((jp(icol,lay)-12)*5+(jt1(icol,lay)-1))*nspb(1) + 1
                scalen2(icol) = colbrd(icol,lay) * scaleminorn2(icol,lay)
                corradj(icol) =  1._r8 - 0.15_r8 * (pavel(icol,lay) / 95.6_r8)
            END DO

            DO ig =1,ng1
                !dir$ SIMD
                DO icol=1,ncol  ! Vectorizes with dir 14.0.2
                    taufor(icol) = forfac(icol,lay) * (forref(icol,indfor(icol,lay),ig) + &
                        forfrac(icol,lay) * (forref(icol,indfor(icol,lay)+1,ig) - forref(icol,indfor(icol,lay),ig)))
                    taun2 = scalen2(icol)*(kb_mn2(icol,indminor(icol,lay),ig) + &
                        minorfrac(icol,lay) * (kb_mn2(icol,indminor(icol,lay)+1,ig) - kb_mn2(icol,indminor(icol,lay),ig)))
                    taug(lay,ig) = corradj(icol) * (colh2o(icol,lay) * &
                        (fac00(icol,lay) * absb(icol,ind0,ig) + &
                        fac10(icol,lay) * absb(icol,ind0+1,ig) + &
                        fac01(icol,lay) * absb(icol,ind1,ig) + &
                        fac11(icol,lay) * absb(icol,ind1+1,ig)) &
                        + taufor(icol) + taun2(icol))
                    fracs(icol,lay,ig) = fracrefb(icol,ig)
                END DO
            END DO

        END DO
    END SUBROUTINE taugb01_upr
END module mo_taugb01
