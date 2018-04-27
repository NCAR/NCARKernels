!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2






      module mo_nln_matrix

          USE shr_kind_mod, ONLY: rkind_comp
          USE chem_mods, ONLY: nzcnt, gas_pcnst, rxntot
          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE ppgrid, ONLY: veclen
          PRIVATE
          PUBLIC nlnmat

      contains

      subroutine nlnmat01( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1223) = -(rxt(k,116)*y(k,2) + rxt(k,134)*y(k,3) + rxt(k,161)*y(k,22) &
                      + rxt(k,166)*y(k,23) + rxt(k,174)*y(k,24) + rxt(k,189)*y(k,9) &
                      + rxt(k,192)*y(k,10) + rxt(k,204)*y(k,28) + rxt(k,231)*y(k,37) &
                      + rxt(k,290)*y(k,44) + rxt(k,311)*y(k,59) + rxt(k,333)*y(k,77) &
                      + rxt(k,339)*y(k,78) + rxt(k,357)*y(k,83) + rxt(k,395)*y(k,105))
         mat(k,1464) = -rxt(k,116)*y(k,1)
         mat(k,1358) = -rxt(k,134)*y(k,1)
         mat(k,1261) = -rxt(k,161)*y(k,1)
         mat(k,1180) = -rxt(k,166)*y(k,1)
         mat(k,1334) = -rxt(k,174)*y(k,1)
         mat(k,976) = -rxt(k,189)*y(k,1)
         mat(k,1393) = -rxt(k,192)*y(k,1)
         mat(k,1427) = -rxt(k,204)*y(k,1)
         mat(k,918) = -rxt(k,231)*y(k,1)
         mat(k,260) = -rxt(k,290)*y(k,1)
         mat(k,605) = -rxt(k,311)*y(k,1)
         mat(k,760) = -rxt(k,333)*y(k,1)
         mat(k,651) = -rxt(k,339)*y(k,1)
         mat(k,547) = -rxt(k,357)*y(k,1)
         mat(k,324) = -rxt(k,395)*y(k,1)

         mat(k,1223) = mat(k,1223) + .100_rkind_comp*rxt(k,357)*y(k,83) + .200_rkind_comp*rxt(k,333) &
                      *y(k,77) + .200_rkind_comp*rxt(k,339)*y(k,78)
         mat(k,1464) = mat(k,1464) + rxt(k,115)*y(k,4)
         mat(k,846) = rxt(k,115)*y(k,2)
         mat(k,1334) = mat(k,1334) + .250_rkind_comp*rxt(k,301)*y(k,48) + .250_rkind_comp*rxt(k,349) &
                      *y(k,76)
         mat(k,547) = mat(k,547) + .100_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,807) = .250_rkind_comp*rxt(k,301)*y(k,24)
         mat(k,760) = mat(k,760) + .200_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,651) = mat(k,651) + .200_rkind_comp*rxt(k,339)*y(k,1)
         mat(k,779) = .250_rkind_comp*rxt(k,349)*y(k,24)

         mat(k,1471) = -(rxt(k,115)*y(k,4) + rxt(k,116)*y(k,1) + 4._rkind_comp*rxt(k,117) &
                      *y(k,2) + rxt(k,165)*y(k,23) + rxt(k,172)*y(k,21) + rxt(k,173) &
                      *y(k,24) + rxt(k,176)*y(k,25) + rxt(k,187)*y(k,9) + (rxt(k,190) &
                      + rxt(k,191)) * y(k,10) + rxt(k,198)*y(k,11) + rxt(k,211) &
                      *y(k,30) + rxt(k,224)*y(k,33) + rxt(k,225)*y(k,34) + rxt(k,228) &
                      *y(k,35) + rxt(k,234)*y(k,38) + rxt(k,244)*y(k,39) + rxt(k,245) &
                      *y(k,40) + rxt(k,246)*y(k,41) + rxt(k,268)*y(k,19) + (rxt(k,451) &
                      + rxt(k,452)) * y(k,127) + rxt(k,458)*y(k,129))
         mat(k,852) = -rxt(k,115)*y(k,2)
         mat(k,1230) = -rxt(k,116)*y(k,2)
         mat(k,1187) = -rxt(k,165)*y(k,2)
         mat(k,667) = -rxt(k,172)*y(k,2)
         mat(k,1341) = -rxt(k,173)*y(k,2)
         mat(k,344) = -rxt(k,176)*y(k,2)
         mat(k,983) = -rxt(k,187)*y(k,2)
         mat(k,1400) = -(rxt(k,190) + rxt(k,191)) * y(k,2)
         mat(k,1024) = -rxt(k,198)*y(k,2)
         mat(k,904) = -rxt(k,211)*y(k,2)
         mat(k,1088) = -rxt(k,224)*y(k,2)
         mat(k,517) = -rxt(k,225)*y(k,2)
         mat(k,590) = -rxt(k,228)*y(k,2)
         mat(k,829) = -rxt(k,234)*y(k,2)
         mat(k,483) = -rxt(k,244)*y(k,2)
         mat(k,409) = -rxt(k,245)*y(k,2)
         mat(k,278) = -rxt(k,246)*y(k,2)
         mat(k,1251) = -rxt(k,268)*y(k,2)
         mat(k,375) = -(rxt(k,451) + rxt(k,452)) * y(k,2)
         mat(k,229) = -rxt(k,458)*y(k,2)

         mat(k,1365) = (rxt(k,129)+rxt(k,130))*y(k,4)
         mat(k,852) = mat(k,852) + (rxt(k,129)+rxt(k,130))*y(k,3) + rxt(k,182)*y(k,8) &
                      + rxt(k,457)*y(k,129) + rxt(k,449)*y(k,130)
         mat(k,497) = rxt(k,182)*y(k,4) + rxt(k,183)*y(k,9) + rxt(k,184)*y(k,10) &
                      + rxt(k,454)*y(k,128)
         mat(k,983) = mat(k,983) + rxt(k,183)*y(k,8)
         mat(k,1400) = mat(k,1400) + rxt(k,184)*y(k,8)
         mat(k,1187) = mat(k,1187) + 2.000_rkind_comp*rxt(k,168)*y(k,23)
         mat(k,1268) = rxt(k,164)*y(k,24)
         mat(k,1341) = mat(k,1341) + rxt(k,164)*y(k,22)
         mat(k,427) = rxt(k,454)*y(k,8) + 1.150_rkind_comp*rxt(k,462)*y(k,132)
         mat(k,229) = mat(k,229) + rxt(k,457)*y(k,4)
         mat(k,337) = rxt(k,449)*y(k,4)
         mat(k,435) = rxt(k,461)*y(k,132)
         mat(k,446) = 1.150_rkind_comp*rxt(k,462)*y(k,128) + rxt(k,461)*y(k,131)

         mat(k,1362) = -((rxt(k,129) + rxt(k,130)) * y(k,4) + rxt(k,131)*y(k,134) &
                      + rxt(k,134)*y(k,1) + rxt(k,151)*y(k,138) + rxt(k,152)*y(k,139) &
                      + rxt(k,156)*y(k,21) + rxt(k,157)*y(k,33) + rxt(k,158)*y(k,39) &
                      + rxt(k,159)*y(k,42))
         mat(k,850) = -(rxt(k,129) + rxt(k,130)) * y(k,3)
         mat(k,863) = -rxt(k,131)*y(k,3)
         mat(k,1227) = -rxt(k,134)*y(k,3)
         mat(k,74) = -rxt(k,151)*y(k,3)
         mat(k,88) = -rxt(k,152)*y(k,3)
         mat(k,665) = -rxt(k,156)*y(k,3)
         mat(k,1085) = -rxt(k,157)*y(k,3)
         mat(k,482) = -rxt(k,158)*y(k,3)
         mat(k,93) = -rxt(k,159)*y(k,3)

         mat(k,850) = mat(k,850) + rxt(k,179)*y(k,133)
         mat(k,426) = .850_rkind_comp*rxt(k,462)*y(k,132)
         mat(k,254) = rxt(k,179)*y(k,4)
         mat(k,445) = .850_rkind_comp*rxt(k,462)*y(k,128)

         mat(k,843) = -(rxt(k,115)*y(k,2) + rxt(k,125)*y(k,6) + rxt(k,129)*y(k,3) &
                      + rxt(k,160)*y(k,22) + rxt(k,179)*y(k,133) + rxt(k,182)*y(k,8) &
                      + rxt(k,288)*y(k,56) + rxt(k,449)*y(k,130) + (rxt(k,456) &
                      + rxt(k,457)) * y(k,129) + rxt(k,459)*y(k,127))
         mat(k,1454) = -rxt(k,115)*y(k,4)
         mat(k,76) = -rxt(k,125)*y(k,4)
         mat(k,1349) = -rxt(k,129)*y(k,4)
         mat(k,1253) = -rxt(k,160)*y(k,4)
         mat(k,252) = -rxt(k,179)*y(k,4)
         mat(k,491) = -rxt(k,182)*y(k,4)
         mat(k,205) = -rxt(k,288)*y(k,4)
         mat(k,336) = -rxt(k,449)*y(k,4)
         mat(k,228) = -(rxt(k,456) + rxt(k,457)) * y(k,4)
         mat(k,372) = -rxt(k,459)*y(k,4)

         mat(k,1213) = 2.000_rkind_comp*rxt(k,116)*y(k,2) + 2.000_rkind_comp*rxt(k,134)*y(k,3) &
                      + rxt(k,189)*y(k,9) + rxt(k,192)*y(k,10) + rxt(k,166)*y(k,23) &
                      + rxt(k,161)*y(k,22) + 2.000_rkind_comp*rxt(k,174)*y(k,24) + rxt(k,204) &
                      *y(k,28) + rxt(k,231)*y(k,37)
         mat(k,1454) = mat(k,1454) + 2.000_rkind_comp*rxt(k,116)*y(k,1) + 2.000_rkind_comp*rxt(k,117) &
                      *y(k,2) + rxt(k,124)*y(k,6) + rxt(k,190)*y(k,10) + rxt(k,165) &
                      *y(k,23) + rxt(k,198)*y(k,11) + rxt(k,173)*y(k,24) + rxt(k,211) &
                      *y(k,30) + rxt(k,234)*y(k,38)
         mat(k,1349) = mat(k,1349) + 2.000_rkind_comp*rxt(k,134)*y(k,1)
         mat(k,843) = mat(k,843) + 2.000_rkind_comp*rxt(k,125)*y(k,6)
         mat(k,76) = mat(k,76) + rxt(k,124)*y(k,2) + 2.000_rkind_comp*rxt(k,125)*y(k,4)
         mat(k,491) = mat(k,491) + rxt(k,186)*y(k,10)
         mat(k,966) = rxt(k,189)*y(k,1) + rxt(k,455)*y(k,128)
         mat(k,1383) = rxt(k,192)*y(k,1) + rxt(k,190)*y(k,2) + rxt(k,186)*y(k,8)
         mat(k,1170) = rxt(k,166)*y(k,1) + rxt(k,165)*y(k,2) + rxt(k,202)*y(k,13) &
                      + rxt(k,167)*y(k,24) + rxt(k,213)*y(k,30)
         mat(k,1009) = rxt(k,198)*y(k,2) + rxt(k,200)*y(k,24)
         mat(k,231) = rxt(k,202)*y(k,23)
         mat(k,1047) = rxt(k,271)*y(k,24)
         mat(k,1253) = mat(k,1253) + rxt(k,161)*y(k,1) + rxt(k,163)*y(k,24)
         mat(k,1324) = 2.000_rkind_comp*rxt(k,174)*y(k,1) + rxt(k,173)*y(k,2) + rxt(k,167) &
                      *y(k,23) + rxt(k,200)*y(k,11) + rxt(k,271)*y(k,16) + rxt(k,163) &
                      *y(k,22) + 2.000_rkind_comp*rxt(k,175)*y(k,24) + rxt(k,207)*y(k,28) &
                      + rxt(k,214)*y(k,30) + rxt(k,232)*y(k,37) + rxt(k,236)*y(k,38) &
                      + rxt(k,319)*y(k,64) + .750_rkind_comp*rxt(k,349)*y(k,76) + rxt(k,293) &
                      *y(k,46) + rxt(k,314)*y(k,61) + rxt(k,323)*y(k,67)
         mat(k,1417) = rxt(k,204)*y(k,1) + rxt(k,207)*y(k,24)
         mat(k,887) = rxt(k,211)*y(k,2) + rxt(k,213)*y(k,23) + rxt(k,214)*y(k,24) + ( &
                      + 2.000_rkind_comp*rxt(k,218)+2.000_rkind_comp*rxt(k,219))*y(k,30) + (rxt(k,240) &
                       +rxt(k,241))*y(k,38)
         mat(k,908) = rxt(k,231)*y(k,1) + rxt(k,232)*y(k,24)
         mat(k,817) = rxt(k,234)*y(k,2) + rxt(k,236)*y(k,24) + (rxt(k,240)+rxt(k,241)) &
                      *y(k,30) + 2.000_rkind_comp*rxt(k,242)*y(k,38)
         mat(k,424) = rxt(k,455)*y(k,9)
         mat(k,470) = rxt(k,319)*y(k,24)
         mat(k,772) = .750_rkind_comp*rxt(k,349)*y(k,24)
         mat(k,503) = rxt(k,293)*y(k,24)
         mat(k,524) = rxt(k,314)*y(k,24)
         mat(k,629) = rxt(k,323)*y(k,24)

         mat(k,78) = -(rxt(k,118)*y(k,2) + rxt(k,119)*y(k,4) + rxt(k,121)*y(k,1))
         mat(k,1436) = -rxt(k,118)*y(k,5)
         mat(k,831) = -rxt(k,119)*y(k,5)
         mat(k,1189) = -rxt(k,121)*y(k,5)

         mat(k,1343) = rxt(k,129)*y(k,4)
         mat(k,831) = mat(k,831) + rxt(k,129)*y(k,3)

         mat(k,75) = -(rxt(k,124)*y(k,2) + rxt(k,125)*y(k,4))
         mat(k,1435) = -rxt(k,124)*y(k,6)
         mat(k,830) = -rxt(k,125)*y(k,6)

         mat(k,1188) = rxt(k,121)*y(k,5)
         mat(k,1435) = mat(k,1435) + rxt(k,118)*y(k,5)
         mat(k,830) = mat(k,830) + rxt(k,119)*y(k,5)
         mat(k,77) = rxt(k,121)*y(k,1) + rxt(k,118)*y(k,2) + rxt(k,119)*y(k,4)

         mat(k,657) = -(rxt(k,156)*y(k,3) + rxt(k,170)*y(k,23) + rxt(k,172)*y(k,2) &
                      + rxt(k,205)*y(k,28) + rxt(k,248)*y(k,141))
         mat(k,1348) = -rxt(k,156)*y(k,21)
         mat(k,1161) = -rxt(k,170)*y(k,21)
         mat(k,1452) = -rxt(k,172)*y(k,21)
         mat(k,1415) = -rxt(k,205)*y(k,21)
         mat(k,449) = -rxt(k,248)*y(k,21)

         mat(k,1252) = rxt(k,163)*y(k,24)
         mat(k,1315) = rxt(k,163)*y(k,22)

         mat(k,609) = -((rxt(k,264) + rxt(k,265)) * y(k,23))
         mat(k,1156) = -(rxt(k,264) + rxt(k,265)) * y(k,20)

         mat(k,1200) = .560_rkind_comp*rxt(k,311)*y(k,59) + .300_rkind_comp*rxt(k,357)*y(k,83) &
                      + .500_rkind_comp*rxt(k,290)*y(k,44) + .050_rkind_comp*rxt(k,333)*y(k,77) &
                      + .200_rkind_comp*rxt(k,339)*y(k,78)
         mat(k,1451) = rxt(k,268)*y(k,19)
         mat(k,953) = .220_rkind_comp*rxt(k,340)*y(k,79) + .250_rkind_comp*rxt(k,375)*y(k,91)
         mat(k,1156) = mat(k,1156) + rxt(k,267)*y(k,19) + rxt(k,306)*y(k,53) &
                      + rxt(k,327)*y(k,68) + .350_rkind_comp*rxt(k,283)*y(k,135)
         mat(k,997) = rxt(k,266)*y(k,19) + .220_rkind_comp*rxt(k,342)*y(k,79) + rxt(k,328) &
                      *y(k,68) + .500_rkind_comp*rxt(k,376)*y(k,91)
         mat(k,1035) = .110_rkind_comp*rxt(k,344)*y(k,79) + .200_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,1234) = rxt(k,268)*y(k,2) + rxt(k,267)*y(k,23) + rxt(k,266)*y(k,11) &
                      + rxt(k,209)*y(k,28) + rxt(k,233)*y(k,37)
         mat(k,1413) = rxt(k,209)*y(k,19)
         mat(k,906) = rxt(k,233)*y(k,19)
         mat(k,595) = .560_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,537) = .300_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,790) = .220_rkind_comp*rxt(k,345)*y(k,79) + .250_rkind_comp*rxt(k,379)*y(k,91)
         mat(k,258) = .500_rkind_comp*rxt(k,290)*y(k,1)
         mat(k,398) = rxt(k,306)*y(k,23)
         mat(k,745) = .050_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,643) = .200_rkind_comp*rxt(k,339)*y(k,1)
         mat(k,727) = .220_rkind_comp*rxt(k,340)*y(k,9) + .220_rkind_comp*rxt(k,342)*y(k,11) &
                      + .110_rkind_comp*rxt(k,344)*y(k,16) + .220_rkind_comp*rxt(k,345)*y(k,48)
         mat(k,668) = rxt(k,327)*y(k,23) + rxt(k,328)*y(k,11)
         mat(k,681) = .250_rkind_comp*rxt(k,375)*y(k,9) + .500_rkind_comp*rxt(k,376)*y(k,11) &
                      + .200_rkind_comp*rxt(k,378)*y(k,16) + .250_rkind_comp*rxt(k,379)*y(k,48)
         mat(k,123) = .350_rkind_comp*rxt(k,283)*y(k,23)

         mat(k,489) = -(rxt(k,181)*y(k,23) + rxt(k,182)*y(k,4) + rxt(k,183)*y(k,9) &
                      + (rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,10) + rxt(k,454) &
                      *y(k,128))
         mat(k,1146) = -rxt(k,181)*y(k,8)
         mat(k,840) = -rxt(k,182)*y(k,8)
         mat(k,947) = -rxt(k,183)*y(k,8)
         mat(k,1375) = -(rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,8)
         mat(k,423) = -rxt(k,454)*y(k,8)

         mat(k,1448) = rxt(k,458)*y(k,129) + rxt(k,180)*y(k,133)
         mat(k,840) = mat(k,840) + rxt(k,456)*y(k,129)
         mat(k,370) = 1.100_rkind_comp*rxt(k,463)*y(k,132)
         mat(k,227) = rxt(k,458)*y(k,2) + rxt(k,456)*y(k,4)
         mat(k,431) = .200_rkind_comp*rxt(k,461)*y(k,132)
         mat(k,251) = rxt(k,180)*y(k,2)
         mat(k,441) = 1.100_rkind_comp*rxt(k,463)*y(k,127) + .200_rkind_comp*rxt(k,461)*y(k,131)

      end do

      end subroutine nlnmat01

      subroutine nlnmat02( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,971) = -(rxt(k,183)*y(k,8) + rxt(k,187)*y(k,2) + rxt(k,188)*y(k,24) &
                      + rxt(k,189)*y(k,1) + rxt(k,197)*y(k,11) + rxt(k,216)*y(k,30) &
                      + rxt(k,237)*y(k,38) + rxt(k,270)*y(k,16) + rxt(k,278)*y(k,137) &
                      + rxt(k,286)*y(k,55) + rxt(k,292)*y(k,46) + rxt(k,299)*y(k,48) &
                      + rxt(k,313)*y(k,61) + rxt(k,318)*y(k,64) + rxt(k,322)*y(k,67) &
                      + rxt(k,331)*y(k,73) + rxt(k,335)*y(k,74) + (rxt(k,340) &
                      + rxt(k,341)) * y(k,79) + rxt(k,347)*y(k,76) + rxt(k,359) &
                      *y(k,88) + rxt(k,365)*y(k,89) + rxt(k,372)*y(k,84) + rxt(k,375) &
                      *y(k,91) + rxt(k,383)*y(k,96) + rxt(k,390)*y(k,100) + rxt(k,393) &
                      *y(k,103) + rxt(k,397)*y(k,106) + rxt(k,455)*y(k,128))
         mat(k,492) = -rxt(k,183)*y(k,9)
         mat(k,1459) = -rxt(k,187)*y(k,9)
         mat(k,1329) = -rxt(k,188)*y(k,9)
         mat(k,1218) = -rxt(k,189)*y(k,9)
         mat(k,1012) = -rxt(k,197)*y(k,9)
         mat(k,892) = -rxt(k,216)*y(k,9)
         mat(k,822) = -rxt(k,237)*y(k,9)
         mat(k,1052) = -rxt(k,270)*y(k,9)
         mat(k,188) = -rxt(k,278)*y(k,9)
         mat(k,415) = -rxt(k,286)*y(k,9)
         mat(k,504) = -rxt(k,292)*y(k,9)
         mat(k,803) = -rxt(k,299)*y(k,9)
         mat(k,526) = -rxt(k,313)*y(k,9)
         mat(k,472) = -rxt(k,318)*y(k,9)
         mat(k,631) = -rxt(k,322)*y(k,9)
         mat(k,135) = -rxt(k,331)*y(k,9)
         mat(k,360) = -rxt(k,335)*y(k,9)
         mat(k,736) = -(rxt(k,340) + rxt(k,341)) * y(k,9)
         mat(k,775) = -rxt(k,347)*y(k,9)
         mat(k,714) = -rxt(k,359)*y(k,9)
         mat(k,460) = -rxt(k,365)*y(k,9)
         mat(k,386) = -rxt(k,372)*y(k,9)
         mat(k,688) = -rxt(k,375)*y(k,9)
         mat(k,308) = -rxt(k,383)*y(k,9)
         mat(k,171) = -rxt(k,390)*y(k,9)
         mat(k,220) = -rxt(k,393)*y(k,9)
         mat(k,574) = -rxt(k,397)*y(k,9)
         mat(k,425) = -rxt(k,455)*y(k,9)

         mat(k,1459) = mat(k,1459) + rxt(k,190)*y(k,10)
         mat(k,844) = rxt(k,182)*y(k,8) + rxt(k,179)*y(k,133)
         mat(k,492) = mat(k,492) + rxt(k,182)*y(k,4) + 2.000_rkind_comp*rxt(k,185)*y(k,10) &
                      + rxt(k,181)*y(k,23)
         mat(k,1388) = rxt(k,190)*y(k,2) + 2.000_rkind_comp*rxt(k,185)*y(k,8)
         mat(k,1175) = rxt(k,181)*y(k,8)
         mat(k,253) = rxt(k,179)*y(k,4)

         mat(k,1398) = -((rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,8) + (rxt(k,190) &
                      + rxt(k,191)) * y(k,2) + rxt(k,192)*y(k,1) + rxt(k,193)*y(k,11) &
                      + rxt(k,195)*y(k,23) + rxt(k,201)*y(k,24) + rxt(k,217)*y(k,30) &
                      + rxt(k,238)*y(k,38) + rxt(k,300)*y(k,48) + rxt(k,353)*y(k,76) &
                      + rxt(k,387)*y(k,98))
         mat(k,496) = -(rxt(k,184) + rxt(k,185) + rxt(k,186)) * y(k,10)
         mat(k,1469) = -(rxt(k,190) + rxt(k,191)) * y(k,10)
         mat(k,1228) = -rxt(k,192)*y(k,10)
         mat(k,1022) = -rxt(k,193)*y(k,10)
         mat(k,1185) = -rxt(k,195)*y(k,10)
         mat(k,1339) = -rxt(k,201)*y(k,10)
         mat(k,902) = -rxt(k,217)*y(k,10)
         mat(k,827) = -rxt(k,238)*y(k,10)
         mat(k,811) = -rxt(k,300)*y(k,10)
         mat(k,783) = -rxt(k,353)*y(k,10)
         mat(k,85) = -rxt(k,387)*y(k,10)

         mat(k,1228) = mat(k,1228) + rxt(k,189)*y(k,9)
         mat(k,1469) = mat(k,1469) + rxt(k,187)*y(k,9) + rxt(k,198)*y(k,11)
         mat(k,981) = rxt(k,189)*y(k,1) + rxt(k,187)*y(k,2) + 2.000_rkind_comp*rxt(k,197) &
                      *y(k,11) + rxt(k,270)*y(k,16) + rxt(k,188)*y(k,24) + rxt(k,216) &
                      *y(k,30) + rxt(k,237)*y(k,38) + rxt(k,318)*y(k,64) + rxt(k,299) &
                      *y(k,48) + rxt(k,331)*y(k,73) + .900_rkind_comp*rxt(k,372)*y(k,84) &
                      + rxt(k,335)*y(k,74) + .900_rkind_comp*rxt(k,383)*y(k,96) + rxt(k,397) &
                      *y(k,106) + .900_rkind_comp*rxt(k,390)*y(k,100) + .900_rkind_comp*rxt(k,393) &
                      *y(k,103) + .920_rkind_comp*rxt(k,359)*y(k,88) + rxt(k,340)*y(k,79) &
                      + rxt(k,347)*y(k,76) + rxt(k,292)*y(k,46) + rxt(k,313)*y(k,61) &
                      + rxt(k,286)*y(k,55) + rxt(k,322)*y(k,67) + 1.206_rkind_comp*rxt(k,365) &
                      *y(k,89) + rxt(k,375)*y(k,91) + rxt(k,278)*y(k,137)
         mat(k,1398) = mat(k,1398) + .700_rkind_comp*rxt(k,387)*y(k,98)
         mat(k,1185) = mat(k,1185) + rxt(k,199)*y(k,11) + rxt(k,202)*y(k,13) &
                      + rxt(k,329)*y(k,82) + .400_rkind_comp*rxt(k,369)*y(k,90)
         mat(k,1022) = mat(k,1022) + rxt(k,198)*y(k,2) + 2.000_rkind_comp*rxt(k,197)*y(k,9) &
                      + rxt(k,199)*y(k,23) + rxt(k,200)*y(k,24) + rxt(k,360)*y(k,88) &
                      + rxt(k,342)*y(k,79) + rxt(k,348)*y(k,76) + rxt(k,396)*y(k,105) &
                      + 1.206_rkind_comp*rxt(k,366)*y(k,89) + rxt(k,370)*y(k,90) + rxt(k,376) &
                      *y(k,91)
         mat(k,236) = rxt(k,202)*y(k,23)
         mat(k,1062) = rxt(k,270)*y(k,9)
         mat(k,1339) = mat(k,1339) + rxt(k,188)*y(k,9) + rxt(k,200)*y(k,11) &
                      + .206_rkind_comp*rxt(k,367)*y(k,89)
         mat(k,902) = mat(k,902) + rxt(k,216)*y(k,9)
         mat(k,827) = mat(k,827) + rxt(k,237)*y(k,9)
         mat(k,476) = rxt(k,318)*y(k,9)
         mat(k,811) = mat(k,811) + rxt(k,299)*y(k,9)
         mat(k,160) = rxt(k,329)*y(k,23)
         mat(k,138) = rxt(k,331)*y(k,9)
         mat(k,390) = .900_rkind_comp*rxt(k,372)*y(k,9)
         mat(k,363) = rxt(k,335)*y(k,9)
         mat(k,311) = .900_rkind_comp*rxt(k,383)*y(k,9)
         mat(k,85) = mat(k,85) + .700_rkind_comp*rxt(k,387)*y(k,10)
         mat(k,578) = rxt(k,397)*y(k,9)
         mat(k,173) = .900_rkind_comp*rxt(k,390)*y(k,9)
         mat(k,222) = .900_rkind_comp*rxt(k,393)*y(k,9)
         mat(k,722) = .920_rkind_comp*rxt(k,359)*y(k,9) + rxt(k,360)*y(k,11)
         mat(k,743) = rxt(k,340)*y(k,9) + rxt(k,342)*y(k,11)
         mat(k,783) = mat(k,783) + rxt(k,347)*y(k,9) + rxt(k,348)*y(k,11)
         mat(k,509) = rxt(k,292)*y(k,9)
         mat(k,326) = rxt(k,396)*y(k,11)
         mat(k,531) = rxt(k,313)*y(k,9)
         mat(k,419) = rxt(k,286)*y(k,9)
         mat(k,636) = rxt(k,322)*y(k,9)
         mat(k,465) = 1.206_rkind_comp*rxt(k,365)*y(k,9) + 1.206_rkind_comp*rxt(k,366)*y(k,11) &
                      + .206_rkind_comp*rxt(k,367)*y(k,24)
         mat(k,565) = .400_rkind_comp*rxt(k,369)*y(k,23) + rxt(k,370)*y(k,11)
         mat(k,695) = rxt(k,375)*y(k,9) + rxt(k,376)*y(k,11)
         mat(k,191) = rxt(k,278)*y(k,9)

         mat(k,1179) = -(rxt(k,165)*y(k,2) + rxt(k,166)*y(k,1) + rxt(k,167)*y(k,24) &
                      + (4._rkind_comp*rxt(k,168) + 4._rkind_comp*rxt(k,169)) * y(k,23) + rxt(k,170) &
                      *y(k,21) + rxt(k,171)*y(k,25) + rxt(k,177)*y(k,42) + rxt(k,178) &
                      *y(k,43) + rxt(k,181)*y(k,8) + rxt(k,195)*y(k,10) + rxt(k,196) &
                      *y(k,12) + rxt(k,199)*y(k,11) + rxt(k,202)*y(k,13) + (rxt(k,212) &
                      + rxt(k,213)) * y(k,30) + rxt(k,223)*y(k,33) + rxt(k,227) &
                      *y(k,34) + rxt(k,229)*y(k,35) + rxt(k,235)*y(k,38) + rxt(k,243) &
                      *y(k,39) + (rxt(k,264) + rxt(k,265)) * y(k,20) + rxt(k,267) &
                      *y(k,19) + rxt(k,274)*y(k,18) + rxt(k,275)*y(k,17) + rxt(k,276) &
                      *y(k,136) + rxt(k,283)*y(k,135) + rxt(k,284)*y(k,45) + rxt(k,285) &
                      *y(k,44) + rxt(k,291)*y(k,49) + rxt(k,296)*y(k,47) + rxt(k,297) &
                      *y(k,50) + rxt(k,304)*y(k,54) + rxt(k,305)*y(k,52) + rxt(k,306) &
                      *y(k,53) + rxt(k,307)*y(k,51) + rxt(k,309)*y(k,58) + rxt(k,310) &
                      *y(k,59) + rxt(k,316)*y(k,62) + rxt(k,317)*y(k,60) + rxt(k,320) &
                      *y(k,65) + rxt(k,321)*y(k,63) + rxt(k,325)*y(k,69) + rxt(k,326) &
                      *y(k,66) + rxt(k,327)*y(k,68) + rxt(k,329)*y(k,82) + rxt(k,330) &
                      *y(k,70) + rxt(k,332)*y(k,77) + rxt(k,334)*y(k,72) + rxt(k,337) &
                      *y(k,75) + rxt(k,338)*y(k,78) + rxt(k,346)*y(k,80) + rxt(k,355) &
                      *y(k,81) + rxt(k,356)*y(k,83) + rxt(k,362)*y(k,93) + rxt(k,368) &
                      *y(k,71) + rxt(k,369)*y(k,90) + rxt(k,371)*y(k,87) + rxt(k,374) &
                      *y(k,85) + rxt(k,380)*y(k,92) + rxt(k,382)*y(k,94) + rxt(k,385) &
                      *y(k,97) + rxt(k,386)*y(k,95) + rxt(k,388)*y(k,99) + rxt(k,391) &
                      *y(k,102) + rxt(k,394)*y(k,105) + rxt(k,399)*y(k,107) + rxt(k,404) &
                      *y(k,156) + (rxt(k,405) + rxt(k,406)) * y(k,157) + rxt(k,408) &
                      *y(k,159))
         mat(k,1463) = -rxt(k,165)*y(k,23)
         mat(k,1222) = -rxt(k,166)*y(k,23)
         mat(k,1333) = -rxt(k,167)*y(k,23)
         mat(k,663) = -rxt(k,170)*y(k,23)
         mat(k,341) = -rxt(k,171)*y(k,23)
         mat(k,91) = -rxt(k,177)*y(k,23)
         mat(k,49) = -rxt(k,178)*y(k,23)
         mat(k,493) = -rxt(k,181)*y(k,23)
         mat(k,1392) = -rxt(k,195)*y(k,23)
         mat(k,874) = -rxt(k,196)*y(k,23)
         mat(k,1016) = -rxt(k,199)*y(k,23)
         mat(k,234) = -rxt(k,202)*y(k,23)
         mat(k,896) = -(rxt(k,212) + rxt(k,213)) * y(k,23)
         mat(k,1080) = -rxt(k,223)*y(k,23)
         mat(k,515) = -rxt(k,227)*y(k,23)
         mat(k,587) = -rxt(k,229)*y(k,23)
         mat(k,825) = -rxt(k,235)*y(k,23)
         mat(k,480) = -rxt(k,243)*y(k,23)
         mat(k,610) = -(rxt(k,264) + rxt(k,265)) * y(k,23)
         mat(k,1243) = -rxt(k,267)*y(k,23)
         mat(k,268) = -rxt(k,274)*y(k,23)
         mat(k,195) = -rxt(k,275)*y(k,23)
         mat(k,265) = -rxt(k,276)*y(k,23)
         mat(k,124) = -rxt(k,283)*y(k,23)
         mat(k,118) = -rxt(k,284)*y(k,23)
         mat(k,259) = -rxt(k,285)*y(k,23)
         mat(k,330) = -rxt(k,291)*y(k,23)
         mat(k,152) = -rxt(k,296)*y(k,23)
         mat(k,620) = -rxt(k,297)*y(k,23)
         mat(k,248) = -rxt(k,304)*y(k,23)
         mat(k,554) = -rxt(k,305)*y(k,23)
         mat(k,399) = -rxt(k,306)*y(k,23)
         mat(k,108) = -rxt(k,307)*y(k,23)
         mat(k,241) = -rxt(k,309)*y(k,23)
         mat(k,604) = -rxt(k,310)*y(k,23)
         mat(k,178) = -rxt(k,316)*y(k,23)
         mat(k,54) = -rxt(k,317)*y(k,23)
         mat(k,292) = -rxt(k,320)*y(k,23)
         mat(k,396) = -rxt(k,321)*y(k,23)
         mat(k,184) = -rxt(k,325)*y(k,23)
         mat(k,640) = -rxt(k,326)*y(k,23)
         mat(k,674) = -rxt(k,327)*y(k,23)
         mat(k,159) = -rxt(k,329)*y(k,23)
         mat(k,38) = -rxt(k,330)*y(k,23)
         mat(k,759) = -rxt(k,332)*y(k,23)
         mat(k,202) = -rxt(k,334)*y(k,23)
         mat(k,143) = -rxt(k,337)*y(k,23)
         mat(k,650) = -rxt(k,338)*y(k,23)
         mat(k,147) = -rxt(k,346)*y(k,23)
         mat(k,283) = -rxt(k,355)*y(k,23)
         mat(k,546) = -rxt(k,356)*y(k,23)
         mat(k,300) = -rxt(k,362)*y(k,23)
         mat(k,35) = -rxt(k,368)*y(k,23)
         mat(k,562) = -rxt(k,369)*y(k,23)
         mat(k,156) = -rxt(k,371)*y(k,23)
         mat(k,353) = -rxt(k,374)*y(k,23)
         mat(k,113) = -rxt(k,380)*y(k,23)
         mat(k,64) = -rxt(k,382)*y(k,23)
         mat(k,166) = -rxt(k,385)*y(k,23)
         mat(k,68) = -rxt(k,386)*y(k,23)
         mat(k,41) = -rxt(k,388)*y(k,23)
         mat(k,44) = -rxt(k,391)*y(k,23)
         mat(k,323) = -rxt(k,394)*y(k,23)
         mat(k,213) = -rxt(k,399)*y(k,23)
         mat(k,57) = -rxt(k,404)*y(k,23)
         mat(k,101) = -(rxt(k,405) + rxt(k,406)) * y(k,23)
         mat(k,47) = -rxt(k,408)*y(k,23)

         mat(k,1222) = mat(k,1222) + rxt(k,161)*y(k,22) + rxt(k,174)*y(k,24) &
                      + .330_rkind_comp*rxt(k,311)*y(k,59) + .270_rkind_comp*rxt(k,357)*y(k,83) &
                      + .120_rkind_comp*rxt(k,290)*y(k,44) + .080_rkind_comp*rxt(k,333)*y(k,77) &
                      + .215_rkind_comp*rxt(k,339)*y(k,78) + .700_rkind_comp*rxt(k,395)*y(k,105)
         mat(k,1463) = mat(k,1463) + rxt(k,172)*y(k,21) + rxt(k,268)*y(k,19) &
                      + rxt(k,173)*y(k,24) + rxt(k,176)*y(k,25) + rxt(k,224)*y(k,33) &
                      + rxt(k,225)*y(k,34) + rxt(k,244)*y(k,39) + rxt(k,245)*y(k,40)
         mat(k,1357) = rxt(k,156)*y(k,21) + rxt(k,159)*y(k,42) + 2.000_rkind_comp*rxt(k,131) &
                      *y(k,134) + rxt(k,157)*y(k,33) + rxt(k,158)*y(k,39)
         mat(k,663) = mat(k,663) + rxt(k,172)*y(k,2) + rxt(k,156)*y(k,3)
         mat(k,975) = rxt(k,188)*y(k,24)
         mat(k,1179) = mat(k,1179) + .300_rkind_comp*rxt(k,275)*y(k,17) + .500_rkind_comp*rxt(k,320) &
                      *y(k,65) + .100_rkind_comp*rxt(k,346)*y(k,80) + .500_rkind_comp*rxt(k,296) &
                      *y(k,47) + .650_rkind_comp*rxt(k,283)*y(k,135)
         mat(k,1016) = mat(k,1016) + rxt(k,200)*y(k,24)
         mat(k,195) = mat(k,195) + .300_rkind_comp*rxt(k,275)*y(k,23)
         mat(k,91) = mat(k,91) + rxt(k,159)*y(k,3)
         mat(k,1243) = mat(k,1243) + rxt(k,268)*y(k,2)
         mat(k,1260) = rxt(k,161)*y(k,1) + 2.000_rkind_comp*rxt(k,162)*y(k,24)
         mat(k,1333) = mat(k,1333) + rxt(k,174)*y(k,1) + rxt(k,173)*y(k,2) &
                      + rxt(k,188)*y(k,9) + rxt(k,200)*y(k,11) + 2.000_rkind_comp*rxt(k,162) &
                      *y(k,22) + rxt(k,208)*y(k,28) + .206_rkind_comp*rxt(k,367)*y(k,89)
         mat(k,341) = mat(k,341) + rxt(k,176)*y(k,2)
         mat(k,861) = 2.000_rkind_comp*rxt(k,131)*y(k,3) + rxt(k,247)*y(k,141)
         mat(k,1426) = rxt(k,208)*y(k,24)
         mat(k,1080) = mat(k,1080) + rxt(k,224)*y(k,2) + rxt(k,157)*y(k,3)
         mat(k,515) = mat(k,515) + rxt(k,225)*y(k,2)
         mat(k,480) = mat(k,480) + rxt(k,244)*y(k,2) + rxt(k,158)*y(k,3)
         mat(k,407) = rxt(k,245)*y(k,2)
         mat(k,604) = mat(k,604) + .330_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,546) = mat(k,546) + .270_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,292) = mat(k,292) + .500_rkind_comp*rxt(k,320)*y(k,23)
         mat(k,259) = mat(k,259) + .120_rkind_comp*rxt(k,290)*y(k,1)
         mat(k,759) = mat(k,759) + .080_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,650) = mat(k,650) + .215_rkind_comp*rxt(k,339)*y(k,1)
         mat(k,147) = mat(k,147) + .100_rkind_comp*rxt(k,346)*y(k,23)
         mat(k,152) = mat(k,152) + .500_rkind_comp*rxt(k,296)*y(k,23)
         mat(k,323) = mat(k,323) + .700_rkind_comp*rxt(k,395)*y(k,1)
         mat(k,462) = .206_rkind_comp*rxt(k,367)*y(k,24)
         mat(k,124) = mat(k,124) + .650_rkind_comp*rxt(k,283)*y(k,23)
         mat(k,454) = rxt(k,247)*y(k,134)

      end do

      end subroutine nlnmat02

      subroutine nlnmat03( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1013) = -(rxt(k,193)*y(k,10) + rxt(k,197)*y(k,9) + rxt(k,198)*y(k,2) &
                      + rxt(k,199)*y(k,23) + rxt(k,200)*y(k,24) + rxt(k,266)*y(k,19) &
                      + rxt(k,298)*y(k,50) + rxt(k,312)*y(k,59) + rxt(k,328)*y(k,68) &
                      + rxt(k,342)*y(k,79) + rxt(k,348)*y(k,76) + rxt(k,358)*y(k,83) &
                      + rxt(k,360)*y(k,88) + rxt(k,366)*y(k,89) + rxt(k,370)*y(k,90) &
                      + rxt(k,376)*y(k,91) + rxt(k,396)*y(k,105) + rxt(k,407)*y(k,157))
         mat(k,1389) = -rxt(k,193)*y(k,11)
         mat(k,972) = -rxt(k,197)*y(k,11)
         mat(k,1460) = -rxt(k,198)*y(k,11)
         mat(k,1176) = -rxt(k,199)*y(k,11)
         mat(k,1330) = -rxt(k,200)*y(k,11)
         mat(k,1240) = -rxt(k,266)*y(k,11)
         mat(k,618) = -rxt(k,298)*y(k,11)
         mat(k,602) = -rxt(k,312)*y(k,11)
         mat(k,673) = -rxt(k,328)*y(k,11)
         mat(k,737) = -rxt(k,342)*y(k,11)
         mat(k,776) = -rxt(k,348)*y(k,11)
         mat(k,544) = -rxt(k,358)*y(k,11)
         mat(k,715) = -rxt(k,360)*y(k,11)
         mat(k,461) = -rxt(k,366)*y(k,11)
         mat(k,561) = -rxt(k,370)*y(k,11)
         mat(k,689) = -rxt(k,376)*y(k,11)
         mat(k,322) = -rxt(k,396)*y(k,11)
         mat(k,100) = -rxt(k,407)*y(k,11)

         mat(k,1219) = rxt(k,192)*y(k,10)
         mat(k,1460) = mat(k,1460) + rxt(k,191)*y(k,10) + rxt(k,228)*y(k,35) &
                      + rxt(k,246)*y(k,41)
         mat(k,1389) = mat(k,1389) + rxt(k,192)*y(k,1) + rxt(k,191)*y(k,2)
         mat(k,1176) = mat(k,1176) + rxt(k,196)*y(k,12) + rxt(k,229)*y(k,35) &
                      + rxt(k,309)*y(k,58) + .500_rkind_comp*rxt(k,355)*y(k,81)
         mat(k,871) = rxt(k,196)*y(k,23) + rxt(k,250)*y(k,141)
         mat(k,1423) = rxt(k,230)*y(k,35)
         mat(k,585) = rxt(k,228)*y(k,2) + rxt(k,229)*y(k,23) + rxt(k,230)*y(k,28)
         mat(k,276) = rxt(k,246)*y(k,2)
         mat(k,239) = rxt(k,309)*y(k,23)
         mat(k,282) = .500_rkind_comp*rxt(k,355)*y(k,23)
         mat(k,452) = rxt(k,250)*y(k,12)

         mat(k,870) = -(rxt(k,196)*y(k,23) + rxt(k,250)*y(k,141))
         mat(k,1172) = -rxt(k,196)*y(k,12)
         mat(k,451) = -rxt(k,250)*y(k,12)

         mat(k,1385) = rxt(k,195)*y(k,23)
         mat(k,1172) = mat(k,1172) + rxt(k,195)*y(k,10)
         mat(k,1011) = rxt(k,266)*y(k,19) + rxt(k,298)*y(k,50) + rxt(k,328)*y(k,68) &
                      + rxt(k,407)*y(k,157)
         mat(k,1237) = rxt(k,266)*y(k,11)
         mat(k,1073) = (rxt(k,435)+rxt(k,440)+rxt(k,446))*y(k,35)
         mat(k,583) = (rxt(k,435)+rxt(k,440)+rxt(k,446))*y(k,33)
         mat(k,617) = rxt(k,298)*y(k,11)
         mat(k,672) = rxt(k,328)*y(k,11)
         mat(k,99) = rxt(k,407)*y(k,11)

         mat(k,230) = -(rxt(k,202)*y(k,23))
         mat(k,1123) = -rxt(k,202)*y(k,13)

         mat(k,1368) = rxt(k,201)*y(k,24)
         mat(k,1285) = rxt(k,201)*y(k,10)


         mat(k,1367) = rxt(k,193)*y(k,11)
         mat(k,986) = rxt(k,193)*y(k,10)

         mat(k,1054) = -(rxt(k,215)*y(k,30) + rxt(k,270)*y(k,9) + rxt(k,271)*y(k,24) &
                      + (4._rkind_comp*rxt(k,272) + 4._rkind_comp*rxt(k,273)) * y(k,16) + rxt(k,294) &
                      *y(k,46) + rxt(k,302)*y(k,48) + rxt(k,315)*y(k,61) + rxt(k,324) &
                      *y(k,67) + rxt(k,344)*y(k,79) + rxt(k,350)*y(k,76) + rxt(k,363) &
                      *y(k,88) + rxt(k,378)*y(k,91))
         mat(k,894) = -rxt(k,215)*y(k,16)
         mat(k,973) = -rxt(k,270)*y(k,16)
         mat(k,1331) = -rxt(k,271)*y(k,16)
         mat(k,505) = -rxt(k,294)*y(k,16)
         mat(k,805) = -rxt(k,302)*y(k,16)
         mat(k,527) = -rxt(k,315)*y(k,16)
         mat(k,632) = -rxt(k,324)*y(k,16)
         mat(k,738) = -rxt(k,344)*y(k,16)
         mat(k,777) = -rxt(k,350)*y(k,16)
         mat(k,716) = -rxt(k,363)*y(k,16)
         mat(k,690) = -rxt(k,378)*y(k,16)

         mat(k,1220) = .310_rkind_comp*rxt(k,311)*y(k,59)
         mat(k,973) = mat(k,973) + rxt(k,299)*y(k,48)
         mat(k,1177) = .700_rkind_comp*rxt(k,275)*y(k,17) + rxt(k,291)*y(k,49)
         mat(k,1054) = mat(k,1054) + .900_rkind_comp*rxt(k,302)*y(k,48)
         mat(k,194) = .700_rkind_comp*rxt(k,275)*y(k,23)
         mat(k,603) = .310_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,329) = rxt(k,291)*y(k,23)
         mat(k,805) = mat(k,805) + rxt(k,299)*y(k,9) + .900_rkind_comp*rxt(k,302)*y(k,16) &
                      + 4.000_rkind_comp*rxt(k,303)*y(k,48) + rxt(k,364)*y(k,88) + rxt(k,345) &
                      *y(k,79) + rxt(k,351)*y(k,76) + rxt(k,379)*y(k,91)
         mat(k,716) = mat(k,716) + rxt(k,364)*y(k,48)
         mat(k,738) = mat(k,738) + rxt(k,345)*y(k,48)
         mat(k,777) = mat(k,777) + rxt(k,351)*y(k,48)
         mat(k,690) = mat(k,690) + rxt(k,379)*y(k,48)

         mat(k,192) = -(rxt(k,275)*y(k,23))
         mat(k,1119) = -rxt(k,275)*y(k,17)

         mat(k,1027) = rxt(k,271)*y(k,24)
         mat(k,1281) = rxt(k,271)*y(k,16)

         mat(k,90) = -(rxt(k,159)*y(k,3) + rxt(k,177)*y(k,23))
         mat(k,1345) = -rxt(k,159)*y(k,42)
         mat(k,1103) = -rxt(k,177)*y(k,42)

         mat(k,48) = -(rxt(k,178)*y(k,23))
         mat(k,1095) = -rxt(k,178)*y(k,43)

         mat(k,1245) = -(rxt(k,209)*y(k,28) + rxt(k,233)*y(k,37) + rxt(k,266)*y(k,11) &
                      + rxt(k,267)*y(k,23) + rxt(k,268)*y(k,2) + rxt(k,269)*y(k,24))
         mat(k,1428) = -rxt(k,209)*y(k,19)
         mat(k,919) = -rxt(k,233)*y(k,19)
         mat(k,1018) = -rxt(k,266)*y(k,19)
         mat(k,1181) = -rxt(k,267)*y(k,19)
         mat(k,1465) = -rxt(k,268)*y(k,19)
         mat(k,1335) = -rxt(k,269)*y(k,19)

         mat(k,1224) = .540_rkind_comp*rxt(k,311)*y(k,59) + .600_rkind_comp*rxt(k,357)*y(k,83) &
                      + rxt(k,290)*y(k,44) + .800_rkind_comp*rxt(k,333)*y(k,77) &
                      + .700_rkind_comp*rxt(k,339)*y(k,78)
         mat(k,977) = rxt(k,270)*y(k,16) + rxt(k,318)*y(k,64) + .500_rkind_comp*rxt(k,331) &
                      *y(k,73) + .100_rkind_comp*rxt(k,372)*y(k,84) + .550_rkind_comp*rxt(k,359) &
                      *y(k,88) + .250_rkind_comp*rxt(k,340)*y(k,79) + rxt(k,347)*y(k,76) &
                      + .500_rkind_comp*rxt(k,286)*y(k,55) + rxt(k,322)*y(k,67) &
                      + .072_rkind_comp*rxt(k,365)*y(k,89) + .250_rkind_comp*rxt(k,375)*y(k,91)
         mat(k,1181) = mat(k,1181) + .300_rkind_comp*rxt(k,275)*y(k,17) + .500_rkind_comp*rxt(k,304) &
                      *y(k,54) + rxt(k,309)*y(k,58) + .500_rkind_comp*rxt(k,355)*y(k,81) &
                      + rxt(k,274)*y(k,18) + .800_rkind_comp*rxt(k,305)*y(k,52)
         mat(k,1018) = mat(k,1018) + .600_rkind_comp*rxt(k,360)*y(k,88) + .250_rkind_comp*rxt(k,342) &
                      *y(k,79) + rxt(k,348)*y(k,76) + .072_rkind_comp*rxt(k,366)*y(k,89)
         mat(k,1058) = rxt(k,270)*y(k,9) + (4.000_rkind_comp*rxt(k,272)+2.000_rkind_comp*rxt(k,273)) &
                      *y(k,16) + rxt(k,215)*y(k,30) + rxt(k,302)*y(k,48) &
                      + 1.200_rkind_comp*rxt(k,363)*y(k,88) + .880_rkind_comp*rxt(k,344)*y(k,79) &
                      + 2.000_rkind_comp*rxt(k,350)*y(k,76) + .700_rkind_comp*rxt(k,294)*y(k,46) &
                      + rxt(k,315)*y(k,61) + .800_rkind_comp*rxt(k,324)*y(k,67) &
                      + .800_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,196) = .300_rkind_comp*rxt(k,275)*y(k,23)
         mat(k,1335) = mat(k,1335) + .206_rkind_comp*rxt(k,367)*y(k,89)
         mat(k,898) = rxt(k,215)*y(k,16)
         mat(k,606) = .540_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,548) = .600_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,474) = rxt(k,318)*y(k,9)
         mat(k,808) = rxt(k,302)*y(k,16) + .600_rkind_comp*rxt(k,364)*y(k,88) &
                      + .250_rkind_comp*rxt(k,345)*y(k,79) + rxt(k,351)*y(k,76) &
                      + .250_rkind_comp*rxt(k,379)*y(k,91)
         mat(k,249) = .500_rkind_comp*rxt(k,304)*y(k,23)
         mat(k,242) = rxt(k,309)*y(k,23)
         mat(k,261) = rxt(k,290)*y(k,1)
         mat(k,284) = .500_rkind_comp*rxt(k,355)*y(k,23)
         mat(k,136) = .500_rkind_comp*rxt(k,331)*y(k,9)
         mat(k,388) = .100_rkind_comp*rxt(k,372)*y(k,9)
         mat(k,719) = .550_rkind_comp*rxt(k,359)*y(k,9) + .600_rkind_comp*rxt(k,360)*y(k,11) &
                      + 1.200_rkind_comp*rxt(k,363)*y(k,16) + .600_rkind_comp*rxt(k,364)*y(k,48)
         mat(k,761) = .800_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,652) = .700_rkind_comp*rxt(k,339)*y(k,1)
         mat(k,740) = .250_rkind_comp*rxt(k,340)*y(k,9) + .250_rkind_comp*rxt(k,342)*y(k,11) &
                      + .880_rkind_comp*rxt(k,344)*y(k,16) + .250_rkind_comp*rxt(k,345)*y(k,48)
         mat(k,780) = rxt(k,347)*y(k,9) + rxt(k,348)*y(k,11) + 2.000_rkind_comp*rxt(k,350) &
                      *y(k,16) + rxt(k,351)*y(k,48) + 4.000_rkind_comp*rxt(k,352)*y(k,76)
         mat(k,507) = .700_rkind_comp*rxt(k,294)*y(k,16)
         mat(k,529) = rxt(k,315)*y(k,16)
         mat(k,269) = rxt(k,274)*y(k,23)
         mat(k,555) = .800_rkind_comp*rxt(k,305)*y(k,23)
         mat(k,417) = .500_rkind_comp*rxt(k,286)*y(k,9)
         mat(k,634) = rxt(k,322)*y(k,9) + .800_rkind_comp*rxt(k,324)*y(k,16)
         mat(k,463) = .072_rkind_comp*rxt(k,365)*y(k,9) + .072_rkind_comp*rxt(k,366)*y(k,11) &
                      + .206_rkind_comp*rxt(k,367)*y(k,24)
         mat(k,692) = .250_rkind_comp*rxt(k,375)*y(k,9) + .800_rkind_comp*rxt(k,378)*y(k,16) &
                      + .250_rkind_comp*rxt(k,379)*y(k,48)

         mat(k,1263) = -(rxt(k,160)*y(k,4) + rxt(k,161)*y(k,1) + (rxt(k,162) + rxt(k,163) &
                      + rxt(k,164)) * y(k,24))
         mat(k,848) = -rxt(k,160)*y(k,22)
         mat(k,1225) = -rxt(k,161)*y(k,22)
         mat(k,1336) = -(rxt(k,162) + rxt(k,163) + rxt(k,164)) * y(k,22)

         mat(k,1466) = rxt(k,172)*y(k,21) + rxt(k,165)*y(k,23)
         mat(k,1360) = rxt(k,156)*y(k,21)
         mat(k,664) = rxt(k,172)*y(k,2) + rxt(k,156)*y(k,3) + rxt(k,170)*y(k,23) &
                      + rxt(k,205)*y(k,28) + rxt(k,248)*y(k,141)
         mat(k,611) = rxt(k,264)*y(k,23)
         mat(k,494) = rxt(k,181)*y(k,23)
         mat(k,1182) = rxt(k,165)*y(k,2) + rxt(k,170)*y(k,21) + rxt(k,264)*y(k,20) &
                      + rxt(k,181)*y(k,8) + rxt(k,267)*y(k,19)
         mat(k,1246) = rxt(k,267)*y(k,23)
         mat(k,1429) = rxt(k,205)*y(k,21)
         mat(k,455) = rxt(k,248)*y(k,21)

         mat(k,1337) = -((rxt(k,162) + rxt(k,163) + rxt(k,164)) * y(k,22) + rxt(k,167) &
                      *y(k,23) + rxt(k,173)*y(k,2) + rxt(k,174)*y(k,1) + 4._rkind_comp*rxt(k,175) &
                      *y(k,24) + rxt(k,188)*y(k,9) + rxt(k,200)*y(k,11) + rxt(k,201) &
                      *y(k,10) + (rxt(k,207) + rxt(k,208)) * y(k,28) + rxt(k,214) &
                      *y(k,30) + rxt(k,232)*y(k,37) + rxt(k,236)*y(k,38) + rxt(k,269) &
                      *y(k,19) + rxt(k,271)*y(k,16) + rxt(k,279)*y(k,137) + rxt(k,287) &
                      *y(k,55) + rxt(k,293)*y(k,46) + rxt(k,301)*y(k,48) + rxt(k,314) &
                      *y(k,61) + rxt(k,319)*y(k,64) + rxt(k,323)*y(k,67) + rxt(k,336) &
                      *y(k,74) + rxt(k,343)*y(k,79) + rxt(k,349)*y(k,76) + rxt(k,361) &
                      *y(k,88) + rxt(k,367)*y(k,89) + rxt(k,373)*y(k,84) + rxt(k,377) &
                      *y(k,91) + rxt(k,384)*y(k,96) + rxt(k,389)*y(k,100) + rxt(k,392) &
                      *y(k,103) + rxt(k,398)*y(k,106))
         mat(k,1264) = -(rxt(k,162) + rxt(k,163) + rxt(k,164)) * y(k,24)
         mat(k,1183) = -rxt(k,167)*y(k,24)
         mat(k,1467) = -rxt(k,173)*y(k,24)
         mat(k,1226) = -rxt(k,174)*y(k,24)
         mat(k,979) = -rxt(k,188)*y(k,24)
         mat(k,1020) = -rxt(k,200)*y(k,24)
         mat(k,1396) = -rxt(k,201)*y(k,24)
         mat(k,1430) = -(rxt(k,207) + rxt(k,208)) * y(k,24)
         mat(k,900) = -rxt(k,214)*y(k,24)
         mat(k,921) = -rxt(k,232)*y(k,24)
         mat(k,826) = -rxt(k,236)*y(k,24)
         mat(k,1247) = -rxt(k,269)*y(k,24)
         mat(k,1060) = -rxt(k,271)*y(k,24)
         mat(k,190) = -rxt(k,279)*y(k,24)
         mat(k,418) = -rxt(k,287)*y(k,24)
         mat(k,508) = -rxt(k,293)*y(k,24)
         mat(k,810) = -rxt(k,301)*y(k,24)
         mat(k,530) = -rxt(k,314)*y(k,24)
         mat(k,475) = -rxt(k,319)*y(k,24)
         mat(k,635) = -rxt(k,323)*y(k,24)
         mat(k,362) = -rxt(k,336)*y(k,24)
         mat(k,742) = -rxt(k,343)*y(k,24)
         mat(k,782) = -rxt(k,349)*y(k,24)
         mat(k,721) = -rxt(k,361)*y(k,24)
         mat(k,464) = -rxt(k,367)*y(k,24)
         mat(k,389) = -rxt(k,373)*y(k,24)
         mat(k,694) = -rxt(k,377)*y(k,24)
         mat(k,310) = -rxt(k,384)*y(k,24)
         mat(k,172) = -rxt(k,389)*y(k,24)
         mat(k,221) = -rxt(k,392)*y(k,24)
         mat(k,577) = -rxt(k,398)*y(k,24)

         mat(k,1226) = mat(k,1226) + rxt(k,166)*y(k,23) + .190_rkind_comp*rxt(k,311)*y(k,59) &
                      + .060_rkind_comp*rxt(k,357)*y(k,83) + .120_rkind_comp*rxt(k,290)*y(k,44) &
                      + .060_rkind_comp*rxt(k,333)*y(k,77) + .275_rkind_comp*rxt(k,339)*y(k,78) &
                      + rxt(k,395)*y(k,105)
         mat(k,1467) = mat(k,1467) + rxt(k,268)*y(k,19) + rxt(k,176)*y(k,25)
         mat(k,849) = rxt(k,160)*y(k,22) + rxt(k,288)*y(k,56)
         mat(k,612) = rxt(k,265)*y(k,23)
         mat(k,979) = mat(k,979) + rxt(k,270)*y(k,16) + rxt(k,318)*y(k,64) &
                      + rxt(k,331)*y(k,73) + .900_rkind_comp*rxt(k,372)*y(k,84) &
                      + .900_rkind_comp*rxt(k,383)*y(k,96) + rxt(k,397)*y(k,106) &
                      + .900_rkind_comp*rxt(k,390)*y(k,100) + .900_rkind_comp*rxt(k,393)*y(k,103) &
                      + .920_rkind_comp*rxt(k,359)*y(k,88) + .470_rkind_comp*rxt(k,340)*y(k,79) &
                      + rxt(k,292)*y(k,46) + rxt(k,313)*y(k,61) + .250_rkind_comp*rxt(k,286) &
                      *y(k,55) + .794_rkind_comp*rxt(k,365)*y(k,89) + rxt(k,375)*y(k,91) &
                      + rxt(k,278)*y(k,137)
         mat(k,1396) = mat(k,1396) + .700_rkind_comp*rxt(k,387)*y(k,98)
         mat(k,1183) = mat(k,1183) + rxt(k,166)*y(k,1) + rxt(k,265)*y(k,20) &
                      + rxt(k,199)*y(k,11) + rxt(k,177)*y(k,42) + rxt(k,178)*y(k,43) &
                      + rxt(k,171)*y(k,25) + rxt(k,212)*y(k,30) + rxt(k,235)*y(k,38) &
                      + .500_rkind_comp*rxt(k,355)*y(k,81) + .250_rkind_comp*rxt(k,382)*y(k,94) &
                      + rxt(k,306)*y(k,53) + .200_rkind_comp*rxt(k,346)*y(k,80) + rxt(k,274) &
                      *y(k,18) + rxt(k,307)*y(k,51) + rxt(k,305)*y(k,52) + rxt(k,326) &
                      *y(k,66) + rxt(k,369)*y(k,90) + .350_rkind_comp*rxt(k,283)*y(k,135) &
                      + rxt(k,276)*y(k,136) + .500_rkind_comp*rxt(k,406)*y(k,157)
         mat(k,1020) = mat(k,1020) + rxt(k,199)*y(k,23) + rxt(k,266)*y(k,19) &
                      + rxt(k,360)*y(k,88) + .470_rkind_comp*rxt(k,342)*y(k,79) &
                      + .794_rkind_comp*rxt(k,366)*y(k,89) + rxt(k,370)*y(k,90) + rxt(k,376) &
                      *y(k,91)
         mat(k,1060) = mat(k,1060) + rxt(k,270)*y(k,9) + 4.000_rkind_comp*rxt(k,272)*y(k,16) &
                      + rxt(k,215)*y(k,30) + .900_rkind_comp*rxt(k,302)*y(k,48) + rxt(k,363) &
                      *y(k,88) + .730_rkind_comp*rxt(k,344)*y(k,79) + rxt(k,350)*y(k,76) &
                      + rxt(k,294)*y(k,46) + rxt(k,315)*y(k,61) + .300_rkind_comp*rxt(k,324) &
                      *y(k,67) + .800_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,92) = rxt(k,177)*y(k,23)
         mat(k,50) = rxt(k,178)*y(k,23)
         mat(k,1247) = mat(k,1247) + rxt(k,268)*y(k,2) + rxt(k,266)*y(k,11) &
                      + rxt(k,209)*y(k,28) + rxt(k,233)*y(k,37)
         mat(k,1264) = mat(k,1264) + rxt(k,160)*y(k,4)
         mat(k,342) = rxt(k,176)*y(k,2) + rxt(k,171)*y(k,23) + rxt(k,206)*y(k,28)
         mat(k,1430) = mat(k,1430) + rxt(k,209)*y(k,19) + rxt(k,206)*y(k,25)
         mat(k,900) = mat(k,900) + rxt(k,212)*y(k,23) + rxt(k,215)*y(k,16)
         mat(k,921) = mat(k,921) + rxt(k,233)*y(k,19)
         mat(k,826) = mat(k,826) + rxt(k,235)*y(k,23)
         mat(k,607) = .190_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,549) = .060_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,475) = mat(k,475) + rxt(k,318)*y(k,9)
         mat(k,810) = mat(k,810) + .900_rkind_comp*rxt(k,302)*y(k,16) + rxt(k,364)*y(k,88) &
                      + .470_rkind_comp*rxt(k,345)*y(k,79) + rxt(k,379)*y(k,91)
         mat(k,262) = .120_rkind_comp*rxt(k,290)*y(k,1)
         mat(k,285) = .500_rkind_comp*rxt(k,355)*y(k,23)
         mat(k,137) = rxt(k,331)*y(k,9)
         mat(k,389) = mat(k,389) + .900_rkind_comp*rxt(k,372)*y(k,9)
         mat(k,65) = .250_rkind_comp*rxt(k,382)*y(k,23)
         mat(k,310) = mat(k,310) + .900_rkind_comp*rxt(k,383)*y(k,9)
         mat(k,84) = .700_rkind_comp*rxt(k,387)*y(k,10)
         mat(k,577) = mat(k,577) + rxt(k,397)*y(k,9)
         mat(k,400) = rxt(k,306)*y(k,23)
         mat(k,172) = mat(k,172) + .900_rkind_comp*rxt(k,390)*y(k,9)
         mat(k,221) = mat(k,221) + .900_rkind_comp*rxt(k,393)*y(k,9)
         mat(k,721) = mat(k,721) + .920_rkind_comp*rxt(k,359)*y(k,9) + rxt(k,360)*y(k,11) &
                      + rxt(k,363)*y(k,16) + rxt(k,364)*y(k,48)
         mat(k,763) = .060_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,654) = .275_rkind_comp*rxt(k,339)*y(k,1)
         mat(k,742) = mat(k,742) + .470_rkind_comp*rxt(k,340)*y(k,9) + .470_rkind_comp*rxt(k,342) &
                      *y(k,11) + .730_rkind_comp*rxt(k,344)*y(k,16) + .470_rkind_comp*rxt(k,345) &
                      *y(k,48)
         mat(k,148) = .200_rkind_comp*rxt(k,346)*y(k,23)
         mat(k,782) = mat(k,782) + rxt(k,350)*y(k,16)
         mat(k,508) = mat(k,508) + rxt(k,292)*y(k,9) + rxt(k,294)*y(k,16) &
                      + 2.400_rkind_comp*rxt(k,295)*y(k,46)
         mat(k,325) = rxt(k,395)*y(k,1)
         mat(k,530) = mat(k,530) + rxt(k,313)*y(k,9) + rxt(k,315)*y(k,16)
         mat(k,270) = rxt(k,274)*y(k,23)
         mat(k,109) = rxt(k,307)*y(k,23)
         mat(k,556) = rxt(k,305)*y(k,23)
         mat(k,642) = rxt(k,326)*y(k,23)
         mat(k,418) = mat(k,418) + .250_rkind_comp*rxt(k,286)*y(k,9)
         mat(k,207) = rxt(k,288)*y(k,4)
         mat(k,635) = mat(k,635) + .300_rkind_comp*rxt(k,324)*y(k,16)
         mat(k,464) = mat(k,464) + .794_rkind_comp*rxt(k,365)*y(k,9) + .794_rkind_comp*rxt(k,366) &
                      *y(k,11)
         mat(k,564) = rxt(k,369)*y(k,23) + rxt(k,370)*y(k,11)
         mat(k,694) = mat(k,694) + rxt(k,375)*y(k,9) + rxt(k,376)*y(k,11) &
                      + .800_rkind_comp*rxt(k,378)*y(k,16) + rxt(k,379)*y(k,48)
         mat(k,125) = .350_rkind_comp*rxt(k,283)*y(k,23)
         mat(k,266) = rxt(k,276)*y(k,23)
         mat(k,190) = mat(k,190) + rxt(k,278)*y(k,9)
         mat(k,102) = .500_rkind_comp*rxt(k,406)*y(k,23)

      end do

      end subroutine nlnmat03

      subroutine nlnmat04( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,338) = -(rxt(k,171)*y(k,23) + rxt(k,176)*y(k,2) + rxt(k,206)*y(k,28))
         mat(k,1136) = -rxt(k,171)*y(k,25)
         mat(k,1441) = -rxt(k,176)*y(k,25)
         mat(k,1406) = -rxt(k,206)*y(k,25)

         mat(k,1136) = mat(k,1136) + 2.000_rkind_comp*rxt(k,169)*y(k,23)
         mat(k,1293) = 2.000_rkind_comp*rxt(k,175)*y(k,24)

         mat(k,856) = -(rxt(k,131)*y(k,3) + rxt(k,247)*y(k,141))
         mat(k,1350) = -rxt(k,131)*y(k,134)
         mat(k,450) = -rxt(k,247)*y(k,134)

         mat(k,658) = rxt(k,170)*y(k,23)
         mat(k,1171) = rxt(k,170)*y(k,21) + 2.000_rkind_comp*rxt(k,168)*y(k,23) + rxt(k,196) &
                      *y(k,12) + rxt(k,202)*y(k,13) + rxt(k,275)*y(k,17) + rxt(k,267) &
                      *y(k,19) + rxt(k,167)*y(k,24) + rxt(k,171)*y(k,25) + rxt(k,223) &
                      *y(k,33) + rxt(k,227)*y(k,34) + rxt(k,243)*y(k,39) + rxt(k,297) &
                      *y(k,50) + rxt(k,291)*y(k,49) + rxt(k,320)*y(k,65) + rxt(k,304) &
                      *y(k,54) + rxt(k,284)*y(k,45) + .500_rkind_comp*rxt(k,338)*y(k,78) &
                      + rxt(k,317)*y(k,60) + rxt(k,316)*y(k,62) + rxt(k,321)*y(k,63) &
                      + rxt(k,325)*y(k,69) + rxt(k,327)*y(k,68) + (rxt(k,380) &
                       +rxt(k,381))*y(k,92) + rxt(k,276)*y(k,136) + rxt(k,408) &
                      *y(k,159)
         mat(k,869) = rxt(k,196)*y(k,23)
         mat(k,232) = rxt(k,202)*y(k,23)
         mat(k,193) = rxt(k,275)*y(k,23)
         mat(k,1236) = rxt(k,267)*y(k,23)
         mat(k,1254) = rxt(k,164)*y(k,24)
         mat(k,1325) = rxt(k,167)*y(k,23) + rxt(k,164)*y(k,22)
         mat(k,339) = rxt(k,171)*y(k,23)
         mat(k,1072) = rxt(k,223)*y(k,23) + (rxt(k,436)+rxt(k,441)+rxt(k,447))*y(k,34) + ( &
                      + rxt(k,437)+rxt(k,448))*y(k,40)
         mat(k,512) = rxt(k,227)*y(k,23) + (rxt(k,436)+rxt(k,441)+rxt(k,447))*y(k,33)
         mat(k,478) = rxt(k,243)*y(k,23)
         mat(k,404) = (rxt(k,437)+rxt(k,448))*y(k,33)
         mat(k,616) = rxt(k,297)*y(k,23)
         mat(k,328) = rxt(k,291)*y(k,23)
         mat(k,291) = rxt(k,320)*y(k,23)
         mat(k,246) = rxt(k,304)*y(k,23)
         mat(k,116) = rxt(k,284)*y(k,23)
         mat(k,649) = .500_rkind_comp*rxt(k,338)*y(k,23)
         mat(k,53) = rxt(k,317)*y(k,23)
         mat(k,177) = rxt(k,316)*y(k,23)
         mat(k,394) = rxt(k,321)*y(k,23)
         mat(k,183) = rxt(k,325)*y(k,23)
         mat(k,671) = rxt(k,327)*y(k,23)
         mat(k,112) = (rxt(k,380)+rxt(k,381))*y(k,23)
         mat(k,264) = rxt(k,276)*y(k,23)
         mat(k,46) = rxt(k,408)*y(k,23)

         mat(k,1433) = -(rxt(k,204)*y(k,1) + rxt(k,205)*y(k,21) + rxt(k,206)*y(k,25) &
                      + (rxt(k,207) + rxt(k,208)) * y(k,24) + rxt(k,209)*y(k,19) &
                      + rxt(k,226)*y(k,34) + rxt(k,230)*y(k,35) + rxt(k,282)*y(k,45))
         mat(k,1229) = -rxt(k,204)*y(k,28)
         mat(k,666) = -rxt(k,205)*y(k,28)
         mat(k,343) = -rxt(k,206)*y(k,28)
         mat(k,1340) = -(rxt(k,207) + rxt(k,208)) * y(k,28)
         mat(k,1250) = -rxt(k,209)*y(k,28)
         mat(k,516) = -rxt(k,226)*y(k,28)
         mat(k,589) = -rxt(k,230)*y(k,28)
         mat(k,119) = -rxt(k,282)*y(k,28)

         mat(k,1470) = rxt(k,211)*y(k,30) + rxt(k,224)*y(k,33)
         mat(k,1364) = rxt(k,157)*y(k,33) + rxt(k,152)*y(k,139)
         mat(k,982) = rxt(k,216)*y(k,30)
         mat(k,1186) = rxt(k,212)*y(k,30) + rxt(k,223)*y(k,33)
         mat(k,1063) = rxt(k,215)*y(k,30)
         mat(k,903) = rxt(k,211)*y(k,2) + rxt(k,216)*y(k,9) + rxt(k,212)*y(k,23) &
                      + rxt(k,215)*y(k,16) + (4.000_rkind_comp*rxt(k,218)+2.000_rkind_comp*rxt(k,220)) &
                      *y(k,30) + rxt(k,240)*y(k,38)
         mat(k,1087) = rxt(k,224)*y(k,2) + rxt(k,157)*y(k,3) + rxt(k,223)*y(k,23)
         mat(k,828) = rxt(k,240)*y(k,30)
         mat(k,89) = rxt(k,152)*y(k,3)


         mat(k,1401) = rxt(k,230)*y(k,35)
         mat(k,882) = 2.000_rkind_comp*rxt(k,219)*y(k,30)
         mat(k,1065) = (rxt(k,436)+rxt(k,441)+rxt(k,447))*y(k,34) + (rxt(k,435) &
                       +rxt(k,440)+rxt(k,446))*y(k,35)
         mat(k,510) = (rxt(k,436)+rxt(k,441)+rxt(k,447))*y(k,33)
         mat(k,579) = rxt(k,230)*y(k,28) + (rxt(k,435)+rxt(k,440)+rxt(k,446))*y(k,33)

         mat(k,890) = -(rxt(k,211)*y(k,2) + (rxt(k,212) + rxt(k,213)) * y(k,23) &
                      + rxt(k,214)*y(k,24) + rxt(k,215)*y(k,16) + rxt(k,216)*y(k,9) &
                      + rxt(k,217)*y(k,10) + (4._rkind_comp*rxt(k,218) + 4._rkind_comp*rxt(k,219) &
                      + 4._rkind_comp*rxt(k,220) + 4._rkind_comp*rxt(k,221)) * y(k,30) + (rxt(k,239) &
                      + rxt(k,240) + rxt(k,241)) * y(k,38))
         mat(k,1457) = -rxt(k,211)*y(k,30)
         mat(k,1173) = -(rxt(k,212) + rxt(k,213)) * y(k,30)
         mat(k,1327) = -rxt(k,214)*y(k,30)
         mat(k,1050) = -rxt(k,215)*y(k,30)
         mat(k,969) = -rxt(k,216)*y(k,30)
         mat(k,1386) = -rxt(k,217)*y(k,30)
         mat(k,820) = -(rxt(k,239) + rxt(k,240) + rxt(k,241)) * y(k,30)

         mat(k,1216) = rxt(k,204)*y(k,28)
         mat(k,1457) = mat(k,1457) + rxt(k,225)*y(k,34) + rxt(k,228)*y(k,35)
         mat(k,1173) = mat(k,1173) + rxt(k,227)*y(k,34)
         mat(k,1327) = mat(k,1327) + rxt(k,208)*y(k,28)
         mat(k,1420) = rxt(k,204)*y(k,1) + rxt(k,208)*y(k,24) + rxt(k,226)*y(k,34)
         mat(k,513) = rxt(k,225)*y(k,2) + rxt(k,227)*y(k,23) + rxt(k,226)*y(k,28)
         mat(k,584) = rxt(k,228)*y(k,2)


         mat(k,881) = 2.000_rkind_comp*rxt(k,220)*y(k,30) + rxt(k,239)*y(k,38)
         mat(k,812) = rxt(k,239)*y(k,30)


         mat(k,880) = 2.000_rkind_comp*rxt(k,221)*y(k,30)

         mat(k,1079) = -(rxt(k,157)*y(k,3) + rxt(k,223)*y(k,23) + rxt(k,224)*y(k,2) &
                      + (rxt(k,435) + rxt(k,440) + rxt(k,446)) * y(k,35) + (rxt(k,436) &
                      + rxt(k,441) + rxt(k,447)) * y(k,34) + (rxt(k,437) + rxt(k,448) &
                      ) * y(k,40))
         mat(k,1356) = -rxt(k,157)*y(k,33)
         mat(k,1178) = -rxt(k,223)*y(k,33)
         mat(k,1462) = -rxt(k,224)*y(k,33)
         mat(k,586) = -(rxt(k,435) + rxt(k,440) + rxt(k,446)) * y(k,33)
         mat(k,514) = -(rxt(k,436) + rxt(k,441) + rxt(k,447)) * y(k,33)
         mat(k,406) = -(rxt(k,437) + rxt(k,448)) * y(k,33)

         mat(k,662) = rxt(k,205)*y(k,28)
         mat(k,1178) = mat(k,1178) + rxt(k,213)*y(k,30)
         mat(k,1242) = rxt(k,209)*y(k,28)
         mat(k,1332) = rxt(k,207)*y(k,28)
         mat(k,340) = rxt(k,206)*y(k,28)
         mat(k,1425) = rxt(k,205)*y(k,21) + rxt(k,209)*y(k,19) + rxt(k,207)*y(k,24) &
                      + rxt(k,206)*y(k,25) + rxt(k,226)*y(k,34) + rxt(k,282)*y(k,45)
         mat(k,895) = rxt(k,213)*y(k,23)
         mat(k,514) = mat(k,514) + rxt(k,226)*y(k,28)
         mat(k,117) = rxt(k,282)*y(k,28)

         mat(k,511) = -(rxt(k,225)*y(k,2) + rxt(k,226)*y(k,28) + rxt(k,227)*y(k,23) &
                      + (rxt(k,436) + rxt(k,441) + rxt(k,447)) * y(k,33))
         mat(k,1449) = -rxt(k,225)*y(k,34)
         mat(k,1410) = -rxt(k,226)*y(k,34)
         mat(k,1148) = -rxt(k,227)*y(k,34)
         mat(k,1068) = -(rxt(k,436) + rxt(k,441) + rxt(k,447)) * y(k,34)

         mat(k,1148) = mat(k,1148) + rxt(k,229)*y(k,35)
         mat(k,1305) = rxt(k,214)*y(k,30)
         mat(k,884) = rxt(k,214)*y(k,24)
         mat(k,580) = rxt(k,229)*y(k,23)

         mat(k,581) = -(rxt(k,228)*y(k,2) + rxt(k,229)*y(k,23) + rxt(k,230)*y(k,28) &
                      + (rxt(k,435) + rxt(k,440) + rxt(k,446)) * y(k,33))
         mat(k,1450) = -rxt(k,228)*y(k,35)
         mat(k,1154) = -rxt(k,229)*y(k,35)
         mat(k,1412) = -rxt(k,230)*y(k,35)
         mat(k,1069) = -(rxt(k,435) + rxt(k,440) + rxt(k,446)) * y(k,35)

         mat(k,1376) = rxt(k,217)*y(k,30)
         mat(k,885) = rxt(k,217)*y(k,10)


         mat(k,883) = rxt(k,241)*y(k,38)
         mat(k,1066) = (rxt(k,437)+rxt(k,448))*y(k,40)
         mat(k,813) = rxt(k,241)*y(k,30)
         mat(k,401) = (rxt(k,437)+rxt(k,448))*y(k,33)

         mat(k,912) = -(rxt(k,231)*y(k,1) + rxt(k,232)*y(k,24) + rxt(k,233)*y(k,19))
         mat(k,1217) = -rxt(k,231)*y(k,37)
         mat(k,1328) = -rxt(k,232)*y(k,37)
         mat(k,1238) = -rxt(k,233)*y(k,37)

         mat(k,1458) = rxt(k,234)*y(k,38) + rxt(k,244)*y(k,39)
         mat(k,1352) = rxt(k,158)*y(k,39)
         mat(k,970) = rxt(k,237)*y(k,38)
         mat(k,1174) = rxt(k,235)*y(k,38) + rxt(k,243)*y(k,39)
         mat(k,891) = (rxt(k,239)+rxt(k,240))*y(k,38)
         mat(k,821) = rxt(k,234)*y(k,2) + rxt(k,237)*y(k,9) + rxt(k,235)*y(k,23) + ( &
                      + rxt(k,239)+rxt(k,240))*y(k,30) + 4.000_rkind_comp*rxt(k,242)*y(k,38)
         mat(k,479) = rxt(k,244)*y(k,2) + rxt(k,158)*y(k,3) + rxt(k,243)*y(k,23)

         mat(k,816) = -(rxt(k,234)*y(k,2) + rxt(k,235)*y(k,23) + rxt(k,236)*y(k,24) &
                      + rxt(k,237)*y(k,9) + rxt(k,238)*y(k,10) + (rxt(k,239) + rxt(k,240) &
                      + rxt(k,241)) * y(k,30) + 4._rkind_comp*rxt(k,242)*y(k,38))
         mat(k,1453) = -rxt(k,234)*y(k,38)
         mat(k,1169) = -rxt(k,235)*y(k,38)
         mat(k,1323) = -rxt(k,236)*y(k,38)
         mat(k,965) = -rxt(k,237)*y(k,38)
         mat(k,1382) = -rxt(k,238)*y(k,38)
         mat(k,886) = -(rxt(k,239) + rxt(k,240) + rxt(k,241)) * y(k,38)

         mat(k,1212) = rxt(k,231)*y(k,37)
         mat(k,1453) = mat(k,1453) + rxt(k,245)*y(k,40) + rxt(k,246)*y(k,41)
         mat(k,907) = rxt(k,231)*y(k,1)
         mat(k,403) = rxt(k,245)*y(k,2)
         mat(k,273) = rxt(k,246)*y(k,2)

         mat(k,477) = -(rxt(k,158)*y(k,3) + rxt(k,243)*y(k,23) + rxt(k,244)*y(k,2))
         mat(k,1347) = -rxt(k,158)*y(k,39)
         mat(k,1145) = -rxt(k,243)*y(k,39)
         mat(k,1447) = -rxt(k,244)*y(k,39)

         mat(k,1233) = rxt(k,233)*y(k,37)
         mat(k,1303) = rxt(k,232)*y(k,37)
         mat(k,905) = rxt(k,233)*y(k,19) + rxt(k,232)*y(k,24)

         mat(k,402) = -(rxt(k,245)*y(k,2) + (rxt(k,437) + rxt(k,448)) * y(k,33))
         mat(k,1443) = -rxt(k,245)*y(k,40)
         mat(k,1067) = -(rxt(k,437) + rxt(k,448)) * y(k,40)

         mat(k,1299) = rxt(k,236)*y(k,38)
         mat(k,815) = rxt(k,236)*y(k,24)

         mat(k,271) = -(rxt(k,246)*y(k,2))
         mat(k,1439) = -rxt(k,246)*y(k,41)

         mat(k,1370) = rxt(k,238)*y(k,38)
         mat(k,814) = rxt(k,238)*y(k,10)

         mat(k,366) = -((rxt(k,451) + rxt(k,452)) * y(k,2) + rxt(k,459)*y(k,4) &
                      + rxt(k,463)*y(k,132))
         mat(k,1442) = -(rxt(k,451) + rxt(k,452)) * y(k,127)
         mat(k,836) = -rxt(k,459)*y(k,127)
         mat(k,437) = -rxt(k,463)*y(k,127)

         mat(k,420) = -(rxt(k,454)*y(k,8) + rxt(k,455)*y(k,9) + rxt(k,462)*y(k,132))
         mat(k,486) = -rxt(k,454)*y(k,128)
         mat(k,942) = -rxt(k,455)*y(k,128)
         mat(k,438) = -rxt(k,462)*y(k,128)

         mat(k,837) = rxt(k,459)*y(k,127) + rxt(k,456)*y(k,129) + rxt(k,449)*y(k,130)
         mat(k,367) = rxt(k,459)*y(k,4)
         mat(k,225) = rxt(k,456)*y(k,4)
         mat(k,332) = rxt(k,449)*y(k,4)

         mat(k,223) = -((rxt(k,456) + rxt(k,457)) * y(k,4) + rxt(k,458)*y(k,2))
         mat(k,833) = -(rxt(k,456) + rxt(k,457)) * y(k,129)
         mat(k,1437) = -rxt(k,458)*y(k,129)

         mat(k,331) = -(rxt(k,449)*y(k,4))
         mat(k,835) = -rxt(k,449)*y(k,130)

         mat(k,1440) = rxt(k,452)*y(k,127) + rxt(k,458)*y(k,129)
         mat(k,365) = rxt(k,452)*y(k,2)
         mat(k,224) = rxt(k,458)*y(k,2)

      end do

      end subroutine nlnmat04

      subroutine nlnmat05( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,429) = -(rxt(k,461)*y(k,132))
         mat(k,439) = -rxt(k,461)*y(k,131)

         mat(k,1445) = rxt(k,451)*y(k,127)
         mat(k,838) = rxt(k,457)*y(k,129)
         mat(k,487) = rxt(k,454)*y(k,128)
         mat(k,943) = rxt(k,455)*y(k,128)
         mat(k,368) = rxt(k,451)*y(k,2)
         mat(k,421) = rxt(k,454)*y(k,8) + rxt(k,455)*y(k,9)
         mat(k,226) = rxt(k,457)*y(k,4)

         mat(k,250) = -(rxt(k,179)*y(k,4) + rxt(k,180)*y(k,2))
         mat(k,834) = -rxt(k,179)*y(k,133)
         mat(k,1438) = -rxt(k,180)*y(k,133)

         mat(k,1438) = mat(k,1438) + rxt(k,451)*y(k,127)
         mat(k,364) = rxt(k,451)*y(k,2) + .900_rkind_comp*rxt(k,463)*y(k,132)
         mat(k,428) = .800_rkind_comp*rxt(k,461)*y(k,132)
         mat(k,436) = .900_rkind_comp*rxt(k,463)*y(k,127) + .800_rkind_comp*rxt(k,461)*y(k,131)

         mat(k,440) = -(rxt(k,461)*y(k,131) + rxt(k,462)*y(k,128) + rxt(k,463) &
                      *y(k,127))
         mat(k,430) = -rxt(k,461)*y(k,132)
         mat(k,422) = -rxt(k,462)*y(k,132)
         mat(k,369) = -rxt(k,463)*y(k,132)

         mat(k,594) = -(rxt(k,310)*y(k,23) + rxt(k,311)*y(k,1) + rxt(k,312)*y(k,11))
         mat(k,1155) = -rxt(k,310)*y(k,59)
         mat(k,1199) = -rxt(k,311)*y(k,59)
         mat(k,996) = -rxt(k,312)*y(k,59)

         mat(k,1199) = mat(k,1199) + .070_rkind_comp*rxt(k,357)*y(k,83)
         mat(k,536) = .070_rkind_comp*rxt(k,357)*y(k,1)

         mat(k,534) = -(rxt(k,356)*y(k,23) + rxt(k,357)*y(k,1) + rxt(k,358)*y(k,11))
         mat(k,1150) = -rxt(k,356)*y(k,83)
         mat(k,1195) = -rxt(k,357)*y(k,83)
         mat(k,992) = -rxt(k,358)*y(k,83)

         mat(k,467) = -(rxt(k,318)*y(k,9) + rxt(k,319)*y(k,24))
         mat(k,946) = -rxt(k,318)*y(k,64)
         mat(k,1302) = -rxt(k,319)*y(k,64)

         mat(k,1144) = rxt(k,310)*y(k,59) + .500_rkind_comp*rxt(k,320)*y(k,65)
         mat(k,593) = rxt(k,310)*y(k,23)
         mat(k,288) = .500_rkind_comp*rxt(k,320)*y(k,23)

         mat(k,614) = -(rxt(k,297)*y(k,23) + rxt(k,298)*y(k,11))
         mat(k,1157) = -rxt(k,297)*y(k,50)
         mat(k,998) = -rxt(k,298)*y(k,50)

         mat(k,1201) = .500_rkind_comp*rxt(k,311)*y(k,59) + .040_rkind_comp*rxt(k,333)*y(k,77)
         mat(k,954) = rxt(k,318)*y(k,64) + rxt(k,331)*y(k,73) + .400_rkind_comp*rxt(k,372) &
                      *y(k,84) + rxt(k,335)*y(k,74) + rxt(k,292)*y(k,46) &
                      + .270_rkind_comp*rxt(k,313)*y(k,61)
         mat(k,1157) = mat(k,1157) + .500_rkind_comp*rxt(k,296)*y(k,47) + rxt(k,307)*y(k,51)
         mat(k,1036) = .800_rkind_comp*rxt(k,294)*y(k,46)
         mat(k,596) = .500_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,468) = rxt(k,318)*y(k,9)
         mat(k,134) = rxt(k,331)*y(k,9)
         mat(k,383) = .400_rkind_comp*rxt(k,372)*y(k,9)
         mat(k,358) = rxt(k,335)*y(k,9)
         mat(k,746) = .040_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,502) = rxt(k,292)*y(k,9) + .800_rkind_comp*rxt(k,294)*y(k,16) &
                      + 3.200_rkind_comp*rxt(k,295)*y(k,46)
         mat(k,151) = .500_rkind_comp*rxt(k,296)*y(k,23)
         mat(k,521) = .270_rkind_comp*rxt(k,313)*y(k,9)
         mat(k,107) = rxt(k,307)*y(k,23)

         mat(k,327) = -(rxt(k,291)*y(k,23))
         mat(k,1135) = -rxt(k,291)*y(k,49)

         mat(k,1193) = .250_rkind_comp*rxt(k,311)*y(k,59) + .200_rkind_comp*rxt(k,357)*y(k,83)
         mat(k,1029) = .100_rkind_comp*rxt(k,302)*y(k,48)
         mat(k,1292) = .250_rkind_comp*rxt(k,301)*y(k,48) + .250_rkind_comp*rxt(k,349)*y(k,76)
         mat(k,592) = .250_rkind_comp*rxt(k,311)*y(k,1)
         mat(k,532) = .200_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,787) = .100_rkind_comp*rxt(k,302)*y(k,16) + .250_rkind_comp*rxt(k,301)*y(k,24)
         mat(k,767) = .250_rkind_comp*rxt(k,349)*y(k,24)

         mat(k,287) = -(rxt(k,320)*y(k,23))
         mat(k,1130) = -rxt(k,320)*y(k,65)

         mat(k,1288) = rxt(k,319)*y(k,64)
         mat(k,466) = rxt(k,319)*y(k,24)

         mat(k,799) = -(rxt(k,299)*y(k,9) + rxt(k,300)*y(k,10) + rxt(k,301)*y(k,24) &
                      + rxt(k,302)*y(k,16) + 4._rkind_comp*rxt(k,303)*y(k,48) + rxt(k,345) &
                      *y(k,79) + rxt(k,364)*y(k,88) + rxt(k,379)*y(k,91))
         mat(k,964) = -rxt(k,299)*y(k,48)
         mat(k,1381) = -rxt(k,300)*y(k,48)
         mat(k,1322) = -rxt(k,301)*y(k,48)
         mat(k,1046) = -rxt(k,302)*y(k,48)
         mat(k,733) = -rxt(k,345)*y(k,48)
         mat(k,711) = -rxt(k,364)*y(k,48)
         mat(k,685) = -rxt(k,379)*y(k,48)

         mat(k,964) = mat(k,964) + rxt(k,335)*y(k,74) + .530_rkind_comp*rxt(k,340)*y(k,79) &
                      + rxt(k,347)*y(k,76) + rxt(k,322)*y(k,67)
         mat(k,1168) = rxt(k,297)*y(k,50) + .500_rkind_comp*rxt(k,304)*y(k,54) + rxt(k,327) &
                      *y(k,68)
         mat(k,1008) = rxt(k,298)*y(k,50) + .530_rkind_comp*rxt(k,342)*y(k,79) + rxt(k,348) &
                      *y(k,76) + rxt(k,328)*y(k,68)
         mat(k,1046) = mat(k,1046) + .260_rkind_comp*rxt(k,344)*y(k,79) + rxt(k,350)*y(k,76) &
                      + .300_rkind_comp*rxt(k,324)*y(k,67)
         mat(k,615) = rxt(k,297)*y(k,23) + rxt(k,298)*y(k,11)
         mat(k,799) = mat(k,799) + .530_rkind_comp*rxt(k,345)*y(k,79)
         mat(k,245) = .500_rkind_comp*rxt(k,304)*y(k,23)
         mat(k,359) = rxt(k,335)*y(k,9)
         mat(k,733) = mat(k,733) + .530_rkind_comp*rxt(k,340)*y(k,9) + .530_rkind_comp*rxt(k,342) &
                      *y(k,11) + .260_rkind_comp*rxt(k,344)*y(k,16) + .530_rkind_comp*rxt(k,345) &
                      *y(k,48)
         mat(k,771) = rxt(k,347)*y(k,9) + rxt(k,348)*y(k,11) + rxt(k,350)*y(k,16) &
                      + 4.000_rkind_comp*rxt(k,352)*y(k,76)
         mat(k,628) = rxt(k,322)*y(k,9) + .300_rkind_comp*rxt(k,324)*y(k,16)
         mat(k,670) = rxt(k,327)*y(k,23) + rxt(k,328)*y(k,11)

         mat(k,244) = -(rxt(k,304)*y(k,23))
         mat(k,1125) = -rxt(k,304)*y(k,54)

         mat(k,1286) = .750_rkind_comp*rxt(k,301)*y(k,48) + .750_rkind_comp*rxt(k,349)*y(k,76)
         mat(k,786) = .750_rkind_comp*rxt(k,301)*y(k,24)
         mat(k,765) = .750_rkind_comp*rxt(k,349)*y(k,24)

         mat(k,237) = -(rxt(k,309)*y(k,23))
         mat(k,1124) = -rxt(k,309)*y(k,58)

         mat(k,1369) = rxt(k,300)*y(k,48)
         mat(k,785) = rxt(k,300)*y(k,10)

         mat(k,157) = -(rxt(k,329)*y(k,23))
         mat(k,1114) = -rxt(k,329)*y(k,82)

         mat(k,928) = .100_rkind_comp*rxt(k,372)*y(k,84)
         mat(k,988) = rxt(k,312)*y(k,59)
         mat(k,591) = rxt(k,312)*y(k,11)
         mat(k,376) = .100_rkind_comp*rxt(k,372)*y(k,9)

         mat(k,114) = -(rxt(k,282)*y(k,28) + rxt(k,284)*y(k,23))
         mat(k,1402) = -rxt(k,282)*y(k,45)
         mat(k,1107) = -rxt(k,284)*y(k,45)

         mat(k,255) = -(rxt(k,281)*y(k,28) + rxt(k,285)*y(k,23) + rxt(k,290)*y(k,1))
         mat(k,1404) = -rxt(k,281)*y(k,44)
         mat(k,1126) = -rxt(k,285)*y(k,44)
         mat(k,1190) = -rxt(k,290)*y(k,44)

         mat(k,33) = -(rxt(k,368)*y(k,23))
         mat(k,1090) = -rxt(k,368)*y(k,71)

         mat(k,279) = -(rxt(k,355)*y(k,23))
         mat(k,1129) = -rxt(k,355)*y(k,81)

         mat(k,1371) = rxt(k,353)*y(k,76)
         mat(k,766) = rxt(k,353)*y(k,10)

         mat(k,36) = -(rxt(k,330)*y(k,23))
         mat(k,1091) = -rxt(k,330)*y(k,70)

         mat(k,132) = -(rxt(k,331)*y(k,9))
         mat(k,926) = -rxt(k,331)*y(k,73)

         mat(k,1109) = rxt(k,330)*y(k,70)
         mat(k,37) = rxt(k,330)*y(k,23)

         mat(k,380) = -(rxt(k,372)*y(k,9) + rxt(k,373)*y(k,24))
         mat(k,938) = -rxt(k,372)*y(k,84)
         mat(k,1296) = -rxt(k,373)*y(k,84)

         mat(k,1139) = rxt(k,368)*y(k,71) + rxt(k,374)*y(k,85)
         mat(k,34) = rxt(k,368)*y(k,23)
         mat(k,348) = rxt(k,374)*y(k,23)

         mat(k,346) = -(rxt(k,374)*y(k,23))
         mat(k,1137) = -rxt(k,374)*y(k,85)

         mat(k,1294) = rxt(k,373)*y(k,84)
         mat(k,378) = rxt(k,373)*y(k,24)

         mat(k,198) = -(rxt(k,334)*y(k,23))
         mat(k,1120) = -rxt(k,334)*y(k,72)

         mat(k,931) = .800_rkind_comp*rxt(k,372)*y(k,84)
         mat(k,377) = .800_rkind_comp*rxt(k,372)*y(k,9)

         mat(k,357) = -(rxt(k,335)*y(k,9) + rxt(k,336)*y(k,24))
         mat(k,937) = -rxt(k,335)*y(k,74)
         mat(k,1295) = -rxt(k,336)*y(k,74)

         mat(k,1138) = rxt(k,334)*y(k,72) + rxt(k,337)*y(k,75)
         mat(k,199) = rxt(k,334)*y(k,23)
         mat(k,140) = rxt(k,337)*y(k,23)

         mat(k,139) = -(rxt(k,337)*y(k,23))
         mat(k,1110) = -rxt(k,337)*y(k,75)

         mat(k,1273) = rxt(k,336)*y(k,74)
         mat(k,356) = rxt(k,336)*y(k,24)

         mat(k,61) = -(rxt(k,382)*y(k,23))
         mat(k,1098) = -rxt(k,382)*y(k,94)

         mat(k,66) = -(rxt(k,386)*y(k,23))
         mat(k,1099) = -rxt(k,386)*y(k,95)

         mat(k,1099) = mat(k,1099) + .250_rkind_comp*rxt(k,382)*y(k,94)
         mat(k,62) = .250_rkind_comp*rxt(k,382)*y(k,23)

         mat(k,304) = -(rxt(k,383)*y(k,9) + rxt(k,384)*y(k,24))
         mat(k,935) = -rxt(k,383)*y(k,96)
         mat(k,1290) = -rxt(k,384)*y(k,96)

         mat(k,1132) = .700_rkind_comp*rxt(k,382)*y(k,94) + rxt(k,385)*y(k,97)
         mat(k,63) = .700_rkind_comp*rxt(k,382)*y(k,23)
         mat(k,162) = rxt(k,385)*y(k,23)

         mat(k,161) = -(rxt(k,385)*y(k,23))
         mat(k,1115) = -rxt(k,385)*y(k,97)

         mat(k,1276) = rxt(k,384)*y(k,96)
         mat(k,303) = rxt(k,384)*y(k,24)

         mat(k,82) = -(rxt(k,387)*y(k,10))
         mat(k,1366) = -rxt(k,387)*y(k,98)

         mat(k,1101) = rxt(k,386)*y(k,95)
         mat(k,67) = rxt(k,386)*y(k,23)

         mat(k,568) = -(rxt(k,397)*y(k,9) + rxt(k,398)*y(k,24))
         mat(k,952) = -rxt(k,397)*y(k,106)
         mat(k,1309) = -rxt(k,398)*y(k,106)

         mat(k,1153) = rxt(k,399)*y(k,107) + rxt(k,394)*y(k,105)
         mat(k,995) = rxt(k,396)*y(k,105)
         mat(k,210) = rxt(k,399)*y(k,23)
         mat(k,319) = rxt(k,394)*y(k,23) + rxt(k,396)*y(k,11)

         mat(k,208) = -(rxt(k,399)*y(k,23))
         mat(k,1121) = -rxt(k,399)*y(k,107)

         mat(k,1283) = rxt(k,398)*y(k,106)
         mat(k,566) = rxt(k,398)*y(k,24)


         mat(k,936) = .900_rkind_comp*rxt(k,383)*y(k,96) + .900_rkind_comp*rxt(k,390)*y(k,100) &
                      + .620_rkind_comp*rxt(k,393)*y(k,103)
         mat(k,1372) = .700_rkind_comp*rxt(k,387)*y(k,98)
         mat(k,305) = .900_rkind_comp*rxt(k,383)*y(k,9)
         mat(k,83) = .700_rkind_comp*rxt(k,387)*y(k,10)
         mat(k,169) = .900_rkind_comp*rxt(k,390)*y(k,9)
         mat(k,217) = .620_rkind_comp*rxt(k,393)*y(k,9)

         mat(k,397) = -(rxt(k,306)*y(k,23))
         mat(k,1141) = -rxt(k,306)*y(k,53)

         mat(k,940) = .450_rkind_comp*rxt(k,383)*y(k,96) + .900_rkind_comp*rxt(k,390)*y(k,100) &
                      + .340_rkind_comp*rxt(k,393)*y(k,103) + .020_rkind_comp*rxt(k,359)*y(k,88) &
                      + .250_rkind_comp*rxt(k,375)*y(k,91)
         mat(k,1141) = mat(k,1141) + .200_rkind_comp*rxt(k,305)*y(k,52) + .650_rkind_comp*rxt(k,283) &
                      *y(k,135)
         mat(k,990) = .250_rkind_comp*rxt(k,376)*y(k,91)
         mat(k,1031) = .100_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,788) = .250_rkind_comp*rxt(k,379)*y(k,91)
         mat(k,306) = .450_rkind_comp*rxt(k,383)*y(k,9)
         mat(k,170) = .900_rkind_comp*rxt(k,390)*y(k,9)
         mat(k,218) = .340_rkind_comp*rxt(k,393)*y(k,9)
         mat(k,699) = .020_rkind_comp*rxt(k,359)*y(k,9)
         mat(k,551) = .200_rkind_comp*rxt(k,305)*y(k,23)
         mat(k,679) = .250_rkind_comp*rxt(k,375)*y(k,9) + .250_rkind_comp*rxt(k,376)*y(k,11) &
                      + .100_rkind_comp*rxt(k,378)*y(k,16) + .250_rkind_comp*rxt(k,379)*y(k,48)
         mat(k,122) = .650_rkind_comp*rxt(k,283)*y(k,23)

      end do

      end subroutine nlnmat05

      subroutine nlnmat06( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len 
         mat(k,39) = -(rxt(k,388)*y(k,23))
         mat(k,1092) = -rxt(k,388)*y(k,99)

         mat(k,168) = -(rxt(k,389)*y(k,24) + rxt(k,390)*y(k,9))
         mat(k,1277) = -rxt(k,389)*y(k,100)
         mat(k,929) = -rxt(k,390)*y(k,100)

         mat(k,1116) = rxt(k,388)*y(k,99)
         mat(k,40) = rxt(k,388)*y(k,23)


         mat(k,1269) = rxt(k,389)*y(k,100)
         mat(k,167) = rxt(k,389)*y(k,24)

         mat(k,42) = -(rxt(k,391)*y(k,23))
         mat(k,1093) = -rxt(k,391)*y(k,102)

         mat(k,216) = -(rxt(k,392)*y(k,24) + rxt(k,393)*y(k,9))
         mat(k,1284) = -rxt(k,392)*y(k,103)
         mat(k,933) = -rxt(k,393)*y(k,103)

         mat(k,1122) = rxt(k,391)*y(k,102)
         mat(k,43) = rxt(k,391)*y(k,23)


         mat(k,1270) = rxt(k,392)*y(k,103)
         mat(k,215) = rxt(k,392)*y(k,24)

         mat(k,707) = -(rxt(k,359)*y(k,9) + rxt(k,360)*y(k,11) + rxt(k,361)*y(k,24) &
                      + rxt(k,363)*y(k,16) + rxt(k,364)*y(k,48))
         mat(k,960) = -rxt(k,359)*y(k,88)
         mat(k,1004) = -rxt(k,360)*y(k,88)
         mat(k,1318) = -rxt(k,361)*y(k,88)
         mat(k,1042) = -rxt(k,363)*y(k,88)
         mat(k,795) = -rxt(k,364)*y(k,88)

         mat(k,1164) = rxt(k,356)*y(k,83) + .200_rkind_comp*rxt(k,362)*y(k,93)
         mat(k,539) = rxt(k,356)*y(k,23)
         mat(k,298) = .200_rkind_comp*rxt(k,362)*y(k,23)

         mat(k,750) = -(rxt(k,332)*y(k,23) + rxt(k,333)*y(k,1))
         mat(k,1166) = -rxt(k,332)*y(k,77)
         mat(k,1209) = -rxt(k,333)*y(k,77)

         mat(k,1209) = mat(k,1209) + .200_rkind_comp*rxt(k,357)*y(k,83) + rxt(k,395)*y(k,105)
         mat(k,962) = rxt(k,397)*y(k,106) + .320_rkind_comp*rxt(k,359)*y(k,88) &
                      + .039_rkind_comp*rxt(k,365)*y(k,89)
         mat(k,1006) = .350_rkind_comp*rxt(k,360)*y(k,88) + .039_rkind_comp*rxt(k,366)*y(k,89)
         mat(k,1044) = .260_rkind_comp*rxt(k,363)*y(k,88)
         mat(k,1320) = .039_rkind_comp*rxt(k,367)*y(k,89)
         mat(k,540) = .200_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,797) = .350_rkind_comp*rxt(k,364)*y(k,88)
         mat(k,571) = rxt(k,397)*y(k,9)
         mat(k,709) = .320_rkind_comp*rxt(k,359)*y(k,9) + .350_rkind_comp*rxt(k,360)*y(k,11) &
                      + .260_rkind_comp*rxt(k,363)*y(k,16) + .350_rkind_comp*rxt(k,364)*y(k,48)
         mat(k,321) = rxt(k,395)*y(k,1)
         mat(k,459) = .039_rkind_comp*rxt(k,365)*y(k,9) + .039_rkind_comp*rxt(k,366)*y(k,11) &
                      + .039_rkind_comp*rxt(k,367)*y(k,24)

         mat(k,644) = -(rxt(k,338)*y(k,23) + rxt(k,339)*y(k,1))
         mat(k,1160) = -rxt(k,338)*y(k,78)
         mat(k,1204) = -rxt(k,339)*y(k,78)

         mat(k,1204) = mat(k,1204) + .400_rkind_comp*rxt(k,357)*y(k,83) + rxt(k,395)*y(k,105)
         mat(k,957) = rxt(k,397)*y(k,106) + .230_rkind_comp*rxt(k,359)*y(k,88) &
                      + .167_rkind_comp*rxt(k,365)*y(k,89)
         mat(k,1001) = .250_rkind_comp*rxt(k,360)*y(k,88) + .167_rkind_comp*rxt(k,366)*y(k,89)
         mat(k,1039) = .190_rkind_comp*rxt(k,363)*y(k,88)
         mat(k,1314) = .167_rkind_comp*rxt(k,367)*y(k,89)
         mat(k,538) = .400_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,792) = .250_rkind_comp*rxt(k,364)*y(k,88)
         mat(k,570) = rxt(k,397)*y(k,9)
         mat(k,704) = .230_rkind_comp*rxt(k,359)*y(k,9) + .250_rkind_comp*rxt(k,360)*y(k,11) &
                      + .190_rkind_comp*rxt(k,363)*y(k,16) + .250_rkind_comp*rxt(k,364)*y(k,48)
         mat(k,320) = rxt(k,395)*y(k,1)
         mat(k,458) = .167_rkind_comp*rxt(k,365)*y(k,9) + .167_rkind_comp*rxt(k,366)*y(k,11) &
                      + .167_rkind_comp*rxt(k,367)*y(k,24)

         mat(k,731) = -((rxt(k,340) + rxt(k,341)) * y(k,9) + rxt(k,342)*y(k,11) &
                      + rxt(k,343)*y(k,24) + rxt(k,344)*y(k,16) + rxt(k,345)*y(k,48))
         mat(k,961) = -(rxt(k,340) + rxt(k,341)) * y(k,79)
         mat(k,1005) = -rxt(k,342)*y(k,79)
         mat(k,1319) = -rxt(k,343)*y(k,79)
         mat(k,1043) = -rxt(k,344)*y(k,79)
         mat(k,796) = -rxt(k,345)*y(k,79)

         mat(k,1165) = rxt(k,332)*y(k,77) + .500_rkind_comp*rxt(k,338)*y(k,78) &
                      + .200_rkind_comp*rxt(k,346)*y(k,80)
         mat(k,749) = rxt(k,332)*y(k,23)
         mat(k,646) = .500_rkind_comp*rxt(k,338)*y(k,23)
         mat(k,145) = .200_rkind_comp*rxt(k,346)*y(k,23)

         mat(k,144) = -(rxt(k,346)*y(k,23))
         mat(k,1111) = -rxt(k,346)*y(k,80)

         mat(k,1274) = rxt(k,343)*y(k,79)
         mat(k,723) = rxt(k,343)*y(k,24)

         mat(k,770) = -(rxt(k,347)*y(k,9) + rxt(k,348)*y(k,11) + rxt(k,349)*y(k,24) &
                      + rxt(k,350)*y(k,16) + rxt(k,351)*y(k,48) + 4._rkind_comp*rxt(k,352) &
                      *y(k,76) + rxt(k,353)*y(k,10))
         mat(k,963) = -rxt(k,347)*y(k,76)
         mat(k,1007) = -rxt(k,348)*y(k,76)
         mat(k,1321) = -rxt(k,349)*y(k,76)
         mat(k,1045) = -rxt(k,350)*y(k,76)
         mat(k,798) = -rxt(k,351)*y(k,76)
         mat(k,1380) = -rxt(k,353)*y(k,76)

         mat(k,1210) = .200_rkind_comp*rxt(k,357)*y(k,83)
         mat(k,1167) = .500_rkind_comp*rxt(k,338)*y(k,78) + .500_rkind_comp*rxt(k,346)*y(k,80)
         mat(k,541) = .200_rkind_comp*rxt(k,357)*y(k,1)
         mat(k,647) = .500_rkind_comp*rxt(k,338)*y(k,23)
         mat(k,146) = .500_rkind_comp*rxt(k,346)*y(k,23)

         mat(k,501) = -(rxt(k,292)*y(k,9) + rxt(k,293)*y(k,24) + rxt(k,294)*y(k,16) &
                      + 4._rkind_comp*rxt(k,295)*y(k,46))
         mat(k,948) = -rxt(k,292)*y(k,46)
         mat(k,1304) = -rxt(k,293)*y(k,46)
         mat(k,1032) = -rxt(k,294)*y(k,46)

         mat(k,1147) = rxt(k,284)*y(k,45) + .500_rkind_comp*rxt(k,296)*y(k,47)
         mat(k,1409) = rxt(k,282)*y(k,45)
         mat(k,115) = rxt(k,284)*y(k,23) + rxt(k,282)*y(k,28)
         mat(k,150) = .500_rkind_comp*rxt(k,296)*y(k,23)

         mat(k,149) = -(rxt(k,296)*y(k,23))
         mat(k,1112) = -rxt(k,296)*y(k,47)

         mat(k,1275) = rxt(k,293)*y(k,46)
         mat(k,499) = rxt(k,293)*y(k,24)

         mat(k,318) = -(rxt(k,394)*y(k,23) + rxt(k,395)*y(k,1) + rxt(k,396)*y(k,11))
         mat(k,1134) = -rxt(k,394)*y(k,105)
         mat(k,1192) = -rxt(k,395)*y(k,105)
         mat(k,989) = -rxt(k,396)*y(k,105)

         mat(k,51) = -(rxt(k,317)*y(k,23))
         mat(k,1096) = -rxt(k,317)*y(k,60)

         mat(k,520) = -(rxt(k,313)*y(k,9) + rxt(k,314)*y(k,24) + rxt(k,315)*y(k,16))
         mat(k,949) = -rxt(k,313)*y(k,61)
         mat(k,1306) = -rxt(k,314)*y(k,61)
         mat(k,1033) = -rxt(k,315)*y(k,61)

         mat(k,1149) = rxt(k,317)*y(k,60) + rxt(k,316)*y(k,62)
         mat(k,52) = rxt(k,317)*y(k,23)
         mat(k,176) = rxt(k,316)*y(k,23)

         mat(k,174) = -(rxt(k,316)*y(k,23))
         mat(k,1117) = -rxt(k,316)*y(k,62)

         mat(k,1278) = rxt(k,314)*y(k,61)
         mat(k,518) = rxt(k,314)*y(k,24)

         mat(k,391) = -(rxt(k,321)*y(k,23))
         mat(k,1140) = -rxt(k,321)*y(k,63)

         mat(k,939) = .500_rkind_comp*rxt(k,331)*y(k,73) + .250_rkind_comp*rxt(k,372)*y(k,84) &
                      + .100_rkind_comp*rxt(k,397)*y(k,106) + .820_rkind_comp*rxt(k,313)*y(k,61)
         mat(k,1030) = .820_rkind_comp*rxt(k,315)*y(k,61)
         mat(k,133) = .500_rkind_comp*rxt(k,331)*y(k,9)
         mat(k,381) = .250_rkind_comp*rxt(k,372)*y(k,9)
         mat(k,567) = .100_rkind_comp*rxt(k,397)*y(k,9)
         mat(k,519) = .820_rkind_comp*rxt(k,313)*y(k,9) + .820_rkind_comp*rxt(k,315)*y(k,16)

         mat(k,180) = -(rxt(k,325)*y(k,23))
         mat(k,1118) = -rxt(k,325)*y(k,69)

         mat(k,1279) = rxt(k,323)*y(k,67)
         mat(k,623) = rxt(k,323)*y(k,24)

         mat(k,267) = -(rxt(k,274)*y(k,23))
         mat(k,1128) = -rxt(k,274)*y(k,18)

         mat(k,1028) = 2.000_rkind_comp*rxt(k,273)*y(k,16) + .250_rkind_comp*rxt(k,363)*y(k,88) &
                      + .250_rkind_comp*rxt(k,344)*y(k,79) + .300_rkind_comp*rxt(k,294)*y(k,46) &
                      + .500_rkind_comp*rxt(k,324)*y(k,67) + .300_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,697) = .250_rkind_comp*rxt(k,363)*y(k,16)
         mat(k,724) = .250_rkind_comp*rxt(k,344)*y(k,16)
         mat(k,500) = .300_rkind_comp*rxt(k,294)*y(k,16)
         mat(k,624) = .500_rkind_comp*rxt(k,324)*y(k,16)
         mat(k,678) = .300_rkind_comp*rxt(k,378)*y(k,16)

         mat(k,106) = -(rxt(k,307)*y(k,23))
         mat(k,1105) = -rxt(k,307)*y(k,51)

         mat(k,1025) = .200_rkind_comp*rxt(k,294)*y(k,46)
         mat(k,498) = .200_rkind_comp*rxt(k,294)*y(k,16) + .800_rkind_comp*rxt(k,295)*y(k,46)

         mat(k,552) = -(rxt(k,305)*y(k,23))
         mat(k,1151) = -rxt(k,305)*y(k,52)

         mat(k,841) = rxt(k,288)*y(k,56)
         mat(k,950) = .020_rkind_comp*rxt(k,359)*y(k,88) + .530_rkind_comp*rxt(k,340)*y(k,79) &
                      + .250_rkind_comp*rxt(k,375)*y(k,91)
         mat(k,993) = .530_rkind_comp*rxt(k,342)*y(k,79) + .250_rkind_comp*rxt(k,376)*y(k,91)
         mat(k,1034) = .260_rkind_comp*rxt(k,344)*y(k,79) + .100_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,789) = .530_rkind_comp*rxt(k,345)*y(k,79) + .250_rkind_comp*rxt(k,379)*y(k,91)
         mat(k,700) = .020_rkind_comp*rxt(k,359)*y(k,9)
         mat(k,725) = .530_rkind_comp*rxt(k,340)*y(k,9) + .530_rkind_comp*rxt(k,342)*y(k,11) &
                      + .260_rkind_comp*rxt(k,344)*y(k,16) + .530_rkind_comp*rxt(k,345)*y(k,48)
         mat(k,204) = rxt(k,288)*y(k,4)
         mat(k,680) = .250_rkind_comp*rxt(k,375)*y(k,9) + .250_rkind_comp*rxt(k,376)*y(k,11) &
                      + .100_rkind_comp*rxt(k,378)*y(k,16) + .250_rkind_comp*rxt(k,379)*y(k,48)

         mat(k,637) = -(rxt(k,326)*y(k,23))
         mat(k,1159) = -rxt(k,326)*y(k,66)

         mat(k,956) = .020_rkind_comp*rxt(k,359)*y(k,88) + .220_rkind_comp*rxt(k,340)*y(k,79) &
                      + .250_rkind_comp*rxt(k,375)*y(k,91)
         mat(k,1159) = mat(k,1159) + .500_rkind_comp*rxt(k,320)*y(k,65) + .500_rkind_comp*rxt(k,355) &
                      *y(k,81)
         mat(k,1000) = .220_rkind_comp*rxt(k,342)*y(k,79) + .250_rkind_comp*rxt(k,376)*y(k,91)
         mat(k,1038) = .230_rkind_comp*rxt(k,344)*y(k,79) + .200_rkind_comp*rxt(k,324)*y(k,67) &
                      + .100_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,290) = .500_rkind_comp*rxt(k,320)*y(k,23)
         mat(k,791) = .220_rkind_comp*rxt(k,345)*y(k,79) + .250_rkind_comp*rxt(k,379)*y(k,91)
         mat(k,280) = .500_rkind_comp*rxt(k,355)*y(k,23)
         mat(k,703) = .020_rkind_comp*rxt(k,359)*y(k,9)
         mat(k,728) = .220_rkind_comp*rxt(k,340)*y(k,9) + .220_rkind_comp*rxt(k,342)*y(k,11) &
                      + .230_rkind_comp*rxt(k,344)*y(k,16) + .220_rkind_comp*rxt(k,345)*y(k,48)
         mat(k,626) = .200_rkind_comp*rxt(k,324)*y(k,16)
         mat(k,682) = .250_rkind_comp*rxt(k,375)*y(k,9) + .250_rkind_comp*rxt(k,376)*y(k,11) &
                      + .100_rkind_comp*rxt(k,378)*y(k,16) + .250_rkind_comp*rxt(k,379)*y(k,48)

         mat(k,412) = -(rxt(k,286)*y(k,9) + rxt(k,287)*y(k,24))
         mat(k,941) = -rxt(k,286)*y(k,55)
         mat(k,1300) = -rxt(k,287)*y(k,55)

         mat(k,1142) = rxt(k,285)*y(k,44)
         mat(k,257) = rxt(k,285)*y(k,23)

         mat(k,203) = -(rxt(k,288)*y(k,4))
         mat(k,832) = -rxt(k,288)*y(k,56)

         mat(k,932) = .750_rkind_comp*rxt(k,286)*y(k,55)
         mat(k,411) = .750_rkind_comp*rxt(k,286)*y(k,9)


         mat(k,1271) = rxt(k,287)*y(k,55)
         mat(k,410) = rxt(k,287)*y(k,24)

         mat(k,154) = -(rxt(k,371)*y(k,23))
         mat(k,1113) = -rxt(k,371)*y(k,87)

         mat(k,927) = .330_rkind_comp*rxt(k,359)*y(k,88)
         mat(k,1113) = mat(k,1113) + rxt(k,369)*y(k,90)
         mat(k,987) = .400_rkind_comp*rxt(k,360)*y(k,88) + rxt(k,370)*y(k,90)
         mat(k,1026) = .300_rkind_comp*rxt(k,363)*y(k,88)
         mat(k,784) = .400_rkind_comp*rxt(k,364)*y(k,88)
         mat(k,696) = .330_rkind_comp*rxt(k,359)*y(k,9) + .400_rkind_comp*rxt(k,360)*y(k,11) &
                      + .300_rkind_comp*rxt(k,363)*y(k,16) + .400_rkind_comp*rxt(k,364)*y(k,48)
         mat(k,557) = rxt(k,369)*y(k,23) + rxt(k,370)*y(k,11)

      end do

      end subroutine nlnmat06

      subroutine nlnmat07( avec_len, mat, y, rxt )



      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)


!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,625) = -(rxt(k,322)*y(k,9) + rxt(k,323)*y(k,24) + rxt(k,324)*y(k,16))
         mat(k,955) = -rxt(k,322)*y(k,67)
         mat(k,1312) = -rxt(k,323)*y(k,67)
         mat(k,1037) = -rxt(k,324)*y(k,67)

         mat(k,1158) = rxt(k,321)*y(k,63) + rxt(k,325)*y(k,69)
         mat(k,392) = rxt(k,321)*y(k,23)
         mat(k,181) = rxt(k,325)*y(k,23)

         mat(k,669) = -(rxt(k,327)*y(k,23) + rxt(k,328)*y(k,11))
         mat(k,1162) = -rxt(k,327)*y(k,68)
         mat(k,1002) = -rxt(k,328)*y(k,68)

         mat(k,1205) = .950_rkind_comp*rxt(k,333)*y(k,77) + .800_rkind_comp*rxt(k,339)*y(k,78)
         mat(k,958) = .450_rkind_comp*rxt(k,383)*y(k,96) + .540_rkind_comp*rxt(k,393)*y(k,103) &
                      + .020_rkind_comp*rxt(k,359)*y(k,88) + .250_rkind_comp*rxt(k,340)*y(k,79) &
                      + .250_rkind_comp*rxt(k,375)*y(k,91)
         mat(k,1162) = mat(k,1162) + rxt(k,329)*y(k,82) + rxt(k,326)*y(k,66)
         mat(k,1002) = mat(k,1002) + .250_rkind_comp*rxt(k,342)*y(k,79) + .250_rkind_comp*rxt(k,376) &
                      *y(k,91)
         mat(k,1040) = .240_rkind_comp*rxt(k,344)*y(k,79) + .500_rkind_comp*rxt(k,324)*y(k,67) &
                      + .100_rkind_comp*rxt(k,378)*y(k,91)
         mat(k,793) = .250_rkind_comp*rxt(k,345)*y(k,79) + .250_rkind_comp*rxt(k,379)*y(k,91)
         mat(k,158) = rxt(k,329)*y(k,23)
         mat(k,307) = .450_rkind_comp*rxt(k,383)*y(k,9)
         mat(k,219) = .540_rkind_comp*rxt(k,393)*y(k,9)
         mat(k,705) = .020_rkind_comp*rxt(k,359)*y(k,9)
         mat(k,748) = .950_rkind_comp*rxt(k,333)*y(k,1)
         mat(k,645) = .800_rkind_comp*rxt(k,339)*y(k,1)
         mat(k,729) = .250_rkind_comp*rxt(k,340)*y(k,9) + .250_rkind_comp*rxt(k,342)*y(k,11) &
                      + .240_rkind_comp*rxt(k,344)*y(k,16) + .250_rkind_comp*rxt(k,345)*y(k,48)
         mat(k,638) = rxt(k,326)*y(k,23)
         mat(k,627) = .500_rkind_comp*rxt(k,324)*y(k,16)
         mat(k,683) = .250_rkind_comp*rxt(k,375)*y(k,9) + .250_rkind_comp*rxt(k,376)*y(k,11) &
                      + .100_rkind_comp*rxt(k,378)*y(k,16) + .250_rkind_comp*rxt(k,379)*y(k,48)

         mat(k,456) = -(rxt(k,365)*y(k,9) + rxt(k,366)*y(k,11) + rxt(k,367)*y(k,24))
         mat(k,945) = -rxt(k,365)*y(k,89)
         mat(k,991) = -rxt(k,366)*y(k,89)
         mat(k,1301) = -rxt(k,367)*y(k,89)

         mat(k,991) = mat(k,991) + rxt(k,358)*y(k,83)
         mat(k,533) = rxt(k,358)*y(k,11)

         mat(k,558) = -(rxt(k,369)*y(k,23) + rxt(k,370)*y(k,11))
         mat(k,1152) = -rxt(k,369)*y(k,90)
         mat(k,994) = -rxt(k,370)*y(k,90)

         mat(k,951) = .080_rkind_comp*rxt(k,359)*y(k,88) + .800_rkind_comp*rxt(k,341)*y(k,79) &
                      + .794_rkind_comp*rxt(k,365)*y(k,89)
         mat(k,994) = mat(k,994) + .794_rkind_comp*rxt(k,366)*y(k,89)
         mat(k,1308) = .794_rkind_comp*rxt(k,367)*y(k,89)
         mat(k,701) = .080_rkind_comp*rxt(k,359)*y(k,9)
         mat(k,726) = .800_rkind_comp*rxt(k,341)*y(k,9)
         mat(k,457) = .794_rkind_comp*rxt(k,365)*y(k,9) + .794_rkind_comp*rxt(k,366)*y(k,11) &
                      + .794_rkind_comp*rxt(k,367)*y(k,24)

         mat(k,684) = -(rxt(k,375)*y(k,9) + rxt(k,376)*y(k,11) + rxt(k,377)*y(k,24) &
                      + rxt(k,378)*y(k,16) + rxt(k,379)*y(k,48))
         mat(k,959) = -rxt(k,375)*y(k,91)
         mat(k,1003) = -rxt(k,376)*y(k,91)
         mat(k,1317) = -rxt(k,377)*y(k,91)
         mat(k,1041) = -rxt(k,378)*y(k,91)
         mat(k,794) = -rxt(k,379)*y(k,91)

         mat(k,1163) = rxt(k,371)*y(k,87) + rxt(k,380)*y(k,92) + .800_rkind_comp*rxt(k,362) &
                      *y(k,93)
         mat(k,155) = rxt(k,371)*y(k,23)
         mat(k,111) = rxt(k,380)*y(k,23)
         mat(k,297) = .800_rkind_comp*rxt(k,362)*y(k,23)

         mat(k,110) = -((rxt(k,380) + rxt(k,381)) * y(k,23))
         mat(k,1106) = -(rxt(k,380) + rxt(k,381)) * y(k,92)

         mat(k,1272) = rxt(k,377)*y(k,91)
         mat(k,677) = rxt(k,377)*y(k,24)

         mat(k,295) = -(rxt(k,362)*y(k,23))
         mat(k,1131) = -rxt(k,362)*y(k,93)

         mat(k,1289) = rxt(k,361)*y(k,88)
         mat(k,698) = rxt(k,361)*y(k,24)

         mat(k,120) = -(rxt(k,280)*y(k,28) + rxt(k,283)*y(k,23))
         mat(k,1403) = -rxt(k,280)*y(k,135)
         mat(k,1108) = -rxt(k,283)*y(k,135)

         mat(k,263) = -(rxt(k,276)*y(k,23))
         mat(k,1127) = -rxt(k,276)*y(k,136)

         mat(k,1191) = .500_rkind_comp*rxt(k,290)*y(k,44)
         mat(k,934) = rxt(k,278)*y(k,137)
         mat(k,1127) = mat(k,1127) + .350_rkind_comp*rxt(k,283)*y(k,135)
         mat(k,1287) = rxt(k,279)*y(k,137)
         mat(k,256) = .500_rkind_comp*rxt(k,290)*y(k,1)
         mat(k,121) = .350_rkind_comp*rxt(k,283)*y(k,23)
         mat(k,187) = rxt(k,278)*y(k,9) + rxt(k,279)*y(k,24)

         mat(k,186) = -(rxt(k,278)*y(k,9) + rxt(k,279)*y(k,24))
         mat(k,930) = -rxt(k,278)*y(k,137)
         mat(k,1280) = -rxt(k,279)*y(k,137)

         mat(k,1231) = rxt(k,269)*y(k,24)
         mat(k,1280) = mat(k,1280) + rxt(k,269)*y(k,19)

         mat(k,72) = -(rxt(k,151)*y(k,3))
         mat(k,1342) = -rxt(k,151)*y(k,138)

         mat(k,86) = -(rxt(k,152)*y(k,3))
         mat(k,1344) = -rxt(k,152)*y(k,139)


         mat(k,655) = rxt(k,248)*y(k,141)
         mat(k,866) = rxt(k,250)*y(k,141)
         mat(k,853) = rxt(k,247)*y(k,141)
         mat(k,447) = rxt(k,248)*y(k,21) + rxt(k,250)*y(k,12) + rxt(k,247)*y(k,134)

         mat(k,448) = -(rxt(k,247)*y(k,134) + rxt(k,248)*y(k,21) + rxt(k,250)*y(k,12))
         mat(k,854) = -rxt(k,247)*y(k,141)
         mat(k,656) = -rxt(k,248)*y(k,141)
         mat(k,867) = -rxt(k,250)*y(k,141)

         mat(k,1346) = 2.000_rkind_comp*rxt(k,151)*y(k,138) + rxt(k,152)*y(k,139)
         mat(k,73) = 2.000_rkind_comp*rxt(k,151)*y(k,3)
         mat(k,87) = rxt(k,152)*y(k,3)

         mat(k,56) = -(rxt(k,404)*y(k,23))
         mat(k,1097) = -rxt(k,404)*y(k,156)

         mat(k,1097) = mat(k,1097) + (rxt(k,405)+.500_rkind_comp*rxt(k,406))*y(k,157)
         mat(k,984) = rxt(k,407)*y(k,157)
         mat(k,97) = (rxt(k,405)+.500_rkind_comp*rxt(k,406))*y(k,23) + rxt(k,407)*y(k,11)

         mat(k,98) = -((rxt(k,405) + rxt(k,406)) * y(k,23) + rxt(k,407)*y(k,11))
         mat(k,1104) = -(rxt(k,405) + rxt(k,406)) * y(k,157)
         mat(k,985) = -rxt(k,407)*y(k,157)


         mat(k,1089) = rxt(k,404)*y(k,156)
         mat(k,55) = rxt(k,404)*y(k,23)

         mat(k,45) = -(rxt(k,408)*y(k,23))
         mat(k,1094) = -rxt(k,408)*y(k,159)
      end do
      end subroutine nlnmat07
      subroutine nlnmat_finit( avec_len, mat, lmat, dti )
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(rkind_comp), intent(in) :: dti
      integer, intent(in) :: avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: lmat(veclen,nzcnt)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len 
         mat(k, 1) = lmat(k, 1)
         mat(k, 2) = lmat(k, 2)
         mat(k, 3) = lmat(k, 3)
         mat(k, 4) = lmat(k, 4)
         mat(k, 5) = lmat(k, 5)
         mat(k, 6) = lmat(k, 6)
         mat(k, 7) = lmat(k, 7)
         mat(k, 8) = lmat(k, 8)
         mat(k, 9) = lmat(k, 9)
         mat(k, 10) = lmat(k, 10)
         mat(k, 11) = lmat(k, 11)
         mat(k, 12) = lmat(k, 12)
         mat(k, 13) = lmat(k, 13)
         mat(k, 14) = lmat(k, 14)
         mat(k, 15) = lmat(k, 15)
         mat(k, 16) = lmat(k, 16)
         mat(k, 17) = lmat(k, 17)
         mat(k, 18) = lmat(k, 18)
         mat(k, 19) = lmat(k, 19)
         mat(k, 20) = lmat(k, 20)
         mat(k, 21) = lmat(k, 21)
         mat(k, 22) = lmat(k, 22)
         mat(k, 23) = lmat(k, 23)
         mat(k, 24) = lmat(k, 24)
         mat(k, 25) = lmat(k, 25)
         mat(k, 26) = lmat(k, 26)
         mat(k, 27) = lmat(k, 27)
         mat(k, 28) = lmat(k, 28)
         mat(k, 29) = lmat(k, 29)
         mat(k, 30) = lmat(k, 30)
         mat(k, 31) = lmat(k, 31)
         mat(k, 32) = lmat(k, 32)
         mat(k, 33) = mat(k, 33) + lmat(k, 33)
         mat(k, 36) = mat(k, 36) + lmat(k, 36)
         mat(k, 39) = mat(k, 39) + lmat(k, 39)
         mat(k, 42) = mat(k, 42) + lmat(k, 42)
         mat(k, 45) = mat(k, 45) + lmat(k, 45)
         mat(k, 48) = mat(k, 48) + lmat(k, 48)
         mat(k, 51) = mat(k, 51) + lmat(k, 51)
         mat(k, 56) = mat(k, 56) + lmat(k, 56)
         mat(k, 58) = lmat(k, 58)
         mat(k, 59) = lmat(k, 59)
         mat(k, 60) = lmat(k, 60)
         mat(k, 61) = mat(k, 61) + lmat(k, 61)
         mat(k, 66) = mat(k, 66) + lmat(k, 66)
         mat(k, 69) = lmat(k, 69)
         mat(k, 70) = lmat(k, 70)
         mat(k, 71) = lmat(k, 71)
         mat(k, 72) = mat(k, 72) + lmat(k, 72)
         mat(k, 73) = mat(k, 73) + lmat(k, 73)
         mat(k, 75) = mat(k, 75) + lmat(k, 75)
         mat(k, 76) = mat(k, 76) + lmat(k, 76)
         mat(k, 77) = mat(k, 77) + lmat(k, 77)
         mat(k, 78) = mat(k, 78) + lmat(k, 78)
         mat(k, 79) = lmat(k, 79)
         mat(k, 80) = lmat(k, 80)
         mat(k, 81) = lmat(k, 81)
         mat(k, 82) = mat(k, 82) + lmat(k, 82)
         mat(k, 86) = mat(k, 86) + lmat(k, 86)
         mat(k, 87) = mat(k, 87) + lmat(k, 87)
         mat(k, 89) = mat(k, 89) + lmat(k, 89)
         mat(k, 90) = mat(k, 90) + lmat(k, 90)
         mat(k, 94) = lmat(k, 94)
         mat(k, 95) = lmat(k, 95)
         mat(k, 96) = lmat(k, 96)
         mat(k, 98) = mat(k, 98) + lmat(k, 98)
         mat(k, 103) = lmat(k, 103)
         mat(k, 104) = lmat(k, 104)
         mat(k, 105) = lmat(k, 105)
         mat(k, 106) = mat(k, 106) + lmat(k, 106)
         mat(k, 110) = mat(k, 110) + lmat(k, 110)
         mat(k, 113) = mat(k, 113) + lmat(k, 113)
         mat(k, 114) = mat(k, 114) + lmat(k, 114)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 126) = lmat(k, 126)
         mat(k, 127) = lmat(k, 127)
         mat(k, 128) = lmat(k, 128)
         mat(k, 129) = lmat(k, 129)
         mat(k, 130) = lmat(k, 130)
         mat(k, 131) = lmat(k, 131)
         mat(k, 132) = mat(k, 132) + lmat(k, 132)
         mat(k, 139) = mat(k, 139) + lmat(k, 139)
         mat(k, 141) = lmat(k, 141)
         mat(k, 142) = lmat(k, 142)
         mat(k, 143) = mat(k, 143) + lmat(k, 143)
         mat(k, 144) = mat(k, 144) + lmat(k, 144)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 152) = mat(k, 152) + lmat(k, 152)
         mat(k, 153) = lmat(k, 153)
         mat(k, 154) = mat(k, 154) + lmat(k, 154)
         mat(k, 157) = mat(k, 157) + lmat(k, 157)
         mat(k, 161) = mat(k, 161) + lmat(k, 161)
         mat(k, 163) = lmat(k, 163)
         mat(k, 164) = lmat(k, 164)
         mat(k, 165) = lmat(k, 165)
         mat(k, 166) = mat(k, 166) + lmat(k, 166)
         mat(k, 168) = mat(k, 168) + lmat(k, 168)
         mat(k, 174) = mat(k, 174) + lmat(k, 174)
         mat(k, 175) = lmat(k, 175)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 179) = lmat(k, 179)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 182) = lmat(k, 182)
         mat(k, 184) = mat(k, 184) + lmat(k, 184)
         mat(k, 185) = lmat(k, 185)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 189) = lmat(k, 189)
         mat(k, 190) = mat(k, 190) + lmat(k, 190)
         mat(k, 192) = mat(k, 192) + lmat(k, 192)
         mat(k, 195) = mat(k, 195) + lmat(k, 195)
         mat(k, 196) = mat(k, 196) + lmat(k, 196)
         mat(k, 197) = lmat(k, 197)
         mat(k, 198) = mat(k, 198) + lmat(k, 198)
         mat(k, 200) = lmat(k, 200)
         mat(k, 201) = lmat(k, 201)
         mat(k, 203) = mat(k, 203) + lmat(k, 203)
         mat(k, 206) = lmat(k, 206)
         mat(k, 207) = mat(k, 207) + lmat(k, 207)
         mat(k, 208) = mat(k, 208) + lmat(k, 208)
         mat(k, 209) = lmat(k, 209)
         mat(k, 211) = lmat(k, 211)
         mat(k, 212) = lmat(k, 212)
         mat(k, 213) = mat(k, 213) + lmat(k, 213)
         mat(k, 214) = lmat(k, 214)
         mat(k, 216) = mat(k, 216) + lmat(k, 216)
         mat(k, 223) = mat(k, 223) + lmat(k, 223)
         mat(k, 230) = mat(k, 230) + lmat(k, 230)
         mat(k, 233) = lmat(k, 233)
         mat(k, 234) = mat(k, 234) + lmat(k, 234)
         mat(k, 235) = lmat(k, 235)
         mat(k, 236) = mat(k, 236) + lmat(k, 236)
         mat(k, 237) = mat(k, 237) + lmat(k, 237)
         mat(k, 238) = lmat(k, 238)
         mat(k, 239) = mat(k, 239) + lmat(k, 239)
         mat(k, 240) = lmat(k, 240)
         mat(k, 243) = lmat(k, 243)
         mat(k, 244) = mat(k, 244) + lmat(k, 244)
         mat(k, 247) = lmat(k, 247)
         mat(k, 248) = mat(k, 248) + lmat(k, 248)
         mat(k, 250) = mat(k, 250) + lmat(k, 250)
         mat(k, 255) = mat(k, 255) + lmat(k, 255)
         mat(k, 263) = mat(k, 263) + lmat(k, 263)
         mat(k, 267) = mat(k, 267) + lmat(k, 267)
         mat(k, 271) = mat(k, 271) + lmat(k, 271)
         mat(k, 272) = lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 274) = lmat(k, 274)
         mat(k, 275) = lmat(k, 275)
         mat(k, 276) = mat(k, 276) + lmat(k, 276)
         mat(k, 277) = lmat(k, 277)
         mat(k, 279) = mat(k, 279) + lmat(k, 279)
         mat(k, 281) = lmat(k, 281)
         mat(k, 286) = lmat(k, 286)
         mat(k, 287) = mat(k, 287) + lmat(k, 287)
         mat(k, 289) = lmat(k, 289)
         mat(k, 292) = mat(k, 292) + lmat(k, 292)
         mat(k, 293) = lmat(k, 293)
         mat(k, 294) = lmat(k, 294)
         mat(k, 295) = mat(k, 295) + lmat(k, 295)
         mat(k, 296) = lmat(k, 296)
         mat(k, 299) = lmat(k, 299)
         mat(k, 301) = lmat(k, 301)
         mat(k, 302) = lmat(k, 302)
         mat(k, 304) = mat(k, 304) + lmat(k, 304)
         mat(k, 312) = lmat(k, 312)
         mat(k, 313) = lmat(k, 313)
         mat(k, 314) = lmat(k, 314)
         mat(k, 315) = lmat(k, 315)
         mat(k, 316) = lmat(k, 316)
         mat(k, 317) = lmat(k, 317)
         mat(k, 318) = mat(k, 318) + lmat(k, 318)
         mat(k, 327) = mat(k, 327) + lmat(k, 327)
         mat(k, 331) = mat(k, 331) + lmat(k, 331)
         mat(k, 332) = mat(k, 332) + lmat(k, 332)
         mat(k, 333) = lmat(k, 333)
         mat(k, 334) = lmat(k, 334)
         mat(k, 335) = lmat(k, 335)
         mat(k, 338) = mat(k, 338) + lmat(k, 338)
         mat(k, 341) = mat(k, 341) + lmat(k, 341)
         mat(k, 345) = lmat(k, 345)
         mat(k, 346) = mat(k, 346) + lmat(k, 346)
         mat(k, 349) = lmat(k, 349)
         mat(k, 351) = lmat(k, 351)
         mat(k, 353) = mat(k, 353) + lmat(k, 353)
         mat(k, 354) = lmat(k, 354)
         mat(k, 355) = lmat(k, 355)
         mat(k, 357) = mat(k, 357) + lmat(k, 357)
         mat(k, 366) = mat(k, 366) + lmat(k, 366)
         mat(k, 380) = mat(k, 380) + lmat(k, 380)
         mat(k, 391) = mat(k, 391) + lmat(k, 391)
         mat(k, 393) = lmat(k, 393)
         mat(k, 395) = lmat(k, 395)
         mat(k, 397) = mat(k, 397) + lmat(k, 397)
         mat(k, 398) = mat(k, 398) + lmat(k, 398)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 402) = mat(k, 402) + lmat(k, 402)
         mat(k, 405) = lmat(k, 405)
         mat(k, 407) = mat(k, 407) + lmat(k, 407)
         mat(k, 412) = mat(k, 412) + lmat(k, 412)
         mat(k, 420) = mat(k, 420) + lmat(k, 420)
         mat(k, 421) = mat(k, 421) + lmat(k, 421)
         mat(k, 425) = mat(k, 425) + lmat(k, 425)
         mat(k, 429) = mat(k, 429) + lmat(k, 429)
         mat(k, 440) = mat(k, 440) + lmat(k, 440)
         mat(k, 447) = mat(k, 447) + lmat(k, 447)
         mat(k, 448) = mat(k, 448) + lmat(k, 448)
         mat(k, 453) = lmat(k, 453)
         mat(k, 456) = mat(k, 456) + lmat(k, 456)
         mat(k, 467) = mat(k, 467) + lmat(k, 467)
         mat(k, 477) = mat(k, 477) + lmat(k, 477)
         mat(k, 479) = mat(k, 479) + lmat(k, 479)
         mat(k, 481) = lmat(k, 481)
         mat(k, 484) = lmat(k, 484)
         mat(k, 488) = lmat(k, 488)
         mat(k, 489) = mat(k, 489) + lmat(k, 489)
         mat(k, 501) = mat(k, 501) + lmat(k, 501)
         mat(k, 511) = mat(k, 511) + lmat(k, 511)
         mat(k, 515) = mat(k, 515) + lmat(k, 515)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 520) = mat(k, 520) + lmat(k, 520)
         mat(k, 534) = mat(k, 534) + lmat(k, 534)
         mat(k, 552) = mat(k, 552) + lmat(k, 552)
         mat(k, 553) = lmat(k, 553)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 556) = mat(k, 556) + lmat(k, 556)
         mat(k, 558) = mat(k, 558) + lmat(k, 558)
         mat(k, 559) = lmat(k, 559)
         mat(k, 563) = lmat(k, 563)
         mat(k, 564) = mat(k, 564) + lmat(k, 564)
         mat(k, 565) = mat(k, 565) + lmat(k, 565)
         mat(k, 568) = mat(k, 568) + lmat(k, 568)
         mat(k, 580) = mat(k, 580) + lmat(k, 580)
         mat(k, 581) = mat(k, 581) + lmat(k, 581)
         mat(k, 583) = mat(k, 583) + lmat(k, 583)
         mat(k, 584) = mat(k, 584) + lmat(k, 584)
         mat(k, 585) = mat(k, 585) + lmat(k, 585)
         mat(k, 588) = lmat(k, 588)
         mat(k, 589) = mat(k, 589) + lmat(k, 589)
         mat(k, 594) = mat(k, 594) + lmat(k, 594)
         mat(k, 609) = mat(k, 609) + lmat(k, 609)
         mat(k, 613) = lmat(k, 613)
         mat(k, 614) = mat(k, 614) + lmat(k, 614)
         mat(k, 619) = lmat(k, 619)
         mat(k, 622) = lmat(k, 622)
         mat(k, 625) = mat(k, 625) + lmat(k, 625)
         mat(k, 637) = mat(k, 637) + lmat(k, 637)
         mat(k, 639) = lmat(k, 639)
         mat(k, 641) = lmat(k, 641)
         mat(k, 642) = mat(k, 642) + lmat(k, 642)
         mat(k, 643) = mat(k, 643) + lmat(k, 643)
         mat(k, 644) = mat(k, 644) + lmat(k, 644)
         mat(k, 647) = mat(k, 647) + lmat(k, 647)
         mat(k, 648) = lmat(k, 648)
         mat(k, 652) = mat(k, 652) + lmat(k, 652)
         mat(k, 654) = mat(k, 654) + lmat(k, 654)
         mat(k, 657) = mat(k, 657) + lmat(k, 657)
         mat(k, 668) = mat(k, 668) + lmat(k, 668)
         mat(k, 669) = mat(k, 669) + lmat(k, 669)
         mat(k, 670) = mat(k, 670) + lmat(k, 670)
         mat(k, 676) = lmat(k, 676)
         mat(k, 684) = mat(k, 684) + lmat(k, 684)
         mat(k, 707) = mat(k, 707) + lmat(k, 707)
         mat(k, 731) = mat(k, 731) + lmat(k, 731)
         mat(k, 744) = lmat(k, 744)
         mat(k, 745) = mat(k, 745) + lmat(k, 745)
         mat(k, 750) = mat(k, 750) + lmat(k, 750)
         mat(k, 752) = lmat(k, 752)
         mat(k, 758) = lmat(k, 758)
         mat(k, 770) = mat(k, 770) + lmat(k, 770)
         mat(k, 799) = mat(k, 799) + lmat(k, 799)
         mat(k, 816) = mat(k, 816) + lmat(k, 816)
         mat(k, 821) = mat(k, 821) + lmat(k, 821)
         mat(k, 829) = mat(k, 829) + lmat(k, 829)
         mat(k, 835) = mat(k, 835) + lmat(k, 835)
         mat(k, 837) = mat(k, 837) + lmat(k, 837)
         mat(k, 839) = lmat(k, 839)
         mat(k, 843) = mat(k, 843) + lmat(k, 843)
         mat(k, 850) = mat(k, 850) + lmat(k, 850)
         mat(k, 852) = mat(k, 852) + lmat(k, 852)
         mat(k, 855) = lmat(k, 855)
         mat(k, 856) = mat(k, 856) + lmat(k, 856)
         mat(k, 861) = mat(k, 861) + lmat(k, 861)
         mat(k, 862) = lmat(k, 862)
         mat(k, 863) = mat(k, 863) + lmat(k, 863)
         mat(k, 865) = lmat(k, 865)
         mat(k, 870) = mat(k, 870) + lmat(k, 870)
         mat(k, 874) = mat(k, 874) + lmat(k, 874)
         mat(k, 877) = lmat(k, 877)
         mat(k, 890) = mat(k, 890) + lmat(k, 890)
         mat(k, 903) = mat(k, 903) + lmat(k, 903)
         mat(k, 904) = mat(k, 904) + lmat(k, 904)
         mat(k, 912) = mat(k, 912) + lmat(k, 912)
         mat(k, 943) = mat(k, 943) + lmat(k, 943)
         mat(k, 944) = lmat(k, 944)
         mat(k, 947) = mat(k, 947) + lmat(k, 947)
         mat(k, 971) = mat(k, 971) + lmat(k, 971)
         mat(k, 983) = mat(k, 983) + lmat(k, 983)
         mat(k,1009) = mat(k,1009) + lmat(k,1009)
         mat(k,1011) = mat(k,1011) + lmat(k,1011)
         mat(k,1012) = mat(k,1012) + lmat(k,1012)
         mat(k,1013) = mat(k,1013) + lmat(k,1013)
         mat(k,1022) = mat(k,1022) + lmat(k,1022)
         mat(k,1024) = mat(k,1024) + lmat(k,1024)
         mat(k,1054) = mat(k,1054) + lmat(k,1054)
         mat(k,1079) = mat(k,1079) + lmat(k,1079)
         mat(k,1083) = lmat(k,1083)
         mat(k,1087) = mat(k,1087) + lmat(k,1087)
         mat(k,1100) = lmat(k,1100)
         mat(k,1102) = lmat(k,1102)
         mat(k,1171) = mat(k,1171) + lmat(k,1171)
         mat(k,1174) = mat(k,1174) + lmat(k,1174)
         mat(k,1177) = mat(k,1177) + lmat(k,1177)
         mat(k,1179) = mat(k,1179) + lmat(k,1179)
         mat(k,1183) = mat(k,1183) + lmat(k,1183)
         mat(k,1186) = mat(k,1186) + lmat(k,1186)
         mat(k,1188) = mat(k,1188) + lmat(k,1188)
         mat(k,1213) = mat(k,1213) + lmat(k,1213)
         mat(k,1223) = mat(k,1223) + lmat(k,1223)
         mat(k,1227) = mat(k,1227) + lmat(k,1227)
         mat(k,1230) = mat(k,1230) + lmat(k,1230)
         mat(k,1234) = mat(k,1234) + lmat(k,1234)
         mat(k,1235) = lmat(k,1235)
         mat(k,1245) = mat(k,1245) + lmat(k,1245)
         mat(k,1246) = mat(k,1246) + lmat(k,1246)
         mat(k,1263) = mat(k,1263) + lmat(k,1263)
         mat(k,1293) = mat(k,1293) + lmat(k,1293)
         mat(k,1337) = mat(k,1337) + lmat(k,1337)
         mat(k,1342) = mat(k,1342) + lmat(k,1342)
         mat(k,1344) = mat(k,1344) + lmat(k,1344)
         mat(k,1346) = mat(k,1346) + lmat(k,1346)
         mat(k,1348) = mat(k,1348) + lmat(k,1348)
         mat(k,1349) = mat(k,1349) + lmat(k,1349)
         mat(k,1352) = mat(k,1352) + lmat(k,1352)
         mat(k,1353) = lmat(k,1353)
         mat(k,1355) = lmat(k,1355)
         mat(k,1357) = mat(k,1357) + lmat(k,1357)
         mat(k,1359) = lmat(k,1359)
         mat(k,1360) = mat(k,1360) + lmat(k,1360)
         mat(k,1361) = lmat(k,1361)
         mat(k,1362) = mat(k,1362) + lmat(k,1362)
         mat(k,1364) = mat(k,1364) + lmat(k,1364)
         mat(k,1365) = mat(k,1365) + lmat(k,1365)
         mat(k,1385) = mat(k,1385) + lmat(k,1385)
         mat(k,1388) = mat(k,1388) + lmat(k,1388)
         mat(k,1392) = mat(k,1392) + lmat(k,1392)
         mat(k,1398) = mat(k,1398) + lmat(k,1398)
         mat(k,1400) = mat(k,1400) + lmat(k,1400)
         mat(k,1413) = mat(k,1413) + lmat(k,1413)
         mat(k,1421) = lmat(k,1421)
         mat(k,1424) = lmat(k,1424)
         mat(k,1425) = mat(k,1425) + lmat(k,1425)
         mat(k,1430) = mat(k,1430) + lmat(k,1430)
         mat(k,1433) = mat(k,1433) + lmat(k,1433)
         mat(k,1440) = mat(k,1440) + lmat(k,1440)
         mat(k,1446) = lmat(k,1446)
         mat(k,1471) = mat(k,1471) + lmat(k,1471)
         mat(k, 309) = 0._rkind_comp
         mat(k, 347) = 0._rkind_comp
         mat(k, 350) = 0._rkind_comp
         mat(k, 352) = 0._rkind_comp
         mat(k, 361) = 0._rkind_comp
         mat(k, 371) = 0._rkind_comp
         mat(k, 373) = 0._rkind_comp
         mat(k, 374) = 0._rkind_comp
         mat(k, 379) = 0._rkind_comp
         mat(k, 382) = 0._rkind_comp
         mat(k, 384) = 0._rkind_comp
         mat(k, 385) = 0._rkind_comp
         mat(k, 387) = 0._rkind_comp
         mat(k, 408) = 0._rkind_comp
         mat(k, 413) = 0._rkind_comp
         mat(k, 414) = 0._rkind_comp
         mat(k, 416) = 0._rkind_comp
         mat(k, 432) = 0._rkind_comp
         mat(k, 433) = 0._rkind_comp
         mat(k, 434) = 0._rkind_comp
         mat(k, 442) = 0._rkind_comp
         mat(k, 443) = 0._rkind_comp
         mat(k, 444) = 0._rkind_comp
         mat(k, 469) = 0._rkind_comp
         mat(k, 471) = 0._rkind_comp
         mat(k, 473) = 0._rkind_comp
         mat(k, 485) = 0._rkind_comp
         mat(k, 490) = 0._rkind_comp
         mat(k, 495) = 0._rkind_comp
         mat(k, 506) = 0._rkind_comp
         mat(k, 522) = 0._rkind_comp
         mat(k, 523) = 0._rkind_comp
         mat(k, 525) = 0._rkind_comp
         mat(k, 528) = 0._rkind_comp
         mat(k, 535) = 0._rkind_comp
         mat(k, 542) = 0._rkind_comp
         mat(k, 543) = 0._rkind_comp
         mat(k, 545) = 0._rkind_comp
         mat(k, 550) = 0._rkind_comp
         mat(k, 560) = 0._rkind_comp
         mat(k, 569) = 0._rkind_comp
         mat(k, 572) = 0._rkind_comp
         mat(k, 573) = 0._rkind_comp
         mat(k, 575) = 0._rkind_comp
         mat(k, 576) = 0._rkind_comp
         mat(k, 582) = 0._rkind_comp
         mat(k, 597) = 0._rkind_comp
         mat(k, 598) = 0._rkind_comp
         mat(k, 599) = 0._rkind_comp
         mat(k, 600) = 0._rkind_comp
         mat(k, 601) = 0._rkind_comp
         mat(k, 608) = 0._rkind_comp
         mat(k, 621) = 0._rkind_comp
         mat(k, 630) = 0._rkind_comp
         mat(k, 633) = 0._rkind_comp
         mat(k, 653) = 0._rkind_comp
         mat(k, 659) = 0._rkind_comp
         mat(k, 660) = 0._rkind_comp
         mat(k, 661) = 0._rkind_comp
         mat(k, 675) = 0._rkind_comp
         mat(k, 686) = 0._rkind_comp
         mat(k, 687) = 0._rkind_comp
         mat(k, 691) = 0._rkind_comp
         mat(k, 693) = 0._rkind_comp
         mat(k, 702) = 0._rkind_comp
         mat(k, 706) = 0._rkind_comp
         mat(k, 708) = 0._rkind_comp
         mat(k, 710) = 0._rkind_comp
         mat(k, 712) = 0._rkind_comp
         mat(k, 713) = 0._rkind_comp
         mat(k, 717) = 0._rkind_comp
         mat(k, 718) = 0._rkind_comp
         mat(k, 720) = 0._rkind_comp
         mat(k, 730) = 0._rkind_comp
         mat(k, 732) = 0._rkind_comp
         mat(k, 734) = 0._rkind_comp
         mat(k, 735) = 0._rkind_comp
         mat(k, 739) = 0._rkind_comp
         mat(k, 741) = 0._rkind_comp
         mat(k, 747) = 0._rkind_comp
         mat(k, 751) = 0._rkind_comp
         mat(k, 753) = 0._rkind_comp
         mat(k, 754) = 0._rkind_comp
         mat(k, 755) = 0._rkind_comp
         mat(k, 756) = 0._rkind_comp
         mat(k, 757) = 0._rkind_comp
         mat(k, 762) = 0._rkind_comp
         mat(k, 764) = 0._rkind_comp
         mat(k, 768) = 0._rkind_comp
         mat(k, 769) = 0._rkind_comp
         mat(k, 773) = 0._rkind_comp
         mat(k, 774) = 0._rkind_comp
         mat(k, 778) = 0._rkind_comp
         mat(k, 781) = 0._rkind_comp
         mat(k, 800) = 0._rkind_comp
         mat(k, 801) = 0._rkind_comp
         mat(k, 802) = 0._rkind_comp
         mat(k, 804) = 0._rkind_comp
         mat(k, 806) = 0._rkind_comp
         mat(k, 809) = 0._rkind_comp
         mat(k, 818) = 0._rkind_comp
         mat(k, 819) = 0._rkind_comp
         mat(k, 823) = 0._rkind_comp
         mat(k, 824) = 0._rkind_comp
         mat(k, 842) = 0._rkind_comp
         mat(k, 845) = 0._rkind_comp
         mat(k, 847) = 0._rkind_comp
         mat(k, 851) = 0._rkind_comp
         mat(k, 857) = 0._rkind_comp
         mat(k, 858) = 0._rkind_comp
         mat(k, 859) = 0._rkind_comp
         mat(k, 860) = 0._rkind_comp
         mat(k, 864) = 0._rkind_comp
         mat(k, 868) = 0._rkind_comp
         mat(k, 872) = 0._rkind_comp
         mat(k, 873) = 0._rkind_comp
         mat(k, 875) = 0._rkind_comp
         mat(k, 876) = 0._rkind_comp
         mat(k, 878) = 0._rkind_comp
         mat(k, 879) = 0._rkind_comp
         mat(k, 888) = 0._rkind_comp
         mat(k, 889) = 0._rkind_comp
         mat(k, 893) = 0._rkind_comp
         mat(k, 897) = 0._rkind_comp
         mat(k, 899) = 0._rkind_comp
         mat(k, 901) = 0._rkind_comp
         mat(k, 909) = 0._rkind_comp
         mat(k, 910) = 0._rkind_comp
         mat(k, 911) = 0._rkind_comp
         mat(k, 913) = 0._rkind_comp
         mat(k, 914) = 0._rkind_comp
         mat(k, 915) = 0._rkind_comp
         mat(k, 916) = 0._rkind_comp
         mat(k, 917) = 0._rkind_comp
         mat(k, 920) = 0._rkind_comp
         mat(k, 922) = 0._rkind_comp
         mat(k, 923) = 0._rkind_comp
         mat(k, 924) = 0._rkind_comp
         mat(k, 925) = 0._rkind_comp
         mat(k, 967) = 0._rkind_comp
         mat(k, 968) = 0._rkind_comp
         mat(k, 974) = 0._rkind_comp
         mat(k, 978) = 0._rkind_comp
         mat(k, 980) = 0._rkind_comp
         mat(k, 999) = 0._rkind_comp
         mat(k,1010) = 0._rkind_comp
         mat(k,1014) = 0._rkind_comp
         mat(k,1015) = 0._rkind_comp
         mat(k,1017) = 0._rkind_comp
         mat(k,1019) = 0._rkind_comp
         mat(k,1021) = 0._rkind_comp
         mat(k,1023) = 0._rkind_comp
         mat(k,1048) = 0._rkind_comp
         mat(k,1049) = 0._rkind_comp
         mat(k,1051) = 0._rkind_comp
         mat(k,1053) = 0._rkind_comp
         mat(k,1055) = 0._rkind_comp
         mat(k,1056) = 0._rkind_comp
         mat(k,1057) = 0._rkind_comp
         mat(k,1059) = 0._rkind_comp
         mat(k,1061) = 0._rkind_comp
         mat(k,1064) = 0._rkind_comp
         mat(k,1070) = 0._rkind_comp
         mat(k,1071) = 0._rkind_comp
         mat(k,1074) = 0._rkind_comp
         mat(k,1075) = 0._rkind_comp
         mat(k,1076) = 0._rkind_comp
         mat(k,1077) = 0._rkind_comp
         mat(k,1078) = 0._rkind_comp
         mat(k,1081) = 0._rkind_comp
         mat(k,1082) = 0._rkind_comp
         mat(k,1084) = 0._rkind_comp
         mat(k,1086) = 0._rkind_comp
         mat(k,1133) = 0._rkind_comp
         mat(k,1143) = 0._rkind_comp
         mat(k,1184) = 0._rkind_comp
         mat(k,1194) = 0._rkind_comp
         mat(k,1196) = 0._rkind_comp
         mat(k,1197) = 0._rkind_comp
         mat(k,1198) = 0._rkind_comp
         mat(k,1202) = 0._rkind_comp
         mat(k,1203) = 0._rkind_comp
         mat(k,1206) = 0._rkind_comp
         mat(k,1207) = 0._rkind_comp
         mat(k,1208) = 0._rkind_comp
         mat(k,1211) = 0._rkind_comp
         mat(k,1214) = 0._rkind_comp
         mat(k,1215) = 0._rkind_comp
         mat(k,1221) = 0._rkind_comp
         mat(k,1232) = 0._rkind_comp
         mat(k,1239) = 0._rkind_comp
         mat(k,1241) = 0._rkind_comp
         mat(k,1244) = 0._rkind_comp
         mat(k,1248) = 0._rkind_comp
         mat(k,1249) = 0._rkind_comp
         mat(k,1255) = 0._rkind_comp
         mat(k,1256) = 0._rkind_comp
         mat(k,1257) = 0._rkind_comp
         mat(k,1258) = 0._rkind_comp
         mat(k,1259) = 0._rkind_comp
         mat(k,1262) = 0._rkind_comp
         mat(k,1265) = 0._rkind_comp
         mat(k,1266) = 0._rkind_comp
         mat(k,1267) = 0._rkind_comp
         mat(k,1282) = 0._rkind_comp
         mat(k,1291) = 0._rkind_comp
         mat(k,1297) = 0._rkind_comp
         mat(k,1298) = 0._rkind_comp
         mat(k,1307) = 0._rkind_comp
         mat(k,1310) = 0._rkind_comp
         mat(k,1311) = 0._rkind_comp
         mat(k,1313) = 0._rkind_comp
         mat(k,1316) = 0._rkind_comp
         mat(k,1326) = 0._rkind_comp
         mat(k,1338) = 0._rkind_comp
         mat(k,1351) = 0._rkind_comp
         mat(k,1354) = 0._rkind_comp
         mat(k,1363) = 0._rkind_comp
         mat(k,1373) = 0._rkind_comp
         mat(k,1374) = 0._rkind_comp
         mat(k,1377) = 0._rkind_comp
         mat(k,1378) = 0._rkind_comp
         mat(k,1379) = 0._rkind_comp
         mat(k,1384) = 0._rkind_comp
         mat(k,1387) = 0._rkind_comp
         mat(k,1390) = 0._rkind_comp
         mat(k,1391) = 0._rkind_comp
         mat(k,1394) = 0._rkind_comp
         mat(k,1395) = 0._rkind_comp
         mat(k,1397) = 0._rkind_comp
         mat(k,1399) = 0._rkind_comp
         mat(k,1405) = 0._rkind_comp
         mat(k,1407) = 0._rkind_comp
         mat(k,1408) = 0._rkind_comp
         mat(k,1411) = 0._rkind_comp
         mat(k,1414) = 0._rkind_comp
         mat(k,1416) = 0._rkind_comp
         mat(k,1418) = 0._rkind_comp
         mat(k,1419) = 0._rkind_comp
         mat(k,1422) = 0._rkind_comp
         mat(k,1431) = 0._rkind_comp
         mat(k,1432) = 0._rkind_comp
         mat(k,1434) = 0._rkind_comp
         mat(k,1444) = 0._rkind_comp
         mat(k,1455) = 0._rkind_comp
         mat(k,1456) = 0._rkind_comp
         mat(k,1461) = 0._rkind_comp
         mat(k,1468) = 0._rkind_comp
         mat(k, 1) = mat(k, 1) - dti
         mat(k, 2) = mat(k, 2) - dti
         mat(k, 3) = mat(k, 3) - dti
         mat(k, 4) = mat(k, 4) - dti
         mat(k, 5) = mat(k, 5) - dti
         mat(k, 6) = mat(k, 6) - dti
         mat(k, 7) = mat(k, 7) - dti
         mat(k, 8) = mat(k, 8) - dti
         mat(k, 9) = mat(k, 9) - dti
         mat(k, 10) = mat(k, 10) - dti
         mat(k, 11) = mat(k, 11) - dti
         mat(k, 12) = mat(k, 12) - dti
         mat(k, 13) = mat(k, 13) - dti
         mat(k, 14) = mat(k, 14) - dti
         mat(k, 15) = mat(k, 15) - dti
         mat(k, 16) = mat(k, 16) - dti
         mat(k, 18) = mat(k, 18) - dti
         mat(k, 19) = mat(k, 19) - dti
         mat(k, 21) = mat(k, 21) - dti
         mat(k, 22) = mat(k, 22) - dti
         mat(k, 23) = mat(k, 23) - dti
         mat(k, 24) = mat(k, 24) - dti
         mat(k, 25) = mat(k, 25) - dti
         mat(k, 26) = mat(k, 26) - dti
         mat(k, 27) = mat(k, 27) - dti
         mat(k, 28) = mat(k, 28) - dti
         mat(k, 29) = mat(k, 29) - dti
         mat(k, 30) = mat(k, 30) - dti
         mat(k, 33) = mat(k, 33) - dti
         mat(k, 36) = mat(k, 36) - dti
         mat(k, 39) = mat(k, 39) - dti
         mat(k, 42) = mat(k, 42) - dti
         mat(k, 45) = mat(k, 45) - dti
         mat(k, 48) = mat(k, 48) - dti
         mat(k, 51) = mat(k, 51) - dti
         mat(k, 56) = mat(k, 56) - dti
         mat(k, 58) = mat(k, 58) - dti
         mat(k, 61) = mat(k, 61) - dti
         mat(k, 66) = mat(k, 66) - dti
         mat(k, 69) = mat(k, 69) - dti
         mat(k, 72) = mat(k, 72) - dti
         mat(k, 75) = mat(k, 75) - dti
         mat(k, 78) = mat(k, 78) - dti
         mat(k, 80) = mat(k, 80) - dti
         mat(k, 82) = mat(k, 82) - dti
         mat(k, 86) = mat(k, 86) - dti
         mat(k, 90) = mat(k, 90) - dti
         mat(k, 94) = mat(k, 94) - dti
         mat(k, 98) = mat(k, 98) - dti
         mat(k, 103) = mat(k, 103) - dti
         mat(k, 106) = mat(k, 106) - dti
         mat(k, 110) = mat(k, 110) - dti
         mat(k, 114) = mat(k, 114) - dti
         mat(k, 120) = mat(k, 120) - dti
         mat(k, 126) = mat(k, 126) - dti
         mat(k, 132) = mat(k, 132) - dti
         mat(k, 139) = mat(k, 139) - dti
         mat(k, 144) = mat(k, 144) - dti
         mat(k, 149) = mat(k, 149) - dti
         mat(k, 154) = mat(k, 154) - dti
         mat(k, 157) = mat(k, 157) - dti
         mat(k, 161) = mat(k, 161) - dti
         mat(k, 168) = mat(k, 168) - dti
         mat(k, 174) = mat(k, 174) - dti
         mat(k, 180) = mat(k, 180) - dti
         mat(k, 186) = mat(k, 186) - dti
         mat(k, 192) = mat(k, 192) - dti
         mat(k, 198) = mat(k, 198) - dti
         mat(k, 203) = mat(k, 203) - dti
         mat(k, 208) = mat(k, 208) - dti
         mat(k, 216) = mat(k, 216) - dti
         mat(k, 223) = mat(k, 223) - dti
         mat(k, 230) = mat(k, 230) - dti
         mat(k, 237) = mat(k, 237) - dti
         mat(k, 244) = mat(k, 244) - dti
         mat(k, 250) = mat(k, 250) - dti
         mat(k, 255) = mat(k, 255) - dti
         mat(k, 263) = mat(k, 263) - dti
         mat(k, 267) = mat(k, 267) - dti
         mat(k, 271) = mat(k, 271) - dti
         mat(k, 279) = mat(k, 279) - dti
         mat(k, 287) = mat(k, 287) - dti
         mat(k, 295) = mat(k, 295) - dti
         mat(k, 304) = mat(k, 304) - dti
         mat(k, 312) = mat(k, 312) - dti
         mat(k, 318) = mat(k, 318) - dti
         mat(k, 327) = mat(k, 327) - dti
         mat(k, 331) = mat(k, 331) - dti
         mat(k, 338) = mat(k, 338) - dti
         mat(k, 346) = mat(k, 346) - dti
         mat(k, 357) = mat(k, 357) - dti
         mat(k, 366) = mat(k, 366) - dti
         mat(k, 380) = mat(k, 380) - dti
         mat(k, 391) = mat(k, 391) - dti
         mat(k, 397) = mat(k, 397) - dti
         mat(k, 402) = mat(k, 402) - dti
         mat(k, 412) = mat(k, 412) - dti
         mat(k, 420) = mat(k, 420) - dti
         mat(k, 429) = mat(k, 429) - dti
         mat(k, 440) = mat(k, 440) - dti
         mat(k, 448) = mat(k, 448) - dti
         mat(k, 456) = mat(k, 456) - dti
         mat(k, 467) = mat(k, 467) - dti
         mat(k, 477) = mat(k, 477) - dti
         mat(k, 489) = mat(k, 489) - dti
         mat(k, 501) = mat(k, 501) - dti
         mat(k, 511) = mat(k, 511) - dti
         mat(k, 520) = mat(k, 520) - dti
         mat(k, 534) = mat(k, 534) - dti
         mat(k, 552) = mat(k, 552) - dti
         mat(k, 558) = mat(k, 558) - dti
         mat(k, 568) = mat(k, 568) - dti
         mat(k, 581) = mat(k, 581) - dti
         mat(k, 594) = mat(k, 594) - dti
         mat(k, 609) = mat(k, 609) - dti
         mat(k, 614) = mat(k, 614) - dti
         mat(k, 625) = mat(k, 625) - dti
         mat(k, 637) = mat(k, 637) - dti
         mat(k, 644) = mat(k, 644) - dti
         mat(k, 657) = mat(k, 657) - dti
         mat(k, 669) = mat(k, 669) - dti
         mat(k, 684) = mat(k, 684) - dti
         mat(k, 707) = mat(k, 707) - dti
         mat(k, 731) = mat(k, 731) - dti
         mat(k, 750) = mat(k, 750) - dti
         mat(k, 770) = mat(k, 770) - dti
         mat(k, 799) = mat(k, 799) - dti
         mat(k, 816) = mat(k, 816) - dti
         mat(k, 843) = mat(k, 843) - dti
         mat(k, 856) = mat(k, 856) - dti
         mat(k, 870) = mat(k, 870) - dti
         mat(k, 890) = mat(k, 890) - dti
         mat(k, 912) = mat(k, 912) - dti
         mat(k, 971) = mat(k, 971) - dti
         mat(k,1013) = mat(k,1013) - dti
         mat(k,1054) = mat(k,1054) - dti
         mat(k,1079) = mat(k,1079) - dti
         mat(k,1179) = mat(k,1179) - dti
         mat(k,1223) = mat(k,1223) - dti
         mat(k,1245) = mat(k,1245) - dti
         mat(k,1263) = mat(k,1263) - dti
         mat(k,1337) = mat(k,1337) - dti
         mat(k,1362) = mat(k,1362) - dti
         mat(k,1398) = mat(k,1398) - dti
         mat(k,1433) = mat(k,1433) - dti
         mat(k,1471) = mat(k,1471) - dti
      end do
      end subroutine nlnmat_finit
      subroutine nlnmat(avec_len, mat, y, rxt, lmat, dti )
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      real(rkind_comp), intent(in) :: dti
      integer, intent(in) ::  avec_len ! total spatial points in chunk;
      real(rkind_comp), intent(in) :: y(veclen,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(veclen,rxntot)
      real(rkind_comp), intent(inout) :: mat(veclen,nzcnt)
      real(rkind_comp), intent(in) :: lmat(veclen,nzcnt) 
      call nlnmat01( avec_len, mat, y, rxt )
      call nlnmat02( avec_len, mat, y, rxt )
      call nlnmat03( avec_len, mat, y, rxt )
      call nlnmat04( avec_len, mat, y, rxt )
      call nlnmat05( avec_len, mat, y, rxt )
      call nlnmat06( avec_len, mat, y, rxt )
      call nlnmat07( avec_len, mat, y, rxt )
      call nlnmat_finit( avec_len, mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
