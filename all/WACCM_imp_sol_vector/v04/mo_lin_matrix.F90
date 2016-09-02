!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2






      module mo_lin_matrix

          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE chem_mods, ONLY: nzcnt, gas_pcnst, rxntot
          USE ppgrid, ONLY: veclen
          PRIVATE
          PUBLIC linmat

      contains

      subroutine linmat01( avec_len, mat, y, rxt, het_rates)
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------


          USE shr_kind_mod, ONLY: r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(in) :: het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k


      do k = 1,avec_len

         mat(k,1223) = -( rxt(k,3) + rxt(k,4) + het_rates(k,1) )

         mat(k,1471) = -( rxt(k,89) + rxt(k,90) + rxt(k,91) + rxt(k,102) + rxt(k,103) &
                      + rxt(k,104) + het_rates(k,2) )
         mat(k,852) = rxt(k,1) + 2.000_r8*rxt(k,2) + rxt(k,95) + rxt(k,96) + rxt(k,97) &
                      + 2.000_r8*rxt(k,100) + rxt(k,107) + rxt(k,108) + rxt(k,109) &
                      + 2.000_r8*rxt(k,112)
         mat(k,1230) = rxt(k,4)
         mat(k,983) = rxt(k,6)
         mat(k,1400) = rxt(k,8)
         mat(k,131) = rxt(k,10)
         mat(k,1024) = rxt(k,12)
         mat(k,865) = rxt(k,21)
         mat(k,904) = rxt(k,24)
         mat(k,60) = rxt(k,25)
         mat(k,829) = rxt(k,32)
         mat(k,1365) = rxt(k,128)

         mat(k,1362) = -( rxt(k,128) + rxt(k,132)*y(k,7) + rxt(k,133)*y(k,7) &
                      + rxt(k,135)*y(k,110) + rxt(k,136)*y(k,111) + rxt(k,137)*y(k,112) &
                      + rxt(k,138)*y(k,120) + rxt(k,139)*y(k,121) + rxt(k,140)*y(k,113) &
                      + rxt(k,141)*y(k,118) + rxt(k,142)*y(k,119) + rxt(k,143)*y(k,114) &
                      + rxt(k,144)*y(k,109) + rxt(k,145)*y(k,117) + rxt(k,146)*y(k,116) &
                      + rxt(k,147)*y(k,122) + rxt(k,148)*y(k,123) + rxt(k,149)*y(k,124) &
                      + rxt(k,150)*y(k,125) + rxt(k,153)*y(k,15) + rxt(k,154)*y(k,15) &
                      + rxt(k,155)*y(k,15) + het_rates(k,3) )
         mat(k,850) = rxt(k,1)
         mat(k,1227) = rxt(k,3)
         mat(k,863) = rxt(k,20)

         mat(k,843) = -( rxt(k,1) + rxt(k,2) + rxt(k,93) + rxt(k,95) + rxt(k,96) &
                      + rxt(k,97) + rxt(k,100) + rxt(k,105) + rxt(k,107) + rxt(k,108) &
                      + rxt(k,109) + rxt(k,112) + het_rates(k,4) )
         mat(k,1213) = rxt(k,4)
         mat(k,1009) = rxt(k,13)
         mat(k,79) = rxt(k,123)
         mat(k,76) = rxt(k,126) + rxt(k,127)
         mat(k,1349) = rxt(k,133)*y(k,7)

         mat(k,78) = -( rxt(k,120) + rxt(k,123) + rxt(k,122)*y(k,126) + het_rates(k,5) )

         mat(k,75) = -( rxt(k,126) + rxt(k,127) + het_rates(k,6) )
         mat(k,1188) = rxt(k,3)
         mat(k,77) = rxt(k,120) + rxt(k,122)*y(k,126)

         mat(k,657) = -( het_rates(k,21) )
         mat(k,1235) = rxt(k,18)
         mat(k,855) = rxt(k,20)
         mat(k,1348) = rxt(k,155)*y(k,15)

         mat(k,609) = -( het_rates(k,20) )
         mat(k,1234) = rxt(k,17) + rxt(k,18)
         mat(k,613) = rxt(k,61)
         mat(k,643) = 1.340_r8*rxt(k,67)
         mat(k,745) = .700_r8*rxt(k,68)
         mat(k,668) = rxt(k,74)
         mat(k,559) = rxt(k,76)
         mat(k,553) = rxt(k,79)
         mat(k,314) = .450_r8*rxt(k,81)
         mat(k,398) = 2.000_r8*rxt(k,82)
         mat(k,1413) = rxt(k,251)*y(k,108)
         mat(k,335) = rxt(k,453)*y(k,126)

         mat(k,489) = -( rxt(k,92) + het_rates(k,8) )
         mat(k,947) = rxt(k,6)
         mat(k,334) = rxt(k,450)

         mat(k,971) = -( rxt(k,6) + rxt(k,7) + het_rates(k,9) )
         mat(k,1388) = rxt(k,8) + .500_r8*rxt(k,402)
         mat(k,128) = rxt(k,10)
         mat(k,1012) = rxt(k,13)
         mat(k,425) = rxt(k,460)
         mat(k,1353) = 2.000_r8*rxt(k,132)*y(k,7)

         mat(k,1398) = -( rxt(k,8) + rxt(k,402) + het_rates(k,10) )
         mat(k,130) = rxt(k,9) + rxt(k,194)
         mat(k,877) = rxt(k,11)
         mat(k,1022) = rxt(k,12)
         mat(k,236) = rxt(k,15) + rxt(k,203)
         mat(k,588) = rxt(k,30)
         mat(k,277) = rxt(k,36)
         mat(k,243) = .600_r8*rxt(k,64) + rxt(k,308)
         mat(k,286) = rxt(k,65) + rxt(k,354)
         mat(k,565) = rxt(k,76)

         mat(k,1179) = -( rxt(k,252)*y(k,108) + rxt(k,253)*y(k,115) + rxt(k,254)*y(k,113) &
                      + rxt(k,255)*y(k,109) + rxt(k,257)*y(k,118) + rxt(k,258)*y(k,119) &
                      + rxt(k,259)*y(k,125) + rxt(k,260)*y(k,124) + rxt(k,263)*y(k,15) &
                 + het_rates(k,23) )
         mat(k,874) = rxt(k,11)
         mat(k,234) = rxt(k,14)
         mat(k,195) = rxt(k,16)
         mat(k,861) = rxt(k,19)
         mat(k,341) = 2.000_r8*rxt(k,22)
         mat(k,515) = rxt(k,27)
         mat(k,407) = rxt(k,33)
         mat(k,292) = rxt(k,62)
         mat(k,248) = rxt(k,63)
         mat(k,152) = rxt(k,69)
         mat(k,71) = rxt(k,70)
         mat(k,178) = rxt(k,71)
         mat(k,184) = rxt(k,72)
         mat(k,113) = rxt(k,75)
         mat(k,353) = rxt(k,83)
         mat(k,143) = rxt(k,84)
         mat(k,166) = rxt(k,85)
         mat(k,213) = rxt(k,86)
         mat(k,1392) = .500_r8*rxt(k,402)
         mat(k,1357) = rxt(k,153)*y(k,15)

         mat(k,1013) = -( rxt(k,12) + rxt(k,13) + rxt(k,401) + het_rates(k,11) )
         mat(k,129) = rxt(k,9) + rxt(k,10) + rxt(k,194)
         mat(k,233) = rxt(k,14)
         mat(k,585) = rxt(k,29)
         mat(k,276) = rxt(k,35)
         mat(k,239) = .400_r8*rxt(k,64)

         mat(k,870) = -( rxt(k,11) + het_rates(k,12) )
         mat(k,127) = 2.000_r8*rxt(k,400) + 2.000_r8*rxt(k,432) + 2.000_r8*rxt(k,438) &
                      + 2.000_r8*rxt(k,443)
         mat(k,1011) = rxt(k,401)
         mat(k,1385) = .500_r8*rxt(k,402)
         mat(k,583) = rxt(k,433) + rxt(k,439) + rxt(k,444)
         mat(k,274) = rxt(k,434) + rxt(k,442) + rxt(k,445)

         mat(k,230) = -( rxt(k,14) + rxt(k,15) + rxt(k,203) + het_rates(k,13) )

         mat(k,126) = -( rxt(k,9) + rxt(k,10) + rxt(k,194) + rxt(k,400) + rxt(k,432) &
                      + rxt(k,438) + rxt(k,443) + het_rates(k,14) )

         mat(k,1054) = -( het_rates(k,16) )
         mat(k,619) = rxt(k,61)
         mat(k,247) = rxt(k,63)
         mat(k,240) = .400_r8*rxt(k,64)
         mat(k,758) = .300_r8*rxt(k,68)
         mat(k,395) = rxt(k,73)
         mat(k,1355) = rxt(k,153)*y(k,15)
         mat(k,1424) = rxt(k,210)*y(k,15)
         mat(k,453) = rxt(k,249)*y(k,15)
         mat(k,1177) = rxt(k,263)*y(k,15)

         mat(k,192) = -( rxt(k,16) + het_rates(k,17) )

         mat(k,90) = -( het_rates(k,42) )

         mat(k,48) = -( het_rates(k,43) )

         mat(k,1245) = -( rxt(k,17) + rxt(k,18) + het_rates(k,19) )
         mat(k,196) = rxt(k,16)
         mat(k,293) = rxt(k,62)
         mat(k,652) = 1.340_r8*rxt(k,66)
         mat(k,185) = rxt(k,72)
         mat(k,563) = rxt(k,76)
         mat(k,301) = .690_r8*rxt(k,77)
         mat(k,641) = rxt(k,78)
         mat(k,555) = rxt(k,79)
         mat(k,354) = .100_r8*rxt(k,83)
         mat(k,189) = rxt(k,277)
         mat(k,206) = 2.000_r8*rxt(k,289)
         mat(k,1359) = rxt(k,154)*y(k,15) + rxt(k,155)*y(k,15)

         mat(k,1263) = -( het_rates(k,22) )
         mat(k,197) = rxt(k,16)
         mat(k,1246) = 2.000_r8*rxt(k,17)
         mat(k,862) = rxt(k,19) + 2.000_r8*rxt(k,21)
         mat(k,1083) = rxt(k,28)
         mat(k,481) = rxt(k,34)
         mat(k,96) = rxt(k,57)
         mat(k,1360) = rxt(k,154)*y(k,15)

         mat(k,1337) = -( rxt(k,410) + het_rates(k,24) )
         mat(k,235) = rxt(k,15) + rxt(k,203)
         mat(k,622) = rxt(k,61)
         mat(k,294) = rxt(k,62)
         mat(k,654) = 1.340_r8*rxt(k,66) + .660_r8*rxt(k,67)
         mat(k,153) = rxt(k,69)
         mat(k,179) = rxt(k,71)
         mat(k,676) = rxt(k,74)
         mat(k,564) = rxt(k,76)
         mat(k,302) = rxt(k,77)
         mat(k,642) = rxt(k,78)
         mat(k,556) = 2.000_r8*rxt(k,79)
         mat(k,317) = .560_r8*rxt(k,81)
         mat(k,400) = 2.000_r8*rxt(k,82)
         mat(k,355) = .900_r8*rxt(k,83)
         mat(k,214) = rxt(k,86)
         mat(k,190) = rxt(k,277)
         mat(k,207) = rxt(k,289)
         mat(k,1361) = rxt(k,154)*y(k,15)
         mat(k,1430) = rxt(k,251)*y(k,108) + rxt(k,256)*y(k,109)
         mat(k,1183) = rxt(k,252)*y(k,108) + rxt(k,255)*y(k,109)

         mat(k,338) = -( rxt(k,22) + het_rates(k,25) )
         mat(k,1293) = .500_r8*rxt(k,410)

         mat(k,856) = -( rxt(k,19) + rxt(k,20) + rxt(k,21) + het_rates(k,134) )
         mat(k,1171) = rxt(k,252)*y(k,108) + rxt(k,253)*y(k,115) + rxt(k,254)*y(k,113) &
                      + rxt(k,255)*y(k,109) + rxt(k,259)*y(k,125) + rxt(k,263)*y(k,15)

         mat(k,1433) = -( rxt(k,210)*y(k,15) + rxt(k,251)*y(k,108) + rxt(k,256)*y(k,109) &
                      + rxt(k,261)*y(k,125) + rxt(k,262)*y(k,124) + het_rates(k,28) )
         mat(k,81) = 2.000_r8*rxt(k,23)
         mat(k,903) = rxt(k,24)
         mat(k,32) = 2.000_r8*rxt(k,26)
         mat(k,516) = rxt(k,27)
         mat(k,1087) = rxt(k,28)
         mat(k,589) = rxt(k,29)
         mat(k,105) = rxt(k,31)
         mat(k,89) = rxt(k,56)
         mat(k,1364) = 2.000_r8*rxt(k,135)*y(k,110) + 2.000_r8*rxt(k,136)*y(k,111) &
                      + 2.000_r8*rxt(k,137)*y(k,112) + 2.000_r8*rxt(k,138)*y(k,120) &
                      + rxt(k,139)*y(k,121) + rxt(k,140)*y(k,113) + rxt(k,141)*y(k,118) &
                      + rxt(k,142)*y(k,119) + 4.000_r8*rxt(k,143)*y(k,114) &
                      + rxt(k,145)*y(k,117)
         mat(k,1186) = rxt(k,252)*y(k,108) + 3.000_r8*rxt(k,253)*y(k,115) &
                      + rxt(k,254)*y(k,113) + rxt(k,257)*y(k,118) + rxt(k,258)*y(k,119)

         mat(k,80) = -( rxt(k,23) + het_rates(k,29) )

         mat(k,890) = -( rxt(k,24) + het_rates(k,30) )
         mat(k,59) = rxt(k,25)
         mat(k,584) = rxt(k,30)
         mat(k,31) = 2.000_r8*rxt(k,222)

         mat(k,58) = -( rxt(k,25) + het_rates(k,31) )

         mat(k,30) = -( rxt(k,26) + rxt(k,222) + het_rates(k,32) )

         mat(k,1079) = -( rxt(k,28) + het_rates(k,33) )
         mat(k,1425) = rxt(k,210)*y(k,15) + 2.000_r8*rxt(k,251)*y(k,108) &
                      + rxt(k,256)*y(k,109) + rxt(k,261)*y(k,125) + rxt(k,262)*y(k,124)

      end do

      end subroutine linmat01

      subroutine linmat02( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------


          USE shr_kind_mod, ONLY: r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(in) :: het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k


      do k = 1,avec_len 

         mat(k,511) = -( rxt(k,27) + het_rates(k,34) )
         mat(k,580) = rxt(k,433) + rxt(k,439) + rxt(k,444)

         mat(k,581) = -( rxt(k,29) + rxt(k,30) + rxt(k,433) + rxt(k,439) + rxt(k,444) &
                 + het_rates(k,35) )

         mat(k,103) = -( rxt(k,31) + het_rates(k,36) )

         mat(k,912) = -( het_rates(k,37) )
         mat(k,104) = rxt(k,31)
         mat(k,821) = rxt(k,32)
         mat(k,405) = rxt(k,33)
         mat(k,479) = rxt(k,34)
         mat(k,275) = rxt(k,35)
         mat(k,1352) = rxt(k,144)*y(k,109) + rxt(k,145)*y(k,117) + rxt(k,146)*y(k,116) &
                      + 2.000_r8*rxt(k,147)*y(k,122) + 2.000_r8*rxt(k,148)*y(k,123) &
                      + 3.000_r8*rxt(k,149)*y(k,124) + 2.000_r8*rxt(k,150)*y(k,125)
         mat(k,1174) = rxt(k,255)*y(k,109) + 2.000_r8*rxt(k,259)*y(k,125) &
                      + 3.000_r8*rxt(k,260)*y(k,124)
         mat(k,1421) = rxt(k,256)*y(k,109) + 2.000_r8*rxt(k,261)*y(k,125) &
                      + 3.000_r8*rxt(k,262)*y(k,124)

         mat(k,816) = -( rxt(k,32) + het_rates(k,38) )
         mat(k,273) = rxt(k,36)

         mat(k,477) = -( rxt(k,34) + het_rates(k,39) )

         mat(k,402) = -( rxt(k,33) + het_rates(k,40) )
         mat(k,272) = rxt(k,434) + rxt(k,442) + rxt(k,445)

         mat(k,271) = -( rxt(k,35) + rxt(k,36) + rxt(k,434) + rxt(k,442) + rxt(k,445) &
                 + het_rates(k,41) )

         mat(k,366) = -( het_rates(k,127) )

         mat(k,420) = -( rxt(k,460) + het_rates(k,128) )
         mat(k,837) = rxt(k,93) + rxt(k,105)
         mat(k,332) = rxt(k,453)*y(k,126)

         mat(k,223) = -( het_rates(k,129) )
         mat(k,484) = rxt(k,92)

         mat(k,331) = -( rxt(k,450) + rxt(k,453)*y(k,126) + het_rates(k,130) )
         mat(k,1440) = rxt(k,89) + rxt(k,90) + rxt(k,91) + rxt(k,102) + rxt(k,103) &
                      + rxt(k,104)
         mat(k,835) = rxt(k,95) + rxt(k,96) + rxt(k,97) + rxt(k,107) + rxt(k,108) &
                      + rxt(k,109)

         mat(k,429) = -( het_rates(k,131) )
         mat(k,943) = rxt(k,7)
         mat(k,333) = rxt(k,450)
         mat(k,421) = rxt(k,460)

         mat(k,250) = -( het_rates(k,133) )

         mat(k,440) = -( het_rates(k,132) )
         mat(k,944) = rxt(k,7)
         mat(k,1446) = rxt(k,89) + rxt(k,90) + rxt(k,91) + rxt(k,102) + rxt(k,103) &
                      + rxt(k,104)
         mat(k,488) = rxt(k,92)
         mat(k,839) = rxt(k,93) + rxt(k,95) + rxt(k,96) + rxt(k,97) + rxt(k,105) &
                      + rxt(k,107) + rxt(k,108) + rxt(k,109)

         mat(k,594) = -( het_rates(k,59) )
         mat(k,744) = .700_r8*rxt(k,68)

         mat(k,534) = -( het_rates(k,83) )

         mat(k,467) = -( het_rates(k,64) )

         mat(k,614) = -( rxt(k,61) + het_rates(k,50) )
         mat(k,289) = rxt(k,62)
         mat(k,151) = rxt(k,69)
         mat(k,351) = .400_r8*rxt(k,83)
         mat(k,141) = rxt(k,84)

         mat(k,327) = -( het_rates(k,49) )

         mat(k,287) = -( rxt(k,62) + het_rates(k,65) )

         mat(k,799) = -( het_rates(k,48) )
         mat(k,238) = .600_r8*rxt(k,64) + rxt(k,308)
         mat(k,648) = 1.340_r8*rxt(k,66)
         mat(k,752) = .300_r8*rxt(k,68)
         mat(k,182) = rxt(k,72)
         mat(k,393) = rxt(k,73)
         mat(k,670) = rxt(k,74)
         mat(k,639) = rxt(k,78)
         mat(k,201) = rxt(k,80)
         mat(k,316) = .130_r8*rxt(k,81)
         mat(k,142) = rxt(k,84)

         mat(k,244) = -( rxt(k,63) + het_rates(k,54) )

         mat(k,237) = -( rxt(k,64) + rxt(k,308) + het_rates(k,58) )

         mat(k,157) = -( het_rates(k,82) )

         mat(k,114) = -( het_rates(k,45) )

         mat(k,255) = -( het_rates(k,44) )

         mat(k,33) = -( het_rates(k,71) )

         mat(k,279) = -( rxt(k,65) + rxt(k,354) + het_rates(k,81) )

         mat(k,36) = -( het_rates(k,70) )

         mat(k,132) = -( het_rates(k,73) )

         mat(k,380) = -( het_rates(k,84) )

         mat(k,346) = -( rxt(k,83) + het_rates(k,85) )

         mat(k,198) = -( rxt(k,80) + het_rates(k,72) )
         mat(k,345) = .800_r8*rxt(k,83)

         mat(k,357) = -( het_rates(k,74) )

         mat(k,139) = -( rxt(k,84) + het_rates(k,75) )

         mat(k,61) = -( het_rates(k,94) )

         mat(k,66) = -( het_rates(k,95) )

         mat(k,304) = -( het_rates(k,96) )

         mat(k,161) = -( rxt(k,85) + het_rates(k,97) )

         mat(k,82) = -( het_rates(k,98) )

         mat(k,568) = -( het_rates(k,106) )

         mat(k,208) = -( rxt(k,86) + het_rates(k,107) )

         mat(k,312) = -( rxt(k,81) + het_rates(k,86) )
         mat(k,163) = .900_r8*rxt(k,85)

         mat(k,397) = -( rxt(k,82) + het_rates(k,53) )
         mat(k,313) = .130_r8*rxt(k,81)
         mat(k,164) = .450_r8*rxt(k,85)

         mat(k,39) = -( het_rates(k,99) )

         mat(k,168) = -( het_rates(k,100) )

         mat(k,1) = -( het_rates(k,101) )

         mat(k,42) = -( het_rates(k,102) )

         mat(k,216) = -( het_rates(k,103) )

         mat(k,2) = -( het_rates(k,104) )

         mat(k,707) = -( het_rates(k,88) )

         mat(k,750) = -( rxt(k,68) + het_rates(k,77) )
         mat(k,299) = .402_r8*rxt(k,77)
         mat(k,212) = rxt(k,86)

         mat(k,644) = -( rxt(k,66) + rxt(k,67) + het_rates(k,78) )
         mat(k,296) = .288_r8*rxt(k,77)
         mat(k,211) = rxt(k,86)

         mat(k,731) = -( het_rates(k,79) )

         mat(k,144) = -( het_rates(k,80) )

         mat(k,770) = -( het_rates(k,76) )
         mat(k,281) = rxt(k,65) + rxt(k,354)
         mat(k,647) = .660_r8*rxt(k,66)

         mat(k,501) = -( het_rates(k,46) )
         mat(k,200) = rxt(k,80)

         mat(k,149) = -( rxt(k,69) + het_rates(k,47) )

         mat(k,318) = -( het_rates(k,105) )

         mat(k,51) = -( het_rates(k,60) )

         mat(k,520) = -( het_rates(k,61) )

         mat(k,174) = -( rxt(k,71) + het_rates(k,62) )

         mat(k,391) = -( rxt(k,73) + het_rates(k,63) )
         mat(k,175) = .820_r8*rxt(k,71)
         mat(k,349) = .250_r8*rxt(k,83)
         mat(k,209) = .100_r8*rxt(k,86)

         mat(k,180) = -( rxt(k,72) + het_rates(k,69) )

         mat(k,267) = -( het_rates(k,18) )

         mat(k,106) = -( het_rates(k,51) )

         mat(k,552) = -( rxt(k,79) + het_rates(k,52) )

         mat(k,637) = -( rxt(k,78) + het_rates(k,66) )

         mat(k,412) = -( het_rates(k,55) )

         mat(k,203) = -( rxt(k,289) + het_rates(k,56) )
         mat(k,70) = rxt(k,70)

         mat(k,69) = -( rxt(k,70) + het_rates(k,57) )

         mat(k,154) = -( het_rates(k,87) )

         mat(k,625) = -( het_rates(k,67) )

         mat(k,669) = -( rxt(k,74) + het_rates(k,68) )
         mat(k,315) = .180_r8*rxt(k,81)
         mat(k,165) = .450_r8*rxt(k,85)

         mat(k,456) = -( het_rates(k,89) )

         mat(k,558) = -( rxt(k,76) + het_rates(k,90) )

         mat(k,684) = -( het_rates(k,91) )

         mat(k,110) = -( rxt(k,75) + het_rates(k,92) )

         mat(k,295) = -( rxt(k,77) + het_rates(k,93) )

         mat(k,120) = -( het_rates(k,135) )

         mat(k,263) = -( het_rates(k,136) )

         mat(k,186) = -( rxt(k,277) + het_rates(k,137) )

         mat(k,72) = -( rxt(k,55) + het_rates(k,138) )
         mat(k,1342) = rxt(k,136)*y(k,111) + rxt(k,137)*y(k,112) &
                      + 2.000_r8*rxt(k,138)*y(k,120) + 2.000_r8*rxt(k,139)*y(k,121) &
                      + rxt(k,140)*y(k,113) + rxt(k,142)*y(k,119) + rxt(k,145)*y(k,117) &
                      + rxt(k,146)*y(k,116) + rxt(k,147)*y(k,122) &
                      + 2.000_r8*rxt(k,148)*y(k,123)
         mat(k,1100) = rxt(k,254)*y(k,113) + rxt(k,258)*y(k,119)

         mat(k,86) = -( rxt(k,56) + het_rates(k,139) )
         mat(k,1344) = rxt(k,135)*y(k,110) + rxt(k,137)*y(k,112) + rxt(k,141)*y(k,118)
         mat(k,1102) = rxt(k,257)*y(k,118)

         mat(k,94) = -( rxt(k,57) + het_rates(k,140) )
         mat(k,447) = rxt(k,249)*y(k,15)

         mat(k,448) = -( rxt(k,249)*y(k,15) + het_rates(k,141) )
         mat(k,73) = 2.000_r8*rxt(k,55)
         mat(k,87) = rxt(k,56)
         mat(k,95) = rxt(k,57)
         mat(k,1346) = rxt(k,139)*y(k,121) + rxt(k,146)*y(k,116)

         mat(k,56) = -( het_rates(k,156) )

         mat(k,98) = -( het_rates(k,157) )

         mat(k,3) = -( rxt(k,415) + het_rates(k,158) )

         mat(k,45) = -( het_rates(k,159) )

         mat(k,4) = -( rxt(k,421) + het_rates(k,160) )

         mat(k,5) = -( rxt(k,422) + het_rates(k,161) )

         mat(k,6) = -( rxt(k,416) + het_rates(k,146) )

         mat(k,7) = -( rxt(k,417) + het_rates(k,147) )

         mat(k,8) = -( rxt(k,419) + het_rates(k,148) )

         mat(k,9) = -( rxt(k,418) + het_rates(k,149) )

         mat(k,10) = -( rxt(k,420) + het_rates(k,150) )

         mat(k,11) = -( het_rates(k,151) )

         mat(k,12) = -( het_rates(k,152) )

         mat(k,13) = -( het_rates(k,153) )

         mat(k,14) = -( het_rates(k,154) )

         mat(k,15) = -( het_rates(k,155) )

         mat(k,16) = -( rxt(k,403) + rxt(k,411) + het_rates(k,142) )

         mat(k,18) = -( rxt(k,412) + het_rates(k,143) )
         mat(k,17) = rxt(k,403)

         mat(k,19) = -( rxt(k,409) + rxt(k,413) + het_rates(k,144) )

         mat(k,21) = -( rxt(k,414) + het_rates(k,145) )
         mat(k,20) = rxt(k,409)

         mat(k,22) = -( rxt(k,423) + het_rates(k,162) )

         mat(k,23) = -( rxt(k,424) + het_rates(k,163) )

         mat(k,24) = -( rxt(k,425) + het_rates(k,164) )

         mat(k,25) = -( rxt(k,426) + het_rates(k,165) )

         mat(k,26) = -( rxt(k,427) + het_rates(k,166) )

         mat(k,27) = -( rxt(k,428) + het_rates(k,167) )

         mat(k,28) = -( rxt(k,429) + het_rates(k,168) )

         mat(k,29) = -( rxt(k,430) + het_rates(k,169) )

      end do

      end subroutine linmat02

      subroutine linmat( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
! ... linear matrix entries for implicit species
!----------------------------------------------


          USE shr_kind_mod, ONLY: r8 => shr_kind_r8

      implicit none

!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(in) :: het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) :: mat(veclen,nzcnt)

      call linmat01( avec_len, mat, y, rxt, het_rates )
      call linmat02( avec_len, mat, y, rxt, het_rates )

      end subroutine linmat

      end module mo_lin_matrix
