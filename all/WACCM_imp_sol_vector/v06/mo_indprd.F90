!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2






      module mo_indprd

          USE shr_kind_mod, ONLY: rkind_comp
          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          USE chem_mods, ONLY: clscnt4, gas_pcnst, rxntot, extcnt
          PRIVATE
          PUBLIC indprd

      contains

      subroutine indprd( class, prod, y, extfrc, rxt, chnkpnts )


      implicit none

!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: class
      integer, intent(in) :: chnkpnts
      real(rkind_comp), intent(in) :: y(chnkpnts,gas_pcnst)
      real(rkind_comp), intent(in) :: rxt(chnkpnts,rxntot)
      real(rkind_comp), intent(in) :: extfrc(chnkpnts,extcnt)
      real(rkind_comp), intent(inout) :: prod(chnkpnts,clscnt4)

!--------------------------------------------------------------------
! ... "independent" production for Explicit species
!--------------------------------------------------------------------
      if( class == 1 ) then
         prod(:,1) = 0._rkind_comp

         prod(:,2) =.080_rkind_comp*rxt(:,311)*y(:,59)*y(:,1)

         prod(:,3) =rxt(:,184)*y(:,10)*y(:,8)

         prod(:,4) = 0._rkind_comp

         prod(:,5) = 0._rkind_comp

         prod(:,6) = 0._rkind_comp

         prod(:,7) = 0._rkind_comp

         prod(:,8) = 0._rkind_comp

         prod(:,9) = 0._rkind_comp

         prod(:,10) = 0._rkind_comp

         prod(:,11) = 0._rkind_comp

         prod(:,12) = 0._rkind_comp

         prod(:,13) = 0._rkind_comp

         prod(:,14) = 0._rkind_comp

         prod(:,15) = 0._rkind_comp

         prod(:,16) = 0._rkind_comp

         prod(:,17) = 0._rkind_comp

         prod(:,18) = 0._rkind_comp

         prod(:,19) = 0._rkind_comp

         prod(:,20) = 0._rkind_comp

         prod(:,21) = 0._rkind_comp

         prod(:,22) = (rxt(:,264)*y(:,20) +rxt(:,265)*y(:,20) +rxt(:,276)*y(:,136) + &
                 rxt(:,291)*y(:,49) +.500_rkind_comp*rxt(:,304)*y(:,54) + &
                 .800_rkind_comp*rxt(:,305)*y(:,52) +rxt(:,306)*y(:,53) + &
                 .500_rkind_comp*rxt(:,355)*y(:,81))*y(:,23) + (rxt(:,299)*y(:,9) + &
                 .900_rkind_comp*rxt(:,302)*y(:,16) +2.000_rkind_comp*rxt(:,303)*y(:,48) + &
                 2.000_rkind_comp*rxt(:,351)*y(:,76) +rxt(:,379)*y(:,91))*y(:,48) &
                  + (rxt(:,350)*y(:,16) +2.000_rkind_comp*rxt(:,352)*y(:,76))*y(:,76) &
                  +rxt(:,63)*y(:,54) +.400_rkind_comp*rxt(:,64)*y(:,58)

         prod(:,23) = 0._rkind_comp

         prod(:,24) = 0._rkind_comp

         prod(:,25) = 0._rkind_comp

         prod(:,26) = 0._rkind_comp

         prod(:,27) = 0._rkind_comp

         prod(:,28) = 0._rkind_comp

         prod(:,29) = 0._rkind_comp

         prod(:,30) = 0._rkind_comp

         prod(:,31) = + extfrc(:,15)

         prod(:,32) = 0._rkind_comp

         prod(:,33) = 0._rkind_comp

         prod(:,34) = 0._rkind_comp

         prod(:,35) = 0._rkind_comp

         prod(:,36) = 0._rkind_comp

         prod(:,37) = 0._rkind_comp

!--------------------------------------------------------------------
! ... "independent" production for Implicit species
!--------------------------------------------------------------------
      else if( class == 4 ) then
         prod(:,139) = 0._rkind_comp

         prod(:,146) = (rxt(:,58) +rxt(:,114))*y(:,126) +.180_rkind_comp*rxt(:,60)*y(:,15)

         prod(:,143) =rxt(:,5)*y(:,7)

         prod(:,129) = 0._rkind_comp

         prod(:,43) = 0._rkind_comp

         prod(:,42) = 0._rkind_comp

         prod(:,120) =1.440_rkind_comp*rxt(:,60)*y(:,15)

         prod(:,115) = (rxt(:,58) +rxt(:,114))*y(:,126) +.380_rkind_comp*rxt(:,60)*y(:,15) &
                  + extfrc(:,3)

         prod(:,105) = (rxt(:,98) +.800_rkind_comp*rxt(:,101) +rxt(:,110) +.800_rkind_comp*rxt(:,113)) &
                  + extfrc(:,12)

         prod(:,134) = + extfrc(:,1)

         prod(:,144) = + extfrc(:,2)

         prod(:,138) =.330_rkind_comp*rxt(:,60)*y(:,15) + extfrc(:,14)

         prod(:,135) = 0._rkind_comp

         prod(:,131) = 0._rkind_comp

         prod(:,73) = 0._rkind_comp

         prod(:,55) = 0._rkind_comp

         prod(:,136) =rxt(:,59)*y(:,15) +rxt(:,37)*y(:,108) +rxt(:,48)*y(:,109)

         prod(:,67) = 0._rkind_comp

         prod(:,47) = 0._rkind_comp

         prod(:,34) = 0._rkind_comp

         prod(:,140) =.180_rkind_comp*rxt(:,60)*y(:,15)

         prod(:,141) = (rxt(:,59) +.330_rkind_comp*rxt(:,60))*y(:,15)

         prod(:,142) = 0._rkind_comp

         prod(:,89) = 0._rkind_comp

         prod(:,130) =.050_rkind_comp*rxt(:,60)*y(:,15)

         prod(:,145) =rxt(:,37)*y(:,108) +2.000_rkind_comp*rxt(:,40)*y(:,110) &
                  +2.000_rkind_comp*rxt(:,41)*y(:,111) +2.000_rkind_comp*rxt(:,42)*y(:,112) +rxt(:,45) &
                 *y(:,113) +4.000_rkind_comp*rxt(:,38)*y(:,114) +3.000_rkind_comp*rxt(:,39)*y(:,115) &
                  +rxt(:,50)*y(:,117) +rxt(:,46)*y(:,118) +rxt(:,47)*y(:,119) &
                  +2.000_rkind_comp*rxt(:,43)*y(:,120) +rxt(:,44)*y(:,121)

         prod(:,44) = 0._rkind_comp

         prod(:,132) = 0._rkind_comp

         prod(:,37) = 0._rkind_comp

         prod(:,28) = 0._rkind_comp

         prod(:,137) = 0._rkind_comp

         prod(:,107) = 0._rkind_comp

         prod(:,113) = 0._rkind_comp

         prod(:,50) = 0._rkind_comp

         prod(:,133) =rxt(:,48)*y(:,109) +rxt(:,49)*y(:,116) +rxt(:,50)*y(:,117) &
                  +2.000_rkind_comp*rxt(:,53)*y(:,122) +2.000_rkind_comp*rxt(:,54)*y(:,123) &
                  +3.000_rkind_comp*rxt(:,51)*y(:,124) +2.000_rkind_comp*rxt(:,52)*y(:,125)

         prod(:,128) = 0._rkind_comp

         prod(:,104) = 0._rkind_comp

         prod(:,96) = 0._rkind_comp

         prod(:,80) = 0._rkind_comp

         prod(:,92) = (rxt(:,94) +rxt(:,106)) + extfrc(:,10)

         prod(:,98) = + extfrc(:,8)

         prod(:,72) = (rxt(:,98) +rxt(:,99) +rxt(:,110) +rxt(:,111)) + extfrc(:,9)

         prod(:,88) = + extfrc(:,7)

         prod(:,99) = 0._rkind_comp

         prod(:,76) = (rxt(:,99) +1.200_rkind_comp*rxt(:,101) +rxt(:,111) + &
                 1.200_rkind_comp*rxt(:,113)) + extfrc(:,11)

         prod(:,100) = (rxt(:,94) +rxt(:,98) +rxt(:,99) +rxt(:,106) +rxt(:,110) + &
                 rxt(:,111)) + extfrc(:,13)

         prod(:,114) = 0._rkind_comp

         prod(:,109) = 0._rkind_comp

         prod(:,103) = 0._rkind_comp

         prod(:,116) = 0._rkind_comp

         prod(:,87) = 0._rkind_comp

         prod(:,82) = 0._rkind_comp

         prod(:,127) = 0._rkind_comp

         prod(:,75) = 0._rkind_comp

         prod(:,74) = 0._rkind_comp

         prod(:,61) = 0._rkind_comp

         prod(:,53) = 0._rkind_comp

         prod(:,77) = 0._rkind_comp

         prod(:,29) = 0._rkind_comp

         prod(:,81) = 0._rkind_comp

         prod(:,30) = 0._rkind_comp

         prod(:,56) = 0._rkind_comp

         prod(:,93) = 0._rkind_comp

         prod(:,90) = 0._rkind_comp

         prod(:,68) = 0._rkind_comp

         prod(:,91) = 0._rkind_comp

         prod(:,57) = 0._rkind_comp

         prod(:,38) = 0._rkind_comp

         prod(:,39) = 0._rkind_comp

         prod(:,84) = 0._rkind_comp

         prod(:,62) = 0._rkind_comp

         prod(:,45) = 0._rkind_comp

         prod(:,112) = 0._rkind_comp

         prod(:,70) = 0._rkind_comp

         prod(:,85) = 0._rkind_comp

         prod(:,95) = 0._rkind_comp

         prod(:,31) = 0._rkind_comp

         prod(:,63) = 0._rkind_comp

         prod(:,1) = 0._rkind_comp

         prod(:,32) = 0._rkind_comp

         prod(:,71) = 0._rkind_comp

         prod(:,2) = 0._rkind_comp

         prod(:,123) = 0._rkind_comp

         prod(:,125) = 0._rkind_comp

         prod(:,119) = 0._rkind_comp

         prod(:,124) = 0._rkind_comp

         prod(:,58) = 0._rkind_comp

         prod(:,126) = 0._rkind_comp

         prod(:,106) = 0._rkind_comp

         prod(:,59) = 0._rkind_comp

         prod(:,86) = 0._rkind_comp

         prod(:,35) = 0._rkind_comp

         prod(:,108) = 0._rkind_comp

         prod(:,64) = 0._rkind_comp

         prod(:,94) = 0._rkind_comp

         prod(:,65) = 0._rkind_comp

         prod(:,79) = 0._rkind_comp

         prod(:,51) = 0._rkind_comp

         prod(:,110) = 0._rkind_comp

         prod(:,118) = 0._rkind_comp

         prod(:,97) = 0._rkind_comp

         prod(:,69) = 0._rkind_comp

         prod(:,40) = 0._rkind_comp

         prod(:,60) = 0._rkind_comp

         prod(:,117) = 0._rkind_comp

         prod(:,121) = 0._rkind_comp

         prod(:,102) = 0._rkind_comp

         prod(:,111) = 0._rkind_comp

         prod(:,122) = 0._rkind_comp

         prod(:,52) = 0._rkind_comp

         prod(:,83) = 0._rkind_comp

         prod(:,54) = 0._rkind_comp

         prod(:,78) = 0._rkind_comp

         prod(:,66) = 0._rkind_comp

         prod(:,41) =rxt(:,41)*y(:,111) +rxt(:,42)*y(:,112) +rxt(:,45)*y(:,113) &
                  +rxt(:,49)*y(:,116) +rxt(:,50)*y(:,117) +rxt(:,47)*y(:,119) &
                  +2.000_rkind_comp*rxt(:,43)*y(:,120) +2.000_rkind_comp*rxt(:,44)*y(:,121) +rxt(:,53) &
                 *y(:,122) +2.000_rkind_comp*rxt(:,54)*y(:,123)

         prod(:,46) =rxt(:,40)*y(:,110) +rxt(:,42)*y(:,112) +rxt(:,46)*y(:,118)

         prod(:,48) = 0._rkind_comp

         prod(:,101) =rxt(:,49)*y(:,116) +rxt(:,44)*y(:,121)

         prod(:,36) = + extfrc(:,4)

         prod(:,49) = 0._rkind_comp

         prod(:,3) = + extfrc(:,5)

         prod(:,33) = 0._rkind_comp

         prod(:,4) = 0._rkind_comp

         prod(:,5) = 0._rkind_comp

         prod(:,6) = 0._rkind_comp

         prod(:,7) = 0._rkind_comp

         prod(:,8) = 0._rkind_comp

         prod(:,9) = 0._rkind_comp

         prod(:,10) = 0._rkind_comp

         prod(:,11) = 0._rkind_comp

         prod(:,12) = 0._rkind_comp

         prod(:,13) = 0._rkind_comp

         prod(:,14) = 0._rkind_comp

         prod(:,15) = 0._rkind_comp

         prod(:,16) = + extfrc(:,6)

         prod(:,17) = 0._rkind_comp

         prod(:,18) = 0._rkind_comp

         prod(:,19) = 0._rkind_comp

         prod(:,20) = 0._rkind_comp

         prod(:,21) = 0._rkind_comp

         prod(:,22) = 0._rkind_comp

         prod(:,23) = 0._rkind_comp

         prod(:,24) = 0._rkind_comp

         prod(:,25) = 0._rkind_comp

         prod(:,26) = 0._rkind_comp

         prod(:,27) = 0._rkind_comp

      end if

      end subroutine indprd

      end module mo_indprd
