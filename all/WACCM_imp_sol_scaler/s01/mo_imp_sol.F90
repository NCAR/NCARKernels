!KGEN-generated Fortran source file

!Generated at : 2016-03-01 08:44:53
!KGEN version : 0.6.2

module mo_imp_sol
    USE shr_kind_mod, ONLY: r8 => shr_kind_r8
    USE chem_mods, ONLY: clscnt4, gas_pcnst, clsmap
    USE cam_logfile, ONLY: iulog
    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
    IMPLICIT NONE
    PRIVATE
    PUBLIC imp_sol
    SAVE
  !-----------------------------------------------------------------------
  ! Newton-Raphson iteration limits
  !-----------------------------------------------------------------------
  integer, parameter :: itermax = 11
  integer, parameter :: cut_limit = 5
  real(r8) :: small
  real(r8) :: epsilon(clscnt4)
  logical :: factor(itermax)
  integer :: ox_ndx
  integer :: o1d_ndx = -1
  integer :: h2o_ndx = -1
  integer :: oh_ndx, ho2_ndx, ch3o2_ndx, po2_ndx, ch3co3_ndx
  integer :: c2h5o2_ndx, isopo2_ndx, macro2_ndx, mco3_ndx, c3h7o2_ndx
  integer :: ro2_ndx, xo2_ndx, no_ndx, no2_ndx, no3_ndx, n2o5_ndx
  integer :: c2h4_ndx, c3h6_ndx, isop_ndx, mvk_ndx, c10h16_ndx
  integer :: ox_p1_ndx, ox_p2_ndx, ox_p3_ndx, ox_p4_ndx, ox_p5_ndx
  integer :: ox_p6_ndx, ox_p7_ndx, ox_p8_ndx, ox_p9_ndx, ox_p10_ndx
  integer :: ox_p11_ndx
  integer :: ox_l1_ndx, ox_l2_ndx, ox_l3_ndx, ox_l4_ndx, ox_l5_ndx
  integer :: ox_l6_ndx, ox_l7_ndx, ox_l8_ndx, ox_l9_ndx, usr4_ndx
  integer :: usr16_ndx, usr17_ndx, c2o3_ndx,ole_ndx
  integer :: tolo2_ndx, terpo2_ndx, alko2_ndx, eneo2_ndx, eo2_ndx, meko2_ndx
  integer :: ox_p17_ndx,ox_p12_ndx,ox_p13_ndx,ox_p14_ndx,ox_p15_ndx,ox_p16_ndx
  logical :: full_ozone_chem = .false.
  logical :: middle_atm_chem = .false.
  logical :: reduced_ozone_chem = .false.
  ! for xnox ozone chemistry diagnostics
  integer :: o3a_ndx, xno2_ndx, no2xno3_ndx, xno2no3_ndx, xno3_ndx, o1da_ndx, xno_ndx
  integer :: usr4a_ndx, usr16a_ndx, usr16b_ndx, usr17b_ndx
  PUBLIC kr_externs_in_mo_imp_sol
  PUBLIC kr_externs_out_mo_imp_sol
contains
    !-----------------------------------------------------------------------
    ! ... Initialize the implict solver
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    ! ... Local variables
    !-----------------------------------------------------------------------
    ! small = 1.e6_r8 * tiny( small )


















    !do m = 1,max(1,clscnt4)



























































  subroutine imp_sol( base_sol, reaction_rates, het_rates, extfrc, delt, &
       xhnm, ncol, lchnk, ltrop, o3s_loss )
    !-----------------------------------------------------------------------
    ! ... imp_sol advances the volumetric mixing ratio
    ! forward one time step via the fully implicit euler scheme.
    ! this source is meant for small l1 cache machines such as
    ! the intel pentium and itanium cpus
    !-----------------------------------------------------------------------
      USE chem_mods, ONLY: rxntot, extcnt, nzcnt, permute, cls_rxt_cnt
      USE mo_tracname, ONLY: solsym
      USE ppgrid, ONLY: pver
      USE mo_lin_matrix, ONLY: linmat
      USE mo_nln_matrix, ONLY: nlnmat
      USE mo_lu_factor, ONLY: lu_fac
      USE mo_lu_solve, ONLY: lu_slv
      USE mo_prod_loss, ONLY: imp_prod_loss
      USE mo_indprd, ONLY: indprd
      USE time_manager, ONLY: get_nstep
    implicit none
    !-----------------------------------------------------------------------
    ! ... dummy args
    !-----------------------------------------------------------------------
    integer, intent(in) :: ncol ! columns in chunck
    integer, intent(in) :: lchnk ! chunk id
    real(r8), intent(in) :: delt ! time step (s)
    real(r8), intent(in) :: reaction_rates(ncol,pver,max(1,rxntot)), & ! rxt rates (1/cm^3/s)
         extfrc(ncol,pver,max(1,extcnt)), & ! external in-situ forcing (1/cm^3/s)
         het_rates(ncol,pver,max(1,gas_pcnst)) ! washout rates (1/s)
    real(r8), intent(inout) :: base_sol(ncol,pver,gas_pcnst) ! species mixing ratios (vmr)
    real(r8), intent(in) :: xhnm(ncol,pver)
    integer, intent(in) :: ltrop(ncol) ! chemistry troposphere boundary (index)
    real(r8), optional, intent(out) :: o3s_loss(ncol,pver)
    !-----------------------------------------------------------------------
    ! ... local variables
    !-----------------------------------------------------------------------
    integer :: nr_iter, &
         lev, &
         i, &
         j, &
         k, l, &
         m
    integer :: fail_cnt, cut_cnt, stp_con_cnt
    integer :: nstep
    real(r8) :: interval_done, dt, dti, wrk
    real(r8) :: max_delta(max(1,clscnt4))
    real(r8) :: sys_jac(max(1,nzcnt))
    real(r8) :: lin_jac(max(1,nzcnt))
    real(r8), dimension(max(1,clscnt4)) :: &
         solution, &
         forcing, &
         iter_invariant, &
         prod, &
         loss
    real(r8) :: lrxt(max(1,rxntot))
    real(r8) :: lsol(max(1,gas_pcnst))
    real(r8) :: lhet(max(1,gas_pcnst))
    real(r8), dimension(ncol,pver,max(1,clscnt4)) :: &
         ind_prd
    logical :: convergence
    logical :: frc_mask, iter_conv
    logical :: converged(max(1,clscnt4))
    real(r8), dimension(ncol,pver,max(1,clscnt4)) :: prod_out, loss_out
    real(r8), dimension(ncol,pver) :: prod_hydrogen_peroxides_out
    if (present(o3s_loss)) then
       o3s_loss(:,:) = 0._r8
    endif
    prod_out(:,:,:) = 0._r8
    loss_out(:,:,:) = 0._r8
    prod_hydrogen_peroxides_out(:,:) = 0._r8
    solution(:) = 0._r8
    !-----------------------------------------------------------------------
    ! ... class independent forcing
    !-----------------------------------------------------------------------
    if( cls_rxt_cnt(1,4) > 0 .or. extcnt > 0 ) then
       call indprd( 4, ind_prd, clscnt4, base_sol, extfrc, &
            reaction_rates, ncol )
    else
       do m = 1,max(1,clscnt4)
          ind_prd(:,:,m) = 0._r8
       end do
    end if
    level_loop : do lev = 1,pver
       column_loop : do i = 1,ncol
          IF (lev <= ltrop(i)) CYCLE column_loop
          !-----------------------------------------------------------------------
          ! ... transfer from base to local work arrays
          !-----------------------------------------------------------------------
          do m = 1,rxntot
             lrxt(m) = reaction_rates(i,lev,m)
          end do
          if( gas_pcnst > 0 ) then
             do m = 1,gas_pcnst
                lhet(m) = het_rates(i,lev,m)
             end do
          end if
          !-----------------------------------------------------------------------
          ! ... time step loop
          !-----------------------------------------------------------------------
          dt = delt
          cut_cnt = 0
          fail_cnt = 0
          stp_con_cnt = 0
          interval_done = 0._r8
          time_step_loop : do
             dti = 1._r8 / dt
             !-----------------------------------------------------------------------
             ! ... transfer from base to local work arrays
             !-----------------------------------------------------------------------
             do m = 1,gas_pcnst
                lsol(m) = base_sol(i,lev,m)
             end do
             !-----------------------------------------------------------------------
             ! ... transfer from base to class array
             !-----------------------------------------------------------------------
             do k = 1,clscnt4
                j = clsmap(k,4)
                m = permute(k,4)
                solution(m) = lsol(j)
             end do
             !-----------------------------------------------------------------------
             ! ... set the iteration invariant part of the function f(y)
             !-----------------------------------------------------------------------
             if( cls_rxt_cnt(1,4) > 0 .or. extcnt > 0 ) then
                do m = 1,clscnt4
                   iter_invariant(m) = dti * solution(m) + ind_prd(i,lev,m)
                end do
             else
                do m = 1,clscnt4
                   iter_invariant(m) = dti * solution(m)
                end do
             end if
             !-----------------------------------------------------------------------
             ! ... the linear component
             !-----------------------------------------------------------------------
             !if( cls_rxt_cnt(2,4) > 0 ) then
                call linmat( lin_jac, lsol, lrxt, lhet )
             !end if
             !=======================================================================
             ! the newton-raphson iteration for f(y) = 0
             !=======================================================================
             iter_loop : do nr_iter = 1,itermax
                !-----------------------------------------------------------------------
                ! ... the non-linear component
                !-----------------------------------------------------------------------
                if( factor(nr_iter) ) then
                   call nlnmat( sys_jac, lsol, lrxt, lin_jac, dti )
                   !-----------------------------------------------------------------------
                   ! ... factor the "system" matrix
                   !-----------------------------------------------------------------------
                   call lu_fac( sys_jac )
                end if
                !-----------------------------------------------------------------------
                ! ... form f(y)
                !-----------------------------------------------------------------------
                call imp_prod_loss( prod, loss, lsol, lrxt, lhet )
                do m = 1,clscnt4
                   forcing(m) = solution(m)*dti - (iter_invariant(m) + prod(m) - loss(m))
                end do
                !-----------------------------------------------------------------------
                ! ... solve for the mixing ratio at t(n+1)
                !-----------------------------------------------------------------------
                call lu_slv( sys_jac, forcing )
                do m = 1,clscnt4
                   solution(m) = solution(m) + forcing(m)
                end do
                !-----------------------------------------------------------------------
                ! ... convergence measures
                !-----------------------------------------------------------------------
                if( nr_iter > 1 ) then
                   do k = 1,clscnt4
                      m = permute(k,4)
                      if( abs(solution(m)) > 1.e-20_r8 ) then
                         max_delta(k) = abs( forcing(m)/solution(m) )
                      else
                         max_delta(k) = 0._r8
                      end if
                   end do
                end if
                !-----------------------------------------------------------------------
                ! ... limit iterate
                !-----------------------------------------------------------------------
                where( solution(:) < 0._r8 )
                   solution(:) = 0._r8
                endwhere
                !-----------------------------------------------------------------------
                ! ... transfer latest solution back to work array
                !-----------------------------------------------------------------------
                do k = 1,clscnt4
                   j = clsmap(k,4)
                   m = permute(k,4)
                   lsol(j) = solution(m)
                end do
                !-----------------------------------------------------------------------
                ! ... check for convergence
                !-----------------------------------------------------------------------
                converged(:) = .true.
                if( nr_iter > 1 ) then
                   do k = 1,clscnt4
                      m = permute(k,4)
                      frc_mask = abs( forcing(m) ) > small
                      if( frc_mask ) then
                         converged(k) = abs(forcing(m)) <= epsilon(k)*abs(solution(m))
                      else
                         converged(k) = .true.
                      end if
                   end do
                   convergence = all( converged(:) )
                   if( convergence ) then
                      exit
                   end if
                end if
             end do iter_loop
!!!             write(*,*)'** newton-raphson steps: ',nr_iter
             !-----------------------------------------------------------------------
             ! ... check for newton-raphson convergence
             !-----------------------------------------------------------------------
             if( .not. convergence ) then
                !-----------------------------------------------------------------------
                ! ... non-convergence
                !-----------------------------------------------------------------------
                fail_cnt = fail_cnt + 1
                nstep = get_nstep()
                write(iulog,'('' imp_sol: Time step '',1p,e21.13,'' failed to converge @ (lchnk,lev,col,nstep) = '',4i6)') &
                     dt,lchnk,lev,i,nstep
                stp_con_cnt = 0
                if( cut_cnt < cut_limit ) then
                   cut_cnt = cut_cnt + 1
                   if( cut_cnt < cut_limit ) then
                      dt = .5_r8 * dt
                   else
                      dt = .1_r8 * dt
                   end if
                   cycle time_step_loop
                else
                   write(iulog,'('' imp_sol: Failed to converge @ (lchnk,lev,col,nstep,dt,time) = '',4i6,1p,2e21.13)') &
                        lchnk,lev,i,nstep,dt,interval_done+dt
                   do m = 1,clscnt4
                      if( .not. converged(m) ) then
                         write(iulog,'(1x,a8,1x,1pe10.3)') solsym(clsmap(m,4)), max_delta(m)
                      end if
                   end do
                end if
             end if
             !-----------------------------------------------------------------------
             ! ... check for interval done
             !-----------------------------------------------------------------------
             interval_done = interval_done + dt
             if( abs( delt - interval_done ) <= .0001_r8 ) then
                if( fail_cnt > 0 ) then
                   write(iulog,*) 'imp_sol : @ (lchnk,lev,col) = ',lchnk,lev,i,' failed ',fail_cnt,' times'
                end if
                exit time_step_loop
             else
                !-----------------------------------------------------------------------
                ! ... transfer latest solution back to base array
                !-----------------------------------------------------------------------
                if( convergence ) then
                   stp_con_cnt = stp_con_cnt + 1
                end if
                do m = 1,gas_pcnst
                   base_sol(i,lev,m) = lsol(m)
                end do
                if( stp_con_cnt >= 2 ) then
                   dt = 2._r8*dt
                   stp_con_cnt = 0
                end if
                dt = min( dt,delt-interval_done )
                ! write(iulog,'('' imp_sol: New time step '',1p,e21.13)') dt
             end if
          end do time_step_loop
          !-----------------------------------------------------------------------
          ! ... Transfer latest solution back to base array
          !-----------------------------------------------------------------------
          cls_loop: do k = 1,clscnt4
             j = clsmap(k,4)
             m = permute(k,4)
             base_sol(i,lev,j) = solution(m)
          end do cls_loop
          !-----------------------------------------------------------------------
          ! ... Prod/Loss history buffers...
          !-----------------------------------------------------------------------
          cls_loop2: do k = 1,clscnt4
             j = clsmap(k,4)
             m = permute(k,4)
             has_o3_chem: if( ( full_ozone_chem .or. reduced_ozone_chem .or. middle_atm_chem ) .and. &
                              (j == ox_ndx .or. j == o3a_ndx )) then
                if( o1d_ndx < 1 ) then
                   loss_out(i,lev,k) = reaction_rates(i,lev,ox_l1_ndx)
                else
                   if (j == ox_ndx) &
                      loss_out(i,lev,k) = reaction_rates(i,lev,ox_l1_ndx) * base_sol(i,lev,o1d_ndx) &
                                          / base_sol(i,lev,ox_ndx)
                   if (j == o3a_ndx) &
                      loss_out(i,lev,k) = reaction_rates(i,lev,ox_l1_ndx) * base_sol(i,lev,o1da_ndx)&
                                          / base_sol(i,lev,o3a_ndx)
                   if ( h2o_ndx > 0 ) &
                      loss_out(i,lev,k) = loss_out(i,lev,k) * base_sol(i,lev,h2o_ndx)
                end if
                if ( full_ozone_chem ) then
                   prod_out(i,lev,k) = reaction_rates(i,lev,ox_p1_ndx) * base_sol(i,lev,ho2_ndx) &
                        + reaction_rates(i,lev,ox_p2_ndx) * base_sol(i,lev,ch3o2_ndx) &
                        + reaction_rates(i,lev,ox_p3_ndx) * base_sol(i,lev,po2_ndx) &
                        + reaction_rates(i,lev,ox_p4_ndx) * base_sol(i,lev,ch3co3_ndx) &
                        + reaction_rates(i,lev,ox_p5_ndx) * base_sol(i,lev,c2h5o2_ndx) &
                        + .92_r8* reaction_rates(i,lev,ox_p6_ndx) * base_sol(i,lev,isopo2_ndx) &
                        + reaction_rates(i,lev,ox_p7_ndx) * base_sol(i,lev,macro2_ndx) &
                        + reaction_rates(i,lev,ox_p8_ndx) * base_sol(i,lev,mco3_ndx) &
                        + reaction_rates(i,lev,ox_p9_ndx) * base_sol(i,lev,c3h7o2_ndx) &
                        + reaction_rates(i,lev,ox_p10_ndx)* base_sol(i,lev,ro2_ndx) &
                        + reaction_rates(i,lev,ox_p11_ndx)* base_sol(i,lev,xo2_ndx) &
                        + .9_r8*reaction_rates(i,lev,ox_p12_ndx)*base_sol(i,lev,tolo2_ndx) &
                        + reaction_rates(i,lev,ox_p13_ndx)*base_sol(i,lev,terpo2_ndx)&
                        + .9_r8*reaction_rates(i,lev,ox_p14_ndx)*base_sol(i,lev,alko2_ndx) &
                        + reaction_rates(i,lev,ox_p15_ndx)*base_sol(i,lev,eneo2_ndx) &
                        + reaction_rates(i,lev,ox_p16_ndx)*base_sol(i,lev,eo2_ndx) &
                        + reaction_rates(i,lev,ox_p17_ndx)*base_sol(i,lev,meko2_ndx)
                   loss_out(i,lev,k) = loss_out(i,lev,k) &
                        + reaction_rates(i,lev,ox_l2_ndx) * base_sol(i,lev,oh_ndx) &
                        + reaction_rates(i,lev,ox_l3_ndx) * base_sol(i,lev,ho2_ndx) &
                        + reaction_rates(i,lev,ox_l6_ndx) * base_sol(i,lev,c2h4_ndx) &
                        + reaction_rates(i,lev,ox_l4_ndx) * base_sol(i,lev,c3h6_ndx) &
                        + .9_r8* reaction_rates(i,lev,ox_l5_ndx) * base_sol(i,lev,isop_ndx) &
                        + .8_r8*( reaction_rates(i,lev,ox_l7_ndx) * base_sol(i,lev,mvk_ndx) &
                        + reaction_rates(i,lev,ox_l8_ndx) * base_sol(i,lev,macro2_ndx)) &
                        + .235_r8*reaction_rates(i,lev,ox_l9_ndx) * base_sol(i,lev,c10h16_ndx)
                else if ( reduced_ozone_chem ) then
                   prod_out(i,lev,k) = reaction_rates(i,lev,ox_p1_ndx ) * base_sol(i,lev,ho2_ndx) &
                        + reaction_rates(i,lev,ox_p2_ndx ) * base_sol(i,lev,ch3o2_ndx) &
                        + reaction_rates(i,lev,ox_p3_ndx ) * base_sol(i,lev,c2o3_ndx) &
                        + reaction_rates(i,lev,ox_p11_ndx ) * base_sol(i,lev,xo2_ndx)
                   loss_out(i,lev,k) = loss_out(i,lev,k) &
                        + reaction_rates(i,lev,ox_l2_ndx) * base_sol(i,lev,oh_ndx) &
                        + reaction_rates(i,lev,ox_l3_ndx) * base_sol(i,lev,ho2_ndx) &
                        + .9_r8* reaction_rates(i,lev,ox_l5_ndx) * base_sol(i,lev,isop_ndx) &
                        + reaction_rates(i,lev,ox_l6_ndx) * base_sol(i,lev,c2h4_ndx) &
                        + reaction_rates(i,lev,ox_l7_ndx) * base_sol(i,lev,ole_ndx)
                else if ( middle_atm_chem ) then
                   loss_out(i,lev,k) = loss_out(i,lev,k) &
                        + reaction_rates(i,lev,ox_l2_ndx) * base_sol(i,lev,oh_ndx) &
                        + reaction_rates(i,lev,ox_l3_ndx) * base_sol(i,lev,ho2_ndx)
                endif
                if (j == ox_ndx) then
                   if ( .not. middle_atm_chem ) then
                      loss_out(i,lev,k) = loss_out(i,lev,k) &
                           + ( reaction_rates(i,lev,usr4_ndx) * base_sol(i,lev,no2_ndx) * base_sol(i,lev,oh_ndx) &
                           + 3._r8 * reaction_rates(i,lev,usr16_ndx) * base_sol(i,lev,n2o5_ndx) &
                           + 2._r8 * reaction_rates(i,lev,usr17_ndx) * base_sol(i,lev,no3_ndx) ) &
                           / max( base_sol(i,lev,ox_ndx),1.e-20_r8 )
                   end if
                   if (present(o3s_loss)) then
                      o3s_loss(i,lev) = loss_out(i,lev,k)
                   endif
                   loss_out(i,lev,k) = loss_out(i,lev,k) * base_sol(i,lev,ox_ndx)
                   prod_out(i,lev,k) = prod_out(i,lev,k) * base_sol(i,lev,no_ndx)
                else if (j == o3a_ndx) then
                   loss_out(i,lev,k) = loss_out(i,lev,k) &
                        + ( reaction_rates(i,lev,usr4a_ndx) * base_sol(i,lev,xno2_ndx) * base_sol(i,lev,oh_ndx) &
                        + 1._r8 * reaction_rates(i,lev,usr16a_ndx) * base_sol(i,lev,xno2no3_ndx) &
                        + 2._r8 * reaction_rates(i,lev,usr16b_ndx) * base_sol(i,lev,no2xno3_ndx) &
                        + 2._r8 * reaction_rates(i,lev,usr17b_ndx) * base_sol(i,lev,xno3_ndx) ) &
                        / max( base_sol(i,lev,o3a_ndx),1.e-20_r8 )
                   loss_out(i,lev,k) = loss_out(i,lev,k) * base_sol(i,lev,o3a_ndx)
                   prod_out(i,lev,k) = prod_out(i,lev,k) * base_sol(i,lev,xno_ndx)
                endif
             else
                prod_out(i,lev,k) = prod(m) + ind_prd(i,lev,m)
                loss_out(i,lev,k) = loss(m)
             endif has_o3_chem
          end do cls_loop2
       end do column_loop
    end do level_loop
    do i = 1,clscnt4
       j = clsmap(i,4)
       prod_out(:,:,i) = prod_out(:,:,i)*xhnm
       loss_out(:,:,i) = loss_out(:,:,i)*xhnm
!
! added code for ROOH production !PJY not "RO2 production"
!
       if ( trim(solsym(j)) == 'ALKOOH' &
        .or.trim(solsym(j)) == 'C2H5OOH' &
        .or.trim(solsym(j)) == 'CH3OOH' & !PJY added this
        .or.trim(solsym(j)) == 'CH3COOH' &
        .or.trim(solsym(j)) == 'CH3COOOH' &
        .or.trim(solsym(j)) == 'C3H7OOH' & !PJY corrected this (from CH3H7OOH)
        .or.trim(solsym(j)) == 'EOOH' &
! .or.trim(solsym(j)) == 'H2O2' & !PJY removed as H2O2 production asked for separately (as I read 4.2.3, point 7)
! .or.trim(solsym(j)) == 'HCOOH' & !PJY removed this as this is formic acid HC(O)OH - i.e. not H-C-O-O-H
        .or.trim(solsym(j)) == 'ISOPOOH' &
        .or.trim(solsym(j)) == 'MACROOH' &
        .or.trim(solsym(j)) == 'MEKOOH' &
        .or.trim(solsym(j)) == 'POOH' &
        .or.trim(solsym(j)) == 'ROOH' &
        .or.trim(solsym(j)) == 'TERPOOH' &
        .or.trim(solsym(j)) == 'TOLOOH' &
        .or.trim(solsym(j)) == 'XOOH' ) then
! .or.trim(solsym(j)) == 'H2O2' & !PJY removed as H2O2 production asked for separately (as I read 4.2.3, point 7)
! .or.trim(solsym(j)) == 'HCOOH' & !PJY removed this as this is formic acid HC(O)OH - i.e. not H-C-O-O-H
!
          prod_hydrogen_peroxides_out(:,:) = prod_hydrogen_peroxides_out(:,:) + prod_out(:,:,i)
!
       end if
!
    enddo
!
!
  end subroutine imp_sol
  !read state subroutine for kr_externs_in_mo_imp_sol
  SUBROUTINE kr_externs_in_mo_imp_sol(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
      
      READ (UNIT = kgen_unit) small
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) kgen_array_sum
          READ (UNIT = kgen_unit) epsilon
          CALL kgen_array_sumcheck("epsilon", kgen_array_sum, REAL(SUM(epsilon), 8), .TRUE.)
      END IF 
      READ (UNIT = kgen_unit) kgen_istrue
      IF (kgen_istrue) THEN
          READ (UNIT = kgen_unit) factor
      END IF 
      READ (UNIT = kgen_unit) ox_ndx
      READ (UNIT = kgen_unit) o1d_ndx
      READ (UNIT = kgen_unit) h2o_ndx
      READ (UNIT = kgen_unit) po2_ndx
      READ (UNIT = kgen_unit) oh_ndx
      READ (UNIT = kgen_unit) ho2_ndx
      READ (UNIT = kgen_unit) ch3co3_ndx
      READ (UNIT = kgen_unit) ch3o2_ndx
      READ (UNIT = kgen_unit) c2h5o2_ndx
      READ (UNIT = kgen_unit) mco3_ndx
      READ (UNIT = kgen_unit) isopo2_ndx
      READ (UNIT = kgen_unit) c3h7o2_ndx
      READ (UNIT = kgen_unit) macro2_ndx
      READ (UNIT = kgen_unit) xo2_ndx
      READ (UNIT = kgen_unit) no_ndx
      READ (UNIT = kgen_unit) n2o5_ndx
      READ (UNIT = kgen_unit) ro2_ndx
      READ (UNIT = kgen_unit) no2_ndx
      READ (UNIT = kgen_unit) no3_ndx
      READ (UNIT = kgen_unit) mvk_ndx
      READ (UNIT = kgen_unit) c10h16_ndx
      READ (UNIT = kgen_unit) isop_ndx
      READ (UNIT = kgen_unit) c3h6_ndx
      READ (UNIT = kgen_unit) c2h4_ndx
      READ (UNIT = kgen_unit) ox_p2_ndx
      READ (UNIT = kgen_unit) ox_p3_ndx
      READ (UNIT = kgen_unit) ox_p4_ndx
      READ (UNIT = kgen_unit) ox_p5_ndx
      READ (UNIT = kgen_unit) ox_p1_ndx
      READ (UNIT = kgen_unit) ox_p10_ndx
      READ (UNIT = kgen_unit) ox_p6_ndx
      READ (UNIT = kgen_unit) ox_p7_ndx
      READ (UNIT = kgen_unit) ox_p8_ndx
      READ (UNIT = kgen_unit) ox_p9_ndx
      READ (UNIT = kgen_unit) ox_p11_ndx
      READ (UNIT = kgen_unit) ox_l1_ndx
      READ (UNIT = kgen_unit) ox_l2_ndx
      READ (UNIT = kgen_unit) ox_l3_ndx
      READ (UNIT = kgen_unit) ox_l4_ndx
      READ (UNIT = kgen_unit) ox_l5_ndx
      READ (UNIT = kgen_unit) ox_l6_ndx
      READ (UNIT = kgen_unit) ox_l7_ndx
      READ (UNIT = kgen_unit) ox_l8_ndx
      READ (UNIT = kgen_unit) ox_l9_ndx
      READ (UNIT = kgen_unit) usr4_ndx
      READ (UNIT = kgen_unit) ole_ndx
      READ (UNIT = kgen_unit) usr17_ndx
      READ (UNIT = kgen_unit) usr16_ndx
      READ (UNIT = kgen_unit) c2o3_ndx
      READ (UNIT = kgen_unit) eneo2_ndx
      READ (UNIT = kgen_unit) alko2_ndx
      READ (UNIT = kgen_unit) terpo2_ndx
      READ (UNIT = kgen_unit) eo2_ndx
      READ (UNIT = kgen_unit) tolo2_ndx
      READ (UNIT = kgen_unit) meko2_ndx
      READ (UNIT = kgen_unit) ox_p17_ndx
      READ (UNIT = kgen_unit) ox_p13_ndx
      READ (UNIT = kgen_unit) ox_p16_ndx
      READ (UNIT = kgen_unit) ox_p12_ndx
      READ (UNIT = kgen_unit) ox_p14_ndx
      READ (UNIT = kgen_unit) ox_p15_ndx
      READ (UNIT = kgen_unit) full_ozone_chem
      READ (UNIT = kgen_unit) middle_atm_chem
      READ (UNIT = kgen_unit) reduced_ozone_chem
      READ (UNIT = kgen_unit) o3a_ndx
      READ (UNIT = kgen_unit) xno2_ndx
      READ (UNIT = kgen_unit) no2xno3_ndx
      READ (UNIT = kgen_unit) xno_ndx
      READ (UNIT = kgen_unit) xno3_ndx
      READ (UNIT = kgen_unit) o1da_ndx
      READ (UNIT = kgen_unit) xno2no3_ndx
      READ (UNIT = kgen_unit) usr4a_ndx
      READ (UNIT = kgen_unit) usr17b_ndx
      READ (UNIT = kgen_unit) usr16b_ndx
      READ (UNIT = kgen_unit) usr16a_ndx
  END SUBROUTINE kr_externs_in_mo_imp_sol
  
  !read state subroutine for kr_externs_out_mo_imp_sol
  SUBROUTINE kr_externs_out_mo_imp_sol(kgen_unit)
      INTEGER, INTENT(IN) :: kgen_unit
      
      LOGICAL :: kgen_istrue
      REAL(KIND=8) :: kgen_array_sum
  END SUBROUTINE kr_externs_out_mo_imp_sol
  
end module mo_imp_sol