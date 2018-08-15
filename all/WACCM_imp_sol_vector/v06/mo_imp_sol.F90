!KGEN-generated Fortran source file

!Generated at : 2016-03-01 11:27:40
!KGEN version : 0.6.2






      module mo_imp_sol

          USE shr_kind_mod, ONLY: rkind_comp
          USE shr_kind_mod, ONLY: rkind_io
          USE ppgrid, ONLY: veclen
          USE chem_mods, ONLY: clscnt4, gas_pcnst, clsmap

          USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck
          IMPLICIT NONE

!-----------------------------------------------------------------------
! newton-Raphson iteration limits
!-----------------------------------------------------------------------
      integer, parameter :: itermax = 11
      integer, parameter :: cut_limit = 5
      real(rkind_comp), parameter :: sol_min = 1.e-20_rkind_comp
#if defined(USE_R4)
      real(rkind_comp), parameter :: small = 1.e-35_rkind_comp
#else
      real(rkind_comp), parameter :: small = 1.e-40_rkind_comp
#endif

      SAVE

      real(rkind_comp) :: epsilon(clscnt4)
      real(rkind_io)   :: epsilon_io(clscnt4)
      logical :: factor(itermax)
      logical :: first_time = .true.

      PRIVATE
      PUBLIC imp_sol

      PUBLIC kr_externs_in_mo_imp_sol
      PUBLIC kr_externs_out_mo_imp_sol
      contains

!-----------------------------------------------------------------------
! ... initialize the implict solver
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------

!!$      eps((/id_o3,id_no,id_no2,id_no3,id_hno3,id_ho2no2,id_n2o5,id_oh,id_ho2/)) = .0001_rkind_comp



      subroutine imp_sol( base_sol_full, reaction_rates_full, het_rates_full, extfrc_full, delt, &
                          ncol, lchnk, chnkpnts )
!-----------------------------------------------------------------------
! ... imp_sol advances the volumetric mixing ratio
! forward one time step via the fully implicit euler scheme.
! this source is meant for vector architectures such as the
! nec sx6 and cray x1
!-----------------------------------------------------------------------

          USE chem_mods, ONLY: rxntot, extcnt, nzcnt, permute, cls_rxt_cnt
          USE mo_tracname, ONLY: solsym
          USE mo_lin_matrix, ONLY: linmat
          USE mo_nln_matrix, ONLY: nlnmat
          USE mo_lu_factor, ONLY: lu_fac
          USE mo_lu_solve, ONLY: lu_slv
          USE mo_prod_loss, ONLY: imp_prod_loss, imp_prod_loss_blk
          USE mo_indprd, ONLY: indprd

      implicit none

!-----------------------------------------------------------------------
! ... dummy args
!-----------------------------------------------------------------------
      integer, intent(in) :: ncol ! columns in chunck
      integer, intent(in) :: lchnk ! chunk id
      integer, intent(in) :: chnkpnts ! total spatial points in chunk; ncol*pver
      real(rkind_comp), intent(in) :: delt ! time step (s)
      real(rkind_comp), intent(in) :: reaction_rates_full(chnkpnts,max(1,rxntot)) ! rxt rates (1/cm^3/s)
      real(rkind_comp), intent(in) :: extfrc_full(chnkpnts,max(1,extcnt)) ! external in-situ forcing (1/cm^3/s)
      real(rkind_comp), intent(in) :: het_rates_full(chnkpnts,max(1,gas_pcnst)) ! washout rates (1/s)
      real(rkind_comp), intent(inout) :: base_sol_full(chnkpnts,gas_pcnst) ! species mixing ratios (vmr)

!-----------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------
      integer :: nr_iter
      integer :: ofl
      integer :: ofu
      integer :: bndx ! base index
      integer :: cndx ! class index
      integer :: pndx ! permuted class index
      integer :: i, m
      integer :: fail_cnt
      integer :: cut_cnt
      integer :: stp_con_cnt
      integer :: nstep
      integer :: avec_len
      real(rkind_comp) :: interval_done
      real(rkind_comp) :: dt
      real(rkind_comp) :: dti
      real(rkind_comp) :: max_delta(max(1,clscnt4))
      real(rkind_comp) :: ind_prd_full(chnkpnts,max(1,clscnt4))
      logical :: convergence
      logical :: converged(max(1,clscnt4))

      real(rkind_comp) :: sys_jac_blk(veclen,max(1,nzcnt))
      real(rkind_comp) :: lin_jac_blk(veclen,max(1,nzcnt))
      real(rkind_comp) :: solution_blk(veclen,max(1,clscnt4))
      real(rkind_comp) :: forcing_blk(veclen,max(1,clscnt4))
      real(rkind_comp) :: iter_invariant_blk(veclen,max(1,clscnt4))
      real(rkind_comp) :: prod_blk(veclen,max(1,clscnt4))
      real(rkind_comp) :: loss_blk(veclen,max(1,clscnt4))
      real(rkind_comp) :: ind_prd_blk(veclen,max(1,clscnt4))
      real(rkind_comp) :: sbase_sol_blk(veclen,gas_pcnst)
      real(rkind_comp) :: wrk_blk(veclen)
      logical :: spc_conv_blk(veclen,max(1,clscnt4))
      logical :: cls_conv_blk(veclen)
      real(rkind_comp) :: reaction_rates_blk(veclen,max(1,rxntot))
      real(rkind_comp) :: extfrc_blk(veclen,max(1,extcnt))
      real(rkind_comp) :: het_rates_blk(veclen,max(1,gas_pcnst))
      real(rkind_comp) :: base_sol_blk(veclen,gas_pcnst)

!-----------------------------------------------------------------------
! ... class independent forcing
!-----------------------------------------------------------------------
      if( cls_rxt_cnt(1,4) > 0 .or. extcnt > 0 ) then
         call indprd( 4, ind_prd_full, base_sol_full, extfrc_full, reaction_rates_full, chnkpnts )
      else
         do m = 1,clscnt4
            ind_prd_full(:,m) = 0._rkind_comp
         end do
      end if

      ofl = 1
chnkpnts_loop : &
      do 
         ofu = min( chnkpnts,ofl + veclen - 1 )
         avec_len = (ofu - ofl) + 1
!         write(*,*) 'avec_len = ',avec_len, ofl, ofu

!         sys_jac_blk = 0._rkind_comp
!         lin_jac_blk = 0._rkind_comp
!         solution_blk = 0._rkind_comp
!         forcing_blk = 0._rkind_comp
!         iter_invariant_blk = 0._rkind_comp
!         prod_blk = 0._rkind_comp
!         loss_blk = 0._rkind_comp
!         ind_prd_blk = 0._rkind_comp
!         sbase_sol_blk = 0._rkind_comp
!         wrk_blk = 0._rkind_comp
!         spc_conv_blk = 0
!         cls_conv_blk = 0
!         reaction_rates_blk = 0._rkind_comp
!         extfrc_blk = 0._rkind_comp
!         het_rates_blk = 0._rkind_comp
!         base_sol_blk = 0._rkind_comp

         reaction_rates_blk(1:avec_len,:) = reaction_rates_full(ofl:ofu,:)
         extfrc_blk(1:avec_len,:) = extfrc_full(ofl:ofu,:)
         het_rates_blk(1:avec_len,:) = het_rates_full(ofl:ofu,:)
         ind_prd_blk(1:avec_len,:) = ind_prd_full(ofl:ofu,:)
         base_sol_blk(1:avec_len,:) = base_sol_full(ofl:ofu,:)

         do m = 1,gas_pcnst
           do i = 1, avec_len
             sbase_sol_blk(i,m) = base_sol_blk(i,m)
           end do
         end do
!-----------------------------------------------------------------------
! ... time step loop
!-----------------------------------------------------------------------
         dt = delt
         cut_cnt = 0
         fail_cnt = 0
         stp_con_cnt = 0
         interval_done = 0._rkind_comp
time_step_loop : &
         do
            dti = 1._rkind_comp / dt
!-----------------------------------------------------------------------
! ... transfer from base to class array
!-----------------------------------------------------------------------
            do cndx = 1,clscnt4
               bndx = clsmap(cndx,4)
               pndx = permute(cndx,4)
               do i = 1, avec_len
                 solution_blk(i,pndx) = base_sol_blk(i,bndx)
               end do
            end do
!-----------------------------------------------------------------------
! ... set the iteration invariant part of the function f(y)
!-----------------------------------------------------------------------
            if( cls_rxt_cnt(1,4) > 0 .or. extcnt > 0 ) then
               do m = 1,clscnt4
                 do i = 1, avec_len
                   iter_invariant_blk(i,m) = dti * solution_blk(i,m) + ind_prd_blk(i,m)
                 enddo
               end do
            else
               do m = 1,clscnt4
                 do i = 1, avec_len
                   iter_invariant_blk(i,m) = dti * solution_blk(i,m)
                 end do
               end do
            end if
!-----------------------------------------------------------------------
! ... the linear component
!-----------------------------------------------------------------------
            if( cls_rxt_cnt(2,4) > 0 ) then
               call linmat( avec_len, lin_jac_blk, base_sol_blk, reaction_rates_blk, het_rates_blk)
            end if
!=======================================================================
! the newton-raphson iteration for f(y) = 0
!=======================================================================
            do i = 1, avec_len
              cls_conv_blk(i) = .false.
            end do
iter_loop : do nr_iter = 1,itermax
!-----------------------------------------------------------------------
! ... the non-linear component
!-----------------------------------------------------------------------
               if( factor(nr_iter) ) then
                  call nlnmat( avec_len, sys_jac_blk, base_sol_blk, reaction_rates_blk, lin_jac_blk, dti)
!-----------------------------------------------------------------------
! ... factor the "system" matrix
!-----------------------------------------------------------------------
                  call lu_fac( avec_len, sys_jac_blk)
               end if
!-----------------------------------------------------------------------
! ... form f(y)
!-----------------------------------------------------------------------
               call imp_prod_loss( avec_len, prod_blk, loss_blk, base_sol_blk, &
                                   reaction_rates_blk, het_rates_blk )
               do m = 1,clscnt4
                 do i = 1, avec_len
                   forcing_blk(i,m) = solution_blk(i,m)*dti &
                                - (iter_invariant_blk(i,m) + prod_blk(i,m) - loss_blk(i,m))
                 enddo
               end do
!-----------------------------------------------------------------------
! ... solve for the mixing ratio at t(n+1)
!-----------------------------------------------------------------------
               call lu_slv( avec_len, sys_jac_blk, forcing_blk )

               do m = 1,clscnt4
                 do i = 1, avec_len
                   if( .not. cls_conv_blk(i) ) then
                     solution_blk(i,m) = solution_blk(i,m) + forcing_blk(i,m)
                   else
                     forcing_blk(i,m) = 0._rkind_comp
                   endif
                 end do
               end do
!-----------------------------------------------------------------------
! ... convergence measures and test
!-----------------------------------------------------------------------
! SAM conv_chk : if( nr_iter > 1 ) then
               if( nr_iter > 1 ) then
!-----------------------------------------------------------------------
! ... check for convergence
!-----------------------------------------------------------------------
                  do cndx = 1,clscnt4
                     pndx = permute(cndx,4)
                     bndx = clsmap(cndx,4)
                     do i = 1, avec_len
                       if ( abs( solution_blk(i,pndx) ) > sol_min ) then
                          wrk_blk(i) = abs( forcing_blk(i,pndx)/solution_blk(i,pndx) )
                       else
                        wrk_blk(i) = 0._rkind_comp
                       endif
                     enddo

                     max_delta(cndx) = maxval( wrk_blk(1:avec_len) )
                     do i = 1, avec_len
                       solution_blk(i,pndx) = max( 0._rkind_comp,solution_blk(i,pndx) )
                       base_sol_blk(i,bndx) = solution_blk(i,pndx)
                       if ( abs( forcing_blk(i,pndx) ) > small ) then
                         spc_conv_blk(i,cndx) = abs(forcing_blk(i,pndx)) <= epsilon(cndx)*abs(solution_blk(i,pndx))
                       else
                         spc_conv_blk(i,cndx) = .true.
                       endif
                     enddo
                     converged(cndx) = all( spc_conv_blk(1:avec_len,cndx) )
                  end do

                  convergence = all( converged(:) )
                  if( convergence ) then
                     base_sol_full(ofl:ofu,:) = base_sol_blk(1:avec_len,:)
                     ind_prd_full(ofl:ofu,:) = ind_prd_blk(1:avec_len,:)
                     exit iter_loop
                  end if
                  do m = 1, avec_len
                     if( .not. cls_conv_blk(m) ) then
                        cls_conv_blk(m) = all( spc_conv_blk(m,:) )
                     end if
                  end do
! SAM               else conv_chk
                else !conv_chk
!-----------------------------------------------------------------------
! ... limit iterate
!-----------------------------------------------------------------------
                  do m = 1,clscnt4
                    do i = 1, avec_len
                      solution_blk(i,m) = max( 0._rkind_comp,solution_blk(i,m) )
                    end do
                  end do
!-----------------------------------------------------------------------
! ... transfer latest solution back to base array
!-----------------------------------------------------------------------
                  do cndx = 1,clscnt4
                     pndx = permute(cndx,4)
                     bndx = clsmap(cndx,4)
                     do i = 1, avec_len
                       base_sol_blk(i,bndx) = solution_blk(i,pndx)
                     end do
                  end do
! SAM               end if conv_chk
                end if !conv_chk
            end do iter_loop
!!!            write(*,*)'** newton-raphson steps: ',nr_iter
!-----------------------------------------------------------------------
! ... check for newton-raphson convergence
!-----------------------------------------------------------------------
! SAM  non_conv : if( .not. convergence ) then
            if( .not. convergence ) then
!-----------------------------------------------------------------------
! ... non-convergence
!-----------------------------------------------------------------------
               fail_cnt = fail_cnt + 1
!               nstep = get_nstep()
!               write(*,'('' imp_sol: time step '',1p,g15.7,'' failed to converge @ (lchnk,lev,nstep) = '',3i6)') &
!                              dt,lchnk,lev,nstep
               stp_con_cnt = 0
! SAM step_reduction : &
               if( cut_cnt < cut_limit ) then
                  cut_cnt = cut_cnt + 1
                  if( cut_cnt < cut_limit ) then
                     dt = .5_rkind_comp * dt
                  else
                     dt = .1_rkind_comp * dt
                  end if
                  do m = 1,gas_pcnst
                     base_sol_blk(:,m) = sbase_sol_blk(:,m)
                  end do
                  cycle time_step_loop
! SAM               else step_reduction
                  else !step_reduction
                  write(*,*)' imp_sol: step failed to converge @ (lchnk,lev,nstep,dt,time) = ',ofl,ofu!,3i6,1p,2g15.7)') &
!                        lchnk,lev,nstep,dt,interval_done+dt
!                 do m = 1,clscnt4
!                     if( .not. converged(m) ) then
!                        write(*,'(1x,a8,1x,1pe10.3)') solsym(clsmap(m,4)), max_delta(m)
!                     end if
!                  end do
! SM               end if step_reduction
                  end if !step_reduction
! SAM            end if non_conv
            end if !non_conv
!-----------------------------------------------------------------------
! ... check for interval done
!-----------------------------------------------------------------------
            interval_done = interval_done + dt
! SAM time_step_done : &
            if( abs( delt - interval_done ) <= .0001_rkind_comp ) then
!JMD               if( fail_cnt > 0 ) then
!JMD                  write(*,*) 'imp_sol : @ (lchnk,lev) = '!,lchnk,lev,' failed ',fail_cnt,' times'
!JMD               end if
               base_sol_full(ofl:ofu,:) = base_sol_blk(1:avec_len,:)
               ind_prd_full(ofl:ofu,:) = ind_prd_blk(1:avec_len,:)
               exit time_step_loop
! SAM            else time_step_done
            else !time_step_done
!-----------------------------------------------------------------------
! ... transfer latest solution back to base array
!-----------------------------------------------------------------------
               if( convergence ) then
                  stp_con_cnt = stp_con_cnt + 1
               end if
               do m = 1,gas_pcnst
                 do i = 1, avec_len
                   sbase_sol_blk(i,m) = base_sol_blk(i,m)
                 end do
               end do
               if( stp_con_cnt >= 2 ) then
                  dt = 2._rkind_comp*dt
                  stp_con_cnt = 0
               end if
               dt = min( dt,delt-interval_done )
! SAM            end if time_step_done
               end if !time_step_done
         end do time_step_loop
         ofl = ofu + 1
         if( ofl > chnkpnts ) then
            exit chnkpnts_loop
         end if
      end do chnkpnts_loop

      end subroutine imp_sol

      !read state subroutine for kr_externs_in_mo_imp_sol
      SUBROUTINE kr_externs_in_mo_imp_sol(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          LOGICAL :: kgen_istrue
          REAL(KIND=kgen_dp) :: kgen_array_sum
          
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              READ (UNIT = kgen_unit) kgen_array_sum
              READ (UNIT = kgen_unit) epsilon_io
              epsilon=REAL(epsilon_io,kind=rkind_comp)
              CALL kgen_array_sumcheck("epsilon", kgen_array_sum, REAL(SUM(epsilon_io), kgen_dp), .TRUE.)
          END IF 
          READ (UNIT = kgen_unit) kgen_istrue
          IF (kgen_istrue) THEN
              READ (UNIT = kgen_unit) factor
          END IF 
      END SUBROUTINE kr_externs_in_mo_imp_sol
      
      !read state subroutine for kr_externs_out_mo_imp_sol
      SUBROUTINE kr_externs_out_mo_imp_sol(kgen_unit)
          INTEGER, INTENT(IN) :: kgen_unit
          
          LOGICAL :: kgen_istrue
          REAL(KIND=kgen_dp) :: kgen_array_sum
      END SUBROUTINE kr_externs_out_mo_imp_sol
      
      end module mo_imp_sol
