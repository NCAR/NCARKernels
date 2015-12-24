!KGEN-generated Fortran source file

!Generated at : 2015-12-24 11:55:35
!KGEN version : 0.6.0

! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://1-dev.github.com/license.html
!
!==================================================================================================
 module mpas_atmphys_o3climatology
     USE mpas_kind_types

!wrf physics:

     IMPLICIT NONE
     PRIVATE
     PUBLIC vinterp_ozn


!>\brief
!> mpas_atmphys_o3climatology contains the subroutines needed to initialize,interpolate,and update
!> the climatological monthly-mean ozone volume mixing ratios o3clim to the MPAS grid. Input data
!> files are the same as the ones used in the CAM long- and short-wave radiation codes.
!> when namelist parameter config_o3climatology is set to true, o3clim is used in the RRTMG long-
!> wave and short-wave radiation codes,and replaces the annual-mean ozone sounding used by default.
!>\author Laura D. Fowler (send comments to laura@ucar.edu).
!>\date 2013-07-03.

!>\details
!> subroutines in mpas_atmphys_o3climatology:
!> ------------------------------------------
!> init_o3climatology     : read the CAM ozone data files.
!> update_o3climatology   : interpolates the ozone volume mixing ratio to the current Julian day
!>                          as done for the greeness fraction in the MPAS time manager.
!> o3climatology_from_MPAS: interpolates the ozone volume mixing ratio to the current Julian day
!>                          as in the CAM radiation codes.      
!> vinterp_ozn            : vertical interpolation of the ozone volume mixing ratios from fixed
!>                          ozone pressure levels to the MPAS pressure levels.
!>
!> add-ons and modifications to sourcecode:
!> ----------------------------------------
!>    * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
!>    * modified sourcecode to use pools.
!>      Laura D. Fowler (laura@ucar.edu) / 2014-05-15.


 contains


!==================================================================================================
!==================================================================================================

!This subroutine assumes a uniform distribution of ozone concentration. It should be replaced
!with monthly climatology varying ozone distribution.

!input arguments:

!inout arguments:

!local pointers:

!local variables:

 

!--------------------------------------------------------------------------------------------------
! write(0,*)
! write(0,*) '--- enter subroutine physics_init_o3:'



!-- read in ozone pressure data:



!-- read in ozone lat data:

!   write(0,101) j,lat_ozone(j)

!-- read in ozone data:




!INTERPOLATION OF INPUT OZONE DATA TO MPAS GRID:
!write(0,*) 'max latCell=', maxval(latCell)/degrad
!write(0,*) 'min latCell=', minval(latCell)/degrad
!write(0,*) 'max lonCell=', maxval(lonCell)/degrad
!write(0,*) 'min lonCell=', minval(lonCell)/degrad
!write(0,*)
!write(0,*) 'max lat_ozone=',maxval(lat_ozone)
!write(0,*) 'min lat_ozone=',minval(lat_ozone)



!   do k = 1, levsiz
!      write(0,102) iCell,i1,i2,lat_ozone(i1),lat,lat_ozone(i2),ozmixin(1,k,i1,1), &
!                   ozmixm(1,k,iCell),ozmixin(1,k,i2,1)
!   enddo

! write(0,*) '--- end subroutine physics_init_o3.'

!formats:


!==================================================================================================
!==================================================================================================

!input arguments:

!inout arguments:

!local pointers:

!local variables:

!--------------------------------------------------------------------------------------------------
! write(0,*)
! write(0,*) '--- enter subroutine physics_update_o3:'




! write(0,*) '--- end subroutine physics_update_o3:'


!==================================================================================================
!==================================================================================================

!input arguments:

!inout arguments:

!local pointers:

!local variables:



!--------------------------------------------------------------------------------------------------

!write(0,*)
!write(0,*) '--- enter subroutine o3climatology_from_MPAS:'

 


!julian starts from 0.0 at 0Z on 1 Jan.

!jan 1st 00z is julian=1.0 here
!note that following will drift. need to use actual month/day info to compute julian.





 
!write(0,*) '--- end subroutine getfactors:'
!write(0,*) 'fact1 =', fact1
!write(0,*) 'fact2 =', fact2 

!Time interpolation.

!write(0,101) k,o3clim(k,1),ozmixm(nm,k,1),ozmixm(np,k,1), &
!             o3clim(k,nCellsSolve),ozmixm(nm,k,nCellsSolve),ozmixm(np,k,nCellsSolve)

!write(0,*) '--- end subroutine o3climatology_from_MPAS'


!==================================================================================================
 subroutine vinterp_ozn (lchnk, ncol, pcols, pver, pmid_in, pin_in, levsiz, ozmix_in, o3vmr_out)
!--------------------------------------------------------------------------------------------------
! 
! Purpose: Interpolate ozone from current time-interpolated values to model levels
! 
! Method: Use pressure values to determine interpolation levels
! 
! Author: Bruce Briegleb
! 
!--------------------------------------------------------------------------------------------------
!  use shr_kind_mod, only: r8 => shr_kind_r8
!  use ppgrid
!  use phys_grid,     only: get_lat_all_p, get_lon_all_p
!  use comozp
!  use abortutils, only: endrun
!--------------------------------------------------------------------------
   implicit none
!--------------------------------------------------------------------------
!
! Arguments
!
   integer, intent(in) :: lchnk               ! chunk identifier
   integer, intent(in) :: pcols, pver
   integer, intent(in) :: ncol                ! number of atmospheric columns
   integer, intent(in) :: levsiz              ! number of ozone layers

!ldf begin:
!  real(kind=RKIND), intent(in) :: pmid(pcols,pver)   ! level pressures (mks)
!  real(kind=RKIND), intent(in) :: pin(levsiz)        ! ozone data level pressures (mks)
!  real(kind=RKIND), intent(in) :: ozmix(pcols,levsiz)! ozone mixing ratio

!  real(kind=RKIND), intent(out) :: o3vmr(pcols,pver) ! ozone volume mixing ratio

   real(kind=RKIND), intent(in) :: pmid_in(pcols,pver)   ! level pressures (mks)
   real(kind=RKIND), intent(in) :: pin_in(levsiz)        ! ozone data level pressures (mks)
   real(kind=RKIND), intent(in) :: ozmix_in(pcols,levsiz)! ozone mixing ratio

   real(kind=RKIND), intent(out) :: o3vmr_out(pcols,pver)! ozone volume mixing ratio
!ldf end.
!
! local storage
!
   integer i                   ! longitude index
   integer k, kk, kkstart      ! level indices
   integer kupper(pcols)       ! Level indices for interpolation
   integer kount               ! Counter
   integer lats(pcols)         ! latitude indices
   integer lons(pcols)         ! latitude indices

   real(kind=RKIND) dpu        ! upper level pressure difference
   real(kind=RKIND) dpl        ! lower level pressure difference

!ldf begin:
   real(kind=RKIND):: pmid(pcols,pver)                ! level pressures (mks)
   real(kind=RKIND):: pin(levsiz)                     ! ozone data level pressures (mks)
   real(kind=RKIND):: ozmix(pcols,levsiz)             ! ozone mixing ratio
   real(kind=RKIND):: o3vmr(pcols,pver) ! ozone volume mixing ratio
!
! Initialize latitude indices
!
!  call get_lat_all_p(lchnk, ncol, lats)
!  call get_lon_all_p(lchnk, ncol, lons)
!

!ldf begin:
   do k = 1,levsiz
      pin(k) = pin_in(k)
   enddo
   do i = 1,pcols
      do k = 1,levsiz
         ozmix(i,k) = ozmix_in(i,k)
      enddo
   enddo
   do i = 1,pcols
      do k = 1,pver
         kk = pver-k+1
         pmid(i,kk) = pmid_in(i,k)
      enddo
   enddo
!ldf end.

! Initialize index array
!
   do i=1,ncol
      kupper(i) = 1
   end do

   do k=1,pver
!
! Top level we need to start looking is the top level for the previous k
! for all longitude points
!
      kkstart = levsiz
      do i=1,ncol
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0
!
! Store level indices for interpolation
!
      do kk=kkstart,levsiz-1
         do i=1,ncol
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do
!
! If all indices for this level have been found, do the interpolation and
! go to the next level
!
         if (kount.eq.ncol) then
            do i=1,ncol
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               o3vmr(i,k) = (ozmix(i,kupper(i))*dpl + &
                             ozmix(i,kupper(i)+1)*dpu)/(dpl + dpu)
            end do
            goto 35
         end if
      end do
!
! If we've fallen through the kk=1,levsiz-1 loop, we cannot interpolate and
! must extrapolate from the bottom or top ozone data level for at least some
! of the longitude points.
!
      do i=1,ncol
         if (pmid(i,k) .lt. pin(1)) then
            o3vmr(i,k) = ozmix(i,1)*pmid(i,k)/pin(1)
         else if (pmid(i,k) .gt. pin(levsiz)) then
            o3vmr(i,k) = ozmix(i,levsiz)
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            o3vmr(i,k) = (ozmix(i,kupper(i))*dpl + &
                          ozmix(i,kupper(i)+1)*dpu)/(dpl + dpu)
         end if
      end do

      if (kount.gt.ncol) then
!        call endrun ('VINTERP_OZN: Bad ozone data: non-monotonicity suspected')
      end if
35    continue
   end do

!ldf begin:
   do i = 1,pcols
   do k = 1,pver
      kk = pver-k+1
      o3vmr_out(i,kk) = o3vmr(i,k)
   enddo
   enddo
!ldf end.

   return
end subroutine vinterp_ozn

!==================================================================================================
 end module mpas_atmphys_o3climatology
!==================================================================================================