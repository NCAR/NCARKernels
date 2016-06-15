module camsrfexch
!-----------------------------------------------------------------------
!
! Module to handle data that is exchanged between the 1 atmosphere
! model and the surface models (land, sea-ice, and ocean).
!
!-----------------------------------------------------------------------
!
! USES:
!
  use shr_kind_mod,  only: r8 => shr_kind_r8, r4 => shr_kind_r4
  use constituents,  only: pcnst
  use ppgrid,        only: pcols, begchunk, endchunk
  use phys_grid,     only: get_ncols_p, phys_grid_initialized
  use infnan,        only: posinf, assignment(=)
  use cam_abortutils,only: endrun
  use cam_logfile,   only: iulog

  implicit none

!----------------------------------------------------------------------- 
! PRIVATE: Make default data and interfaces private
!----------------------------------------------------------------------- 
  private     ! By default all data is private to this module
!
! Public interfaces
!
  public atm2hub_alloc              ! Atmosphere to surface data allocation method
  public hub2atm_alloc              ! Merged hub surface to atmosphere data allocation method
  public atm2hub_deallocate
  public hub2atm_deallocate
  public cam_export
!
! Public data types
!
  public cam_out_t                  ! Data from atmosphere
  public cam_in_t                   ! Merged surface data

!---------------------------------------------------------------------------
! This is the data that is sent from the atmosphere to the surface models
!---------------------------------------------------------------------------

  type cam_out_t 
     integer  :: lchnk               ! chunk index
     integer  :: ncol                ! number of columns in chunk
     real(r8) :: tbot(pcols)         ! bot level temperature
     real(r8) :: zbot(pcols)         ! bot level height above surface
     real(r8) :: ubot(pcols)         ! bot level u wind
     real(r8) :: vbot(pcols)         ! bot level v wind
     real(r8) :: qbot(pcols,pcnst)   ! bot level specific humidity
     real(r8) :: pbot(pcols)         ! bot level pressure
     real(r8) :: rho(pcols)          ! bot level density	
     real(r8) :: netsw(pcols)        !	
     real(r8) :: flwds(pcols)        ! 
     real(r8) :: precsc(pcols)       !
     real(r8) :: precsl(pcols)       !
     real(r8) :: precc(pcols)        ! 
     real(r8) :: precl(pcols)        ! 
     real(r8) :: soll(pcols)         ! 
     real(r8) :: sols(pcols)         ! 
     real(r8) :: solld(pcols)        !
     real(r8) :: solsd(pcols)        !
     real(r8) :: thbot(pcols)        ! 
     real(r8) :: co2prog(pcols)      ! prognostic co2
     real(r8) :: co2diag(pcols)      ! diagnostic co2
     real(r8) :: psl(pcols)
     real(r8) :: bcphiwet(pcols)     ! wet deposition of hydrophilic black carbon
     real(r8) :: bcphidry(pcols)     ! dry deposition of hydrophilic black carbon
     real(r8) :: bcphodry(pcols)     ! dry deposition of hydrophobic black carbon
     real(r8) :: ocphiwet(pcols)     ! wet deposition of hydrophilic organic carbon
     real(r8) :: ocphidry(pcols)     ! dry deposition of hydrophilic organic carbon
     real(r8) :: ocphodry(pcols)     ! dry deposition of hydrophobic organic carbon
     real(r8) :: dstwet1(pcols)      ! wet deposition of dust (bin1)
     real(r8) :: dstdry1(pcols)      ! dry deposition of dust (bin1)
     real(r8) :: dstwet2(pcols)      ! wet deposition of dust (bin2)
     real(r8) :: dstdry2(pcols)      ! dry deposition of dust (bin2)
     real(r8) :: dstwet3(pcols)      ! wet deposition of dust (bin3)
     real(r8) :: dstdry3(pcols)      ! dry deposition of dust (bin3)
     real(r8) :: dstwet4(pcols)      ! wet deposition of dust (bin4)
     real(r8) :: dstdry4(pcols)      ! dry deposition of dust (bin4)
  end type cam_out_t 

!---------------------------------------------------------------------------
! This is the merged state of sea-ice, land and ocean surface parameterizations
!---------------------------------------------------------------------------

  type cam_in_t    
     integer  :: lchnk                   ! chunk index
     integer  :: ncol                    ! number of active columns
     real(r8) :: asdir(pcols)            ! albedo: shortwave, direct
     real(r8) :: asdif(pcols)            ! albedo: shortwave, diffuse
     real(r8) :: aldir(pcols)            ! albedo: longwave, direct
     real(r8) :: aldif(pcols)            ! albedo: longwave, diffuse
     real(r8) :: lwup(pcols)             ! longwave up radiative flux
     real(r8) :: lhf(pcols)              ! latent heat flux
     real(r8) :: shf(pcols)              ! sensible heat flux
     real(r8) :: wsx(pcols)              ! surface u-stress (N)
     real(r8) :: wsy(pcols)              ! surface v-stress (N)
     real(r8) :: tref(pcols)             ! ref height surface air temp
     real(r8) :: qref(pcols)             ! ref height specific humidity 
     real(r8) :: u10(pcols)              ! 10m wind speed
     real(r8) :: ts(pcols)               ! merged surface temp 
     real(r8) :: sst(pcols)              ! sea surface temp
     real(r8) :: snowhland(pcols)        ! snow depth (liquid water equivalent) over land 
     real(r8) :: snowhice(pcols)         ! snow depth over ice
     real(r8) :: fco2_lnd(pcols)         ! co2 flux from lnd
     real(r8) :: fco2_ocn(pcols)         ! co2 flux from ocn
     real(r8) :: fdms(pcols)             ! dms flux
     real(r8) :: landfrac(pcols)         ! land area fraction
     real(r8) :: icefrac(pcols)          ! sea-ice areal fraction
     real(r8) :: ocnfrac(pcols)          ! ocean areal fraction
     real(r8), pointer, dimension(:) :: ram1  !aerodynamical resistance (s/m) (pcols)
     real(r8), pointer, dimension(:) :: fv    !friction velocity (m/s) (pcols)
     real(r8), pointer, dimension(:) :: soilw !volumetric soil water (m3/m3)
     real(r8) :: cflx(pcols,pcnst)       ! constituent flux (emissions)
     real(r8) :: ustar(pcols)            ! atm/ocn saved version of ustar
     real(r8) :: re(pcols)               ! atm/ocn saved version of re
     real(r8) :: ssq(pcols)              ! atm/ocn saved version of ssq
     real(r8), pointer, dimension(:,:) :: depvel ! deposition velocities
     real(r8), pointer, dimension(:,:) :: dstflx ! dust fluxes
     real(r8), pointer, dimension(:,:) :: meganflx ! MEGAN fluxes
     real(r8), pointer, dimension(:,:) :: fireflx ! wild fire emissions
     real(r8), pointer, dimension(:)   :: fireztop ! wild fire emissions vert distribution top
  end type cam_in_t    

!===============================================================================
  PUBLIC kw_camsrfexch_cam_in_t 
CONTAINS
!===============================================================================

!----------------------------------------------------------------------- 
! 
! BOP
!
! !IROUTINE: hub2atm_alloc
!
! !DESCRIPTION:
!
!   Allocate space for the surface to atmosphere data type. And initialize
!   the values.
! 
!-----------------------------------------------------------------------
!
! !INTERFACE
!
  subroutine hub2atm_alloc( cam_in )
    use seq_drydep_mod,  only: lnd_drydep, n_drydep
    use cam_cpl_indices, only: index_x2a_Sl_ram1, index_x2a_Sl_fv, index_x2a_Sl_soilw, index_x2a_Fall_flxdst1
    use cam_cpl_indices, only: index_x2a_Fall_flxvoc
    use shr_megan_mod,   only: shr_megan_mechcomps_n
    use cam_cpl_indices, only: index_x2a_Fall_flxfire
    use shr_fire_emis_mod,only: shr_fire_emis_mechcomps_n
!
!!ARGUMENTS:
!
   type(cam_in_t), pointer ::  cam_in(:)     ! Merged surface state
!
!!LOCAL VARIABLES:
!
    integer :: c        ! chunk index
    integer :: ierror   ! Error code
!----------------------------------------------------------------------- 
! 
! EOP
!
    if ( .not. phys_grid_initialized() ) call endrun( "HUB2ATM_ALLOC error: phys_grid not called yet" )
    allocate (cam_in(begchunk:endchunk), stat=ierror)
    if ( ierror /= 0 )then
      write(iulog,*) 'Allocation error: ', ierror
      call endrun('HUB2ATM_ALLOC error: allocation error')
    end if

    do c = begchunk,endchunk
       nullify(cam_in(c)%ram1)
       nullify(cam_in(c)%fv)
       nullify(cam_in(c)%soilw)
       nullify(cam_in(c)%depvel)
       nullify(cam_in(c)%dstflx)
       nullify(cam_in(c)%meganflx)
       nullify(cam_in(c)%fireflx)
       nullify(cam_in(c)%fireztop)
    enddo  
    do c = begchunk,endchunk 
       if (index_x2a_Sl_ram1>0) then
          allocate (cam_in(c)%ram1(pcols), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error ram1')
       endif
       if (index_x2a_Sl_fv>0) then
          allocate (cam_in(c)%fv(pcols), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error fv')
       endif
       if (index_x2a_Sl_soilw /= 0) then
          allocate (cam_in(c)%soilw(pcols), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error soilw')
       end if
       if (index_x2a_Fall_flxdst1>0) then
          ! Assume 4 bins from surface model ....
          allocate (cam_in(c)%dstflx(pcols,4), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error dstflx')
       endif
       if ( index_x2a_Fall_flxvoc>0 .and. shr_megan_mechcomps_n>0 ) then
          allocate (cam_in(c)%meganflx(pcols,shr_megan_mechcomps_n), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error meganflx')
       endif
    end do

    if (lnd_drydep .and. n_drydep>0) then
       do c = begchunk,endchunk 
          allocate (cam_in(c)%depvel(pcols,n_drydep), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error depvel')
       end do
    endif

    if ( index_x2a_Fall_flxfire>0 .and. shr_fire_emis_mechcomps_n>0 ) then
       do c = begchunk,endchunk 
          allocate(cam_in(c)%fireflx(pcols,shr_fire_emis_mechcomps_n), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error fireflx')
          allocate(cam_in(c)%fireztop(pcols), stat=ierror)
          if ( ierror /= 0 ) call endrun('HUB2ATM_ALLOC error: allocation error fireztop')
       enddo
    endif

    do c = begchunk,endchunk
       cam_in(c)%lchnk = c
       cam_in(c)%ncol  = get_ncols_p(c)
       cam_in(c)%asdir    (:) = 0._r8
       cam_in(c)%asdif    (:) = 0._r8
       cam_in(c)%aldir    (:) = 0._r8
       cam_in(c)%aldif    (:) = 0._r8
       cam_in(c)%lwup     (:) = 0._r8
       cam_in(c)%lhf      (:) = 0._r8
       cam_in(c)%shf      (:) = 0._r8
       cam_in(c)%wsx      (:) = 0._r8
       cam_in(c)%wsy      (:) = 0._r8
       cam_in(c)%tref     (:) = 0._r8
       cam_in(c)%qref     (:) = 0._r8
       cam_in(c)%u10      (:) = 0._r8
       cam_in(c)%ts       (:) = 0._r8
       cam_in(c)%sst      (:) = 0._r8
       cam_in(c)%snowhland(:) = 0._r8
       cam_in(c)%snowhice (:) = 0._r8
       cam_in(c)%fco2_lnd (:) = 0._r8
       cam_in(c)%fco2_ocn (:) = 0._r8
       cam_in(c)%fdms     (:) = 0._r8
       cam_in(c)%landfrac (:) = posinf
       cam_in(c)%icefrac  (:) = posinf
       cam_in(c)%ocnfrac  (:) = posinf

       if (associated(cam_in(c)%ram1)) &
            cam_in(c)%ram1  (:) = 0.1_r8
       if (associated(cam_in(c)%fv)) &
            cam_in(c)%fv    (:) = 0.1_r8
       if (associated(cam_in(c)%soilw)) &
            cam_in(c)%soilw (:) = 0.0_r8
       if (associated(cam_in(c)%dstflx)) &
            cam_in(c)%dstflx(:,:) = 0.0_r8
       if (associated(cam_in(c)%meganflx)) &
            cam_in(c)%meganflx(:,:) = 0.0_r8

       cam_in(c)%cflx   (:,:) = 0._r8
       cam_in(c)%ustar    (:) = 0._r8
       cam_in(c)%re       (:) = 0._r8
       cam_in(c)%ssq      (:) = 0._r8
       if (lnd_drydep .and. n_drydep>0) then
          cam_in(c)%depvel (:,:) = 0._r8
       endif
       if ( index_x2a_Fall_flxfire>0 .and. shr_fire_emis_mechcomps_n>0 ) then
          cam_in(c)%fireflx(:,:) = 0._r8
          cam_in(c)%fireztop(:) = 0._r8
       endif
    end do

  end subroutine hub2atm_alloc

!
!===============================================================================
!

!----------------------------------------------------------------------- 
! 
! BOP
!
! !IROUTINE: atm2hub_alloc
!
! !DESCRIPTION:
!
!   Allocate space for the atmosphere to surface data type. And initialize
!   the values.
! 
!-----------------------------------------------------------------------
!
! !INTERFACE
!
  subroutine atm2hub_alloc( cam_out )
!
!!USES:
!
!
!!ARGUMENTS:
!
   type(cam_out_t), pointer :: cam_out(:)    ! Atmosphere to surface input
!
!!LOCAL VARIABLES:
!
    integer :: c            ! chunk index
    integer :: ierror       ! Error code
    !----------------------------------------------------------------------- 

    if ( .not. phys_grid_initialized() ) call endrun( "ATM2HUB_ALLOC error: phys_grid not called yet" )
    allocate (cam_out(begchunk:endchunk), stat=ierror)
    if ( ierror /= 0 )then
      write(iulog,*) 'Allocation error: ', ierror
      call endrun('ATM2HUB_ALLOC error: allocation error')
    end if

    do c = begchunk,endchunk
       cam_out(c)%lchnk       = c
       cam_out(c)%ncol        = get_ncols_p(c)
       cam_out(c)%tbot(:)     = 0._r8
       cam_out(c)%zbot(:)     = 0._r8
       cam_out(c)%ubot(:)     = 0._r8
       cam_out(c)%vbot(:)     = 0._r8
       cam_out(c)%qbot(:,:)   = 0._r8
       cam_out(c)%pbot(:)     = 0._r8
       cam_out(c)%rho(:)      = 0._r8
       cam_out(c)%netsw(:)    = 0._r8
       cam_out(c)%flwds(:)    = 0._r8
       cam_out(c)%precsc(:)   = 0._r8
       cam_out(c)%precsl(:)   = 0._r8
       cam_out(c)%precc(:)    = 0._r8
       cam_out(c)%precl(:)    = 0._r8
       cam_out(c)%soll(:)     = 0._r8
       cam_out(c)%sols(:)     = 0._r8
       cam_out(c)%solld(:)    = 0._r8
       cam_out(c)%solsd(:)    = 0._r8
       cam_out(c)%thbot(:)    = 0._r8
       cam_out(c)%co2prog(:)  = 0._r8
       cam_out(c)%co2diag(:)  = 0._r8
       cam_out(c)%psl(:)      = 0._r8
       cam_out(c)%bcphidry(:) = 0._r8
       cam_out(c)%bcphodry(:) = 0._r8
       cam_out(c)%bcphiwet(:) = 0._r8
       cam_out(c)%ocphidry(:) = 0._r8
       cam_out(c)%ocphodry(:) = 0._r8
       cam_out(c)%ocphiwet(:) = 0._r8
       cam_out(c)%dstdry1(:)  = 0._r8
       cam_out(c)%dstwet1(:)  = 0._r8
       cam_out(c)%dstdry2(:)  = 0._r8
       cam_out(c)%dstwet2(:)  = 0._r8
       cam_out(c)%dstdry3(:)  = 0._r8
       cam_out(c)%dstwet3(:)  = 0._r8
       cam_out(c)%dstdry4(:)  = 0._r8
       cam_out(c)%dstwet4(:)  = 0._r8
    end do

  end subroutine atm2hub_alloc

  subroutine atm2hub_deallocate(cam_out)
    type(cam_out_t), pointer :: cam_out(:)    ! Atmosphere to surface input
    if(associated(cam_out)) then
       deallocate(cam_out)
    end if
    nullify(cam_out)

  end subroutine atm2hub_deallocate
  subroutine hub2atm_deallocate(cam_in)
    type(cam_in_t), pointer :: cam_in(:)    ! Atmosphere to surface input
    integer :: c

    if(associated(cam_in)) then
       do c=begchunk,endchunk
          if(associated(cam_in(c)%ram1)) then
             deallocate(cam_in(c)%ram1)
             nullify(cam_in(c)%ram1)
          end if
          if(associated(cam_in(c)%fv)) then
             deallocate(cam_in(c)%fv)
             nullify(cam_in(c)%fv)
          end if
          if(associated(cam_in(c)%soilw)) then
             deallocate(cam_in(c)%soilw)
             nullify(cam_in(c)%soilw)
          end if
          if(associated(cam_in(c)%dstflx)) then
             deallocate(cam_in(c)%dstflx)
             nullify(cam_in(c)%dstflx)
          end if
          if(associated(cam_in(c)%meganflx)) then
             deallocate(cam_in(c)%meganflx)
             nullify(cam_in(c)%meganflx)
          end if
          if(associated(cam_in(c)%depvel)) then
             deallocate(cam_in(c)%depvel)
             nullify(cam_in(c)%depvel)
          end if
          
       enddo

       deallocate(cam_in)
    end if
    nullify(cam_in)

  end subroutine hub2atm_deallocate


!======================================================================

subroutine cam_export(state,cam_out,pbuf)

!----------------------------------------------------------------------- 
! 
! Purpose: 
! Transfer atmospheric fields into necessary surface data structures
! 
! Author: L. Bath  CMS Contact: M. Vertenstein
! 
!-----------------------------------------------------------------------
   use physics_types,    only: physics_state
   use ppgrid,           only: pver
   use cam_history,      only: outfld
   use comsrf,           only: psm1, srfrpdel, prcsnw
   use chem_surfvals,    only: chem_surfvals_get
   use co2_cycle,        only: co2_transport, c_i
   use physconst,        only: mwdry, mwco2
   use constituents,     only: pcnst
   use cam_control_mod,  only: rair
   use physics_buffer,   only: pbuf_get_index, pbuf_get_field, physics_buffer_desc
   implicit none

   !------------------------------Arguments--------------------------------
   !
   ! Input arguments
   !
   type(physics_state),  intent(in)    :: state
   type (cam_out_t),     intent(inout) :: cam_out
   type(physics_buffer_desc), pointer  :: pbuf(:)

   !
   !---------------------------Local variables-----------------------------
   !
   integer :: i              ! Longitude index
   integer :: m              ! constituent index
   integer :: lchnk          ! Chunk index
   integer :: ncol
   integer :: prec_dp_idx, snow_dp_idx, prec_sh_idx, snow_sh_idx
   integer :: prec_sed_idx,snow_sed_idx,prec_pcw_idx,snow_pcw_idx

   real(r8), pointer :: prec_dp(:)                 ! total precipitation   from ZM convection
   real(r8), pointer :: snow_dp(:)                 ! snow from ZM   convection
   real(r8), pointer :: prec_sh(:)                 ! total precipitation   from Hack convection
   real(r8), pointer :: snow_sh(:)                 ! snow from   Hack   convection
   real(r8), pointer :: prec_sed(:)                ! total precipitation   from ZM convection
   real(r8), pointer :: snow_sed(:)                ! snow from ZM   convection
   real(r8), pointer :: prec_pcw(:)                ! total precipitation   from Hack convection
   real(r8), pointer :: snow_pcw(:)                ! snow from Hack   convection
   !-----------------------------------------------------------------------

   lchnk = state%lchnk
   ncol  = state%ncol

   prec_dp_idx = pbuf_get_index('PREC_DP')
   snow_dp_idx = pbuf_get_index('SNOW_DP')
   prec_sh_idx = pbuf_get_index('PREC_SH')
   snow_sh_idx = pbuf_get_index('SNOW_SH')
   prec_sed_idx = pbuf_get_index('PREC_SED')
   snow_sed_idx = pbuf_get_index('SNOW_SED')
   prec_pcw_idx = pbuf_get_index('PREC_PCW')
   snow_pcw_idx = pbuf_get_index('SNOW_PCW')

   call pbuf_get_field(pbuf, prec_dp_idx, prec_dp)
   call pbuf_get_field(pbuf, snow_dp_idx, snow_dp)
   call pbuf_get_field(pbuf, prec_sh_idx, prec_sh)
   call pbuf_get_field(pbuf, snow_sh_idx, snow_sh)
   call pbuf_get_field(pbuf, prec_sed_idx, prec_sed)
   call pbuf_get_field(pbuf, snow_sed_idx, snow_sed)
   call pbuf_get_field(pbuf, prec_pcw_idx, prec_pcw)
   call pbuf_get_field(pbuf, snow_pcw_idx, snow_pcw)

   do i=1,ncol
      cam_out%tbot(i)  = state%t(i,pver)
      cam_out%thbot(i) = state%t(i,pver) * state%exner(i,pver)
      cam_out%zbot(i)  = state%zm(i,pver)
      cam_out%ubot(i)  = state%u(i,pver)
      cam_out%vbot(i)  = state%v(i,pver)
      cam_out%pbot(i)  = state%pmid(i,pver)
      cam_out%rho(i)   = cam_out%pbot(i)/(rair*cam_out%tbot(i))
      psm1(i,lchnk)    = state%ps(i)
      srfrpdel(i,lchnk)= state%rpdel(i,pver)
   end do
   do m = 1, pcnst
     do i = 1, ncol
        cam_out%qbot(i,m) = state%q(i,pver,m) 
     end do
   end do

   cam_out%co2diag(:ncol) = chem_surfvals_get('CO2VMR') * 1.0e+6_r8 
   if (co2_transport()) then
      do i=1,ncol
         cam_out%co2prog(i) = state%q(i,pver,c_i(4)) * 1.0e+6_r8 *mwdry/mwco2
      end do
   end if
   !
   ! Precipation and snow rates from shallow convection, deep convection and stratiform processes.
   ! Compute total convective and stratiform precipitation and snow rates
   !
   do i=1,ncol
      cam_out%precc (i) = prec_dp(i)  + prec_sh(i)
      cam_out%precl (i) = prec_sed(i) + prec_pcw(i)
      cam_out%precsc(i) = snow_dp(i)  + snow_sh(i)
      cam_out%precsl(i) = snow_sed(i) + snow_pcw(i)

      ! jrm These checks should not be necessary if they exist in the parameterizations
      if (cam_out%precc(i) .lt.0._r8) cam_out%precc(i)=0._r8
      if (cam_out%precl(i) .lt.0._r8) cam_out%precl(i)=0._r8
      if (cam_out%precsc(i).lt.0._r8) cam_out%precsc(i)=0._r8
      if (cam_out%precsl(i).lt.0._r8) cam_out%precsl(i)=0._r8
      if (cam_out%precsc(i).gt.cam_out%precc(i)) cam_out%precsc(i)=cam_out%precc(i)
      if (cam_out%precsl(i).gt.cam_out%precl(i)) cam_out%precsl(i)=cam_out%precl(i)
      ! end jrm
   end do

   ! total snowfall rate: needed by slab ocean model
   prcsnw(:ncol,lchnk) = cam_out%precsc(:ncol) + cam_out%precsl(:ncol)   

end subroutine cam_export

!read state subroutine for kw_camsrfexch_cam_in_t 
RECURSIVE SUBROUTINE kw_camsrfexch_cam_in_t(var, kgen_unit, printvar) 
    TYPE(cam_in_t), INTENT(IN) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    kgen_istrue = .TRUE. 
    WRITE (UNIT = kgen_unit) var%lchnk 
    IF (PRESENT( printvar )) THEN 
        WRITE (*, *) "** KGEN DEBUG: " // printvar // "%lchnk **" // NEW_LINE("A"), var%lchnk 
    END IF   
      
    kgen_istrue = .TRUE. 
    WRITE (UNIT = kgen_unit) var%ncol 
    IF (PRESENT( printvar )) THEN 
        WRITE (*, *) "** KGEN DEBUG: " // printvar // "%ncol **" // NEW_LINE("A"), var%ncol 
    END IF   
      
    IF (SIZE(var%asdir)==1) THEN 
        IF (UBOUND(var%asdir, 1)<LBOUND(var%asdir, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%asdir, 1)==0 .AND. LBOUND(var%asdir, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%asdir)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%asdir, mask=(var%asdir .eq. var%asdir)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%asdir 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%asdir), 8) **", REAL(SUM(var%asdir), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%asdif)==1) THEN 
        IF (UBOUND(var%asdif, 1)<LBOUND(var%asdif, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%asdif, 1)==0 .AND. LBOUND(var%asdif, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%asdif)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%asdif, mask=(var%asdif .eq. var%asdif)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%asdif 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%asdif), 8) **", REAL(SUM(var%asdif), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%aldir)==1) THEN 
        IF (UBOUND(var%aldir, 1)<LBOUND(var%aldir, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%aldir, 1)==0 .AND. LBOUND(var%aldir, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%aldir)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%aldir, mask=(var%aldir .eq. var%aldir)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%aldir 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%aldir), 8) **", REAL(SUM(var%aldir), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%aldif)==1) THEN 
        IF (UBOUND(var%aldif, 1)<LBOUND(var%aldif, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%aldif, 1)==0 .AND. LBOUND(var%aldif, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%aldif)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%aldif, mask=(var%aldif .eq. var%aldif)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%aldif 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%aldif), 8) **", REAL(SUM(var%aldif), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%lwup)==1) THEN 
        IF (UBOUND(var%lwup, 1)<LBOUND(var%lwup, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%lwup, 1)==0 .AND. LBOUND(var%lwup, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%lwup)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%lwup, mask=(var%lwup .eq. var%lwup)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%lwup 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%lwup), 8) **", REAL(SUM(var%lwup), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%lhf)==1) THEN 
        IF (UBOUND(var%lhf, 1)<LBOUND(var%lhf, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%lhf, 1)==0 .AND. LBOUND(var%lhf, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%lhf)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%lhf, mask=(var%lhf .eq. var%lhf)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%lhf 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%lhf), 8) **", REAL(SUM(var%lhf), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%shf)==1) THEN 
        IF (UBOUND(var%shf, 1)<LBOUND(var%shf, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%shf, 1)==0 .AND. LBOUND(var%shf, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%shf)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%shf, mask=(var%shf .eq. var%shf)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%shf 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%shf), 8) **", REAL(SUM(var%shf), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%wsx)==1) THEN 
        IF (UBOUND(var%wsx, 1)<LBOUND(var%wsx, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%wsx, 1)==0 .AND. LBOUND(var%wsx, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%wsx)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%wsx, mask=(var%wsx .eq. var%wsx)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%wsx 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%wsx), 8) **", REAL(SUM(var%wsx), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%wsy)==1) THEN 
        IF (UBOUND(var%wsy, 1)<LBOUND(var%wsy, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%wsy, 1)==0 .AND. LBOUND(var%wsy, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%wsy)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%wsy, mask=(var%wsy .eq. var%wsy)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%wsy 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%wsy), 8) **", REAL(SUM(var%wsy), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%tref)==1) THEN 
        IF (UBOUND(var%tref, 1)<LBOUND(var%tref, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%tref, 1)==0 .AND. LBOUND(var%tref, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%tref)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%tref, mask=(var%tref .eq. var%tref)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%tref 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%tref), 8) **", REAL(SUM(var%tref), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%qref)==1) THEN 
        IF (UBOUND(var%qref, 1)<LBOUND(var%qref, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%qref, 1)==0 .AND. LBOUND(var%qref, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%qref)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%qref, mask=(var%qref .eq. var%qref)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%qref 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%qref), 8) **", REAL(SUM(var%qref), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%u10)==1) THEN 
        IF (UBOUND(var%u10, 1)<LBOUND(var%u10, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%u10, 1)==0 .AND. LBOUND(var%u10, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%u10)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%u10, mask=(var%u10 .eq. var%u10)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%u10 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%u10), 8) **", REAL(SUM(var%u10), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%ts)==1) THEN 
        IF (UBOUND(var%ts, 1)<LBOUND(var%ts, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%ts, 1)==0 .AND. LBOUND(var%ts, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%ts)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%ts, mask=(var%ts .eq. var%ts)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%ts 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%ts), 8) **", REAL(SUM(var%ts), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%sst)==1) THEN 
        IF (UBOUND(var%sst, 1)<LBOUND(var%sst, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%sst, 1)==0 .AND. LBOUND(var%sst, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%sst)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%sst, mask=(var%sst .eq. var%sst)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%sst 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%sst), 8) **", REAL(SUM(var%sst), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%snowhland)==1) THEN 
        IF (UBOUND(var%snowhland, 1)<LBOUND(var%snowhland, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%snowhland, 1)==0 .AND. LBOUND(var%snowhland, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%snowhland)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%snowhland, mask=(var%snowhland .eq. var%snowhland)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%snowhland 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%snowhland), 8) **", REAL(SUM(var%snowhland), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%snowhice)==1) THEN 
        IF (UBOUND(var%snowhice, 1)<LBOUND(var%snowhice, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%snowhice, 1)==0 .AND. LBOUND(var%snowhice, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%snowhice)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%snowhice, mask=(var%snowhice .eq. var%snowhice)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%snowhice 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%snowhice), 8) **", REAL(SUM(var%snowhice), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%fco2_lnd)==1) THEN 
        IF (UBOUND(var%fco2_lnd, 1)<LBOUND(var%fco2_lnd, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%fco2_lnd, 1)==0 .AND. LBOUND(var%fco2_lnd, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%fco2_lnd)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%fco2_lnd, mask=(var%fco2_lnd .eq. var%fco2_lnd)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%fco2_lnd 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%fco2_lnd), 8) **", REAL(SUM(var%fco2_lnd), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%fco2_ocn)==1) THEN 
        IF (UBOUND(var%fco2_ocn, 1)<LBOUND(var%fco2_ocn, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%fco2_ocn, 1)==0 .AND. LBOUND(var%fco2_ocn, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%fco2_ocn)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%fco2_ocn, mask=(var%fco2_ocn .eq. var%fco2_ocn)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%fco2_ocn 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%fco2_ocn), 8) **", REAL(SUM(var%fco2_ocn), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%fdms)==1) THEN 
        IF (UBOUND(var%fdms, 1)<LBOUND(var%fdms, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%fdms, 1)==0 .AND. LBOUND(var%fdms, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%fdms)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%fdms, mask=(var%fdms .eq. var%fdms)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%fdms 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%fdms), 8) **", REAL(SUM(var%fdms), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%landfrac)==1) THEN 
        IF (UBOUND(var%landfrac, 1)<LBOUND(var%landfrac, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%landfrac, 1)==0 .AND. LBOUND(var%landfrac, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%landfrac)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%landfrac, mask=(var%landfrac .eq. var%landfrac)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%landfrac 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%landfrac), 8) **", REAL(SUM(var%landfrac), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%icefrac)==1) THEN 
        IF (UBOUND(var%icefrac, 1)<LBOUND(var%icefrac, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%icefrac, 1)==0 .AND. LBOUND(var%icefrac, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%icefrac)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%icefrac, mask=(var%icefrac .eq. var%icefrac)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%icefrac 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%icefrac), 8) **", REAL(SUM(var%icefrac), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%ocnfrac)==1) THEN 
        IF (UBOUND(var%ocnfrac, 1)<LBOUND(var%ocnfrac, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%ocnfrac, 1)==0 .AND. LBOUND(var%ocnfrac, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%ocnfrac)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%ocnfrac, mask=(var%ocnfrac .eq. var%ocnfrac)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%ocnfrac 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%ocnfrac), 8) **", REAL(SUM(var%ocnfrac), 8) 
        END IF   
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%ram1, kgen_unit, printvar // "%ram1") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%ram1, kgen_unit) 
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%fv, kgen_unit, printvar // "%fv") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%fv, kgen_unit) 
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%soilw, kgen_unit, printvar // "%soilw") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%soilw, kgen_unit) 
    END IF   
      
    IF (SIZE(var%cflx)==1) THEN 
        IF (UBOUND(var%cflx, 1)<LBOUND(var%cflx, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%cflx, 1)==0 .AND. LBOUND(var%cflx, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%cflx)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%cflx, mask=(var%cflx .eq. var%cflx)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%cflx 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%cflx), 8) **", REAL(SUM(var%cflx), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%ustar)==1) THEN 
        IF (UBOUND(var%ustar, 1)<LBOUND(var%ustar, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%ustar, 1)==0 .AND. LBOUND(var%ustar, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%ustar)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%ustar, mask=(var%ustar .eq. var%ustar)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%ustar 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%ustar), 8) **", REAL(SUM(var%ustar), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%re)==1) THEN 
        IF (UBOUND(var%re, 1)<LBOUND(var%re, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%re, 1)==0 .AND. LBOUND(var%re, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%re)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%re, mask=(var%re .eq. var%re)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%re 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%re), 8) **", REAL(SUM(var%re), 8) 
        END IF   
    END IF   
      
    IF (SIZE(var%ssq)==1) THEN 
        IF (UBOUND(var%ssq, 1)<LBOUND(var%ssq, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var%ssq, 1)==0 .AND. LBOUND(var%ssq, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var%ssq)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var%ssq, mask=(var%ssq .eq. var%ssq)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) var%ssq 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "%ssq), 8) **", REAL(SUM(var%ssq), 8) 
        END IF   
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%depvel, kgen_unit, printvar // "%depvel") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%depvel, kgen_unit) 
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%dstflx, kgen_unit, printvar // "%dstflx") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%dstflx, kgen_unit) 
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%meganflx, kgen_unit, printvar // "%meganflx") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%meganflx, kgen_unit) 
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%fireflx, kgen_unit, printvar // "%fireflx") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim2_ptr(var%fireflx, kgen_unit) 
    END IF   
      
    IF (PRESENT( printvar )) THEN 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%fireztop, kgen_unit, printvar // "%fireztop") 
    ELSE 
        CALL kw_cam_in_t_real__r8_dim1_ptr(var%fireztop, kgen_unit) 
    END IF   
      
END SUBROUTINE kw_camsrfexch_cam_in_t 
  
!write state subroutine for kw_cam_in_t_real__r8_dim1_ptr 
SUBROUTINE kw_cam_in_t_real__r8_dim1_ptr(var, kgen_unit, printvar) 
    REAL(KIND=r8), INTENT(IN), POINTER, DIMENSION(:) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    INTEGER :: idx1 
    REAL(KIND=8) :: kgen_array_sum 
      
    IF (SIZE(var)==1) THEN 
        IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    IF (.NOT. ASSOCIATED(var)) THEN 
        kgen_istrue = .FALSE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var, mask=(var .eq. var)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
        WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
        WRITE (UNIT = kgen_unit) var 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8) 
        END IF   
    END IF   
END SUBROUTINE kw_cam_in_t_real__r8_dim1_ptr 
  
!write state subroutine for kw_cam_in_t_real__r8_dim2_ptr 
SUBROUTINE kw_cam_in_t_real__r8_dim2_ptr(var, kgen_unit, printvar) 
    REAL(KIND=r8), INTENT(IN), POINTER, DIMENSION(:,:) :: var 
    INTEGER, INTENT(IN) :: kgen_unit 
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
    LOGICAL :: kgen_istrue 
    INTEGER :: idx1, idx2 
    REAL(KIND=8) :: kgen_array_sum 
      
    IF (SIZE(var)==1) THEN 
        IF (UBOUND(var, 1)<LBOUND(var, 1)) THEN 
            kgen_istrue = .FALSE. 
        ELSE IF (UBOUND(var, 1)==0 .AND. LBOUND(var, 1)==0) THEN 
            kgen_istrue = .FALSE. 
        ELSE 
            kgen_istrue = .TRUE. 
        END IF   
    ELSE IF (SIZE(var)==0) THEN 
        kgen_istrue = .FALSE. 
    ELSE 
        kgen_istrue = .TRUE. 
    END IF   
    IF (.NOT. ASSOCIATED(var)) THEN 
        kgen_istrue = .FALSE. 
    END IF   
    WRITE (UNIT = kgen_unit) kgen_istrue 
    IF (kgen_istrue) THEN 
        kgen_array_sum = REAL(SUM(var, mask=(var .eq. var)), 8) 
        WRITE (UNIT = kgen_unit) kgen_array_sum 
        WRITE (UNIT = kgen_unit) LBOUND(var, 1) 
        WRITE (UNIT = kgen_unit) UBOUND(var, 1) 
        WRITE (UNIT = kgen_unit) LBOUND(var, 2) 
        WRITE (UNIT = kgen_unit) UBOUND(var, 2) 
        WRITE (UNIT = kgen_unit) var 
        IF (PRESENT( printvar )) THEN 
            WRITE (*, *) "** KGEN DEBUG: REAL(SUM(" // printvar // "), 8) **", REAL(SUM(var), 8) 
        END IF   
    END IF   
END SUBROUTINE kw_cam_in_t_real__r8_dim2_ptr 
  
end module camsrfexch