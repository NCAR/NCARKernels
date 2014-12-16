! used file-locals
! read extern input states used for kernel
! read input state to kernel
! read output state from kernel
! verification

    module micro_mg_cam

! NOTE:
! * only leave required use information
! * only leave required access spec(public, private) information
! * if it is access spec without arguments, leave it always
! * keep implicit, save
! * leave required declarations only

    use shr_kind_mod,   only: r8=>shr_kind_r8
    USE kgen_utils, only : read_var, verify_var, KGENinitcheck, KGENPrtCheck, KGENApplyPert, check_t

    public :: micro_mg_cam_tend, read_extern_micro_mg_cam

    integer :: num_steps ! Number of MG substeps

    contains

    ! read external states required for kernel in this module
    subroutine read_extern_micro_mg_cam(kgen_unit)
    integer, intent(in) :: kgen_unit

    READ(UNIT=kgen_unit) num_steps

    end subroutine read_extern_micro_mg_cam


! NOTE : left dummy arguments that are requied to call kernel
! NOTE: put read statements of intent(in) argument in call-site of this subroutine
! NOTE: generate dummy argument list during resolution
! NOTE: need to have general representation of variable to support derived type. For example, name, type, component used, etc.... here state is used and psetcols component is used.

    subroutine micro_mg_cam_tend( dtime, kgen_unit )
! NOTE:
! * only leave required use information

    use micro_mg2_0, only: micro_mg_tend2_0 => micro_mg_tend

    implicit none 

    real(r8),                    intent(in)    :: dtime
    integer, intent(in) :: kgen_unit
    integer :: omp_get_num_threads

    ! Packed versions of inputs.
    real(r8), allocatable :: packed_t(:,:)
    real(r8), allocatable :: packed_q(:,:)
    real(r8), allocatable :: packed_qc(:,:)
    real(r8), allocatable :: packed_qi(:,:)
    real(r8), allocatable :: packed_nc(:,:)
    real(r8), allocatable :: packed_ni(:,:)
    real(r8), allocatable :: packed_qr(:,:)
    real(r8), allocatable :: packed_qs(:,:)
    real(r8), allocatable :: packed_nr(:,:)
    real(r8), allocatable :: packed_ns(:,:)

    real(r8), allocatable :: packed_relvar(:,:)
    real(r8), allocatable :: packed_accre_enhan(:,:)

    real(r8), allocatable :: packed_p(:,:)
    real(r8), allocatable :: packed_pdel(:,:)

    real(r8), allocatable :: packed_naai(:,:)
    real(r8), allocatable :: packed_npccn(:,:)

    real(r8), allocatable :: packed_rndst(:,:,:)
    real(r8), allocatable :: packed_nacon(:,:,:)

    ! Optional outputs.
    real(r8), pointer :: packed_tnd_qsnow(:,:)
    real(r8), pointer :: packed_tnd_nsnow(:,:)
    real(r8), pointer :: packed_re_ice(:,:)

    real(r8), pointer :: packed_frzimm(:,:)
    real(r8), pointer :: packed_frzcnt(:,:)
    real(r8), pointer :: packed_frzdep(:,:)

    real(r8), allocatable :: packed_cldn(:,:)
    real(r8), allocatable :: packed_liqcldf(:,:)
    real(r8), allocatable :: packed_icecldf(:,:)

   

    ! Packed versions of outputs.
    real(r8), allocatable, target :: packed_rate1ord_cw2pr_st(:,:), ref_packed_rate1ord_cw2pr_st(:,:)
    real(r8), allocatable, target :: packed_tlat(:,:), ref_packed_tlat(:,:)
    real(r8), allocatable, target :: packed_qvlat(:,:), ref_packed_qvlat(:,:)
    real(r8), allocatable, target :: packed_qctend(:,:), ref_packed_qctend(:,:)
    real(r8), allocatable, target :: packed_qitend(:,:), ref_packed_qitend(:,:)
    real(r8), allocatable, target :: packed_nctend(:,:), ref_packed_nctend(:,:)
    real(r8), allocatable, target :: packed_nitend(:,:), ref_packed_nitend(:,:)

    real(r8), allocatable, target :: packed_qrtend(:,:), ref_packed_qrtend(:,:)
    real(r8), allocatable, target :: packed_qstend(:,:), ref_packed_qstend(:,:)
    real(r8), allocatable, target :: packed_nrtend(:,:), ref_packed_nrtend(:,:)
    real(r8), allocatable, target :: packed_nstend(:,:), ref_packed_nstend(:,:)

    real(r8), allocatable, target :: packed_prect(:), ref_packed_prect(:)
    real(r8), allocatable, target :: packed_preci(:), ref_packed_preci(:)
    real(r8), allocatable, target :: packed_nevapr(:,:), ref_packed_nevapr(:,:)
    real(r8), allocatable, target :: packed_evapsnow(:,:), ref_packed_evapsnow(:,:)
    real(r8), allocatable, target :: packed_prain(:,:), ref_packed_prain(:,:)
    real(r8), allocatable, target :: packed_prodsnow(:,:), ref_packed_prodsnow(:,:)
    real(r8), allocatable, target :: packed_cmeout(:,:), ref_packed_cmeout(:,:)
    real(r8), allocatable, target :: packed_qsout(:,:), ref_packed_qsout(:,:)
    real(r8), allocatable, target :: packed_rflx(:,:), ref_packed_rflx(:,:)
    real(r8), allocatable, target :: packed_sflx(:,:), ref_packed_sflx(:,:)
    real(r8), allocatable, target :: packed_qrout(:,:), ref_packed_qrout(:,:)
    real(r8), allocatable, target :: packed_qcsevap(:,:), ref_packed_qcsevap(:,:)
    real(r8), allocatable, target :: packed_qisevap(:,:), ref_packed_qisevap(:,:)
    real(r8), allocatable, target :: packed_qvres(:,:), ref_packed_qvres(:,:)
    real(r8), allocatable, target :: packed_cmei(:,:), ref_packed_cmei(:,:)
    real(r8), allocatable, target :: packed_vtrmc(:,:), ref_packed_vtrmc(:,:)
    real(r8), allocatable, target :: packed_vtrmi(:,:), ref_packed_vtrmi(:,:)
    real(r8), allocatable, target :: packed_umr(:,:), ref_packed_umr(:,:)
    real(r8), allocatable, target :: packed_ums(:,:), ref_packed_ums(:,:)
    real(r8), allocatable, target :: packed_qcsedten(:,:), ref_packed_qcsedten(:,:)
    real(r8), allocatable, target :: packed_qisedten(:,:), ref_packed_qisedten(:,:)
    real(r8), allocatable, target :: packed_pra(:,:), ref_packed_pra(:,:)
    real(r8), allocatable, target :: packed_prc(:,:), ref_packed_prc(:,:)
    real(r8), allocatable, target :: packed_mnuccc(:,:), ref_packed_mnuccc(:,:)
    real(r8), allocatable, target :: packed_mnucct(:,:), ref_packed_mnucct(:,:)
    real(r8), allocatable, target :: packed_msacwi(:,:), ref_packed_msacwi(:,:)
    real(r8), allocatable, target :: packed_psacws(:,:), ref_packed_psacws(:,:)
    real(r8), allocatable, target :: packed_bergs(:,:), ref_packed_bergs(:,:)
    real(r8), allocatable, target :: packed_berg(:,:), ref_packed_berg(:,:)
    real(r8), allocatable, target :: packed_melt(:,:), ref_packed_melt(:,:)
    real(r8), allocatable, target :: packed_homo(:,:), ref_packed_homo(:,:)
    real(r8), allocatable, target :: packed_qcres(:,:), ref_packed_qcres(:,:)
    real(r8), allocatable, target :: packed_prci(:,:), ref_packed_prci(:,:)
    real(r8), allocatable, target :: packed_prai(:,:), ref_packed_prai(:,:)
    real(r8), allocatable, target :: packed_qires(:,:), ref_packed_qires(:,:)
    real(r8), allocatable, target :: packed_mnuccr(:,:), ref_packed_mnuccr(:,:)
    real(r8), allocatable, target :: packed_pracs(:,:), ref_packed_pracs(:,:)
    real(r8), allocatable, target :: packed_meltsdt(:,:), ref_packed_meltsdt(:,:)
    real(r8), allocatable, target :: packed_frzrdt(:,:), ref_packed_frzrdt(:,:)
    real(r8), allocatable, target :: packed_mnuccd(:,:), ref_packed_mnuccd(:,:)
    real(r8), allocatable, target :: packed_nrout(:,:), ref_packed_nrout(:,:)
    real(r8), allocatable, target :: packed_nsout(:,:), ref_packed_nsout(:,:)
    real(r8), allocatable, target :: packed_refl(:,:), ref_packed_refl(:,:)
    real(r8), allocatable, target :: packed_arefl(:,:), ref_packed_arefl(:,:)
    real(r8), allocatable, target :: packed_areflz(:,:), ref_packed_areflz(:,:)
    real(r8), allocatable, target :: packed_frefl(:,:), ref_packed_frefl(:,:)
    real(r8), allocatable, target :: packed_csrfl(:,:), ref_packed_csrfl(:,:)
    real(r8), allocatable, target :: packed_acsrfl(:,:), ref_packed_acsrfl(:,:)
    real(r8), allocatable, target :: packed_fcsrfl(:,:), ref_packed_fcsrfl(:,:)
    real(r8), allocatable, target :: packed_rercld(:,:), ref_packed_rercld(:,:)
    real(r8), allocatable, target :: packed_ncai(:,:), ref_packed_ncai(:,:)
    real(r8), allocatable, target :: packed_ncal(:,:), ref_packed_ncal(:,:)
    real(r8), allocatable, target :: packed_qrout2(:,:), ref_packed_qrout2(:,:)
    real(r8), allocatable, target :: packed_qsout2(:,:), ref_packed_qsout2(:,:)
    real(r8), allocatable, target :: packed_nrout2(:,:), ref_packed_nrout2(:,:)
    real(r8), allocatable, target :: packed_nsout2(:,:), ref_packed_nsout2(:,:)
    real(r8), allocatable, target :: packed_freqs(:,:), ref_packed_freqs(:,:)
    real(r8), allocatable, target :: packed_freqr(:,:), ref_packed_freqr(:,:)
    real(r8), allocatable, target :: packed_nfice(:,:), ref_packed_nfice(:,:)
    real(r8), allocatable, target :: packed_qcrat(:,:), ref_packed_qcrat(:,:)


    real(r8), allocatable, target :: packed_rel(:,:), ref_packed_rel(:,:)
    real(r8), allocatable, target :: packed_rei(:,:), ref_packed_rei(:,:)
    real(r8), allocatable, target :: packed_lambdac(:,:), ref_packed_lambdac(:,:)
    real(r8), allocatable, target :: packed_mu(:,:), ref_packed_mu(:,:)
    real(r8), allocatable, target :: packed_des(:,:), ref_packed_des(:,:)
    real(r8), allocatable, target :: packed_dei(:,:), ref_packed_dei(:,:)


    ! Dummy arrays for cases where we throw away the MG version and
    ! recalculate sizes on the CAM grid to avoid time/subcolumn averaging
    ! issues.
    real(r8), allocatable :: rel_fn_dum(:,:), ref_rel_fn_dum(:,:)
    real(r8), allocatable :: dsout2_dum(:,:), ref_dsout2_dum(:,:)
    real(r8), allocatable :: drout_dum(:,:), ref_drout_dum(:,:)
    real(r8), allocatable :: reff_rain_dum(:,:), ref_reff_rain_dum(:,:)
    real(r8), allocatable :: reff_snow_dum(:,:), ref_reff_snow_dum(:,:)
    
    integer :: nlev   ! number of levels where cloud physics is done
    integer :: mgncol ! size of mgcols
    character(128) :: errstring   ! return status (non-blank for error return)

    integer*8 c1,c2,cr,cm
    real*8 dt
    integer :: itmax
    integer :: it, nThreads
    character(len=40) :: FMT,FMT2
   
    character(len=80), parameter :: kname='[MG2]'

    type(check_t) :: status, vstatus
    real(r8) :: tolerance
!dir$ attributes align:64 :: packed_t, packed_q, packed_qc, packed_qi, packed_nc, packed_ni 
!dir$ attributes align:64 :: packed_qr, packed_qs, packed_nr, packed_ns, packed_relvar


    tolerance = 6.0e-11
    call KGENinitcheck(vstatus,tolerance)

    ! READ local variables
    READ(UNIT=kgen_unit) nlev
    READ(UNIT=kgen_unit) mgncol
    READ(UNIT=kgen_unit) errstring

    ! READ all vars
    call read_var(packed_t, kgen_unit)
    call read_var(packed_q, kgen_unit)
    call read_var(packed_qc, kgen_unit)
    call read_var(packed_qi, kgen_unit)
    call read_var(packed_nc, kgen_unit)
    call read_var(packed_ni, kgen_unit)
    call read_var(packed_qr, kgen_unit)
    call read_var(packed_qs, kgen_unit)
    call read_var(packed_nr, kgen_unit)
    call read_var(packed_ns, kgen_unit)
    call read_var(packed_relvar, kgen_unit)
    call read_var(packed_accre_enhan, kgen_unit)
    call read_var(packed_p, kgen_unit)
    call read_var(packed_pdel, kgen_unit)
    call read_var(packed_cldn, kgen_unit)
    call read_var(packed_liqcldf, kgen_unit)
    call read_var(packed_icecldf, kgen_unit)
    call read_var(packed_rate1ord_cw2pr_st, kgen_unit)
    call read_var(packed_naai, kgen_unit)
    call read_var(packed_npccn, kgen_unit)
    call read_var(packed_rndst, kgen_unit)
    call read_var(packed_nacon, kgen_unit)
    call read_var(packed_tlat, kgen_unit)
    call read_var(packed_qvlat, kgen_unit)
    call read_var(packed_qctend, kgen_unit)
    call read_var(packed_qitend, kgen_unit)
    call read_var(packed_nctend, kgen_unit)
    call read_var(packed_nitend, kgen_unit)
    call read_var(packed_qrtend, kgen_unit)
    call read_var(packed_qstend, kgen_unit)
    call read_var(packed_nrtend, kgen_unit)
    call read_var(packed_nstend, kgen_unit)
    call read_var(packed_prect, kgen_unit)
    call read_var(packed_preci, kgen_unit)
    call read_var(packed_nevapr, kgen_unit)
    call read_var(packed_evapsnow, kgen_unit)
    call read_var(packed_prain, kgen_unit)
    call read_var(packed_prodsnow, kgen_unit)
    call read_var(packed_cmeout, kgen_unit)
    call read_var(packed_qsout, kgen_unit)
    call read_var(packed_rflx, kgen_unit)
    call read_var(packed_sflx, kgen_unit)
    call read_var(packed_qrout, kgen_unit)
    call read_var(packed_qcsevap, kgen_unit)
    call read_var(packed_qisevap, kgen_unit)
    call read_var(packed_qvres, kgen_unit)
    call read_var(packed_cmei, kgen_unit)
    call read_var(packed_vtrmc, kgen_unit)
    call read_var(packed_vtrmi, kgen_unit)
    call read_var(packed_umr, kgen_unit)
    call read_var(packed_ums, kgen_unit)
    call read_var(packed_qcsedten, kgen_unit)
    call read_var(packed_qisedten, kgen_unit)
    call read_var(packed_pra, kgen_unit)
    call read_var(packed_prc, kgen_unit)
    call read_var(packed_mnuccc, kgen_unit)
    call read_var(packed_mnucct, kgen_unit)
    call read_var(packed_msacwi, kgen_unit)
    call read_var(packed_psacws, kgen_unit)
    call read_var(packed_bergs, kgen_unit)
    call read_var(packed_berg, kgen_unit)
    call read_var(packed_melt, kgen_unit)
    call read_var(packed_homo, kgen_unit)
    call read_var(packed_qcres, kgen_unit)
    call read_var(packed_prci, kgen_unit)
    call read_var(packed_prai, kgen_unit)
    call read_var(packed_qires, kgen_unit)
    call read_var(packed_mnuccr, kgen_unit)
    call read_var(packed_pracs, kgen_unit)
    call read_var(packed_meltsdt, kgen_unit)
    call read_var(packed_frzrdt, kgen_unit)
    call read_var(packed_mnuccd, kgen_unit)
    call read_var(packed_nrout, kgen_unit)
    call read_var(packed_nsout, kgen_unit)
    call read_var(packed_refl, kgen_unit)
    call read_var(packed_arefl, kgen_unit)
    call read_var(packed_areflz, kgen_unit)
    call read_var(packed_frefl, kgen_unit)
    call read_var(packed_csrfl, kgen_unit)
    call read_var(packed_acsrfl, kgen_unit)
    call read_var(packed_fcsrfl, kgen_unit)
    call read_var(packed_rercld, kgen_unit)
    call read_var(packed_ncai, kgen_unit)
    call read_var(packed_ncal, kgen_unit)
    call read_var(packed_qrout2, kgen_unit)
    call read_var(packed_qsout2, kgen_unit)
    call read_var(packed_nrout2, kgen_unit)
    call read_var(packed_nsout2, kgen_unit)
    call read_var(packed_freqs, kgen_unit)
    call read_var(packed_freqr, kgen_unit)
    call read_var(packed_nfice, kgen_unit)
    call read_var(packed_qcrat, kgen_unit)
    call read_var(packed_rel, kgen_unit)
    call read_var(packed_rei, kgen_unit)
    call read_var(packed_lambdac, kgen_unit)
    call read_var(packed_mu, kgen_unit)
    call read_var(packed_des, kgen_unit)
    call read_var(packed_dei, kgen_unit)
    call read_var(rel_fn_dum, kgen_unit)
    call read_var(dsout2_dum, kgen_unit)
    call read_var(drout_dum, kgen_unit)
    call read_var(reff_rain_dum, kgen_unit)
    call read_var(reff_snow_dum, kgen_unit)
    call read_var(packed_tnd_qsnow, kgen_unit, .true.)
    call read_var(packed_tnd_nsnow, kgen_unit, .true.)
    call read_var(packed_re_ice, kgen_unit, .true.)
    call read_var(packed_frzimm, kgen_unit, .true.)
    call read_var(packed_frzcnt, kgen_unit, .true.)
    call read_var(packed_frzdep, kgen_unit, .true.)

    !READ out vars
    call read_var(ref_packed_rate1ord_cw2pr_st, kgen_unit)
    call read_var(ref_packed_tlat, kgen_unit)
    call read_var(ref_packed_qvlat, kgen_unit)
    call read_var(ref_packed_qctend, kgen_unit)
    call read_var(ref_packed_qitend, kgen_unit)
    call read_var(ref_packed_nctend, kgen_unit)
    call read_var(ref_packed_nitend, kgen_unit)
    call read_var(ref_packed_qrtend, kgen_unit)
    call read_var(ref_packed_qstend, kgen_unit)
    call read_var(ref_packed_nrtend, kgen_unit)
    call read_var(ref_packed_nstend, kgen_unit)
    call read_var(ref_packed_prect, kgen_unit)
    call read_var(ref_packed_preci, kgen_unit)
    call read_var(ref_packed_nevapr, kgen_unit)
    call read_var(ref_packed_evapsnow, kgen_unit)
    call read_var(ref_packed_prain, kgen_unit)
    call read_var(ref_packed_prodsnow, kgen_unit)
    call read_var(ref_packed_cmeout, kgen_unit)
    call read_var(ref_packed_qsout, kgen_unit)
    call read_var(ref_packed_rflx, kgen_unit)
    call read_var(ref_packed_sflx, kgen_unit)
    call read_var(ref_packed_qrout, kgen_unit)
    call read_var(ref_packed_qcsevap, kgen_unit)
    call read_var(ref_packed_qisevap, kgen_unit)
    call read_var(ref_packed_qvres, kgen_unit)
    call read_var(ref_packed_cmei, kgen_unit)
    call read_var(ref_packed_vtrmc, kgen_unit)
    call read_var(ref_packed_vtrmi, kgen_unit)
    call read_var(ref_packed_umr, kgen_unit)
    call read_var(ref_packed_ums, kgen_unit)
    call read_var(ref_packed_qcsedten, kgen_unit)
    call read_var(ref_packed_qisedten, kgen_unit)
    call read_var(ref_packed_pra, kgen_unit)
    call read_var(ref_packed_prc, kgen_unit)
    call read_var(ref_packed_mnuccc, kgen_unit)
    call read_var(ref_packed_mnucct, kgen_unit)
    call read_var(ref_packed_msacwi, kgen_unit)
    call read_var(ref_packed_psacws, kgen_unit)
    call read_var(ref_packed_bergs, kgen_unit)
    call read_var(ref_packed_berg, kgen_unit)
    call read_var(ref_packed_melt, kgen_unit)
    call read_var(ref_packed_homo, kgen_unit)
    call read_var(ref_packed_qcres, kgen_unit)
    call read_var(ref_packed_prci, kgen_unit)
    call read_var(ref_packed_prai, kgen_unit)
    call read_var(ref_packed_qires, kgen_unit)
    call read_var(ref_packed_mnuccr, kgen_unit)
    call read_var(ref_packed_pracs, kgen_unit)
    call read_var(ref_packed_meltsdt, kgen_unit)
    call read_var(ref_packed_frzrdt, kgen_unit)
    call read_var(ref_packed_mnuccd, kgen_unit)
    call read_var(ref_packed_nrout, kgen_unit)
    call read_var(ref_packed_nsout, kgen_unit)
    call read_var(ref_packed_refl, kgen_unit)
    call read_var(ref_packed_arefl, kgen_unit)
    call read_var(ref_packed_areflz, kgen_unit)
    call read_var(ref_packed_frefl, kgen_unit)
    call read_var(ref_packed_csrfl, kgen_unit)
    call read_var(ref_packed_acsrfl, kgen_unit)
    call read_var(ref_packed_fcsrfl, kgen_unit)
    call read_var(ref_packed_rercld, kgen_unit)
    call read_var(ref_packed_ncai, kgen_unit)
    call read_var(ref_packed_ncal, kgen_unit)
    call read_var(ref_packed_qrout2, kgen_unit)
    call read_var(ref_packed_qsout2, kgen_unit)
    call read_var(ref_packed_nrout2, kgen_unit)
    call read_var(ref_packed_nsout2, kgen_unit)
    call read_var(ref_packed_freqs, kgen_unit)
    call read_var(ref_packed_freqr, kgen_unit)
    call read_var(ref_packed_nfice, kgen_unit)
    call read_var(ref_packed_qcrat, kgen_unit)
    call read_var(ref_packed_rel, kgen_unit)
    call read_var(ref_packed_rei, kgen_unit)
    call read_var(ref_packed_lambdac, kgen_unit)
    call read_var(ref_packed_mu, kgen_unit)
    call read_var(ref_packed_des, kgen_unit)
    call read_var(ref_packed_dei, kgen_unit)
    call read_var(ref_rel_fn_dum, kgen_unit)
    call read_var(ref_dsout2_dum, kgen_unit)
    call read_var(ref_drout_dum, kgen_unit)
    call read_var(ref_reff_rain_dum, kgen_unit)
    call read_var(ref_reff_snow_dum, kgen_unit)

    ! call KGENApplyPert(packed_t)
    ! RUN KERNEL
    call micro_mg_tend2_0( &
         mgncol,         nlev,           dtime/num_steps,&
         packed_t,               packed_q,               &
         packed_qc,              packed_qi,              &
         packed_nc,              packed_ni,              &
         packed_qr,              packed_qs,              &
         packed_nr,              packed_ns,              &
         packed_relvar,          packed_accre_enhan,     &
         packed_p,               packed_pdel,            &
         packed_cldn,    packed_liqcldf, packed_icecldf, &
         packed_rate1ord_cw2pr_st,                       &
         packed_naai,            packed_npccn,           &
         packed_rndst,           packed_nacon,           &
         packed_tlat,            packed_qvlat,           &
         packed_qctend,          packed_qitend,          &
         packed_nctend,          packed_nitend,          &
         packed_qrtend,          packed_qstend,          &
         packed_nrtend,          packed_nstend,          &
         packed_rel,     rel_fn_dum,     packed_rei,     &
         packed_prect,           packed_preci,           &
         packed_nevapr,          packed_evapsnow,        &
         packed_prain,           packed_prodsnow,        &
         packed_cmeout,          packed_dei,             &
         packed_mu,              packed_lambdac,         &
         packed_qsout,           packed_des,             &
         packed_rflx,    packed_sflx,    packed_qrout,   &
         reff_rain_dum,          reff_snow_dum,          &
         packed_qcsevap, packed_qisevap, packed_qvres,   &
         packed_cmei,    packed_vtrmc,   packed_vtrmi,   &
         packed_umr,             packed_ums,             &
         packed_qcsedten,        packed_qisedten,        &
         packed_pra,             packed_prc,             &
         packed_mnuccc,  packed_mnucct,  packed_msacwi,  &
         packed_psacws,  packed_bergs,   packed_berg,    &
         packed_melt,            packed_homo,            &
         packed_qcres,   packed_prci,    packed_prai,    &
         packed_qires,   packed_mnuccr,  packed_pracs,   &
         packed_meltsdt, packed_frzrdt,  packed_mnuccd,  &
         packed_nrout,           packed_nsout,           &
         packed_refl,    packed_arefl,   packed_areflz,  &
         packed_frefl,   packed_csrfl,   packed_acsrfl,  &
         packed_fcsrfl,          packed_rercld,          &
         packed_ncai,            packed_ncal,            &
         packed_qrout2,          packed_qsout2,          &
         packed_nrout2,          packed_nsout2,          &
         drout_dum,              dsout2_dum,             &
         packed_freqs,           packed_freqr,           &
         packed_nfice,           packed_qcrat,           &
         errstring, &
         packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
         packed_frzimm,  packed_frzcnt,  packed_frzdep   )

    ! verify kernel output
    call verify_var("packed_rate1ord_cw2pr_st", vstatus, packed_rate1ord_cw2pr_st, ref_packed_rate1ord_cw2pr_st)
    call verify_var("packed_tlat", vstatus, packed_tlat, ref_packed_tlat)
    call verify_var("packed_qvlat", vstatus, packed_qvlat, ref_packed_qvlat)
    call verify_var("packed_qctend", vstatus, packed_qctend, ref_packed_qctend)
    call verify_var("packed_qitend", vstatus, packed_qitend, ref_packed_qitend)
    call verify_var("packed_nctend", vstatus, packed_nctend, ref_packed_nctend)
    call verify_var("packed_nitend", vstatus, packed_nitend, ref_packed_nitend)
    call verify_var("packed_qrtend", vstatus, packed_qrtend, ref_packed_qrtend)
    call verify_var("packed_qstend", vstatus, packed_qstend, ref_packed_qstend)
    call verify_var("packed_nrtend", vstatus, packed_nrtend, ref_packed_nrtend)
    call verify_var("packed_nstend", vstatus, packed_nstend, ref_packed_nstend)
    call verify_var("packed_prect",vstatus,  packed_prect, ref_packed_prect)
    call verify_var("packed_preci",vstatus,  packed_preci, ref_packed_preci)
    call verify_var("packed_nevapr", vstatus, packed_nevapr, ref_packed_nevapr)
    call verify_var("packed_evapsnow", vstatus, packed_evapsnow, ref_packed_evapsnow)
    call verify_var("packed_prain", vstatus, packed_prain, ref_packed_prain)
    call verify_var("packed_prodsnow", vstatus, packed_prodsnow, ref_packed_prodsnow)
    call verify_var("packed_cmeout", vstatus, packed_cmeout, ref_packed_cmeout)
    call verify_var("packed_qsout", vstatus, packed_qsout, ref_packed_qsout)
    call verify_var("packed_rflx", vstatus, packed_rflx, ref_packed_rflx)
    call verify_var("packed_sflx", vstatus, packed_sflx, ref_packed_sflx)
    call verify_var("packed_qrout", vstatus, packed_qrout, ref_packed_qrout)
    call verify_var("packed_qcsevap", vstatus, packed_qcsevap, ref_packed_qcsevap)
    call verify_var("packed_qisevap", vstatus, packed_qisevap, ref_packed_qisevap)
    call verify_var("packed_qvres", vstatus, packed_qvres, ref_packed_qvres)
    call verify_var("packed_cmei", vstatus, packed_cmei, ref_packed_cmei)
    call verify_var("packed_vtrmc", vstatus, packed_vtrmc, ref_packed_vtrmc)
    call verify_var("packed_vtrmi", vstatus, packed_vtrmi, ref_packed_vtrmi)
    call verify_var("packed_umr", vstatus, packed_umr, ref_packed_umr)
    call verify_var("packed_ums", vstatus, packed_ums, ref_packed_ums)
    call verify_var("packed_qcsedten", vstatus, packed_qcsedten, ref_packed_qcsedten)
    call verify_var("packed_qisedten", vstatus, packed_qisedten, ref_packed_qisedten)
    call verify_var("packed_pra", vstatus, packed_pra, ref_packed_pra)
    call verify_var("packed_prc", vstatus, packed_prc, ref_packed_prc)
    call verify_var("packed_mnuccc", vstatus, packed_mnuccc, ref_packed_mnuccc)
    call verify_var("packed_mnucct", vstatus, packed_mnucct, ref_packed_mnucct)
    call verify_var("packed_msacwi", vstatus, packed_msacwi, ref_packed_msacwi)
    call verify_var("packed_psacws", vstatus, packed_psacws, ref_packed_psacws)
    call verify_var("packed_bergs", vstatus, packed_bergs, ref_packed_bergs)
    call verify_var("packed_berg", vstatus, packed_berg, ref_packed_berg)
    call verify_var("packed_melt", vstatus, packed_melt, ref_packed_melt)
    call verify_var("packed_homo", vstatus, packed_homo, ref_packed_homo)
    call verify_var("packed_qcres", vstatus, packed_qcres, ref_packed_qcres)
    call verify_var("packed_prci", vstatus, packed_prci, ref_packed_prci)
    call verify_var("packed_prai", vstatus, packed_prai, ref_packed_prai)
    call verify_var("packed_qires", vstatus, packed_qires, ref_packed_qires)
    call verify_var("packed_mnuccr", vstatus, packed_mnuccr, ref_packed_mnuccr)
    call verify_var("packed_pracs", vstatus, packed_pracs, ref_packed_pracs)
    call verify_var("packed_meltsdt", vstatus, packed_meltsdt, ref_packed_meltsdt)
    call verify_var("packed_frzrdt", vstatus, packed_frzrdt, ref_packed_frzrdt)
    call verify_var("packed_mnuccd", vstatus, packed_mnuccd, ref_packed_mnuccd)
    call verify_var("packed_nrout", vstatus, packed_nrout, ref_packed_nrout)
    call verify_var("packed_nsout", vstatus, packed_nsout, ref_packed_nsout)
    call verify_var("packed_refl", vstatus, packed_refl, ref_packed_refl)
    call verify_var("packed_arefl", vstatus, packed_arefl, ref_packed_arefl)
    call verify_var("packed_areflz", vstatus, packed_areflz, ref_packed_areflz)
    call verify_var("packed_frefl", vstatus, packed_frefl, ref_packed_frefl)
    call verify_var("packed_csrfl", vstatus, packed_csrfl, ref_packed_csrfl)
    call verify_var("packed_acsrfl", vstatus, packed_acsrfl, ref_packed_acsrfl)
    call verify_var("packed_fcsrfl", vstatus, packed_fcsrfl, ref_packed_fcsrfl)
    call verify_var("packed_rercld", vstatus, packed_rercld, ref_packed_rercld)
    call verify_var("packed_ncai", vstatus, packed_ncai, ref_packed_ncai)
    call verify_var("packed_ncal", vstatus, packed_ncal, ref_packed_ncal)
    call verify_var("packed_qrout2", vstatus, packed_qrout2, ref_packed_qrout2)
    call verify_var("packed_qsout2", vstatus, packed_qsout2, ref_packed_qsout2)
    call verify_var("packed_nrout2", vstatus, packed_nrout2, ref_packed_nrout2)
    call verify_var("packed_nsout2", vstatus, packed_nsout2, ref_packed_nsout2)
    call verify_var("packed_freqs", vstatus, packed_freqs, ref_packed_freqs)
    call verify_var("packed_freqr", vstatus, packed_freqr, ref_packed_freqr)
    call verify_var("packed_nfice", vstatus, packed_nfice, ref_packed_nfice)
    call verify_var("packed_qcrat", vstatus, packed_qcrat, ref_packed_qcrat)
    call verify_var("packed_rel", vstatus, packed_rel, ref_packed_rel)
    call verify_var("packed_rei", vstatus, packed_rei, ref_packed_rei)
    call verify_var("packed_lambdac", vstatus, packed_lambdac, ref_packed_lambdac)
    call verify_var("packed_mu", vstatus, packed_mu, ref_packed_mu)
    call verify_var("packed_des", vstatus, packed_des, ref_packed_des)
    call verify_var("packed_dei", vstatus, packed_dei, ref_packed_dei)
    call verify_var("rel_fn_dum", vstatus, rel_fn_dum, ref_rel_fn_dum)
    call verify_var("dsout2_dum", vstatus, dsout2_dum, ref_dsout2_dum)
    call verify_var("drout_dum", vstatus, drout_dum, ref_drout_dum)
    call verify_var("reff_rain_dum", vstatus, reff_rain_dum, ref_reff_rain_dum)
    call verify_var("reff_snow_dum", vstatus, reff_snow_dum, ref_reff_snow_dum)

    call KGENPrtCheck(kname,vstatus)
 
#if defined(_OPENMP) 
    !$OMP PARALLEL
        nThreads = omp_get_num_threads()
    !$OMP END PARALLEL
#else
    nThreads = 1
#endif
    itmax=1024*nThreads

    call system_clock(c1,cr,cm)
    !$OMP PARALLEL DEFAULT(NONE) &
    !$OMP SHARED(itmax,mgncol,nlev,dtime,num_steps, &
    !$OMP packed_t, packed_q, packed_qc, packed_qi, packed_nc, packed_ni, packed_qr, packed_qs,  &
    !$OMP packed_nr, packed_ns, packed_relvar, packed_accre_enhan, packed_p, packed_pdel, packed_cldn, &
    !$OMP packed_liqcldf, packed_icecldf, packed_naai, packed_npccn, packed_rndst, packed_nacon) & 
    !$OMP PRIVATE(it,packed_rate1ord_cw2pr_st,packed_tlat,packed_qvlat, &
    !$OMP packed_qctend, packed_qitend, packed_nctend, packed_nitend, packed_qrtend, packed_qstend, &
    !$OMP packed_nrtend,packed_nstend, packed_rel, rel_fn_dum, packed_rei, packed_prect, packed_preci, &
    !$OMP packed_nevapr,packed_evapsnow, packed_prain, packed_prodsnow, packed_cmeout, packed_dei, &
    !$OMP packed_mu,packed_lambdac, packed_qsout, packed_des, packed_rflx, packed_sflx, packed_qrout, &
    !$OMP reff_rain_dum, reff_snow_dum, packed_qcsevap, packed_qisevap, packed_qvres, packed_cmei, packed_vtrmc, packed_vtrmi, &
    !$OMP packed_umr, packed_ums, packed_qcsedten, packed_qisedten, packed_pra,             packed_prc, &
    !$OMP packed_mnuccc, packed_mnucct, packed_msacwi, packed_psacws,  packed_bergs,   packed_berg, packed_melt,            packed_homo, &
    !$OMP packed_qcres,packed_prci, packed_prai, packed_qires,   packed_mnuccr,  packed_pracs,packed_meltsdt, packed_frzrdt,  packed_mnuccd, &
    !$OMP packed_nrout, packed_nsout, packed_refl, packed_arefl,   packed_areflz,packed_frefl,   packed_csrfl,   packed_acsrfl, &
    !$OMP packed_fcsrfl, packed_rercld, packed_ncai, packed_ncal, packed_qrout2,          packed_qsout2, &
    !$OMP packed_nrout2, packed_nsout2, drout_dum, dsout2_dum, packed_freqs,           packed_freqr, & 
    !$OMP packed_nfice, packed_qcrat, errstring, packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,packed_frzimm,  packed_frzcnt,  packed_frzdep   )

    !$OMP DO
    do it=1,itmax
    ! RUN KERNEL
    call micro_mg_tend2_0( &
         mgncol,         nlev,           dtime/num_steps,&
         packed_t,               packed_q,               &
         packed_qc,              packed_qi,              &
         packed_nc,              packed_ni,              &
         packed_qr,              packed_qs,              &
         packed_nr,              packed_ns,              &
         packed_relvar,          packed_accre_enhan,     &
         packed_p,               packed_pdel,            &
         packed_cldn,    packed_liqcldf, packed_icecldf, &
         packed_rate1ord_cw2pr_st,                       &
         packed_naai,            packed_npccn,           &
         packed_rndst,           packed_nacon,           &
         packed_tlat,            packed_qvlat,           &
         packed_qctend,          packed_qitend,          &
         packed_nctend,          packed_nitend,          &
         packed_qrtend,          packed_qstend,          &
         packed_nrtend,          packed_nstend,          &
         packed_rel,     rel_fn_dum,     packed_rei,     &
         packed_prect,           packed_preci,           &
         packed_nevapr,          packed_evapsnow,        &
         packed_prain,           packed_prodsnow,        &
         packed_cmeout,          packed_dei,             &
         packed_mu,              packed_lambdac,         &
         packed_qsout,           packed_des,             &
         packed_rflx,    packed_sflx,    packed_qrout,   &
         reff_rain_dum,          reff_snow_dum,          &
         packed_qcsevap, packed_qisevap, packed_qvres,   &
         packed_cmei,    packed_vtrmc,   packed_vtrmi,   &
         packed_umr,             packed_ums,             &
         packed_qcsedten,        packed_qisedten,        &
         packed_pra,             packed_prc,             &
         packed_mnuccc,  packed_mnucct,  packed_msacwi,  &
         packed_psacws,  packed_bergs,   packed_berg,    &
         packed_melt,            packed_homo,            &
         packed_qcres,   packed_prci,    packed_prai,    &
         packed_qires,   packed_mnuccr,  packed_pracs,   &
         packed_meltsdt, packed_frzrdt,  packed_mnuccd,  &
         packed_nrout,           packed_nsout,           &
         packed_refl,    packed_arefl,   packed_areflz,  &
         packed_frefl,   packed_csrfl,   packed_acsrfl,  &
         packed_fcsrfl,          packed_rercld,          &
         packed_ncai,            packed_ncal,            &
         packed_qrout2,          packed_qsout2,          &
         packed_nrout2,          packed_nsout2,          &
         drout_dum,              dsout2_dum,             &
         packed_freqs,           packed_freqr,           &
         packed_nfice,           packed_qcrat,           &
         errstring, &
         packed_tnd_qsnow,packed_tnd_nsnow,packed_re_ice,&
         packed_frzimm,  packed_frzcnt,  packed_frzdep   )
    enddo 
    !$OMP END DO
    !$OMP END PARALLEL
    call system_clock(c2,cr,cm)
    dt = dble(c2-c1)/dble(cr)

   
    FMT = "(1X,A,A,I3,A,I5,A,F7.5)"
    write(*,FMT) TRIM(kname), ' [NTHR := ',nThreads, '] [itmax:= ',itmax,'] total time (sec): ',dt
    if( nThreads == 1) then 
        FMT2 = "(1X,A,A,I3,A,I5,A,F9.3)"
        write(*,FMT2) TRIM(kname), ' [NTHR := ',nThreads,'] [itmax:= ',itmax,'] time per call (usec): ',1.e6*dt/dble(itmax)
    endif


    end subroutine micro_mg_cam_tend

    end module micro_mg_cam
