# Makefile for KGEN-generated kernel

# Originally used compiler(s)
#FC_0 := /glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/bin/intel64/ifort

MPI ?=

ifdef MPI
	FC_0 := mpiifort
	FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -xCORE_AVX2 -no-fma -O2 -debug minimal -free -fpp -mkl -D_MPI
	NPROCS ?= 4
	RUN_PREFIX := mpirun -np ${NPROCS}

else
	FC_0 := ifort
	FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -xCORE_AVX2 -no-fma -O2 -debug minimal -free -fpp -mkl
	RUN_PREFIX :=

endif

ALL_OBJS := clubb_intr.o shr_kind_mod.o ppgrid.o ref_pres.o physics_types.o physconst.o shr_const_mod.o scamMod.o camsrfexch.o constituents.o clubb_api_module.o clubb_precision.o grid_class.o model_flags.o constants_clubb.o interpolation.o stats_variables.o parameter_indices.o parameters_tunable.o error_code.o parameters_model.o pdf_parameter_module.o stats_clubb_utilities.o advance_clubb_core_module.o fill_holes.o numerical_check.o stats_type_utilities.o stats_type.o stat_file_module.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o adg1_adg2_3d_luhar_pdf.o calc_roots.o new_pdf_main.o new_pdf.o new_tsdadg_pdf.o LY93_pdf.o pdf_utilities.o saturation.o array_index.o T_in_K_module.o mixing_length.o advance_helper_module.o surface_varnce_module.o advance_xm_wpxp_module.o mono_flux_limiter.o turbulent_adv_pdf.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_xp3_module.o advance_windm_edsclrm_module.o time_manager.o ESMF.o ESMF_TimeIntervalMod.o ESMF_BaseMod.o ESMF_TimeMod.o ESMF_AlarmMod.o ESMF_ShrTimeMod.o ESMF_BaseTimeMod.o ESMF_CalendarMod.o ESMF_ClockMod.o shr_infnan_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

build: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^  

run: build
	${RUN_PREFIX} ./kernel.exe

clubb_intr.o: clubb_intr.F90 shr_kind_mod.o ppgrid.o ref_pres.o physics_types.o physconst.o shr_const_mod.o scamMod.o camsrfexch.o constituents.o clubb_api_module.o clubb_precision.o grid_class.o model_flags.o constants_clubb.o interpolation.o stats_variables.o parameter_indices.o parameters_tunable.o error_code.o parameters_model.o pdf_parameter_module.o stats_clubb_utilities.o advance_clubb_core_module.o fill_holes.o numerical_check.o stats_type_utilities.o stats_type.o stat_file_module.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o adg1_adg2_3d_luhar_pdf.o calc_roots.o new_pdf_main.o new_pdf.o new_tsdadg_pdf.o LY93_pdf.o pdf_utilities.o saturation.o array_index.o T_in_K_module.o mixing_length.o advance_helper_module.o surface_varnce_module.o advance_xm_wpxp_module.o mono_flux_limiter.o turbulent_adv_pdf.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_xp3_module.o advance_windm_edsclrm_module.o time_manager.o ESMF.o ESMF_TimeIntervalMod.o ESMF_BaseMod.o ESMF_TimeMod.o ESMF_AlarmMod.o ESMF_ShrTimeMod.o ESMF_BaseTimeMod.o ESMF_CalendarMod.o ESMF_ClockMod.o shr_infnan_mod.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ref_pres.o: ref_pres.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

physics_types.o: physics_types.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o constituents.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

physconst.o: physconst.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_const_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

scamMod.o: scamMod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

camsrfexch.o: camsrfexch.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o ppgrid.o constituents.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

constituents.o: constituents.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clubb_api_module.o: clubb_api_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o stats_variables.o parameter_indices.o parameters_tunable.o error_code.o parameters_model.o constants_clubb.o pdf_parameter_module.o stats_clubb_utilities.o advance_clubb_core_module.o advance_xp2_xpyp_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o tprof_mod.o clubb_precision.o model_flags.o constants_clubb.o interpolation.o error_code.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

model_flags.o: model_flags.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o tprof_mod.o clubb_precision.o shr_const_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

interpolation.o: interpolation.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o model_flags.o error_code.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o tprof_mod.o clubb_precision.o stats_type.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameter_indices.o: parameter_indices.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameters_tunable.o: parameters_tunable.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameter_indices.o constants_clubb.o error_code.o grid_class.o model_flags.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameters_model.o: parameters_model.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_parameter_module.o: pdf_parameter_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o error_code.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_clubb_utilities.o: stats_clubb_utilities.F90 kgen_utils.o tprof_mod.o stats_variables.o clubb_precision.o grid_class.o pdf_parameter_module.o parameters_model.o T_in_K_module.o stats_type_utilities.o variables_diagnostic_module.o saturation.o pdf_utilities.o constants_clubb.o interpolation.o fill_holes.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_clubb_core_module.o: advance_clubb_core_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o error_code.o model_flags.o constants_clubb.o parameters_tunable.o stats_variables.o fill_holes.o numerical_check.o stats_type_utilities.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o pdf_utilities.o array_index.o saturation.o T_in_K_module.o mixing_length.o advance_helper_module.o surface_varnce_module.o advance_xm_wpxp_module.o advance_xp2_xpyp_module.o clip_explicit.o advance_wp2_wp3_module.o advance_xp3_module.o advance_windm_edsclrm_module.o interpolation.o stats_clubb_utilities.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

fill_holes.o: fill_holes.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

numerical_check.o: numerical_check.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o constants_clubb.o error_code.o pdf_parameter_module.o stats_variables.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_type_utilities.o: stats_type_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o stats_type.o stat_file_module.o grid_class.o error_code.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_type.o: stats_type.F90 kgen_utils.o tprof_mod.o clubb_precision.o stat_file_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stat_file_module.o: stat_file_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

Skx_module.o: Skx_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameters_tunable.o constants_clubb.o grid_class.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

sigma_sqd_w_module.o: sigma_sqd_w_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_closure_module.o: pdf_closure_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o constants_clubb.o adg1_adg2_3d_luhar_pdf.o new_pdf_main.o new_tsdadg_pdf.o LY93_pdf.o pdf_utilities.o model_flags.o stats_variables.o saturation.o error_code.o array_index.o numerical_check.o parameters_tunable.o interpolation.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

adg1_adg2_3d_luhar_pdf.o: adg1_adg2_3d_luhar_pdf.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o constants_clubb.o parameters_tunable.o calc_roots.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

calc_roots.o: calc_roots.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

new_pdf_main.o: new_pdf_main.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o pdf_parameter_module.o constants_clubb.o parameters_tunable.o new_pdf.o model_flags.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

new_pdf.o: new_pdf.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o calc_roots.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

new_tsdadg_pdf.o: new_tsdadg_pdf.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

LY93_pdf.o: LY93_pdf.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_utilities.o: pdf_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

saturation.o: saturation.F90 kgen_utils.o tprof_mod.o clubb_precision.o model_flags.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

array_index.o: array_index.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

T_in_K_module.o: T_in_K_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mixing_length.o: mixing_length.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o model_flags.o saturation.o parameters_tunable.o error_code.o numerical_check.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_helper_module.o: advance_helper_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o model_flags.o constants_clubb.o parameters_model.o T_in_K_module.o saturation.o parameters_tunable.o stats_variables.o stats_type_utilities.o interpolation.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

surface_varnce_module.o: surface_varnce_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameters_model.o constants_clubb.o parameters_tunable.o array_index.o error_code.o numerical_check.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_xm_wpxp_module.o: advance_xm_wpxp_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o model_flags.o pdf_closure_module.o constants_clubb.o parameters_tunable.o stats_variables.o stats_type_utilities.o error_code.o mono_flux_limiter.o turbulent_adv_pdf.o advance_helper_module.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o fill_holes.o clip_explicit.o sponge_layer_damping.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mono_flux_limiter.o: mono_flux_limiter.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o stats_variables.o stats_type_utilities.o mean_adv.o lapack_wrap.o error_code.o fill_holes.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

turbulent_adv_pdf.o: turbulent_adv_pdf.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

diffusion.o: diffusion.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mean_adv.o: mean_adv.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o model_flags.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clip_semi_implicit.o: clip_semi_implicit.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

lapack_wrap.o: lapack_wrap.F90 kgen_utils.o tprof_mod.o clubb_precision.o error_code.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pos_definite_module.o: pos_definite_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o error_code.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clip_explicit.o: clip_explicit.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o stats_variables.o model_flags.o stats_type_utilities.o constants_clubb.o parameters_model.o parameters_tunable.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

sponge_layer_damping.o: sponge_layer_damping.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_xp2_xpyp_module.o: advance_xp2_xpyp_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o pdf_parameter_module.o parameters_model.o constants_clubb.o error_code.o parameters_tunable.o model_flags.o turbulent_adv_pdf.o pdf_closure_module.o stats_variables.o stats_type_utilities.o diffusion.o mean_adv.o advance_helper_module.o lapack_wrap.o interpolation.o fill_holes.o clip_explicit.o sponge_layer_damping.o array_index.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_wp2_wp3_module.o: advance_wp2_wp3_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o pdf_parameter_module.o model_flags.o parameters_tunable.o constants_clubb.o error_code.o stats_variables.o stats_type_utilities.o pdf_closure_module.o diffusion.o mean_adv.o lapack_wrap.o fill_holes.o clip_explicit.o sponge_layer_damping.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_xp3_module.o: advance_xp3_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o stats_variables.o stats_type_utilities.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_windm_edsclrm_module.o: advance_windm_edsclrm_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o model_flags.o stats_variables.o stats_type_utilities.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o lapack_wrap.o error_code.o sponge_layer_damping.o clip_explicit.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

time_manager.o: time_manager.F90 kgen_utils.o tprof_mod.o ESMF.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF.o: ESMF.F90 kgen_utils.o tprof_mod.o ESMF_BaseMod.o ESMF_ClockMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_TimeIntervalMod.o: ESMF_TimeIntervalMod.F90 kgen_utils.o tprof_mod.o ESMF_BaseTimeMod.o ESMF_ShrTimeMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_BaseMod.o: ESMF_BaseMod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_TimeMod.o: ESMF_TimeMod.F90 kgen_utils.o tprof_mod.o ESMF_ShrTimeMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_AlarmMod.o: ESMF_AlarmMod.F90 kgen_utils.o tprof_mod.o ESMF_TimeIntervalMod.o ESMF_TimeMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_ShrTimeMod.o: ESMF_ShrTimeMod.F90 kgen_utils.o tprof_mod.o ESMF_BaseTimeMod.o ESMF_CalendarMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_BaseTimeMod.o: ESMF_BaseTimeMod.F90 kgen_utils.o tprof_mod.o ESMF_BaseMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_CalendarMod.o: ESMF_CalendarMod.F90 kgen_utils.o tprof_mod.o ESMF_BaseMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ESMF_ClockMod.o: ESMF_ClockMod.F90 kgen_utils.o tprof_mod.o ESMF_TimeIntervalMod.o ESMF_TimeMod.o ESMF_BaseMod.o ESMF_AlarmMod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_infnan_mod.o: shr_infnan_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 clubb_intr.o shr_kind_mod.o ppgrid.o ref_pres.o physics_types.o physconst.o shr_const_mod.o scamMod.o camsrfexch.o constituents.o clubb_api_module.o clubb_precision.o grid_class.o model_flags.o constants_clubb.o interpolation.o stats_variables.o parameter_indices.o parameters_tunable.o error_code.o parameters_model.o pdf_parameter_module.o stats_clubb_utilities.o advance_clubb_core_module.o fill_holes.o numerical_check.o stats_type_utilities.o stats_type.o stat_file_module.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o adg1_adg2_3d_luhar_pdf.o calc_roots.o new_pdf_main.o new_pdf.o new_tsdadg_pdf.o LY93_pdf.o pdf_utilities.o saturation.o array_index.o T_in_K_module.o mixing_length.o advance_helper_module.o surface_varnce_module.o advance_xm_wpxp_module.o mono_flux_limiter.o turbulent_adv_pdf.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_xp3_module.o advance_windm_edsclrm_module.o time_manager.o ESMF.o ESMF_TimeIntervalMod.o ESMF_BaseMod.o ESMF_TimeMod.o ESMF_AlarmMod.o ESMF_ShrTimeMod.o ESMF_BaseTimeMod.o ESMF_CalendarMod.o ESMF_ClockMod.o shr_infnan_mod.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -D STATEFILE='"../data/kgen_statefile.lst"' -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
