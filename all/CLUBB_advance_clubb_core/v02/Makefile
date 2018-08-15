# Makefile for KGEN-generated kernel

include ./Macros

ALL_OBJS := clubb_intr.o advance_clubb_core_module.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o stats_variables.o fill_holes.o error_code.o numerical_check.o constants_clubb.o stats_type_utilities.o stats_type.o stat_file_module.o model_flags.o variables_diagnostic_module.o parameters_tunable.o interpolation.o Skw_module.o sigma_sqd_w_module.o pdf_closure_module.o saturation.o shr_const_mod.o shr_kind_mod.o anl_erf.o array_index.o pdf_utilities.o mixing_length.o advance_helper_module.o surface_varnce_module.o T_in_K_module.o advance_xm_wpxp_module.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o stats_clubb_utilities.o ppgrid.o kernel_driver.o kgen_utils.o

run: build

build: ${ALL_OBJS}
	${FC} -o kernel.exe $^ ${LD_FLAGS}

clubb_intr.o: clubb_intr.F90 advance_clubb_core_module.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o stats_variables.o fill_holes.o error_code.o numerical_check.o constants_clubb.o stats_type_utilities.o stats_type.o stat_file_module.o model_flags.o variables_diagnostic_module.o parameters_tunable.o interpolation.o Skw_module.o sigma_sqd_w_module.o pdf_closure_module.o saturation.o shr_const_mod.o shr_kind_mod.o anl_erf.o array_index.o pdf_utilities.o mixing_length.o advance_helper_module.o surface_varnce_module.o T_in_K_module.o advance_xm_wpxp_module.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o stats_clubb_utilities.o ppgrid.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_clubb_core_module.o: advance_clubb_core_module.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o stats_variables.o fill_holes.o error_code.o numerical_check.o stats_type_utilities.o model_flags.o variables_diagnostic_module.o parameters_tunable.o constants_clubb.o Skw_module.o sigma_sqd_w_module.o pdf_closure_module.o pdf_utilities.o array_index.o mixing_length.o advance_helper_module.o surface_varnce_module.o saturation.o T_in_K_module.o advance_xm_wpxp_module.o advance_xp2_xpyp_module.o clip_explicit.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o interpolation.o stats_clubb_utilities.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o clubb_precision.o model_flags.o constants_clubb.o interpolation.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_model.o: parameters_model.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_parameter_module.o: pdf_parameter_module.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o stats_type.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

fill_holes.o: fill_holes.F90 kgen_utils.o clubb_precision.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

numerical_check.o: numerical_check.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_model.o constants_clubb.o error_code.o pdf_parameter_module.o stats_variables.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o clubb_precision.o shr_const_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_type_utilities.o: stats_type_utilities.F90 kgen_utils.o clubb_precision.o stats_type.o stat_file_module.o grid_class.o error_code.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_type.o: stats_type.F90 kgen_utils.o clubb_precision.o stat_file_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stat_file_module.o: stat_file_module.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

model_flags.o: model_flags.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o clubb_precision.o pdf_parameter_module.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_tunable.o: parameters_tunable.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

interpolation.o: interpolation.F90 kgen_utils.o clubb_precision.o constants_clubb.o model_flags.o
	${FC} ${FC_FLAGS} -c -o $@ $<

Skw_module.o: Skw_module.F90 kgen_utils.o clubb_precision.o parameters_tunable.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

sigma_sqd_w_module.o: sigma_sqd_w_module.F90 kgen_utils.o clubb_precision.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_closure_module.o: pdf_closure_module.F90 kgen_utils.o clubb_precision.o parameters_model.o pdf_parameter_module.o error_code.o constants_clubb.o parameters_tunable.o stats_variables.o saturation.o anl_erf.o array_index.o numerical_check.o interpolation.o
	${FC} ${FC_FLAGS} -c -o $@ $<

saturation.o: saturation.F90 kgen_utils.o clubb_precision.o model_flags.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

anl_erf.o: anl_erf.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

array_index.o: array_index.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_utilities.o: pdf_utilities.F90 kgen_utils.o clubb_precision.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mixing_length.o: mixing_length.F90 kgen_utils.o clubb_precision.o grid_class.o error_code.o model_flags.o saturation.o constants_clubb.o parameters_tunable.o numerical_check.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_helper_module.o: advance_helper_module.F90 kgen_utils.o clubb_precision.o grid_class.o constants_clubb.o parameters_model.o
	${FC} ${FC_FLAGS} -c -o $@ $<

surface_varnce_module.o: surface_varnce_module.F90 kgen_utils.o clubb_precision.o parameters_model.o constants_clubb.o error_code.o array_index.o numerical_check.o
	${FC} ${FC_FLAGS} -c -o $@ $<

T_in_K_module.o: T_in_K_module.F90 kgen_utils.o clubb_precision.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_xm_wpxp_module.o: advance_xm_wpxp_module.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_model.o model_flags.o constants_clubb.o parameters_tunable.o stats_variables.o stats_type_utilities.o error_code.o mono_flux_limiter.o advance_helper_module.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o fill_holes.o clip_explicit.o sponge_layer_damping.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mono_flux_limiter.o: mono_flux_limiter.F90 kgen_utils.o clubb_precision.o grid_class.o anl_erf.o constants_clubb.o stats_variables.o stats_type_utilities.o error_code.o mean_adv.o lapack_wrap.o fill_holes.o
	${FC} ${FC_FLAGS} -c -o $@ $<

diffusion.o: diffusion.F90 kgen_utils.o clubb_precision.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mean_adv.o: mean_adv.F90 kgen_utils.o clubb_precision.o constants_clubb.o grid_class.o model_flags.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clip_semi_implicit.o: clip_semi_implicit.F90 kgen_utils.o clubb_precision.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

lapack_wrap.o: lapack_wrap.F90 kgen_utils.o clubb_precision.o error_code.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pos_definite_module.o: pos_definite_module.F90 kgen_utils.o clubb_precision.o grid_class.o error_code.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clip_explicit.o: clip_explicit.F90 kgen_utils.o clubb_precision.o grid_class.o stats_variables.o stats_type_utilities.o constants_clubb.o parameters_model.o model_flags.o
	${FC} ${FC_FLAGS} -c -o $@ $<

sponge_layer_damping.o: sponge_layer_damping.F90 kgen_utils.o clubb_precision.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_xp2_xpyp_module.o: advance_xp2_xpyp_module.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_model.o constants_clubb.o error_code.o parameters_tunable.o model_flags.o mean_adv.o diffusion.o stats_variables.o advance_helper_module.o stats_type_utilities.o lapack_wrap.o interpolation.o fill_holes.o clip_explicit.o array_index.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_wp2_wp3_module.o: advance_wp2_wp3_module.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_tunable.o error_code.o constants_clubb.o stats_variables.o stats_type_utilities.o diffusion.o model_flags.o advance_helper_module.o mean_adv.o lapack_wrap.o fill_holes.o clip_explicit.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_windm_edsclrm_module.o: advance_windm_edsclrm_module.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_model.o error_code.o stats_variables.o stats_type_utilities.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o lapack_wrap.o sponge_layer_damping.o model_flags.o clip_explicit.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_clubb_utilities.o: stats_clubb_utilities.F90 kgen_utils.o clubb_precision.o grid_class.o pdf_parameter_module.o parameters_model.o stats_variables.o T_in_K_module.o stats_type_utilities.o variables_diagnostic_module.o saturation.o pdf_utilities.o constants_clubb.o interpolation.o fill_holes.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.F90 clubb_intr.o advance_clubb_core_module.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o stats_variables.o fill_holes.o error_code.o numerical_check.o constants_clubb.o stats_type_utilities.o stats_type.o stat_file_module.o model_flags.o variables_diagnostic_module.o parameters_tunable.o interpolation.o Skw_module.o sigma_sqd_w_module.o pdf_closure_module.o saturation.o shr_const_mod.o shr_kind_mod.o anl_erf.o array_index.o pdf_utilities.o mixing_length.o advance_helper_module.o surface_varnce_module.o T_in_K_module.o advance_xm_wpxp_module.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o stats_clubb_utilities.o ppgrid.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.L ${ALL_OBJS}
