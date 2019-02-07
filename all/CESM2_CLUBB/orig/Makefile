# Makefile for KGEN-generated kernel

# 16 or 192
PCOLS ?=16

.PHONY: .FORCE

# Original compiler(s) used
#FC_0 := /glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/bin/intel64/ifort
FC_0 := mpif90
FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -xCORE_AVX2 -no-fma -O2 -debug minimal -free -fpp -D_MPI -mkl
# Uncomment below if you like to replicate the kernel extraction environment.
# NOTE: the environment is valid only if you are using this kernel on the same computing system that the kernel was extracted from.
#PRERUN_BUILD := module purge;module try-load ncarenv/1.2;module try-load ncarcompilers/0.4.1;module try-load intel/17.0.1;module try-load mkl/2017.0.1;module try-load impi/2017.1.132; 
#PRERUN_RUN := module purge;module try-load ncarenv/1.2;module try-load ncarcompilers/0.4.1;module try-load intel/17.0.1;module try-load mkl/2017.0.1;module try-load impi/2017.1.132; 

ALL_OBJS := clubb_intr.o shr_kind_mod.o ppgrid.o ref_pres.o physics_types.o physconst.o shr_const_mod.o scamMod.o camsrfexch.o constituents.o clubb_api_module.o stats_variables.o clubb_precision.o parameter_indices.o parameters_tunable.o constants_clubb.o grid_class.o error_code.o model_flags.o interpolation.o stats_clubb_utilities.o parameters_model.o pdf_parameter_module.o advance_clubb_core_module.o fill_holes.o numerical_check.o stats_type_utilities.o stats_type.o stat_file_module.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o calc_roots.o saturation.o anl_erf.o array_index.o pdf_utilities.o T_in_K_module.o mixing_length.o surface_varnce_module.o advance_helper_module.o advance_xm_wpxp_module.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o cam_abortutils.o shr_sys_mod.o shr_abort_mod.o shr_log_mod.o shr_mpi_mod.o shr_infnan_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

build: ${ALL_OBJS}
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^

run: build
	${PRERUN_RUN}mpirun -np 2 ./kernel.exe

clubb_intr.o: clubb_intr.F90 shr_kind_mod.o ppgrid.o ref_pres.o physics_types.o physconst.o shr_const_mod.o scamMod.o camsrfexch.o constituents.o clubb_api_module.o stats_variables.o clubb_precision.o parameter_indices.o parameters_tunable.o constants_clubb.o grid_class.o error_code.o model_flags.o interpolation.o stats_clubb_utilities.o parameters_model.o pdf_parameter_module.o advance_clubb_core_module.o fill_holes.o numerical_check.o stats_type_utilities.o stats_type.o stat_file_module.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o calc_roots.o saturation.o anl_erf.o array_index.o pdf_utilities.o T_in_K_module.o mixing_length.o surface_varnce_module.o advance_helper_module.o advance_xm_wpxp_module.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o cam_abortutils.o shr_sys_mod.o shr_abort_mod.o shr_log_mod.o shr_mpi_mod.o shr_infnan_mod.o kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ppgrid.o: .FORCE kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -D PCOLS=${PCOLS} -c -o $@ ppgrid.F90

ref_pres.o: ref_pres.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

physics_types.o: physics_types.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o constituents.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

physconst.o: physconst.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_const_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

scamMod.o: scamMod.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

camsrfexch.o: camsrfexch.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o ppgrid.o constituents.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

constituents.o: constituents.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clubb_api_module.o: clubb_api_module.F90 kgen_utils.o tprof_mod.o stats_variables.o clubb_precision.o parameter_indices.o parameters_tunable.o grid_class.o constants_clubb.o stats_clubb_utilities.o parameters_model.o pdf_parameter_module.o advance_clubb_core_module.o advance_xp2_xpyp_module.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o tprof_mod.o clubb_precision.o stats_type.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameter_indices.o: parameter_indices.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameters_tunable.o: parameters_tunable.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameter_indices.o constants_clubb.o grid_class.o error_code.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o tprof_mod.o clubb_precision.o shr_const_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o model_flags.o interpolation.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o tprof_mod.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

model_flags.o: model_flags.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

interpolation.o: interpolation.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o model_flags.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_clubb_utilities.o: stats_clubb_utilities.F90 kgen_utils.o tprof_mod.o stats_variables.o clubb_precision.o grid_class.o pdf_parameter_module.o parameters_model.o T_in_K_module.o stats_type_utilities.o variables_diagnostic_module.o saturation.o pdf_utilities.o constants_clubb.o interpolation.o fill_holes.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameters_model.o: parameters_model.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_parameter_module.o: pdf_parameter_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_clubb_core_module.o: advance_clubb_core_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o pdf_parameter_module.o error_code.o model_flags.o constants_clubb.o parameters_tunable.o stats_variables.o fill_holes.o numerical_check.o stats_type_utilities.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o pdf_utilities.o array_index.o saturation.o T_in_K_module.o mixing_length.o surface_varnce_module.o advance_helper_module.o advance_xm_wpxp_module.o advance_xp2_xpyp_module.o clip_explicit.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o interpolation.o stats_clubb_utilities.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

fill_holes.o: fill_holes.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

numerical_check.o: numerical_check.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o constants_clubb.o error_code.o pdf_parameter_module.o stats_variables.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_type_utilities.o: stats_type_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o stats_type.o stat_file_module.o grid_class.o error_code.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_type.o: stats_type.F90 kgen_utils.o tprof_mod.o clubb_precision.o stat_file_module.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stat_file_module.o: stat_file_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o pdf_parameter_module.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

Skx_module.o: Skx_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameters_tunable.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

sigma_sqd_w_module.o: sigma_sqd_w_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_closure_module.o: pdf_closure_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameters_model.o pdf_parameter_module.o error_code.o constants_clubb.o model_flags.o parameters_tunable.o calc_roots.o stats_variables.o saturation.o anl_erf.o array_index.o numerical_check.o interpolation.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

calc_roots.o: calc_roots.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

saturation.o: saturation.F90 kgen_utils.o tprof_mod.o clubb_precision.o model_flags.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

anl_erf.o: anl_erf.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

array_index.o: array_index.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_utilities.o: pdf_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

T_in_K_module.o: T_in_K_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mixing_length.o: mixing_length.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o error_code.o model_flags.o saturation.o constants_clubb.o parameters_tunable.o numerical_check.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

surface_varnce_module.o: surface_varnce_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o parameters_model.o constants_clubb.o error_code.o array_index.o numerical_check.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_helper_module.o: advance_helper_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o model_flags.o T_in_K_module.o saturation.o constants_clubb.o parameters_model.o parameters_tunable.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_xm_wpxp_module.o: advance_xm_wpxp_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o model_flags.o constants_clubb.o parameters_tunable.o advance_helper_module.o stats_variables.o stats_type_utilities.o interpolation.o fill_holes.o error_code.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mono_flux_limiter.o: mono_flux_limiter.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o anl_erf.o constants_clubb.o stats_variables.o stats_type_utilities.o error_code.o mean_adv.o lapack_wrap.o fill_holes.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

diffusion.o: diffusion.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mean_adv.o: mean_adv.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o grid_class.o model_flags.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clip_semi_implicit.o: clip_semi_implicit.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

lapack_wrap.o: lapack_wrap.F90 kgen_utils.o tprof_mod.o clubb_precision.o error_code.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pos_definite_module.o: pos_definite_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o error_code.o constants_clubb.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clip_explicit.o: clip_explicit.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o stats_variables.o stats_type_utilities.o constants_clubb.o parameters_model.o model_flags.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

sponge_layer_damping.o: sponge_layer_damping.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_xp2_xpyp_module.o: advance_xp2_xpyp_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o constants_clubb.o error_code.o parameters_tunable.o model_flags.o mean_adv.o diffusion.o stats_variables.o advance_helper_module.o stats_type_utilities.o lapack_wrap.o interpolation.o fill_holes.o clip_explicit.o array_index.o pdf_parameter_module.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_wp2_wp3_module.o: advance_wp2_wp3_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_tunable.o model_flags.o constants_clubb.o error_code.o stats_variables.o stats_type_utilities.o diffusion.o advance_helper_module.o mean_adv.o lapack_wrap.o fill_holes.o clip_explicit.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

advance_windm_edsclrm_module.o: advance_windm_edsclrm_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o grid_class.o parameters_model.o error_code.o stats_variables.o stats_type_utilities.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o lapack_wrap.o sponge_layer_damping.o model_flags.o clip_explicit.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

cam_abortutils.o: cam_abortutils.F90 kgen_utils.o tprof_mod.o shr_sys_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_sys_mod.o: shr_sys_mod.F90 kgen_utils.o tprof_mod.o shr_abort_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_abort_mod.o: shr_abort_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_log_mod.o shr_mpi_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_log_mod.o: shr_log_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_mpi_mod.o: shr_mpi_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_log_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_infnan_mod.o: shr_infnan_mod.F90 kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kernel_driver.o: .FORCE clubb_intr.o shr_kind_mod.o ppgrid.o ref_pres.o physics_types.o physconst.o shr_const_mod.o scamMod.o camsrfexch.o constituents.o clubb_api_module.o stats_variables.o clubb_precision.o parameter_indices.o parameters_tunable.o constants_clubb.o grid_class.o error_code.o model_flags.o interpolation.o stats_clubb_utilities.o parameters_model.o pdf_parameter_module.o advance_clubb_core_module.o fill_holes.o numerical_check.o stats_type_utilities.o stats_type.o stat_file_module.o variables_diagnostic_module.o Skx_module.o sigma_sqd_w_module.o pdf_closure_module.o calc_roots.o saturation.o anl_erf.o array_index.o pdf_utilities.o T_in_K_module.o mixing_length.o surface_varnce_module.o advance_helper_module.o advance_xm_wpxp_module.o mono_flux_limiter.o diffusion.o mean_adv.o clip_semi_implicit.o lapack_wrap.o pos_definite_module.o clip_explicit.o sponge_layer_damping.o advance_xp2_xpyp_module.o advance_wp2_wp3_module.o advance_windm_edsclrm_module.o cam_abortutils.o shr_sys_mod.o shr_abort_mod.o shr_log_mod.o shr_mpi_mod.o shr_infnan_mod.o kgen_utils.o tprof_mod.o
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -D STATEFILE='"../data/pcols${PCOLS}/kgen_statefile.lst"' -c -o $@ kernel_driver.f90

kgen_utils.o: kgen_utils.f90
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${PRERUN_BUILD}${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

.FORCE:

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
