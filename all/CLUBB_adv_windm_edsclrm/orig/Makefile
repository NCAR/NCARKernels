# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -xHost -O2 -mkl

ALL_OBJS := advance_clubb_core_module.o advance_windm_edsclrm_module.o clubb_precision.o grid_class.o parameters_model.o error_code.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o model_flags.o lapack_wrap.o sponge_layer_damping.o clip_explicit.o variables_diagnostic_module.o kernel_driver.o kgen_utils.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

advance_clubb_core_module.o: advance_clubb_core_module.F90 advance_windm_edsclrm_module.o clubb_precision.o grid_class.o parameters_model.o error_code.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o model_flags.o lapack_wrap.o sponge_layer_damping.o clip_explicit.o variables_diagnostic_module.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

advance_windm_edsclrm_module.o: advance_windm_edsclrm_module.F90 kgen_utils.o clubb_precision.o grid_class.o parameters_model.o error_code.o stats_variables.o stats_type_utilities.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o lapack_wrap.o sponge_layer_damping.o model_flags.o clip_explicit.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_model.o: parameters_model.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o stats_type.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_type_utilities.o: stats_type_utilities.F90 kgen_utils.o clubb_precision.o stats_type.o stat_file_module.o error_code.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_type.o: stats_type.F90 kgen_utils.o clubb_precision.o stat_file_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stat_file_module.o: stat_file_module.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

diffusion.o: diffusion.F90 kgen_utils.o clubb_precision.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_tunable.o: parameters_tunable.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mean_adv.o: mean_adv.F90 kgen_utils.o clubb_precision.o constants_clubb.o grid_class.o model_flags.o
	${FC} ${FC_FLAGS} -c -o $@ $<

model_flags.o: model_flags.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

lapack_wrap.o: lapack_wrap.F90 kgen_utils.o clubb_precision.o error_code.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

sponge_layer_damping.o: sponge_layer_damping.F90 kgen_utils.o clubb_precision.o grid_class.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clip_explicit.o: clip_explicit.F90 kgen_utils.o clubb_precision.o grid_class.o stats_variables.o stats_type_utilities.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 advance_clubb_core_module.o advance_windm_edsclrm_module.o clubb_precision.o grid_class.o parameters_model.o error_code.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o constants_clubb.o diffusion.o parameters_tunable.o mean_adv.o model_flags.o lapack_wrap.o sponge_layer_damping.o clip_explicit.o variables_diagnostic_module.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
