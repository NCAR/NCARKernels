# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -xMIC-AVX512 -no-opt-dynamic-align -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -O2 -mkl

ALL_OBJS := advance_clubb_core_module.o pdf_closure_module.o clubb_precision.o parameters_model.o pdf_parameter_module.o error_code.o constants_clubb.o parameters_tunable.o stats_variables.o saturation.o model_flags.o shr_const_mod.o shr_kind_mod.o anl_erf.o array_index.o numerical_check.o grid_class.o variables_diagnostic_module.o interpolation.o kernel_driver.o kgen_utils.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

advance_clubb_core_module.o: advance_clubb_core_module.F90 pdf_closure_module.o clubb_precision.o parameters_model.o pdf_parameter_module.o error_code.o constants_clubb.o parameters_tunable.o stats_variables.o saturation.o model_flags.o shr_const_mod.o shr_kind_mod.o anl_erf.o array_index.o numerical_check.o grid_class.o variables_diagnostic_module.o interpolation.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_closure_module.o: pdf_closure_module.F90 kgen_utils.o clubb_precision.o parameters_model.o pdf_parameter_module.o error_code.o constants_clubb.o parameters_tunable.o stats_variables.o saturation.o anl_erf.o array_index.o numerical_check.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_model.o: parameters_model.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_parameter_module.o: pdf_parameter_module.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o clubb_precision.o shr_const_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_tunable.o: parameters_tunable.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

saturation.o: saturation.F90 kgen_utils.o clubb_precision.o model_flags.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

model_flags.o: model_flags.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

anl_erf.o: anl_erf.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

array_index.o: array_index.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

numerical_check.o: numerical_check.F90 kgen_utils.o clubb_precision.o pdf_parameter_module.o parameters_model.o stats_variables.o constants_clubb.o error_code.o
	${FC} ${FC_FLAGS} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o clubb_precision.o model_flags.o constants_clubb.o interpolation.o
	${FC} ${FC_FLAGS} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

interpolation.o: interpolation.F90 kgen_utils.o clubb_precision.o constants_clubb.o model_flags.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 advance_clubb_core_module.o pdf_closure_module.o clubb_precision.o parameters_model.o pdf_parameter_module.o error_code.o constants_clubb.o parameters_tunable.o stats_variables.o saturation.o model_flags.o shr_const_mod.o shr_kind_mod.o anl_erf.o array_index.o numerical_check.o grid_class.o variables_diagnostic_module.o interpolation.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
