# Makefile for KGEN-generated kernel
#
include ./Macros

pcols ?= 144

#FC := ifort
#FC_FLAGS := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp

MPI ?=


#ifdef MPI
#    FC := mpiifort
#    FC_FLAGS := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp -D_MPI
#    NPROCS ?= 4
#    RUN_PREFIX := mpirun -np ${NPROCS}
#
#else
#    FC := ifort
#    FC_FLAGS := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp -p
#    #FC_FLAGS := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model fast -no-fma -O3 -debug minimal -free -fpp
#    RUN_PREFIX :=
#
#endif

ALL_OBJS := subcol_SILHS.o silhs_api_module.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o mt95.o generate_uniform_sample_module.o latin_hypercube_arrays.o silhs_importance_sample_module.o array_index.o error_code.o fill_holes.o grid_class.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o transform_to_pdf_module.o matrix_operations.o output_2D_samples_module.o clubb_api_module.o corr_varnce_module.o ppgrid.o ref_pres.o shr_kind_mod.o variables_diagnostic_module.o kernel_driver.o kgen_utils.o tprof_mod.o dtrmv.o lsame.o xerbla.o

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS} -o kernel.exe $^  

run: build
	${RUN_PREFIX} ./kernel.exe

subcol_SILHS.o: subcol_SILHS.F90 silhs_api_module.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o mt95.o generate_uniform_sample_module.o latin_hypercube_arrays.o silhs_importance_sample_module.o array_index.o error_code.o fill_holes.o grid_class.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o transform_to_pdf_module.o matrix_operations.o output_2D_samples_module.o clubb_api_module.o corr_varnce_module.o ppgrid.o ref_pres.o shr_kind_mod.o variables_diagnostic_module.o kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

silhs_api_module.o: silhs_api_module.F90 kgen_utils.o tprof_mod.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_parameter_module.o: pdf_parameter_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

hydromet_pdf_parameter_module.o: hydromet_pdf_parameter_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

latin_hypercube_driver_module.o: latin_hypercube_driver_module.F90 kgen_utils.o tprof_mod.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o generate_uniform_sample_module.o silhs_importance_sample_module.o latin_hypercube_arrays.o array_index.o fill_holes.o grid_class.o error_code.o stats_variables.o stats_type_utilities.o transform_to_pdf_module.o output_2D_samples_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parameters_silhs.o: parameters_silhs.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

pdf_utilities.o: pdf_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${FC} ${FC_FLAGS} -c -o $@ $<

math_utilities.o: math_utilities.F90 kgen_utils.o tprof_mod.o mt95.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mt95.o: mt95.f90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

generate_uniform_sample_module.o: generate_uniform_sample_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o mt95.o constants_clubb.o latin_hypercube_arrays.o
	${FC} ${FC_FLAGS} -c -o $@ $<

latin_hypercube_arrays.o: latin_hypercube_arrays.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

silhs_importance_sample_module.o: silhs_importance_sample_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o pdf_utilities.o generate_uniform_sample_module.o constants_clubb.o pdf_parameter_module.o hydromet_pdf_parameter_module.o parameters_silhs.o error_code.o
	${FC} ${FC_FLAGS} -c -o $@ $<

array_index.o: array_index.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

fill_holes.o: fill_holes.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o tprof_mod.o stats_type.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_type_utilities.o: stats_type_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o stats_type.o stat_file_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stats_type.o: stats_type.F90 kgen_utils.o tprof_mod.o clubb_precision.o stat_file_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

stat_file_module.o: stat_file_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

transform_to_pdf_module.o: transform_to_pdf_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o array_index.o constants_clubb.o matrix_operations.o
	${FC} ${FC_FLAGS} -c -o $@ $<

matrix_operations.o: matrix_operations.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

output_2D_samples_module.o: output_2D_samples_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o stat_file_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clubb_api_module.o: clubb_api_module.F90 kgen_utils.o tprof_mod.o corr_varnce_module.o pdf_parameter_module.o variables_diagnostic_module.o hydromet_pdf_parameter_module.o
	${FC} ${FC_FLAGS} -c -o $@ $<

corr_varnce_module.o: corr_varnce_module.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ref_pres.o: ref_pres.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC} ${FC_FLAGS} -c -o $@ $<

dtrmv.o: dtrmv.f lsame.o xerbla.o
	${FC} -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -c -o $@ $<

lsame.o: lsame.f
	${FC} -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -c -o $@ $<

xerbla.o: xerbla.f
	${FC} -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -c -o $@ $<

kernel_driver.o: kernel_driver.F90 subcol_SILHS.o silhs_api_module.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o mt95.o generate_uniform_sample_module.o latin_hypercube_arrays.o silhs_importance_sample_module.o array_index.o error_code.o fill_holes.o grid_class.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o transform_to_pdf_module.o matrix_operations.o output_2D_samples_module.o clubb_api_module.o corr_varnce_module.o ppgrid.o ref_pres.o shr_kind_mod.o variables_diagnostic_module.o kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -DSPCOLSSIZE=${pcols}  -D STATEFILE='"../data/spcols${pcols}/kgen_statefile.lst"' -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS} *.optrpt
