# Makefile for KGEN-generated kernel

# Originally used compiler(s)
#FC_0 := /glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/bin/intel64/ifort
#FC_0 := mpif90
#FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp -D_MPI
FC_0 := ifort
FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp

MPI ?=

ifdef MPI
    FC_0 := mpiifort
    FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp -D_MPI
    NPROCS ?= 4
    RUN_PREFIX := mpirun -np ${NPROCS}

else
    FC_0 := ifort
    FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -free -fpp
    RUN_PREFIX :=

endif

ALL_OBJS := subcol_SILHS.o silhs_api_module.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o mt95.o generate_uniform_sample_module.o latin_hypercube_arrays.o silhs_importance_sample_module.o array_index.o error_code.o fill_holes.o grid_class.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o transform_to_pdf_module.o matrix_operations.o output_2D_samples_module.o clubb_api_module.o corr_varnce_module.o ppgrid.o ref_pres.o shr_kind_mod.o variables_diagnostic_module.o kernel_driver.o kgen_utils.o tprof_mod.o dtrmv.o lsame.o xerbla.o

build: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^  

run: build
	${RUN_PREFIX} ./kernel.exe

subcol_SILHS.o: subcol_SILHS.F90 silhs_api_module.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o mt95.o generate_uniform_sample_module.o latin_hypercube_arrays.o silhs_importance_sample_module.o array_index.o error_code.o fill_holes.o grid_class.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o transform_to_pdf_module.o matrix_operations.o output_2D_samples_module.o clubb_api_module.o corr_varnce_module.o ppgrid.o ref_pres.o shr_kind_mod.o variables_diagnostic_module.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

silhs_api_module.o: silhs_api_module.F90 kgen_utils.o tprof_mod.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_parameter_module.o: pdf_parameter_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clubb_precision.o: clubb_precision.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

hydromet_pdf_parameter_module.o: hydromet_pdf_parameter_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

latin_hypercube_driver_module.o: latin_hypercube_driver_module.F90 kgen_utils.o tprof_mod.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o generate_uniform_sample_module.o silhs_importance_sample_module.o latin_hypercube_arrays.o array_index.o fill_holes.o grid_class.o error_code.o stats_variables.o stats_type_utilities.o transform_to_pdf_module.o output_2D_samples_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

parameters_silhs.o: parameters_silhs.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

constants_clubb.o: constants_clubb.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

pdf_utilities.o: pdf_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o constants_clubb.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

math_utilities.o: math_utilities.F90 kgen_utils.o tprof_mod.o mt95.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mt95.o: mt95.f90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

generate_uniform_sample_module.o: generate_uniform_sample_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o mt95.o constants_clubb.o latin_hypercube_arrays.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

latin_hypercube_arrays.o: latin_hypercube_arrays.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

silhs_importance_sample_module.o: silhs_importance_sample_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o pdf_utilities.o generate_uniform_sample_module.o constants_clubb.o pdf_parameter_module.o hydromet_pdf_parameter_module.o parameters_silhs.o error_code.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

array_index.o: array_index.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

error_code.o: error_code.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

fill_holes.o: fill_holes.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

grid_class.o: grid_class.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_variables.o: stats_variables.F90 kgen_utils.o tprof_mod.o stats_type.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_type_utilities.o: stats_type_utilities.F90 kgen_utils.o tprof_mod.o clubb_precision.o stats_type.o stat_file_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stats_type.o: stats_type.F90 kgen_utils.o tprof_mod.o clubb_precision.o stat_file_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

stat_file_module.o: stat_file_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

transform_to_pdf_module.o: transform_to_pdf_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o array_index.o constants_clubb.o matrix_operations.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

matrix_operations.o: matrix_operations.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

output_2D_samples_module.o: output_2D_samples_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o stat_file_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clubb_api_module.o: clubb_api_module.F90 kgen_utils.o tprof_mod.o corr_varnce_module.o pdf_parameter_module.o variables_diagnostic_module.o hydromet_pdf_parameter_module.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

corr_varnce_module.o: corr_varnce_module.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

ref_pres.o: ref_pres.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

variables_diagnostic_module.o: variables_diagnostic_module.F90 kgen_utils.o tprof_mod.o clubb_precision.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

dtrmv.o: dtrmv.f lsame.o xerbla.o
	${FC_0} -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -c -o $@ $<

lsame.o: lsame.f
	${FC_0} -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -c -o $@ $<

xerbla.o: xerbla.f
	${FC_0} -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -no-fma -O2 -debug minimal -c -o $@ $<

kernel_driver.o: kernel_driver.f90 subcol_SILHS.o silhs_api_module.o pdf_parameter_module.o clubb_precision.o hydromet_pdf_parameter_module.o latin_hypercube_driver_module.o parameters_silhs.o constants_clubb.o pdf_utilities.o math_utilities.o mt95.o generate_uniform_sample_module.o latin_hypercube_arrays.o silhs_importance_sample_module.o array_index.o error_code.o fill_holes.o grid_class.o stats_variables.o stats_type_utilities.o stats_type.o stat_file_module.o transform_to_pdf_module.o matrix_operations.o output_2D_samples_module.o clubb_api_module.o corr_varnce_module.o ppgrid.o ref_pres.o shr_kind_mod.o variables_diagnostic_module.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0}  -D STATEFILE='"../data/kgen_statefile.lst"' -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS} *.optrpt
