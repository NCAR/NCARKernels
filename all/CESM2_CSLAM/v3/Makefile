# Makefile for KGEN-generated kernel

FC_0 := /glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/bin/intel64/ifort
FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -finstrument-functions -g -xCORE_AVX2 -no-fma -O2 -debug minimal -free

ALL_OBJS := fvm_consistent_se_cslam.o fvm_control_volume_mod.o shr_kind_mod.o dimensions_mod.o constituents.o coordinate_systems_mod.o perf_mod.o perf_utils.o fvm_reconstruction_mod.o control_mod.o cam_abortutils.o shr_sys_mod.o shr_abort_mod.o shr_log_mod.o shr_mpi_mod.o element_mod.o gridgraph_mod.o edgetype_mod.o fvm_analytic_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^  

fvm_consistent_se_cslam.o: fvm_consistent_se_cslam.F90 fvm_control_volume_mod.o shr_kind_mod.o dimensions_mod.o constituents.o coordinate_systems_mod.o perf_mod.o perf_utils.o fvm_reconstruction_mod.o control_mod.o cam_abortutils.o shr_sys_mod.o shr_abort_mod.o shr_log_mod.o shr_mpi_mod.o element_mod.o gridgraph_mod.o edgetype_mod.o fvm_analytic_mod.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

fvm_control_volume_mod.o: fvm_control_volume_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o dimensions_mod.o coordinate_systems_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

dimensions_mod.o: dimensions_mod.F90 kgen_utils.o tprof_mod.o constituents.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

constituents.o: constituents.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

coordinate_systems_mod.o: coordinate_systems_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

perf_mod.o: perf_mod.F90 kgen_utils.o tprof_mod.o perf_utils.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

perf_utils.o: perf_utils.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

fvm_reconstruction_mod.o: fvm_reconstruction_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o perf_mod.o control_mod.o cam_abortutils.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

control_mod.o: control_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

cam_abortutils.o: cam_abortutils.F90 kgen_utils.o tprof_mod.o shr_sys_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_sys_mod.o: shr_sys_mod.F90 kgen_utils.o tprof_mod.o shr_abort_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_abort_mod.o: shr_abort_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_log_mod.o shr_mpi_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_log_mod.o: shr_log_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_mpi_mod.o: shr_mpi_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_log_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

element_mod.o: element_mod.F90 kgen_utils.o tprof_mod.o coordinate_systems_mod.o dimensions_mod.o shr_kind_mod.o gridgraph_mod.o edgetype_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

gridgraph_mod.o: gridgraph_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

edgetype_mod.o: edgetype_mod.F90 kgen_utils.o tprof_mod.o coordinate_systems_mod.o shr_kind_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

fvm_analytic_mod.o: fvm_analytic_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 fvm_consistent_se_cslam.o fvm_control_volume_mod.o shr_kind_mod.o dimensions_mod.o constituents.o coordinate_systems_mod.o perf_mod.o perf_utils.o fvm_reconstruction_mod.o control_mod.o cam_abortutils.o shr_sys_mod.o shr_abort_mod.o shr_log_mod.o shr_mpi_mod.o element_mod.o gridgraph_mod.o edgetype_mod.o fvm_analytic_mod.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.optrpt ${ALL_OBJS}
