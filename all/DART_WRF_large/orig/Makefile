# Makefile for KGEN-generated kernel


#STATES_PER_RANK=81K
STATES_PER_RANK=162K
#STATES_PER_RANK=323K

include ./Macros

#FC_0 := mpif90
#FC_FLAGS_SET_0 := -O3 -fp-model fast  -g -traceback -assume buffered_io -xHost -fpp -D_MPI

#FC_0 := ifort
#FC_FLAGS_SET_0 := -O3 -fp-model fast -g -traceback -assume buffered_io -xHost -fpp

ALL_OBJS := assim_tools_mod.o types_mod.o mpi_utilities_mod.o utilities_mod.o ensemble_manager_mod.o time_manager_mod.o cov_cutoff_mod.o location_mod.o sampling_error_correction_mod.o reg_factor_mod.o adaptive_inflate_mod.o random_seq_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS} -o kernel.exe $^  

assim_tools_mod.o: assim_tools_mod.f90 types_mod.o mpi_utilities_mod.o utilities_mod.o ensemble_manager_mod.o time_manager_mod.o cov_cutoff_mod.o location_mod.o sampling_error_correction_mod.o reg_factor_mod.o adaptive_inflate_mod.o random_seq_mod.o kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

types_mod.o: types_mod.f90 kgen_utils.o tprof_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpi_utilities_mod.o: mpi_utilities_mod.f90 kgen_utils.o tprof_mod.o types_mod.o utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

utilities_mod.o: utilities_mod.f90 kgen_utils.o tprof_mod.o types_mod.o mpi_utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ensemble_manager_mod.o: ensemble_manager_mod.f90 kgen_utils.o tprof_mod.o types_mod.o time_manager_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

time_manager_mod.o: time_manager_mod.f90 kgen_utils.o tprof_mod.o utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

cov_cutoff_mod.o: cov_cutoff_mod.f90 kgen_utils.o tprof_mod.o types_mod.o location_mod.o utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

location_mod.o: location_mod.f90 kgen_utils.o tprof_mod.o types_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

sampling_error_correction_mod.o: sampling_error_correction_mod.f90 kgen_utils.o tprof_mod.o types_mod.o utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

reg_factor_mod.o: reg_factor_mod.f90 kgen_utils.o tprof_mod.o types_mod.o time_manager_mod.o utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

adaptive_inflate_mod.o: adaptive_inflate_mod.f90 kgen_utils.o tprof_mod.o types_mod.o random_seq_mod.o utilities_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

random_seq_mod.o: random_seq_mod.f90 kgen_utils.o tprof_mod.o types_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 assim_tools_mod.o types_mod.o mpi_utilities_mod.o utilities_mod.o ensemble_manager_mod.o time_manager_mod.o cov_cutoff_mod.o location_mod.o sampling_error_correction_mod.o reg_factor_mod.o adaptive_inflate_mod.o random_seq_mod.o kgen_utils.o tprof_mod.o Makefile
	${FC} ${FC_FLAGS} -D STATEFILE='"../data/${STATES_PER_RANK}/kgen_statefile.lst"' -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.optrpt ${ALL_OBJS}
