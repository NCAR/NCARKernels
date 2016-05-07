# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -xMIC-AVX512 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -O2

ALL_OBJS := kernel_driver.o hmix_gm.o kgen_utils.o constants.o kinds_mod.o domain.o exit_mod.o grid.o time_management.o hmix_gm_submeso_share.o domain_size.o POP_KindsMod.o blocks.o tavg.o

run: build
	numactl --membind 1 ./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 hmix_gm.o kgen_utils.o constants.o kinds_mod.o domain.o exit_mod.o grid.o time_management.o hmix_gm_submeso_share.o domain_size.o POP_KindsMod.o blocks.o tavg.o
	${FC} ${FC_FLAGS} -c -o $@ $<

hmix_gm.o: hmix_gm.F90 kgen_utils.o tavg.o hmix_gm_submeso_share.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constants.o: constants.F90 kgen_utils.o kinds_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kinds_mod.o: kinds_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

domain.o: domain.F90 kgen_utils.o blocks.o kinds_mod.o domain_size.o
	${FC} ${FC_FLAGS} -c -o $@ $<

exit_mod.o: exit_mod.F90 kgen_utils.o constants.o
	${FC} ${FC_FLAGS} -c -o $@ $<

grid.o: grid.F90 kgen_utils.o POP_KindsMod.o blocks.o domain_size.o
	${FC} ${FC_FLAGS} -c -o $@ $<

time_management.o: time_management.F90 kgen_utils.o grid.o
	${FC} ${FC_FLAGS} -c -o $@ $<

hmix_gm_submeso_share.o: hmix_gm_submeso_share.F90 kgen_utils.o kinds_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

domain_size.o: domain_size.F90 kgen_utils.o kinds_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

POP_KindsMod.o: POP_KindsMod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

blocks.o: blocks.F90 kgen_utils.o kinds_mod.o domain_size.o exit_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

tavg.o: tavg.F90 kgen_utils.o domain.o time_management.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
