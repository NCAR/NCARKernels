# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -mmic -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -O2

ALL_OBJS := kernel_driver.o ecosys_mod.o kgen_utils.o constants.o state_mod.o kinds_mod.o domain.o shr_kind_mod.o co2calc.o grid.o domain_size.o time_management.o POP_KindsMod.o shr_vmath_mod.o blocks.o

run: build
	ssh mike-mic1 "cd ${PWD}; ./kernel.exe"

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 ecosys_mod.o kgen_utils.o constants.o state_mod.o kinds_mod.o domain.o shr_kind_mod.o co2calc.o grid.o domain_size.o time_management.o POP_KindsMod.o shr_vmath_mod.o blocks.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ecosys_mod.o: ecosys_mod.F90 kgen_utils.o co2calc.o domain.o grid.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constants.o: constants.F90 kgen_utils.o kinds_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

state_mod.o: state_mod.F90 kgen_utils.o kinds_mod.o domain.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kinds_mod.o: kinds_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

domain.o: domain.F90 kgen_utils.o domain_size.o kinds_mod.o blocks.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

co2calc.o: co2calc.F90 kgen_utils.o kinds_mod.o blocks.o constants.o state_mod.o shr_vmath_mod.o domain_size.o domain.o time_management.o
	${FC} ${FC_FLAGS} -c -o $@ $<

grid.o: grid.F90 kgen_utils.o POP_KindsMod.o blocks.o domain_size.o
	${FC} ${FC_FLAGS} -c -o $@ $<

domain_size.o: domain_size.F90 kgen_utils.o kinds_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

time_management.o: time_management.F90 kgen_utils.o kinds_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

POP_KindsMod.o: POP_KindsMod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_vmath_mod.o: shr_vmath_mod.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

blocks.o: blocks.F90 kgen_utils.o kinds_mod.o domain_size.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
