# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -xMIC-AVX512 -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source   -O2

ALL_OBJS := kernel_driver.o mo_imp_sol.o kgen_utils.o chem_mods.o mo_lu_factor.o shr_kind_mod.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 mo_imp_sol.o kgen_utils.o chem_mods.o mo_lu_factor.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_imp_sol.o: mo_imp_sol.F90 kgen_utils.o mo_lu_factor.o shr_kind_mod.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

chem_mods.o: chem_mods.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_factor.o: mo_lu_factor.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
