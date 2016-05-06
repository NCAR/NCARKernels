# Makefile for KGEN-generated kernel

FC := ifort
#FC_FLAGS := -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source   -xHost  -O2
FC_FLAGS := -mmic -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source    -O2
# -inline-max-size=7800 -inline-max-total-size=10000
#FC_FLAGS := -no-fma -qopt-report=5 -override-limits -no-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model fast  -xCORE-AVX2  -O3
#FC_FLAGS := -qopt-report=5 -no-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model fast  -xCORE-AVX2  -O3
#FC_FLAGS := -qopt-report=5 -no-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model fast -xCORE-AVX2  -O3

ALL_OBJS := kernel_driver.o mo_imp_sol.o kgen_utils.o chem_mods.o mo_lu_solve.o mo_lu_solve_r4.o mo_lu_solve_vec.o mo_lu_solve_vecr4.o shr_kind_mod.o

run: build
	ssh mike-mic1 "cd ${PWD}; ./kernel.exe"

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 mo_imp_sol.o kgen_utils.o chem_mods.o mo_lu_solve.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_imp_sol.o: mo_imp_sol.F90 kgen_utils.o mo_lu_solve.o mo_lu_solve_r4.o mo_lu_solve_vec.o mo_lu_solve_vecr4.o shr_kind_mod.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

chem_mods.o: chem_mods.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_solve.o: mo_lu_solve.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_solve_r4.o: mo_lu_solve_r4.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_solve_vec.o: mo_lu_solve_vec.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_solve_vecr4.o: mo_lu_solve_vecr4.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
