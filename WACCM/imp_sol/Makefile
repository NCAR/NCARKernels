# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -no-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source   -xAVX  -O2

ALL_OBJS := kernel_driver.o mo_gas_phase_chemdr.o kgen_utils.o mo_tracname.o mo_nln_matrix.o mo_lu_solve.o chem_mods.o mo_prod_loss.o mo_lin_matrix.o ppgrid.o mo_imp_sol.o shr_kind_mod.o mo_lu_factor.o mo_indprd.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 mo_gas_phase_chemdr.o kgen_utils.o mo_tracname.o mo_nln_matrix.o mo_lu_solve.o chem_mods.o mo_prod_loss.o mo_lin_matrix.o ppgrid.o mo_imp_sol.o shr_kind_mod.o mo_lu_factor.o mo_indprd.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_gas_phase_chemdr.o: mo_gas_phase_chemdr.F90 kgen_utils.o mo_imp_sol.o shr_kind_mod.o ppgrid.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_tracname.o: mo_tracname.F90 kgen_utils.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_nln_matrix.o: mo_nln_matrix.F90 kgen_utils.o shr_kind_mod.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_solve.o: mo_lu_solve.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

chem_mods.o: chem_mods.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_prod_loss.o: mo_prod_loss.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lin_matrix.o: mo_lin_matrix.F90 kgen_utils.o shr_kind_mod.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_imp_sol.o: mo_imp_sol.F90 kgen_utils.o ppgrid.o chem_mods.o shr_kind_mod.o mo_indprd.o mo_lin_matrix.o mo_nln_matrix.o mo_lu_factor.o mo_prod_loss.o mo_lu_solve.o mo_tracname.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_factor.o: mo_lu_factor.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_indprd.o: mo_indprd.F90 kgen_utils.o shr_kind_mod.o ppgrid.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
