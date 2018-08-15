# Makefile for KGEN-generated kernel

include ./Macros


ALL_OBJS := mo_gas_phase_chemdr.o mo_imp_sol.o shr_kind_mod.o chem_mods.o mo_indprd.o mo_lin_matrix.o mo_nln_matrix.o mo_lu_factor.o mo_prod_loss.o mo_lu_solve.o mo_tracname.o ppgrid.o kernel_driver.o kgen_utils.o kgen_extensions.o kernel_info.o

build: ${ALL_OBJS}
	${FC} ${LD_FLAGS}   -o kernel.exe $^

run: build 
	./kernel.exe

mo_gas_phase_chemdr.o: mo_gas_phase_chemdr.F90 mo_imp_sol.o shr_kind_mod.o chem_mods.o mo_indprd.o mo_lin_matrix.o mo_nln_matrix.o mo_lu_factor.o mo_prod_loss.o mo_lu_solve.o mo_tracname.o ppgrid.o kgen_utils.o kgen_extensions.o 
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_imp_sol.o: mo_imp_sol.F90 kgen_utils.o shr_kind_mod.o chem_mods.o mo_indprd.o mo_lin_matrix.o mo_nln_matrix.o mo_lu_factor.o mo_prod_loss.o mo_lu_solve.o mo_tracname.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

chem_mods.o: chem_mods.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_indprd.o: mo_indprd.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lin_matrix.o: mo_lin_matrix.F90 kgen_utils.o shr_kind_mod.o ppgrid.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_nln_matrix.o: mo_nln_matrix.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_factor.o: mo_lu_factor.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_prod_loss.o: mo_prod_loss.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_lu_solve.o: mo_lu_solve.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mo_tracname.o: mo_tracname.F90 kgen_utils.o chem_mods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

ppgrid.o: ppgrid.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.F90 mo_gas_phase_chemdr.o mo_imp_sol.o shr_kind_mod.o chem_mods.o mo_indprd.o mo_lin_matrix.o mo_nln_matrix.o mo_lu_factor.o mo_prod_loss.o mo_lu_solve.o mo_tracname.o ppgrid.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_extensions.o: kgen_extensions.F90 kernel_info.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_info.o: kernel_info.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
