run: build
	cd /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/logs/bld/; make run

build: replace
	cd /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/logs/bld/; make build
	mv -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

replace: save
	cp -f mo_gas_phase_chemdr.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90
	cp -f mo_imp_sol.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	cp -f chem_mods.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	cp -f mo_tracname.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

recover:
	cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

recover_from_locals:
	cp -f mo_gas_phase_chemdr.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90
	cp -f mo_imp_sol.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	cp -f chem_mods.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	cp -f mo_tracname.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

save:
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90.kgen_org; fi
	if [ ! -f mo_gas_phase_chemdr.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_gas_phase_chemdr.F90 mo_gas_phase_chemdr.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org; fi
	if [ ! -f mo_imp_sol.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90 mo_imp_sol.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org; fi
	if [ ! -f chem_mods.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_VECT_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90 chem_mods.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org; fi
	if [ ! -f mo_tracname.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90 mo_tracname.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
