run: build
	cd /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/logs/bld/; make run

build: replace
	cd /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/logs/bld/; make build
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90
	mv -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

replace: save
	cp -f mo_gas_phase_chemdr.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90
	cp -f mo_imp_sol.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	cp -f chem_mods.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	cp -f time_manager.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90
	cp -f ESMF_TimeIntervalMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90
	cp -f ESMF_TimeMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90
	cp -f ESMF_AlarmMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90
	cp -f ESMF_ShrTimeMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90
	cp -f ESMF_BaseTimeMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90
	cp -f ESMF_CalendarMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90
	cp -f ESMF_ClockMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90
	cp -f cam_logfile.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90
	cp -f shr_log_mod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90
	cp -f mo_tracname.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

recover:
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90
	cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

recover_from_locals:
	cp -f mo_gas_phase_chemdr.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90
	cp -f mo_imp_sol.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90
	cp -f chem_mods.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90
	cp -f time_manager.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90
	cp -f ESMF_TimeIntervalMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90
	cp -f ESMF_TimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90
	cp -f ESMF_AlarmMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90
	cp -f ESMF_ShrTimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90
	cp -f ESMF_BaseTimeMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90
	cp -f ESMF_CalendarMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90
	cp -f ESMF_ClockMod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90
	cp -f cam_logfile.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90
	cp -f shr_log_mod.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90
	cp -f mo_tracname.F90.kgen_org /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90

save:
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90.kgen_org; fi
	if [ ! -f mo_gas_phase_chemdr.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_gas_phase_chemdr.F90 mo_gas_phase_chemdr.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90.kgen_org; fi
	if [ ! -f mo_imp_sol.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/mo_imp_sol.F90 mo_imp_sol.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90 /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90.kgen_org; fi
	if [ ! -f chem_mods.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/f19_f19_SCALAR_cesm1_4_alpha07c/SourceMods/src.cam/chem_mods.F90 chem_mods.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90.kgen_org; fi
	if [ ! -f time_manager.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/utils/time_manager.F90 time_manager.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90.kgen_org; fi
	if [ ! -f ESMF_TimeIntervalMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeIntervalMod.F90 ESMF_TimeIntervalMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90.kgen_org; fi
	if [ ! -f ESMF_TimeMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_TimeMod.F90 ESMF_TimeMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90.kgen_org; fi
	if [ ! -f ESMF_AlarmMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_AlarmMod.F90 ESMF_AlarmMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90.kgen_org; fi
	if [ ! -f ESMF_ShrTimeMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ShrTimeMod.F90 ESMF_ShrTimeMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90.kgen_org; fi
	if [ ! -f ESMF_BaseTimeMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_BaseTimeMod.F90 ESMF_BaseTimeMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90.kgen_org; fi
	if [ ! -f ESMF_CalendarMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_CalendarMod.F90 ESMF_CalendarMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90.kgen_org; fi
	if [ ! -f ESMF_ClockMod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/esmf_wrf_timemgr/ESMF_ClockMod.F90 ESMF_ClockMod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90.kgen_org; fi
	if [ ! -f cam_logfile.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/control/cam_logfile.F90 cam_logfile.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org; fi
	if [ ! -f shr_log_mod.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/cime/share/csm_share/shr/shr_log_mod.F90 shr_log_mod.F90.kgen_org; fi
	if [ ! -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90 /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90.kgen_org; fi
	if [ ! -f mo_tracname.F90.kgen_org ]; then cp -f /glade/p/work/mickelso/cesm_sandbox/cesm1_4_alpha07c/components/cam/src/chemistry/mozart/mo_tracname.F90 mo_tracname.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
