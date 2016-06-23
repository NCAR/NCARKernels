# Makefile for KGEN-generated instrumentation


run: build
	cd /glade/scratch/youngsun/kgensystest/kapp_sys_ys_cesm_intel_clubb_tend_cam_Test; cd /glade/scratch/youngsun/kgensystest/KINTCESM; ./KINTCESM.submit

build: replace
	cd /glade/scratch/youngsun/kgensystest/kapp_sys_ys_cesm_intel_clubb_tend_cam_Test; cd /glade/scratch/youngsun/kgensystest/KINTCESM; ./KINTCESM.build
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90
	mv -f /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90

replace: save
	cp -f clubb_intr.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90
	cp -f physics_types.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90
	cp -f physconst.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90
	cp -f scamMod.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90
	cp -f camsrfexch.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90
	cp -f stats_variables.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90
	cp -f parameters_tunable.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90
	cp -f grid_class.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90
	cp -f error_code.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90
	cp -f model_flags.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90
	cp -f stats_clubb_utilities.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90
	cp -f advance_clubb_core_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90
	cp -f parameters_model.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90
	cp -f pdf_parameter_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90
	cp -f numerical_check.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90
	cp -f stats_type_utilities.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90
	cp -f stats_type.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90
	cp -f stat_file_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90
	cp -f variables_diagnostic_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90
	cp -f pdf_closure_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90
	cp -f saturation.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90
	cp -f array_index.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90
	cp -f sponge_layer_damping.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90
	cp -f advance_xp2_xpyp_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90
	cp -f shr_log_mod.F90 /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90

recover:
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90
	cp -f /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90

recover_from_locals:
	cp -f clubb_intr.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90
	cp -f physics_types.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90
	cp -f physconst.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90
	cp -f scamMod.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90
	cp -f camsrfexch.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90
	cp -f stats_variables.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90
	cp -f parameters_tunable.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90
	cp -f grid_class.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90
	cp -f error_code.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90
	cp -f model_flags.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90
	cp -f stats_clubb_utilities.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90
	cp -f advance_clubb_core_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90
	cp -f parameters_model.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90
	cp -f pdf_parameter_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90
	cp -f numerical_check.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90
	cp -f stats_type_utilities.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90
	cp -f stats_type.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90
	cp -f stat_file_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90
	cp -f variables_diagnostic_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90
	cp -f pdf_closure_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90
	cp -f saturation.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90
	cp -f array_index.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90
	cp -f sponge_layer_damping.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90
	cp -f advance_xp2_xpyp_module.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90
	cp -f shr_log_mod.F90.kgen_org /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90

save:
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90.kgen_org; fi
	if [ ! -f clubb_intr.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/clubb_intr.F90 clubb_intr.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90.kgen_org; fi
	if [ ! -f physics_types.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/cam/physics_types.F90 physics_types.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90.kgen_org; fi
	if [ ! -f physconst.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/utils/physconst.F90 physconst.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90.kgen_org; fi
	if [ ! -f scamMod.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/scamMod.F90 scamMod.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90.kgen_org; fi
	if [ ! -f camsrfexch.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/control/camsrfexch.F90 camsrfexch.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90.kgen_org; fi
	if [ ! -f stats_variables.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_variables.F90 stats_variables.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90.kgen_org; fi
	if [ ! -f parameters_tunable.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_tunable.F90 parameters_tunable.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90.kgen_org; fi
	if [ ! -f grid_class.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/grid_class.F90 grid_class.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90.kgen_org; fi
	if [ ! -f error_code.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/error_code.F90 error_code.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90.kgen_org; fi
	if [ ! -f model_flags.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/model_flags.F90 model_flags.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90.kgen_org; fi
	if [ ! -f stats_clubb_utilities.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_clubb_utilities.F90 stats_clubb_utilities.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90.kgen_org; fi
	if [ ! -f advance_clubb_core_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_clubb_core_module.F90 advance_clubb_core_module.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90.kgen_org; fi
	if [ ! -f parameters_model.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/parameters_model.F90 parameters_model.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90.kgen_org; fi
	if [ ! -f pdf_parameter_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_parameter_module.F90 pdf_parameter_module.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90.kgen_org; fi
	if [ ! -f numerical_check.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/numerical_check.F90 numerical_check.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90.kgen_org; fi
	if [ ! -f stats_type_utilities.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type_utilities.F90 stats_type_utilities.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90.kgen_org; fi
	if [ ! -f stats_type.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stats_type.F90 stats_type.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90.kgen_org; fi
	if [ ! -f stat_file_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/stat_file_module.F90 stat_file_module.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90.kgen_org; fi
	if [ ! -f variables_diagnostic_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/variables_diagnostic_module.F90 variables_diagnostic_module.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90.kgen_org; fi
	if [ ! -f pdf_closure_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/pdf_closure_module.F90 pdf_closure_module.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90.kgen_org; fi
	if [ ! -f saturation.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/saturation.F90 saturation.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90.kgen_org; fi
	if [ ! -f array_index.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/array_index.F90 array_index.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90.kgen_org; fi
	if [ ! -f sponge_layer_damping.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/sponge_layer_damping.F90 sponge_layer_damping.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90 /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90.kgen_org; fi
	if [ ! -f advance_xp2_xpyp_module.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/components/cam/src/physics/clubb/advance_xp2_xpyp_module.F90 advance_xp2_xpyp_module.F90.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90 /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90.kgen_org; fi
	if [ ! -f shr_log_mod.F90.kgen_org ]; then cp -f /glade/scratch/youngsun/kgensystest/cesm_work/cime/share/csm_share/shr/shr_log_mod.F90 shr_log_mod.F90.kgen_org; fi

clean:
	cd /glade/scratch/youngsun/kgensystest/KINTCESM; ./KINTCESM.clean_build all

