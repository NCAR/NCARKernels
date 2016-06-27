run: build
	

build: replace
	
	mv -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90
	mv -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90
	mv -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90
	mv -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90

replace: save
	cp -f micro_mg_utils.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90
	cp -f micro_mg2_0.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90
	cp -f wv_sat_methods.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90
	cp -f micro_mg_cam.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90

recover:
	cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90
	cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90
	cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90
	cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90

recover_from_locals:
	cp -f micro_mg_utils.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90
	cp -f micro_mg2_0.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90
	cp -f wv_sat_methods.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90
	cp -f micro_mg_cam.F90.kgen_org /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90

save:
	if [ ! -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90.kgen_org; fi
	if [ ! -f micro_mg_utils.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_utils.F90 micro_mg_utils.F90.kgen_org; fi
	if [ ! -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90.kgen_org; fi
	if [ ! -f micro_mg2_0.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg2_0.F90 micro_mg2_0.F90.kgen_org; fi
	if [ ! -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90.kgen_org; fi
	if [ ! -f wv_sat_methods.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/wv_sat_methods.F90 wv_sat_methods.F90.kgen_org; fi
	if [ ! -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90 /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90.kgen_org; fi
	if [ ! -f micro_mg_cam.F90.kgen_org ]; then cp -f /glade/p/work/dennis/cesm/cesm1_4_beta07/components/cam/src/physics/cam/micro_mg_cam.F90 micro_mg_cam.F90.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
