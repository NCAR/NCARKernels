run: build
	

build: replace
	cd /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release; ./bld.sh; bsub < submit_init.bash
	mv -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F.kgen_org /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F
	mv -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F.kgen_org /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F
	mv -f /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F.kgen_org /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F

replace: save
	cp -f mpas_atmphys_driver_convection.F /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F
	cp -f mpas_atmphys_vars.F /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F
	cp -f module_cu_tiedtke.F /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F

recover:
	cp -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F.kgen_org /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F
	cp -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F.kgen_org /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F
	cp -f /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F.kgen_org /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F

recover_from_locals:
	cp -f mpas_atmphys_driver_convection.F.kgen_org /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F
	cp -f mpas_atmphys_vars.F.kgen_org /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F
	cp -f module_cu_tiedtke.F.kgen_org /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F

save:
	if [ ! -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F.kgen_org ]; then cp -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F.kgen_org; fi
	if [ ! -f mpas_atmphys_driver_convection.F.kgen_org ]; then cp -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_driver_convection.F mpas_atmphys_driver_convection.F.kgen_org; fi
	if [ ! -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F.kgen_org ]; then cp -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F.kgen_org; fi
	if [ ! -f mpas_atmphys_vars.F.kgen_org ]; then cp -f /glade/u/home/youngsun/trepo/temp/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/mpas_atmphys_vars.F mpas_atmphys_vars.F.kgen_org; fi
	if [ ! -f /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F.kgen_org ]; then cp -f /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F.kgen_org; fi
	if [ ! -f module_cu_tiedtke.F.kgen_org ]; then cp -f /glade/scratch/youngsun/kgen_system_test/branches/initial/MPAS-Release/src/core_atmosphere/physics/physics_wrf/module_cu_tiedtke.F module_cu_tiedtke.F.kgen_org; fi

#clean:
	#rm -f kernel.exe *.mod *.o
