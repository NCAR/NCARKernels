# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -xMIC-AVX512 -real-size 64 -O3 -convert big_endian -FR

ALL_OBJS := mpas_atmphys_driver_radiation_lw.o mpas_kind_types.o mpas_atmphys_vars.o module_ra_rrtmg_lw.o mpas_atmphys_o3climatology.o mpas_atmphys_constants.o mpas_constants.o kernel_driver.o kgen_utils.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

mpas_atmphys_driver_radiation_lw.o: mpas_atmphys_driver_radiation_lw.F mpas_kind_types.o mpas_atmphys_vars.o module_ra_rrtmg_lw.o mpas_atmphys_o3climatology.o mpas_atmphys_constants.o mpas_constants.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_kind_types.o: mpas_kind_types.F kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_atmphys_vars.o: mpas_atmphys_vars.F kgen_utils.o mpas_kind_types.o
	${FC} ${FC_FLAGS} -c -o $@ $<

module_ra_rrtmg_lw.o: module_ra_rrtmg_lw.F kgen_utils.o mpas_atmphys_o3climatology.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_atmphys_o3climatology.o: mpas_atmphys_o3climatology.F kgen_utils.o mpas_kind_types.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_atmphys_constants.o: mpas_atmphys_constants.F kgen_utils.o mpas_constants.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_constants.o: mpas_constants.F kgen_utils.o mpas_kind_types.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 mpas_atmphys_driver_radiation_lw.o mpas_kind_types.o mpas_atmphys_vars.o module_ra_rrtmg_lw.o mpas_atmphys_o3climatology.o mpas_atmphys_constants.o mpas_constants.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
