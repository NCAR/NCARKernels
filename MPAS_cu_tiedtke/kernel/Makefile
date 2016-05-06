# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -xCORE-AVX2 -real-size 64 -O3 -convert big_endian -FR

ALL_OBJS := mpas_atmphys_driver_convection.o mpas_kind_types.o mpas_atmphys_vars.o module_cu_tiedtke.o kernel_driver.o kgen_utils.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

mpas_atmphys_driver_convection.o: mpas_atmphys_driver_convection.F mpas_kind_types.o mpas_atmphys_vars.o module_cu_tiedtke.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_kind_types.o: mpas_kind_types.F kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_atmphys_vars.o: mpas_atmphys_vars.F kgen_utils.o mpas_kind_types.o
	${FC} ${FC_FLAGS} -c -o $@ $<

module_cu_tiedtke.o: module_cu_tiedtke.F kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 mpas_atmphys_driver_convection.o mpas_kind_types.o mpas_atmphys_vars.o module_cu_tiedtke.o kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
