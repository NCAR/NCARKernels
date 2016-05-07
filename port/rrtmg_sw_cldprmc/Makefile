# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS :=  -mmic -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs 
NODE := $(shell uname -n)

ALL_OBJS := kernel_driver.o rrtmg_sw_rad.o kgen_utils.o shr_kind_mod.o rrtmg_sw_cldprmc.o rrsw_wvn.o rrsw_cld.o parrrsw.o rrsw_vsn.o

run: build
	ssh ${NODE}-mic1 "cd ${PWD}; ./kernel.exe"

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 rrtmg_sw_rad.o kgen_utils.o shr_kind_mod.o rrtmg_sw_cldprmc.o rrsw_wvn.o rrsw_cld.o parrrsw.o rrsw_vsn.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_rad.o: rrtmg_sw_rad.f90 kgen_utils.o rrtmg_sw_cldprmc.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_cldprmc.o: rrtmg_sw_cldprmc.f90 kgen_utils.o shr_kind_mod.o parrrsw.o rrsw_vsn.o rrsw_wvn.o rrsw_cld.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_wvn.o: rrsw_wvn.f90 kgen_utils.o parrrsw.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_cld.o: rrsw_cld.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrsw.o: parrrsw.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_vsn.o: rrsw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
