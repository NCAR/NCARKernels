# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS :=  -mmic -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs
NODE := $(shell uname -n)

ALL_OBJS := kernel_driver.o rrtmg_lw_rad.o kgen_utils.o shr_kind_mod.o rrlw_vsn.o rrtmg_lw_rtrnmc.o parrrtm.o rrlw_wvn.o rrlw_tbl.o rrlw_con.o

run: build
	ssh ${NODE}-mic1 "cd ${PWD}; ./kernel.exe"

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 rrtmg_lw_rad.o kgen_utils.o shr_kind_mod.o rrlw_vsn.o rrtmg_lw_rtrnmc.o parrrtm.o rrlw_wvn.o rrlw_tbl.o rrlw_con.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_lw_rad.o: rrtmg_lw_rad.f90 kgen_utils.o rrtmg_lw_rtrnmc.o shr_kind_mod.o parrrtm.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_vsn.o: rrlw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_lw_rtrnmc.o: rrtmg_lw_rtrnmc.f90 kgen_utils.o shr_kind_mod.o parrrtm.o rrlw_vsn.o rrlw_wvn.o rrlw_tbl.o rrlw_con.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrtm.o: parrrtm.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_wvn.o: rrlw_wvn.f90 kgen_utils.o parrrtm.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_tbl.o: rrlw_tbl.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_con.o: rrlw_con.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
