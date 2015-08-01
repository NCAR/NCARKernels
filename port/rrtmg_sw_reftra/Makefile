# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS :=  -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs  -xAVX

ALL_OBJS := kernel_driver.o rrtmg_sw_spcvmc.o kgen_utils.o rrsw_vsn.o rrtmg_sw_reftra.o shr_kind_mod.o rrsw_tbl.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 rrtmg_sw_spcvmc.o kgen_utils.o rrsw_vsn.o rrtmg_sw_reftra.o shr_kind_mod.o rrsw_tbl.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_spcvmc.o: rrtmg_sw_spcvmc.f90 kgen_utils.o rrtmg_sw_reftra.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_vsn.o: rrsw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_reftra.o: rrtmg_sw_reftra.f90 kgen_utils.o shr_kind_mod.o rrsw_vsn.o rrsw_tbl.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_tbl.o: rrsw_tbl.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
