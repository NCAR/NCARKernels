# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS :=  -xMIC-AVX512 -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs

ALL_OBJS := kernel_driver.o rrtmg_sw_rad.o kgen_utils.o rrsw_ref.o rrtmg_sw_setcoef.o shr_kind_mod.o parrrsw.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 rrtmg_sw_rad.o kgen_utils.o rrsw_ref.o rrtmg_sw_setcoef.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_rad.o: rrtmg_sw_rad.f90 kgen_utils.o rrtmg_sw_setcoef.o shr_kind_mod.o parrrsw.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrsw_ref.o: rrsw_ref.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_sw_setcoef.o: rrtmg_sw_setcoef.f90 kgen_utils.o shr_kind_mod.o rrsw_ref.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrsw.o: parrrsw.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
