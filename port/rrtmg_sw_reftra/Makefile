# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS_SNB :=  -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs  -xHost -vec-threshold0 -qopt-report=5 -align array256byte
FC_FLAGS_HSW :=  -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -vec-threshold0 -xCORE-AVX2 -qopt-report=5 -align array256byte
FC_FLAGS_PHI :=  -mmic -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -vec-threshold0 -qopt-report=5 -align array256byte

#FC_FLAGS := ${FC_FLAGS_SNB}
FC_FLAGS := ${FC_FLAGS_HSW}
#FC_FLAGS := ${FC_FLAGS_PHI}

ALL_OBJS := kernel_driver.o rrtmg_sw_spcvmc.o kgen_utils.o rrsw_vsn.o rrtmg_sw_reftra.o shr_kind_mod.o rrsw_tbl.o

run: build
	./kernel.exe

runphi: build
	ssh `hostname`-mic0 "cd ${PWD}; ./kernel.exe" | tee phi.out

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
	rm -f kernel.exe *.mod *.o *.optrpt phi.out
