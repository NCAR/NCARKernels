# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS :=  -xCORE-AVX2 -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs 

ALL_OBJS := kernel_driver.o rrtmg_lw_rad.o kgen_utils.o rrtmg_lw_cldprmc.o shr_kind_mod.o rrlw_vsn.o parrrtm.o rrlw_wvn.o rrlw_cld.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 rrtmg_lw_rad.o kgen_utils.o rrtmg_lw_cldprmc.o shr_kind_mod.o rrlw_vsn.o parrrtm.o rrlw_wvn.o rrlw_cld.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_lw_rad.o: rrtmg_lw_rad.f90 kgen_utils.o rrtmg_lw_cldprmc.o shr_kind_mod.o parrrtm.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_lw_cldprmc.o: rrtmg_lw_cldprmc.f90 kgen_utils.o shr_kind_mod.o parrrtm.o rrlw_vsn.o rrlw_cld.o rrlw_wvn.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_vsn.o: rrlw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrtm.o: parrrtm.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_wvn.o: rrlw_wvn.f90 kgen_utils.o parrrtm.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_cld.o: rrlw_cld.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
