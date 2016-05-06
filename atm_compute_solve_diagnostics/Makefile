# Makefile for KGEN-generated kernel

FC := ifort 
DEFINE := -DSINGLE_PRECISION

#FC_FLAGS := $(DEFINE) -O2 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs  -xAVX
#FC_FLAGS := $(DEFINE) -qopt-report=5 -O3 -real-size 64 -convert big_endian -FR
#FC_FLAGS := $(DEFINE) -qopt-report=5  -O3 -xCORE-AVX2 -real_size 64 -convert big_endian -FR
#FC_FLAGS := $(DEFINE) -qopt-report=5  -O3 -xAVX -real-size 64 -convert big_endian -FR

# SINGLE_PRECISION flags 
# FC_FLAGS := $(DEFINE) -qopt-report=5  -O3 -xCORE-AVX2 -convert big_endian -FR
FC_FLAGS := $(DEFINE) -xMIC-AVX512 -pgo -align array32byte -qopt-report=5  -O3 -convert big_endian -FR
#FC_FLAGS := $(DEFINE) -qopt-report=5  -O3 -xHost -convert big_endian -FR

ALL_OBJS := kernel_driver.o mpas_atm_time_integration.o kgen_utils.o mpas_kinds_types.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.F90 mpas_atm_time_integration.o kgen_utils.o mpas_kinds_types.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_atm_time_integration.o: mpas_atm_time_integration.F90 kgen_utils.o mpas_kinds_types.o
	${FC} ${FC_FLAGS} -c -o $@ $<

mpas_kinds_types.o: mpas_kinds_types.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
