# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -assume byterecl -fp-model precise -ftz -O3

ALL_OBJS := kernel_driver.o prim_advection_mod.o dimensions_mod.o kinds.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 prim_advection_mod.o dimensions_mod.o kinds.o
	${FC} ${FC_FLAGS} -c -o $@ $<

prim_advection_mod.o: prim_advection_mod.F90 kinds.o dimensions_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

dimensions_mod.o: dimensions_mod.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

kinds.o: kinds.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
