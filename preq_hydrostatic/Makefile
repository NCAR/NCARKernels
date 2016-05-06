# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := -xMIC-AVX512 -fp-model source -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -O2

ALL_OBJS := kernel_driver.o prim_advance_mod.o kgen_utils.o kinds.o shr_const_mod.o physical_constants.o shr_kind_mod.o prim_si_mod.o element_mod.o physconst.o coordinate_systems_mod.o gridgraph_mod.o edge_mod.o dimensions_mod.o constituents.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 prim_advance_mod.o kgen_utils.o kinds.o shr_const_mod.o physical_constants.o shr_kind_mod.o prim_si_mod.o element_mod.o physconst.o coordinate_systems_mod.o gridgraph_mod.o edge_mod.o dimensions_mod.o constituents.o
	${FC} ${FC_FLAGS} -c -o $@ $<

prim_advance_mod.o: prim_advance_mod.F90 kgen_utils.o prim_si_mod.o kinds.o dimensions_mod.o element_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kinds.o: kinds.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

physical_constants.o: physical_constants.F90 kgen_utils.o physconst.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

prim_si_mod.o: prim_si_mod.F90 kgen_utils.o kinds.o dimensions_mod.o physical_constants.o
	${FC} ${FC_FLAGS} -c -o $@ $<

element_mod.o: element_mod.F90 kgen_utils.o kinds.o coordinate_systems_mod.o dimensions_mod.o gridgraph_mod.o edge_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

physconst.o: physconst.F90 kgen_utils.o shr_kind_mod.o shr_const_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

coordinate_systems_mod.o: coordinate_systems_mod.F90 kgen_utils.o kinds.o
	${FC} ${FC_FLAGS} -c -o $@ $<

gridgraph_mod.o: gridgraph_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

edge_mod.o: edge_mod.F90 kgen_utils.o kinds.o coordinate_systems_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

dimensions_mod.o: dimensions_mod.F90 kgen_utils.o constituents.o
	${FC} ${FC_FLAGS} -c -o $@ $<

constituents.o: constituents.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
