# Makefile for KGEN-generated kernel

pcols ?= 16

FC_0 := /glade/u/apps/opt/intel/2017u1/compilers_and_libraries/linux/bin/intel64/ifort
FC_FLAGS_SET_0 := -qno-opt-dynamic-align -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source -qopt-report -xCORE_AVX2 -no-fma -O2 -debug minimal -free

ALL_OBJS := micro_mg_cam.o micro_mg2_0.o micro_mg_utils.o wv_sat_methods.o shr_spfn_mod.o shr_kind_mod.o shr_const_mod.o kernel_driver.o kgen_utils.o tprof_mod.o

.PHONY: kernel_driver.o 

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0} -o kernel.exe $^  

micro_mg_cam.o: micro_mg_cam.F90 micro_mg2_0.o micro_mg_utils.o wv_sat_methods.o shr_spfn_mod.o shr_kind_mod.o shr_const_mod.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

micro_mg2_0.o: micro_mg2_0.F90 kgen_utils.o tprof_mod.o micro_mg_utils.o wv_sat_methods.o shr_spfn_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

micro_mg_utils.o: micro_mg_utils.F90 kgen_utils.o tprof_mod.o shr_spfn_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

wv_sat_methods.o: wv_sat_methods.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_spfn_mod.o: shr_spfn_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o shr_const_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o tprof_mod.o shr_kind_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kernel_driver.o: kernel_driver.F90 micro_mg_cam.o micro_mg2_0.o micro_mg_utils.o wv_sat_methods.o shr_spfn_mod.o shr_kind_mod.o shr_const_mod.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -D STATEFILE='"../data/pcols${pcols}/kgen_statefile.lst"' -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS} *.optrpt
