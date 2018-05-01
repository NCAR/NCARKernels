# Makefile for KGEN-generated kernel

include ./Macros

ALL_OBJS := kernel_driver.o micro_mg_cam.o kgen_utils.o micro_mg_utils.o shr_const_mod.o micro_mg2_0.o shr_kind_mod.o shr_spfn_mod.o wv_sat_methods.o

run: build
	

build: ${ALL_OBJS}
	${FC} ${LD_FLAGS}   -o kernel.exe $^ 

kernel_driver.o: kernel_driver.F90 micro_mg_cam.o kgen_utils.o micro_mg_utils.o shr_const_mod.o micro_mg2_0.o shr_kind_mod.o shr_spfn_mod.o wv_sat_methods.o
	${FC} ${FC_FLAGS} -c -o $@ $<

micro_mg_cam.o: micro_mg_cam.F90 kgen_utils.o micro_mg2_0.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

micro_mg_utils.o: micro_mg_utils.F90 kgen_utils.o shr_spfn_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

params.o: params.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

micro_mg2_0.o: micro_mg2_0.F90 params.o kgen_utils.o micro_mg_utils.o wv_sat_methods.o shr_spfn_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_spfn_mod.o: shr_spfn_mod.F90 kgen_utils.o shr_kind_mod.o shr_const_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

wv_sat_methods.o: wv_sat_methods.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.F90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o *.opt*
