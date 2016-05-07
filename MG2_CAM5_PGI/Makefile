# Makefile for KGEN-generated kernel

FC := pgfortran
FC_FLAGS := -i4 -gopt -Mlist -time -Mextend -byteswapio -Mflushz -Kieee   -O  -mp

ALL_OBJS := kernel_driver.o micro_mg_cam.o micro_mg_utils.o shr_kind_mod.o micro_mg2_0.o shr_spfn_mod.o wv_sat_methods.o shr_const_mod.o

run: build
	numactl --membind 1 ./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 micro_mg_cam.o micro_mg_utils.o shr_kind_mod.o micro_mg2_0.o shr_spfn_mod.o wv_sat_methods.o shr_const_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

micro_mg_cam.o: micro_mg_cam.F90 micro_mg2_0.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

micro_mg_utils.o: micro_mg_utils.F90 shr_spfn_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

micro_mg2_0.o: micro_mg2_0.F90 micro_mg_utils.o wv_sat_methods.o shr_spfn_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_spfn_mod.o: shr_spfn_mod.F90 shr_kind_mod.o shr_const_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

wv_sat_methods.o: wv_sat_methods.F90 
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o *.lst
