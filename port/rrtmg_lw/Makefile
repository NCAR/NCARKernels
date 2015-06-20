# Makefile for KGEN-generated kernel

FC := ifort
FC_FLAGS := 

ALL_OBJS := kernel_driver.o rrtmg_lw_rad.o kgen_utils.o rrlw_kg08.o rrlw_kg15.o rrlw_kg06.o rrlw_kg01.o rrlw_kg10.o rrlw_ref.o rrlw_wvn.o rrlw_kg16.o rrlw_kg02.o shr_kind_mod.o rrtmg_lw_taumol.o rrlw_vsn.o rrlw_kg03.o rrlw_kg07.o rrlw_kg14.o rrlw_kg04.o rrlw_kg12.o rrlw_kg13.o parrrtm.o rrlw_kg05.o rrlw_kg11.o rrlw_con.o rrlw_kg09.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS}   -o kernel.exe $^

kernel_driver.o: kernel_driver.f90 rrtmg_lw_rad.o kgen_utils.o rrlw_kg08.o rrlw_kg15.o rrlw_kg06.o rrlw_kg01.o rrlw_kg10.o rrlw_ref.o rrlw_wvn.o rrlw_kg16.o rrlw_kg02.o shr_kind_mod.o rrtmg_lw_taumol.o rrlw_vsn.o rrlw_kg03.o rrlw_kg07.o rrlw_kg14.o rrlw_kg04.o rrlw_kg12.o rrlw_kg13.o parrrtm.o rrlw_kg05.o rrlw_kg11.o rrlw_con.o rrlw_kg09.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_lw_rad.o: rrtmg_lw_rad.f90 kgen_utils.o rrtmg_lw_taumol.o shr_kind_mod.o parrrtm.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg08.o: rrlw_kg08.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg15.o: rrlw_kg15.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg06.o: rrlw_kg06.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg01.o: rrlw_kg01.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg10.o: rrlw_kg10.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_ref.o: rrlw_ref.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_wvn.o: rrlw_wvn.f90 kgen_utils.o parrrtm.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg16.o: rrlw_kg16.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg02.o: rrlw_kg02.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrtmg_lw_taumol.o: rrtmg_lw_taumol.f90 kgen_utils.o shr_kind_mod.o rrlw_vsn.o rrlw_wvn.o parrrtm.o rrlw_kg01.o rrlw_kg02.o rrlw_ref.o rrlw_con.o rrlw_kg03.o rrlw_kg04.o rrlw_kg05.o rrlw_kg06.o rrlw_kg07.o rrlw_kg08.o rrlw_kg09.o rrlw_kg10.o rrlw_kg11.o rrlw_kg12.o rrlw_kg13.o rrlw_kg14.o rrlw_kg15.o rrlw_kg16.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_vsn.o: rrlw_vsn.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg03.o: rrlw_kg03.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg07.o: rrlw_kg07.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg14.o: rrlw_kg14.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg04.o: rrlw_kg04.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg12.o: rrlw_kg12.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg13.o: rrlw_kg13.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

parrrtm.o: parrrtm.f90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg05.o: rrlw_kg05.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg11.o: rrlw_kg11.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_con.o: rrlw_con.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

rrlw_kg09.o: rrlw_kg09.f90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod *.o
