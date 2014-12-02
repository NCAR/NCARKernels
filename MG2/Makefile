FC := ifort
FFLAGS := -O3 -I./
SRCS := shr_kind_mod.F90 kgen_utils.F90 micro_mg_utils.F90 wv_sat_methods.F90 micro_mg2_0.F90 micro_mg_cam.F90 kernel_micro_mg_tend.F90

run: kernel.exe
	./kernel.exe

kernel.exe: $(SRCS)
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -f *.exe *.mod *.o
