#
# Intel 
FC := ifort
FFLAGS := -mmic -O3 -qopt-report=5 -fp-model fast -no-prec-div -no-prec-sqrt -I./
#FFLAGS := -O3 -fp-model precise -I./ -DCPRINTEL
#FFLAGS := -O3 -fp-model fast -xHost -no-prec-div -no-prec-sqrt  -I./ -DCPRINTEL
#
# PGI 
#FC := pgf95
#FFLAGS := -O3 -DCPRPGI -I./
#
# GFORTRAN
#FC :=gfortran
#FFLAGS := -O3 -ffree-form -ffree-line-length-none -D__GFORTRAN__ -I./
all: kernel.exe
SRCS := shr_kind_mod.F90 kgen_utils.F90 shr_spfn_mod.F90 micro_mg_utils.F90 wv_sat_methods.F90 micro_mg2_0.F90 micro_mg_cam.F90 kernel_micro_mg_tend.F90

kernel.exe: $(SRCS)
	$(FC) $(FFLAGS) -o $@ $^

clean:
	rm -f *.exe *.mod *.o
