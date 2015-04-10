#
# Intel 
  FC := ifort
#
# Knights Corner
#
# FFLAGS := -mmic -O3 -qopt-report=5 -fp-model fast -no-prec-div -no-prec-sqrt -I./
# FFLAGS := -O3 -fp-model precise -I./ -DCPRINTEL
# FFLAGS := -O3 -fp-model fast -xHost -no-prec-div -no-prec-sqrt  -I./ -DCPRINTEL
 FFLAGS := -check all -O2 -fp-model precise -mmic -I./ -DCPRINTEL
#  FFLAGS := -O3 -mmic -no-opt-dynamic-align -ftz -g -I./  -ip   -fimf-precision=low -fp-model fast -no-prec-div -no-prec-sqrt -override-limits -align array64byte -DCPRINTEL  -qopt-report=5
#
# Haswell
# 
# FFLAGS := -O3 -xCORE-AVX2 -no-prec-div -no-prec-sqrt  -I./ -DCPRINTEL
#  FFLAGS := -O3 -xCORE-AVX2 -no-opt-dynamic-align -ftz -g -I./  -ip -no-opt-dynamic-align -fp-model fast -no-prec-div -no-prec-sqrt -override-limits -align array64byte -qopt-report=5
#
# Sandy Bridge/Ivy Bridge
#
# FFLAGS := -O3 -xAVX -ftz -g -I./  -ip                                      -no-prec-div -no-prec-sqrt -override-limits                    -DCPRINTEL -qopt-report=5 
#  FFLAGS := -O3 -xAVX -ftz -g -I./  -ip -no-opt-dynamic-align -fp-model fast -no-prec-div -no-prec-sqrt -override-limits -align array64byte -qopt-report=5
#
# PGI 
# FC := pgf95
# FFLAGS := -fast -Minfo -DCPRPGI -I./ Mipa=fast,inline
#
# GFORTRAN
# FC :=gfortran
# FFLAGS := -O3 -ffree-form -ffree-line-length-none -D__GFORTRAN__ -I./
#
# Cray 
# FC := ftn
# FFLAGS := -O3 -N 255 -r mo -V -DCPRCRAY -h fp4 -I./
#

all: kernel.exe
SRCS := shr_kind_mod.F90 kgen_utils.F90 shr_spfn_mod.F90 micro_mg_utils.F90 wv_sat_methods.F90 micro_mg2_0.F90 micro_mg_cam.F90 kernel_micro_mg_tend.F90

kernel.exe: $(SRCS)
	$(FC) $(FFLAGS) -o $@ $^ -lm
#${HOME}/bin/mpi_trace_linux_x86_64/lib/libmpitrace.a -lbfd
#${HOME}/bin/amdlibm-3.1-lin64/lib/static/libamdlibm.a

clean:
	rm -f *.exe *.mod *.o *.optrpt *.lst
