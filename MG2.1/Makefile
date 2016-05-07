# Makefile for KGEN-generated kernel
# #
# Intel 
FC := ifort
#
# Knights Corner
#
 FFLAGS := -mmic -O3 -qopt-report=5 -fp-model fast -no-prec-div -no-prec-sqrt -I./
NODE := $(shell uname -n)

# FFLAGS := -O3 -fp-model precise -I./ -DCPRINTEL
# FFLAGS := -O3 -fp-model fast -xHost -no-prec-div -no-prec-sqrt  -I./ -DCPRINTEL
#FFLAGS := -O2 -fp-model precise -xHost -I./ -DCPRINTEL
#
# Haswell
# 
# FFLAGS := -O3 -xCORE-AVX2 -no-prec-div -no-prec-sqrt  -I./ -DCPRINTEL
# FFLAGS := -O3 -openmp -xCORE-AVX2 -no-prec-div -no-prec-sqrt  -I./ -DCPRINTEL
#
# Sandy Bridge/Ivy Bridge

# FFLAGS := -O3 -xAVX -g -I./ -no-prec-div -no-prec-sqrt -override-limits -qopt-report=5 
#-prof-use -prof-dir/global/u1/c/ckerr/tests/xeon/MG2.1_NCAR
#-prof-gen

# Use of these option below gives a 6-percent improvement in performance, but causes minor differences in answers
# FFLAGS := -O3 -xAVX -ftz -g -I./     -fimf-precision=low -no-prec-sqrt -no-prec-div -override-limits -qopt-report=5 \

#  FFLAGS := -O3 -xAVX -ftz -g -I./ -ipo4 -ipo-jobs4 -fimf-precision=low -no-prec-sqrt -no-prec-div -override-limits -qopt-report=5 -openmp 
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
# FFLAGS := -O2 -N 255 -r mo -V  -h fp4  -G1
#

run: kernel.exe
	ssh ${NODE}-mic1 "cd ${PWD};  ./kernel.exe"

SRCS := shr_kind_mod.F90 kgen_utils.F90 shr_spfn_mod.F90 micro_mg_utils.F90 wv_sat_methods.F90 micro_mg2_0.F90 micro_mg_cam.F90 kernel_micro_mg_tend.F90


kernel.exe: $(SRCS)
	$(FC) $(FFLAGS) -o $@ $^ -lm 

#${HOME}/bin/mpi_trace_linux_x86_64/lib/libmpitrace.a -lbfd

clean:
	rm -f *.exe *.mod *.o *.optrpt *.lst
