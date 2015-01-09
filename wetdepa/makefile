.SUFFIXES:
.SUFFIXES: .F90 .f90 .o
FPP = cpp
FPPFLAGS = -I. -traditional -P

FC = ifort
#BASE = -mmic -vec-report=6 -fp-model fast -ftz -traceback
BASE = -qopt-report=5 -ftz -fp-model fast -traceback

# -02
#FFLAGS = -O2 $(BASE)

# -O3
#FFLAGS = -O3 $(BASE)

# -O3 -fast
# FFLAGS = -O3 -fast -mmic $(BASE)
FFLAGS = -O2 -xAVX $(BASE)

#
# Cray compiler
#
#FC := ftn
#FFLAGS := -O2 -N 255


OBJS  = wetdepa_driver.o wetdep.o kinds_mod.o params.o shr_const_mod.o shr_kind_mod.o
OBJS0 = wetdepa_driver.o wetdep_orig.o kinds_mod.o params.o shr_const_mod.o shr_kind_mod.o

all: wetdepa_driver wetdepa_driver_v0

.F90.o:
	$(FC) $(FFLAGS) -c $<

#.F90.f90:
#	$(FPP) $(FPPFLAGS) $< >$*.f90 

wetdepa_driver: $(OBJS)
	$(FC) $(FFLAGS) -o wetdepa_driver $(OBJS)

wetdepa_driver_v0: $(OBJS0)
	$(FC) $(FFLAGS) -o wetdepa_driver_v0 $(OBJS0)

shr_const_mod.o: shr_kind_mod.o
params.o: shr_kind_mod.o shr_const_mod.o
wetdep.o: wetdep.f90 kinds_mod.o params.o
wetdep.f90: wetdep.F90
wetdep_orig.o: wetdep_orig.f90 kinds_mod.o params.o 
wetdep_orig.f90: wetdep_orig.F90
wetdepa_driver.o: wetdep.o
wetdepa_orig_driver.o: wetdep_orig.o

clean:
	rm -rf *.o *.mod wetdepa_driver wetdepa_driver_v0
