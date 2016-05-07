# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := kissvec_mod.F90 uniform.F90
EXE := uniform.exe
FFLAGS := -O3 -xMIC-AVX512 -fp-model fast
#FFLAGS := -O3 -xCORE-AVX2 -fp-model fast -mkl

#FFLAGS := -O3 -mmic -fp-model fast -mkl
#FFLAGS := -O3 -qopt-report=5 -fp-model fast -mkl
FLAGS := ${FFLAGS}

run: ${EXE}
	numactl --membind 1  ./${EXE}

${EXE}: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC} $(LIBS)

clean:
	rm -f *.exe *.mod
