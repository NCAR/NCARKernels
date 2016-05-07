# Makefile for KGEN-generated kernel
FC := ifort
SRC := ./kernel_laplace_sphere_wk.F90
EXE := ${SRC:.F90=.exe}
#FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
FFLAGS := -O3 -xMIC-AVX512 -qopt-report=5 -fp-model fast
#FFLAGS := -O3 -qopt-report=5 -fp-model fast
FLAGS := ${FFLAGS}

run: ${EXE}
	numactl --membind 1 ./${EXE}

${EXE}: build

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
