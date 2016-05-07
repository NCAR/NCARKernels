# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := ./kernel_laplace_sphere_wk.F90
EXE := ${SRC:.F90=.exe}
#FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
FFLAGS := -xAVX -O3 -qopt-report=5 -fp-model fast
FLAGS := ${FFLAGS}

run: build
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
