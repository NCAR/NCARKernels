# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := ./kernel_reftra_sw.F90
EXE := ${SRC:.F90=.exe}
#FLAGS := -O3 -mmic -vec-report7  -fp-model precise
#FLAGS := -O3 -fp-model precise
FLAGS := -xCORE-AVX2 ${FFLAGS}

all: run build

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe *.mod
