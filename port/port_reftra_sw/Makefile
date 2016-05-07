# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := ./kernel_reftra_sw.F90
EXE := ${SRC:.F90=.exe}
FLAGS := -O3 -mmic -vec-report7  -fp-model precise
#FLAGS := -O3 -fp-model precise
FLAGS := ${FFLAGS}
NODE := $(shell uname -n)


run: ${EXE}
	ssh ${NODE}-mic1 "cd ${PWD}; ./${EXE}"

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe *.mod
