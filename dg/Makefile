# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := ./dg_kernel.F90
EXE := ${SRC:.F90=.exe}
PARAMS := -DSET_NX=4 -DSET_NIT=1000 -DSET_NELEM=1200
FLAGS := -mmic ${FFLAGS} ${PARAMS}
NODE := $(shell uname -n)


run: ${EXE}
	ssh ${NODE}-mic1 "cd ${PWD}; ${EXE}"

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
