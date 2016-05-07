# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := ./kernel_cldprmc.F90
EXE := ${SRC:.F90=.exe}
FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
#FFLAGS := -fp-model precise
FLAGS := ${FFLAGS}
NODE := $(shell uname -n)


run: ${EXE}
	ssh ${NODE}-mic1 "cd ${PWD}; ./${EXE}"

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
