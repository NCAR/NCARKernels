# Makefile for KGEN-generated kernel
#
FC := ifort
SRC := ./kernel_binterp.F90
EXE := ${SRC:.F90=.exe}
FFLAGS :=  -mmic -O3 -ftz  -ip  -no-fp-port -fp-model fast -no-prec-div -no-prec-sqrt -override-limits -align array64byte -DCPRINTEL -mkl
FLAGS := ${FFLAGS}
NODE := $(shell uname -n)


run: ${EXE}
	ssh ${NODE}-mic1 "cd ${PWD}; ./${EXE}"

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe *.mod
