# Makefile for KGEN-generated kernel
FC := ifort
SRC := ./dg_kernel.F90
EXE := ${SRC:.F90=.exe}
PARAMS := -DSET_NX=4 -DSET_NIT=1000 -DSET_NELEM=1200
FLAGS := -xAVX ${FFLAGS} ${PARAMS}

all: run build

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
