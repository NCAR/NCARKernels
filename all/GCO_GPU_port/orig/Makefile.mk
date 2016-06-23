FC := ifort
FFLAGS := -O3 -xHost -inline-level=2 -I./
#FC := pgfortran
#FFLAGS := -O2 -I./
#FC := gfortran
#FFLAGS := -O3 -ffree-line-length-none -I./
SRCS := gco_para_omp.f90 gco_cpu_omp.f90 gco_program_omp.f90
EXE := ./kernel.exe

FLAGS := ${FFLAGS}

all: run build

run: ${EXE}
	taskset 0x00000010 ${EXE}

build: ${EXE}

${EXE}: ${SRCS} clean
	${FC} ${FLAGS} -o ${EXE} ${SRCS}

clean:
	rm -f *.exe *.mod *.o
