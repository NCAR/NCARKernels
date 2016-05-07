# Makefile for KGEN-generated kernel
#
FC := ifort
FFLAGS := -O3 -xMIC-AVX512 -I./
#FC := pgfortran
#FFLAGS := -O3 -I./
#FC := gfortran
#FFLAGS := -O3 -ffree-line-length-none -I./
SRCS := rdtsc.s mo_kind.f90 kgen_utils.f90 mo_rrtm_params.f90 mo_lrtm_setup.f90 mo_lrtm_kgs.f90 mo_lrtm_gas_optics.f90 kernel_driver.f90
EXE := ./kernel.exe

FLAGS := ${FFLAGS}

run: ${EXE}
	numactl --membind 1  ./${EXE}

build: ${EXE}

${EXE}: ${SRCS} clean
	${FC} ${FLAGS} -o ${EXE} ${SRCS}

clean:
	rm -f *.exe *.mod *.o
