FC := ifort
SRC := ./kernel_laplace_sphere_wk.F90
EXE := ${SRC:.F90=.exe}
#FFLAGS := -O3 -mmic
#FFLAGS := -O3 -mmic -fp-model fast=2
#FFLAGS := -O3 -mmic -fp-model precise
#FFLAGS := -O3 -mmic -fp-model precise -no-vec
#FFLAGS := -O3 -mmic -fp-model precise -mP2OPT_hpo_matrix_opt_framework=0

#FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast=2
FFLAGS := -xMIC-AVX512 -O3 -qopt-report=5 -fp-model fast
FLAGS := ${FFLAGS}

all:  build 

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
