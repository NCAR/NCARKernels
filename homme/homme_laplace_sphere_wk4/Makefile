FC := ifort
SRC := ./kernel_laplace_sphere_wk.F90
EXE := ${SRC:.F90=.exe}

# Cases
#FFLAGS := -O3 -g -debug inline-debug-info -mmic #-Fa
#FFLAGS := -O3 -g -debug inline-debug-info -mmic -fp-model fast=2
#FFLAGS := -O3 -g -debug inline-debug-info -mmic -fp-model precise -Fa
#FFLAGS := -O3 -g -debug inline-debug-info -mmic -fp-model precise -no-vec
#FFLAGS := -O3 -g -debug inline-debug-info -mmic -fp-model precise -mP2OPT_hpo_matrix_opt_framework=0

FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast=2
#FFLAGS := -O3 -qopt-report=5 -fp-model fast
FLAGS := ${FFLAGS}

all: build 

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
