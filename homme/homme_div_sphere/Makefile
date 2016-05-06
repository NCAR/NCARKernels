
SRC := ./kernel_divergence_sphere.F90
EXE := ${SRC:.F90=.exe}

#
# PGI
#
#FC := pgf95
#FFLAGS := -O3
#
# Intel
#
 FC := ifort
# FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
# FFLAGS := -O3 -xCORE-AVX2 -qopt-report=5 -fp-model fast
 FFLAGS := -O3 -xMIC-AVX512 -qopt-report=5 -fp-model fast
#
# GFORTRAN
# 
# FC :=gfortran
# FFLAGS := -O3 -ffree-form -ffree-line-length-none -D__GFORTRAN__ -I./
# #
#
# Cray 
#
#  FC := ftn
#  FFLAGS := -O2
#

FLAGS := ${FFLAGS}

all: build

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
