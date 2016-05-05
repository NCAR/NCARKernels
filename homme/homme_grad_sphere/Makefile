
SRC := ./kernel_gradient_sphere.F90
EXE := ${SRC:.F90=.exe}

#
# PGI
#
# FC := pgf95
# FFLAGS := -O3
#
# Intel
#
 FC := ifort
# FFLAGS := -O3 -xCORE-AVX2 -qopt-report=5 -fp-model fast
# FFLAGS := -O3 -align array64byte  -xCORE-AVX2 -qopt-report=5 -fp-model fast=2
# FFLAGS := -O3  -xCORE-AVX2 -qopt-report=5 -fp-model fast=2
# FFLAGS := -O3 -align array64byte -xAVX -fp-model fast=2
FFLAGS := -O3 -align array64byte -mmic -qopt-report=5 -fp-model fast=2
# FFLAGS := -O3 -xAVX -qopt-report=5 -fp-model fast=2
#
# GFORTRAN
# 
#FC :=gfortran
#FFLAGS := -O3 -ffree-form -ffree-line-length-none -D__GFORTRAN__ -I./
#
#  Cray
# FC := ftn
# FFLAGS := -O2
#

FLAGS :=  ${FFLAGS}

all: build

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
