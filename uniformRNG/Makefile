FC := ifort
SRC := kissvec_mod.F90 uniform.F90
EXE := uniform.exe
#FFLAGS := -O3 -xMIC-AVX512 -fp-model fast
#FFLAGS := -O3 -xCORE-AVX2 -fp-model fast

#FFLAGS := -O3 -mmic -fp-model fast
FFLAGS := -O3 -qopt-report=5 -fp-model fast
FLAGS := ${FFLAGS}

all: run build 

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe *.mod
