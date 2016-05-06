FC := ifort
SRC := ./kernel_rtrnmc.F90
EXE := ${SRC:.F90=.exe}
#FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
#FFLAGS := -fp-model precise
FLAGS := -xCORE-AVX2 ${FFLAGS}

all: build 
run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
