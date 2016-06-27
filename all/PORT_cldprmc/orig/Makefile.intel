FC := ifort
SRC := ./kernel_cldprmc.F90
EXE := ${SRC:.F90=.exe}
#FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
#FFLAGS := -fp-model precise
FLAGS := ${FFLAGS}

run: build
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe *.mod
