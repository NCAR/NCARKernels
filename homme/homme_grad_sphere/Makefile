
FC := ifort
SRC := ./kernel_gradient_sphere.F90
EXE := ${SRC:.F90=.exe}
#FFLAGS := -O3 -qopt-report=5 -fp-model fast
FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
FLAGS :=  ${FFLAGS}

all: build

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
