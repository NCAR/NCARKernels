
FC := ifort
SRC := ./kernel_gradient_sphere.F90
EXE := ${SRC:.F90=.exe}
FLAGS :=  ${FFLAGS}

all: run build

run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC}

clean:
	rm -f *.exe
