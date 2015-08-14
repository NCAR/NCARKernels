FC := ifort
SRC := ./kernel_rtrnmc.F90
EXE := ${SRC:.F90=.exe}
FFLAGS := -O4 -xAVX  -I./ -no-prec-div -no-prec-sqrt -override-limits -qopt-report=5 -align array64byte -fimf-precision=low 
FLAGS := ${FFLAGS}

all: build 
run: ${EXE}
	${EXE}

build: ${SRC} clean
	${FC} ${FLAGS} -o ${EXE} ${SRC} -lm # ${HOME}/bin/mpi_trace_linux_x86_64/lib/libmpitrace.a -lbfd

clean:
	rm -f *.exe *.mod *.optrpt
