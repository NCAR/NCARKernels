
include ./Macros

OBJS = kernel_laplace_sphere_wk.o kgen_utils.o

all: clean build 

build: ${OBJS}
	${FC} ${LD_FLAGS} -o kernel.exe $^

kernel_laplace_sphere_wk.o: kernel_laplace_sphere_wk.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kgen_utils.o: kgen_utils.F90
	${FC} ${FC_FLAGS} -c -o $@ $<



clean:
	rm -f *.exe *.o *.mod
