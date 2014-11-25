export

SHELL = /bin/sh
 
ARCH  = LINUX

srcdir = .
top_srcdir = .

prefix = .
exec_prefix = ${prefix}

bindir = ${exec_prefix}

NETCDF_ROOT   = $(NETCDF_DIR)
NETCDF_LIB     = $(NETCDF)
#NETCDF_LIB     = $(NETCDF) $(HDF5)
NETCDF_INCLUDE = -I$(NETCDF_DIR)/include
LIBS+=${NETCDF_LIB}

program_transform_name = s,x,x,

MODOPT   = -I
MODULES  = ./

INCLUDE  = ../include
INCLUDES = $(MODOPT)$(MODULES) -I$(INCLUDE) $(NETCDF_INCLUDE) 

F90=mpiifort
F90FLAGS = -fpp -g -traceback ${INCLUDES} -DHAVE_FAST_MATH_LIB -DHAVE_MKL -openmp
#F90FLAGS = -fpp -g -traceback -Wl,-stack_size,0x10000000 ${INCLUDES} -DHAVE_FAST_MATH_LIB -DHAVE_MKL
# Production
F90FLAGS += -O3 -mmic -qopt-report=5 -fp-model fast
F90FLAGS_NOOPT = -O1 -mmic -fpp -g -traceback ${INCLUDES} -DHAVE_FAST_MATH_LIB -DHAVE_MKL -openmp
# Debugging
#F90FLAGS += -check bounds,uninit,pointers,stack -warn all -fpe0 -ftrapuv
LDFLAGS = -O3 -mmic -openmp -traceback 
LIBS += -lmkl_rt
SRCDIRS = src

all:
	@for DIR in $(SRCDIRS) ;\
	  do \
	    back=`pwd`; \
	    cd $$DIR ;\
	    $(MAKE) ; status=$$? ; \
	    if [ $$status != 0 ] ; then \
	      echo "Exit status from make was $$status" ; exit $$status ; \
	    fi ; \
	    cd $$back ; \
	  done 

clean:
	@for DIR in $(SRCDIRS) ;\
	  do \
	  (cd $$DIR ;\
	  $(MAKE) clean ; if [ $$? != 0 ] ; then \
	        echo "Exit status from make was $$?" ; exit 1 ; fi ;) ;\
	  done
	-rm -f config.cache

tar:
	@tarfile=../echam5.f90.`date +%y%m%d`.taz ; gtar zcvf $$tarfile \
	`ls */*.f90 */*.[fhc] */*inc */Makefile Makefile.in Makefile run/hjob*`

