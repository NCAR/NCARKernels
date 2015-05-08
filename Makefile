# Makefile created by mkmf $Id: mkmf,v 18.0.18.4 2012/12/04 15:24:15 Seth.Underwood Exp $ 

SRCROOT = /glade/u/home/ck/tests/xeon/uniformRNG/

CPPDEFS = -DDSFMT_MEXP=19937 -DHAVE_SSE2


include /glade/u/home/ck/tests/xeon/uniformRNG/intel.mk


.DEFAULT:
	-echo $@ does not exist.
all: random.exe
dSFMT.o: ./dSFMT_F03/dSFMT.c
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c	./dSFMT_F03/dSFMT.c
dSFMT_interface.o: ./dSFMT_F03/dSFMT_interface.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./dSFMT_F03/dSFMT_interface.F90
dSFMT_utils.o: ./dSFMT_F03/dSFMT_utils.c ./dSFMT_F03/dSFMT.h
	$(CC) $(CPPDEFS) $(CPPFLAGS) $(CFLAGS) $(OTHERFLAGS) -c -I./dSFMT_F03	./dSFMT_F03/dSFMT_utils.c
kissvec_mod.o: ./KISSVEC/kissvec_mod.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./KISSVEC/kissvec_mod.F90
main.o: ./DRIVERS/main.F90 random_number_mod.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./DRIVERS/main.F90
mersennetwister_mod.o: ./MT19937/mersennetwister_mod.F90
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./MT19937/mersennetwister_mod.F90
random_number_mod.o: ./DRIVERS/random_number_mod.F90 kissvec_mod.o mersennetwister_mod.o dSFMT_interface.o
	$(FC) $(CPPDEFS) $(CPPFLAGS) $(FPPFLAGS) $(FFLAGS) $(OTHERFLAGS) -c	./DRIVERS/random_number_mod.F90
./dSFMT.h: ./dSFMT_F03/dSFMT.h
	cp ./dSFMT_F03/dSFMT.h .
./dSFMT_utils.c: ./dSFMT_F03/dSFMT_utils.c
	cp ./dSFMT_F03/dSFMT_utils.c .
./mersennetwister_mod.F90: ./MT19937/mersennetwister_mod.F90
	cp ./MT19937/mersennetwister_mod.F90 .
./dSFMT.c: ./dSFMT_F03/dSFMT.c
	cp ./dSFMT_F03/dSFMT.c .
./random_number_mod.F90: ./DRIVERS/random_number_mod.F90
	cp ./DRIVERS/random_number_mod.F90 .
./main.F90: ./DRIVERS/main.F90
	cp ./DRIVERS/main.F90 .
./kissvec_mod.F90: ./KISSVEC/kissvec_mod.F90
	cp ./KISSVEC/kissvec_mod.F90 .
./dSFMT_interface.F90: ./dSFMT_F03/dSFMT_interface.F90
	cp ./dSFMT_F03/dSFMT_interface.F90 .
SRC = ./DRIVERS/main.F90 ./KISSVEC/kissvec_mod.F90 ./MT19937/mersennetwister_mod.F90 ./dSFMT_F03/dSFMT.c ./dSFMT_F03/dSFMT_interface.F90 ./dSFMT_F03/dSFMT_utils.c ./DRIVERS/random_number_mod.F90 ./dSFMT_F03/dSFMT.h
OBJ = main.o kissvec_mod.o mersennetwister_mod.o dSFMT.o dSFMT_interface.o dSFMT_utils.o random_number_mod.o
OFF = ./dSFMT_F03/dSFMT.h ./dSFMT_F03/dSFMT_utils.c ./MT19937/mersennetwister_mod.F90 ./dSFMT_F03/dSFMT.c ./DRIVERS/random_number_mod.F90 ./DRIVERS/main.F90 ./KISSVEC/kissvec_mod.F90 ./dSFMT_F03/dSFMT_interface.F90
clean: neat
	-rm -f .random.exe.cppdefs $(OBJ) random.exe
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
random.exe: $(OBJ) 
	$(LD) $(OBJ) -o random.exe  $(LDFLAGS)
