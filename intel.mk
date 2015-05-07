# template for the Intel fortran compiler
#
############
# commands #
############
FC = ifort 
CC = icc
LD = ifort 

#########
# flags #
#########

INCLUDE := $(MKLROOT)/include
FPPFLAGS := -fpp -Wp,-w 

 FFLAGS += -O3 -xAVX -fp-model fast -i4 -r8 -I$(INCLUDE) -I./      -no-prec-div -no-prec-sqrt -override-limits -qopt-report=5
#FFLAGS += -O3 -xAVX -fp-model fast -i4 -r8 -I$(INCLUDE) -I./ -ipo -no-prec-div -no-prec-sqrt -override-limits -qopt-report=5

CFLAGS := -D__IFC 
CFLAGS += -O3 $(CFLAGS)

LDFLAGS := 

# start with blank LIBS
LIBS := -mkl

LIBS += 
LDFLAGS += $(LIBS)

#---------------------------------------------------------------------------
# you should never need to change any lines below.

# see the MIPSPro F90 manual for more details on some of the file extensions
# discussed here.
# this makefile template recognizes fortran sourcefiles with extensions
# .f, .f90, .F, .F90. Given a sourcefile <file>.<ext>, where <ext> is one of
# the above, this provides a number of default actions:

# make <file>.opt	create an optimization report
# make <file>.o		create an object file
# make <file>.s		create an assembly listing
# make <file>.x		create an executable file, assuming standalone
#			source
# make <file>.i		create a preprocessed file (for .F)
# make <file>.i90	create a preprocessed file (for .F90)

# The macro TMPFILES is provided to slate files like the above for removal.

RM = rm -f
SHELL = /bin/csh -f
TMPFILES = .*.m *.B *.L *.i *.i90 *.l *.s *.mod *.opt

.SUFFIXES: .F .F90 .H .L .T .f .f90 .h .i .i90 .l .o .s .opt .x

.f.L:
	$(FC) $(FFLAGS) -c -listing $*.f
.f.opt:
	$(FC) $(FFLAGS) -c -opt_report_level max -opt_report_phase all -opt_report_file $*.opt $*.f
.f.l:
	$(FC) $(FFLAGS) -c $(LIST) $*.f
.f.T:
	$(FC) $(FFLAGS) -c -cif $*.f
.f.o:
	$(FC) $(FFLAGS) -c $*.f
.f.s:
	$(FC) $(FFLAGS) -S $*.f
.f.x:
	$(FC) $(FFLAGS) -o $*.x $*.f *.o $(LDFLAGS)
.f90.L:
	$(FC) $(FFLAGS) -c -listing $*.f90
.f90.opt:
	$(FC) $(FFLAGS) -c -opt_report_level max -opt_report_phase all -opt_report_file $*.opt $*.f90
.f90.l:
	$(FC) $(FFLAGS) -c $(LIST) $*.f90
.f90.T:
	$(FC) $(FFLAGS) -c -cif $*.f90
.f90.o:
	$(FC) $(FFLAGS) -c $*.f90
.f90.s:
	$(FC) $(FFLAGS) -c -S $*.f90
.f90.x:
	$(FC) $(FFLAGS) -o $*.x $*.f90 *.o $(LDFLAGS)
.F.L:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -listing $*.F
.F.opt:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -opt_report_level max -opt_report_phase all -opt_report_file $*.opt $*.F
.F.l:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c $(LIST) $*.F
.F.T:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -cif $*.F
.F.f:
	$(FC) $(FPPFLAGS) $(CPPDEFS) -EP $*.F > $*.f
.F.i:
	$(FC) $(FPPFLAGS) $(CPPDEFS) -P $*.F
.F.o:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c $*.F
.F.s:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -S $*.F
.F.x:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -o $*.x $*.F *.o $(LDFLAGS)
.F90.L:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -listing $*.F90
.F90.opt:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -opt_report_level max -opt_report_phase all -opt_report_file $*.opt $*.F90
.F90.l:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c $(LIST) $*.F90
.F90.T:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -cif $*.F90
.F90.f90:
	$(FC) $(FPPFLAGS) $(CPPDEFS) -EP $*.F90 > $*.f90
.F90.i90:
	$(FC) $(FPPFLAGS) $(CPPDEFS) -P $*.F90
.F90.o:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c $*.F90
.F90.s:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -c -S $*.F90
.F90.x:
	$(FC) $(FFLAGS) $(FPPFLAGS) $(CPPDEFS) -o $*.x $*.F90 *.o $(LDFLAGS)
