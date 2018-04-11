#
# Makefile for COPY BENCHMARK
#

O_DIR = $(ARCH)/

ifeq ($(ARCH),SUN)
  AR       = /usr/xpg4/bin/ar
  FTN      = f90
  FBASE    =
  FOPT     = -fast -xtarget=ultra2 -xarch=v9
  FPREC    = -xtypemap=real:64,integer:32
  CPP      = cpp 
  CPPFLAGS  = -I. -DSUN -P
  CC       = cc
  CBASE    = -DSUN -DLWR_CASE_USCORE
  COPT     = -xtarget=ultra2 -xarch=v9
  LDFLAGS  = 
endif

ifeq ($(ARCH),HP)
  AR       = ar
  FTN      = f77
  FBASE    = +Oinfo
  FOPT     = +O2
  FPREC    = 
  CPP      = /lib/cpp 
  CPPFLAGS  = -I. -DHP -P
  CC       = /opt/ansic/bin/cc
  CBASE    =  +e -DHP -DLWR_CASE
  COPT     = 
endif

ifeq ($(ARCH),HP_IA64)
  AR       = ar
  FTN      = f90
  FBASE    = +Oinfo
  FOPT     = +O2
  FPREC    =
  CPP      = /lib/cpp
  CPPFLAGS  = -I. -DHP -P
  CC       = /opt/ansic/bin/cc
  CBASE    =  +e -DHP -DLWR_CASE_USCORE
  COPT     =
endif

ifeq ($(ARCH),SGI)
  AR       = ar
  FTN      = f77
  FBASE    =
  FOPT     = -O3 -mips4 -64
  FPREC    = -r8 -i4 
  CPP      = /lib/cpp 
  CPPFLAGS  = -DSGI -P
  CC       = cc
  CBASE    = -64 -DSGI -DLWR_CASE_USCORE
  COPT     = 
endif

ifeq ($(ARCH),CRAY_PVP)
  AR       = ar
  FTN      = f90
  FFLAGS   =
  FBASE    =
  FOPT     =
  FPREC    =
  CPP      = /opt/ctl/bin/cpp
  CPPFLAGS = -DCRAY -P
  CC       = cc
  CBASE    =
  COPT     =
endif

ifeq ($(ARCH),DEC)
  AR       = /usr/bin/ar
  FTN      = /usr/bin/f90
  FBASE    =
  FOPT     = -fast -arch ev6 -tune ev6
  FPREC    = -double_size 64
  CPP      = /usr/bin/cpp 
  CPPFLAGS  = -DDEC -P 
  CC       = cc
  CBASE    = -DDEC -DLWR_CASE_USCORE
  COPT     = 
endif

ifeq ($(ARCH),IBM)
  AR       = ar
  FTN      = xlf
  FBASE    =
  FOPT     = -O3 
  FPREC    =
  CPP      = cc -E
  CPPFLAGS = -I. -DIBM -P
  CC       = xlc
  CBASE    = -DIBM -DLWR_CASE
  COPT     = -O3
endif

ifeq ($(ARCH),DARWIN_INTEL)
  AR       = ar
  FTN      = ifort
  FBASE    =
  FOPT     = -O3
  FPREC    =
  CPP      = icc -E
  CPPFLAGS = -I. -DOSX -P       
  CC       = icc
  CBASE    = -DIBM -DLWR_CASE_USCORE
  COPT     = -O3
endif

ifeq ($(ARCH),DARWIN_GNU)
  AR       = ar
  FTN      = gfortran
  FBASE    =
  FOPT     = -O3
  FPREC    =
  CPP      = gcc-mp-4.7 -E
  CPPFLAGS = -I. -DOSX -P       
  CC       = gcc-mp-4.7
  CBASE    = -DIBM -DLWR_CASE_USCORE
  COPT     = -O3
endif

ifeq ($(ARCH),LINUX_INTEL)
  AR       = ar
  FTN      = ifort
  FBASE    =
  FOPT     = -O
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = icc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     =
  LDFLAGS  =
endif

ifeq ($(ARCH),LINUX_INTELO2Precise)
  AR       = ar
  FTN      = ifort
  FBASE    =
  FOPT     = -O2 -fp-model precise
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = icc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     =  -O2 -fp-model precise
  LDFLAGS  =
endif

ifeq ($(ARCH),LINUX_INTELMIC)
  AR       = ar
  FTN      = ifort
  FFLAGS   = -mmic
  FBASE    =
  FOPT     = -O -mmic
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = icc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     = -mmic
  LDFLAGS  = -mmic
endif

ifeq ($(ARCH),LINUX_AVX2_IFORT)
  AR       = ar
  FTN      = ifort
  FFLAGS   = -xCORE-AVX2
  FBASE    =
  FOPT     = -O2 -xCORE-AVX2
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = icc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     = 
  LDFLAGS  = 
endif

ifeq ($(ARCH),LINUX_AVX2_GNU)
  AR       = ar
  FTN      = gfortran
  FFLAGS   = -march=core-avx2
  FBASE    =
  FOPT     = -O2
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = gcc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     = 
  LDFLAGS  = 
endif
ifeq ($(ARCH),LINUX_AVX2_FLANG)
  AR       = ar
  FTN      = flang
  FFLAGS   = -march=native
  FBASE    =
  FOPT     = -O3 
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = clang
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE -DLINUX_PC
  COPT     = 
  LDFLAGS  = 
endif

ifeq ($(ARCH),LINUX_INTELMICO2Precise)
  AR       = ar
  FTN      = ifort
  FFLAGS   = -mmic
  FBASE    =
  FOPT     = -O -mmic -fp-model precise
  FPREC    =
  CPP      = cpp -traditional
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = icc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     = -mmic -fp-model precise
  LDFLAGS  = -mmic
endif


ifeq ($(ARCH),LINUX_PC)
  AR       = ar
  FTN      = pgf77
  FBASE    =
  FOPT     = -O
  FPREC    = 
  CPP      = /lib/cpp 
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = cc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     = 
  LDFLAGS  = 
endif
ifeq ($(ARCH),LINUX_PGI)
  AR       = ar
  FTN      = pgfortran
  FBASE    =
  FOPT     = -O
  FPREC    = 
  CPP      = /lib/cpp 
  CPPFLAGS  = -I. -DLINUX_PC -P
  CC       = cc
  CBASE    = -DLINUX_PC -DLWR_CASE_USCORE
  COPT     = 
  LDFLAGS  = 
endif

ifeq ($(ARCH),LINUX_IA64)
  AR       = ar
  FTN      = efc 
  FBASE    = 
  FOPT     = -O3 
  PREC     = 
  CPP      = /lib/cpp
  CPPFLAGS = -I. -DLINUX_IA64 -P -traditional
  CC       = ecc
  CBASE    = -DLINUX_IA64 
  COPT     = -O3
  LDFLAGS  =
endif  


ifeq ($(ARCH),LINUX_ARM)
  FTN      = armflang
  FBASE    =  -mcpu=native -I. -I../include
  FOPT     = -O3 -ffp-contract=fast
  FPREC    =
  CPP      = /lib/cpp
  CPPFLAG  = -D$(ARCH) -DTIMER_LIB -P -traditional
  CC       = armclang
  CBASE    = -DLWR_CASE_DUSCORE
  COPT     = -O3
  LDFLAGS  = -L${ARMPL_DIR}/lib -larmpl_lp64
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer
  VLIBS    =
endif

FFLAGS  = $(FBASE) $(FPREC) $(FOPT)
CFLAGS  = $(CBASE)          $(COPT) 


ismic=

ifeq ($(ARCH),LINUX_INTELMIC)
  ismic = yes
endif
ifeq ($(ARCH),LINUX_INTELMICMKL)
  ismic = yes
endif
ifeq ($(ARCH),LINUX_INTELMICO2Precise)
  ismic = yes
endif

LIBTIMER = 
ifdef ismic
  LIBTIMER = libtimer_mic.a
else
  LIBTIMER = libtimer.a
endif

HDRS = timer.h

FSOURCES= timer_clear.f timer_start.f timer_stop.f timer_time.f\
          timer_calls.f

FOBJECTS= timer_clear.o timer_start.o timer_stop.o timer_time.o\
          timer_calls.o

CSOURCES= timer_clock.c 

COBJECTS= timer_clock.o

$(FOBJECTS): $(HDRS)


ifeq ($(ARCH),dec)
timer_clock.o: timer_clock.fpp $(HDRS)
	$(CPP) $(CPPFLAGS) timer_clock.fpp > timer_clock.f
	$(FTN) $(FFLAGS) -c timer_clock.f
else
timer_clock.o: timer_clock.c
	$(CC) $(CFLAGS) -c timer_clock.c
endif

timer_clear.o: timer_clear.fpp $(HDRS)
	$(CPP) $(CPPFLAGS) timer_clear.fpp > timer_clear.f
	$(FTN) $(FFLAGS) -c timer_clear.f

timer_start.o: timer_start.fpp $(HDRS)
	$(CPP) $(CPPFLAGS) timer_start.fpp > timer_start.f
	$(FTN) $(FFLAGS) -c timer_start.f

timer_stop.o: timer_stop.fpp $(HDRS)
	$(CPP) $(CPPFLAGS) timer_stop.fpp > timer_stop.f
	$(FTN) $(FFLAGS) -c timer_stop.f

timer_time.o: timer_time.fpp $(HDRS)
	$(CPP) $(CPPFLAGS) timer_time.fpp > timer_time.f
	$(FTN) $(FFLAGS) -c timer_time.f

timer_calls.o: timer_calls.fpp $(HDRS)
	$(CPP) $(CPPFLAGS) timer_calls.fpp > timer_calls.f
	$(FTN) $(FFLAGS) -c timer_calls.f

ifdef ismic
libtimer_mic.a: $(FOBJECTS) $(COBJECTS)
	$(AR) ruvs $(LIBTIMER) $(FOBJECTS) $(COBJECTS)
else
libtimer.a: $(FOBJECTS) $(COBJECTS)
	$(AR) ruvs $(LIBTIMER) $(FOBJECTS) $(COBJECTS)
endif

clean:
	rm -rf *.o libtimer.a *.f libtimer_mic.a !
