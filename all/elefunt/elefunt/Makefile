#
# Makefile for ELEFUNT BENCHMARKS
#

O_DIR = $(ARCH)/
SUFF = 

ifeq ($(ARCH),SUN)
  FTN      = f90
  FBASE    =
  FOPT     = -O3 -xtarget=ultra2 -xarch=v9
  FPREC    = 
  CPP      = cpp 
  CPPFLAG  = -D$(ARCH) -P
  CC       = cc
  CBASE    = 
  COPT     = -xtarget=ultra2 -xarch=v9
  LDFLAG  = -xtarget=ultra2 -xarch=v9
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer 
  VLIBS    =
endif

ifeq ($(ARCH),HP)
  FTN      = f77
  FBASE    = +Oinfo +Oreport
  FOPT     = +Ocache_pad_common +O3 +Ovectorize +Oinfo +Onofltacc \
             +DS2.0a +DA2.0N +Odataprefetch +Olibcalls
  FPREC    = 
  FFLAG_MACHAR = $(FBASE) $(FOPT) $(FPREC)
  CPP      = /lib/cpp 
  CPPFLAG  = -D$(ARCH) -P
  CC       = cc
  CBASE    = 
  COPT     = 
  LDFLAG  = 
  TIMER_LIB  = -Wl,-L../lib/$(O_DIR) -ltimer 
  VLIBS    = -lail
endif

ifeq ($(ARCH),HP_IA64)
  FTN          = f90
  FBASE        = +DSitanium
  FOPT         = +O3 +Ocache_pad_common +Odataprefetch +Olibcalls
  FPREC        =
  FFLAG_MACHAR = $(FBASE) $(FOPT) $(FPREC)
  CPP          = /lib/cpp
  CPPFLAG      = -D$(ARCH) -P
  CC           = cc
  CBASE        =
  COPT         =
  LDFLAG       =
  TIMER_LIB    = -Wl,-L../lib/$(O_DIR) -ltimer
  VLIBS        = 
endif

ifeq ($(ARCH),SGI)
  FTN      = f77
  FBASE    = -i4
  FOPT     = -O3 -64
  FPREC    = 
  FFLAG_MACHAR = -i4 -64 -mips4 
  CPP      = /lib/cpp 
  CPPFLAG  = -D$(ARCH) -P
  CC       = cc
  CBASE    = 
  COPT     = 
  LDFLAG  = $(FBASE) $(FOPT)
  VLIBS    =
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer 
endif

ifeq ($(ARCH),CRAY_PVP)
  FTN      = f90
  FBASE    = 
  FOPT     = -dp
  FPREC    = 
  FFLAG_MACHAR = -dp -O0
  CPP      = /opt/ctl/bin/cpp 
  CPPFLAG  = -D$(ARCH) -DSINGLE_PREC_ONLY -P
  CC       = cc
  CBASE    = 
  COPT     = 
  LDFLAG  = $(FBASE) $(FOPT)
  VLIBS    =
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer 
endif

ifeq ($(ARCH),DEC)
  AR       = /usr/bin/ar
  FTN      = /usr/bin/f90
  FBASE    = -fpe4
  FOPT     = -O4 -arch ev6 -tune ev6 
  FPREC    = -double_size 64
  CPP      = /usr/bin/cpp 
  FFLAG_MACHAR = -fpe2 -g $(FPREC)
  CPPFLAG  = -D$(ARCH) -P
  CC       = cc
  CBASE    = -DLWR_CASE -D$(ARCH)
  COPT     = 
  TIMER_LIB=
  TIMER_SOURCE = timer.fpp
  TIMER_OBJECT = timer.o
endif

ifeq ($(ARCH),IBM)
  AR       = ar
  FTN      = xlf
  FBASE    =
  FOPT     = -O3 -qhot -qarch=auto -qtune=auto
  FPREC    =
  CPP      = /usr/lib/cpp
  FFLAG_MACHAR =
  CPPFLAG  = -D$(ARCH) -P
  CC       = xlc
  CBASE    = -D$(ARCH) -DLWR_CASE
  COPT     = -O3
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
  TIMER_SOURCE =
  TIMER_OBJECT = 
endif

ifeq ($(ARCH),LINUX_INTEL)
  AR           = ar
  FTN          = ifort
  FOPT         = -O3 -xavx
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
  SUFF         =_host
endif


ifeq ($(ARCH),NEC)
  AR           = ar
  FTN          = nfort
  FOPT         = -O4 -mno-vector
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAGS     = -I. -DNEC -P
  CC           = ncc
  CBASE        = -DNEC -DLWR_CASE_USCORE
  COPT         = -O4 -mno-vector -g
  TIMER_LIB    = -L../timer -ltimer
  SUFF         =_host
endif


ifeq ($(ARCH),LINUX_INTELO2Precise)
  AR           = ar
  FTN          = ifort
  FOPT         = -O2 -fp-model precise -xHost
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g -fp-model precise -xHost
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
  SUFF         =_hostO2precise 
endif


ifeq ($(ARCH),LINUX_INTELMIC)
  AR           = ar
  FTN          = ifort -mmic
  FOPT         = -O3 -mmic
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g -mmic
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer_mic
  SUFF         =_mic
endif

ifeq ($(ARCH),LINUX_INTELMICO2Precise)
  AR           = ar
  FTN          = ifort -mmic
  FOPT         = -O2 -mmic -fp-model precise
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g -mmic -fp-model precise
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer_mic
  SUFF         =_micO2precise
endif

ifeq ($(ARCH),LINUX_INTELMKL)
  AR           = ar
  FTN          = ifort -Wl,--start-group  $(MKLROOT)/lib/intel64/libmkl_intel_ilp64.a $(MKLROOT)/lib/intel64/libmkl_sequential.a $(MKLROOT)/lib/intel64/libmkl_core.a -Wl,--end-group -lpthread -lm
  FOPT         = -O3 -xavx
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
  SUFF         =_hostmkl
endif

ifeq ($(ARCH),LINUX_AVX2_IFORT)
  AR           = ar
  FTN          = ifort 
  FOPT         = -O3 -xCORE-AVX2
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer -L/glade/u/apps/ch/opt/glibc/2.17/gnu/4.7.2/lib -lm -lc
  SUFF         =
endif

ifeq ($(ARCH),LINUX_AVX2_GNU)
  AR           = ar
  FTN          = gfortran
  FOPT         = -O3 -march=core-avx2
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer -L/glade/u/apps/ch/opt/glibc/2.17/gnu/4.7.2/lib -lm -lc
  SUFF         =
endif

ifeq ($(ARCH),LINUX_AVX2_FLANG)
  AR           = ar
  FTN          = flang
  FOPT         = -O3 -march=native -O3 -ffp-contract=fast
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = clang
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
  SUFF         =
endif

ifeq ($(ARCH),LINUX_BDW_NOMKL)
  AR           = ar
#  FTN          = ifort -Wl,--start-group  $(MKLROOT)/lib/intel64/libmkl_intel_ilp64.a $(MKLROOT)/lib/intel64/libmkl_sequential.a $(MKLROOT)/lib/intel64/libmkl_core.a -Wl,--end-group -lpthread -lm
  FTN          = ifort 
  FOPT         = -O3 -xCORE-AVX2
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
  SUFF         = _nomkl
endif

ifeq ($(ARCH),LINUX_INTELMICMKL)
  AR           = ar
  FTN          = ifort -mmic -Wl,--start-group  $(MKLROOT)/lib/mic/libmkl_intel_ilp64.a $(MKLROOT)/lib/mic/libmkl_sequential.a $(MKLROOT)/lib/mic/libmkl_core.a -Wl,--end-group -lpthread -lm
  FOPT         = -O3 -mmic
  FFLAG_MACHAR = -O0
  CPP          = cpp -traditional
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g -mmic
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer_mic
  SUFF         =_micmkl 
endif


ifeq ($(ARCH),DARWIN_INTEL)
  AR           = ar 
  FTN          = ifort
  FOPT         = -O3 -xavx
  FFLAG_MACHAR = -O0
  CPP          = cpp
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
endif

ifeq ($(ARCH),DARWIN_INTELMKL)
  AR           = ar 
  FTN          = ifort $(MKLROOT)/lib/libmkl_intel_ilp64.a $(MKLROOT)/lib/libmkl_sequential.a $(MKLROOT)/lib/libmkl_core.a -lpthread -lm
  FOPT         = -O3 -xavx
  FFLAG_MACHAR = -O0
  CPP          = cpp
  CPPFLAG      = -D$(ARCH) -P
  CC           = icc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
endif


ifeq ($(ARCH),DARWIN_GNU)
  AR           = ar 
  FTN          = gfortran
  FOPT         = -O3 
  FFLAG_MACHAR = -O0
  CPP          = cpp
  CPPFLAG      = -D$(ARCH) -P
  CC           = gcc
  CBASE        = -D$(ARCH) -DLWR_CASE
  COPT         = -O2 -g
  TIMER_LIB    = -L../lib/$(O_DIR) -ltimer
endif


ifeq ($(ARCH),LINUX_PC)
  FTN      = pgf77
  FBASE    =
  FOPT     = -O2 -pc64 -Munroll=c:8,n:8 -Mdalign -Mextend -I. -I../include
  FPREC    = 
  CPP      = /lib/cpp 
  CPPFLAG = -D$(ARCH) -DTIMER_LIB -P
  CC       = cc
  CBASE    = -DLWR_CASE_DUSCORE
  COPT     = 
  LDFLAG   = 
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer 
  VLIBS    =
endif

ifeq ($(ARCH),LINUX_IA64)
  FTN      = efc
  FBASE    = -w90 -w95 -I. -I../include
  FOPT     = -O3 -pad -align -ip
  FPREC    =
  CPP      = /lib/cpp
  CPPFLAG  = -D$(ARCH) -DTIMER_LIB -P -traditional
  CC       = ecc
  CBASE    = -DLWR_CASE_DUSCORE
  COPT     = -O3
  LDFLAG   = -ip
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer
  VLIBS    =
endif

ifeq ($(ARCH),LINUX_ARM)
  FTN      = armflang
  FBASE    =  -mcpu=native -I. -I../include
  FOPT     = -O3 -ffp-contract=fast 
  FPREC    =
  CPP      = gfortran -E
  CPPFLAG  = -D$(ARCH) -DTIMER_LIB -P -traditional
  CC       = armclang
  CBASE    = -DLWR_CASE_DUSCORE
  COPT     = -O3
  LDFLAG   = -L${ARMPL_DIR}/lib -larmpl_lp64 -lamath
  TIMER_LIB  = -L../lib/$(O_DIR) -ltimer
  VLIBS    =
endif


FFLAG  = $(FBASE) $(FPREC) $(FOPT)
CFLAG  = $(CBASE)          $(COPT) 

LIB     = $(VLIBS) $(TIMER_LIB)

FSOURCES= alogit.fpp  expit.fpp  powerit.fpp  sinit.fpp  sqrtit.fpp nintit.fpp \
          dlogit.fpp dexpit.fpp dpowerit.fpp dsinit.fpp dsqrtit.fpp dnintit.fpp \
					ddivit.fpp drecipit.fpp \
	  testfun.fpp my_pe.fpp 

FOBJECTS= alogit.o  expit.o  powerit.o  sinit.o  sqrtit.o nintit.o \
          dlogit.o dexpit.o dpowerit.o dsinit.o dsqrtit.o dnintit.o \
					ddivit.o drecipit.o \
	  testfun.o my_pe.o 

CSOURCES= 

COBJECTS= 

$(FOBJECTS): $(HDRS)

#
# single precision accuracy test source
#

alog.o: alog.F
	$(CPP) $(CPPFLAG) alog.F > alog.fpp
	$(FTN) $(FFLAG) -c alog.fpp

exp.o: exp.F
	$(CPP) $(CPPFLAG) exp.F > exp.fpp
	$(FTN) $(FFLAG) -c exp.fpp

power.o: power.F
	$(CPP) $(CPPFLAG) power.F > power.fpp
	$(FTN) $(FFLAG) -c power.fpp

sin.o: sin.F
	$(CPP) $(CPPFLAG) sin.F > sin.fpp
	$(FTN) $(FFLAG) -c sin.fpp

sqrt.o: sqrt.F
	$(CPP) $(CPPFLAG) sqrt.F > sqrt.fpp
	$(FTN) $(FFLAG) -c sqrt.fpp

machar.o: machar.F
	$(CPP) $(CPPFLAG) machar.F > machar.fpp
	$(FTN) $(FFLAG_MACHAR) -c machar.fpp


#
# double precision accuracy test source
# 

dlog.o: dlog.F
	$(CPP) $(CPPFLAG) dlog.F > dlog.fpp
	$(FTN) $(FFLAG) -c dlog.fpp

dexp.o: dexp.F
	$(CPP) $(CPPFLAG) dexp.F > dexp.fpp
	$(FTN) $(FFLAG) -c dexp.fpp

dpower.o: dpower.F
	$(CPP) $(CPPFLAG) dpower.F > dpower.fpp
	$(FTN) $(FFLAG) -c dpower.fpp

dsin.o: dsin.F
	$(CPP) $(CPPFLAG) dsin.F > dsin.fpp
	$(FTN) $(FFLAG) -c dsin.fpp

dsqrt.o: dsqrt.F
	$(CPP) $(CPPFLAG) dsqrt.F > dsqrt.fpp
	$(FTN) $(FFLAG) -c dsqrt.fpp


dmachar.o: machar.F
	$(CPP) -DDBL $(CPPFLAG) machar.F > dmachar.fpp
	$(FTN) $(FFLAG_MACHAR) -c dmachar.fpp

#
# Performance tests
#

testfun.o: testfun.F
	$(CPP) $(CPPFLAG) testfun.F > testfun.fpp
	$(FTN) $(FFLAG) -c testfun.fpp

my_pe.o: my_pe.F
	$(CPP) $(CPPFLAG) my_pe.F > my_pe.fpp
	$(FTN) $(FFLAG) -c my_pe.fpp

ifeq ($(ARCH),DEC)
 
#
# Timer
#

timer.o: timer.F
	$(CPP) $(CPPFLAG) timer.F > timer.fpp
	$(FTN) $(FFLAG) -free -c timer.fpp
endif

#
# Single precision performance source
#

alogit.o: alogit.F
	$(CPP) $(CPPFLAG) alogit.F > alogit.fpp
	$(FTN) $(FFLAG) -c alogit.fpp

expit.o: expit.F
	$(CPP) $(CPPFLAG) expit.F > expit.fpp
	$(FTN) $(FFLAG) -c expit.fpp

powerit.o: powerit.F
	$(CPP) $(CPPFLAG) powerit.F > powerit.fpp
	$(FTN) $(FFLAG) -c powerit.fpp

sinit.o: sinit.F
	$(CPP) $(CPPFLAG) sinit.F > sinit.fpp
	$(FTN) $(FFLAG) -c sinit.fpp

sqrtit.o: sqrtit.F
	$(CPP) $(CPPFLAG) sqrtit.F > sqrtit.fpp
	$(FTN) $(FFLAG) -c sqrtit.fpp

nintit.o: nintit.F
	$(CPP) $(CPPFLAG) nintit.F > nintit.fpp
	$(FTN) $(FFLAG) -c nintit.fpp

#
# double precision performance source
# 

dlogit.o: dlogit.F
	$(CPP) $(CPPFLAG) dlogit.F > dlogit.fpp
	$(FTN) $(FFLAG) -c dlogit.fpp

dexpit.o: dexpit.F
	$(CPP) $(CPPFLAG) dexpit.F > dexpit.fpp
	$(FTN) $(FFLAG) -c dexpit.fpp

dpowerit.o: dpowerit.F
	$(CPP) $(CPPFLAG) dpowerit.F > dpowerit.fpp
	$(FTN) $(FFLAG) -c dpowerit.fpp

dsinit.o: dsinit.F
	$(CPP) $(CPPFLAG) dsinit.F > dsinit.fpp
	$(FTN) $(FFLAG) -c dsinit.fpp

dsqrtit.o: dsqrtit.F
	$(CPP) $(CPPFLAG) dsqrtit.F > dsqrtit.fpp
	$(FTN) $(FFLAG) -c dsqrtit.fpp

dnintit.o: dnintit.F
	$(CPP) $(CPPFLAG) dnintit.F > dnintit.fpp
	$(FTN) $(FFLAG) -c dnintit.fpp

ddivit.o: ddivit.F
	$(CPP) $(CPPFLAG) ddivit.F > ddivit.fpp
	$(FTN) $(FFLAG) -c ddivit.fpp

drecipit.o: drecipit.F
	$(CPP) $(CPPFLAG) drecipit.F > drecipit.fpp
	$(FTN) $(FFLAG) -c drecipit.fpp

#
# Performance tests
#
testx: $(TIMER_OBJECT) $(FOBJECTS) $(COBJECTS)
	$(FTN) $(LDFLAG) $(TIMER_OBJECT) $(FOBJECTS) $(COBJECTS) $(LIB) -o $@$(SUFF) 

#
# single precision executables
#

alogx: alog.o machar.o
	$(FTN) alog.o machar.o  $(LDFLAG) -o $@$(SUFF) 

expx: exp.o machar.o
	$(FTN) exp.o  machar.o  $(LDFLAG) -o $@$(SUFF) 

powerx: power.o machar.o
	$(FTN) power.o  machar.o  $(LDFLAG) -o $@$(SUFF) 

sinx: sin.o machar.o
	$(FTN) sin.o  machar.o  $(LDFLAG) -o $@$(SUFF) 

sqrtx: sqrt.o machar.o
	$(FTN) sqrt.o  machar.o  $(LDFLAG) -o $@$(SUFF) 

#
# double precision tests
#

dlogx: dlog.o dmachar.o
	$(FTN) dlog.o dmachar.o  $(LDFLAG) -o $@$(SUFF) 

dexpx: dexp.o dmachar.o
	$(FTN) dexp.o  dmachar.o  $(LDFLAG) -o $@$(SUFF) 

dpowerx: dpower.o dmachar.o
	$(FTN) dpower.o  dmachar.o  $(LDFLAG) -o $@$(SUFF) 

dsinx: dsin.o dmachar.o
	$(FTN) dsin.o  dmachar.o  $(LDFLAG) -o $@$(SUFF) 

dsqrtx: dsqrt.o dmachar.o
	$(FTN) dsqrt.o  dmachar.o  $(LDFLAG) -o $@$(SUFF) 

%.o: %.c
	$(CC) $(CFLAG) -c $*.c

clean:
	rm -rf *.o *x *.fpp *.mod 

