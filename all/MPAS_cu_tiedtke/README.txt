[General Information ]

Kernel: cu_tiedtke
Source: MPAS-Release-v4.0
Compiler: Intel Fortran Compiler 15.0.1
KGEN Version: 0.6.0 (https://github.com/NCAR/KGen.git "devel" branch)

[ Running the kernel ]
>> cd kernel
>> module load intel/15.0.1
>> make

[ Files ]
./Makefile.kgen : makefile used to extract the kernel
./include.ini : KGEN include file to provide file search paths and macros
./kernel/* : kernel source files and makefile to build and run the kernel
./state/* : kgen-instrumented source files and makefile to generate state data
./data/* : state data generated from running MPAs with files in ./state sub-directory

