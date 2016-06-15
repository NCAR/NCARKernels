[General Information ]

Kernel: CLUBB_clubb_tend_cam
Source: cesm1_4_beta07
Compiler: Intel Fortran Compiler 16.0.1
KGEN Version: 0.7.0

[ Running the kernel on Yellowstone(NCAR)]
>> cd kernel
>> vi Makefile # modify FC_0 and/or FC_FLAGS_SET_0 and/or PRERUN_RUN to match with your test environment
>> module load intel/16.0.1
>> module load mkl
>> make

[ Files ]
./kgen_cmds.sh: KGen command used to extract the kernel
./include.ini : KGEN include file to provide file search paths and macros
./kernel/* : kernel source files and makefile to build and run the kernel
./state/* : kgen-instrumented source files and makefile to generate state data
./data/* : state data generated from running CESM with files in ./state sub-directory
./CESM_Kernel_License_12-08-2015.pdf: CESM Kernel License File
