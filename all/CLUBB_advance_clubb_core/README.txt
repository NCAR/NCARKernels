[General Information ]

Kernel: CLUBB_adv_clubb_core
Source: cesm1_4_beta06
Compiler: Intel Fortran Compiler 16.0.1
KGEN Version: 0.6.2 (https://github.com/NCAR/KGen.git "devel" branch)

[ Running the kernel ]
>> cd orig
>> module load intel/16.0.1
>> module load mkl
>> make

[ Files ]
config/Makefile.kgen : makefile used to extract the kernel
config/include.ini : KGEN include file to provide file search paths and macros
orig/* : kernel source files and makefile to build and run the kernel
state/* : kgen-instrumented source files and makefile to generate state data
data/* : state data generated from running CESM with files in ./state sub-directory
config/src/* : Temporary files genrated and removed during CESM compilation
CESM_Kernel_License_12-08-2015.pdf: CESM Kernel License File
