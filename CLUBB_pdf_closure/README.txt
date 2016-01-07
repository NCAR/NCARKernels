[General Information ]

Kernel: CLUBB_pdf_closure
Source: cesm1_4_beta06
Compiler: Intel Fortran Compiler 16.0.1
KGEN Version: 0.6.2 (https://github.com/NCAR/KGen.git "devel" branch)

[ Running the kernel ]
>> cd kernel
>> module load intel/16.0.1
>> module load mkl
>> make

[ Files ]
./Makefile.kgen : makefile used to extract the kernel
./include.ini : KGEN include file to provide file search paths and macros
./kernel/* : kernel source files and makefile to build and run the kernel
./state/* : kgen-instrumented source files and makefile to generate state data
./data/* : state data generated from running CESM with files in ./state sub-directory
./src/* : Temporary files genrated and removed during CESM compilation
./CESM_Kernel_License_12-08-2015.pdf: CESM Kernel License File
