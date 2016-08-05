#!/bin/tcsh
#SBATCH --time=15:00
#SBATCH -n 1
#SBATCH -p haswell-4 

#source  ~/setUpCompilers.sh
#export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

#make clean

unlimit
#make
./kernel.exe

