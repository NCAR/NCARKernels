#!/bin/bash
#SBATCH --time=60:00
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -p phi-5110 

source  ~/setUpCompilers.sh
export LD_LIBRARY_PATH=/lustre/system/phi/intel/2015_update1/lib/mic:/lustre/system/phi/intel/2015_update1/impi/5.0.2.044/mic/lib:$LD_LIBRARY_PATH
#export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

#make clean
#make
#export KMP_AFFINITY=verbose,scatter

ulimit -s unlimited

#export I_MPI_PIN_MODE=lib
#export SLURM_CPU_BIND=verbose


#echo $SLURM_JOB_NODELIST >& hostfile.txt
#cat hostfile.txt 

#export OMP_NUM_THREADS=1

mpirun -n 1 ./kernel.exe

