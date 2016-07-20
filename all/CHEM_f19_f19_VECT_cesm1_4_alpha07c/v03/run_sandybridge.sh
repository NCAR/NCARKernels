#!/bin/bash
#SBATCH --time=60:00
#SBATCH -n 16
#SBATCH -N 1
#SBATCH -c 2
#SBATCH -p sandybridge

source  ~/setUpCompilers.sh
export LD_LIBRARY_PATH=/lustre/system/phi/intel/2015_update1/lib/mic:/lustre/system/phi/intel/2015_update1/impi/5.0.2.044/mic/lib:$LD_LIBRARY_PATH
#export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

#make clean
#make
#export KMP_AFFINITY=verbose,scatter

ulimit -s unlimited

export I_MPI_PIN_MODE=lib
export SLURM_CPU_BIND=verbose


echo $SLURM_JOB_NODELIST >& hostfile.txt
cat hostfile.txt 

export OMP_NUM_THREADS=1

#./kernel.exe
#mpirun -n 1 ./kernel_par.exe
#mpiexec.hydra -f hostfile.txt  -n 1 ./kernel_par.exe
#mpiexec.hydra -f hostfile.txt  -n 1 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE ./kernel_par.exe
#mpiexec.hydra -f hostfile.txt  -n 1 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH  ./kernel_par.exe

mpiexec.hydra -f hostfile.txt  -n 1 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH  ./kernel_s.exe 
mpiexec.hydra -f hostfile.txt  -n 2 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH  ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 4 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH  ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 6 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH  ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 8 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH  ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 10 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 12 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 14 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_s.exe
mpiexec.hydra -f hostfile.txt  -n 16 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_s.exe


