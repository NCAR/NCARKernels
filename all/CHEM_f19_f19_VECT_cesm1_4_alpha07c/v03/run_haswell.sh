#!/bin/bash
#SBATCH --time=6:00:00
#SBATCH -n 72
#SBATCH -N 1
#SBATCH -c 2
#SBATCH -p haswell-4

source  ~/setUpCompilers.sh
export LD_LIBRARY_PATH=/lustre/system/phi/intel/2015_update1/lib/mic:/lustre/system/phi/intel/2015_update1/impi/5.0.2.044/mic/lib:$LD_LIBRARY_PATH
#export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so

#make clean
#make
export KMP_AFFINITY=verbose,scatter

ulimit -s unlimited

export I_MPI_PIN_MODE=lib
export SLURM_CPU_BIND=verbose


echo $SLURM_JOB_NODELIST >& hostfile.txt
cat hostfile.txt 

export OMP_NUM_THREADS=1
echo 1
echo ""
mpiexec.hydra -f hostfile.txt  -n 1 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe 
echo 2
echo ""
mpiexec.hydra -f hostfile.txt  -n 2 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 4
echo ""
mpiexec.hydra -f hostfile.txt  -n 4 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 6
echo ""
mpiexec.hydra -f hostfile.txt  -n 6 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 8
echo ""
mpiexec.hydra -f hostfile.txt  -n 8 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 10
echo ""
mpiexec.hydra -f hostfile.txt  -n 10 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 12
echo ""
mpiexec.hydra -f hostfile.txt  -n 12 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 14
echo ""
mpiexec.hydra -f hostfile.txt  -n 14 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 16
echo ""
mpiexec.hydra -f hostfile.txt  -n 16 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 18
echo ""
mpiexec.hydra -f hostfile.txt  -n 18 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 20
echo ""
mpiexec.hydra -f hostfile.txt  -n 20 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 22
echo ""
mpiexec.hydra -f hostfile.txt  -n 22 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 24
echo ""
mpiexec.hydra -f hostfile.txt  -n 24 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 26
echo ""
mpiexec.hydra -f hostfile.txt  -n 26 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 28
echo ""
mpiexec.hydra -f hostfile.txt  -n 28 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 30
echo ""
mpiexec.hydra -f hostfile.txt  -n 30 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 32
echo ""
mpiexec.hydra -f hostfile.txt  -n 32 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 34
echo ""
mpiexec.hydra -f hostfile.txt  -n 34 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 36
echo ""
mpiexec.hydra -f hostfile.txt  -n 36 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 38
echo ""
mpiexec.hydra -f hostfile.txt  -n 38 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 40
echo ""
mpiexec.hydra -f hostfile.txt  -n 40 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 42
echo ""
mpiexec.hydra -f hostfile.txt  -n 42 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 44
echo ""
mpiexec.hydra -f hostfile.txt  -n 44 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 46
echo ""
mpiexec.hydra -f hostfile.txt  -n 46 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 48
echo ""
mpiexec.hydra -f hostfile.txt  -n 48 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 50
echo ""
mpiexec.hydra -f hostfile.txt  -n 50 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 52
echo ""
mpiexec.hydra -f hostfile.txt  -n 52 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 54
echo ""
mpiexec.hydra -f hostfile.txt  -n 54 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 56
echo ""
mpiexec.hydra -f hostfile.txt  -n 56 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 58
echo ""
mpiexec.hydra -f hostfile.txt  -n 58 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 60
echo ""
mpiexec.hydra -f hostfile.txt  -n 60 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 62
echo ""
mpiexec.hydra -f hostfile.txt  -n 62 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 64
echo ""
mpiexec.hydra -f hostfile.txt  -n 64 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 66
echo ""
mpiexec.hydra -f hostfile.txt  -n 66 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 68
echo ""
mpiexec.hydra -f hostfile.txt  -n 68 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 70
echo ""
mpiexec.hydra -f hostfile.txt  -n 70 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe
echo 72
echo ""
mpiexec.hydra -f hostfile.txt  -n 72 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH ./kernel_h.exe

