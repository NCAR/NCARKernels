#!/bin/bash -l 
#SBATCH --time=2:00:00
#SBATCH -N 1
#SBATCH -c 4
#SBATCH -p knl

export LM_LICENSE_FILE="28518@128.117.177.41"
source /usr/local/intel/2016_update3/bin/compilervars.sh intel64
source /usr/local/intel/2016_update3/impi/5.1.3.210/intel64/bin/mpivars.sh

ulimit -s unlimited

export I_MPI_PIN_MODE=lib
#export I_MPI_PIN_MODE=mpd
export SLURM_CPU_BIND=verbose
export LD_LIBRARY_PATH=/usr/lib64/libpmi.so:$LD_LIBRARY_PATH
export OMP_NUM_THREADS=1

echo $SLURM_JOB_NODELIST >& hostfile.txt
cat hostfile.txt 

echo 1
mpiexec.hydra -f hostfile.txt  -n 1 -env LD_LIBRARY_PATH $LD_LIBRARY_PATH numactl --membind 0  ./kernel.avx512.exe >& knl_v04_membind=0.txt
mpiexec.hydra -f hostfile.txt  -n 1 -env LD_LIBRARY_PATH $LD_LIBRARY_PATH numactl --membind 1  ./kernel.avx512.exe >& knl_v04_membind=1.txt

for i in {2..68..2}; do
##  echo $i
##  echo ""
  mpiexec.hydra -f hostfile.txt  -n $i -env LD_LIBRARY_PATH $LD_LIBRARY_PATH numactl --membind 0  ./kernel.avx512.exe >> knl_v04_membind=0.txt
  mpiexec.hydra -f hostfile.txt  -n $i -env LD_LIBRARY_PATH $LD_LIBRARY_PATH numactl --membind 1  ./kernel.avx512.exe >> knl_v04_membind=1.txt
done
