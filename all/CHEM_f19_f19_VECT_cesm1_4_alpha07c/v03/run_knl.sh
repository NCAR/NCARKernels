#!/bin/bash

ulimit -s unlimited

#export I_MPI_PIN_MODE=lib
export I_MPI_PIN_MODE=mpd
export SLURM_CPU_BIND=verbose
export LD_LIBRARY_PATH=/usr/lib64/libpmi.so:$LD_LIBRARY_PATH
export I_MPI_DEBUG=4

echo $SLURM_JOB_NODELIST >& hostfile.txt
cat hostfile.txt 

echo 1
#mpiexec.hydra -f hostfile.txt  -n 1 -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env I_MPI_PIN_DOMAIN=cache -env I_MPI_DEBUG=4 numactl --membind 0  ./kernel_64_knl.exe
mpiexec.hydra -f hostfile.txt  -n 1 -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env I_MPI_PIN_DOMAIN=cache numactl --membind 1  ./kernel_64_knl.exe
for i in {2..136..2}; do
  export OMP_NUM_THREADS=1
  echo $i
  echo ""
#  mpiexec.hydra -f hostfile.txt  -n $i -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env I_MPI_PIN_DOMAIN=cache -env I_MPI_DEBUG=4 numactl --membind 0  ./kernel_64_knl.exe
  mpiexec.hydra -f hostfile.txt  -n $i -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env  I_MPI_PIN_DOMAIN=cache numactl --membind 1  ./kernel_64_knl.exe
done
