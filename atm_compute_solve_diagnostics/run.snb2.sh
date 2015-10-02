#!/bin/bash -l
#SBATCH --time=1:00:00
#SBATCH -n 16
#SBATCH -N 1
#SBATCH -c 2
#SBATCH -p sandybridge

########################################
# Above is header, below is run data
########################################

export LM_LICENSE_FILE="28518@128.117.177.41"
source /usr/local/intel/2015/bin/compilervars.sh intel64
source /usr/local/intel/2015/impi_latest/bin64/mpivars.sh

source /usr/local/intel/2015/vtune_amplifier_xe/amplxe-vars.sh
source /usr/local/intel/2015/itac/9.0.1.033/intel64/bin/itacvars.sh

export I_MPI_PIN_MODE=lib

hostname 
export SLURM_CPU_BIND=verbose

export LD_LIBRARY_PATH=/ncar/asap/opt/netcdf-c/4.3.3.1/snb/intel/15.0.0/hdf5/lib:$LD_LIBRARY_PATH

ulimit -s unlimited
rm -f movies/*

echo $SLURM_JOB_NODELIST >& hostfile.txt
cat hostfile.txt 

export LID="`date +%y%m%d-%H%M%S`"


#mpiexec.hydra -f hostfile.txt -n 1 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.1x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 1 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.1x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 2 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.2x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 2 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.2x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 4 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.4x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 4 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.4x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 6 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.6x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 6 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.6x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 8 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.8x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 8 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.8x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 10 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.10x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 10 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.10x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 12 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.12x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 12 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.12x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 14 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.14x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 14 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.14x1.log.$LID

#mpiexec.hydra -f hostfile.txt -n 16 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v0.r4.exe >& kernel.v0.r4.96x1.16x1.log.$LID
mpiexec.hydra -f hostfile.txt -n 16 -env I_MPI_PIN_MODE $I_MPI_PIN_MODE -env LD_LIBRARY_PATH $LD_LIBRARY_PATH -env KMP_AFFINITY 'verbose,scatter' ./kernel.avx.v2.r4.exe >& kernel.v2.r4.96x1.16x1.log.$LID

