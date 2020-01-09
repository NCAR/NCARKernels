#!/bin/bash -l
module use /glade/work/cponder/SHARE/Modules/Latest
module use /glade/work/cponder/SHARE/Modules/Legacy

module use --append /glade/work/cponder/SHARE/Modules/Bundles

for dir in /glade/work/cponder/SHARE/Modules/PrgEnv/*/*
do
    module use --append $dir
done

module purge
#module load cuda/10.1
module load PrgEnv/PGI+OpenMPI/2019-04-30 
module load pgi
ulimit -s unlimited
module list

echo $LD_LIBRARY_PATH
#nvprof --print-gpu-trace ./kernel.exe
nvvp ./kernel.exe

