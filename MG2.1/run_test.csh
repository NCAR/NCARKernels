#!/bin/csh -vf

#BSUB -a poe
#BSUB -P STDD0002

#BSUB -q small
#BSUB -W 0:10
#BSUB -x

#BSUB -J MG2

#BSUB -o MG2.stdout.%J
#BSUB -e MG2.stderr.%J

#BSUB -n 1
#BSUB -R "span[ptile=1]" 

########################################
# Above is header, below is run data
########################################
 
  setenv OMP_NUM_THREADS 1
  setenv OMP_STACKSIZE 128M
  setenv MKL_VERBOSE 1
 
  cd /glade/u/home/ck/tests/xeon/MG2_NCAR_DEV
 
  mpirun.lsf ./kernel.exe

  exit 
