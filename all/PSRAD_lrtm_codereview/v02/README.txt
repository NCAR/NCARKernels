This version of the kernel uses the 'Macros' file to define flags for each
compiler and should be usable across Arm & Xeon architectures by simply 
changing the the COMPILER and MPI variable.  GNU fortran, Arm flang and
Ifort are supported on their proper architectures.

This is a work in progress, with additional minor changes in the works.  For
performance testing in parallel (MPI), every rank prints out lots of 
information, but at the end you'll get an average call rate across all ranks,
which is the important number.  (Streamlining the I/O is on the 'to do'
list.)

