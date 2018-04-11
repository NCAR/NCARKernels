This is a 'curated' version of the WACCM_imp_sol_vector kernel which has 
been tested with the Intel, GNU and Arm (armflang) compilers.  It also
adds a few options (see ./kernel.exe -h) and makes minor changes like
switching files from .f90 -> .F90 format to enable preprocessor directives.

It also now uses a 'Macros' file that defines the flags for each compiler,
and allows the end-user to change three values (COMPILER, ARCH and MPI) to
build and test on different platforms.  This can also be scripted.

This is a work in progress, with additional minor changes in the works.  For
performance testing in parallel (MPI), every rank prints out lots of 
information, but at the end you'll get an average call rate across all ranks,
which is the important numbers.  (Streamlining the I/O is on the 'to do'
list.)

