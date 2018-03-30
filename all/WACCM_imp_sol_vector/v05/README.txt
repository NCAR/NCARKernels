This is a 'curated' version of the WACCM_imp_sol_vector kernel which has 
been tested with the Intel, GNU and Arm (armflang) compilers.  It also
adds a few options (see ./kernel.exe -h) and makes minor changes like
switching files from .f90 -> .F90 format to enable preprocessor directives.

It also now uses a 'Macros' file that defines the flags for each compiler,
and allows the end-user to change three values (COMPILER, ARCH and MPI) to
build and test on different platforms.  This can also be scripted.

This is a work in progress, with additional minor changes coming soon, 
including:
  - verifying that the MPI mode works as expected
  - ideally, adding a '-summary' and '-verbose' options, and changing 
         I/O to simplify scripting of this benchmark for rapid deployment
         on additional platforms


