vorticity_sphere kernel
Edited 03/17/2015
Amogh Simha

*kernel and supporting files
	-the vorticity_sphere subroutine is located in the derivative_mod.F90 file
	-subroutine call is in the compute_and_apply_rhs subroutine in the prim_advance_mod.F90 file

*compilation and execution
	-Just download the enclosing directory
	-Run make

*verification
	-The make command will trigger the verification of the kernel.
	-It is considered to have passed verification if the tolerance for normalized RMS is less than 9.999999824516700E-015
	-Input data is provided by vorticity_sphere.1.0 vorticity_sphere.10.0, and vorticity_sphere.20.0

*performance measurement
	-The elapsed time in seconds is printed to stdout for each input file specified 

