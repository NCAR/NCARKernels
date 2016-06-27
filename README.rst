NCAR Climate/Weather Kernels
===============================

A collection of *kernels* extracted from various climate/weather simulation software.

1. How to use the kernels
===============================

1.1 Download
--------

    >>> git clone https://github.com/NCAR/kernelOptimization.git

Please see a license document located at the root directory of each kernel.
If a license document does not exist, the license applied to original software that the kernel is extracted from is also applied to the kernel.
If you find any conflicts on applying the license to the kernel, please contact us to kgen@ucar.edu.

1.2 Directory structure
-------------------

Directory tree of the repo.:
   .. image:: dirtree.png

![alt text](dirtree.png "Directory tree")

1.2.1 Conventions
'''''''''''

**<name>**: a variable name. User will select proper name for the variable such as kernel name.

**[name]**: an optional name. User will choose to use or not use the name such as optional directory name.

**name**: a literal name. This name should be used as it is such as "all" for a directory including all kernels.

1.2.2 Directories and files
'''''''''''''''''''''

**all**: The directory contains all kernels

**scripts**: The directory contains all scripts that use kernels in this repo.

**silver**: The directory contains symbolic links to some of all kernels. Current category of this directory is based on their relative importance determined by adminstrator of this repo.

**<kernel>**: The directory contains all materials for a <kernel>

**orig**: The directory is an only mandatory directory that contains source files and <makefile> to build and run the <kernel>. There could be multiple <makefile>s to support various compilers and computing systems. See <makefile> section for details.

**[config]**: The directory contains materials for configuration of the kernel and other miscellaneous ones.

**[data]**: The directory contains data files used by the <kernel>

**[final]**: The directory contains optimized version of the <kernel>

**[README.txt]**: The file contains information of the <kernel>

**[state]**: The directory contains information to generate files in [data] directory.

1.3 building/running the kernels
----------------------------

1.3.1 <makefile>(s)
'''''''''''''

Each "orig" directory should contains one or more <makefile>(s). The name of <makefile> is arbitrary. However, for automating test tasks, it is highly desirable to use the same <makefile> name across all kernels. For example, we may put "Makefile.gnu" for all kernels so that a test script can build/run al the kernels automatically.

There are three mandatory targets in each <makefile>: clean, build, and run.

1.3.2 clean/build/run Makefile targets
''''''''''''''''''''''''''''''''

User can clean/build/run each kernel by issuing "<makefile> target". It is assumed that user setup compilation and building environment before running the command.

2. How to add/remove a kernel
==========================

User can freely fork this repo. and manage your own kernels independently.  If you want to add or to remove a kernel in this repo., please contact us to kgen@ucar.edu.

3. Sharing test result
===================

While we encourage you to share your test result with others, please make sure that you are allowed to do so. For example, any test result bound to a certain NDA(Non-disclosure agreement) should not be shared through this repo.

That being said, we have come up with a draft of test report specification for kernels in this repo.

3.1 test result report
------------------

    We recommend that test result report follows following rules.

3.1.1 file format: JSON
'''''''''''''''''

3.1.2 content of test result:
'''''''''''''''''''''''

      - Essential items are:
      
        * "cpu_model": cpu model name of a test system - string
        
        * "total_memsize": total memory of a test system - string
        
        * "os": opearting system of a test system - string
        
        * "compiler": compiler version being used - string
        
        * "cases": this is a dictionary that contains results of each test cases - dictionary.
        
            ** <testname>: Each test result should contain at least following items. - dictionary in the form of <testname> : <dictionary>
            
                *** "begin": test start date and time (YYYY-MM-DD HH:MM:SS.SSSSSS) - string
                
                *** "end": test end date and time - string
                
                *** "passed": verification result - boolean
                
                *** "tolerance": tolerance for verification - float
                
                *** "difference": Verification difference - list of float numbers
                
                *** "diff_type": The type of difference and tolerance  0: Normalized RMS difference
                
                *** "elapsed_time": Elapsed time in micro-second for a test case list of float numbers
                
      - Other test information can be added in similar way to above essential items.

3.2 test result poltting
--------------------

There may be various plotting needs. As of this writing, one plotting format is developed.

   .. image:: plot.png
   
