#!/bin/bash

    ${KGEN}/bin/kgen \
    --cmd-build "cd ${CASE_DIR}; ./KINTCESM.build" \
    --prerun kernel_run="export LD_LIBRARY_PATH=/ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016.1.150/linux/mkl/lib/intel64_lin:/ncar/opt/intel/psxe-2016_update1/compilers_and_libraries_2016.1.150/linux/compiler/lib/intel64_lin:\${LD_LIBRARY_PATH}" \
    --outdir ${WORK_DIR} \
    --kernel-option -mkl=link \
    --invocation 100:0-1:10,100:0-1:50,300:0-1:10,300:0-1:50,500:0-1:10,500:0-1:50 \
    --timing repeat=1 \
    --openmp enable \
    -e ${WORK_DIR}/exclude.ini \
    --cmd-clean "cd ${CASE_DIR}; ./KINTCESM.clean_build all" \
    --cmd-run "cd ${CASE_DIR}; ./KINTCESM.submit" \
    --mpi comm=mpicom,use="spmd_utils:mpicom" \
    ${CESM_HOME}/components/cam/src/physics/cam/clubb_intr.F90
