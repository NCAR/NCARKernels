KGEN_HOME ?= ${HOME}/repos/github/KGen
KGEN := $(KGEN_HOME)/bin/kgen

MPAS_HOME := ${HOME}/trepo/temp/kgen_system_test/branches/initial/MPAS-Release
SRC := ${MPAS_HOME}/src/core_atmosphere/physics/mpas_atmphys_driver_radiation_lw.F
OUTPUT_DIR := ${HOME}/trepo/temp/kgen_system_test/branches/initial/test_cases/MPAS_rrtmg_lw


all: extract

test: gendata
	 ${MAKE} -C kernel

gendata: extract
	if [ -d "./data" ]; then \
		if [ -a  "./data/atm_compute_solve_diagnostics.1.1" ]; then \
			cp -f data/* kernel; \
			echo "Using pre-generated data files"; \
		else \
			rm -f ${SOURCE_MODS}/*; \
			${MAKE} -C state || exit 1; \
			echo "Generating new data files"; \
			echo "Rerun test once data is successfully generated."; \
			exit 1; \
		fi \
	else \
		rm -f ${SOURCE_MODS}/*; \
		${MAKE} -C state || exit 1; \
		echo "Generating new data files"; \
		echo "Rerun test once data is successfully generated."; \
		exit 1; \
	fi 

extract: clean
	${KGEN} \
        -i include.ini \
        --outdir ${OUTPUT_DIR} \
		--timing repeat=1 \
		--source format=free,strict=no,alias=/glade/scratch/youngsun:/glade/u/home/youngsun/trepo/temp \
        --invocation 1:5:10 \
		--mpi ranks=1:4:8 \
		--state-build cmds="cd ${MPAS_HOME}; ./bld.sh; bsub < submit_init.bash" \
        --kernel-compile FC=ifort,FC_FLAGS='-real-size 64 -O3 -convert big_endian -FR' \
		--logging select.name=module_ra_rrtmg_lw:RRTMG_LWRAD:ngptlw \
        ${SRC}:mpas_atmphys_driver_radiation_lw:driver_radiation_lw:rrtmg_lwrad
		#--debug printvar=packed_tnd_qsnow,do_cldice

clean:
	rm -rf kgen.log kernel state
