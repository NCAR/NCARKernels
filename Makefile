SUBDIRS := $(shell ls -d */)

# set the fortran flags here 
#FFLAGS := -O3 -mmic -vec-report=6 -mP2OPT_hpo_matrix_opt_framework=0 -fp-model precise
#FFLAGS := -O3 -mmic -fp-model precise
FFLAGS := -O3 -mmic -qopt-report=5 -fp-model fast
#FFLAGS := -O3 -vec-report=7 -fp-model fast
export FFLAGS

all:
	@for s in $(SUBDIRS); do \
	echo "###############################################################################"; \
	echo "                          Executing make all in  $$s                            "; \
	echo "###############################################################################"; \
	$(MAKE) all -C $$s || exit 1; done

run:
	@for s in $(SUBDIRS); do \
	echo "###############################################################################"; \
	echo "                          Executing make run in  $$s                            "; \
	echo "###############################################################################"; \
	$(MAKE) run -C $$s || exit 1; done

build:
	echo ${FFLAGS}
	@for s in $(SUBDIRS); do \
	echo "###############################################################################"; \
	echo "                          Executing make build in  $$s                            "; \
	echo "###############################################################################"; \
	$(MAKE) build -C $$s || exit 1; done

clean:
	@for s in $(SUBDIRS); do \
	echo "###############################################################################"; \
	echo "                          Executing make clean in $$s                            "; \
	echo "###############################################################################"; \
	$(MAKE) clean -C $$s || exit 1; done
