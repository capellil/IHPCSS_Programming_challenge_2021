MAX_TEMPERATURE=50.0
C_DIRECTORY=C
FORTRAN_DIRECTORY=FORTRAN

SRC_DIRECTORY=src
BIN_DIRECTORY=bin
DOC_DIRECTORY=doc
DAT_DIRECTORY=datasets

CC=mpicc
CFLAGS=-O2 -Wall -Wextra -fopenmp -D_GNU_SOURCE -lm -DMAX_TEMPERATURE=$(MAX_TEMPERATURE)

IHPCSS_FOLDER=`pwd`

default: all

all: verify_modules \
     create_directories \
	 $(BIN_DIRECTORY)/dataset_generator \
	 $(BIN_DIRECTORY)/verify \
	 $(BIN_DIRECTORY)/main

create_directories:
	@if [ ! -d $(BIN_DIRECTORY) ]; then mkdir $(BIN_DIRECTORY); fi; \
	if [ ! -d $(DAT_DIRECTORY) ]; then mkdir $(DAT_DIRECTORY); fi; \
	if [ ! -d $(BIN_DIRECTORY)/$(C_DIRECTORY) ]; then mkdir $(BIN_DIRECTORY)/$(C_DIRECTORY); fi; \
	if [ ! -d $(BIN_DIRECTORY)/$(FORTRAN_DIRECTORY) ]; then mkdir $(BIN_DIRECTORY)/$(FORTRAN_DIRECTORY); fi 

verify_modules:
	@if ! type "mpicc" > /dev/null 2>&1; then \
		clear; \
		echo -e "\n    . "; \
		echo -e "   / \\"; \
		echo -e "  / ! \\  It looks like the GCC wrapper for MPI is not loaded."; \
		echo -e " /_____\\ On Bridges 2 please issue 'module load mvapich2/2.3.5-gcc8.3.1'. You can now make again :)\n"; \
		exit -1; \
	fi

$(BIN_DIRECTORY)/dataset_generator: $(SRC_DIRECTORY)/utils/dataset_generator.c
	$(CC) -o $@ $^ $(CFLAGS) -DIHPCSS_FOLDER=\"$(IHPCSS_FOLDER)\"

$(BIN_DIRECTORY)/verify: $(SRC_DIRECTORY)/utils/verify.c
	$(CC) -o $@ $^ $(CFLAGS)

$(BIN_DIRECTORY)/main: $(SRC_DIRECTORY)/main.c
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	@if [ -d $(BIN_DIRECTORY) ]; then rm -rf $(BIN_DIRECTORY); fi;
