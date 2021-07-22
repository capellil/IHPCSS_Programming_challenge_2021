MAX_TEMPERATURE=50.0
C_DIRECTORY=c
FORTRAN_DIRECTORY=f

SRC_DIRECTORY=src
BIN_DIRECTORY=bin
DOC_DIRECTORY=doc
DAT_DIRECTORY=datasets

CC=mpicc
CFLAGS=-O2 -Wall -Wextra -D_GNU_SOURCE -lm -DMAX_TEMPERATURE=$(MAX_TEMPERATURE) -DIHPCSS_FOLDER=\"/jet/home/${USER}/IHPCSS_Programming_challenge_2021\"

default: all

all: create_directories \
	 verify_modules \
	 $(BIN_DIRECTORY)/c/cpu_big \
	 $(BIN_DIRECTORY)/c/cpu_small \
	 $(BIN_DIRECTORY)/c/gpu_big \
	 $(BIN_DIRECTORY)/c/gpu_small

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

$(BIN_DIRECTORY)/c/cpu_big: $(SRC_DIRECTORY)/c/cpu.c
	$(CC) -o $@ $^ $(CFLAGS) -DMPI_ONLY -DBIG -fopenmp

$(BIN_DIRECTORY)/c/cpu_small: $(SRC_DIRECTORY)/c/cpu.c
	$(CC) -o $@ $^ $(CFLAGS) -DMPI_ONLY -DSMALL -fopenmp

$(BIN_DIRECTORY)/c/gpu_big: $(SRC_DIRECTORY)/c/gpu.c
	$(CC) -acc -Minfo=accel -o $@ $^ $(CFLAGS) -DMPI_OPENACC -DBIG

$(BIN_DIRECTORY)/c/gpu_small: $(SRC_DIRECTORY)/c/gpu.c
	$(CC) -acc -Minfo=accel -o $@ $^ $(CFLAGS) -DMPI_OPENACC -DSMALL

clean:
	@if [ -d $(BIN_DIRECTORY) ]; then rm -rf $(BIN_DIRECTORY); fi;
