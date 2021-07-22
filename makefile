MAX_TEMPERATURE=50.0
C_DIRECTORY=c
FORTRAN_DIRECTORY=f

SRC_DIRECTORY=src
BIN_DIRECTORY=bin
DOC_DIRECTORY=doc
DAT_DIRECTORY=datasets

CC=mpicc
CFLAGS=-O2 -Wall -Wextra -D_GNU_SOURCE -lm -DMAX_TEMPERATURE=$(MAX_TEMPERATURE) -DIHPCSS_FOLDER=\"/jet/home/${USER}/IHPCSS_Programming_challenge_2021\"

CF=mpif90
FFLAGS=-O2 -mcmodel=medium -DMAX_TEMPERATURE=$(MAX_TEMPERATURE)

default: warning all

warning:
	@clear; \
	echo -e "  ^"; \
	echo -e " /!\ You are issuing a 'make' manually!"; \
	echo -e "/___\\"; \
	echo -e "\n"; \
	echo -e "The problem being we cannot change which module you have loaded from within the makefile, and that turns out to be a problem -> GPU versions require the nvhpc module. However, it seems to generate bugs for the CPU versions so we will use an mvapich module instead. Therefore, in order to make sure you have the right module loaded in, there are two scripts; compile_cpu_versions.sh and compile_gpu_versions.sh that have been written. When you execute them, they will check you have the right module loaded for the type of code (CPU or GPU) you are trying to compile :)"

all: all_cpu \
	 all_gpu

all_cpu: create_directories \
		 $(BIN_DIRECTORY)/c/cpu_big \
	  	 $(BIN_DIRECTORY)/c/cpu_small \
		 $(BIN_DIRECTORY)/f/cpu_big \
	  	 $(BIN_DIRECTORY)/f/cpu_small

all_gpu: create_directories \
	 	 $(BIN_DIRECTORY)/f/gpu_big \
	 	 $(BIN_DIRECTORY)/f/gpu_small \
	 	 $(BIN_DIRECTORY)/c/gpu_big \
	 	 $(BIN_DIRECTORY)/c/gpu_small

create_directories:
	@if [ ! -d $(BIN_DIRECTORY) ]; then mkdir $(BIN_DIRECTORY); fi; \
	if [ ! -d $(DAT_DIRECTORY) ]; then mkdir $(DAT_DIRECTORY); fi; \
	if [ ! -d $(BIN_DIRECTORY)/$(C_DIRECTORY) ]; then mkdir $(BIN_DIRECTORY)/$(C_DIRECTORY); fi; \
	if [ ! -d $(BIN_DIRECTORY)/$(FORTRAN_DIRECTORY) ]; then mkdir $(BIN_DIRECTORY)/$(FORTRAN_DIRECTORY); fi 

$(BIN_DIRECTORY)/c/cpu_big: $(SRC_DIRECTORY)/c/cpu.c
	$(CC) -o $@ $^ $(CFLAGS) -fopenmp -DROWS=15360 -DCOLUMNS=15360 -DROWS_PER_MPI_PROCESS=3840 -DCOLUMNS_PER_MPI_PROCESS=15360 -DBIG

$(BIN_DIRECTORY)/c/cpu_small: $(SRC_DIRECTORY)/c/cpu.c
	$(CC) -o $@ $^ $(CFLAGS) -fopenmp -DROWS=512 -DCOLUMNS=512 -DROWS_PER_MPI_PROCESS=128 -DCOLUMNS_PER_MPI_PROCESS=512 -DSMALL

$(BIN_DIRECTORY)/c/gpu_big: $(SRC_DIRECTORY)/c/gpu.c
	$(CC) -acc -Minfo=accel -o $@ $^ $(CFLAGS) -DROWS=15360 -DCOLUMNS=15360 -DROWS_PER_MPI_PROCESS=1920 -DCOLUMNS_PER_MPI_PROCESS=15360 -DBIG 

$(BIN_DIRECTORY)/c/gpu_small: $(SRC_DIRECTORY)/c/gpu.c
	$(CC) -acc -Minfo=accel -o $@ $^ $(CFLAGS) -DSMALL -DROWS=512 -DCOLUMNS=512 -DROWS_PER_MPI_PROCESS=256 -DCOLUMNS_PER_MPI_PROCESS=512

$(BIN_DIRECTORY)/f/cpu_big: $(SRC_DIRECTORY)/f/util.F90 $(SRC_DIRECTORY)/f/cpu.F90 
	$(CF) -o $@ $^ $(FFLAGS) -fopenmp  -DROWS=15360 -DCOLUMNS=15360 -DROWS_PER_MPI_PROCESS=15360 -DCOLUMNS_PER_MPI_PROCESS=3840 -DBIG

$(BIN_DIRECTORY)/f/cpu_small: $(SRC_DIRECTORY)/f/util.F90 $(SRC_DIRECTORY)/f/cpu.F90
	$(CF) -o $@ $^ $(FFLAGS) -fopenmp -DROWS=512 -DCOLUMNS=512 -DROWS_PER_MPI_PROCESS=512 -DCOLUMNS_PER_MPI_PROCESS=128 -DSMALL

$(BIN_DIRECTORY)/f/gpu_big: $(SRC_DIRECTORY)/f/util.F90 $(SRC_DIRECTORY)/f/gpu.F90
	$(CF) -acc -Minfo=accel -o $@ $^ $(FFLAGS) -DROWS=15360 -DCOLUMNS=15360 -DROWS_PER_MPI_PROCESS=15360 -DCOLUMNS_PER_MPI_PROCESS=1920 -DBIG 

$(BIN_DIRECTORY)/f/gpu_small: $(SRC_DIRECTORY)/f/util.F90 $(SRC_DIRECTORY)/f/gpu.F90
	$(CF) -acc -Minfo=accel -o $@ $^ $(FFLAGS) -DSMALL -DROWS=512 -DCOLUMNS=512 -DROWS_PER_MPI_PROCESS=512 -DCOLUMNS_PER_MPI_PROCESS=256

clean:
	@if [ -d $(BIN_DIRECTORY) ]; then rm -rf $(BIN_DIRECTORY); fi;

clean_cpu:
	@if [ -d $(BIN_DIRECTORY) ]; then rm -rf $(BIN_DIRECTORY)/c/cpu* $(BIN_DIRECTORY)/f/cpu*; fi;

clean_gpu:
	@if [ -d $(BIN_DIRECTORY) ]; then rm -rf $(BIN_DIRECTORY)/c/gpu* $(BIN_DIRECTORY)/f/gpu*; fi;
