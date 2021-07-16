#!/bin/bash
#SBATCH --nodes=2
#SBATCH --partition=RM
#SBATCH --time=00:00:45
#SBATCH --ntasks-per-node=128
#SBATCH --job-name=IHPCSS
#SBATCH --output=report.txt
#SBATCH --error=error.txt

# OpenMP placement
#export OMP_NUM_THREADS=32
#export OMP_PROC_BIND=true
#export OMP_PLACES=cores

cd /jet/home/capellil/IHPCSS_Programming_challenge_2021

# Create the output directory in case it does not already exist
mkdir -p outputs;
# Empty that output directory in case it contains output from previous run
rm -f outputs/*;

# Run the binary
mpirun -n ${SLURM_NTASKS} ./bin/main ./datasets/C_challenge.dat
