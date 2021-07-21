#!/bin/bash
#SBATCH --nodes=2
#SBATCH --partition=RM-small
#SBATCH --time=00:01:30
#SBATCH --ntasks-per-node=2
#SBATCH --cpus-per-task=64
#SBATCH --job-name=IHPCSS
#SBATCH --output=report_%N_%j.txt
#SBATCH --error=error.txt

# OpenMP placement
export OMP_NUM_THREADS=64
export OMP_PROC_BIND=true
export OMP_PLACES=cores

cd /jet/home/capellil/IHPCSS_Programming_challenge_2021

# Create the output directory in case it does not already exist
mkdir -p outputs;
# Empty that output directory in case it contains output from previous run
rm -f outputs/*;

# Run the binary
mpirun -np ${SLURM_NTASKS} --map-by socket -bind-to socket -report-bindings ./bin/main ./datasets/C_challenge.dat
