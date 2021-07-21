#!/bin/bash
#SBATCH --nodes=2
#SBATCH --partition=RM
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

# Run the binary
mpirun -np ${SLURM_NTASKS} --map-by socket -bind-to socket -report-bindings ./bin/${1}/mpi_openmp_big ./datasets/${1}/challenge.dat
