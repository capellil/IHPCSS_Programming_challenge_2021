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

# Run the binary
module load nvhpc
mpirun -np ${SLURM_NTASKS} --map-by socket -bind-to socket -report-bindings ./bin/${1}/cpu_big
