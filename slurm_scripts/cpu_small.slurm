#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=RM-shared
#SBATCH --time=00:01:30
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=4
#SBATCH --job-name=IHPCSS
#SBATCH --output=error.txt
#SBATCH --error=error.txt

# OpenMP placement
export OMP_NUM_THREADS=4
export OMP_PROC_BIND=true
export OMP_PLACES=cores

# Run the binary
module unload nvhpc
module load mvapich2/2.3.5-gcc8.3.1;
mpirun -np ${SLURM_NTASKS} --map-by core -bind-to core ./bin/${1}/cpu_small > $2
