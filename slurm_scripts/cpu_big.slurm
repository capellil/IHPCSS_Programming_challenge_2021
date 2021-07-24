#!/bin/bash
#SBATCH --nodes=2
#SBATCH --partition=RM
#SBATCH --time=00:01:30
#SBATCH --ntasks-per-node=2
#SBATCH --cpus-per-task=64
#SBATCH --job-name=IHPCSS
#SBATCH --output=error.txt
#SBATCH --error=error.txt
#SBATCH --reservation=hybridIHPCSS

# OpenMP placement
export OMP_NUM_THREADS=64
export OMP_PROC_BIND=true
export OMP_PLACES=cores

# Run the binary
module unload nvhpc
module load mvapich2/2.3.5-gcc8.3.1
mpirun -np ${SLURM_NTASKS} --map-by socket -bind-to socket ./bin/${1}/cpu_big > $2
