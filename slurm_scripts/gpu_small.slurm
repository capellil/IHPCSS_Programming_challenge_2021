#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=GPU-shared
#SBATCH --time=00:01:30
#SBATCH --ntasks-per-node=2
#SBATCH --cpus-per-task=1
#SBATCH --job-name=IHPCSS
#SBATCH --output=error.txt
#SBATCH --error=error.txt
#SBATCH --gres=gpu:v100:2

# OpenMP placement
export OMP_NUM_THREADS=1
export OMP_PROC_BIND=true
export OMP_PLACES=sockets

# Run the binary
module load nvhpc/21.2
mpirun -np ${SLURM_NTASKS} --map-by socket -bind-to socket -report-bindings ./bin/${1}/gpu_small > $2
