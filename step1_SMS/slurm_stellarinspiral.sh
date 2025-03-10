#!/bin/bash
#SBATCH --account=corinne
#SBATCH --job-name=st1smse3e3
#SBATCH --partition=public-cpu,shared-cpu,private-astro-cpu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task 10
#SBATCH --time=5:00:00
#SBATCH --output=output.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jaime.romangarza@unige.ch
#SBATCH --mem=10000


export MESA_DIR=/home/users/r/romangar/MESA/mesa-23051

export OMP_NUM_THREADS=10

export MESASDK_ROOT=/home/users/r/romangar/MESA/mesasdk
source $MESASDK_ROOT/bin/mesasdk_init.sh


# srun ./clean && ./mk && ./rn
srun ./clean && ./mk && ./rn